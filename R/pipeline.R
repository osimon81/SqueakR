######### Pipeline Functions #########

#' @title Full Experiment Creation
#'
#' @description A pipeline function which creates a new experiment object and adds
#' data by prompting the user
#'
#' @return An object containing the full experiment with all data added.
#'
#' @examples expt <- experiment_pipeline()
#'
#' @import readxl
#' @import dplyr
#' @export
experiment_pipeline <- function() {
  experiment_name <- readline(prompt = "Enter a name for this experiment: ")
  experiment <- create_experiment(experiment_name = experiment_name)

  message()

  data_directory <- readline(prompt = "Enter the full path to the folder containing the DeepSqueak Excel data: ")

  files <- list.files(path = data_directory, pattern = paste0("\\.xlsx$"), recursive = TRUE)

  for (data_file in files) {
    message()
    message("File loaded: ", data_file)
    t1_data <- readline(prompt = paste0("Enter the start time for call extraction for this file (leave blank to extract from the beginning): "))
    t2_data <- readline(prompt = paste0("Enter the end time for call extraction for this file (leave blank to extract calls until the end): "))
    data_group <- readline(prompt = paste0("Enter the group for this file: "))
    data_experimenter <- readline(prompt = paste0("Enter the experimenter who collected data for this file: "))

    new_data <- add_timepoint_data(data_path = file.path(data_directory, data_file),
                                   t1 = t1_data, t2 = t2_data) %>%
      score_timepoint_data(group = data_group, experimenter = data_experimenter)

    experiment <- add_to_experiment(experiment, new_data)
  }

  message()

  save_prompt <- readline(prompt = "Would you like to save this experiment locally? (y/n): ")

    if (tolower(save_prompt) == "y") {
      save_path <- readline(prompt = "Enter the full path for where you'd like to save the experiment: ")
      experiment <- update_experiment(experiment) %>%
        save_experiment(save_path = save_path)
    } else if (tolower(save_prompt) == "yes") {
      save_path <- readline(prompt = "Enter the full path for where you'd like to save the experiment: ")
      experiment <- update_experiment(experiment) %>%
        save_experiment(save_path = save_path)
    }
    else {
      experiment <- update_experiment(experiment)
    }

  message("Pipeline complete!")
  message()

  describe_experiment(experiment)

  return(experiment)
}


