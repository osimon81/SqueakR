data(deepsqueak_data, envir=environment())

######### Hidden Functions #########

#' Loads and cleans Google Sheets metadata sheet, through user prompting
#' @keywords internal
#' @noRd
load_cleaned_sheet <- function() {
  picked_link = readline(prompt = "Insert the link to your Google Sheets file which contains your experimental metadata (may require authentification): ")
  picked_sheet = as.numeric(readline(prompt = "Write the sheet number you want to load (default is 1): "))

  if (picked_sheet == "") {
    picked_sheet <-  1
    message("Setting sheet number to default: 1")
  }

  data <- read_sheet(picked_link, sheet = picked_sheet)

  happywithsettings <- FALSE

  while (happywithsettings == FALSE) {
    filecol <- as.numeric(readline(prompt = "Enter the column number which corresponds to your file names: "))
    groupcol <- as.numeric(readline(prompt = "Enter the column number which corresponds to your experimental group names: "))
    exptrcol <- as.numeric(readline(prompt = "Enter the column number which corresponds to the name of the experimenter who collected data: "))
    animalcol <- as.numeric(readline(prompt = "Enter the column number which corresponds to which animal was recorded: "))
    t1col <- as.numeric(readline(prompt = "Enter the column number which corresponds to T1 (when calls will START being extracted): "))
    t2col <- as.numeric(readline(prompt = "Enter the column number which corresponds to T2 (when calls will STOP being extracted): "))

    message("Filename column: ", as.character(filecol))
    message("Group column: ", as.character(groupcol))
    message("Experimenter column: ", as.character(exptrcol))
    message("Animal column: ", as.character(animalcol))
    message("Timepoint 1 column: ", as.character(t1col))
    message("Timepoint 2 column: ", as.character(t2col))

    confirm <- readline(prompt = paste0("You have assigned the columns shown above. Are these settings correct? (y/n): "))
    if (confirm == "y") {
      happywithsettings = TRUE

    } else {
      message("Reassigning columns...")
    }
  }

  names(data)[filecol] = "file"
  names(data)[groupcol] = "group"
  names(data)[exptrcol] = "experimenter"
  names(data)[animalcol] = "animal"
  names(data)[t1col] = "timepoint1"
  names(data)[t2col] = "timepoint2"

  return(data)
}


######### Pipeline Functions #########

#' @title Semi-Automatic Experiment Creation
#'
#' @description A pipeline function which creates a new experiment object and adds
#' data by prompting the user for metadata (for every file that is added).
#'
#' @return An object containing the full experiment with all data added.
#'
#' @examples \dontrun{expt <- semisqueakRpipeline()}
#'
#' @import readxl
#' @import dplyr
#' @export
semisqueakRpipeline <- function() {
  experiment_name <- readline(prompt = "Enter a name for this experiment: ")
  experiment <- create_experiment(experiment_name = experiment_name)

  message()

  data_directory <- readline(prompt = "Enter the full path to the folder containing the DeepSqueak Excel data: ")

  files <- list.files(path = data_directory, pattern = paste0("\\.xlsx$"), recursive = TRUE)

  for (data_file in files) {
    message()
    message("File loaded: ", data_file)
    id_data <- data_file
    t1_data <- readline(prompt = paste0("Enter the start time for call extraction for this file (leave blank to extract from the beginning): "))
    t2_data <- readline(prompt = paste0("Enter the end time for call extraction for this file (leave blank to extract calls until the end): "))
    animal_data <- readline(prompt = paste0("Enter the ID(s) for the animal(s) recorded for this file: "))
    data_group <- readline(prompt = paste0("Enter the experimental group for this file: "))
    data_experimenter <- readline(prompt = paste0("Enter the experimenter who collected data for this file: "))

    new_data <- add_timepoint_data(
      data_path = file.path(data_directory, data_file),
      t1 = t1_data, t2 = t2_data
    )
    new_data <- score_timepoint_data(new_data, group = data_group, id = data_file, animal = animal_data,
                                     experimenter = data_experimenter)

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
  } else {
    experiment <- update_experiment(experiment)
  }

  message("Pipeline complete!")
  message()

  describe_experiment(experiment)

  return(experiment)
}


#' @title Fully-Automatic Experiment Creation
#'
#' @description A pipeline function which creates a new experiment object,
#' and adds data and metadata referenced from an external Google Sheets document.
#'
#' @return An object containing the full experiment with all data added.
#'
#' @examples \dontrun{expt <- autosqueakRpipeline()}
#'
#' @import googlesheets4
#' @export
autosqueakRpipeline <- function() {
  experiment_name <- readline(prompt = "Enter a name for this experiment: ")
  experiment <- create_experiment(experiment_name = experiment_name)

  message()

  data_sheet <- load_cleaned_sheet()

  data_directory <- readline(prompt = "Enter the full path to the folder containing the DeepSqueak Excel data: ")

  files <- list.files(path = data_directory, pattern = paste0("\\.xlsx$"), recursive = TRUE)

  for (data_file in files) {
    message()
    message("File loaded: ", data_file)

    index <- which(data_sheet$file == data_file)
    message("Matched with index ", as.character(index), " of remote metadata.")

    id_data <- data_sheet$file[index]
    message("ID assigned: ", as.character(id_data))

    t1_data <- data_sheet$timepoint1[index]
    message("T1 assigned: ", as.character(t1_data))

    t2_data <- data_sheet$timepoint2[index]
    message("T2 assigned: ", as.character(t2_data))

    data_group <- data_sheet$group[index]
    message("Group assigned: ", as.character(data_group))

    data_animal <- data_sheet$animal[index]
    message("Animal assigned: ", as.character(data_animal))

    data_experimenter <- data_sheet$experimenter[index]
    message("Experimenter assigned: ", as.character(data_experimenter))

    new_data <- add_timepoint_data(data_path = file.path(data_directory, data_file),
                                   t1 = t1_data, t2 = t2_data)
    new_data <- score_timepoint_data(new_data, group = data_group, id = id_data,
                                     animal = as.character(data_animal),
                                     experimenter = data_experimenter)

    experiment <- add_to_experiment(experiment, new_data)
  }

  save_prompt <- readline(prompt = "Would you like to save this experiment locally? (y/n): ")

  if (tolower(save_prompt) == "y") {
    save_path <- readline(prompt = "Enter the full path for where you'd like to save the experiment: ")
    experiment <- update_experiment(experiment) %>%
      save_experiment(save_path = save_path)
  } else if (tolower(save_prompt) == "yes") {
    save_path <- readline(prompt = "Enter the full path for where you'd like to save the experiment: ")
    experiment <- update_experiment(experiment) %>%
      save_experiment(save_path = save_path)
  } else {
    experiment <- update_experiment(experiment)
  }

  message("Pipeline complete!")
  message()

  describe_experiment(experiment)

  return(experiment)
}
