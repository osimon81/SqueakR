data(deepsqueak, envir=environment())

######### Data Cleaning Functions #########

#' @title Add Timepoint Data
#'
#' @description Loads in a specific Excel File, and (optional) selects a time subset of data.
#'
#' @param data_path The full path to the Excel file exported from DeepSqueak
#' @param t1 The start time in the recording (in seconds) to start extracting calls
#' @param t2 The end time in the recording (in seconds) to stop extracting calls
#'
#' @return An object containing the call data (under time constraints if specified)
#'
#' @examples add_timepoint_data(
#'   data_path = "inst/extdata/Example_Mouse_Data.xlsx",
#'   t1 = 3, t2 = 12
#' )
#'
#' @import readxl
#' @import dplyr
#' @export
add_timepoint_data <- function(data_path, t1 = "", t2 = "") {
  message("Adding call features Excel file to workspace...")
  data_subset <- loadSpecData(data_path)
  if (t1 == "") {
    t1 <- min(data_subset$`Begin Time (s)`)
  }
  if (t2 == "") {
    t2 <- max(data_subset$`End Time (s)`)
  }

  data_subset <- filter(data_subset, `Begin Time (s)` >= as.double(t1), `End Time (s)` <= as.double(t2))
  message("Restricting data to range: ", as.character(t1), " to ", as.character(t2), " seconds...")
  return(data_subset)
}


#' @title Score Timepoint Data
#'
#' @description Transforms data into a list of mean, standard deviation, and range of several call metrics.
#'
#' @param data_subset The object created in `add_timepoint_data()` which will be scored
#' @param group The experimental group (i.e. "Control") these data correspond to
#' @param experimenter The experimenter who recorded these results
#'
#' @return A list object containing statistics and metadata for each file.
#'
#' @examples score_timepoint_data(
#'   data_subset = my_subsetted_data, group = "Control",
#'   experimenter = "Experimenter 1"
#' )
#'
#' @import readxl
#' @import dplyr
#' @export
score_timepoint_data <- function(data_subset, group, experimenter) {
  message("Summarizing call features for datapoint...")
  timepoint_metrics <- list(
    group = group,
    experimenter = experimenter,
    calls_n = count(data_subset)[[1]],
    call_length = list(
      mean = mean(data_subset$`Call Length (s)`),
      standard_deviation = sd(data_subset$`Call Length (s)`),
      range = range(data_subset$`Call Length (s)`)[2] - range(data_subset$`Call Length (s)`)[1]
    ),
    delta_frequency = list(
      mean = mean(data_subset$`Delta Freq (kHz)`),
      standard_deviation = sd(data_subset$`Delta Freq (kHz)`),
      range = range(data_subset$`Delta Freq (kHz)`)[2] - range(data_subset$`Delta Freq (kHz)`)[1]
    ),
    high_frequency = list(
      mean = mean(data_subset$`High Freq (kHz)`),
      standard_deviation = sd(data_subset$`High Freq (kHz)`),
      range = range(data_subset$`High Freq (kHz)`)[2] - range(data_subset$`High Freq (kHz)`)[1]
    ),
    low_frequency = list(
      mean = mean(data_subset$`Low Freq (kHz)`),
      standard_deviation = sd(data_subset$`Low Freq (kHz)`),
      range = range(data_subset$`Low Freq (kHz)`)[2] - range(data_subset$`Low Freq (kHz)`)[1]
    ),
    peak_frequency = list(
      mean = mean(data_subset$`Peak Freq (kHz)`),
      standard_deviation = sd(data_subset$`Peak Freq (kHz)`),
      range = range(data_subset$`Peak Freq (kHz)`)[2] - range(data_subset$`Peak Freq (kHz)`)[1]
    ),
    power = list(
      mean = mean(data_subset$`Mean Power (dB/Hz)`),
      standard_deviation = sd(data_subset$`Mean Power (dB/Hz)`),
      range = range(data_subset$`Mean Power (dB/Hz)`)[2] - range(data_subset$`Mean Power (dB/Hz)`)[1]
    ),
    principal_frequency = list(
      mean = mean(data_subset$`Principal Frequency (kHz)`),
      standard_deviation = sd(data_subset$`Principal Frequency (kHz)`),
      range = range(data_subset$`Principal Frequency (kHz)`)[2] - range(data_subset$`Principal Frequency (kHz)`)[1]
    ),
    sinuosity = list(
      mean = mean(data_subset$`Sinuosity`),
      standard_deviation = sd(data_subset$`Sinuosity`),
      range = range(data_subset$`Sinuosity`)[2] - range(data_subset$`Sinuosity`)[1]
    ),
    slope = list(
      mean = mean(data_subset$`Slope (kHz/s)`),
      standard_deviation = sd(data_subset$`Slope (kHz/s)`),
      range = range(data_subset$`Slope (kHz/s)`)[2] - range(data_subset$`Slope (kHz/s)`)[1]
    ),
    tonality = list(
      mean = mean(data_subset$`Tonality`),
      standard_deviation = sd(data_subset$`Tonality`),
      range = range(data_subset$`Tonality`)[2] - range(data_subset$`Tonality`)[1]
    ),
    raw = data_subset
  )
  return(timepoint_metrics)
}

######### Experiment Functions #########

#' @title Create New Experiment
#'
#' @description Creates an experiment object which will contain all data and metadata.
#' This object will be saved and timestamped by date, so if working with this file over
#' different dates, the object will not be overwritten as a new object will be created
#' (to preserve backups).
#'
#' @param experiment_name The name of the experiment
#'
#' @return A list object containing statistics and metadata for the entire experiment.
#' The `groups` and `experimenters` field will auto-populate based on added data using
#' the `update_experiment()` function.
#'
#' @examples create_experiment(experiment_name = "My-Project")
#'
#' @export
create_experiment <- function(experiment_name) {
  message("Creating new experiment...")
  experiment <- list(
    name = experiment_name,
    last_saved = Sys.time(),
    groups = c(),
    experimenters = c(),
    experimental_data = list()
  )
  return(experiment)
}

#' @title Add Scored Data to Experiment Object
#'
#' @description Adds summarized timepoint data (acquired by running `add_timepoint_data()`,
#' followed by `score_timepoint_data()`) to the created experiment object.
#'
#' @param experiment_name The name of the experiment
#'
#' @return A list object containing statistics and metadata for the entire experiment.
#' The `groups` and `experimenters` field will auto-populate based on added data using
#' the `update_experiment()` function.
#'
#' @examples add_to_experiment(experiment_name = "My-Project", added_data = my_data)
#'
#' @import dplyr
#' @export
add_to_experiment <- function(experiment, added_data) {
  message("Adding summarized data to experiment object...")
  experiment$experimental_data <- experiment$experimental_data %>%
    append(list(call_data = added_data))
  experiment <- update_experiment(experiment)
  return(experiment)
}

#' @title Updates Experiment Object Metadata
#'
#' @description Auto-populates `groups` and `experimenters` fields in experiment object
#' by checking experimental data (within the experiment object) for new data.
#'
#' @param experiment The experiment object
#'
#' @return A list object containing statistics and metadata for the entire experiment.
#' The `groups` and `experimenters` field will auto-populate based on added data using
#' the `update_experiment()` function.
#'
#' @examples update_experiment(experiment)
#'
#' @export
update_experiment <- function(experiment) {
  message("Updating experiment metadata...")
  extracted_groups <- c()
  extracted_experimenters <- c()

  for (i in 1:length(experiment$experimental_data)) {
    extracted_groups <- append(extracted_groups, experiment$experimental_data[i]$call_data$group)
  }

  for (i in 1:length(experiment$experimental_data)) {
    extracted_experimenters <- append(extracted_experimenters, experiment$experimental_data[i]$call_data$experimenter)
  }

  experiment$groups <- unique(extracted_groups)
  experiment$experimenters <- unique(extracted_experimenters)
  return(experiment)
}

#' @title Save Experiment Locally
#'
#' @description Saves the experiment as an .RData file in a specified location.
#'
#' @param experiment The experiment object to be saved
#' @param save_path The full path where the experiment object will be saved locally
#'
#' @return The experiment object, saved as "[NAME_OF_EXPERIMENT] ([SAVE_DATE]).RData" to the specified location.
#'
#' @examples save_experiment(experiment = experiment_object, save_path = tempdir())
#'
#' @import dplyr
#' @export
save_experiment <- function(experiment, save_path) {
  message("Saving and timestamping experiment object...")
  experiment$last_saved <- Sys.time()
  save(object = experiment, file = paste0(save_path, "/", experiment$name, " (", as.character(Sys.Date()), ").RData"))

  return(experiment)
}

#' @title Describe Experiment
#'
#' @description Lists a condensed summary of data stored in the experiment object.
#'
#' @param experiment The experiment object to be saved
#'
#' @return A list of information about the experiment
#'
#' @examples describe_experiment(experiment = experiment_object)
#'
#' @export
describe_experiment <- function(experiment) {
  message("Experimental name: ", experiment$name)
  message("Last saved: ", as.character(experiment$last_saved))
  message("Experimenters: ", paste(experiment$experimenters, collapse = ", "))
  message("Experimental groups: ", paste(experiment$groups, collapse = ", "))
  message("Total call datapoints: ", as.character(length(experiment$experimental_data)))
  for (this_group in experiment$groups) {
    group_count <- 0
    for (i in 1:length(experiment$experimental_data)) {
      if (experiment$experimental_data[i]$call_data$group == this_group) {
        group_count <- group_count + 1
      }
    }
    message("Data for ", this_group, ": ", as.character(group_count))
  }
}
