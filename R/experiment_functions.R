data(deepsqueak_data, envir=environment())

globalVariables(c("sd"))

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
#' @examples \dontrun{add_timepoint_data(
#'   data_path = "~/inst/extdata/Example_Mouse_Data.xlsx",
#'   t1 = 3, t2 = 12
#' )}
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
#' @param id The full name of the experiment, including the extension.
#' @param animal The animal or animal group ID for these data
#' @param experimenter The experimenter who recorded these results
#'
#' @return A list object containing statistics and metadata for each file.
#'
#' @examples \dontrun{score_timepoint_data(
#'   data_subset = my_subsetted_data, group = "Control",
#'   experimenter = "Experimenter 1"
#' )}
#'
#' @import readxl
#' @import dplyr
#' @export
score_timepoint_data <- function(data_subset, group, animal, id, experimenter) {
  message("Summarizing call features for datapoint...")
  timepoint_metrics <- list(
    id = id,
    animal = animal,
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
    animals = c(),
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
#' @param experiment The experiment object
#' @param added_data The scored data object to be added to the experiment
#'
#' @return A list object containing statistics and metadata for the entire experiment.
#' The `groups` and `experimenters` field will auto-populate based on added data using
#' the `update_experiment()` function.
#'
#' @examples \dontrun{add_to_experiment(experiment = experiment, added_data = my_data)}
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
#' @description Auto-populates `groups`, `experimenters`, and `animals` fields in experiment object
#' by checking experimental data (within the experiment object) for new data.
#'
#' @param experiment The experiment object
#'
#' @return A list object containing statistics and metadata for the entire experiment.
#' The `groups`, `experimenters`, and `animals` fields will auto-populate based on added data using
#' the `update_experiment()` function.
#'
#' @examples \dontrun{update_experiment(experiment)}
#'
#' @export
update_experiment <- function(experiment) {
  message("Updating experiment metadata...")
  extracted_groups <- c()
  extracted_experimenters <- c()
  extracted_animals <- c()

  for (i in 1:length(experiment$experimental_data)) {
    extracted_groups <- append(extracted_groups, experiment$experimental_data[i]$call_data$group)
  }

  for (i in 1:length(experiment$experimental_data)) {
    extracted_experimenters <- append(extracted_experimenters, experiment$experimental_data[i]$call_data$experimenter)
  }

  for (i in 1:length(experiment$experimental_data)) {
    extracted_animals <- append(extracted_animals, experiment$experimental_data[i]$call_data$animal)
  }

  experiment$groups <- unique(extracted_groups)
  experiment$experimenters <- unique(extracted_experimenters)
  experiment$animals <- unique(extracted_animals)
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
#' @examples \dontrun{save_experiment(experiment = experiment_object, save_path = tempdir())}
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
#' @examples \dontrun{describe_experiment(experiment = experiment_object)}
#'
#' @export
describe_experiment <- function(experiment) {
  message("Experiment name: ", experiment$name)
  message("Last saved: ", as.character(experiment$last_saved))
  message("Experimenter(s): ", paste(experiment$experimenters, collapse = ", "))
  message("Animal(s): ", paste(experiment$animals, collapse = ", "))
  message("Experimental group(s): ", paste(experiment$groups, collapse = ", "))
  message("Total call datasets: ", as.character(length(experiment$experimental_data)))
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

#' @title Remove Data from Experiment File
#'
#' @description Removes the data object of interest from the experiment according to
#' a data_id. Removing the data with data_id = 12 removes the 12th dataset added to
#' the experiment.
#'
#' @param experiment The experiment object
#' @param data_id The number of the data file to be removed
#'
#' @return The experiment file, with the rejected call data removed.
#'
#' @examples \dontrun{experiment <- remove_experiment_data(experiment = experiment_object, data_id = 12)}
#'
#' @export
remove_experiment_data <- function(experiment, data_id) {
  experiment$experimental_data <- experiment$experimental_data[-as.numeric(data_id)]
  experiment <- update_experiment(experiment)
  return(experiment)
}


#' @title Decode Experiment IDs
#'
#' @description Creates a vector of the original call file names, indexed
#' by the order they are listed in the experiment. This allows experimenters
#' to unblind themselves to the data they collect
#'
#' @param experiment The experiment object
#'
#' @return A vector representing the original call file names
#'
#' @examples \dontrun{decode_experiment_ids(experiment)}
#'
#' @export
unblind_all_ids <- function(experiment) {
  decode_vector <- c()
  for (dataset in 1:length(experiment$experimental_data)) {
    decode_vector <- c(decode_vector, experiment$experimental_data[dataset]$call_data$id)
  }
  return(decode_vector)
}


#' @title Find Matching Experiment ID
#'
#' @description Finds the index of a dataset matching a supplied file name
#' in the experiment.
#'
#' @param experiment The experiment object
#' @param filename The full name of the file, including the extension
#'
#' @return A number or numbers representing index or indices where that file
#' appears in the experiment
#'
#' @examples \dontrun{unblind_data_id(experiment, "my_data1.xlsx")}
#'
#' @export
unblind_data_id <- function(experiment, filename) {
  expt_ids <- unblind_all_ids(experiment)
  index <- which(expt_ids == filename)
  return(index)
}


#' @title Find Matching Experiment Name
#'
#' @description Finds the name of a dataset matching a supplied index in the set of data
#'
#' @param experiment The experiment object
#' @param id The dataset number to be unblinded
#'
#' @return The name of the original file, corresponding to the data at
#' the requested index
#'
#' @examples \dontrun{unblind_data_name(experiment, 2)}
#'
#' @export
unblind_data_name <- function(experiment, id) {
  expt_ids <- unblind_all_ids(experiment)
  name <- expt_ids[as.numeric(id)]
  return(name)
}


