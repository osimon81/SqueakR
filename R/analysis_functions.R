data(deepsqueak_data, envir=environment())

globalVariables(c("group", "sd"))

#' @title Analyze Factor
#'
#' @description Compares data across experimental groups, plotting the data as a
#' bar graph with error bars.
#'
#' @param experiment The experiment object
#' @param analysis_factor A string representing the factor to analyze between groups
#'
#' @return A bar graph comparing the analysis_factor between groups
#'
#' @examples \dontrun{analyze_factor(experiment = experiment, analysis_factor = "tonality")}
#'
#' @import dplyr
#' @import rlist
#' @import ggplot2
#' @export
analyze_factor <- function(experiment, analysis_factor) {
  group_organized <- list.group(experiment$experimental_data, group)

  expt_groups <- list(
    group1 = c(),
    group2 = c(),
    group3 = c(),
    group4 = c(),
    group5 = c(),
    group6 = c(),
  )

  names(expt_groups) <- unique(names(group_organized))

  for (h in 1:length(group_organized)) {
    for (i in 1:length(group_organized[[h]])) {
      if (analysis_factor == "call_length") {
        expt_groups[[h]] <- c(expt_groups[[h]], group_organized[[h]][i]$call_data$raw$`Call Length (s)`)
      } else if (analysis_factor == "delta_frequency") {
        expt_groups[[h]] <- c(expt_groups[[h]], group_organized[[h]][i]$call_data$raw$`Delta Freq (kHz)`)
      } else if (analysis_factor == "high_frequency") {
        expt_groups[[h]] <- c(expt_groups[[h]], group_organized[[h]][i]$call_data$raw$`High Freq (kHz)`)
      } else if (analysis_factor == "low_frequency") {
        expt_groups[[h]] <- c(expt_groups[[h]], group_organized[[h]][i]$call_data$raw$`Low Freq (kHz)`)
      } else if (analysis_factor == "peak_frequency") {
        expt_groups[[h]] <- c(expt_groups[[h]], group_organized[[h]][i]$call_data$raw$`Peak Freq (kHz)`)
      } else if (analysis_factor == "power") {
        expt_groups[[h]] <- c(expt_groups[[h]], group_organized[[h]][i]$call_data$raw$`Mean Power (dB/Hz)`)
      } else if (analysis_factor == "principal_frequency") {
        expt_groups[[h]] <- c(expt_groups[[h]], group_organized[[h]][i]$call_data$raw$`Principal Frequency (kHz)`)
      } else if (analysis_factor == "sinuosity") {
        expt_groups[[h]] <- c(expt_groups[[h]], group_organized[[h]][i]$call_data$raw$Sinuosity)
      } else if (analysis_factor == "slope") {
        expt_groups[[h]] <- c(expt_groups[[h]], group_organized[[h]][i]$call_data$raw$`Slope (kHz/s)`)
      } else if (analysis_factor == "tonality") {
        expt_groups[[h]] <- c(expt_groups[[h]], group_organized[[h]][i]$call_data$raw$Tonality)
      } else {
        stop("Out of bounds analysis_factor")
      }
    }
  }

  names(expt_groups) <- unique(names(group_organized))

  for (i in 1:length(expt_groups)) {
    if (is.null(expt_groups[[i]])) {
      expt_groups[i] <- NA
    }
  }

  expt_groups <- expt_groups[!is.na(expt_groups)]

  ns <- c()

  for (i in 1:length(expt_groups)) {
    ns <- c(ns, length(expt_groups[[i]]))
  }

  means <- unlist(lapply(expt_groups, mean, na.rm = TRUE))
  stdevs <- unlist(lapply(expt_groups, sd, na.rm = TRUE))
  stderr <- stdevs / sqrt(ns)


  df <- as.data.frame(rbind(means, stdevs))
  df <- as.data.frame(lapply(df, unlist))


  ggplot() +
    geom_col(mapping = aes(x = names(df), y = unlist(df[1, ]), fill = names(df)), color = "black") +
    geom_errorbar(mapping = aes(
      x = names(df), y = unlist(df[1, ]),
      ymin = means - 2 * stderr, ymax = means + 2 * stderr, width = .2
    )) +
    geom_text(aes(
      x = names(df), y = 0,
      label = paste0("n = ", ns)
    ), position = position_dodge(width = 0.9), vjust = -0.5) +
    xlab("Group") +
    ylab("Mean value") +
    ggtitle("Comparison between groups") +
    scale_fill_hue(name = "Group") +
    labs(
      caption = ("Error bars represent the mean +/- 2 standard errors."),
      subtitle = paste0("Factor being compared: ", analysis_factor)
    )
}


#' @title Generate ANOVA tables for Detected Calls
#'
#' @description Aggregates raw data tables for all data added to experiment object,
#' and calculates and displays ANOVA statistics.
#'
#' @param experiment The experiment object
#' @param analysis_factor A string representing the factor to analyze between groups
#'
#' @return A bar graph comparing the analysis_factor between groups
#'
#' @examples \dontrun{squeakrANOVA(experiment = experiment, analysis_factor = "Delta_Freq")}
#'
#' @import dplyr
#' @import rlist
#' @importFrom rstatix anova_test
#' @export
squeakrANOVA <- function(experiment, analysis_factor) {
  group_organized <- list.group(experiment$experimental_data, group)

  combined_df <- data.frame(matrix(ncol = ncol(experiment$experimental_data[1]$call_data$raw), nrow = 0))

  for (selected_group in 1:length(names(group_organized))) {
    for (dataset in 1:length(group_organized[[selected_group]])) {
      set_to_add <- group_organized[[selected_group]][dataset]$call_data$raw %>%
        mutate(group = names(group_organized)[selected_group])
      combined_df <- rbind(combined_df, set_to_add)
    }
  }

  combined_df <- combined_df %>%
    mutate(index = 1:nrow(combined_df))
  colnames(combined_df) <- trimws(gsub(r"{\s*\([^\)]+\)}","", as.character(names(combined_df))))
  colnames(combined_df) <- gsub(" ", "_", names(combined_df), fixed = TRUE)
  res.aov <- anova_test(data = combined_df, dv = analysis_factor, wid = index, between = group)
  return(get_anova_table(res.aov))
}







