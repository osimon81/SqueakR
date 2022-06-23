data(deepsqueak_data, envir=environment())

library(readxl)
library(ggplot2)
library(gghighlight)
library(ggpubr)
library(dplyr)
library(ggeasy)
library(ggcorrplot)

globalVariables(names(deepsqueak_data))
globalVariables(c("cor", "duration_cat", "fr_cat"))

######### Hidden functions #########

#' Loads DeepSqueak Spectrogram Data
#' @param file_path The full path to the Excel file
#' @keywords internal
#' @noRd
loadSpecData <- function(data_path) {
  if (is.list(data_path)) {
    excel_file <- data_path
  } else if (is.list(data_path) == FALSE) {
    excel_file <- read_excel(data_path)
  }
  return(excel_file)
}


######### User Functions #########


#### Ethnograms ####

#' @title Plot Ethnogram
#'
#' @description Plots an ethnogram, representing when detected calls occur through the length of the recording.
#'
#' @param data_path The full path to the Excel file exported from DeepSqueak
#' @param graph_title The title for the graph (there is a default title if not specified)
#' @param graph_subtitle The subtitle for the graph (there is a default subtitle if not specified)
#'
#' @return A ggplot2 visualization of the ethnogram shown in the viewer window, which can be manually exported.
#'
#' @examples \dontrun{plotEthnogram(
#'   data_path = "inst/extdata/Example_Mouse_Data.xlsx",
#'   graph_title = "myTitle", graph_subtitle = "myDescription"
#' )}
#'
#' @import readxl
#' @import ggplot2
#' @import gghighlight
#' @import ggpubr
#' @import dplyr
#' @import ggeasy
#' @export
plotEthnogram <- function(data_path,
                          graph_title = "Call Ethnogram",
                          graph_subtitle = "Calls are indicated by a vertical line.") {
  excel_file <- loadSpecData(data_path)
  excel_file <- excel_file %>%
    select(`Begin Time (s)`)
  ggplot(data = excel_file) +
    theme_classic() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    geom_errorbar(aes(x = `Begin Time (s)`, ymin = 0, ymax = 0.2), width = 0) +
    labs(
      title = graph_title,
      subtitle = graph_subtitle,
      caption = paste0("n = ", count(excel_file)[1], " observed calls."),
      x = "Time Coordinate (s)", y = ""
    ) +
    easy_remove_y_axis() +
    theme(aspect.ratio = 1 / 18)
}



#' @title Plot Tonality-colored Ethnogram
#'
#' @description Plots a tonality ethnogram, representing tonality (clarity) of calls throughout the recording.
#'
#' @param data_path The full path to the Excel file exported from DeepSqueak
#' @param graph_title The title for the graph (there is a default title if not specified)
#' @param graph_subtitle The subtitle for the graph (there is a default subtitle if not specified)
#'
#' @return A ggplot2 visualization of the ethnogram shown in the viewer window, which can be manually exported.
#'
#' @examples \dontrun{plotEthnogramSplitByTonality(
#'   data_path = "inst/extdata/Example_Mouse_Data.xlsx",
#'   graph_title = "myTitle", graph_subtitle = "myDescription"
#' )}
#'
#' @import readxl
#' @import ggplot2
#' @import gghighlight
#' @import ggpubr
#' @import dplyr
#' @import ggeasy
#' @export
plotEthnogramSplitByTonality <- function(data_path,
                                         graph_title = "Ethnogram Split By Tonality",
                                         graph_subtitle = "Tonality: Signal/noise") {
  excel_file <- loadSpecData(data_path)
  excel_file <- excel_file %>%
    select(`Begin Time (s)`, Tonality)
  excel_file["Tonality"] <- round(excel_file["Tonality"], 1)
  ggplot(data = excel_file) +
    theme_bw() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    geom_errorbar(aes(x = `Begin Time (s)`, ymin = 0, ymax = 0.2, color = Tonality), width = 0) +
    facet_wrap(~Tonality, nrow = length(unique(excel_file$Tonality))) +
    labs(
      title = graph_title,
      subtitle = graph_subtitle,
      caption = paste0("n = ", count(excel_file)[1], " observed calls."),
      x = "Time Coordinate (s)", y = "",
      key = "Tonality"
    ) +
    easy_remove_y_axis()
}


#### Frequency Graphs ####


#' @title Plot Density Graph Stacked by Frequency
#'
#' @description Plots a density graph, grouped by frequency group collapsed into a single graph.
#'
#' @param data_path The full path to the Excel file exported from DeepSqueak
#' @param graph_title The title for the graph (there is a default title if not specified)
#' @param graph_subtitle The subtitle for the graph (there is a default subtitle if not specified)
#' @param chosen_group Specifying a particular group to be highlighted in the graph
#'
#' @return A ggplot2 visualization of the density graph shown in the viewer window, which can be manually exported.
#'
#' @examples \dontrun{plotDensityStackedByFrequency(
#'   data_path = "inst/extdata/Example_Mouse_Data.xlsx",
#'   graph_title = "myTitle", graph_subtitle = "myDescription"
#' )}
#'
#' @import readxl
#' @import ggplot2
#' @import gghighlight
#' @import ggpubr
#' @import dplyr
#' @import ggeasy
#' @export
plotDensityStackedByFrequency <- function(data_path,
                                          graph_title = "Call Distribution Grouped by Frequency Range (kHz)",
                                          graph_subtitle = "Calls are grouped by frequency ranges of 10 kHz.",
                                          chosen_group = c()) {
  excel_file <- loadSpecData(data_path)
  excel_file <- excel_file %>%
    select(`Label`, `Begin Time (s)`, `Principal Frequency (kHz)`) %>%
    mutate(fr_cat = round(`Principal Frequency (kHz)`, -1))
  ggplot(data = excel_file) +
    geom_density(aes(fill = factor(fr_cat), x = `Begin Time (s)`), alpha = 0.4) +
    labs(
      title = graph_title,
      subtitle = graph_subtitle,
      caption = paste0("n = ", count(excel_file)[1], " observed calls."),
      x = "Time Coordinate (s)",
      y = "Proportion of total calls",
      fill = "USV range (kHz)"
    ) +
    if (length(chosen_group) > 0) {
      gghighlight(fr_cat == chosen_group)
    }
}



#' @title Plot Density Graph, Splitting Groups by Frequency
#'
#' @description Plots a density graph, separating each frequency group into a separate plot in the figure.
#'
#' @param data_path The full path to the Excel file exported from DeepSqueak
#' @param graph_title The title for the graph (there is a default title if not specified)
#' @param graph_subtitle The subtitle for the graph (there is a default subtitle if not specified)
#' @param chosen_group Specifying a particular group to be highlighted in the graph
#'
#' @return A ggplot2 visualization of the density graph shown in the viewer window, which can be manually exported.
#'
#' @examples \dontrun{plotDensitySplitByFrequency(
#'   data_path = "inst/extdata/Example_Mouse_Data.xlsx",
#'   graph_title = "myTitle", graph_subtitle = "myDescription"
#' )}
#'
#' @import readxl
#' @import ggplot2
#' @import gghighlight
#' @import ggpubr
#' @import dplyr
#' @import ggeasy
#' @export
plotDensitySplitByFrequency <- function(data_path,
                                        graph_title = "Call Distribution, Split by Frequency Range (kHz)",
                                        graph_subtitle = "Calls are split by frequency ranges of 10 kHz.",
                                        chosen_group = c()) {
  excel_file <- loadSpecData(data_path)
  excel_file <- excel_file %>%
    select(`Label`, `Begin Time (s)`, `Principal Frequency (kHz)`) %>%
    mutate(fr_cat = round(`Principal Frequency (kHz)`, -1))
  ggplot(data = excel_file) +
    geom_density(aes(fill = factor(fr_cat), x = `Begin Time (s)`), alpha = 0.4) +
    facet_grid(fr_cat ~ .) +
    labs(
      title = graph_title,
      subtitle = graph_subtitle,
      caption = paste0("n = ", count(excel_file)[1], " observed calls."),
      x = "Time Coordinate (s)",
      y = "Proportion of total calls",
      fill = "USV range (kHz)"
    ) +
    if (length(chosen_group) > 0) {
      gghighlight(fr_cat == chosen_group)
    }
}


#### Custom Label Graphs ####


#' @title Plot Density Graph Stacked by Custom Labels
#'
#' @description Plots a density graph, grouped by custom labels set in DeepSqueak, collapsed into a single graph.
#'
#' @param data_path The full path to the Excel file exported from DeepSqueak
#' @param graph_title The title for the graph (there is a default title if not specified)
#' @param graph_subtitle The subtitle for the graph (there is a default subtitle if not specified)
#' @param chosen_group Specifying a particular group to be highlighted in the graph
#'
#' @return A ggplot2 visualization of the density graph shown in the viewer window, which can be manually exported.
#'
#' @examples \dontrun{plotDensityStackedByCustom(
#'   data_path = "inst/extdata/Example_Mouse_Data.xlsx",
#'   graph_title = "myTitle", graph_subtitle = "myDescription"
#' )}
#'
#' @import readxl
#' @import ggplot2
#' @import gghighlight
#' @import ggpubr
#' @import dplyr
#' @import ggeasy
#' @export
plotDensityStackedByCustom <- function(data_path,
                                       graph_title = "Call Distribution Grouped by Custom Category Labels",
                                       graph_subtitle = "Calls are grouped by custom categories designated in DeepSqueak.",
                                       chosen_group = c()) {
  excel_file <- loadSpecData(data_path)
  excel_file <- excel_file %>%
    select(`Label`, `Begin Time (s)`)
  ggplot(data = excel_file) +
    geom_density(aes(fill = factor(`Label`), x = `Begin Time (s)`), alpha = 0.4) +
    labs(
      title = graph_title,
      subtitle = graph_subtitle,
      caption = paste0("n = ", count(excel_file)[1], " observed calls."),
      x = "Time Coordinate (s)",
      y = "Proportion of total calls",
      fill = "Custom Label"
    ) +
    if (length(chosen_group) > 0) {
      gghighlight(Label == chosen_group)
    }
}


#' @title Plot Density Graph, Splitting Groups by Custom Label
#'
#' @description Plots a density graph, separating each custom label group into a separate plot in the figure.
#'
#' @param data_path The full path to the Excel file exported from DeepSqueak
#' @param graph_title The title for the graph (there is a default title if not specified)
#' @param graph_subtitle The subtitle for the graph (there is a default subtitle if not specified)
#' @param chosen_group Specifying a particular group to be highlighted in the graph
#'
#' @return A ggplot2 visualization of the density graph shown in the viewer window, which can be manually exported.
#'
#' @examples \dontrun{plotDensitySplitByCustom(
#'   data_path = "inst/extdata/Example_Mouse_Data.xlsx",
#'   graph_title = "myTitle", graph_subtitle = "myDescription"
#' )}
#'
#' @import readxl
#' @import ggplot2
#' @import gghighlight
#' @import ggpubr
#' @import dplyr
#' @import ggeasy
#' @export
plotDensitySplitByCustom <- function(data_path,
                                     graph_title = "Call Distribution, Split by Custom Category Labels",
                                     graph_subtitle = "Calls are split by custom labels designated in DeepSqueak.",
                                     chosen_group = c()) {
  excel_file <- loadSpecData(data_path)
  excel_file <- excel_file %>%
    select(`Label`, `Begin Time (s)`)
  ggplot(data = excel_file) +
    geom_density(aes(fill = factor(Label), x = `Begin Time (s)`), alpha = 0.4) +
    facet_grid(Label ~ .) +
    labs(
      title = graph_title,
      subtitle = graph_subtitle,
      caption = paste0("n = ", count(excel_file)[1], " observed calls."),
      x = "Time Coordinate (s)",
      y = "Proportion of total calls",
      fill = "Custom Label"
    ) +
    if (length(chosen_group) > 0) {
      gghighlight(Label == chosen_group)
    }
}


#### Duration Graphs ####


#' @title Plot Density Graph Stacked by Duration
#'
#' @description Plots a density graph, grouped by duration of each call (rounded to the nearest 0.01 second), collapsed into a single graph.
#'
#' @param data_path The full path to the Excel file exported from DeepSqueak
#' @param graph_title The title for the graph (there is a default title if not specified)
#' @param graph_subtitle The subtitle for the graph (there is a default subtitle if not specified)
#' @param chosen_group Specifying a particular group to be highlighted in the graph
#'
#' @return A ggplot2 visualization of the density graph shown in the viewer window, which can be manually exported.
#'
#' @examples \dontrun{plotDensityStackedByDuration(
#'   data_path = "inst/extdata/Example_Mouse_Data.xlsx",
#'   graph_title = "myTitle", graph_subtitle = "myDescription"
#' )}
#'
#' @import readxl
#' @import ggplot2
#' @import gghighlight
#' @import ggpubr
#' @import dplyr
#' @import ggeasy
#' @export
plotDensityStackedByDuration <- function(data_path,
                                         graph_title = "Call Distribution Grouped by Duration (s)",
                                         graph_subtitle = "Duration groups are rounded to the nearest 0.01 second.",
                                         chosen_group = c()) {
  excel_file <- loadSpecData(data_path)

  excel_file <- excel_file %>%
    select(`Begin Time (s)`, `Call Length (s)`) %>%
    mutate(duration_cat = round(`Call Length (s)`, 2))

  ggplot(data = excel_file) +
    geom_density(aes(fill = factor(duration_cat), x = `Begin Time (s)`), alpha = 0.4) +
    labs(
      title = graph_title,
      subtitle = graph_subtitle,
      caption = paste0("n = ", count(excel_file)[1], " observed calls."),
      x = "Time Coordinate (s)",
      y = "Proportion of total calls",
      fill = "Call Length (s)"
    ) +
    if (length(chosen_group) > 0) {
      gghighlight(duration_cat == chosen_group)
    }
}



#' @title Plot Density Graph, Splitting Groups by Duration
#'
#' @description Plots a density graph, separating each duration group into a separate plot in the figure.
#'
#' @param data_path The full path to the Excel file exported from DeepSqueak
#' @param graph_title The title for the graph (there is a default title if not specified)
#' @param graph_subtitle The subtitle for the graph (there is a default subtitle if not specified)
#' @param chosen_group Specifying a particular group to be highlighted in the graph
#'
#' @return A ggplot2 visualization of the density graph shown in the viewer window, which can be manually exported.
#'
#' @examples \dontrun{plotDensitySplitByDuration(
#'   data_path = "inst/extdata/Example_Mouse_Data.xlsx",
#'   graph_title = "myTitle", graph_subtitle = "myDescription"
#' )}
#'
#' @import readxl
#' @import ggplot2
#' @import gghighlight
#' @import ggpubr
#' @import dplyr
#' @import ggeasy
#' @export
plotDensitySplitByDuration <- function(data_path,
                                       graph_title = "Call Distribution Grouped by Duration (s)",
                                       graph_subtitle = "Duration groups are rounded to the nearest 0.01 second.",
                                       chosen_group = c()) {
  excel_file <- loadSpecData(data_path)

  excel_file <- excel_file %>%
    select(`Begin Time (s)`, `Call Length (s)`) %>%
    mutate(duration_cat = round(`Call Length (s)`, 2))
  ggplot(data = excel_file) +
    geom_density(aes(fill = factor(duration_cat), x = `Begin Time (s)`), alpha = 0.4) +
    facet_grid(duration_cat ~ .) +
    labs(
      title = graph_title,
      subtitle = graph_subtitle,
      caption = paste0("n = ", count(excel_file)[1], " observed calls."),
      x = "Time Coordinate (s)",
      y = "Proportion of total calls",
      fill = "Call Length (s)"
    ) +
    if (length(chosen_group) > 0) {
      gghighlight(duration_cat == chosen_group)
    }
}


#### Histogram Graphs ####


#' @title Plot Delta Frequency Histogram
#'
#' @description Plots a distribution of delta frequencies as a histogram.
#'
#' @param data_path The full path to the Excel file exported from DeepSqueak
#' @param graph_title The title for the graph (there is a default title if not specified)
#' @param graph_subtitle The subtitle for the graph (there is a default subtitle if not specified)
#'
#' @return A ggplot2 visualization of the histogram shown in the viewer window, which can be manually exported.
#'
#' @examples \dontrun{plotDeltaHistogram(
#'   data_path = "inst/extdata/Example_Mouse_Data.xlsx",
#'   graph_title = "myTitle", graph_subtitle = "myDescription"
#' )}
#'
#' @import readxl
#' @import ggplot2
#' @import gghighlight
#' @import ggpubr
#' @import dplyr
#' @import ggeasy
#' @export
plotDeltaHistogram <- function(data_path,
                               graph_title = "Delta Frequency-Labeled Histogram",
                               graph_subtitle = "Delta Frequency measures the kHz range of each detected call.") {
  excel_file <- loadSpecData(data_path)
  excel_file <- excel_file %>%
    select(`Delta Freq (kHz)`)
  ggplot(data = excel_file) +
    geom_histogram(
      mapping = aes(x = `Delta Freq (kHz)`),
      binwidth = 1, fill = "lightblue", color = "black"
    ) +
    theme_bw() +
    labs(
      title = graph_title,
      subtitle = graph_subtitle,
      caption = paste0("n = ", count(excel_file)[1], " observed calls."),
      x = "Distribution of Delta Frequencies (kHz)", y = "Count (n)"
    )
}


#### Box Plot Graphs ####


#' @title Plot Principal Frequency Box-Plot
#'
#' @description Plots a box-plot based on principal frequency, grouped by custom labels designated in DeepSqueak.
#'
#' @param data_path The full path to the Excel file exported from DeepSqueak
#' @param graph_title The title for the graph (there is a default title if not specified)
#' @param graph_subtitle The subtitle for the graph (there is a default subtitle if not specified)
#'
#' @return A ggplot2 visualization of the box-plot shown in the viewer window, which can be manually exported.
#'
#' @examples \dontrun{plotPrincipalBoxplot(
#'   data_path = "inst/extdata/Example_Mouse_Data.xlsx",
#'   graph_title = "myTitle", graph_subtitle = "myDescription"
#' )}
#'
#' @import readxl
#' @import ggplot2
#' @import gghighlight
#' @import ggpubr
#' @import dplyr
#' @import ggeasy
#' @export
plotPrincipalBoxplot <- function(data_path,
                                 graph_title = "Principal Frequency-Labeled Box-Plot",
                                 graph_subtitle = "Main frequencies where calls labeled in DeepSqueak predominate.") {
  excel_file <- loadSpecData(data_path)
  excel_file <- excel_file %>%
    select(Label, `Principal Frequency (kHz)`)
  ggplot(data = excel_file) +
    theme_bw() +
    geom_boxplot(mapping = aes(y = `Principal Frequency (kHz)`, fill = Label), color = "black") +
    labs(
      title = graph_title,
      subtitle = graph_subtitle,
      caption = paste0("n = ", count(excel_file)[1], " observed calls."),
      y = "Principal Frequency (kHz)"
    ) +
    easy_remove_x_axis()
}


#### Correlation Graphs ####


#' @title Plot Correlation Matrix
#'
#' @description Plots a box-plot based on principal frequency, grouped by custom labels designated in DeepSqueak.
#'
#' @param data_path The full path to the Excel file exported from DeepSqueak
#' @param graph_title The title for the graph (there is a default title if not specified)
#' @param graph_subtitle The subtitle for the graph (there is a default subtitle if not specified)
#'
#' @return A ggplot2 visualization of the box-plot shown in the viewer window, which can be manually exported.
#'
#' @examples \dontrun{plotCorrelations(
#'   data_path = "inst/extdata/Example_Mouse_Data.xlsx",
#'   graph_title = "myTitle", graph_subtitle = "myDescription"
#' )}
#'
#' @import readxl
#' @import ggplot2
#' @import gghighlight
#' @import ggpubr
#' @import dplyr
#' @import ggeasy
#' @importFrom ggcorrplot ggcorrplot
#' @importFrom stats cor
#' @export
plotCorrelations <- function(data_path,
                             graph_title = "Correlation Matrix",
                             graph_subtitle = "Correlation coefficients labeled.") {
  excel_file <- loadSpecData(data_path) %>%
    select(
      `Call Length (s)`, `Principal Frequency (kHz)`, `Delta Freq (kHz)`,
      `Slope (kHz/s)`, `Sinuosity`, `Mean Power (dB/Hz)`, `Tonality`, `Peak Freq (kHz)`
    )

  sqkr_correlation_matrix <- round(cor(excel_file), 1)
  ggcorrplot(sqkr_correlation_matrix,
    hc.order = TRUE, outline.color = "white",
    type = "lower", ggtheme = ggplot2::theme_gray, lab = TRUE
  ) +
    labs(
      title = graph_title,
      subtitle = graph_subtitle,
      caption = paste0("n = ", count(excel_file)[1], " observed calls.")
    )
}


#' @title 3D Call Clusters (Custom Label) Plot
#'
#' @description Plots call clusters within a data-point in a 3D Plotly graph, with
#' principal frequency on the x-axis, mean power on the y-axis, and call length on
#' the z-axis.
#'
#' @param data_path The path to the raw data
#'
#' @return 3D Plotly plot
#'
#' @examples \dontrun{plotClusters(experiment = experiment, data_id = 1)}
#'
#' @importFrom plotly plot_ly add_markers add_lines layout
#' @import RColorBrewer
#' @export
plotClusters <- function(data_path) {
  nb.cols <- length(unique(data_path$Label))

  plot_ly(data_path,
          x = ~`Principal Frequency (kHz)`,
          y = ~`Mean Power (dB/Hz)`,
          z = ~`Call Length (s)`,
          color = ~`Label`, colors = colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)) %>%
    add_markers() %>%
    add_lines(showlegend = FALSE) %>%
    layout(
      scene = list(
        xaxis = list(title = "Principal Frequency (kHz)"),
        yaxis = list(title = "Mean Power (dB/Hz)"),
        zaxis = list(title = "Call Length (s)")
      )
    )
}


