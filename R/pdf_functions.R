data(deepsqueak, envir=environment())

#' @title Export PDF
#'
#' @description Plots a density graph, separating each frequency group into a separate plot in the figure.
#'
#' @param data_path The full path to the Excel file exported from DeepSqueak
#' @param save_path The full path to the directory where the PDF will be saved (default is the current working directory)
#' @param experimenter The name of the experimenter for naming the save file (default is "Anon")
#'
#' @return A PDF containing a series of relevant ethnograms and density graphs assessing the recording.
#'
#' @examples plotSummaryPDF(
#'   data_path = "inst/extdata/Example_Mouse_Data.xlsx",
#'   save_path = tempdir(), experimenter = "experimenter_name"
#' )
#'
#' @import readxl
#' @import ggplot2
#' @import gghighlight
#' @import ggpubr
#' @import dplyr
#' @import ggeasy
#' @import grDevices
#' @export
plotSummaryPDF <- function(data_path,
                           save_path = tempdir(),
                           experimenter = "Anon") {
  message("Loading DeepSqueak Excel file...")
  excel_file <- loadSpecData(data_path)
  message("Writing PDF file...")
  pdf(file = paste0(save_path, "/", experimenter, "_Data_Summary_", as.character(Sys.Date()), ".pdf"))

  message("Adding charts to PDF...")

  plot(ggarrange(plotEthnogram(data_path),
    ncol = 1, nrow = 1
  ))
  plot(ggarrange(plotEthnogramSplitByTonality(data_path),
    ncol = 1, nrow = 1
  ))
  plot(ggarrange(plotDensityStackedByFrequency(data_path),
    plotDensityStackedByCustom(data_path),
    ncol = 1, nrow = 2
  ))
  plot(ggarrange(plotDensityStackedByDuration(data_path),
    ncol = 1, nrow = 1
  ))
  plot(ggarrange(plotDensitySplitByFrequency(data_path),
    ncol = 1, nrow = 1
  ))
  plot(ggarrange(plotDensitySplitByCustom(data_path),
    ncol = 1, nrow = 1
  ))
  plot(ggarrange(plotDensitySplitByDuration(data_path),
    ncol = 1, nrow = 1
  ))
  plot(ggarrange(plotPrincipalBoxplot(data_path),
    plotDeltaHistogram(data_path),
    ncol = 1, nrow = 2
  ))
  plot(ggarrange(plotCorrelations(data_path),
    ncol = 1, nrow = 1
  ))
  dev.off()
  message("Saving PDF...")
}
