#' DeepSqueak Example Data
#'
#' Data exported from DeepSqueak after processing `Example Mouse Recording.flac`
#'
#' @docType data
#'
#' @usage data(deepsqueak_data)
#'
#' @format A \code{"data.frame"} with 714 rows and 17 columns. The definitions of
#' these parameters are sourced from the link in the references section.
#' \describe{
#'     \item{ID}{The number of the call}
#'     \item{Label}{The label extracted from DeepSqueak}
#'     \item{Accepted}{Whether the call was accepted (as a call)}
#'     \item{Score}{The call score, from DeepSqueak}
#'     \item{Begin Time (s)}{The time when the call started}
#'     \item{End Time (s)}{The time when the call ended}
#'     \item{Call Length (s)}{The length of the call}
#'     \item{Principal Frequency (kHz)}{The median frequency of the call}
#'     \item{Low Freq (kHz)}{The minimum frequency of the call}
#'     \item{High Freq (kHz)}{The maximum frequency of the call}
#'     \item{Delta Freq (kHz)}{The difference between the maximum and minimum call frequencies}
#'     \item{Frequency Standard Deviation (kHz)}{The standard deviation of the call}
#'     \item{Slope (kHz/s)}{The slope of the call}
#'     \item{Sinuosity}{The call contour path length divided by the distance between start and end of the call}
#'     \item{Mean Power (dB/Hz)}{}
#'     \item{Tonality}{The signal/noise ratio}
#'     \item{Peak Freq}{The frequency at the highest power}
#' }
#'
#' @keywords datasets
#'
#' @references (\href{https://github.com/DrCoffey/DeepSqueak/wiki/export-to-excel}{DeepSqueak Wiki: Export to Excel Page})
#'
#' @source (\href{https://github.com/DrCoffey/DeepSqueak}{DeepSqueak GitHub})
#'
#' @examples
#' data(deepsqueak_data)
"deepsqueak_data"
