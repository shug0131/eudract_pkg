#' Example of safety data
#'
#' A dataset containing some example data of safety event in raw source format
#'
#' @format a data frame with 8 columns and 16 rows
#' \describe{
#'    \item{pt}{meddra preferred term code}
#'    \item{subjid}{a unique subject identifier}
#'    \item{related}{a logical indicating if the event is related to the treatment}
#'    \item{soc}{the meddra code for the System Organ Class}
#'    \item{fatal}{a numerical 0/1 to indicate if the event was fatal}
#'    \item{serious}{a numerical 0/1 to indicate if the event was serious}
#'    \item{group}{the treatment group for the subject}
#'    \item{term}{a text description of the event. Needs to be matching 1-1 with the pt code}
#' }
#'
#' @details The data contains one row per patient-event. So the numbers exposed in each arm cannot be inferred from these data,
#' as patients with no events will not be included in these data.
#'
#' The variable names and formats are those required by \code{\link{safety_summary}}. The variable \code{pt} is not strictly required.
#' An alternative to \code{soc} would be the equivalent character string from \code{\link{soc_code}}
#'
#'
#'
#'
#'
"safety"
