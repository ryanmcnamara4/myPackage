#' Load all excel sheets
#'
#' Loads all of the sheets of the excel file \code{filepath}, and returns them as
#' a list of tibbles or data.frames.
#'
#' @param filepath Path to an excel file.
#' @param cnames Logical value determining if column names should be included.
#'     Defaults to TRUE.
#' @param tibble Logical value determining if the data should be loaded as tibbles
#'     or data.frames. Defaults to FALSE.
#'
#' @return List of tibbles or data.frames, each corresponding to one sheet.
#' @export
#'
#' @examples
#' file = system.file('extdata', 'human_viruses.xls', package = 'myPackage')
#' read_excel_allsheets(file, cnames = TRUE, tibble = TRUE)
#' read_excel_allsheets(file)
read_excel_allsheets = function(filepath, cnames = TRUE, tibble = FALSE) {

  sheets = readxl::excel_sheets(filepath)
  data = lapply(sheets, function(x) readxl::read_excel(filepath, sheet = x, col_names = cnames))

  if (!tibble) data = lapply(data, as.data.frame)

  names(data) = sheets

  return(data)

}
