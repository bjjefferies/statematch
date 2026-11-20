#' Convert state abbreviations to full state names
#'
#' The reverse of [state_abb()]: a strict, case-insensitive lookup from postal
#' abbreviation to full state name. No fuzzy matching is done, because a
#' two-letter code is either valid or it is not. Leading/trailing whitespace is
#' ignored. Unknown codes and `NA` return `NA`.
#'
#' @param abbr A character vector (or factor) of postal abbreviations.
#' @param territories Logical. Also recognise PR, GU, VI, AS and MP?
#'   Default `FALSE`.
#' @return A character vector the same length as `abbr`.
#' @examples
#' abbr2state(c("AZ", "tx", " dc ", "ZZ", NA))
#' abbr2state("PR", territories = TRUE)
#' @export
abbr2state <- function(abbr, territories = FALSE) {
  tbl <- if (isTRUE(territories)) rbind(.states, .territories) else .states
  tbl$name[match(toupper(trimws(as.character(abbr))), tbl$abb)]
}
