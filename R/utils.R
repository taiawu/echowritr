#' Utilities
#'
#' abort_bad_argument taken from Advanced R! Written by Hadley Wichkham.
#' @param arg the argument
#' @param must allowed values of argument
#' @param not the bare argument
#'
#' @return utilities
#'
#' @importFrom glue glue
#' @importFrom rlang abort
#'
#' @export
#'

abort_bad_argument <- function(arg, must, not = NULL) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    msg <- glue::glue("{msg}; not `{not}`.")
  }

  rlang::abort("error_bad_argument",
               message = msg,
               arg = arg,
               must = must,
               not = not
  )
}
