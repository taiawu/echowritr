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


#' Make experiment header
#'
#' Not sure what package this really belong in in the end, but its useful here, so adding it here for now.
#'
#' @param expnum the experiment number. Defaults to "0000"
#'
#' @return a string fo the standard experiment number prefix format: "Exp<expnum>--YYYYMMDD_"
#'
#' @export
make_exp_header <- function(expnum = "0000") {
  paste0("Exp",
         expnum,
         "--",
         Sys.Date() %>% gsub("-", "", .),
         "_")

}
