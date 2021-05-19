#' Summarise the rounding resulting from the transfer calculations
#'
#' Wil likely get built out further for a more comprehensive changes report.
#'
#' @param transfers the transfers tibble
#' @param return_filtered if TRUE, returns a tibble containins only rows corresponding to rounded values. If FALSE, no filtering is performed.
#'
#' @return the transfers tibbl, with only the columns relevant to rounding. These columns are renamed so that users can guess at their meaning. Verbose but hopefully clearer.
#'
#' @importFrom dplyr select
#' @importFrom purrr set_names
#' @export
summarise_rounding <- function(transfers, return_filtered = FALSE) {
  rounded <-  transfers %>%
    #filter(.data$rounded_up != 0) %>%
    select(c(.data$`Destination Well`, .data$compound, .data$daughter_conc, .data$final_conc, .data$rounded_up, .data$rounded_up_perc )) %>%
    set_names(c("Destination Well", "compound", "original_daughter_conc", "rounded_daugher_conc", "rounded_up_by", "percent_conc_increase_by_rounding"))

  if (return_filtered){
    out <- rounded %>%
      filter(.data$rounded_up != 0)
  } else {
    out <- rounded
  }

  out
}
