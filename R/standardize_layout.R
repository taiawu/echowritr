#' Standardize plate layouts
#'
#'
#'
#'
#' @param layout A plate layout, containing columns with compound, concentration, and, for daughter plates only, volume.
#' @param which_plate The type of layout to standardize. Can be "mother" or "daughter".
#' @param .well_col,.compound_col,.concentration_col,.volume_col Names of the columns in the input layout containg necessary information.
#'  \itemize{
#'  \item{.well_col: The well names.}
#'  \item{.compound_col: The compounds to be transferred. Compounds must go by the same name in mother and daughter plates.}
#'  \item{.concentration_col: The concentration of each compound, either in the mother plate, or the desired concentration in the daughter plate.}
#'  \item{.volume_vol: For daughter plates only. The desired volume in nL in each well of the daughter. }
#' }
#'
#' @return A plate layout, containing standardized columns and column names for use in downstream echowritr functions.
#'
#' @importFrom magrittr "%>%"
#' @importFrom glue glue glue_collapse
#' @importFrom dplyr filter across select mutate
#' @importFrom tidyselect all_of everything any_of
#' @importFrom purrr set_names
#' @importFrom tidyr drop_na
#'
#'
#' @export
standardize_layout <- function(layout,
                        which_plate,
                        .well_col = "well",
                        .compound_col = "compound",
                        .concentration_col = "concentration",
                        .volume_col = "volume") {

  # ensure valid plate type selection, or return error

  if (!which_plate %in% c("mother", "daughter")) {
    abort_bad_argument("which_plate", must = "be either 'daughter' or 'mother", not = which_plate)}

  # update the column names and columns to be selected according to plate selected, or return error
  col_names <- switch(which_plate, "daughter" = c("Destination Well", "compound", "daughter_conc", "daughter_final_vol"),
                      "mother" = c("Source Well", "compound", "mother_conc"))

  which_cols <- switch(which_plate,
                       "daughter" = c({{ .well_col }}, {{ .compound_col }}, {{ .concentration_col }}, {{ .volume_col }}),
                       "mother" = c({{ .well_col }}, {{ .compound_col }}, {{ .concentration_col }}))

  numeric_cols <- switch(which_plate,
                         "daughter" = c({{ .concentration_col }}, {{ .volume_col }}),
                         "mother" = c({{ .compound_col }}))

  if (!.well_col %in% names(layout)) {
    abort_bad_argument(".well_col", must = glue::glue("be in {which_plate} layout names. Looked for {.well_col}, but layout names are {glue_collapse(names(layout),  sep = ', ')}"),
                       not = NULL)
  }

  # condition the layout
  layout %>%
    filter(is.na({{ .compound_col }}) == FALSE) %>%
    select(all_of(which_cols)) %>%
    mutate(across(.cols = everything(), as.character),
           across(any_of(c("concentration", "volume")), as.numeric)) %>%
    set_names(col_names) %>%
    drop_na() ## if anything became NA when mutated to numeric--but this is something that folks should probably be alerted to.
}
