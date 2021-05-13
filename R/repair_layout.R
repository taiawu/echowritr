#' Repair impossible transfer requests
#'
#' Repairs mother and daughter layouts resulting impossible transfers. Allows users to choose options to minimally modify layouts to remove impossible transfers.
#'
#' @param mother A standardized mother layout.
#' @param daughter A standardized daughter layout.
#' @param if_missing How to resolve cases where a compound is present in the daughter, but not the mother.
#'  \itemize{
#'  \item{"stop": Halt execution, and return an informative error message.}
#'  \item{"drop": Remove these compounds from the daughter layout.}
#' }
#' @param if_varied How to resolve cases where a single compound is present in the mother at multiple concentrations.
#'  \itemize{
#'  \item{"stop": Halt execution, and return an informative error message.}
#'  \item{"keep_max": Transfer only from wells containing the highest concentration of this compound.}
#'  \item{"keep_most": Transfer only from wells containing the most abundant concentration of this compound.}
#' }
#' @param if_impossible How to resolve cases where a compound in the daughter at a concentration exceeding its concentration in the mother.
#'  \itemize{
#'  \item{"stop": Halt execution, and return an informative error message.}
#'  \item{"drop": Remove these concentrations from the daughter layout.}
#'  \item{"make_max": Replace these concentrations with the highest concentration achievable with the given mother plate.}
#'  \item{"scale_down": Replace all concentartions of offending compounds with a scaled-down concentration, such that the relative concentrations of this compound are retained, and all requested concentrations are equal to or less than the concentration of this compound in the mother plate. }
#' }
#'
#' @return A list
#' \itemize{
#' \item{mother: a mother plate, with any issues repaired as specified}
#' \item{daughter: a daughter plate, with any issues repaired as specified}}
#'
#' @importFrom dplyr distinct group_by group_modify if_else left_join n_distinct pull ungroup
#' @importFrom utils globalVariables
#' @importFrom rlang .data
#'
#' @export
repair_layout <- function(mother, daughter,
if_missing = "stop",
if_varied = "stop",
if_impossible  = "stop") {

  mother_rep <- repair_varied(mother, if_varied = if_varied) # will throw an error if "stop" and repair needed

  daughter_rep <- daughter %>%
    repair_missing(mother, ., if_missing = if_missing) %>% # will throw an error if "stop" and repair needed
    repair_conc(mother, ., if_impossible = if_impossible) # will throw an error if "stop" and repair needed

  list("mother" = mother_rep,
       "daughter" = daughter_rep)
}

#### ---- Repair compounds missing from the mother plate
repair_missing <-function(mother, daughter, if_missing = "stop") { ## used to be  handle_missing_compounds()

  if (!if_missing %in% c("stop", "drop")) { abort_bad_argument("if_missing", must = glue::glue("be 'stop' or 'drop'"),
                                                               not = if_missing ) }


  daughter_cmpds <- daughter$compound
  mother_cmpds <- mother$compound

  if (all(daughter_cmpds %in% mother_cmpds) == FALSE) {
    missing <- daughter_cmpds[!daughter_cmpds %in% mother_cmpds]

    if (if_missing == "drop") { # drop the missing compounds from the daughter
      daughter <- daughter %>% filter(.data$compound %in% mother_cmpds)

      # option 2 (default): throw an error
    } else if (if_missing == "stop") { # or throw an error

      missing_cmpd <- daughter_cmpds[!daughter_cmpds %in% mother_cmpds]

      abort_bad_argument("Mother layout", must = glue::glue("contain all compounds in the daughter layout. {glue_collapse(unique(missing_cmpd), sep = ',')} found in daughter layout, but not mother layout. To drop {glue_collapse(length(missing_cmpd))} well(s) containing missing compound(s) from the daughter, set if_missing = 'drop'"),
                         not = NULL )

    }
  }
  daughter
}

# helper for repair_conc (1 of 2)
make_max <- function(by_compound) {
  by_compound %>%
    mutate(daughter_conc = if_else(.data$daughter_conc > .data$mother_conc, .data$mother_conc, .data$daughter_conc))
}

# helper for repair_conc (2 of 2)
scale_down <- function(by_compound) {
  by_compound %>%
    group_modify(~ {
      .x %>%
        mutate(daughter_conc = .data$daughter_conc*(.data$mother_conc/max(.data$daughter_conc)))
    })
}

#### ---- Repair daughter concentrations exceeding mother concentrations
repair_conc <- function(mother, daughter, if_impossible = "stop") {

  if (!if_impossible %in% c("stop", "drop", "make_max", "scale_down")) { abort_bad_argument("if_impossible", must = glue::glue("be 'stop', 'drop', 'make_max' or 'scale_down'"),
                                                                                            not = if_impossible ) }

  # identify compounds needing repair
  df <- left_join(daughter, mother, by = "compound") %>%
    select(.data$compound, .data$mother_conc, .data$daughter_conc) %>%
    distinct() %>%
    group_by(.data$compound) %>%
    mutate(repair = if_else(.data$daughter_conc > .data$mother_conc, true = "repair", false = "ok"))

  out <- switch(if_impossible,
                "stop" =  if (!all(df$repair == "ok")) {
                  abort_bad_argument("Each compound", must = "be be present in the daughter layout at or below its concentration in the mother. ", not = NULL)}, # throw an error if repairs are needed
                "drop" = df %>% filter(.data$repair == "ok"),
                "make_max" = df %>% make_max(),
                "scale_down" = df %>% scale_down())
  out %>%
    ungroup() %>%
    select(-.data$repair)
}

#### ---- Repair compounds present in the mother plate at multiple concentrations
repair_varied <- function(mother, if_varied = "keep_max") {

  if (!if_varied %in% c("keep_max", "keep_most", "stop")) { abort_bad_argument("if_varied", must = glue::glue("be 'stop', 'keep_max' or 'keep_most'"),
                                                                               not = if_varied ) }

  tallied <- mother %>%
    group_by(.data$compound) %>% # for each compound
    mutate(n_conc = n_distinct(.data$mother_conc), # how many conc. in mother
           max_conc = max(.data$mother_conc)) %>% # which conc is highest
    group_by(.data$compound, .data$mother_conc) %>%
    mutate(well_per_conc = n_distinct(.data$`Source Well`), # how many wells per conc
           most_wells = max(.data$well_per_conc)[[1]]) %>% # which conc has the most wells
    ungroup()

  if(max(tallied$n_conc) > 1) {

    if (if_varied == "stop") {
      multi_conc <- tallied %>% filter(.data$n_conc > 1) %>% pull(.data$compound) %>% unique()
      abort(message = glue::glue("Different concentrations of the same compound in the mother plate are not supported. Compound(s) `{glue_collapse(multi_conc)}` present in multiple concentrations in the mother plate. To resolve this, direct how a single concentration is chosen for each compound by setting `if_varied` to 'keep_max' or 'keep_most'. "))
    } else {
      most_wells_conc <- tallied %>%
        filter(.data$well_per_conc == .data$most_wells) %>%
        pull(.data$mother_conc) %>%
        unique()

      out <- switch(if_varied,
                    "keep_max" = tallied %>% filter(.data$mother_conc == .data$max_conc), # keep the wells with the highest conc
                    "keep_most" = tallied %>% filter(.data$well_per_conc == .data$most_wells,
                                                     .data$mother_conc == most_wells_conc)) # keep the concentrations with the most wells

      # remove the columns created by these operations
      out <-out %>% select(-c(.data$n_conc, .data$max_conc, .data$well_per_conc, .data$most_wells ))
    }
  } else {
    out <- mother
  }
  out
}

utils::globalVariables(c("."))
