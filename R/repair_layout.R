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
#' @importFrom dplyr distinct group_by group_modify if_else left_join n_distinct pull ungroup filter
#' @importFrom utils globalVariables
#' @importFrom rlang .data
#' @importFrom glue glue glue_collapse
#'
#' @export
repair_layout <- #_______(primary function) Repair design errors in layouts_______#
  function(mother, daughter,
           if_missing = "stop",    # stop gives error for shinyAlert()
           if_varied = "stop",     # stop gives error for shinyAlert()
           if_impossible  = "stop" # stop gives error for shinyAlert()
  ) {

    mother_rep <- #__Remove unsupported cases: multile conc. for single compounds in mother__
      repair_varied(mother, if_varied = if_varied)

    daughter_rep <- #__Remove impossible cases: missing compounds and conc. in excess of mother__
      daughter %>%
      repair_missing(mother_rep, ., if_missing = if_missing) %>% # especially useful for typos
      repair_conc(mother_rep, ., if_impossible = if_impossible) # common mistake

    list("mother" = mother_rep,
         "daughter" = daughter_rep)
  }

repair_missing <- #_______(helper function) Remove orphan compounds from daughter_______#
  function(mother, daughter, if_missing = "stop") { ## used to be  handle_missing_compounds()

    #_____Catch argument errors____
    if (!if_missing %in% c("stop", "drop")) { # catch typos etc.
      abort_bad_argument("if_missing",
                         must = glue::glue("be 'stop' or 'drop'"),
                         not = if_missing )
    }

    #_____Repair missing compounds____
    daughter_cmpds <- daughter$compound
    mother_cmpds <- mother$compound

    if (all(daughter_cmpds %in% mother_cmpds) == FALSE) {
      missing_cmpd <- # prints in errors and warnings to ease manual repair
        daughter_cmpds[!daughter_cmpds %in% mother_cmpds]

      if (if_missing == "drop") {
        #____Alert users of the one-shot function____
        warning(glue::glue("Warning!
          {glue::glue_collapse(length(unique(missing_cmpd)), sep = ', ')} compounds in daughter not present in the mother.
          The following compounds were missing, and have been dropped from the daughter:
          {glue::glue_collapse(unique(missing_cmpd), sep = ', ')}")
        )

        daughter <- # only supported repair is to remove
          daughter %>%
          filter(.data$compound %in% mother_cmpds)

      } else if (if_missing == "stop") {
        abort_bad_argument("Mother layout",
                           must = glue::glue("contain all compounds in the daughter layout.
                                    {glue_collapse(unique(missing_cmpd), sep = ',')}
                                    found in daughter layout, but not mother layout.
                                    To drop {glue_collapse(length(missing_cmpd))}
                                    well(s) containing missing compound(s) from the
                                    daughter, set if_missing = 'drop'"),
                           not = NULL )
      }
    }
    daughter # return unchanged if no mismatched
  }


make_max <- #_______(helper function - repair_conc) Overwrites to max conc_______#
  function(by_compound) {
    by_compound %>%
      mutate(daughter_conc = if_else(.data$daughter_conc > .data$mother_conc, # impossible ask
                                     true = .data$mother_conc, # closest possible conc.
                                     false = .data$daughter_conc))
  }

scale_down <-  #_______(helper function - repair_conc) Scales all concentrations down to preserve DRCs_______#
  function(by_compound) {
    by_compound %>%
      group_modify(~ {
        .x %>%
          mutate(daughter_conc = .data$daughter_conc*(.data$mother_conc/max(.data$daughter_conc)))
      })
  }


repair_conc <- #_______(helper function - repair_layout) Repairs unacievably high daughter concentrations_______#
  function(mother, daughter, if_impossible = "stop") {
    #_____Catch argument errors____
    if (!if_impossible %in% c("stop", "drop", "make_max", "scale_down")) {
      abort_bad_argument("if_impossible",
                         must = glue::glue("be 'stop', 'drop', 'make_max' or 'scale_down'"),
                         not = if_impossible )
    }

    #_____Identify compounds needing repair____
    df <- left_join(daughter, mother, by = "compound") %>%
      select(.data$`Destination Well`, .data$compound, .data$mother_conc, .data$daughter_conc, .data$daughter_final_vol) %>%
      distinct() %>%
      group_by(.data$compound) %>%
      mutate(repair = if_else(.data$daughter_conc > .data$mother_conc, true = "repair", false = "ok"))

    if ("repair" %in% df$repair) {
      to_repair <- # wells / compounds / concentrations to repair
        df %>%
        filter(.data$repair == "repair")

      #____Alert users of the one-shot function____
      warning(glue::glue("Warning!
          {glue::glue_collapse(length(unique(to_repair$`Destination Well`)))} well(s) in daughter plate are at a concentration in excess of the mother concentration for that compound.
          These wells are: {glue::glue_collapse(unique(to_repair$`Destination Well`), sep = ', ')}
          Containing compunds: {glue::glue_collapse(unique(to_repair$compound), sep = ', ')}
          These issues were repaired with the method: {if_impossible}, specified by the `if_impossible` argument."))

      #_____User specifies repair method____
      out <- switch(if_impossible,
                    "stop" =  if (!all(df$repair == "ok")) { # interfaces with shinyAlerts() to prompt user decision
                      abort_bad_argument("Each compound",
                                         must = "be be present in the daughter layout
                                       at or below its concentration in the mother. ",
                                         not = NULL)}, # throw an error if repairs are needed
                    "drop" = df %>% filter(.data$repair == "ok"),
                    "make_max" = df %>% make_max(),
                    "scale_down" = df %>% scale_down())

      #_____Match input format____
      out <- out %>%
        ungroup() %>%
        select(-c(.data$repair, .data$mother_conc)) %>% # temporary cols created for this function only
        select(c(.data$`Destination Well`, .data$compound, .data$daughter_conc, .data$daughter_final_vol)) # the expected column order, visually
    } else {
      out <- daughter
    }
    out
  }

repair_varied <- #_______(helper function - repair_layout) Repairs unacievably high daughter concentrations_______#
  function(mother, if_varied = "keep_max") {
    #_____Catch argument errors____
    if (!if_varied %in% c("keep_max", "keep_min", "keep_most", "stop")) {
      abort_bad_argument("if_varied",
                         must = glue::glue("be 'stop', 'keep_max' or 'keep_most'"),
                         not = if_varied ) }

    #_____Identify compounds with multiple concentrations____
    tallied_by_cmpd <- mother %>%
      group_by(.data$compound) %>%
      mutate(n_conc = n_distinct(.data$mother_conc), # how many conc. in mother
             max_conc = max(.data$mother_conc),
             min_conc = min(.data$mother_conc)) %>%
      group_by(.data$compound, .data$mother_conc) %>% # needed if keep_most selected
      mutate(well_per_conc = n_distinct(.data$`Source Well`)) %>%
      group_by(.data$compound) %>%
      mutate(most_wells = max(.data$well_per_conc))

    #_____User specifies repair method____
    if(max(tallied_by_cmpd$n_conc) > 1) {
      to_repair <- # for more helpful errors and warnings
        tallied_by_cmpd %>%
        filter(.data$n_conc > 1)

      #____Alert users of the one-shot function____
      warning(glue::glue("Warning!
          {glue::glue_collapse(length(unique(to_repair$compound)))} compound(s) present in multiple concentrations in the mother plate.
          These compounds are: {glue::glue_collapse(unique(to_repair$compound), sep = ', ')}
          These issues were repaired with the method: {if_varied}, specified by the `if_varied` argument."))

      if (if_varied == "stop") { # interfaces with shinyAlerts() to prompt user decision
        multi_conc <- # prints in error to ease manual repair
          tallied_by_cmpd %>%
          filter(.data$n_conc > 1) %>%
          pull(.data$compound) %>%
          unique()

        rlang::abort(message = glue::glue("Different concentrations of the same compound
                                          in the mother plate are not supported.
                                          Compound(s) `{glue_collapse(multi_conc)}`
                                          present in multiple concentrations in the mother plate.
                                          To resolve this, direct how a single concentration
                                          is chosen for each compound by setting
                                          `if_varied` to 'keep_max' or 'keep_most'. "))
      } else {
        out <-
          switch(if_varied,
                 "keep_max" = tallied_by_cmpd %>% filter(.data$mother_conc == .data$max_conc),
                 "keep_min" = tallied_by_cmpd %>% filter(.data$mother_conc == .data$min_conc),
                 "keep_most" = tallied_by_cmpd %>%
                   filter(.data$well_per_conc == .data$most_wells) %>%
                   mutate(max_conc = max(.data$mother_conc)) %>% # if >1 conc. meets criteria
                   filter(.data$mother_conc == .data$max_conc)
          )

        out <- # remove the columns created by these operations
          out %>% select(-c(.data$n_conc, .data$max_conc, .data$min_conc, .data$well_per_conc, .data$most_wells ))
      }
    } else { # don't mess with error-free mothers
      out <- mother
    }
    out %>% ungroup() # downstream expects ungrouped
  }

utils::globalVariables(c("."))
