#' Calculate transfer steps from layouts
#'
#' @param daughter A daughter layout, standardized and repaired
#' @param mother A mother layout, standardized and repaired
#' @param .echo_drop_nL The transfer volume of the Echo instrument to be used, in nL. Defaults to 25.
#' @param .dilutant_name The name of the compound in the mother to be used for dilutions. Defaults to "DMSO".
#'
#' @return A tibble containing all transfer steps, and final conditions representing any necessary rounding
#' @importFrom plyr round_any
#' @importFrom dplyr bind_rows rename row_number
#' @importFrom tidyr nest unnest
#' @importFrom purrr map_dbl
#' @importFrom utils globalVariables
#' @export
calculate_transfers <- #_______(primary function) Write all transfer steps from layouts, rounding where necessary_______#
  function(daughter, mother, .echo_drop_nL = 25, .dilutant_name = "DMSO") {

    compound_transfers <-  #__compound transfers__
      concentrations_to_transfers(daughter, mother, .echo_drop_nL) %>% # rounds where necessary based on .echo_drop_nL
      mutate(dilution_vol = .data$daughter_final_vol - .data$mother_vol) %>%
      distribute_shared(.echo_drop_nL)

    dilution_transfers <- #__calc dilution transfers separately, to respect necessary rounding and desired final volume__
      make_dilutions_plate(compound_transfers, mother, .echo_drop_nL = .echo_drop_nL, .dilutant_name = .dilutant_name)


    transfers <-  #__All transfer steps and final conditions__#
      bind_rows(compound_transfers, dilution_transfers) %>%
      mutate(across(where(is.numeric)), round(2)) # for readability
  }

concentrations_to_transfers <- #_______(helper function) Convert concentrations to transfer steps, rounding where necessary_______#
  function(daughter, mother, .echo_drop_nL = 25) { # if there are compounds in the daughter not in the mother
    #_____Catch argument errors____
    if (!is.numeric(.echo_drop_nL)) {
      abort_bad_argument(".echo_drop_nL",
                         must = "be numeric",
                         not = typeof(.echo_drop_nL)) }

    by_compound_m <-  #__INTERMEDIATE: mother layout, nested by compound__
      mother %>%
      group_by(.data$compound, .data$mother_conc) %>%
      nest()


    transfers <-  #__OUTPUT:  all calculated transfer steps__#
      daughter %>%
      left_join( . , by_compound_m, by = "compound")  %>%
      mutate(mother_dil = (.data$daughter_conc/.data$mother_conc) * ( .data$daughter_final_vol),
             mother_vol = round_any(.data$mother_dil, .echo_drop_nL, ceiling),
             final_conc = (.data$mother_conc*.data$mother_vol)/.data$daughter_final_vol,
             rounded_up = .data$final_conc - .data$daughter_conc,
             rounded_up_perc = if_else(.data$daughter_conc == 0, true = 0, false = round(100*.data$rounded_up/.data$daughter_conc, 1))) %>%
      filter(is.na(.data$mother_vol) == FALSE)
  }

make_dilutions_plate <-  #_______(helper function) Calculate dilution transfers separately to avoid rounding errors_______#
  function(transfers, mother, .echo_drop_nL = 25, .dilutant_name = "DMSO") {
    #_____Catch argument errors____
    if (!.dilutant_name %in% mother$compound) {
      abort_bad_argument(".dilutant_name",
                         must = "be a compound in the mother plate. {.dilutant_name} not found in mother.",
                         not = NULL) }
    # .echo_drop_nL used only in concentrations_to_transfer, errors caught there

    dil_mother <-  #__INTERMEDIATE: mother, with dilutant only__
      mother %>%
      filter(.data$compound == .dilutant_name) %>%
      mutate(mother_conc = 1) # just needs to match daughter for concentrations_to_transfers

    dil_daughter <- #__INTERMEDIATE: daughter, with dilutions only__
      transfers %>%
      filter(.data$dilution_vol > 0) %>%
      mutate(daughter_final_vol = .data$dilution_vol) %>%
      select(.data$`Destination Well`, .data$daughter_final_vol, ) %>%
      mutate(compound = .dilutant_name,
             daughter_conc = 1) %>%  # just needs to match mother for concentrations_to_transfers
      distinct() # remove duplicates for each transfer

    dil_transfer <- #__OUTPUT: dilution transfers__#
      concentrations_to_transfers(dil_daughter, dil_mother, .echo_drop_nL) %>%
      distribute_shared(.echo_drop_nL) %>%
      ungroup()
  }

distribute_shared <- #_______(helper function) Distribute transfers over common source wells_______#
  function(transfers, .echo_drop_nL = 25) {
    #_____Catch argument errors____
    if (!is.numeric(.echo_drop_nL)) {
      abort_bad_argument(".echo_drop_nL",
                         must = "be numeric",
                         not = typeof(.echo_drop_nL)) }

    distributed <- #__OUTPUT: transfers, distributed over common source wells__#
      transfers %>%
      mutate(n_wells = map_dbl(.data$data, nrow)) %>% #  the number of wells of mother
      mutate(per_well = (.data$mother_vol/.data$n_wells) - (.data$mother_vol/.data$n_wells)%%.echo_drop_nL, # how many transfers per well?
             extra_transfer = .data$mother_vol - .data$per_well*.data$n_wells) %>% # how many left over transfers, after even division over wells?
      rename(mother_vol_total = .data$mother_vol,
             mother_vol = .data$per_well) %>%
      unnest(cols = c(.data$data)) %>%  # this unnesting step adds the divided transfer volume to all mother source wells
      group_by(.data$compound, .data$mother_conc, .data$`Destination Well`, .add = TRUE) %>%
      mutate(mother_vol = if_else(row_number() == 1, .data$mother_vol + .data$extra_transfer, .data$mother_vol)) %>% # add the extra transfer to just one of the wells
      select(-.data$extra_transfer) %>% # drop unneeded column to match input
      ungroup()
  }

utils::globalVariables("where") # workaround: tidyselect::where() is not an exported function
# See: https://github.com/r-lib/tidyselect/issues/201#issuecomment-650547846
