#' Update starting layout with final conditions
#'
#' @param transfers the transfers tibble
#' @param raw_layout the raw layout, as uploaded by the user
#'
#' @return a list of 2 elements. layout, the updated layout, and wide_layout, the updated layout in wideform, ready to be saved as a csv.
#'
#' @importFrom dplyr filter rename select across right_join
#' @importFrom tidyselect all_of everything
#' @importFrom purrr set_names
#' @export
make_updated_layout <- function(transfers, raw_layout) {

  updated_layout <- transfers %>%
    filter(.data$transfer_type  == "compound_transfer") %>% # not dilutions
    rename("well" = .data$`Destination Well`) %>%
    select(all_of(c("well", "compound", "final_conc", "rounded_up"))) %>%
    set_names(c("well", "final_compound", "final_concentration", "concentration_rounded_by")) %>%
    right_join( . , raw_layout, by = "well")

  to_layout_block <-  updated_layout %>%
    select(-any_of(c("row", "column", "well"))) %>%
    names()

  for_wide <- updated_layout %>%
    mutate(across(.cols = everything(), as.character)) %>%
    add_empty_wells() %>%
    replace(is.na(.), "Empty") %>%
    bind_layouts( . , to_layout_block )

  list(layout =  updated_layout,
       wide_layout = for_wide)

}

#' Make wideform layouts for multiple variables
#'
#' A helper function for make_updated_layout().
#' Relies on its own helper function, make_layout_wide().
#' Ultimately, this function and its helpers should be a part of a different package--likely the layouts package, if we ever get that thing done.
#'
#' @param data a layout
#' @param var_list the columns in the layout to be make into individual layout blocks
#'
#' @return a tibble of wideform layout blocks, ready to be saved as a csv
#'
#' @importFrom dplyr bind_rows
#' @importFrom purrr set_names
#'
#' @export
bind_layouts <- function(data, var_list) {
  var_list %>%
    lapply( . , function(x) make_layout_wide(data, x)) %>%
    bind_rows()   %>%
    set_names(c("Type", "row", c(1:(ncol(.)-2))))
}


#' Convert a single layout variable into a wideform layout block
#'
#' A helper function to make_updated_layout.
#' Ultimately, this function should be a part of a different package--likely the layouts package, if we ever get that thing done.
#'
#' @param data the layout
#' @param .filt_col a single column in data to be converted into a layout block
#' @param .fill_empties what to fill in for wells without contents. Defaults to "Empty"
#'
#' @return A tibble, containing a wideform layout block.
#'
#' @importFrom dplyr select mutate distinct arrange across relocate
#' @importFrom tidyr replace_na pivot_wider unnest
#' @importFrom tidyselect everything
#' @importFrom rlang `:=`
#'
#' @export
make_layout_wide <- function(data, .filt_col, .fill_empties = "Empty") {
  data %>%
    select(.data$row, .data$column, {{ .filt_col }}) %>%
    mutate("{.filt_col}" := replace_na(!!sym(.filt_col), .fill_empties)) %>%
    distinct() %>%
    mutate(column = as.numeric(.data$column)) %>%
    arrange(.data$column) %>%
    pivot_wider(id_cols = .data$row, names_from = .data$column, values_from = {{ .filt_col }}) %>%
    mutate(across(cols = everything(), as.character())) %>%
    rbind(names(.), .) %>%
    mutate(Type = {{ .filt_col }} ) %>%
    relocate(all_of("Type")) %>%
    unnest(cols = c(everything())) # this is a save that keeps bind_rows() from failing if these are list columns, but they really shouldn't be...
}
