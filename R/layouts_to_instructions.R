#' The one-shot function to convert layouts into an echo instructions file, and save the associated fodler
#'
#' @param expnum The experiment number, e.g. "4242"
#' @param save_path the path to save the instructions folder in
#' @param daughter_path path to the daughter layout
#' @param mother_path to the mother layout
#' @param save_output if TRUE, saves the resulting instructions in the save_path
#' @param .well_col_daughter passed to standardize_layouts for daughter. Defaults to "well".
#' @param .compound_col_daughter  passed to standardize_layouts for daughter. Defaults to "compound".
#' @param .concentration_col_daughter  passed to standardize_layouts for daughter. Defaults to "concentration".
#' @param .volume_col_daughter passed to standardize_layouts for daughter. Defaults to "volume".
#' @param .well_col_mother passed to standardize_layouts for mother. Defaults to "well".
#' @param .compound_col_mother passed to standardize_layouts for mother. Defaults to "compound".
#' @param .concentration_col_mother passed to standardize_layouts for mother. Defaults to "concentration".
#' @param .if_missing action to take if compounds in the daughter are not in the mother. Defaults to "stop". Passed to repair_layout.
#' @param .if_varied action to take if ccompounds are present at multile concentrations in the mother. Defaults to "stop". Passed to repair_layout.
#' @param .if_impossible action to take if compounds are present in the daughter at concentrations greater than the mother can provide. Defaults to "stop". Passed to repair_layout.
#' @param .echo_drop_vol the volume of the droplet for the echo instrument to be used. Defaults to 25.
#' @param .max_mother_pull the maximum volume to be pulled from any one well in the mother. If exceeded, this will be noted in the the final plateview plots, and transfer csv. Defaults to 35.
#'
#' @return a list containing all intermediate and final objects. If save_output = TRUE, also saves the echo instrutions in a folder.
#'
#' @export
layouts_to_instructions <- function(expnum,
                                    save_path = "",


                                    daughter_path,
                                    mother_path,
                                    save_output = FALSE,

                                    .well_col_daughter = "well",
                                    .compound_col_daughter = "compound",
                                    .concentration_col_daughter = "concentration",
                                    .volume_col_daughter = "volume",

                                    .well_col_mother = "well",
                                    .compound_col_mother = "compound",
                                    .concentration_col_mother = "concentration",

                                    .if_missing = "stop",
                                    .if_varied = "stop",
                                    .if_impossible = "stop",

                                    .echo_drop_vol = 25,
                                    .max_mother_pull = 35) {

  original_daughter <- read_plate_layout(daughter_path)

  daughter_raw <- ##---- INTERMEDIATE: standardized daughter layout ---
    original_daughter %>%
    standardize_layout("daughter",
                        .well_col = .well_col_daughter,
                        .compound_col = .compound_col_daughter,
                        .concentration_col = .concentration_col_daughter,
                        .volume_col = .volume_col_daughter
    )
  print("daughter layout read and standardized")

  original_mother <-  read_plate_layout(mother_path)
  mother_raw <- ##---- INTERMEDIATE: standardized mother layout ---
    original_mother %>%
    standardize_layout("mother",
                        .well_col = .well_col_mother,
                        .compound_col = .compound_col_mother,
                        .concentration_col = .concentration_col_mother)
  print("mother layout read and standardized")

  layouts <- ##---- OUTPUT: the cleaned layouts ---##
    repair_layout(mother_raw,
                  daughter_raw,
                  if_missing = .if_missing,
                  if_varied = .if_varied,
                  if_impossible = .if_impossible
                  )
  print("layouts repaired")

  transfers <- calculate_transfers(layouts$daughter, layouts$mother, .echo_drop_vol)
  print("transfers calculated")
  instructions <- write_instructions_file(transfers)
  print("instructions written")
  depletion <- monitor_source_depletion(transfers, .max_mother_pull)
  print("source plate depletion calculated")
  for_plotting <- all_plateview_vars(daughter_raw, transfers, depletion)
  all_plots <- make_all_plots(for_plotting$data, for_plotting$plot_vars)
  print("plateview plots made")
  final_layouts <- make_updated_layout(transfers, original_daughter)
  print("daughter layout updated to reflect reparis and rounding")

  if (save_output) {
    save_instructions_folder(expnum,
                             save_path,
                             final_layouts = final_layouts,
                             instructions = instructions,
                             all_plots = all_plots,
                             layouts = layouts,
                             transfers = transfers,
                             depletion = depletion
    )

    print("**Saved instructions folder**")
  }

  out <- list(original_mother = original_mother,
              original_daughter = original_daughter,
              standardized_mother = mother_raw,
              standardized_daughter = daughter_raw,

              repaired_layouts = layouts,
              transfers = transfers,
              instructions = instructions,
              depletion = depletion,
              layouts_for_plotting = for_plotting,
              all_plots = all_plots,
              final_layouts = final_layouts)
}


#' Read a plate layout file into a tibble
#'
#' This is pasted exactly from the dsfworld package. Sloppy, i know, but I don't know how to add dependencies to internal packages, and since its the only one in this package, i feel like it's ok?
#'
#'read_layout() reads a plate layout file (.csv, .txt, .xls, or .xlsx), and returns it as a formatted tibble. Variable types are guessed with readr::parse_guess(). The originally required "Type" column heading is now optional.
#'
#'
#'
#' @param filepath A complete file path, pointing to the plate layout file
#'
#' @return Returns a tibble, mapping experimental variables to well positions. All outputs contain the columns: row, column, and well. Additionally, a single column is added for each user-defined variable in the plate layout file, from which one additional "condtion" column is created, which contains all experimental variables. If all experimental variables are defined in the layout, wells with identical entries in the "condition" column are technical replicates.
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom tools file_ext
#' @importFrom readr read_csv read_tsv parse_guess
#' @importFrom readxl read_excel
#' @importFrom purrr set_names discard
#' @importFrom dplyr filter if_all mutate across
#' @importFrom tidyr pivot_longer pivot_wider unite
#' @importFrom utils "globalVariables"
#'
#' @export
read_plate_layout <- function(filepath){

  # read file based on it's type
  ext <- file_ext(filepath)

  raw <- switch(ext,
                csv = read_csv(filepath, col_names = FALSE),
                txt = read_tsv(filepath, col_names = FALSE),
                xlsx = read_excel(filepath, col_names = FALSE),
                xls =  read_excel(filepath, col_names = FALSE)
  ) %>% base:: suppressMessages()

  # handle files with or without the "Type" header
  first_cell <- raw[1,1][[1]]
  out <- switch(first_cell,
                Type = raw[-1,],
                raw)

  # convert into layout form
  out %>%
    set_names( c("variable", "row", .[1,][-c(1,2)])) %>%
    filter(row %in% base::LETTERS[1:16]) %>%
    discard(~all(is.na(.x)))  %>% # drop columns if everything is NA
    filter(if_all(everything(), ~ !is.na(.x))) %>%
    mutate(across(everything(), as.character)) %>% # make all character, to prevent issues in pivot
    pivot_longer(-c(.data$variable, .data$row), names_to = "column", values_to = "value") %>%
    pivot_wider(names_from = .data$variable, values_from = .data$value) %>%
    mutate(well = paste0(.data$row, .data$column)) %>% # make well column
    unite(condition, -c(.data$row, .data$column, .data$well), sep = "__", remove = FALSE) %>% # make condition column
    filter(!across(-c(.data$well, .data$row, .data$column, .data$condition)) == "Empty") %>% # if all variables are "Empty",   wells
    filter(!is.na(across(-c(.data$well, .data$row, .data$column, .data$condition))) == TRUE) %>% # if all variables are NA,   wells
    mutate(across(everything(), parse_guess)) # convert likely numeric variables to numeric
}

utils::globalVariables(c(".", "condition"))

