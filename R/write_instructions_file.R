#' Write instructions file for by Echo Plate Reformat software
#'
#' Minor reformatting of calculated transfers tibble for uploading to the Echo Plate Reformat software
#'
#' @param transfers A tibble containing all transfer steps, returned by echowritr::calculate_transfers()
#' @param save_file If TRUE, saves instructions file under user-specified name.
#' @param .save_name File name for saved echo instructions
#' @param save_default A string appended to the end of the .save_name. Defaults to "_echo_transfer_instructions".
#'
#' @return A tibble containing echo instructions. If save_file = TRUE, this tibble is also saved as a csv.
#'
#' @importFrom dplyr select rename mutate select
#' @importFrom tidyselect all_of
#' @importFrom readr write_csv
#'
#' @export
write_instructions_file <- #_______(primary function) Save instructions for uploading to echo plate reformat software_______#
  function(transfers, save_file = FALSE, .save_name, save_default = "_echo_transfer_instructions") {
    instructions <- transfers %>%
      select(.data$`Destination Well`, .data$`Source Well`, .data$mother_vol) %>% # only these columns go to instructions
      rename("Transfer Volume" = .data$mother_vol) %>% # plate reformat software expects this name

      mutate("Source Plate Name" = "Source[1]", # these may be somewhat protocol variant, but also easy to change manually
             "Destination Plate Name" = "Destination[1]",
             "Destination Well X Offset"	= NA,
             "Destination Well Y Offset"	= NA) %>%

      select(all_of(c("Source Plate Name",	"Source Well",
                      "Destination Plate Name",	"Destination Well",
                      "Transfer Volume",	"Destination Well X Offset",
                      "Destination Well Y Offset")))

    if (save_file == TRUE) {
      instructions %>%
        readr::write_csv(x = . , paste0(Sys.Date(), "_", .save_name, save_default, ".csv"))
    }
    instructions
  }
