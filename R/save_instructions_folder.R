#' Save echo instructions folder
#'
#'
#' @param expnum The experiment number, including wordy details, e.g "4242_daugher_plates"
#' @param save_path The path to where the folder should be saved. Will proceed the experiment number and "_echo instructions". Defaults to "", saving the folder as "Exp<expnum>_echo_instructions/"
#' @param final_layouts The final layouts list, created by make_updated_layout()
#' @param instructions The echo instructions tibble, created by write_instructions_file()
#' @param all_plots The plateview plots, created by make_all_plots()
#' @param layouts The standardized layouts, created by repair_layout()
#' @param transfers The calculated transfers, created by calculate_transfers()
#' @param depletion The tibble of mother plate depletions, created by monitor_source_depletion()
#' @param version_num The version number for the echowrtir package. Gets written into the reademe; is actually the only thing in the readme.txt at this point...
#'
#' @return A folder in the given directory, containing everything needed to echo your plate, and accurately manage the resulting data, including any rounding or omissions.
#'
#' @importFrom fs dir_exists dir_create
#' @importFrom rlang abort
#' @importFrom glue glue
#' @importFrom ggplot2 ggsave
#' @importFrom readr write_csv
#' @export
save_instructions_folder <- function(expnum,
                                     save_path = "",
                                     final_layouts,
                                     instructions,
                                     all_plots,
                                     layouts,
                                     transfers,
                                     depletion,
                                     version_num = "0.0.0.9000"
) {

  exp_header <- make_exp_header(expnum)
  final_path <- paste0(save_path, "/", exp_header, "echo_instructions")

  if (fs::dir_exists(final_path)) {
    rlang::abort(message = glue::glue("The name {final_path} already exists in this directory! Please provide a unique name."))
  }

  fs::dir_create(final_path, new_path = c("supporting_files", "updated_layout"))


  # in the main folder
  main_path <- paste0(final_path, "/", exp_header)
  write_csv(x = instructions, paste0(main_path, "echo_instructions.csv"), quote_escape = FALSE)

  ggsave(paste0(main_path, "plateview_plots.pdf"),
         all_plots$all_plots_fig,
         width = all_plots$save_width,
         height = all_plots$save_height)

  # the only thing in the final layout folder
  write_csv(x = final_layouts$wide_layout, paste0(final_path, "/updated_layout/", exp_header, "final_layout.csv"))

  # in the additional things folder
  supp_path <- paste0(final_path, "/supporting_files/", exp_header)
  write_csv(x = layouts$mother, paste0(supp_path, "repaired_mother_layout.csv"))
  write_csv(x = layouts$mother, paste0(supp_path, "repaired_daughter_layout.csv"))
  write_csv(x = transfers, paste0(supp_path, "transfers.csv"))
  write_csv(x = depletion, paste0(supp_path, "source_depletion.csv"))
  write_csv(x = depletion, paste0(supp_path, "source_depletion.csv"))
  write(paste0("Created using echowritr version ", version_num, ". Happy echoing!"), file = paste0(supp_path,"readme.txt"))
}
