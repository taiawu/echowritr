#' Make a Plate-view plot
#'
#' Makes a plot that mimics looking down at a well plate. Wells are colored based on a user-defined column from a layout.
#'
#' @param plate_data a layout tibble (as created by dsfworld::read_plate_layout()), containing columns for plate row (called "row"), plate column (called "column"), and one variable by which the plate will be colored
#' @param fill_col the column in the input layout by which to color in the wells in the plot
#' @param title_var the column for which the plot will be named. Shuldn't be an argument in the future; I struggled with tidyeval here so just put it at as different variable... Gets sent to glue::glue(). See make_all_plots function for the origin of this issue, with !!sym(x) working for the fill_col argument, but un-passable to glue.
#' @param shape the shape of the points used to make the wells. Defaults to 22 (filled square).
#' @param size the shape of the points used to make the wells. Defaults to 4.
#' @param title_prefix a prefix to be added to the title. Functionally, this can remain the same while title_var changes, to create consistent names when mapping over many variables. Also hopefully only a temporary argument, to be fixed in later versions.
#' @param ... to be passed to the ggplot2 aesetheics inside this function. Not actually used inside the function yet... another place to improve things here in the near furture.
#'
#' @return a plate-view plot, with wells colored based on the information on that well in the user-defined variable given in .fill_var.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter rename
#' @importFrom ggplot2 ggplot aes scale_y_discrete geom_point scale_x_continuous theme labs scale_fill_viridis_d scale_fill_viridis_c  element_rect element_blank element_line
#' @importFrom plyr rbind.fill
#' @importFrom tidyselect any_of
#' @importFrom tidyr fill
#' @importFrom stringr str_extract_all str_to_upper
#' @importFrom purrr as_vector
#' @importFrom readr parse_number
#'
#' @export
#'
plateview_plot <-
  function(plate_data,
           fill_col,
           title_var,
           shape = 22,
           size = 4,
           title_prefix = Sys.Date(),
           ...) {

    plot_title <- glue::glue("{title_prefix} Plate-view plot: {title_var}")

    fill_scale <- plate_data %>%
      pull({{ fill_col }})  %>%
      get_fill_scale( )

    plate_data
    ggplot(plate_data, aes(x = .data$column, y = .data$row)) +
      blank_plate(shape = shape, size = size)+

      geom_point(data = plate_data %>%
                   filter(is.na({{ fill_col }}) == FALSE),
                 aes(fill = {{ fill_col }}),
                 color = "#969696",
                 shape = shape,
                 size = size) +
      fill_scale +
      plate_theme_dark() +
      labs(title = plot_title)
  }


#' Add empty wells to a partially-filled layout
#'
#' A helper function for making plate plots using the plate_plot() function
#'
#' @param df the layout file to have empty wells appended
#' @param n_wells the number of wells in the plate. Options are "384" and "96". Defaults to "384".
#' @param .df_well_col the name of the colum containing well information. Defaults to "well".
#' @param .fill_down_cols a vector of any columns which should be filled in for the empty wells. These will be filled with a single value, determined by the plyr::fill(.direction = "down") function.
#' @param add_rows_cols should plate rows and columns be added to the layout (e.g. A, B, . . ., 1, 2, 3, ...)? Defaults to TRUE.
#'
#' @return the input layout, with all empty wells appended. These values are NA, unless specified in the .fill_down_cols argument of this function.
#'
#' @importFrom ggpubr ggarrange
#'
#' @export
add_empty_wells <- function(df,
                            n_wells = "384",
                            .df_well_col = "well",
                            .fill_down_cols = "",
                            add_rows_cols = TRUE) {
  well_vec <- switch(n_wells,
                     "384" = c("A1","B1","C1","D1","E1","F1","G1","H1","I1","J1","K1","L1","M1","N1","O1","P1","A2","B2","C2","D2","E2","F2","G2","H2","I2","J2","K2","L2","M2","N2","O2","P2","A3","B3","C3","D3","E3","F3","G3","H3","I3","J3","K3","L3","M3","N3","O3","P3","A4","B4","C4","D4","E4","F4","G4","H4","I4","J4","K4","L4","M4","N4","O4","P4","A5","B5","C5","D5","E5","F5","G5","H5","I5","J5","K5","L5","M5","N5","O5","P5","A6","B6","C6","D6","E6","F6","G6","H6","I6","J6","K6","L6","M6","N6","O6","P6","A7","B7","C7","D7","E7","F7","G7","H7","I7","J7","K7","L7","M7","N7","O7","P7","A8","B8","C8","D8","E8","F8","G8","H8","I8","J8","K8","L8","M8","N8","O8","P8","A9","B9","C9","D9","E9","F9","G9","H9","I9","J9","K9","L9","M9","N9","O9","P9","A10","B10","C10","D10","E10","F10","G10","H10","I10","J10","K10","L10","M10","N10","O10","P10","A11","B11","C11","D11","E11","F11","G11","H11","I11","J11","K11","L11","M11","N11","O11","P11","A12","B12","C12","D12","E12","F12","G12","H12","I12","J12","K12","L12","M12","N12","O12","P12","A13","B13","C13","D13","E13","F13","G13","H13","I13","J13","K13","L13","M13","N13","O13","P13","A14","B14","C14","D14","E14","F14","G14","H14","I14","J14","K14","L14","M14","N14","O14","P14","A15","B15","C15","D15","E15","F15","G15","H15","I15","J15","K15","L15","M15","N15","O15","P15","A16","B16","C16","D16","E16","F16","G16","H16","I16","J16","K16","L16","M16","N16","O16","P16","A17","B17","C17","D17","E17","F17","G17","H17","I17","J17","K17","L17","M17","N17","O17","P17","A18","B18","C18","D18","E18","F18","G18","H18","I18","J18","K18","L18","M18","N18","O18","P18","A19","B19","C19","D19","E19","F19","G19","H19","I19","J19","K19","L19","M19","N19","O19","P19","A20","B20","C20","D20","E20","F20","G20","H20","I20","J20","K20","L20","M20","N20","O20","P20","A21","B21","C21","D21","E21","F21","G21","H21","I21","J21","K21","L21","M21","N21","O21","P21","A22","B22","C22","D22","E22","F22","G22","H22","I22","J22","K22","L22","M22","N22","O22","P22","A23","B23","C23","D23","E23","F23","G23","H23","I23","J23","K23","L23","M23","N23","O23","P23","A24","B24","C24","D24","E24","F24","G24","H24","I24","J24","K24","L24","M24","N24","O24","P24"),
                     "96" = c("A1","B1","C1","D1","E1","F1","G1","H1","A2","B2","C2","D2","E2","F2","G2","H2","A3","B3","C3","D3","E3","F3","G3","H3","A4","B4","C4","D4","E4","F4","G4","H4","A5","B5","C5","D5","E5","F5","G5","H5","A6","B6","C6","D6","E6","F6","G6","H6","A7","B7","C7","D7","E7","F7","G7","H7","A8","B8","C8","D8","E8","F8","G8","H8","A9","B9","C9","D9","E9","F9","G9","H9","A10","B10","C10","D10","E10","F10","G10","H10","A11","B11","C11","D11","E11","F11","G11","H11","A12","B12","C12","D12","E12","F12","G12","H12"))


  df_wells <- df %>% pull({{ .df_well_col }}) %>% unique()

  df_out <- tibble("well" = well_vec %>% as.character()) %>%
    filter(!.data$well %in% df_wells) %>%
    plyr::rbind.fill(df,  . ) %>%# fills in everything
    as_tibble() %>% # convert back to tibble from data.frame
    fill(any_of(.fill_down_cols), .direction = "down")

  if (add_rows_cols == TRUE) {

    df_out <- df_out %>%
      mutate(row =  str_extract_all(.data$well, "[A-Z; a-z]", simplify = TRUE) %>%
               str_to_upper(locale = "en") %>%
               as_vector(),
             column = parse_number(.data$well))
  }

  df_out

}


#' make_all_plots
#'
#' Make a plateview plot for a every variable in a provided list.
#'
#' @param plate_data the layout tibble from which the plots will be made
#' @param plot_these a list of column names in plate_data--a plateview plot will be made for each element in this list.
#'
#' @return A list, containing (1) "individual_plots", which has each individual variable plotted, stored as a sub-itme under the name of the variable it contains. (2) "all_plots_fig", a ggpubr object containing all of the plotted variables in a single figure, useful for simple batch download. (3) save_width: the recommended widith to save the ggpubr object, set to 12. (4) save_height: the recommended height to save the all_plots_fig, calculated to depend on the number of plots that this figure contains.
#'
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map
#' @importFrom rlang sym
#' @export
make_all_plots <- function(plate_data, plot_these) {

  plate_plots <- map(plot_these,
                     function(x) {
                       plateview_plot(plate_data,
                                      fill_col = !!sym(x),
                                      title_var = x) }) %>%
    set_names(plot_these)

  plate_fig <- plate_plots %>%
    ggpubr::ggarrange(plotlist = .,
                      ncol = 1,
                      align = "v")

  list(individual_plots = plate_plots,
       all_plots_fig = plate_fig,
       save_width = 12,
       save_height = 3*length(plate_plots))

}

## not exported
blank_plate <- # shared between most plots
  function(col_breaks = 1,
           shape = 22,
           size = 4,
           alpha = 1,
           ...) {
    list(
      scale_y_discrete(limits = rev), # start with row A
      geom_point(color ="#737373", # background
                 shape = shape,
                 size = size,
                 alpha = alpha,
                 ...),
      scale_x_continuous(breaks = seq(from = 1, to = 24, by = col_breaks))
    )
  }

#' Extract variables worth making plateview plots
#'
#' Mostly just here to mask out things like wells, rows, and columns
#'
#' @param data layout tibble
#' @param drop_vars vector of variables not worth plotting. Defaults to: c("Destination Well","Source Well","well","Well","row","column","mother_vol","rounded_up_perc","mother_conc" )
#'
#' @return a character vector of variables from the input data maybe work making into a plateview plot.
#' @export
get_plotworthy_vars <-
  function(data,
           drop_vars = c("Destination Well",
                         "Source Well",
                         "well",
                         "Well",
                         "row",
                         "column",
                         "mother_vol",
                         "rounded_up_perc",
                         "mother_conc" )) {
    data %>%
      select(-any_of(drop_vars)) %>%
      names()
  }

plate_theme_dark <-
  function() {
    theme(
      aspect.ratio = 16/24,

      panel.background  =
        element_rect(
          color = "#525252",
          fill = "#525252"),

      panel.grid.minor =
        element_blank(),

      panel.grid.major =
        element_line(
          color = "#737373",
          size = 0.5),

      legend.position = "right",

      axis.title =
        element_blank(),

      plot.background =
        element_rect(
          fill = "transparent",
          colour = NA),
    )
  }

get_fill_scale <- # handle discrete or continuous
  function(fill_vec) {
    scale_type <- # if unsure, use discrete
      if_else(is.numeric(fill_vec),
              true = "use_numeric",
              false = "use_discrete",
              "use_discrete")

    switch(scale_type,
           "use_numeric" = scale_fill_viridis_c(),
           "use_discrete" = scale_fill_viridis_d())
  }


#' Create a single tibble summarizing the transfer conditions and any important changes and repairs
#'
#' @param daughter_raw the standardized daughter
#' @param transfers the transfers tibble
#' @param depletion the depletion tibble
#'
#' @return a list, with elements data: a tibble containins pertinent columns from the three input tibbles, and plot_vars, a list of variables from this tibble to be passed to make_all_plots
#'
#' @importFrom purrr set_names
#' @importFrom dplyr rename
#'
#' @export
all_plateview_vars <- function(daughter_raw, transfers, depletion) {

  daughter_plotworthy <- daughter_raw %>%
    get_plotworthy_vars()

  daughter_vars <- daughter_raw %>%
    select(all_of(c("Destination Well", daughter_plotworthy))) %>%
    rename("original_compound_layout" = .data$compound) %>%
    rename("original_concentrations" = .data$daughter_conc)

  transfer_vars <- transfers %>%
    filter(.data$transfer_type == "compound_transfer") %>% # not the dilutant
    summarise_rounding() %>% # just final_conc, rounded_up, rounded_up_perc
    select(c(.data$`Destination Well`, .data$original_daughter_conc, .data$rounded_daugher_conc, .data$rounded_up_by, .data$compound)) %>%
    rename("concentrations_after_repairs" = .data$original_daughter_conc)

  depletion_vars <- depletion %>%  # just uL used and over_drawn
    select(c(.data$`Source Well`, .data$uL_used, .data$over_drawn)) %>%
    set_names(c("well", "uL_used_in_mother", "mother_well_overdrawn")) # purrr

  for_plateview <- left_join(daughter_vars, transfer_vars, by = "Destination Well")  %>%
    mutate(well = .data$`Destination Well`) %>%
    select(-.data$`Destination Well`) %>%
    left_join(. , depletion_vars, by = "well") %>%
    add_empty_wells()


  list(data = for_plateview,
       plot_vars = for_plateview %>% get_plotworthy_vars())
}


