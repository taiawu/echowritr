% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_instructions_file.R
\name{write_instructions_file}
\alias{write_instructions_file}
\title{Write instructions file for by Echo Plate Reformat software}
\usage{
write_instructions_file(
  transfers,
  save_file = FALSE,
  .save_name,
  save_default = "_echo_transfer_instructions"
)
}
\arguments{
\item{transfers}{A tibble containing all transfer steps, returned by echowritr::calculate_transfers()}

\item{save_file}{If TRUE, saves instructions file under user-specified name.}

\item{.save_name}{File name for saved echo instructions}

\item{save_default}{A string appended to the end of the .save_name. Defaults to "_echo_transfer_instructions".}
}
\value{
A tibble containing echo instructions. If save_file = TRUE, this tibble is also saved as a csv.
}
\description{
Minor reformatting of calculated transfers tibble for uploading to the Echo Plate Reformat software
}
