% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_dosing_tool.R
\name{update_dosing_tool}
\alias{update_dosing_tool}
\title{Update animal information for the dosing tool}
\usage{
update_dosing_tool(
  excel_file,
  weather_path,
  year,
  coefficients,
  output_file = "output.xlsx"
)
}
\arguments{
\item{excel_file}{the Excel file containing the final (cleaned) dosing tool}

\item{weather_path}{a path to a folder containing the raw weather data}

\item{year}{the year for the new dosing tool}

\item{coefficients}{the coefficients extracted from the fitted model}

\item{output_file}{the file to save the output to (must end with .xlsx)}
}
\description{
Update animal information for the dosing tool
}
