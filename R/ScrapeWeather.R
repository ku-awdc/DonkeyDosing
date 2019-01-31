#' @title Scrape the requrired weather data
#' @name ScrapeWeather
#' @aliases ScrapeWeather

#' @description
#' This function can be used to scrape weather data from a website (intended to be run automatically on a regular basis).  If historical data is provided as input then the scraped data will be combined with this.

#' @return
#' A data frame with the available weather data in an appropriate format for the donkey dosing model

#' @param input_file either a path to an Excel or CSV file containing previously recorded (or scraped) weather data, a data frame of the same, or NA
#' @param output_file either a path to an Excel or CSV file to which the combined data will be written, or NA
#' @param ... additional arguments to be passed through to the darksky package

#' @importFrom darksky get_current_forecast get_forecast_for

#' @export ScrapeWeather

ScrapeWeather <- function(input_file=NA, output_file=NA, ...){
	
	# Code missing!!
	
}