#' Scrape OpenWeather data for the Donkey Dosing model
#'
#' @param path a path to a folder containing the raw weather data
#' @param api_key either an OpenWeather API key, or NULL in which case the key will be read from an "OpenWeatherKey.txt" file in the specified path
#' @param lat the latitude for the location used for scraping (default is a point roughly in the middle of the Donkey Sanctuary):
#' @param lon the longitude for the location used for scraping (default is a point roughly in the middle of the Donkey Sanctuary):
#'
#' @return a character vector of paths to which weather observations have been saved, invisibly
#'
#' @importFrom RJSONIO fromJSON
#'
#' @export
scrape_openweather <- function(path, api_key = NULL, lat = 50.725, lon = -3.175){

  ## Check path exists
  if(!is.character(path) || length(path)!=1L || !dir.exists(path)){
    stop("Specificed path does not exist", call.=FALSE)
  }

  ## If api_key is not specified, find it in the path:
  if(is.null(api_key)){
    stopifnot(file.exists(file.path(path, "OpenWeatherKey.txt")))
    api_key <- readLines(file.path(path, "OpenWeatherKey.txt"))[1]
  }

  if(!is.character(api_key) || length(api_key)!=1L || is.na(api_key)) stop("Invalid api_key", call.=FALSE)

  ## Possible dates we can retrieve:

  cdt <- Sys.Date()
  obt <- character(0)

  for(delta in (5L:1L)){

    date <- cdt - delta

    yr <- format(date, "%Y")
    if(!dir.exists(file.path(path, yr))) mkdir(file.path(path, yr))

    fl <- file.path(path, yr, format(date, "%Y-%m-%d.rds"))
    if(file.exists(fl)) next()

    dt <- floor(as.numeric(as.POSIXct(date)))
    wthr <- fromJSON(paste0("https://api.openweathermap.org/data/2.5/onecall/timemachine?lat=", lat, "&lon=", lon, "&dt=", dt, "&appid=", api_key))
    saveRDS(wthr, fl)

    obt <- c(obt, fl)

    ## Wait for 1 second to avoid overloading the server:
    Sys.sleep(1L)
  }

  cat("Collected", length(obt), "weather observations\n")

  invisible(obt)

}
