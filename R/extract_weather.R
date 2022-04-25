#' Read weather from a local cache and format it for use with the Donkey Dosing model
#'
#' @param path a path to a folder containing the raw weather data
#' @param year the year for which to extract data
#' @param use_cache should the cache file be used where possible?
#' @param impute_missing should missing data be imputed?
#' @param include_date should the date be included in the return value?
#' @param type the source type for weather data (only OpenWeather is currently permitted)
#'
#' @importFrom pbapply pblapply
#' @importFrom imputeTS na_kalman
#' @importFrom stringr str_count
#'
#' @return
#'
#' @export
extract_weather <- function(path, year = format(Sys.Date(), "%Y"), use_cache = TRUE, impute_missing = FALSE, include_date = FALSE, type="OpenWeather"){

  ## Only OpenWeather data format supported now:
  if(!is.character(type) || length(type)!=1L || is.na(type)) stop("Invalid type argument", call.=FALSE)
  if(type != "OpenWeather") stop("Only OpenWeather type is supported", call.=FALSE)

  ## Check path exists:
  if(!is.character(path) || length(path)!=1L || !dir.exists(path)){
    stop("Specificed path does not exist", call.=FALSE)
  }

  ## Check year is valid:
  if(is.numeric(year)) year <- as.character(year)
  if(!is.character(year) || length(year)!=1L || is.na(year) || str_count(year)!=4L) stop("Invalid year", call.=FALSE)

  ## If using the cache, check it exists:
  if(isTRUE(use_cache) && file.exists(file.path(path, "cache.csv"))){
    cache <- read_csv(file.path(path, "cache.csv"), show_col_types = FALSE)
  }else{
    cache <- data.frame(Date = as.Date(character(0)), Temp_high = numeric(0), Temp_avg = numeric(0), Temp_low = numeric(0), Rel_Humidity_avg = numeric(0), Abs_Humidity_avg = numeric(0))
  }
  if(!identical(names(cache), c("Date", "Temp_high", "Temp_avg", "Temp_low", "Rel_Humidity_avg", "Abs_Humidity_avg"))) stop("The cache file has unexpected names", call.=FALSE)
  if(!identical(as.character(sapply(cache, class)), c("Date", "numeric", "numeric", "numeric", "numeric", "numeric"))) stop("The cache file has unexpected types", call.=FALSE)

  ## Find all available files not already in the cache:
  allyrs <- list.files(path, pattern="20[[:digit:]][[:digit:]]", full.names = TRUE)
  allfiles <- list.files(allyrs, pattern="*.rds$", full.names = TRUE)
  alldates <- as.Date(gsub("^.*/", "", allfiles), format="%Y-%m-%d.rds")
  if(any(is.na(alldates))) stop("One or more file could not be parsed as a date", call.=FALSE)
  using <- which(!alldates %in% cache$Date)

  ## Load new files:
  allfiles[using] |>
    pblapply(function(x){
      date <- as.Date(gsub("^.*/", "", gsub(".rds$","",x)))
      ss <- try({
        wthr <- readRDS(x)
        wthr <- vapply(wthr$hourly, function(y){
          rv <- c(y$temp, y$humidity, y$dew_point, 0)
          if("rain" %in% names(y)) rv[4] <- y$rain
          rv
        }, numeric(4L))

        # NB: convert Kelvin to Celsius:
        wthr[1,] <- wthr[1,] -273.15
        wthr[3,] <- wthr[3,] -273.15

        # Back-calculate absolute and relative humidities from dew point:
        tp <- wthr[1,]
        dp <- wthr[3,]
        rhcalc <- 100 * (exp((17.625*dp)/(243.04+dp)) / exp((17.625*tp)/(243.04+tp)))
        # NB: should be the same as wthr[2,]
        abshum <- (6.112 * exp((17.67*tp)/(243.5+tp)) * rhcalc * 2.1674) / (273.15 + tp)

        # Return the things we need:
        rv <- data.frame(date,max(tp), mean(tp), min(tp), mean(rhcalc), mean(abshum))
      })
      if(inherits(ss, "try-error")){
        cat("NOTE: reading data failed for file", gsub("^.*/", "", x), "\n")
        rv <- data.frame(date,0,0,0,0,0)
      }
      names(rv) <- c("Date", "Temp_high", "Temp_avg", "Temp_low", "Rel_Humidity_avg", "Abs_Humidity_avg")

      return(rv)
    }) |>
    bind_rows() ->
    alldata

  ## Add cache and then fill in missing dates:
  alldata <- bind_rows(cache, alldata)
  alldata |>
    full_join(data.frame(Date = seq(min(alldata$Date), max(alldata$Date), by=1L)), by="Date") |>
    arrange(Date) ->
    alldata

  ## Optionally impute missing:
  if(isTRUE(impute_missing)){
    for(cc in c("Temp_high", "Temp_avg", "Temp_low", "Rel_Humidity_avg", "Abs_Humidity_avg")){
      alldata[[cc]] <- na_kalman(alldata[[cc]])
    }
  }

  ## Do some rounding:
  for(cc in c("Temp_high", "Temp_avg", "Temp_low", "Rel_Humidity_avg", "Abs_Humidity_avg")){
    alldata[[cc]] <- round(alldata[[cc]], 2L)
  }

  ## Save cache:
  write_csv(alldata, file.path(path, "cache.csv"), na = "")

  ## Check that we have an early enough start and a late enough end:
  # We need to go back to week -36 so check there are 1st Jan - 37*7 days
  mindate <- as.Date(paste0(year, "-01-01")) - (7L*37L)
  if(min(alldata$Date) > mindate) stop("Unable to extract data:  dates do not go back far enough!", call.=FALSE)
  if(max(alldata$Date) < (Sys.Date() - 1L)) stop("Unable to extract data as the scraping has not yet been run today", call.=FALSE)

  ## Format correctly and return:
  rv <- get_excel_weather(alldata, as.numeric(year))
  if(!isTRUE(include_date)) rv$Date <- NULL

  return(rv)
}
