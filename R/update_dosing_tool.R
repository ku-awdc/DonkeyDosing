#' Update animal information for the dosing tool
#'
#' @param excel_file the Excel file containing the final (cleaned) dosing tool
#' @param weather_path a path to a folder containing the raw weather data
#' @param year the year for the new dosing tool
#' @param coefficients the coefficients extracted from the fitted model
#' @param output_file the file to save the output to (must end with .xlsx)
#'
#' @importFrom writexl write_xlsx
#' @export
update_dosing_tool <- function(excel_file, weather_path, year, coefficients, output_file="output.xlsx"){

  if(!is.character(excel_file) || length(excel_file)!=1 || !(grepl('\\.xlsx$', excel_file) || grepl('\\.xls$', excel_file) ))
    stop('The specified excel_file argument must be a length 1 character string specifying a file with a .xlsx or .xls file extension')
  if(!is.character(output_file) || length(output_file)!=1 || !(grepl('\\.xlsx$', output_file) ))
    stop('The specified output_file argument must be a length 1 character string specifying a file with a .xlsx file extension')

  excel_sheet <- "Locations"
  if(!excel_sheet %in% excel_sheets(excel_file)) stop('The required sheet "Locations" is missing from the specified excel file')
  locations <- read_excel(excel_file, excel_sheet, .name_repair = "unique")
  if(! all(c('Farm','Location','Hygiene','Sensitivity') %in% names(locations)))
    stop('One or more of the required columns Farm, Location, Hygiene and Sensitivity was missing from the specified excel file/sheet')

  stopifnot("Coefficients must be produced by the GetCoefficients() function" = is.data.frame(coefficients), "Coefficients must be produced by the GetCoefficients() function" = ncol(coefficients)==2L, "Coefficients must be produced by the GetCoefficients() function" = names(coefficients)==c("groupcoefnames","groupcoefs"))

  ## Get weather data first to check year:
  cat("Extracting weather data for ", year, "...\n", sep="")
  new_weather <- extract_weather(weather_path, year = year)

  ## Then extract FEC data:
  year <- as.numeric(year)-1L
  locations <- locations |>
    mutate(Year = year) |>
    mutate(Location = str_c(str_replace_all(.data$Farm, '[[:space:]]',''), "_", str_replace_all(.data$Location, '[[:space:]]','')))

  farms <- unique(locations$Farm)
  output <- farms %>% as.list()
  names(output) <- farms

  for(farm in farms){
    cat("Calculating averages for farm ", farm, "...\n", sep="")

    data <- dosing_sheet(year, farm, excel_file, farm, locations, silent=TRUE)$rawdata

    # Add a row number and then calculate mean FEC:
    data$row <- 1:nrow(data)

    meanfecs <- data %>%
      filter(.data$Location != 'DEAD') %>%
      select(.data$row, starts_with('F')) %>%
      gather("week", "fec", -.data$row) %>%
      mutate(fec = suppressWarnings(as.numeric(.data$fec))) %>%
      filter(!is.na(.data$fec)) %>%
      group_by(.data$row) %>%
      summarise(meanfec = mean(.data$fec, na.rm=TRUE)) %>%
      mutate(meanfec = ifelse(is.nan(.data$meanfec), NA, round(.data$meanfec)))

    # Need to increment ages by 1 year:
    dataout <- data %>%
      right_join(meanfecs, by='row') %>%
      mutate(Age = as.numeric(.data$Age)+1) %>%
      # And fill in any missing clinical risk as Normal:
      mutate(ClinicalRisk = ifelse(is.na(.data$ClinicalRisk) | .data$ClinicalRisk=='', 'Normal', .data$ClinicalRisk)) %>%
      # Rename notes as NotesOld:
      mutate(NewNotes = '') %>%
      select(.data$Name:.data$ClinicalRisk, AvFEC = .data$meanfec, OldNotes = .data$Notes, Notes=.data$NewNotes)

    # Then add the blank row 1:
    blank <- dataout[1,]
    blank[] <- NA
    blank$AvFEC <- year

    output[[farm]] <- rbind(blank,dataout) %>% as_tibble()

  }
  cat("Done\n")

  write_xlsx(c(output, list(WeatherData=new_weather, EffectEstimates=coefficients)), output_file)

}
