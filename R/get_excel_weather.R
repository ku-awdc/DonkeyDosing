get_excel_weather <- function(weather, year){

  weather$Year <- as.numeric(format(weather$Date, "%Y"))
  weather$Month <- as.numeric(format(weather$Date, "%m"))
  weather$Day <- as.numeric(format(weather$Date, "%d"))

	# First get current year, add week day and week, then reverse order by date:
	cyweather <- weather %>%
		filter(.data$Year == year) %>%
		mutate(Week = as.numeric(strftime(.data$Date, format='%W'))) %>%
		mutate(WeekDay = as.numeric(strftime(.data$Date, format='%u'))) %>%
		arrange(desc(.data$Date))
	stopifnot(nrow(cyweather)>0)
	tail(cyweather)

	# Then get last year, alter the year, and add blank week and weekday:
	lyweather <- weather %>%
		filter(.data$Year == (year-1)) %>%
		mutate(Year = .data$Year+1, Week=NA, WeekDay=NA) %>%
		arrange(desc(.data$Date))
	stopifnot(nrow(lyweather)>0)

	# Then stick them together, and re-create missing week and weekdays from the previous week:
	allweather <- rbind(cyweather, lyweather)
	for(i in which(is.na(allweather$Week))){
		allweather$Week[i] <- allweather$Week[i-7]-1
		allweather$WeekDay[i] <- allweather$WeekDay[i-7]
	}

	# Ensure all dates are observed once:
	alldates <- rev(seq(min(allweather$Date), max(allweather$Date), by='day'))
	if(any(alldates != allweather$Date)){
		stop('There are some days missing in the weather data between ', year-1, ' and ', year)
	}

	# Ensure all weeks are observed 7 times:
	checkwks <- allweather %>%
		filter(! .data$Week %in% range(.data$Week)) %>%
		group_by(.data$Week) %>% tally()
	stopifnot(all(checkwks$n==7))
	# And all days of weeks the same number of times
	checkdow <- allweather %>%
		filter(! .data$Week %in% range(.data$Week)) %>%
		group_by(.data$WeekDay) %>% tally()
	stopifnot(all(checkdow$n==nrow(checkwks)))
	# And all combinations are observed once:
	checkcomb <- allweather %>%
		group_by(.data$Week,.data$WeekDay) %>% tally()
	stopifnot(all(checkcomb$n==1))
	# And recreated day of week is the same:
	check2 <- allweather %>%
		mutate(WeekDay2 = as.numeric(strftime(.data$Date, format='%u')))
	stopifnot(all(check2$WeekDay==check2$WeekDay2))

	# Then trim the data to 12+24 weeks before week 0:
	allweather <- allweather %>%
		filter(.data$Week >= -36) %>%
		arrange(.data$Date) %>%
		select("Date", "Year", "Week", "WeekDay", "Month", "Day", "Temp_high", "Temp_avg", "Temp_low", "Rel_Humidity_avg", "Abs_Humidity_avg") %>%
		mutate_at(vars(.data$Temp_high:.data$Abs_Humidity_avg), round, digits=1)

	if(any(is.na(allweather))) warning("Something went wrong formatting the weather: there are missing vales!", call.=FALSE)

	return(allweather)
}

