get_excel_weather <- function(weather, year){

	# First get current year, add week day and week, then reverse order by date:
	cyweather <- weather %>%
		filter(.data$Year == year) %>%
		mutate(Week = as.numeric(strftime(Date, format='%W'))) %>%
		mutate(WeekDay = as.numeric(strftime(Date, format='%u'))) %>%
		arrange(desc(Date))
	stopifnot(nrow(cyweather)>0)
	tail(cyweather)

	# Then get last year, alter the year, and add blank week and weekday:
	lyweather <- weather %>%
		filter(.data$Year == (year-1)) %>%
		mutate(Year = .data$Year+1, Week=NA, WeekDay=NA) %>%
		arrange(desc(Date))
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
		filter(! Week %in% range(Week)) %>%
		group_by(Week) %>% tally
	stopifnot(all(checkwks$n==7))
	# And all days of weeks the same number of times
	checkdow <- allweather %>%
		filter(! Week %in% range(Week)) %>%
		group_by(WeekDay) %>% tally
	stopifnot(all(checkdow$n==nrow(checkwks)))
	# And all combinations are observed once:
	checkcomb <- allweather %>%
		group_by(Week,WeekDay) %>% tally
	stopifnot(all(checkcomb$n==1))
	# And recreated day of week is the same:
	check2 <- allweather %>%
		mutate(WeekDay2 = as.numeric(strftime(Date, format='%u')))
	stopifnot(all(check2$WeekDay==check2$WeekDay2))

	# Then trim the data to 12+24 weeks before week 0:
	allweather <- allweather %>%
		filter(Week >= -36) %>%
		arrange(Date) %>%
		select(Year, Week, WeekDay, Month, Day, Temp_high, Temp_avg, Temp_low, Rel_Humidity_avg, Abs_Humidity_avg) %>%
		mutate_at(vars(.data$Temp_high:.data$Abs_Humidity_avg), round, digits=1)

	write.csv(allweather, file='weather_2018.csv', row.names=FALSE)
	return(allweather)
}

