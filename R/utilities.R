# Utility functions - not exported:

matchweek <- function(fecdata, sensitivity, week_offset, errstr){

	stopifnot('Observation' %in% names(fecdata))
	stopifnot('Value' %in% names(fecdata))
	stopifnot('AnimalID' %in% names(fecdata))
	stopifnot('Location' %in% names(fecdata))
	stopifnot('Year' %in% names(fecdata))

	atmtcs <- dosing_getOption('anthelmintics')

	fecdata$Week <- as.numeric(NA)
	fecdata$FEC <- as.numeric(NA)
	fecdata$Dose <- FALSE
	fecdata$DoseText <- as.character(NA)

	fecs <- which(grepl('^F[[:digit:]]*', fecdata$Observation))
	fec_week <- as.numeric(gsub('^F','', fecdata$Observation[fecs]))
	fec_check <- fecdata$Value[fecs]
	fec_value <- suppressWarnings(as.numeric(fecdata$Value[fecs]))
	if(any(sapply(atmtcs, function(x) return(grepl(x, fec_check, fixed=TRUE))))){
		prob <- which(apply(sapply(atmtcs, function(x) return(grepl(x, fec_check, fixed=TRUE))), 1, any))
		stop(paste(paste0('Likely anthelmintic treatment found in FEC column for ', errstr, ': ', fec_check[prob]), collapse='\n'))
	}
	if(any(table(fec_week, is.na(fec_value))[,'FALSE']==0)){
		tt <- table(fec_week, is.na(fec_value))[,'FALSE']==0
		# stop(paste(paste0('All missing FEC column for ', errstr, ': F', names(tt)[tt]), collapse='\n'))
	}

	fecdata$Week[fecs] <- fec_week - week_offset
	fecdata$FEC[fecs] <- fec_value
	stopifnot(all(fecdata$FEC >= 0, na.rm=TRUE))

	txs <- which(grepl('^T[[:digit:]]*', fecdata$Observation))
	tx_week <- as.numeric(gsub('^T','', fecdata$Observation[txs]))
	tx_value <- !is.na(fecdata$Value[txs]) & !(fecdata$Value[txs]=='')
	tx_text <- fecdata$Value[txs]
	tx_text[is.na(tx_text)] <- ''

	stopifnot(!any(txs %in% fecs))

	fecdata$Week[txs] <- tx_week - week_offset
	fecdata$Dose[txs] <- tx_value
	fecdata$DoseText[txs] <- tx_text

	fecdata <- fecdata[c(fecs,txs),]

	check <- fecdata %>%
		group_by(.data$AnimalID, .data$Year, .data$Week) %>%
		summarise(numfec = sum(!is.na(.data$FEC)), numtx = sum(.data$Dose), numtext = sum(!is.na(.data$DoseText)))
	stopifnot(all(check$numfec <= 1))
	stopifnot(all(check$numtx <= 1))
	stopifnot(all(check$numtext %in% c(0,1)))

	toret <- fecdata %>%
		group_by(.data$AnimalID, .data$Location, .data$Year, .data$Week) %>%
		summarise(FEC = max(c(-1, .data$FEC), na.rm=TRUE), Count = 0, Dose = any(.data$Dose), DoseText = ifelse(any(!is.na(.data$DoseText)), .data$DoseText[!is.na(.data$DoseText)], as.character(NA))) %>%
			 # Hack using max with -1 and reset -1 values to NA:
		mutate(FEC = ifelse(.data$FEC < 0, NA, .data$FEC)) %>%
		filter(!is.na(.data$FEC) | .data$Dose)
	toret$Count <- toret$FEC / sensitivity[toret$Location]

	stopifnot(!any(is.na(fecdata$Week)))

	return(toret)
}


## Process weather data:
#' @importFrom utils read.csv setTxtProgressBar txtProgressBar
process_weather <- function(weather, min_year){

	# Needed for models:
	l4 <- round(7*seq(1,4-1/7,by=1/7))
	l8 <- round(7*seq(4,8-1/7,by=1/7))
	l12 <- round(7*seq(8,12-1/7,by=1/7))

	weather$Week <- as.numeric(strftime(weather$Date, format='%W'))
	weather$Year <- as.numeric(strftime(weather$Date, format='%Y'))
	weather$Monday <- as.Date(paste(weather$Year, weather$Week,'1',sep='-'), '%Y-%W-%w')

	rollingweather <- weather %>%
		group_by(.data$Year, .data$Week, .data$Monday) %>%
#		filter(.data$Year >= min_year, .data$Year <= max_year) %>%
		filter(.data$Year >= min_year) %>%
		summarise %>%
		ungroup
	
	if(nrow(rollingweather) == 0){
#		stop('No weather data was found between specified years ', min_year, ' and ', max_year)
		stop('No weather data was found after specified year ', min_year)
	}
	
	rollingweather$Temp_low_4 <- NA
	rollingweather$Temp_low_8 <- NA
	rollingweather$Temp_low_12 <- NA
	rollingweather$Temp_low_16 <- NA
	rollingweather$Temp_low_20 <- NA
	rollingweather$Temp_low_24 <- NA
	rollingweather$Temp_frost_4 <- NA
	rollingweather$Temp_frost_8 <- NA
	rollingweather$Temp_frost_12 <- NA
	rollingweather$Temp_frost_16 <- NA
	rollingweather$Temp_frost_20 <- NA
	rollingweather$Temp_frost_24 <- NA
	rollingweather$Temp_avg_4 <- NA
	rollingweather$Temp_avg_8 <- NA
	rollingweather$Temp_avg_12 <- NA
	rollingweather$Temp_avg_16 <- NA
	rollingweather$Temp_avg_20 <- NA
	rollingweather$Temp_avg_24 <- NA
	rollingweather$RelHumid_4 <- NA
	rollingweather$RelHumid_8 <- NA
	rollingweather$RelHumid_12 <- NA
	rollingweather$RelHumid_16 <- NA
	rollingweather$RelHumid_20 <- NA
	rollingweather$RelHumid_24 <- NA
	rollingweather$AbsHumid_4 <- NA
	rollingweather$AbsHumid_8 <- NA
	rollingweather$AbsHumid_12 <- NA
	rollingweather$AbsHumid_16 <- NA
	rollingweather$AbsHumid_20 <- NA
	rollingweather$AbsHumid_24 <- NA

	misswarn <- FALSE
	omitwarn <- FALSE

	pb <- txtProgressBar(style=3)
	for(i in 1:nrow(rollingweather)){

		# Check that the required number of weeks of weather data are available:
		tdates <- rollingweather$Monday[i] - 1:(24*7)
		dat <- weather %>% filter(.data$Date %in% tdates) %>%
			select(.data$Temp_avg, .data$Temp_low, .data$Rel_Humidity_avg, .data$Abs_Humidity_avg, .data$Temp_avg)
		if(nrow(dat)!=168 && !omitwarn){
			warning('One or more required days was not found in the weather data: some rolling averages will be NA')
			omitwarn <- TRUE			
		}
		if(any(is.na(dat)) && !misswarn){
			warning('One or more missing observations in the weather data: some rolling averages will be NA')
			misswarn <- TRUE
		}

		# Relative to absolute humidity:
		# https://carnotcycle.wordpress.com/2012/08/04/how-to-convert-relative-humidity-to-absolute-humidity/
		# Absolute Humidity (grams/m3) = (6.112 x e^[(17.67 x T)/(T+243.5)] x rh x 2.1674) / (273.15+T)
		# R code to calculate abs humidity from relative humidity (rh) and temperature (tp):
		# (6.112 * exp((17.67*tp)/(tp+243.5)) * rh * 2.1674) / (273.15 + tp)
		
		# Weather weeks work like this:
		# Temp_avg_0:  1-4 weeks ago
		# Temp_avg_4:  5-8 weeks ago
		# Temp_avg_8:  9-12 weeks ago
		# etc
		# So Temp_avg_4 is the nearest that can be used for prediction 4 weeks into the future
		
		for(sw in seq(0,24,by=4)){
			tdates <- rollingweather$Monday[i] - ((sw*7)+1):((sw+4)*7)
			stopifnot(length(tdates)==28)
			dat <- weather %>% filter(.data$Date %in% tdates) %>%
				select(.data$Temp_avg, .data$Temp_low, .data$Rel_Humidity_avg, .data$Abs_Humidity_avg, .data$Temp_avg)
			if(nrow(dat)==28){
				rollingweather[[paste0('Temp_avg_',sw)]][i] <- mean(dat$Temp_avg)
				rollingweather[[paste0('Temp_low_',sw)]][i] <- min(dat$Temp_low)
				rollingweather[[paste0('Temp_frost_',sw)]][i] <- sum(dat$Temp_low < 0)
				rollingweather[[paste0('RelHumid_',sw)]][i] <- mean(dat$Rel_Humidity_avg)
				rollingweather[[paste0('AbsHumid_',sw)]][i] <- mean(dat$Abs_Humidity_avg)
			}
		}

		setTxtProgressBar(pb, i/nrow(rollingweather))
	}
	close(pb)

	rollingweather$Year <- as.character(rollingweather$Year)

	return(rollingweather)
}
