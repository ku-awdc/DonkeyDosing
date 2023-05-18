dosing_sheet <- function(year, farm, excel_file, excel_sheet, all_locations, silent=FALSE){

	if(!is.numeric(year) || length(year)!=1 || year < 2000 || year > 3000)
		stop('The specified year argument must be numeric of length 1 (in the range 2000-3000)')

	if(!is.character(farm) || length(farm)!=1)
		stop('The specified farm argument must be character of length 1')

	if(!is.character(excel_file) || length(excel_file)!=1 || !( grepl('\\.xlsx$', excel_file) || grepl('\\.xls$', excel_file) ))
		stop('The specified excel_file argument must be a length 1 character string specifying a file with a .xlsx or .xls file extension')

	lmatch <- all_locations %>% filter(.data$Farm==farm, .data$Year==year)
	if(nrow(lmatch)==0)
		stop(paste0('Location data for the farm "', farm, '" and year ', year, ' is not available - import the locations first using AddLocations'))

	# First check that column names are unique:
	dnames <- gsub(' ', '', as.character(read_excel(excel_file, excel_sheet, col_types='text', col_names=FALSE, .name_repair='minimal')[1,]))
	# Note: .name_repair='minimal' requires tibble version >=2
	if(any(table(dnames)>1)){
		tt <- table(dnames)>1
		stop(paste(paste0('Duplicated column name for ', farm, ' ', year, ': ', names(tt)[tt]), collapse='\n'))
	}
	# Then try to recover average fec:
	possav <- grepl('av',tolower(dnames)) & grepl('fec',tolower(dnames))
	if(sum(possav)==1){
		dnames[possav] <- 'AvFEC'
	}else if(!'AvFEC' %in% dnames){
		stop('A column with name AvFEC (representing the previous years average FEC) was not found')
	}
	dnames[is.na(dnames)] <- paste0('blank', 1:sum(is.na(dnames)))

	# Then read the data and remove spaces from column names:
	data <- read_excel(excel_file, sheet=excel_sheet, col_types='text', col_names=TRUE, .name_repair='minimal')
	names(data) <- dnames

	# The first 7 column names must the following (even though ClinicalRisk is not available pre-2018):
	if(!all(c('Name','Tag','Sex','Age','Location','ClinicalRisk','AvFEC') == names(data)[1:7]))
		stop('One or more of the 7 required column names (Name, Tag, Sex, Age, Location, ClinicalRisk, AvFEC) is missing or in the wrong order')


	# In Excel the 1st Jan is always in week 1 and weeks start on Sunday by default
	# iCal in UK spec uses ISO 8601 which is complicated and depends if 1st is after thursday
	#	The same as %V for strftime
	# Should probably stick to a system where weeks start on Monday and 1st jan is in week 1

	# See if a week numbering offset is needed for R to parse the week number:
	first_fec_date <- which(grepl('^F[[:digit:]]',names(data)) & !is.na(data[1,]) & !data[1,]=='')
	if(length(first_fec_date)==0){
		stop(paste0('No reference/check date provided for farm ', farm, ' and year ', year, ': ensure that the second row of the Excel sheet contains at least one date corresponding to the week number, and that this row is formatted as a date in Excel'))
	}
	first_fec_date <- first_fec_date[1]
	wk <- as.numeric(gsub('[[:alpha:]]','',names(data)[first_fec_date]))
	dt <- as.Date('1899-12-30')+as.numeric(data[1, first_fec_date])
	if(is.na(dt)){
		stop(paste0('Unable to read the reference/check date provided for the ', names(data)[first_fec_date], ' column for farm ', farm, ' and year ', year, ': ensure that the second row of the Excel sheet contains at least one date corresponding to the week number, and that this row is formatted as a date in Excel'))
	}
	rwk <- as.numeric(strftime(dt, '%W'))

	week_offset <- wk - rwk
	if((year %in% 2014:2015 && week_offset!=1) || (year %in% 2016:2017 && week_offset!=0))
		warning('Possible week offset problem')

	# Retain the raw data for use when creating next years' sheet:
	rawdata <- data %>% slice(-1)

	# Remove the first row (dates) and any white space at the bottom:
	data <- data %>%
		slice(-1) %>%
		filter(!is.na(.data$Name) | !is.na(.data$Tag))

	# The columns that cannot be blank:
	if(any(is.na(data$Name)))
		stop('One or more blank entry in the Name column was found')
	if(any(is.na(data$Sex)))
		stop('One or more blank entry in the Sex column was found')
	if(any(is.na(data$Age)))
		stop('One or more blank entry in the Age column was found')
	if(any(is.na(data$Location)))
		stop('One or more blank entry in the Location column was found')

	# Find locations that aren't listed in the locations sheet:
	data$Field <- data$Location
	data$Location <- gsub('[[:space:]]','',paste(farm,data$Location,sep='_'))
	nolocmatch <- data %>% filter(!.data$Location %in% lmatch$Location)
	if(nrow(nolocmatch)>0){
		nani <- nrow(nolocmatch)
		nolocmatch <- nolocmatch %>% group_by(.data$Field) %>% tally
		if(!silent){
  		cat('\tNote: A total of ', nani, ' animals at the following invalid locations will not be used in the model:\n', sep='')
  		for(r in seq_len(nrow(nolocmatch))){
  			cat('\t\t', nolocmatch$Field[r], ' (N=', nolocmatch$n[r], ')\n', sep='')
  		}
		}
	}

	data$Location[! data$Location %in% lmatch$Location] <- NA
	if(sum(!is.na(data$Location))==0){
	  stop('No rows with valid locations were found in the data')
	}

	data$Age[data$Age %in% c('<1')] <- 0
	numconv <- as.numeric(data$Age)
	if(any(is.na(numconv))){
		stop(paste0('Error converting the following age into a number: ', data$Age[which(is.na(numconv))][1]))
	}
	data$Age <- numconv
	data$AgeCat <- cut(data$Age, breaks=c(-1,5.5,10.5,20.5,100), labels=c('Age_0-5', 'Age_6-10', 'Age_11-20', 'Age_21+'))

	numconv <- as.numeric(data$AvFEC)
	if(any(is.na(numconv) & data$AvFEC!='' & !is.na(data$AvFEC))){
		stop(paste0('Error converting the following average FEC into a number: ', data$AvFEC[which(is.na(numconv) & data$AvFEC!='' & !is.na(data$AvFEC))][1]))
	}
	data$AvFEC <- numconv
	data$AvFEC[data$AvFEC=='' | is.na(data$AvFEC)] <- NA

	data$Sex <- toupper(gsub(' ','',data$Sex))
	sexchange <- dosing_getOption('allsexes')
	if(any(! data$Sex %in% names(sexchange))){
		stop(paste0('Unrecognised Sex code: ', data$Sex[which(!data$Sex %in% names(sexchange))][1]))
	}
	data$Sex <- factor(sexchange[data$Sex], levels=c('DKF','DKM','MUPY'))

	data$ClinicalRisk <- tolower(gsub(' ','',data$ClinicalRisk))
	if(any(is.na(data$ClinicalRisk))){
		cat('\tNote: Blank/missing ClinicalRisk code for ', sum(is.na(data$ClinicalRisk)), ' animals including ', data$Name[is.na(data$ClinicalRisk)][1], ': setting these to Normal\n', sep='')
		data$ClinicalRisk[is.na(data$ClinicalRisk)] <- 'Normal'
	}
	if(any(! tolower(gsub(' ','',data$ClinicalRisk)) %in% c('normal','high','veryhigh'))){
		stop('Unrecognised ClinicalRisk code: ', data$ClinicalRisk[which(! tolower(gsub(' ','',data$ClinicalRisk)) %in% c('normal','high','veryhigh'))][1])
	}
	data$ClinicalRisk <- factor(tolower(gsub(' ','',data$ClinicalRisk)), levels=c('normal','high','veryhigh'), labels=c('Normal','High','VeryHigh'))

	data$Year <- as.character(year)
	data$AnimalID <- paste(farm,data$Name,sep='_')
	data$Farm <- farm
	data$Tag <- as.character(data$Tag)
	nt <- paste(data$Name, data$Tag)
	if(any(table(nt)>1)){
		tt <- table(nt)>1
		stop(paste(paste0('Duplicated Name/Tag for ', farm, ' ', year, ': ', names(tt)[tt]), collapse='\n'))
	}
	data <- merge(data, all_locations, all.x=TRUE)

	allanimals <- as.data.frame(data %>% select(.data$Year, .data$Farm, .data$Location, .data$Field, .data$AnimalID, .data$Tag, .data$Sex, .data$Age, .data$AgeCat, .data$AvFEC, .data$ClinicalRisk)) %>%
		filter(!is.na(.data$AnimalID), !is.na(.data$Farm), !is.na(.data$Year))


	sensitivity <- lmatch$Sensitivity
	names(sensitivity) <- lmatch$Location
	fecdata <- data %>% select(.data$AnimalID, .data$Location, .data$Year, matches('^F[[:digit:]]'), matches('^T[[:digit:]]')) %>%
		gather("Observation", "Value", -.data$AnimalID, -.data$Location, -.data$Year) %>%
		matchweek(sensitivity, week_offset, errstr=paste(farm,year)) %>%
		ungroup()

	allfec <- as.data.frame(fecdata) %>% filter(!is.na(.data$AnimalID), !is.na(.data$Year))

	# Re-create dates as the monday of the relevant week, using the date of Monday of week 1 as a fixed reference:
	if(any(allfec$Year != year)) stop(paste0('Inconsistent year detected for ', farm, ' ', year))
	firstmon <- as.Date(paste0(year,'-1-1',sep='-'), '%Y-%W-%w')
	allfec$Monday <- firstmon + (allfec$Week-1)*7
	stopifnot(all(!is.na(allfec$Monday)))

	# And calculate the time of last dosing:
	# The fast way:
	ldf <- function(mondays, dosed) sapply(mondays, function(m) ifelse(any(dosed[mondays < m]), as.numeric(difftime(m, max(mondays[mondays < m & dosed]), units='weeks')), Inf))
	allfec <- allfec %>% arrange(.data$AnimalID, .data$Monday) %>% group_by(.data$AnimalID) %>% mutate(LastDose = ldf(.data$Monday, .data$Dose)) %>% ungroup

	# The slow way - disabled:
	if(FALSE){
		allfec$LastDoseTest <- Inf
		pb <- txtProgressBar(style=3)
		for(i in 1:nrow(allfec)){
			anidat <- allfec %>%
				filter(.data$AnimalID == allfec$AnimalID[i], .data$Monday < allfec$Monday[i], Dose==TRUE) %>%
				arrange(desc(.data$Monday))
			if(nrow(anidat)>0){
				allfec$LastDoseTest[i] <- as.numeric(difftime(allfec$Monday[i], anidat$Monday[1], units='weeks'))
			}
			setTxtProgressBar(pb, i/nrow(allfec))
		}
		close(pb)

		stopifnot(all(allfec$LastDose == allfec$LastDoseTest))
		allfec$LastDoseTest <- NULL
	}

	return(list(allanimals=allanimals, allfec=allfec, rawdata=rawdata))
}
