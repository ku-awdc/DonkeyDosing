library('tidyverse')
options(stringsAsFactors=FALSE)

# This file is just used to simulate data and does not need to be tested - it is just kept here for prosperity

if(FALSE){

set.seed(2018-12-08)

locations <- tribble(
~Farm, ~Location, ~Hygiene,
"Fruits", "Apples", "None",
"Fruits", "Pears", "Manual",
"Fruits", "Grapes", "Automated",
"Vegetables", "Carrots", "None",
"Vegetables", "Parsnips", "Manual",
"Vegetables", "Turnips", "Automated",
"Vegetables", "Potatos", "None",
"Dairy", "Milk", "Manual",
"Dairy", "Cheese", "Manual"
)
locations$Sensitivity <- 50

stopifnot(all(locations$Hygiene %in% c('None','Automated','Manual')))

# Generate sampling weeks (for simplicity it is 100% sampling every 4 weeks for all locations):
startweek <- 12
weeks <- seq(startweek, startweek+36, length=10)
stopifnot(all((weeks %% 1) == 0))

# Generate means per week:
means <- exp(6.5 + 0.5*poly(weeks,2)[,1] + -1*poly(weeks,2)[,2])
means
# And cleaning effects:
cleaning <- c(None=1, Automated=0.8, Manual=0.6)

# Simulate minimum 2 years:
years <- 2001:2002

# Need to have consistent animals present at each location:
allanimals <- lapply(seq_len(nrow(locations)), function(row){

	# Generate between 21-121 animals (with gibberish names) per location:
	Nani <- sample.int(100, 1)+20
	newdata <- data.frame(Farm=locations$Farm[row],
		Name = sapply(1:Nani, function(n) paste(letters[sample.int(26,10,replace=TRUE)], collapse='')),
		Tag = sapply(1:Nani, function(n) paste0('T',paste((1:9)[sample.int(9,6,replace=TRUE)], collapse=''))),
		Sex = sample(c('DKF','DKM','POG'), Nani, replace=TRUE, prob=c(0.45,0.45,0.1)),
		Age = sample.int(30, Nani, replace=TRUE),
		Location = locations$Location[row]
	)

	return(newdata)
})

if(!file.exists('simdata')){
	dir.create('simdata')
}
for(y in seq_along(years)){
	alldata <- lapply(seq_len(nrow(locations)), function(row){
	
		# Retrieve animal data:
		newdata <- allanimals[[row]]
		# Subsample some animals so we have some missing previous FEC:
		Nani <- rbinom(1, nrow(newdata), prob=0.975)
		newdata <- newdata[sort(sample.int(nrow(newdata), Nani)),]
		# Increment age by year:
		newdata$Age <- newdata$Age + y
		newdata$ClinicalRisk <- sample(c('Normal','High','Very High'), Nani, replace=TRUE, prob=c(0.8,0.15,0.05))
		# Sample randomly - doesn't matter:
		newdata$AvFEC <- round(rnorm(Nani,750,250))
		newdata$Notes <- ""
	
		# Simulate FEC data (almost) randomly:
		fecdata <- vector('list', length=length(weeks))
		for(w in seq_along(weeks)){
			fec <- rnbinom(Nani, size=1, mu=means[w]/50 * cleaning[locations$Hygiene[row]])*50
			dose <- ifelse(fec >= 2000, 'DOSE', '')
			fecdata[[w]] <- data.frame(fec=fec, dose=dose)
			names(fecdata[[w]]) <- paste0(c('F','T'), weeks[w])
		}
	
		return(cbind(newdata, do.call('cbind', fecdata)))
	})
	# A blank line is needed with dates under FEC week numbers:
	# Date of the wednesday for relevant week number:
	year <- years[y]
	wdate <- matrix('', nrow=2, ncol=length(weeks))
	wdate[1,] <- as.character(as.Date(paste0(year,'-', weeks, '-3'), format='%Y-%W-%u'))
	dim(wdate) <- NULL
	blank <- as.data.frame(matrix(c(rep('',9), wdate), nrow=1, dimnames=list(NULL, names(alldata[[1]]))))
	stopifnot(ncol(blank)==ncol(alldata[[1]]))
	alldata <- do.call('rbind', c(list(blank), alldata))
	
	# Visualise distribution of FEC:
	suppressWarnings(fecs <- as.numeric(as.matrix(alldata)))
	plot.ecdf(fecs)
	abline(v=quantile(fecs, probs=c(0.025,0.5,0.975), na.rm=TRUE))
	
	# Save the data by farm:
	for(farm in unique(locations$Farm)){
		towrite <- alldata %>% filter(Farm%in%c('',farm)) %>% select(-Farm)
		if(!all(c('Name','Tag','Sex','Age','Location','ClinicalRisk','AvFEC') == names(towrite)[1:7]))
			stop('One or more of the 7 required column names (Name, Tag, Sex, Age, Location, ClinicalRisk, AvFEC) is missing or in the wrong order')
		write.csv(towrite, file=paste0('simdata/', farm, '_', year, '.csv'), row.names=FALSE)
	}
}


# Generate some random daily weather for a 5-year period:
weather <- data.frame(Date=as.Date('2000-01-01')+(0:1826)) %>%
	mutate(Year = as.numeric(strftime(Date, format='%Y')), Week=as.numeric(strftime(Date, format='%W')),
		WeekDay=as.numeric(strftime(Date, format='%u')), Month=as.numeric(strftime(Date, format='%m')),
		Day=as.numeric(strftime(Date, format='%d')), Temp_high=runif(n(), -5, 30), Temp_avg=runif(n(), -5, 30), Temp_low=runif(n(),-15,20), Rel_Humidity_avg=runif(n(),60,100), Abs_Humidity_avg=runif(n(),60,100))
write.csv(weather, file='simdata/Weather.csv', row.names=FALSE)

}


cat('simulate_data.R script does not need testing\n')
