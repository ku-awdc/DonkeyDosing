#' @importFrom lme4 fixef ranef refit
#' @importFrom lmerTest lmer
#' @importFrom MASS glm.nb
#' @importFrom broom tidy
#' @importFrom testthat capture_warnings
#' @importFrom scales date_format
#' @importFrom stats predict coef lm relevel resid sd update model.frame pnorm
#' @importFrom utils capture.output write.table write.csv tail
#' @import dplyr
#' @import ggplot2

get_prediction_models <- function(gpdata, allfec, rollingweather, group_model, expected_efficacy, min_N, min_week, max_week, maxiters=1000, tol=10^-3, txtime=0, txrm=FALSE, lastyear=TRUE, individual_intercept=FALSE, resid_doy_interaction=FALSE, common_re=TRUE){

	# maxiters refers to iterating between individual and residual model fits
	# tol is the biggest difference in successive iterative fits that is used for convergence
	# txtime is an interaction between previous FEC effect and dosing just before that FEC - hard to interpret results
	# lastyear is an option to use the last year average FEC or not
	# individual_intercept adds an individual-level intercept to the various individual model, with FALSE these are removed and the only intercept is the residual model fit
	# common_re adds a random effect of individual animal to the common model, otherwise it is removed so repeated observations from the same animal are assumed independent

	# These are the only txtime currently implemented (and needed):
	stopifnot(txtime %in% c(0,8,12))

	## Check that factor levels are in agreement:
	for(column in c('Year','Farm','Location','Hygiene'))
		stopifnot(identical(levels(gpdata[[column]]), levels(allfec[[column]])))

	# Re-level Farm, sex and age category to the most numerous observations:
	stopifnot(is.factor(allfec$Farm))
	ml <- as.character((allfec %>% group_by(.data$Farm) %>% tally %>% arrange(desc(.data$n)) %>% slice(1))$Farm)
	stopifnot(!is.na(ml))
	allfec$Farm <- relevel(allfec$Farm, ml)

	stopifnot(is.factor(allfec$Sex))
	ml <- as.character((allfec %>% group_by(.data$Sex) %>% tally %>% arrange(desc(.data$n)) %>% slice(1))$Sex)
	stopifnot(!is.na(ml))
	allfec$Sex <- relevel(allfec$Sex, ml)

	stopifnot(is.factor(allfec$AgeCat))
	ml <- as.character((allfec %>% group_by(.data$AgeCat) %>% tally %>% arrange(desc(.data$n)) %>% slice(1))$AgeCat)
	stopifnot(!is.na(ml))
	allfec$AgeCat <- relevel(allfec$AgeCat, ml)

	red <- 1 - expected_efficacy

	## Group seasonality curve is based on the full data but group means
	## Also includes weather data and proportion dosed 4,8,12 weeks ago
	## Only run the model for March to Nov as otherwise contamination not an issue
	## (and Mox treatment in Dec/Jan may not be universally recorded in earlier years)

	gpmoddat <- gpdata %>% filter(.data$TotalFEC >= min_N)
	yrs <- gpdata %>% count(as.numeric(as.character(.data$Year)))
	cat('Fitting the group model to ', nrow(gpmoddat), ' average FEC observations from a minimum of ', min_N, ' animals for ', length(yrs), ' years of data between ', min(yrs), ' and ', max(yrs), '...\n', sep='')
	# I get a false positive note:  group_predictions: no visible binding for global variable ‘NewTotalFEC’
	gpmoddat$NewTotalFEC <- gpmoddat$TotalFEC
	NewTotalFEC <- 'this is just to mute the R CMD check NOTE'
	# No idea why - it is a false positive

	gpmodel <- lmer(group_model, data=gpmoddat, weights=NewTotalFEC)

	#### Need to drop each farm/year in sequence and make predictions for that farm/year from the others
	allyearfarms <- gpdata %>%
		group_by(.data$Farm, .data$Year) %>%
		summarise() %>%
		ungroup()

	# Loop through with sequential exclusion:
	predicted <- rep(FALSE, nrow(allfec))
	allmods <- vector('list', length=nrow(allyearfarms))

	# Predictions including weather:
	fakedata1 <- expand.grid(LocationYear = '0', Location = '0', Week = 0:52, Year=unique(gpdata$Year), DoseProp4=0, DoseProp8=0, DoseProp12=0, Hygiene=unique(gpdata$Hygiene), MeanAge=15, TotalAnimals=mean(gpdata$TotalAnimals), Farm=unique(gpdata$Farm)) %>%
		mutate(sdoy = sin(2*pi*.data$Week/52), cdoy = cos(2*pi*.data$Week/52))
	fakedata1 <- fakedata1 %>% left_join(rollingweather, by = c("Week", "Year"))
	fakedata1$FarmHygiene <- paste(fakedata1$Farm, fakedata1$Hygiene, sep='_')
	fakedata1$FarmHygiene <- factor(fakedata1$FarmHygiene, levels=levels(gpdata$FarmHygiene))

	# Predictions time of year only:
	fakedata2 <- expand.grid(LocationYear = '0', Location = '0', Week = 0:52, Year=unique(gpdata$Year), DoseProp4=0, DoseProp8=0, DoseProp12=0, Hygiene=unique(gpdata$Hygiene), MeanAge=15, TotalAnimals=mean(gpdata$TotalAnimals), Farm=unique(gpdata$Farm)) %>%
		mutate(sdoy = sin(2*pi*.data$Week/52), cdoy = cos(2*pi*.data$Week/52))
	fakedata2 <- fakedata2 %>% left_join(rollingweather %>% select(.data$Week, .data$Year, .data$Monday), by = c("Week", "Year"))
	fakedata2$FarmHygiene <- paste(fakedata2$Farm, fakedata2$Hygiene, sep='_')
	fakedata2$FarmHygiene <- factor(fakedata2$FarmHygiene, levels=levels(gpdata$FarmHygiene))
	# Add all weather info as the average:
	cols <- names(gpdata %>% select(starts_with('Temp'), starts_with('RelHumid'), starts_with('AbsHumid')))
	for(col in cols){
		fakedata2[[col]] <- mean(gpdata[[col]])
	}

	# Predicted vs observed:
	fakedata3 <- gpdata

	# Predictions from a single model, including weather:
	fakedata1$pred_single <- predict(gpmodel, newdata=fakedata1, allow.new.levels=TRUE)
	# Predictions from a single model, time of year only:
	fakedata2$pred_single <- predict(gpmodel, newdata=fakedata2, allow.new.levels=TRUE)

	cat('Refitting group models for each farm/year...\n')

	tfake1=tfake2=tfake3 <- vector('list', length=nrow(allyearfarms))
	predicted <- logical(nrow(allfec))
	allcoefs <- matrix(nrow=length(names(fixef(gpmodel))), ncol=nrow(allyearfarms), dimnames=list(names(fixef(gpmodel)), apply(allyearfarms,1,paste,collapse='_')))

	pb <- txtProgressBar(style=3)
	for(r in 1:nrow(allyearfarms)){

		tdata <- gpdata %>%
			filter(.data$Farm != allyearfarms$Farm[r] | .data$Year != allyearfarms$Year[r]) %>%
			filter(.data$TotalFEC >= min_N)
		stopifnot(all(!is.na(tdata$MeanAge)))

		# I get a false positive note:  group_predictions: no visible binding for global variable ‘NewTotalFEC’
		tdata$NewTotalFEC <- tdata$TotalFEC
		NewTotalFEC <- 'this is just to mute the R CMD check NOTE'
		# No idea why - it is a false positive
		out <- capture.output(allmods[[r]] <- lmer(group_model, data=tdata, weights=NewTotalFEC), type='message')
		if(any(!grepl('fixed-effect model matrix is rank deficient so dropping', out))){
			warning(out[!grepl('fixed-effect model matrix is rank deficient so dropping', out)])
		}

		allcoefs[names(fixef(allmods[[r]])),r] <- fixef(allmods[[r]])

		repindex <- allfec$Farm == allyearfarms$Farm[r] & allfec$Year == allyearfarms$Year[r]
		stopifnot(nrow(repindex)>0)
		stopifnot(all(! (repindex & predicted )))
		predicted[repindex] <- TRUE

		tdata <- allfec[repindex,]
		tdata$Location <- '0'
		tdata$LocationYear <- '0'
		allfec$GroupPredictionIncTx[repindex] <- predict(allmods[[r]], newdata=tdata, allow.new.levels=TRUE)
		tdata$DoseProp4 <- 0
		tdata$DoseProp8 <- 0
		tdata$DoseProp12 <- 0
		allfec$GroupPredictionNoTx[repindex] <- predict(allmods[[r]], newdata=tdata, allow.new.levels=TRUE)

		tfake1[[r]] <- fakedata1 %>%
			filter(.data$Farm == allyearfarms$Farm[r] & .data$Year == allyearfarms$Year[r])
		tfake1[[r]]$pred_exclusion <- predict(allmods[[r]], newdata=tfake1[[r]], allow.new.levels=TRUE)

		tfake2[[r]] <- fakedata2 %>%
			filter(.data$Farm == allyearfarms$Farm[r] & .data$Year == allyearfarms$Year[r])
		tfake2[[r]]$pred_exclusion <- predict(allmods[[r]], newdata=tfake2[[r]], allow.new.levels=TRUE)

		tfake3[[r]] <- fakedata3 %>%
			filter(.data$Farm == allyearfarms$Farm[r] & .data$Year == allyearfarms$Year[r])
		tfake3[[r]]$pred_exclusion <- predict(allmods[[r]], newdata=tfake3[[r]], allow.new.levels=TRUE)

		setTxtProgressBar(pb, r/nrow(allyearfarms))
	}
	close(pb)

	stopifnot(all(predicted))
	fecpred <- allfec %>% select("Year", "Week", "Monday", "AnimalID", "Location", "FEC", "Count", "Dose", "LastDose", "Farm", "Hygiene", "Tag", "Sex", "Age", "AgeCat", "GroupPredictionIncTx", "GroupPredictionNoTx")

	fakedata1 <- do.call('rbind', tfake1)
	fakedata2 <- do.call('rbind', tfake2)
	fakedata3 <- do.call('rbind', tfake3)

	fakedata1$PredExclusion <- exp(fakedata1$pred_exclusion)
	fakedata1$PredSingle <- exp(fakedata1$pred_single)
	fakedata2$PredExclusion <- exp(fakedata2$pred_exclusion)
	fakedata2$PredSingle <- exp(fakedata2$pred_single)
	fakedata3$PredExclusion <- exp(fakedata3$pred_exclusion)

	exclusionplot1 <- ggplot(fakedata1, aes(x=.data[["Week"]], y=.data[["PredExclusion"]], col=.data[["Hygiene"]])) +
		geom_line() +
		facet_grid(.data$Year~.data$Farm) +
		ylim(0,1500)

	exclusionplot2 <- ggplot(fakedata2, aes(x=.data[["Week"]], y=.data[["PredExclusion"]], col=.data[["Hygiene"]])) +
		geom_line() +
		facet_grid(.data$Year~.data$Farm) +
		ylim(0,1500)

	## For saving for clinical use:
	group_data_output <- list(pred=fakedata1, obs=fakedata3, model=gpmodel)

	## Far too many panels:
	if(FALSE){
		meanfec <- allfec %>%
			group_by(.data$Week, .data$Location, .data$Year) %>%
			summarise(meanLogFEC = mean(log(.data$FEC+1))) %>%
			ungroup
		exclusionplot3 <- ggplot(fakedata3, aes(x=.data[["Week"]], y=.data[["PredExclusion"]], col=.data[["Year"]], group=.data[["Year"]])) +
			geom_line() +
			facet_wrap(~Location) +
			geom_point(aes(x=Week, y=exp(meanLogFEC)), meanfec) +
			ylim(0,1500)
	}

	combinedplot1 <- ggplot(fakedata1, aes(x=.data[["Week"]], y=.data[["PredSingle"]], col=.data[["Hygiene"]])) +
		geom_line() +
		facet_grid(.data$Year~.data$Farm) +
		ylim(0,1500) +
		xlim(13, 47)

	combinedplot2 <- ggplot(fakedata2, aes(x=.data[["Week"]], y=.data[["PredSingle"]], col=.data[["Hygiene"]])) +
		geom_line() +
		facet_grid(.data$Year~.data$Farm) +
		ylim(0,1500) +
		xlim(13, 47)


	if(FALSE){
		browser()
		# TOOD: Incorporate Code needed to produce graphs etc:

		theme_set(theme_light())
		fakedata1 <- fakedata1 %>%
			mutate(FakeDate = as.Date(gsub('^[[:digit:]]*\\-','2001-',strftime(.data$Monday))))
		fakedata2 <- fakedata2 %>%
			mutate(FakeDate = as.Date(gsub('^[[:digit:]]*\\-','2001-',strftime(.data$Monday))))
		fakedata3 <- fakedata3 %>%
			mutate(FakeDate = as.Date(gsub('^[[:digit:]]*\\-','2001-',strftime(.data$Monday))))

		ggplot(fakedata1 %>% filter(.data$Week >= min_week, .data$Week <= max_week), aes(x=FakeDate, y=PredSingle, col=Hygiene)) +
			geom_line() +
			facet_grid(.data$Year~.data$Farm) +
			ylim(0,1500) +
			scale_x_date(date_breaks='1 month', labels = date_format("%B")) +
			theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
			ylab('Predicted Group Mean FWEC') + xlab('')
		ggsave('groupplot1.pdf', width=12, height=8)

		# Note: this is based on iteratively dropping farm/year so effects not necessarily identical

		ggplot(fakedata2 %>% filter(.data$Week >= min_week, .data$Week <= max_week), aes(x=FakeDate, y=PredSingle, col=Hygiene)) +
			geom_line() +
			facet_grid(.data$Year~.data$Farm) +
			ylim(0,1500) +
			scale_x_date(date_breaks='1 month', labels = date_format("%B")) +
			theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
			ylab('Predicted Group Mean FWEC') + xlab('')
		ggsave('groupplot2.pdf', width=12, height=8)

		#ggplot(fakedata3, aes(x=Week, y=exp(pred), col=Location, group=Location)) +
		#	geom_point(pch=3) +
		#	facet_grid(Year~Farm) +
		#	geom_point(aes(x=Week, y=exp(meanLogFEC), col=Location, group=Location), gpdata) +
		#	ylim(0,1500) +
		#	xlim(min_week, max_week) +
		#	theme(legend.pos='none')

		#ggplot(fakedata3, aes_string(x=Week, y=PredSingle, col="Year", group= "Year")) +
		#	geom_point(pch=3) +
		#	facet_wrap(~Location) +
		#	geom_point(aes(x=Week, y=exp(meanLogFEC), col= Year, group= Year), gpdata) +
		#	ylim(0,1500) +
		#	xlim(min_week, max_week) +
		#	theme(legend.pos='below')

			ranef(gpmodel)
			summary(gpmodel)
			fixedef <- as.data.frame(summary(gpmodel)$coefficients) %>%
				rownames_to_column %>%
				select(Variable=.data$rowname, Estimate, P.value='Pr(>|t|)')
			ranef <- suppressWarnings(tidy(refit(gpmodel))) %>%
				filter(group!='fixed') %>%
				select(Variable=.data$group, Estimate=.data$estimate) %>%
				mutate(Variable=paste0('Random: ', .data$Variable), P.value=NA)
			combef <- rbind(fixedef, ranef) %>%
				mutate(Estimate = format(round(.data$Estimate, 3)), P.value=format(round(.data$P.value,3))) %>%
				mutate(P.value=ifelse(.data$P.value=='0.000','<0.001',.data$P.value))
			write.csv(combef, row.names=FALSE, file='groupmodel_effects.csv')

			extremes <- ranef(gpmodel)$LocationYear %>%
				rownames_to_column %>%
				select(.data$rowname, Estimate='(Intercept)') %>%
				separate(rowname,c('Farm','Location','Year'),'_') %>%
				mutate(Estimate=format(round(.data$Estimate,2))) %>%
				spread(.data$Year, .data$Estimate, fill='')
			write.csv(extremes, row.names=FALSE, file='extremes.csv')

	}


	#### Individual model from here

	cat('Calculating individual covariates...\n')

	gpmeans <- fecpred %>%
		group_by(.data$Year, .data$Week, .data$Monday, .data$Farm, .data$Location) %>%
		summarise(GroupMean = mean(log(.data$FEC+1)), GroupPrediction = mean(.data$GroupPredictionIncTx), NFEC = n()) %>%
		ungroup

	farmloc <- gpmeans %>% group_by(.data$Farm, .data$Location) %>% summarise %>% ungroup
	allweeks <- expand.grid(Monday=min(gpmeans$Monday)+seq(0,7*52*4,by=7), Location=unique(farmloc$Location)) %>%
		mutate(Year = factor(strftime(.data$Monday, format='%Y'), levels=levels(gpmeans$Year)),
			Week = as.numeric(strftime(.data$Monday, format='%W'))) %>%
		full_join(farmloc, by='Location')

	weightmean <- function(GroupPrediction, NFEC, lags){
		value_matrix <- vapply(lags, function(x) lag(GroupPrediction, x), numeric(length(GroupPrediction)))
		n_matrix <- vapply(lags, function(x) lag(NFEC, x), numeric(length(NFEC)))
		wmean <- apply(value_matrix * n_matrix, 1, sum, na.rm=TRUE) / apply(n_matrix, 1, sum, na.rm=TRUE)
		wmean[is.nan(wmean)] <- NA
		return(cbind(wmean, pmax(0, apply(n_matrix, 1, sum, na.rm=TRUE), na.rm=TRUE)))
	}

	# Location mean with a tolerance:
	locmeans <- merge(gpmeans, allweeks, all=TRUE) %>%
		mutate(NFEC = ifelse(is.na(.data$NFEC), 0, .data$NFEC)) %>%
		group_by(.data$Location) %>%
		arrange(.data$Monday) %>%
		mutate(loc_pred_4wk = weightmean(.data$GroupPrediction, .data$NFEC, 3:5)[,1], loc_n_4wk = weightmean(.data$GroupPrediction, .data$NFEC, 3:5)[,2]) %>%
		mutate(loc_obs_4wk = weightmean(.data$GroupMean, .data$NFEC, 3:5)[,1]) %>%
		mutate(loc_pred_8wk = weightmean(.data$GroupPrediction, .data$NFEC, 7:9)[,1], loc_n_8wk = weightmean(.data$GroupPrediction, .data$NFEC, 7:9)[,2]) %>%
		mutate(loc_obs_8wk = weightmean(.data$GroupMean, .data$NFEC, 7:9)[,1]) %>%
		mutate(loc_pred_12wk = weightmean(.data$GroupPrediction, .data$NFEC, 11:13)[,1], loc_n_12wk = weightmean(.data$GroupPrediction, .data$NFEC, 11:13)[,2]) %>%
		mutate(loc_obs_12wk = weightmean(.data$GroupMean, .data$NFEC, 11:13)[,1]) %>%
		ungroup() %>%
		select(-.data$GroupPrediction, -.data$GroupMean, -.data$NFEC)

	# Merge back into the main FEC:
	fecpred <- fecpred %>%
		left_join(locmeans, by = c("Year", "Week", "Monday", "Location", "Farm"))

	## Then get animal level info of previous FEC:
	allmeans <- allfec %>%
		group_by(.data$AnimalID, .data$Location, .data$Farm, .data$Year) %>%
		summarise(meanFEC = log(mean(.data$FEC, na.rm=TRUE)+1)) %>%
		filter(!is.na(.data$meanFEC)) %>%
		ungroup %>%
		group_by(.data$AnimalID) %>%
		arrange(.data$AnimalID, .data$Year) %>%
		mutate(lastYearMeanFEC = .data$meanFEC) %>%
		select(-.data$meanFEC) %>%
		ungroup() %>%
		mutate(Year=as.numeric(as.character(.data$Year))+1) %>%
		filter(.data$Year <= max(as.numeric(as.character(fecpred$Year)))) %>%
		mutate(Year = factor(.data$Year, levels=levels(allfec$Year)))


	totalyears <- length(levels(allfec$Year))
	allweeksani <- expand.grid(Monday=min(allfec$Monday)+seq(0,7*52*totalyears,by=7), AnimalID=unique(allfec$AnimalID)) %>%
		mutate(Year = factor(strftime(.data$Monday, format='%Y'), levels=levels(gpmeans$Year)),
			Week = as.numeric(strftime(.data$Monday, format='%W')))

	cat('Calculating historical FEC and dosing data at individual level...\n')
	prevobs <- merge(allfec, allweeksani, all=TRUE) %>%
		group_by(.data$AnimalID) %>%
		arrange(.data$AnimalID, .data$Monday) %>%
		mutate(fec4wk = apply(vapply(3:5, function(x) lag(log(.data$FEC+1), x), numeric(length(.data$FEC))), 1, mean, na.rm=TRUE)) %>%
		mutate(dose5_12wk = apply(vapply(5:12, function(x) lag(.data$Dose, x), logical(length(.data$Dose))), 1, any, na.rm=TRUE)) %>%
		mutate(dose5_16wk = apply(vapply(5:16, function(x) lag(.data$Dose, x), logical(length(.data$Dose))), 1, any, na.rm=TRUE)) %>%
		mutate(fec8wk = apply(vapply(7:9, function(x) lag(log(.data$FEC+1), x), numeric(length(.data$FEC))), 1, mean, na.rm=TRUE)) %>%
		mutate(dose9_16wk = apply(vapply(9:16, function(x) lag(.data$Dose, x), logical(length(.data$Dose))), 1, any, na.rm=TRUE)) %>%
		mutate(dose9_20wk = apply(vapply(9:20, function(x) lag(.data$Dose, x), logical(length(.data$Dose))), 1, any, na.rm=TRUE)) %>%
		mutate(fec12wk = apply(vapply(11:13, function(x) lag(log(.data$FEC+1), x), numeric(length(.data$FEC))), 1, mean, na.rm=TRUE)) %>%
		mutate(dose13_20wk = apply(vapply(13:20, function(x) lag(.data$Dose, x), logical(length(.data$Dose))), 1, any, na.rm=TRUE)) %>%
		mutate(dose13_24wk = apply(vapply(13:24, function(x) lag(.data$Dose, x), logical(length(.data$Dose))), 1, any, na.rm=TRUE)) %>%
		ungroup()

	if(!txrm){
		if(txtime==8){
			if(sum(prevobs$dose5_12wk)<5 || sum(!prevobs$dose5_12wk)<5){
				stop('Insufficient leverage in dose5_12wk to fit interaction of past treatment with past fec')
			}
			if(sum(prevobs$dose9_16wk)<5 || sum(!prevobs$dose9_16wk)<5){
				stop('Insufficient leverage in dose9_16wk to fit interaction of past treatment with past fec')
			}
			if(sum(prevobs$dose13_20wk)<5 || sum(!prevobs$dose13_20wk)<5){
				stop('Insufficient leverage in dose13_20wk to fit interaction of past treatment with past fec')
			}
		}else if(txtime==12){
			if(sum(prevobs$dose5_16wk)<5 || sum(!prevobs$dose5_16wk)<5){
				stop('Insufficient leverage in dose5_16wk to fit interaction of past treatment with past fec')
			}
			if(sum(prevobs$dose9_20wk)<5 || sum(!prevobs$dose9_20wk)<5){
				stop('Insufficient leverage in dose9_20wk to fit interaction of past treatment with past fec')
			}
			if(sum(prevobs$dose13_24wk)<5 || sum(!prevobs$dose13_24wk)<5){
				stop('Insufficient leverage in dose13_24wk to fit interaction of past treatment with past fec')
			}
		}else if(txtime!=0){
			stop('Unimplemented txtime in check')
		}
	}

	## Need to run the models in three parts:
	# First a common model for all (field, weather, etc) including intercept
	# Then a model to correct for previous residuals at group predictions depending on what is available on the location:  field 3-5 weeks ago, field 7-9 weeks ago, field 11-13 weeks ago, nothing (ignore location)
	# Then depending on what is available on the individual:  last years mean FEC, obs FEC 3-5, 7-9, 11-13
	# Then run both sets of models using offsets from the others and iterate until convergence

	fecpred <- merge(fecpred, prevobs, all.x=TRUE)
	fecpred <- merge(fecpred, allmeans, all.x=TRUE)

	fecpred <- fecpred %>%
		filter(!is.na(.data$GroupPredictionNoTx), !is.na(.data$FEC), !is.na(.data$Sex), !is.na(.data$AgeCat), !is.na(.data$LastDose))

	fecpred$Obs <- log(fecpred$FEC+1)


	### Run separate models, although some data will appear multiple times

	# First remove individual observations that can't be fit by any of the individual models:
	if(lastyear){
		whichnai <- with(fecpred, is.na(lastYearMeanFEC))
	}else{
		whichnai <- with(fecpred, is.na(fec4wk) & is.na(fec8wk) & is.na(fec12wk))
	}
	fecpred <- fecpred[!whichnai,]

	# Models actually fit all non-NA data - this just determines which are used for offsets for next model:
	which0i <- with(fecpred, !is.na(lastYearMeanFEC) & is.na(fec4wk) & is.na(fec8wk) & is.na(fec12wk))
	if(lastyear){
		which4i <- with(fecpred, !is.na(lastYearMeanFEC) & !is.na(fec4wk))
		which8i <- with(fecpred, !is.na(lastYearMeanFEC) & is.na(fec4wk) & !is.na(fec8wk))
		which12i <- with(fecpred, !is.na(lastYearMeanFEC) & is.na(fec4wk) & is.na(fec8wk) & !is.na(fec12wk))
	}else{
		which4i <- with(fecpred, !is.na(fec4wk))
		which8i <- with(fecpred, is.na(fec4wk) & !is.na(fec8wk))
		which12i <- with(fecpred, is.na(fec4wk) & is.na(fec8wk) & !is.na(fec12wk))
	}
	stopifnot(all((which0i + which4i + which8i + which12i) == 1))


	## Observations with 4-week previous group mean:
	which4 <- fecpred$loc_n_4wk >=5 & fecpred$farm_n_4wk >=5
	which8 <- fecpred$loc_n_8wk >=5 & fecpred$farm_n_8wk >=5 & !which4
	which12 <- fecpred$loc_n_12wk >=5 & fecpred$farm_n_12wk >=5 & !which4 & !which8
	which0 <- !which4 & !which8 & !which12
	stopifnot(all((which4+which8+which12+which0) == 1))

	fecpred$residual_location4 <- with(fecpred, loc_pred_4wk - loc_obs_4wk)
	fecpred$residual_location8 <- with(fecpred, loc_pred_8wk - loc_obs_8wk)
	fecpred$residual_location12 <- with(fecpred, loc_pred_12wk - loc_obs_12wk)

	# If animal has been dosed up to x weeks before fec then remove the fec as non-representative:
	if(txtime == 8 && txrm){
		fecpred$fec4wk[fecpred$dose5_12wk] <- NA
		fecpred$fec8wk[fecpred$dose9_16wk] <- NA
		fecpred$fec12wk[fecpred$dose13_20wk] <- NA
	}else if(txtime == 12 && txrm){
		fecpred$fec4wk[fecpred$dose5_16wk] <- NA
		fecpred$fec8wk[fecpred$dose9_20wk] <- NA
		fecpred$fec12wk[fecpred$dose13_24wk] <- NA
	}else if(txrm && !txtime==0){
		stop('Unimplemented txtime in removal')
	}

	# Centralise previous obs and residuals to facilitate removing the intercepts:
	feclastyearmean <- mean(fecpred$lastYearMeanFEC, na.rm=TRUE)
	fecpred$lastYearMeanFEC <- fecpred$lastYearMeanFEC - feclastyearmean
	fec4wkmean <- mean(fecpred$fec4wk, na.rm=TRUE)
	fecpred$fec4wk <- fecpred$fec4wk - fec4wkmean
	fec8wkmean <- mean(fecpred$fec8wk, na.rm=TRUE)
	fecpred$fec8wk <- fecpred$fec8wk - fec8wkmean
	fec12wkmean <- mean(fecpred$fec12wk, na.rm=TRUE)
	fecpred$fec12wk <- fecpred$fec12wk - fec12wkmean
	res4mean <- mean(fecpred$residual_location4, na.rm=TRUE)
	fecpred$residual_location4 <- fecpred$residual_location4 - res4mean
	res8mean <- mean(fecpred$residual_location8, na.rm=TRUE)
	fecpred$residual_location8 <- fecpred$residual_location8 - res8mean
	res12mean <- mean(fecpred$residual_location12, na.rm=TRUE)
	fecpred$residual_location12 <- fecpred$residual_location12 - res12mean
	centering <- c(feclastyearmean, fec4wkmean, fec8wkmean, fec12wkmean, res4mean, res8mean, res12mean)
	names(centering) <- c("feclastyearmean", "fec4wkmean", "fec8wkmean", "fec12wkmean", "res4mean", "res8mean", "res12mean")

	# Initial offsets are all 0:
	fecpred$commonoffset <- 0
	fecpred$groupoffset <- 0
	fecpred$indoffset <- 0

	cat('Iterating individaul model...\n')

	newcoefs <- list()
	maxdiff <- c(1,1,1)
	for(i in 1:maxiters){
		lastcoefs <- newcoefs

		if(common_re){
			# It is better to use the mixed effects model here as sex and age don't change within animal,
			# BUT we don't want to make predictions using these random effects as this reduces the previous FEC
			# influence
			commonmod <- lmer(Obs ~ offset(groupoffset) + offset(indoffset) + offset(GroupPredictionNoTx) + Sex + AgeCat + LastDoseCat + (1 | AnimalID), data=fecpred)
			commoncoef <- fixef(commonmod)
			td <- fecpred
			td$AnimalID <- '0'
			fecpred$commonoffset <- with(fecpred, predict(commonmod, newdata=td, allow.new.levels=TRUE) - groupoffset - indoffset)
		}else{
			commonmod <- lm(Obs ~ offset(groupoffset) + offset(indoffset) + offset(GroupPredictionNoTx) + Sex + AgeCat + LastDoseCat, data=fecpred)
			commoncoef <- coef(commonmod)
			fecpred$commonoffset <- with(fecpred, predict(commonmod) - groupoffset - indoffset)
		}

		# Done separately as the residual is further back in time:
		if(resid_doy_interaction){
			gpmod4 <- lm(Obs ~ 0 + offset(commonoffset) + offset(indoffset) + residual_location4 + cdoy:residual_location4 + sdoy:residual_location4, data=fecpred)
			gpmod8 <- lm(Obs ~ 0 + offset(commonoffset) + offset(indoffset) + residual_location8 + cdoy:residual_location8 + sdoy:residual_location8, data=fecpred)
			gpmod12 <- lm(Obs ~ 0 + offset(commonoffset) + offset(indoffset) + residual_location12 + cdoy:residual_location12 + sdoy:residual_location12, data=fecpred)
		}else{
			gpmod4 <- lm(Obs ~ 0 + offset(commonoffset) + offset(indoffset) + residual_location4, data=fecpred)
			gpmod8 <- lm(Obs ~ 0 + offset(commonoffset) + offset(indoffset) + residual_location8, data=fecpred)
			gpmod12 <- lm(Obs ~ 0 + offset(commonoffset) + offset(indoffset) + residual_location12, data=fecpred)
		}

		# Order is irrelevant: note there is no gpmod0 as it is empty anyway:
		fecpred$groupoffset[which4] <- with(fecpred[which4,], predict(gpmod4, newdata=fecpred[which4,]) - commonoffset - indoffset)
		fecpred$groupoffset[which8] <- with(fecpred[which8,], predict(gpmod8, newdata=fecpred[which8,]) - commonoffset - indoffset)
		fecpred$groupoffset[which12] <- with(fecpred[which12,], predict(gpmod12, newdata=fecpred[which12,]) - commonoffset - indoffset)

		# Easier to interpret if there is a maximum of 1 previous obs:
		# Just use previous FEC only if not dose within 12 weeks before that i.e.:
		# 4 wk > 8 wk > 12 wk > last year mean. but remove 4, 8, 12 week if dosed 4-16, 8-20, 12-24 i.e. dose before obs
		# NB. dose between previous and now is accounted for already

		if(individual_intercept){
			indmod0 <- lm(Obs ~ offset(groupoffset) + offset(commonoffset) + lastYearMeanFEC, data=fecpred)
			if(txrm || txtime==0){
				indmod4 <- lm(Obs ~ offset(groupoffset) + offset(commonoffset) + fec4wk, data=fecpred)
				indmod8 <- lm(Obs ~ offset(groupoffset) + offset(commonoffset) + fec8wk, data=fecpred)
				indmod12 <- lm(Obs ~ offset(groupoffset) + offset(commonoffset) + fec12wk, data=fecpred)
			}else if(txtime==8){
				indmod4 <- lm(Obs ~ offset(groupoffset) + offset(commonoffset) + fec4wk:dose5_12wk, data=fecpred)
				indmod8 <- lm(Obs ~ offset(groupoffset) + offset(commonoffset) + fec8wk:dose9_16wk, data=fecpred)
				indmod12 <- lm(Obs ~ offset(groupoffset) + offset(commonoffset) + fec12wk:dose13_20wk, data=fecpred)
			}else if(txtime==12){
				indmod4 <- lm(Obs ~ offset(groupoffset) + offset(commonoffset) + fec4wk:dose5_16wk, data=fecpred)
				indmod8 <- lm(Obs ~ offset(groupoffset) + offset(commonoffset) + fec8wk:dose9_20wk, data=fecpred)
				indmod12 <- lm(Obs ~ offset(groupoffset) + offset(commonoffset) + fec12wk:dose13_24wk, data=fecpred)
			}else{
				stop('Invalid txtime')
			}
		}else{
			indmod0 <- lm(Obs ~ 0 + offset(groupoffset) + offset(commonoffset) + lastYearMeanFEC, data=fecpred)
			if(txrm || txtime==0){
				indmod4 <- lm(Obs ~ 0 + offset(groupoffset) + offset(commonoffset) + fec4wk, data=fecpred)
				indmod8 <- lm(Obs ~ 0 + offset(groupoffset) + offset(commonoffset) + fec8wk, data=fecpred)
				indmod12 <- lm(Obs ~ 0 + offset(groupoffset) + offset(commonoffset) + fec12wk, data=fecpred)
			}else if(txtime==8){
				indmod4 <- lm(Obs ~ 0 + offset(groupoffset) + offset(commonoffset) + fec4wk:dose5_12wk, data=fecpred)
				indmod8 <- lm(Obs ~ 0 + offset(groupoffset) + offset(commonoffset) + fec8wk:dose9_16wk, data=fecpred)
				indmod12 <- lm(Obs ~ 0 + offset(groupoffset) + offset(commonoffset) + fec12wk:dose13_20wk, data=fecpred)
			}else if(txtime==12){
				indmod4 <- lm(Obs ~ 0 + offset(groupoffset) + offset(commonoffset) + fec4wk:dose5_16wk, data=fecpred)
				indmod8 <- lm(Obs ~ 0 + offset(groupoffset) + offset(commonoffset) + fec8wk:dose9_20wk, data=fecpred)
				indmod12 <- lm(Obs ~ 0 + offset(groupoffset) + offset(commonoffset) + fec12wk:dose13_24wk, data=fecpred)
			}else{
				stop('Invalid txtime')
			}
		}
		if(lastyear){
			indmod4 <- update(indmod4, .~. + lastYearMeanFEC)
			indmod8 <- update(indmod8, .~. + lastYearMeanFEC)
			indmod12 <- update(indmod12, .~. + lastYearMeanFEC)
		}

		fecpred$indoffset[which0i] <- with(fecpred[which0i,], predict(indmod0, newdata=fecpred[which0i,]) - commonoffset - groupoffset)
		fecpred$indoffset[which4i] <- with(fecpred[which4i,], predict(indmod4, newdata=fecpred[which4i,]) - commonoffset - groupoffset)
		fecpred$indoffset[which8i] <- with(fecpred[which8i,], predict(indmod8, newdata=fecpred[which8i,]) - commonoffset - groupoffset)
		fecpred$indoffset[which12i] <- with(fecpred[which12i,], predict(indmod12, newdata=fecpred[which12i,]) - commonoffset - groupoffset)

		newcoefs <- unlist(list(commoncoef, coef(gpmod4), coef(gpmod8), coef(gpmod12), coef(indmod0), coef(indmod4), coef(indmod8), coef(indmod12)))
		if(!identical(lastcoefs, list())){
			# Relative difference:
			maxdiff <- c(maxdiff[-1], max(abs((newcoefs-lastcoefs)/((newcoefs+lastcoefs)/2))))
			if(all(maxdiff < tol))
				break
			cat('\tIteration', i, '- max relative difference:', maxdiff[length(maxdiff)], '\n')
		}
	}
	convergence <- cbind(newcoefs,lastcoefs,reldiff=abs((newcoefs-lastcoefs)/((newcoefs+lastcoefs)/2)))
	if(!all(maxdiff < tol)){
		warning(paste0('Convergence failure: max relative difference = ', maxdiff[length(maxdiff)]))
	}

	cat('Estimating residual standard deviation...\n')
	# The last step is to estimate the extra-Poisson variance using a negative binomial model:
	fecpred$totpred <- with(fecpred, commonoffset+groupoffset+indoffset)
	# ggplot(fecpred, aes(totpred, Obs)) + geom_point() + stat_smooth(method='lm') + geom_abline(slope=1,intercept=0)
	fecpred$FakeCount <- round(fecpred$Count) # Round the occasional non-integer Count (Moredun method etc)
	nbmod <- glm.nb(FakeCount ~ 0 + totpred, data=fecpred, weights=log(100/fecpred$Sensitivity))
	resid_sd <- sqrt(1/nbmod$theta)
	# Similar to:  sd(resid(nbmod))

	cat('Done\n')

	rval <- list(group=gpmodel, common=commonmod, residual_4=gpmod4, residual_8=gpmod8, residual_12=gpmod12, ind_0=indmod0, ind_4=indmod4, ind_8=indmod8, ind_12=indmod12, residual_sd=resid_sd, all_coefs=newcoefs, group_exclusion_coefs=allcoefs, centering=centering, hygiene_levels=levels(gpdata$FarmHygiene), group_data_output=group_data_output, plots=list(exclusionplot1=exclusionplot1, exclusionplot2=exclusionplot2, combinedplot1=combinedplot1, combinedplot2=combinedplot2), convergence=convergence, settings=list(expected_efficacy=expected_efficacy, min_N=min_N, min_week=min_week, max_week=max_week))

	return(rval)

}

