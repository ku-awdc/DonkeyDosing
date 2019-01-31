forward_predict <- function(samplings, fec_data, rollingweather, models, expected_efficacy, threshold, min_week, max_week, year){

	models$group
	models$common
	models$residual_4
	models$residual_8
	models$residual_12
	models$ind_0
	models$ind_4
	models$ind_8
	models$ind_12
	models$centering

	red <- 1-expected_efficacy

	cat('Getting predictions...\n')
	
	getpred <- function(row){

		allanimals <- fec_data %>%
			filter(Location == samplings$Location[row]) %>%
			group_by(AnimalID) %>%
			tally()

		# Need to make group predictions for 4,8,12 weeks back as well as current:
		group_residual=past_predictions <- numeric(3)
		for(gp in 1:4){

			targetweek <- samplings$Week[row] + c(-8,-4,0,4)[gp]
			mindoseweek <- targetweek
			if(gp==4)
				mindoseweek <- samplings$Week[row]

			# Extract data for which a prediction is being made:
			predict_data <- fec_data %>%
				filter(Location == samplings$Location[row], Week %in% (targetweek - 0:24)) %>%
				select(AnimalID, Week, FEC, Dose, LastDose)
			# Get date of last dose in 1 of 2 ways depending on if the last obs was a treatment:
			lastdosedate1 <- predict_data %>%
				filter(Week < mindoseweek) %>%
				group_by(AnimalID) %>%
				arrange(Week) %>%
				slice(n()) %>%
				mutate(LastDoseWeek = Week - LastDose, Type=1) %>%
				select(AnimalID, LastDoseWeek)
			lastdosedate2 <- predict_data %>%
				filter(Week < mindoseweek) %>%
				group_by(AnimalID) %>%
				arrange(Week) %>%
				filter(Dose) %>%
				slice(n()) %>%
				mutate(LastDoseWeek = Week, Type=2) %>%
				select(AnimalID, LastDoseWeek)
			lastdose <- rbind(lastdosedate1, lastdosedate2) %>%
					group_by(AnimalID) %>%
					summarise(LastDoseWeek = max(LastDoseWeek, na.rm=TRUE))

			allweeksani <- expand.grid(Week=targetweek-(0:24), AnimalID=unique(fec_data$AnimalID))
			predict_data <- merge(predict_data, allweeksani, all=TRUE) %>%
				group_by(AnimalID) %>%
				arrange(AnimalID, Week) %>%
				mutate(fec4wk = apply(vapply(3:5, function(x) lag(log(FEC+1), x), numeric(length(FEC))), 1, mean, na.rm=TRUE)) %>%
				mutate(dose5_12wk = apply(vapply(5:12, function(x) lag(Dose, x), logical(length(Dose))), 1, any, na.rm=TRUE)) %>%
				mutate(dose5_16wk = apply(vapply(5:16, function(x) lag(Dose, x), logical(length(Dose))), 1, any, na.rm=TRUE)) %>%
				mutate(fec8wk = apply(vapply(7:9, function(x) lag(log(FEC+1), x), numeric(length(FEC))), 1, mean, na.rm=TRUE)) %>%
				mutate(dose9_16wk = apply(vapply(9:16, function(x) lag(Dose, x), logical(length(Dose))), 1, any, na.rm=TRUE)) %>%
				mutate(dose9_20wk = apply(vapply(9:20, function(x) lag(Dose, x), logical(length(Dose))), 1, any, na.rm=TRUE)) %>%
				mutate(fec12wk = apply(vapply(11:13, function(x) lag(log(FEC+1), x), numeric(length(FEC))), 1, mean, na.rm=TRUE)) %>%
				mutate(dose13_20wk = apply(vapply(13:20, function(x) lag(Dose, x), logical(length(Dose))), 1, any, na.rm=TRUE)) %>%
				mutate(dose13_24wk = apply(vapply(13:24, function(x) lag(Dose, x), logical(length(Dose))), 1, any, na.rm=TRUE)) %>%
				ungroup() %>%
				filter(Week == targetweek)

			# Merge back with animal level data:
			predict_data <- merge(predict_data, fec_data %>% group_by(AnimalID, Location, Farm, Year, Sex, Age, AgeCat, Hygiene, AvFEC) %>% summarise, all.x=TRUE) %>%
				filter(Location == samplings$Location[row])
			# And last dose date:
			predict_data <- merge(predict_data, lastdose, all.x=TRUE) %>%
				mutate(LastDoseWeek = ifelse(is.na(LastDoseWeek), -Inf, LastDoseWeek), LastDose = Week - LastDoseWeek) %>%
				select(-LastDoseWeek)

			stopifnot(all(predict_data$LastDoseWeek <= targetweek, na.rm=TRUE))
			stopifnot(all(table(predict_data$AnimalID) %in% c(0,1)))

			# Calculate group info for each week to predict:
			group_data <- predict_data %>%
				filter(Location == samplings$Location[row]) %>%
				group_by(Farm, Location, Hygiene) %>%
				summarise(TotalFEC = n(), MeanAge = mean(Age, na.rm=TRUE), DosedWithin4 = sum(LastDose <= 4),
					DosedWithin8 = sum(LastDose <= 8)-DosedWithin4, DosedWithin12 = sum(LastDose <= 12)-(DosedWithin4+DosedWithin8)) %>%
				# This reflects the expected reduction in mean count due to dosing proportion of animals (NOT TotalFEC):
				mutate(TotalAnimals = nrow(allanimals)) %>%
				mutate(DoseProp4 = log(1 * (TotalAnimals-DosedWithin4)/TotalAnimals + red * DosedWithin4/TotalAnimals)) %>%
				mutate(DoseProp8 = log(1 * (TotalAnimals-DosedWithin8)/TotalAnimals + red * DosedWithin8/TotalAnimals)) %>%
				mutate(DoseProp12 = log(1 * (TotalAnimals-DosedWithin12)/TotalAnimals + red * DosedWithin12/TotalAnimals)) %>%
				mutate(Year=year, Week=targetweek) %>%
				select(Farm, Location, Hygiene, Year, Week, TotalAnimals, TotalFEC, MeanAge, DoseProp4, DoseProp8, DoseProp12) %>%
			  ungroup

			# Merge with weather info and create cdoy etc:
			stopifnot(group_data$Year %in% as.character(levels(rollingweather$Year)))
			group_data$Year <- factor(group_data$Year, levels=levels(rollingweather$Year))
			group_data <- group_data %>% inner_join(rollingweather, by = c("Year", "Week")) %>%
				mutate(Year = '0', LocationYear = '0', sdoy = sin(2*pi*Week/52), cdoy = cos(2*pi*Week/52))
			stopifnot(nrow(group_data)==1)
			group_data$FarmHygiene <- with(group_data, paste(Farm, Hygiene, sep='_'))

			# Make group prediction and calculate the group residual if necessary:
			if(gp < 4){
				past_predictions[gp] <- predict(models$group, newdata=group_data, allow.new.levels=TRUE)
				group_residual[gp] <- past_predictions[gp] - mean(log(predict_data$FEC+1), na.rm=TRUE)
				if(sum(!is.na(predict_data$FEC))<5)
					group_residual[gp] <- NA
			}else{
				group_data$DoseProp4 <- 0
				group_data$DoseProp8 <- 0
				group_data$DoseProp12 <- 0
				group_prediction <- predict(models$group, newdata=group_data, allow.new.levels=TRUE)
			}
		}
		
		# This should agree with the intercept Coefficients $ L7 in Excel:
		# exp(group_prediction)-1
		# Note: past_predictions and group_residual can also be compared to Excel values
		
		# The individual data should be for the relevant week:
		stopifnot(targetweek == samplings$Week[row]+4)
		stopifnot(all(predict_data$Location == samplings$Location[row]))
		stopifnot(all(predict_data$Week == targetweek))

		# Add the group prediction and residuals:
		predict_data$GroupPredictionNoTx <- group_prediction
		
		predict_data$residual_location4 <- group_residual[3] - models$centering['res4mean']
		predict_data$residual_location8 <- group_residual[2] - models$centering['res8mean']
		predict_data$residual_location12 <- group_residual[1] - models$centering['res12mean']

		# Center previous fec:
		predict_data$LastFEC <- exp(predict_data$fec4wk)-1
		predict_data$lastYearMeanFEC <- log(predict_data$AvFEC+1) - models$centering['feclastyearmean']
		predict_data$fec4wk <- predict_data$fec4wk - models$centering['fec4wkmean']
		predict_data$fec8wk <- predict_data$fec8wk - models$centering['fec8wkmean']
		predict_data$fec12wk <- predict_data$fec12wk - models$centering['fec12wkmean']

		# Add stuff needed for individual etc models:
		predict_data$LastDoseCat <- relevel(cut(predict_data$LastDose, breaks=c(0.5, 4.5, 8.5, 12.5, Inf), labels=c('1-4','5-8','9-12','>12')), '>12')
		# If no previous FEC from last year then make it 0:
		predict_data$lastYearMeanFEC[is.na(predict_data$lastYearMeanFEC)] <- 0

		# Group model offset starting from 4 weeks and going up to 12:
		predict_data$commonoffset <- 0
		predict_data$groupoffset <- 0
		predict_data$indoffset <- 0
		if(!is.na(group_residual[3])){
			predict_data$groupoffset <- with(predict_data, predict(models$residual_4, newdata=predict_data) - commonoffset - indoffset)
		}else if(!is.na(group_residual[2])){
			predict_data$groupoffset <- with(predict_data, predict(models$residual_8, newdata=predict_data) - commonoffset - indoffset)
		}else if(!is.na(group_residual[1])){
			predict_data$groupoffset <- with(predict_data, predict(models$residual_12, newdata=predict_data) - commonoffset - indoffset)
		}else{
			predict_data$groupoffset <- 0
		}
		group_prediction + predict_data$groupoffset[1] + fixef(models$common)[1]

		# Individual model offset starting with 4wk and going up to 12 then last year only:
		stopifnot(all(!is.na(predict_data$lastYearMeanFEC)))
		which4i <- with(predict_data, !is.na(fec4wk))
		which8i <- with(predict_data, is.na(fec4wk) & !is.na(fec8wk))
		which12i <- with(predict_data, is.na(fec4wk) & is.na(fec8wk) & !is.na(fec12wk))
		which0i <- with(predict_data, is.na(fec4wk) & is.na(fec8wk) & is.na(fec12wk))
		stopifnot(all((which0i + which4i + which8i + which12i) == 1))
		predict_data$indoffset[which4i] <- with(predict_data[which4i,], predict(models$ind_4, newdata=predict_data[which4i,]) - commonoffset - groupoffset)
		predict_data$indoffset[which8i] <- with(predict_data[which8i,], predict(models$ind_8, newdata=predict_data[which8i,]) - commonoffset - groupoffset)
		predict_data$indoffset[which12i] <- with(predict_data[which12i,], predict(models$ind_12, newdata=predict_data[which12i,]) - commonoffset - groupoffset)
		predict_data$indoffset[which0i] <- with(predict_data[which0i,], predict(models$ind_0, newdata=predict_data[which0i,]) - commonoffset - groupoffset)

		# Finally make common prediction:
		td <- predict_data
		td$AnimalID <- '0'
		predict_data$predictedmean <- predict(models$common, newdata=td, allow.new.levels=TRUE)
		stopifnot(all(!is.na(predict_data$predictedmean)))
		
		# Probability of going over the threshold:
		predict_data$ThresholdProbability <- round(1-pnorm(log(threshold+1), predict_data$predictedmean, models$residual_sd), 2)
		predict_data$PredictedGroupMean <- round(exp(predict_data$GroupPredictionNoTx)-1, 0)
		predict_data$PredictedAnimalMean <- round(exp(predict_data$predictedmean)-1, 0)
		
		newprobs <- predict_data %>% mutate(Farm=samplings$Farm[row]) %>% select(Farm, Location, Week, AnimalID, LastFEC, PredictedGroupMean, PredictedAnimalMean, ThresholdProbability, ObservedFEC=FEC, LastDose)
		stopifnot(all(newprobs$Location == samplings$Location[row]))

		return(newprobs)

	}

	#allprobs <- lapply(1:nrow(samplings), getpred)
	allprobs <- mclapply(1:nrow(samplings), getpred)
	allprobs <- do.call('rbind', allprobs)
	stopifnot(all(allprobs$Location %in% samplings$Location))

	cat('Done\n')

	return(allprobs)

}
