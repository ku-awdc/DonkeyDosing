test_thresholds <- function(){

	(load('../allpreds_no2018.Rdata'))
	(load('../allpreds_all.Rdata'))
	allprobs <- allpreds
	
	pdf('ProbComparisons.pdf', width=20, height=10)
	ggplot(allprobs, aes(x=ThresholdProbability, group=Week, col=factor(Week))) +
		stat_ecdf() +
		facet_wrap(~ Location) +
		xlim(0,0.6)
	dev.off()
	
	ggplot(allprobs %>% group_by(Location, Week) %>% slice(1), aes(x=Week, y=PredictedGroupMean, col=Hygiene, group=Location)) +
		geom_line() +
		geom_point() +
		facet_wrap(~ Farm)
	
	#anova(lm(ThresholdProbability ~ factor(Week) + Location + AnimalID, data=allprobs))
	#summary(lmer(ThresholdProbability ~ (1 | Week) + (1 | Location) + (1 | AnimalID), data=allprobs))
	
	
	pdf('Predictions.pdf', width=10, height=8)
	ggplot(allprobs, aes(x=Week, y=PredictedGroupMean, group=Location, col=Hygiene)) +
		geom_line() +
		geom_point() +
		facet_wrap(~ Farm, nrow=2) +
		ggtitle('Predicted Group Mean')
	ggplot(allprobs, aes(x=Week, y= PredictedAnimalMean, group=Week)) +
		geom_boxplot() +
		facet_wrap(~ Farm, nrow=2) +
		ggtitle('Predicted Animal Mean')
	ggplot(allprobs, aes(x=Week, y=ThresholdProbability, group=Week)) +
		geom_boxplot() +
		facet_wrap(~ Farm, nrow=2) +
		ggtitle('Threshold Probability')
	dev.off()
	
	# Magic probability threshold:  35% ?
	pdf('ProbabilityThresholds_no2018.pdf', width=10, height=8)
	for(pt in seq(0.25,0.6,by=0.025)){
		totaldoses <- allprobs %>%
			mutate(IndividualDose = IndividDose, ModelDose = ThresholdProbability >= pt,
				DoseType = factor(c('None','Individual','Model','Both')[1 + IndividualDose + ModelDose*2], levels=c('None','Model','Individual','Both'))) %>%
			group_by(Farm, Week) %>%
			mutate(Total = n()) %>%
			group_by(Farm, Week, DoseType) %>%
			summarise(N = n(), Proportion = N/Total[1]) %>%
			ungroup()
		totaldoses$Proportion[totaldoses$Proportion>0.25 & totaldoses$DoseType=='Model'] <- 0.25
		print({
		ggplot(totaldoses, aes(x=Week, y=Proportion, fill=DoseType)) +
			geom_bar(stat='identity', position='stack') +
			facet_wrap(~ Farm, nrow=2) +
			ylim(0,0.4) +
			ggtitle(paste0('Probability threshold: ', pt)) +
			scale_fill_manual(values=c(None='grey92', Model='blue', Individual='red', Both='orange'))
		})
		totaldoses %>% group_by(Farm, DoseType) %>% select(N) %>% summarise_all(sum) %>% filter(DoseType != 'None')
	}
	dev.off()
	
	sum(totaldoses$n[totaldoses$Dose]) / sum(totaldoses$n)
	totaldoses %>% 
		group_by(Week, Dose) %>%
		summarise(Total = sum(n)) %>%
		group_by(Week) %>%
		arrange(Dose) %>%
		summarise(Proportion = Total[2] / sum(Total))
	pt1 <- ggplot(totaldoses, aes(x=Week, y=n, fill=Dose)) +
		geom_bar(stat='identity') +
		facet_wrap(~ Location, scales='free_y')
	totaldoses <- allprobs_no2017 %>%
		mutate(Dose = ThresholdProbability >= pt) %>%
		group_by(Location, Week, Dose) %>%
		tally
	sum(totaldoses$n[totaldoses$Dose]) / sum(totaldoses$n)
	pt2 <- ggplot(totaldoses, aes(x=Week, y=n, fill=Dose)) +
		geom_bar(stat='identity') +
		facet_wrap(~ Location, scales='free_y')
	
	# Donkeys
		# Vet check at the top
		# Trow and Axnoller at 25% frequency
	
		# Pre-eth hemo/bio, histo: specific lesions
		# Retrospective analysis of post-mortem findings in 1,444 aged donkeys
	
		# Risk groups:  low, medium, high:  600, 1000,  2000
		# Vet check:  low, medium, high:  600, 3000, 3000 ???
		# High risk:  repeat FWEC 14-day later
		# High risk:  clinically immunocompromised
		# Medium risk:  weak immune, age < 3, ponies/horses
		# Column for very high or high clinical suspicion to be added
		# Trial:  weeks 14-43 inclusive (30 weeks)
		# MOX trial:  start december
	
		# People there using R!

}
