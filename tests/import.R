# Test that data can be read (in various formats) and written

library('DonkeyDosing')

internalpath <- system.file('extdata', package='DonkeyDosing')

container <- DonkeyDosing()

container$AddLocations(2001, file.path(internalpath, 'ExampleData2001.xlsx'), 'Locations')
container$AddLocations(2002, file.path(internalpath, 'ExampleData2002.xlsx'), 'Locations')
# Should error:
err <- try(container$AddLocations(2001, file.path(internalpath, 'ExampleData.xlsx'), 'Locations'), silent=TRUE)
stopifnot(inherits(err,'try-error'))

container$AddDosingSheet(2001, 'Fruits', file.path(internalpath, 'ExampleData2001.xlsx'))
container$AddDosingSheet(2001, 'Vegetables', file.path(internalpath, 'ExampleData2001.xlsx'))
container$AddDosingSheet(2001, 'Dairy', file.path(internalpath, 'ExampleData2001.xlsx'))
container$AddDosingSheet(2002, 'Fruits', file.path(internalpath, 'ExampleData2002.xlsx'))
container$AddDosingSheet(2002, 'Vegetables', file.path(internalpath, 'ExampleData2002.xlsx'))
container$AddDosingSheet(2002, 'Dairy', file.path(internalpath, 'ExampleData2002.xlsx'))

# Should error:
err <- try(container$AddDosingSheet(2001, 'Dairy', file.path(internalpath, 'ExampleData2001.xlsx')), silent=TRUE)
stopifnot(inherits(err,'try-error'))

container$AddWeatherSheet(file.path(internalpath, 'ExampleWeather.csv'))


container$SaveDataArchive('test1.Rdata', years=2001)
container$SaveDataArchive('testa.Rdata')
# container$SaveDataArchive('ExampleDataArchive.Rdata')


# Should error:
err <- try(container$SaveDataArchive('testa.Rdata'), silent=TRUE)
stopifnot(inherits(err,'try-error'))

# Should error:
err <- try(container$LoadDataArchive('test1.Rdata'), silent=TRUE)
stopifnot(inherits(err,'try-error'))

container1 <- DonkeyDosing()
container1$LoadDataArchive('test1.Rdata')
# Should error:
err <- try(container1$LoadDataArchive('testa.Rdata'), silent=TRUE)
stopifnot(inherits(err,'try-error'))

container1$Reset()
container1$LoadDataArchive(file.path(internalpath, 'ExampleDataArchive.Rdata'))

md <- container$GetGroupModelData()
md1 <- container1$GetGroupModelData()

stopifnot(identical(md, md1))

file.remove('test1.Rdata','testa.Rdata')

container$Locations
container$Animal
container$FEC
container$Weather

gpdat <- container$GetGroupModelData()

plotdata <- merge(container$Locations, container$FEC) %>%
  group_by(Location, Hygiene, Monday) %>%
  summarise(meanCount = mean(FEC))
pt <- ggplot(plotdata, aes(x=Monday, y=meanCount, col=Hygiene, group=Location)) +
  geom_line() +
  geom_point()

ff <- meanLogFEC ~ (1 | LocationYear) + Farm + Hygiene + log(TotalAnimals) + DoseProp4 + DoseProp8 +
  DoseProp12 + Temp_avg_8 + I(Temp_avg_8^2/10) + Temp_avg_12 +
  I(Temp_avg_12^2/10) + Temp_avg_16 + I(Temp_avg_16^2/10) +
  Temp_frost_8 + I(Temp_frost_8^2/10) + Temp_frost_12 + I(Temp_frost_12^2/10) +
  Temp_frost_16 + I(Temp_frost_16^2/10) + AbsHumid_8 + I(AbsHumid_8^2/10) +
  AbsHumid_12 + I(AbsHumid_12^2/10) + AbsHumid_16 + I(AbsHumid_16^2/10) +
  AbsHumid_20 + I(AbsHumid_20^2/10) + sdoy + cdoy

# Reduced model to avoid a singular fit from the limited data:
ff <- meanLogFEC ~ (1 | LocationYear) + DoseProp4 + DoseProp8 +
  DoseProp12 + Temp_avg_8 + I(Temp_avg_8^2/10) +
  Temp_frost_8 + I(Temp_frost_8^2/10) +
  AbsHumid_8 + I(AbsHumid_8^2/10) + sdoy + cdoy

container <- DonkeyDosing()
container$LoadDataArchive(system.file('extdata/ExampleDataArchive.Rdata', package='DonkeyDosing'))
allmod <- container$FitPredictionModel(ff)
coefs <- container$GetCoefficients(write=FALSE)
coefs

file <- internalpath <- system.file('extdata/ExampleData2002.xlsx', package='DonkeyDosing')
#for(fm in unique(container$Locations$Farm))
#  container$AddDosingSheet(2018, fm, file)
fm <- 'Fruits'
preds <- container$GetPredictions(2002, fm, file)


summary(allmod$group)
summary(allmod$common)
summary(allmod$residual_4)
summary(allmod$residual_8)
summary(allmod$residual_12)
summary(allmod$ind_0)
summary(allmod$ind_4)
summary(allmod$ind_8)
summary(allmod$ind_12)


if(FALSE){

  container <- DonkeyDosing()

  file <- '/Users/matthewdenwood/Documents/Research/Projects/Donkey Sanctuary 2/Final/Deliverables/Data/data_2013_2017.Rdata'
  container$LoadDataArchive(file)

  file <- '/Users/matthewdenwood/Documents/Research/Projects/Donkey Sanctuary 2/Final/Deliverables/Data/Dosing Tool 2018.xlsx'
container$AddLocations(2018, file, 'Locations')
for(fm in unique(container$Locations$Farm))
  container$AddDosingSheet(2018, fm, file)

# All years should have some missing locations:
stopifnot(all(2014:2018 %in% (container$Animal %>% filter(is.na(Location)) %>% group_by(Year) %>% tally)$Year))

container$AddWeatherSheet(file, 'all', sheet='WeatherData', replace=TRUE)

container$SaveDataArchive('/Users/matthewdenwood/Documents/Research/Projects/Donkey Sanctuary 2/Final/Deliverables/Data/data_2013_2018.Rdata', overwrite=TRUE)

mdat1 <- container$GetGroupModelData()


container2 <- DonkeyDosing()
container2$LoadDataArchive('/Users/matthewdenwood/Documents/Research/Projects/Donkey Sanctuary 2/Final/Deliverables/Data/data_2013_2018.Rdata')
mdat2 <- container2$GetGroupModelData()

stopifnot(identical(mdat1, mdat2))
stopifnot(nrow(container$FEC)==nrow(container2$FEC))

# Weather data from 2013 to 2018 should be present:
weather <- container$Weather
summary(weather)
alldates <- seq(from=min(weather$Date), to=max(weather$Date), by='day')
stopifnot(all(alldates %in% weather$Date))


plotdata <- merge(container$Locations, container$FEC) %>%
  group_by(Farm, Location, Hygiene, Monday) %>%
  summarise(meanCount = mean(FEC))
ggplot(plotdata %>% filter(!is.na(meanCount)), aes(x=Monday, y=meanCount, col=Hygiene, group=Location)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Farm)

container <- DonkeyDosing()
container$LoadDataArchive('/Users/matthewdenwood/Documents/Research/Projects/Donkey Sanctuary 2/Final/Deliverables/Data/data_2013_2018.Rdata')
dosing_setOption(min_N=10)
#mdat <- container$GetGroupModelData()
predmod_all <- container$FitPredictionModel(years='all')
predmod_no18 <- container$FitPredictionModel(years=2014:2017)

useall <- FALSE
  if(useall){
    mdat <- container$GetGroupModelData()
    container$LoadPredictionModel(predmod_all)
    coefs <- container$GetCoefficients(coefs_file='estimates_allyears.csv')
  }else{
    mdat <- container$GetGroupModelData(years=2014:2017)
    container$LoadPredictionModel(predmod_no18)
    coefs <- container$GetCoefficients(coefs_file='estimates_no2018.csv')
  }

coefs <- container$GetCoefficients(write=FALSE)


#file <- '/Users/matthewdenwood/Documents/Research/Projects/Donkey Sanctuary 2/Final/Deliverables/Data/Dosing Tool 2018.xlsx'
#for(fm in unique(container$Locations$Farm))
#  container$AddDosingSheet(2018, fm, file)
fm <- 'Paccombe'
afm <- unique(container$Animal$Farm)
allpreds <- vector('list', length=length(afm))
names(allpreds) <- afm
for(fm in afm){
  preds <- container$GetPredictions(2018, fm, file)
  # save(preds, file=paste0('rdpreds_', fm,'.Rdata'))
  allpreds[[fm]] <- preds$processed
  if(useall)
    write.csv(preds$formatted, file=paste0('cpreds_', fm,'.csv'), row.names=FALSE, na='')
}
allpreds <- do.call('rbind', allpreds)
if(useall){
  save(allpreds, file=paste0('allpreds_all.Rdata'))
}else{
  save(allpreds, file=paste0('allpreds_no2018.Rdata'))
}

ggplot(preds$allprobs %>% filter(LastDose > 8), aes(PredictedAnimalMean, ObservedFEC)) +
  geom_point() +
  stat_smooth(method='lm') +
  facet_wrap(~Week)

res_sd <- coefs[coefs[,1]=='ResidualSD',2]
predconf <- data.frame(PredictedAnimalMean=seq(0,2500,by=100)) %>%
  mutate(LogMean=log(PredictedAnimalMean+1)) %>%
  mutate(MeanPred=exp(LogMean)-1, UPred=exp(LogMean+1.96*res_sd)-1, LPred=exp(LogMean-1.96*res_sd)-1) %>%
  gather(Type, ObservedFEC, -PredictedAnimalMean, -LogMean)

ggplot(preds$allprobs %>% filter(LastDose > 8), aes(PredictedAnimalMean, ObservedFEC)) +
  geom_point() +
  geom_line(data=predconf, mapping=aes(PredictedAnimalMean, ObservedFEC, group=Type), col='red') +
  facet_wrap(~Week) +
  ylim(0,4000)

ggplot(preds$allprobs %>% filter(LastDose > 8), aes(PredictedAnimalMean, ObservedFEC)) +
  geom_point() +
  geom_line(data=predconf, mapping=aes(PredictedAnimalMean, ObservedFEC, group=Type), col='red') +
  facet_wrap(~Week) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10')

container$Weather


container$predictions$allyears$group

mdat <- mdat$allyears
names(mdat)
summary(mdat$group)
summary(mdat$common)
summary(mdat$residual_4)
summary(mdat$residual_8)
summary(mdat$residual_12)
summary(mdat$ind_0)
summary(mdat$ind_4)
summary(mdat$ind_8)
summary(mdat$ind_12)

mdat$plots[[2]]

# All years should have some missing locations:
stopifnot(all(2014:2018 %in% (container$Animal %>% filter(is.na(Location)) %>% group_by(Year) %>% tally)$Year))

}

