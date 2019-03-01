#' @title Scrape the required weather data
#' @name ScrapeWeather
#' @aliases ScrapeWeather

#' @description
#' This function can be used to scrape weather data from a website (intended to be run automatically on a regular basis).  If historical data is provided as input then the scraped data will be combined with this. Your darksky API keyy will need to be added to your enviornment variables using Sys.setenv(DARKSKY_API_KEY="YOURAPIKEY").

#' @return
#' A data frame with the available weather data in an appropriate format for the donkey dosing model

#' @param input_file either a path to an Excel or CSV file containing previously recorded (or scraped) weather data, a data frame of the same, or NA
#' @param output_file either a path to an Excel or CSV file to which the combined data will be written, or NA
#' @param ... additional arguments to be passed through to the darksky package

#' @importFrom darksky get_current_forecast get_forecast_for

#' @export ScrapeWeather

ScrapeWeather <- function(input_file=NA,existing_weather=NA, output_file=NA, ...){
	
  loc<-read.csv(input_file)
  
  #List of farms required for the model
  kl<-c("Axnoller Farm (Dorset)",
        "Brookfield Farm",
        "Slade House Farm",
        "Paccombe Farm",
        "Strete Isolation",
        "Town Barton Farm",
        "Trow Farm",
        "Woods Farm")
  
  
  #Subset the required farms
  loc<-subset(loc, Name%in%kl)
  
  reqe<-NULL
  finalwd<-NULL
  
  
  for(i in 1:nrow(loc)) {
    row<-loc[i,]
    lat<-row$lat
    lon<-row$lon
    location<-row$Name
    
    reqe[[i]]<-seq(Sys.Date()-137, Sys.Date()-137, "1 day") %>% 
      map(~get_forecast_for(paste(lat),paste(lon), units = "uk2", .x)) 
    
    weatherdata<-reqe[[i]]%>% map_df("daily")
    weatherdata$lon<-lon
    weatherdata$lat<-lat
    weatherdata$location<-location
    weatherdata$humidity<-weatherdata$humidity*100
    weatherdata$cloudCover<-weatherdata$cloudCover*100
    
    weatherdata$year <- as.numeric(format(as.Date(weatherdata$time), format = "%Y"))
    weatherdata$month <- as.numeric(format(as.Date(weatherdata$time), format = "%m"))
    weatherdata$day <- as.numeric(format(as.Date(weatherdata$time), format = "%d"))
    weatherdata$week <- as.numeric(format(as.Date(weatherdata$time), format = "%V"))
    weatherdata$weekday <- as.numeric(format(as.Date(weatherdata$time), format = "%u"))
    
    #Calcaute average tempreature
    hourly<-reqe[[i]]%>% map_df("hourly")
    hourly$lon<-lon
    hourly$lat<-lat
    hourly$location<-location
    hourly$date<-as.Date(hourly$time)
    temp<-summaryBy(temperature~date+location, data=hourly, FUN=mean, keep.names = T)
    weatherdata$time<-as.Date(weatherdata$time)
    weatherdata<-left_join(weatherdata,temp, by=c("location","time"="date"))
    
    weatherdata$Abs_humid<-(6.112*exp((17.67*weatherdata$temperature)/(weatherdata$temperature+243.5))*weatherdata$humidity*2.1674/(273.15+weatherdata$temperature))
    
    if (is.null(finalwd)){
      finalwd<-rbind(finalwd, weatherdata)
    }else{
      finalwd<-suppressMessages(smartbind(finalwd, weatherdata))
    }
    finalwd<-subset(finalwd, select=c("time","year","week","weekday","month","day","temperatureMax","temperature","temperatureMin","humidity","Abs_humid"))
    colnames(finalwd)<-c("Date",	"Year",	"Week",	"WeekDay",	"Month",	"Day",	"Temp_high",	"Temp_avg",	"Temp_low",	"Rel_Humidity_avg",	"Abs_Humidity_avg")
    
  }
  
  wdf<-read.csv(existing_weather)
  wdf<-subset(wdf,!is.na(wdf$Date))
  wdf<- suppressWarnings(smartbind(finalwd, wdf))
  wdf<-wdf[!duplicated(wdf),]

  wdf %<>%
    group_by(Date,Year,Week,WeekDay,Month,Day) %>%
    summarise_at(.vars = c("Temp_high",	"Temp_avg",	"Temp_low",	"Rel_Humidity_avg",	"Abs_Humidity_avg"),.funs = mean)
  wdf<-subset(wdf,!is.na(wdf$Date))
  wdf <- wdf[rev(order(as.Date(wdf$Date))),]
  write.csv(wdf,existing_weather, row.names = F)
 
  return(wdf)
  
}
