rm(list=ls())
setwd("C:/Users/hhong.TRADECO/Desktop/stdev/Processed")
library(zoo)
library(xts)

#making sure the environment is in GMT
env.tz <- "GMT"
Sys.setenv(TZ=env.tz)
database <- read.csv("C:/Users/hhong.TRADECO/Desktop/stdev/Daylight_database.csv")
db_nyc <- subset(database, database$City == "New York")

df <- read.csv("ESYM_U6_Jul6.csv", skip = 2, header = TRUE, row.names=NULL)
df_sub = df[, c(1, 6, 7, 8, 9)]
options(digits.secs = 3)
df_t <- as.POSIXct(df_sub[,1], format = "%Y-%m-%d %H:%M:%OS")
all_data = zoo(df_sub[2: 5], df_t)$spread.mid

#checking for daylights
startDate <- as.Date(as.POSIXct(first(index(all_data)), format = "%Y-%m-%d", tz = "GMT"))
endDate <- as.Date(as.POSIXct(last(index(all_data)), format = "%Y-%m-%d", tz = "GMT"))


#creating a function to check data bounds and GMT offsets
get_gmt_offset <- function(in_date, calendar_db) {
  
  ## in_date should come in as output from as.Date()
  
  if (in_date < as.Date(as.POSIXct(calendar_db$Start.Daylight, format = "%m/%d/%Y")[1])) {
    # RETURN ERROR
    return('ERR: Out Of Bounds')
  } 
  if (in_date > as.Date(as.POSIXct(calendar_db$End.Daylight, format = "%m/%d/%Y")[nrow(calendar_db)])) {
    # RETURN ERROR
    return('ERR: Out Of Bounds')
  }
  
  return_val <- 'Err: Out Of Bounds'
  for (i in 1:nrow(calendar_db)) {
    
    if (in_date < as.Date(as.POSIXct(calendar_db$Start.Daylight, format = "%m/%d/%Y")[i])) { 
      return(calendar_db$GMT.offset.1[i-1]) 
    }
    
    if ((in_date >= as.Date(as.POSIXct(calendar_db$Start.Daylight, format = "%m/%d/%Y")[i])) && (in_date < as.Date(as.POSIXct(calendar_db$End.Daylight, format = "%m/%d/%Y")[i]))) { 
      return (calendar_db$GMT.offset[i])
    }
  }
  
  return('Err: Out Of Bounds')
}


# check the GMT offset of the start and end date in 'data'
gmt_offset_start <- get_gmt_offset(startDate, db_nyc)
cat('Start Date: ')
print(startDate)
cat('Start Date GMT Offset: ')
print(gmt_offset_start)

gmt_offset_end <- get_gmt_offset(endDate, db_nyc)
cat('End Date: ')
print(endDate)
cat('End Date GMT Offset: ')
print(gmt_offset_end)

daylight_savings_change_date <- function(startDate, endDate, calendar_db){
  #cat("start date: ")
  #print(startDate)
  #cat(    "end date: ")
  #print(endDate)
  
  for (i in 1:nrow(calendar_db)){
    if ((startDate < as.Date(as.POSIXct(calendar_db$Start.Daylight, format = "%m/%d/%Y")[i])) && (endDate >= as.Date(as.POSIXct(calendar_db$Start.Daylight, format = "%m/%d/%Y")[i]))){  
      return (as.Date(as.POSIXct(calendar_db$Start.Daylight, format = '%m/%d/%Y')[i]))
    }
    if ((startDate < as.Date(as.POSIXct(calendar_db$End.Daylight, format = "%m/%d/%Y")[i])) && (endDate >= as.Date(as.POSIXct(calendar_db$End.Daylight, format = "%m/%d/%Y")[i]))){
      return (as.Date(as.POSIXct(calendar_db$End.Daylight, format = '%m/%d/%Y')[i]))
    } 
    #print(i)
  }
}


# merging the data
dst_change_date <- daylight_savings_change_date(startDate, endDate, db_nyc)


regular_bars <- function(data, freq){
  aligned.data <- align.time(as.xts(data), freq)
  ep = endpoints(aligned.data,on = "seconds")
  bars = aligned.data[ep]
  names(bars) = "close"
  rm(aligned.data)
  data_list = split(bars, as.Date(index(bars)))
  Stdate = as.Date(first(index(bars)))
  Endate = as.Date(last(index(bars)))
  currentDate = Stdate
  bar_all = NULL
  while(currentDate <= Endate) {
    bar_Date = strftime(currentDate , format = "%Y-%m-%d", tz = 'GMT')
    bar_data = data_list[[bar_Date]]
    if(!is.null(bar_data)){
      regidx <- seq(min(index(bar_data)), max(index(bar_data)), by = paste(freq, "secs"))
      nareg <- zoo(rep(NA, length(regidx)), order.by = regidx)
      regbars <- na.locf(merge(nareg, bar_data, all = c(TRUE,TRUE)))[,2]
      regbars <- na.omit(regbars)
      names(regbars) <- "Close"
      bar_all = append(bar_all, regbars)
    }
    currentDate = currentDate + 1
  }
  return(bar_all)
}

reg_data <- regular_bars(all_data, 1)


data = reg_data
# if the start and end gmt offsets are equal
if(gmt_offset_start == gmt_offset_end) {
  
  Data.xts <- xts(data)

  
  if (gmt_offset_start == -5) {
    if (length(data) > 0) { Night <- Data.xts['T01:00:00/T14:14:59']}
    if (length(data) > 0) { Open <- Data.xts['T14:15:00/T14:44:59']}
    if (length(data) > 0) { Middle <- Data.xts['T14:45:00/T20:14:59']}
    if (length(data) > 0) { Close <- Data.xts['T20:15:00/T21:15:00']}
    
  } else if (gmt_offset_start == -4) {
    if (length(data) > 0) { Night <- Data.xts['T00:00:00/T13:14:59']}
    if (length(data) > 0) { Open <- Data.xts['T13:15:00/T13:44:59']}
    if (length(data) > 0) { Middle <- Data.xts['T13:45:00/T19:14:59']}
    if (length(data) > 0) { Close <- Data.xts['T19:15:00/T20:15:00']}
  }
  
} else {
  dst_change_date <- daylight_savings_change_date(startDate, endDate, db_nyc)
  data_range1 <- subset(data, as.Date(index(data)) < dst_change_date)
  data_range2 <- subset(data, as.Date(index(data)) >= dst_change_date)
  
  Data_Range1.xts <- xts(data_range1)
  Data_Range2.xts <- xts(data_range2)
  
  
  startDate_range1 <- as.Date(as.POSIXct(first(index(data_range1)), format = "%Y-%m-%d", tz = "GMT"))
  #endDate_range1 <- as.Date(as.POSIXct(last(index(date_range1)), format = "%Y-%m-%d", tz = "GMT"))
  startDate_range2 <- as.Date(as.POSIXct(first(index(data_range2)), format = "%Y-%m-%d", tz = "GMT"))
  #endDate_range2 <- as.Date(as.POSIXct(last(index(date_range2)), format = "%Y-%m-%d", tz = "GMT"))
  gmt_offset1 <- get_gmt_offset(startDate_range1, db_nyc)
  gmt_offset1
  gmt_offset2 <- get_gmt_offset(startDate_range2, db_nyc)
  gmt_offset2
  
  if (gmt_offset1 == -5){
    if (length(data_range1) > 0) { Night1 <- Data_Range1.xts['T01:00:00/T14:14:59']}
    if (length(data_range1) > 0) { Open1 <- Data_Range1.xts['T14:15:00/T14:44:59']}
    if (length(data_range1) > 0) { Middle1 <- Data_Range1.xts['T14:45:00/T20:14:59']}
    if (length(data_range1) > 0) { Close1 <- Data_Range1.xts['T20:15:00/T21:15:00']}
    
  } else if(gmt_offset1 == -4){
    if (length(data_range1) > 0) { Night1 <- Data_Range1.xts['T00:00:00/T13:14:59']}
    if (length(data_range1) > 0) { Open1 <- Data_Range1.xts['T13:15:00/T13:44:59']}
    if (length(data_range1) > 0) { Middle1 <- Data_Range1.xts['T13:45:00/T19:14:59']}
    if (length(data_range1) > 0) { Close1 <- Data_Range1.xts['T19:15:00/T20:15:00']}
    
  }
  
  if(gmt_offset2 == -5){
    if (length(data_range2) > 0) { Night2 <- Data_Range1.xts['T01:00:00/T14:14:59']}
    if (length(data_range2) > 0) { Open2 <- Data_Range1.xts['T14:15:00/T14:44:59']}
    if (length(data_range2) > 0) { Middle2 <- Data_Range1.xts['T14:45:00/T20:14:59']}
    if (length(data_range2) > 0) { Close2 <- Data_Range1.xts['T20:15:00/T21:15:00']}
    
  } else if(gmt_offset2 == -4){
    if (length(data_range2) > 0) { Night2 <- Data_Range1.xts['T00:00:00/T13:14:59']}
    if (length(data_range2) > 0) { Open2 <- Data_Range1.xts['T13:15:00/T13:44:59']}
    if (length(data_range2) > 0) { Middle2 <- Data_Range1.xts['T13:45:00/T19:14:59']}
    if (length(data_range2) > 0) { Close2 <- Data_Range1.xts['T19:15:00/T20:15:00']}
    
  }
  Night = rbind(Night1, Night2)
  Open = rbind(Open1, Open2)
  Middle = rbind(Middle1, Middle2)
  Close = rbind(Close1, Close2)
}

rolling_std <- function(data, t) {
  data_time <- regular_bars(data, t*60)
  change = diff(data_time, lag = 1)
  RMS = function(change) sqrt((sum(change^2))/(length(change) - 1))
  std = RMS(na.omit(change))
  print(paste("std:(",as.Date(last(index(data))),") ",std,sep=""))
  return (as.numeric(std))
}

currentDate = startDate 
stdevList = NULL
while(currentDate <= endDate){
  if(nrow(Night[format(currentDate, '%Y-%m-%d')]) > 0) {
    night_std = rolling_std(Night[format(currentDate, '%Y-%m-%d')], 8)
  } else {
    night_std = NA
  }
  
  if(nrow(Open[format(currentDate, '%Y-%m-%d')]) > 0) {
    open_std = rolling_std(Open[format(currentDate, '%Y-%m-%d')], 8)
  } else {
    open_std = NA
  }
  
  if(nrow(Middle[format(currentDate, '%Y-%m-%d')]) > 0) {
    middle_std = rolling_std(Middle[format(currentDate, '%Y-%m-%d')], 8)
  } else {
    middle_std = NA
  }
  
  if(nrow(Close[format(currentDate, '%Y-%m-%d')]) > 0) {
    close_std = rolling_std(Close[format(currentDate, '%Y-%m-%d')], 8)
  } else {
    close_std = NA
  }
  
  stdevList = rbind(stdevList, c(format(currentDate, '%Y-%m-%d'), night_std, open_std, middle_std, close_std))
  currentDate = currentDate + 1
}

#stdevList = as.data.frame(stdevList)
#names(stdevList) <- c("Date", "Night", "Open", "Middle", "Close")
#write.csv(stdevList, "Stdevs_M6_1.csv", row.names=FALSE)

