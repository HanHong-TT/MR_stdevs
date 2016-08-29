template <- function() {
  legA_filename <- paste(str_legA_contract,"_",str_date,"_quotes.csv",sep="")
  legB_filename <- paste(str_legB_contract,"_",str_date,"_quotes.csv",sep="")
  df1 <- read.csv(legA_filename)
  df2 <- read.csv(legB_filename)
  
  sprd_filename <- paste(str_sprd_contract,"_",str_date,".csv",sep="")
  write.csv(spread_spec, sprd_filename)
  write.zoo(spread_cmb, sprd_filename, append = TRUE, sep = ",")
}




build_ca_hg <- function(str_sprd_contract, str_legA_contract, str_legB_contract, str_date) {
  
  #loading in the outright legs
  legA_filename <- paste(str_legA_contract,"_",str_date,"_quotes.csv",sep="")
  legB_filename <- paste(str_legB_contract,"_",str_date,"_quotes.csv",sep="")
  df1 <- read.csv(legA_filename)
  df2 <- read.csv(legB_filename)
  database <- read.csv("C:/Users/hhong.TRADECO/Desktop/stdev/Daylight_database.csv")
  db_lon <- subset(database, database$City == "London")
  
  #subsetting the data
  df1_sub <- df1[,c(1,5,6)]
  df2_sub <- df2[,c(1,5,6)]
  
  
  #format the date column mm/dd/YYYY HH:MM:ss.fff
  options(digits.secs = 6)
  df1_sub.t <- as.POSIXct(df1_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df2_sub.t <- as.POSIXct(df2_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df1_sub.t <- df1_sub.t + 0.0001
  df2_sub.t <- df2_sub.t + 0.0001
  
  #create zoo object
  df1_zoo <- zoo(df1_sub[2:3], df1_sub.t)
  df2_zoo <- zoo(df2_sub[2:3], df2_sub.t)
  
  #data dates
  startDate1 <- as.Date(as.POSIXct(first(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate1 <- as.Date(as.POSIXct(last(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate2 <- as.Date(as.POSIXct(first(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate2 <- as.Date(as.POSIXct(last(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate <- max(startDate1, startDate2)
  endDate <- min(endDate1, endDate2)
  
  
  # merging the data
  df1_list <- split(df1_zoo,as.Date(index(df1_zoo)))
  df2_list <- split(df2_zoo,as.Date(index(df2_zoo)))
  
  currentDate = startDate
  data = NULL
  while(currentDate <= endDate){
    setDate = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
    df1_Date = df1_list[[setDate]]
    df2_Date = df2_list[[setDate]]
    if(!is.null(df1_Date) && !is.null(df2_Date)){
      data_Date <- zoo(merge(df1_Date, df2_Date, all=TRUE))
    } else {
      data_Date = NULL
    }
    if(length(data_Date)!=0){
      data_Date <- na.locf(data_Date)
      data_Date <- na.omit(data_Date)
    }
    if(length(data_Date) == 0){
      data = data
    } else {
      data = append(data, data_Date, after = length(data))
    }
    currentDate = currentDate + 1
  }
  
  #adding in the day
  data$day <- as.POSIXlt(index(data))$wday
  data$day <- weekdays(as.Date(as.POSIXct(index(data))))
  
  
  
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
  gmt_offset_start <- get_gmt_offset(startDate, db_lon)
  cat('Start Date: ')
  print(startDate)
  cat('Start Date GMT Offset: ')
  print(gmt_offset_start)
  
  gmt_offset_end <- get_gmt_offset(endDate, db_lon)
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
  
  
  
  # if they're different... find the date it switches on
  # make two sub data sets, one before the date and one after
  # then process those two sub data sets based on the gmt offset
  # using the code below as a guide
  
  
  # if the start and end gmt offsets are equal
  if(gmt_offset_start == gmt_offset_end) {
    
    Data.xts <- xts(data)
    
    if (gmt_offset_start == 1) {
      if (length(data) > 0) { Data <- Data.xts['T00:00:00/T17:59:59']}
      
    } else if (gmt_offset_start == 0) {
      if (length(data) > 0) { Data <- Data.xts['T01:00:00/T18:59:59']}
      
    }
    
    Week = NULL
    if (length(Data) > 0) { Week <- rbind(Week, Data)}
    
  } else {
    dst_change_date <- daylight_savings_change_date(startDate, endDate, db_lon)
    data_range1 <- subset(data, as.Date(index(data)) < dst_change_date)
    data_range2 <- subset(data, as.Date(index(data)) >= dst_change_date)
    
    Data_Range1.xts <- xts(data_range1)
    Data_Range2.xts <- xts(data_range2)
    
    
    startDate_range1 <- as.Date(as.POSIXct(first(index(data_range1)), format = "%Y-%m-%d", tz = "GMT"))
    #endDate_range1 <- as.Date(as.POSIXct(last(index(date_range1)), format = "%Y-%m-%d", tz = "GMT"))
    startDate_range2 <- as.Date(as.POSIXct(first(index(data_range2)), format = "%Y-%m-%d", tz = "GMT"))
    #endDate_range2 <- as.Date(as.POSIXct(last(index(date_range2)), format = "%Y-%m-%d", tz = "GMT"))
    gmt_offset1 <- get_gmt_offset(startDate_range1, db_lon)
    gmt_offset1
    gmt_offset2 <- get_gmt_offset(startDate_range2, db_lon)
    gmt_offset2
    
    if (gmt_offset1 == 1){
      
      if(length(data_range1) > 0) {Data1 <- Data_Range1.xts['T00:00:00/T17:59:59']}
      
    } else if(gmt_offset1 == 0){
      
      if(length(data_range1) > 0) {Data1 <- Data_Range1.xts['T01:00:00/T18:59:59']}
      
    }
    
    if(gmt_offset2 == 1){
      
      if(length(data_range2) > 0) {Data2 <- Data_Range2.xts['T00:00:00/T17:59:59']}
      
    } else if(gmt_offset2 == 0){
      
      if(length(data_range2) > 0) {Data2 <- Data_Range2.xts['T01:00:00/T18:59:59']}
      
    }
    
    Week1 = NULL
    if (length(Data1) > 0) { Week1 <- rbind(Week1, Data1)}
    
    Week2 = NULL
    if (length(Data2) > 0) { Week2 <- rbind(Week2, Data2)}
    
    Week = rbind(Week1, Week2)    
  }
  
  spread_data = Week[,1:4]
  names(spread_data) = c("bid.1", "ask.1", "bid.2", "ask.2")
  #ratios
  r1 <- 5
  r2 <- -11
  #multipliers
  #m1 <- 0.00045359
  #m2 <- -1
  m1 <- 1
  m2 <- -2204.623
  spread_data$spread.bid = m1 * as.numeric(spread_data$bid.1) + m2 * as.numeric(spread_data$ask.2)
  spread_data$spread.ask = m1 * as.numeric(spread_data$ask.1) + m2 * as.numeric(spread_data$bid.2)
  spread_data$spread.mid = (spread_data$spread.ask + spread_data$spread.bid) / 2
  spread_data$spread.bidAskSpread = spread_data$spread.ask - spread_data$spread.bid
  
  #adding spread specifications
  spread_name <- "CAHG"
  spread_spec <- cbind(spread_name, r1, m1, r2, m2)
  
  #formatting data
  spread_zoo = zoo(spread_data)
  spread_digits <- format(spread_zoo[,5:8], digits=4)
  spread_fmt <- spread_zoo[,1:4]
  spread_cmb <- cbind(spread_fmt, spread_digits)
  
  sprd_filename <- paste(str_sprd_contract,"_",str_date,".csv",sep="")
  write.csv(spread_spec, sprd_filename)
  write.zoo(spread_cmb, sprd_filename, append = TRUE, sep = ",")
}

build_gc_si <- function(str_sprd_contract, str_legA_contract, str_legB_contract, str_date) {
  #loading in the outright legs
  legA_filename <- paste(str_legA_contract,"_",str_date,"_quotes.csv",sep="")
  legB_filename <- paste(str_legB_contract,"_",str_date,"_quotes.csv",sep="")
  df1 <- read.csv(legA_filename)
  df2 <- read.csv(legB_filename)
  database <- read.csv("C:/Users/hhong.TRADECO/Desktop/stdev/Daylight_database.csv")
  db_nyc <- subset(database, database$City == "New York")
  
  #subsetting the data
  df1_sub <- df1[,c(1,5,6)]
  df2_sub <- df2[,c(1,5,6)]
  
  #df1_sub <- df1[,c(2,6,7)]
  #df2_sub <- df2[,c(2,6,7)]
  
  
  #format the date column mm/dd/YYYY HH:MM:ss.fff
  options(digits.secs = 6)
  df1_sub.t <- as.POSIXct(df1_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df2_sub.t <- as.POSIXct(df2_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df1_sub.t <- df1_sub.t + 0.0001
  df2_sub.t <- df2_sub.t + 0.0001
  
  #create zoo object
  df1_zoo <- zoo(df1_sub[2:3], df1_sub.t)
  df2_zoo <- zoo(df2_sub[2:3], df2_sub.t)
  rm(df1_sub.t, df2_sub.t)
  
  #data dates
  startDate1 <- as.Date(as.POSIXct(first(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate1 <- as.Date(as.POSIXct(last(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate2 <- as.Date(as.POSIXct(first(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate2 <- as.Date(as.POSIXct(last(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate <- max(startDate1, startDate2) + 1
  endDate <- min(endDate1, endDate2)
  
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
    
    for (i in 1:length(calendar_db)){
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
  
  df1_list <- split(df1_zoo,as.Date(index(df1_zoo)))
  df2_list <- split(df2_zoo,as.Date(index(df2_zoo)))
  rm(df1_zoo, df2_zoo)
  
  currentDate = startDate 
  data = NULL
  while(currentDate <= endDate + 1){
    gmt_offset <- get_gmt_offset(currentDate, db_nyc)
    if(gmt_offset == -4) {
      setDate1 = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate1]])){
        df1_Date_1 = as.xts(df1_list[[setDate1]])['T00:00:00.000/T21:14:59.999'] 
      } else { df1_Date_1 = NULL }
      if(!is.null(df2_list[[setDate1]])){
        df2_Date_1 = as.xts(df2_list[[setDate1]])['T00:00:00.000/T21:14:59.999'] 
      } else { df2_Date_1 = NULL }
      
      setDate2 = strftime(currentDate-1, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate2]])){
        df1_Date_2 = as.xts(df1_list[[setDate2]])['T22:00:00.000/T23:59:59.999'] 
      } else { df1_Date_2 = NULL }
      if(!is.null(df2_list[[setDate2]])){
        df2_Date_2 = as.xts(df2_list[[setDate2]])['T22:00:00.000/T23:59:59.999'] 
      } else { df2_Date_2 = NULL }
    } 
    if(gmt_offset == -5) {
      setDate1 = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate1]])){
        df1_Date_1 = as.xts(df1_list[[setDate1]])['T00:00:00.000/T22:14:59.999'] 
      } else { df1_Date_1 = NULL }
      if(!is.null(df2_list[[setDate1]])){
        df2_Date_1 = as.xts(df2_list[[setDate1]])['T00:00:00.000/T22:14:59.999'] 
      } else { df2_Date_1 = NULL }
      
      setDate2 = strftime(currentDate-1, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate2]])){
        df1_Date_2 = as.xts(df1_list[[setDate2]])['T23:00:00.000/T23:59:59.999'] 
      } else { df1_Date_2 = NULL }
      if(!is.null(df2_list[[setDate2]])){
        df2_Date_2 = as.xts(df2_list[[setDate2]])['T23:00:00.000/T23:59:59.999'] 
      } else { df2_Date_2 = NULL }
    } 
    
    df1_Date = rbind(df1_Date_1, df1_Date_2)
    df2_Date = rbind(df2_Date_1, df2_Date_2)
    
    data_Date <- merge(df1_Date, df2_Date, all=TRUE)
    if(length(data_Date)!=0){
      data_Date <- na.locf(data_Date)
      data_Date <- na.omit(data_Date)
      if(length(data_Date) == 0){
        data = data
      } else {
        data = append(data, data_Date, after = length(data))
      }
    }
    currentDate = currentDate + 1
  }
  
  
  #adding in the day
  #data$day <- as.POSIXlt(index(data))$wday
  #data$day <- weekdays(as.Date(as.POSIXct(index(data))))
  
  
  
  # if they're different... find the date it switches on
  # make two sub data sets, one before the date and one after
  # then process those two sub data sets based on the gmt offset
  # using the code below as a guide
  
  
  # if the start and end gmt offsets are equal
  if(gmt_offset_start == gmt_offset_end) {
    
    Data.xts <- xts(data)
    
    if (gmt_offset_start == -5) {
      if (length(data) > 0) { Eve <- Data.xts['T23:00:00/T23:59:59']}
      if (length(data) > 0) { Morn <- Data.xts['T00:00:00/T22:14:59']}
      
    } else if (gmt_offset_start == -4) {
      if (length(data) > 0) { Eve <- Data.xts['T22:00:00/T23:59:59']}
      if (length(data) > 0) { Morn <- Data.xts['T00:00:00/T21:14:59']}
      
    }
    
    Week = NULL
    if (length(Eve) > 0) { Week <- rbind(Week, Eve)}
    if (length(Morn) > 0) { Week <- rbind(Week, Morn)}
    
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
      
      if(length(data_range1) > 0) {Eve1 <- Data_Range1.xts['T23:00:00/T23:59:59']}
      if(length(data_range1) > 0) {Morn1 <- Data_Range1.xts['T00:00:00/T22:14:59']}
      
    } else if(gmt_offset1 == -4){
      
      if(length(data_range1) > 0) {Eve1 <- Data_Range1.xts['T22:00:00/T23:59:59']}
      if(length(data_range1) > 0) {Morn1 <- Data_Range1.xts['T00:00:00/T21:14:59']}
      
    }
    
    if(gmt_offset2 == -5){
      
      if(length(data_range2) > 0) {Eve2 <- Data_Range2.xts['T23:00:00/T23:59:59']}
      if(length(data_range2) > 0) {Morn2 <- Data_Range2.xts['T00:00:00/T22:14:59']}
      
    } else if(gmt_offset2 == -4){
      
      if(length(data_range2) > 0) {Eve2 <- Data_Range2.xts['T22:00:00/T23:59:59']}
      if(length(data_range2) > 0) {Morn2 <- Data_Range2.xts['T00:00:00/T21:14:59']}
      
    }
    
    Week1 = NULL
    if (length(Eve1) > 0) { Week1 <- rbind(Week1, Eve1)}
    if (length(Morn1) > 0) { Week1 <- rbind(Week1, Morn1)}
    
    Week2 = NULL
    if (length(Eve2) > 0) { Week2 <- rbind(Week2, Eve2)}
    if (length(Morn2) > 0) { Week2 <- rbind(Week2, Morn2)}
    
    Week = rbind(Week1, Week2)    
  }
  
  spread_data = Week[,1:4]
  names(spread_data) = c("bid.1", "ask.1", "bid.2", "ask.2")
  rm(df1_list, df2_list)
  #ratios
  r1 <- 1
  r2 <- -1
  #multipliers
  m1 <- 1
  m2 <- -50
  spread_data$spread.bid = m1 * as.numeric(spread_data$bid.1) + m2 * as.numeric(spread_data$ask.2)
  spread_data$spread.ask = m1 * as.numeric(spread_data$ask.1) + m2 * as.numeric(spread_data$bid.2)
  spread_data$spread.mid = (spread_data$spread.ask + spread_data$spread.bid) / 2
  spread_data$spread.bidAskSpread = spread_data$spread.ask - spread_data$spread.bid
  
  #adding spread specifications
  spread_name <- "GCSI"
  spread_spec <- cbind(spread_name, r1, m1, r2, m2)
  
  #formatting data
  spread_zoo = zoo(spread_data)
  rm(spread_data)
  spread_digits <- format(spread_zoo[,5:8], digits=4)
  spread_fmt <- spread_zoo[,1:4]
  spread_cmb <- cbind(spread_fmt, spread_digits)
  
  sprd_filename <- paste(str_sprd_contract,"_",str_date,".csv",sep="")
  write.csv(spread_spec, sprd_filename)
  write.zoo(spread_cmb, sprd_filename, append = TRUE, sep = ",")
}

build_pl_gc <- function(str_sprd_contract, str_legA_contract, str_legB_contract, str_date) {
  #loading in the outright legs
  legA_filename <- paste(str_legA_contract,"_",str_date,"_quotes.csv",sep="")
  legB_filename <- paste(str_legB_contract,"_",str_date,"_quotes.csv",sep="")
  df1 <- read.csv(legA_filename)
  df2 <- read.csv(legB_filename)
  database <- read.csv("C:/Users/hhong.TRADECO/Desktop/stdev/Daylight_database.csv")
  db_nyc <- subset(database, database$City == "New York")
  
  #subsetting the data
  df1_sub <- df1[,c(1,5,6)]
  df2_sub <- df2[,c(1,5,6)]
  
  #df1_sub <- df1[,c(2,6,7)]
  #df2_sub <- df2[,c(2,6,7)]
  
  
  #format the date column mm/dd/YYYY HH:MM:ss.fff
  options(digits.secs = 6)
  df1_sub.t <- as.POSIXct(df1_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df2_sub.t <- as.POSIXct(df2_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df1_sub.t <- df1_sub.t + 0.0001
  df2_sub.t <- df2_sub.t + 0.0001
  
  #create zoo object
  df1_zoo <- zoo(df1_sub[2:3], df1_sub.t)
  df2_zoo <- zoo(df2_sub[2:3], df2_sub.t)
  rm(df1_sub.t, df2_sub.t)
  
  #data dates
  startDate1 <- as.Date(as.POSIXct(first(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate1 <- as.Date(as.POSIXct(last(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate2 <- as.Date(as.POSIXct(first(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate2 <- as.Date(as.POSIXct(last(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate <- max(startDate1, startDate2) + 1
  endDate <- min(endDate1, endDate2)
  
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
    
    for (i in 1:length(calendar_db)){
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
  
  df1_list <- split(df1_zoo,as.Date(index(df1_zoo)))
  df2_list <- split(df2_zoo,as.Date(index(df2_zoo)))
  rm(df1_zoo, df2_zoo)
  
  currentDate = startDate 
  data = NULL
  while(currentDate <= endDate + 1){
    gmt_offset <- get_gmt_offset(currentDate, db_nyc)
    if(gmt_offset == -4) {
      setDate1 = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate1]])){
        df1_Date_1 = as.xts(df1_list[[setDate1]])['T00:00:00.000/T21:14:59.999'] 
      } else { df1_Date_1 = NULL }
      if(!is.null(df2_list[[setDate1]])){
        df2_Date_1 = as.xts(df2_list[[setDate1]])['T00:00:00.000/T21:14:59.999'] 
      } else { df2_Date_1 = NULL }
      
      print(setDate1)
      
      setDate2 = strftime(currentDate-1, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate2]])){
        df1_Date_2 = as.xts(df1_list[[setDate2]])['T22:00:00.000/T23:59:59.999'] 
      } else { df1_Date_2 = NULL }
      if(!is.null(df2_list[[setDate2]])){
        df2_Date_2 = as.xts(df2_list[[setDate2]])['T22:00:00.000/T23:59:59.999'] 
      } else { df2_Date_2 = NULL }
      
      print(setDate2)
    } 
    if(gmt_offset == -5) {
      setDate1 = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate1]])){
        df1_Date_1 = as.xts(df1_list[[setDate1]])['T00:00:00.000/T22:14:59.999'] 
      } else { df1_Date_1 = NULL }
      if(!is.null(df2_list[[setDate1]])){
        df2_Date_1 = as.xts(df2_list[[setDate1]])['T00:00:00.000/T22:14:59.999'] 
      } else { df2_Date_1 = NULL }
      
      setDate2 = strftime(currentDate-1, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate2]])){
        df1_Date_2 = as.xts(df1_list[[setDate2]])['T23:00:00.000/T23:59:59.999'] 
      } else { df1_Date_2 = NULL }
      if(!is.null(df2_list[[setDate2]])){
        df2_Date_2 = as.xts(df2_list[[setDate2]])['T23:00:00.000/T23:59:59.999'] 
      } else { df2_Date_2 = NULL }
    } 
    print("A")
    print(length(df2_Date_1))
    print(length(df2_Date_2))
    print("A2")
    df1_Date = rbind(df1_Date_1, df1_Date_2)
    df2_Date = rbind(df2_Date_1, df2_Date_2)
    print("B")
    
    print(length(df1_Date))
    print(length(df2_Date))
    data_Date <- merge(df1_Date, df2_Date, all=TRUE)
    print("C")
    if(length(data_Date)!=0){
      data_Date <- na.locf(data_Date)
      data_Date <- na.omit(data_Date)
      if(length(data_Date) == 0){
        data = data
      } else {
        print("D")
        print(length(data))
        print(head(data))
        print("D1")
        print(length(data_Date))
        print(head(data_Date))
        print("D2")
        data = append(data, data_Date, after = length(data))
        print("E")
      }
    }
    currentDate = currentDate + 1
  }
  
  
  #adding in the day
  #data$day <- as.POSIXlt(index(data))$wday
  #data$day <- weekdays(as.Date(as.POSIXct(index(data))))
  
  
  
  # if they're different... find the date it switches on
  # make two sub data sets, one before the date and one after
  # then process those two sub data sets based on the gmt offset
  # using the code below as a guide
  
  
  # if the start and end gmt offsets are equal
  if(gmt_offset_start == gmt_offset_end) {
    
    Data.xts <- xts(data)
    
    if (gmt_offset_start == -5) {
      if (length(data) > 0) { Eve <- Data.xts['T23:00:00/T23:59:59']}
      if (length(data) > 0) { Morn <- Data.xts['T00:00:00/T22:14:59']}
      
    } else if (gmt_offset_start == -4) {
      if (length(data) > 0) { Eve <- Data.xts['T22:00:00/T23:59:59']}
      if (length(data) > 0) { Morn <- Data.xts['T00:00:00/T21:14:59']}
      
    }
    
    Week = NULL
    if (length(Eve) > 0) { Week <- rbind(Week, Eve)}
    if (length(Morn) > 0) { Week <- rbind(Week, Morn)}
    
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
      
      if(length(data_range1) > 0) {Eve1 <- Data_Range1.xts['T23:00:00/T23:59:59']}
      if(length(data_range1) > 0) {Morn1 <- Data_Range1.xts['T00:00:00/T22:14:59']}
      
    } else if(gmt_offset1 == -4){
      
      if(length(data_range1) > 0) {Eve1 <- Data_Range1.xts['T22:00:00/T23:59:59']}
      if(length(data_range1) > 0) {Morn1 <- Data_Range1.xts['T00:00:00/T21:14:59']}
      
    }
    
    if(gmt_offset2 == -5){
      
      if(length(data_range2) > 0) {Eve2 <- Data_Range2.xts['T23:00:00/T23:59:59']}
      if(length(data_range2) > 0) {Morn2 <- Data_Range2.xts['T00:00:00/T22:14:59']}
      
    } else if(gmt_offset2 == -4){
      
      if(length(data_range2) > 0) {Eve2 <- Data_Range2.xts['T22:00:00/T23:59:59']}
      if(length(data_range2) > 0) {Morn2 <- Data_Range2.xts['T00:00:00/T21:14:59']}
      
    }
    
    Week1 = NULL
    if (length(Eve1) > 0) { Week1 <- rbind(Week1, Eve1)}
    if (length(Morn1) > 0) { Week1 <- rbind(Week1, Morn1)}
    
    Week2 = NULL
    if (length(Eve2) > 0) { Week2 <- rbind(Week2, Eve2)}
    if (length(Morn2) > 0) { Week2 <- rbind(Week2, Morn2)}
    
    Week = rbind(Week1, Week2)    
  }
  
  spread_data = Week[,1:4]
  names(spread_data) = c("bid.1", "ask.1", "bid.2", "ask.2")
  rm(df1_list, df2_list)
  #ratios
  r1 <- 2
  r2 <- -1
  #multipliers
  m1 <- 1
  m2 <- -1
  spread_data$spread.bid = m1 * as.numeric(spread_data$bid.1) + m2 * as.numeric(spread_data$ask.2)
  spread_data$spread.ask = m1 * as.numeric(spread_data$ask.1) + m2 * as.numeric(spread_data$bid.2)
  spread_data$spread.mid = (spread_data$spread.ask + spread_data$spread.bid) / 2
  spread_data$spread.bidAskSpread = spread_data$spread.ask - spread_data$spread.bid
  
  #adding spread specifications
  spread_name <- "PLGC"
  spread_spec <- cbind(spread_name, r1, m1, r2, m2)
  
  #formatting data
  spread_zoo = zoo(spread_data)
  rm(spread_data)
  spread_digits <- format(spread_zoo[,5:8], digits=4)
  spread_fmt <- spread_zoo[,1:4]
  spread_cmb <- cbind(spread_fmt, spread_digits)
  
  sprd_filename <- paste(str_sprd_contract,"_",str_date,".csv",sep="")
  write.csv(spread_spec, sprd_filename)
  write.zoo(spread_cmb, sprd_filename, append = TRUE, sep = ",")
}

build_pl_pa <- function(str_sprd_contract, str_legA_contract, str_legB_contract, str_date) {
  #loading in the outright legs
  legA_filename <- paste(str_legA_contract,"_",str_date,"_quotes.csv",sep="")
  legB_filename <- paste(str_legB_contract,"_",str_date,"_quotes.csv",sep="")
  df1 <- read.csv(legA_filename)
  df2 <- read.csv(legB_filename)
  database <- read.csv("C:/Users/hhong.TRADECO/Desktop/stdev/Daylight_database.csv")
  db_nyc <- subset(database, database$City == "New York")
  
  #subsetting the data
  df1_sub <- df1[,c(1,5,6)]
  df2_sub <- df2[,c(1,5,6)]
  
  #df1_sub <- df1[,c(2,6,7)]
  #df2_sub <- df2[,c(2,6,7)]
  
  
  #format the date column mm/dd/YYYY HH:MM:ss.fff
  options(digits.secs = 6)
  df1_sub.t <- as.POSIXct(df1_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df2_sub.t <- as.POSIXct(df2_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df1_sub.t <- df1_sub.t + 0.0001
  df2_sub.t <- df2_sub.t + 0.0001
  
  #create zoo object
  df1_zoo <- zoo(df1_sub[2:3], df1_sub.t)
  df2_zoo <- zoo(df2_sub[2:3], df2_sub.t)
  rm(df1_sub.t, df2_sub.t)
  
  #data dates
  startDate1 <- as.Date(as.POSIXct(first(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate1 <- as.Date(as.POSIXct(last(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate2 <- as.Date(as.POSIXct(first(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate2 <- as.Date(as.POSIXct(last(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate <- max(startDate1, startDate2) 
  endDate <- min(endDate1, endDate2)
  
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
    
    for (i in 1:length(calendar_db)){
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
  
  df1_list <- split(df1_zoo,as.Date(index(df1_zoo)))
  df2_list <- split(df2_zoo,as.Date(index(df2_zoo)))
  rm(df1_zoo, df2_zoo)
  
  currentDate = startDate 
  data = NULL
  while(currentDate <= endDate + 1){
    gmt_offset <- get_gmt_offset(currentDate, db_nyc)
    if(gmt_offset == -4) {
      setDate1 = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate1]])){
        df1_Date_1 = as.xts(df1_list[[setDate1]])['T00:00:00.000/T21:14:59.999'] 
      } else { df1_Date_1 = NULL }
      if(!is.null(df2_list[[setDate1]])){
        df2_Date_1 = as.xts(df2_list[[setDate1]])['T00:00:00.000/T21:14:59.999'] 
      } else { df2_Date_1 = NULL }
      
      setDate2 = strftime(currentDate-1, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate2]])){
        df1_Date_2 = as.xts(df1_list[[setDate2]])['T22:00:00.000/T23:59:59.999'] 
      } else { df1_Date_2 = NULL }
      if(!is.null(df2_list[[setDate2]])){
        df2_Date_2 = as.xts(df2_list[[setDate2]])['T22:00:00.000/T23:59:59.999'] 
      } else { df2_Date_2 = NULL }
    } 
    if(gmt_offset == -5) {
      setDate1 = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate1]])){
        df1_Date_1 = as.xts(df1_list[[setDate1]])['T00:00:00.000/T22:14:59.999'] 
      } else { df1_Date_1 = NULL }
      if(!is.null(df2_list[[setDate1]])){
        df2_Date_1 = as.xts(df2_list[[setDate1]])['T00:00:00.000/T22:14:59.999'] 
      } else { df2_Date_1 = NULL }
      
      setDate2 = strftime(currentDate-1, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate2]])){
        df1_Date_2 = as.xts(df1_list[[setDate2]])['T23:00:00.000/T23:59:59.999'] 
      } else { df1_Date_2 = NULL }
      if(!is.null(df2_list[[setDate2]])){
        df2_Date_2 = as.xts(df2_list[[setDate2]])['T23:00:00.000/T23:59:59.999'] 
      } else { df2_Date_2 = NULL }
    } 
    
    df1_Date = rbind(df1_Date_1, df1_Date_2)
    df2_Date = rbind(df2_Date_1, df2_Date_2)
    
    data_Date <- merge(df1_Date, df2_Date, all=TRUE)
    if(length(data_Date)!=0){
      data_Date <- na.locf(data_Date)
      data_Date <- na.omit(data_Date)
      if(length(data_Date) == 0){
        data = data
      } else {
        data = append(data, data_Date, after = length(data))
      }
    }
    currentDate = currentDate + 1
  }
  
  
  #adding in the day
  #data$day <- as.POSIXlt(index(data))$wday
  #data$day <- weekdays(as.Date(as.POSIXct(index(data))))
  
  
  
  # if they're different... find the date it switches on
  # make two sub data sets, one before the date and one after
  # then process those two sub data sets based on the gmt offset
  # using the code below as a guide
  
  
  # if the start and end gmt offsets are equal
  if(gmt_offset_start == gmt_offset_end) {
    
    Data.xts <- xts(data)
    
    if (gmt_offset_start == -5) {
      if (length(data) > 0) { Eve <- Data.xts['T23:00:00/T23:59:59']}
      if (length(data) > 0) { Morn <- Data.xts['T00:00:00/T22:14:59']}
      
    } else if (gmt_offset_start == -4) {
      if (length(data) > 0) { Eve <- Data.xts['T22:00:00/T23:59:59']}
      if (length(data) > 0) { Morn <- Data.xts['T00:00:00/T21:14:59']}
      
    }
    
    Week = NULL
    if (length(Eve) > 0) { Week <- rbind(Week, Eve)}
    if (length(Morn) > 0) { Week <- rbind(Week, Morn)}
    
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
      
      if(length(data_range1) > 0) {Eve1 <- Data_Range1.xts['T23:00:00/T23:59:59']}
      if(length(data_range1) > 0) {Morn1 <- Data_Range1.xts['T00:00:00/T22:14:59']}
      
    } else if(gmt_offset1 == -4){
      
      if(length(data_range1) > 0) {Eve1 <- Data_Range1.xts['T22:00:00/T23:59:59']}
      if(length(data_range1) > 0) {Morn1 <- Data_Range1.xts['T00:00:00/T21:14:59']}
      
    }
    
    if(gmt_offset2 == -5){
      
      if(length(data_range2) > 0) {Eve2 <- Data_Range2.xts['T23:00:00/T23:59:59']}
      if(length(data_range2) > 0) {Morn2 <- Data_Range2.xts['T00:00:00/T22:14:59']}
      
    } else if(gmt_offset2 == -4){
      
      if(length(data_range2) > 0) {Eve2 <- Data_Range2.xts['T22:00:00/T23:59:59']}
      if(length(data_range2) > 0) {Morn2 <- Data_Range2.xts['T00:00:00/T21:14:59']}
      
    }
    
    Week1 = NULL
    if (length(Eve1) > 0) { Week1 <- rbind(Week1, Eve1)}
    if (length(Morn1) > 0) { Week1 <- rbind(Week1, Morn1)}
    
    Week2 = NULL
    if (length(Eve2) > 0) { Week2 <- rbind(Week2, Eve2)}
    if (length(Morn2) > 0) { Week2 <- rbind(Week2, Morn2)}
    
    Week = rbind(Week1, Week2)    
  }
  
  spread_data = Week[,1:4]
  names(spread_data) = c("bid.1", "ask.1", "bid.2", "ask.2")
  rm(df1_list, df2_list)
  #ratios
  r1 <- 2
  r2 <- -1
  #multipliers
  m1 <- 1
  m2 <- -1
  spread_data$spread.bid = m1 * as.numeric(spread_data$bid.1) + m2 * as.numeric(spread_data$ask.2)
  spread_data$spread.ask = m1 * as.numeric(spread_data$ask.1) + m2 * as.numeric(spread_data$bid.2)
  spread_data$spread.mid = (spread_data$spread.ask + spread_data$spread.bid) / 2
  spread_data$spread.bidAskSpread = spread_data$spread.ask - spread_data$spread.bid
  
  #adding spread specifications
  spread_name <- "PLPA"
  spread_spec <- cbind(spread_name, r1, m1, r2, m2)
  
  #formatting data
  spread_zoo = zoo(spread_data)
  rm(spread_data)
  spread_digits <- format(spread_zoo[,5:8], digits=4)
  spread_fmt <- spread_zoo[,1:4]
  spread_cmb <- cbind(spread_fmt, spread_digits)
  
  sprd_filename <- paste(str_sprd_contract,"_",str_date,".csv",sep="")
  write.csv(spread_spec, sprd_filename)
  write.zoo(spread_cmb, sprd_filename, append = TRUE, sep = ",") 
}

build_ho_go <- function(str_sprd_contract, str_legA_contract, str_legB_contract, str_date) {
  
  #loading in the outright legs
  legA_filename <- paste(str_legA_contract,"_",str_date,"_quotes.csv",sep="")
  legB_filename <- paste(str_legB_contract,"_",str_date,"_quotes.csv",sep="")
  df1 <- read.csv(legA_filename)
  df2 <- read.csv(legB_filename)
  database <- read.csv("C:/Users/hhong.TRADECO/Desktop/stdev/Daylight_database.csv")
  db_nyc <- subset(database, database$City == "New York")
  
  #subsetting the data
  df1_sub <- df1[,c(1,5,6)]
  df2_sub <- df2[,c(1,5,6)]
  
  
  #format the date column mm/dd/YYYY HH:MM:ss.fff
  options(digits.secs = 3)
  df1_sub.t <- as.POSIXct(df1_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df2_sub.t <- as.POSIXct(df2_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df1_sub.t <- df1_sub.t + 0.0001
  df2_sub.t <- df2_sub.t + 0.0001
  
  #create zoo object
  df1_zoo <- zoo(df1_sub[2:3], df1_sub.t)
  df2_zoo <- zoo(df2_sub[2:3], df2_sub.t)
  
  rm(df2_sub.t)
  rm(df1_sub.t)
  
  #data dates
  startDate1 <- as.Date(as.POSIXct(first(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate1 <- as.Date(as.POSIXct(last(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate2 <- as.Date(as.POSIXct(first(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate2 <- as.Date(as.POSIXct(last(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate <- min(startDate1, startDate2)
  endDate <- max(endDate1, endDate2)
  
  
  # merging the data
  df1_list <- split(df1_zoo,as.Date(index(df1_zoo)))
  df2_list <- split(df2_zoo,as.Date(index(df2_zoo)))
  
  rm(df1_zoo)
  rm(df2_zoo)
  
  currentDate = startDate
  data = NULL
  while(currentDate <= endDate){
    setDate = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
    df1_Date = df1_list[[setDate]]
    df2_Date = df2_list[[setDate]]
    if(!is.null(df1_Date) && !is.null(df2_Date)){
      data_Date <- zoo(merge(df1_Date, df2_Date, all=TRUE))
    } else {
      data_Date = NULL
    }
    if(length(data_Date)!=0){
      data_Date <- na.locf(data_Date)
      data_Date <- na.omit(data_Date)
    }
    if(length(data_Date) == 0){
      data = data
    } else {
      data = append(data, data_Date, after = length(data))
    }
    currentDate = currentDate + 1
  }
  
  #adding in the day
  data$day <- as.POSIXlt(index(data))$wday
  data$day <- weekdays(as.Date(as.POSIXct(index(data))))
  
  
  
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
  print(paste('Start Date: ', startDate))
  print(paste('Start Date GMT Offset: ', gmt_offset_start))
  
  gmt_offset_end <- get_gmt_offset(endDate, db_nyc)
  print(paste('End Date: ', endDate))
  print(paste('End Date GMT Offset: ', gmt_offset_end))
  
  daylight_savings_change_date <- function(startDate, endDate, calendar_db){
    
    
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
  
  
  
  # if they're different... find the date it switches on
  # make two sub data sets, one before the date and one after
  # then process those two sub data sets based on the gmt offset
  # using the code below as a guide
  
  
  # if the start and end gmt offsets are equal
  if(gmt_offset_start == gmt_offset_end) {
    
    Sunday <- subset(data, data$day == 'Sunday')
    Sunday.xts <- xts(Sunday)
    Monday <- subset(data, data$day == 'Monday')
    Monday.xts <- xts(Monday)
    Tuesday <- subset(data, data$day == 'Tuesday')
    Tuesday.xts <- xts(Tuesday)
    Wednesday <- subset(data, data$day == 'Wednesday')
    Wednesday.xts <- xts(Wednesday)
    Thursday <- subset(data, data$day == 'Thursday')
    Thursday.xts <- xts(Thursday)
    Friday <- subset(data, data$day == 'Friday')
    Friday.xts <- xts(Friday)
    
    if (gmt_offset_start == -5) {
      if (length(Sunday) > 0) { Sunday <- Sunday.xts['T23:00:00/T23:59:59'] }
      if (length(Monday) > 0) { Monday <- Monday.xts['T00:00:00/T21:59:59'] }
      if (length(Tuesday) > 0) { Tuesday <- Tuesday.xts['T01:00:00/T21:59:59']}
      if (length(Wednesday) > 0) { Wednesday <- Wednesday.xts['T01:00:00/T21:59:59'] }
      if (length(Thursday) > 0) { Thursday <- Thursday.xts['T01:00:00/T21:59:59'] }
      if (length(Friday) > 0) { Friday <- Friday.xts['T01:00:00/T21:59:59'] }
    } else if (gmt_offset_start == -4) {
      if (length(Sunday) > 0) { Sunday <- Sunday.xts['T22:00:00/T23:59:59'] }
      if (length(Monday) > 0) { Monday <- Monday.xts['T00:00:00/T20:59:59'] }
      if (length(Tuesday) > 0) { Tuesday <- Tuesday.xts['T00:00:00/T20:59:59'] }
      if (length(Wednesday) > 0) { Wednesday <- Wednesday.xts['T00:00:00/T20:59:59'] }
      if (length(Thursday) > 0) { Thursday <- Thursday.xts['T00:00:00/T20:59:59'] }
      if (length(Friday) > 0) { Friday <- Friday.xts['T00:00:00/T20:59:59']}
    }
    
    Week = NULL
    if (length(Sunday) > 0) { Week <- rbind(Week, Sunday)}
    if (length(Monday) > 0) { Week <- rbind(Week, Monday)}
    if (length(Tuesday) > 0) { Week <- rbind(Week, Tuesday)}
    if (length(Wednesday) > 0) { Week <- rbind(Week, Wednesday)}
    if (length(Thursday) > 0) { Week <- rbind(Week, Thursday)}
    if (length(Friday) > 0) { Week <- rbind(Week, Friday)}
    
  } else {
    dst_change_date <- daylight_savings_change_date(startDate, endDate, db_nyc)
    data_range1 <- subset(data, as.Date(index(data)) < dst_change_date)
    data_range2 <- subset(data, as.Date(index(data)) >= dst_change_date)
    
    Sunday1 <- subset(data_range1, data_range1$day == 'Sunday')
    Sunday1.xts <- xts(Sunday1)
    Monday1 <- subset(data_range1, data_range1$day == 'Monday')
    Monday1.xts <- xts(Monday1)
    Tuesday1 <- subset(data_range1, data_range1$day == 'Tuesday')
    Tuesday1.xts <- xts(Tuesday1)
    Wednesday1 <- subset(data_range1, data_range1$day == 'Wednesday')
    Wednesday1.xts <- xts(Wednesday1)
    Thursday1 <- subset(data_range1, data_range1$day == 'Thursday')
    Thursday1.xts <- xts(Thursday1)
    Friday1 <- subset(data_range1, data_range1$day == 'Friday')
    Friday1.xts <- xts(Friday1)
    
    Sunday2 <- subset(data_range2, data_range2$day == 'Sunday')
    Sunday2.xts <- xts(Sunday2)
    Monday2 <- subset(data_range2, data_range2$day == 'Monday')
    Monday2.xts <- xts(Monday2)
    Tuesday2 <- subset(data_range2, data_range2$day == 'Tuesday')
    Tuesday2.xts <- xts(Tuesday2)
    Wednesday2 <- subset(data_range2, data_range2$day == 'Wednesday')
    Wednesday2.xts <- xts(Wednesday2)
    Thursday2 <- subset(data_range2, data_range2$day == 'Thursday')
    Thursday2.xts <- xts(Thursday2)
    Friday2 <- subset(data_range2, data_range2$day == 'Friday')
    Friday2.xts <- xts(Friday2)
    
    startDate_range1 <- as.Date(as.POSIXct(first(index(data_range1)), format = "%Y-%m-%d", tz = "GMT"))
    #endDate_range1 <- as.Date(as.POSIXct(last(index(date_range1)), format = "%Y-%m-%d", tz = "GMT"))
    startDate_range2 <- as.Date(as.POSIXct(first(index(data_range2)), format = "%Y-%m-%d", tz = "GMT"))
    #endDate_range2 <- as.Date(as.POSIXct(last(index(date_range2)), format = "%Y-%m-%d", tz = "GMT"))
    gmt_offset1 <- get_gmt_offset(startDate_range1, db_nyc)
    gmt_offset1
    gmt_offset2 <- get_gmt_offset(startDate_range2, db_nyc)
    gmt_offset2
    
    if (gmt_offset1 == -5){
      if (length(Sunday1) > 0) { Sunday1 <- Sunday1.xts['T23:00:00/T23:59:59']  }
      if (length(Monday1) > 0) { Monday1 <- Monday1.xts['T00:00:00/T21:59:59']  }
      if (length(Tuesday1) > 0) { Tuesday1 <- Tuesday1.xts['T01:00:00/T21:59:59']  }
      if (length(Wednesday1) > 0) { Wednesday1 <- Wednesday1.xts['T01:00:00/T21:59:59']  }
      if (length(Thursday1) > 0) { Thursday1 <- Thursday1.xts['T01:00:00/T21:59:59']  }
      if (length(Friday1) > 0) { Friday1 <- Friday1.xts['T01:00:00/T21:59:59']  }
    } else if(gmt_offset1 == -4){
      if (length(Sunday1) > 0) { Sunday1 <- Sunday1.xts['T22:00:00/T23:59:59']  }
      if (length(Monday1) > 0) { Monday1 <- Monday1.xts['T00:00:00/T20:59:59']  }
      if (length(Tuesday1) > 0) { Tuesday1 <- Tuesday1.xts['T00:00:00/T20:59:59']  }
      if (length(Wednesday1) > 0) { Wednesday1 <- Wednesday1.xts['T00:00:00/T20:59:59']  }
      if (length(Thursday1) > 0) { Thursday1 <- Thursday1.xts['T00:00:00/T20:59:59']  }
      if (length(Friday1) > 0) { Friday1 <- Friday1.xts['T00:00:00/T20:59:59']  }
    }
    
    if(gmt_offset2 == -5){
      if (length(Sunday2) > 0) { Sunday2 <- Sunday2.xts['T23:00:00/T23:59:59']  }
      if (length(Monday2) > 0) { Monday2 <- Monday2.xts['T00:00:00/T21:59:59']  }
      if (length(Tuesday2) > 0) { Tuesday2 <- Tuesday2.xts['T01:00:00/T21:59:59']  }
      if (length(Wednesday2) > 0) { Wednesday2 <- Wednesday2.xts['T01:00:00/T21:59:59']  }
      if (length(Thursday2) > 0) { Thursday2 <- Thursday2.xts['T01:00:00/T21:59:59']  }
      if (length(Friday2) > 0) { Friday2 <- Friday2.xts['T01:00:00/T21:59:59']  }
    } else if(gmt_offset2 == -4){
      if (length(Sunday2) > 0) { Sunday2 <- Sunday2.xts['T22:00:00/T23:59:59']  }
      if (length(Monday2) > 0) { Monday2 <- Monday2.xts['T00:00:00/T20:59:59']  }
      if (length(Tuesday2) > 0) { Tuesday2 <- Tuesday2.xts['T00:00:00/T20:59:59']  }
      if (length(Wednesday2) > 0) { Wednesday2 <- Wednesday2.xts['T00:00:00/T20:59:59']  }
      if (length(Thursday2) > 0) { Thursday2 <- Thursday2.xts['T00:00:00/T20:59:59']  }
      if (length(Friday2) > 0) { Friday2 <- Friday2.xts['T00:00:00/T20:59:59']  }
    }
    
    Week1 = NULL
    if (length(Sunday1) > 0) { Week1 <- rbind(Week1, Sunday1)}
    if (length(Monday1) > 0) { Week1 <- rbind(Week1, Monday1)}
    if (length(Tuesday1) > 0) { Week1 <- rbind(Week1, Tuesday1)}
    if (length(Wednesday1) > 0) { Week1 <- rbind(Week1, Wednesday1)}
    if (length(Thursday1) > 0) { Week1 <- rbind(Week1, Thursday1)}
    if (length(Friday1) > 0) { Week1 <- rbind(Week1, Friday1)}
    
    Week2 = NULL
    if (length(Sunday2) > 0) { Week2 <- rbind(Week2, Sunday2)}
    if (length(Monday2) > 0) { Week2 <- rbind(Week2, Monday2)}
    if (length(Tuesday2) > 0) { Week2 <- rbind(Week2, Tuesday2)}
    if (length(Wednesday2) > 0) { Week2 <- rbind(Week2, Wednesday2)}
    if (length(Thursday2) > 0) { Week2 <- rbind(Week2, Thursday2)}
    if (length(Friday2) > 0) { Week2 <- rbind(Week2, Friday2)}
    
    Week = rbind(Week1, Week2)    
  }
  
  spread_data = Week[,1:4]
  names(spread_data) = c("bid.1", "ask.1", "bid.2", "ask.2")
  #ratios
  r1 <- 3
  r2 <- -4
  #multipliers
  m1 <- 1
  m2 <- -0.003196
  spread_data$spread.bid = m1 * as.numeric(spread_data$bid.1) + m2 * as.numeric(spread_data$ask.2)
  spread_data$spread.ask = m1 * as.numeric(spread_data$ask.1) + m2 * as.numeric(spread_data$bid.2)
  spread_data$spread.mid = (spread_data$spread.ask + spread_data$spread.bid) / 2
  spread_data$spread.bidAskSpread = spread_data$spread.ask - spread_data$spread.bid
  
  #adding spread specifications
  spread_name <- "HOGO"
  spread_spec <- cbind(spread_name, r1, m1, r2, m2)
  
  #formatting data
  spread_zoo = zoo(spread_data)
  rm(spread_data)
  rm(Week)
  rm(df1_sub)
  rm(df2_sub)
  
  spread_digits <- format(spread_zoo[,5:8], digits=4)
  spread_fmt <- spread_zoo[,1:4]
  rm(spread_zoo)
  spread_cmb <- cbind(spread_fmt, spread_digits)
  rm(spread_fmt)
  
  sprd_filename <- paste(str_sprd_contract,"_",str_date,".csv",sep="")
  write.csv(spread_spec, sprd_filename)
  write.zoo(spread_cmb, sprd_filename, append = TRUE, sep = ",")
  
}

build_ho_cl <- function(str_sprd_contract, str_legA_contract, str_legB_contract, str_date) {
  
  #loading in the outright legs
  legA_filename <- paste(str_legA_contract,"_",str_date,"_quotes.csv",sep="")
  legB_filename <- paste(str_legB_contract,"_",str_date,"_quotes.csv",sep="")
  df1 <- read.csv(legA_filename)
  df2 <- read.csv(legB_filename)
  database <- read.csv("C:/Users/hhong.TRADECO/Desktop/stdev/Daylight_database.csv")
  db_nyc <- subset(database, database$City == "New York")
  
  #subsetting the data
  df1_sub <- df1[,c(1,5,6)]
  df2_sub <- df2[,c(1,5,6)]
  
  #df1_sub <- df1[,c(2,6,7)]
  #df2_sub <- df2[,c(2,6,7)]
  
  
  #format the date column mm/dd/YYYY HH:MM:ss.fff
  options(digits.secs = 6)
  df1_sub.t <- as.POSIXct(df1_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df2_sub.t <- as.POSIXct(df2_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df1_sub.t <- df1_sub.t + 0.0001
  df2_sub.t <- df2_sub.t + 0.0001
  
  #create zoo object
  df1_zoo <- zoo(df1_sub[2:3], df1_sub.t)
  df2_zoo <- zoo(df2_sub[2:3], df2_sub.t)
  rm(df1_sub.t, df2_sub.t)
  
  #data dates
  startDate1 <- as.Date(as.POSIXct(first(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate1 <- as.Date(as.POSIXct(last(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate2 <- as.Date(as.POSIXct(first(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate2 <- as.Date(as.POSIXct(last(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate <- max(startDate1, startDate2) + 1
  endDate <- min(endDate1, endDate2)
  
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
  
  df1_list <- split(df1_zoo,as.Date(index(df1_zoo)))
  df2_list <- split(df2_zoo,as.Date(index(df2_zoo)))
  rm(df1_zoo, df2_zoo)
  
  currentDate = startDate 
  data = NULL
  while(currentDate <= endDate + 1){
    gmt_offset <- get_gmt_offset(currentDate, db_nyc)
    if(gmt_offset == -4) {
      setDate1 = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate1]])){
        df1_Date_1 = as.xts(df1_list[[setDate1]])['T00:00:00.000/T21:14:59.999'] 
      } else { df1_Date_1 = NULL }
      if(!is.null(df2_list[[setDate1]])){
        df2_Date_1 = as.xts(df2_list[[setDate1]])['T00:00:00.000/T21:14:59.999'] 
      } else { df2_Date_1 = NULL }
      
      setDate2 = strftime(currentDate-1, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate2]])){
        df1_Date_2 = as.xts(df1_list[[setDate2]])['T22:00:00.000/T23:59:59.999'] 
      } else { df1_Date_2 = NULL }
      if(!is.null(df2_list[[setDate2]])){
        df2_Date_2 = as.xts(df2_list[[setDate2]])['T22:00:00.000/T23:59:59.999'] 
      } else { df2_Date_2 = NULL }
    } 
    if(gmt_offset == -5) {
      setDate1 = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate1]])){
        df1_Date_1 = as.xts(df1_list[[setDate1]])['T00:00:00.000/T22:14:59.999'] 
      } else { df1_Date_1 = NULL }
      if(!is.null(df2_list[[setDate1]])){
        df2_Date_1 = as.xts(df2_list[[setDate1]])['T00:00:00.000/T22:14:59.999'] 
      } else { df2_Date_1 = NULL }
      
      setDate2 = strftime(currentDate-1, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate2]])){
        df1_Date_2 = as.xts(df1_list[[setDate2]])['T23:00:00.000/T23:59:59.999'] 
      } else { df1_Date_2 = NULL }
      if(!is.null(df2_list[[setDate2]])){
        df2_Date_2 = as.xts(df2_list[[setDate2]])['T23:00:00.000/T23:59:59.999'] 
      } else { df2_Date_2 = NULL }
    } 
    
    df1_Date = rbind(df1_Date_1, df1_Date_2)
    df2_Date = rbind(df2_Date_1, df2_Date_2)
    
    data_Date <- merge(df1_Date, df2_Date, all=TRUE)
    if(length(data_Date)!=0){
      data_Date <- na.locf(data_Date)
      data_Date <- na.omit(data_Date)
      if(length(data_Date) == 0){
        data = data
      } else {
        data = append(data, data_Date, after = length(data))
      }
    }
    currentDate = currentDate + 1
  }
  
  
  #adding in the day
  #data$day <- as.POSIXlt(index(data))$wday
  #data$day <- weekdays(as.Date(as.POSIXct(index(data))))
  
  
  
  # if they're different... find the date it switches on
  # make two sub data sets, one before the date and one after
  # then process those two sub data sets based on the gmt offset
  # using the code below as a guide
  
  
  # if the start and end gmt offsets are equal
  if(gmt_offset_start == gmt_offset_end) {
    
    Data.xts <- xts(data)
    
    if (gmt_offset_start == -5) {
      if (length(data) > 0) { Eve <- Data.xts['T23:00:00/T23:59:59']}
      if (length(data) > 0) { Morn <- Data.xts['T00:00:00/T22:14:59']}
      
    } else if (gmt_offset_start == -4) {
      if (length(data) > 0) { Eve <- Data.xts['T22:00:00/T23:59:59']}
      if (length(data) > 0) { Morn <- Data.xts['T00:00:00/T21:14:59']}
      
    }
    
    Week = NULL
    if (length(Eve) > 0) { Week <- rbind(Week, Eve)}
    if (length(Morn) > 0) { Week <- rbind(Week, Morn)}
    
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
      
      if(length(data_range1) > 0) {Eve1 <- Data_Range1.xts['T23:00:00/T23:59:59']}
      if(length(data_range1) > 0) {Morn1 <- Data_Range1.xts['T00:00:00/T22:14:59']}
      
    } else if(gmt_offset1 == -4){
      
      if(length(data_range1) > 0) {Eve1 <- Data_Range1.xts['T22:00:00/T23:59:59']}
      if(length(data_range1) > 0) {Morn1 <- Data_Range1.xts['T00:00:00/T21:14:59']}
      
    }
    
    if(gmt_offset2 == -5){
      
      if(length(data_range2) > 0) {Eve2 <- Data_Range2.xts['T23:00:00/T23:59:59']}
      if(length(data_range2) > 0) {Morn2 <- Data_Range2.xts['T00:00:00/T22:14:59']}
      
    } else if(gmt_offset2 == -4){
      
      if(length(data_range2) > 0) {Eve2 <- Data_Range2.xts['T22:00:00/T23:59:59']}
      if(length(data_range2) > 0) {Morn2 <- Data_Range2.xts['T00:00:00/T21:14:59']}
      
    }
    
    Week1 = NULL
    if (length(Eve1) > 0) { Week1 <- rbind(Week1, Eve1)}
    if (length(Morn1) > 0) { Week1 <- rbind(Week1, Morn1)}
    
    Week2 = NULL
    if (length(Eve2) > 0) { Week2 <- rbind(Week2, Eve2)}
    if (length(Morn2) > 0) { Week2 <- rbind(Week2, Morn2)}
    
    Week = rbind(Week1, Week2)    
  }
  
  spread_data = Week[,1:4]
  names(spread_data) = c("bid.1", "ask.1", "bid.2", "ask.2")
  rm(df1_list, df2_list)
  #ratios
  r1 <- 1
  r2 <- -1
  #multipliers
  m1 <- 42
  m2 <- -1
  spread_data$spread.bid = m1 * as.numeric(spread_data$bid.1) + m2 * as.numeric(spread_data$ask.2)
  spread_data$spread.ask = m1 * as.numeric(spread_data$ask.1) + m2 * as.numeric(spread_data$bid.2)
  spread_data$spread.mid = (spread_data$spread.ask + spread_data$spread.bid) / 2
  spread_data$spread.bidAskSpread = spread_data$spread.ask - spread_data$spread.bid
  
  #adding spread specifications
  spread_name <- "HOCL"
  spread_spec <- cbind(spread_name, r1, m1, r2, m2)
  
  #formatting data
  spread_zoo = zoo(spread_data)
  rm(spread_data)
  spread_digits <- format(spread_zoo[,5:8], digits=4)
  spread_fmt <- spread_zoo[,1:4]
  spread_cmb <- cbind(spread_fmt, spread_digits)
  
  sprd_filename <- paste(str_sprd_contract,"_",str_date,".csv",sep="")
  write.csv(spread_spec, sprd_filename)
  write.zoo(spread_cmb, sprd_filename, append = TRUE, sep = ",")
}

build_ti_brent <- function(str_sprd_contract, str_legA_contract, str_legB_contract, str_date) {
  
  #loading in the outright legs
  legA_filename <- paste(str_legA_contract,"_",str_date,"_quotes.csv",sep="")
  legB_filename <- paste(str_legB_contract,"_",str_date,"_quotes.csv",sep="")
  df1 <- read.csv(legA_filename)
  df2 <- read.csv(legB_filename)
  database <- read.csv("C:/Users/hhong.TRADECO/Desktop/stdev/Daylight_database.csv")
  db_nyc <- subset(database, database$City == "New York")
  
  #subsetting the data
  df1_sub <- df1[,c(1,5,6)]
  df2_sub <- df2[,c(1,5,6)]
  
  
  #format the date column mm/dd/YYYY HH:MM:ss.fff
  options(digits.secs = 3)
  df1_sub.t <- as.POSIXct(df1_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df2_sub.t <- as.POSIXct(df2_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df1_sub.t <- df1_sub.t + 0.0001
  df2_sub.t <- df2_sub.t + 0.0001
  
  #create zoo object
  df1_zoo <- zoo(df1_sub[2:3], df1_sub.t)
  df2_zoo <- zoo(df2_sub[2:3], df2_sub.t)
  
  rm(df2_sub.t)
  rm(df1_sub.t)
  
  #data dates
  startDate1 <- as.Date(as.POSIXct(first(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate1 <- as.Date(as.POSIXct(last(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate2 <- as.Date(as.POSIXct(first(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate2 <- as.Date(as.POSIXct(last(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate <- min(startDate1, startDate2)
  endDate <- max(endDate1, endDate2)
  
  
  # merging the data
  df1_list <- split(df1_zoo,as.Date(index(df1_zoo)))
  df2_list <- split(df2_zoo,as.Date(index(df2_zoo)))
  
  rm(df1_zoo)
  rm(df2_zoo)
  
  currentDate = startDate
  data = NULL
  while(currentDate <= endDate){
    setDate = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
    df1_Date = df1_list[[setDate]]
    df2_Date = df2_list[[setDate]]
    if(!is.null(df1_Date) && !is.null(df2_Date)){
      data_Date <- zoo(merge(df1_Date, df2_Date, all=TRUE))
    } else {
      data_Date = NULL
    }
    if(length(data_Date)!=0){
      data_Date <- na.locf(data_Date)
      data_Date <- na.omit(data_Date)
    }
    if(length(data_Date) == 0){
      data = data
    } else {
      data = append(data, data_Date, after = length(data))
    }
    currentDate = currentDate + 1
  }
  
  #adding in the day
  data$day <- as.POSIXlt(index(data))$wday
  data$day <- weekdays(as.Date(as.POSIXct(index(data))))
  
  
  
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
  print(paste('Start Date: ', startDate))
  print(paste('Start Date GMT Offset: ', gmt_offset_start))
  
  gmt_offset_end <- get_gmt_offset(endDate, db_nyc)
  print(paste('End Date: ', endDate))
  print(paste('End Date GMT Offset: ', gmt_offset_end))
  
  daylight_savings_change_date <- function(startDate, endDate, calendar_db){
    
    
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
  
  
  
  # if they're different... find the date it switches on
  # make two sub data sets, one before the date and one after
  # then process those two sub data sets based on the gmt offset
  # using the code below as a guide
  
  
  # if the start and end gmt offsets are equal
  if(gmt_offset_start == gmt_offset_end) {
    
    Sunday <- subset(data, data$day == 'Sunday')
    Sunday.xts <- xts(Sunday)
    Monday <- subset(data, data$day == 'Monday')
    Monday.xts <- xts(Monday)
    Tuesday <- subset(data, data$day == 'Tuesday')
    Tuesday.xts <- xts(Tuesday)
    Wednesday <- subset(data, data$day == 'Wednesday')
    Wednesday.xts <- xts(Wednesday)
    Thursday <- subset(data, data$day == 'Thursday')
    Thursday.xts <- xts(Thursday)
    Friday <- subset(data, data$day == 'Friday')
    Friday.xts <- xts(Friday)
    
    if (gmt_offset_start == -5) {
      if (length(Sunday) > 0) { Sunday <- Sunday.xts['T23:00:00/T23:59:59'] }
      if (length(Monday) > 0) { Monday <- Monday.xts['T00:00:00/T21:59:59'] }
      if (length(Tuesday) > 0) { Tuesday <- Tuesday.xts['T01:00:00/T21:59:59']}
      if (length(Wednesday) > 0) { Wednesday <- Wednesday.xts['T01:00:00/T21:59:59'] }
      if (length(Thursday) > 0) { Thursday <- Thursday.xts['T01:00:00/T21:59:59'] }
      if (length(Friday) > 0) { Friday <- Friday.xts['T01:00:00/T21:59:59'] }
    } else if (gmt_offset_start == -4) {
      if (length(Sunday) > 0) { Sunday <- Sunday.xts['T22:00:00/T23:59:59'] }
      if (length(Monday) > 0) { Monday <- Monday.xts['T00:00:00/T20:59:59'] }
      if (length(Tuesday) > 0) { Tuesday <- Tuesday.xts['T00:00:00/T20:59:59'] }
      if (length(Wednesday) > 0) { Wednesday <- Wednesday.xts['T00:00:00/T20:59:59'] }
      if (length(Thursday) > 0) { Thursday <- Thursday.xts['T00:00:00/T20:59:59'] }
      if (length(Friday) > 0) { Friday <- Friday.xts['T00:00:00/T20:59:59']}
    }
    
    Week = NULL
    if (length(Sunday) > 0) { Week <- rbind(Week, Sunday)}
    if (length(Monday) > 0) { Week <- rbind(Week, Monday)}
    if (length(Tuesday) > 0) { Week <- rbind(Week, Tuesday)}
    if (length(Wednesday) > 0) { Week <- rbind(Week, Wednesday)}
    if (length(Thursday) > 0) { Week <- rbind(Week, Thursday)}
    if (length(Friday) > 0) { Week <- rbind(Week, Friday)}
    
  } else {
    dst_change_date <- daylight_savings_change_date(startDate, endDate, db_nyc)
    data_range1 <- subset(data, as.Date(index(data)) < dst_change_date)
    data_range2 <- subset(data, as.Date(index(data)) >= dst_change_date)
    
    Sunday1 <- subset(data_range1, data_range1$day == 'Sunday')
    Sunday1.xts <- xts(Sunday1)
    Monday1 <- subset(data_range1, data_range1$day == 'Monday')
    Monday1.xts <- xts(Monday1)
    Tuesday1 <- subset(data_range1, data_range1$day == 'Tuesday')
    Tuesday1.xts <- xts(Tuesday1)
    Wednesday1 <- subset(data_range1, data_range1$day == 'Wednesday')
    Wednesday1.xts <- xts(Wednesday1)
    Thursday1 <- subset(data_range1, data_range1$day == 'Thursday')
    Thursday1.xts <- xts(Thursday1)
    Friday1 <- subset(data_range1, data_range1$day == 'Friday')
    Friday1.xts <- xts(Friday1)
    
    Sunday2 <- subset(data_range2, data_range2$day == 'Sunday')
    Sunday2.xts <- xts(Sunday2)
    Monday2 <- subset(data_range2, data_range2$day == 'Monday')
    Monday2.xts <- xts(Monday2)
    Tuesday2 <- subset(data_range2, data_range2$day == 'Tuesday')
    Tuesday2.xts <- xts(Tuesday2)
    Wednesday2 <- subset(data_range2, data_range2$day == 'Wednesday')
    Wednesday2.xts <- xts(Wednesday2)
    Thursday2 <- subset(data_range2, data_range2$day == 'Thursday')
    Thursday2.xts <- xts(Thursday2)
    Friday2 <- subset(data_range2, data_range2$day == 'Friday')
    Friday2.xts <- xts(Friday2)
    
    startDate_range1 <- as.Date(as.POSIXct(first(index(data_range1)), format = "%Y-%m-%d", tz = "GMT"))
    #endDate_range1 <- as.Date(as.POSIXct(last(index(date_range1)), format = "%Y-%m-%d", tz = "GMT"))
    startDate_range2 <- as.Date(as.POSIXct(first(index(data_range2)), format = "%Y-%m-%d", tz = "GMT"))
    #endDate_range2 <- as.Date(as.POSIXct(last(index(date_range2)), format = "%Y-%m-%d", tz = "GMT"))
    gmt_offset1 <- get_gmt_offset(startDate_range1, db_nyc)
    gmt_offset1
    gmt_offset2 <- get_gmt_offset(startDate_range2, db_nyc)
    gmt_offset2
    
    if (gmt_offset1 == -5){
      if (length(Sunday1) > 0) { Sunday1 <- Sunday1.xts['T23:00:00/T23:59:59']  }
      if (length(Monday1) > 0) { Monday1 <- Monday1.xts['T00:00:00/T21:59:59']  }
      if (length(Tuesday1) > 0) { Tuesday1 <- Tuesday1.xts['T01:00:00/T21:59:59']  }
      if (length(Wednesday1) > 0) { Wednesday1 <- Wednesday1.xts['T01:00:00/T21:59:59']  }
      if (length(Thursday1) > 0) { Thursday1 <- Thursday1.xts['T01:00:00/T21:59:59']  }
      if (length(Friday1) > 0) { Friday1 <- Friday1.xts['T01:00:00/T21:59:59']  }
    } else if(gmt_offset1 == -4){
      if (length(Sunday1) > 0) { Sunday1 <- Sunday1.xts['T22:00:00/T23:59:59']  }
      if (length(Monday1) > 0) { Monday1 <- Monday1.xts['T00:00:00/T20:59:59']  }
      if (length(Tuesday1) > 0) { Tuesday1 <- Tuesday1.xts['T00:00:00/T20:59:59']  }
      if (length(Wednesday1) > 0) { Wednesday1 <- Wednesday1.xts['T00:00:00/T20:59:59']  }
      if (length(Thursday1) > 0) { Thursday1 <- Thursday1.xts['T00:00:00/T20:59:59']  }
      if (length(Friday1) > 0) { Friday1 <- Friday1.xts['T00:00:00/T20:59:59']  }
    }
    
    if(gmt_offset2 == -5){
      if (length(Sunday2) > 0) { Sunday2 <- Sunday2.xts['T23:00:00/T23:59:59']  }
      if (length(Monday2) > 0) { Monday2 <- Monday2.xts['T00:00:00/T21:59:59']  }
      if (length(Tuesday2) > 0) { Tuesday2 <- Tuesday2.xts['T01:00:00/T21:59:59']  }
      if (length(Wednesday2) > 0) { Wednesday2 <- Wednesday2.xts['T01:00:00/T21:59:59']  }
      if (length(Thursday2) > 0) { Thursday2 <- Thursday2.xts['T01:00:00/T21:59:59']  }
      if (length(Friday2) > 0) { Friday2 <- Friday2.xts['T01:00:00/T21:59:59']  }
    } else if(gmt_offset2 == -4){
      if (length(Sunday2) > 0) { Sunday2 <- Sunday2.xts['T22:00:00/T23:59:59']  }
      if (length(Monday2) > 0) { Monday2 <- Monday2.xts['T00:00:00/T20:59:59']  }
      if (length(Tuesday2) > 0) { Tuesday2 <- Tuesday2.xts['T00:00:00/T20:59:59']  }
      if (length(Wednesday2) > 0) { Wednesday2 <- Wednesday2.xts['T00:00:00/T20:59:59']  }
      if (length(Thursday2) > 0) { Thursday2 <- Thursday2.xts['T00:00:00/T20:59:59']  }
      if (length(Friday2) > 0) { Friday2 <- Friday2.xts['T00:00:00/T20:59:59']  }
    }
    
    Week1 = NULL
    if (length(Sunday1) > 0) { Week1 <- rbind(Week1, Sunday1)}
    if (length(Monday1) > 0) { Week1 <- rbind(Week1, Monday1)}
    if (length(Tuesday1) > 0) { Week1 <- rbind(Week1, Tuesday1)}
    if (length(Wednesday1) > 0) { Week1 <- rbind(Week1, Wednesday1)}
    if (length(Thursday1) > 0) { Week1 <- rbind(Week1, Thursday1)}
    if (length(Friday1) > 0) { Week1 <- rbind(Week1, Friday1)}
    
    Week2 = NULL
    if (length(Sunday2) > 0) { Week2 <- rbind(Week2, Sunday2)}
    if (length(Monday2) > 0) { Week2 <- rbind(Week2, Monday2)}
    if (length(Tuesday2) > 0) { Week2 <- rbind(Week2, Tuesday2)}
    if (length(Wednesday2) > 0) { Week2 <- rbind(Week2, Wednesday2)}
    if (length(Thursday2) > 0) { Week2 <- rbind(Week2, Thursday2)}
    if (length(Friday2) > 0) { Week2 <- rbind(Week2, Friday2)}
    
    Week = rbind(Week1, Week2)    
  }
  
  spread_data = Week[,1:4]
  names(spread_data) = c("bid.1", "ask.1", "bid.2", "ask.2")
  #ratios
  r1 <- 1
  r2 <- -1
  #multipliers
  m1 <- 1
  m2 <- -1
  spread_data$spread.bid = m1 * as.numeric(spread_data$bid.1) + m2 * as.numeric(spread_data$ask.2)
  spread_data$spread.ask = m1 * as.numeric(spread_data$ask.1) + m2 * as.numeric(spread_data$bid.2)
  spread_data$spread.mid = (spread_data$spread.ask + spread_data$spread.bid) / 2
  spread_data$spread.bidAskSpread = spread_data$spread.ask - spread_data$spread.bid
  
  #adding spread specifications
  spread_name <- "TIBR"
  spread_spec <- cbind(spread_name, r1, m1, r2, m2)
  
  #formatting data
  spread_zoo = zoo(spread_data)
  rm(spread_data)
  rm(Week)
  rm(df1_sub)
  rm(df2_sub)
  
  spread_digits <- format(spread_zoo[,5:8], digits=4)
  spread_fmt <- spread_zoo[,1:4]
  rm(spread_zoo)
  spread_cmb <- cbind(spread_fmt, spread_digits)
  rm(spread_fmt)
  
  sprd_filename <- paste(str_sprd_contract,"_",str_date,".csv",sep="")
  write.csv(spread_spec, sprd_filename)
  write.zoo(spread_cmb, sprd_filename, append = TRUE, sep = ",")
  
}

build_sugar <- function(str_sprd_contract, str_legA_contract, str_legB_contract, str_date) {
  #loading in the outright legs
  legA_filename <- paste(str_legA_contract,"_",str_date,"_quotes.csv",sep="")
  legB_filename <- paste(str_legB_contract,"_",str_date,"_quotes.csv",sep="")
  df1 <- read.csv(legA_filename)
  df2 <- read.csv(legB_filename)
  database <- read.csv("C:/Users/hhong.TRADECO/Desktop/stdev/Daylight_database.csv")
  db_nyc <- subset(database, database$City == "New York")
  db_lon <- subset(database, database$City == "London")
  
  #subsetting the data
  df1_sub <- df1[,c(1,5,6)]
  df2_sub <- df2[,c(1,5,6)]
  
  #df1_sub <- df1[,c(2,6,7)]
  #df2_sub <- df2[,c(2,6,7)]
  
  
  #format the date column mm/dd/YYYY HH:MM:ss.fff
  options(digits.secs = 6)
  df1_sub.t <- as.POSIXct(df1_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df2_sub.t <- as.POSIXct(df2_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df1_sub.t <- df1_sub.t + 0.0001
  df2_sub.t <- df2_sub.t + 0.0001
  
  #create zoo object
  df1_zoo <- zoo(df1_sub[2:3], df1_sub.t)
  df2_zoo <- zoo(df2_sub[2:3], df2_sub.t)
  
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
  
  
  
  daylight_savings_change_date <- function(startDate, endDate, calendar_db){
    
    
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
  
  
  #data dates
  startDate1 <- as.Date(as.POSIXct(first(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate1 <- as.Date(as.POSIXct(last(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate2 <- as.Date(as.POSIXct(first(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate2 <- as.Date(as.POSIXct(last(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate <- min(startDate1, startDate2)
  endDate <- max(endDate1, endDate2)
  
  # check the GMT offset of the start and end date in 'data'
  gmt_offset_nyc_start <- get_gmt_offset(startDate, db_nyc)
  cat('Start Date: ')
  print(startDate)
  cat('Start Date GMT Offset: ')
  print(gmt_offset_nyc_start)
  
  gmt_offset_nyc_end <- get_gmt_offset(endDate, db_nyc)
  cat('End Date: ')
  print(endDate)
  cat('End Date GMT Offset: ')
  print(gmt_offset_nyc_end)
  
  gmt_offset_lon_start <- get_gmt_offset(startDate, db_lon)
  cat('Start Date: ')
  print(startDate)
  cat('Start Date GMT Offset: ')
  print(gmt_offset_lon_start)
  
  gmt_offset_lon_end <- get_gmt_offset(endDate, db_lon)
  cat('End Date: ')
  print(endDate)
  cat('End Date GMT Offset: ')
  print(gmt_offset_lon_end)
  
  
  
  # merging the data
  df1_list <- split(df1_zoo,as.Date(index(df1_zoo)))
  df2_list <- split(df2_zoo,as.Date(index(df2_zoo)))
  
  
  currentDate = startDate
  data = NULL
  while(currentDate <= endDate){
    setDate = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
    df1_Date = df1_list[[setDate]]
    df2_Date = df2_list[[setDate]]
    if(!is.null(df1_Date) && !is.null(df2_Date)){
      data_Date <- zoo(merge(df1_Date, df2_Date, all=TRUE))
    } else {
      data_Date = NULL
    }
    if(length(data_Date)!=0){
      data_Date <- na.locf(data_Date)
      data_Date <- na.omit(data_Date)
    }
    if(length(data_Date) == 0){
      data = data
    } else {
      data = append(data, data_Date, after = length(data))
    }
    currentDate = currentDate + 1
  }
  
  
  #adding in the day
  data$day <- as.POSIXlt(index(data))$wday
  data$day <- weekdays(as.Date(as.POSIXct(index(data))))
  
  
  
  # if they're different... find the date it switches on
  # make two sub data sets, one before the date and one after
  # then process those two sub data sets based on the gmt offset
  # using the code below as a guide
  
  
  # if the start and end gmt offsets are equal
  if(gmt_offset_nyc_start == gmt_offset_nyc_end && gmt_offset_lon_start == gmt_offset_lon_end) {
    
    Data.xts <- xts(data)
    
    if (gmt_offset_nyc_start == -4 && gmt_offset_lon_start == 1) {
      if (length(data) > 0) { Data <- Data.xts['T07:45:00/T16:59:59']}
      
    } else if (gmt_offset_nyc_start == -4 && gmt_offset_lon_start == 0) {
      if (length(data) > 0) { Data <- Data.xts['T08:45:00/T16:54:59']}
      
    } else if (gmt_offset_nyc_start == -5 && gmt_offset_lon_start == 0) {
      if (length(data) > 0) { Data <- Data.xts['T08:45:00/T17:59:59']}
    }
    
    Week = NULL
    if (length(Data) > 0) { Week <- rbind(Week, Data)}
    
  } else {
    dst_change_date_nyc <- daylight_savings_change_date(startDate, endDate, db_nyc)
    dst_change_date_lon <- daylight_savings_change_date(startDate, endDate, db_lon)
    
    dst_change_date = paste(dst_change_date_lon, dst_change_date_nyc, sep="")
    
    data_range1 <- subset(data, as.Date(index(data)) < dst_change_date)
    data_range2 <- subset(data, as.Date(index(data)) >= dst_change_date)
    
    Data_Range1.xts <- xts(data_range1)
    Data_Range2.xts <- xts(data_range2)
    
    
    startDate_range1 <- as.Date(as.POSIXct(first(index(data_range1)), format = "%Y-%m-%d", tz = "GMT"))
    #endDate_range1 <- as.Date(as.POSIXct(last(index(date_range1)), format = "%Y-%m-%d", tz = "GMT"))
    startDate_range2 <- as.Date(as.POSIXct(first(index(data_range2)), format = "%Y-%m-%d", tz = "GMT"))
    #endDate_range2 <- as.Date(as.POSIXct(last(index(date_range2)), format = "%Y-%m-%d", tz = "GMT"))
    gmt_nyc_offset1 <- get_gmt_offset(startDate_range1, db_nyc)
    gmt_nyc_offset1
    gmt_nyc_offset2 <- get_gmt_offset(startDate_range2, db_nyc)
    gmt_nyc_offset2
    gmt_lon_offset1 <- get_gmt_offset(startDate_range1, db_lon)
    gmt_lon_offset1
    gmt_lon_offset2 <- get_gmt_offset(startDate_range2, db_lon)
    gmt_lon_offset2
    
    
    if (gmt_nyc_offset1 == -4 && gmt_nyc_offset2 == -4 && gmt_lon_offset1 == 0 && gmt_lon_offset2 == 1){
      
      if(length(data_range1) > 0) { Data1 <- Data_Range1.xts['T08:45:00/T16:54:59']}
      if(length(data_range2) > 0) { Data2 <- Data_Range2.xts['T07:45:00/T16:59:59']}
      
    } else if(gmt_nyc_offset1 == -5 && gmt_nyc_offset2 == -4 && gmt_lon_offset1 == 0 && gmt_lon_offset2 == 0){
      
      if(length(data_range1) > 0) { Data1 <- Data_Range1.xts['T08:45:00/T17:59:59']}
      if(length(data_range2) > 0) { Data2 <- Data_Range2.xts['T08:45:00/T16:54:59']}
      
    } else if(gmt_nyc_offset1 == -4 && gmt_nyc_offset2 == -5 && gmt_lon_offset1 == 0 && gmt_lon_offset2 == 0){
      
      if(length(data_range1) > 0) { Data1 <- Data_Range1.xts['T08:45:00/T16:54:59']}
      if(length(data_range2) > 0) { Data2 <- Data_Range2.xts['T08:45:00/T17:59:59']}
      
    } else if(gmt_nyc_offset1 == -4 && gmt_nyc_offset2 == -4 && gmt_lon_offset1 == 1 && gmt_lon_offset2 == 0){
      
      if(length(data_range1) > 0) { Data1 <- Data_Range1.xts['T07:45:00/T16:59:59']}
      if(length(data_range2) > 0) { Data2 <- Data_Range2.xts['T08:45:00/T16:54:59']}
      
    } 
    
    
    Week = NULL
    if (length(Data1) > 0) { Week <- rbind(Week, Data1)}
    if (length(Data2) > 0) { Week <- rbind(Week, Data2)}
    
    
  }
  
  spread_data = Week[,1:4]
  names(spread_data) = c("bid.1", "ask.1", "bid.2", "ask.2")
  #ratios
  r1 <- 1
  r2 <- -1
  #multipliers
  m1 <- 0.045
  m2 <- -1
  spread_data$spread.bid = m1 * as.numeric(spread_data$bid.1) + m2 * as.numeric(spread_data$ask.2)
  spread_data$spread.ask = m1 * as.numeric(spread_data$ask.1) + m2 * as.numeric(spread_data$bid.2)
  spread_data$spread.mid = (spread_data$spread.ask + spread_data$spread.bid) / 2
  spread_data$spread.bidAskSpread = spread_data$spread.ask - spread_data$spread.bid
  
  #adding spread specifications
  spread_name <- "Sugar"
  spread_spec <- cbind(spread_name, r1, m1, r2, m2)
  
  #formatting data
  spread_zoo = zoo(spread_data)
  spread_digits <- format(spread_zoo[,5:8], digits=4)
  spread_fmt <- spread_zoo[,1:4]
  spread_cmb <- cbind(spread_fmt, spread_digits)
  
  sprd_filename <- paste(str_sprd_contract,"_",str_date,".csv",sep="")
  write.csv(spread_spec, sprd_filename)
  write.zoo(spread_cmb, sprd_filename, append = TRUE, sep = ",")
}

build_cocoa <- function(str_sprd_contract, str_legA_contract, str_legB_contract, str_legC_contract, str_date) {
  #loading in the outright cocoa legs
  legA_filename <- paste(str_legA_contract,"_",str_date,"_quotes.csv",sep="")
  legB_filename <- paste(str_legB_contract,"_",str_date,"_quotes.csv",sep="")
  legC_filename <- paste(str_legC_contract,"_",str_date,"_quotes.csv",sep="")
  df1 <- read.csv(legA_filename)
  df2 <- read.csv(legB_filename)
  df3 <- read.csv(legC_filename)
  
  #BP_df1 <- read.csv("BPU4_Z4_quotes.csv")
  #BP_df2 <- read.csv("BPZ4_Z4_quotes.csv")
  #BP_df3 <- read.csv("BPH5_H5_quotes.csv")
  
  #combining 
  #df3 <- rbind(BP_df1, BP_df2)
  #df3 <- rbind(BP_df1, BP_df2, BP_df3)
  
  #loading the database for GMT offset
  database <- read.csv("C:/Users/hhong.TRADECO/Desktop/stdev/Daylight_database.csv")
  db_lon <- subset(database, database$City == "London")
  #db_nyc <- subset(database, database$City == "New York")
  
  #subsetting the data
  df1_sub <- df1[,c(1,5,6)]
  df2_sub <- df2[,c(1,5,6)]
  df3_sub <- df3[,c(1,5,6)]
  
  
  #format the date column mm/dd/YYYY HH:MM:ss.fff
  options(digits.secs = 6)
  df1_sub.t <- as.POSIXct(df1_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df2_sub.t <- as.POSIXct(df2_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df3_sub.t <- as.POSIXct(df3_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  
  df1_sub.t <- df1_sub.t + 0.0001
  df2_sub.t <- df2_sub.t + 0.0001
  df3_sub.t <- df3_sub.t + 0.0001
  
  #create zoo object
  df1_zoo <- zoo(df1_sub[2:3], df1_sub.t)
  df2_zoo <- zoo(df2_sub[2:3], df2_sub.t)
  df3_zoo <- zoo(df3_sub[2:3], df3_sub.t)
  
  #data dates
  startDate1 <- as.Date(as.POSIXct(first(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate1 <- as.Date(as.POSIXct(last(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate2 <- as.Date(as.POSIXct(first(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate2 <- as.Date(as.POSIXct(last(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate3 <- as.Date(as.POSIXct(first(index(df3_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate3 <- as.Date(as.POSIXct(last(index(df3_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate <- max(startDate1, startDate2, startDate3)
  endDate <- min(endDate1, endDate2, endDate3)
  
  
  # merging the data
  df1_list <- split(df1_zoo,as.Date(index(df1_zoo)))
  df2_list <- split(df2_zoo,as.Date(index(df2_zoo)))
  df3_list <- split(df3_zoo,as.Date(index(df3_zoo)))
  
  currentDate = startDate
  data = NULL
  while(currentDate <= endDate){
    setDate = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
    df1_Date = df1_list[[setDate]]
    df2_Date = df2_list[[setDate]]
    df3_Date = df3_list[[setDate]]
    if(!is.null(df1_Date) && !is.null(df2_Date) && !is.null(df3_Date)){
      data_Date <- zoo(merge(df1_Date, df2_Date, df3_Date, all=TRUE))
    } else {
      data_Date = NULL
    }
    if(length(data_Date)!=0){
      data_Date <- na.locf(data_Date)
      data_Date <- na.omit(data_Date)
    }
    if(length(data_Date) == 0){
      data = data
    } else {
      data = append(data, data_Date, after = length(data))
    }
    currentDate = currentDate + 1
  }
  
  #adding in the day
  #data$day <- as.POSIXlt(index(data))$wday
  #data$day <- weekdays(as.Date(as.POSIXct(index(data))))
  
  
  
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
  gmt_offset_start <- get_gmt_offset(startDate, db_lon)
  cat('Start Date: ')
  print(startDate)
  cat('Start Date GMT Offset: ')
  print(gmt_offset_start)
  
  gmt_offset_end <- get_gmt_offset(endDate, db_lon)
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
  
  
  
  # if they're different... find the date it switches on
  # make two sub data sets, one before the date and one after
  # then process those two sub data sets based on the gmt offset
  # using the code below as a guide
  
  
  # if the start and end gmt offsets are equal
  if(gmt_offset_start == gmt_offset_end) {
    
    Data.xts <- xts(data)
    
    if (gmt_offset_start == 1) {
      if (length(data) > 0) { Data <- Data.xts['T08:45:00/T15:54:59']}
      
    } else if (gmt_offset_start == 0) {
      if (length(data) > 0) { Data <- Data.xts['T09:45:00/T16:54:59']}
      
    }
    
    Week = NULL
    if (length(Data) > 0) { Week <- rbind(Week, Data)}
    
  } else {
    dst_change_date <- daylight_savings_change_date(startDate, endDate, db_lon)
    data_range1 <- subset(data, as.Date(index(data)) < dst_change_date)
    data_range2 <- subset(data, as.Date(index(data)) >= dst_change_date)
    
    Data_Range1.xts <- xts(data_range1)
    Data_Range2.xts <- xts(data_range2)
    
    
    startDate_range1 <- as.Date(as.POSIXct(first(index(data_range1)), format = "%Y-%m-%d", tz = "GMT"))
    #endDate_range1 <- as.Date(as.POSIXct(last(index(date_range1)), format = "%Y-%m-%d", tz = "GMT"))
    startDate_range2 <- as.Date(as.POSIXct(first(index(data_range2)), format = "%Y-%m-%d", tz = "GMT"))
    #endDate_range2 <- as.Date(as.POSIXct(last(index(date_range2)), format = "%Y-%m-%d", tz = "GMT"))
    gmt_offset1 <- get_gmt_offset(startDate_range1, db_lon)
    gmt_offset1
    gmt_offset2 <- get_gmt_offset(startDate_range2, db_lon)
    gmt_offset2
    
    if (gmt_offset1 == 1){
      
      if(length(data_range1) > 0) {Data1 <- Data_Range1.xts['T08:45:00/T15:54:59']}
      
    } else if(gmt_offset1 == 0){
      
      if(length(data_range1) > 0) {Data1 <- Data_Range1.xts['T09:45:00/T16:54:59']}
      
    }
    
    if(gmt_offset2 == 1){
      
      if(length(data_range2) > 0) {Data2 <- Data_Range2.xts['T08:45:00/T15:54:59']}
      
    } else if(gmt_offset2 == 0){
      
      if(length(data_range2) > 0) {Data2 <- Data_Range2.xts['T09:45:00/T16:54:59']}
      
    }
    
    Week1 = NULL
    if (length(Data1) > 0) { Week1 <- rbind(Week1, Data1)}
    
    Week2 = NULL
    if (length(Data2) > 0) { Week2 <- rbind(Week2, Data2)}
    
    Week = rbind(Week1, Week2)    
  }
  
  spread_data = Week[,1:6]
  names(spread_data) = c("bid.1", "ask.1", "bid.2", "ask.2", "bid.3", "ask.3")
  #ratios
  r1 <- 1  #LCC
  r2 <- -1 #CC
  r3 <- 0  #6B
  #multipliers
  m1 <- 1
  m2 <- -1
  m3 <- 1
  spread_data$spread.bid = m1 * as.numeric(spread_data$bid.1) * as.numeric(spread_data$bid.3) + m2 * as.numeric(spread_data$ask.2)
  spread_data$spread.ask = m1 * as.numeric(spread_data$ask.1) * as.numeric(spread_data$ask.3) + m2 * as.numeric(spread_data$bid.2)
  spread_data$spread.mid = (spread_data$spread.ask + spread_data$spread.bid) / 2
  spread_data$spread.bidAskSpread = spread_data$spread.ask - spread_data$spread.bid
  
  #adding spread specifications
  spread_name <- "Cocoa"
  spread_spec <- cbind(spread_name, r1, m1, r2, m2, r3, m3)
  
  #formatting data
  spread_zoo = zoo(spread_data)
  spread_digits <- format(spread_zoo[,7:10], digits=4)
  spread_fmt <- spread_zoo[,1:6]
  spread_cmb <- cbind(spread_fmt, spread_digits)
  
  sprd_filename <- paste(str_sprd_contract,"_",str_date,".csv",sep="")
  write.csv(spread_spec, sprd_filename)
  write.zoo(spread_cmb, sprd_filename, append = TRUE, sep = ",")
  
}

build_es_ym <- function(str_sprd_contract, str_legA_contract, str_legB_contract, str_date) {
  #loading in the outright legs
  legA_filename <- paste(str_legA_contract,"_",str_date,"_quotes.csv",sep="")
  legB_filename <- paste(str_legB_contract,"_",str_date,"_quotes.csv",sep="")
  df1 <- read.csv(legA_filename)
  df2 <- read.csv(legB_filename)
  
  database <- read.csv("C:/Users/hhong.TRADECO/Desktop/stdev/Daylight_database.csv")
  db_nyc <- subset(database, database$City == "New York")
  
  #subsetting the data
  df1_sub <- df1[,c(1,5,6)]
  df2_sub <- df2[,c(1,5,6)]
  
  #df1_sub <- df1[,c(2,6,7)]
  #df2_sub <- df2[,c(2,6,7)]
  
  
  #format the date column mm/dd/YYYY HH:MM:ss.fff
  options(digits.secs = 6)
  df1_sub.t <- as.POSIXct(df1_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df2_sub.t <- as.POSIXct(df2_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df1_sub.t <- df1_sub.t + 0.0001
  df2_sub.t <- df2_sub.t + 0.0001
  
  #create zoo object
  df1_zoo <- zoo(df1_sub[2:3], df1_sub.t)
  df2_zoo <- zoo(df2_sub[2:3], df2_sub.t)
  rm(df1_sub.t, df2_sub.t)
  
  #data dates
  startDate1 <- as.Date(as.POSIXct(first(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate1 <- as.Date(as.POSIXct(last(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate2 <- as.Date(as.POSIXct(first(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate2 <- as.Date(as.POSIXct(last(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate <- max(startDate1, startDate2) 
  endDate <- min(endDate1, endDate2)
  
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
  
  df1_list <- split(df1_zoo,as.Date(index(df1_zoo)))
  df2_list <- split(df2_zoo,as.Date(index(df2_zoo)))
  rm(df1_zoo, df2_zoo)
  
  currentDate = startDate 
  data = NULL
  while(currentDate <= endDate + 1){
    gmt_offset <- get_gmt_offset(currentDate, db_nyc)
    if(gmt_offset == -4) {
      setDate1 = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate1]])){
        df1_Date_1 = as.xts(df1_list[[setDate1]])['T00:00:00.000/T21:14:59.999'] 
      } else { df1_Date_1 = NULL }
      if(!is.null(df2_list[[setDate1]])){
        df2_Date_1 = as.xts(df2_list[[setDate1]])['T00:00:00.000/T21:14:59.999'] 
      } else { df2_Date_1 = NULL }
      
      setDate2 = strftime(currentDate-1, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate2]])){
        df1_Date_2 = as.xts(df1_list[[setDate2]])['T22:00:00.000/T23:59:59.999'] 
      } else { df1_Date_2 = NULL }
      if(!is.null(df2_list[[setDate2]])){
        df2_Date_2 = as.xts(df2_list[[setDate2]])['T22:00:00.000/T23:59:59.999'] 
      } else { df2_Date_2 = NULL }
    } 
    if(gmt_offset == -5) {
      setDate1 = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate1]])){
        df1_Date_1 = as.xts(df1_list[[setDate1]])['T00:00:00.000/T22:14:59.999'] 
      } else { df1_Date_1 = NULL }
      if(!is.null(df2_list[[setDate1]])){
        df2_Date_1 = as.xts(df2_list[[setDate1]])['T00:00:00.000/T22:14:59.999'] 
      } else { df2_Date_1 = NULL }
      
      setDate2 = strftime(currentDate-1, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate2]])){
        df1_Date_2 = as.xts(df1_list[[setDate2]])['T23:00:00.000/T23:59:59.999'] 
      } else { df1_Date_2 = NULL }
      if(!is.null(df2_list[[setDate2]])){
        df2_Date_2 = as.xts(df2_list[[setDate2]])['T23:00:00.000/T23:59:59.999'] 
      } else { df2_Date_2 = NULL }
    } 
    
    df1_Date = rbind(df1_Date_1, df1_Date_2)
    df2_Date = rbind(df2_Date_1, df2_Date_2)
    
    data_Date <- merge(df1_Date, df2_Date, all=TRUE)
    if(length(data_Date)!=0){
      data_Date <- na.locf(data_Date)
      data_Date <- na.omit(data_Date)
      if(length(data_Date) == 0){
        data = data
      } else {
        data = append(data, data_Date, after = length(data))
      }
    }
    currentDate = currentDate + 1
  }
  
  
  #adding in the day
  #data$day <- as.POSIXlt(index(data))$wday
  #data$day <- weekdays(as.Date(as.POSIXct(index(data))))
  
  
  
  # if they're different... find the date it switches on
  # make two sub data sets, one before the date and one after
  # then process those two sub data sets based on the gmt offset
  # using the code below as a guide
  
  
  # if the start and end gmt offsets are equal
  if(gmt_offset_start == gmt_offset_end) {
    
    Data.xts <- xts(data)
    
    if (gmt_offset_start == -5) {
      if (length(data) > 0) { Eve <- Data.xts['T23:00:00/T23:59:59']}
      if (length(data) > 0) { Morn <- Data.xts['T00:00:00/T22:14:59']}
      
    } else if (gmt_offset_start == -4) {
      if (length(data) > 0) { Eve <- Data.xts['T22:00:00/T23:59:59']}
      if (length(data) > 0) { Morn <- Data.xts['T00:00:00/T21:14:59']}
      
    }
    
    Week = NULL
    if (length(Eve) > 0) { Week <- rbind(Week, Eve)}
    if (length(Morn) > 0) { Week <- rbind(Week, Morn)}
    
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
      
      if(length(data_range1) > 0) {Eve1 <- Data_Range1.xts['T23:00:00/T23:59:59']}
      if(length(data_range1) > 0) {Morn1 <- Data_Range1.xts['T00:00:00/T22:14:59']}
      
    } else if(gmt_offset1 == -4){
      
      if(length(data_range1) > 0) {Eve1 <- Data_Range1.xts['T22:00:00/T23:59:59']}
      if(length(data_range1) > 0) {Morn1 <- Data_Range1.xts['T00:00:00/T21:14:59']}
      
    }
    
    if(gmt_offset2 == -5){
      
      if(length(data_range2) > 0) {Eve2 <- Data_Range2.xts['T23:00:00/T23:59:59']}
      if(length(data_range2) > 0) {Morn2 <- Data_Range2.xts['T00:00:00/T22:14:59']}
      
    } else if(gmt_offset2 == -4){
      
      if(length(data_range2) > 0) {Eve2 <- Data_Range2.xts['T22:00:00/T23:59:59']}
      if(length(data_range2) > 0) {Morn2 <- Data_Range2.xts['T00:00:00/T21:14:59']}
      
    }
    
    Week1 = NULL
    if (length(Eve1) > 0) { Week1 <- rbind(Week1, Eve1)}
    if (length(Morn1) > 0) { Week1 <- rbind(Week1, Morn1)}
    
    Week2 = NULL
    if (length(Eve2) > 0) { Week2 <- rbind(Week2, Eve2)}
    if (length(Morn2) > 0) { Week2 <- rbind(Week2, Morn2)}
    
    Week = rbind(Week1, Week2)    
  }
  
  spread_data = Week[,1:4]
  names(spread_data) = c("bid.1", "ask.1", "bid.2", "ask.2")
  rm(df1_list, df2_list)
  #ratios
  r1 <- 1
  r2 <- -1
  #multipliers
  m1 <- 0.01
  m2 <- -0.085
  spread_data$spread.bid = m1 * as.numeric(spread_data$bid.1) + m2 * as.numeric(spread_data$ask.2)
  spread_data$spread.ask = m1 * as.numeric(spread_data$ask.1) + m2 * as.numeric(spread_data$bid.2)
  spread_data$spread.mid = (spread_data$spread.ask + spread_data$spread.bid) / 2
  spread_data$spread.bidAskSpread = spread_data$spread.ask - spread_data$spread.bid
  
  #adding spread specifications
  spread_name <- "ES_YM_Ratio"
  spread_spec <- cbind(spread_name, r1, m1, r2, m2)
  
  #formatting data
  spread_zoo = zoo(spread_data)
  rm(spread_data)
  spread_digits <- format(spread_zoo[,5:8], digits=4)
  spread_fmt <- spread_zoo[,1:4]
  spread_cmb <- cbind(spread_fmt, spread_digits)
  rm(spread_fmt)
  
  sprd_filename <- paste(str_sprd_contract,"_",str_date,".csv",sep="")
  write.csv(spread_spec, sprd_filename)
  write.zoo(spread_cmb, sprd_filename, append = TRUE, sep = ",")
}

build_es_nq <- function(str_sprd_contract, str_legA_contract, str_legB_contract, str_date) {
  #loading in the outright legs
  legA_filename <- paste(str_legA_contract,"_",str_date,"_quotes.csv",sep="")
  legB_filename <- paste(str_legB_contract,"_",str_date,"_quotes.csv",sep="")
  df1 <- read.csv(legA_filename)
  df2 <- read.csv(legB_filename)
  
  database <- read.csv("C:/Users/hhong.TRADECO/Desktop/stdev/Daylight_database.csv")
  db_nyc <- subset(database, database$City == "New York")
  
  #subsetting the data
  df1_sub <- df1[,c(1,5,6)]
  df2_sub <- df2[,c(1,5,6)]
  
  #df1_sub <- df1[,c(2,6,7)]
  #df2_sub <- df2[,c(2,6,7)]
  
  
  #format the date column mm/dd/YYYY HH:MM:ss.fff
  options(digits.secs = 6)
  df1_sub.t <- as.POSIXct(df1_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df2_sub.t <- as.POSIXct(df2_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df1_sub.t <- df1_sub.t + 0.0001
  df2_sub.t <- df2_sub.t + 0.0001
  
  #create zoo object
  df1_zoo <- zoo(df1_sub[2:3], df1_sub.t)
  df2_zoo <- zoo(df2_sub[2:3], df2_sub.t)
  rm(df1_sub.t, df2_sub.t)
  
  #data dates
  startDate1 <- as.Date(as.POSIXct(first(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate1 <- as.Date(as.POSIXct(last(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate2 <- as.Date(as.POSIXct(first(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate2 <- as.Date(as.POSIXct(last(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate <- max(startDate1, startDate2) 
  endDate <- min(endDate1, endDate2)
  
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
  
  df1_list <- split(df1_zoo,as.Date(index(df1_zoo)))
  df2_list <- split(df2_zoo,as.Date(index(df2_zoo)))
  rm(df1_zoo, df2_zoo)
  
  currentDate = startDate 
  data = NULL
  while(currentDate <= endDate + 1){
    gmt_offset <- get_gmt_offset(currentDate, db_nyc)
    if(gmt_offset == -4) {
      setDate1 = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate1]])){
        df1_Date_1 = as.xts(df1_list[[setDate1]])['T00:00:00.000/T21:14:59.999'] 
      } else { df1_Date_1 = NULL }
      if(!is.null(df2_list[[setDate1]])){
        df2_Date_1 = as.xts(df2_list[[setDate1]])['T00:00:00.000/T21:14:59.999'] 
      } else { df2_Date_1 = NULL }
      
      setDate2 = strftime(currentDate-1, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate2]])){
        df1_Date_2 = as.xts(df1_list[[setDate2]])['T22:00:00.000/T23:59:59.999'] 
      } else { df1_Date_2 = NULL }
      if(!is.null(df2_list[[setDate2]])){
        df2_Date_2 = as.xts(df2_list[[setDate2]])['T22:00:00.000/T23:59:59.999'] 
      } else { df2_Date_2 = NULL }
    } 
    if(gmt_offset == -5) {
      setDate1 = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate1]])){
        df1_Date_1 = as.xts(df1_list[[setDate1]])['T00:00:00.000/T22:14:59.999'] 
      } else { df1_Date_1 = NULL }
      if(!is.null(df2_list[[setDate1]])){
        df2_Date_1 = as.xts(df2_list[[setDate1]])['T00:00:00.000/T22:14:59.999'] 
      } else { df2_Date_1 = NULL }
      
      setDate2 = strftime(currentDate-1, format = "%Y-%m-%d", tz = 'GMT')
      if(!is.null(df1_list[[setDate2]])){
        df1_Date_2 = as.xts(df1_list[[setDate2]])['T23:00:00.000/T23:59:59.999'] 
      } else { df1_Date_2 = NULL }
      if(!is.null(df2_list[[setDate2]])){
        df2_Date_2 = as.xts(df2_list[[setDate2]])['T23:00:00.000/T23:59:59.999'] 
      } else { df2_Date_2 = NULL }
    } 
    
    df1_Date = rbind(df1_Date_1, df1_Date_2)
    df2_Date = rbind(df2_Date_1, df2_Date_2)
    
    data_Date <- merge(df1_Date, df2_Date, all=TRUE)
    if(length(data_Date)!=0){
      data_Date <- na.locf(data_Date)
      data_Date <- na.omit(data_Date)
      if(length(data_Date) == 0){
        data = data
      } else {
        data = append(data, data_Date, after = length(data))
      }
    }
    currentDate = currentDate + 1
  }
  
  
  #adding in the day
  #data$day <- as.POSIXlt(index(data))$wday
  #data$day <- weekdays(as.Date(as.POSIXct(index(data))))
  
  
  
  # if they're different... find the date it switches on
  # make two sub data sets, one before the date and one after
  # then process those two sub data sets based on the gmt offset
  # using the code below as a guide
  
  
  # if the start and end gmt offsets are equal
  if(gmt_offset_start == gmt_offset_end) {
    
    Data.xts <- xts(data)
    
    if (gmt_offset_start == -5) {
      if (length(data) > 0) { Eve <- Data.xts['T23:00:00/T23:59:59']}
      if (length(data) > 0) { Morn <- Data.xts['T00:00:00/T22:14:59']}
      
    } else if (gmt_offset_start == -4) {
      if (length(data) > 0) { Eve <- Data.xts['T22:00:00/T23:59:59']}
      if (length(data) > 0) { Morn <- Data.xts['T00:00:00/T21:14:59']}
      
    }
    
    Week = NULL
    if (length(Eve) > 0) { Week <- rbind(Week, Eve)}
    if (length(Morn) > 0) { Week <- rbind(Week, Morn)}
    
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
      
      if(length(data_range1) > 0) {Eve1 <- Data_Range1.xts['T23:00:00/T23:59:59']}
      if(length(data_range1) > 0) {Morn1 <- Data_Range1.xts['T00:00:00/T22:14:59']}
      
    } else if(gmt_offset1 == -4){
      
      if(length(data_range1) > 0) {Eve1 <- Data_Range1.xts['T22:00:00/T23:59:59']}
      if(length(data_range1) > 0) {Morn1 <- Data_Range1.xts['T00:00:00/T21:14:59']}
      
    }
    
    if(gmt_offset2 == -5){
      
      if(length(data_range2) > 0) {Eve2 <- Data_Range2.xts['T23:00:00/T23:59:59']}
      if(length(data_range2) > 0) {Morn2 <- Data_Range2.xts['T00:00:00/T22:14:59']}
      
    } else if(gmt_offset2 == -4){
      
      if(length(data_range2) > 0) {Eve2 <- Data_Range2.xts['T22:00:00/T23:59:59']}
      if(length(data_range2) > 0) {Morn2 <- Data_Range2.xts['T00:00:00/T21:14:59']}
      
    }
    
    Week1 = NULL
    if (length(Eve1) > 0) { Week1 <- rbind(Week1, Eve1)}
    if (length(Morn1) > 0) { Week1 <- rbind(Week1, Morn1)}
    
    Week2 = NULL
    if (length(Eve2) > 0) { Week2 <- rbind(Week2, Eve2)}
    if (length(Morn2) > 0) { Week2 <- rbind(Week2, Morn2)}
    
    Week = rbind(Week1, Week2)    
  }
  
  spread_data = Week[,1:4]
  names(spread_data) = c("bid.1", "ask.1", "bid.2", "ask.2")
  rm(df1_list, df2_list)
  #ratios
  r1 <- 1
  r2 <- -1
  #multipliers
  m1 <- 2.1
  m2 <- -1
  spread_data$spread.bid = m1 * as.numeric(spread_data$bid.1) + m2 * as.numeric(spread_data$ask.2)
  spread_data$spread.ask = m1 * as.numeric(spread_data$ask.1) + m2 * as.numeric(spread_data$bid.2)
  spread_data$spread.mid = (spread_data$spread.ask + spread_data$spread.bid) / 2
  spread_data$spread.bidAskSpread = spread_data$spread.ask - spread_data$spread.bid
  
  #adding spread specifications
  spread_name <- "ES_YM_Ratio"
  spread_spec <- cbind(spread_name, r1, m1, r2, m2)
  
  #formatting data
  spread_zoo = zoo(spread_data)
  rm(spread_data)
  spread_digits <- format(spread_zoo[,5:8], digits=4)
  spread_fmt <- spread_zoo[,1:4]
  spread_cmb <- cbind(spread_fmt, spread_digits)
  rm(spread_fmt)
  
  sprd_filename <- paste(str_sprd_contract,"_",str_date,".csv",sep="")
  write.csv(spread_spec, sprd_filename)
  write.zoo(spread_cmb, sprd_filename, append = TRUE, sep = ",")
}

build_cows <- function(str_sprd_contract, str_legA_contract, str_legB_contract, str_date) {
  #loading in the outright legs
  legA_filename <- paste(str_legA_contract,"_",str_date,"_quotes.csv",sep="")
  legB_filename <- paste(str_legB_contract,"_",str_date,"_quotes.csv",sep="")
  df1 <- read.csv(legA_filename)
  df2 <- read.csv(legB_filename)
  database <- read.csv("C:/Users/hhong.TRADECO/Desktop/stdev/Daylight_database.csv")
  db_chi <- subset(database, database$City == "Chicago")
  
  df1_sub <- df1[,c(1,5,6)]
  df2_sub <- df2[,c(1,5,6)]
  
  #df1_sub <- df1[,c(2,6,7)]
  #df2_sub <- df2[,c(2,6,7)]
  
  #format the date column mm/dd/YYYY HH:MM:ss.fff
  options(digits.secs = 3)
  df1_sub.t <- as.POSIXct(df1_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df2_sub.t <- as.POSIXct(df2_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  df1_sub.t <- df1_sub.t + 0.0001
  df2_sub.t <- df2_sub.t + 0.0001
  
  
  #create zoo object
  df1_zoo <- zoo(df1_sub[2:3], df1_sub.t)
  df2_zoo <- zoo(df2_sub[2:3], df2_sub.t)
  
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
  
  #data dates
  startDate1 <- as.Date(as.POSIXct(first(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate1 <- as.Date(as.POSIXct(last(index(df1_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate2 <- as.Date(as.POSIXct(first(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  endDate2 <- as.Date(as.POSIXct(last(index(df2_zoo)), format = "%Y-%m-%d", tz = "GMT"))
  startDate <- min(startDate1, startDate2)
  endDate <- max(endDate1, endDate2)
  
  # check the GMT offset of the start and end date in 'data'
  gmt_offset_start <- get_gmt_offset(startDate, db_chi)
  cat('Start Date: ')
  print(startDate)
  cat('Start Date GMT Offset: ')
  print(gmt_offset_start)
  
  gmt_offset_end <- get_gmt_offset(endDate, db_chi)
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
  df1_list <- split(df1_zoo,as.Date(index(df1_zoo)))
  df2_list <- split(df2_zoo,as.Date(index(df2_zoo)))
  
  currentDate = startDate 
  data = NULL
  while(currentDate <= endDate ){
    setDate = strftime(currentDate, format = "%Y-%m-%d", tz = 'GMT')
    df1_Date = df1_list[[setDate]]
    df2_Date = df2_list[[setDate]]
    if(!is.null(df1_Date) && !is.null(df2_Date)){
      data_Date <- zoo(merge(df1_Date, df2_Date, all=TRUE))
    } else { data_Date = NULL }
    if(length(data_Date)!=0){
      data_Date <- na.locf(data_Date)
      data_Date <- na.omit(data_Date)
      if(length(data_Date) == 0){
        data = data
      } else {
        data = append(data, data_Date, after = length(data))
      }
    }
    currentDate = currentDate + 1
  }
  
  
  #adding in the day
  #data$day <- as.POSIXlt(index(data))$wday
  #data$day <- weekdays(as.Date(as.POSIXct(index(data))))
  
  
  
  # if they're different... find the date it switches on
  # make two sub data sets, one before the date and one after
  # then process those two sub data sets based on the gmt offset
  # using the code below as a guide
  
  
  # if the start and end gmt offsets are equal
  if(gmt_offset_start == gmt_offset_end) {
    
    Data.xts <- xts(data)
    
    if (gmt_offset_start == -5) {
      if (length(data) > 0) { Data <- Data.xts['T13:30:00/T18:04:59']}
      
    } else if (gmt_offset_start == -6) {
      if (length(data) > 0) { Data <- Data.xts['T14:30:00/T19:04:59']}
      
    }
    
    Week = NULL
    if (length(Data) > 0) { Week <- rbind(Week, Data)}
    
  } else {
    dst_change_date <- daylight_savings_change_date(startDate, endDate, db_chi)
    data_range1 <- subset(data, as.Date(index(data)) < dst_change_date)
    data_range2 <- subset(data, as.Date(index(data)) >= dst_change_date)
    
    Data_Range1.xts <- xts(data_range1)
    Data_Range2.xts <- xts(data_range2)
    
    
    startDate_range1 <- as.Date(as.POSIXct(first(index(data_range1)), format = "%Y-%m-%d", tz = "GMT"))
    #endDate_range1 <- as.Date(as.POSIXct(last(index(date_range1)), format = "%Y-%m-%d", tz = "GMT"))
    startDate_range2 <- as.Date(as.POSIXct(first(index(data_range2)), format = "%Y-%m-%d", tz = "GMT"))
    #endDate_range2 <- as.Date(as.POSIXct(last(index(date_range2)), format = "%Y-%m-%d", tz = "GMT"))
    gmt_offset1 <- get_gmt_offset(startDate_range1, db_chi)
    gmt_offset1
    gmt_offset2 <- get_gmt_offset(startDate_range2, db_chi)
    gmt_offset2
    
    if (gmt_offset1 == -5){
      
      if(length(data_range1) > 0) {Data1 <- Data_Range1.xts['T13:30:00/T18:04:59']}
      
    } else if(gmt_offset1 == -6){
      
      if(length(data_range1) > 0) {Data1 <- Data_Range1.xts['T14:30:00/T19:04:59']}
      
    }
    
    if(gmt_offset2 == -5){
      
      if(length(data_range2) > 0) {Data2 <- Data_Range2.xts['T13:30:00/T18:04:59']}
      
    } else if(gmt_offset2 == -6){
      
      if(length(data_range2) > 0) {Data2 <- Data_Range2.xts['T14:30:00/T19:04:59']}
      
    }
    
    Week1 = NULL
    if (length(Data1) > 0) { Week1 <- rbind(Week1, Data1)}
    
    Week2 = NULL
    if (length(Data2) > 0) { Week2 <- rbind(Week2, Data2)}
    
    Week = rbind(Week1, Week2)    
  }
  
  spread_data = Week[,1:4]
  names(spread_data) = c("bid.1", "ask.1", "bid.2", "ask.2")
  #ratios
  r1 <- 1
  r2 <- -2
  #multipliers
  m1 <- 5
  m2 <- -8
  spread_data$spread.bid = m1 * as.numeric(spread_data$bid.1) + m2 * as.numeric(spread_data$ask.2)
  spread_data$spread.ask = m1 * as.numeric(spread_data$ask.1) + m2 * as.numeric(spread_data$bid.2)
  spread_data$spread.mid = (spread_data$spread.ask + spread_data$spread.bid) / 2
  spread_data$spread.bidAskSpread = spread_data$spread.ask - spread_data$spread.bid
  
  #adding spread specifications
  spread_name <- "Feeder_Live_Cattle"
  spread_spec <- cbind(spread_name, r1, m1, r2, m2)
  
  #formatting data
  spread_zoo = zoo(spread_data)
  spread_digits <- format(spread_zoo[,5:8], digits=4)
  spread_fmt <- spread_zoo[,1:4]
  spread_cmb <- cbind(spread_fmt, spread_digits)
  
  sprd_filename <- paste(str_sprd_contract,"_",str_date,".csv",sep="")
  write.csv(spread_spec, sprd_filename)
  write.zoo(spread_cmb, sprd_filename, append = TRUE, sep = ",")
}

