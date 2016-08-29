rm(list = ls())
setwd("C:/Users/hhong.TRADECO/Desktop/stdev/Processed")
library(zoo)
library(xts)
library(graphics)

source("C:/Users/hhong.TRADECO/Desktop/stdev/Spread_Creation_Functions.R")

#making sure the environment is in GMT
env.tz <- "GMT"
Sys.setenv(TZ=env.tz)

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

#rms stdev but not rolling data USE THIS
rolling_std <- function(data, t) {
  data_time <- regular_bars(data, t*60)
  change = diff(data_time, lag = 1)
  RMS = function(change) sqrt((sum(change^2))/(length(change) - 1))
  std = RMS(na.omit(change))
  #print(paste("std:(",as.Date(last(index(data))),") ",std,sep=""))
  return (as.numeric(std))
}

run_outright <- function(str_contract, str_date, std_period) {
  #outright
  filename <- paste(str_contract,"_",str_date,"_quotes.csv",sep="")
  #print(filename)
  df <- read.csv(filename)
  #forHO
  #df <- df[-(1:2),]
  df_sub = df[, c(1, 5, 6)]
  df_sub$mid = (df_sub$bid + df_sub$ask)/2
  options(digits.secs = 3)
  df_t <- as.POSIXct(df_sub[,1], format = "%m/%d/%Y %H:%M:%OS")
  all_data = zoo(df_sub[2: 4], df_t)$mid
  reg_data <- regular_bars(all_data, 1)
  stdev <- rolling_std(reg_data, std_period)
  print(paste("STDEV for",str_contract,"is",stdev))
}

run_spread <- function(str_contract, str_date, std_period, num_legs) {
  #spread
  filename <- paste(str_contract,"_",str_date,".csv",sep="")
  df <- read.csv(filename, skip = 2, header = TRUE, row.names=NULL)
  if (num_legs == 2) {
    #two legged spread
    df_sub = df[, c(1, 6, 7, 8, 9)]
  }
  else {
    #three legged spread
    df_sub = df[, c(1, 8, 9, 10, 11)]
  }
  options(digits.secs = 3)
  df_t <- as.POSIXct(df_sub[,1], format = "%Y-%m-%d %H:%M:%OS")
  all_data = zoo(df_sub[2: 5], df_t)$spread.mid
  reg_data <- regular_bars(all_data, 1)
  stdev <- rolling_std(reg_data, std_period)
  print(paste("STDEV for",str_contract,"is",stdev))
}

str_date <- "Aug17"

cme_hg <- "HGU6"
lme_ca <- "CAU6"
cme_gc <- "GCZ6"
cme_si <- "SIU6"
cme_pl <- "PLV6"
cme_pa <- "PAU6"
cme_ho1 <- "HOU6"
cme_ho2 <- "HOV6"
ius_go1 <- "GOU6"
ius_go2 <- "GOV6"
cme_cl <- "CLv6"
cme_rb <- "RBV6"
ieu_brent <- "LCOV6"
ius_lsu <- "LSUV6"
ius_sb <- "SBV6"
ius_lcc <- "LCCZ6"
ius_cc <- "CCZ6"
cme_bp <- "BPU6"
cme_es <- "ESU6"
cme_ym <- "YMU6"
cme_nq <- "NQU6"
cme_gf <- "GFV6"
cme_le <- "LEV6"

ca_hg <- "CAHG_U6"
gc_si <- "GCSI_Z6U6"
pl_gc <- "PLGC_V6Z6"
pl_pa <- "PLPA_V6U6"
ho_go1 <- "HOGO_U6"
ho_go2 <- "HOGO_V6"
ho_cl <- "HOCL_V6"
rb_cl <- "RBCL_V6"
ti_brent <- "TIBR_U6"
sugar <- "Sugar_V6"
cocoa <- "Cocoa_Z6"
es_ym <- "ESYM_U6"
es_nq <- "ESNQ_U6"
cows <- "Cows_V6"

build_ca_hg(ca_hg, lme_ca, cme_hg, str_date)
build_gc_si(gc_si, cme_gc, cme_si, str_date)
build_pl_gc(pl_gc, cme_pl, cme_gc, str_date)
build_pl_pa(pl_pa, cme_pl, cme_pa, str_date)
build_ho_go(ho_go1, cme_ho1, ius_go1, str_date)
build_ho_go(ho_go2, cme_ho2, ius_go2, str_date)
build_ho_cl(ho_cl, cme_ho2, cme_cl, str_date)
build_ho_cl(rb_cl, cme_rb, cme_cl, str_date)
build_ti_brent(ti_brent, cme_cl, ieu_brent, str_date)
build_sugar(sugar, ius_lsu, ius_sb, str_date)
build_cocoa(cocoa, ius_lcc, ius_cc, cme_bp, str_date)
build_es_ym(es_ym, cme_ym, cme_es, str_date)
build_es_nq(es_nq, cme_es, cme_nq, str_date)
build_cows(cows, cme_gf, cme_le, str_date)

run_outright(cme_hg, str_date, 30)
run_outright(lme_ca, str_date, 30)
run_outright(cme_gc, str_date, 8)
run_outright(cme_si, str_date, 8)
run_outright(cme_pl, str_date, 8)
run_outright(cme_pa, str_date, 8)
run_outright(cme_ho1, str_date, 8)
run_outright(cme_ho2, str_date, 8)
run_outright(ius_go1, str_date, 8)
run_outright(ius_go2, str_date, 8)
run_outright(cme_cl1, str_date, 8)
run_outright(cme_cl2, str_date, 8)
run_outright(cme_rb, str_date, 8)
run_outright(ieu_brent, str_date, 8)
run_outright(ius_lsu, str_date, 8)
run_outright(ius_sb, str_date, 8)
run_outright(ius_lcc, str_date, 8)
run_outright(ius_cc, str_date, 8)
run_outright(cme_bp, str_date, 8)
run_outright(cme_es, str_date, 8)
run_outright(cme_ym, str_date, 8)
run_outright(cme_nq, str_date, 8)
run_outright(cme_gf, str_date, 8)
run_outright(cme_le, str_date, 8)

run_spread(ca_hg, str_date, 30, 2)
run_spread(gc_si, str_date, 8, 2)
run_spread(pl_gc, str_date, 8, 2)
run_spread(pl_pa, str_date, 8, 2)
run_spread(ho_go1, str_date, 8, 2)
run_spread(ho_go2, str_date, 8, 2)
run_spread(ho_cl, str_date, 8, 2)
run_spread(rb_cl, str_date, 8, 2)
run_spread(ti_brent, str_date, 8, 2)
run_spread(sugar, str_date, 8, 2)
run_spread(cocoa, str_date, 8, 3)
run_spread(es_nq, str_date, 8, 2)
run_spread(es_ym, str_date, 8, 2)
run_spread(cows, str_date, 8, 2)
