
#******** CHECK THESE VALUES *****************************
DataDate <- "Aug17"
startingDir <- "C:/Users/hhong.TRADECO/Desktop/stdev/ToProcess/"
#*********************************************************

filez <- list.files(startingDir,pattern="jason.shaffer@trading*")

tmp <- sapply(filez,FUN=function(eachPath){  
  contr <- unlist(strsplit(eachPath, "-"))[2] 
  new_path <- paste(startingDir,"/",contr,"_",DataDate,".csv.gz",sep="")
  old_path <- paste(startingDir,"/",eachPath,sep="")
  file.rename(from=old_path,to=new_path)
})





