#####################################################################
# 
# R Script to  pre-process Automatic Identification System (AIS) data to produce statistics with passage lines
# Developped by Florent NICOLAS, HELCOM Secretariat - 12/11/18 - florent.nicolas@helcom.fi
# R Version 3.4.3
#
# Input data: monthly AIS data files already harmonized (pre-processed) using the script 1 on the HELCOM Secretariat GitHub repository (https://github.com/helcomsecretariat/AIS-data-processing-for-statistics-and-maps) 
# Required data: 
   #- a shape file with the line as polygons. An example is available on this repository (AIS_predefined_lines.zip).
   #- a csv file with informations on the ship types of the ships to have more precised statistics. An example of the data is also available in this repository. 
# Ouput data: a csv file with the number of passages per line / per year and per ship type. A sample of the data is also available in this repository.
#
# The script was developped to produce statistics for the Report of Shipping Accidents in the Baltic Sea from 2014 to 2017 by the HELCOM Secretariat 
   #--> http://www.helcom.fi/Lists/Publications/Ship-accidents-2014-2017-report.pdf)
#
# !! The current script is processing the year 2017 To produce the same data for the year XXXX, replace XXXX by XXXX (CTRL + A and CTRL + F)
#
#####################################################################
rm(list = ls()) #clear environment.
rm(list = ls())

# packages needed
library("sp")
library("rgdal")
library("maps")
library("rworldmap")
library("RgoogleMaps")
library("plyr")
library("dplyr")
library("tidyr")
library("data.table")
library("lubridate")


# import data

setwd("E:/test_division/division_finale2017/file_list_months_weeks") # path were the monthly files are stored
fileList <- list.files(pattern="ais_.*\\.csv", recursive=FALSE)

for (file in fileList) {
  data_2017<-read.table(file, header=T, fill=T, sep=",",na.strings=c(""," ", "null", "NA"))   
  
  # test with firsts 100000 rows of may 2017
  #data_2017  <- read.table("E:/test_division/division_finale2017/file_list_months_weeks/ais_may_2017.csv",header=T, fill=T, sep=",",na.strings=c(""," ", "null", "NA"), nrows= 100000)
  
  # select relevant parameters
  data_2017<- data_2017[,c("timestamp_pretty","timestamp","mmsi", "lat","long", "sog", "cog",  "imo")]

  # get the month of the monthly file that is processing  
  Sys.setlocale("LC_TIME", "C")
  date <- strptime(data_2017$timestamp_pretty, "%d/%m/%Y %H:%M:%S")
  month <- unique(months(as.Date(date)))
  
  # removing wrong mmsi (111111111) and wrong imo numbers if in data
  data_2017 <- subset(data_2017,data_2017$mmsi != "111111111")
  #for too big IMO numbers:
  data_2017$imo <- as.numeric(as.character(data_2017$imo))
  data_2017$imo[data_2017$imo < 999999] <- NA
  #for too small IMO numbers:
  data_2017$imo[data_2017$imo >9999999] <- NA
  
  
  # only IMO ships
  data_2017<-subset(data_2017, (!is.na(imo)))
  data_2017<- data_2017[,c("timestamp_pretty","imo", "sog", "cog", "lat","long")] 
  data_2017$imo<- as.numeric(data_2017$imo)
  
  #sort data by ship and time
  data_2017 <- data_2017[order(data_2017$imo, data_2017$timestamp_pretty),] 
  
  data <- data_2017
  
  ######################### ######################### #########################   A. Definition of the signals inside the polygons 
  ######################### ######################### #########################
  
  
  # 1. definition of signals in the polygnes
  coordinates(data) <- c("long", "lat")
  
  # 2. import polygons from shp file. 
  polygons <-readOGR("W:/shipping/AIS_crossing_lines","AIS_predefined_lines")
  #Polygons are used because it is possible to have the exact time when the ships are crossing it and it is easier for the computer to process
  #the polygons have a rather small width, only few kilometers.
  
  # 3 . to confirm the same reference system
  proj4string(data) <- proj4string(polygons)
  
  
  # 4. overlap AIS data and polygons  
  into_polygons <- !is.na(over(data, as(polygons, "SpatialPolygons")))
  mean(into_polygons)
  
  
  #build data frame
  data2<-as.data.frame(data)
  data2$into_polygons <-into_polygons *1
  
  # get the name of the polygons
  
  Name_polygon <- over(data, polygons[,"Name"])
  data2$Name_polygon <- NA
  data2$Name_polygon <- Name_polygon$Name
  summary(data2$Name_polygon)
  
  #plot to check
  #t <- subset(data2, (data2$into_polygons==1))
  #newmap <- getMap(resolution = "low")
  #plot(newmap, xlim = c(5.39, 30), ylim = c(52, 65.5), asp = 1, col="grey")
  #points(t$long, t$lat, col = "red", cex = 1)
  
  # create object opposite to inside polygons
  data2$opp_into_polygons <- ifelse(data2$into_polygons== 1,0,1)
  
  # count the passage in the polygons
  data <- data2
  data$Passage <- setDT(data)[, v1 := if(all(!opp_into_polygons)) c(TRUE, imo[-1]!= imo[-.N]) 
                              else rep(FALSE, .N), .(grp = rleid(opp_into_polygons))][,cumsum(v1)*(!opp_into_polygons)]
  
  # information about the passage
  data_passage <- filter(data, Passage != 0) %>% # drop rows with Stop == 0 
    unite(dates, timestamp_pretty, sep = " ") %>% #create date object
    mutate(dates = as.POSIXct(strptime(dates, format = "%d/%m/%Y %H:%M:%S"))) %>%
    group_by(Passage) %>% # for each stop
    filter(dates == min(dates) | dates == max(dates)) %>% #select rows with min and max dates
    dplyr::summarise(minTime = min(dates),
                     maxTime = max(dates),
                     duration_minutes = round(difftime(max(dates), min(dates), units="mins")),
                     mean_COG= mean(cog),
                     location=Name_polygon[1],
                     Ship = imo[1])
  
  # import shiptype information (bought from commercial provider). This helps to have more accurate information on the shiptypes than the AIS data offer. 
  shiplist <- read.csv("E:/ship_list/shiplist_2017_final.csv", sep=";")
  shiptype <- shiplist[,c("imo","HELCOM_Gross_ShipType", "HELCOM_Detail_ShipType")] 
  colnames(shiptype)[1] <- "Ship"
  
  data_passage <- merge(data_passage,shiptype, by= "Ship" ) 
  
  # subset each gross shiptype, produce summary and save temp file
  dir.create(file.path("E:/AIS_passage_lines"), showWarnings=F)
  dir.create(file.path("E:/AIS_passage_lines/temp_monthly"), showWarnings=F)
  
  # Cargo
  shiptype <- "Cargo"
  df <- subset(data_passage, data_passage$HELCOM_Gross_ShipType == shiptype)
  df <- table(df$location)
  df <- as.data.frame(df)
  colnames(df)<- c("Location","N_passage")
  directory=paste("E:/AIS_passage_lines/temp_monthly/2017_", shiptype, "_", month, sep="", ".csv" )
  write.table(df, directory, sep=";", col.names = T, row.names=F)
  
  # Tanker
  shiptype <- "Tanker"
  df <- subset(data_passage, data_passage$HELCOM_Gross_ShipType == shiptype)
  df <- table(df$location)
  df <- as.data.frame(df)
  colnames(df)<- c("Location","N_passage")
  directory=paste("E:/AIS_passage_lines/temp_monthly/2017_", shiptype, "_", month, sep="", ".csv" )
  write.table(df, directory, sep=";", col.names = T, row.names=F)
  
  # Passenger
  shiptype <- "Passenger"
  df <- subset(data_passage, data_passage$HELCOM_Gross_ShipType == shiptype)
  df <- table(df$location)
  df <- as.data.frame(df)
  colnames(df)<- c("Location","N_passage")
  directory=paste("E:/AIS_passage_lines/temp_monthly/2017_", shiptype, "_", month, sep="", ".csv" )
  write.table(df, directory, sep=";", col.names = T, row.names=F)
  
  #Other
  df<-subset(data_passage, data_passage$HELCOM_Gross_ShipType == "Container" |
               data_passage$HELCOM_Gross_ShipType == "Other" |
               data_passage$HELCOM_Gross_ShipType == "Fishing" |
               data_passage$HELCOM_Gross_ShipType == "Service" |
               data_passage$HELCOM_Gross_ShipType == "Rorocargo" |
               data_passage$HELCOM_Gross_ShipType == "Unknown" )
  
  df <- table(df$location)
  df <- as.data.frame(df)
  colnames(df)<- c("Location","N_passage")
  directory=paste("E:/AIS_passage_lines/temp_monthly/2017_", "Other", "_", month, sep="", ".csv" )
  write.table(df, directory, sep=";", col.names = T, row.names=F)
  
}


rm(list = ls())
rm(list = ls())
#
# merge file for each shiptype:
setwd("E:/AIS_passage_lines/temp_monthly/")

# Cargo
shiptype <- "Cargo"
pattern <- paste("2017_",shiptype,"_.*\\.csv",sep="" )
filelist <- list.files(pattern=pattern, recursive=FALSE)  
# merging
filelist<- ldply(filelist, read.table, header=T, sep = ";", fill=T)
filelist <- ddply(filelist, c("Location"), summarise,
                  N_passage    = sum(N_passage))
column.names <- paste("N_passage_",shiptype, sep="" )
colnames(filelist)[2] <- column.names
directory=paste("E:/AIS_passage_lines/2017_", shiptype,".csv", sep="" )
write.table(filelist, directory, sep=";", col.names = T, row.names=F)

# Tanker
shiptype <- "Tanker"
pattern <- paste("2017_",shiptype,"_.*\\.csv",sep="" )
filelist <- list.files(pattern=pattern, recursive=FALSE)  
# merging
filelist<- ldply(filelist, read.table, header=T, sep = ";", fill=T)
filelist <- ddply(filelist, c("Location"), summarise,
                  N_passage    = sum(N_passage))
column.names <- paste("N_passage_",shiptype, sep="" )
colnames(filelist)[2] <- column.names
directory=paste("E:/AIS_passage_lines/2017_", shiptype,".csv", sep="" )
write.table(filelist, directory, sep=";", col.names = T, row.names=F)

# Passenger
shiptype <- "Passenger"
pattern <- paste("2017_",shiptype,"_.*\\.csv",sep="" )
filelist <- list.files(pattern=pattern, recursive=FALSE)  
# merging
filelist<- ldply(filelist, read.table, header=T, sep = ";", fill=T)
filelist <- ddply(filelist, c("Location"), summarise,
                  N_passage    = sum(N_passage))
column.names <- paste("N_passage_",shiptype, sep="" )
colnames(filelist)[2] <- column.names
directory=paste("E:/AIS_passage_lines/2017_", shiptype,".csv", sep="" )
write.table(filelist, directory, sep=";", col.names = T, row.names=F)

# Other
shiptype <- "Other"
pattern <- paste("2017_",shiptype,"_.*\\.csv",sep="" )
filelist <- list.files(pattern=pattern, recursive=FALSE)  
# merging
filelist<- ldply(filelist, read.table, header=T, sep = ";", fill=T)
filelist <- ddply(filelist, c("Location"), summarise,
                  N_passage    = sum(N_passage))
column.names <- paste("N_passage_",shiptype, sep="" )
colnames(filelist)[2] <- column.names
directory=paste("E:/AIS_passage_lines/2017_", shiptype,".csv", sep="" )
write.table(filelist, directory, sep=";", col.names = T, row.names=F)




# create one table for each year
rm(list = ls())
rm(list = ls())
setwd("E:/AIS_passage_lines/")
#
cargo <- read.csv("E:/AIS_passage_lines/2017_Cargo.csv", sep=";")
tanker <- read.csv("E:/AIS_passage_lines/2017_Tanker.csv", sep=";")
passenger <- read.csv("E:/AIS_passage_lines/2017_Passenger.csv", sep=";")
other <- read.csv("E:/AIS_passage_lines/2017_other.csv", sep=";")

df1 <- merge(cargo,passenger)
df2 <- merge(tanker,other)
total <- merge(df1,df2)
colnames(total)[which(names(total) == "N_passage_Cargo")] <- "N_passage_Cargo_2017"
colnames(total)[which(names(total) == "N_passage_Passenger")] <- "N_passage_Passenger_2017"
colnames(total)[which(names(total) == "N_passage_Tanker")] <- "N_passage_Tanker_2017"
colnames(total)[which(names(total) == "N_passage_Other")] <- "N_passage_Other_2017"


write.table(total, "E:/AIS_passage_lines/2017_all_passages.csv", sep=";", col.names = T, row.names=F)


#merge all of the yearly files
data0 <-read.csv("E:/AIS_passage_lines/2006_all_passages.csv", fill=T, sep=";") 
data1 <-read.csv("E:/AIS_passage_lines/2007_all_passages.csv", fill=T, sep=";") 
data2 <-read.csv("E:/AIS_passage_lines/2008_all_passages.csv", fill=T, sep=";") 
data3 <-read.csv("E:/AIS_passage_lines/2009_all_passages.csv", fill=T, sep=";") 
data4 <-read.csv("E:/AIS_passage_lines/2010_all_passages.csv", fill=T, sep=";") 
data5 <-read.csv("E:/AIS_passage_lines/2011_all_passages.csv", fill=T, sep=";") 
data6 <-read.csv("E:/AIS_passage_lines/2012_all_passages.csv", fill=T, sep=";") 
data7 <-read.csv("E:/AIS_passage_lines/2013_all_passages.csv", fill=T, sep=";") 
data8 <-read.csv("E:/AIS_passage_lines/2014_all_passages.csv", fill=T, sep=";") 
data9 <-read.csv("E:/AIS_passage_lines/2015_all_passages.csv", fill=T, sep=";") 
data10 <-read.csv("E:/AIS_passage_lines/2016_all_passages.csv", fill=T, sep=";") 
data11 <-read.csv("E:/AIS_passage_lines/2017_all_passages.csv", fill=T, sep=";") 

data  <- merge(data0,data1, all=T)

data  <- merge(data,data2, all=T) 
data  <- merge(data,data3, all=T) 
data  <- merge(data,data4, all=T) 
data  <- merge(data,data5, all=T) 
data  <- merge(data,data6, all=T) 
data  <- merge(data,data7, all=T) 
data  <- merge(data,data8, all=T) 
data <- merge(data, data9, all=T)
data <- merge(data, data10, all=T)   
data <- merge(data, data11, all=T)   


directory <- paste("E:/AIS_passage_lines/passage_lines_year_shiptypes.csv")
write.table(data, directory, sep=";", col.names = T, row.names=F)

