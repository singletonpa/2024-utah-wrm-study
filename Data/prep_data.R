########################################
# Project:  UDOT-23.206 Snow PMs
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
#           Shailendra Khanal (s.khanal@usu.edu)
# File:     prep_data.R
# About:    Loads, combines, cleans, and prepares survey data
########################################
# Notes

# Open R project first, then open this R script

# 2024-03-12 PS created
# 2024-05-01 SK edited
# 2024-05-26 PS edited
# 2024-06-18 SK edited
# 2025-03-17 SK edited
# 2025-06-21 PS edited
# 2025-06-30 PS edited

# Load packages
library(dplyr)
library(sf)
library(mapview)

########################################
# Load data

# Survey 1 A
fpath <- file.path("Data", "Survey 1 A", "UDOT+Snow+PMs+Survey+1+A_February+19,+2024_FL.csv")
# data and column names
dat1 <- read.csv(fpath, stringsAsFactors=F); rm(fpath)
dat1_names <- data.frame(NAME1=as.character(names(dat1)), stringsAsFactors=F)
dat1_names$NAME2 <- as.character(dat1[1,])
dat1_names$NAME3 <- as.character(dat1[2,])
dat1 <- dat1[-c(1:2),]
row.names(dat1) <- 1:nrow(dat1)
# backup
tdat1 <- dat1
tdat1_names <- dat1_names

# Survey 2 F
fpath <- file.path("Data", "Survey 2 F", "UDOT+Snow+PMs+Survey+2+F_February+19,+2024_FL.csv")
# data and column names
dat2 <- read.csv(fpath, stringsAsFactors=F); rm(fpath)
dat2_names <- data.frame(NAME1=as.character(names(dat2)), stringsAsFactors=F)
dat2_names$NAME2 <- as.character(dat2[1,])
dat2_names$NAME3 <- as.character(dat2[2,])
dat2 <- dat2[-c(1:2),]
row.names(dat2) <- 1:nrow(dat2)
# backup
tdat2 <- dat2
tdat2_names <- dat2_names

# Survey 3 BCDE
fpath <- file.path("Data", "Survey 3 BCDE", "UDOT+Snow+PMs+Survey+3+BCDE_March+11,+2024_FL.csv")
# data and column names
dat3 <- read.csv(fpath, stringsAsFactors=F); rm(fpath)
dat3_names <- data.frame(NAME1=as.character(names(dat3)), stringsAsFactors=F)
dat3_names$NAME2 <- as.character(dat3[1,])
dat3_names$NAME3 <- as.character(dat3[2,])
dat3 <- dat3[-c(1:2),]
row.names(dat3) <- 1:nrow(dat3)
# backup
tdat3 <- dat3
tdat3_names <- dat3_names

########################################
# Combine data

# Check common columns
table(names(dat1) %in% names(dat2))
table(names(dat2) %in% names(dat1))
table(names(dat1) %in% names(dat3))
table(names(dat3) %in% names(dat1))
table(names(dat2) %in% names(dat3))
table(names(dat3) %in% names(dat2))
names(dat1)[!(names(dat1) %in% names(dat3))]
names(dat2)[!(names(dat2) %in% names(dat3))]
names(dat3)[!(names(dat3) %in% names(dat1))]
names(dat3)[!(names(dat3) %in% names(dat2))]
table(unique(dat1$opp))
table(unique(dat1$ppid))
table(unique(dat2$opp))
table(unique(dat2$ppid))
table(unique(dat3$Q_RelevantIDLastStartDate))
table(unique(dat3$HOME1_9_TEXT))
table(unique(dat3$Zipcode_List_BCDE_Qual2751.0126UtahStormTracking.JW.))
table(unique(dat3$Zipcode_List))
table(unique(dat3$Bzips))
table(unique(dat3$Czips))
table(unique(dat3$Dzips))
table(unique(dat3$Ezips))

# Remove unnecessary un-common columns
dat1[,c("opp", "ppid")] <- NULL
dat2[,c("opp", "ppid")] <- NULL
dat3[,c("Q_RelevantIDLastStartDate", "Zipcode_List_BCDE_Qual2751.0126UtahStormTracking.JW.")] <- NULL
dat3[,c("Bzips", "Czips", "Dzips", "Ezips", "Zipcode_List")] <- NULL
dat1_names <- dat1_names[!(dat1_names$NAME1 %in% c("opp", "ppid")),]
dat2_names <- dat2_names[!(dat2_names$NAME1 %in% c("opp", "ppid")),]
dat3_names <- dat3_names[!(dat3_names$NAME1 %in% c("Q_RelevantIDLastStartDate", "Zipcode_List_BCDE_Qual2751.0126UtahStormTracking.JW.")),]
dat3_names <- dat3_names[!(dat3_names$NAME1 %in% c("Bzips", "Czips", "Dzips", "Ezips", "Zipcode_List")),]

# Add necessary un-common columns
table(dat3$HOME1_9_TEXT)
dat1$HOME1_9_TEXT <- ""
dat2$HOME1_9_TEXT <- ""
dat1_names <- rbind(dat1_names, dat3_names[dat3_names$NAME1=="HOME1_9_TEXT",])
dat2_names <- rbind(dat2_names, dat3_names[dat3_names$NAME1=="HOME1_9_TEXT",])

# Check common columns again
table(names(dat1) %in% names(dat2))
table(names(dat2) %in% names(dat1))
table(names(dat1) %in% names(dat3))
table(names(dat3) %in% names(dat1))
table(names(dat2) %in% names(dat3))
table(names(dat3) %in% names(dat2))

# Put columns in same order as dat3
dat1 <- dat1[,names(dat3)]
dat2 <- dat2[,names(dat3)]
row.names(dat1_names) <- dat1_names$NAME1
row.names(dat2_names) <- dat2_names$NAME1
row.names(dat3_names) <- dat3_names$NAME1
dat1_names <- dat1_names[row.names(dat3_names),]
dat2_names <- dat2_names[row.names(dat3_names),]

# Check columns again
table(names(dat1)==names(dat2))
table(names(dat1)==names(dat3))
table(names(dat2)==names(dat3))
table(dat1_names==dat2_names)
table(dat1_names==dat3_names)
table(dat2_names==dat3_names)
dat1_names[dat1_names!=dat2_names]
dat2_names[dat2_names!=dat1_names]
dat1_names[dat1_names!=dat3_names]
dat3_names[dat3_names!=dat1_names]
dat2_names[dat2_names!=dat3_names]
dat3_names[dat3_names!=dat2_names]
# all are okay

# Combine data
dat <- rbind(dat1, dat2)
dat <- rbind(dat, dat3)
dat_names <- dat3_names

# Remove
rm(dat1, dat2, dat3)
rm(dat1_names, dat2_names, dat3_names)
rm(tdat1, tdat2, tdat3)
rm(tdat1_names, tdat2_names, tdat3_names)

########################################
# Remove unnecessary or identifiable columns

# Inspect
# View(dat)
# names(dat)

# Remove columns
rcol <- c("Status", "IPAddress", 
          "RecipientLastName", "RecipientFirstName", "RecipientEmail", 
          "ExternalReference", "LocationLatitude", "LocationLongitude", 
          "DistributionChannel", "UserLanguage", "Q_RecaptchaScore", 
          "Q_RelevantIDDuplicate", "Q_RelevantIDDuplicateScore", 
          "Q_RelevantIDFraudScore", "Q_BallotBoxStuffing", 
          "Q_TotalDuration", "Q_CHL", "wspid", "rid", "RISN", "transaction_id", 
          "SVID", "gc", "term", "zips", "pureSpectrumRedirectUrl", 
          "pureSpectrumSignatureValue", "LS")
dat <- dat[,!(names(dat) %in% rcol)]
dat_names <- dat_names[!(dat_names$NAME1 %in% rcol),]
row.names(dat_names) <- 1:nrow(dat_names)
rm(rcol)

# Backup
dat0 <- dat
# dat <- dat0

########################################
# Format data for analysis

# Start
# StartDate
table(dat$StartDate)
dat$StartDate <- as.POSIXct(dat$StartDate, tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
summary(dat$StartDate)
# EndDate
table(dat$EndDate)
dat$EndDate <- as.POSIXct(dat$EndDate, tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
summary(dat$EndDate)
# Progress
table(dat$Progress)
dat$Progress <- as.integer(dat$Progress)
# Duration..in.seconds. -> DurationSec
names(dat)[which(names(dat)=="Duration..in.seconds.")] <- "DurationSec"
table(dat$DurationSec)
dat$DurationSec <- as.integer(dat$DurationSec)
summary(dat$DurationSec)
# Finished
table(dat$Finished)
dat$Finished <- ifelse(dat$Finished=="True", T, ifelse(dat$Finished=="False", F, NA))
# RecordedDate
table(dat$RecordedDate)
dat$RecordedDate <- as.POSIXct(dat$RecordedDate, tz="America/Denver", format="%Y-%m-%d %H:%M:%S")
summary(dat$RecordedDate)
# ResponseId
unique(dat$ResponseId)
# ZIPCODE
table(dat$ZIPCODE)
dat$ZIPCODE <- factor(dat$ZIPCODE)
# LOIYES
table(dat$LOIYES)
dat$LOIYES <- factor(dat$LOIYES, levels=c("Accept", "Decline"))
# RESID
table(dat$RESID)
dat$RESID <- factor(dat$RESID, levels=c("Yes", "No"))
# AGE18
table(dat$AGE18)
dat$AGE18 <- factor(dat$AGE18, levels=c("Yes", "No"))
# rearrange columns in Start
dat1 <- dat[,c("ResponseId", "Finished", "Progress", "StartDate", "EndDate", "RecordedDate", "DurationSec", "LOIYES", "RESID", "AGE18", "ZIPCODE")]
dat2 <- dat[,c(which(names(dat)=="INCHES"):ncol(dat))]
dat <- cbind(dat1, dat2)
rm(dat1, dat2)

# Winter storm
# INCHES
table(dat$INCHES)
dat$INCHES <- factor(dat$INCHES, levels=c("Less than 1 inch", "1 inch", "2 inches", "3 inches", "4-6 inches", "7-11 inches", "1 foot or more"), ordered=T)
dat1 <- dat[,1:which(names(dat)=="INCHES")] 
dat2 <- dat[,(which(names(dat)=="INCHES")+1):ncol(dat)]
# - convert to factor
dat1$INCHES_4CAT <- factor(
  case_when(
    dat1$INCHES %in% c("Less than 1 inch", "1 inch") ~ "Light Snowfall",
    dat1$INCHES %in% c("2 inches", "3 inches") ~ "Moderate Snowfall",
    dat1$INCHES %in% c("4-6 inches") ~ "Heavy Snowfall",
    dat1$INCHES %in% c("7-11 inches", "1 foot or more") ~ "Extreme Snowfall"
  ),
  levels = c("Light Snowfall", "Moderate Snowfall", "Heavy Snowfall", "Extreme Snowfall"),
  ordered = TRUE
)
dat <- cbind(dat1, dat2)
rm(dat1, dat2)
table(dat$INCHES_4CAT)
# COMP1
table(dat$COMP1)
dat$COMP1 <- factor(dat$COMP1, levels=c("Much less", "Somewhat less", "About the same", "Somewhat more", "Much more"), ordered=T)
# COMP2
table(dat$COMP2)
dat$COMP2 <- factor(dat$COMP2, levels=c("Much shorter", "Somewhat shorter", "About the same", "Somewhat longer", "Much longer"), ordered=T)
# COMP3
table(dat$COMP3)
dat$COMP3 <- factor(dat$COMP3, levels=c("Much less", "Somewhat less", "About the same", "Somewhat more", "Much more"), ordered=T)
# COMP4
table(dat$COMP4)
dat$COMP4 <- factor(dat$COMP4, levels=c("Much worse", "Somewhat worse", "About the same", "Somewhat better", "Much better"), ordered=T)

# Travel behavior
# TRAVEL
table(dat$TRAVEL)
dat$TRAVEL <- factor(dat$TRAVEL, levels=c("Yes", "No"))
# NOTRAVEL
table(dat$NOTRAVEL)
unique(dat$NOTRAVEL_5_TEXT)
# - converting other responses
dat[dat$NOTRAVEL_5_TEXT=="work remotely",c("NOTRAVEL","NOTRAVEL_5_TEXT")] <- list("Things were cancelled (work, school, meetings, appointments, events).", "")
dat[dat$NOTRAVEL_5_TEXT=="Road down the mountain was closed",c("NOTRAVEL","NOTRAVEL_5_TEXT")] <- list("The roads, streets, and sidewalks were not in good condition.", "")
dat[dat$NOTRAVEL_5_TEXT=="Postponed Saturday work schedule",c("NOTRAVEL","NOTRAVEL_5_TEXT")] <- list("Things were cancelled (work, school, meetings, appointments, events).", "")
dat[dat$NOTRAVEL_5_TEXT=="Hard to see",c("NOTRAVEL","NOTRAVEL_5_TEXT")] <- list("The roads, streets, and sidewalks were not in good condition.", "")
dat[dat$NOTRAVEL_5_TEXT=="Worried about how the people would drive around me.",c("NOTRAVEL","NOTRAVEL_5_TEXT")] <- list("The roads, streets, and sidewalks were not in good condition.", "")
dat[dat$NOTRAVEL_5_TEXT=="I work from home and had food. ",c("NOTRAVEL","NOTRAVEL_5_TEXT")] <- list(" I had no reason to travel", "")
dat[dat$NOTRAVEL_5_TEXT=="No snow tires.",c("NOTRAVEL","NOTRAVEL_5_TEXT")] <- list("I had no transportation options.", "")
list_notravel <- c("I had no reason to travel", "Things were cancelled", "The roads, streets, and sidewalks were not in good condition.", "I had no transportation options.","Other")
dat1 <- dat[,1:which(names(dat)=="NOTRAVEL")]
dat2 <- dat[,(which(names(dat)=="NOTRAVEL")+1):ncol(dat)]
for (i in 1:length(list_notravel)) {
  dat1[,paste0("NOTRAVEL_", i)] <- grepl(list_notravel[i], dat1$NOTRAVEL)
}; rm(i)
dat <- cbind(dat1, dat2)
rm(dat1, dat2, list_notravel)
apply(dat[,grepl("NOTRAVEL_", names(dat))][,1:4], 2, table)
# PURPOSE
table(dat$PURPOSE)
unique(dat$PURPOSE_12_TEXT)
# - converting other responses
dat[dat$PURPOSE_12_TEXT=="Haircut",c("PURPOSE","PURPOSE_12_TEXT")] <- list("Other errands or appointments (bank, professional office, doctor/dentist, etc.)", "")
dat[dat$PURPOSE_12_TEXT=="Dentist",c("PURPOSE","PURPOSE_12_TEXT")] <- list("Other errands or appointments (bank, professional office, doctor/dentist, etc.)", "")
dat[dat$PURPOSE_12_TEXT=="hair appointment",c("PURPOSE","PURPOSE_12_TEXT")] <- list("Other errands or appointments (bank, professional office, doctor/dentist, etc.)", "")
dat[dat$PURPOSE_12_TEXT=="Church",c("PURPOSE","PURPOSE_12_TEXT")] <- list("Civic or religious activities", "")
dat[dat$PURPOSE_12_TEXT=="Funeral ",c("PURPOSE","PURPOSE_12_TEXT")] <- list("Civic or religious activities", "")
dat[dat$PURPOSE_12_TEXT=="Attend o of u basketball game ",c("PURPOSE","PURPOSE_12_TEXT")] <- list("Outdoor or indoor exercise (sports, jogging, bicycling, walking dog, gym, etc.)", "")
dat[dat$PURPOSE_12_TEXT=="Shovel show ",c("PURPOSE","PURPOSE_12_TEXT")] <- list("Social or entertainment activities (friends/relatives, movie, etc.)", "")
dat[dat$PURPOSE_12_TEXT=="work out",c("PURPOSE","PURPOSE_12_TEXT")] <- list("Outdoor or indoor exercise (sports, jogging, bicycling, walking dog, gym, etc.)", "")
dat[dat$PURPOSE_12_TEXT=="take out meal",c("PURPOSE","PURPOSE_12_TEXT")] <- list("Eat meal at restaurant", "")
dat[dat$PURPOSE_12_TEXT=="Living environment ",c("PURPOSE","PURPOSE_12_TEXT")] <- list("Outdoor or indoor exercise (sports, jogging, bicycling, walking dog, gym, etc.)", "")
list_purpose <- c("Work", "Attend school", "Work- or school-related activities", "Eat meal at restaurant", "Service private vehicle" ,
                  "Shopping", "Drop off or pick up passenger", "Civic or religious activities","Other errands or appointments",
                  "Outdoor or indoor exercise","Social or entertainment activities ","please specify")
dat1 <- dat[,1:which(names(dat)=="PURPOSE")]
dat2 <- dat[,(which(names(dat)=="PURPOSE")+1):ncol(dat)]
for (i in 1:length(list_purpose)) {
  dat1[,paste0("PURPOSE_", i)] <- grepl(list_purpose[i], dat1$PURPOSE)
}; rm(i)
dat <- cbind(dat1, dat2)
rm(dat1, dat2, list_purpose)
apply(dat[,grepl("PURPOSE_", names(dat))][,1:12], 2, table)
# MODE
table(dat$MODE)
unique(dat$MODE_9_TEXT)
# - converting other responses
dat[dat$MODE_9_TEXT=="Walk",c("MODE","MODE_9_TEXT")] <- list("Walk", "")
dat[dat$MODE_9_TEXT=="Hiking ",c("MODE","MODE_9_TEXT")] <- list("Walk", "")
dat[dat$MODE_9_TEXT=="Uber to/from doctor appointments",c("MODE","MODE_9_TEXT")] <- list("Car/Van/Truck/SUV Passenger", "")
list_mode <- c("Walk", "Bicycle", "Skateboard, scooter, wheelchair, or other small device", "Car/Van/Truck/SUV Driver", 
               "Car/Van/Truck/SUV Passenger", "Motorcycle", "Public bus or train", "School bus", "Other")
dat1 <- dat[,1:which(names(dat)=="MODE")]
dat2 <- dat[,(which(names(dat)=="MODE")+1):ncol(dat)]
for (i in 1:length(list_mode)) {
  dat1[,paste0("MODE_", i)] <- grepl(list_mode[i], dat1$MODE)
}; rm(i)
dat <- cbind(dat1, dat2)
rm(dat1, dat2, list_mode)
apply(dat[,grepl("MODE_", names(dat))][,1:9], 2, table)
# CHANGE
table(dat$CHANGE)
unique(dat$CHANGE_7_TEXT)
# - converting other responses
dat[dat$CHANGE_7_TEXT=="did not travel",c("CHANGE", "CHANGE_7_TEXT")] <- list("I canceled some of my planned activities / trips.", "")
dat[dat$CHANGE_7_TEXT=="during the storm, had no reason to leave home",c("CHANGE", "CHANGE_7_TEXT")] <- list("I canceled some of my planned activities / trips.", "")
dat[dat$CHANGE_7_TEXT=="I used my suv rather than my ev car",c("CHANGE", "CHANGE_7_TEXT")] <- list("I traveled using different transportation modes than normal.", "")
dat[dat$CHANGE_7_TEXT=="Had to clear the driveway first",c("CHANGE", "CHANGE_7_TEXT")] <- list("I waited until after the storm to do some activities / trips.", "")
dat[dat$CHANGE_7_TEXT=="Traveled just before the storm hit",c("CHANGE", "CHANGE_7_TEXT")] <- list("I changed the time-of-day when I did some activities / trips.", "")
dat[dat$CHANGE_7_TEXT=="No plan to travel at that time anyway.",c("CHANGE", "CHANGE_7_TEXT")] <- list("No changes", "")
dat[dat$CHANGE_7_TEXT=="Needed to go to get groceries but didn’t want to face the storm. Wait til it’s over ",c("CHANGE", "CHANGE_7_TEXT")] <- list("I changed the time-of-day when I did some activities / trips.", "")
list_change <- c("I did more activities / made more trips before the storm arrived.", "I canceled some of my planned activities / trips.", "I waited until after the storm to do some activities / trips.", "I changed the time-of-day when I did some activities / trips.", 
               "I traveled using different transportation modes than normal.", "I traveled more slowly or cautiously than normal.", "Other")
dat1 <- dat[,1:which(names(dat)=="CHANGE")]
dat2 <- dat[,(which(names(dat)=="CHANGE")+1):ncol(dat)]
for (i in 1:length(list_change)) {
  dat1[,paste0("CHANGE_", i)] <- grepl(list_change[i], dat1$CHANGE)
}; rm(i)
dat <- cbind(dat1, dat2)
rm(dat1, dat2, list_change)
apply(dat[,grepl("CHANGE_", names(dat))][,1:6], 2, table)

# Satisfaction
# SAT_TRANSPORT
table(dat$SAT_TRANSPORT_1)
table(dat$SAT_TRANSPORT_2)
table(dat$SAT_TRANSPORT_3)
table(dat$SAT_TRANSPORT_4)
table(dat$SAT_TRANSPORT_5)
table(dat$SAT_TRANSPORT_6)
dat$SAT_TRANSPORT_1[dat$SAT_TRANSPORT_1=="I don't know, or Not applicable"] <- NA
dat$SAT_TRANSPORT_2[dat$SAT_TRANSPORT_2=="I don't know, or Not applicable"] <- NA
dat$SAT_TRANSPORT_3[dat$SAT_TRANSPORT_3=="I don't know, or Not applicable"] <- NA
dat$SAT_TRANSPORT_4[dat$SAT_TRANSPORT_4=="I don't know, or Not applicable"] <- NA
dat$SAT_TRANSPORT_5[dat$SAT_TRANSPORT_5=="I don't know, or Not applicable"] <- NA
dat$SAT_TRANSPORT_6[dat$SAT_TRANSPORT_6=="I don't know, or Not applicable"] <- NA
levels_sat <- c("★☆☆☆☆ (1)", "★★☆☆☆ (2)", "★★★☆☆ (3)", "★★★★☆ (4)", "★★★★★ (5)")
labels_sat <- c(1,2,3,4,5)
dat$SAT_TRANSPORT_1 <- factor(dat$SAT_TRANSPORT_1, levels=levels_sat, labels=labels_sat, ordered=T)
dat$SAT_TRANSPORT_2 <- factor(dat$SAT_TRANSPORT_2, levels=levels_sat, labels=labels_sat, ordered=T)
dat$SAT_TRANSPORT_3 <- factor(dat$SAT_TRANSPORT_3, levels=levels_sat, labels=labels_sat, ordered=T)
dat$SAT_TRANSPORT_4 <- factor(dat$SAT_TRANSPORT_4, levels=levels_sat, labels=labels_sat, ordered=T)
dat$SAT_TRANSPORT_5 <- factor(dat$SAT_TRANSPORT_5, levels=levels_sat, labels=labels_sat, ordered=T)
dat$SAT_TRANSPORT_6 <- factor(dat$SAT_TRANSPORT_6, levels=levels_sat, labels=labels_sat, ordered=T)
rm(levels_sat, labels_sat)
# SAT_GROUP
table(dat$SAT_GROUP_1)
table(dat$SAT_GROUP_2)
table(dat$SAT_GROUP_3)
table(dat$SAT_GROUP_4)
dat$SAT_GROUP_1[dat$SAT_GROUP_1=="I don't know, or Not applicable"] <- NA
dat$SAT_GROUP_2[dat$SAT_GROUP_2=="I don't know, or Not applicable"] <- NA
dat$SAT_GROUP_3[dat$SAT_GROUP_3=="I don't know, or Not applicable"] <- NA
dat$SAT_GROUP_4[dat$SAT_GROUP_4=="I don't know, or Not applicable"] <- NA
levels_sat <- c("★☆☆☆☆ (1)", "★★☆☆☆ (2)", "★★★☆☆ (3)", "★★★★☆ (4)", "★★★★★ (5)")
labels_sat <- c(1,2,3,4,5)
dat$SAT_GROUP_1 <- factor(dat$SAT_GROUP_1, levels=levels_sat, labels=labels_sat, ordered=T)
dat$SAT_GROUP_2 <- factor(dat$SAT_GROUP_2, levels=levels_sat, labels=labels_sat, ordered=T)
dat$SAT_GROUP_3 <- factor(dat$SAT_GROUP_3, levels=levels_sat, labels=labels_sat, ordered=T)
dat$SAT_GROUP_4 <- factor(dat$SAT_GROUP_4, levels=levels_sat, labels=labels_sat, ordered=T)
rm(levels_sat, labels_sat)
# SATISFACTION
table(dat$SATISFACTION)
dat$SATISFACTION <- factor(dat$SATISFACTION, levels = c("Extremely dissatisfied", "Somewhat dissatisfied", "Neither satisfied nor dissatisfied", "Somewhat satisfied", "Extremely satisfied"), ordered=T)
# LOC_BETTER
unique(dat$LOC_BETTER)
# LOC_WORSE
unique(dat$LOC_WORSE)

# Information
# HEAR
table(dat$HEAR)
dat$HEAR <- factor(dat$HEAR, levels = c("None at all", "A little", "A moderate amount", "A lot", "A great deal"), ordered=T)
# INFO
table(dat$INFO)
list_info <- c("TV", "Radio", "Newspaper", "Road signs", "App", "Alert", "Online", "Social media", "Email", "Word-of-mouth", "Other")
dat1 <- dat[,1:which(names(dat)=="INFO")]
dat2 <- dat[,(which(names(dat)=="INFO")+1):ncol(dat)]
for (i in 1:length(list_info)) {
  dat1[,paste0("INFO_", i)] <- grepl(list_info[i], dat1$INFO)
}; rm(i)
dat <- cbind(dat1, dat2)
rm(dat1, dat2, list_info)
apply(dat[,grepl("INFO_", names(dat))][,1:11], 2, table)
dat1 <- dat[,1:which(names(dat)=="INFO_DETAIL_10")]
dat2 <- dat[,(which(names(dat)=="INFO_DETAIL_10")+1):ncol(dat)]
dat1$INFO_DETAIL_11 <- dat1$INFO_11_TEXT
dat1$INFO_11_TEXT <- NULL
dat <- cbind(dat1, dat2)
rm(dat1, dat2)
apply(dat[,grepl("INFO_", names(dat))][,1:11], 2, table)
unique(dat$INFO_DETAIL_1)
unique(dat$INFO_DETAIL_2)
unique(dat$INFO_DETAIL_3)
unique(dat$INFO_DETAIL_4)
unique(dat$INFO_DETAIL_5)
unique(dat$INFO_DETAIL_6)
unique(dat$INFO_DETAIL_7)
unique(dat$INFO_DETAIL_8)
unique(dat$INFO_DETAIL_9)
unique(dat$INFO_DETAIL_10)
unique(dat$INFO_DETAIL_11)
# - decided not to convert INFO_DETAIL_11 to other categories (INFO_{1-11}) and text to INFO_DETAIL
# APP
table(dat$APP)
dat$APP <- factor(dat$APP, levels=c("No", "Yes, a little", "Yes, a lot"), ordered=T)
# APP_USES
unique(dat$APP_USES)
# APP_FEEDBACK
unique(dat$APP_FEEDBACK)

# Priorities
# PRIORITY_TRANSPORT
table(dat$PRIORITY_TRANSPORT_1)
table(dat$PRIORITY_TRANSPORT_2)
table(dat$PRIORITY_TRANSPORT_3)
table(dat$PRIORITY_TRANSPORT_5)
table(dat$PRIORITY_TRANSPORT_6)
table(dat$PRIORITY_TRANSPORT_7)
levels_priority <- as.character(6:1)
dat$PRIORITY_TRANSPORT_1 <- factor(dat$PRIORITY_TRANSPORT_1, levels=levels_priority, ordered=T)
dat$PRIORITY_TRANSPORT_2 <- factor(dat$PRIORITY_TRANSPORT_2, levels=levels_priority, ordered=T)
dat$PRIORITY_TRANSPORT_3 <- factor(dat$PRIORITY_TRANSPORT_3, levels=levels_priority, ordered=T)
dat$PRIORITY_TRANSPORT_5 <- factor(dat$PRIORITY_TRANSPORT_5, levels=levels_priority, ordered=T)
dat$PRIORITY_TRANSPORT_6 <- factor(dat$PRIORITY_TRANSPORT_6, levels=levels_priority, ordered=T)
dat$PRIORITY_TRANSPORT_7 <- factor(dat$PRIORITY_TRANSPORT_7, levels=levels_priority, ordered=T)
rm(levels_priority)
# PRIORITY_PLACES
table(dat$PRIORITY_PLACES_1)
table(dat$PRIORITY_PLACES_2)
table(dat$PRIORITY_PLACES_3)
table(dat$PRIORITY_PLACES_4)
table(dat$PRIORITY_PLACES_5)
table(dat$PRIORITY_PLACES_6)
levels_priority <- as.character(6:1)
dat$PRIORITY_PLACES_1 <- factor(dat$PRIORITY_PLACES_1, levels=levels_priority, ordered=T)
dat$PRIORITY_PLACES_2 <- factor(dat$PRIORITY_PLACES_2, levels=levels_priority, ordered=T)
dat$PRIORITY_PLACES_3 <- factor(dat$PRIORITY_PLACES_3, levels=levels_priority, ordered=T)
dat$PRIORITY_PLACES_4 <- factor(dat$PRIORITY_PLACES_4, levels=levels_priority, ordered=T)
dat$PRIORITY_PLACES_5 <- factor(dat$PRIORITY_PLACES_5, levels=levels_priority, ordered=T)
dat$PRIORITY_PLACES_6 <- factor(dat$PRIORITY_PLACES_6, levels=levels_priority, ordered=T)
rm(levels_priority)

# Personal and household characteristics
# AGE
table(dat$AGE)
dat$AGE <- as.integer(dat$AGE)
summary(dat$AGE)
dat1 <- dat[,c(1:which(names(dat)=="AGE"))]
dat2 <- dat[,c((which(names(dat)=="AGE")+1):ncol(dat))]
dat1$AGE_6CAT <- cut(dat$AGE, breaks=c(18,25,35,45,65,85,100), labels=c("18-24", "25-34", "35-44", "45-64", "65-84", "85+"), right=F)
dat1$AGE_6CAT <- factor(dat1$AGE_6CAT, ordered=T)
dat1$AGE_5CAT <- cut(dat$AGE, breaks=c(18,25,35,45,65,100), labels=c("18-24", "25-34", "35-44", "45-64", "65+"), right=F)
dat1$AGE_5CAT <- factor(dat1$AGE_5CAT, ordered=T)
dat1$AGE_4CAT <- cut(dat$AGE, breaks=c(18,35,50,65,100), labels=c("18-34", "35-49", "50-64", "65+"), right=F)
dat1$AGE_4CAT <- factor(dat1$AGE_4CAT, ordered=T)
summary(dat1$AGE_6CAT)
summary(dat1$AGE_5CAT)
summary(dat1$AGE_4CAT)
dat <- cbind(dat1, dat2)
rm(dat1, dat2)
# GENDER
table(dat$GENDER)
dat$GENDER[dat$GENDER=="Prefer not to answer"] <- NA
dat$GENDER <- factor(dat$GENDER, levels=c("Female", "Male", "Nonbinary/nonconforming", "Other:"))
unique(dat$GENDER_5_TEXT)
dat1 <- dat[,c(1:which(names(dat)=="GENDER"))]
dat2 <- dat[,c((which(names(dat)=="GENDER")+1):ncol(dat))]
dat1$GENDER_2CAT <- factor(dat$GENDER, levels=c("Female", "Male"))
dat <- cbind(dat1, dat2)
rm(dat1, dat2)
# RACEETH
table(dat$RACEETH)
unique(dat$RACEETH_9_TEXT)
# - decided not to convert other responses
dat$RACEETH[dat$RACEETH=="Prefer not to say"] <- NA
dat$RACEETH <- factor(dat$RACEETH, levels=c("African American or Black", "Asian", "Hispanic or Latino/a/x", "Middle Eastern" , "Native American or Alaskan Native" , "Pacific Islander or Native Hawaiian" , "South Asian or Indian" , "White or Caucasian" , "Other(s):"))
dat1 <- dat[,c(1:which(names(dat)=="RACEETH"))]
dat2 <- dat[,c((which(names(dat)=="RACEETH")+1):ncol(dat))]
dat1$RACE_2CAT <- "Non-White"
dat1$RACE_2CAT[dat1$RACEETH == "White or Caucasian"] <- "White"
dat1$RACE_2CAT <- factor(dat1$RACE_2CAT, levels = c("White", "Non-White"))
dat <- cbind(dat1, dat2)
table(dat$RACE_2CAT)
rm(dat1, dat2)
# STUD
table(dat$STUD)
dat$STUD <- factor(dat$STUD, levels=c("No", "Yes"))
# EDUC
table(dat$EDUC)
dat$EDUC[dat$EDUC=="Prefer not to answer"] <- NA
dat$EDUC <- factor(dat$EDUC, levels=c("Less than a high school diploma", "High school diploma or equivalent (e.g. GED)", "Bachelor's or associate degree", "Master's degree, doctorate degree, or professional degree beyond bachelor's degree"), ordered=T)
dat1 <- dat[,c(1:which(names(dat)=="EDUC"))]
dat2 <- dat[,c((which(names(dat)=="EDUC")+1):ncol(dat))]
dat1$EDUC_4CAT <- dat1$EDUC
levels(dat1$EDUC_4CAT) <- c("LessHS", "HighSch", "Bachelor", "Higher")
dat1$EDUC_3CAT <- dat1$EDUC_4CAT
levels(dat1$EDUC_3CAT) <- c("HSBelow", "HSBelow", "Bachelor", "Higher")
summary(dat1$EDUC_4CAT)
summary(dat1$EDUC_3CAT)
dat <- cbind(dat1, dat2)
rm(dat1, dat2)
# WORK
table(dat$WORK)
dat$WORK <- factor(dat$WORK, levels=c("Yes", "No"))
# HHINC
table(dat$HHINC)
dat$HHINC[dat$HHINC %in% c("Prefer not to answer", "Don't know")] <- NA
dat$HHINC <- factor(dat$HHINC, levels=c("Less than $10,000", "$10,000 to $14,999", "$15,000 to $24,999", "$25,000 to $34,999", "$35,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999", "$100,000 to $149,999", "$150,000 or more"), ordered=T)
dat1 <- dat[,c(1:which(names(dat)=="HHINC"))]
dat2 <- dat[,c((which(names(dat)=="HHINC")+1):ncol(dat))]
dat1$HHINC_9CAT <- dat1$HHINC
levels(dat1$HHINC_9CAT) <- c("0-10", "10-15", "15-25", "25-35", "35-50", "50-75", "75-100", "100-150", "150+")
dat1$HHINC_6CAT <- dat1$HHINC_9CAT
levels(dat1$HHINC_6CAT) <- c("0-25", "0-25", "0-25", "25-50", "25-50", "50-75", "75-100", "100-150", "150+")
dat1$HHINC_3CAT <- dat1$HHINC_6CAT
levels(dat1$HHINC_3CAT) <- c("0-50", "0-50", "50-100", "50-100", "100+", "100+")
summary(dat1$HHINC_9CAT)
summary(dat1$HHINC_6CAT)
summary(dat1$HHINC_3CAT)
dat <- cbind(dat1, dat2)
rm(dat1, dat2)
# HOME1
table(dat$HOME1)
dat$HOME1 <- factor(dat$HOME1, levels=c("Single-family house, detached from any other house", "Single-family house, attached to other houses (row house)", "Building with 2 apartments/condos (duplex)", "Building with 3 or 4 apartments/condos", "Building with 5 to 9 apartments/condos", "Building with 10 to 19 apartments/condos", "Building with 20 or more apartments/condos", "Mobile home or trailer", "Other (please specify)"))
unique(dat$HOME1_9_TEXT)
dat[dat$HOME1_9_TEXT=="Rv",c("HOME1", "HOME1_9_TEXT")] <- list("Mobile home or trailer", "")
dat1 <- dat[,c(1:which(names(dat)=="HOME1_9_TEXT"))]
dat2 <- dat[,c((which(names(dat)=="HOME1_9_TEXT")+1):ncol(dat))]
dat1$HOME1_8CAT <- dat1$HOME1
levels(dat1$HOME1_8CAT) <- c("SFdet", "SFatt", "Apt2", "Apt3-4", "Apt5-9", "Apt10-19", "Apt20+", "Mobile", NA)
dat1$HOME1_5CAT <- dat1$HOME1
levels(dat1$HOME1_5CAT) <- c("SFdet", "SFatt", "Apt2-4", "Apt2-4", "Apt5+", "Apt5+", "Apt5+", "Mobile", NA)
dat1$HOME1_4CAT <- dat1$HOME1_5CAT
levels(dat1$HOME1_4CAT) <- c("SFdet", "SFatt", "Apartment", "Apartment", "Mobile", NA)
dat1$HOME1_4CAT[dat1$HOME1_9_TEXT %in% c("APARTMENT BUILDNG", "Apartment ")] <- "Apartment"
dat1$HOME1_2CAT <- dat1$HOME1_4CAT
levels(dat1$HOME1_2CAT) <- c("SFdet", "Other", "Other", "Other", NA)
dat1$HOME1_2CAT[dat1$HOME1_9_TEXT!=""] <- "Other"
summary(dat1$HOME1_8CAT)
summary(dat1$HOME1_5CAT)
summary(dat1$HOME1_4CAT)
summary(dat1$HOME1_2CAT)
dat <- cbind(dat1, dat2)
rm(dat1, dat2)
# HOME2
table(dat$HOME2)
dat$HOME2 <- factor(dat$HOME2, levels=c("Owned or mortgaged", "Rented"))
# ADULTS
table(dat$ADULTS)
dat$ADULTS <- factor(dat$ADULTS, levels=c("1 (just me)", "2", "3", "4", "5", "6+"), labels=c(1:5,"6+"), ordered=T)
dat1 <- dat[,c(1:which(names(dat)=="ADULTS"))]
dat2 <- dat[,c((which(names(dat)=="ADULTS")+1):ncol(dat))]
dat1$ADULTS_NUM <- as.integer(dat1$ADULTS)
table(dat1$ADULTS_NUM)
dat <- cbind(dat1, dat2)
rm(dat1, dat2)
# CHILDREN
table(dat$CHILDREN)
dat$CHILDREN <- factor(dat$CHILDREN, levels=c("0", "1", "2", "3", "4", "5+"), ordered=T)
dat1 <- dat[,c(1:which(names(dat)=="CHILDREN"))]
dat2 <- dat[,c((which(names(dat)=="CHILDREN")+1):ncol(dat))]
dat1$CHILDREN_NUM <- as.integer(dat1$CHILDREN)-1
table(dat1$CHILDREN_NUM)
dat <- cbind(dat1, dat2)
rm(dat1, dat2)
# BIKES
table(dat$BIKES)
dat$BIKES <- factor(dat$BIKES, levels=c("0", "1", "2", "3", "4", "5+"), ordered=T)
dat1 <- dat[,c(1:which(names(dat)=="BIKES"))]
dat2 <- dat[,c((which(names(dat)=="BIKES")+1):ncol(dat))]
dat1$BIKES_NUM <- as.integer(dat1$BIKES)-1
table(dat1$BIKES_NUM)
dat <- cbind(dat1, dat2)
rm(dat1, dat2)
# CARS
table(dat$CARS)
dat$CARS <- factor(dat$CARS, levels=c("0", "1", "2", "3", "4", "5+"), ordered=T)
dat1 <- dat[,c(1:which(names(dat)=="CARS"))]
dat2 <- data.frame(END=dat[,c((which(names(dat)=="CARS")+1):ncol(dat))])
dat1$CARS_NUM <- as.integer(dat1$CARS)-1
table(dat1$CARS_NUM)
dat <- cbind(dat1, dat2)
rm(dat1, dat2)
# END
unique(dat$END)

# Anonymize some text columns
dat$NOTRAVEL_5_TEXT <- ""
dat$PURPOSE_12_TEXT <- ""
dat$MODE_9_TEXT <- ""
dat$CHANGE_7_TEXT <- ""
dat$GENDER_5_TEXT <- ""
dat$RACEETH_9_TEXT <- ""
dat$HOME1_9_TEXT <- ""

# Backup
dat1 <- dat
# dat <- dat1

########################################
# Add storm event and zip code list context

# Load zip code list
zipslist <- read.csv(file=file.path("Data", "Regions-ZIPs", "zipslist.csv"))

# Add zip code list (A-F)
ziplist <- list(A=zipslist$A, B=zipslist$B, C=zipslist$C, D=zipslist$D, E=zipslist$E, F=zipslist$F)
for (i in 1:length(ziplist)) {
  ziplist[[i]] <- ziplist[[i]][!is.na(ziplist[[i]])]
}; rm(i)
data <- dat[,c(1:which(names(dat)=="ZIPCODE"))]
datb <- dat[,c((which(names(dat)=="ZIPCODE")+1):ncol(dat))]
data$ZONE <- NA
for (i in 1:length(ziplist)) {
  data$ZONE[data$ZIPCODE %in% ziplist[[i]]] <- names(ziplist)[i]
}; rm(i)
data$ZONE <- factor(data$ZONE, levels=c("A", "B", "C", "D", "E", "F"))
summary(data$ZONE)

# Add storm date information
data$STORMDATE1 <- ifelse(data$ZONE %in% c("A", "F"), "2024-02-08", ifelse(data$ZONE %in% c("B", "C", "D", "E"), "2024-03-02", NA))
data$STORMDATE2 <- ifelse(data$ZONE %in% c("A", "F"), "2024-02-09", ifelse(data$ZONE %in% c("B", "C", "D", "E"), "2024-03-03", NA))
data$STORMDATE1 <- as.Date(data$STORMDATE1, tz="America/Denver", format="%Y-%m-%d")
data$STORMDATE2 <- as.Date(data$STORMDATE2, tz="America/Denver", format="%Y-%m-%d")
table(data$STORMDATE1)
table(data$STORMDATE2)
dat <- cbind(data, datb)
rm(data, datb)

# Remove
rm(zipslist, ziplist)

########################################
# Add storm data

# Load storm data
feb_storm_data <- readRDS(file.path("Data", "Storm Data", "feb_storm_data_zip.rds"))
mar_storm_data <- readRDS(file.path("Data", "Storm Data", "mar_storm_data_zip.rds"))

# Load zipcode shapefile
utah_zipcodes <- st_read("Data/Regions-ZIPs/Utah_ZIP_Code_Areas/ZipCodes.shp") %>%  st_make_valid()

# Load storm data sensor locations
feb_storm_data_rwis <- readRDS(file.path("Data", "Storm Data", "feb_storm_data_rwis.rds"))
mar_storm_data_rwis <- readRDS(file.path("Data", "Storm Data", "mar_storm_data_rwis.rds"))
rwis_locations <- feb_storm_data_rwis[c("SITE", "LATITUDE", "LONGITUDE")]
rm(feb_storm_data_rwis, mar_storm_data_rwis)
rwis_locations <- st_as_sf(rwis_locations, coords=c("LONGITUDE","LATITUDE"), crs=4326)

# Check zipcodes survey versus storm data
tzips1 <- unique(dat$ZIPCODE)
tzips2 <- unique(c(feb_storm_data$ZIP5, mar_storm_data$ZIP5))
table(tzips1 %in% tzips2)
sort(as.character(tzips1[which(!(tzips1 %in% tzips2))]))

# Inspect on map
zips <- utah_zipcodes
zips$surveyed <- zips$ZIP5 %in% tzips1
zips$stormdata <- zips$ZIP5 %in% tzips2
zips$check <- ifelse(zips$surveyed & zips$stormdata, "survey+stormdata", 
              ifelse(zips$surveyed & !zips$stormdata, "survey-nostormdata", 
              ifelse(!zips$surveyed & zips$stormdata, "nosurvey-stormdata", NA)))
mapview(zips, zcol="surveyed")
mapview(zips, zcol="stormdata")
mapview(zips, zcol="check") + mapview(rwis_locations, legend=F)

# Map inspection: which missing, which replacement
tlist <- list(
  "84004"="84003", "84010"="84014", "84054"="84014", "84070"="84047", 
  "84087"="84014", "84093"="84047", "84094"="84047", 
  "84314"="84302", "84318"="84341", 
  "84602"="84606", "84622"="84665", "84624"="84631", "84627"="84642", 
  "84634"="84665", "84635"="84631", "84643"="84665", "84647"="84629", 
  "84662"="84629", "84664"="84606", 
  "84712"="84732", "84714"="84721", "84750"="84740", "84754"="84701"
)

# Initialize
dat$ID <- 1:nrow(dat)
dat$ZIPMATCH <- as.character(dat$ZIPCODE)
# assign zipcodes
for (i in 1:length(tlist)) {
  zip1 <- names(tlist)[i]
  zip2 <- unlist(tlist[i])[1]
  dat$ZIPMATCH[dat$ZIPMATCH==zip1] <- zip2
  rm(zip1, zip2)
}; rm(i)
# check
table(unique(dat$ZIPCODE) %in% tzips2)
table(unique(dat$ZIPMATCH) %in% tzips2)
table(dat$ZIPCODE)
table(dat$ZIPMATCH)

# Split data
datAF <- dat[dat$ZONE %in% c("A","F"),]
datBCDE <- dat[dat$ZONE %in% c("B","C","D","E"),]

# Merge storm data
datAF <- merge(datAF, feb_storm_data, by.x="ZIPMATCH", by.y="ZIP5", all.x=T, all.y=F)
datBCDE <- merge(datBCDE, mar_storm_data, by.x="ZIPMATCH", by.y="ZIP5", all.x=T, all.y=F)

# Merge data
dat <- rbind(datAF, datBCDE)
dat <- dat[order(dat$ID),]
row.names(dat) <- NULL
dat$ZIPMATCH <- NULL
dat$ID <- NULL

# Remove
rm(datAF, datBCDE)
rm(tzips1, tzips2, tlist)
rm(zips, utah_zipcodes)
rm(rwis_locations)
rm(feb_storm_data, mar_storm_data)

# Backup
dat2 <- dat
# dat <- dat2

########################################
# Save data

# Inspect
summary(dat)

# Save dat0
write.csv(dat_names, file.path("Data", "dat0_names.csv"), row.names=F)
saveRDS(dat_names, file.path("Data", "dat0_names.rds"))
write.csv(dat0, file.path("Data", "dat0.csv"), row.names=F)
saveRDS(dat0, file.path("Data", "dat0.rds"))

# Save dat1
write.csv(dat1, file.path("Data", "dat1.csv"), row.names=F)
saveRDS(dat1, file.path("Data", "dat1.rds"))

# Save dat2
write.csv(dat2, file.path("Data", "dat2.csv"), row.names=F)
saveRDS(dat2, file.path("Data", "dat2.rds"))

# Remove
rm(dat, dat_names, dat0, dat1, dat2)
gc()

########################################
# END
########################################