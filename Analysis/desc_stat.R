########################################
# Project:  UDOT-23.206 Snow PMs
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
#           Shailendra Khanal (s.khanal@usu.edu)
# File:     desc_stat.R
# About:    Calculate descriptive statistics
########################################
# Notes

# Open R project first, then open this R script

# 2024-10-23 SK created
# 2024-11-12 SK edited
# 2025-02-21 SK edited
# 2025-03-28 PS edited
# 2025-04-04 SK edited
# 2025-07-02 PS edited
# 2025-07-12 PS edited

# Load packages
# library()

########################################
# Load and process data

# Load data
dat <- readRDS(file.path("Data", "dat2.rds"))
dat_names <- readRDS(file.path("Data", "dat0_names.rds"))

# Backup
xdat <- dat
# dat <- xdat

# Inspect
summary(dat)

# Load functions for figures
source(file.path("Analysis", "funs_figs.R"))

########################################
# Calculate descriptive statistics

# Initialize
vars <- c("INCHES_4CAT", "AverageIndex", 
            "AGE_4CAT", "GENDER_2CAT", "RACE_2CAT", "STUD", "EDUC_3CAT", 
            "WORK", "HHINC_6CAT", "ZONE", "HOME1_2CAT", "HOME2", 
            "ADULTS_NUM", "CHILDREN_NUM", "BIKES_NUM", "CARS_NUM")
ds <- data.frame(var=character(), lev=character(), num=integer(), perc=numeric(), mean=numeric(), sd=numeric())

# Calculate descriptive stats
for (i in 1:length(vars)) {
  td <- dat[,vars[i]]
  if ("factor" %in% class(td)) {
    tt <- table(td)
    tp <- prop.table(table(td))
    tn <- names(tt)
    ds <- rbind(ds, data.frame(var=vars[i], lev=tn, num=as.vector(tt), perc=100*as.vector(tp), mean=NA, sd=NA))
    rm(tt, tp, tn)
  } else if ("numeric" %in% class(td) | "integer" %in% class(td)) {
    tm <- mean(td, na.rm=T)
    tsd <- sd(td, na.rm=T)
    ds <- rbind(ds, data.frame(var=vars[i], lev=NA, num=NA, perc=NA, mean=tm, sd=tsd))
    rm(tm, tsd)
  }
  rm(td)
}; rm(i)

# Inspect
ds

# Save
write.csv(ds, file=file.path("Analysis", "desc_stat.csv"), row.names=F)

########################################
# Create figures

# Plotting parameters
dims1 <- list("png", 500, 500, "px", 150)
dims2 <- list("png", 1000, 500, "px", 150)
dims1a <- list("png", 6.0, 5, "in", 300)
dims2a <- list("png", 12.0, 5, "in", 300)

# Winter storm
# - INCHES
plot_bar1(dat$INCHES, dims=dims2a, colpal="violet", 
          lab1=dat_names$NAME2[dat_names$NAME1=="INCHES"], 
          save=T, file="1_WS_INCHES")
plot_box_cat_cont(df=dat, catvar="INCHES", contvar="AverageIndex", 
                  lab1=dat_names$NAME2[dat_names$NAME1=="INCHES"],
                  lab2="RWIS SII",
                  title="RWIS Average Storm Intensity Index (SII) by Perceived Snow Accumulation",
                  dims = dims2a, colpal="skyblue", 
                  save=T, file="1_WS_INCHES_SII")
plot_box_cat_cont(df=dat, catvar="INCHES", contvar="StormEventIndex", 
                  lab1=dat_names$NAME2[dat_names$NAME1=="INCHES"],
                  lab2="RWIS SSI",
                  title="RWIS Average Storm Severity Index (SSI) by Perceived Snow Accumulation",
                  dims = dims2a, colpal="lightgreen", 
                  save=T, file="1_WS_INCHES_SSI")
# - COMP
plot_bar1(dat$COMP1, dims=dims2a, colpal="violet", 
          lab1=dat_names$NAME2[dat_names$NAME1=="COMP1"], 
          save=T, file="1_WS_COMP1")
plot_bar1(dat$COMP2, dims=dims2a, colpal="violet", 
          lab1=dat_names$NAME2[dat_names$NAME1=="COMP2"], 
          save=T, file="1_WS_COMP2")
plot_bar1(dat$COMP3, dims=dims2a, colpal="violet", 
          lab1=dat_names$NAME2[dat_names$NAME1=="COMP3"], 
          save=T, file="1_WS_COMP3")
plot_bar1(dat$COMP4, dims=dims2a, colpal="violet", 
          lab1=dat_names$NAME2[dat_names$NAME1=="COMP4"], 
          save=T, file="1_WS_COMP4")

# Travel behavior
# - TRAVEL
plot_bar1(dat$TRAVEL, dims=dims2a, colpal="blue", 
          lab1=dat_names$NAME2[dat_names$NAME1=="TRAVEL"], 
          save=T, file="2_TB_TRAVEL")
# - NOTRAVEL
df <- dat[,paste("NOTRAVEL",c(1:5), sep="_")]
names(df) <- c("I had no reason to travel.", 
               "Things were cancelled (work,\nschool, meetings, appointments, events).", 
               "The roads, streets, and sidewalks\nwere not in good condition.", 
               "I had no transportation options", 
               "Other")
dat_names$NAME2[dat_names$NAME1=="NOTRAVEL"]
plot_barXTF(df, dims=dims2a, colpal="blue", 
            lab1="Why didn't you travel (leave home)?", 
            save=T, file="2_TB_NOTRAVEL")
rm(df)
# - PURPOSE
df <- dat[,paste("PURPOSE",c(1:12), sep="_")]
names(df) <- c("Work", 
               "Attend school", 
               "Work- or school-related activities", 
               "Eat meal at restaurant", 
               "Service private vehicle\n(gas, oil, repairs, etc.)", 
               "Shopping (groceries, clothing,\nconvenience store, etc)", 
               "Drop off or pick up passenger(s)", 
               "Civic or religious activities", 
               "Other errands or appointments (bank,\nprofessional office, doctor/dentist, etc.)", 
               "Outdoor or indoor exercise (sports,\njogging, bicycling, walking dog, gym, etc.)", 
               "Social or entertainment activities\n(friends/relatives, movie, etc.)", 
               "Other")
dat_names$NAME2[dat_names$NAME1=="PURPOSE"]
plot_barXTF(df, dims=dims2a, colpal="blue", 
            lab1="Why did you travel (leave home)? For what purpose(s)?", 
            save=T, file="2_TB_PURPOSE")
rm(df)
# - MODE
df <- dat[,paste("MODE",c(1:9), sep="_")]
names(df) <- c("Walk", 
               "Bicycle", 
               "Skateboard, scooter, wheelchair,\nor other small device", 
               "Car/Van/Truck/SUV Driver", 
               "Car/Van/Truck/SUV Passenger", 
               "Motorcycle", 
               "Public bus or train (examples:\nUTA, TRAX, FrontRunner)", 
               "School bus", 
               "Other")
dat_names$NAME2[dat_names$NAME1=="MODE"]
plot_barXTF(df, dims=dims2a, colpal="blue", 
            lab1="What transportation mode(s) did you use?", 
            save=T, file="2_TB_MODE")
rm(df)
# - CHANGE
df <- dat[,paste("CHANGE",c(1:7), sep="_")]
names(df) <- c("I did more activities / made\nmore trips before the storm arrived.", 
               "I canceled some of my planned\nactivities / trips.", 
               "I waited until after the storm\nto do some activities / trips.", 
               "I changed the time-of-day when\nI did some activities / trips.", 
               "I traveled using different\ntransportation modes than normal.", 
               "I traveled more slowly or\ncautiously than normal.", 
               "Other")
dat_names$NAME2[dat_names$NAME1=="CHANGE"]
plot_barXTF(df, dims=dims2a, colpal="blue", 
            lab1="Because of the winter storm, did you make any changes to how you traveled (got around)?", 
            save=T, file="2_TB_CHANGE")
rm(df)

# Satisfaction
# - SAT_TRANSPORT
df <- dat[,paste("SAT_TRANSPORT",c(1:6), sep="_")]
names(df) <- c("Limited-access highways\n(example: I-15)", 
               "Major roads with more traffic\n(example: US-89)", 
               "Local neighborhood streets", 
               "Sidewalks and pedestrian crossings", 
               "Bus stops and train stations", 
               "Bike lanes and trails")
star_labels <- c("★★★★★ (5)", "★★★★☆ (4)", "★★★☆☆ (3)", "★★☆☆☆ (2)",  "★☆☆☆☆ (1)")
df[] <- lapply(df, function(x) {
  factor(x, levels=c(5,4,3,2,1), labels=star_labels, exclude=NULL)
})
dat_names$NAME2[dat_names$NAME1=="SAT_TRANSPORT_1"]
plot_barXABC(df, dims=dims2a, colpal="Blues", 
             lab1="How would you rate (1-5 stars) snow and ice clearance on the following types of transportation facilities?", 
             save=T, file="3_SAT_TRANSPORT") 
rm(star_labels, df)
# - SAT_GROUP
df <- dat[,paste("SAT_GROUP",c(1:4), sep="_")]
names(df) <- c("A building owner or property manager", 
               "Local businesses", 
               "City or county government", 
               "State government (UDOT)")
star_labels <- c("★★★★★ (5)", "★★★★☆ (4)", "★★★☆☆ (3)", "★★☆☆☆ (2)",  "★☆☆☆☆ (1)")
df[] <- lapply(df, function(x) {
  factor(x, levels=c(5,4,3,2,1), labels=star_labels, exclude=NULL)
})
dat_names$NAME2[dat_names$NAME1=="SAT_GROUP_1"]
plot_barXABC(df, dims=dims2a, colpal="Blues", 
             lab1="How would you rate (1-5 stars) snow and ice clearance by the following groups?", 
             save=T, file="3_SAT_GROUP") 
rm(star_labels, df)
# - SATISFACTION
plot_bar1(dat$SATISFACTION, dims=dims2a, colpal="steelblue", 
          lab1=dat_names$NAME2[dat_names$NAME1=="INCHES"], 
          save=T, file="3_SAT_SATISFACTION")
# - LOC_BETTER, LOC_WORSE
# none

# Information
# - HEAR
plot_bar1(dat$HEAR, dims=dims2a, colpal="forestgreen", 
          lab1=dat_names$NAME2[dat_names$NAME1=="HEAR"], 
          save=T, file="4_INFO_HEAR")
# - INFO
df <- dat[,paste("INFO",c(1:11), sep="_")]
names(df) <- c("TV", 
               "Radio", 
               "Newspaper", 
               "Road signs", 
               "App", 
               "Alert", 
               "Online", 
               "Social media", 
               "Email", 
               "Word-of-mouth", 
               "Other")
dat_names$NAME2[dat_names$NAME1=="INFO"]
plot_barXTF(df, dims=dims2a, colpal="forestgreen", 
            lab1="How do you normally get information about winter storms and transportation conditions?", 
            save=T, file="4_INFO_INFO")
rm(df)
# - INFO_DETAIL
# none
# - APP
plot_bar1(dat$APP, dims=dims2a, colpal="forestgreen", 
          lab1=dat_names$NAME2[dat_names$NAME1=="APP"], 
          save=T, file="4_INFO_APP")
# - APP_USES, APP_FEEDBACK
# none

# Priorities
# - PRIORITY_TRANSPORT
df <- dat[,paste("PRIORITY_TRANSPORT",c(1:3,5:7), sep="_")]
names(df) <- c("Limited-access highways\n(example: I-15)", 
               "Major roads with more traffic\n(example: US-89)", 
               "Local neighborhood streets", 
               "Sidewalks and pedestrian crossings", 
               "Bus stops and train stations", 
               "Bike lanes and trails")
priority_labels <- c("1 (Highest Priority)", "2", "3", "4", "5", "6 (Lowest Priority)")
df[] <- lapply(df, function(x) {
  factor(x, levels=c(1,2,3,4,5,6), labels=priority_labels, exclude=NULL)
})
dat_names$NAME2[dat_names$NAME1=="PRIORITY_TRANSPORT_1"]
plot_barXABC(df, dims=dims2a, colpal="Oranges", 
             lab1="How would you prioritize clearing snow and ice from the following types of transportation facilities?", 
             save=T, file="5_PRI_TRANSPORT")
rm(priority_labels, df)
# - PRIORITY_PLACES
df <- dat[,paste("PRIORITY_PLACES",c(1:6), sep="_")]
names(df) <- c("In downtowns and major business districts", 
               "Near schools and hospitals", 
               "Near parks and recreational areas", 
               "Near bus and rail transit centers", 
               "In residential neighborhoods", 
               "In industrial areas")
priority_labels <- c("1 (Highest Priority)", "2", "3", "4", "5", "6 (Lowest Priority)")
df[] <- lapply(df, function(x) {
  factor(x, levels=c(1,2,3,4,5,6), labels=priority_labels, exclude=NULL)
})
dat_names$NAME2[dat_names$NAME1=="PRIORITY_PLACES_1"]
plot_barXABC(df, dims=dims2a, colpal="Oranges", 
             lab1="How would you prioritize clearing snow and ice from the following types of places?", 
             save=T, file="5_PRI_PLACES")
rm(priority_labels, df)

# Personal and household characteristics
# - AGE, GENDER, RACEETH, STUD, EDUC, WORK
# none
# - HHINC, HOME1, HOME2, ADULTS, CHILDREN, BIKES, CARS
# none

########################################
# Cleanup

# Remove
rm(dat, dat_names, xdat)
rm(dims1, dims2, dims1a, dims2a)
rm(plot_bar1, plot_barXABC, plot_barXTF, plot_box_cat_cont, plot_boxX, plot_count2, plot_hist1)
gc()

########################################
# END
########################################