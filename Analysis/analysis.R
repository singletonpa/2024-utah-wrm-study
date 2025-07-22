########################################
# Project:  UDOT-23.206 Snow PMs
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
#           Shailendra Khanal (s.khanal@usu.edu)
# File:     analysis.R
# About:    Analyzes survey data
########################################
# Notes

# Open R project first, then open this R script

# 2024-10-23 SK created
# 2024-11-12 SK edited
# 2025-02-21 SK edited
# 2025-03-28 PS edited
# 2025-04-04 SK edited
# 2025-07-02 PS edited

# Load packages
library(MASS)
library(lmtest)
library(pscl)
library(openxlsx)

########################################
# Load and process data

# Load data
dat <- readRDS(file.path("Data", "dat2.rds"))

# Backup
xdat <- dat
# dat <- xdat

# Inspect
summary(dat)

# Relevel
dat$ZONE <- relevel(as.factor(dat$ZONE), ref="C")

########################################
# Functions

# Function to extract model statistics
extract_model_info <- function(model) {
  # null model
  m_null <- update(model, formula=". ~ 1")
  
  # sample size
  ss <- nrow(model$model)
  # log-likelihoods
  ll_n <- logLik(m_null)
  ll_m <- logLik(model)
  # pseudo R2
  pr2 <- 1 - (ll_m / ll_n)
  # AIC, BIC
  aic <- AIC(model)
  bic <- BIC(model)
  
  # combine
  mstats <- c(ss, ll_n, ll_m, pr2, aic, bic)
  
  # return
  return(mstats)
}

########################################
# Satisfaction with WRM overall

# Overall
ms_o <- polr(SATISFACTION ~ INCHES_4CAT + AverageIndex + 
               AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
               WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
               ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
             data=dat, method="logistic")
summary(ms_o)
coeftest(ms_o)

# Extract model statistics
ms_o_stats <- data.frame(o=extract_model_info(ms_o))
row.names(ms_o_stats) <- c("n", "ll_n", "ll_m", "pr2", "aic", "bic")

# Export to Excel worksheet
mylist <- list(o=as.data.frame(coeftest(ms_o)[,]), 
               stats=ms_o_stats)
write.xlsx(mylist, file=file.path("Analysis", "models_sato.xlsx"), rowNames=T)

# Export models
modlist <- list(ms_o=ms_o, ms_o_stats=ms_o_stats, dat=dat)
saveRDS(modlist, file=file.path("Analysis", "models_sato.rds"))

# Remove
rm(mylist, modlist, ms_o_stats, ms_o)
gc()

########################################
# Satisfaction with WRM for transportation facilities

# Limited-access highways (example: I-15)
ms_t1 <- polr(SAT_TRANSPORT_1 ~ INCHES_4CAT + AverageIndex + 
                AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
                WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
                ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
              data=dat, method="logistic")
summary(ms_t1)
coeftest(ms_t1)

# Major roads with more traffic (example: US-89)
ms_t2 <- polr(SAT_TRANSPORT_2 ~ INCHES_4CAT + AverageIndex + 
                AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
                WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
                ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
              data=dat, method="logistic")
summary(ms_t2)
coeftest(ms_t2)

# Local neighborhood streets
ms_t3 <- polr(SAT_TRANSPORT_3 ~ INCHES_4CAT + AverageIndex + 
                AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
                WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
                ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
              data=dat, method="logistic")
summary(ms_t3)
coeftest(ms_t3)

# Sidewalks and pedestrian crossings
ms_t4 <- polr(SAT_TRANSPORT_4 ~ INCHES_4CAT + AverageIndex + 
                AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
                WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
                ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
              data=dat, method="logistic")
summary(ms_t4)
coeftest(ms_t4)

# Bus stops and train stations
ms_t5 <- polr(SAT_TRANSPORT_5 ~ INCHES_4CAT + AverageIndex + 
                AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
                WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
                ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
              data=dat, method="logistic")
summary(ms_t5)
coeftest(ms_t5)

# Bike lanes and trails
ms_t6 <- polr(SAT_TRANSPORT_6 ~ INCHES_4CAT + AverageIndex + 
                AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
                WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
                ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
              data=dat, method="logistic")
summary(ms_t6)
coeftest(ms_t6)

# Extract model statistics
ms_t_stats <- data.frame(t1=extract_model_info(ms_t1))
ms_t_stats <- cbind(ms_t_stats, t2=extract_model_info(ms_t2))
ms_t_stats <- cbind(ms_t_stats, t3=extract_model_info(ms_t3))
ms_t_stats <- cbind(ms_t_stats, t4=extract_model_info(ms_t4))
ms_t_stats <- cbind(ms_t_stats, t5=extract_model_info(ms_t5))
ms_t_stats <- cbind(ms_t_stats, t6=extract_model_info(ms_t6))
row.names(ms_t_stats) <- c("n", "ll_n", "ll_m", "pr2", "aic", "bic")

# Export to Excel worksheet
mylist <- list(t1=as.data.frame(coeftest(ms_t1)[,]), 
               t2=as.data.frame(coeftest(ms_t2)[,]), 
               t3=as.data.frame(coeftest(ms_t3)[,]), 
               t4=as.data.frame(coeftest(ms_t4)[,]), 
               t5=as.data.frame(coeftest(ms_t5)[,]), 
               t6=as.data.frame(coeftest(ms_t6)[,]), 
               stats=ms_t_stats)
write.xlsx(mylist, file=file.path("Analysis", "models_satt.xlsx"), rowNames=T)

# Export models
modlist <- list(ms_t1=ms_t1, ms_t2=ms_t2, ms_t3=ms_t3, 
                ms_t4=ms_t4, ms_t5=ms_t5, ms_t6=ms_t6, 
                ms_t_stats=ms_t_stats, dat=dat)
saveRDS(modlist, file=file.path("Analysis", "models_satt.rds"))

# Remove
rm(mylist, modlist, ms_t_stats)
rm(ms_t1, ms_t2, ms_t3, ms_t4, ms_t5, ms_t6)
gc()

########################################
# Satisfaction with WRM for groups

# A building owner or property manager
ms_g1 <- polr(SAT_GROUP_1 ~ INCHES_4CAT + AverageIndex + 
                AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
                WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
                ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
              data=dat, method="logistic")
summary(ms_g1)
coeftest(ms_g1)

# Local businesses
ms_g2 <- polr(SAT_GROUP_2 ~ INCHES_4CAT + AverageIndex + 
                AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
                WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
                ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
              data=dat, method="logistic")
summary(ms_g2)
coeftest(ms_g2)

# City or county government
ms_g3 <- polr(SAT_GROUP_3 ~ INCHES_4CAT + AverageIndex + 
                AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
                WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
                ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
              data=dat, method="logistic")
summary(ms_g3)
coeftest(ms_g3)

# State government (UDOT)
ms_g4 <- polr(SAT_GROUP_4 ~ INCHES_4CAT + AverageIndex + 
                AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
                WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
                ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
              data=dat, method="logistic")
summary(ms_g4)
coeftest(ms_g4)

# Extract model statistics
ms_g_stats <- data.frame(g1=extract_model_info(ms_g1))
ms_g_stats <- cbind(ms_g_stats, g2=extract_model_info(ms_g2))
ms_g_stats <- cbind(ms_g_stats, g3=extract_model_info(ms_g3))
ms_g_stats <- cbind(ms_g_stats, g4=extract_model_info(ms_g4))
row.names(ms_g_stats) <- c("n", "ll_n", "ll_m", "pr2", "aic", "bic")

# Export to Excel worksheet
mylist <- list(g1=as.data.frame(coeftest(ms_g1)[,]), 
               g2=as.data.frame(coeftest(ms_g2)[,]), 
               g3=as.data.frame(coeftest(ms_g3)[,]), 
               g4=as.data.frame(coeftest(ms_g4)[,]), 
               stats=ms_g_stats)
write.xlsx(mylist, file=file.path("Analysis", "models_satg.xlsx"), rowNames=T)

# Export models
modlist <- list(ms_g1=ms_g1, ms_g2=ms_g2, ms_g3=ms_g3, ms_g4=ms_g4, 
                ms_g_stats=ms_g_stats, dat=dat)
saveRDS(modlist, file=file.path("Analysis", "models_satg.rds"))

# Remove
rm(mylist, modlist, ms_g_stats)
rm(ms_g1, ms_g2, ms_g3, ms_g4)
gc()

########################################
# Travel behavior - Didn't travel

# Did you travel (leave your home) for any reason during or soon after this recent winter storm?
mt_t <- glm(I(TRAVEL=="No") ~ INCHES_4CAT + AverageIndex + 
              AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
              WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
              ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
            data=dat, family=binomial(link="logit"))
summary(mt_t)
coeftest(mt_t)

# Extract model statistics
mt_t_stats <- data.frame(t=extract_model_info(mt_t))
row.names(mt_t_stats) <- c("n", "ll_n", "ll_m", "pr2", "aic", "bic")

# Export to Excel worksheet
mylist <- list(t=as.data.frame(coeftest(mt_t)[,]), 
               stats=mt_t_stats)
write.xlsx(mylist, file=file.path("Analysis", "models_beht.xlsx"), rowNames=T)

# Export models
modlist <- list(mt_t=mt_t, mt_t_stats=mt_t_stats, dat=dat)
saveRDS(modlist, file=file.path("Analysis", "models_beht.rds"))

# Remove
rm(mylist, modlist, mt_t_stats, mt_t)
gc()

########################################
# Travel behavior - Use of transportation modes

# Create binary outcomes for transportation modes
# active & public transit: 
# (1) Walk, (2) Bicycle, 
# (3) Skateboard, scooter, wheelchair, or other small device
# (7) Public bus or train (examples: UTA, TRAX, FrontRunner)
# (8) School bus
dat$MODE_APT <- ifelse(dat$MODE_1==T | dat$MODE_2==T | dat$MODE_3==T | 
                       dat$MODE_7==T | dat$MODE_8==T, T, F)
# car driver: (4) Car/Van/Truck/SUV Driver
dat$MODE_CDR <- ifelse(dat$MODE_4==T, T, F)
# car passenger: (5) Car/Van/Truck/SUV Passenger
dat$MODE_CPA <- ifelse(dat$MODE_5==T, T, F)
# inspect
summary(dat$MODE_APT)
summary(dat$MODE_CDR)
summary(dat$MODE_CPA)

# Active & public transit
mt_m1 <- glm(MODE_APT ~ INCHES_4CAT + AverageIndex + 
               AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
               WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
               ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
             data=dat, family=binomial(link="logit"))
summary(mt_m1)
coeftest(mt_m1)

# Car driver
mt_m2 <- glm(MODE_CDR ~ INCHES_4CAT + AverageIndex + 
               AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
               WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
               ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
             data=dat, family=binomial(link="logit"))
summary(mt_m2)
coeftest(mt_m2)

# Car passenger
mt_m3 <- glm(MODE_CPA ~ INCHES_4CAT + AverageIndex + 
               AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
               WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
               ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
             data=dat, family=binomial(link="logit"))
summary(mt_m3)
coeftest(mt_m3)

# Extract model statistics
mt_m_stats <- data.frame(m1=extract_model_info(mt_m1))
mt_m_stats <- cbind(mt_m_stats, m2=extract_model_info(mt_m2))
mt_m_stats <- cbind(mt_m_stats, m3=extract_model_info(mt_m3))
row.names(mt_m_stats) <- c("n", "ll_n", "ll_m", "pr2", "aic", "bic")

# Export to Excel worksheet
mylist <- list(m1=as.data.frame(coeftest(mt_m1)[,]), 
               m2=as.data.frame(coeftest(mt_m2)[,]), 
               m3=as.data.frame(coeftest(mt_m3)[,]), 
               stats=mt_m_stats)
write.xlsx(mylist, file=file.path("Analysis", "models_behm.xlsx"), rowNames=T)

# Export models
modlist <- list(mt_m1=mt_m1, mt_m2=mt_m2, mt_m3=mt_m3, 
                mt_m_stats=mt_m_stats, dat=dat)
saveRDS(modlist, file=file.path("Analysis", "models_behm.rds"))

# Remove
rm(mylist, modlist, mt_m_stats)
rm(mt_m1, mt_m2, mt_m3)
gc()

########################################
# Travel behavior - Purpose for traveling

# Create binary outcomes for travel purpose
# mandatory activities: 
# (1) Work
# (2) Attend school
# (3) Work- or school-related activities
dat$PURPOSE_M <- ifelse(dat$PURPOSE_1==T | dat$PURPOSE_2==T | 
                        dat$PURPOSE_3==T, T, F)
# semi-mandatory / semi-discretionary activities: 
# (5) Service private vehicle (gas, oil, repairs, etc.)
# (7) Drop off or pick up passenger(s)
# (8) Civic or religious activities
# (9) Other errands or appointments (bank, professional office, doctor/dentist, etc.)
dat$PURPOSE_S <- ifelse(dat$PURPOSE_5==T | dat$PURPOSE_7==T | 
                        dat$PURPOSE_8==T | dat$PURPOSE_9==T, T, F)
# discretionary activities: 
# (4) Eat meal at restaurant
# (6) Shopping (groceries, clothing, convenience store, etc)
# (10) Outdoor or indoor exercise (sports, jogging, bicycling, walking dog, gym, etc.)
# (11) Social or entertainment activities (friends/relatives, movie, etc.)
dat$PURPOSE_D <- ifelse(dat$PURPOSE_4==T | dat$PURPOSE_6==T | 
                        dat$PURPOSE_10==T | dat$PURPOSE_11==T, T, F)
# inspect
summary(dat$PURPOSE_M)
summary(dat$PURPOSE_S)
summary(dat$PURPOSE_D)

# Mandatory activities
mt_p1 <- glm(PURPOSE_M ~ INCHES_4CAT + AverageIndex + 
               AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
               WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
               ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
             data=dat, family=binomial(link="logit"))
summary(mt_p1)
coeftest(mt_p1)

# Semi- activities
mt_p2 <- glm(PURPOSE_S ~ INCHES_4CAT + AverageIndex + 
               AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
               WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
               ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
             data=dat, family=binomial(link="logit"))
summary(mt_p2)
coeftest(mt_p2)

# Discretionary activities
mt_p3 <- glm(PURPOSE_D ~ INCHES_4CAT + AverageIndex + 
               AGE_4CAT + GENDER_2CAT + RACE_2CAT + STUD + EDUC_3CAT + 
               WORK + HHINC_6CAT + ZONE + HOME1_2CAT + HOME2 + 
               ADULTS_NUM + CHILDREN_NUM + BIKES_NUM + CARS_NUM, 
             data=dat, family=binomial(link="logit"))
summary(mt_p3)
coeftest(mt_p3)

# Extract model statistics
mt_p_stats <- data.frame(p1=extract_model_info(mt_p1))
mt_p_stats <- cbind(mt_p_stats, p2=extract_model_info(mt_p2))
mt_p_stats <- cbind(mt_p_stats, p3=extract_model_info(mt_p3))
row.names(mt_p_stats) <- c("n", "ll_n", "ll_m", "pr2", "aic", "bic")

# Export to Excel worksheet
mylist <- list(p1=as.data.frame(coeftest(mt_p1)[,]), 
               p2=as.data.frame(coeftest(mt_p2)[,]), 
               p3=as.data.frame(coeftest(mt_p3)[,]), 
               stats=mt_p_stats)
write.xlsx(mylist, file=file.path("Analysis", "models_behp.xlsx"), rowNames=T)

# Export models
modlist <- list(mt_p1=mt_p1, mt_p2=mt_p2, mt_p3=mt_p3, 
                mt_p_stats=mt_p_stats, dat=dat)
saveRDS(modlist, file=file.path("Analysis", "models_behp.rds"))

# Remove
rm(mylist, modlist, mt_p_stats)
rm(mt_p1, mt_p2, mt_p3)
gc()

########################################
# Cleanup

# Remove
rm(extract_model_info)
rm(dat, xdat)
gc()

########################################
# END
########################################