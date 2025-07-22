########################################
# Project:  UDOT-23.206 Snow PMs
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
#           Shailendra Khanal (s.khanal@usu.edu)
# File:     storm_data.R
# About:    Loads and processes storm data
########################################
# Notes

# Open R project first, then open this R script

# 2024-08-23 SK created
# 2024-09-09 SK updated
# 2024-09-27 PS updated
# 2024-10-04 SK updated
# 2024-11-17 SK updated
# 2025-04-01 SK updated
# 2025-06-21 PS updated

# Load packages
library(dplyr)
library(ggplot2)
library(mapview)
library(sf)
library(tigris)
library(stringdist)

########################################
# Load data

# RWIS storm data: Feb 8-9, Zones A/F
fpath <- file.path("Data", "Storm Data", "PerformanceGradeExport_Feb_8_9.csv")
feb_storm_data <- read.csv(fpath, stringsAsFactors=F); rm(fpath)
# - inspect
str(feb_storm_data)
feb_storm_data <- feb_storm_data[feb_storm_data$RwisLocationIntId!="Grade of * means insufficient data for calculation",]
feb_storm_data$EventStartSampleTime <- as.POSIXct(feb_storm_data$EventStartSampleTime, format="%m/%d/%Y %I:%M:%S %p", tz="America/Denver")
feb_storm_data$EventEndSampleTime <- as.POSIXct(feb_storm_data$EventEndSampleTime, format="%m/%d/%Y %I:%M:%S %p", tz="America/Denver")

# RWIS storm data: Mar 2-3, Zones B/C/D/E/F
fpath <- file.path("Data", "Storm Data", "PerformanceGradeExport_Mar_2_3.csv")
mar_storm_data <- read.csv(fpath, stringsAsFactors=F); rm(fpath)
# - inspect
str(mar_storm_data)
mar_storm_data <- mar_storm_data[mar_storm_data$RwisLocationIntId!="Grade of * means insufficient data for calculation",]
mar_storm_data$EventStartSampleTime <- as.POSIXct(mar_storm_data$EventStartSampleTime, format="%m/%d/%Y %I:%M:%S %p", tz="America/Denver")
mar_storm_data$EventEndSampleTime <- as.POSIXct(mar_storm_data$EventEndSampleTime, format="%m/%d/%Y %I:%M:%S %p", tz="America/Denver")

# RWIS locations
fpath <- file.path("Data", "Storm Data", "UDOT RWIS - UPlan - Public - RWIS.csv")
rwis_locations_data <- read.csv(fpath, stringsAsFactors=F); rm(fpath)
# - inspect
str(rwis_locations_data)
unique(rwis_locations_data$SITE)
# - convert to spatial
rwis_sf <- st_as_sf(rwis_locations_data, coords=c("LONGITUDE","LATITUDE"), crs=4326)
# - inspect
str(rwis_sf)

# Read Utah ZIP code shapefile
zip_path <- "Data/Regions-ZIPs/Utah_ZIP_Code_Areas/ZipCodes.shp"
utah_zipcodes <- st_read(zip_path); rm(zip_path)
# - inspect
str(utah_zipcodes)

########################################
# Plot map

# Ensure geometries are valid
utah_zipcodes <- st_make_valid(utah_zipcodes)

# Get Utah state boundary
utah_state <- states(cb=T) %>% filter(NAME=="Utah")

# Plot all layers
ggplot() +
  geom_sf(data = utah_state, fill = "#e6f2ff", color = "black", size = 0.6) +
  geom_sf(data = utah_zipcodes, fill = NA, color = "gray", size = 0.2) +
  geom_sf(data = rwis_sf, shape = 3, color = "#1f78b4", size = 1)+
  labs(
    title = "RWIS Sensor Locations in Utah",
    subtitle = "ZIP Code Boundaries and State Border",
    caption = "Data: UDOT RWIS | Map by: Shailendra Khanal",
    x = "Longitude", y = "Latitude"
  ) +
  coord_sf(xlim = c(-114.1, -109), ylim = c(36.8, 42.3), expand = FALSE) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#f8f9fa", color = NA),
    panel.grid.major = element_line(color = "gray85"),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10, color = "gray40")
  )

# Save the map
ggsave(
  filename = file.path("Data", "Storm Data", "RWIS_UTAH_ZIP_Map.png"),
  plot = last_plot(),
  width = 6, height = 8, dpi = 1200
)

########################################
# Process storm data and link with RWIS locations

# Inspecting and removing RWIS sensor without having a road sensor
filtered_data <- rwis_locations_data[rwis_locations_data$`ROAD.SENSOR` == 'x', ]
rwis_locations_data <- rwis_locations_data[rwis_locations_data$`ROAD.SENSOR` != 'x', ]
rwis_sf <- rwis_sf[rwis_sf$`ROAD.SENSOR` != 'x', ]
rm(filtered_data)

# Renaming column name RWIS Location Name to SITE in merged_storm_data for easy merging
feb_storm_data <- feb_storm_data %>% rename(SITE = RwisLocatonName)
mar_storm_data <- mar_storm_data %>% rename(SITE = RwisLocatonName)
unique(feb_storm_data$SITE)
unique(mar_storm_data$SITE)

# Checking the SITE name in both datasets
common_sites_feb <- intersect(unique(feb_storm_data$SITE), unique(rwis_locations_data$SITE)); common_sites_feb
common_sites_mar <- intersect(unique(mar_storm_data$SITE), unique(rwis_locations_data$SITE)); common_sites_mar
rm(common_sites_feb, common_sites_mar)
unique(feb_storm_data$SITE)[!(unique(feb_storm_data$SITE) %in% unique(rwis_locations_data$SITE))]
unique(mar_storm_data$SITE)[!(unique(mar_storm_data$SITE) %in% unique(rwis_locations_data$SITE))]
unique(rwis_locations_data$SITE)[!(unique(rwis_locations_data$SITE) %in% unique(feb_storm_data$SITE))]
unique(rwis_locations_data$SITE)[!(unique(rwis_locations_data$SITE) %in% unique(mar_storm_data$SITE))]

# Ensuring all SITE names are in uppercase for consistency
feb_storm_data$SITE <- toupper(feb_storm_data$SITE)
mar_storm_data$SITE <- toupper(mar_storm_data$SITE)
rwis_locations_data$SITE <- toupper(rwis_locations_data$SITE)
rwis_sf$SITE <- toupper(rwis_sf$SITE)

# Trimming white spaces in both datasets
feb_storm_data$SITE <- trimws(feb_storm_data$SITE)
mar_storm_data$SITE <- trimws(mar_storm_data$SITE)
rwis_locations_data$SITE <- trimws(rwis_locations_data$SITE)
rwis_sf$SITE <- trimws(rwis_sf$SITE)

# View map
# mapview(rwis_sf)

# Calculate string distances between unmatched sites
unmatched_feb_storm_data <- unique(feb_storm_data$SITE)[!(unique(feb_storm_data$SITE) %in% unique(rwis_locations_data$SITE))]; unmatched_feb_storm_data
unmatched_mar_storm_data <- unique(mar_storm_data$SITE)[!(unique(mar_storm_data$SITE) %in% unique(rwis_locations_data$SITE))]; unmatched_mar_storm_data
unmatched_feb_rwis_locations <- unique(rwis_locations_data$SITE)[!(unique(rwis_locations_data$SITE) %in% unique(feb_storm_data$SITE))]; unmatched_feb_rwis_locations
unmatched_mar_rwis_locations <- unique(rwis_locations_data$SITE)[!(unique(rwis_locations_data$SITE) %in% unique(mar_storm_data$SITE))]; unmatched_mar_rwis_locations

# Find the closest match for each unmatched SITE from data3
for (site in unmatched_feb_storm_data) {
  distances <- stringdist::stringdist(site, unmatched_feb_rwis_locations, method = "jw")
  closest_match <- unmatched_feb_rwis_locations[which.min(distances)]
  print(paste("Closest match for", site, "is", closest_match))
}; rm(site, distances, closest_match)
for (site in unmatched_mar_storm_data) {
  distances <- stringdist::stringdist(site, unmatched_mar_rwis_locations, method = "jw")
  closest_match <- unmatched_mar_rwis_locations[which.min(distances)]
  print(paste("Closest match for", site, "is", closest_match))
}; rm(site, distances, closest_match)

# Remove
rm(unmatched_feb_rwis_locations, unmatched_mar_rwis_locations, unmatched_feb_storm_data, unmatched_mar_storm_data)

# Replacement names for feb_storm_data
replacements_feb <- c(
  "FOOTHILL DR. @ MARIO CAPECCHI DR." = "SR-186 FOOTHILL DR. @ MARIO CAPECCHI DR.",
  "I-15 @ MP 252.5 BEER CREEK" = "I-15 @ BEER CREEK",
  "I-215 @ MP 3 OLYMPUS COVE" = "I-215 @ OLYMPUS COVE",
  "I-70 @ MP 226 HARLEY DOME" = "I-70 @ HARLEY DOME",
  "I-70 @ MP 69 SALINA CANYON" = "I-70 @ SALINA CANYON",
  "I-80 @ MP 128 MOUTH OF PARLEYS" = "I-80 @ MOUTH OF PARLEYS",
  "I-80 @ MP 132 MT. AIRE EB" = "I-80 @ MP 132 MT. AIRE",
  "I-80 @ MP 132 MT. AIRE WB" = "I-80 @ MP 132 MT. AIRE",
  "I-80 @ MP 134 EAST CANYON EB" = "I-80 @ MP 134 EAST CANYON",
  "I-80 @ MP 136 LAMBS CANYON EB" = "I-80 @ MP 136 LAMBS CANYON",
  "I-80 @ MP 136 LAMBS CANYON WB" = "I-80 @ MP 136 LAMBS CANYON",
  "I-80 @ MP 139.5 PARLEYS LANE BRIDGE WB" = "I-80 @ PARLEYS SUMMIT",
  "I-80 @ MP 155.5 WANSHIP" = "I-80 @ WANSHIP",
  "I-80 @ PARLEYS CANYON QUARRY EB" = "I-80 @ PARLEYS CANYON QUARRY",
  "I-80 @ PARLEYS CANYON QUARRY WB" = "I-80 @ PARLEYS CANYON QUARRY",
  "I-80 @ WAHSATCH" = "I-80 @ WAHSATCH HILL",
  "I-84 @ MP 108 TAGGARTS EB" = "I-84 @ MP 108 TAGGARTS",
  "I-84 @ MP 108 TAGGARTS WB" = "I-84 @ MP 108 TAGGARTS",
  "I-84 @ MP 14 CHAULK HILL" = "I-84 @ CHAULK HILL",
  "I-84 @ MP 30.5 WHITES VALLEY" = "I-84 @ WHITES VALLEY",
  "SR-154 @ SR-71" = "SR-154 @ MP 7 RIVERTON",
  "SR-201 @ SR-85" = "SR-85 @ SR-201",
  "SR-71 @ SR-154" = "SR-154 @ MP 7 RIVERTON"
)

# Replacement names for mar_storm_data
replacements_mar <- c(
  "FOOTHILL DR. @ MARIO CAPECCHI DR." = "SR-186 FOOTHILL DR. @ MARIO CAPECCHI DR.",
  "I-15 @ MP 125.5 PINE CREEK HILL" = "I-15 @ PINE CREEK HILL",
  "I-15 @ MP 140 BAKER CANYON NB" = "I-15 @ MP 140 BAKER CANYON",
  "I-15 @ MP 140 BAKER CANYON SB" = "I-15 @ MP 140 BAKER CANYON",
  "I-15 @ MP 252.5 BEER CREEK" = "I-15 @ BEER CREEK",
  "I-215 @ MP 3 OLYMPUS COVE" = "I-215 @ OLYMPUS COVE",
  "I-70 @ MP 11 FISH CREEK" = "I-70 @ FISH CREEK",  
  "I-70 @ MP 137 RATTLESNAKE BENCH" = "I-70 @ RATTLESNAKE BENCH",
  "I-70 @ MP 226 HARLEY DOME" = "I-70 @ HARLEY DOME",
  "I-70 @ MP 69 SALINA CANYON" = "I-70 @ SALINA CANYON",
  "I-70 @ MP 93 FREMONT JCT" = "I-70 @ FREMONT JCT",
  "I-80 @ MP 128 MOUTH OF PARLEYS" = "I-80 @ MOUTH OF PARLEYS",
  "I-80 @ MP 134 EAST CANYON EB" = "I-80 @ MP 134 EAST CANYON",
  "I-80 @ MP 136 LAMBS CANYON EB" = "I-80 @ MP 136 LAMBS CANYON",
  "I-80 @ MP 136 LAMBS CANYON WB" = "I-80 @ MP 136 LAMBS CANYON",
  "I-80 @ MP 139.5 PARLEYS LANE BRIDGE WB" = "I-80 @ PARLEYS SUMMIT",
  "I-80 @ MP 155.5 WANSHIP" = "I-80 @ WANSHIP",
  "I-80 @ MP 163 COALVILLE" = "I-80 @ COALVILLE",
  "I-80 @ PARLEYS CANYON QUARRY EB" = "I-80 @ PARLEYS CANYON QUARRY",
  "I-80 @ WAHSATCH" = "I-80 @ WAHSATCH HILL",
  "I-84 @ MP 108 TAGGARTS EB" = "I-84 @ MP 108 TAGGARTS",
  "I-84 @ MP 108 TAGGARTS WB" = "I-84 @ MP 108 TAGGARTS",
  "I-84 @ MP 14 CHAULK HILL" = "I-84 @ CHAULK HILL",
  "I-84 @ MP 30.5 WHITES VALLEY" = "I-84 @ WHITES VALLEY",
  "SR-154 @ SR-71" = "SR-154 @ MP 7 RIVERTON",
  "SR-199 @ MP 12 JOHNSONS PASS" = "SR-199 @ JOHNSON'S PASS",
  "SR-201 @ SR-85" = "SR-85 @ SR-201",
  "SR-36 @ LAKEPOINT" = "I-80 @ LAKEPOINT",
  "SR-71 @ SR-154" = "SR-154 @ MP 7 RIVERTON",
  "SR-95 @ MP 97 SALVATION KNOLL" = "SR-95 @ SALVATION KNOLL",
  "US-191 @ MP 62 MONTICELLO" = "US-191 @ MONTICELLO",
  "US-40 @ MP 81.5 STARVATION" = "US-40 @ STARVATION RESERVOIR"
)

# Replace the SITE names in merged__storm_data
feb_storm_data$SITE <- ifelse(feb_storm_data$SITE %in% names(replacements_feb), replacements_feb[feb_storm_data$SITE], feb_storm_data$SITE)
mar_storm_data$SITE <- ifelse(mar_storm_data$SITE %in% names(replacements_mar), replacements_mar[mar_storm_data$SITE], mar_storm_data$SITE)
rm(replacements_feb, replacements_mar)

# Checking common/unmatched sites after merging
common_sites_feb <- intersect(unique(feb_storm_data$SITE), unique(rwis_locations_data$SITE)); common_sites_feb
common_sites_mar <- intersect(unique(mar_storm_data$SITE), unique(rwis_locations_data$SITE)); common_sites_mar
unmatched_sites_feb <- unique(feb_storm_data$SITE)[!(unique(feb_storm_data$SITE) %in% unique(rwis_locations_data$SITE))]; unmatched_sites_feb
unmatched_sites_mar <- unique(mar_storm_data$SITE)[!(unique(mar_storm_data$SITE) %in% unique(rwis_locations_data$SITE))]; unmatched_sites_mar
# - none, great!
unmatched_rwis_feb <- unique(rwis_locations_data$SITE)[!(unique(rwis_locations_data$SITE) %in% unique(feb_storm_data$SITE))]; unmatched_rwis_feb
unmatched_rwis_mar <- unique(rwis_locations_data$SITE)[!(unique(rwis_locations_data$SITE) %in% unique(mar_storm_data$SITE))]; unmatched_rwis_mar
rm(common_sites_feb, common_sites_mar, unmatched_sites_feb, unmatched_sites_mar, unmatched_rwis_feb, unmatched_rwis_mar)

# Merging the two datasets based on the 'SITE' column and assigning NA to rows where there is no value in certain columns
feb_combined_data <- merge(feb_storm_data, rwis_locations_data, by = "SITE", all = TRUE)
mar_combined_data <- merge(mar_storm_data, rwis_locations_data, by = "SITE", all = TRUE)

# Inspect
str(feb_combined_data)
str(mar_combined_data)

# Select relevant columns
tnames <- c("SITE", "EventStartSampleTime", "EventEndSampleTime", 
            "AverageIndex", "StormEventIndex", "StormEventDurationHours", "exceptional", "acceptable", "unacceptable", "resourceGrade", 
            "LATITUDE", "LONGITUDE", "COUNTY")
feb_combined_data <- feb_combined_data[, tnames]
mar_combined_data <- mar_combined_data[, tnames]
rm(tnames)

# Convert missing into default values
# - see "Storm data parameters.docx" document for details and justifications
# - feb_combined_data
feb_combined_data$AverageIndex[is.na(feb_combined_data$AverageIndex)] <- 0
feb_combined_data$StormEventIndex[is.na(feb_combined_data$StormEventIndex)] <- 0
feb_combined_data$StormEventDurationHours[is.na(feb_combined_data$StormEventDurationHours)] <- 0
feb_combined_data$exceptional[is.na(feb_combined_data$exceptional)] <- 100
feb_combined_data$acceptable[is.na(feb_combined_data$acceptable)] <- 0
feb_combined_data$unacceptable[is.na(feb_combined_data$unacceptable)] <- 0
feb_combined_data$exceptional[feb_combined_data$resourceGrade=="*"] <- NA
feb_combined_data$acceptable[feb_combined_data$resourceGrade=="*"] <- NA
feb_combined_data$unacceptable[feb_combined_data$resourceGrade=="*"] <- NA
# - mar_combined_data
mar_combined_data$AverageIndex[is.na(mar_combined_data$AverageIndex)] <- 0
mar_combined_data$StormEventIndex[is.na(mar_combined_data$StormEventIndex)] <- 0
mar_combined_data$StormEventDurationHours[is.na(mar_combined_data$StormEventDurationHours)] <- 0
mar_combined_data$exceptional[is.na(mar_combined_data$exceptional)] <- 100
mar_combined_data$acceptable[is.na(mar_combined_data$acceptable)] <- 0
mar_combined_data$unacceptable[is.na(mar_combined_data$unacceptable)] <- 0
mar_combined_data$exceptional[mar_combined_data$resourceGrade=="*"] <- NA
mar_combined_data$acceptable[mar_combined_data$resourceGrade=="*"] <- NA
mar_combined_data$unacceptable[mar_combined_data$resourceGrade=="*"] <- NA

# Deal with duplicates: feb_combined_data
ttab <- table(feb_combined_data$SITE)
View(feb_combined_data[feb_combined_data$SITE %in% names(ttab[ttab>=2]),])
rm(ttab)
# - duplicates with missing data (remove)
tsites <- c("I-80 @ MP 136 LAMBS CANYON", "I-80 @ PARLEYS CANYON QUARRY", "SR-154 @ MP 7 RIVERTON")
feb_combined_data <- feb_combined_data[!(feb_combined_data$SITE %in% tsites & feb_combined_data$resourceGrade=="*"),]
rm(tsites)
# - duplicates with overlaps (keep longer duration)
tsites <- c("I-80 @ MP 132 MT. AIRE", "I-84 @ MP 108 TAGGARTS", "SR-85 @ SR-201")
for (i in 1:length(tsites)) {
  tw <- which(feb_combined_data$SITE %in% tsites[i])
  temp <- feb_combined_data[tw,]
  trem <- which(temp$StormEventDurationHours!=max(temp$StormEventDurationHours))
  feb_combined_data <- feb_combined_data[-tw[trem],]
  rm(tw, temp, trem)
}; rm(i)
# - aggregate multiple storm events at same location (sum duration, time-weighted-average others)
tsites <- c("I-15 @ PLYMOUTH", "SR-112 @ MP 6 UTAH AVENUE", "US-189 @ CHARLESTON")
for (i in 1:length(tsites)) {
  temp <- feb_combined_data[feb_combined_data$SITE %in% tsites[i],]
  tlist <- list(tsites[i], min(temp$EventStartSampleTime), max(temp$EventEndSampleTime), 
                NA, NA, sum(temp$StormEventDurationHours), 
                NA, NA, NA, NA, temp$LATITUDE[1], temp$LONGITUDE[1], temp$COUNTY[1])
  tlist[[4]] <- sum(temp$AverageIndex*temp$StormEventDurationHours)/tlist[[6]]
  tlist[[5]] <- tlist[[4]]*tlist[[6]]
  tlist[[7]] <- sum(temp$exceptional*temp$StormEventDurationHours)/tlist[[6]]
  tlist[[8]] <- sum(temp$acceptable*temp$StormEventDurationHours)/tlist[[6]]
  tlist[[9]] <- sum(temp$unacceptable*temp$StormEventDurationHours)/tlist[[6]]
  feb_combined_data <- feb_combined_data[!(feb_combined_data$SITE==tsites[i]),]
  feb_combined_data <- rbind(feb_combined_data, tlist)
  rm(temp, tlist)
}; rm(i)
rm(tsites)
# - check
ttab <- table(feb_combined_data$SITE)
View(feb_combined_data[feb_combined_data$SITE %in% names(ttab[ttab>=2]),])
rm(ttab)

# Deal with duplicates: mar_combined_data
ttab <- table(mar_combined_data$SITE)
View(mar_combined_data[mar_combined_data$SITE %in% names(ttab[ttab>=2]),])
rm(ttab)
# - duplicates with missing data (remove)
tsites <- c("I-15 @ MP 140 BAKER CANYON", "I-80 @ LAKEPOINT", "SR-154 @ MP 7 RIVERTON", "SR-85 @ SR-201")
mar_combined_data <- mar_combined_data[!(mar_combined_data$SITE %in% tsites & mar_combined_data$resourceGrade=="*"),]
rm(tsites)
# - duplicates with overlaps (keep longer duration)
mar_combined_data <- mar_combined_data[!(mar_combined_data$SITE=="I-80 @ LAKEPOINT" & mar_combined_data$EventEndSampleTime=="2024-03-03 08:30:00"),]
mar_combined_data <- mar_combined_data[!(mar_combined_data$SITE=="I-84 @ MP 108 TAGGARTS" & mar_combined_data$StormEventIndex=="7.39"),]
tsites <- c("I-80 @ MP 136 LAMBS CANYON")
for (i in 1:length(tsites)) {
  tw <- which(mar_combined_data$SITE %in% tsites[i])
  temp <- mar_combined_data[tw,]
  trem <- which(temp$StormEventDurationHours!=max(temp$StormEventDurationHours))
  mar_combined_data <- mar_combined_data[-tw[trem],]
  rm(tw, temp, trem)
}; rm(i)
# - aggregate multiple storm events at same location (sum duration, time-weighted-average others)
tsites <- c("I-15 @ MP 350 FARR WEST", "I-80 @ GRASSY", "I-80 @ LAKEPOINT", "I-84 @ CHAULK HILL", 
            "SR-177 @ MP 14.5 SYRACUSE", "SR-201 @ I-80", "US-91 @ MP 6 MANTUA")
for (i in 1:length(tsites)) {
  temp <- mar_combined_data[mar_combined_data$SITE %in% tsites[i],]
  tlist <- list(tsites[i], min(temp$EventStartSampleTime), max(temp$EventEndSampleTime), 
                NA, NA, sum(temp$StormEventDurationHours), 
                NA, NA, NA, NA, temp$LATITUDE[1], temp$LONGITUDE[1], temp$COUNTY[1])
  tlist[[4]] <- sum(temp$AverageIndex*temp$StormEventDurationHours)/tlist[[6]]
  tlist[[5]] <- tlist[[4]]*tlist[[6]]
  tlist[[7]] <- sum(temp$exceptional*temp$StormEventDurationHours)/tlist[[6]]
  tlist[[8]] <- sum(temp$acceptable*temp$StormEventDurationHours)/tlist[[6]]
  tlist[[9]] <- sum(temp$unacceptable*temp$StormEventDurationHours)/tlist[[6]]
  mar_combined_data <- mar_combined_data[!(mar_combined_data$SITE==tsites[i]),]
  mar_combined_data <- rbind(mar_combined_data, tlist)
  rm(temp, tlist)
}; rm(i)
rm(tsites)
# - check
ttab <- table(mar_combined_data$SITE)
View(mar_combined_data[mar_combined_data$SITE %in% names(ttab[ttab>=2]),])
rm(ttab)

# Order by SITE again
feb_combined_data <- feb_combined_data[order(feb_combined_data$SITE),]
row.names(feb_combined_data) <- NULL
mar_combined_data <- mar_combined_data[order(mar_combined_data$SITE),]
row.names(mar_combined_data) <- NULL

# Remove column
feb_combined_data$resourceGrade <- NULL
mar_combined_data$resourceGrade <- NULL

# Inspect
summary(feb_combined_data)
summary(mar_combined_data)

########################################
# Link storm data (at RWIS stations) to zip codes

# Create 2-mile (3218.69 meters) buffer around each zipcode
buffered_zipcodes <- st_buffer(utah_zipcodes, dist=3218.69)

# View maps
mapview(utah_zipcodes, alpha.regions=0.3) + mapview(rwis_sf, col.regions="orange")
mapview(buffered_zipcodes, alpha.regions=0.3) + mapview(rwis_sf, col.regions="orange")

# Spatial join
rwis_zipcodes <- st_join(rwis_sf[,c("SITE")], buffered_zipcodes[,c("ZIP5")], join=st_within)

# Aggregate 
# - feb_combined_data
temp <- merge(rwis_zipcodes, feb_combined_data, by="SITE", all.x=T, all.y=F)
temp <- st_drop_geometry(temp)
feb_agg_data <- aggregate(cbind(AverageIndex, StormEventIndex, StormEventDurationHours, exceptional, acceptable, unacceptable) ~ ZIP5, data=temp, FUN=mean, na.rm=T)
rm(temp)
# - mar_combined_data
temp <- merge(rwis_zipcodes, mar_combined_data, by="SITE", all.x=T, all.y=F)
temp <- st_drop_geometry(temp)
mar_agg_data <- aggregate(cbind(AverageIndex, StormEventIndex, StormEventDurationHours, exceptional, acceptable, unacceptable) ~ ZIP5, data=temp, FUN=mean, na.rm=T)
rm(temp)

# Inspect
summary(feb_agg_data)
summary(mar_agg_data)

########################################
# Save files

# Save
# - feb
saveRDS(feb_combined_data, file.path("Data", "Storm Data", "feb_storm_data_rwis.rds"))
write.csv(feb_combined_data, file.path("Data", "Storm Data", "feb_storm_data_rwis.csv"), row.names=F)
saveRDS(feb_agg_data, file.path("Data", "Storm Data", "feb_storm_data_zip.rds"))
write.csv(feb_agg_data, file.path("Data", "Storm Data", "feb_storm_data_zip.csv"), row.names=F)
# - mar
saveRDS(mar_combined_data, file.path("Data", "Storm Data", "mar_storm_data_rwis.rds"))
write.csv(mar_combined_data, file.path("Data", "Storm Data", "mar_storm_data_rwis.csv"), row.names=F)
saveRDS(mar_agg_data, file.path("Data", "Storm Data", "mar_storm_data_zip.rds"))
write.csv(mar_agg_data, file.path("Data", "Storm Data", "mar_storm_data_zip.csv"), row.names=F)

########################################
# Cleanup

# Remove
rm(feb_storm_data, feb_combined_data, feb_agg_data)
rm(mar_storm_data, mar_combined_data, mar_agg_data)
rm(utah_zipcodes, buffered_zipcodes, utah_state)
rm(rwis_locations_data, rwis_sf, rwis_zipcodes)
gc()

########################################
# END
########################################