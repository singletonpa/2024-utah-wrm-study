# script.R
# get ZIP codes for regions
# Patrick Singleton
# 2024-01-26

# load packages
library(sf)
library(mapview)

# load shapefiles
counties <- read_sf(file.path("Data", "Regions-ZIPs", "Utah_County_Boundaries"))
zipcodes <- read_sf(file.path("Data", "Regions-ZIPs", "Utah_ZIP_Code_Areas"))
nwszones <- read_sf(file.path("Data", "Regions-ZIPs", "Utah_NWS_Forecast_Zones"))

# view
mapview(counties, zcol="COUNTYNBR")
mapview(zipcodes, zocl="ZIP5")
mapview(nwszones, zcol="ZONE")
mapview(zipcodes, zcol="ZIP5") + mapview(counties, alpha.regions=0, lwd=3)
mapview(zipcodes, zcol="ZIP5") + mapview(nwszones, alpha.regions=0, lwd=3)
mapview(nwszones, zcol="ZONE") + mapview(counties, alpha.regions=0, lwd=3)
mapview(counties, zcol="COUNTYNBR") + mapview(nwszones, alpha.regions=0, lwd=3)

# load winter storm events
storms <- read.csv(file.path("Data", "Regions-ZIPs", "NOAA NCEI Storm Events", "storm_data_search_results.csv"))
storms$BEGIN_DATE <- as.Date(storms$BEGIN_DATE, format="%m/%d/%Y", tz="America/Denver")
storms <- storms[storms$EVENT_TYPE %in% c("Heavy Snow", "Winter Storm", "Winter Weather"),]
storms <- storms[storms$BEGIN_DATE >= "2022-10-01" & storms$BEGIN_DATE <= "2023-09-30",]

# inspect
table(storms$CZ_NAME_STR)
# less than 4: 
# - BRYCE CANYON COUNTRY (ZONE), 
# - CANYONLANDS / NATURAL BRIDGES (ZONE), 
# - CASTLE COUNTRY (ZONE)
# - EASTERN JUAB/MILLARD COUNTIES (ZONE)
# - GLEN CANYON RECREATION AREA/LAKE POWELL (ZONE)
# - GRAND FLAT AND ARCHES (ZONE)
# - LOWER WASHINGTON COUNTY (ZONE)
# - WESTERN CANYONLANDS (ZONE)
# - WESTERN JUAB/MILLARD COUNTIES (ZONE)
# - WESTERN UINTA BASIN (ZONE)
sort(table(storms$CZ_NAME_STR))
# 23: LA SAL & ABAJO MOUNTAINS (ZONE)
# - TAVAPUTS PLATEAU (ZONE), SOUTHERN MOUNTAINS (ZONE)
# 22: WASATCH MOUNTAINS SOUTH OF I-80 (ZONE)
# - WASATCH MOUNTAINS I-80 NORTH (ZONE)
# 20: EASTERN UINTA MOUNTAINS (ZONE)
# - LA SAL & ABAJO MOUNTAINS (ZONE)
# 19: WASATCH MOUNTAINS I-80 NORTH (ZONE)
# - WASATCH MOUNTAINS SOUTH OF I-80 (ZONE), WASATCH PLATEAU/BOOK CLIFFS (ZONE)
# 17: WASATCH PLATEAU/BOOK CLIFFS (ZONE)
# - WASATCH MOUNTAINS SOUTH OF I-80 (ZONE), WASATCH MOUNTAINS I-80 NORTH (ZONE)
# 17: SOUTHERN MOUNTAINS (ZONE)
# - LA SAL & ABAJO MOUNTAINS (ZONE)
# 15: TAVAPUTS PLATEAU (ZONE)
# - LA SAL & ABAJO MOUNTAINS (ZONE)
# 13: CENTRAL MOUNTAINS (ZONE)
# - WASATCH PLATEAU/BOOK CLIFFS (ZONE), SOUTHERN MOUNTAINS (ZONE), LA SAL & ABAJO MOUNTAINS (ZONE)
# 12: WESTERN UINTA MOUNTAINS (ZONE)
# - WASATCH PLATEAU/BOOK CLIFFS (ZONE), WASATCH MOUNTAINS SOUTH OF I-80 (ZONE)
# 12: NORTHERN WASATCH FRONT (ZONE)
# - WASATCH MOUNTAINS I-80 NORTH (ZONE)
# 08: WASATCH BACK (ZONE)
# - WASATCH MOUNTAINS SOUTH OF I-80 (ZONE)
# 08: UTAH VALLEY (ZONE)
# - WASATCH PLATEAU/BOOK CLIFFS (ZONE), WASATCH MOUNTAINS SOUTH OF I-80 (ZONE)
# 08: SALT LAKE VALLEY (ZONE)
# - NORTHERN WASATCH FRONT (ZONE)
# 08: EASTERN BOX ELDER COUNTY (ZONE)
# - NORTHERN WASATCH FRONT (ZONE), CACHE VALLEY/UTAH PORTION (ZONE)
# 08: CACHE VALLEY/UTAH PORTION (ZONE)
# - NORTHERN WASATCH FRONT (ZONE), EASTERN BOX ELDER COUNTY (ZONE)
# 06: TOOELE AND RUSH VALLEYS (ZONE)
# - WASATCH MOUNTAINS SOUTH OF I-80 (ZONE)
# 06: GREAT SALT LAKE DESERT AND MOUNTAINS (ZONE)
# - WASATCH PLATEAU/BOOK CLIFFS (ZONE), CENTRAL MOUNTAINS (ZONE)
# 06: EASTERN UINTA BASIN (ZONE)
# - LA SAL & ABAJO MOUNTAINS (ZONE)
# 05: SOUTHWEST UTAH (ZONE)
# - SOUTHERN MOUNTAINS (ZONE)
# 05: BEAR LAKE AND BEAR RIVER VALLEY (ZONE)
# - many
# 04: SOUTHEAST UTAH (ZONE)
# - LA SAL & ABAJO MOUNTAINS (ZONE), TAVAPUTS PLATEAU (ZONE)
# 04: SOUTH CENTRAL UTAH (ZONE)
# - LA SAL & ABAJO MOUNTAINS (ZONE), TAVAPUTS PLATEAU (ZONE)

myzone <- "TOOELE AND RUSH VALLEYS (ZONE)"
t1 <- storms[storms$CZ_NAME_STR==myzone,]
t2 <- storms[storms$CZ_NAME_STR!=myzone,]
t3 <- t1[0,]
for (i in 1:nrow(t1)) {
  tt <- t2
  tt$DIFF <- abs(tt$BEGIN_DATE - t1$BEGIN_DATE[i])
  tt <- tt[tt$DIFF <= 1,]
  tt$DIFF <- NULL
  t3 <- rbind(t3, tt)
  rm(tt)
}; rm(i)
t3 <- t3[!duplicated(t3$EVENT_ID),]
sort(table(t3$CZ_NAME_STR))

rm(t1, t2, t3, myzone)

# conclusions about groups
# Group A: Cache County (all), Box Elder County (eastern)
# - CACHE VALLEY/UTAH PORTION (ZONE), EASTERN BOX ELDER COUNTY (ZONE)
# Group B: Weber County (western), Davis County (all)
# - NORTHERN WASATCH FRONT (ZONE)
# Group C: Salt Lake County (all)
# - SALT LAKE VALLEY (ZONE)
# Group D: Utah County (all)
# - UTAH VALLEY (ZONE)
# Group E: Weber County (eastern), Morgan County (all), Summit County (western), Wasatch County (all)
# - WASATCH MOUNTAINS I-80 NORTH (ZONE), WASATCH MOUNTAINS SOUTH OF I-80 (ZONE), 
#   WASATCH PLATEAU/BOOK CLIFFS (ZONE)
# Group F: I-15 and US-89 Corridors, Utah County to Washington County (exclusive)
# - WESTERN JUAB/MILLARD COUNTIES (ZONE), EASTERN JUAB/MILLARD COUNTIES (ZONE), 
#   SANPETE VALLEY, SEVIER VALLEY, CENTRAL MOUNTAINS, SOUTHERN MOUNTAINS, SOUTHWEST UTAH

# --> NEXT STEPS
# - get lists of ZIP codes for each of these groups
# - email to Qualtrics

sf::sf_use_s2(FALSE)

# A: Cache County (all), Box Elder County (eastern)
tc <- counties[counties$NAME %in% c("CACHE", "BOX ELDER"),]
t1 <- st_intersection(zipcodes, tc)
t1$newarea <- st_area(t1)
mapview(t1)
la <- aggregate(newarea ~ ZIP5, FUN=sum, data=t1)
la <- la[as.numeric(la$newarea)>10000,]
la$ZIP5 <- as.integer(la$ZIP5)
la <- la[!(la$ZIP5 %in% c(84404,84029,84329,84313,83342)),]
mapview(tc, alpha.regions=0, lwd=2) + mapview(zipcodes[zipcodes$ZIP5 %in% la$ZIP5,])
rm(tc, t1)

# B: Weber County (western), Davis County (all)
tc <- counties[counties$NAME %in% c("WEBER", "DAVIS"),]
t1 <- st_intersection(zipcodes, tc)
t1$newarea <- st_area(t1)
mapview(t1)
lb <- aggregate(newarea ~ ZIP5, FUN=sum, data=t1)
lb <- lb[as.numeric(lb$newarea)>10000,]
lb$ZIP5 <- as.integer(lb$ZIP5)
lb <- lb[!(lb$ZIP5 %in% c(84116,84086,84310,84317)),]
mapview(tc, alpha.regions=0, lwd=2) + mapview(zipcodes[zipcodes$ZIP5 %in% lb$ZIP5,])
rm(tc, t1)

# C: Salt Lake County (all)
tc <- counties[counties$NAME %in% c("SALT LAKE"),]
t1 <- st_intersection(zipcodes, tc)
t1$newarea <- st_area(t1)
mapview(t1)
lc <- aggregate(newarea ~ ZIP5, FUN=sum, data=t1)
lc <- lc[as.numeric(lc$newarea)>10000,]
lc$ZIP5 <- as.integer(lc$ZIP5)
lc <- lc[!(lc$ZIP5 %in% c(84087,84054,84074,84013,84043)),]
mapview(tc, alpha.regions=0, lwd=2) + mapview(zipcodes[zipcodes$ZIP5 %in% lc$ZIP5,])
rm(tc, t1)

# D: Utah County (all)
tc <- counties[counties$NAME %in% c("UTAH"),]
t1 <- st_intersection(zipcodes, tc)
t1$newarea <- st_area(t1)
mapview(t1)
ld <- aggregate(newarea ~ ZIP5, FUN=sum, data=t1)
ld <- ld[as.numeric(ld$newarea)>10000,]
ld$ZIP5 <- as.integer(ld$ZIP5)
ld <- ld[!(ld$ZIP5 %in% c(84648,84629,84526,84021,84032,84020,84065,84096)),]
mapview(tc, alpha.regions=0, lwd=2) + mapview(zipcodes[zipcodes$ZIP5 %in% ld$ZIP5,])
rm(tc, t1)

# E: Weber County (eastern), Morgan County (all), Summit County (western), Wasatch County (all)
tc <- counties[counties$NAME %in% c("WEBER", "MORGAN", "SUMMIT", "WASATCH"),]
t1 <- st_intersection(zipcodes, tc)
t1$newarea <- st_area(t1)
mapview(t1)
le <- aggregate(newarea ~ ZIP5, FUN=sum, data=t1)
le <- le[as.numeric(le$newarea)>10000,]
le$ZIP5 <- as.integer(le$ZIP5)
le <- le[!(le$ZIP5 %in% c(84414,84404,84401,84403,84408,84405,84067,84315,84405,84056,84015,84315)),]
le <- le[!(le$ZIP5 %in% c(84109,84604,84660,84027,84031,82930,84046)),]
mapview(tc, alpha.regions=0, lwd=2) + mapview(zipcodes[zipcodes$ZIP5 %in% le$ZIP5,])
rm(tc, t1)

# F: I-15 and US-89 Corridors, Utah County to Washington County (exclusive)
tc <- counties[counties$NAME %in% c("JUAB", "SANPETE", "MILLARD", "SEVIER", "BEAVER", "PIUTE", "IRON", "GARFIELD"),]
t1 <- st_intersection(zipcodes, tc)
t1$newarea <- st_area(t1)
mapview(t1)
lf <- aggregate(newarea ~ ZIP5, FUN=sum, data=t1)
lf <- lf[as.numeric(lf$newarea)>10000,]
lf$ZIP5 <- as.integer(lf$ZIP5)
lf <- lf[!(lf$ZIP5 %in% c(84728,84083,84034,84022,84080,84655,84651,84660)),]
lf <- lf[!(lf$ZIP5 %in% c(84526,84528,84537,84523,84522,84747,84775,84773,84734)),]
lf <- lf[!(lf$ZIP5 %in% c(84726,84716,84533,84741,84710,84762,84757,84725)),]
mapview(tc, alpha.regions=0, lwd=2) + mapview(zipcodes[zipcodes$ZIP5 %in% lf$ZIP5,])
rm(tc, t1)

# inspect
mapview(zipcodes[zipcodes$ZIP5 %in% la$ZIP5,]) + mapview(nwszones, alpha.regions=0, lwd=2)
mapview(zipcodes[zipcodes$ZIP5 %in% lb$ZIP5,]) + mapview(nwszones, alpha.regions=0, lwd=2)
mapview(zipcodes[zipcodes$ZIP5 %in% lc$ZIP5,]) + mapview(nwszones, alpha.regions=0, lwd=2)
mapview(zipcodes[zipcodes$ZIP5 %in% ld$ZIP5,]) + mapview(nwszones, alpha.regions=0, lwd=2)
mapview(zipcodes[zipcodes$ZIP5 %in% le$ZIP5,]) + mapview(nwszones, alpha.regions=0, lwd=2)
mapview(zipcodes[zipcodes$ZIP5 %in% lf$ZIP5,]) + mapview(nwszones, alpha.regions=0, lwd=2)

# combine
zipcodes$GROUP <- NA
zipcodes$GROUP <- ifelse(zipcodes$ZIP5 %in% la$ZIP5, "A", zipcodes$GROUP)
zipcodes$GROUP <- ifelse(zipcodes$ZIP5 %in% lb$ZIP5, "B", zipcodes$GROUP)
zipcodes$GROUP <- ifelse(zipcodes$ZIP5 %in% lc$ZIP5, "C", zipcodes$GROUP)
zipcodes$GROUP <- ifelse(zipcodes$ZIP5 %in% ld$ZIP5, "D", zipcodes$GROUP)
zipcodes$GROUP <- ifelse(zipcodes$ZIP5 %in% le$ZIP5, "E", zipcodes$GROUP)
zipcodes$GROUP <- ifelse(zipcodes$ZIP5 %in% lf$ZIP5, "F", zipcodes$GROUP)
mapview(zipcodes, zcol="GROUP") + mapview(nwszones, alpha.regions=0, lwd=2)

# create list
zmax <- max(c(nrow(la), nrow(lb), nrow(lc), nrow(ld)), nrow(le), nrow(lf))
zips <- data.frame(matrix(data="", nrow=zmax, ncol=6))
rm(zmax)
names(zips) <- c("A", "B", "C", "D", "E", "F")
zips$A[1:nrow(la)] <- sort(la$ZIP5)
zips$B[1:nrow(lb)] <- sort(lb$ZIP5)
zips$C[1:nrow(lc)] <- sort(lc$ZIP5)
zips$D[1:nrow(ld)] <- sort(ld$ZIP5)
zips$E[1:nrow(le)] <- sort(le$ZIP5)
zips$F[1:nrow(lf)] <- sort(lf$ZIP5)
zips
write.csv(zips, file.path("Data", "Regions-ZIPs", "zipslist.csv"), row.names=F)

# print list
# A: Cache County (all), Box Elder County (eastern)
sort(la$ZIP5)
# B: Weber County (western), Davis County (all)
sort(lb$ZIP5)
# C: Salt Lake County (all)
sort(lc$ZIP5)
# D: Utah County (all)
sort(ld$ZIP5)
# E: Weber County (eastern), Morgan County (all), Summit County (western), Wasatch County (all)
sort(le$ZIP5)
# F: I-15 and US-89 Corridors, Utah County to Washington County (exclusive)
sort(lf$ZIP5)
# view
mapview(zipcodes, zcol="GROUP")
saveRDS(zipcodes, file.path("Data", "Regions-ZIPs", "zipcodes.rds"))

# cleanup
rm(la, lb, lc, ld, le, lf)
rm(counties, nwszones, storms, zipcodes, zips)
gc()
