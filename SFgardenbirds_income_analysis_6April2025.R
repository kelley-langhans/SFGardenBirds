# Calculating garden neighborhood income
# cleaning census data
# cleaning zillow data
# Last updated 11 April 2025

#################
# Set WD
#################

# Set working directory to where ever files are stored
#setwd("")


###################
# load libraries
##################

library(tidyverse)
library(openxlsx)
library(sf)

##################
# read in data
##################

# Census data from SF county updated jan 2023 (using 2021 tiger lines and acs)
# American Community Survey income data
acs21income <- read.xlsx("ACSST5Y2021.S1903-Data.xlsx", sheet = "Median_household_est_nomissing")
# Census tract shapefile
tracts21 <- st_read(dsn = "./tl_2021_SF_city_tracts", layer = "tl_2021_SF_city_tracts")

# additional data to include northern San Mateo county census tracts
acs21incomeSM <- read.xlsx("ACSST5Y2021.S1903-Data_SM.xlsx", sheet = "Median_household_est_nomissing")
tracts21SMSF <- st_read(dsn = "./tl_2021_SF_nSM_tracts", layer = "tl_2021_SF_nSM_tracts")

## For median income calculation
# read in intersection of 2021 ACS median hh income tract data (w/missing data filled in) and 1627 m buffers around garden centroids for SF and SM county
tractxbuffsfsm <- read.csv("gardens_buff_1627m_censustract_intersection_SFSM.csv") 

# garden centroids
garden.cent <- st_read(dsn = "./SF_garden_centroids", layer = "SF_gardens_26943")


################
# examine data
#################

head(acs21income)
dim(acs21income) #244
unique(acs21income$medhhinc_2021)

head(tracts21) #248, explore this discrepancy later
dim(tracts21)

head(acs21incomeSM)
dim(acs21incomeSM) # 174

dim(tracts21SMSF) # 279

head(tractxbuffsfsm)
dim(tractxbuffsfsm)
length(unique(tractxbuffsfsm$Name)) #20 gardens

###############
# Clean data
#################

# ACS data

# make IDs match
acs21income$GEOID <- gsub("1400000US([0-9]+)", "\\1", acs21income$GEO_ID)
head(acs21income)

is.numeric(acs21income$medhhinc_2021)
acs21income$medinc <- as.numeric(gsub(",", "", acs21income$medhhinc_2021))
acs21income$medinc

acs21income$nhouse <- acs21income$S1903_C01_001E
acs21income$nhouse

acs21income2 <- acs21income[,c("GEOID", "medinc", "nhouse")]

# make IDs match
acs21incomeSM$GEOID <- gsub("1400000US([0-9]+)", "\\1", acs21incomeSM$GEO_ID)
head(acs21incomeSM)

is.numeric(acs21incomeSM$medhhinc_2021)
acs21incomeSM$medinc <- as.numeric(gsub(",", "", acs21incomeSM$S1903_C03_001E))
acs21incomeSM$medinc

acs21incomeSM$nhouse <- acs21incomeSM$S1903_C01_001E
acs21incomeSM$nhouse

acs21incomeSM2 <- acs21incomeSM[,c("GEOID", "medinc", "nhouse")]


##################
# Merge data
###################


## Census data for just SF

tracts21.2 <- tracts21
tracts21.2 <- merge(tracts21.2, acs21income2, by = c("GEOID"), all.x=FALSE)
summary(tracts21.2) # 240 long


subset(acs21income2$GEOID, !(acs21income2$GEOID %in% tracts21.2$GEOID)) # 4 of these are non-matching... seems like 4 missing, 4 unmatched
# all 4 in income dataset, not shapefile are sensible, I've excluded them
# these 4 are 1) alcatraz + treasure island, which I've excluded 06075017903
# 2) the farallons, which I've excluded 06075980401
# 3) part of the ocean, which I've exluded 06075990100
# 4) another part of the ocean

# conclusion: we can exclude all tracts w/o a match in both datasets


# look at distribution of # of households in each census tract in SF
hist(tracts21.2$nhouse)
median(tracts21.2$nhouse)
subset(tracts21.2, tracts21.2$nhouse < 500)
# only 7, 3 around 400, 2 around 100, one 17, one 0
# census tract 9802 has 0 households, landsend, also has NA estimate
# census tract 9803 has 17 households, ggp, has estimate
# census tract 9805.01 has 99 households, mclaren park
# census tract 9809 has 127 households, looks mostly industrial north of bayview

# workflow: fill in missing data for target tracts (off of R by hand, and document how each filled in)
# load into R, keep this cleaning/subsetting to match with geography
# also subset out <400, leave these as legitimate missing data on the map and in dealing with calculations


# Subset out tracts with <400 households
tracts21.3 <- subset(tracts21.2, tracts21.2$nhouse > 400)
summary(tracts21.3) # length 236, subsetted out the above

tracts21.3$medinc
tracts21.3$NAME

# subsetting out all with < 400 households would remove one missing data point
write_sf(tracts21.3, dsn = "Census/tl_2021_SF_city_tracts_acs5ymedincome_nomissing", layer = "tl_2021_SF_city_tracts_acs5ymedincome_nomissing", driver = "ESRI Shapefile")

# missing data 6/236 = 2.5%



## Census data for northern SM and SF

## merge together

# combine two income datasets for merging
head(acs21incomeSM2)
head(acs21income2)
acs21incomeSFSM <- rbind(acs21income2, acs21incomeSM2)
dim(acs21incomeSFSM) # 418

# merge income with shapefile
tracts21SMSF.2 <- tracts21SMSF
tracts21SMSF.2 <- merge(tracts21SMSF.2, acs21incomeSFSM, by = c("GEOID"), all.x=FALSE)
summary(tracts21SMSF.2) # 279 long

# Subset out tracts with <400 households
tracts21SMSF.3 <- subset(tracts21SMSF.2, tracts21SMSF.2$nhouse > 400)
summary(tracts21SMSF.3) # length 274, subsetted out the above

tracts21SMSF.3$medinc

write_sf(tracts21SMSF.3, dsn = "Census/tl_2021_SF_nSM_tracts_acs5ymedincome_nomissing", layer = "tl_2021_SF_nSM_tracts_acs5ymedincome_nomissing", driver = "ESRI Shapefile")


###########################################
# Neighborhood median hh income calculation
############################################

## Use data from SF and San Mateo county, 1627 m buffer
# intersection between garden buffers and census tracts done in R

# clean up data so easier to work with
tractxbuffsfsm2 <- tractxbuffsfsm[,c("NAME", "Name_2", "medinc", "area")]

# for each garden, calculate total land area in new df thru ddply
gardenbuff.larea <- plyr::ddply(tractxbuffsfsm2, c("Name_2"), summarize, 
                                bufflarea = sum(area))
# Max is 8180078, which is approx pi(1672)^2

# add a new column that's total area for that garden thru merge
tractxbuffsfsm3 <- merge(tractxbuffsfsm2, gardenbuff.larea, by = c("Name_2"), all.x = TRUE)
head(tractxbuffsfsm3)

# Math to get decimal percent of total that that income makes up

# math to get weighted average of income (should be percent1*tract1income + percent2*tract2income etc.)
# ie if tract 1 is 75% of area and has an income of 2, and tract 2 is 25% of the area and has an income of 4, average = .75(2) + .25(4) = 1.5 + 1 = 2.5
# same as saying there are 4 equal areas, 3 have income 2 and one has income of 4, so (2+2+2+4)/4 = (3*2 + 1*4)/4 = 3/4(2) + 1/4(4) = 2.5

# create a column with the area ratio
tractxbuffsfsm3$area_ratio <- tractxbuffsfsm3$area/tractxbuffsfsm3$bufflarea
# create a column with med income for that tract * tract area ratio to get proportional income (to be added in next step)
tractxbuffsfsm3$medinc_proport <- tractxbuffsfsm3$medinc * tractxbuffsfsm3$area_ratio

# sum up proportional incomes for each garden buffsfsmer to get spatially weighted average of median hh income
gardenbuffsfsm.medhhincome <- plyr::ddply(tractxbuffsfsm3, c("Name_2"), summarize, 
                                      medhhincome = sum(medinc_proport))
dim(gardenbuffsfsm.medhhincome)
gardenbuffsfsm.medhhincome
range(gardenbuffsfsm.medhhincome$medhhincome) # 82621.34 173373.40
mean(gardenbuffsfsm.medhhincome$medhhincome) # 136427.4 vs. SF med hh income in 2021 from ACS 5 yr est reported as $126,187, pretty close

# merge with gardens for visualization

gardenbuffsfsm.medhhincome$Name <- ifelse(gardenbuffsfsm.medhhincome$Name=="Dogpath/Miller Memorial Community Garden", "Dogpatch/Miller Memorial Community Garden", 
                                      ifelse(gardenbuffsfsm.medhhincome$Name=="Portrero del Sol Community Garden", "Potrero del Sol Community Garden",
                                             ifelse(gardenbuffsfsm.medhhincome$Name=="Portrero Hill Community Garden" , "Potrero Hill Community Garden",
                                                    ifelse(gardenbuffsfsm.medhhincome$Name=="Visitacion Valley Greenway Community Garden", "Visitación Valley Greenway Community Garden", gardenbuffsfsm.medhhincome$Name))))
unique(subset(garden.cent$Name, !(garden.cent$Name %in% gardenbuffsfsm.medhhincome$Name))) # all match now

garden.cent2sfsm <- merge(garden.cent, gardenbuffsfsm.medhhincome, by = c("Name"))
dim(garden.cent2sfsm)
garden.cent2sfsm

write_sf(garden.cent2sfsm, dsn = "./garden_income_buffers/garden_centroids_income_1627m_SFSM", layer = "garden_centroids_income_1627m_SFSM", driver = "ESRI Shapefile")

write.csv(gardenbuffsfsm.medhhincome[,c("Name", "medhhincome")], "GardenIncome.csv")


