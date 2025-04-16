## SF Garden Birds bird data cleaning
# Author: Kelley Langhans
# Last updated 6 April 2025

#################
# Set WD
#################
# set working directory to where ever files are saved
# setwd("")

###################
# load libraries
##################

library(tidyverse)
library(openxlsx)
library(lubridate)
library(reshape2)
library(glmm)

##################
# read in data
##################

# Bird Survey data 
birdsurv <- read.xlsx("SFgardens_birdsurvey_data.xlsx", sheet = "Survey_bird_data")

# Survey environmental data
envsurv <- read.xlsx("SFgardens_birdsurvey_data.xlsx", sheet = "Survey_environmental_data")

# Species codes and names, with migratory info
spec <- read.xlsx("SFgardens_birdsurvey_data.xlsx", sheet = "Species_codes")

##################
# Explore data
###############

### Unfiltered data

dim(birdsurv) # 3244 bird observations! (unique species/site/time/location, so actually more)
head(birdsurv) # row for survey ID, species, where observed, number, substrate, notes, whether ID resolved, whether close or far

dim(envsurv) # 275 surveys - not all included in final study, non-point count methodologies tried
length(unique(birdsurv$Survey_ID)) # 275
head(envsurv)

dim(spec)
head(spec)

sitenames <- envsurv[,c("Survey_ID", "Site_code", "Final_data")]
birdsurv2 <- merge(birdsurv, sitenames, by = c("Survey_ID"), all.x= TRUE)
dim(birdsurv2)

# Some quick data questions
# How many birds did we see total, with no data cleaning?
sum(birdsurv$Number) # 8615

# per site? (we do have different survey effort)
x <- plyr::ddply(birdsurv2, c("Site_code"), summarize, 
            Nbirds = sum(Number))
x[order(x$Nbirds, decreasing = TRUE), ]

# How many species did we see, with no data cleaning?
length(unique(birdsurv2$Species_ID)) #72, but this includes unresolved IDs

# per site? (we do have different survey effort)
x2 <- plyr::ddply(birdsurv2, c("Site_code", "Species_ID"), summarize, 
                 Nbirds = sum(Number))
count(x2, Site_code, sort = TRUE, name = "SR") 

# But of course none of this is accounting for observation effort, which was pretty uneven. 
# And includes non-focal surveys!

### 12 focal surveys
# includes point counts and perimeter surveys

# subset down to 12 focal surveys
birdsurvf <- subset(birdsurv2, birdsurv2$Final_data==1)
dim(birdsurvf) #2782 
length(unique(birdsurvf$Survey_ID)) # 240 (12 surveys *20 sites)

#Some quick data questions
# How many birds did we see total, with no data cleaning?
sum(birdsurvf$Number) # 7278

# per site? 
x <- plyr::ddply(birdsurvf, c("Site_code"), summarize, 
                 Nbirds = sum(Number))
x[order(x$Nbirds, decreasing = TRUE), ]
mean(x$Nbirds)

# How many species did we see, with no data cleaning?
length(unique(birdsurvf$Species_ID)) #70, but this includes unresolved IDs

# per site? (we do have different survey effort)
x2 <- plyr::ddply(birdsurvf, c("Site_code", "Species_ID"), summarize, 
                  Nbirds = sum(Number))
count(x2, Site_code, sort = TRUE, name = "SR") 
mean(count(x2, Site_code, sort = TRUE, name = "SR")$SR) #26

# visualization of distribution of date of focal surveys

head(envsurv)
hist(dmy(envsurv$Date), breaks = "days", freq = TRUE, 
     col = "lightgray", xlab = "Survey Date", ylab = "# of Surveys", main = "Distribution of survey dates")

pdf("./Figures/survey_dates.pdf", height = 6, width = 9)
hist(dmy(envsurv$Date), breaks = "days", freq = TRUE, 
     col = "lightgray", xlab = "Survey Date", ylab = "# of Surveys", main = "Distribution of survey dates")
dev.off()


########################
# Data cleaning
########################

### Filtering bird species

# Filter out migrants that arrive in the fall, because most of surveying was in the summer
# Remove single warbler sp. observation, because it was made 10 Sept, so likely a migratory warbler
# Not filtering out one sp. that leaves in september
mig <- spec[,c("Species_ID", "Fall_arrival")]
birdsurvf2 <- merge(birdsurvf, mig, by = c("Species_ID"), all.x = TRUE)
birdsurvf.nomig <- subset(birdsurvf2, birdsurvf2$Fall_arrival!=1  | (is.na(birdsurvf2$Fall_arrival) & birdsurvf2$Species_ID != "Warbler sp.")) #is.na will preserve unknown species IDs
dim(birdsurvf) #2782
dim(birdsurvf.nomig) #2771
head(birdsurvf.nomig)
unique(birdsurvf.nomig$Species_ID)
subset(birdsurvf, !(birdsurvf$Species_ID %in% birdsurvf.nomig$Species_ID)) # removes YEWA, AMKE, YBCH, TOWA, Warbler sp

# Remove far away birds 
birdsurvf.nomig.nofar <- subset(birdsurvf.nomig, is.na(birdsurvf.nomig$Far.away))
dim(birdsurvf.nomig.nofar) # 2754


# Take a look at flyovers
fo <- subset(birdsurvf.nomig.nofar, birdsurvf.nomig.nofar$Obs_type=="flyover")
dim(fo)
unique(fo$Species_ID)
length(unique(fo$Species_ID)) # 40
# Includes a lot of species: ones in garden, RMPA and doves which often just flew above, seabirds, raptors (which often also just flew above, but do interact w/site)
# first, let's try removing all (this will likely remove hawks..)
# then, let's try removing just seabirds
# then, a more careful approach--picking and choosing with a rationale

# Remove all FOs
birdsurvf.nomig.nofar.nofo <- subset(birdsurvf.nomig.nofar, !(birdsurvf.nomig.nofar$Obs_type=="flyover"))
dim(birdsurvf.nomig.nofar.nofo) # 2018 --> now 2019


# # Remove just seabirds
# seabirds <- c("BRPE", "CAGU", "CATE", "Cormorant sp.", "Duck sp.", "ELTE", "GBHE", "Gull sp.", "OSPR", "Sternidae sp.", "WEGU")
# birdsurvf.nomig.nofar.nosea <- subset(birdsurvf.nomig.nofar, !(birdsurvf.nomig.nofar$Species_ID %in% seabirds))
# dim(birdsurvf.nomig.nofar.nosea) #2505

# Decided to keep 2 versions, one with all FOs removed, and one with no birds removed--including outside birds. This is our most conservative and least conservative versions of what people interact with at the site.



### Separating out 2 survey types

survtype <- envsurv[,c("Survey_ID", "Survey_type")]
birdsurvf.nomig.nofar2 <- merge(birdsurvf.nomig.nofar, survtype, by = c("Survey_ID"), all.x= TRUE)

perim <- subset(birdsurvf.nomig.nofar2, birdsurvf.nomig.nofar2$Survey_type=="Perimeter") # These surveys were not included in final analysis.
point <- subset(birdsurvf.nomig.nofar2, birdsurvf.nomig.nofar2$Survey_type=="Point")
length(unique(perim$Survey_ID))
length(unique(point$Survey_ID))



### Point count surveys

## All area (within point count radius, outside, flyovers) - data used in final manuscript

# How many birds did we see total?
sum(point$Number) # 3654 

write.csv(point, "point.unresolved2.csv", row.names = FALSE)

# per site? 
x <- plyr::ddply(point, c("Site_code"), summarize, 
                 Nbirds_point = sum(Number))
point.nsite <- x[order(x$Nbirds_point, decreasing = TRUE), ]
mean(point.nsite$Nbirds_point) # 183

write.csv(point.nsite, "point.unresolved.nsite2.csv", row.names = FALSE)

# How many species did we see?

# take out unresolved species (only identified at genus level or higher)
# Get list of species 
point.sp <- unique(point$Species_ID)
point.resolved <- point

# Remove any unresolved IDs that can't be a species observed already over all
# If there is a RTHA or RSHA in the point survey, remove any observations of "Buteo sp."
if ("RTHA" %in% point.sp | "RSHA" %in% point.sp) {
  point.resolved <- subset(point.resolved, point.resolved$Species_ID != "Buteo sp.")
}

# If there is a ECDO, MODO, ROPI, BTPI in the point survey, remove any observations of "Columbidae sp."
if ("ECDO" %in% point.sp | "MODO" %in% point.sp | "ROPI" %in% point.sp | "BTPI" %in% point.sp) {
  point.resolved <- subset(point.resolved, point.resolved$Species_ID != "Columbidae sp.")
}

# If there is a crow or raven in the point survey, remove any observations of "Corvus sp."
if ("AMCR" %in% point.sp | "CORA" %in% point.sp) {
  point.resolved <- subset(point.resolved, point.resolved$Species_ID != "Corvus sp.")
}

# If there is a WEGU or CAGU in the point survey, remove any observations of "Gull sp."
if ("WEGU" %in% point.sp | "CAGU" %in% point.sp) {
  point.resolved <- subset(point.resolved, point.resolved$Species_ID != "Gull sp.")
}

# If there is a RTHA, RSHA, COHA, remove any observations of "Hawk sp."
if ("RTHA" %in% point.sp | "RSHA" %in% point.sp | "COHA" %in% point.sp) {
  point.resolved <- subset(point.resolved, point.resolved$Species_ID != "Hawk sp.")
}

# If there is a BASW, CLSW remove any observations of "Hirundinidae sp."
if ("BASW" %in% point.sp | "CLSW" %in% point.sp) {
  point.resolved <- subset(point.resolved, point.resolved$Species_ID != "Hirundinidae sp.")
}

# If there is a HOFI or HOSP in the point survey, remove any observations of "Passerida sp."
if ("HOFI" %in% point.sp | "HOSP" %in% point.sp) {
  point.resolved <- subset(point.resolved, point.resolved$Species_ID != "Passerida sp.")
}

# If there is a DOWO or NUWO in the point survey, remove any observations of "Picoides sp."
if ("DOWO" %in% point.sp | "NUWO" %in% point.sp) {
  point.resolved <- subset(point.resolved, point.resolved$Species_ID != "Picoides sp.")
}

# If there is a WCSP, SOSP, HOSP in the point survey, remove any observations of "Sparrow sp."
if ("WCSP" %in% point.sp | "SOSP" %in% point.sp | "HOSP" %in% point.sp) {
  point.resolved <- subset(point.resolved, point.resolved$Species_ID != "Sparrow sp.")
}

# If there is a CATE, ELTE in the point survey, remove any observations of "Sternidae sp."
if ("CATE" %in% point.sp | "ELTE" %in% point.sp) {
  point.resolved <- subset(point.resolved, point.resolved$Species_ID != "Sternidae sp.")
}

# If there is a ANHU, ALHU in the point survey, remove any observations of "Trochilidae sp."
if ("ANHU" %in% point.sp | "ALHU" %in% point.sp) {
  point.resolved <- subset(point.resolved, point.resolved$Species_ID != "Trochilidae sp.")
}

# Cut any "unknown" designations
point.resolved <- subset( point.resolved,  point.resolved$Species_ID != "Unknown")

dim(point) # 1376
dim(point.resolved) # 1219
# Tally up number of species in resolved point survey
length(unique(point.resolved$Species_ID)) # 49


# per site? 
point.spxsite <- plyr::ddply(point, c("Site_code", "Species_ID"), summarize, 
                             Nbirds = sum(Number))

# Build in loop to only keep unresolved IDs if they couldn't be a species observed already at the site
# Assign them "unknown" IDs if they could match ID of species observed at site
# remove "unknown"

# create a list of garden names
gardens <- unique(point.spxsite$Site_code)
length(gardens)

# create an empty dataframe to paste garden results into
point.spxsite.resolve <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(point.spxsite.resolve) <- c("Site_code", "Species_ID", "Nbirds")

for (i in 1:length(gardens)){
  
  # Pull garden name
  gar_ID <- gardens[[i]]
  
  # Subset to data from that garden
  gar <- subset(point.spxsite, point.spxsite$Site_code==gar_ID)
  
  # Get list of species from that garden
  gar.sp <- gar$Species_ID
  
  # Remove any unresolved IDs that can't be a species observed already at the site
  
  # If there is a RTHA or RSHA in the garden, remove any observations of "Buteo sp."
  if ("RTHA" %in% gar.sp | "RSHA" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Buteo sp.")
  }
  
  # If there is a ECDO, MODO, ROPI, BTPI in the garden, remove any observations of "Columbidae sp."
  if ("ECDO" %in% gar.sp | "MODO" %in% gar.sp | "ROPI" %in% gar.sp | "BTPI" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Columbidae sp.")
  }
  
  # If there is a crow or raven in the garden, remove any observations of "Corvus sp."
  if ("AMCR" %in% gar.sp | "CORA" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Corvus sp.")
  }
  
  # If there is a WEGU or CAGU in the garden, remove any observations of "Gull sp."
  if ("WEGU" %in% gar.sp | "CAGU" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Gull sp.")
  }
  
  # If there is a RTHA, RSHA, COHA, remove any observations of "Hawk sp."
  if ("RTHA" %in% gar.sp | "RSHA" %in% gar.sp | "COHA" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Hawk sp.")
  }
  
  # If there is a BASW, CLSW remove any observations of "Hirundinidae sp."
  if ("BASW" %in% gar.sp | "CLSW" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Hirundinidae sp.")
  }
  
  
  # If there is a HOFI or HOSP in the garden, remove any observations of "Passerida sp."
  if ("HOFI" %in% gar.sp | "HOSP" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Passerida sp.")
  }
  
  # If there is a DOWO or NUWO in the garden, remove any observations of "Picoides sp."
  if ("DOWO" %in% gar.sp | "NUWO" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Picoides sp.")
  }
  
  # If there is a WCSP, SOSP, HOSP in the garden, remove any observations of "Sparrow sp."
  if ("WCSP" %in% gar.sp | "SOSP" %in% gar.sp | "HOSP" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Sparrow sp.")
  }
  
  # If there is a CATE, ELTE in the garden, remove any observations of "Sternidae sp."
  if ("CATE" %in% gar.sp | "ELTE" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Sternidae sp.")
  }
  
  # If there is a ANHU, ALHU in the garden, remove any observations of "Trochilidae sp."
  if ("ANHU" %in% gar.sp | "ALHU" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Trochilidae sp.")
  }
  
  # Cut any "unknown" designations
  gar <- subset(gar, gar$Species_ID != "Unknown")
  
  # Add this garden to the dataframe
  point.spxsite.resolve <- rbind(point.spxsite.resolve, gar)
}

head(point.spxsite.resolve)
dim(point.spxsite.resolve) # 386
dim(point.spxsite) # 431

write.csv(point.spxsite.resolve, "point.spxsite.resolve2.csv", row.names = FALSE)

# Tally up SR
point.srsite.resolve <- count(point.spxsite.resolve, Site_code, sort = TRUE, name = "SR_point") 
point.srsite.resolve
mean(point.srsite.resolve$SR_point) # 19

write.csv(point.srsite.resolve, "point.srsite.resolve2.csv", row.names = FALSE)



## Only garden, buffer (none outside point count radius, flyover)

point.gb <- subset(point, !(point$Obs_type=="outside" | point$Obs_type=="flyover"))

write.csv(point.gb, "point.gb.unresolved2.csv", row.names = FALSE)

# How many birds did we see total?
sum(point.gb$Number) # 1984

# per site? 
x <- plyr::ddply(point.gb, c("Site_code"), summarize, 
                 Nbirds_point = sum(Number))
point.gb.nsite <- x[order(x$Nbirds_point, decreasing = TRUE), ]
mean(point.gb.nsite$Nbirds_point) # 99

write.csv(point.gb.nsite, "point.gb.unresolved.nsite2.csv", row.names = FALSE)


# How many species did we see?

# take out unresolved species
# Get list of species from the survey
point.gb.sp <- unique(point.gb$Species_ID)
point.gb.resolved <- point.gb

# Remove any unresolved IDs that can't be a species observed already at the site
# If there is a RTHA or RSHA in the survey, remove any observations of "Buteo sp."
if ("RTHA" %in% point.gb.sp | "RSHA" %in% point.gb.sp) {
  point.gb.resolved <- subset(point.gb.resolved, point.gb.resolved$Species_ID != "Buteo sp.")
}

# If there is a ECDO, MODO, ROPI, BTPI in the survey, remove any observations of "Columbidae sp."
if ("ECDO" %in% point.gb.sp | "MODO" %in% point.gb.sp | "ROPI" %in% point.gb.sp | "BTPI" %in% point.gb.sp) {
  point.gb.resolved <- subset(point.gb.resolved, point.gb.resolved$Species_ID != "Columbidae sp.")
}

# If there is a crow or raven in the survey, remove any observations of "Corvus sp."
if ("AMCR" %in% point.gb.sp | "CORA" %in% point.gb.sp) {
  point.gb.resolved <- subset(point.gb.resolved, point.gb.resolved$Species_ID != "Corvus sp.")
}

# If there is a WEGU or CAGU in the survey, remove any observations of "Gull sp."
if ("WEGU" %in% point.gb.sp | "CAGU" %in% point.gb.sp) {
  point.gb.resolved <- subset(point.gb.resolved, point.gb.resolved$Species_ID != "Gull sp.")
}

# If there is a RTHA, RSHA, COHA, remove any observations of "Hawk sp."
if ("RTHA" %in% point.gb.sp | "RSHA" %in% point.gb.sp | "COHA" %in% point.gb.sp) {
  point.gb.resolved <- subset(point.gb.resolved, point.gb.resolved$Species_ID != "Hawk sp.")
}

# If there is a BASW, CLSW remove any observations of "Hirundinidae sp."
if ("BASW" %in% point.gb.sp | "CLSW" %in% point.gb.sp) {
  point.gb.resolved <- subset(point.gb.resolved, point.gb.resolved$Species_ID != "Hirundinidae sp.")
}

# If there is a HOFI or HOSP in the survey, remove any observations of "Passerida sp."
if ("HOFI" %in% point.gb.sp | "HOSP" %in% point.gb.sp) {
  point.gb.resolved <- subset(point.gb.resolved, point.gb.resolved$Species_ID != "Passerida sp.")
}

# If there is a DOWO or NUWO in the survey, remove any observations of "Picoides sp."
if ("DOWO" %in% point.gb.sp | "NUWO" %in% point.gb.sp) {
  point.gb.resolved <- subset(point.gb.resolved, point.gb.resolved$Species_ID != "Picoides sp.")
}

# If there is a WCSP, SOSP, HOSP in the survey, remove any observations of "Sparrow sp."
if ("WCSP" %in% point.gb.sp | "SOSP" %in% point.gb.sp | "HOSP" %in% point.gb.sp) {
  point.gb.resolved <- subset(point.gb.resolved, point.gb.resolved$Species_ID != "Sparrow sp.")
}

# If there is a CATE, ELTE in the survey, remove any observations of "Sternidae sp."
if ("CATE" %in% point.gb.sp | "ELTE" %in% point.gb.sp) {
  point.gb.resolved <- subset(point.gb.resolved, point.gb.resolved$Species_ID != "Sternidae sp.")
}

# If there is a ANHU, ALHU in the survey, remove any observations of "Trochilidae sp."
if ("ANHU" %in% point.gb.sp | "ALHU" %in% point.gb.sp) {
  point.gb.resolved <- subset(point.gb.resolved, point.gb.resolved$Species_ID != "Trochilidae sp.")
}

# Cut any "unknown" designations
point.gb.resolved <- subset( point.gb.resolved,  point.gb.resolved$Species_ID != "Unknown")

dim(point.gb) # 826
dim(point.gb.resolved) # 804  (extra was an uNK)
# Tally up number of species in resolved point survey
length(unique(point.gb.resolved$Species_ID)) # 39 species


# per site? 
point.gb.spxsite <- plyr::ddply(point.gb, c("Site_code", "Species_ID"), summarize, 
                  Nbirds = sum(Number))


# Build in loop to only keep unresolved IDs if they couldn't be a species observed already at the site
# Assign them "unknown" IDs if they could match ID of species observed at site
# remove "unknown"

# create a list of garden names
gardens <- unique(point.gb.spxsite$Site_code)
length(gardens)

# create an empty dataframe to paste garden results into
point.gb.spxsite.resolve <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(point.gb.spxsite.resolve) <- c("Site_code", "Species_ID", "Nbirds")

for (i in 1:length(gardens)){
  
  # Pull garden name
  gar_ID <- gardens[[i]]
  
  # Subset to data from that garden
  gar <- subset(point.gb.spxsite, point.gb.spxsite$Site_code==gar_ID)
  
  # Get list of species from that garden
  gar.sp <- gar$Species_ID
  
  # Remove any unresolved IDs that can't be a species observed already at the site
  
  # If there is a RTHA or RSHA in the garden, remove any observations of "Buteo sp."
  if ("RTHA" %in% gar.sp | "RSHA" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Buteo sp.")
  }
  
  # If there is a ECDO, MODO, ROPI, BTPI in the garden, remove any observations of "Columbidae sp."
  if ("ECDO" %in% gar.sp | "MODO" %in% gar.sp | "ROPI" %in% gar.sp | "BTPI" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Columbidae sp.")
  }
  
  # If there is a crow or raven in the garden, remove any observations of "Corvus sp."
  if ("AMCR" %in% gar.sp | "CORA" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Corvus sp.")
  }
  
  # If there is a WEGU or CAGU in the garden, remove any observations of "Gull sp."
  if ("WEGU" %in% gar.sp | "CAGU" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Gull sp.")
  }
  
  # If there is a RTHA, RSHA, COHA, remove any observations of "Hawk sp."
  if ("RTHA" %in% gar.sp | "RSHA" %in% gar.sp | "COHA" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Hawk sp.")
  }
  
  # If there is a BASW, CLSW remove any observations of "Hirundinidae sp."
  if ("BASW" %in% gar.sp | "CLSW" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Hirundinidae sp.")
  }
  
  
  # If there is a HOFI or HOSP in the garden, remove any observations of "Passerida sp."
  if ("HOFI" %in% gar.sp | "HOSP" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Passerida sp.")
  }
  
  # If there is a DOWO or NUWO in the garden, remove any observations of "Picoides sp."
  if ("DOWO" %in% gar.sp | "NUWO" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Picoides sp.")
  }
  
  # If there is a WCSP, SOSP, HOSP in the garden, remove any observations of "Sparrow sp."
  if ("WCSP" %in% gar.sp | "SOSP" %in% gar.sp | "HOSP" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Sparrow sp.")
  }
  
  # If there is a CATE, ELTE in the garden, remove any observations of "Sternidae sp."
  if ("CATE" %in% gar.sp | "ELTE" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Sternidae sp.")
  }
  
  # If there is a ANHU, ALHU in the garden, remove any observations of "Trochilidae sp."
  if ("ANHU" %in% gar.sp | "ALHU" %in% gar.sp) {
    gar <- subset(gar, gar$Species_ID != "Trochilidae sp.")
  }
  
  # Cut any "unknown" designations
  gar <- subset(gar, gar$Species_ID != "Unknown")
  
  # Add this garden to the dataframe
  point.gb.spxsite.resolve <- rbind(point.gb.spxsite.resolve, gar)
}

head(point.gb.spxsite.resolve)
dim(point.gb.spxsite.resolve) # 272
dim(point.gb.spxsite) # 290

write.csv(point.gb.spxsite.resolve, "point.gb.spxsite.resolve2.csv", row.names = FALSE)

# Tally up SR
point.gb.srsite.resolve <- count(point.gb.spxsite.resolve, Site_code, sort = TRUE, name = "SR_point") 
point.gb.srsite.resolve
mean(point.gb.srsite.resolve$SR_point) # 14

write.csv(point.gb.srsite.resolve, "point.gb.srsite.resolve2.csv", row.names = FALSE)


## Write out a single dataframe with all of the relevant point count data for analysis
# For each site, for gb and gbfo: unresolved n, resolved sr

point.combinedmetrics <- merge(point.gb.srsite.resolve, point.srsite.resolve, by = c("Site_code"))
point.combinedmetrics2 <- merge(point.combinedmetrics, point.gb.nsite, by = c("Site_code"))
point.combinedmetrics3 <- merge(point.combinedmetrics2, point.nsite, by = c("Site_code"))
head(point.combinedmetrics3)
colnames(point.combinedmetrics3) <- c("Site_code", "SR_point_gb", "SR_point_gbfo", "Nbirds_point_gb", "Nbirds_point_gbfo")
head(point.combinedmetrics3)

write.csv(point.combinedmetrics3, "point.allmetrics.csv")


###########################################
# Exploring potential confounding factors
############################################

# How does survey temperature, month, and # of observers affect the number and SR of birds we see?
# look at this for point count surveys w/gb and gbfo data, assuming this is our final data (unresolved for n, resolved for SR)

head(point)
head(point.gb)
head(envsurv)

## Tally up SR and N for each survey
# N including unresolved IDs
survey_n <- plyr::ddply(point, c("Survey_ID"), summarize, 
                 Nbirds_point = sum(Number))
survey.gb_n <- plyr::ddply(point.gb, c("Survey_ID"), summarize, 
                        Nbirds_point_gb = sum(Number))
dim(survey.gb_n)
survey.gb_n <- rbind(survey.gb_n, c("FLFA_1507_o", 0)) # bind on missing survey w/no observations
dim(survey.gb_n)
survey.gb_n$Nbirds_point_gb <- as.numeric(survey.gb_n$Nbirds_point_gb)

# SR for resolved IDs
survey_unresolved_sp <- plyr::ddply(point, c("Survey_ID", "Species_ID"), summarize, 
                                 Nbirds = sum(Number))
survey.gb_unresolved_sp <- plyr::ddply(point.gb, c("Survey_ID", "Species_ID"), summarize, 
                                    Nbirds = sum(Number))

# create a list of survey names
survs <- unique(survey_unresolved_sp$Survey_ID)
length(survs)

# create an empty dataframe to paste garden results into
survey_resolved_sp <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(survey_resolved_sp) <- c("Site_code", "Species_ID", "Nbirds")

for (i in 1:length(survs)){
  
  # Pull survey name
  surv_ID <- survs[[i]]
  
  # Subset to data from that survey
  surv <- subset(survey_unresolved_sp, survey_unresolved_sp$Survey_ID==surv_ID)
  
  # Get list of species from that survey
  surv.sp <- surv$Species_ID
  
  # Remove any unresolved IDs that can't be a species observed already at the site
  
  # If there is a RTHA or RSHA in the survey, remove any observations of "Buteo sp."
  if ("RTHA" %in% surv.sp | "RSHA" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Buteo sp.")
  }
  
  # If there is a ECDO, MODO, ROPI, BTPI in the survey, remove any observations of "Columbidae sp."
  if ("ECDO" %in% surv.sp | "MODO" %in% surv.sp | "ROPI" %in% surv.sp | "BTPI" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Columbidae sp.")
  }
  
  # If there is a crow or raven in the survey, remove any observations of "Corvus sp."
  if ("AMCR" %in% surv.sp | "CORA" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Corvus sp.")
  }
  
  # If there is a WEGU or CAGU in the survey, remove any observations of "Gull sp."
  if ("WEGU" %in% surv.sp | "CAGU" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Gull sp.")
  }
  
  # If there is a RTHA, RSHA, COHA, remove any observations of "Hawk sp."
  if ("RTHA" %in% surv.sp | "RSHA" %in% surv.sp | "COHA" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Hawk sp.")
  }
  
  # If there is a BASW, CLSW remove any observations of "Hirundinidae sp."
  if ("BASW" %in% surv.sp | "CLSW" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Hirundinidae sp.")
  }
  
  
  # If there is a HOFI or HOSP in the survey, remove any observations of "Passerida sp."
  if ("HOFI" %in% surv.sp | "HOSP" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Passerida sp.")
  }
  
  # If there is a DOWO or NUWO in the survey, remove any observations of "Picoides sp."
  if ("DOWO" %in% surv.sp | "NUWO" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Picoides sp.")
  }
  
  # If there is a WCSP, SOSP, HOSP in the survey, remove any observations of "Sparrow sp."
  if ("WCSP" %in% surv.sp | "SOSP" %in% surv.sp | "HOSP" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Sparrow sp.")
  }
  
  # If there is a CATE, ELTE in the survey, remove any observations of "Sternidae sp."
  if ("CATE" %in% surv.sp | "ELTE" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Sternidae sp.")
  }
  
  # If there is a ANHU, ALHU in the survey, remove any observations of "Trochilidae sp."
  if ("ANHU" %in% surv.sp | "ALHU" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Trochilidae sp.")
  }
  
  # Cut any "unknown" designations
  surv <- subset(surv, surv$Species_ID != "Unknown")
  
  # Add this survey to the dataframe
  survey_resolved_sp <- rbind(survey_resolved_sp, surv)
}

head(survey_resolved_sp)
dim(survey_resolved_sp)
length(unique(survey_resolved_sp$Survey_ID))

survey_resolved_sr <- count(survey_resolved_sp, Survey_ID, sort = TRUE, name = "SR_point") 
survey_resolved_sr


survey.gb_resolved_sp <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(survey.gb_resolved_sp) <- c("Site_code", "Species_ID", "Nbirds")

for (i in 1:length(survs)){
  
  # Pull survey.gb name
  surv_ID <- survs[[i]]
  
  # Subset to data from that survey.gb
  surv <- subset(survey.gb_unresolved_sp, survey.gb_unresolved_sp$Survey_ID==surv_ID)
  
  # Get list of species from that survey.gb
  surv.sp <- surv$Species_ID
  
  # Remove any unresolved IDs that can't be a species observed already at the site
  
  # If there is a RTHA or RSHA in the survey.gb, remove any observations of "Buteo sp."
  if ("RTHA" %in% surv.sp | "RSHA" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Buteo sp.")
  }
  
  # If there is a ECDO, MODO, ROPI, BTPI in the survey.gb, remove any observations of "Columbidae sp."
  if ("ECDO" %in% surv.sp | "MODO" %in% surv.sp | "ROPI" %in% surv.sp | "BTPI" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Columbidae sp.")
  }
  
  # If there is a crow or raven in the survey.gb, remove any observations of "Corvus sp."
  if ("AMCR" %in% surv.sp | "CORA" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Corvus sp.")
  }
  
  # If there is a WEGU or CAGU in the survey.gb, remove any observations of "Gull sp."
  if ("WEGU" %in% surv.sp | "CAGU" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Gull sp.")
  }
  
  # If there is a RTHA, RSHA, COHA, remove any observations of "Hawk sp."
  if ("RTHA" %in% surv.sp | "RSHA" %in% surv.sp | "COHA" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Hawk sp.")
  }
  
  # If there is a BASW, CLSW remove any observations of "Hirundinidae sp."
  if ("BASW" %in% surv.sp | "CLSW" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Hirundinidae sp.")
  }
  
  
  # If there is a HOFI or HOSP in the survey.gb, remove any observations of "Passerida sp."
  if ("HOFI" %in% surv.sp | "HOSP" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Passerida sp.")
  }
  
  # If there is a DOWO or NUWO in the survey.gb, remove any observations of "Picoides sp."
  if ("DOWO" %in% surv.sp | "NUWO" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Picoides sp.")
  }
  
  # If there is a WCSP, SOSP, HOSP in the survey.gb, remove any observations of "Sparrow sp."
  if ("WCSP" %in% surv.sp | "SOSP" %in% surv.sp | "HOSP" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Sparrow sp.")
  }
  
  # If there is a CATE, ELTE in the survey.gb, remove any observations of "Sternidae sp."
  if ("CATE" %in% surv.sp | "ELTE" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Sternidae sp.")
  }
  
  # If there is a ANHU, ALHU in the survey.gb, remove any observations of "Trochilidae sp."
  if ("ANHU" %in% surv.sp | "ALHU" %in% surv.sp) {
    surv <- subset(surv, surv$Species_ID != "Trochilidae sp.")
  }
  
  # Cut any "unknown" designations
  surv <- subset(surv, surv$Species_ID != "Unknown")
  
  # Add this survey.gb to the dataframe
  survey.gb_resolved_sp <- rbind(survey.gb_resolved_sp, surv)
}

head(survey.gb_resolved_sp)
dim(survey.gb_resolved_sp)
length(unique(survey.gb_unresolved_sp$Survey_ID)) # 119
length(unique(survey.gb_resolved_sp$Survey_ID))# 119

survey.gb_resolved_sr <- count(survey.gb_resolved_sp, Survey_ID, sort = TRUE, name = "SR_point_gb") 
survey.gb_resolved_sr

survey.gb_resolved_sr <- rbind(survey.gb_resolved_sr, c("FLFA_1507_o", 0))
dim(survey.gb_resolved_sr)

survey.gb_resolved_sr$SR_point_gb <- as.numeric(survey.gb_resolved_sr$SR_point_gb)


## Calculate # observers

unique(envsurv$Observers)

envsurv$n_observer <- ifelse((envsurv$Observers == "Kelley, Maya" | envsurv$Observers == "Kelley, Olive" | envsurv$Observers == "Kelley, David"), 2,
                             ifelse((envsurv$Observers == "Kelley, Maya, Lydia" | envsurv$Observers == "Kelley, Maya, Ale" | envsurv$Observers == "Kelley, Maya, Oliver"), 3, 1))

## Calculate presence of Kelley + Maya (two primary surveryers)
envsurv$kel <- ifelse(envsurv$Observers == "Kelley, Maya" | envsurv$Observers == "Kelley, Olive" | envsurv$Observers == "Kelley, David" |
                        envsurv$Observers == "Kelley, Maya, Lydia" | envsurv$Observers == "Kelley, Maya, Ale" | envsurv$Observers == "Kelley, Maya, Oliver" 
                      | envsurv$Observers == "Kelley" | envsurv$Observers == "Kelley ", 1, 0)
envsurv$may <- ifelse(envsurv$Observers == "Kelley, Maya" | envsurv$Observers == "Kelley, Maya, Lydia" | envsurv$Observers == "Kelley, Maya, Ale" 
                      | envsurv$Observers == "Kelley, Maya, Oliver" | envsurv$Observers == "Maya, (Kelley)" | envsurv$Observers == "Maya", 1, 0)

## Parse as dates and times
envsurv$Date.format <- dmy(envsurv$Date)
envsurv$Date.sec <- as.numeric(as.POSIXct(envsurv$Date.format, format="%Y-%m-%d"))
envsurv$Time.format <- format(strptime(sprintf('%04d', envsurv$Time), format='%H%M'), '%H:%M')
envsurv$Time.sec <- as.numeric(as.POSIXct(envsurv$Time.format, format="%H:%M"))

## Merge all together
envsurv.sm <- envsurv[,c("Survey_ID", "Site_code", "Temperature", "n_observer", "Survey_type", "Date.format", "Time.format", "Time.sec", "Date.sec", "kel", "may")]
surv.div.env <- merge(envsurv.sm, survey_n, by = c("Survey_ID"))
surv.div.env2 <- merge(surv.div.env, survey.gb_n, by = c("Survey_ID"))
surv.div.env3 <- merge(surv.div.env2, survey_resolved_sr, by = c("Survey_ID"))
surv.div.env4 <- merge(surv.div.env3, survey.gb_resolved_sr, by = c("Survey_ID"))

head(surv.div.env4)
dim(surv.div.env4) # 120


## Plot

# Temperature

SRxtemp <- ggplot(surv.div.env4, aes(x=Temperature, y=SR_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Species Richness", x = "Survey Temperature (F)") + 
  theme_minimal()
SRxtemp
# fewer species when it's hotter rip

Nxtemp <- ggplot(surv.div.env4, aes(x=Temperature, y=Nbirds_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Relative Abundance", x = "Survey Temperature (F)") + 
  theme_minimal()
Nxtemp
# strong relationship, way fewer birds when it's hot double rip


SRxtemp2 <- ggplot(surv.div.env4, aes(x=Temperature, y=SR_point_gb)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Species Richness", x = "Survey Temperature (F)") + 
  theme_minimal()
SRxtemp2
# way weaker

Nxtemp2 <- ggplot(surv.div.env4, aes(x=Temperature, y=Nbirds_point_gb)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Relative Abundance", x = "Survey Temperature (F)") + 
  theme_minimal()
Nxtemp2
# Negative, with one strong outlier


## Number of observers

SRxobs <- ggplot(surv.div.env4, aes(x=n_observer, y=SR_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Species Richness", x = "Number of Observers") + 
  theme_minimal()
SRxobs
# potentially more species with 2 than 1 observer

Nxobs <- ggplot(surv.div.env4, aes(x=n_observer, y=Nbirds_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Relative Abundance", x = "Number of Observers") + 
  theme_minimal()
Nxobs
# potentially more individuals as well

SRxobs2 <- ggplot(surv.div.env4, aes(x=n_observer, y=SR_point_gb)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Species Richness", x = "Number of Observers") + 
  theme_minimal()
SRxobs2
# potentially more species with 2 than 1 observer

Nxobs2 <- ggplot(surv.div.env4, aes(x=n_observer, y=Nbirds_point_gb)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Relative Abundance", x = "Number of Observers") + 
  theme_minimal()
Nxobs2
# potentially more individuals as well


## Date

SRxdate <- ggplot(surv.div.env4, aes(x=Date.format, y=SR_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Species Richness", x = "Survey Date") + 
  theme_minimal()
SRxdate
# Doesn't look super diff

Nxdate <- ggplot(surv.div.env4, aes(x=Date.format, y=Nbirds_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Relative Abundance", x = "Survey Date") + 
  theme_minimal()
Nxdate
# Abundance looks quite a bit lower in fall.... (that's also when I did all my solo observations, so may be confounding)

SRxdate2 <- ggplot(surv.div.env4, aes(x=Date.format, y=SR_point_gb)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Species Richness", x = "Survey Date") + 
  theme_minimal()
SRxdate2
# Doesn't look super diff

Nxdate2 <- ggplot(surv.div.env4, aes(x=Date.format, y=Nbirds_point_gb)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Relative Abundance", x = "Survey Date") + 
  theme_minimal()
Nxdate2
# Abundance looks quite a bit lower in fall.... (that's also when I did all my solo observations, so may be confounding)


## Time

SRxtime <- ggplot(surv.div.env4, aes(x=Time.sec, y=SR_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Species Richness", x = "Survey time") + 
  theme_minimal()
SRxtime
# Doesn't look super diff

Nxtime <- ggplot(surv.div.env4, aes(x=Time.sec, y=Nbirds_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Relative Abundance", x = "Survey time") + 
  theme_minimal()
Nxtime
# potentially some effect but it's not strong

SRxtime2 <- ggplot(surv.div.env4, aes(x=Time.sec, y=SR_point_gb)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Species Richness", x = "Survey time") + 
  theme_minimal()
SRxtime2
# Doesn't look super diff

Nxtime2 <- ggplot(surv.div.env4, aes(x=Time.sec, y=Nbirds_point_gb)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Relative Abundance", x = "Survey time") + 
  theme_minimal()
Nxtime2
# potentially some effect but it's not strong


## Correlations

## Temperature

# Pearson correlation
# Assumptions:
# Continuous variable: yes
# No outliers: there sort of are temperature outliers.....could try a correlation that's more robust to outliers?
# Random sample: yes
# Expect a linear relationship: yes
# Each variable normally distributed: 
hist(surv.div.env4$Temperature) # skewed but w/e
hist(surv.div.env4$Nbirds_point) # skewed but w/e
hist(surv.div.env4$SR_point) # skewed but w/e
hist(surv.div.env4$SR_point_gb) # skewed but w/e
hist(surv.div.env4$Nbirds_point_gb) # skewed but w/e

cor.test(surv.div.env4$Temperature, surv.div.env4$SR_point, method=c("pearson"))
# -0.17, p = .07
# insignificant!

cor.test(surv.div.env4$Temperature, surv.div.env4$SR_point_gb, method=c("pearson"))
# -0.06, p = .52
# insignificant!

cor.test(surv.div.env4$Temperature, surv.div.env4$Nbirds_point, method=c("pearson"))
# -0.32, p = .00064
# negative and significant  
# requires further investigation

cor.test(surv.div.env4$Temperature, surv.div.env4$Nbirds_point_gb, method=c("pearson"))
# -0.19, p = .03
# negative and significant  
# requires further investigation

## N observers

#Kendall's correlation
# non-parametric, doesn't require continuous data
# Is data ranked?: yes, ordinal
# Is the data monotonic?: expect quasi-linear relationship

cor.test(surv.div.env4$n_observer, surv.div.env4$SR_point, method=c("kendall"))
# 0.23, p = .002
# sig

cor.test(surv.div.env4$n_observer, surv.div.env4$SR_point_gb, method=c("kendall"))
# 0.16, p = .04
# sig

cor.test(surv.div.env4$n_observer, surv.div.env4$Nbirds_point, method=c("kendall"))
# 0.3, p < .0001
# sig

cor.test(surv.div.env4$n_observer, surv.div.env4$Nbirds_point_gb, method=c("kendall"))
# 0.22, p = .003
# sig


## Date

# Pearson correlation
# Assumptions:
# Continuous variable: yes
# No outliers: nah
# Random sample: yes
# Expect a linear relationship: yes, from graph
# Each variable normally distributed: no lol data gap, dates should be randomly dist, but this isn't a requirement

cor.test(surv.div.env4$Date.sec, surv.div.env4$SR_point, method=c("pearson"))
# -0.17, p = .06
# insignificant!

cor.test(surv.div.env4$Date.sec, surv.div.env4$SR_point_gb, method=c("pearson"))
# -0.04, p = .68
# insignificant!

cor.test(surv.div.env4$Date.sec, surv.div.env4$Nbirds_point, method=c("pearson"))
# -0.31, p = .0004
# Significant and negative, but probably also highly correlated w/# observers, because 1 observer only after sept...

cor.test(surv.div.env4$Date.sec, surv.div.env4$Nbirds_point_gb, method=c("pearson"))
# -0.16, p = .07
# insig

datexobserver <- ggplot(surv.div.env4, aes(x=Date.sec, y=n_observer)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "N observer", x = "Survey date") + 
  theme_minimal()
datexobserver

## Time

# Pearson correlation
# Assumptions:
# Continuous variable: yes
# No outliers: yest
# Random sample: yes
# Expect a linear relationship: maybe parabolic??
# Each variable normally distributed: 
hist(surv.div.env4$Time.sec) # absolutely not, it's bimodal bc of sampling scheme
# for others, see above

# -> Use a non-parametric correlation test instead. Spearman's or Kendall's are both options, but Kendall's is more robust, so let's use it (source: https://www.phdata.io/blog/data-science-stats-review/#:~:text=Spearman's%20Rank%20Correlation,preferred%20method%20of%20the%20two.)

#Kendall's correlation
# non-parametric, doesn't require continuous data
# Is data ranked?: yes
# Is the data monotonic?: no, parabola

# to deal w/non-monotonic data, split into 2 monotonic sections
# Will need to change this number every time, because every time re-calculate time in seconds it will use today's date and change 
# Find time that correponds to noon
noon <- as.numeric(as.POSIXct("12:00", format="%H:%M"))
am <- subset(surv.div.env4, surv.div.env4$Time.sec<noon)
pm <- subset(surv.div.env4, surv.div.env4$Time.sec>noon)
dim(am)
dim(pm)

cor.test(am$Time.sec, am$SR_point, method=c("kendall"))
# -0.14, p = .11
# nonsig

cor.test(pm$Time.sec, pm$SR_point, method=c("kendall"))
# 0.16, p = .87
# nonsig

cor.test(am$Time.sec, am$SR_point_gb, method=c("kendall"))
# -0.06, p = .54
# nonsig

cor.test(pm$Time.sec, pm$SR_point_gb, method=c("kendall"))
# 0.0006, p = .99
# nonsig

cor.test(am$Time.sec, am$Nbirds_point, method=c("kendall"))
# -0.2, p = .02
# Significant and negative! 

cor.test(pm$Time.sec, pm$Nbirds_point, method=c("kendall"))
# -0.05, p = .56
# nonsig

cor.test(am$Time.sec, am$Nbirds_point_gb, method=c("kendall"))
# -0.11, p = .21
# Significant and negative! 

cor.test(pm$Time.sec, pm$Nbirds_point_gb, method=c("kendall"))
# -0.016, p = .86
# nonsig

# Summary: Temperature is sig neg w/ # birds; N observers sig pos w/# of birds and SR, in the morning time is sig and neg w/ # of birds GBFO


## Let's look at distribution of each of those across farm ID

envsurv.point <- subset(envsurv, envsurv$Survey_ID %in% surv.div.env4$Survey_ID)
dim(envsurv.point)

# temperature
tempdist<-ggplot(envsurv.point, aes(x=Site_code, y=Temperature)) + 
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
tempdist

tempdist2<-ggplot(envsurv.point, aes(x=Site_code, y=Temperature)) + 
  geom_boxplot()
tempdist2
# means and distributions look variable (which in some cases may be because sites are colder...LAGR is actually the coolest site)
# but in some cases I sampled on super hot days, like at brooks park, POHI, FLFA

# n observers
obsdist<-ggplot(envsurv.point, aes(x=Site_code, y=n_observer)) + 
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
obsdist

obsdist2<-ggplot(envsurv.point, aes(x=Site_code, y=n_observer)) + 
  geom_boxplot()
obsdist2
 # different mean observers for each site

# time (in the morning). By design this should be evenly distributed across sites, so I'm not going to deal with it. 
envsurv.point.am <- subset(envsurv.point, envsurv.point$Survey_ID %in% am$Survey_ID)
amdist<-ggplot(envsurv.point.am, aes(x=Site_code, y=Time.sec)) + 
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
amdist

ammean<-ggplot(envsurv.point.am, aes(x=Site_code, y=Time.sec)) + 
  geom_boxplot()
ammean


## Calculate correction factors for confounding varialbes
# Average survey temperature at each site
# Total observers at each site

site.corrections <- plyr::ddply(envsurv, c("Site_code"), summarize,
                          avg_temp = mean(Temperature), 
                          tot_temp = sum(Temperature),
                          tot_obs = sum(n_observer))
site.corrections  

# look at aggregated relationships

pc.corr <-  merge(site.corrections, point.nsite, by = c("Site_code"))
pc.corr2 <- merge(pc.corr, point.srsite.resolve, by = c("Site_code"))

dim(pc.corr2) 
pc.corr2

Nxavgtemp <- ggplot(pc.corr2, aes(x=avg_temp, y=Nbirds_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Relative Abundance", x = "Average temperature (F)") + 
  theme_minimal()
Nxavgtemp
# maybe some positive relationship

SRxavgtemp <- ggplot(pc.corr2, aes(x=avg_temp, y=SR_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Species Richness", x = "Average temperature (F)") + 
  theme_minimal()
SRxavgtemp
# no relationship

Nxtottemp <- ggplot(pc.corr2, aes(x=tot_temp, y=Nbirds_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Relative Abundance", x = "Total temperature (F)") + 
  theme_minimal()
Nxtottemp
# Random

SRxtottemp <- ggplot(pc.corr2, aes(x=tot_temp, y=SR_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Species Richness", x = "Total temperature (F)") + 
  theme_minimal()
SRxtottemp
# negative

Nxtotobs <- ggplot(pc.corr2, aes(x=tot_obs, y=Nbirds_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Relative Abundance", x = "Total Observers") + 
  theme_minimal()
Nxtotobs
# Positive

SRxtotobs <- ggplot(pc.corr2, aes(x=tot_obs, y=SR_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Species Richness", x = "Total Observers") + 
  theme_minimal()
SRxtotobs
# looks kind of random

# write to csv
write.csv(pc.corr2, "point.nunres.srres.corrections.csv", row.names = FALSE)


pc.gb.corr <-  merge(site.corrections, point.gb.nsite, by = c("Site_code"))
pc.gb.corr2 <- merge(pc.gb.corr, point.gb.srsite.resolve, by = c("Site_code"))

dim(pc.gb.corr2) 
pc.gb.corr2

Nxavgtemp2 <- ggplot(pc.gb.corr2, aes(x=avg_temp, y=Nbirds_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Relative Abundance", x = "Average temperature (F)") + 
  theme_minimal()
Nxavgtemp2
# maybe some positive relationship

SRxavgtemp2 <- ggplot(pc.gb.corr2, aes(x=avg_temp, y=SR_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Species Richness", x = "Average temperature (F)") + 
  theme_minimal()
SRxavgtemp2
# no relationship

Nxtottemp2 <- ggplot(pc.gb.corr2, aes(x=tot_temp, y=Nbirds_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Relative Abundance", x = "Total temperature (F)") + 
  theme_minimal()
Nxtottemp2
# Random

SRxtottemp2 <- ggplot(pc.gb.corr2, aes(x=tot_temp, y=SR_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Species Richness", x = "Total temperature (F)") + 
  theme_minimal()
SRxtottemp2
# random

Nxtotobs2 <- ggplot(pc.gb.corr2, aes(x=tot_obs, y=Nbirds_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Relative Abundance", x = "Total Observers") + 
  theme_minimal()
Nxtotobs2
# Positive

SRxtotobs2 <- ggplot(pc.gb.corr2, aes(x=tot_obs, y=SR_point)) + 
  geom_point() +
  # geom_text(hjust=1, vjust=1) +
  labs(y = "Bird Species Richness", x = "Total Observers") + 
  theme_minimal()
SRxtotobs2
# looks kinda random

# write to csv
write.csv(pc.gb.corr2, "point.gb.nunres.srres.corrections.csv", row.names = FALSE)

### Second check on observer identity - Kelley vs. Maya
# There were two main observers, one of whom was present for every survey (~1/2 of surveys included both, most of the rest performed by Kelley with a few by Maya, and a few included additional observers)
# do note that this is complicated by different numbers of observers -- when Kelley was not present only Maya was present. 

## First check: were there differences in species and abundance detected in surveys where each of kelley and maya was present or not?
# Do this only for gbfo, point count surveys, as that is what was used in final modeling
surv.div.env4.pt <- subset(surv.div.env4, Survey_type=="Point")

# check for normality 
hist(subset(surv.div.env4.pt, kel==1)$Nbirds_point)
hist(subset(surv.div.env4.pt, kel==0)$Nbirds_point)
hist(subset(surv.div.env4.pt, may==1)$Nbirds_point)
hist(subset(surv.div.env4.pt, may==0)$Nbirds_point)
# - non-normal

hist(subset(surv.div.env4.pt, kel==1)$SR_point)
hist(subset(surv.div.env4.pt, kel==0)$SR_point)
hist(subset(surv.div.env4.pt, may==1)$SR_point)
hist(subset(surv.div.env4.pt, may==0)$SR_point)
# non-normal


# Model with site as a random effect, observer identity as fixed effects, on a per-survey basis
library("glmm")
surv.div.env4.pt$Nbirds_point_int <- as.integer(surv.div.env4.pt$Nbirds_point)
m1 <- glmm(Nbirds_point_int ~ kel + may, 
           random = list(Nbirds_point_int~ 0 + Site_code),
           m = 100000,
           varcomps.names = c("Site_code"),
          family.glmm = poisson.glmm,
          data = surv.div.env4.pt)
summary(m1)
# estimates are all 0, with high significance- don't know how to interpret that. 

# let's literally just look at numbers

# Average abundance in surveys where each of Kelley was present or absent
mean(subset(surv.div.env4.pt, kel==1)$Nbirds_point) # 30
mean(subset(surv.div.env4.pt, kel==0)$Nbirds_point) # 31
# Same for Maya
mean(subset(surv.div.env4.pt, may==1)$Nbirds_point) # 34
mean(subset(surv.div.env4.pt, may==0)$Nbirds_point) # 24
# Maya's presence means more abundance detected, but this is complicated by effect of number of observers

# Average SR in surveys where each of Kelley was present or absent
mean(subset(surv.div.env4.pt, kel==1)$SR_point) # 9
mean(subset(surv.div.env4.pt, kel==0)$SR_point) # 9
# Same for Maya
mean(subset(surv.div.env4.pt, may==1)$SR_point) # 10
mean(subset(surv.div.env4.pt, may==0)$SR_point) # 8
# Maya's presence means more species richness detected, but this is complicated by effect of number of observers

## Relationship between Maya's presence and number of observers
mean(subset(surv.div.env4.pt, may==1)$n_observer) # 2
mean(subset(surv.div.env4.pt, may==0)$n_observer) # 1
# Surveys where Maya was present had 2 people, surveys without her present had 1

## Is Maya's presence equally distributed across sites?
site.may <- plyr::ddply(surv.div.env4.pt, c("Site_code"), summarise, 
                                 may_avg=mean(may))
# No, 11 sites 3/6 visits, 8 sites 4/6 visits, 1 sites 2/6 visits


##############################
# Summary metrics 
##############################

# Use only resolved IDs!

# sp x site, with resolved IDs (already summed up)
head(point.gb.spxsite.resolve)
head(point.spxsite.resolve)

# full survey, with resolved IDs (needs summing by species)
head(point.gb.resolved)
head(point.resolved)

## What are the most common species across all sites?
# Summing up species by site
point.gb.resolved.spsum <- plyr::ddply(point.gb.resolved, c("Species_ID"), summarize,
                                        N_birds = sum(Number))
dim(point.gb.resolved.spsum)
point.gb.resolved.spsum[order(-point.gb.resolved.spsum$N_birds),]
# just the top 10
point.gb.resolved.spsum[order(-point.gb.resolved.spsum$N_birds),][1:10,]


# Summing up species by site
point.resolved.spsum <- plyr::ddply(point.resolved, c("Species_ID"), summarize,
                                        N_birds = sum(Number))
dim(point.resolved.spsum)
point.resolved.spsum[order(-point.resolved.spsum$N_birds),]
# just the top 10
point.resolved.spsum[order(-point.resolved.spsum$N_birds),][1:10,]
# rarest 
subset(point.resolved.spsum, point.resolved.spsum$N_birds<10)

write.csv(point.resolved.spsum, "point.resolved.spsum.csv", row.names = FALSE)


# sum up unresolved species (point gbfo)
point.unresolved.spsum <- plyr::ddply(point, c("Species_ID"), summarize,
                                    N_birds = sum(Number))
point.unresolved.spsum[order(-point.unresolved.spsum$N_birds),]

write.csv(point.unresolved.spsum, "point.unresolved.spsum.csv", row.names = FALSE)


## Which of our study species are in which garden?
focal.ids <- c("RTHA", "AMCR", "CASJ", "NOMO", "AMRO", "HOFI", "LEGO", "ANHU", "HOSP", "BLPH")
# subset to focal species
point.gb.focal <- subset(point.gb.spxsite.resolve, point.gb.spxsite.resolve$Species_ID %in% focal.ids)
dim(point.gb.focal)
head(point.gb.focal)
point.gb.focal

write.csv(point.gb.focal, "point.gb.focalspn.csv", row.names = FALSE)

point.gbfo.focal <- subset(point.spxsite.resolve, point.spxsite.resolve$Species_ID %in% focal.ids)
dim(point.gbfo.focal)
head(point.gbfo.focal)
point.gbfo.focal

write.csv(point.gbfo.focal, "point.gbfo.focalspn.csv", row.names = FALSE)


# shape into an easier to look at dataframe
point.gb.focal.w <- dcast(point.gb.focal, Site_code ~ Species_ID, mean)
point.gb.focal.w[is.na(point.gb.focal.w)] <- 0
dim(point.gb.focal.w)
point.gb.focal.w

write.csv(point.gb.focal.w, "point.gb.focalspn.w.csv", row.names = FALSE)

point.gbfo.focal.w <- dcast(point.gbfo.focal, Site_code ~ Species_ID, mean)
point.gbfo.focal.w[is.na(point.gbfo.focal.w)] <- 0
dim(point.gbfo.focal.w)
point.gbfo.focal.w

write.csv(point.gbfo.focal.w, "point.gbfo.focalspn.w.csv", row.names = FALSE)

## How many gardens are each of the study species in? 
count(point.gb.focal, Species_ID)
t(count(point.gb.focal, Species_ID))

count(point.gbfo.focal, Species_ID)
t(count(point.gbfo.focal, Species_ID))

## How many study species does each garden have?
count(point.gb.focal, Site_code)

count(point.gbfo.focal, Site_code)

## How many individuals of each study species does each garden have?
t(plyr::ddply(point.gb.focal, c("Species_ID"), summarize,
           n_birds = sum(Nbirds)))

t(plyr::ddply(point.gbfo.focal, c("Species_ID"), summarize,
              n_birds = sum(Nbirds)))


           
           

