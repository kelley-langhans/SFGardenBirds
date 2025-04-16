## SF Garden Birds main data analysis
# Script meant to be run after XX and XX script
# Author: Kelley Langhans
# Last updated: 3 April 2025

#################
# Set WD
#################
# Set working directory to whereever you have downloaded project data
# setwd("")

###################
# load libraries
##################

library(tidyverse)
library(openxlsx)
library(lubridate) # tidyverse
library(ggeffects)
library(abdiv)
library(corrplot)
library(car)
library(sf)
library(nlme)
library(biscale)

# not in final analyses for paper
library(gridExtra)
library(sp)
library(MASS)
library(betareg)
library(ggpubr)
library(reshape2)
library(factoextra)


##################
# read in data
##################

# income data (with all income metrics)
income <- read.csv("GardenIncome.csv")

# site level SR and N data, confounding survey corrections (control for temperature, observer)
# GBFO (garden boundaries + flyovers + outside)
# This is data used in final analysis for paper
point.gbfo.srn.corr <- read.csv("point.nunres.srres.corrections.csv")
# GB (just garden boundaries)
# Not used in final analysis, conservative estimate of SR and N
point.gb.srn.corr <- read.csv("point.gb.nunres.srres.corrections.csv")

# site level focal species abundance data
# GB
# Not used in final analysis
point.gb.focal <- read.csv("point.gb.focalspn.csv")
# GBFO
# Used in final analysis
point.gbfo.focal <- read.csv("point.gbfo.focalspn.csv")

# site level bird community data
point.gbfo.allsp <- read.csv("point.spxsite.resolve2.csv")

# species sentiment data
sp.sentiment.stats <- read.csv("sp.sentiment.stats.csv")

# Survey environmental data
# Site information
sitecodes <- read.xlsx("SFgardens_birdsurvey_data.xlsx", sheet = "Site_codes")
# Vegetation survey
vegsurv <- read.xlsx("SFgardens_vegsurvey_data.xlsx", sheet = "Survey_veg_data")
# distance to roads - generated in QGIS
road <- read.xlsx("SFgardens_vegsurvey_data.xlsx", sheet = "Roads")
# distance to ocean - generated in QGIS
ocean <- read.xlsx("SFgardens_vegsurvey_data.xlsx", sheet = "Oceans")
# distance to greenspace - generated in QGIS
greensp <- read.xlsx("SFgardens_vegsurvey_data.xlsx", sheet = "Parks")

# Data from QGIS
# Matching garden names and codes, and farm area calculated from QGIS
gar.name.area <- read.csv("garden_namematch_area.csv")

# Canopy cover, impervious surface, and non-tree vegetation from QGIS
imper <- read.csv("buff_500_percent_imp.csv")
canop <- read.csv("buff_500_percent_canopy_reproj.csv")
ntveg <- read.csv("buff_500_percent_nontreeveg.csv")

# Garden centroids
garden.pt <- st_read(dsn = "./SF_garden_centroids/", layer = "SF_gardens_26943")

##########################
# Examine data
########################

income
dim(income)
colnames(income) <- c("Garden.name", "neighborhood.medhhincome")
point.gbfo.srn.corr
dim(point.gbfo.srn.corr)
point.gb.srn.corr
dim(point.gb.srn.corr)
point.gbfo.focal
length(unique(point.gbfo.focal$Site_code))
length(unique(point.gbfo.focal$Species_ID))
point.gb.focal
length(unique(point.gb.focal$Site_code))
length(unique(point.gb.focal$Species_ID))
sp.sentiment.stats
sitecodes

head(vegsurv)
head(road)
head(ocean)
head(gar.name.area)

head(garden.pt)
plot(garden.pt$x, garden.pt$y)

############################
# Cleaning and merging data
###########################

## Biodiversity x site (+income, correction factors)
sitecodes2 <- sitecodes[,c(1:2)]
colnames(sitecodes2) <- c("Site_code", "Garden.name")
colnames(point.gb.srn.corr)[which(names(point.gb.srn.corr) == "Nbirds_point")] <- "Nbirds_gb"
colnames(point.gb.srn.corr)[which(names(point.gb.srn.corr) == "SR_point")] <- "SR_gb"
colnames(point.gbfo.srn.corr)[which(names(point.gbfo.srn.corr) == "Nbirds_point")] <- "Nbirds_gbfo"
colnames(point.gbfo.srn.corr)[which(names(point.gbfo.srn.corr) == "SR_point")] <- "SR_gbfo"
# garden.pt.df <- garden.pt@data[,c("Name", "x", "y")]
garden.pt.df <- `st_geometry<-` (garden.pt, NULL)
garden.pt.df <- garden.pt.df[,c("Name", "x", "y")]
colnames(garden.pt.df)[which(names(garden.pt.df) == "Name")] <- "Garden.name"

incomexdiv <- merge(sitecodes2, income, by = c("Garden.name"))
incomexdiv2 <- merge(incomexdiv, point.gb.srn.corr, by = c("Site_code"))
incomexdiv3 <- merge(incomexdiv2, point.gbfo.srn.corr, by = c("Site_code"))
incomexdiv3.5 <- merge(incomexdiv3, garden.pt.df, by = c("Garden.name"))

dim(incomexdiv3) 
incomexdiv3

# remove columns we don't need, clean up names
incomexdiv4 <- incomexdiv3.5[,!(names(incomexdiv3.5) %in% c("tot_temp.y", "avg_temp.y", "tot_obs.y"))]
colnames(incomexdiv4)[which(names(incomexdiv4) == "tot_temp.x")] <- "tot_temp"
colnames(incomexdiv4)[which(names(incomexdiv4) == "avg_temp.x")] <- "avg_temp"
colnames(incomexdiv4)[which(names(incomexdiv4) == "tot_obs.x")] <- "tot_obs"
colnames(incomexdiv4)[which(names(incomexdiv4) == "neighborhood.medhhincome")] <- "neighborhood.1627.medhhincome"
incomexdiv4

###############################################
# Exploring N and SR x income relationships
##############################################

##### Visualization

## N
# do with gb and gbfo

Ngbxinc1627 <- ggplot(incomexdiv4, aes(x=neighborhood.1627.medhhincome, y=Nbirds_gb, label=Site_code)) + 
  geom_point() +
  geom_text(hjust=1, vjust=1) +
  labs(y = "Relative bird abundance (gb)", x = "Average Gardener Income (1627m)") + 
  theme_minimal()
Ngbxinc1627

Ngbfoxinc1627 <- ggplot(incomexdiv4, aes(x=neighborhood.1627.medhhincome, y=Nbirds_gbfo, label=Site_code)) + 
  geom_point() +
  geom_text(hjust=1, vjust=1) +
  labs(y = "Relative bird abundance (gbfo)", x = "Average Gardener Income (1627m)") + 
  theme_minimal()
Ngbfoxinc1627

grid.arrange(Ngbxinc1627, Ngbfoxinc1627, nrow = 1)
# No clear pattern

## SR
# gb and gbfo

SRgbxinc1627 <- ggplot(incomexdiv4, aes(x=neighborhood.1627.medhhincome, y=SR_gb, label=Site_code)) + 
  geom_point() +
  geom_text(hjust=1, vjust=1) +
  labs(y = "Species richness (gb)", x = "Average Gardener Income (1627m)") + 
  theme_minimal()
SRgbxinc1627

SRgbfoxinc1627 <- ggplot(incomexdiv4, aes(x=neighborhood.1627.medhhincome, y=SR_gbfo, label=Site_code)) + 
  geom_point() +
  geom_text(hjust=1, vjust=1) +
  labs(y = "Species richness (gbfo)", x = "Average Gardener Income (1627m)") + 
  theme_minimal()
SRgbfoxinc1627

grid.arrange(SRgbxinc1627, SRgbfoxinc1627, nrow = 1)
# even less of a pattern

###### Data exploration

### N
## Cleveland Dotplots: checking for outliers

dotchart(incomexdiv4$Nbirds_gb)
dotchart(incomexdiv4$Nbirds_gbfo)
# there are 1-2 slight outliers in N

## Pairplots

pairs(incomexdiv4[,c("Nbirds_gb", "Nbirds_gbfo", 
                      "neighborhood.1627.medhhincome", 
                     "avg_temp", "tot_temp", "tot_obs")])
# as expected, high correlation between both N metrics
# Also corr btwn tot temp and tot obs, suggests better to use ag temp
# Nbirds and tot obs

### SR
## Cleveland Dotplots: checking for outliers

dotchart(incomexdiv4$SR_gb)
dotchart(incomexdiv4$SR_gbfo)
# No outliers

## Pairplots

pairs(incomexdiv4[,c("SR_gb", "SR_gbfo", 
                      "neighborhood.1627.medhhincome", 
                     "avg_temp", "tot_temp", "tot_obs")])
# as expected, high correlation between both SR metrics
# Also corr btwn tot temp and tot obs, suggests better to use ag temp
# SR and tot obs
# no non-linear relationships obvious

####### Model + check assumptions

### Abundance

# N vs. income with GB data (not in final paper)
# model(poisson)
ngb.in1627.pois <- glm(Nbirds_gb ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                      family = poisson,
                      data = incomexdiv4)
summary(ngb.in1627.pois)
# check for overdispersion (variance> mean)
ngb.in1627.pois$deviance/ngb.in1627.pois$df.residual
# much bigger than 1, looks overdispersed

# Try quasi-poisson
ngb.in1627.qpois <- glm(Nbirds_gb ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                       family = quasipoisson,
                       data = incomexdiv4)
summary(ngb.in1627.qpois)
par(mfrow=c(2,2))
plot(ngb.in1627.qpois)
# there might be a pattern in the residuals (-quadratic)
# no points with too much leverage
EP <- resid(ngb.in1627.qpois, type = "pearson") 
ED <- resid(ngb.in1627.qpois, type = "deviance") 
mu <- predict(ngb.in1627.qpois, type = "response") 
E <- incomexdiv4$Nbirds_gb - mu 
EP2 <- E / sqrt(7.630148 * mu) 
op <- par(mfrow = c(2, 2)) 
plot(x = mu, y = E, main = "Response residuals") 
plot(x = mu, y = EP, main = "Pearson residuals") 
plot(x = mu, y = EP2, main = "Pearson residuals scaled") 
plot(x = mu, y = ED, main = "Deviance residuals") 
par(op)
# looks like there is a pattern

# Try negative binomial 
ngb.in1627.nb <- glm.nb(Nbirds_gb ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                       link = "log",
                       data = incomexdiv4)
summary(ngb.in1627.nb)
# check for overdispersion
ngb.in1627.nb$deviance/ngb.in1627.nb$df.residual
vif(ngb.in1627.nb)

# just minor
# check for patterns in residuals
plot(ngb.in1627.nb)
# not bad

#plot
df <- ggpredict(ngb.in1627.nb, terms=c("neighborhood.1627.medhhincome"))
ngb.in1627.nb.plt <- ggplot(df, aes(x = x, y = predicted)) +
  geom_line() +
  geom_point(aes(y = Nbirds_gb, x = neighborhood.1627.medhhincome), data = incomexdiv4) +
  labs(y = 'Nbirds_gb', x = 'neighborhood.1627.medhhincome') +
  geom_ribbon(data = df, aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1) +
  theme_classic()
ngb.in1627.nb.plt

## N vs. income GBFO data (in paper)
# model(poisson)
ngbfo.in1627.pois <- glm(Nbirds_gbfo ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                       family = poisson,
                       data = incomexdiv4)
summary(ngbfo.in1627.pois)
# check for overdispersion (variance> mean)
ngbfo.in1627.pois$deviance/ngbfo.in1627.pois$df.residual
# much bigger than 1, looks overdispersed
vif(ngbfo.in1627.pois)

# Try quasi-poisson
ngbfo.in1627.qpois <- glm(Nbirds_gbfo ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                        family = quasipoisson,
                        data = incomexdiv4)
summary(ngbfo.in1627.qpois)
par(mfrow=c(2,2))
plot(ngbfo.in1627.qpois)
# less resid pattern
# no points with too much leverage
EP <- resid(ngbfo.in1627.qpois, type = "pearson") 
ED <- resid(ngbfo.in1627.qpois, type = "deviance") 
mu <- predict(ngbfo.in1627.qpois, type = "response") 
E <- incomexdiv4$Nbirds_gbfo - mu 
EP2 <- E / sqrt(7.630148 * mu) 
op <- par(mfrow = c(2, 2)) 
plot(x = mu, y = E, main = "Response residuals") 
plot(x = mu, y = EP, main = "Pearson residuals") 
plot(x = mu, y = EP2, main = "Pearson residuals scaled") 
plot(x = mu, y = ED, main = "Deviance residuals") 
par(op)
# looks like there is a pattern

# Try negative binomial - final model in paper
ngbfo.in1627.nb <- glm.nb(Nbirds_gbfo ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                        link = "log",
                        data = incomexdiv4)
summary(ngbfo.in1627.nb)
# check for overdispersion
ngbfo.in1627.nb$deviance/ngbfo.in1627.nb$df.residual
# just minor
# check for patterns in residuals
plot(ngbfo.in1627.nb)
# still pattern
vif(ngbfo.in1627.nb)

#plot
df <- ggpredict(ngbfo.in1627.nb, terms=c("neighborhood.1627.medhhincome"))
ngbfo.in1627.nb.plt <- ggplot(df, aes(x = x, y = predicted)) +
  geom_line() +
  geom_point(aes(y = Nbirds_gbfo, x = neighborhood.1627.medhhincome), data = incomexdiv4) +
  labs(y = 'Bird Abundance', x = 'Neighborhood Median Household Income ($)') +
  geom_ribbon(data = df, aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1) +
  theme_classic()
ngbfo.in1627.nb.plt


## R1 revision - investigate quadratic relationship as well
ngbfo.in1627.nb.quad <- glm.nb(Nbirds_gbfo ~ neighborhood.1627.medhhincome + I(neighborhood.1627.medhhincome^2) + avg_temp + tot_obs,
                          link = "log",
                          data = incomexdiv4)
summary(ngbfo.in1627.nb.quad)
# check for overdispersion
ngbfo.in1627.nb.quad$deviance/ngbfo.in1627.nb.quad$df.residual
# just minor
# check for patterns in residuals
plot(ngbfo.in1627.nb.quad)
# still pattern
vif(ngbfo.in1627.nb.quad) # large for linear and quadratic income terms.

#plot
df <- ggpredict(ngbfo.in1627.nb.quad, terms=c("neighborhood.1627.medhhincome"))
ngbfo.in1627.nb.quad.plt <- ggplot(df, aes(x = x, y = predicted)) +
  geom_line() +
  geom_point(aes(y = Nbirds_gbfo, x = neighborhood.1627.medhhincome), data = incomexdiv4) +
  labs(y = 'Bird Abundance', x = 'Neighborhood Median Household Income ($)') +
  geom_ribbon(data = df, aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1) +
  theme_classic()
ngbfo.in1627.nb.quad.plt


### Species Richness

# SR vs income, GB data (not in final paper)
# model(poisson)
srgb.in1627.pois <- glm(SR_gb ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                       family = poisson,
                       data = incomexdiv4)
summary(srgb.in1627.pois)
# check for overdispersion (variance> mean)
srgb.in1627.pois$deviance/srgb.in1627.pois$df.residual
# much bigger than 1, looks overdispersed

# Try quasi-poisson
srgb.in1627.qpois <- glm(SR_gb ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                        family = quasipoisson,
                        data = incomexdiv4)
summary(srgb.in1627.qpois)
par(mfrow=c(2,2))
plot(srgb.in1627.qpois)
# there might be a pattern in the residuals (-quadratic)
# no points with too much leverage
EP <- resid(srgb.in1627.qpois, type = "pearson") 
ED <- resid(srgb.in1627.qpois, type = "deviance") 
mu <- predict(srgb.in1627.qpois, type = "response") 
E <- incomexdiv4$SR_gb - mu 
EP2 <- E / sqrt(7.630148 * mu) 
op <- par(mfrow = c(2, 2)) 
plot(x = mu, y = E, main = "Response residuals") 
plot(x = mu, y = EP, main = "Pearson residuals") 
plot(x = mu, y = EP2, main = "Pearson residuals scaled") 
plot(x = mu, y = ED, main = "Deviance residuals") 
par(op)
# looks like there is a pattern

# Try negative binomial
srgb.in1627.nb <- glm.nb(SR_gb ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                        link = "log",
                        data = incomexdiv4)
summary(srgb.in1627.nb)
# check for overdispersion
srgb.in1627.nb$deviance/srgb.in1627.nb$df.residual
# just minor
vif(srgb.in1627.nb)
# check for patterns in residuals
plot(srgb.in1627.nb)
# still pattern

#plot
df <- ggpredict(srgb.in1627.nb, terms=c("neighborhood.1627.medhhincome"))
srgb.in1627.nb.plt <- ggplot(df, aes(x = x, y = predicted)) +
  geom_line() +
  geom_point(aes(y = SR_gb, x = neighborhood.1627.medhhincome), data = incomexdiv4) +
  labs(y = 'SR_gb', x = 'neighborhood.1627.medhhincome') +
  geom_ribbon(data = df, aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1) +
  theme_classic()
srgb.in1627.nb.plt

# SR vs income, GBFO data (in final paper)
# model(poisson)
srgbfo.in1627.pois <- glm(SR_gbfo ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                         family = poisson,
                         data = incomexdiv4)
summary(srgbfo.in1627.pois)
# check for overdispersion (variance> mean)
srgbfo.in1627.pois$deviance/srgbfo.in1627.pois$df.residual
# much bigger than 1, looks overdispersed

# Try quasi-poisson
srgbfo.in1627.qpois <- glm(SR_gbfo ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                          family = quasipoisson,
                          data = incomexdiv4)
summary(srgbfo.in1627.qpois)
par(mfrow=c(2,2))
plot(srgbfo.in1627.qpois)
# less resid pattern
# no points with too much leverage
EP <- resid(srgbfo.in1627.qpois, type = "pearson") 
ED <- resid(srgbfo.in1627.qpois, type = "deviance") 
mu <- predict(srgbfo.in1627.qpois, type = "response") 
E <- incomexdiv4$SR_gbfo - mu 
EP2 <- E / sqrt(7.630148 * mu) 
op <- par(mfrow = c(2, 2)) 
plot(x = mu, y = E, main = "Response residuals") 
plot(x = mu, y = EP, main = "Pearson residuals") 
plot(x = mu, y = EP2, main = "Pearson residuals scaled") 
plot(x = mu, y = ED, main = "Deviance residuals") 
par(op)
# looks like there is a pattern

# Try negative binomial - final model in paper
srgbfo.in1627.nb <- glm.nb(SR_gbfo ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                          link = "log",
                          data = incomexdiv4)
summary(srgbfo.in1627.nb)
# check for overdispersion
srgbfo.in1627.nb$deviance/srgbfo.in1627.nb$df.residual
# just minor
vif(srgbfo.in1627.nb)
# check for patterns in residuals
plot(srgbfo.in1627.nb)

df <- ggpredict(srgbfo.in1627.nb, terms=c("neighborhood.1627.medhhincome"))
srgbfo.in1627.nb.plt <- ggplot(df, aes(x = x, y = predicted)) +
  geom_line() +
  geom_point(aes(y = SR_gbfo, x = neighborhood.1627.medhhincome), data = incomexdiv4) +
  labs(y = 'Bird Species Richness', x = 'Neighborhood Median Household Income ($)') +
  geom_ribbon(data = df, aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1) +
  theme_classic()
srgbfo.in1627.nb.plt

## Revision round 1 update: try with quadratic term
srgbfo.in1627.nb.quad <- glm.nb(SR_gbfo ~ neighborhood.1627.medhhincome + I(neighborhood.1627.medhhincome^2) + avg_temp + tot_obs,
                           link = "log",
                           data = incomexdiv4)
summary(srgbfo.in1627.nb.quad)
# check for overdispersion
srgbfo.in1627.nb.quad$deviance/srgbfo.in1627.nb.quad$df.residual
# just minor
vif(srgbfo.in1627.nb.quad)
# check for patterns in residuals
plot(srgbfo.in1627.nb.quad)
# still pattern

df <- ggpredict(srgbfo.in1627.nb.quad, terms=c("neighborhood.1627.medhhincome"))
srgbfo.in1627.nb.quad.plt <- ggplot(df, aes(x = x, y = predicted)) +
  geom_line() +
  geom_point(aes(y = SR_gbfo, x = neighborhood.1627.medhhincome), data = incomexdiv4) +
  labs(y = 'Bird Species Richness', x = 'Neighborhood Median Household Income ($)') +
  geom_ribbon(data = df, aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1) +
  theme_classic()
srgbfo.in1627.nb.quad.plt


### Plot them all together

ggarrange(ngb.in1627.nb.plt, ngbfo.in1627.nb.plt,
          ncol=2, nrow = 1)

ggarrange(srgb.in1627.nb.plt, srgbfo.in1627.nb.plt,
          ncol=2, nrow = 1)

##############################
# Guild abundance vs. income
##############################

# Based on R1 revisions, checking how the abundance of species in different guilds is related to income
# pilot test with abundance of 5 synanthropic, 5 brush associated, 5 tree associated species
# all gbfo data, 1627 radius income

### Set up data

# make list of species in each group
synan <- c("HOFI", "NOMO", "HOSP", "EUST", "AMCR")
bush <- c("BUSH", "LEGO", "CATO", "ANHU", "ORJU")
tree <- c("NUWO", "ANHU", "CASJ", "PYNU", "CBCH")

# subset point count df by each species
point.synan <- subset(point.gbfo.allsp, Species_ID %in% synan)
point.bush <- subset(point.gbfo.allsp, Species_ID %in% bush)
point.tree <- subset(point.gbfo.allsp, Species_ID %in% tree)

# Calculate total number of birds in each species at each site
point.synan.n <- plyr::ddply(point.synan, "Site_code", summarize,
                             N_synan = sum(Nbirds))
point.bush.n <- plyr::ddply(point.bush, "Site_code", summarize,
                             N_bush = sum(Nbirds))
point.tree.n <- plyr::ddply(point.tree, "Site_code", summarize,
                             N_tree = sum(Nbirds))

# Merge back with the main dataframe
incomexdiv4.1 <- merge(incomexdiv4, point.synan.n, by=c("Site_code"))
incomexdiv4.2 <- merge(incomexdiv4.1, point.bush.n, by=c("Site_code"))
incomexdiv4.3 <- merge(incomexdiv4.2, point.tree.n, by=c("Site_code"))

### t test high vs low income sites in guild abundance differences

# Divide gardens into high and low income
incomexdiv4.3$neighborhood.1627.medhhincome.cat <- ifelse(incomexdiv4.3$neighborhood.1627.medhhincome > median(incomexdiv4.3$neighborhood.1627.medhhincome),
                                                          "high", 
                                                          "low")
# Checking t test assumption of normality
highincome <- subset(incomexdiv4.3, neighborhood.1627.medhhincome.cat=="high")
lowincome <- subset(incomexdiv4.3, neighborhood.1627.medhhincome.cat=="low")
hist(highincome$N_synan) # no
hist(lowincome$N_synan) # no
hist(highincome$N_bush) #no
hist(lowincome$N_bush)
hist(highincome$N_tree) # no
hist(lowincome$N_tree) # no

# Non parametric Mann-Whitney U
wilcox.test(N_synan ~ neighborhood.1627.medhhincome.cat, data = incomexdiv4.3, exact = FALSE) # NS
wilcox.test(N_bush ~ neighborhood.1627.medhhincome.cat, data = incomexdiv4.3, exact = FALSE) # NS
wilcox.test(N_tree ~ neighborhood.1627.medhhincome.cat, data = incomexdiv4.3, exact = FALSE) # NS

### regression of income vs. guild abundance

## Synanthropes

# model(poisson)
nsynan.pois <- glm(N_synan ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                         family = poisson,
                         data = incomexdiv4.3)
summary(nsynan.pois)
# check for overdispersion (variance> mean)
nsynan.pois$deviance/nsynan.pois$df.residual
# much bigger than 1, looks overdispersed
vif(nsynan.pois)

# Try quasi-poisson
nsynan.qpois <- glm(N_synan ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                          family = quasipoisson,
                          data = incomexdiv4.3)
summary(nsynan.qpois)
par(mfrow=c(2,2))
plot(nsynan.qpois)
# less resid pattern
# no points with too much leverage
EP <- resid(nsynan.qpois, type = "pearson") 
ED <- resid(nsynan.qpois, type = "deviance") 
mu <- predict(nsynan.qpois, type = "response") 
E <- incomexdiv4$Nbirds_gbfo - mu 
EP2 <- E / sqrt(7.630148 * mu) 
op <- par(mfrow = c(2, 2)) 
plot(x = mu, y = E, main = "Response residuals") 
plot(x = mu, y = EP, main = "Pearson residuals") 
plot(x = mu, y = EP2, main = "Pearson residuals scaled") 
plot(x = mu, y = ED, main = "Deviance residuals") 
par(op)
# looks like there is a pattern

# Try negative binomial
nsynan.nb <- glm.nb(N_synan ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                          link = "log",
                          data = incomexdiv4.3)
summary(nsynan.nb)
# check for overdispersion
nsynan.nb$deviance/nsynan.nb$df.residual
# just minor
# check for patterns in residuals
plot(nsynan.nb)
# still pattern
vif(nsynan.nb) # <2

#plot
df <- ggpredict(nsynan.nb, terms=c("neighborhood.1627.medhhincome"))
nsynan.nb.plt <- ggplot(df, aes(x = x, y = predicted)) +
  geom_line() +
  geom_point(aes(y = N_synan, x = neighborhood.1627.medhhincome), data = incomexdiv4.3) +
  labs(y = 'Synanthropic Bird Abundance', x = 'Neighborhood Median Household Income ($)') +
  geom_ribbon(data = df, aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1) +
  theme_classic()
nsynan.nb.plt

## Bush specialists

## Synanthropes

# model(poisson)
nbush.pois <- glm(N_bush ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                   family = poisson,
                   data = incomexdiv4.3)
summary(nbush.pois)
# check for overdispersion (variance> mean)
nbush.pois$deviance/nbush.pois$df.residual
# much bigger than 1, looks overdispersed
vif(nbush.pois)

# Try quasi-poisson
nbush.qpois <- glm(N_bush ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                    family = quasipoisson,
                    data = incomexdiv4.3)
summary(nbush.qpois)
par(mfrow=c(2,2))
plot(nbush.qpois)
# less resid pattern
# no points with too much leverage
EP <- resid(nbush.qpois, type = "pearson") 
ED <- resid(nbush.qpois, type = "deviance") 
mu <- predict(nbush.qpois, type = "response") 
E <- incomexdiv4$Nbirds_gbfo - mu 
EP2 <- E / sqrt(7.630148 * mu) 
op <- par(mfrow = c(2, 2)) 
plot(x = mu, y = E, main = "Response residuals") 
plot(x = mu, y = EP, main = "Pearson residuals") 
plot(x = mu, y = EP2, main = "Pearson residuals scaled") 
plot(x = mu, y = ED, main = "Deviance residuals") 
par(op)
# looks like there is a pattern

# Try negative binomial
nbush.nb <- glm.nb(N_bush ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                    link = "log",
                    data = incomexdiv4.3)
summary(nbush.nb)
# check for overdispersion
nbush.nb$deviance/nbush.nb$df.residual
# just minor
# check for patterns in residuals
plot(nbush.nb)
# still pattern
vif(nbush.nb) # <2

#plot
df <- ggpredict(nbush.nb, terms=c("neighborhood.1627.medhhincome"))
nbush.nb.plt <- ggplot(df, aes(x = x, y = predicted)) +
  geom_line() +
  geom_point(aes(y = N_bush, x = neighborhood.1627.medhhincome), data = incomexdiv4.3) +
  labs(y = 'Bush Specialist Bird Abundance', x = 'Neighborhood Median Household Income ($)') +
  geom_ribbon(data = df, aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1) +
  theme_classic()
nbush.nb.plt


## tree specialists

## Synanthropes

# model(poisson)
ntree.pois <- glm(N_tree ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                  family = poisson,
                  data = incomexdiv4.3)
summary(ntree.pois)
# check for overdispersion (variance> mean)
ntree.pois$deviance/ntree.pois$df.residual
# much bigger than 1, looks overdispersed
vif(ntree.pois)

# Try quasi-poisson
ntree.qpois <- glm(N_tree ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                   family = quasipoisson,
                   data = incomexdiv4.3)
summary(ntree.qpois)
par(mfrow=c(2,2))
plot(ntree.qpois)
# less resid pattern
# no points with too much leverage
EP <- resid(ntree.qpois, type = "pearson") 
ED <- resid(ntree.qpois, type = "deviance") 
mu <- predict(ntree.qpois, type = "response") 
E <- incomexdiv4$Nbirds_gbfo - mu 
EP2 <- E / sqrt(7.630148 * mu) 
op <- par(mfrow = c(2, 2)) 
plot(x = mu, y = E, main = "Response residuals") 
plot(x = mu, y = EP, main = "Pearson residuals") 
plot(x = mu, y = EP2, main = "Pearson residuals scaled") 
plot(x = mu, y = ED, main = "Deviance residuals") 
par(op)
# looks like there is a pattern

# Try negative binomial
ntree.nb <- glm.nb(N_tree ~ neighborhood.1627.medhhincome + avg_temp + tot_obs,
                   link = "log",
                   data = incomexdiv4.3)
summary(ntree.nb)
# check for overdispersion
ntree.nb$deviance/ntree.nb$df.residual
# just minor
# check for patterns in residuals
plot(ntree.nb)
# still pattern
vif(ntree.nb) # <2

#plot
df <- ggpredict(ntree.nb, terms=c("neighborhood.1627.medhhincome"))
ntree.nb.plt <- ggplot(df, aes(x = x, y = predicted)) +
  geom_line() +
  geom_point(aes(y = N_tree, x = neighborhood.1627.medhhincome), data = incomexdiv4.3) +
  labs(y = 'Tree Specialist Bird Abundance', x = 'Neighborhood Median Household Income ($)') +
  geom_ribbon(data = df, aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1) +
  theme_classic()
ntree.nb.plt

### Plot all
pdf("./Figures/Incomexguildabundanceplots.pdf", height = 6, width = 18)
ggarrange(nsynan.nb.plt, nbush.nb.plt, ntree.nb.plt,
          ncol=3, nrow = 1, 
          labels = c("A", "B", "C"))
dev.off()

####################################
# Species access scores vs. income
#################################

# Done separately for GB and GBFO data, but only GBFO data used in final MS.

# merge sentiment score w/ species abundance x garden df by species ID
point.gb.focal.sent <- merge(point.gb.focal, sp.sentiment.stats, by = c("Species_ID"), all.x = TRUE)
point.gbfo.focal.sent <- merge(point.gbfo.focal, sp.sentiment.stats, by = c("Species_ID"), all.x = TRUE)

# make access score component column, abundance x sentiment score
# #if there are 0 birds of one species, will be lower (weighted by overall abundance)
point.gb.focal.sent$sentxn <- point.gb.focal.sent$Nbirds*point.gb.focal.sent$sentscore_weight
point.gbfo.focal.sent$sentxn <- point.gbfo.focal.sent$Nbirds*point.gbfo.focal.sent$sentscore_weight

# sum up all access score components by garden 
garden.gb.spaccess <- plyr::ddply(point.gb.focal.sent, c("Site_code"), summarise, 
                            spaccess_gb = sum(sentxn),
                            Nbirds_focal_gb = sum(Nbirds))

garden.gbfo.spaccess <- plyr::ddply(point.gbfo.focal.sent, c("Site_code"), summarise, 
                                  spaccess_gbfo = sum(sentxn),
                                  Nbirds_focal_gbfo = sum(Nbirds))

# combine w/main df
incomexdiv5 <- merge(incomexdiv4, garden.gb.spaccess, by=c("Site_code"), all.x = TRUE)
incomexdiv6 <- merge(incomexdiv5, garden.gbfo.spaccess, by=c("Site_code"), all.x = TRUE)
dim(incomexdiv4)
dim(incomexdiv6)

# Will species access metric just be a measure of abundance?

# Metric 1
ggplot(incomexdiv6, aes(x = Nbirds_gb, y = spaccess_gb)) +
  geom_point() 
# super strong relationship

ggplot(incomexdiv6, aes(x = Nbirds_gbfo, y = spaccess_gbfo)) +
  geom_point() 
# also pretty darn strong!


#### Modeling time

# Take a look at relationships
pairs(incomexdiv6[,-c(1:2)])

## Species access metric vs. income (sentiment score*N summed for focal species)
# Continuous metric, linear model should be appropriate as long as assumptions are met

# GB, 1627
m1gb.in1627.lm <- lm(spaccess_gb ~ neighborhood.1627.medhhincome + avg_temp + tot_obs, data=incomexdiv6)
summary(m1gb.in1627.lm)
#checking assumptions
par(mfrow=c(2,2))
plot(m1gb.in1627.lm)
# Some evidence for non-normality in QQ plot, non-homogeneity of variance (fanning residuals, only 2 points...)

## Final model in paper
# gbfo, 1627
m1gbfo.in1627.lm <- lm(spaccess_gbfo ~ neighborhood.1627.medhhincome + avg_temp + tot_obs, data=incomexdiv6)
summary(m1gbfo.in1627.lm)
#checking assumptions
par(mfrow=c(2,2))
plot(m1gbfo.in1627.lm)
# point 10 has big residual. It's a bit of an outlier, but not a huge one--deciding not to transform data.

## For R1, add quadratic term
m1gbfo.in1627.lm.quad <- lm(spaccess_gbfo ~ neighborhood.1627.medhhincome + I(neighborhood.1627.medhhincome^2) + avg_temp + tot_obs, data=incomexdiv6)
summary(m1gbfo.in1627.lm.quad)


############# Plot models

df <- ggpredict(m1gb.in1627.lm, terms=c("neighborhood.1627.medhhincome"))
m1gb.in1627.lm.plt <- ggplot(df, aes(x = x, y = predicted)) +
  geom_line() +
  geom_point(aes(y = spaccess_gb, x = neighborhood.1627.medhhincome), data = incomexdiv6) +
  labs(y = 'Species Access Metric (gb)', x = 'neighborhood.1627.medhhincome') +
  geom_ribbon(data = df, aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1) +
  theme_classic()
m1gb.in1627.lm.plt

df <- ggpredict(m1gbfo.in1627.lm, terms=c("neighborhood.1627.medhhincome"))
m1gbfo.in1627.lm.plt <- ggplot(df, aes(x = x, y = predicted)) +
  geom_line() +
  geom_point(aes(y = spaccess_gbfo, x = neighborhood.1627.medhhincome), data = incomexdiv6) +
  labs(y = 'Species Access Metric', x = 'Neighborhood Median Household Income ($)') +
  geom_ribbon(data = df, aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1) +
  theme_classic()
m1gbfo.in1627.lm.plt

ggarrange(m1gb.in1627.lm.plt, m1gbfo.in1627.lm.plt,
          ncol=2, nrow = 1)

##quadratic
df <- ggpredict(m1gbfo.in1627.lm.quad, terms=c("neighborhood.1627.medhhincome"))
m1gbfo.in1627.lm.quad.plt <- ggplot(df, aes(x = x, y = predicted)) +
  geom_line() +
  geom_point(aes(y = spaccess_gbfo, x = neighborhood.1627.medhhincome), data = incomexdiv6) +
  labs(y = 'Species Access Metric', x = 'Neighborhood Median Household Income ($)') +
  geom_ribbon(data = df, aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1) +
  theme_classic()
m1gbfo.in1627.lm.quad.plt


###### Make one big plot of all final models for paper

df <- ggpredict(ngbfo.in1627.nb, terms=c("neighborhood.1627.medhhincome"))
ngbfo.in1627.nb.plt <- ggplot(df, aes(x = x, y = predicted)) +
  geom_line() +
  geom_point(aes(y = Nbirds_gbfo, x = neighborhood.1627.medhhincome), data = incomexdiv4, size=4) +
  labs(y = 'Bird Abundance', x = 'Neighborhood Median Household Income ($)') +
  geom_ribbon(data = df, aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1) +
  xlim(80000, 180000) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 18),
        axis.title.x = element_text(vjust=-0.75))
ngbfo.in1627.nb.plt

df <- ggpredict(srgbfo.in1627.nb, terms=c("neighborhood.1627.medhhincome"))
srgbfo.in1627.nb.plt <- ggplot(df, aes(x = x, y = predicted)) +
  geom_line() +
  geom_point(aes(y = SR_gbfo, x = neighborhood.1627.medhhincome), data = incomexdiv4, size = 4) +
  labs(y = 'Bird Species Richness', x = 'Neighborhood Median Household Income ($)') +
  geom_ribbon(data = df, aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1) +
  xlim(80000, 180000) +
  theme_classic()+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 18),
        axis.title.x = element_text(vjust=-0.75))
srgbfo.in1627.nb.plt

df <- ggpredict(m1gbfo.in1627.lm, terms=c("neighborhood.1627.medhhincome"))
m1gbfo.in1627.lm.plt <- ggplot(df, aes(x = x, y = predicted)) +
  geom_line() +
  geom_point(aes(y = spaccess_gbfo, x = neighborhood.1627.medhhincome), data = incomexdiv6, size = 4) +
  labs(y = 'Species Access Metric', x = 'Neighborhood Median Household Income ($)') +
  geom_ribbon(data = df, aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1) +
  xlim(80000, 180000) +
  theme_classic()+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 18),
        axis.title.x = element_text(vjust=-0.75))
m1gbfo.in1627.lm.plt

pdf("./Figures/Incomexaccessplots.pdf", height = 6, width = 18)
ggarrange(ngbfo.in1627.nb.plt, srgbfo.in1627.nb.plt, m1gbfo.in1627.lm.plt,
          ncol=3, nrow = 1)
dev.off()


################################ Vegetation x biodiversity analysis ###############################################

##########################
# Cleaning and merging data
############################

# Get basic veg data all together in 1 df 

# Matching names for merging
garden.names <- gar.name.area[,c("Name", "Site_code")]
garden.names[garden.names=="KOPA"] <- "KOSH"

# Making landscape scales variables into small clean dfs with garden codes
# percent canopy cover in 500m buffer
canop1 <- canop[,c("Name", "percent_canopy")]
canop1[canop1=="Visitación Valley Greenway Community Garden"] <- "Visitacion Valley Greenway Community Garden"
canop2 <- merge(canop1, garden.names, by=c("Name"))
canop3 <- canop2[,c("Site_code", "percent_canopy")]
# change proportion to percent
canop3$percent_canopy <- canop3$percent_canopy*100

# percent impervious surface in 500m buffer
imper1 <- imper[,c("Name", "percent_imp")]
imper1[imper1=="Portrero Hill Community Garden"] <- "Potrero Hill Community Garden"
imper1[imper1=="Dogpath/Miller Memorial Community Garden"] <- "Dogpatch/Miller Memorial Community Garden"
imper1[imper1=="Portrero del Sol Community Garden"] <- "Potrero del Sol Community Garden"
imper2 <- merge(imper1, garden.names, by=c("Name"))
imper3 <- imper2[,c("Site_code", "percent_imp")]
# change proportion to percent
imper3$percent_imp <- imper3$percent_imp*100

# percent non-tree vegetation in 500m buffer
ntveg1 <- ntveg[,c("Name", "percent_ntveg")]
ntveg1[ntveg1=="Visitación Valley Greenway Community Garden"] <- "Visitacion Valley Greenway Community Garden"
ntveg2 <- merge(ntveg1, garden.names, by=c("Name"))
ntveg3 <- ntveg2[,c("Site_code", "percent_ntveg")]
# change proportion to percent
ntveg3$percent_ntveg <- ntveg3$percent_ntveg*100

# farm area
farm.area <- gar.name.area[,c("Site_code", "total_area")]

# roads
road2 <- road[,c("Site_code", "Distance_to_road", "Distance_to_big_road")]

# oceans
ocean2 <- ocean[,c("Site_code", "Distance_to_ocean")]

# greenspace
greensp2 <- greensp[,c("Site_code", "Large_park_distance")]

# Veg survey data
# did include: birdbaths, number posts, water sources, tallest tree, edge vegetation, buffer structural diversity (to match buffer LULC)
# detractors: model raptors, CDs, shiny plastic
# Left out:
# only one site didn't have native veg, seeds, and all had flowers and fruit
# also almost all had almost all crop types
# also left out bird feeders and hummingbird feeders bc many were empty

# tried adding from veg survey: farm structural diversity and LULC
# restructure water: presence/absence
# detractors: presence/absence

vegsurv.sm <- vegsurv[,c("Site_code", 
                         "birdbath", "posts", "water_sources", 
                         "model_raptors", "cds", "shiny_plastic",
                         "tallest_tree",
                         "edge_per_tree", "edge_per_shrub", "edge_per_vine",
                         "site_per_ancrop", "site_per_woodcrop", "site_per_ncrop_nnative", "site_per_ncrop_native", "site_per_bare", "site_per_imper")]

# combine some categories
vegsurv.sm$detractors <- vegsurv.sm$model_raptors + vegsurv.sm$cds + vegsurv.sm$shiny_plastic
vegsurv.sm$detractors <- ifelse(vegsurv.sm$detractors>0, 1, 0)
vegsurv.sm$tot_water <- vegsurv.sm$birdbath + vegsurv.sm$water_sources # not entirely sure these don't overlap, but don't appear to from numbers
vegsurv.sm$water <- ifelse(vegsurv.sm$tot_water>0, 1, 0)

vegsurv.sm2 <- vegsurv.sm[,c("Site_code", 
                          "water", "posts",  
                          "detractors",
                          "tallest_tree",
                          "edge_per_tree", "edge_per_shrub", "edge_per_vine",
                          "site_per_ancrop", "site_per_woodcrop", "site_per_ncrop_nnative", "site_per_ncrop_native", "site_per_bare", "site_per_imper")]

# simpsons eveness
# https://search.r-project.org/CRAN/refmans/abdiv/html/simpson.html
# https://groups.nceas.ucsb.edu/sun/meetings/calculating-evenness-of-habitat-distributions.html

vegsurv.ev <- vegsurv[,c("Site_code","site_per_forb", "site_per_shrub", "site_per_tree", "site_per_ground")]

vegsurv.ev$simpson.ev.site <- 0

for (i in 1:nrow(vegsurv.ev)){
  vegsurv.ev[i, c("simpson.ev.site")] <- simpson_e(vegsurv.ev[i,2:5])
}

## Combine all veg data

vegdat <- farm.area
vegdat2 <- merge(vegdat, vegsurv.sm2, by = c("Site_code"))
vegdat3 <- merge(vegdat2, vegsurv.ev, by = c("Site_code"))
vegdat4 <- merge(vegdat3, canop3, by = c("Site_code"))
vegdat5 <- merge(vegdat4, imper3, by = c("Site_code"))
vegdat5.5 <- merge(vegdat5, ntveg3, by = c("Site_code"))
vegdat6 <- merge(vegdat5.5, road2, by = c("Site_code"))
vegdat6.5 <- merge(vegdat6, greensp2, by = c("Site_code"))
vegdat7 <- merge(vegdat6.5, ocean2, by = c("Site_code"))

head(vegdat7)
dim(vegdat7)  

## combine with biodiversity data to take a look

biod <- incomexdiv6[,c("Site_code", "SR_gb", "Nbirds_gb", "SR_gbfo", "Nbirds_gbfo",
                       "avg_temp", "tot_obs", "x", "y")]
vegxbiod <- merge(vegdat7, biod, by = c("Site_code"))
dim(vegxbiod)
head(vegxbiod)

## look at any spatial structure in SR and N
ggplot(vegxbiod, aes(x, y, colour = SR_gbfo)) +
  viridis::scale_colour_viridis() +
  geom_point(size = 3)

ggplot(vegxbiod, aes(x, y, colour = Nbirds_gbfo)) +
  viridis::scale_colour_viridis() +
  geom_point(size = 3)
# hard to tell if any! 

# note detractors is ordinal, total water and posts are count data

# take a look at correlations
par(mfrow=c(1,1))
corrplot(cor(vegxbiod[,-c(1)]))

pairs(vegxbiod[,-c(1)])

# Try putting in a PCA:
# 1. All site level veg/lulc (edge %s, structural, lulc)

site.veg.forpca <- subset(vegdat5, select = c(tallest_tree,
                                              edge_per_shrub, edge_per_tree, edge_per_vine,
                                              site_per_ancrop, site_per_woodcrop, site_per_ncrop_nnative, site_per_ncrop_native, site_per_bare, site_per_imper,
                                              site_per_forb, site_per_shrub, site_per_tree, site_per_ground))
rownames(site.veg.forpca) <- vegdat5$Site_code
# Calculate PCA with scaling
site.veg.pca2 <- prcomp(site.veg.forpca, scale = TRUE)
summary(site.veg.pca2)
factoextra::fviz_eig(site.veg.pca2) # PC4 gets us to 78% of variation
site.veg.pca2$rotation
write.csv(site.veg.pca2$rotation, "siteveg.pca.loadings.csv")
site.veg.pca2$x


fviz_pca_ind(site.veg.pca2,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
biplot(site.veg.pca2, scale = 0)

# print out
pdf("./Figures/screeplot_sitevegPCA.pdf", height = 6, width = 9)
factoextra::fviz_eig(site.veg.pca2) # PC4 gets us to 95% of variation, 1st 2 most important
dev.off()

pdf("./Figures/biplot_sitevegPCA.pdf", height = 9, width = 9)
biplot(site.veg.pca2, 
       xlab= "PC1 (27.9%)", 
       ylab = "PC2 (21.1%)")
dev.off()

## try recombining w/veg data, look at corrs!! 

site.veg.pcs <- as.data.frame(site.veg.pca2$x[,c(1:4)])
site.veg.pcs$Site_code <- rownames(site.veg.pcs)
colnames(site.veg.pcs) <- c("PC1_siteveg", "PC2_siteveg", "PC3_siteveg", "PC4_siteveg", "Site_code")

## Combine all veg data

## combine with biodiversity data to take a look

vegxbiod2 <- merge(vegxbiod, site.veg.pcs, by = c("Site_code"))
vegxbiod4 <- subset(vegxbiod2, select = -c(tallest_tree,
                                              edge_per_shrub, edge_per_tree, edge_per_vine,
                                              site_per_ancrop, site_per_woodcrop, site_per_ncrop_nnative, site_per_ncrop_native, site_per_bare, site_per_imper,
                                              site_per_forb, site_per_shrub, site_per_tree, site_per_ground))
dim(vegxbiod4)
head(vegxbiod4)
corrplot(cor(vegxbiod4[,-c(1)]))

pairs(vegxbiod4[, !names(vegxbiod4) %in% c("Site_code", "x", "y", "SR_gbfo", "Nbirds_gbfo", "water", "simpson.ev.site", 
                    "PC1_localveg", "PC2_localveg", "PC3_localveg", "PC4_localveg")])
# area needs log transformation
# distance to road, big road maybe

write.csv(vegxbiod4, "vegxbiod.csv")


###########################################
# Modeling time! Biodiversity vs. site data
##########################################

# Double check, these all say poisson but they should be quasi-poisson

### SR gbfo

# Version 1 -  try starting with all variables (using PCA), then selecting down
## Keep this as final version!! 
# left out only evenness because in PCA
# First, drop collinear variables based on VIF >3, then do stepwise AIC-based model selection
m1 <- glm(SR_gbfo ~ log(total_area) + posts + detractors + water +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_imp + percent_canopy + percent_ntveg +
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance
          + avg_temp + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m1)
vif(m1) # remove by largest VIF
m2 <- glm(SR_gbfo ~ log(total_area) + posts + detractors + water +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_imp + percent_canopy + percent_ntveg +
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance
          + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m2) # drop nt_veg (too correlated with canopy and imp)
vif(m2)
m3 <- glm(SR_gbfo ~ log(total_area) + posts + detractors + water +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_imp + percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance
          + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m3) # drop perc_imp
vif(m3)
m3 <- glm(SR_gbfo ~ log(total_area) + posts + detractors + water +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance
          + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m3) 
vif(m3)
m3 <- glm(SR_gbfo ~ log(total_area) + posts + detractors + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance
          + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m3)
vif(m3)
m3 <- glm(SR_gbfo ~ log(total_area) + posts + detractors + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Large_park_distance
          + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m3) # dropping area bc fo collinearity (with PC1)
vif(m3)
m3 <- glm(SR_gbfo ~ posts + detractors + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Large_park_distance
          + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m3) 
vif(m3) 
m3 <- glm(SR_gbfo ~ posts + detractors + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Large_park_distance,
          family = poisson,
          data = vegxbiod4)
summary(m3) 
vif(m3) # all VIF <3
# Model selection
step(m3)
m3 <- glm(SR_gbfo ~ detractors +
            PC1_siteveg + log(Distance_to_big_road),
          family = poisson,
          data = vegxbiod4)
summary(m3)
vif(m3)
# PC1, detractors, distance to big road
# check for overdispersion
m3$deviance/m3$df.residual # good! <1 
plot(m3)

# Version 2 - hypothesis-based
# Start with variables with hypothesis and correlation (esp dropping things w/low corr with SR highly corr w/other things, no simpson bc in pc)
# left out water, canopy, eve, distance to parks
m1 <- glm(SR_gbfo ~ log(total_area) + posts + detractors +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_imp + percent_ntveg +
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean
          + avg_temp + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m1)
vif(m1) # drop nt_veg
m1 <- glm(SR_gbfo ~ log(total_area) + posts + detractors +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_imp + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean
          + avg_temp + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m1)
vif(m1) # drop avg temp (corr w/ocean dist)
m2 <- glm(SR_gbfo ~ log(total_area) + posts + detractors +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_imp + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean
          + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m2)
vif(m2)
step(m2)
m3 <- glm(SR_gbfo ~ detractors +
            PC1_siteveg + log(Distance_to_big_road),
          family = poisson,
          data = vegxbiod4)
summary(m3)
vif(m3)
# got same result as previous, PC1, detractors, distance to big road
m3$deviance/m3$df.residual # good!
plot(m3)


# Version 3 - try without PCA variables and instead input all site level variables. This might make results easier to interpet
# leave out water, canopy (corr w/imp), ntveg (corr w/imp), park distance, water, woody crops, forbs, large park dist, annual crops, temp
m1 <- glm(SR_gbfo ~ log(total_area) + posts + detractors + simpson.ev.site + tallest_tree +
            edge_per_tree + edge_per_shrub + edge_per_vine +
            site_per_ncrop_nnative + site_per_ncrop_native +
            site_per_imper + site_per_shrub + site_per_tree + site_per_ground +
            percent_imp + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean
            + tot_obs,
          family = poisson,
          data = vegxbiod)
summary(m1)
vif(m1)
m1 <- glm(SR_gbfo ~ log(total_area) + posts + detractors + simpson.ev.site + tallest_tree +
            edge_per_shrub + edge_per_vine +
            site_per_ncrop_nnative + site_per_ncrop_native +
            site_per_imper + site_per_shrub + site_per_tree + site_per_ground +
            percent_imp + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean
          + tot_obs,
          family = poisson,
          data = vegxbiod)
summary(m1)
vif(m1)
m1 <- glm(SR_gbfo ~ log(total_area) + posts + detractors + simpson.ev.site + tallest_tree +
            edge_per_shrub + edge_per_vine +
            site_per_ncrop_native +
            site_per_imper + site_per_shrub + site_per_tree + site_per_ground +
            percent_imp + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean
          + tot_obs,
          family = poisson,
          data = vegxbiod)
summary(m1)
vif(m1)
m1 <- glm(SR_gbfo ~ log(total_area) + posts + detractors + simpson.ev.site + tallest_tree +
            edge_per_shrub + edge_per_vine +
            site_per_ncrop_native +
            site_per_imper + site_per_shrub + site_per_ground +
            percent_imp + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean
          + tot_obs,
          family = poisson,
          data = vegxbiod)
summary(m1)
vif(m1)
m1 <- glm(SR_gbfo ~ log(total_area) + posts + detractors + simpson.ev.site + tallest_tree +
            edge_per_shrub + edge_per_vine +
            site_per_imper + site_per_shrub + site_per_ground +
            percent_imp + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean
          + tot_obs,
          family = poisson,
          data = vegxbiod)
summary(m1)
vif(m1)
m1 <- glm(SR_gbfo ~ log(total_area) + posts + detractors + simpson.ev.site + tallest_tree +
            edge_per_shrub + edge_per_vine +
            site_per_imper + site_per_shrub + site_per_ground + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean
          + tot_obs,
          family = poisson,
          data = vegxbiod)
summary(m1)
vif(m1)
m1 <- glm(SR_gbfo ~ log(total_area) + posts + simpson.ev.site + tallest_tree +
            edge_per_shrub + edge_per_vine +
            site_per_imper + site_per_shrub + site_per_ground + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean
          + tot_obs,
          family = poisson,
          data = vegxbiod)
summary(m1)
vif(m1)
m1 <- glm(SR_gbfo ~ posts + simpson.ev.site + tallest_tree +
            edge_per_shrub + edge_per_vine +
            site_per_imper + site_per_shrub + site_per_ground + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean
          + tot_obs,
          family = poisson,
          data = vegxbiod)
summary(m1)
vif(m1)
m1 <- glm(SR_gbfo ~ posts + simpson.ev.site + tallest_tree +
            edge_per_shrub + edge_per_vine +
            site_per_imper + site_per_shrub + site_per_ground + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean,
          family = poisson,
          data = vegxbiod)
summary(m1)
vif(m1)
step(m1)
# tallest tree is the only sig var! as before. Don't use because not easy to interpret after all.



#### N gbfo 

# Version 1 -  try starting with all variables (using PCA), then selecting down
# Keep this one! 
# using quasipoisson because poisson was over-dispersed
m2 <- glm(Nbirds_gbfo ~ log(total_area) + detractors + posts + water +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + percent_imp + percent_ntveg +
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance +
            + avg_temp + tot_obs,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) # avg temp
vif(m2)
m2 <- glm(Nbirds_gbfo ~ log(total_area) + detractors + posts + water +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + percent_imp + percent_ntveg +
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance +
            +  tot_obs,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2)
vif(m2) # remove nt_veg
m2 <- glm(Nbirds_gbfo ~ log(total_area) + detractors + posts + water +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + percent_imp + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance +
            +  tot_obs,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) # remove imp
vif(m2)
m2 <- glm(Nbirds_gbfo ~ log(total_area) + detractors + posts + water +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance +
            +  tot_obs,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) # remove water
vif(m2)
m2 <- glm(Nbirds_gbfo ~ log(total_area) + detractors + posts + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance +
            +  tot_obs,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) # remove ocean
vif(m2)
m2 <- glm(Nbirds_gbfo ~ detractors + posts + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Large_park_distance +
            +  tot_obs,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) # remove observers
vif(m2)
m2 <- glm(Nbirds_gbfo ~ detractors + posts + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Large_park_distance,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) 
vif(m2)
drop1(m2, test = "F") # remove large park
m2 <- glm(Nbirds_gbfo ~ detractors + posts + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road),
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) 
vif(m2)
drop1(m2, test = "F")
m2 <- glm(Nbirds_gbfo ~ detractors + posts + 
            PC1_siteveg + PC2_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road),
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) 
vif(m2)
drop1(m2, test = "F")
m2 <- glm(Nbirds_gbfo ~ detractors + posts + 
            PC1_siteveg + PC2_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road),
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) 
vif(m2)
drop1(m2, test = "F")
m2 <- glm(Nbirds_gbfo ~ detractors + posts + 
            PC1_siteveg + PC2_siteveg +
            percent_canopy + 
            log(Distance_to_big_road),
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) 
vif(m2)
drop1(m2, test = "F")

# comparison to poisson to show overdispersed
m2p <- glm(Nbirds_gbfo ~ detractors + posts + 
            PC1_siteveg + PC2_siteveg +
            percent_canopy + 
            log(Distance_to_big_road),
          family = poisson,
          data = vegxbiod4)
m2p$deviance/m2p$df.residual # >1, overdispersed


# Version 2 - hypothesis-based
# Start with variables with hypothesis and correlation (esp dropping things w/low corr with N highly corr w/other things, no simpson bc in pc)
# leave out water, eve (in PC), per_imp (corr w/canopy, less corr w/N), per_ntveg
m1 <- glm(Nbirds_gbfo ~ log(total_area) + detractors + posts + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean
          + avg_temp + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m1)
vif(m1)
m1$deviance/m1$df.residual # overdispersed
# Change to quasi poisson
m2 <- glm(Nbirds_gbfo ~ log(total_area) + detractors + posts + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean
          + avg_temp + tot_obs,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2)
vif(m2)
# Model selection using drop1 and F test
drop1(m2, test = "F")
m2.5 <- glm(Nbirds_gbfo ~ detractors + posts + 
              PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
              percent_canopy + log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean
            + avg_temp + tot_obs,
            family = quasipoisson,
            data = vegxbiod4)
drop1(m2.5, test = "F")
m3 <- glm(Nbirds_gbfo ~ detractors + posts + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + log(Distance_to_big_road) + Distance_to_ocean
          + avg_temp + tot_obs,
          family = quasipoisson,
          data = vegxbiod4)
drop1(m3, test = "F")
m3 <- glm(Nbirds_gbfo ~ detractors + posts + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + log(Distance_to_big_road) + Distance_to_ocean
          + avg_temp,
          family = quasipoisson,
          data = vegxbiod4)
drop1(m3, test = "F")
m4 <- glm(Nbirds_gbfo ~ detractors + posts + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg +
            percent_canopy + log(Distance_to_big_road) + Distance_to_ocean
          + avg_temp,
          family = quasipoisson,
          data = vegxbiod4)
drop1(m4, test = "F")
m5 <- glm(Nbirds_gbfo ~ detractors + posts + 
            PC1_siteveg + PC2_siteveg + 
            percent_canopy + log(Distance_to_big_road) + Distance_to_ocean
          + avg_temp,
          family = quasipoisson,
          data = vegxbiod4)
drop1(m5, test = "F") # all below .1, but still so many...worried about overfitting
m6 <- glm(Nbirds_gbfo ~ detractors + posts + 
            PC1_siteveg + PC2_siteveg + 
            percent_canopy + log(Distance_to_big_road) + Distance_to_ocean,
          family = quasipoisson,
          data = vegxbiod4)
drop1(m6, test = "F")
summary(m6)
vif(m6)
plot(m6)

# Version 3 - try without PCA variables and instead input all site level variables. This might make results easier to interpet
# Again, hypothesis-based, keep variables not correlated with each other but correlated with N
# leave out water, an crops, wood crops, forb, shrub, imp, ntveg, distance big road, distance park
m1 <- glm(Nbirds_gbfo ~ log(total_area) + posts + detractors + simpson.ev.site + tallest_tree +
            edge_per_tree + edge_per_shrub + edge_per_vine +
            site_per_ncrop_nnative + site_per_ncrop_native +
            site_per_imper + site_per_tree + site_per_ground +
            percent_canopy + 
            log(Distance_to_road) + Distance_to_ocean
          + tot_obs + avg_temp,
          family = quasipoisson,
          data = vegxbiod)
summary(m1)
vif(m1)
m1 <- glm(Nbirds_gbfo ~ posts + detractors + simpson.ev.site + tallest_tree +
            edge_per_tree + edge_per_shrub + edge_per_vine +
            site_per_ncrop_nnative + site_per_ncrop_native +
            site_per_imper + site_per_tree + site_per_ground +
            percent_canopy + 
            log(Distance_to_road) + Distance_to_ocean
          + tot_obs + avg_temp,
          family = quasipoisson,
          data = vegxbiod)
summary(m1)
vif(m1)
m1 <- glm(Nbirds_gbfo ~ posts + detractors + simpson.ev.site + tallest_tree +
            edge_per_shrub + edge_per_vine +
            site_per_ncrop_nnative + site_per_ncrop_native +
            site_per_imper + site_per_tree + site_per_ground +
            percent_canopy + 
            log(Distance_to_road) + Distance_to_ocean
          + tot_obs + avg_temp,
          family = quasipoisson,
          data = vegxbiod)
summary(m1)
vif(m1)
m1 <- glm(Nbirds_gbfo ~ posts + detractors + simpson.ev.site + tallest_tree +
            edge_per_shrub + edge_per_vine +
            site_per_ncrop_nnative + site_per_ncrop_native +
            site_per_imper + site_per_ground +
            percent_canopy + 
            log(Distance_to_road) + Distance_to_ocean
          + tot_obs + avg_temp,
          family = quasipoisson,
          data = vegxbiod)
summary(m1)
vif(m1)
m1 <- glm(Nbirds_gbfo ~ posts + detractors + simpson.ev.site + tallest_tree +
            edge_per_shrub + edge_per_vine +
            site_per_ncrop_native +
            site_per_imper + site_per_ground +
            percent_canopy + 
            log(Distance_to_road) + Distance_to_ocean
          + tot_obs + avg_temp,
          family = quasipoisson,
          data = vegxbiod)
summary(m1)
vif(m1)
m1 <- glm(Nbirds_gbfo ~ posts + simpson.ev.site + tallest_tree +
            edge_per_shrub + edge_per_vine +
            site_per_ncrop_native +
            site_per_imper + site_per_ground +
            percent_canopy + 
            log(Distance_to_road) + Distance_to_ocean
          + tot_obs + avg_temp,
          family = quasipoisson,
          data = vegxbiod)
summary(m1)
vif(m1)
m1 <- glm(Nbirds_gbfo ~ posts + simpson.ev.site + tallest_tree +
            edge_per_shrub + edge_per_vine +
            site_per_ncrop_native +
            site_per_imper + site_per_ground +
            log(Distance_to_road) + Distance_to_ocean
          + tot_obs + avg_temp,
          family = quasipoisson,
          data = vegxbiod)
summary(m1)
vif(m1)
m1 <- glm(Nbirds_gbfo ~ posts + simpson.ev.site + 
            edge_per_shrub + edge_per_vine +
            site_per_ncrop_native +
            site_per_imper + site_per_ground +
            log(Distance_to_road) + Distance_to_ocean
          + tot_obs + avg_temp,
          family = quasipoisson,
          data = vegxbiod)
summary(m1)
vif(m1)
m1 <- glm(Nbirds_gbfo ~ posts + simpson.ev.site + 
            edge_per_shrub + edge_per_vine +
            site_per_imper + site_per_ground +
            log(Distance_to_road) + Distance_to_ocean
          + tot_obs + avg_temp,
          family = quasipoisson,
          data = vegxbiod)
summary(m1)
vif(m1)
drop1(m1, test = "F")
m1 <- glm(Nbirds_gbfo ~ posts + simpson.ev.site + 
            edge_per_shrub + 
            site_per_imper + site_per_ground +
            log(Distance_to_road) + Distance_to_ocean
          + tot_obs + avg_temp,
          family = quasipoisson,
          data = vegxbiod)
summary(m1)
vif(m1)
drop1(m1, test = "F")
m1 <- glm(Nbirds_gbfo ~ posts + simpson.ev.site +
            site_per_imper + site_per_ground +
            log(Distance_to_road) + Distance_to_ocean
          + tot_obs + avg_temp,
          family = quasipoisson,
          data = vegxbiod)
summary(m1)
vif(m1)
drop1(m1, test = "F")
m1 <- glm(Nbirds_gbfo ~ posts + simpson.ev.site +site_per_ground +
            log(Distance_to_road) + Distance_to_ocean
          + tot_obs + avg_temp,
          family = quasipoisson,
          data = vegxbiod)
summary(m1)
vif(m1)
drop1(m1, test = "F")
m1 <- glm(Nbirds_gbfo ~ posts + simpson.ev.site +site_per_ground +
            log(Distance_to_road)
          + tot_obs + avg_temp,
          family = quasipoisson,
          data = vegxbiod)
summary(m1)
vif(m1)
drop1(m1, test = "F")
m1 <- glm(Nbirds_gbfo ~ posts + simpson.ev.site +
            log(Distance_to_road)
          + tot_obs + avg_temp,
          family = quasipoisson,
          data = vegxbiod)
summary(m1)
vif(m1)
drop1(m1, test = "F")
m1 <- glm(Nbirds_gbfo ~ posts + simpson.ev.site +
            log(Distance_to_road)
          + tot_obs,
          family = quasipoisson,
          data = vegxbiod)
summary(m1)
vif(m1)
drop1(m1, test = "F")
# Not kept, harder to interpret


#### Not used in final paper - GB analyses

### SR_gb 
m1 <- glm(SR_gb ~ log(total_area) + posts + detractors + water +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_imp + percent_canopy + percent_ntveg +
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance
          + avg_temp + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m1)
vif(m1) # remove by largest VIF
m2 <- glm(SR_gb ~ log(total_area) + posts + detractors + water +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_imp + percent_canopy + percent_ntveg +
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance
          + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m2) # drop nt_veg (too correlated with canopy and imp)
vif(m2)
m3 <- glm(SR_gb ~ log(total_area) + posts + detractors + water +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_imp + percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance
          + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m3) # drop perc_imp
vif(m3)
m3 <- glm(SR_gb ~ log(total_area) + posts + detractors + water +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance
          + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m3) 
vif(m3)
m3 <- glm(SR_gb ~ log(total_area) + posts + detractors + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance
          + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m3)
vif(m3)
m3 <- glm(SR_gb ~ log(total_area) + posts + detractors + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Large_park_distance
          + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m3) # dropping area bc fo collinearity (with PC1)
vif(m3)
m3 <- glm(SR_gb ~ posts + detractors + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Large_park_distance
          + tot_obs,
          family = poisson,
          data = vegxbiod4)
summary(m3) 
vif(m3) 
m3 <- glm(SR_gb ~ posts + detractors + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Large_park_distance,
          family = poisson,
          data = vegxbiod4)
summary(m3) 
vif(m3) # all VIF <3
# Model selection
step(m3)
m3 <- glm(SR_gbfo ~ detractors +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + log(Distance_to_big_road),
          family = poisson,
          data = vegxbiod4)
summary(m3)
vif(m3)
# Similar to GBFO model, but 3 PCs included and none significant.
# check for overdispersion
m3$deviance/m3$df.residual # good! <1 
plot(m3)


### N_gb
# using quasipoisson because poisson was over-dispersed
m2 <- glm(Nbirds_gb ~ log(total_area) + detractors + posts + water +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + percent_imp + percent_ntveg +
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance +
            + avg_temp + tot_obs,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) # avg temp
vif(m2)
m2 <- glm(Nbirds_gb ~ log(total_area) + detractors + posts + water +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + percent_imp + percent_ntveg +
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance +
            +  tot_obs,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2)
vif(m2) # remove nt_veg
m2 <- glm(Nbirds_gb ~ log(total_area) + detractors + posts + water +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + percent_imp + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance +
            +  tot_obs,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) # remove imp
vif(m2)
m2 <- glm(Nbirds_gb ~ log(total_area) + detractors + posts + water +
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance +
            +  tot_obs,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) # remove water
vif(m2)
m2 <- glm(Nbirds_gb ~ log(total_area) + detractors + posts + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Distance_to_ocean + Large_park_distance +
            +  tot_obs,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) # remove ocean
vif(m2)
m2 <- glm(Nbirds_gb ~ detractors + posts + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Large_park_distance +
            +  tot_obs,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) # remove observers
vif(m2)
m2 <- glm(Nbirds_gb ~ detractors + posts + 
            PC1_siteveg + PC2_siteveg + PC3_siteveg + PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Large_park_distance,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) 
vif(m2)
drop1(m2, test = "F") # remove pc3
m2 <- glm(Nbirds_gb ~ detractors + posts + 
            PC1_siteveg + PC2_siteveg +  PC4_siteveg +
            percent_canopy + 
            log(Distance_to_road) + log(Distance_to_big_road) + Large_park_distance,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) 
vif(m2)
drop1(m2, test = "F") # distance to road
m2 <- glm(Nbirds_gb ~ detractors + posts + 
            PC1_siteveg + PC2_siteveg +  PC4_siteveg +
            percent_canopy + 
            log(Distance_to_big_road) + Large_park_distance,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) 
vif(m2)
drop1(m2, test = "F") # drop posts
m2 <- glm(Nbirds_gb ~ detractors + 
            PC1_siteveg + PC2_siteveg +  PC4_siteveg +
            percent_canopy + 
            log(Distance_to_big_road) + Large_park_distance,
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) 
vif(m2)
drop1(m2, test = "F") # drop large park
m2 <- glm(Nbirds_gb ~ detractors + 
            PC1_siteveg + PC2_siteveg +  PC4_siteveg +
            percent_canopy + 
            log(Distance_to_big_road),
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) 
vif(m2)
drop1(m2, test = "F") # drop pc4
m2 <- glm(Nbirds_gb ~ detractors + 
            PC1_siteveg + PC2_siteveg + 
            percent_canopy + 
            log(Distance_to_big_road),
          family = quasipoisson,
          data = vegxbiod4)
summary(m2) 
vif(m2)
drop1(m2, test = "F")
# only posts is different

# comparison to poisson to show overdispersed
m2p <- glm(Nbirds_gb ~ detractors + 
            PC1_siteveg + PC2_siteveg + 
            percent_canopy + 
            log(Distance_to_big_road),
          family = poisson,
          data = vegxbiod4)
m2p$deviance/m2p$df.residual # >1, overdispersed

#################
# Correlations between significant site variables and income
#################################################

### correlations between income and veg vars of interest

income <- incomexdiv6[,c("Site_code", "neighborhood.1627.medhhincome")]
vegbiodinc <- merge(vegxbiod4, income, by=c("Site_code"))
head(vegbiodinc)
cor.test(vegbiodinc$neighborhood.1627.medhhincome, vegbiodinc$posts) # 0.17, p=.47
cor.test(vegbiodinc$neighborhood.1627.medhhincome, vegbiodinc$detractors) # 0.18, p=.45
cor.test(vegbiodinc$neighborhood.1627.medhhincome, vegbiodinc$percent_canopy) # 0.36, p=.12
cor.test(vegbiodinc$neighborhood.1627.medhhincome, log(vegbiodinc$Distance_to_big_road)) # -0.47, p=.03 !!
cor.test(vegbiodinc$neighborhood.1627.medhhincome, vegbiodinc$Distance_to_ocean) # 0.26, p=.28
cor.test(vegbiodinc$neighborhood.1627.medhhincome, vegbiodinc$PC1_siteveg) # -0.44, p=.051
cor.test(vegbiodinc$neighborhood.1627.medhhincome, vegbiodinc$PC2_siteveg) # -0.37, p=.10



###################################################
# Sentiment vs overall abundance for each species!
####################################################

# Sentiment scores
sp.sentiment.stats$sentimentwordspos.perc.cent <- sp.sentiment.stats$sentimentwordspos.perc - 0.5

# species abundance data! 
head(point.gb.focal.sent)
point.gb.focal.ab <- plyr::ddply(point.gb.focal.sent, c("Species_ID"), summarise, 
                                 Nbirds_gb=sum(Nbirds))
point.gbfo.focal.ab <- plyr::ddply(point.gbfo.focal.sent, c("Species_ID"), summarise, 
                                 Nbirds_gbfo=sum(Nbirds))

# Merge
sp.sentxn <- merge(sp.sentiment.stats,
                   point.gb.focal.ab, by = c("Species_ID"))

sp.sentxn2 <- merge(sp.sentxn,
                   point.gbfo.focal.ab, by = c("Species_ID"))

Ngbxsent <- ggplot(sp.sentxn2, aes(x=Nbirds_gb, y=sentimentwordspos.perc.cent, label=Species_ID)) + 
  geom_point() +
  geom_text(hjust=1, vjust=1) +
  # labs(y = "Relative bird abundance (gb)", x = "Average Gardener Income (285m)") + 
  theme_minimal()

Ngbfoxsent <- ggplot(sp.sentxn2, aes(x=Nbirds_gbfo, y=sentimentwordspos.perc.cent, label=Species_ID)) + 
  geom_point() +
  geom_text(hjust=1, vjust=1) +
  # labs(y = "Relative bird abundance (gb)", x = "Average Gardener Income (285m)") + 
  theme_minimal()

Ngbxpos <- ggplot(sp.sentxn2, aes(x=Nbirds_gb, y=n_pos, label=Species_ID)) + 
  geom_point() +
  geom_text(hjust=1, vjust=1) +
  # labs(y = "Relative bird abundance (gb)", x = "Average Gardener Income (285m)") + 
  theme_minimal()

Ngbfoxpos <- ggplot(sp.sentxn2, aes(x=Nbirds_gbfo, y=n_pos, label=Species_ID)) + 
  geom_point() +
  geom_text(hjust=1, vjust=1) +
  # labs(y = "Relative bird abundance (gb)", x = "Average Gardener Income (285m)") + 
  theme_minimal()

Ngbxneg <- ggplot(sp.sentxn2, aes(x=Nbirds_gb, y=n_neg, label=Species_ID)) + 
  geom_point() +
  geom_text(hjust=1, vjust=1) +
  # labs(y = "Relative bird abundance (gb)", x = "Average Gardener Income (285m)") + 
  theme_minimal()

Ngbfoxneg <- ggplot(sp.sentxn2, aes(x=Nbirds_gbfo, y=n_neg, label=Species_ID)) + 
  geom_point() +
  geom_text(hjust=1, vjust=1) +
  # labs(y = "Relative bird abundance (gb)", x = "Average Gardener Income (285m)") + 
  theme_minimal()

# doesn't appear to be a relationship btwn abundance and sent score
# but there is btwn abundance and # of positive words (more ppl familiar w/, answer)

#####################
# Recognition vs. other metrics
########################

# look at relationship between % recognition and abundance
head(sp.sentxn2)
cor.test(sp.sentxn2$Nbirds_gbfo, sp.sentxn2$perc_rec)
# NS

# look at relationship between % recognition and nwords
cor.test(sp.sentxn2$n_words, sp.sentxn2$perc_rec)
# sig, pos. People associated more words with species they recognized. 
# (makes sense because of survey logic - if they said they didn't recognize species, skipped word association task)

# look at relationship between abundance and sent score
cor.test(sp.sentxn2$Nbirds_gbfo, sp.sentxn2$netsentvtot)
# NS

# look at relationship between % recog and sent score
cor.test(sp.sentxn2$netsentvtot, sp.sentxn2$perc_rec)
# NS

# compare familiarity metrics to abundance

ggplot(sp.sentxn2, aes(x=Nbirds_gbfo, y=(perc_rec)*100, label=species)) + 
  geom_point() +
  geom_text_repel(hjust=1, vjust=1) +
  scale_y_continuous(limits = c(0,100)) +
  xlab("Species abundance") +
  ylab("Percent recognition") +
  theme_classic()

pdf("./Figures/abundancevrecog.pdf", height = 6, width = 6)
ggplot(sp.sentxn2, aes(x=Nbirds_gbfo, y=(perc_rec)*100, label=species)) + 
  geom_point() +
  geom_text_repel(hjust=1, vjust=1) +
  scale_y_continuous(limits = c(0,100)) +
  xlab("Species abundance") +
  ylab("Percent recognition") +
  theme_classic()
dev.off()

# compare familiarity metrics to sentiment score

ggplot(sp.sentxn2, aes(x=netsentvtot, y=perc_rec, label=Species_ID)) + 
  geom_point() +
  geom_text(hjust=1, vjust=1) +
  theme_minimal()
# people generally say more negative things about species they recognize, those less recognized had more good things said abotu them?
# but also ppl who totally don't recognize won't have filled this out


# compare sentiment and abundance
ggplot(sp.sentxn2, aes(x=Nbirds_gbfo, y=netsentvtot, label=Species_ID)) + 
  geom_point() +
  geom_text(hjust=1, vjust=1) +
  theme_minimal()


############################
# bivariate chloropleth plot
#############################

# calculate tertiles
income.ter <- quantile(incomexdiv6$neighborhood.1627.medhhincome, probs = c(0.333, 0.666))
income.33 <- income.ter[1]
income.66 <- income.ter[2]
incomexdiv6$income.1627.ter <- ifelse(incomexdiv6$neighborhood.1627.medhhincome> income.66, "C", 
                                      ifelse(incomexdiv6$neighborhood.1627.medhhincome> income.33, "B", "A"))

SR.ter <- quantile(incomexdiv6$SR_gbfo, probs = c(0.333, 0.666))
SR.33 <- SR.ter[1]
SR.66 <- SR.ter[2]

incomexdiv6$SR.ter <- ifelse(incomexdiv6$SR_gbfo > SR.66, "3", 
                                      ifelse(incomexdiv6$SR_gbfo > SR.33, "2", "1"))

Nbirds.ter <- quantile(incomexdiv6$Nbirds_gbfo, probs = c(0.333, 0.666))
Nbirds.33 <- Nbirds.ter[1]
Nbirds.66 <- Nbirds.ter[2]

incomexdiv6$Nbirds.ter <- ifelse(incomexdiv6$Nbirds_gbfo > Nbirds.66, "3", 
                             ifelse(incomexdiv6$Nbirds_gbfo > Nbirds.33, "2", "1"))

spaccess.ter <- quantile(incomexdiv6$spaccess_gbfo, probs = c(0.333, 0.666))
spaccess.33 <- spaccess.ter[1]
spaccess.66 <- spaccess.ter[2]

incomexdiv6$spaccess.ter <- ifelse(incomexdiv6$spaccess_gbfo > spaccess.66, "3", 
                                 ifelse(incomexdiv6$spaccess_gbfo > spaccess.33, "2", "1"))

# combine tertiles for plotting
incomexdiv6$incomexSR <- paste(incomexdiv6$income.1627.ter, incomexdiv6$SR.ter, sep="")
incomexdiv6$incomexNbirds <- paste(incomexdiv6$income.1627.ter, incomexdiv6$Nbirds.ter, sep="")
incomexdiv6$incomexspaccess <- paste(incomexdiv6$income.1627.ter, incomexdiv6$spaccess.ter, sep="")

# merge with point shapefile, write out! 
garden.pt22  <- garden.pt2
colnames(garden.pt22@data)[which(names(garden.pt22@data) == "Name")] <- "Garden.name"
garden.pt3 <- merge(garden.pt22, incomexdiv6, by = c("Garden.name"))

# writeOGR(garden.pt3, dsn = "~/Desktop/Old_comp_transfer/Stanford/Research/BA_birds/Fieldwork/Data/garden_centroids_bivariate", layer = "garden_centroids_bivariate", driver = "ESRI Shapefile")
st_write(garden.pt3, dsn = "~/Desktop/Old_comp_transfer/Stanford/Research/BA_birds/Fieldwork/Data/garden_centroids_bivariate", layer = "garden_centroids_bivariate", driver = "ESRI Shapefile")

# print out legend
legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Higher Abundance ",
                    ylab = "Higher Income ",
                    size = 26)

legend2 <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Higher Species Richness ",
                    ylab = "Higher Income ",
                    size = 26)

legend3 <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Higher Species Access ",
                    ylab = "Higher Income ",
                    size = 26)

legend4 <- bi_legend(pal = "GrPink",
                     dim = 3,
                     xlab = "Higher Access ",
                     ylab = "Higher Income ",
                     size = 30)

pdf("./Figures/SRbivariatelegend.pdf", height = 6, width = 6)
legend2
dev.off()

pdf("./Figures/Nbivariatelegend.pdf", height = 6, width = 6)
legend
dev.off()

pdf("./Figures/spaccessbivariatelegend.pdf", height = 6, width = 6)
legend3
dev.off()
