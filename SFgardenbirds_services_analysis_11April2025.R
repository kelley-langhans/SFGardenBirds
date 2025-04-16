## SF Garden Birds code set 2 for species word associations analysis
# Author: Kelley Langhans

#################
# Set WD
#################
# Set working directory to wherever you have files stored
# setwd("")

###################
# load libraries
##################

library(tidyverse)
library(openxlsx)
library(lubridate)
library(reshape2)
library(RColorBrewer)
library(dplyr)

##################
# read in data
##################

## read in code2 data for each of 10 focal species
hofi <-  read.xlsx("gardener_survey_species_qual_themes_codeset2.xlsx", sheet = "Q24_hofi")
anhu <-  read.xlsx("gardener_survey_species_qual_themes_codeset2.xlsx", sheet = "Q30_anhu")
lego <-  read.xlsx("gardener_survey_species_qual_themes_codeset2.xlsx", sheet = "Q36_lego")
hosp <-  read.xlsx("gardener_survey_species_qual_themes_codeset2.xlsx", sheet = "Q42_hosp")
amcr <-  read.xlsx("gardener_survey_species_qual_themes_codeset2.xlsx", sheet = "Q48_amcr")
blph <-  read.xlsx("gardener_survey_species_qual_themes_codeset2.xlsx", sheet = "Q54_blph")
nomo <-  read.xlsx("gardener_survey_species_qual_themes_codeset2.xlsx", sheet = "Q60_nomo")
amro <-  read.xlsx("gardener_survey_species_qual_themes_codeset2.xlsx", sheet = "Q66_amro")
casj <-  read.xlsx("gardener_survey_species_qual_themes_codeset2.xlsx", sheet = "Q72_casj")
rtha <-  read.xlsx("gardener_survey_species_qual_themes_codeset2.xlsx", sheet = "Q78_rtha")

## read in lists of words recoded into ecosystem services and disservices
# inclusive list: predation/pest control, scavenging, seed dispersal, pollination, general beneficial words
es1 <-  read.xlsx("ecosystemservices.recode.freq.xlsx", sheet = "ecosystemservicesrecode")
# conservative list: excludes scavenging and seed dispersal
es2 <-  read.xlsx("ecosystemservices.recode.freq.xlsx", sheet = "ecosystemservicesrecode2")
# inclusive list: anything birds do to damage garden or harvest—eat garden plants, eat fruit, eat worms, be “pests”
diss1 <-  read.xlsx("disservices.recode.freq.xlsx", sheet = "disservicesrecode")
# conservative list: excludes eating worms
diss2 <-  read.xlsx("disservices.recode.freq.xlsx", sheet = "disservicesrecode2")

## read in sentiment scores for correlation analysis
sent <- read.csv("sp.sentiment.stats.csv")

##read in % recognition data
recog4 <- read.csv("recog.csv")

###################
# examine data
##################

head(hofi)
dim(hofi) # 165 responses (our n is 165!)
# last row is totals

head(es1)
dim(es1)
dim(es2)
dim(diss1)
dim(diss2)

head(sent)
dim(sent)

#######################
# reformat data
#######################

# Separate into different dataframe for each word column/coding columns, rename those columns
hofi1 <- hofi[,c(1,3:10)]
hofi2 <- hofi[,c(1,11:18)]
hofi3 <- hofi[,c(1,19:26)]
hofi4 <- hofi[,c(1,27:34)]
head(hofi1)
head(hofi2)
head(hofi3)
head(hofi4)
colnames(hofi1) <- c("ResponsesId", "Word", "seehear", "ecol", "gard", "seehearn", "ecoln", "gardn", "food")
colnames(hofi2) <- c("ResponsesId", "Word", "seehear", "ecol", "gard", "seehearn", "ecoln", "gardn", "food")
colnames(hofi3) <- c("ResponsesId", "Word", "seehear", "ecol", "gard", "seehearn", "ecoln", "gardn", "food")
colnames(hofi4) <- c("ResponsesId", "Word", "seehear", "ecol", "gard", "seehearn", "ecoln", "gardn", "food")

# Combine them all together into a single df with 1 word and 7 category columns columns
hofi.l <-rbind(hofi1, hofi2, hofi3, hofi4)
head(hofi.l)
dim(hofi.l) # 664=166*4

# Cut out all NA responses for words
hofi.lc <- hofi.l[complete.cases(hofi.l[,2]),]
dim(hofi.lc) # 294
head(hofi.lc)

# Replace all NAs for codes with 0s
hofi.lc[is.na(hofi.lc)] <- 0
head(hofi.lc)


# Repeat for other 9 species
sp.df <- list(anhu, lego, hosp, amcr, blph, nomo, amro, casj, rtha)
sp.df.final <- replicate(9,data.frame())

for (i in 1:length(sp.df)){
  x <- as.data.frame(sp.df[[i]])
  
  x1 <- x[,c(1,3:10)]
  x2 <- x[,c(1,11:18)]
  x3 <- x[,c(1,19:26)]
  colnames(x1) <- c("ResponsesId", "Word", "seehear", "ecol", "gard", "seehearn", "ecoln", "gardn", "food")
  colnames(x2) <- c("ResponsesId", "Word", "seehear", "ecol", "gard", "seehearn", "ecoln", "gardn", "food")
  colnames(x3) <- c("ResponsesId", "Word", "seehear", "ecol", "gard", "seehearn", "ecoln", "gardn", "food")
  
  # deal with the fact that some birds people responded 3-5 words
  if (dim(x)[2] == 34){
    
    x4 <- x[,c(1,27:34)]
    colnames(x4) <- c("ResponsesId", "Word", "seehear", "ecol", "gard", "seehearn", "ecoln", "gardn", "food")
    x.l <-rbind(x1, x2, x3, x4)
    
  } else if (dim(x)[2] == 42){
    
    x4 <- x[,c(1,27:34)]
    colnames(x4) <- c("ResponsesId", "Word", "seehear", "ecol", "gard", "seehearn", "ecoln", "gardn", "food")
    x5 <- x[,c(1,35:42)]
    colnames(x5) <- c("ResponsesId", "Word", "seehear", "ecol", "gard", "seehearn", "ecoln", "gardn", "food")
    x.l <-rbind(x1, x2, x3, x4,x5)
    
  } else {
    
    x.l <-rbind(x1, x2, x3)
    
  }
  
  x.lc <- x.l[complete.cases(x.l[,2]),]
  x.lc[is.na(x.lc)] <- 0
  
  sp.df.final[[i]] <- x.lc
}

anhu.lc <- sp.df.final[[1]]
lego.lc <- sp.df.final[[2]]
hosp.lc <- sp.df.final[[3]]
amcr.lc <- sp.df.final[[4]]
blph.lc <- sp.df.final[[5]]
nomo.lc <- sp.df.final[[6]]
amro.lc <- sp.df.final[[7]]
casj.lc <- sp.df.final[[8]]
rtha.lc <- sp.df.final[[9]]

head(amcr.lc)
dim(amcr.lc)

######################
# Lists and checking
##########################

# Create a single master words list across all birds
allbirds.words <- rbind(hofi.lc, anhu.lc, lego.lc, hosp.lc, amcr.lc,
                        blph.lc, nomo.lc, amro.lc, casj.lc, rtha.lc)
dim(allbirds.words)
head(allbirds.words) # 2911

# write.csv(allbirds.words, "allwords_posnegneu.csv", row.names=FALSE)

## Check 1: words coded into a category are always coded and never left uncoded: passed

# Create a list of all seehear pos words
allbirds.words.seehear <- subset(allbirds.words, allbirds.words$seehear==1)
dim(allbirds.words.seehear) #431
seehearwords <- unique(allbirds.words.seehear$Word)
#not included
allbirds.words.seehearinv <- subset(allbirds.words, allbirds.words$seehear==0)
seehearwordsinv <- unique(allbirds.words.seehearinv$Word)
#check for coding consistency, words always coded as seehear
Reduce(intersect, list(seehearwords, seehearwordsinv))

# Create a list of all seehear neg words
allbirds.words.seehearn <- subset(allbirds.words, allbirds.words$seehearn==1)
dim(allbirds.words.seehearn) #115
seehearnwords <- unique(allbirds.words.seehearn$Word)
#not included
allbirds.words.seehearninv <- subset(allbirds.words, allbirds.words$seehearn==0)
seehearnwordsinv <- unique(allbirds.words.seehearninv$Word)
#check for coding consistency
Reduce(intersect, list(seehearnwords, seehearnwordsinv))

# Create a list of all ecol pos words
allbirds.words.ecol <- subset(allbirds.words, allbirds.words$ecol==1)
dim(allbirds.words.ecol) #184
ecolwords <- unique(allbirds.words.ecol$Word)
#not included
allbirds.words.ecolinv <- subset(allbirds.words, allbirds.words$ecol==0)
ecolwordsinv <- unique(allbirds.words.ecolinv$Word)
#check for coding consistency
Reduce(intersect, list(ecolwords, ecolwordsinv))

# Create a list of all ecol neg words
allbirds.words.ecoln <- subset(allbirds.words, allbirds.words$ecoln==1)
dim(allbirds.words.ecoln) #13
ecolnwords <- unique(allbirds.words.ecoln$Word)
#not included
allbirds.words.ecolninv <- subset(allbirds.words, allbirds.words$ecoln==0)
ecolnwordsinv <- unique(allbirds.words.ecolninv$Word)
#check for coding consistency
Reduce(intersect, list(ecolnwords, ecolnwordsinv))

# Create a list of all gard pos words
allbirds.words.gard <- subset(allbirds.words, allbirds.words$gard==1)
dim(allbirds.words.gard) #63
gardwords <- unique(allbirds.words.gard$Word)
#not included
allbirds.words.gardinv <- subset(allbirds.words, allbirds.words$gard==0)
gardwordsinv <- unique(allbirds.words.gardinv$Word)
#check for coding consistency
Reduce(intersect, list(gardwords, gardwordsinv))

# Create a list of all gard neg words
allbirds.words.gardn <- subset(allbirds.words, allbirds.words$gardn==1)
dim(allbirds.words.gardn) #13
gardnwords <- unique(allbirds.words.gardn$Word)
#not included
allbirds.words.gardninv <- subset(allbirds.words, allbirds.words$gardn==0)
gardnwordsinv <- unique(allbirds.words.gardninv$Word)
#check for coding consistency
Reduce(intersect, list(gardnwords, gardnwordsinv))

# Create a list of all food words
allbirds.words.food <- subset(allbirds.words, allbirds.words$food==1)
dim(allbirds.words.food) #73
foodwords <- unique(allbirds.words.food$Word)
#not included
allbirds.words.foodinv <- subset(allbirds.words, allbirds.words$food==0)
foodwordsinv <- unique(allbirds.words.foodinv$Word)
#check for coding consistency
Reduce(intersect, list(foodwords, foodwordsinv)) 

## Check 2: Words coded in positive are never coded in negative of same category: passed

Reduce(intersect, list(gardnwords, gardwords))
Reduce(intersect, list(ecolnwords, ecolwords))
Reduce(intersect, list(seehearnwords, seehearwords))

## Check 3: Words coded into one category are exclusive others: partial pass, garden not exclusive with ecol role

Reduce(intersect, list(ecolwords, gardwords)) # non-exclusive
Reduce(intersect, list(ecolwords, seehearwords)) # exclusive
Reduce(intersect, list(seehearwords, gardwords)) # exclusive
Reduce(intersect, list(foodwords, ecolwords)) # exclusive
Reduce(intersect, list(foodwords, gardwords)) # exclusive
Reduce(intersect, list(foodwords, seehearwords)) # exclusive

Reduce(intersect, list(ecolnwords, gardnwords)) # exclusive
Reduce(intersect, list(ecolnwords, seehearnwords)) # exclusive
Reduce(intersect, list(seehearnwords, gardnwords)) # exclusive
Reduce(intersect, list(foodwords, ecolnwords)) # exclusive
Reduce(intersect, list(foodwords, gardnwords)) # exclusive
Reduce(intersect, list(foodwords, seehearnwords)) # exclusive


## Check 4: Sensible coding, all words in each list make sense/have justification and words missing do too

## Ecol

ecolwords
# helpful, beneficial, important, reliable, essential all coded, make sure justified (see Mei's notes) -done

ecolwordsinv
# eats plants should maybe be added, names of food --decided no bc garden context specific
# "omnivore" and "eats many things" should def be added, - done
# "pest predator", "pest catcher", bc talk about eating pests and dietary categories --decided no bc garden context specific

ecolnwords
# only invasive and non native nad overpopulated counted

ecolnwordsinv
# most things

## Gard
# less careful check bc undecided

gardwords  #seems like complete overlap with ecol

gardnwords # not complete overlap with ecoln--about what they eat (plants), being pests

## seehear

seehearwords
# friendly song included - yes

seehearwordsinv
length(seehearwordsinv)
seehearwordsinv[1000:1008]

# delicate, irridescent (shimmery included), striking, music (musical included), eye catching - checked

seehearnwords

seehearnwordsinv
length(seehearnwordsinv)
seehearnwordsinv[1000:1056]
# croaking, squawky, squawk, squawking

## Food

foodwords # eat everything, eats many things -- removed, just in ecol category

foodwordsinv
foodwordsinv[1000:1043]


# Try analysis with and without combining ecol role and garden (probably will combine), same with food

## write out lists of words
allbirds.words.food.freq <- dplyr::count(allbirds.words.food, Word)
head(allbirds.words.food.freq)
write.csv(allbirds.words.food.freq, "all.foodwords.freq.csv", row.names = FALSE)

allbirds.words.ecol.freq <- dplyr::count(allbirds.words.ecol, Word)
head(allbirds.words.ecol.freq)
write.csv(allbirds.words.ecol.freq, "all.ecolwords.freq.csv", row.names = FALSE)

allbirds.words.ecoln.freq <- dplyr::count(allbirds.words.ecoln, Word)
head(allbirds.words.ecoln.freq)
write.csv(allbirds.words.ecoln.freq, "all.ecolnwords.freq.csv", row.names = FALSE)

allbirds.words.gardn.freq <- dplyr::count(allbirds.words.gardn, Word)
head(allbirds.words.gardn.freq)
write.csv(allbirds.words.gardn.freq, "all.gardnwords.freq.csv", row.names = FALSE)

allbirds.words.gard.freq <- dplyr::count(allbirds.words.gard, Word)
head(allbirds.words.gard.freq)
write.csv(allbirds.words.gard.freq, "all.gardwords.freq.csv", row.names = FALSE)

allbirds.words.seehear.freq <- dplyr::count(allbirds.words.seehear, Word)
head(allbirds.words.seehear.freq)
write.csv(allbirds.words.seehear.freq, "all.seehearwords.freq.csv", row.names = FALSE)

allbirds.words.seehearn.freq <- dplyr::count(allbirds.words.seehearn, Word)
head(allbirds.words.seehearn.freq)
write.csv(allbirds.words.seehearn.freq, "all.seehearnwords.freq.csv", row.names = FALSE)

##################################
# Create new columns for analysis
#################################

# Combine food, ecol, gard -- because all refer to birds' role in system (although combines positive of garden with neutral of ecol)
# Combine ecoln, gardn -- refer to negative role in ecosystem or garden
# Add in ecosystem services and disservices recoding, which was done from these two code combinations by hand outside of R
# Treating these as ecological services/disservices (have a conservative and non-conservative codeset)
# Treat see/hear and inverse and aesthetic services and disservies 
# write a loop to automate

sp.df2 <- list(hofi.lc, anhu.lc, lego.lc, hosp.lc, amcr.lc, blph.lc, nomo.lc, amro.lc, casj.lc, rtha.lc)
sp.names <- list("hofi", "anhu", "lego", "hosp", "amcr", "blph", "nomo", "amro", "casj", "rtha")

for (i in 1:length(sp.df2)){
  
  sp.df2[[i]]$ecgarfo <- ifelse(as.data.frame(sp.df2[[i]])$ecol==1, 1,
                      ifelse(as.data.frame(sp.df2[[i]])$gard==1, 1,
                             ifelse(as.data.frame(sp.df2[[i]])$food==1,1,0)))

  sp.df2[[i]]$ecgarn <- ifelse(as.data.frame(sp.df2[[i]])$ecoln==1, 1,
                           ifelse(as.data.frame(sp.df2[[i]])$gardn==1,1,0))
  
  sp.df2[[i]]$es1 <- ifelse(sp.df2[[i]]$Word %in% es1$Word, 1, 0)
  
  sp.df2[[i]]$es2 <- ifelse(sp.df2[[i]]$Word %in% es2$Word, 1, 0)
  
  sp.df2[[i]]$diss1 <- ifelse(sp.df2[[i]]$Word %in% diss1$Word, 1, 0)
  
  sp.df2[[i]]$diss2 <- ifelse(sp.df2[[i]]$Word %in% diss2$Word, 1, 0)
  
}

#check
sp.df2[[2]][c(1:50),]

# delist
hofi.lc2 <- sp.df2[[1]]
anhu.lc2 <- sp.df2[[2]]
lego.lc2 <- sp.df2[[3]]
hosp.lc2 <- sp.df2[[4]]
amcr.lc2 <- sp.df2[[5]]
blph.lc2 <- sp.df2[[6]]
nomo.lc2 <- sp.df2[[7]]
amro.lc2 <- sp.df2[[8]]
casj.lc2 <- sp.df2[[9]]
rtha.lc2 <- sp.df2[[10]]

# Write out lists of words in these new columns by combining code lists

allbirds.words.egf.freq <- rbind(allbirds.words.ecol.freq, allbirds.words.gard.freq, allbirds.words.food.freq)
dim(allbirds.words.egf.freq)
allbirds.words.egf.freq2 <- distinct(allbirds.words.egf.freq) # cut out non-unique rows, from words that were double coded
dim(allbirds.words.egf.freq2)

write.csv(allbirds.words.egf.freq2, "all.ecolgarfoodwords.freq.csv", row.names = FALSE)

allbirds.words.egn.freq <- rbind(allbirds.words.ecoln.freq, allbirds.words.gardn.freq)
dim(allbirds.words.egn.freq)
allbirds.words.egn.freq2 <- distinct(allbirds.words.egn.freq) # cut out non-unique rows, from words that were double coded
dim(allbirds.words.egn.freq2)

write.csv(allbirds.words.egn.freq2, "all.ecolgarnwords.freq.csv", row.names = FALSE)

######################
# Summary stats
######################

### Create a dataframe with # of total response, # pos and neg, % pos and neg per species

# create empty summary stat df
sp.code2.stats <- data.frame(matrix(ncol = 15, nrow = 10))
colnames(sp.code2.stats) <- c("species", "n_words", "n_seehear", "n_ecol", "n_gard", "n_seehearn", "n_ecoln", "n_gardn", "n_food", "n_ecgarfo", "n_ecgarn", "n_es1", "n_es2", "n_diss1", "n_diss2")

# Create a loop to write in total n words, pos, neg
sp.df3 <- list(hofi.lc2, anhu.lc2, lego.lc2, hosp.lc2, amcr.lc2, blph.lc2, nomo.lc2, amro.lc2, casj.lc2, rtha.lc2)
sp.names <- list("hofi", "anhu", "lego", "hosp", "amcr", "blph", "nomo", "amro", "casj", "rtha")

for (i in 1:length(sp.df3)){
  x <- as.data.frame(sp.df3[[i]])
  
  n <- length(x$Word)
  
  x.sh <- subset(x, x$seehear==1)
  x.ec <- subset(x, x$ecol==1)
  x.ga <- subset(x, x$gard==1)
  x.shn <- subset(x, x$seehearn==1)
  x.ecn <- subset(x, x$ecoln==1)
  x.gan <- subset(x, x$garn==1)
  x.fo <- subset(x, x$food==1)
  x.egf <- subset(x, x$ecgarfo==1)
  x.egn <- subset(x, x$ecgarn==1)
  x.es1 <- subset(x, x$es1==1)
  x.es2 <- subset(x, x$es2==1)
  x.diss1 <- subset(x, x$diss1==1)
  x.diss2 <- subset(x, x$diss2==1)
  
  sh <- length(x.sh$Word)
  ec <- length(x.ec$Word)
  ga <- length(x.ga$Word)
  shn <- length(x.shn$Word)
  ecn <- length(x.ecn$Word)
  gan <- length(x.gan$Word)
  fo <- length(x.fo$Word)
  egf <- length(x.egf$Word)
  egn <- length(x.egn$Word)
  es1a <- length(x.es1$Word)
  es2a <- length(x.es2$Word)
  diss1a <- length(x.diss1$Word)
  diss2a <- length(x.diss2$Word)
  
  sp.code2.stats[i,c("species")] <- sp.names[[i]]
  sp.code2.stats[i,c("n_words")] <- n
  sp.code2.stats[i,c("n_seehear")] <- sh
  sp.code2.stats[i,c("n_ecol")] <- ec
  sp.code2.stats[i,c("n_gard")] <- ga
  sp.code2.stats[i,c("n_seehearn")] <- shn 
  sp.code2.stats[i,c("n_ecoln")] <- ecn
  sp.code2.stats[i,c("n_gardn")] <- gan
  sp.code2.stats[i,c("n_food")] <- fo
  sp.code2.stats[i,c("n_ecgarfo")] <- egf
  sp.code2.stats[i,c("n_ecgarn")] <- egn
  sp.code2.stats[i,c("n_es1")] <- es1a
  sp.code2.stats[i,c("n_es2")] <- es2a
  sp.code2.stats[i,c("n_diss1")] <- diss1a
  sp.code2.stats[i,c("n_diss2")] <- diss2a
  
}

sp.code2.stats

# fill in rest of df
sp.code2.stats$perc_seehear <- round((sp.code2.stats$n_seehear/sp.code2.stats$n_words)*100, 1)
sp.code2.stats$perc_ecol <- round((sp.code2.stats$n_ecol/sp.code2.stats$n_words)*100, 1)
sp.code2.stats$perc_gard <- round((sp.code2.stats$n_gard/sp.code2.stats$n_words)*100, 1)
sp.code2.stats$perc_seehearn <- round((sp.code2.stats$n_seehearn/sp.code2.stats$n_words)*100, 1)
sp.code2.stats$perc_ecoln <- round((sp.code2.stats$n_ecoln/sp.code2.stats$n_words)*100, 1)
sp.code2.stats$perc_gardn <- round((sp.code2.stats$n_gardn/sp.code2.stats$n_words)*100, 1)
sp.code2.stats$perc_food <- round((sp.code2.stats$n_food/sp.code2.stats$n_words)*100, 1)
sp.code2.stats$perc_ecgarfo <- round((sp.code2.stats$n_ecgarfo/sp.code2.stats$n_words)*100, 1)
sp.code2.stats$perc_ecgarn <- round((sp.code2.stats$n_ecgarn/sp.code2.stats$n_words)*100, 1)
sp.code2.stats$perc_es1 <- round((sp.code2.stats$n_es1/sp.code2.stats$n_words)*100, 1)
sp.code2.stats$perc_es2 <- round((sp.code2.stats$n_es2/sp.code2.stats$n_words)*100, 1)
sp.code2.stats$perc_diss1 <- round((sp.code2.stats$n_diss1/sp.code2.stats$n_words)*100, 1)
sp.code2.stats$perc_diss2 <- round((sp.code2.stats$n_diss2/sp.code2.stats$n_words)*100, 1)

dim(sp.code2.stats)
sp.code2.stats[,c(1,16:28)]

# write it out
write.csv(sp.code2.stats, "~/Desktop/Old_comp_transfer/Stanford/Research/BA_birds/Fieldwork/Data/sp.codeset2.stats.csv", row.names = FALSE)


######################
# Visualize
#######################

# order alphabetically
sp.allstats <- sp.allstats[order(sp.allstats$species),]

# for color selection: https://mdigi.tools/change-color-hue/#c61ebb

# separate into n words and perc dfs, melted for visualization
sp.allstats.perc <- sp.allstats[,c("species", "perc_seehear", "perc_seehearn", "perc_es1", "perc_diss2", "netsentvtot")]
sp.allstats.perc <- melt(sp.allstats.perc, id = c("species", "netsentvtot"))
head(sp.allstats.perc)

ggplot(sp.allstats.perc, aes(fill=variable, y=value, x=reorder(species, netsentvtot))) + # , x=reorder(species, netsentvtot) left out but will want to order in same as sentiment score figure, probably?
  geom_bar(position="stack", stat="identity") +
  # scale_fill_manual(values=c("#9496c3", "#c3a394", "#94c3c1", "#c3c094"), labels=c("Aesthetic", "Aesthetic disservice", "Ecological", "Ecological disservice"), name=("Sentiment")) +
  ylab("% of words") +
  xlab("Species\n") +
  geom_text(aes(label = netsentvtot, y = 50), vjust = 0.4, size = 8, color= "#5A5A5A") + # add in the unweighted sentiment score
  coord_flip() +
  theme_classic() +
  theme(axis.text = element_text(size = 28), 
        axis.title = element_text(size = 36),
        axis.title.x = element_text(vjust=-0.75),
        legend.text = element_text(size = 28), 
        legend.title = element_text(size = 36),
        legend.box.spacing=unit(20, "pt"),
        legend.spacing.y = unit(0.5, 'cm'))+
  ## important additional element
  guides(fill = guide_legend(byrow = TRUE))

# old color palette for dissertation values=c("#9496c3", "#c3a394", "#94c3c1", "#c3c094")
# new color palette for publication values=c("#9496c3", "#c3a394", "#94c3c1", "#c3c094")
pdf("./Figures/Fig4e_servicecodes_perc50.pdf", height = 12, width = 18)
ggplot(sp.allstats.perc, aes(fill=factor(variable, levels=c('perc_diss2', 'perc_es1', 'perc_seehearn', 'perc_seehear')), 
                             y=value, 
                             x=reorder(species, netsentvtot))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c( "#C17A58", "#6165C7", "#DCBF43", "#4DC5C0"),
                    labels=c("Ecological disservice", "Ecological service","Aesthetic disservice","Aesthetic service"),
                    name=("Ecosystem services"),
                    guide = guide_legend(reverse=TRUE)) +
  ylab("% of words") +
  xlab("Species\n") +
  geom_text(aes(label = netsentvtot, y = 49), vjust = 0.4, size = 11, color= "#5A5A5A") + # add in the unweighted sentiment score
  coord_flip() +
  theme_classic() +
  ylim(0,50) +
  theme(axis.text = element_text(size = 32), 
        axis.title = element_text(size = 36),
        axis.title.x = element_text(vjust=-0.75),
        legend.text = element_text(size = 28), 
        legend.title = element_text(size = 36),
        legend.box.spacing=unit(20, "pt"),
        legend.spacing.y = unit(0.5, 'cm')) + 
  scale_y_continuous(limits = c(0, 51), breaks = c(0,10,20,30,40,50))
dev.off()

# Version with x axis going to 100
pdf("./Figures/Fig4e_servicecodes_perc100.pdf", height = 12, width = 18)
ggplot(sp.allstats.perc, aes(fill=factor(variable, levels=c('perc_diss2', 'perc_es1', 'perc_seehearn', 'perc_seehear')), 
                             y=value, 
                             x=reorder(species, netsentvtot))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c("#9496c3", "#c3a394", "#94c3c1", "#c3c094"),
                    labels=c("Ecological disservice", "Ecological service","Aesthetic disservice","Aesthetic service"),
                    name=("Ecosystem services"),
                    guide = guide_legend(reverse=TRUE)) +
  ylab("% of words") +
  xlab("Species\n") +
  geom_text(aes(label = netsentvtot, y = 98), vjust = 0.4, size = 8, color= "#5A5A5A") + # add in the unweighted sentiment score
  coord_flip() +
  theme_classic() +
  ylim(0,100) +
  theme(axis.text = element_text(size = 28), 
        axis.title = element_text(size = 36),
        axis.title.x = element_text(vjust=-0.75),
        legend.text = element_text(size = 28), 
        legend.title = element_text(size = 36),
        legend.box.spacing=unit(20, "pt"),
        legend.spacing.y = unit(0.5, 'cm'))
dev.off()
