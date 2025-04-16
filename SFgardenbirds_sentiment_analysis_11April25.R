## SF Garden Birds sentiment analysis
# Author: Kelley Langhans
# Last updated: 11 April 2025

#################
# Set WD
#################
# Set working directory to where project data is stored
# setwd("")

###################
# load libraries
##################

library(tidyverse)
library(openxlsx)
library(lubridate)
library(reshape2)
library(wordcloud)
library(RColorBrewer)
library(dplyr)

##################
# read in data
##################

# read in sentiment data for each of 10 focal species
hofi <-  read.xlsx("gardener_survey_species_qual_sentiment.xlsx", sheet = "Q24_hofi")
anhu <-  read.xlsx("gardener_survey_species_qual_sentiment.xlsx", sheet = "Q30_anhu")
lego <-  read.xlsx("gardener_survey_species_qual_sentiment.xlsx", sheet = "Q36_lego")
hosp <-  read.xlsx("gardener_survey_species_qual_sentiment.xlsx", sheet = "Q42_hosp")
amcr <-  read.xlsx("gardener_survey_species_qual_sentiment.xlsx", sheet = "Q48_amcr")
blph <-  read.xlsx("gardener_survey_species_qual_sentiment.xlsx", sheet = "Q54_blph")
nomo <-  read.xlsx("gardener_survey_species_qual_sentiment.xlsx", sheet = "Q60_nomo")
amro <-  read.xlsx("gardener_survey_species_qual_sentiment.xlsx", sheet = "Q66_amro")
casj <-  read.xlsx("gardener_survey_species_qual_sentiment.xlsx", sheet = "Q72_casj")
rtha <-  read.xlsx("gardener_survey_species_qual_sentiment.xlsx", sheet = "Q78_rtha")

#read in % recognition data
recog4 <- read.csv("recog.csv")

###################
# examine data
##################

head(hofi)
dim(hofi) # 165 responses (our n is 165!)


#######################
# reformat data
#######################

# Separate into different dataframe for each word column/coding columns, rename those columns
hofi1 <- hofi[,c(1,3:5)]
hofi2 <- hofi[,c(1,6:8)]
hofi3 <- hofi[,c(1,9:11)]
hofi4 <- hofi[,c(1,12:14)]
head(hofi1)
head(hofi2)
head(hofi3)
head(hofi4)
colnames(hofi1) <- c("ResponsesId", "Word", "pos", "neg")
colnames(hofi2) <- c("ResponsesId", "Word", "pos", "neg")
colnames(hofi3) <- c("ResponsesId", "Word", "pos", "neg")
colnames(hofi4) <- c("ResponsesId", "Word", "pos", "neg")

# Combine them all together into a single df with 1 word and 2 sentiment columns
hofi.l <-rbind(hofi1, hofi2, hofi3, hofi4)
head(hofi.l)
dim(hofi.l) # 660=165*4

# Cut out all NA responses
hofi.lc <- hofi.l[complete.cases(hofi.l[,2]),]
dim(hofi.lc) # 294
head(hofi.lc)

# Repeat for other 9 species
sp.df <- list(anhu, lego, hosp, amcr, blph, nomo, amro, casj, rtha)
sp.df.final <- replicate(9,data.frame())

for (i in 1:length(sp.df)){
  x <- as.data.frame(sp.df[[i]])
  
  x1 <- x[,c(1,3:5)]
  x2 <- x[,c(1,6:8)]
  x3 <- x[,c(1,9:11)]
  colnames(x1) <- c("ResponsesId", "Word", "pos", "neg")
  colnames(x2) <- c("ResponsesId", "Word", "pos", "neg")
  colnames(x3) <- c("ResponsesId", "Word", "pos", "neg")
  
  # deal with the fact that some birds people responded 3-5 words
  if (dim(x)[2] == 14){
    
    x4 <- x[,c(1,12:14)]
    colnames(x4) <- c("ResponsesId", "Word", "pos", "neg")
    x.l <-rbind(x1, x2, x3, x4)
    
  } else if (dim(x)[2] == 17){
    
    x4 <- x[,c(1,12:14)]
    colnames(x4) <- c("ResponsesId", "Word", "pos", "neg")
    x5 <- x[,c(1,15:17)]
    colnames(x5) <- c("ResponsesId", "Word", "pos", "neg")
    x.l <-rbind(x1, x2, x3, x4,x5)
    
  } else {
    
    x.l <-rbind(x1, x2, x3)
    
  }
  
  x.lc <- x.l[complete.cases(x.l[,2]),]
  
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

# Create a single master words list across all birds
allbirds.words <- rbind(hofi.lc, anhu.lc, lego.lc, hosp.lc, amcr.lc,
                        blph.lc, nomo.lc, amro.lc, casj.lc, rtha.lc)
dim(allbirds.words)
head(allbirds.words) # 2911

write.csv(allbirds.words, "allwords_posnegneu.csv", row.names=FALSE)

####################
# Quality check
#######################

# Create a list of all positive words
allbirds.words.pos <- subset(allbirds.words, allbirds.words$pos==1)
dim(allbirds.words.pos) #1030

# Create a list of all negative words
allbirds.words.neg <- subset(allbirds.words, allbirds.words$neg==1)
dim(allbirds.words.neg) # 307

# Create a list of all neutral words
allbirds.words.neu <- subset(allbirds.words, (allbirds.words$pos==0 & allbirds.words$neg==0))
dim(allbirds.words.neu) # 1574

# add up to 2911, no individual words are being double-counted

# check that lists are mutually exclusive

# list of all unique positive words
poswords <- unique(allbirds.words.pos$Word)
neuwords <- unique(allbirds.words.neu$Word)
negwords <- unique(allbirds.words.neg$Word)

poswords

# look at intersection between all 3 lists

# pos/neu
Reduce(intersect, list(poswords, neuwords))

# pos/neg
Reduce(intersect, list(poswords, negwords))

#neg/neu
Reduce(intersect, list(neuwords, negwords))

# No overlaps, no words being coded in 2 different ways!

#############################
# Sentiment summary stats
############################

### Create a dataframe with # of total response, # pos and neg, % pos and neg per species

# create empty summary stat df
sp.sentiment.stats <- data.frame(matrix(ncol = 8, nrow = 10))
colnames(sp.sentiment.stats) <- c("species", "n_words", "n_pos", "n_neg", "n_neu", "perc_pos", "perc_neg", "perc_neu")

# Create a loop to write in total n words, pos, neg
sp.df2 <- list(hofi.lc, anhu.lc, lego.lc, hosp.lc, amcr.lc, blph.lc, nomo.lc, amro.lc, casj.lc, rtha.lc)
sp.names <- list("hofi", "anhu", "lego", "hosp", "amcr", "blph", "nomo", "amro", "casj", "rtha")

for (i in 1:length(sp.df2)){
  x <- as.data.frame(sp.df2[[i]])
  
  n <- length(x$Word)
  
  x.pos <- subset(x, x$pos==1)
  x.neg <- subset(x, x$neg==1)
  
  pos <- length(x.pos$Word)
  neg <- length(x.neg$Word)
  
  sp.sentiment.stats[i,c("species")] <- sp.names[[i]]
  sp.sentiment.stats[i,c("n_words")] <- n
  sp.sentiment.stats[i,c("n_pos")] <- pos
  sp.sentiment.stats[i,c("n_neg")] <- neg
}

sp.sentiment.stats

# fill in rest of df
sp.sentiment.stats$n_neu <- sp.sentiment.stats$n_words - (sp.sentiment.stats$n_pos + sp.sentiment.stats$n_neg)
sp.sentiment.stats$perc_pos <- round((sp.sentiment.stats$n_pos/sp.sentiment.stats$n_words)*100, 1)
sp.sentiment.stats$perc_neg <- round((sp.sentiment.stats$n_neg/sp.sentiment.stats$n_words)*100, 1)
sp.sentiment.stats$perc_neu <- round((sp.sentiment.stats$n_neu/sp.sentiment.stats$n_words)*100, 1)

sp.sentiment.stats

# calculate a summed sentiment statistic (pos -neg)/total sent (bounded -1 to 1)
sp.sentiment.stats$netsentvtot <- round((sp.sentiment.stats$n_pos - sp.sentiment.stats$n_neg )/(sp.sentiment.stats$n_neg +sp.sentiment.stats$n_pos), 2)

# multiply that stat by the % of ppl that recog for access
sp.sentiment.stats$Species_ID <- toupper(sp.sentiment.stats$species)
sp.sentiment.stats2 <- merge(sp.sentiment.stats, recog4, by = c("Species_ID"))
sp.sentiment.stats2

sp.sentiment.stats2$sentscore_weight <- sp.sentiment.stats2$netsentvtot*sp.sentiment.stats2$perc_rec

# order alphabetically
sp.sentiment.stats2 <- sp.sentiment.stats2[order(sp.sentiment.stats2$species),]

write.csv(sp.sentiment.stats2, "~/Desktop/Old_comp_transfer/Stanford/Research/BA_birds/Fieldwork/Data/sp.sentiment.stats.csv", row.names = FALSE)



### visualization

# for color selection: https://mdigi.tools/change-color-hue/#c61ebb
# old color palette for dissertation values=c("#99c394", "#ababab", "#c394a4"), labels=c("Positive", "Neutral", "Negative")
# new color palette for publication values=c("#418E48", "#B9B9B9", "#D0609D"), labels=c("Positive", "Neutral", "Negative")

# separate into n words and perc dfs, melted for visualization
sp.sentiment.stats.n <- sp.sentiment.stats2[,c("species", "n_pos", "n_neu", "n_neg", "netsentvtot")]
sp.sentiment.stats.n <- melt(sp.sentiment.stats.n, id = c("species", "netsentvtot"))

sp.sentiment.stats.perc <- sp.sentiment.stats2[,c("species", "perc_pos", "perc_neu", "perc_neg", "netsentvtot")]
sp.sentiment.stats.perc <- melt(sp.sentiment.stats.perc, id = c("species", "netsentvtot"))

# stacked bar plot of number of words
# colors adjusted and checked for color blind palette
ggplot(sp.sentiment.stats.n, aes(fill=variable, y=value, x=reorder(species, netsentvtot))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c("#99c394", "#ababab", "#c394b1"), labels=c("Positive", "Neutral", "Negative"), name="Sentiment") +
  ylab("Number of words") +
  xlab("Species") +
  geom_text(aes(label = netsentvtot, y = 400), vjust = -0.2, size = 6) + # add in the unweighted sentiment score
  coord_flip() +
  theme_classic() +
  theme(axis.text = element_text(size = 36), 
        axis.title = element_text(size = 36),
        legend.text = element_text(size = 36), 
        legend.title = element_text(size = 36))

# print out, with some adjustments for visualization
pdf("./Figures/Fig4c_sentimentstack_raw.pdf", height = 12, width = 15)
ggplot(sp.sentiment.stats.n, aes(fill=variable, y=value, x=reorder(species, netsentvtot))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c("#99c394", "#ababab", "#c394b1"), labels=c("Positive", "Neutral", "Negative"), name="Sentiment") +
  ylab("Number of words") +
  xlab("Species\n") +
  geom_text(aes(label = netsentvtot, y = 400), vjust = 0.4, size = 8, color= "#5A5A5A") + # add in the unweighted sentiment score
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
dev.off()

# stacked bar plot of number of words
# no neutral workds
# colors adjusted and checked for color blind palette
sp.sentiment.stats.noneu <- subset(sp.sentiment.stats.n, !(sp.sentiment.stats.n$variable=="n_neu"))

ggplot(sp.sentiment.stats.noneu, aes(fill=variable, y=value, x=reorder(species, netsentvtot))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c("#418E48", "#D0609D"), labels=c("Positive", "Negative"), name="Sentiment") +
  ylab("Number of words") +
  xlab("Species") +
  geom_text(aes(label = netsentvtot, y = 250), vjust = -0.2, size = 10) + # add in the unweighted sentiment score
  coord_flip() +
  theme_classic() +
  theme(axis.text = element_text(size = 36), 
        axis.title = element_text(size = 36),
        legend.text = element_text(size = 36), 
        legend.title = element_text(size = 36))


# print out, with some adjustments for visualization
pdf("./Figures/Fig4c_sentimentstack_noneu_raw.pdf", height = 12, width = 15)
ggplot(sp.sentiment.stats.noneu, aes(fill=variable, y=value, x=reorder(species, netsentvtot))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c("#418E48", "#D0609D"), labels=c("Positive", "Negative"), name="Sentiment") +
  ylab("Number of words") +
  xlab("Species\n") +
  geom_text(aes(label = netsentvtot, y = 250), vjust = 0.4, size = 11, color= "#5A5A5A") + # add in the unweighted sentiment score
  coord_flip() +
  theme_classic() +
  theme(axis.text = element_text(size = 32), 
        axis.title = element_text(size = 36),
        axis.title.x = element_text(vjust=-0.75),
        legend.text = element_text(size = 28), 
        legend.title = element_text(size = 36),
        legend.box.spacing=unit(20, "pt"),
        legend.spacing.y = unit(0.5, 'cm'))+
  scale_y_continuous(limits = c(0, 255), breaks = c(0,50,100,150,200,250)) + 
  ## important additional element
  guides(fill = guide_legend(byrow = TRUE))
dev.off()


# stacked bar plot of % responses
ggplot(sp.sentiment.stats.perc, aes(fill=variable, y=value, x=species)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values=c("#99c394", "#ababab", "#c394b1"), labels=c("Positive", "Neutral", "Negative"), name="Sentiment") +
  ylab("Percent of words") +
  xlab("Species") +
  geom_text(aes(label = netsentvtot, y = 1.10), vjust = -0.2) + # add in the unweighted sentiment score
  coord_flip() +
  theme_classic()

# summed statistic plot 

ggplot(sp.sentiment.stats2, aes(y=reorder(species, sentscore_weight), x=sentscore_weight)) + 
  geom_point()+
  scale_x_continuous(limits = c(-1,1)) +
  geom_vline(xintercept = 0) +
  xlab("Weighted sentiment score") +
  ylab("Species") +
  # labs(title = "Species word association sentiment analysis") +
  theme_classic()

pdf("./Figures/Fig4d_sentimentscoreweight_raw.pdf", height = 12, width = 15)
ggplot(sp.sentiment.stats2, aes(y=reorder(species, sentscore_weight), x=sentscore_weight)) + 
  geom_point(size=6)+
  scale_x_continuous(limits = c(-1,1)) +
  geom_vline(xintercept = 0) +
  xlab("Weighted sentiment score") +
  ylab("Species\n") +
  theme_classic() +
  theme(axis.text = element_text(size = 32), 
        axis.title = element_text(size = 36),
        axis.title.x = element_text(vjust=-0.75))
dev.off()

###########################
# Word clouds!
##########################

# word cloud for all positive words
# frequency df
allbirds.words.pos.freq <- dplyr::count(allbirds.words.pos, Word)
head(allbirds.words.pos.freq)
set.seed(4567) # for reproducibility 
wordcloud(words = allbirds.words.pos.freq$Word, freq = allbirds.words.pos.freq$n, min.freq = 2, 
          max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Paired"))

write.csv(allbirds.words.pos.freq, "all.poswords.freq.csv", row.names = FALSE)

# word cloud for all negative words
allbirds.words.neg.freq <- dplyr::count(allbirds.words.neg, Word)
head(allbirds.words.neg.freq)
set.seed(4569) # for reproducibility 
wordcloud(words = allbirds.words.neg.freq$Word, freq = allbirds.words.neg.freq$n, min.freq = 2, 
          max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Paired"))

write.csv(allbirds.words.neg.freq, "all.negwords.freq.csv", row.names = FALSE)

# neutral
allbirds.words.neu.freq <- dplyr::count(allbirds.words.neu, Word)
head(allbirds.words.neu.freq)
write.csv(allbirds.words.neu.freq, "all.neuwords.freq.csv", row.names = FALSE)

# word cloud for all words
allbirds.words2 <- allbirds.words
allbirds.words2$sentiment <- ifelse(allbirds.words2$pos==1, "pos",
                            ifelse(allbirds.words2$neg==1, "neg", "neu"))
allbirds.words.freq <- dplyr::count(allbirds.words2, Word, sentiment)
dim(allbirds.words.freq)
head(allbirds.words.freq)
set.seed(4569) # for reproducibility 
wordcloud(words = allbirds.words.freq$Word, freq = allbirds.words.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#D0609D", "#B9B9B9", "#418E48")[factor(allbirds.words.freq$sentiment)])


write.csv(allbirds.words.freq, "allbirds.words.freq.csv", row.names = FALSE)
View(allbirds.words.freq)

# word cloud for each species

#hofi
head(hofi.lc)
hofi.lc$sentiment <- ifelse(hofi.lc$pos==1, "pos",
                            ifelse(hofi.lc$neg==1, "neg", "neu"))
hofi.freq <- dplyr::count(hofi.lc, Word, sentiment)
head(hofi.freq)
set.seed(4569) # for reproducibility 
wordcloud(words = hofi.freq$Word, freq = hofi.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#D0609D", "#B9B9B9", "#418E48")[factor(hofi.freq$sentiment)])

pdf("./Figures/hofi_wordcloud.pdf", height = 6, width = 6)
set.seed(4569) # for reproducibility 
wordcloud(words = hofi.freq$Word, freq = hofi.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#D0609D", "#B9B9B9", "#418E48")[factor(hofi.freq$sentiment)])
dev.off()


#anhu
head(anhu.lc)
anhu.lc$sentiment <- ifelse(anhu.lc$pos==1, "pos",
                            ifelse(anhu.lc$neg==1, "neg", "neu"))
anhu.freq <- dplyr::count(anhu.lc, Word, sentiment)
head(anhu.freq)
set.seed(4569) # for reproducibility 
wordcloud(words = anhu.freq$Word, freq = anhu.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#c394a4", "#B9B9B9", "#418E48")[factor(anhu.freq$sentiment)])

pdf("./Figures/anhu_wordcloud.pdf", height = 6, width = 6)
set.seed(4569) # for reproducibility 
wordcloud(words = anhu.freq$Word, freq = anhu.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#D0609D", "#B9B9B9", "#418E48")[factor(anhu.freq$sentiment)])
dev.off()

#lego
head(lego.lc)
lego.lc$sentiment <- ifelse(lego.lc$pos==1, "pos",
                            ifelse(lego.lc$neg==1, "neg", "neu"))
lego.freq <- dplyr::count(lego.lc, Word, sentiment)
head(lego.freq)
set.seed(4568) # for reproducibility 
wordcloud(words = lego.freq$Word, freq = lego.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#c394a4", "#B9B9B9", "#418E48")[factor(lego.freq$sentiment)])

pdf("./Figures/lego_wordcloud.pdf", height = 6, width = 6)
set.seed(4568) # for reproducibility 
wordcloud(words = lego.freq$Word, freq = lego.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#D0609D", "#B9B9B9", "#418E48")[factor(lego.freq$sentiment)])
dev.off()

#hosp
head(hosp.lc)
hosp.lc$sentiment <- ifelse(hosp.lc$pos==1, "pos",
                            ifelse(hosp.lc$neg==1, "neg", "neu"))
hosp.freq <- dplyr::count(hosp.lc, Word, sentiment)
head(hosp.freq)
set.seed(4568) # for reproducibility 
wordcloud(words = hosp.freq$Word, freq = hosp.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#c394a4", "#B9B9B9", "#418E48")[factor(hosp.freq$sentiment)])

pdf("./Figures/hosp_wordcloud.pdf", height = 6, width = 6)
set.seed(4568) # for reproducibility 
wordcloud(words = hosp.freq$Word, freq = hosp.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#D0609D", "#B9B9B9", "#418E48")[factor(hosp.freq$sentiment)])
dev.off()

#amcr
head(amcr.lc)
amcr.lc$sentiment <- ifelse(amcr.lc$pos==1, "pos",
                            ifelse(amcr.lc$neg==1, "neg", "neu"))
amcr.freq <- dplyr::count(amcr.lc, Word, sentiment)
head(amcr.freq)
set.seed(4569) # for reproducibility 
wordcloud(words = amcr.freq$Word, freq = amcr.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#c394a4", "#B9B9B9", "#418E48")[factor(amcr.freq$sentiment)])

pdf("./Figures/amcr_wordcloud.pdf", height = 6, width = 6)
set.seed(4569) # for reproducibility 
wordcloud(words = amcr.freq$Word, freq = amcr.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#D0609D", "#B9B9B9", "#418E48")[factor(amcr.freq$sentiment)])
dev.off()

#blph
head(blph.lc)
blph.lc$sentiment <- ifelse(blph.lc$pos==1, "pos",
                            ifelse(blph.lc$neg==1, "neg", "neu"))
blph.freq <- dplyr::count(blph.lc, Word, sentiment)
head(blph.freq)
set.seed(4569) # for reproducibility 
wordcloud(words = blph.freq$Word, freq = blph.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#B9B9B9","#418E48")[factor(blph.freq$sentiment)])

pdf("./Figures/blph_wordcloud.pdf", height = 6, width = 6)
set.seed(4569) # for reproducibility 
wordcloud(words = blph.freq$Word, freq = blph.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c( "#B9B9B9", "#418E48")[factor(blph.freq$sentiment)])
dev.off()

#nomo
head(nomo.lc)
nomo.lc$sentiment <- ifelse(nomo.lc$pos==1, "pos",
                            ifelse(nomo.lc$neg==1, "neg", "neu"))
nomo.freq <- dplyr::count(nomo.lc, Word, sentiment)
head(nomo.freq)
set.seed(4569) # for reproducibility 
wordcloud(words = nomo.freq$Word, freq = nomo.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#c394a4", "#B9B9B9", "#418E48")[factor(nomo.freq$sentiment)])

pdf("./Figures/nomo_wordcloud.pdf", height = 6, width = 6)
set.seed(4569) # for reproducibility 
wordcloud(words = nomo.freq$Word, freq = nomo.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#D0609D", "#B9B9B9", "#418E48")[factor(nomo.freq$sentiment)])
dev.off()

#amro
head(amro.lc)
amro.lc$sentiment <- ifelse(amro.lc$pos==1, "pos",
                            ifelse(amro.lc$neg==1, "neg", "neu"))
amro.freq <- dplyr::count(amro.lc, Word, sentiment)
head(amro.freq)
set.seed(4569) # for reproducibility 
wordcloud(words = amro.freq$Word, freq = amro.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#c394a4", "#B9B9B9", "#418E48")[factor(amro.freq$sentiment)])

pdf("./Figures/amro_wordcloud.pdf", height = 6, width = 6)
set.seed(4569) # for reproducibility 
wordcloud(words = amro.freq$Word, freq = amro.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#D0609D", "#B9B9B9", "#418E48")[factor(amro.freq$sentiment)])
dev.off()

#casj
head(casj.lc)
casj.lc$sentiment <- ifelse(casj.lc$pos==1, "pos",
                            ifelse(casj.lc$neg==1, "neg", "neu"))
casj.freq <- dplyr::count(casj.lc, Word, sentiment)
head(casj.freq)
set.seed(4569) # for reproducibility 
wordcloud(words = casj.freq$Word, freq = casj.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#c394a4", "#B9B9B9", "#418E48")[factor(casj.freq$sentiment)])

pdf("./Figures/casj_wordcloud.pdf", height = 6, width = 6)
set.seed(4569) # for reproducibility 
wordcloud(words = casj.freq$Word, freq = casj.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#D0609D", "#B9B9B9", "#418E48")[factor(casj.freq$sentiment)])
dev.off()

#rtha
head(rtha.lc)
rtha.lc$sentiment <- ifelse(rtha.lc$pos==1, "pos",
                            ifelse(rtha.lc$neg==1, "neg", "neu"))
rtha.freq <- dplyr::count(rtha.lc, Word, sentiment)
head(rtha.freq)
set.seed(4568) # for reproducibility 
wordcloud(words = rtha.freq$Word, freq = rtha.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#c394a4", "#B9B9B9", "#418E48")[factor(rtha.freq$sentiment)])

pdf("./Figures/rtha_wordcloud.pdf", height = 6, width = 6)
set.seed(4568) # for reproducibility 
wordcloud(words = rtha.freq$Word, freq = rtha.freq$n, min.freq = 1, 
          max.words=400, random.order=FALSE, rot.per=0.35, ordered.colors=TRUE, colors=c("#D0609D", "#B9B9B9", "#418E48")[factor(rtha.freq$sentiment)])
dev.off()

