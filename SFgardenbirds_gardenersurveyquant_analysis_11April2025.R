# Exploratory visualization of data from SF Garden Birds Project gardener survey
# Author: Kelley Langhans
# Last updated: 11 April 2025

#################
# Set WD
#################
# Set to whereever you have project data stored
# setwd("")

###################
# load libraries
##################

library(tidyverse)
library(openxlsx)
library(reshape)
library(ggrepel)
library(scales)
library(colorspace)

##################
# read in data
##################

# Survey data on quantitative responses about 10 focal species
like_sp <-  read.xlsx("Birdspecies_master.xlsx", sheet = "likert")
enc_sp <- read.xlsx("Birdspecies_master.xlsx", sheet = "encounter")

# Survey data on species recognition
survey_raw <- read.xlsx("species_recognition_raw.xlsx", sheet = "Cleaned_Data")

# Survey data on attitudes towards birds in general
q19 <- read.csv("Q19_likert_allbirds.csv")

#################
# explore data
#################

like_sp
dim(like_sp)
enc_sp
dim(enc_sp)
q19

# cut!
head(survey_raw)

###################
# Clean data
##################

seehear <- subset(like_sp, like_sp$'#'==1)
birdecol <- subset(like_sp, like_sp$'#'==2)
birdplant <- subset(like_sp, like_sp$'#'==3)

# center the mean around 0, changing 1 (less) to -1, 2 (same) to 0, 3 (more) to 1. Do this by subtracting 2
enc_sp$mean_cent <- enc_sp$Mean - 2

# center mean around 0 for q19
q19$mean_cent <- q19$Mean - 4

#####################
# Visualize data
######################

# I like to see hear this species
seehear$species<- factor(seehear$species, levels=rev(sort(seehear$species)))

p3<-ggplot(seehear, aes(x=species, y=Mean)) + 
  geom_point()+
  geom_errorbar(aes(ymin=Mean-Std.Deviation, ymax=Mean+Std.Deviation), width=.2,
                position=position_dodge(0.05)) +
  scale_y_continuous(breaks=c(1:7), limits = c(0,8)) +
  # ylim(0,8) +
  coord_flip() +
  geom_hline(yintercept = 4) +
  labs(title = "I like to see and hear this species")

p3

pdf("./Figures/seehear_sp.pdf", height = 6, width = 9)
p3
dev.off()

# This species plays an important ecol role
birdecol$species<- factor(birdecol$species, levels=rev(sort(birdecol$species)))

p4<-ggplot(birdecol, aes(x=species, y=Mean)) + 
  geom_point()+
  geom_errorbar(aes(ymin=Mean-Std.Deviation, ymax=Mean+Std.Deviation), width=.2,
                position=position_dodge(0.05)) +
  scale_y_continuous(breaks=c(1:7), limits = c(0,8)) +
  # ylim(0,8) +
  coord_flip() +
  geom_hline(yintercept = 4) +
  labs(title = "This species plays an important ecological role in the garden")

p4

pdf("./Figures/birdecol_sp.pdf", height = 6, width = 9)
p4
dev.off()

# This species is benificial to the plants I am growing 
birdplant$species<- factor(birdplant$species, levels=rev(sort(birdplant$species)))

p5<-ggplot(birdplant, aes(x=species, y=Mean)) + 
  geom_point()+
  geom_errorbar(aes(ymin=Mean-Std.Deviation, ymax=Mean+Std.Deviation), width=.2,
                position=position_dodge(0.05)) +
  scale_y_continuous(breaks=c(1:7), limits = c(0,8)) +
  # ylim(0,8) +
  coord_flip() +
  geom_hline(yintercept = 4) +
  labs(title = "This species is beneficial to the plants I am growing in the garden")

p5

pdf("./Figures/birdplant_sp.pdf", height = 6, width = 9)
p5
dev.off()

# I want to encounter this species
enc_sp$Species<- factor(enc_sp$Species, levels=rev(sort(enc_sp$Species)))

p6<-ggplot(enc_sp, aes(x=Species, y=mean_cent)) + 
  geom_point()+
  geom_errorbar(aes(ymin=mean_cent-Std.Deviation, ymax=mean_cent+Std.Deviation), width=.2,
                position=position_dodge(0.05)) +
  scale_y_continuous(breaks=c(-1:1), limits = c(-1.5,1.5)) +
  # ylim(0,8) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  labs(title = "Desire to encounter species in the garden")

p6

pdf("./Figures/enc_sp.pdf", height = 6, width = 9)
p6
dev.off()


########################
# Q19 - attitudes about birds in the garden overall
##########################

# make a dataframe for raw data for Q19
q19_raw <- survey_raw[,c("ResponseId", "Q19_1", "Q19_2","Q19_3","Q19_4","Q19_5","Q19_6" )]
head(q19_raw)

# separate names
q19_names <- q19_raw[1,]
q19_names[1,] <- gsub("Rate your agreement with the following statements: - ", "", q19_names)
q19_names.l <- melt(q19_names, id = c("ResponseId"), na.rm = TRUE)
colnames(q19_names.l) <- c("ResponseId", "variable", "Qtext")

# melt
q19_raw2 <- q19_raw[-1,]
q19_raw3 <- melt(q19_raw2, id = c("ResponseId"), na.rm = TRUE)
head(q19_raw3)


## try diverging bar chart! 

# summarize data
q19_raw3_summary <- q19_raw3 %>% 
  dplyr::group_by(variable, value) %>% 
  dplyr::count(name = "n_answers") %>% 
  dplyr::group_by(variable) %>% 
  dplyr::mutate(percent_answers = n_answers / sum(n_answers)) %>% 
  ungroup() %>% 
  mutate(percent_answers_label = percent(percent_answers, accuracy = 1))
q19_raw3_summary

# standard bar chart (gives random orders for categories)
q19_raw3_summary %>%
  ggplot(aes(x = variable, 
             y = percent_answers,
             fill = value)) +
  geom_col() +
  geom_text(aes(label = percent_answers_label),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold") +
  coord_flip() +
  scale_x_discrete() +
  scale_fill_viridis_d() +
  labs(title = "Rate your agreement with the following statements",
       x = NULL,
       fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top")

# basic diverging bar chart
q19_raw3_summary_diverging <- q19_raw3_summary %>%
  mutate(percent_answers = if_else(value %in% c("Strongly agree", "Agree", "Slightly agree"), percent_answers, -percent_answers)) %>% 
  mutate(percent_answers_label = percent(percent_answers, accuracy = 1))
q19_raw3_summary_diverging

q19_raw3_summary_diverging %>%
  ggplot(aes(x = variable, 
             y = percent_answers,
             fill = value)) +
  geom_col() +
  geom_text(aes(label = percent_answers_label),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold") +
  coord_flip() +
  scale_x_discrete() +
  scale_fill_viridis_d() +
  labs(title = "Rate agreement",
       x = NULL,
       fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top")

# relabel to make negative % labels not display as neg

q19_raw3_summary_diverging_good_labels <- q19_raw3_summary_diverging %>%
  mutate(percent_answers_label = abs(percent_answers)) %>% 
  mutate(percent_answers_label = percent(percent_answers_label, accuracy = 1))
q19_raw3_summary_diverging_good_labels

q19_raw3_summary_diverging_good_labels %>% 
  ggplot(aes(x = variable, 
             y = percent_answers,
             fill = value)) +
  geom_col() +
  geom_text(aes(label = percent_answers_label),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold") +
  coord_flip() +
  scale_x_discrete() +
  scale_fill_viridis_d() +
  labs(title = "How good is the education at your school?",
       x = NULL,
       fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top")

# reorder bars
q19_raw3_summary_diverging_right_order <- q19_raw3_summary_diverging_good_labels %>% 
  dplyr::mutate(value = fct_relevel(value,
                               "Neither agree nor disagree", "Slightly disagree","Disagree","Strongly disagree", "Slightly agree", "Agree", "Strongly agree"),
         value = fct_rev(value)) 
q19_raw3_summary_diverging_right_order

q19_raw3_summary_diverging_right_order %>%
  ggplot(aes(x = variable, 
             y = percent_answers,
             fill = value)) +
  geom_col() +
  geom_text(
    aes(label = percent_answers_label),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold"
  ) +
  coord_flip() +
  scale_x_discrete() +
  scale_fill_viridis_d() +
  labs(title = "How good is the education at your school?",
       x = NULL,
       fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top")

# Make legend order match
q19_raw3_summary_diverging_right_order %>%
  ggplot(aes(x = variable, 
             y = percent_answers,
             fill = value)) +
  geom_col() +
  geom_text(aes(label = percent_answers_label),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold") +
  coord_flip() +
  scale_x_discrete() +
  scale_fill_viridis_d(breaks = c("Strongly disagree", "Disagree", "Slightly disagree", "Neither agree nor disagree", "Slightly agree", "Agree", "Strongly agree")) +
  labs(title = "How good is the education at your school?",
       x = NULL,
       fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top")

# improve colors

# print out color palette
hcl_palettes("diverging", n = 7, plot = TRUE)
diverging_hcl(7, palette = "Blue-Red")

# get questions attached
q19_raw3_summary_diverging_right_order2 <- merge(q19_raw3_summary_diverging_right_order, q19_names.l, by = c("variable"))

q19_raw3_summary_diverging_right_order2 %>%
  ggplot(aes(x = Qtext, 
             y = percent_answers,
             fill = value)) +
  geom_col() +
  geom_text(data=subset(q19_raw3_summary_diverging_right_order2, abs(q19_raw3_summary_diverging_right_order2$percent_answers)>.05), 
            aes(label = percent_answers_label),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_fill_manual(breaks = c("Strongly disagree", "Disagree", "Slightly disagree", "Neither agree nor disagree", "Slightly agree", "Agree", "Strongly agree"),
                       values = c(
                         "Strongly disagree" = "#8E063B", 
                         "Disagree" = "#BB7784",
                         "Slightly disagree" = "#D6BCC0", 
                         "Neither agree nor disagree" = "#E2E2E2", 
                         "Slightly agree" = "#BEC1D4", 
                         "Agree" = "#7D87B9",
                         "Strongly agree" = "#023FA5")
                    ) +
  labs(title = "",
       x = NULL,
       fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, hjust = 0),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top")

# Aesthetic corrections: change legend position and single row, going to add tick marks and axis in illustrator
q19_raw3_summary_diverging_right_order2 %>%
  ggplot(aes(x = Qtext, 
             y = percent_answers,
             fill = value)) +
  geom_col() +
  geom_text(data=subset(q19_raw3_summary_diverging_right_order2, abs(q19_raw3_summary_diverging_right_order2$percent_answers)>.05), 
            aes(label = percent_answers_label),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_fill_manual(breaks = c("Strongly disagree", "Disagree", "Slightly disagree", "Neither agree nor disagree", "Slightly agree", "Agree", "Strongly agree"),
                    values = c(
                      "Strongly disagree" = "#8E063B", 
                      "Disagree" = "#BB7784",
                      "Slightly disagree" = "#D6BCC0", 
                      "Neither agree nor disagree" = "#E2E2E2", 
                      "Slightly agree" = "#BEC1D4", 
                      "Agree" = "#7D87B9",
                      "Strongly agree" = "#023FA5")
  ) +
  labs(title = "",
       x = NULL,
       fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, hjust = 0),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom", 
        legend.text=element_text(size=14)) +
  guides(fill = guide_legend(nrow = 1))

## Second round of corrections - no percent label, put on in photoshop
q19_raw3_summary_diverging_right_order2 %>%
  ggplot(aes(x = Qtext, 
             y = percent_answers,
             fill = value)) +
  geom_col() +
  # geom_text(data=subset(q19_raw3_summary_diverging_right_order2, abs(q19_raw3_summary_diverging_right_order2$percent_answers)>.05), 
            # aes(label = percent_answers_label),
            # position = position_stack(vjust = 0.5),
            # color = "white",
            # fontface = "bold") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_fill_manual(breaks = c("Strongly disagree", "Disagree", "Slightly disagree", "Neither agree nor disagree", "Slightly agree", "Agree", "Strongly agree"),
                    values = c(
                      "Strongly disagree" = "#8E063B", 
                      "Disagree" = "#BB7784",
                      "Slightly disagree" = "#D6BCC0", 
                      "Neither agree nor disagree" = "#E2E2E2", 
                      "Slightly agree" = "#BEC1D4", 
                      "Agree" = "#7D87B9",
                      "Strongly agree" = "#023FA5")
  ) +
  labs(title = "",
       x = NULL,
       fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, hjust = 0),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom", 
        legend.text=element_text(size=14)) +
  guides(fill = guide_legend(nrow = 1))


# print out as a figure

pdf("./Figures/Fig3_birdsoverall_raw.pdf", height = 6, width = 12)
q19_raw3_summary_diverging_right_order2 %>%
  ggplot(aes(x = Qtext, 
             y = percent_answers,
             fill = value)) +
  geom_col() +
  geom_text(data=subset(q19_raw3_summary_diverging_right_order2, abs(q19_raw3_summary_diverging_right_order2$percent_answers)>.05), 
            aes(label = percent_answers_label),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_fill_manual(breaks = c("Strongly disagree", "Disagree", "Slightly disagree", "Neither agree nor disagree", "Slightly agree", "Agree", "Strongly agree"),
                    values = c(
                      "Strongly disagree" = "#8E063B", 
                      "Disagree" = "#BB7784",
                      "Slightly disagree" = "#D6BCC0", 
                      "Neither agree nor disagree" = "#E2E2E2", 
                      "Slightly agree" = "#BEC1D4", 
                      "Agree" = "#7D87B9",
                      "Strongly agree" = "#023FA5")
  ) +
  labs(title = "",
       x = NULL,
       fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, hjust = 0),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom", 
        legend.text=element_text(size=10, color="#3C3C3C")) +
  guides(fill = guide_legend(nrow = 1))
dev.off()

pdf("./Figures/Fig3_birdsoverall_raw_nolabs.pdf", height = 6, width = 12)
## Second round of corrections - no percent label, put on in photoshop
q19_raw3_summary_diverging_right_order2 %>%
  ggplot(aes(x = Qtext, 
             y = percent_answers,
             fill = value)) +
  geom_col() +
  # geom_text(data=subset(q19_raw3_summary_diverging_right_order2, abs(q19_raw3_summary_diverging_right_order2$percent_answers)>.05), 
  # aes(label = percent_answers_label),
  # position = position_stack(vjust = 0.5),
  # color = "white",
  # fontface = "bold") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_fill_manual(breaks = c("Strongly disagree", "Disagree", "Slightly disagree", "Neither agree nor disagree", "Slightly agree", "Agree", "Strongly agree"),
                    values = c(
                      "Strongly disagree" = "#8E063B", 
                      "Disagree" = "#BB7784",
                      "Slightly disagree" = "#D6BCC0", 
                      "Neither agree nor disagree" = "#E2E2E2", 
                      "Slightly agree" = "#BEC1D4", 
                      "Agree" = "#7D87B9",
                      "Strongly agree" = "#023FA5")
  ) +
  labs(title = "",
       x = NULL,
       fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, hjust = 0),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom", 
        legend.text=element_text(size=14)) +
  guides(fill = guide_legend(nrow = 1))
dev.off()


########################
# look at familiarity
#######################

# look at % of people who said they recognized each bird
recog <- as.data.frame(matrix(nrow=166, ncol=0))
recog$ResponseId <- survey_raw$ResponseId 
recog$HOFI <- survey_raw$Q21
recog$ANHU <- survey_raw$Q27
recog$LEGO <- survey_raw$Q33
recog$HOSP <- survey_raw$Q39
recog$AMCR <- survey_raw$Q45
recog$BLPH <- survey_raw$Q51
recog$NOMO <- survey_raw$Q57
recog$AMRO <- survey_raw$Q63
recog$CASJ <- survey_raw$Q69
recog$RTHA <- survey_raw$Q75

dim(recog)
head(recog)
recog2 <- recog[-1,]

recog3 <- as.data.frame(matrix(nrow=11, ncol = 0))
recog3$Species_ID <- colnames(recog2)
recog3$posrec <- colSums(recog2=="Yes", na.rm = TRUE)
recog3$totans <- colSums(!is.na(recog2))
recog4 <- recog3[-1,]
recog4$perc_rec <- recog4$posrec/recog4$totans
recog4

write.csv(recog4, "recog.csv", row.names = FALSE)



