library(tidyverse)
library(MMWRweek)
library(see)
library(RColorBrewer)
library(patchwork)
library(ggpubr)
library(ggh4x)

# Author: S. Paltra, contact: paltra@tu-berlin.de

#In the first part of this script we compare the incidence of the respondents of the survey to the official incidence reported by RKI

#raw_data <- readRDS(file = "/Users/sydney/Desktop/cleaned_data.rds")

source("DataCleaningPrepForContactAnalysis.R")

## ATTITUDE SCORE

data_reduced <- data_reduced %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "Never",
                                                                    num_c19_infs == "Einmal" ~ "Once",
                                                                    num_c19_infs == "Zweimal" ~ "Twice",
                                                                    num_c19_infs == "Dreimal" ~ "Three Times",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "More Than Three Times",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

data_reduced$num_c19_infs_eng <- factor(data_reduced$num_c19_infs_eng, levels = c("Never", "Once", "Twice", "Three Times", "More Than Three Times", "I Don't Want To Answer"))


palette <- function() {
  c("#f1a340", "#998ec3")
}

palette2 <- function() {
  c("#b35806", "#542788")
}

my_comparisons <- list(c("female", "male"))

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% mutate(gender = case_when(gender == "Weiblich" ~ "female",
                                                        gender == "Männlich" ~ "male", 
                                                        gender == "Ich möchte nicht antworten" ~ "No Answer",
                                                        gender == "Divers" ~ "diverse"))

p1 <- ggplot(data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% 
    filter(!is.na(TypeOfContact)) %>% 
    filter(gender %in% c("female", "male")) %>% filter(TypeOfContact %in% c("Leisure", "Work")) %>%
    filter(value < 100) %>% filter(value > -150) %>%  
    filter(!is.na(TypeOfContact)) %>% group_by(gender, time), aes(gender, value)) +
  geom_violin(aes(fill = gender, color = gender), scale = "area", trim = TRUE) +  
  stat_summary(aes(color=gender), fun.data=mean_sdl, fun.args = list(mult=1), 
                 geom="pointrange", linewidth = 1) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test", symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("**** (p < 0.0001)", "*** (p < 0.001)", "** (p < 0.01)", "* (p < 0.05)", "not significant (p > 0.05)")), size = 6, bracket.size = 1, tip.length = 0.01, vjust = 0, label.y.npc = 0)+
  #geom_violin(aes(color=WhoseContacts), size = 1.3) +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-100, -50, 0,50, 100)) +
  facet_nested(~ TypeOfContact + time) +
  #facet_grid(cols = vars(time), rows = vars(TypeOfContact), switch = "x") +
  theme_minimal() +
  xlab("Point In Time") +
  theme(panel.spacing = unit(4, "lines")) +
  ylab("Change of No. of \n Contacts [in percent]") +
  theme(text = element_text(size = 30)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(panel.spacing.x = unit(c(rep(0,2),5, 0,0), "lines"))
ggsave("CollectionViolinplots_Gender.pdf", p1, dpi = 500, w = 22, h = 9)
ggsave("CollectionViolinplots_Gender.png", p1, dpi = 500, w = 22, h = 9)

mean <- data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% 
    filter(!is.na(TypeOfContact)) %>% 
    filter(gender %in% c("female", "male")) %>% filter(TypeOfContact %in% c("Leisure", "Work")) %>%
    filter(value < 100) %>% filter(value > -150) %>%  
    filter(!is.na(TypeOfContact)) %>% group_by(TypeOfContact, gender, time) %>% summarise(mean =mean(value))
