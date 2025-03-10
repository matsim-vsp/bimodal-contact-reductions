library(tidyverse)
library(igraph)
library(gridExtra)
library(ggiraphExtra)
library(Hmisc)
library(ggpubr)
library(smplot2)
library(sdamr)

source("./AnalysisSP/SecondOrderContactsPaper/DataCleaningPrepForContactAnalysis.R")
source("./AnalysisSP/SecondOrderContactsPaper/mytheme.r")

palette <- function() {
  c("#3C5488FF", "#3C5488FF",  "#3C5488FF")
}

# Pre-Pandemic (2019) Contact Data -------------------------------------------------------

prepandemic_contacts_absolute <- ggplot(data_reduced_tidy %>% 
filter((TypeOfContact %in% c("Work", "Leisure"))) %>% filter(time %in% c("2019")) %>%
filter(!is.na(TypeOfContact)) %>% filter(value < 500) %>%
filter(WhoseContacts=="Respondent"), aes(time, value)) +
  sm_raincloud(mapping=aes(x=TypeOfContact, y=value), fill = "#3C5488FF",
      point.params = list(size = 3, shape = 21, alpha = 0.4, color = "#3C5488FF", fill = "#3C5488FF", position = sdamr::position_jitternudge(
        nudge.x = -0.1,
        jitter.width = 0.1, jitter.height = 0.01      
      )), 
      boxplot.params =  list(alpha = 0.0, width = 0.0), 
              violin.params = list(width = 1),
              shape = 21, sep_level = 2)  +
  scale_fill_manual(values = palette()) +
  ylab("Reported Number\nof Contacts (2019)") +
  xlab("") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,1,3,10,30,100,300)) +
  theme_minimal() +
  my_theme()

ggsave("CollectionViolinplots_Respondent.pdf", prepandemic_contacts_absolute, dpi = 500, w = 7.5, h = 9)
ggsave("CollectionViolinplots_Respondent.png", prepandemic_contacts_absolute, dpi = 500, w = 7.5, h = 9)

# Pandemic Contact Data (2020, 2021, 2023) -------------------------------------------------------

palette <- function() {
  c("#3C5488FF", "#3C5488FF",  "#3C5488FF")
}

palette2 <- function() {
  c("#515972", "#515972", "#515972")
}

pandemic_contacts_relative_work <- ggplot(data_reduced_tidy_rel %>%  
    filter((TypeOfContact %in% c("Work"))) %>% #Need to replace "Work" by "Leisure" if one is interested in leisure contacts instead
    filter(WhoseContacts == "Respondent") %>% filter(!is.na(value)) %>%
   filter(value > -150) %>% filter(value < 100) %>%  
    filter(!is.na(TypeOfContact))) +
  sm_raincloud(mapping=aes(x=time, y=value, fill = time, color = time), 
      point.params = list(size = 3, shape = 21, alpha = 0.4, position = sdamr::position_jitternudge(
        nudge.x = -0.1,
        jitter.width = 0.1, jitter.height = 0.01      
      )), 
      boxplot.params =  list(alpha = 0.0, width = 0.0), 
              violin.params = list(width = 1),
              shape = 21, sep_level = 2)  +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-100, -50, 0,50, 100)) +
  scale_color_manual(values = palette2()) +
  scale_fill_manual(values = palette()) +
  facet_grid(~(TypeOfContact)) +
  theme_minimal() +
  xlab("") +
  theme(panel.spacing = unit(1, "lines")) +
  #ylab("") +
  ylab("Change of No. of\nContacts (percent)") +
  my_theme() +
  theme(axis.text.x = element_text(angle = 90))

#ggarrange(pandemic_contacts_relative_work, pandemic_contacts_relative_leisure, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37))
#ggsave("CollectionViolinplots_RemainingRespondent.pdf", dpi = 500, w = 18, h = 12)
#ggsave("CollectionViolinplots_RemainingRespondentright.png", p1_zeroth_order_percred_leisure, dpi = 500, w = 10, h = 12)

# Household Pandemic Contact Data -------------------------------------------------------

palette <- function() {
  c("#3C5488FF", "#DC0000FF")
}

pandemic_contacts_relative_work_hh <- ggplot(data_reduced_tidy_rel %>%  
    filter((TypeOfContact %in% c("Work"))) %>% 
    filter(!is.na(value)) %>% 
    filter(WhoseContacts %in% c("Respondent", "Household Member")) %>%
    filter(value > -150) %>% filter(value < 100) %>%    
    filter(!is.na(TypeOfContact))%>% 
    group_by(WhoseContacts, TypeOfContact, time), aes(WhoseContacts, value, color = WhoseContacts, fill = WhoseContacts)) +
    sm_raincloud(aes(stat = median_cl), 
    point.params = list(size = 3, shape = 21, alpha = 0.4, position = sdamr::position_jitternudge(
        nudge.x = -0.1,
        jitter.width = 0.1, jitter.height = 0.01      
      )), 
    boxplot.params =  list(alpha = 0.0, width = 0.0, notch = TRUE), 
              violin.params = list(width = 1),
              shape = 21, sep_level = 2)  +
  scale_fill_manual(values = palette()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-100, -50, 0,50, 100)) +
  #facet_nested(~ TypeOfContact + time) +
  facet_grid(~(time), switch="both")+
  #ggtitle("Work") +
  theme_minimal() +
  theme(panel.spacing = unit(4, "lines")) +
  ylab("Change of No. of \n Contacts (in percent)") +
  my_theme() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust=0.5)) +
  theme(axis.ticks.x = element_line(size = 0)) +
  theme(legend.position = "bottom", legend.title = element_blank())

#ggsave("CollectionViolinplots_Work_All.pdf", pandemic_contacts_relative_work_hh, dpi = 500, w = 18, h = 9)
#ggsave("CollectionViolinplots_Work_All.png", pandemic_contacts_relative_work_hh, dpi = 500, w = 18, h = 9)

# Closest Contact Pandemic Contact Data -------------------------------------------------------

palette <- function() {
  c("#3C5488FF", "#DC0000FF")
}

pandemic_contacts_relative_work_cc <- ggplot(data_reduced_tidy_rel %>%  
    filter((TypeOfContact %in% c("Work"))) %>% 
    filter(!is.na(value)) %>% 
    filter(WhoseContacts %in% c("Respondent", "Closest Contact (Pre-Covid)")) %>%
    filter(value > -150) %>% filter(value < 100) %>%    
    filter(!is.na(TypeOfContact))%>% 
    group_by(WhoseContacts, TypeOfContact, time), aes(WhoseContacts, value, color = WhoseContacts, fill = WhoseContacts)) +
    sm_raincloud(aes(stat = median_cl), 
    point.params = list(size = 3, shape = 21, alpha = 0.4, position = sdamr::position_jitternudge(
        nudge.x = -0.1,
        jitter.width = 0.1, jitter.height = 0.01      
      )), 
    boxplot.params =  list(alpha = 0.0, width = 0.0, notch = TRUE), 
              violin.params = list(width = 1),
              shape = 21, sep_level = 2)  +
  scale_fill_manual(values = palette()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-100, -50, 0,50, 100)) +
  #facet_nested(~ TypeOfContact + time) +
  facet_grid(~(time), switch="both")+
  #ggtitle("Work") +
  theme_minimal() +
  theme(panel.spacing = unit(4, "lines")) +
  ylab("Change of No. of \n Contacts (in percent)") +
  my_theme() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust=0.5)) +
  theme(axis.ticks.x = element_line(size = 0)) +
  theme(legend.position = "bottom", legend.title = element_blank())

#ggsave("CollectionViolinplots_Work_All.pdf", pandemic_contacts_relative_work_cc, dpi = 500, w = 18, h = 9)
#ggsave("CollectionViolinplots_Work_All.png", pandemic_contacts_relative_work_cc, dpi = 500, w = 18, h = 9)