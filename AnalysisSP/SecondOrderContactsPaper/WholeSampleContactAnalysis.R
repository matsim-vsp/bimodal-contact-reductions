library(tidyverse)
library(igraph)
library(gridExtra)
library(ggiraphExtra)
library(Hmisc)
library(ggpubr)
library(smplot2)
library(sdamr)
library(here)

here()
source("./AnalysisSP/SecondOrderContactsPaper/DataCleaningPrepForContactAnalysis.R")
source("./AnalysisSP/SecondOrderContactsPaper/mytheme.r")

palette <- function() {
  c("#3C5488FF", "#3C5488FF",  "#3C5488FF")
}

# Pre-Pandemic (2019) Contact Data -------------------------------------------------------

prepandemic_contacts_absolute <- ggplot(data_reduced_tidy %>% 
filter((TypeOfContact %in% c("Work", "Leisure"))) %>% filter(time %in% c("2019")) %>%
filter(!is.na(TypeOfContact)) %>% filter(value < 500) %>%
filter(WhoseContacts=="Respondent"), aes(TypeOfContact, value, fill = "#3C5488FF")) +
      sm_raincloud(aes(stat = median_cl), 
                   point.params = list(size = 3, shape = 21, color = "transparent", alpha = 0.5, position = sdamr::position_jitternudge(
                     nudge.x = -0.1,
                     jitter.width = 0.1, jitter.height = 0.01      
                   )), 
                   boxplot.params =  list(alpha = 0.0, width = 0.0, notch = TRUE), 
                   violin.params = list(width = 1), sep_level = 2)  +
  scale_fill_manual(values = palette()) +
  ylab("Reported Number\nof Contacts (2019)") +
  xlab("") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,1,3,10,30,100,300)) +
  theme_minimal() +
  my_theme()

ggsave("CollectionViolinplots_Respondent.pdf", prepandemic_contacts_absolute, dpi = 500, w = 7.5, h = 9)
ggsave("CollectionViolinplots_Respondent.png", prepandemic_contacts_absolute, dpi = 500, w = 7.5, h = 9)

data2019work <- data_reduced_tidy %>% filter(TypeOfContact == "Work") %>% 
  filter(time %in% c("2019")) %>%
  filter(value < 500) %>%
  filter(WhoseContacts=="Respondent")
round(quantile(data2019work$value, na.rm=TRUE))

data2019leisure <- data_reduced_tidy %>% filter(TypeOfContact == "Leisure") %>% 
  filter(time %in% c("2019")) %>%
  filter(value < 500) %>%
  filter(WhoseContacts=="Respondent")
round(quantile(data2019leisure$value, na.rm=TRUE))

# Pandemic Contact Data (2020, 2021, 2023) -------------------------------------------------------

palette <- function() {
  c("#3C5488FF", "#3C5488FF",  "#3C5488FF")
}

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% mutate(time = case_when(time == "Summer 2021" ~ "Summer\n2021", .default = time))
data_reduced_tidy_rel$time <- factor(data_reduced_tidy_rel$time, levels = c("03/2020", "Summer\n2021", "01/2023"))

pandemic_contacts_relative_leisure<- ggplot(data_reduced_tidy_rel %>%  
    filter((TypeOfContact %in% c("Leisure"))) %>% #Need to replace "Work" by "Leisure" if one is interested in leisure contacts instead
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
  my_theme()

ggarrange(pandemic_contacts_relative_work, pandemic_contacts_relative_leisure, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37))
ggsave("CollectionViolinplots_RemainingRespondent.pdf", dpi = 500, w = 24, h = 9)
ggsave("CollectionViolinplots_RemainingRespondent.png", dpi = 500, w = 24, h = 9)

# Household Pandemic Contact Data -------------------------------------------------------

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% mutate(time = case_when(time == "Summer 2021" ~ "Summer\n2021", .default = time))
data_reduced_tidy_rel$time <- factor(data_reduced_tidy_rel$time, levels = c("03/2020", "Summer\n2021", "01/2023"))

palette <- function() {
  c("#3C5488FF", "#DC0000FF")
}

data_reduced_tidy_rel$combined = interaction(data_reduced_tidy_rel$WhoseContacts, data_reduced_tidy_rel$time)

combined_levels <- levels(interaction(data_reduced_tidy_rel$WhoseContacts, data_reduced_tidy_rel$time))
A_values <- data_reduced_tidy_rel$time[match(combined_levels, interaction(data_reduced_tidy_rel$WhoseContacts, data_reduced_tidy_rel$time))]

unique_A_values <- unique(A_values)
unique_positions <- sapply(unique_A_values, function(a) {
  # For each unique A value, find the first position where it appears
  which(A_values == a)[1]
})

pandemic_contacts_relative_leisure_hh <- ggplot(data_reduced_tidy_rel %>%  
    filter((TypeOfContact %in% c("Leisure"))) %>% 
    filter(!is.na(value)) %>% 
    filter(WhoseContacts %in% c("Respondent", "Household Members")) %>%
    filter(value > -150) %>% filter(value < 100) %>%    
    filter(!is.na(TypeOfContact))%>% 
    group_by(WhoseContacts, TypeOfContact, time), aes(combined, value, color = WhoseContacts, fill = WhoseContacts)) +
    sm_raincloud(aes(stat = median_cl), 
    point.params = list(size = 3, shape = 21, alpha = 0.5, position = sdamr::position_jitternudge(
        nudge.x = -0.1,
        jitter.width = 0.1, jitter.height = 0.01      
      )), 
    boxplot.params =  list(alpha = 0.0, width = 0.0, notch = TRUE), 
              violin.params = list(width = 1),
              shape = 21, sep_level = 2)  +
  scale_fill_manual(values = palette()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-100, -50, 0,50, 100)) +
  #facet_grid(~(time), switch="both")+
  ggtitle("Leisure") +
  theme_minimal() +
  theme(panel.spacing = unit(4, "lines")) +
  ylab("Change of No. of \n Contacts (in percent)") +
  my_theme() +
  theme(axis.title.x = element_blank(), plot.title = element_text(hjust=0.5)) +
  theme(axis.ticks.x = element_line(size = 0)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_discrete(
    breaks = combined_levels[unique_positions],  # Only put breaks at selected positions
    labels = unique_A_values                     # Use corresponding unique A values as labels
  )

ggarrange(pandemic_contacts_relative_work_hh, pandemic_contacts_relative_leisure_hh, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")

ggsave("CollectionViolinplots_HHMember.pdf",  dpi = 500, w = 24, h = 9)
ggsave("CollectionViolinplots_HHMember.png", dpi = 500, w = 24, h = 9)

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
    group_by(WhoseContacts, TypeOfContact, time), aes(combined, value, color = WhoseContacts, fill = WhoseContacts)) +
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
  ggtitle("Work") +
  theme_minimal() +
  theme(panel.spacing = unit(4, "lines")) +
  ylab("Change of No. of \n Contacts (in percent)") +
  my_theme() +
  theme(axis.title.x = element_blank(), plot.title = element_text(hjust=0.5)) +
  theme(axis.ticks.x = element_line(size = 0)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_discrete(
    breaks = combined_levels[unique_positions],  # Only put breaks at selected positions
    labels = unique_A_values                     # Use corresponding unique A values as labels
  )
ggarrange(pandemic_contacts_relative_work_cc, pandemic_contacts_relative_leisure_cc, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")

ggsave("CollectionViolinplots_cc.pdf",  dpi = 500, w = 24, h = 9)
ggsave("CollectionViolinplots_cc.png", dpi = 500, w = 24, h = 9)
