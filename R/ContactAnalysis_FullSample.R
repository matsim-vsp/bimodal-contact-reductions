library(tidyverse)
library(igraph)
library(gridExtra)
library(ggiraphExtra)
library(Hmisc)
library(ggpubr)
library(smplot2)
library(sdamr)
library(here)

#Data prep cor contact analysis, personalized theme is loaded, manual color palette is created
here()
source("./R/DataCleaningPrepForContactAnalysis.R")
source("./R/mytheme.r")

palette <- function() {
  c("#3C5488FF", "#3C5488FF",  "#3C5488FF")
}

# Pre-Pandemic (2019) Contact Data -------------------------------------------------------

# Creation of Supplementary Figure 2
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

ggsave("SupplementaryFigure2.pdf", prepandemic_contacts_absolute, dpi = 500, w = 7.5, h = 9)
ggsave("SupplementaryFigure2.png", prepandemic_contacts_absolute, dpi = 500, w = 7.5, h = 9)


# Analysis of Contact Reductions (03/2020, Summer 2021, 01/2023) ----------

# Produces Figure 1

palette <- function() {
  c("#3C5488FF", "#3C5488FF",  "#3C5488FF", "#3C5488FF")
}

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% mutate(time = case_when(time == "Summer 2021" ~ "Summer\n2021", .default = time))
data_reduced_tidy_rel$time <- factor(data_reduced_tidy_rel$time, levels = c("03/2020", "Summer\n2021", "01/2023"))

pandemic_contacts_relative_work <- ggplot(data_reduced_tidy_rel %>%  
    filter((TypeOfContact %in% c("Work"))) %>% 
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

pandemic_contacts_relative_leisure <- ggplot(data_reduced_tidy_rel %>%  
                                               filter((TypeOfContact %in% c("Leisure"))) %>% 
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
ggsave("Figure1.pdf", dpi = 500, w = 24, h = 9)
ggsave("Figure1.png", dpi = 500, w = 24, h = 9)

# Analysis of Contact Reductions of Household Members ---------------------

# Produces Supplementary Figure 8

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
    point.params = list(size = 3, shape = 21, alpha = 0.4, position = sdamr::position_jitternudge(
                   nudge.x = -0.12,
                   jitter.width = 0.1, jitter.height = 0.01      
                 )), 
                 boxplot.params =  list(alpha = 0.0, width = 0.0, notch = TRUE), 
                 violin.params = list(width = 1.4, scale = "area"),
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
  theme(axis.text.x = element_text(hjust=-0.0001))  +
  scale_x_discrete(
    breaks = combined_levels[unique_positions],  # Only put breaks at selected positions
    labels = unique_A_values                     # Use corresponding unique A values as labels
  ) 

ggarrange(pandemic_contacts_relative_work_hh, pandemic_contacts_relative_leisure_hh, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")

ggsave("SupplementaryFigure8.pdf",  dpi = 500, w = 24, h = 9)
ggsave("SupplementaryFigure8.png", dpi = 500, w = 24, h = 9)

# Analysis of Contact Reductions of Closest Contacts ----------------------

# Produces Supplementary Figure 9

palette <- function() {
  c("#3C5488FF", "#DC0000FF")
}

pandemic_contacts_relative_leisure_cc <- ggplot(data_reduced_tidy_rel %>%  
    filter((TypeOfContact %in% c("Leisure"))) %>% 
    filter(!is.na(value)) %>% 
    filter(WhoseContacts %in% c("Respondent", "Closest Contact (Pre-Covid)")) %>%
    filter(value > -150) %>% filter(value < 100) %>%    
    filter(!is.na(TypeOfContact))%>% 
    group_by(WhoseContacts, TypeOfContact, time), aes(combined, value, color = WhoseContacts, fill = WhoseContacts)) +
    sm_raincloud(aes(stat = median_cl), 
    point.params = list(size = 3, shape = 21, alpha = 0.4, position = sdamr::position_jitternudge(
                   nudge.x = -0.12,
                   jitter.width = 0.1, jitter.height = 0.01      
                 )), 
                 boxplot.params =  list(alpha = 0.0, width = 0.0, notch = TRUE), 
                 violin.params = list(width = 1.4, scale = "area"),
                 shape = 21, sep_level = 2)  +
  scale_fill_manual(values = palette()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-100, -50, 0,50, 100)) +
  ggtitle("Leisure") +
  theme_minimal() +
  theme(panel.spacing = unit(4, "lines")) +
  ylab("Change of No. of \n Contacts (in percent)") +
  my_theme() +
  theme(axis.title.x = element_blank(), plot.title = element_text(hjust=0.5)) +
  theme(axis.ticks.x = element_line(size = 0)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.text.x = element_text(hjust=-0.0001))  +
  scale_x_discrete(
    breaks = combined_levels[unique_positions],  # Only put breaks at selected positions
    labels = unique_A_values                     # Use corresponding unique A values as labels
  ) 
ggarrange(pandemic_contacts_relative_work_cc, pandemic_contacts_relative_leisure_cc, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")

ggsave("SupplementaryFigure9.pdf",  dpi = 500, w = 24, h = 9)
ggsave("SupplementaryFigure9.png", dpi = 500, w = 24, h = 9)

# Mean Reduction Participant vs HH vs CC ----------------------------------

print(data_reduced_tidy_rel %>%  
  filter(!is.na(value)) %>% 
  filter(value > -150) %>% filter(value < 100) %>%    
  filter(!is.na(TypeOfContact)) %>% group_by(TypeOfContact, WhoseContacts, time) %>%
  summarise(meanRed = mean(value)), n = 100)