library(tidyverse)
library(igraph)
library(gridExtra)
library(ggiraphExtra)

# 0th order Results -------------------------------------------------------

source("./AnalysisSP/SecondOrderContactsPaper/DataCleaningPrepForContactAnalysis.R")
source("./AnalysisSP/SecondOrderContactsPaper/mytheme.r")

#Subanalysis for school children

colnames(data_reduced_tidy_rel)

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% mutate(childrenyesno = case_when(number_of_children_under_18 > 0 ~ "Yes",
                                                                                    number_of_children_under_18 == 0 ~ "No"))

palette <- function() {
  c("#3C5488FF", "#3C5488FF",  "#3C5488FF")
}

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% mutate(time = case_when(time == "Summer 2021" ~ "Summer\n2021", .default = time))
data_reduced_tidy_rel$time <- factor(data_reduced_tidy_rel$time, levels = c("03/2020", "Summer\n2021", "01/2023"))

pandemic_contacts_relative_school <- ggplot(data_reduced_tidy_rel %>%  
                                              filter((TypeOfContact %in% c("School"))) %>% #Need to replace "Work" by "Leisure" if one is interested in leisure contacts instead
                                              filter(WhoseContacts == "Household Members") %>% filter(!is.na(value)) %>%
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

ggsave("CollectionViolinplots_RemaininSchool.pdf", pandemic_contacts_relative_school, dpi = 500, w = 24, h = 9)
ggsave("CollectionViolinplots_RemaininSchool.png", pandemic_contacts_relative_school, dpi = 500, w = 24, h = 9)