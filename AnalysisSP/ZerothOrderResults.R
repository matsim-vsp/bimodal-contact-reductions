library(tidyverse)
library(igraph)
library(gridExtra)
library(ggiraphExtra)

# 0th order Results -------------------------------------------------------

source("DataCleaningPrepForContactAnalysis.R")

data_reduced %>% count(age_bracket)

data_reduced_children <- data_reduced %>% filter(hhmember_school_2019 > 0)

palette <- function() {
  c("#006BA6", "#FFBC42", "#8F2D56", "#C93E78")
}

p1 <- ggplot(data_reduced_tidy %>% filter(value < 100) %>% filter(!is.na(TypeOfContact)), aes(WhoseContacts, value)) +
  #geom_violin(aes(color = WhoseContacts), width = 1, trim = FALSE, position=position_dodge(0.9)) + 
  #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
  geom_boxplot(aes(color = WhoseContacts), size = 1.3) +
  scale_color_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols= vars(time)) +
  theme_minimal() +
  ylab("Reported # Of Contacts") +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(panel.spacing = unit(0.8, "cm", data = NULL))
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("CollectionBoxplots_Absolute.pdf", p1, dpi = 500, w = 13, h = 16)
ggsave("CollectionBoxplots_Absolute.png", p1, dpi = 500, w = 13, h = 16)

# Relative no. of contacts

palette <- function() {
  c("#006BA6", "#FFBC42", "#8F2D56", "#C93E78")
}

ggplot(data_reduced_tidy_rel %>% 
    #filter(attitudeScore > 6) %>% 
    filter(value > -100) %>% filter(value < 100) %>%  
    filter(!is.na(TypeOfContact)), aes(WhoseContacts, value)) +
  geom_boxplot(aes(color = WhoseContacts), size = 1.3) +
  #geom_violin(aes(color=WhoseContacts), size = 1.3) +
  scale_color_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols = vars(time)) +
  theme_minimal() +
  ylab("Remaining Share Of Contacts") +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("CollectionBoxplots_Remaining.pdf", dpi = 500, w = 13, h = 16)
ggsave("CollectionBoxplots_Remaining.png", dpi = 500, w = 13, h = 16)