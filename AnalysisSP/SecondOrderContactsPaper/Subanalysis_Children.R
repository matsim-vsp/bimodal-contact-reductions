library(tidyverse)
library(igraph)
library(gridExtra)
library(ggiraphExtra)

# 0th order Results -------------------------------------------------------

source("DataCleaningPrepForContactAnalysis.R")

#Subanalysis for school children
data_reduced_children <- data_reduced_children %>% select(-c(respondent_hsld_size_persons_under_14, number_of_children_under_18)) %>%
                                  select(-contains("attitudes")) %>%
                                  select(-contains("beh_change"))

data_reduced_children <- data_reduced_children %>% pivot_longer(cols = 3:78)

data_reduced_children <- data_reduced_children  %>% mutate(time = case_when(str_detect(name, "2019") ~ "2019",
                                                          str_detect(name, "2020") ~ "03/2020",
                                                          str_detect(name, "2021") ~ "Summer 2021",
                                                          str_detect(name, "2023") ~ "01/2023")) %>%
                                  mutate(WhoseContacts = case_when(str_detect(name, "respondent") ~ "Respondent",
                                  str_detect(name, "cc_pre") ~ "Closest Contact (Pre-Covid)",
                                  str_detect(name, "cc_during") ~ "Closest Contact (During-Covid)",
                                  str_detect(name, "hhmember") ~ "Household Member")) %>%
                                  mutate(TypeOfContact = case_when(str_detect(name, "work") ~ "Work",
                                  str_detect(name, "school") ~ "School",
                                  str_detect(name, "leisure") ~ "Leisure",
                                  str_detect(name, "all") ~ "All"))

data_reduced_children$time <- factor(data_reduced_children$time, levels = c("2019", "03/2020", "Summer 2021", "01/2023"))
data_reduced_children$TypeOfContact <- factor(data_reduced_children$TypeOfContact, levels = c("Work", "Leisure", "School", "All"))
data_reduced_children$WhoseContacts <- factor(data_reduced_children$WhoseContacts, levels = c("Respondent", "Household Member", "Closest Contact (Pre-Covid)", "Closest Contact (During-Covid)"))

ggplot(data_reduced_children %>% filter(WhoseContacts == "Household Member") %>% filter(value < 100) %>% filter(!is.na(TypeOfContact)), aes(WhoseContacts, value)) +
  #geom_violin(aes(color = WhoseContacts), width = 1, trim = FALSE, position=position_dodge(0.9)) + 
  #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
  geom_boxplot(color = "#FFBC42", size = 1.3) +
  scale_color_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols= vars(time)) +
  theme_minimal() +
  ylab("Reported # Of Contacts") +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(panel.spacing = unit(0.8, "cm", data = NULL))
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("CollectionBoxplotsChildren.pdf", dpi = 500, w = 8.5, h = 12)
ggsave("CollectionBoxplotsChildren.png", dpi = 500, w = 8.5, h = 12)


ggplot(data_reduced %>% filter(WhoseContacts == "Household Member") %>% filter(value < 100) %>% filter(!is.na(TypeOfContact)), aes(time, value)) +
  #geom_violin(aes(color = WhoseContacts), width = 1, trim = FALSE, position=position_dodge(0.9)) + 
  #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
  geom_boxplot(color = "#FFBC42", size = 1.3) +
  #scale_color_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols= vars(time)) +
  theme_minimal() +
  ylab("Reported # Of Contacts") +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(panel.spacing = unit(0.8, "cm", data = NULL))
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("CollectionBoxplotsChildrenRelative.pdf", dpi = 500, w = 8.5, h = 12)
ggsave("CollectionBoxplotsChildrenRelative.png", dpi = 500, w = 8.5, h = 12)

