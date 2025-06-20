library(tidyverse)
library(here)
library(socialmixr)
library(smplot2)
library(sdamr)


# POLYMOD data is accessed via the socialmixr package ---------------------

polymod_germany <- polymod["participants"][1]
polymod_germany <- data.frame(polymod_germany)
polymod_germany <- polymod_germany %>% filter(participants.country == "Germany")


polymod_germany <- polymod_germany %>% mutate(age_bracket = case_when(participants.part_age < 18 ~ "0-17",
                                                                      participants.part_age < 40 ~ "18-39",
                                                                      participants.part_age < 60 ~ "40-59",
                                                                      participants.part_age < 80 ~ "60-79",
                                                                      participants.part_age < 100 ~"80+"))


# Extracting Data for Supplementary Table 1 -------------------------------
                                                                      
polymod_germany_age <- polymod_germany %>% count(age_bracket) %>% filter(!is.na(age_bracket)) %>% mutate(percent = n/sum(n)*100)

polymod_germany_gender <- polymod_germany %>% count(participants.part_gender) %>% filter(!is.na(participants.part_gender)) %>% mutate(percent = n/sum(n)*100)

polymod_germany_hh <- polymod_germany %>% mutate(hh_size = case_when(participants.hh_size == 1 ~ "1",
                                                                     participants.hh_size == 2 ~ "2",
                                                                     participants.hh_size == 3 ~ "3",
                                                                     participants.hh_size > 3 ~ "4+")) %>%
                  count(hh_size) %>% mutate(percent = n/sum(n)*100)
quantile(polymod_germany$participants.hh_size, na.rm=TRUE)

# Extracting POLYMOD Work Contact Data ------------------------------------

polymod_contacts <- polymod["contacts"][1]
polymod_contacts<- data.frame(polymod_contacts)
polymod_germany_working <- polymod_germany %>% filter(participants.part_occupation == 1)
german_part_id <- unique(polymod_germany_working$participants.part_id)
polymod_contacts_work <- polymod_contacts %>% filter(contacts.part_id %in% german_part_id)
work <- polymod_contacts_work %>% group_by(contacts.part_id) %>% summarise(Work = sum(contacts.cnt_work)) %>% mutate(group = 1)
mean(work$Work, na.rm=TRUE)
median(work$Work)
max(work$Work, na.rm=TRUE)
min(work$Work, na.rm=TRUE)


# Extracting POLYMOD Leisure Contact Data ---------------------------------

polymod_contacts_leisure <- polymod_contacts %>% filter(contacts.part_id %in% german_part_id)
leisure <- polymod_contacts_leisure %>% group_by(contacts.part_id) %>% summarise(Leisure = sum(contacts.cnt_leisure)) %>% mutate(group = 1)
mean(leisure$Leisure, na.rm=TRUE)
max(leisure$Leisure, na.rm=TRUE)
min(leisure$Leisure, na.rm=TRUE)