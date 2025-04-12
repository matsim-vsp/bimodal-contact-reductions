library(tidyverse)
library(here)
library(socialmixr)
library(smplot2)
library(sdamr)

polymod_germany <- polymod["participants"][1]
polymod_germany <- data.frame(polymod_germany)
polymod_germany <- polymod_germany %>% filter(participants.country == "Germany")


polymod_germany <- polymod_germany %>% mutate(age_bracket = case_when(participants.part_age < 18 ~ "0-17",
                                                                      participants.part_age < 40 ~ "18-39",
                                                                      participants.part_age < 60 ~ "40-59",
                                                                      participants.part_age < 80 ~ "60-79",
                                                                      participants.part_age < 100 ~"80+"))
                                                                      
polymod_germany_age <- polymod_germany %>% count(age_bracket) %>% filter(!is.na(age_bracket)) %>% mutate(percent = n/sum(n)*100)

polymod_germany_gender <- polymod_germany %>% count(participants.part_gender) %>% filter(!is.na(participants.part_gender)) %>% mutate(percent = n/sum(n)*100)

polymod_germany_hh <- polymod_germany %>% mutate(hh_size = case_when(participants.hh_size == 1 ~ "1",
                                                                     participants.hh_size == 2 ~ "2",
                                                                     participants.hh_size == 3 ~ "3",
                                                                     participants.hh_size > 3 ~ "4+")) %>%
                  count(hh_size) %>% mutate(percent = n/sum(n)*100)
quantile(polymod_germany$participants.hh_size, na.rm=TRUE)

# Work --------------------------------------------------------------------

polymod_contacts <- polymod["contacts"][1]
polymod_contacts<- data.frame(polymod_contacts)
polymod_germany_working <- polymod_germany %>% filter(participants.part_occupation == 1)
german_part_id <- unique(polymod_germany_working$participants.part_id)
polymod_contacts_work <- polymod_contacts %>% filter(contacts.part_id %in% german_part_id)
work <- polymod_contacts_work %>% group_by(contacts.part_id) %>% summarise(Work = sum(contacts.cnt_work)) %>% mutate(group = 1)
mean(work$work, na.rm=TRUE)
median(work$work)
max(work$work, na.rm=TRUE)
min(work$work, na.rm=TRUE)

work_polymod <- ggplot(work) +
  sm_raincloud(mapping=aes(x=group, y=work), fill = "#3C5488FF",
               point.params = list(size = 3, shape = 21, alpha = 0.4, color = "#3C5488FF", fill = "#3C5488FF", position = sdamr::position_jitternudge(
                 nudge.x = -0.1,
                 jitter.width = 0.1, jitter.height = 0.01      
               )), 
               boxplot.params =  list(alpha = 0.0, width = 0.0), 
               violin.params = list(width = 1),
               shape = 21, sep_level = 2)  +
  scale_fill_manual(values = palette()) +
  ylab("Reported Number\nof Contacts (POLYMOD)") +
  xlab("Work") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,1,3,10,30,100,300)) +
  theme_minimal() +
  my_theme() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

#ggsave("WorkPolymod.png", work_polymod, dpi = 500, h = 9, w = 6)

round(quantile(work$Work, na.rm=TRUE))

# Leisure --------------------------------------------------------------------

polymod_contacts_leisure <- polymod_contacts %>% filter(contacts.part_id %in% german_part_id)
leisure <- polymod_contacts_leisure %>% group_by(contacts.part_id) %>% summarise(Leisure = sum(contacts.cnt_leisure)) %>% mutate(group = 1)
mean(leisure$leisure, na.rm=TRUE)
max(leisure$leisure, na.rm=TRUE)
min(leisure$leisure, na.rm=TRUE)

leisure_polymod <- ggplot(leisure) +
  sm_raincloud(mapping=aes(x=group, y=Leisure), fill = "#3C5488FF",
               point.params = list(size = 3, shape = 21, alpha = 0.4, color = "#3C5488FF", fill = "#3C5488FF", position = sdamr::position_jitternudge(
                 nudge.x = -0.1,
                 jitter.width = 0.1, jitter.height = 0.01      
               )), 
               boxplot.params =  list(alpha = 0.0, width = 0.0), 
               violin.params = list(width = 1),
               shape = 21, sep_level = 2)  +
  scale_fill_manual(values = palette()) +
  #ylab("Reported Number\nof Contacts (POLYMOD)") +
  ylab("") +
  xlab("Leisure") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,1,3,10,30,100,300)) +
  theme_minimal() +
  my_theme() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggsave("LeisurePolymod.png", dpi = 500, h = 9, w = 6)

round(quantile(leisure$Leisure, na.rm=TRUE))

workleisure <- left_join(work, leisure)
workleisure <- workleisure %>% select(contacts.part_id, Work, Leisure)
workleisure <- workleisure %>% pivot_longer(cols=c(Work,Leisure))

workleisure$name <- factor(workleisure$name, levels = c("Work", "Leisure"))

palette <- function() {
  c("#3C5488FF", "#3C5488FF")
}

leisurework_polymod <- ggplot(workleisure, aes(x = name, y = value, fill = name)) +
  sm_raincloud(aes(stat = median_cl, color = name),
               point.params = list(size = 3, shape = 21, color = "transparent", alpha = 0.5, position = sdamr::position_jitternudge(
                 nudge.x = -0.1,
                 jitter.width = 0.1, jitter.height = 0.01      
               )), 
               boxplot.params =  list(alpha = 0.0, width = 0.0, notch = TRUE), 
               violin.params = list(width = 1),
               sep_level = 2)  +
  ylab("Reported Number\nof Contacts (POLYMOD)") +
  #ylab("") +
  xlab("") +
  scale_fill_manual(values = palette()) +
  #scale_color_manual(values = palette()) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,1,3,10,30,100,300)) +
  theme_minimal() +
  my_theme() 

ggsave("LeisureWorkPolymod.png", leisurework_polymod, dpi = 500, h = 9, w = 9)
ggsave("LeisureWorkPolymod.pdf", leisurework_polymod, dpi = 500, h = 9, w = 9)

##Arranging polymod and survey data
ggarrange(prepandemic_contacts_absolute, leisurework_polymod, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37))
ggsave("PrePandemicContacts_SurveyandPolymod.pdf", dpi = 500, w = 18, h = 9)
ggsave("PrePandemicContacts_SurveyandPolymod.png", dpi = 500, w = 18, h = 9)

##Arranging polymod and daily survey data 
ggarrange(prepandemic_contacts_absolute_daily, leisurework_polymod, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37))
ggsave("PrePandemicContacts_SurveyandPolymodDAILY.pdf", dpi = 500, w = 18, h = 9)
ggsave("PrePandemicContacts_SurveyandPolymodDAILY.png", dpi = 500, w = 18, h = 9)


# Home --------------------------------------------------------------------

polymod_contacts_home <- polymod_contacts %>% filter(contacts.part_id %in% german_part_id)
home <- polymod_contacts_home %>% group_by(contacts.part_id) %>% summarise(home = sum(contacts.cnt_home))
mean(home$home, na.rm=TRUE)
max(home$home, na.rm=TRUE)
min(home$home, na.rm=TRUE)

# Transport --------------------------------------------------------------------

polymod_contacts_transport <- polymod_contacts %>% filter(contacts.part_id %in% german_part_id)
transport <- polymod_contacts_transport %>% group_by(contacts.part_id) %>% summarise(transport = sum(contacts.cnt_transport))
mean(transport$transport, na.rm=TRUE)
max(transport$transport, na.rm=TRUE)
min(transport$transport, na.rm=TRUE)

# Others --------------------------------------------------------------------

polymod_contacts_other <- polymod_contacts %>% filter(contacts.part_id %in% german_part_id)
other <- polymod_contacts_transport %>% group_by(contacts.part_id) %>% summarise(other = sum(contacts.cnt_otherplace))
mean(other$other, na.rm=TRUE)
max(other$other, na.rm=TRUE)
min(other$other, na.rm=TRUE)
