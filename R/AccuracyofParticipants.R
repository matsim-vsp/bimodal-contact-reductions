library(tidyverse) #version 2.0.0
library(igraph) #version 2.1.4
library(gridExtra) #version 2.3
library(ggiraphExtra) #version 0.3.0

# Author: S. Paltra, contact: paltra@tu-berlin.de

# This script produces Supplementary Figure 6,7, and 8 of Supplementary Section Accuracy of Participants’ Reported Contact Numbers.
# In order to recreate these plots, you need access to the survey data CONTAINING the User Ids.

# Data prep

data_reduced <- read_csv(file = "/Users/sydney/Desktop/TwitterLimeSurvey/twitter_data.csv")

# Respondents who were forwarded the survey -------------------------------
data_reduced_second_respondents <- data_reduced %>% filter(!is.na(ref)) %>% 
  filter(!is.na(user_id)) %>%
  # Removal of respondets who's reference is one of the 5 people who share the link on twitter
  filter(!(ref %in% c("4a76b", "dec9d", "7b598", "008b5", "6c8d7"))) %>%
  # Removal of respondents who clicked on the link they were forwarded, but did not fill anything in 
  filter(user_id != "5c0412b6-edfd-4eb8-84df-f8fe25b8c208") %>%
  filter(user_id != "b8bf1416-b83f-4b1b-a411-e715a6437cd2") %>%
  filter(user_id  != "caba33fe-d132-4f6c-8dea-c059392888d4")
# Manually removing the second to last row as this user started the survey twice
data_reduced_second_respondents <- data_reduced_second_respondents[-22,]

data_reduced_second_respondents <- data_reduced_second_respondents %>% select(user_id, ref, cc_change_during_pandemic,  
                                                                              total_hsld_size_persons_under_14, hsld_size_2019_, hsld_size_03_2020_, hsld_size_summer_2021_, hsld_size_01_2023_,
                                                                              wkly_cont_2019_work_uni, wkly_cont_03_2020_work_uni, wkly_cont_summer_2021_work_uni, wkly_cont_01_2023_work_uni, 
                                                                              wkly_cont_2019_school_kinder, wkly_cont_03_2020_school_kinder, wkly_cont_summer_2021_school_kinder, wkly_cont_01_2023_school_kinder,
                                                                              wkly_cont_2019_leisure, wkly_cont_03_2020_leisure, wkly_cont_summer_2021_leisure, wkly_cont_01_2023_leisure)

colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "user_id")] <- "id_second_respondent"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "ref")] <- "id_original_respondent"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "cc_change_during_pandemic")] <- "cc_change_during_pandemic_second_respondent"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "total_hsld_size_persons_under_14")] <- "hsldsize_personsunder14"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "hsld_size_2019_")] <- "hsldsize_2019"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "hsld_size_03_2020_")] <- "hsldsize_03_2020"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "hsld_size_summer_2021_")] <- "hsldsize_summer_2021"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "hsld_size_01_2023_")] <- "hsldsize_01_2023"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_2019_work_uni")] <- "work_2019"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_03_2020_work_uni")] <- "work_03_2020"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_summer_2021_work_uni")] <- "work_summer_2021"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_01_2023_work_uni")] <- "work_01_2023"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_2019_school_kinder")] <- "school_2019"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_03_2020_school_kinder")] <- "school_03_2020"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_summer_2021_school_kinder")] <- "school_summer_2021"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_01_2023_school_kinder")] <- "school_01_2023"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_2019_leisure")] <- "leisure_2019"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_03_2020_leisure")] <- "leisure_03_2020"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_summer_2021_leisure")] <- "leisure_summer_2021"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_01_2023_leisure")] <- "leisure_01_2023"

#Setting no of contacts equal to 0 if person DID not answer 
#TODO: How do I differentiate real zeros from introduced 0? Are there even "real zeros"? Check!
data_reduced_second_respondents <- as.data.frame(data_reduced_second_respondents)
data_reduced_second_respondents[is.na(data_reduced_second_respondents)] <- 0

data_reduced_second_respondents <- data_reduced_second_respondents %>% pivot_longer(cols = c("hsldsize_personsunder14", "hsldsize_2019", "hsldsize_03_2020", "hsldsize_summer_2021", "hsldsize_01_2023", "work_2019", "work_03_2020", "work_summer_2021", "work_01_2023", "school_2019", "school_03_2020", "school_summer_2021", "school_01_2023", "leisure_2019", "leisure_03_2020", "leisure_summer_2021", "leisure_01_2023"), names_to ="category", values_to="value_second_respondent")

data_reduced_second_respondents <- data_reduced_second_respondents %>% separate(category, c("category", "time"), "_", extra = "merge")

data_reduced_second_respondents <- data_reduced_second_respondents  %>% filter(id_original_respondent != "3f12f149-7326-453b-8628-edc7ccd046e8") %>% 
  #filter(id_original_respondent != "5a13ab20-790f-4f32-83e1-b21e55c45a93") %>%
  filter(cc_change_during_pandemic_second_respondent == "Nein") %>%
  filter(time != "personsunder14")

# Respondents who originally filled out the survey ------------------------

data_reduced_original_respondents <- data_reduced %>% filter(user_id %in% unique(data_reduced_second_respondents$id_original_respondent))

data_reduced_original_respondents  <- data_reduced_original_respondents  %>% select(user_id, cc_change_during_pandemic, 
                                                                                    cc_hsld_size_pre_pandemic_2019_num_hsld_members, cc_hsld_size_pre_pandemic_03_2020_num_hsld_members, cc_hsld_size_pre_pandemic_summer_2021_num_hsld_members, cc_hsld_size_pre_pandemic_01_2023_num_hsld_members,
                                                                                    #cc_hsld_size_during_pandemic_2019_num_hsld_members, cc_hsld_size_during_pandemic_03_2020_num_hsld_members, cc_hsld_size_during_pandemic_summer_2021_num_hsld_members, cc_hsld_size_during_pandemic_01_2023_num_hsld_members,
                                                                                    cc_weekly_contacts_2019_work_uni_cont, cc_weekly_contacts_03_2020_work_uni_cont, cc_weekly_contacts_summer_2021_work_uni_cont, cc_weekly_contacts_01_2023_work_uni_cont,
                                                                                    #cc_weekly_cont_during_pandemic_2019_work_uni_cont, cc_weekly_cont_during_pandemic_03_2020_work_uni_cont, cc_weekly_cont_during_pandemic_summer_2021_work_uni_cont, cc_weekly_cont_during_pandemic_01_2023_work_uni_cont,
                                                                                    cc_weekly_contacts_2019_school_kinder_cont, cc_weekly_contacts_03_2020_school_kinder_cont, cc_weekly_contacts_summer_2021_school_kinder_cont, cc_weekly_contacts_01_2023_school_kinder_cont,
                                                                                    #cc_weekly_cont_during_pandemic_2019_school_kg_cont, cc_weekly_cont_during_pandemic_03_2020_school_kg_cont, cc_weekly_cont_during_pandemic_summer_2021_school_kg_cont, cc_weekly_cont_during_pandemic_01_2023_school_kg_cont,
                                                                                    cc_weekly_contacts_2019_leisure_cont, cc_weekly_contacts_03_2020_leisure_cont, cc_weekly_contacts_summer_2021_leisure_cont, cc_weekly_contacts_01_2023_leisure_cont)
#cc_weekly_cont_during_pandemic_2019_leisure_cont, cc_weekly_cont_during_pandemic_03_2020_leisure_cont, cc_weekly_cont_during_pandemic_summer_2021_leisure_cont, cc_weekly_cont_during_pandemic_01_2023_leisure_cont)

colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "user_id")] <- "id_original_respondent"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_change_during_pandemic")] <- "cc_change_during_pandemic_original_respondent"

colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_hsld_size_pre_pandemic_2019_num_hsld_members")] <- "hsldsize_2019"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_hsld_size_pre_pandemic_03_2020_num_hsld_members")] <- "hsldsize_03_2020"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_hsld_size_pre_pandemic_summer_2021_num_hsld_members")] <- "hsldsize_summer_2021"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_hsld_size_pre_pandemic_01_2023_num_hsld_members")] <- "hsldsize_01_2023"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_2019_work_uni_cont")] <- "work_2019"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_03_2020_work_uni_cont")] <- "work_03_2020"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_summer_2021_work_uni_cont")] <- "work_summer_2021"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_01_2023_work_uni_cont")] <- "work_01_2023"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_2019_school_kinder_cont")] <- "school_2019"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_03_2020_school_kinder_cont")] <- "school_03_2020"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_summer_2021_school_kinder_cont")] <- "school_summer_2021"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_01_2023_school_kinder_cont")] <- "school_01_2023"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_2019_leisure_cont")] <- "leisure_2019"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_03_2020_leisure_cont")] <- "leisure_03_2020"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_summer_2021_leisure_cont")] <- "leisure_summer_2021"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_01_2023_leisure_cont")] <- "leisure_01_2023"

data_reduced_original_respondents <- data_reduced_original_respondents %>% pivot_longer(cols = c("hsldsize_2019", "hsldsize_03_2020", "hsldsize_summer_2021", "hsldsize_01_2023", "work_2019", "work_03_2020", "work_summer_2021", "work_01_2023", "school_2019", "school_03_2020", "school_summer_2021", "school_01_2023", "leisure_2019", "leisure_03_2020", "leisure_summer_2021", "leisure_01_2023"), names_to ="category", values_to="value_first_respondent")

data_reduced_original_respondents <- data_reduced_original_respondents %>% separate(category, c("category", "time"), "_", extra = "merge")

data_both <- left_join(data_reduced_original_respondents, data_reduced_second_respondents, by = c("id_original_respondent", "category", "time"))

data_both <- data_both %>% mutate(Diff = value_first_respondent - value_second_respondent)

data_both$time <- factor(data_both$time, levels = c("2019", "03_2020", "summer_2021", "01_2023"))

palette <- function() {
  c("#DC0000FF", "#3C5488FF")
}

data_both <- data_both %>% mutate(pairing = case_when(id_second_respondent == "262c9b2a-0ce2-40b5-9ba6-a658eebbc336" ~ "1st Pair", 
                                                      id_second_respondent == "b0c499e7-59f1-40e2-bf93-dd5a8872043b" ~ "2nd Pair",
                                                      id_second_respondent == "34c136ce-b6e6-4c91-8237-58f88c1757f9" ~ "3rd Pair",
 id_second_respondent == "276ab25f-d77e-49a8-b9b0-602a121ba56a" ~ "4th Pair",
 id_second_respondent == "808d388f-30c4-4b1b-806e-49bbc2a0f0c1" ~ "5th Pair",
 id_second_respondent == "512966ef-6b8b-4ccb-8b94-5c869b75d940" ~ "6th Pair",
  id_second_respondent == "dd5e9f8b-9c29-4a80-8328-0039d809c27f" ~ "7th Pair",
  id_second_respondent == "2b4b3e90-5603-4150-a96d-ee40f0229c61" ~ "8th Pair",
id_second_respondent =="c60925b1-155c-43dd-a2e6-d6ddd2c5219d" ~ "9th Pair"))


# Accuracy of Household Reporting
# Produces Supplementary Figure 6

households <- data_both %>% filter(id_second_respondent != "c34b67f3-53ca-47e4-82b2-56a7bc7f2c44") %>% 
  filter(category == "hsldsize") %>% filter(time == "2019") %>% select(c("pairing", "value_first_respondent", "value_second_respondent"))
households <- households %>% pivot_longer(cols = c("value_first_respondent", "value_second_respondent"))
households <- households %>% mutate(name = case_when(name == "value_first_respondent" ~ "Original Participant",
                                                     name == "value_second_respondent" ~ "Closest Contact"))
households$name <- factor(households$name, levels = c("Original Participant", "Closest Contact"))

palette <- function() {
  c("#3C5488FF", "#DC0000FF")
}

ggplot(households) + 
  geom_point(aes(x = time, y = value, color = name), position = position_dodge2(width = 1), size = 5) +
  theme_bw() +
  facet_wrap(~pairing, nrow=1) +
  ylab("Number of\nHousehold Members") +
  xlab("") +
  theme(legend.position = "bottom", legend.title=element_blank()) +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  scale_color_manual(values = palette()) 

#ggsave(paste0("./plots/", "AccuracyHousehold.pdf"), dpi = 500, w = 20, h = 6)
#ggsave(paste0("./plots/", "AccuracyHousehold.png"), dpi = 500, w = 20, h = 6)

# Accuracy of Work Contacts Reporting
# Produces Supplementary Figure 7

data_both <- data_both %>% mutate(time = case_when(time == "03_2020" ~ "03/2020",
                                                   time == "summer_2021" ~ "Summer\n2021",
                                                   time == "01_2023" ~ "01/2023",
                                                   .default = time))

data_both$time <- factor(data_both$time, levels = c("2019", "03/2020", "Summer\n2021", "01/2023"))

work <- data_both %>% filter(id_second_respondent != "c34b67f3-53ca-47e4-82b2-56a7bc7f2c44") %>% 
  filter(category == "work") %>% select(c("time", "pairing", "value_first_respondent", "value_second_respondent"))
work <- work %>% pivot_longer(cols = c("value_first_respondent", "value_second_respondent"))
work <- work %>% mutate(name = case_when(name == "value_first_respondent" ~ "Original Participant",
                                                     name == "value_second_respondent" ~ "Closest Contact"))
work$name <- factor(work$name, levels = c("Original Participant", "Closest Contact"))

palette <- function() {
  c("#3C5488FF", "#DC0000FF")
}

ggplot(work) + 
  geom_point(aes(x = time, y = value, color = name), position = position_dodge2(width = 1), size = 5) +
  theme_bw() +
  facet_wrap(~pairing, nrow=3) +
  ylab("Number of\nContacts") +
  xlab("") +
  theme(legend.position = "bottom", legend.title=element_blank()) +
  theme(text = element_text(size = 30)) +
  scale_color_manual(values = palette()) 

#ggsave(paste0("./plots/", "AccuracyWork.pdf"), dpi = 500, w = 20, h = 12)
#ggsave(paste0("./plots/", "AccuracyWork.png"), dpi = 500, w = 20, h = 12)

# Accuracy of Leisure Contacts Reporting
# Produces Supplementary Figure 8

leisure <- data_both %>% filter(id_second_respondent != "c34b67f3-53ca-47e4-82b2-56a7bc7f2c44") %>% 
  filter(category == "leisure") %>% select(c("time", "pairing", "value_first_respondent", "value_second_respondent"))
leisure <- leisure %>% pivot_longer(cols = c("value_first_respondent", "value_second_respondent"))
leisure <- leisure %>% mutate(name = case_when(name == "value_first_respondent" ~ "Original Participant",
                                         name == "value_second_respondent" ~ "Closest Contact"))
leisure$name <- factor(leisure$name, levels = c("Original Participant", "Closest Contact"))

palette <- function() {
  c("#3C5488FF", "#DC0000FF")
}

ggplot(leisure) + 
  geom_point(aes(x = time, y = value, color = name), position = position_dodge2(width = 1), size = 5) +
  theme_bw() +
  facet_wrap(~pairing, nrow=3) +
  ylab("Number of\nContacts") +
  xlab("") +
  theme(legend.position = "bottom", legend.title=element_blank()) +
  theme(text = element_text(size = 30)) +
  scale_color_manual(values = palette()) 

#ggsave(paste0("./plots/", "AccuracyLeisure.pdf"), dpi = 500, w = 20, h = 12)
#ggsave(paste0("./plots/", "AccuracyLeisure.png"), dpi = 500, w = 20, h = 12)

