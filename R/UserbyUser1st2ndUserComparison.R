library(tidyverse)
library(igraph)
library(gridExtra)
library(ggiraphExtra)

# Author: S. Paltra, contact: paltra@tu-berlin.de

# Aim of FIRST PART of this script:
# Survey respondents could forward the survey to their CC
# This script compares the answers of the secondary respondents (referred to as 'second responds')
# to the answers of the initial respondents (referred to as 'first respondent')
# (Internal node: Slide 52 f.)

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
  c("#f1a340", "#998ec3")
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

#Difference all categories
ggplot(data_both %>% filter(id_second_respondent != "c34b67f3-53ca-47e4-82b2-56a7bc7f2c44") %>% filter(category %in% c("work", "leisure"))) + 
  geom_col(aes(x = time, y = Diff, fill= factor(category)), position = "dodge", size = 3) +
  theme_minimal() +
  facet_wrap(~pairing, nrow=3) +
  ylab("No. of Contacts Reported by Original Participant Minus\n No. of Contacts Reported by CC themselves") +
  xlab("Point In Time") +
  theme(legend.position = "bottom", legend.title=element_blank()) +
  theme(text = element_text(size = 30)) +
  scale_fill_manual(values = palette())

ggsave("AssumedMinusActual.pdf", dpi = 500, w = 27, h = 12)
ggsave("AssumedMinusActual.png", dpi = 500, w = 27, h = 12)


#Difference household
ggplot(data_both %>% filter(category == "hsldsize")) + 
  geom_point(aes(x = time, y = Diff, colour = factor(category)), size = 3) +
  theme_linedraw() +
  facet_wrap(~id_second_respondent, nrow=2) +
  scale_color_manual(values = palette()) +
  ylab("Assumed # Contacts CC - #Actual Contacts CC") +
  xlab("") +
  theme(legend.position = "bottom", legend.title=element_blank()) +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_text(angle = 90)) 

ggsave("AssumedMinusActualHousehold.pdf", dpi = 500, w = 21, h = 9)
ggsave("AssumedMinusActualHousehold.png", dpi = 500, w = 21, h = 9)

p1 <- ggplot(data_both %>% filter(category == "hsldsize"), aes(time, Diff)) +
  geom_boxplot(color = "#E4572E", linewidth = 1.1) +
  theme_minimal() +
  ggtitle("Household Size") +
  xlab("Point In Time") +
  ylab("Assumed # Contacts CC \n Minus #Actual Contacts CC") +
  theme(text = element_text(size = 22))

ggsave("AssumedMinusActualHouseholdBoxPlot.pdf", dpi = 500, w = 8, h = 4.5)
ggsave("AssumedMinusActualHouseholdBoxPlot.png", dpi = 500, w = 8, h = 4.5)

#Difference work
ggplot(data_both %>% filter(category == "work")) + 
  geom_point(aes(x = time, y = Diff, colour = factor(category)), size = 3) +
  theme_linedraw() +
  facet_wrap(~id_second_respondent, nrow=2) +
  scale_color_manual(values = palette()) +
  ylab("Assumed # Contacts CC - #Actual Contacts CC") +
  xlab("Assumed # Contacts CC \n Minus #Actual Contacts CC") +
  theme(legend.position = "bottom", legend.title=element_blank()) +
  theme(text = element_text(size = 25)) +
  theme(axis.text.x = element_text(angle = 90)) 

ggsave("AssumedMinusActualWork.pdf", dpi = 500, w = 21, h = 9)
ggsave("AssumedMinusActualWork.png", dpi = 500, w = 21, h = 9)

p2 <- ggplot(data_both %>% filter(category == "work"), aes(time, Diff)) +
  geom_boxplot(color = "#E4572E", linewidth = 1.1) +
  theme_minimal() +
  xlab("Time") +
  ggtitle("Work Contacts") +
  ylab("Assumed # Contacts CC \n Minus #Actual Contacts CC") +
  theme(text = element_text(size = 25))

ggsave("AssumedMinusActualWorkBoxPlot.pdf", dpi = 500, w = 8, h = 4.5)
ggsave("AssumedMinusActualWorkBoxPlot.png", dpi = 500, w = 8, h = 4.5)

#Difference school
ggplot(data_both %>% filter(category == "school")) + 
  geom_point(aes(x = time, y = Diff, colour = factor(category)), size = 3) +
  theme_linedraw() +
  facet_wrap(~id_second_respondent, nrow=2) +
  scale_color_manual(values = palette()) +
  ylab("Assumed # Contacts CC - #Actual Contacts CC") +
  xlab("") +
  theme(legend.position = "bottom", legend.title=element_blank()) +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_text(angle = 90)) 

ggsave("AssumedMinusActualSchool.pdf", dpi = 500, w = 21, h = 9)
ggsave("AssumedMinusActualSchool.png", dpi = 500, w = 21, h = 9)

ggplot(data_both %>% filter(category == "school"), aes(time, Diff)) +
  geom_boxplot() +
  theme_minimal() +
  xlab("Time") +
  ylab("Difference") +
  theme(text = element_text(size = 22))

ggsave("AssumedMinusActualSchoolBoxPlot.pdf", dpi = 500, w = 8, h = 4)
ggsave("AssumedMinusActualSchoolBoxPlot.png", dpi = 500, w = 8, h = 4)

#Difference leisure
ggplot(data_both %>% filter(category == "leisure")) + 
  geom_point(aes(x = time, y = Diff, colour = factor(category)), size = 3) +
  theme_linedraw() +
  facet_wrap(~id_second_respondent, nrow=2) +
  scale_color_manual(values = palette()) +
  ylab("Assumed # Contacts CC - #Actual Contacts CC") +
  xlab("") +
  theme(legend.position = "bottom", legend.title=element_blank()) +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_text(angle = 90)) 

ggsave("AssumedMinusActualLeisure.pdf", dpi = 500, w = 21, h = 9)
ggsave("AssumedMinusActualLeisure.png", dpi = 500, w = 21, h = 9)

p3 <- ggplot(data_both %>% filter(category == "leisure"), aes(time, Diff)) +
  geom_boxplot(color = "#E4572E", linewidth = 1.1) +
  theme_minimal() +
  xlab("Time") +
  ggtitle("Leisure Contacts") +
  ylab("") +
  theme(text = element_text(size = 25))

p <- arrangeGrob(p2,p3, nrow=1)
ggsave("AssumedMinusActualBoxPlot.pdf", p, dpi = 500, w = 24, h = 5)
ggsave("AssumedMinusActualBoxPlot.png", p, dpi = 500, w = 24, h = 5)

ggsave("AssumedMinusActualLeisureBoxPlot.pdf", dpi = 500, w = 8, h = 4.5)
ggsave("AssumedMinusActualLeisureBoxPlot.png", dpi = 500, w = 8, h = 4.5)


#Computation of remaining % of contacts for respondent, pre-pandemic and during-pandemic CC
# Respondent
data_reduced <- data_reduced %>% mutate(red_respondent_work_2019_2020 = 100-(100/respondent_work_2019)*(respondent_work_2019-respondent_work_03_2020)) %>%
  mutate(red_respondent_work_2019_2021 = 100-(100/respondent_work_2019)*(respondent_work_2019-respondent_work_summer_2021)) %>%
  mutate(red_respondent_work_2019_2023 = 100-(100/respondent_work_2019)*(respondent_work_2019-respondent_work_01_2023)) %>%
  mutate(red_respondent_all_2019_2020 = 100-(100/respondent_all_2019)*(respondent_all_2019-respondent_all_03_2020)) %>%
  mutate(red_respondent_all_2019_2021 = 100-(100/respondent_all_2019)*(respondent_all_2019-respondent_all_summer_2021)) %>%
  mutate(red_respondent_all_2019_2023 = 100-(100/respondent_all_2019)*(respondent_all_2019-respondent_all_01_2023)) %>%
  mutate(red_respondent_leisure_2019_2020 = 100-(100/respondent_leisure_2019)*(respondent_leisure_2019-respondent_leisure_03_2020)) %>%
  mutate(red_respondent_leisure_2019_2021 = 100-(100/respondent_leisure_2019)*(respondent_leisure_2019-respondent_leisure_summer_2021)) %>%
  mutate(red_respondent_leisure_2019_2023 = 100-(100/respondent_leisure_2019)*(respondent_leisure_2019-respondent_leisure_01_2023))
# Pre-Pandemic CC
data_reduced <- data_reduced %>% mutate(red_cc_pre_work_2019_2020 = 100-(100/cc_pre_work_2019)*(cc_pre_work_2019-cc_pre_work_03_2020)) %>%
  mutate(red_cc_pre_work_2019_2021 = 100-(100/cc_pre_work_2019)*(cc_pre_work_2019-cc_pre_work_summer_2021)) %>%
  mutate(red_cc_pre_work_2019_2023 = 100-(100/cc_pre_work_2019)*(cc_pre_work_2019-cc_pre_work_01_2023)) %>%
  mutate(red_cc_pre_all_2019_2020 = 100-(100/cc_pre_all_2019)*(cc_pre_all_2019-cc_pre_all_03_2020)) %>%
  mutate(red_cc_pre_all_2019_2021 = 100-(100/cc_pre_all_2019)*(cc_pre_all_2019-cc_pre_all_summer_2021)) %>%
  mutate(red_cc_pre_all_2019_2023 = 100-(100/cc_pre_all_2019)*(cc_pre_all_2019-cc_pre_all_01_2023)) %>%
  mutate(red_cc_pre_leisure_2019_2020 = 100-(100/cc_pre_leisure_2019)*(cc_pre_leisure_2019-cc_pre_leisure_03_2020)) %>%
  mutate(red_cc_pre_leisure_2019_2021 = 100-(100/cc_pre_leisure_2019)*(cc_pre_leisure_2019-cc_pre_leisure_summer_2021)) %>%
  mutate(red_cc_pre_leisure_2019_2023 = 100-(100/cc_pre_leisure_2019)*(cc_pre_leisure_2019-cc_pre_leisure_01_2023))
# During-Pandemic CC 
data_reduced <- data_reduced %>% mutate(red_cc_during_work_2019_2020 = 100-(100/cc_during_work_2019)*(cc_during_work_2019-cc_during_work_03_2020)) %>%
  mutate(red_cc_during_work_2019_2021 = 100-(100/cc_during_work_2019)*(cc_during_work_2019-cc_during_work_summer_2021)) %>%
  mutate(red_cc_during_work_2019_2023 = 100-(100/cc_during_work_2019)*(cc_during_work_2019-cc_during_work_01_2023)) %>%
  mutate(red_cc_during_all_2019_2020 = 100-(100/cc_during_all_2019)*(cc_during_all_2019-cc_during_all_03_2020)) %>%
  mutate(red_cc_during_all_2019_2021 = 100-(100/cc_during_all_2019)*(cc_during_all_2019-cc_during_all_summer_2021)) %>%
  mutate(red_cc_during_all_2019_2023 = 100-(100/cc_during_all_2019)*(cc_during_all_2019-cc_during_all_01_2023)) %>%
  mutate(red_cc_during_leisure_2019_2020 = 100-(100/cc_during_leisure_2019)*(cc_during_leisure_2019-cc_during_leisure_03_2020)) %>%
  mutate(red_cc_during_leisure_2019_2021 = 100-(100/cc_during_leisure_2019)*(cc_during_leisure_2019-cc_during_leisure_summer_2021)) %>%
  mutate(red_cc_during_leisure_2019_2023 = 100-(100/cc_during_leisure_2019)*(cc_during_leisure_2019-cc_during_leisure_01_2023))

