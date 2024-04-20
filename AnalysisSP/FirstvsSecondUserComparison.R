library(tidyverse)
library(igraph)

# Author: S. Paltra, contact: paltra@tu-berlin.de

# Aim of this script:
# Survey respondents could forward the survey to their CC
# This script compares the answers of the secondary respondents (referred to as 'second responds')
# to the answers of the initial respondents (referred to as 'first respondent')


raw_data <- read_csv("/Users/sydney/Downloads/twitter_data.csv")


# Reducing data frame to the variables of interest ------------------------

data_reduced <- raw_data %>% select(user_id, ref, cc_change_during_pandemic, total_hsld_size_persons_under_14,
                                    hsld_size_2019_, hsld_size_03_2020_, hsld_size_summer_2021_, hsld_size_01_2023_, 
                                    cc_hsld_size_pre_pandemic_2019_num_hsld_members, cc_hsld_size_pre_pandemic_03_2020_num_hsld_members, cc_hsld_size_pre_pandemic_summer_2021_num_hsld_members, cc_hsld_size_pre_pandemic_01_2023_num_hsld_members,
                                    cc_hsld_size_during_pandemic_2019_num_hsld_members, cc_hsld_size_during_pandemic_03_2020_num_hsld_members, cc_hsld_size_during_pandemic_summer_2021_num_hsld_members, cc_hsld_size_during_pandemic_01_2023_num_hsld_members,
                                    wkly_cont_2019_work_uni, wkly_cont_03_2020_work_uni, wkly_cont_summer_2021_work_uni, wkly_cont_01_2023_work_uni,
                                    hsld_cont__2019_work_uni, hsld_cont__03_2020_work_uni, hsld_cont__summer_2021_work_uni, hsld_cont__01_2023_work_uni,
                                    cc_weekly_contacts_2019_work_uni_cont, cc_weekly_contacts_03_2020_work_uni_cont, cc_weekly_contacts_summer_2021_work_uni_cont, cc_weekly_contacts_01_2023_work_uni_cont,
                                    cc_weekly_cont_during_pandemic_2019_work_uni_cont, cc_weekly_cont_during_pandemic_03_2020_work_uni_cont, cc_weekly_cont_during_pandemic_summer_2021_work_uni_cont, cc_weekly_cont_during_pandemic_01_2023_work_uni_cont,
                                    wkly_cont_2019_school_kinder, wkly_cont_03_2020_school_kinder, wkly_cont_summer_2021_school_kinder, wkly_cont_01_2023_school_kinder,
                                    hsld_cont__2019_school_kinder, hsld_cont__03_2020_school_kinder, hsld_cont__summer_2021_school_kinder, hsld_cont__01_2023_school_kinder,
                                    cc_weekly_contacts_2019_school_kinder_cont, cc_weekly_contacts_03_2020_school_kinder_cont, cc_weekly_contacts_summer_2021_school_kinder_cont, cc_weekly_contacts_01_2023_school_kinder_cont,
                                    cc_weekly_cont_during_pandemic_2019_school_kg_cont, cc_weekly_cont_during_pandemic_03_2020_school_kg_cont, cc_weekly_cont_during_pandemic_summer_2021_school_kg_cont, cc_weekly_cont_during_pandemic_01_2023_school_kg_cont,
                                    wkly_cont_2019_leisure, wkly_cont_03_2020_leisure, wkly_cont_summer_2021_leisure, wkly_cont_01_2023_leisure,
                                    hsld_cont__2019_leisure, hsld_cont__03_2020_leisure, hsld_cont__summer_2021_leisure, hsld_cont__01_2023_leisure,
                                    cc_weekly_contacts_2019_leisure_cont, cc_weekly_contacts_03_2020_leisure_cont, cc_weekly_contacts_summer_2021_leisure_cont, cc_weekly_contacts_01_2023_leisure_cont,
                                    cc_weekly_cont_during_pandemic_2019_leisure_cont, cc_weekly_cont_during_pandemic_03_2020_leisure_cont, cc_weekly_cont_during_pandemic_summer_2021_leisure_cont, cc_weekly_cont_during_pandemic_01_2023_leisure_cont)


# Respondents who were forwarded the survey -------------------------------

data_reduced_second_respondents <- data_reduced %>% filter(!is.na(ref)) %>% 
  filter(!is.na(user_id)) %>%
  # Removal of respondets who's reference is one of the 5 people who share the link on twitter
  filter(!(ref %in% c("4a76b", "dec9d", "7b598", "008b5", "6c8d7"))) %>%
  # Removal of respondents who clicked on the link they were forwarded, but did not fill anything in 
  filter(!(user_id %in% c("5c0412b6-edfd-4eb8-84df-f8fe25b8c208", "b8bf1416-b83f-4b1b-a411-e715a6437cd2", "caba33fe-d132-4f6c-8dea-c059392888d4")))
# Manually removing the second to last row as this user started the survey twice
data_reduced_second_respondents <- data_reduced_second_respondents[-21,]   

data_reduced_second_respondents <- data_reduced_second_respondents %>% select(user_id, ref, cc_change_during_pandemic,  
                                                                              total_hsld_size_persons_under_14, hsld_size_2019_, hsld_size_03_2020_, hsld_size_summer_2021_, hsld_size_01_2023_,
                                                                              wkly_cont_2019_work_uni, wkly_cont_03_2020_work_uni, wkly_cont_summer_2021_work_uni, wkly_cont_01_2023_work_uni, 
                                                                              wkly_cont_2019_school_kinder, wkly_cont_03_2020_school_kinder, wkly_cont_summer_2021_school_kinder, wkly_cont_01_2023_school_kinder,
                                                                              wkly_cont_2019_leisure, wkly_cont_03_2020_leisure, wkly_cont_summer_2021_leisure, wkly_cont_01_2023_leisure)

colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "user_id")] <- "second_user_id"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "ref")] <- "second_ref"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "cc_change_during_pandemic")] <- "second_cc_change"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "total_hsld_size_persons_under_14")] <- "second_hsld_size_persons_under_14"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "hsld_size_2019_")] <- "second_hsld_size_2019"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "hsld_size_03_2020_")] <- "second_hsld_size_03_2020"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "hsld_size_summer_2021_")] <- "second_hsld_size_summer_2021"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "hsld_size_01_2023_")] <- "second_hsld_size_01_2023"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_2019_work_uni")] <- "second_work_2019"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_03_2020_work_uni")] <- "second_work_03_2020"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_summer_2021_work_uni")] <- "second_work_summer_2021"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_01_2023_work_uni")] <- "second_work_01_2023"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_2019_school_kinder")] <- "second_school_2019"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_03_2020_school_kinder")] <- "second_school_03_2020"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_summer_2021_school_kinder")] <- "second_school_summer_2021"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_01_2023_school_kinder")] <- "second_school_01_2023"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_2019_leisure")] <- "second_leisure_2019"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_03_2020_leisure")] <- "second_leisure_03_2020"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_summer_2021_leisure")] <- "second_leisure_summer_2021"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_01_2023_leisure")] <- "second_leisure_01_2023"


#Setting no of contacts equal to 0 if person DID not answer 
#TODO: How do I differentiate real zeros from introduced 0? Are there even "real zeros"? Check!
data_reduced_second_respondents <- as.data.frame(data_reduced_second_respondents)
data_reduced_second_respondents[is.na(data_reduced_second_respondents)] <- 0

data_reduced_second_respondents <- data_reduced_second_respondents %>% filter(second_cc_change == "Nein")

data_first_and_second_resp <- data.frame()

for(i in 1:nrow(data_reduced_second_respondents)){
  secondary_respondent <- data_reduced_second_respondents[i,]
  first_respondent <- data_reduced %>% filter(user_id == secondary_respondent$second_ref)
  
  first_respondent <- first_respondent %>% select(user_id, cc_change_during_pandemic, 
                                      cc_hsld_size_pre_pandemic_2019_num_hsld_members, cc_hsld_size_pre_pandemic_03_2020_num_hsld_members, cc_hsld_size_pre_pandemic_summer_2021_num_hsld_members, cc_hsld_size_pre_pandemic_01_2023_num_hsld_members,
                                      cc_hsld_size_during_pandemic_2019_num_hsld_members, cc_hsld_size_during_pandemic_03_2020_num_hsld_members, cc_hsld_size_during_pandemic_summer_2021_num_hsld_members, cc_hsld_size_during_pandemic_01_2023_num_hsld_members,
                                      cc_weekly_contacts_2019_work_uni_cont, cc_weekly_contacts_03_2020_work_uni_cont, cc_weekly_contacts_summer_2021_work_uni_cont, cc_weekly_contacts_01_2023_work_uni_cont,
                                      cc_weekly_cont_during_pandemic_2019_work_uni_cont, cc_weekly_cont_during_pandemic_03_2020_work_uni_cont, cc_weekly_cont_during_pandemic_summer_2021_work_uni_cont, cc_weekly_cont_during_pandemic_01_2023_work_uni_cont,
                                      cc_weekly_contacts_2019_school_kinder_cont, cc_weekly_contacts_03_2020_school_kinder_cont, cc_weekly_contacts_summer_2021_school_kinder_cont, cc_weekly_contacts_01_2023_school_kinder_cont,
                                      cc_weekly_cont_during_pandemic_2019_school_kg_cont, cc_weekly_cont_during_pandemic_03_2020_school_kg_cont, cc_weekly_cont_during_pandemic_summer_2021_school_kg_cont, cc_weekly_cont_during_pandemic_01_2023_school_kg_cont,
                                      cc_weekly_contacts_2019_leisure_cont, cc_weekly_contacts_03_2020_leisure_cont, cc_weekly_contacts_summer_2021_leisure_cont, cc_weekly_contacts_01_2023_leisure_cont,
                                      cc_weekly_cont_during_pandemic_2019_leisure_cont, cc_weekly_cont_during_pandemic_03_2020_leisure_cont, cc_weekly_cont_during_pandemic_summer_2021_leisure_cont, cc_weekly_cont_during_pandemic_01_2023_leisure_cont)
  
  colnames(first_respondent)[which(names(first_respondent) == "user_id")] <- "first_user_id"
  colnames(first_respondent)[which(names(first_respondent) == "cc_change_during_pandemic")] <- "first_cc_change"
  
  colnames(first_respondent)[which(names(first_respondent) == "cc_hsld_size_pre_pandemic_2019_num_hsld_members")] <- "first_cc_pre_hsld_size_2019"
  colnames(first_respondent)[which(names(first_respondent) == "cc_hsld_size_pre_pandemic_03_2020_num_hsld_members")] <- "first_cc_pre_hsld_size_03_2020"
  colnames(first_respondent)[which(names(first_respondent) == "cc_hsld_size_pre_pandemic_summer_2021_num_hsld_members")] <- "first_cc_pre_hsld_size_summer_2021"
  colnames(first_respondent)[which(names(first_respondent) == "cc_hsld_size_pre_pandemic_01_2023_num_hsld_members")] <- "first_cc_pre_hsld_size_01_2023"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_contacts_2019_work_uni_cont")] <- "first_cc_pre_work_2019"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_contacts_03_2020_work_uni_cont")] <- "first_cc_pre_work_03_2020"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_contacts_summer_2021_work_uni_cont")] <- "first_cc_pre_work_summer_2021"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_contacts_01_2023_work_uni_cont")] <- "first_cc_pre_work_01_2023"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_contacts_2019_school_kinder_cont")] <- "first_cc_pre_school_2019"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_contacts_03_2020_school_kinder_cont")] <- "first_cc_pre_school_03_2020"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_contacts_summer_2021_school_kinder_cont")] <- "first_cc_pre_school_summer_2021"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_contacts_01_2023_school_kinder_cont")] <- "first_cc_pre_school_01_2023"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_contacts_2019_leisure_cont")] <- "first_cc_pre_leisure_2019"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_contacts_03_2020_leisure_cont")] <- "first_cc_pre_leisure_03_2020"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_contacts_summer_2021_leisure_cont")] <- "first_cc_pre_leisure_summer_2021"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_contacts_01_2023_leisure_cont")] <- "first_cc_pre_leisure_01_2023"
  
  colnames(first_respondent)[which(names(first_respondent) == "cc_hsld_size_during_pandemic_2019_num_hsld_members")] <- "first_cc_during_hsld_size_2019"
  colnames(first_respondent)[which(names(first_respondent) == "cc_hsld_size_during_pandemic_03_2020_num_hsld_members")] <- "first_cc_during_hsld_size_03_2020"
  colnames(first_respondent)[which(names(first_respondent) == "cc_hsld_size_during_pandemic_summer_2021_num_hsld_members")] <- "first_cc_during_hsld_size_summer_2021"
  colnames(first_respondent)[which(names(first_respondent) == "cc_hsld_size_during_pandemic_01_2023_num_hsld_members")] <- "first_cc_during_hsld_size_01_2023"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_cont_during_pandemic_2019_work_uni_cont")] <- "first_cc_during_work_2019"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_cont_during_pandemic_03_2020_work_uni_cont")] <- "first_cc_during_work_03_2020"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_cont_during_pandemic_summer_2021_work_uni_cont")] <- "first_cc_during_work_summer_2021"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_cont_during_pandemic_01_2023_work_uni_cont")] <- "first_cc_during_work_01_2023"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_cont_during_pandemic_2019_school_kg_cont")] <- "first_cc_during_school_2019"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_cont_during_pandemic_03_2020_school_kg_cont")] <- "first_cc_during_school_03_2020"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_cont_during_pandemic_summer_2021_school_kg_cont")] <- "first_cc_during_school_summer_2021"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_cont_during_pandemic_01_2023_school_kg_cont")] <- "first_cc_during_school_01_2023"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_cont_during_pandemic_2019_leisure_cont")] <- "first_cc_during_leisure_2019"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_cont_during_pandemic_03_2020_leisure_cont")] <- "first_cc_during_leisure_03_2020"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_cont_during_pandemic_summer_2021_leisure_cont")] <- "first_cc_during_leisure_summer_2021"
  colnames(first_respondent)[which(names(first_respondent) == "cc_weekly_cont_during_pandemic_01_2023_leisure_cont")] <- "first_cc_during_leisure_01_2023"

  secondary_respondent <- cbind(secondary_respondent, first_respondent)
  data_first_and_second_resp <- rbind(data_first_and_second_resp, secondary_respondent)
}


data_first_and_second_resp <- data_first_and_second_resp %>% mutate(diff_hsld_2019 = first_cc_pre_hsld_size_2019 - second_hsld_size_2019,
                                                                    diff_hsld_03_2020 = first_cc_pre_hsld_size_03_2020 - second_hsld_size_03_2020,
                                                                    diff_hsld_summer_2021 = first_cc_pre_hsld_size_summer_2021 - second_hsld_size_summer_2021,
                                                                    diff_hsld_01_2023 = first_cc_pre_hsld_size_01_2023 - second_hsld_size_01_2023,
                                                                    diff_work_2019 = first_cc_pre_work_2019 - second_work_2019,
                                                                    diff_work_03_2020  = first_cc_pre_work_03_2020 - second_work_03_2020,
                                                                    diff_work_summer_2021  = first_cc_pre_work_summer_2021 - second_work_summer_2021,
                                                                    diff_work_01_2023  = first_cc_pre_work_01_2023 - second_work_01_2023,
                                                                    diff_school_2019 = first_cc_pre_school_2019 - second_school_2019,
                                                                    diff_school_03_2020 = first_cc_pre_school_03_2020 - second_school_03_2020,
                                                                    diff_school_summer_2021 = first_cc_pre_school_summer_2021 - second_school_summer_2021,
                                                                    diff_school_01_2023 = first_cc_pre_school_01_2023 - second_school_01_2023,
                                                                    diff_leisure_2019 = first_cc_pre_leisure_2019 - second_leisure_2019,
                                                                    diff_leisure_03_2020 = first_cc_pre_leisure_03_2020 - second_leisure_03_2020,
                                                                    diff_leisure_summer_2021 = first_cc_pre_leisure_summer_2021 - second_leisure_summer_2021,
                                                                    diff_leisure_01_2023 = first_cc_pre_leisure_01_2023 - second_leisure_01_2023)

data_first_and_second_resp <- data_first_and_second_resp %>% pivot_longer(cols=c("second_hsld_size_persons_under_14","second_hsld_size_2019","second_hsld_size_03_2020",             
                                                                          "second_hsld_size_summer_2021", "second_hsld_size_01_2023", 
                                                                          "second_work_2019", "second_work_03_2020", "second_work_summer_2021", "second_work_01_2023",                  
                                                                          "second_school_2019", "second_school_03_2020", "second_school_summer_2021", "second_school_01_2023",              
                                                                          "second_leisure_2019", "second_leisure_03_2020", "second_leisure_summer_2021", "second_leisure_01_2023",
                                                                          "first_cc_pre_hsld_size_2019", "first_cc_pre_hsld_size_03_2020", "first_cc_pre_hsld_size_summer_2021", "first_cc_pre_hsld_size_01_2023",
                                                                          "first_cc_during_hsld_size_2019", "first_cc_during_hsld_size_03_2020", "first_cc_during_hsld_size_summer_2021", "first_cc_during_hsld_size_01_2023",    
                                                                          "first_cc_pre_work_2019", "first_cc_pre_work_03_2020", "first_cc_pre_work_summer_2021", "first_cc_pre_work_01_2023",             
                                                                          "first_cc_during_work_2019", "first_cc_during_work_03_2020", "first_cc_during_work_summer_2021", "first_cc_during_work_01_2023", 
                                                                          "first_cc_pre_school_2019", "first_cc_pre_school_03_2020", "first_cc_pre_school_summer_2021", "first_cc_pre_school_01_2023",          
                                                                          "first_cc_during_school_2019", "first_cc_during_school_03_2020", "first_cc_during_school_summer_2021", "first_cc_during_school_01_2023",
                                                                          "first_cc_pre_leisure_2019", "first_cc_pre_leisure_03_2020", "first_cc_pre_leisure_summer_2021", "first_cc_pre_leisure_01_2023", 
                                                                          "first_cc_during_leisure_2019", "first_cc_during_leisure_03_2020", "first_cc_during_leisure_summer_2021", "first_cc_during_leisure_01_2023",
                                                                          "diff_hsld_2019", "diff_hsld_03_2020", "diff_hsld_summer_2021", "diff_hsld_01_2023",
                                                                          "diff_work_2019", "diff_work_03_2020", "diff_work_summer_2021", "diff_work_01_2023",
                                                                          "diff_school_2019", "diff_school_03_2020", "diff_school_summer_2021", "diff_school_01_2023",
                                                                          "diff_leisure_2019", "diff_leisure_03_2020", "diff_leisure_summer_2021", "diff_leisure_01_2023"))

data_first_and_second_resp$name <- factor(data_first_and_second_resp$name, levels = c("second_hsld_size_persons_under_14",
                                                                                      "second_hsld_size_2019", "first_cc_pre_hsld_size_2019", "first_during_during_hsld_size_2019", "diff_hsld_2019",
                                                                                      "second_hsld_size_03_2020", "first_cc_pre_hsld_size_03_2020", "first_cc_during_hsld_size_03_2020", "diff_hsld_03_2020",      
                                                                                      "second_hsld_size_summer_2021", "first_cc_pre_hsld_size_summer_2021", "first_cc_during_hsld_size_summer_2021", "diff_hsld_summer_2021",
                                                                                      "second_hsld_size_01_2023", "first_cc_pre_hsld_size_01_2023", "first_cc_during_hsld_size_01_2023", "diff_hsld_01_2023",
                                                                                      "second_work_2019", "first_cc_pre_work_2019", "first_cc_during_work_2019", "diff_work_2019", 
                                                                                      "second_work_03_2020", "first_cc_pre_work_03_2020", "first_cc_during_work_03_2020", "diff_work_03_2020",  
                                                                                      "second_work_summer_2021", "first_cc_pre_work_summer_2021", "first_cc_during_work_summer_2021", "diff_work_summer_2021",  
                                                                                      "second_work_01_2023", "first_cc_pre_work_01_2023", "first_cc_during_work_01_2023", "diff_work_01_2023",
                                                                                      "second_school_2019", "first_cc_pre_school_2019", "first_cc_during_school_2019", "diff_school_2019",
                                                                                      "second_school_03_2020", "first_cc_pre_school_03_2020", "first_cc_during_school_03_2020", "diff_school_03_2020",
                                                                                      "second_school_summer_2021", "first_cc_pre_school_summer_2021", "first_cc_during_school_summer_2021", "diff_school_summer_2021",
                                                                                      "second_school_01_2023", "first_cc_pre_school_01_2023", "first_cc_during_school_01_2023", "diff_school_01_2023",
                                                                                      "second_leisure_2019", "first_cc_pre_leisure_2019", "first_cc_during_leisure_2019", "diff_leisure_2019",
                                                                                      "second_leisure_03_2020", "first_cc_pre_leisure_03_2020", "first_cc_during_leisure_03_2020", "diff_leisure_03_2020",
                                                                                      "second_leisure_summer_2021", "first_cc_pre_leisure_summer_2021", "first_cc_during_leisure_summer_2021", "diff_leisure_summer_2021",
                                                                                      "second_leisure_01_2023", "first_cc_pre_leisure_01_2023", "first_cc_during_leisure_01_2023", "diff_leisure_01_2023"
                                                                                      ))

data_first_and_second_resp <- data_first_and_second_resp %>% mutate(time = case_when(str_detect(name, "2019") ~ "2019",
                                                                                     str_detect(name, "03_2020") ~ "03_2020",
                                                                                     str_detect(name, "summer_2021") ~ "summer_2021",
                                                                                     str_detect(name, "01_2023") ~ "01_2023"))

  #Household size plot
ggplot(data_first_and_second_resp %>% filter(!str_detect(name, "diff")) %>% filter(!str_detect(name, "during")) %>% filter(!str_detect(name, "work")) %>% filter(!str_detect(name, "school")) %>% filter(!str_detect(name, "leisure"))) + 
  geom_histogram(aes(x = value, y = after_stat(count / sum(count)), fill = time), bins = 10) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15)) +
  labs(title=paste0("Household Members"))

#ggsave("HouseholdFirstSecondComparison.pdf", dpi = 500, w = 9, h = 6)
#ggsave("HouseholdFirstSecondComparison.png", dpi = 500, w = 9, h = 6)


# Work contacts plot
ggplot(data_first_and_second_resp %>% filter(!str_detect(name, "diff")) %>% filter(!str_detect(name, "during")) %>% filter(!str_detect(name, "hsld")) %>% filter(!str_detect(name, "school")) %>% filter(!str_detect(name, "leisure"))) + 
  geom_histogram(aes(x = value, y = after_stat(count / sum(count)), fill = time), bins = 10) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))  +
  labs(title=paste0("Work Contacts")) 

#ggsave("WorkFirstSecondComparison.pdf", dpi = 500, w = 9, h = 6)
#ggsave("WorkFirstSecondComparison.png", dpi = 500, w = 9, h = 6)


# School contacts plot
ggplot(data_first_and_second_resp %>% filter(!str_detect(name, "diff")) %>% filter(!str_detect(name, "during")) %>% filter(!str_detect(name, "hsld")) %>% filter(!str_detect(name, "work")) %>% filter(!str_detect(name, "leisure"))) + 
  geom_histogram(aes(x = value, y = after_stat(count / sum(count)), fill = time), bins = 10) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))  +
  labs(title=paste0("School Contacts")) 

#ggsave("SchoolFirstSecondComparison.pdf", dpi = 500, w = 9, h = 6)
#ggsave("SchoolFirstSecondComparison.png", dpi = 500, w = 9, h = 6)


# Leisure contacts plot
ggplot(data_first_and_second_resp %>% filter(!str_detect(name, "diff")) %>% filter(!str_detect(name, "during")) %>% filter(!str_detect(name, "hsld")) %>% filter(!str_detect(name, "work")) %>% filter(!str_detect(name, "school"))) + 
  geom_histogram(aes(x = value, y = after_stat(count / sum(count)), fill = time), bins = 10) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))  +
  labs(title=paste0("Leisure Contacts"))

#ggsave("LeisureFirstSecondComparison.pdf", dpi = 500, w = 9, h = 6)
#ggsave("LeisureFirstSecondComparison.png", dpi = 500, w = 9, h = 6)

# Difference in response plot

#TODO: I am filtering for the cases where the abs(difference) < 10 --> I somewhere need to talk about the outliers too

ggplot(data_first_and_second_resp %>% filter(str_detect(name, "diff")) %>% filter(value < 10) %>% filter(value > -10)) + 
  geom_histogram(aes(x = value, y = after_stat(count / sum(count)), fill = time), bins = 10) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))  +
  labs(title=paste0("Differences in response"))

#ggsave("DifferenceFirstSecondComparison.pdf", dpi = 500, w = 18, h = 18)
#ggsave("DifferenceFirstSecondComparison.png", dpi = 500, w = 18, h = 18)