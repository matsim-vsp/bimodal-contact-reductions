library(tidyverse)
library(igraph)
library(gridExtra)
library(ggiraphExtra)

# Author: S. Paltra, contact: paltra@tu-berlin.de

# Aim of this script:
# Clean and prepare data for contact analysis

raw_data <- readRDS(file = "/Users/sydney/Desktop/cleaned_data.rds") #Place to enter the data's path

# Reducing data frame to the variables of interest ------------------------

data_reduced <- raw_data %>% select(gender, date_f1_inf, date_s2_inf, date_t3_inf, num_c19_infs, year_of_birth, cc_change_during_pandemic, total_hsld_size_persons_under_14, number_of_children_under_18,
                                    cond_hbp, cond_diabetes, cond_cardio, cond_resp,
                                    cond_immuno, cond_cancer, cond_post_c19, cond_none,
                                    attitudes_precautions_mar2020_low_infection_risk_perception,                
                                    attitudes_precautions_mar2020_risky_infection_course_assessment,            
                                    attitudes_precautions_mar2020_high_risk_perception,                         
                                    attitudes_precautions_mar2020_avoided_risky_situations,                     
                                    attitudes_precautions_mar2020_aware_distance_rule_effectiveness,         
                                    attitudes_precautions_mar2020_understood_mask_reduces_risk,                
                                    attitudes_precautions_mar2020_followed_measures,                         
                                    attitudes_precautions_mar2020_felt_restricted_by_measures,                  
                                    attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical,
                                    beh_change_start_pandemic_avoid_in_person,                                  
                                    beh_change_start_pandemic_avoid_careless_contacts,                       
                                    beh_change_start_pandemic_contact_cautious_people,                        
                                    beh_change_start_pandemic_avoid_peak_hours,                          
                                    beh_change_start_pandemic_maintain_distance,                                
                                    beh_change_start_pandemic_outdoor_only,                                     
                                    beh_change_start_pandemic_no_visit_high_risk,                               
                                    beh_change_start_pandemic_avoid_busy_places,                               
                                    beh_change_start_pandemic_avoid_public_trans,                               
                                    beh_change_start_pandemic_mask_public_trans,                                
                                    beh_change_start_pandemic_mask_supermarket,                                 
                                    beh_change_start_pandemic_work_from_home,                                  
                                    beh_change_start_pandemic_children_limited_contacts,                       
                                    beh_change_start_pandemic_meet_close_despite_restrict,
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
                                    cc_weekly_cont_during_pandemic_2019_leisure_cont, cc_weekly_cont_during_pandemic_03_2020_leisure_cont, cc_weekly_cont_during_pandemic_summer_2021_leisure_cont, cc_weekly_cont_during_pandemic_01_2023_leisure_cont, 
                                    c19_vaccination_status, c19_vaccination_details_vaccine_dose_1, c19_vaccination_details_vaccine_dose_2, c19_vaccination_details_vaccine_dose_3, c19_vaccination_details_vaccine_dose_4)

## Renaming some of the columns to facilitate analysis
# Renaming done for RESPONDENT
colnames(data_reduced)[which(names(data_reduced) == "cc_change_during_pandemic")] <- "respondent_cc_change"
colnames(data_reduced)[which(names(data_reduced) == "total_hsld_size_persons_under_14")] <- "respondent_hsld_size_persons_under_14"
colnames(data_reduced)[which(names(data_reduced) == "hsld_size_2019_")] <- "respondent_hsld_size_2019"
colnames(data_reduced)[which(names(data_reduced) == "hsld_size_03_2020_")] <- "respondent_hsld_size_03_2020"
colnames(data_reduced)[which(names(data_reduced) == "hsld_size_summer_2021_")] <- "respondent_hsld_size_summer_2021"
colnames(data_reduced)[which(names(data_reduced) == "hsld_size_01_2023_")] <- "respondent_hsld_size_01_2023"
colnames(data_reduced)[which(names(data_reduced) == "wkly_cont_2019_work_uni")] <- "respondent_work_2019"
colnames(data_reduced)[which(names(data_reduced) == "wkly_cont_03_2020_work_uni")] <- "respondent_work_03_2020"
colnames(data_reduced)[which(names(data_reduced) == "wkly_cont_summer_2021_work_uni")] <- "respondent_work_summer_2021"
colnames(data_reduced)[which(names(data_reduced) == "wkly_cont_01_2023_work_uni")] <- "respondent_work_01_2023"
colnames(data_reduced)[which(names(data_reduced) == "wkly_cont_2019_school_kinder")] <- "respondent_school_2019"
colnames(data_reduced)[which(names(data_reduced) == "wkly_cont_03_2020_school_kinder")] <- "respondent_school_03_2020"
colnames(data_reduced)[which(names(data_reduced) == "wkly_cont_summer_2021_school_kinder")] <- "respondent_school_summer_2021"
colnames(data_reduced)[which(names(data_reduced) == "wkly_cont_01_2023_school_kinder")] <- "respondent_school_01_2023"
colnames(data_reduced)[which(names(data_reduced) == "wkly_cont_2019_leisure")] <- "respondent_leisure_2019"
colnames(data_reduced)[which(names(data_reduced) == "wkly_cont_03_2020_leisure")] <- "respondent_leisure_03_2020"
colnames(data_reduced)[which(names(data_reduced) == "wkly_cont_summer_2021_leisure")] <- "respondent_leisure_summer_2021"
colnames(data_reduced)[which(names(data_reduced) == "wkly_cont_01_2023_leisure")] <- "respondent_leisure_01_2023"

data_reduced <- data_reduced %>% mutate(respondent_all_2019 = respondent_hsld_size_2019 + respondent_school_2019 + respondent_work_2019 + respondent_leisure_2019) %>% 
  mutate(respondent_all_03_2020 = respondent_hsld_size_03_2020 + respondent_school_03_2020 + respondent_work_03_2020 + respondent_leisure_03_2020) %>%
  mutate(respondent_all_summer_2021 = respondent_hsld_size_summer_2021 + respondent_school_summer_2021 +respondent_work_summer_2021 + respondent_leisure_summer_2021) %>%
  mutate(respondent_all_01_2023 = respondent_hsld_size_01_2023 + respondent_school_01_2023 +respondent_work_01_2023 + respondent_leisure_01_2023)

#Renaming done for house hold member of respondent
colnames(data_reduced)[which(names(data_reduced) == "hsld_cont__2019_work_uni")] <- "hhmember_work_2019"
colnames(data_reduced)[which(names(data_reduced) == "hsld_cont__03_2020_work_uni")] <- "hhmember_work_03_2020"
colnames(data_reduced)[which(names(data_reduced) == "hsld_cont__summer_2021_work_uni")] <- "hhmember_work_summer_2021"
colnames(data_reduced)[which(names(data_reduced) == "hsld_cont__01_2023_work_uni")] <- "hhmember_work_01_2023"
colnames(data_reduced)[which(names(data_reduced) == "hsld_cont__2019_school_kinder")] <- "hhmember_school_2019"
colnames(data_reduced)[which(names(data_reduced) == "hsld_cont__03_2020_school_kinder")] <- "hhmember_school_03_2020"
colnames(data_reduced)[which(names(data_reduced) == "hsld_cont__summer_2021_school_kinder")] <- "hhmember_school_summer_2021"
colnames(data_reduced)[which(names(data_reduced) == "hsld_cont__01_2023_school_kinder")] <- "hhmember_school_01_2023"
colnames(data_reduced)[which(names(data_reduced) == "hsld_cont__2019_leisure")] <- "hhmember_leisure_2019"
colnames(data_reduced)[which(names(data_reduced) == "hsld_cont__03_2020_leisure")] <- "hhmember_leisure_03_2020"
colnames(data_reduced)[which(names(data_reduced) == "hsld_cont__summer_2021_leisure")] <- "hhmember_leisure_summer_2021"
colnames(data_reduced)[which(names(data_reduced) == "hsld_cont__01_2023_leisure")] <- "hhmember_leisure_01_2023"

data_reduced <- data_reduced %>% mutate(hhmember_all_2019 = respondent_hsld_size_2019 + hhmember_school_2019 + hhmember_work_2019 + hhmember_leisure_2019) %>% 
  mutate(hhmember_all_03_2020 = respondent_hsld_size_03_2020 + hhmember_school_03_2020 + hhmember_work_03_2020 + hhmember_leisure_03_2020) %>%
  mutate(hhmember_all_summer_2021 = respondent_hsld_size_summer_2021 + hhmember_school_summer_2021 + hhmember_work_summer_2021 + hhmember_leisure_summer_2021) %>%
  mutate(hhmember_all_01_2023 = respondent_hsld_size_01_2023 + hhmember_school_01_2023 + hhmember_work_01_2023 + hhmember_leisure_01_2023)


# Renaming done for CC PRE pandemic
colnames(data_reduced)[which(names(data_reduced) == "cc_hsld_size_pre_pandemic_2019_num_hsld_members")] <- "cc_pre_hsld_size_2019"
colnames(data_reduced)[which(names(data_reduced) == "cc_hsld_size_pre_pandemic_03_2020_num_hsld_members")] <- "cc_pre_hsld_size_03_2020"
colnames(data_reduced)[which(names(data_reduced) == "cc_hsld_size_pre_pandemic_summer_2021_num_hsld_members")] <- "cc_pre_hsld_size_summer_2021"
colnames(data_reduced)[which(names(data_reduced) == "cc_hsld_size_pre_pandemic_01_2023_num_hsld_members")] <- "cc_pre_hsld_size_01_2023"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_contacts_2019_work_uni_cont")] <- "cc_pre_work_2019"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_contacts_03_2020_work_uni_cont")] <- "cc_pre_work_03_2020"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_contacts_summer_2021_work_uni_cont")] <- "cc_pre_work_summer_2021"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_contacts_01_2023_work_uni_cont")] <- "cc_pre_work_01_2023"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_contacts_2019_school_kinder_cont")] <- "cc_pre_school_2019"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_contacts_03_2020_school_kinder_cont")] <- "cc_pre_school_03_2020"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_contacts_summer_2021_school_kinder_cont")] <- "cc_pre_school_summer_2021"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_contacts_01_2023_school_kinder_cont")] <- "cc_pre_school_01_2023"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_contacts_2019_leisure_cont")] <- "cc_pre_leisure_2019"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_contacts_03_2020_leisure_cont")] <- "cc_pre_leisure_03_2020"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_contacts_summer_2021_leisure_cont")] <- "cc_pre_leisure_summer_2021"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_contacts_01_2023_leisure_cont")] <- "cc_pre_leisure_01_2023"

data_reduced <- data_reduced %>% mutate(cc_pre_all_2019 = cc_pre_hsld_size_2019 + cc_pre_school_2019 + cc_pre_work_2019 + cc_pre_leisure_2019) %>% 
  mutate(cc_pre_all_03_2020 = cc_pre_hsld_size_03_2020 + cc_pre_school_03_2020 + cc_pre_work_03_2020 + cc_pre_leisure_03_2020) %>%
  mutate(cc_pre_all_summer_2021 = cc_pre_hsld_size_summer_2021 + cc_pre_school_summer_2021 +cc_pre_work_summer_2021 + cc_pre_leisure_summer_2021) %>%
  mutate(cc_pre_all_01_2023 = cc_pre_hsld_size_01_2023 + cc_pre_school_01_2023 +cc_pre_work_01_2023 + cc_pre_leisure_01_2023)

# Renaming done for CC DURING pandemic
colnames(data_reduced)[which(names(data_reduced) == "cc_hsld_size_during_pandemic_2019_num_hsld_members")] <- "cc_during_hsld_size_2019"
colnames(data_reduced)[which(names(data_reduced) == "cc_hsld_size_during_pandemic_03_2020_num_hsld_members")] <- "cc_during_hsld_size_03_2020"
colnames(data_reduced)[which(names(data_reduced) == "cc_hsld_size_during_pandemic_summer_2021_num_hsld_members")] <- "cc_during_hsld_size_summer_2021"
colnames(data_reduced)[which(names(data_reduced) == "cc_hsld_size_during_pandemic_01_2023_num_hsld_members")] <- "cc_during_hsld_size_01_2023"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_cont_during_pandemic_2019_work_uni_cont")] <- "cc_during_work_2019"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_cont_during_pandemic_03_2020_work_uni_cont")] <- "cc_during_work_03_2020"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_cont_during_pandemic_summer_2021_work_uni_cont")] <- "cc_during_work_summer_2021"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_cont_during_pandemic_01_2023_work_uni_cont")] <- "cc_during_work_01_2023"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_cont_during_pandemic_2019_school_kg_cont")] <- "cc_during_school_2019"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_cont_during_pandemic_03_2020_school_kg_cont")] <- "cc_during_school_03_2020"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_cont_during_pandemic_summer_2021_school_kg_cont")] <- "cc_during_school_summer_2021"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_cont_during_pandemic_01_2023_school_kg_cont")] <- "cc_during_school_01_2023"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_cont_during_pandemic_2019_leisure_cont")] <- "cc_during_leisure_2019"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_cont_during_pandemic_03_2020_leisure_cont")] <- "cc_during_leisure_03_2020"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_cont_during_pandemic_summer_2021_leisure_cont")] <- "cc_during_leisure_summer_2021"
colnames(data_reduced)[which(names(data_reduced) == "cc_weekly_cont_during_pandemic_01_2023_leisure_cont")] <- "cc_during_leisure_01_2023"

data_reduced <- data_reduced %>% mutate(cc_during_all_2019 = cc_during_hsld_size_2019 + cc_during_school_2019 + cc_during_work_2019 + cc_during_leisure_2019) %>% 
  mutate(cc_during_all_03_2020 = cc_during_hsld_size_03_2020 + cc_during_school_03_2020 + cc_during_work_03_2020 + cc_during_leisure_03_2020) %>%
  mutate(cc_during_all_summer_2021 = cc_during_hsld_size_summer_2021 + cc_during_school_summer_2021 +cc_during_work_summer_2021 + cc_during_leisure_summer_2021) %>%
  mutate(cc_during_all_01_2023 = cc_during_hsld_size_01_2023 + cc_during_school_01_2023 + cc_during_work_01_2023 + cc_during_leisure_01_2023)

##Adding age brackets
data_reduced <- data_reduced %>% mutate(age_bracket = case_when(year_of_birth <= 1953 ~ "70+",
                                                                year_of_birth <= 1963 ~ "60-70",
                                                                year_of_birth <= 1973 ~ "50-60",
                                                                year_of_birth <= 1983 ~ "40-50",
                                                                year_of_birth <= 1993 ~ "30-40",
                                                                year_of_birth <= 2005 ~ "18-30"))

data_reduced$age_bracket <- factor(data_reduced$age_bracket, levels = c("18-30", "30-40", "40-50", "50-60", "60-70", "70+"))                                                             

#Data Prep Carefulness of Respondents

data_reduced <- data_reduced %>% mutate(attitudes_precautions_mar2020_low_infection_risk_perception = case_when(attitudes_precautions_mar2020_low_infection_risk_perception %in% c("viel weniger", "weniger", "etwas weniger") ~ "Careful",
                                                                                                attitudes_precautions_mar2020_low_infection_risk_perception == "genauso" ~ "Neutral",
                                                                                                attitudes_precautions_mar2020_low_infection_risk_perception %in% c("etwas mehr", "mehr", "viel mehr") ~ "Risky"),                
                        attitudes_precautions_mar2020_risky_infection_course_assessment = case_when(attitudes_precautions_mar2020_risky_infection_course_assessment %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                attitudes_precautions_mar2020_risky_infection_course_assessment == "genauso" ~ "Neutral",
                                                                                                attitudes_precautions_mar2020_risky_infection_course_assessment %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),                   
                        attitudes_precautions_mar2020_high_risk_perception = case_when(attitudes_precautions_mar2020_high_risk_perception %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                attitudes_precautions_mar2020_high_risk_perception == "genauso" ~ "Neutral",
                                                                                                attitudes_precautions_mar2020_high_risk_perception %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),                                
                        attitudes_precautions_mar2020_avoided_risky_situations = case_when(attitudes_precautions_mar2020_avoided_risky_situations %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                attitudes_precautions_mar2020_avoided_risky_situations == "genauso" ~ "Neutral",
                                                                                                attitudes_precautions_mar2020_avoided_risky_situations %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),                            
                        attitudes_precautions_mar2020_aware_distance_rule_effectiveness = case_when(attitudes_precautions_mar2020_aware_distance_rule_effectiveness %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                attitudes_precautions_mar2020_aware_distance_rule_effectiveness == "genauso" ~ "Neutral",
                                                                                                attitudes_precautions_mar2020_aware_distance_rule_effectiveness %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),     
                        attitudes_precautions_mar2020_understood_mask_reduces_risk = case_when(attitudes_precautions_mar2020_understood_mask_reduces_risk %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                attitudes_precautions_mar2020_understood_mask_reduces_risk == "genauso" ~ "Neutral",
                                                                                                attitudes_precautions_mar2020_understood_mask_reduces_risk %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),                 
                        attitudes_precautions_mar2020_followed_measures = case_when(attitudes_precautions_mar2020_followed_measures %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                attitudes_precautions_mar2020_followed_measures == "genauso" ~ "Neutral",
                                                                                                attitudes_precautions_mar2020_followed_measures %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),
                        attitudes_precautions_mar2020_felt_restricted_by_measures = case_when(attitudes_precautions_mar2020_felt_restricted_by_measures %in% c("viel weniger", "weniger", "etwas weniger") ~ "Careful",
                                                                                                attitudes_precautions_mar2020_felt_restricted_by_measures == "genauso" ~ "Neutral",
                                                                                                attitudes_precautions_mar2020_felt_restricted_by_measures %in% c("etwas mehr", "mehr", "viel mehr") ~ "Risky"),                         
                        attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical = case_when(attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical == "genauso" ~ "Neutral",
                                                                                                attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),       
                        beh_change_start_pandemic_avoid_in_person = case_when(beh_change_start_pandemic_avoid_in_person %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                beh_change_start_pandemic_avoid_in_person == "genauso" ~ "Neutral",
                                                                                                beh_change_start_pandemic_avoid_in_person %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),                                         
                        beh_change_start_pandemic_avoid_careless_contacts = case_when(beh_change_start_pandemic_avoid_careless_contacts %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                beh_change_start_pandemic_avoid_careless_contacts == "genauso" ~ "Neutral", 
                                                                                                beh_change_start_pandemic_avoid_careless_contacts %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),                              
                        beh_change_start_pandemic_contact_cautious_people = case_when(beh_change_start_pandemic_contact_cautious_people %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                beh_change_start_pandemic_contact_cautious_people == "genauso" ~ "Neutral",
                                                                                                beh_change_start_pandemic_contact_cautious_people %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),                               
                        beh_change_start_pandemic_avoid_peak_hours = case_when(beh_change_start_pandemic_avoid_peak_hours %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                beh_change_start_pandemic_avoid_peak_hours == "genauso" ~ "Neutral",
                                                                                                beh_change_start_pandemic_avoid_peak_hours %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),                                 
                        beh_change_start_pandemic_maintain_distance = case_when(beh_change_start_pandemic_maintain_distance %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                beh_change_start_pandemic_maintain_distance == "genauso" ~ "Neutral", 
                                                                                                beh_change_start_pandemic_maintain_distance %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),                                       
                        beh_change_start_pandemic_outdoor_only = case_when(beh_change_start_pandemic_outdoor_only %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                beh_change_start_pandemic_outdoor_only == "genauso" ~ "Neutral",
                                                                                                beh_change_start_pandemic_outdoor_only %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),                                            
                        beh_change_start_pandemic_no_visit_high_risk = case_when(beh_change_start_pandemic_no_visit_high_risk %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                beh_change_start_pandemic_no_visit_high_risk == "genauso" ~ "Neutral",
                                                                                                beh_change_start_pandemic_no_visit_high_risk %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),                                      
                        beh_change_start_pandemic_avoid_busy_places = case_when(beh_change_start_pandemic_avoid_busy_places %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                beh_change_start_pandemic_avoid_busy_places == "genauso" ~ "Neutral",
                                                                                                beh_change_start_pandemic_avoid_busy_places %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),                                      
                        beh_change_start_pandemic_avoid_public_trans = case_when(beh_change_start_pandemic_avoid_public_trans %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                beh_change_start_pandemic_avoid_public_trans == "genauso" ~ "Neutral",
                                                                                                beh_change_start_pandemic_avoid_public_trans %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),                                      
                        beh_change_start_pandemic_mask_public_trans = case_when(beh_change_start_pandemic_mask_public_trans %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                beh_change_start_pandemic_mask_public_trans == "genauso" ~ "Neutral",
                                                                                                beh_change_start_pandemic_mask_public_trans %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),                                       
                        beh_change_start_pandemic_mask_supermarket = case_when(beh_change_start_pandemic_mask_supermarket %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                beh_change_start_pandemic_mask_supermarket == "genauso" ~ "Neutral",
                                                                                                beh_change_start_pandemic_mask_supermarket %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),                                        
                        beh_change_start_pandemic_work_from_home = case_when(beh_change_start_pandemic_work_from_home %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                beh_change_start_pandemic_work_from_home == "genauso" ~ "Neutral",
                                                                                                beh_change_start_pandemic_work_from_home %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),                                         
                        beh_change_start_pandemic_children_limited_contacts = case_when(beh_change_start_pandemic_children_limited_contacts %in% c("viel weniger", "weniger", "etwas weniger") ~ "Risky",
                                                                                                beh_change_start_pandemic_children_limited_contacts == "genauso" ~ "Neutral",
                                                                                                beh_change_start_pandemic_children_limited_contacts %in% c("etwas mehr", "mehr", "viel mehr") ~ "Careful"),                              
                        beh_change_start_pandemic_meet_close_despite_restrict = case_when(beh_change_start_pandemic_meet_close_despite_restrict %in% c("viel weniger", "weniger", "etwas weniger") ~ "Careful",
                                                                                                beh_change_start_pandemic_meet_close_despite_restrict == "genauso" ~ "Neutral",
                                                                                                beh_change_start_pandemic_meet_close_despite_restrict %in% c("etwas mehr", "mehr", "viel mehr") ~ "Risky"))

# Convert Responses to Attitude Questions to Integers such that I can compute a "carefulness score"
data_reduced <- data_reduced %>% mutate(attitudes_precautions_mar2020_low_infection_risk_perception_int = case_when(attitudes_precautions_mar2020_low_infection_risk_perception == "Careful" ~ 1,
                                                                                                                attitudes_precautions_mar2020_low_infection_risk_perception == "Neutral" ~ 0,
                                                                                                                attitudes_precautions_mar2020_low_infection_risk_perception == "Risky" ~ -1),
                                         attitudes_precautions_mar2020_risky_infection_course_assessment_int = case_when(attitudes_precautions_mar2020_risky_infection_course_assessment == "Risky" ~ -1,
                                                    attitudes_precautions_mar2020_risky_infection_course_assessment =="Neutral" ~ 0,
                                                    attitudes_precautions_mar2020_risky_infection_course_assessment =="Careful" ~ 1),
                                         attitudes_precautions_mar2020_high_risk_perception_int = case_when(attitudes_precautions_mar2020_high_risk_perception == "Risky" ~ -1,
                                                    attitudes_precautions_mar2020_high_risk_perception == "Neutral" ~ 0,
                                                    attitudes_precautions_mar2020_high_risk_perception == "Careful" ~ 1),
                                         attitudes_precautions_mar2020_avoided_risky_situations_int = case_when(attitudes_precautions_mar2020_avoided_risky_situations == "Risky" ~ -1,
                                                    attitudes_precautions_mar2020_avoided_risky_situations == "Neutral" ~ 0,
                                                    attitudes_precautions_mar2020_avoided_risky_situations == "Careful" ~ 1),
                                         attitudes_precautions_mar2020_aware_distance_rule_effectiveness_int = case_when(attitudes_precautions_mar2020_aware_distance_rule_effectiveness == "Risky" ~ 0,
                                                    attitudes_precautions_mar2020_aware_distance_rule_effectiveness == "Neutral" ~ 0,
                                                    attitudes_precautions_mar2020_aware_distance_rule_effectiveness == "Careful" ~ 1),
                                         attitudes_precautions_mar2020_understood_mask_reduces_risk_int = case_when(attitudes_precautions_mar2020_understood_mask_reduces_risk == "Risky" ~ 0,
                                                    attitudes_precautions_mar2020_understood_mask_reduces_risk == "Neutral" ~ 0,
                                                    attitudes_precautions_mar2020_understood_mask_reduces_risk == "Careful" ~ 1),
                                         attitudes_precautions_mar2020_followed_measures_int = case_when(attitudes_precautions_mar2020_followed_measures == "Risky" ~ 0,
                                                    attitudes_precautions_mar2020_followed_measures == "Neutral" ~ 0,
                                                    attitudes_precautions_mar2020_followed_measures == "Careful" ~ 1),
                                         attitudes_precautions_mar2020_felt_restricted_by_measures_int = case_when(attitudes_precautions_mar2020_felt_restricted_by_measures == "Careful" ~ 1,
                                                    attitudes_precautions_mar2020_felt_restricted_by_measures == "Neutral" ~ 0,
                                                    attitudes_precautions_mar2020_felt_restricted_by_measures == "Risky" ~ -1),
                                         attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical_int = case_when(attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical == "Risky" ~ -1,
                                                    attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical =="Neutral" ~ 0,
                                                    attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical =="Careful" ~ 1),
                                         beh_change_start_pandemic_avoid_in_person_int = case_when(beh_change_start_pandemic_avoid_in_person == "Risky" ~ -1,
                                                    beh_change_start_pandemic_avoid_in_person =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_avoid_in_person =="Careful" ~ 1),
                                         beh_change_start_pandemic_avoid_careless_contacts_int = case_when(beh_change_start_pandemic_avoid_careless_contacts == "Risky" ~ -1,
                                                    beh_change_start_pandemic_avoid_careless_contacts =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_avoid_careless_contacts =="Careful" ~ 1),
                                         beh_change_start_pandemic_contact_cautious_people_int = case_when(beh_change_start_pandemic_contact_cautious_people == "Risky" ~ -1,
                                                    beh_change_start_pandemic_contact_cautious_people =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_contact_cautious_people =="Careful" ~ 1),
                                         beh_change_start_pandemic_avoid_peak_hours_int = case_when(beh_change_start_pandemic_avoid_peak_hours == "Risky" ~ -1,
                                                    beh_change_start_pandemic_avoid_peak_hours =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_avoid_peak_hours =="Careful" ~ 1),
                                         beh_change_start_pandemic_maintain_distance_int = case_when(beh_change_start_pandemic_maintain_distance == "Risky" ~ -1,
                                                    beh_change_start_pandemic_maintain_distance =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_maintain_distance =="Careful" ~ 1),
                                         beh_change_start_pandemic_outdoor_only_int = case_when(beh_change_start_pandemic_outdoor_only == "Risky" ~ -1,
                                                    beh_change_start_pandemic_outdoor_only =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_outdoor_only =="Careful" ~ 1),
                                         beh_change_start_pandemic_no_visit_high_risk_int = case_when(beh_change_start_pandemic_no_visit_high_risk == "Risky" ~ -1,
                                                    beh_change_start_pandemic_no_visit_high_risk =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_no_visit_high_risk =="Careful" ~ 1),
                                         beh_change_start_pandemic_avoid_busy_places_int = case_when(beh_change_start_pandemic_avoid_busy_places == "Risky" ~ -1,
                                                    beh_change_start_pandemic_avoid_busy_places =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_avoid_busy_places =="Careful" ~ 1),
                                         beh_change_start_pandemic_avoid_public_trans_int = case_when(beh_change_start_pandemic_avoid_public_trans == "Risky" ~ -1,
                                                    beh_change_start_pandemic_avoid_public_trans =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_avoid_public_trans =="Careful" ~ 1),
                                         beh_change_start_pandemic_mask_public_trans_int = case_when(beh_change_start_pandemic_mask_public_trans == "Risky" ~ -1,
                                                    beh_change_start_pandemic_mask_public_trans =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_mask_public_trans =="Careful" ~ 1),
                                         beh_change_start_pandemic_mask_supermarket_int = case_when(beh_change_start_pandemic_mask_supermarket == "Risky" ~ -1,
                                                    beh_change_start_pandemic_mask_supermarket =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_mask_supermarket =="Careful" ~ 1),
                                         beh_change_start_pandemic_work_from_home_int = case_when(beh_change_start_pandemic_work_from_home == "Risky" ~ -1,
                                                    beh_change_start_pandemic_work_from_home =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_work_from_home =="Careful" ~ 1),
                                         beh_change_start_pandemic_children_limited_contacts_int = case_when(beh_change_start_pandemic_children_limited_contacts == "Risky" ~ -1,
                                                    beh_change_start_pandemic_children_limited_contacts =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_children_limited_contacts =="Careful" ~ 1),
                                         beh_change_start_pandemic_meet_close_despite_restrict_int = case_when(beh_change_start_pandemic_meet_close_despite_restrict == "Risky" ~ -1,
                                                    beh_change_start_pandemic_meet_close_despite_restrict =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_meet_close_despite_restrict =="Careful" ~ 1))                                                                                                             

data_reduced$attitudes_precautions_mar2020_low_infection_risk_perception_int  <- as.integer(data_reduced$attitudes_precautions_mar2020_low_infection_risk_perception_int)
data_reduced$attitudes_precautions_mar2020_risky_infection_course_assessment_int <- as.integer(data_reduced$attitudes_precautions_mar2020_risky_infection_course_assessment_int)
data_reduced$attitudes_precautions_mar2020_high_risk_perception_int <- as.integer(data_reduced$attitudes_precautions_mar2020_high_risk_perception_int)
data_reduced$attitudes_precautions_mar2020_avoided_risky_situations_int <- as.integer(data_reduced$attitudes_precautions_mar2020_avoided_risky_situations_int)
data_reduced$attitudes_precautions_mar2020_aware_distance_rule_effectiveness_int <- as.integer(data_reduced$attitudes_precautions_mar2020_aware_distance_rule_effectiveness_int)     
data_reduced$attitudes_precautions_mar2020_understood_mask_reduces_risk_int <- as.integer(data_reduced$attitudes_precautions_mar2020_understood_mask_reduces_risk_int)
data_reduced$attitudes_precautions_mar2020_followed_measures_int <- as.integer(data_reduced$attitudes_precautions_mar2020_followed_measures_int)
data_reduced$attitudes_precautions_mar2020_felt_restricted_by_measures_int <- as.integer(data_reduced$attitudes_precautions_mar2020_felt_restricted_by_measures_int)
data_reduced$attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical_int <- as.integer(data_reduced$attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical_int)
data_reduced$beh_change_start_pandemic_avoid_in_person_int <- as.integer(data_reduced$beh_change_start_pandemic_avoid_in_person_int)
data_reduced$beh_change_start_pandemic_avoid_careless_contacts_int <- as.integer(data_reduced$beh_change_start_pandemic_avoid_careless_contacts_int)
data_reduced$beh_change_start_pandemic_contact_cautious_people_int <- as.integer(data_reduced$beh_change_start_pandemic_contact_cautious_people_int)
data_reduced$beh_change_start_pandemic_avoid_peak_hours_int <- as.integer(data_reduced$beh_change_start_pandemic_avoid_peak_hours_int)
data_reduced$beh_change_start_pandemic_maintain_distance_int <- as.integer(data_reduced$beh_change_start_pandemic_maintain_distance_int)
data_reduced$beh_change_start_pandemic_outdoor_only_int <- as.integer(data_reduced$beh_change_start_pandemic_outdoor_only_int)
data_reduced$beh_change_start_pandemic_no_visit_high_risk_int <- as.integer(data_reduced$beh_change_start_pandemic_no_visit_high_risk_int)
data_reduced$beh_change_start_pandemic_avoid_busy_places_int <- as.integer(data_reduced$beh_change_start_pandemic_avoid_busy_places_int)
data_reduced$beh_change_start_pandemic_avoid_public_trans_int <- as.integer(data_reduced$beh_change_start_pandemic_avoid_public_trans_int)
data_reduced$beh_change_start_pandemic_mask_public_trans_int <- as.integer(data_reduced$beh_change_start_pandemic_mask_public_trans_int)
data_reduced$beh_change_start_pandemic_mask_supermarket_int <- as.integer(data_reduced$beh_change_start_pandemic_mask_supermarket_int)
data_reduced$beh_change_start_pandemic_work_from_home_int <- as.integer(data_reduced$beh_change_start_pandemic_work_from_home_int)
data_reduced$beh_change_start_pandemic_children_limited_contacts_int_int <- as.integer(data_reduced$beh_change_start_pandemic_children_limited_contacts_int)
data_reduced$beh_change_start_pandemic_meet_close_despite_restrict_int <- as.integer(data_reduced$beh_change_start_pandemic_meet_close_despite_restrict_int)

data_reduced <- data_reduced %>% mutate(attitudeScore = attitudes_precautions_mar2020_low_infection_risk_perception_int +                
                                    attitudes_precautions_mar2020_risky_infection_course_assessment_int +            
                                    attitudes_precautions_mar2020_high_risk_perception_int +                     
                                    attitudes_precautions_mar2020_avoided_risky_situations_int +                     
                                    attitudes_precautions_mar2020_aware_distance_rule_effectiveness_int +         
                                    attitudes_precautions_mar2020_understood_mask_reduces_risk_int +                
                                    attitudes_precautions_mar2020_followed_measures_int +                  
                                    attitudes_precautions_mar2020_felt_restricted_by_measures_int +                
                                    attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical_int) %>%
                                    mutate(behaviorChangeScore = beh_change_start_pandemic_avoid_in_person_int +
                                    beh_change_start_pandemic_avoid_careless_contacts_int +
                                    beh_change_start_pandemic_contact_cautious_people_int +
                                    beh_change_start_pandemic_avoid_peak_hours_int +
                                    beh_change_start_pandemic_maintain_distance_int +
                                    beh_change_start_pandemic_outdoor_only_int +
                                    beh_change_start_pandemic_no_visit_high_risk_int +
                                    beh_change_start_pandemic_avoid_public_trans_int +
                                    beh_change_start_pandemic_mask_public_trans_int +
                                    beh_change_start_pandemic_mask_supermarket_int +
                                    beh_change_start_pandemic_work_from_home_int +
                                    beh_change_start_pandemic_children_limited_contacts_int +
                                    beh_change_start_pandemic_meet_close_despite_restrict_int)

data_reduced <- data_reduced %>% mutate(RiskyCarefulAtt = case_when(attitudeScore %in% c(-9,-8,-7,-6,-5,-4,-3,-2,-1, 0,1,2,3) ~ "Risky",
                                                                attitudeScore %in% c(4,5,6,7,8,9) ~ "Careful")) %>%
                                mutate(RiskyCarefulBeh = case_when(behaviorChangeScore %in% c(-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1) ~ "Risky",
                                                                    behaviorChangeScore %in% c(14,13,12,11,10,9,8,7,6,5,4,3,2,1) ~ "Careful"))

data_reduced$RiskyCarefulAtt <- factor(data_reduced$RiskyCarefulAtt, levels = c("Risky", "Careful"))
data_reduced$RiskyCarefulBeh <- factor(data_reduced$RiskyCarefulBeh, levels = c("Risky", "Careful"))


#Adding relative no. of contacts

data_reduced <- data_reduced %>% mutate(respondent_work_rel_2019_2020 = (respondent_work_2019-respondent_work_03_2020)/respondent_work_2019*100) %>%
                                  mutate(respondent_work_rel_2019_2021 = (respondent_work_2019-respondent_work_summer_2021)/respondent_work_2019*100) %>%
                                  mutate(respondent_work_rel_2019_2023 = (respondent_work_2019-respondent_work_01_2023)/respondent_work_2019*100) %>%
                                  mutate(respondent_school_rel_2019_2020 = (respondent_school_2019-respondent_school_03_2020)/respondent_school_2019*100) %>%
                                  mutate(respondent_school_rel_2019_2021 = (respondent_school_2019-respondent_school_summer_2021)/respondent_school_2019*100) %>%
                                  mutate(respondent_school_rel_2019_2023 = (respondent_school_2019-respondent_school_01_2023)/respondent_school_2019*100) %>%
                                  mutate(respondent_leisure_rel_2019_2020 = (respondent_leisure_2019-respondent_leisure_03_2020)/respondent_leisure_2019*100) %>%
                                  mutate(respondent_leisure_rel_2019_2021 = (respondent_leisure_2019-respondent_leisure_summer_2021)/respondent_leisure_2019*100) %>%
                                  mutate(respondent_leisure_rel_2019_2023 = (respondent_leisure_2019-respondent_leisure_01_2023)/respondent_leisure_2019*100) %>%
                                  mutate(respondent_all_rel_2019_2020 = (respondent_all_2019-respondent_all_03_2020)/respondent_all_2019*100) %>%
                                  mutate(respondent_all_rel_2019_2021 = (respondent_all_2019-respondent_all_summer_2021)/respondent_all_2019*100) %>%
                                  mutate(respondent_all_rel_2019_2023 = (respondent_all_2019-respondent_all_01_2023)/respondent_all_2019*100)

data_reduced <- data_reduced %>% mutate(hhmember_work_rel_2019_2020 = (hhmember_work_2019-hhmember_work_03_2020)/hhmember_work_2019*100) %>%
                                  mutate(hhmember_work_rel_2019_2021 = (hhmember_work_2019-hhmember_work_summer_2021)/hhmember_work_2019*100) %>%
                                  mutate(hhmember_work_rel_2019_2023 = (hhmember_work_2019-hhmember_work_01_2023)/hhmember_work_2019*100) %>%
                                  mutate(hhmember_school_rel_2019_2020 = (hhmember_school_2019-hhmember_school_03_2020)/hhmember_school_2019*100) %>%
                                  mutate(hhmember_school_rel_2019_2021 = (hhmember_school_2019-hhmember_school_summer_2021)/hhmember_school_2019*100) %>%
                                  mutate(hhmember_school_rel_2019_2023 = (hhmember_school_2019-hhmember_school_01_2023)/hhmember_school_2019*100) %>%
                                  mutate(hhmember_leisure_rel_2019_2020 = (hhmember_leisure_2019-hhmember_leisure_03_2020)/hhmember_leisure_2019*100) %>%
                                  mutate(hhmember_leisure_rel_2019_2021 = (hhmember_leisure_2019-hhmember_leisure_summer_2021)/hhmember_leisure_2019*100) %>%
                                  mutate(hhmember_leisure_rel_2019_2023 = (hhmember_leisure_2019-hhmember_leisure_01_2023)/hhmember_leisure_2019*100) %>%
                                  mutate(hhmember_all_rel_2019_2020 = (hhmember_all_2019-hhmember_all_03_2020)/hhmember_all_2019*100) %>%
                                  mutate(hhmember_all_rel_2019_2021 = (hhmember_all_2019-hhmember_all_summer_2021)/hhmember_all_2019*100) %>%
                                  mutate(hhmember_all_rel_2019_2023 = (hhmember_all_2019-hhmember_all_01_2023)/hhmember_all_2019*100)

data_reduced <- data_reduced %>% mutate(cc_pre_work_rel_2019_2020 = (cc_pre_work_2019-cc_pre_work_03_2020)/cc_pre_work_2019*100) %>%
                                  mutate(cc_pre_work_rel_2019_2021 = (cc_pre_work_2019-cc_pre_work_summer_2021)/cc_pre_work_2019*100) %>%
                                  mutate(cc_pre_work_rel_2019_2023 = (cc_pre_work_2019-cc_pre_work_01_2023)/cc_pre_work_2019*100) %>%
                                  mutate(cc_pre_school_rel_2019_2020 = (cc_pre_school_2019-cc_pre_school_03_2020)/cc_pre_school_2019*100) %>%
                                  mutate(cc_pre_school_rel_2019_2021 = (cc_pre_school_2019-cc_pre_school_summer_2021)/cc_pre_school_2019*100) %>%
                                  mutate(cc_pre_school_rel_2019_2023 = (cc_pre_school_2019-cc_pre_school_01_2023)/cc_pre_school_2019*100) %>%
                                  mutate(cc_pre_leisure_rel_2019_2020 = (cc_pre_leisure_2019-cc_pre_leisure_03_2020)/cc_pre_leisure_2019*100) %>%
                                  mutate(cc_pre_leisure_rel_2019_2021 = (cc_pre_leisure_2019-cc_pre_leisure_summer_2021)/cc_pre_leisure_2019*100) %>%
                                  mutate(cc_pre_leisure_rel_2019_2023 = (cc_pre_leisure_2019-cc_pre_leisure_01_2023)/cc_pre_leisure_2019*100) %>%
                                  mutate(cc_pre_all_rel_2019_2020 = (cc_pre_all_2019-cc_pre_all_03_2020)/cc_pre_all_2019*100) %>%
                                  mutate(cc_pre_all_rel_2019_2021 = (cc_pre_all_2019-cc_pre_all_summer_2021)/cc_pre_all_2019*100) %>%
                                  mutate(cc_pre_all_rel_2019_2023 = (cc_pre_all_2019-cc_pre_all_01_2023)/cc_pre_all_2019*100)

data_reduced <- data_reduced %>% mutate(cc_during_work_rel_2019_2020 = (cc_during_work_2019-cc_during_work_03_2020)/cc_during_work_2019*100) %>%
                                  mutate(cc_during_work_rel_2019_2021 = (cc_during_work_2019-cc_during_work_summer_2021)/cc_during_work_2019*100) %>%
                                  mutate(cc_during_work_rel_2019_2023 = (cc_during_work_2019-cc_during_work_01_2023)/cc_during_work_2019*100) %>%
                                  mutate(cc_during_school_rel_2019_2020 = (cc_during_school_2019-cc_during_school_03_2020)/cc_during_school_2019*100) %>%
                                  mutate(cc_during_school_rel_2019_2021 = (cc_during_school_2019-cc_during_school_summer_2021)/cc_during_school_2019*100) %>%
                                  mutate(cc_during_school_rel_2019_2023 = (cc_during_school_2019-cc_during_school_01_2023)/cc_during_school_2019*100) %>%
                                  mutate(cc_during_leisure_rel_2019_2020 = (cc_during_leisure_2019-cc_during_leisure_03_2020)/cc_during_leisure_2019*100) %>%
                                  mutate(cc_during_leisure_rel_2019_2021 = (cc_during_leisure_2019-cc_during_leisure_summer_2021)/cc_during_leisure_2019*100) %>%
                                  mutate(cc_during_leisure_rel_2019_2023 = (cc_during_leisure_2019-cc_during_leisure_01_2023)/cc_during_leisure_2019*100) %>%
                                  mutate(cc_during_all_rel_2019_2020 = (cc_during_all_2019-cc_during_all_03_2020)/cc_during_all_2019*100) %>%
                                  mutate(cc_during_all_rel_2019_2021 = (cc_during_all_2019-cc_during_all_summer_2021)/cc_during_all_2019*100) %>%
                                  mutate(cc_during_all_rel_2019_2023 = (cc_during_all_2019-cc_during_all_01_2023)/cc_during_all_2019*100)

## Turning data into tidy format (absolute no of contacts)

data_reduced_tidy <- data_reduced %>% select(-c(respondent_hsld_size_persons_under_14, number_of_children_under_18)) %>%
                                      select(-contains("rel")) %>% 
                                      select(-contains("c19_vaccination"))

data_reduced_tidy <- data_reduced_tidy %>% pivot_longer(cols = 49:112)

data_reduced_tidy <- data_reduced_tidy  %>% mutate(time = case_when(str_detect(name, "2019") ~ "2019",
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

data_reduced_tidy$time <- factor(data_reduced_tidy$time, levels = c("2019", "03/2020", "Summer 2021", "01/2023"))
data_reduced_tidy$TypeOfContact <- factor(data_reduced_tidy$TypeOfContact, levels = c("Work", "Leisure", "School", "All"))
data_reduced_tidy$WhoseContacts <- factor(data_reduced_tidy$WhoseContacts, levels = c("Respondent", "Household Member", "Closest Contact (Pre-Covid)", "Closest Contact (During-Covid)"))

## Turning data into tidy format (relative no of contacts)

data_reduced_tidy_rel <- data_reduced %>% select(contains(c("date_f1_inf", "date_s2_inf", "date_t3_inf", "num_c19_infs", "respondent_cc_change", "year", "age_bracket", "cond", "attitude", "beh_change", "rel", "RiskyCarefulAtt")))

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% pivot_longer(cols = 64:111)

data_reduced_tidy_rel <- data_reduced_tidy_rel  %>% mutate(time = case_when(
                                                          str_detect(name, "_rel_2019_2020") ~ "03/2020",
                                                          str_detect(name, "rel_2019_2021") ~ "Summer 2021",
                                                          str_detect(name, "rel_2019_2023") ~ "01/2023")) %>%
                                  mutate(WhoseContacts = case_when(str_detect(name, "respondent") ~ "Respondent",
                                  str_detect(name, "cc_pre") ~ "Closest Contact (Pre-Covid)",
                                  str_detect(name, "cc_during") ~ "Closest Contact (During-Covid)",
                                  str_detect(name, "hhmember") ~ "Household Member")) %>%
                                  mutate(TypeOfContact = case_when(str_detect(name, "work") ~ "Work",
                                  str_detect(name, "school") ~ "School",
                                  str_detect(name, "leisure") ~ "Leisure",
                                  str_detect(name, "all") ~ "All"))

data_reduced_tidy_rel$time <- factor(data_reduced_tidy_rel$time, levels = c("2019", "03/2020", "Summer 2021", "01/2023"))
data_reduced_tidy_rel$TypeOfContact <- factor(data_reduced_tidy_rel$TypeOfContact, levels = c("Work", "Leisure", "School", "All"))
data_reduced_tidy_rel$WhoseContacts <- factor(data_reduced_tidy_rel$WhoseContacts, levels = c("Respondent", "Household Member", "Closest Contact (Pre-Covid)", "Closest Contact (During-Covid)"))

data_reduced_tidy_rel$value[is.nan(data_reduced_tidy_rel$value)] <- -1000
data_reduced_tidy_rel$value[is.na(data_reduced_tidy_rel$value)] <- -1000


### Prep for Correlation Analyis

data_reduced$respondent_work_2019[is.na(data_reduced$respondent_work_2019)] <- -1000
data_reduced$respondent_leisure_2019[is.na(data_reduced$respondent_leisure_2019)] <- -1000
data_reduced$respondent_all_2019[is.na(data_reduced$respondent_all_2019)] <- -1000
data_reduced$cc_pre_work_2019[is.na(data_reduced$cc_pre_work_2019)] <- -1000
data_reduced$cc_pre_leisure_2019[is.na(data_reduced$cc_pre_leisure_2019)] <- -1000
data_reduced$cc_pre_all_2019[is.na(data_reduced$cc_pre_all_2019)] <- -1000

data_reduced$respondent_work_03_2020[is.na(data_reduced$respondent_work_03_2020)] <- -1000
data_reduced$respondent_leisure_03_2020[is.na(data_reduced$respondent_leisure_03_2020)] <- -1000
data_reduced$respondent_all_03_2020[is.na(data_reduced$respondent_all_03_2020)] <- -1000
data_reduced$cc_pre_work_03_2020[is.na(data_reduced$cc_pre_work_03_2020)] <- -1000
data_reduced$cc_pre_leisure_03_2020[is.na(data_reduced$cc_pre_leisure_03_2020)] <- -1000
data_reduced$cc_pre_all_03_2020[is.na(data_reduced$cc_pre_all_03_2020)] <- -1000

data_reduced$respondent_work_rel_2019_2020[is.na(data_reduced$respondent_work_rel_2019_2020)] <- -1000
data_reduced$respondent_leisure_rel_2019_2020[is.na(data_reduced$respondent_leisure_rel_2019_2020)] <- -1000
data_reduced$respondent_all_rel_2019_2020[is.na(data_reduced$respondent_all_rel_2019_2020)] <- -1000
data_reduced$cc_pre_work_rel_2019_2020[is.na(data_reduced$cc_pre_work_rel_2019_2020)] <- -1000
data_reduced$cc_pre_leisure_rel_2019_2020[is.na(data_reduced$cc_pre_leisure_rel_2019_2020)] <- -1000
data_reduced$cc_pre_all_rel_2019_2020[is.na(data_reduced$cc_pre_all_rel_2019_2020)] <- -1000

data_reduced$respondent_work_summer_2021[is.na(data_reduced$respondent_work_summer_2021)] <- -1000
data_reduced$respondent_leisure_summer_2021[is.na(data_reduced$respondent_leisure_summer_2021)] <- -1000
data_reduced$respondent_all_summer_2021[is.na(data_reduced$respondent_all_summer_2021)] <- -1000
data_reduced$cc_pre_work_summer_2021[is.na(data_reduced$cc_pre_work_summer_2021)] <- -1000
data_reduced$cc_pre_leisure_summer_2021[is.na(data_reduced$cc_pre_leisure_summer_2021)] <- -1000
data_reduced$cc_pre_all_summer_2021[is.na(data_reduced$cc_pre_all_summer_2021)] <- -1000

data_reduced$respondent_work_rel_2019_2021[is.na(data_reduced$respondent_work_rel_2019_2021)] <- -1000
data_reduced$respondent_leisure_rel_2019_2021[is.na(data_reduced$respondent_leisure_rel_2019_2021)] <- -1000
data_reduced$respondent_all_rel_2019_2021[is.na(data_reduced$respondent_all_rel_2019_2021)] <- -1000
data_reduced$cc_pre_work_rel_2019_2021[is.na(data_reduced$cc_pre_work_rel_2019_2021)] <- -1000
data_reduced$cc_pre_leisure_rel_2019_2021[is.na(data_reduced$cc_pre_leisure_rel_2019_2021)] <- -1000
data_reduced$cc_pre_all_rel_2019_2021[is.na(data_reduced$cc_pre_all_rel_2019_2021)] <- -1000

data_reduced$respondent_work_01_2023[is.na(data_reduced$respondent_work_01_2023)] <- -1000
data_reduced$respondent_leisure_01_2023[is.na(data_reduced$respondent_leisure_01_2023)] <- -1000
data_reduced$respondent_all_01_2023[is.na(data_reduced$respondent_all_01_2023)] <- -1000
data_reduced$cc_pre_work_01_2023[is.na(data_reduced$cc_pre_work_01_2023)] <- -1000
data_reduced$cc_pre_leisure_01_2023[is.na(data_reduced$cc_pre_leisure_01_2023)] <- -1000
data_reduced$cc_pre_all_01_2023[is.na(data_reduced$cc_pre_all_01_2023)] <- -1000

data_reduced$respondent_work_rel_2019_2023[is.na(data_reduced$respondent_work_rel_2019_2023)] <- -1000
data_reduced$respondent_leisure_rel_2019_2023[is.na(data_reduced$respondent_leisure_rel_2019_2023)] <- -1000
data_reduced$respondent_all_rel_2019_2023[is.na(data_reduced$respondent_all_rel_2019_2023)] <- -1000
data_reduced$cc_pre_work_rel_2019_2023[is.na(data_reduced$cc_pre_work_rel_2019_2023)] <- -1000
data_reduced$cc_pre_leisure_rel_2019_2023[is.na(data_reduced$cc_pre_leisure_rel_2019_2023)] <- -1000
data_reduced$cc_pre_all_rel_2019_2023[is.na(data_reduced$cc_pre_all_rel_2019_2023)] <- -1000

