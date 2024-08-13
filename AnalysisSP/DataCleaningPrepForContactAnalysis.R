library(tidyverse)
library(igraph)
library(gridExtra)
library(ggiraphExtra)

# Author: S. Paltra, contact: paltra@tu-berlin.de

# Aim of this script:
# Clean and prepare data for contact analysis

raw_data <- readRDS(file = "/Users/sydney/Desktop/cleaned_data.rds") #Place to enter the data's path

# Reducing data frame to the variables of interest ------------------------

data_reduced <- raw_data %>% select(date_f1_inf, date_s2_inf, date_t3_inf, num_c19_infs, year_of_birth, cc_change_during_pandemic, total_hsld_size_persons_under_14, number_of_children_under_18,
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
                                    cc_weekly_cont_during_pandemic_2019_leisure_cont, cc_weekly_cont_during_pandemic_03_2020_leisure_cont, cc_weekly_cont_during_pandemic_summer_2021_leisure_cont, cc_weekly_cont_during_pandemic_01_2023_leisure_cont)

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

#Adding relative no. of contacts

data_reduced <- data_reduced %>% mutate(respondent_work_rel_2019_2020 = 100/respondent_work_2019*respondent_work_03_2020) %>%
                                  mutate(respondent_work_rel_2019_2021 = 100/respondent_work_2019*respondent_work_summer_2021) %>%
                                  mutate(respondent_work_rel_2019_2023 = 100/respondent_work_2019*respondent_work_01_2023) %>%
                                  mutate(respondent_school_rel_2019_2020 = 100/respondent_school_2019*respondent_school_03_2020) %>%
                                  mutate(respondent_school_rel_2019_2021 = 100/respondent_school_2019*respondent_school_summer_2021) %>%
                                  mutate(respondent_school_rel_2019_2023 = 100/respondent_school_2019*respondent_school_01_2023) %>%
                                  mutate(respondent_leisure_rel_2019_2020 = 100/respondent_leisure_2019*respondent_leisure_03_2020) %>%
                                  mutate(respondent_leisure_rel_2019_2021 = 100/respondent_leisure_2019*respondent_leisure_summer_2021) %>%
                                  mutate(respondent_leisure_rel_2019_2023 = 100/respondent_leisure_2019*respondent_leisure_01_2023) %>%
                                  mutate(respondent_all_rel_2019_2020 = 100/respondent_all_2019*respondent_all_03_2020) %>%
                                  mutate(respondent_all_rel_2019_2021 = 100/respondent_all_2019*respondent_all_summer_2021) %>%
                                  mutate(respondent_all_rel_2019_2023 = 100/respondent_all_2019*respondent_all_01_2023)

data_reduced <- data_reduced %>% mutate(hhmember_work_rel_2019_2020 = 100/hhmember_work_2019*hhmember_work_03_2020) %>%
                                  mutate(hhmember_work_rel_2019_2021 = 100/hhmember_work_2019*hhmember_work_summer_2021) %>%
                                  mutate(hhmember_work_rel_2019_2023 = 100/hhmember_work_2019*hhmember_work_01_2023) %>%
                                  mutate(hhmember_school_rel_2019_2020 = 100/hhmember_school_2019*hhmember_school_03_2020) %>%
                                  mutate(hhmember_school_rel_2019_2021 = 100/hhmember_school_2019*hhmember_school_summer_2021) %>%
                                  mutate(hhmember_school_rel_2019_2023 = 100/hhmember_school_2019*hhmember_school_01_2023) %>%
                                  mutate(hhmember_leisure_rel_2019_2020 = 100/hhmember_leisure_2019*hhmember_leisure_03_2020) %>%
                                  mutate(hhmember_leisure_rel_2019_2021 = 100/hhmember_leisure_2019*hhmember_leisure_summer_2021) %>%
                                  mutate(hhmember_leisure_rel_2019_2023 = 100/hhmember_leisure_2019*hhmember_leisure_01_2023) %>%
                                  mutate(hhmember_all_rel_2019_2020 = 100/hhmember_all_2019*hhmember_all_03_2020) %>%
                                  mutate(hhmember_all_rel_2019_2021 = 100/hhmember_all_2019*hhmember_all_summer_2021) %>%
                                  mutate(hhmember_all_rel_2019_2023 = 100/hhmember_all_2019*hhmember_all_01_2023)

data_reduced <- data_reduced %>% mutate(cc_pre_work_rel_2019_2020 = 100/cc_pre_work_2019*cc_pre_work_03_2020) %>%
                                  mutate(cc_pre_work_rel_2019_2021 = 100/cc_pre_work_2019*cc_pre_work_summer_2021) %>%
                                  mutate(cc_pre_work_rel_2019_2023 = 100/cc_pre_work_2019*cc_pre_work_01_2023) %>%
                                  mutate(cc_pre_school_rel_2019_2020 = 100/cc_pre_school_2019*cc_pre_school_03_2020) %>%
                                  mutate(cc_pre_school_rel_2019_2021 = 100/cc_pre_school_2019*cc_pre_school_summer_2021) %>%
                                  mutate(cc_pre_school_rel_2019_2023 = 100/cc_pre_school_2019*cc_pre_school_01_2023) %>%
                                  mutate(cc_pre_leisure_rel_2019_2020 = 100/cc_pre_leisure_2019*cc_pre_leisure_03_2020) %>%
                                  mutate(cc_pre_leisure_rel_2019_2021 = 100/cc_pre_leisure_2019*cc_pre_leisure_summer_2021) %>%
                                  mutate(cc_pre_leisure_rel_2019_2023 = 100/cc_pre_leisure_2019*cc_pre_leisure_01_2023) %>%
                                  mutate(cc_pre_all_rel_2019_2020 = 100/cc_pre_all_2019*cc_pre_all_03_2020) %>%
                                  mutate(cc_pre_all_rel_2019_2021 = 100/cc_pre_all_2019*cc_pre_all_summer_2021) %>%
                                  mutate(cc_pre_all_rel_2019_2023 = 100/cc_pre_all_2019*cc_pre_all_01_2023)

data_reduced <- data_reduced %>% mutate(cc_during_work_rel_2019_2020 = 100/cc_during_work_2019*cc_during_work_03_2020) %>%
                                  mutate(cc_during_work_rel_2019_2021 = 100/cc_during_work_2019*cc_during_work_summer_2021) %>%
                                  mutate(cc_during_work_rel_2019_2023 = 100/cc_during_work_2019*cc_during_work_01_2023) %>%
                                  mutate(cc_during_school_rel_2019_2020 = 100/cc_during_school_2019*cc_during_school_03_2020) %>%
                                  mutate(cc_during_school_rel_2019_2021 = 100/cc_during_school_2019*cc_during_school_summer_2021) %>%
                                  mutate(cc_during_school_rel_2019_2023 = 100/cc_during_school_2019*cc_during_school_01_2023) %>%
                                  mutate(cc_during_leisure_rel_2019_2020 = 100/cc_during_leisure_2019*cc_during_leisure_03_2020) %>%
                                  mutate(cc_during_leisure_rel_2019_2021 = 100/cc_during_leisure_2019*cc_during_leisure_summer_2021) %>%
                                  mutate(cc_during_leisure_rel_2019_2023 = 100/cc_during_leisure_2019*cc_during_leisure_01_2023) %>%
                                  mutate(cc_during_all_rel_2019_2020 = 100/cc_during_all_2019*cc_during_all_03_2020) %>%
                                  mutate(cc_during_all_rel_2019_2021 = 100/cc_during_all_2019*cc_during_all_summer_2021) %>%
                                  mutate(cc_during_all_rel_2019_2023 = 100/cc_during_all_2019*cc_during_all_01_2023)

## Turning data into tidy format (absolute no of contacts)

data_reduced_tidy <- data_reduced %>% select(-c(respondent_hsld_size_persons_under_14, number_of_children_under_18)) %>%
                                      select(-contains("rel"))

data_reduced_tidy <- data_reduced_tidy %>% pivot_longer(cols = 37:112)

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

data_reduced_tidy_rel <- data_reduced %>% select(contains(c("date_f1_inf", "date_s2_inf", "date_t3_inf", "num_c19_infs", "respondent_cc_change", "year", "age_bracket", "cond", "attitude", "beh_change", "rel")))

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% pivot_longer(cols = 39:86)

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

