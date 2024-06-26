library(tidyverse)
library(igraph)
library(gridExtra)
library(ggiraphExtra)

# Author: S. Paltra, contact: paltra@tu-berlin.de

# Aim of this script:
# Clean and prepare data for contact analysis

raw_data <- read_csv("/Users/sydney/Downloads/twitter_data.csv") #Place to enter the data's path

# Reducing data frame to the variables of interest ------------------------

data_reduced <- raw_data %>% select(user_id, ref, cc_change_during_pandemic, total_hsld_size_persons_under_14, number_of_children_under_18_time,
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
                                    beh_change_start_pandemic_meet_close_despite_restrict)

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
