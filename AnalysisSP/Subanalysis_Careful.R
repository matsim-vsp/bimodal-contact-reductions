
library(tidyverse)
library(MMWRweek)
library(see)
library(RColorBrewer)
library(patchwork)

# Author: S. Paltra, contact: paltra@tu-berlin.de

#In the first part of this script we compare the incidence of the respondents of the survey to the official incidence reported by RKI

#raw_data <- readRDS(file = "/Users/sydney/Desktop/cleaned_data.rds")

source("DataCleaningPrepForContactAnalysis.R")

# Did careful attitudes/behaviors result in less/later infections? --------

# data_reduced <- raw_data %>% select(num_c19_infs, date_f1_inf, date_s2_inf, date_t3_inf, cc_change_during_pandemic,
#                                     attitudes_precautions_mar2020_low_infection_risk_perception,                
#                                     attitudes_precautions_mar2020_risky_infection_course_assessment,            
#                                     attitudes_precautions_mar2020_high_risk_perception,                         
#                                     attitudes_precautions_mar2020_avoided_risky_situations,                     
#                                     attitudes_precautions_mar2020_aware_distance_rule_effectiveness,         
#                                     attitudes_precautions_mar2020_understood_mask_reduces_risk,                
#                                     attitudes_precautions_mar2020_followed_measures,                         
#                                     attitudes_precautions_mar2020_felt_restricted_by_measures,                  
#                                     attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical,
#                                     beh_change_start_pandemic_avoid_in_person,                                  
#                                     beh_change_start_pandemic_avoid_careless_contacts,                       
#                                     beh_change_start_pandemic_contact_cautious_people,                        
#                                     beh_change_start_pandemic_avoid_peak_hours,                          
#                                     beh_change_start_pandemic_maintain_distance,                                
#                                     beh_change_start_pandemic_outdoor_only,                                     
#                                     beh_change_start_pandemic_no_visit_high_risk,                               
#                                     beh_change_start_pandemic_avoid_busy_places,                               
#                                     beh_change_start_pandemic_avoid_public_trans,                               
#                                     beh_change_start_pandemic_mask_public_trans,                                
#                                     beh_change_start_pandemic_mask_supermarket,                                 
#                                     beh_change_start_pandemic_work_from_home,                                  
#                                     beh_change_start_pandemic_children_limited_contacts,                       
#                                     beh_change_start_pandemic_meet_close_despite_restrict)

attitudesAndBehaviors <- c("attitudes_precautions_mar2020_low_infection_risk_perception",                
                                    "attitudes_precautions_mar2020_risky_infection_course_assessment",            
                                    "attitudes_precautions_mar2020_high_risk_perception",                         
                                    "attitudes_precautions_mar2020_avoided_risky_situations",                     
                                    "attitudes_precautions_mar2020_aware_distance_rule_effectiveness",         
                                    "attitudes_precautions_mar2020_understood_mask_reduces_risk",                
                                    "attitudes_precautions_mar2020_followed_measures",                         
                                    "attitudes_precautions_mar2020_felt_restricted_by_measures",                  
                                    "attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical",
                                    "beh_change_start_pandemic_avoid_in_person",                                  
                                    "beh_change_start_pandemic_avoid_careless_contacts",                       
                                    "beh_change_start_pandemic_contact_cautious_people",                        
                                    "beh_change_start_pandemic_avoid_peak_hours",                          
                                    "beh_change_start_pandemic_maintain_distance",                                
                                    "beh_change_start_pandemic_outdoor_only",                                     
                                    "beh_change_start_pandemic_no_visit_high_risk",                               
                                    "beh_change_start_pandemic_avoid_busy_places",                               
                                    "beh_change_start_pandemic_avoid_public_trans",                               
                                    "beh_change_start_pandemic_mask_public_trans",                                
                                    "beh_change_start_pandemic_mask_supermarket",                                 
                                    "beh_change_start_pandemic_work_from_home",                                  
                                    "beh_change_start_pandemic_children_limited_contacts",                       
                                    "beh_change_start_pandemic_meet_close_despite_restrict")

data_reduced <- data_reduced %>% select(-contains("cond")) %>% 
                select(-respondent_hsld_size_persons_under_14) %>% 
                select(-number_of_children_under_18)

palette <- function() {
  c("#fd5901", "#008083")
}

data_reduced$respondent_cc_change <- factor(data_reduced$respondent_cc_change, levels = c("Nein", "Ja"))

for(attBeh in attitudesAndBehaviors){
data_reduced %>% filter(!is.na(cc_change_during_pandemic)) %>% filter(!!sym(attBeh) != "trifft nicht zu") %>%
filter(!!sym(attBeh) != "keine Angabe") %>%
  count(!!sym(attBeh), cc_change_during_pandemic) %>%
  mutate(n = n / sum(n), .by = !!sym(attBeh)) %>% ungroup() %>%
  filter(!is.na(!!sym(attBeh))) %>% 
  ggplot(aes(!!sym(attBeh), n, fill = cc_change_during_pandemic)) +
  #geom_col(position = position_dodge(preserve = 'single')) +
  geom_col(position = "fill") +
  ggtitle(attBeh) +
  theme_minimal() +
  ylab("Relative Frequency") +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = palette()) +
  #theme(legend.title=element_blank()) +
  theme(panel.spacing.x = unit(2, "lines")) +
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) 
  facet_wrap(~ cc_change_during_pandemic)

ggsave(paste0(attBeh, "_changeCC-fullResponses.pdf"), dpi = 500, w = 12, h = 9)
ggsave(paste0(attBeh, "_changeCC-fullResponses.png"), dpi = 500, w = 12, h = 9)
}

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

#data_reduced <- data_reduced %>% mutate(cc_change_during_pandemic = case_when(cc_change_during_pandemic == "Ja" ~ "Changed CC",
                                                                             #   cc_change_during_pandemic == "Nein" ~ "Did Not Change CC"))

palette <- function() {
  c("#fd5901", "#008083")
}

for(attBeh in attitudesAndBehaviors){
data_reduced %>% filter(!is.na(cc_change_during_pandemic)) %>% filter(!!sym(attBeh) != "trifft nicht zu") %>%
filter(!!sym(attBeh) != "keine Angabe") %>%
  count(!!sym(attBeh) , cc_change_during_pandemic) %>%
  mutate(n = n / sum(n), .by = !!sym(attBeh)) %>%
  filter(!is.na(!!sym(attBeh))) %>% 
  ggplot(aes(!!sym(attBeh), n, fill = cc_change_during_pandemic)) +
  #geom_col(position = position_dodge(preserve = 'single')) +
  geom_col(position = "fill") +
  theme_minimal() +
  ggtitle(attBeh) +
  #facet_wrap(~cc_change_during_pandemic)
  theme(panel.spacing.x = unit(2, "lines")) +
  ylab("Relative Frequency") +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = palette()) +
  #theme(legend.title=element_blank()) +
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1))

ggsave(paste0(attBeh, "_changeCC.pdf"), dpi = 500, w = 9, h = 9)
ggsave(paste0(attBeh, "_changeCC.png"), dpi = 500, w = 9, h = 9)
}

comparedToAverage <- c("Risky", "Careful", "Neutral")

data_reduced <- data_reduced %>% mutate(date_f1_inf = case_when(is.na(date_f1_inf) ~ as.Date("3000-01-01"),
                                        .default = as.Date(as.character(date_f1_inf)))) %>%
                                filter(date_f1_inf != as.Date("1922-03-01")) %>%
                                filter(date_f1_inf != as.Date("1965-06-12")) %>%
                                filter(date_f1_inf != as.Date("2000-12-13")) %>%
                                filter(date_f1_inf != as.Date("2019-12-21")) 

summaryStats <- data.frame(matrix(nrow = 0, ncol = 10))
colnames(summaryStats) <- c("attitude", "average", "min", "firstquartile", "median", "mean", "thirdquartile", "max", "numbersanswers", "numbernoinfections")
class(summaryStats$min) <- "Date"
class(summaryStats$firstquartile) <- "Date"
class(summaryStats$median) <- "Date"
class(summaryStats$mean) <- "Date"
class(summaryStats$thirdquartile) <- "Date"
class(summaryStats$max) <- "Date"
for(attBeh in attitudesAndBehaviors){
    for(avg in comparedToAverage){
    data_reduced_filtered <- data_reduced %>% filter(!!sym(attBeh) == avg)
    data_reduced_noInf <- data_reduced_filtered %>% filter(num_c19_infs == "Nie")
    data_reduced_noInf <- nrow(data_reduced_noInf)
    summaryStats[nrow(summaryStats) + 1, 1] <- attBeh
    summaryStats[nrow(summaryStats), 2] <- avg
    summaryStats[nrow(summaryStats), 3] <- min(reduced_data_filtered$date_f1_inf, na.rm=TRUE)
    summaryStats[nrow(summaryStats), 4] <- unname(quantile(reduced_data_filtered$date_f1_inf, type=1, na.rm=TRUE))[2]
    summaryStats[nrow(summaryStats), 5] <- unname(quantile(reduced_data_filtered$date_f1_inf, type=1, na.rm=TRUE))[3]
    summaryStats[nrow(summaryStats), 6] <- mean(reduced_data_filtered$date_f1_inf, na.rm=TRUE)
    summaryStats[nrow(summaryStats), 7] <- unname(quantile(reduced_data_filtered$date_f1_inf, type=1, na.rm=TRUE))[4]
    summaryStats[nrow(summaryStats), 8] <- max(reduced_data_filtered$date_f1_inf, na.rm=TRUE)
    summaryStats[nrow(summaryStats), 9] <- length(reduced_data_filtered$date_f1_inf)
    summaryStats[nrow(summaryStats), 10] <- reduced_data_noInf
    }
}

palette <- function() {
  c("#008083", "#FFBC42", "#fd5901")
}

data_reduced <- data_reduced %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "Never",
                                                                    num_c19_infs == "Einmal" ~ "Once",
                                                                    num_c19_infs == "Zweimal" ~ "Twice",
                                                                    num_c19_infs == "Dreimal" ~ "Three Times",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "More Than Three Times",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

data_reduced$num_c19_infs_eng <- factor(data_reduced$num_c19_infs_eng, levels = c("Never", "Once", "Twice", "Three Times", "More Than Three Times", "I Don't Want To Answer"))


for(attBeh in attitudesAndBehaviors){
ggplot(data_reduced %>% filter(!is.na(!!sym(attBeh))), aes(date_f1_inf, color = !!sym(attBeh))) +
stat_ecdf(geom="smooth", size = 2) +
theme_minimal() +
ggtitle(attBeh) +
ylab("Empirical Cumulative \n Distribution Function") +
xlab("Date Of 1st Infection") +
scale_color_manual(values = palette()) +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01"))) +
theme(text = element_text(size = 22)) +
theme(legend.title = element_blank(), legend.position = "bottom") +
guides(color = guide_legend(nrow=2))
ggsave(paste0(attBeh, "ECDF.pdf"), dpi = 500, w = 9, h = 9)
ggsave(paste0(attBeh, "ECDF.png"), dpi = 500, w = 9, h = 9)

data_reduced %>%
  count(!!sym(attBeh), num_c19_infs_eng) %>%
  mutate(n = n / sum(n), .by = !!sym(attBeh)) %>%
  filter(!is.na(!!sym(attBeh))) %>% 
  ggplot(aes(num_c19_infs_eng, n, fill = !!sym(attBeh))) +
  geom_col(position = position_dodge(preserve = 'single')) +
  theme_minimal() +
  ggtitle(attBeh) +
  ylab("Relative Frequency") +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom") +
  labs(fill="Attitude/Beh. Change") +
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette())

ggsave(paste0(attBeh, "NumberOfInfections.pdf"), dpi = 500, w = 9, h = 12)
ggsave(paste0(attBeh, "NumberOfInfection.png"), dpi = 500, w = 9, h = 12)
}

# Convert Responses to Attitude Questions to Integers such that I can compute a "carefulness score"
data_reduced <- data_reduced %>% mutate(attitudes_precautions_mar2020_low_infection_risk_perception = case_when(attitudes_precautions_mar2020_low_infection_risk_perception == "Careful" ~ 1,
                                                                                                                attitudes_precautions_mar2020_low_infection_risk_perception == "Neutral" ~ 0,
                                                                                                                attitudes_precautions_mar2020_low_infection_risk_perception == "Risky" ~ -1),
                                         attitudes_precautions_mar2020_risky_infection_course_assessment = case_when(attitudes_precautions_mar2020_risky_infection_course_assessment == "Risky" ~ -1,
                                                    attitudes_precautions_mar2020_risky_infection_course_assessment =="Neutral" ~ 0,
                                                    attitudes_precautions_mar2020_risky_infection_course_assessment =="Careful" ~ 1),
                                         attitudes_precautions_mar2020_high_risk_perception = case_when(attitudes_precautions_mar2020_high_risk_perception == "Risky" ~ -1,
                                                    attitudes_precautions_mar2020_high_risk_perception == "Neutral" ~ 0,
                                                    attitudes_precautions_mar2020_high_risk_perception == "Careful" ~ 1),
                                         attitudes_precautions_mar2020_avoided_risky_situations = case_when(attitudes_precautions_mar2020_avoided_risky_situations == "Risky" ~ -1,
                                                    attitudes_precautions_mar2020_avoided_risky_situations == "Neutral" ~ 0,
                                                    attitudes_precautions_mar2020_avoided_risky_situations == "Careful" ~ 1),
                                         attitudes_precautions_mar2020_aware_distance_rule_effectiveness = case_when(attitudes_precautions_mar2020_aware_distance_rule_effectiveness == "Risky" ~ 0,
                                                    attitudes_precautions_mar2020_aware_distance_rule_effectiveness == "Neutral" ~ 0,
                                                    attitudes_precautions_mar2020_aware_distance_rule_effectiveness == "Careful" ~ 1),
                                         attitudes_precautions_mar2020_understood_mask_reduces_risk = case_when(attitudes_precautions_mar2020_understood_mask_reduces_risk == "Risky" ~ 0,
                                                    attitudes_precautions_mar2020_understood_mask_reduces_risk == "Neutral" ~ 0,
                                                    attitudes_precautions_mar2020_understood_mask_reduces_risk == "Careful" ~ 1),
                                         attitudes_precautions_mar2020_followed_measures = case_when(attitudes_precautions_mar2020_followed_measures == "Risky" ~ 0,
                                                    attitudes_precautions_mar2020_followed_measures == "Neutral" ~ 0,
                                                    attitudes_precautions_mar2020_followed_measures == "Careful" ~ 1),
                                         attitudes_precautions_mar2020_felt_restricted_by_measures = case_when(attitudes_precautions_mar2020_felt_restricted_by_measures == "Careful" ~ 1,
                                                    attitudes_precautions_mar2020_felt_restricted_by_measures == "Neutral" ~ 0,
                                                    attitudes_precautions_mar2020_felt_restricted_by_measures == "Risky" ~ -1),
                                         attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical = case_when(attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical == "Risky" ~ -1,
                                                    attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical =="Neutral" ~ 0,
                                                    attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical =="Careful" ~ 1),
                                         beh_change_start_pandemic_avoid_in_person = case_when(beh_change_start_pandemic_avoid_in_person == "Risky" ~ -1,
                                                    beh_change_start_pandemic_avoid_in_person =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_avoid_in_person =="Careful" ~ 1),
                                         beh_change_start_pandemic_avoid_careless_contacts = case_when(beh_change_start_pandemic_avoid_careless_contacts == "Risky" ~ -1,
                                                    beh_change_start_pandemic_avoid_careless_contacts =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_avoid_careless_contacts =="Careful" ~ 1),
                                         beh_change_start_pandemic_contact_cautious_people = case_when(beh_change_start_pandemic_contact_cautious_people == "Risky" ~ -1,
                                                    beh_change_start_pandemic_contact_cautious_people =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_contact_cautious_people =="Careful" ~ 1),
                                         beh_change_start_pandemic_avoid_peak_hours = case_when(beh_change_start_pandemic_avoid_peak_hours == "Risky" ~ -1,
                                                    beh_change_start_pandemic_avoid_peak_hours =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_avoid_peak_hours =="Careful" ~ 1),
                                         beh_change_start_pandemic_maintain_distance = case_when(beh_change_start_pandemic_maintain_distance == "Risky" ~ -1,
                                                    beh_change_start_pandemic_maintain_distance =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_maintain_distance =="Careful" ~ 1),
                                         beh_change_start_pandemic_outdoor_only = case_when(beh_change_start_pandemic_outdoor_only == "Risky" ~ -1,
                                                    beh_change_start_pandemic_outdoor_only =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_outdoor_only =="Careful" ~ 1),
                                         beh_change_start_pandemic_no_visit_high_risk = case_when(beh_change_start_pandemic_no_visit_high_risk == "Risky" ~ -1,
                                                    beh_change_start_pandemic_no_visit_high_risk =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_no_visit_high_risk =="Careful" ~ 1),
                                         beh_change_start_pandemic_avoid_busy_places = case_when(beh_change_start_pandemic_avoid_busy_places == "Risky" ~ -1,
                                                    beh_change_start_pandemic_avoid_busy_places =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_avoid_busy_places =="Careful" ~ 1),
                                         beh_change_start_pandemic_avoid_public_trans = case_when(beh_change_start_pandemic_avoid_public_trans == "Risky" ~ -1,
                                                    beh_change_start_pandemic_avoid_public_trans =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_avoid_public_trans =="Careful" ~ 1),
                                         beh_change_start_pandemic_mask_public_trans = case_when(beh_change_start_pandemic_mask_public_trans == "Risky" ~ -1,
                                                    beh_change_start_pandemic_mask_public_trans =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_mask_public_trans =="Careful" ~ 1),
                                         beh_change_start_pandemic_mask_supermarket = case_when(beh_change_start_pandemic_mask_supermarket == "Risky" ~ -1,
                                                    beh_change_start_pandemic_mask_supermarket =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_mask_supermarket =="Careful" ~ 1),
                                         beh_change_start_pandemic_work_from_home = case_when(beh_change_start_pandemic_work_from_home == "Risky" ~ -1,
                                                    beh_change_start_pandemic_work_from_home =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_work_from_home =="Careful" ~ 1),
                                         beh_change_start_pandemic_children_limited_contacts = case_when(beh_change_start_pandemic_children_limited_contacts == "Risky" ~ -1,
                                                    beh_change_start_pandemic_children_limited_contacts =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_children_limited_contacts =="Careful" ~ 1),
                                         beh_change_start_pandemic_meet_close_despite_restrict = case_when(beh_change_start_pandemic_meet_close_despite_restrict == "Risky" ~ -1,
                                                    beh_change_start_pandemic_meet_close_despite_restrict =="Neutral" ~ 0,
                                                    beh_change_start_pandemic_meet_close_despite_restrict =="Careful" ~ 1))                                                                                                             

data_reduced$attitudes_precautions_mar2020_low_infection_risk_perception  <- as.integer(data_reduced$attitudes_precautions_mar2020_low_infection_risk_perception)
data_reduced$attitudes_precautions_mar2020_risky_infection_course_assessment <- as.integer(data_reduced$attitudes_precautions_mar2020_risky_infection_course_assessment)
data_reduced$attitudes_precautions_mar2020_high_risk_perception <- as.integer(data_reduced$attitudes_precautions_mar2020_high_risk_perception)
data_reduced$attitudes_precautions_mar2020_avoided_risky_situations <- as.integer(data_reduced$attitudes_precautions_mar2020_avoided_risky_situations)
data_reduced$attitudes_precautions_mar2020_aware_distance_rule_effectiveness <- as.integer(data_reduced$attitudes_precautions_mar2020_aware_distance_rule_effectiveness)     
data_reduced$attitudes_precautions_mar2020_understood_mask_reduces_risk <- as.integer(data_reduced$attitudes_precautions_mar2020_understood_mask_reduces_risk)
data_reduced$attitudes_precautions_mar2020_followed_measures <- as.integer(data_reduced$attitudes_precautions_mar2020_followed_measures)
data_reduced$attitudes_precautions_mar2020_felt_restricted_by_measures <- as.integer(data_reduced$attitudes_precautions_mar2020_felt_restricted_by_measures)
data_reduced$attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical <- as.integer(data_reduced$attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical)
data_reduced$beh_change_start_pandemic_avoid_in_person <- as.integer(data_reduced$beh_change_start_pandemic_avoid_in_person)
data_reduced$beh_change_start_pandemic_avoid_careless_contacts <- as.integer(data_reduced$beh_change_start_pandemic_avoid_careless_contacts)
data_reduced$beh_change_start_pandemic_contact_cautious_people <- as.integer(data_reduced$beh_change_start_pandemic_contact_cautious_people)
data_reduced$beh_change_start_pandemic_avoid_peak_hours <- as.integer(data_reduced$beh_change_start_pandemic_avoid_peak_hours)
data_reduced$beh_change_start_pandemic_maintain_distance <- as.integer(data_reduced$beh_change_start_pandemic_maintain_distance)
data_reduced$beh_change_start_pandemic_outdoor_only <- as.integer(data_reduced$beh_change_start_pandemic_outdoor_only)
data_reduced$beh_change_start_pandemic_no_visit_high_risk <- as.integer(data_reduced$beh_change_start_pandemic_no_visit_high_risk)
data_reduced$beh_change_start_pandemic_avoid_busy_places <- as.integer(data_reduced$beh_change_start_pandemic_avoid_busy_places)
data_reduced$beh_change_start_pandemic_avoid_public_trans <- as.integer(data_reduced$beh_change_start_pandemic_avoid_public_trans)
data_reduced$beh_change_start_pandemic_mask_public_trans <- as.integer(data_reduced$beh_change_start_pandemic_mask_public_trans)
data_reduced$beh_change_start_pandemic_mask_supermarket <- as.integer(data_reduced$beh_change_start_pandemic_mask_supermarket)
data_reduced$beh_change_start_pandemic_work_from_home <- as.integer(data_reduced$beh_change_start_pandemic_work_from_home)
data_reduced$beh_change_start_pandemic_children_limited_contacts <- as.integer(data_reduced$beh_change_start_pandemic_children_limited_contacts)
data_reduced$beh_change_start_pandemic_meet_close_despite_restrict <- as.integer(data_reduced$beh_change_start_pandemic_meet_close_despite_restrict)

data_reduced <- data_reduced %>% mutate(attitudeScore = attitudes_precautions_mar2020_low_infection_risk_perception +                
                                    attitudes_precautions_mar2020_risky_infection_course_assessment +            
                                    attitudes_precautions_mar2020_high_risk_perception +                     
                                    attitudes_precautions_mar2020_avoided_risky_situations +                     
                                    attitudes_precautions_mar2020_aware_distance_rule_effectiveness +         
                                    attitudes_precautions_mar2020_understood_mask_reduces_risk +                
                                    attitudes_precautions_mar2020_followed_measures +                        
                                    attitudes_precautions_mar2020_felt_restricted_by_measures +                
                                    attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical) %>%
                                    mutate(behaviorChangeScore = beh_change_start_pandemic_avoid_in_person +
                                    beh_change_start_pandemic_avoid_careless_contacts +
                                    beh_change_start_pandemic_contact_cautious_people +
                                    beh_change_start_pandemic_avoid_peak_hours +
                                    beh_change_start_pandemic_maintain_distance +
                                    beh_change_start_pandemic_outdoor_only +
                                    beh_change_start_pandemic_no_visit_high_risk +
                                    beh_change_start_pandemic_avoid_public_trans +
                                    beh_change_start_pandemic_mask_public_trans +
                                    beh_change_start_pandemic_mask_supermarket +
                                    beh_change_start_pandemic_work_from_home +
                                    beh_change_start_pandemic_children_limited_contacts +
                                    beh_change_start_pandemic_meet_close_despite_restrict)

cor(data_reduced$attitudeScore, data_reduced$behaviorChangeScore, use ="pairwise.complete.obs")

ggplot(data_reduced) +
geom_point(aes(x = attitudeScore, y =behaviorChangeScore))

ggplot(data_reduced) + 
geom_histogram(aes(attitudeScore))

data_reduced %>% count(attitudeScore)
data_reduced %>% count(behaviorChangeScore)

data_reduced$attitudeScore <- factor(data_reduced$attitudeScore)

data_reduced <- data_reduced %>% mutate(RiskyCarefulAtt = case_when(attitudeScore %in% c(-9,-8,-7,-6,-5,-4,-3,-2,-1, 0,1,2,3) ~ "Risky",
                                                                attitudeScore %in% c(4,5,6,7,8,9) ~ "Careful")) %>%
                                mutate(RiskyCarefulBeh = case_when(behaviorChangeScore %in% c(-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1) ~ "Risky",
                                                                    behaviorChangeScore %in% c(14,13,12,11,10,9,8,7,6,5,4,3,2,1) ~ "Careful"))

data_reduced$RiskyCarefulAtt <- factor(data_reduced$RiskyCarefulAtt, levels = c("Risky", "Careful"))
data_reduced$RiskyCarefulBeh <- factor(data_reduced$RiskyCarefulBeh, levels = c("Risky", "Careful"))


data_reduced %>% count(RiskyCarefulAtt)
data_reduced %>% count(RiskyCarefulBeh)

palette <- function() {
  c("#fd5901", "#008083")
}

## ATTITUDE SCORE

p2 <- ggplot(data_reduced %>% filter(!is.na(RiskyCarefulAtt)), aes(date_f1_inf, color = RiskyCarefulAtt)) +
stat_ecdf(geom="smooth", size = 2) +
theme_minimal() +
ylab("Empirical Cumulative \n Distribution Function") +
xlab("Date Of 1st Infection") +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01"))) +
theme(text = element_text(size = 30)) +
scale_color_manual(values = palette()) +
theme(legend.title = element_blank(), legend.position = "none") +
guides(color = guide_legend(nrow = 2))

ggsave("TimingOfInfectionAttCarefulnessScore.pdf", dpi = 500, w = 9, h = 9)
ggsave("TimingOfInfectionAttCarefulnessScore.png", dpi = 500, w = 9, h = 9)

p3 <- data_reduced %>%
  count(RiskyCarefulAtt, num_c19_infs_eng) %>%
  mutate(n = n / sum(n), .by = RiskyCarefulAtt) %>%
  filter(!is.na(RiskyCarefulAtt)) %>% 
  ggplot(aes(num_c19_infs_eng, n, fill = RiskyCarefulAtt)) +
  geom_col(position = position_dodge(preserve = 'single')) +
  theme_minimal() +
  ylab("Relative Frequency") +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  #labs(fill="Behavioral Group") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette())

ggsave("NumberOfInfectionAttCarefulnessScore.pdf", dpi = 500, w = 9, h = 12)
ggsave("NumberOfInfectionAttCarefulnessScore.png", dpi = 500, w = 9, h = 12)

patch <- (p3/p2) +  plot_annotation(tag_levels = "A") 
p4 <- p1  + plot_spacer() + patch + plot_layout(widths = c(13, 0.5, 4.5)) +  plot_annotation(tag_levels = "A") 
ggsave("BoxplotNoInfectionsECDF_attitudeScore.pdf", p4, dpi = 500, w = 18, h = 14)
ggsave("BoxplotNoInfectionsECDF_attitudeScore.png", p4, dpi = 500, w = 18, h = 14)


### CONTACTS 

Careful <- data_reduced %>% filter(RiskyCarefulAtt == "Careful")
Risky <- data_reduced %>% filter(RiskyCarefulAtt == "Risky")

#Work --> Testing for statistically significant difference in mean and median
t.test(Risky$respondent_work_2019, Careful$respondent_work_2019, alternative = "greater")
wilcox.test(Risky$respondent_work_2019, Careful$respondent_work_2019, paired = FALSE)
t.test(Risky$respondent_work_03_2020, Careful$respondent_work_03_2020, alternative = "greater")
wilcox.test(Risky$respondent_work_03_2020, Careful$respondent_work_03_2020, paired = FALSE)
t.test(Risky$respondent_work_summer_2021, Careful$respondent_work_summer_2021, alternative = "greater")
wilcox.test(Risky$respondent_work_summer_2021, Careful$respondent_work_summer_2021, paired = FALSE)
t.test(Risky$respondent_work_01_2023, Careful$respondent_work_01_2023, alternative = "greater")
wilcox.test(Risky$respondent_work_01_2023, Careful$respondent_work_01_2023, paired = FALSE)

# Leisure --> Testing for statistically significant difference in mean and median
t.test(Risky$respondent_leisure_2019, Careful$respondent_leisure_2019, alternative = "greater")
wilcox.test(Risky$respondent_leisure_2019, Careful$respondent_leisure_2019, paired = FALSE)
t.test(Risky$respondent_leisure_03_2020, Careful$respondent_leisure_03_2020, alternative = "greater")
wilcox.test(Risky$respondent_leisure_03_2020, Careful$respondent_leisure_03_2020, paired = FALSE)
t.test(Risky$respondent_leisure_summer_2021, Careful$respondent_leisure_summer_2021, alternative = "greater")
wilcox.test(Risky$respondent_leisure_summer_2021, Careful$respondent_leisure_summer_2021, paired = FALSE)
t.test(Risky$respondent_leisure_01_2023, Careful$respondent_leisure_01_2023, alternative = "greater")
wilcox.test(Risky$respondent_leisure_01_2023, Careful$respondent_leisure_01_2023, paired = FALSE)

# School --> Testing for statistically significant difference in mean and median
t.test(Risky$respondent_school_2019, Careful$respondent_school_2019, alternative = "greater")
wilcox.test(Risky$respondent_school_2019, Careful$respondent_school_2019, paired = FALSE)
t.test(Risky$respondent_school_03_2020, Careful$respondent_school_03_2020, alternative = "greater")
wilcox.test(Risky$respondent_school_03_2020, Careful$respondent_school_03_2020, paired = FALSE)
t.test(Risky$respondent_school_summer_2021, Careful$respondent_school_summer_2021, alternative = "greater")
wilcox.test(Risky$respondent_school_summer_2021, Careful$respondent_school_summer_2021, paired = FALSE)
t.test(Risky$respondent_school_01_2023, Careful$respondent_school_01_2023, alternative = "greater")
wilcox.test(Risky$respondent_school_01_2023, Careful$respondent_school_01_2023, paired = FALSE)

# All --> Testing for statistically significant difference in mean and median
t.test(Risky$respondent_all_2019, Careful$respondent_all_2019, alternative = "greater")
wilcox.test(Risky$respondent_all_2019, Careful$respondent_all_2019, paired = FALSE)
t.test(Risky$respondent_all_03_2020, Careful$respondent_all_03_2020, alternative = "greater")
wilcox.test(Risky$respondent_all_03_2020, Careful$respondent_all_03_2020, paired = FALSE)
t.test(Risky$respondent_all_summer_2021, Careful$respondent_all_summer_2021, alternative = "greater")
wilcox.test(Risky$respondent_all_summer_2021, Careful$respondent_all_summer_2021, paired = FALSE)
t.test(Risky$respondent_all_01_2023, Careful$respondent_all_01_2023, alternative = "greater")
wilcox.test(Risky$respondent_all_01_2023, Careful$respondent_all_01_2023, paired = FALSE)


data_reduced <- data_reduced %>% pivot_longer(cols = 27:103)

data_reduced <- data_reduced  %>% mutate(time = case_when(str_detect(name, "2019") ~ "2019",
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

data_reduced$time <- factor(data_reduced$time, levels = c("2019", "03/2020", "Summer 2021", "01/2023"))
data_reduced$TypeOfContact <- factor(data_reduced$TypeOfContact, levels = c("Work", "Leisure", "School", "All"))
data_reduced$WhoseContacts <- factor(data_reduced$WhoseContacts, levels = c("Respondent", "Household Member", "Closest Contact (Pre-Covid)", "Closest Contact (During-Covid)"))

palette <- function() {
  c("#fd5901", "#008083")
}

p1 <- ggplot(data_reduced %>% filter(WhoseContacts == "Respondent") %>% filter(value < 100) %>% filter(!is.na(RiskyCarefulAtt)) %>% filter(!is.na(TypeOfContact)), aes(RiskyCarefulAtt, value)) +
  #geom_violin(aes(color = WhoseContacts), width = 1, trim = FALSE, position=position_dodge(0.9)) + 
  #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
  geom_boxplot(aes(color = RiskyCarefulAtt), size = 1.3) +
  scale_color_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols = vars(time)) +
  theme_minimal() +
  ylab("Reported No. Of Contacts") +
  theme(text = element_text(size = 30)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom") +
  labs(color ="Attitude Score ") +
  theme(panel.spacing = unit(0.8, "cm", data = NULL))
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("CollectionBoxplots_AttitudeScore.pdf", dpi = 500, w = 13, h = 19)
ggsave("CollectionBoxplots_AttitudeScore.png", dpi = 500, w = 13, h = 19)

## BEHAVIOR SCORE

ggplot(data_reduced %>% filter(!is.na(RiskyCarefulBeh)), aes(date_f1_inf, color = RiskyCarefulBeh)) +
stat_ecdf(geom="smooth", size = 2) +
theme_minimal() +
ylab("Empirical Cumulative \n Distribution Function") +
xlab("Date Of 1st Infection") +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01"))) +
theme(text = element_text(size = 30)) +
scale_color_manual(values = palette()) +
theme(legend.title = element_blank(), legend.position = "bottom") +
guides(color = guide_legend(nrow = 2))

ggsave("TimingOfInfectionBehCarefulnessScore.pdf", dpi = 500, w = 9, h = 9)
ggsave("TimingOfInfectionBehCarefulnessScore.png", dpi = 500, w = 9, h = 9)

data_reduced %>%
  count(RiskyCarefulBeh, num_c19_infs_eng) %>%
  mutate(n = n / sum(n), .by = RiskyCarefulBeh) %>%
  filter(!is.na(RiskyCarefulBeh)) %>% 
  ggplot(aes(num_c19_infs_eng, n, fill = RiskyCarefulBeh)) +
  geom_col(position = position_dodge(preserve = 'single')) +
  theme_minimal() +
  ylab("Relative Frequency") +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom") +
  labs(fill="Behavioral Group") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette())

ggsave("NumberOfInfectionBehCarefulnessScore.pdf", dpi = 500, w = 9, h = 12)
ggsave("NumberOfInfectionBehCarefulnessScore.png", dpi = 500, w = 9, h = 12)
                                                                                 cc_change_during_pandemic == "Nein" ~ "Did Not Change CC"))

palette <- function() {
  c("#8F2D56", "#006BA6")
}

data_reduced %>% filter(!is.na(cc_change_during_pandemic)) %>% filter(attitudeScore != "trifft nicht zu") %>%
filter(attitudeScore != "keine Angabe") %>%
  count(attitudeScore , cc_change_during_pandemic) %>%
  mutate(n = n / sum(n), .by = attitudeScore) %>%
  filter(!is.na(attitudeScore)) %>% 
  ggplot(aes(as.factor(attitudeScore), n, fill = cc_change_during_pandemic)) +
  geom_col(position = "fill") +
  theme_minimal() +
  ylab("Relative Frequency") +
  xlab("Carefulness Score") +
  theme(text = element_text(size = 25)) +
  theme(legend.position = "bottom") + 
  theme(panel.spacing.x = unit(2, "lines")) +
  scale_fill_manual(values = palette()) +
  theme(legend.title=element_blank())

ggsave("WillingnessChangeCC_attitudeScore.png", dpi = 500, w = 13, h =9)
ggsave("WillingnessChangeCC_attitudeScore.pdf", dpi = 500, w = 13, h =9)


data_reduced %>% filter(!is.na(cc_change_during_pandemic)) %>% filter(behaviorChangeScore != "trifft nicht zu") %>%
filter(behaviorChangeScore != "keine Angabe") %>%
  count(behaviorChangeScore , cc_change_during_pandemic) %>%
  mutate(n = n / sum(n), .by = behaviorChangeScore) %>%
  filter(!is.na(behaviorChangeScore)) %>% 
  ggplot(aes(as.factor(behaviorChangeScore), n, fill = cc_change_during_pandemic)) +
  geom_col(position = "fill") +
  theme_minimal() +
  ylab("Relative Frequency") +
  xlab("Carefulness Score") +
  theme(text = element_text(size = 25)) +
  theme(legend.position = "bottom") + 
  theme(panel.spacing.x = unit(2, "lines")) +
  scale_fill_manual(values = palette()) +
  theme(legend.title=element_blank())

ggsave("WillingnessChangeCC_behScore.png", dpi = 500, w = 13, h =9)
ggsave("WillingnessChangeCC_behScore.pdf", dpi = 500, w = 13, h =9)


#Summary stats for NUMBER of Infections
data_reduced <- data_reduced %>% mutate(noInfInt = case_when(num_c19_infs == "Nie" ~ 0,
                                                            num_c19_infs == "Einmal" ~ 1,
                                                            num_c19_infs == "Zweimal" ~ 2,
                                                            num_c19_infs == "Dreimal" ~ 3,
                                                            num_c19_infs == "Mehr als dreimal" ~ 10))


for(attBeh in attitudesAndBehaviors){
ggplot(data_reduced %>% filter(!is.na(!!sym(attBeh)))) +
geom_point(aes(x=date_f1_inf, y = noInfInt, color = !!sym(attBeh))) +
theme_minimal() +
theme(legend.position="bottom") +
theme(text = element_text(size = 22)) +
xlab("Date Of First Infection") +
ylab("Number of Infections") +
scale_y_continuous(breaks = c(0,1,2,3,10))+ 
guides(color = guide_legend(nrow = 2))

ggsave(paste0(attBeh, ".pdf"), dpi = 500, w = 14, h = 4.5)
ggsave(paste0(attBeh, ".png"), dpi = 500, w = 14, h = 4.5)
}


data_reduced <- data_reduced %>% mutate(noInfInt = case_when(num_c19_infs == "Nie" ~ 0,
                                                            num_c19_infs == "Einmal" ~ 1,
                                                            num_c19_infs == "Zweimal" ~ 2,
                                                            num_c19_infs == "Dreimal" ~ 3,
                                                            num_c19_infs == "Mehr als dreimal" ~ 4))

summaryStats <- data.frame(matrix(nrow = 0, ncol = 8))
colnames(summaryStats) <- c("attitude", "average", "min", "firstquartile", "median", "mean", "thirdquartile", "max")
for(attBeh in attitudesAndBehaviors){
    for(avg in comparedToAverage){
    data_reduced_filtered <- data_reduced %>% filter(!!sym(attBeh) == avg)
    summaryStats[nrow(summaryStats) + 1, 1] <- attBeh
    summaryStats[nrow(summaryStats), 2] <- avg
    summaryStats[nrow(summaryStats), 3] <- min(data_reduced_filtered$noInfInt, na.rm=TRUE)
    summaryStats[nrow(summaryStats), 4] <- unname(quantile(data_reduced_filtered$noInfInt, type=1, na.rm=TRUE))[2]
    summaryStats[nrow(summaryStats), 5] <- unname(quantile(data_reduced_filtered$noInfInt, type=1, na.rm=TRUE))[3]
    summaryStats[nrow(summaryStats), 6] <- mean(data_reduced_filtered$noInfInt, na.rm=TRUE)
    summaryStats[nrow(summaryStats), 7] <- unname(quantile(data_reduced_filtered$noInfInt, type=1, na.rm=TRUE))[4]
    summaryStats[nrow(summaryStats), 8] <- max(data_reduced_filtered$noInfInt, na.rm=TRUE)
    }
}