
library(tidyverse)
library(MMWRweek)
library(see)
library(RColorBrewer)
library(patchwork)

# Author: S. Paltra, contact: paltra@tu-berlin.de

#In the first part of this script we compare the incidence of the respondents of the survey to the official incidence reported by RKI

#raw_data <- readRDS(file = "/Users/sydney/Desktop/cleaned_data.rds")

source("DataCleaningPrepForContactAnalysis.R")

## ATTITUDE SCORE

data_reduced <- data_reduced %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "Never",
                                                                    num_c19_infs == "Einmal" ~ "Once",
                                                                    num_c19_infs == "Zweimal" ~ "Twice",
                                                                    num_c19_infs == "Dreimal" ~ "Three Times",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "More Than Three Times",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

data_reduced$num_c19_infs_eng <- factor(data_reduced$num_c19_infs_eng, levels = c("Never", "Once", "Twice", "Three Times", "More Than Three Times", "I Don't Want To Answer"))



palette <- function() {
  c("#f1a340", "#998ec3")
}

palette2 <- function() {
  c("#b35806", "#542788")
}

p1 <- ggplot(data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% 
    filter(!is.na(RiskyCarefulAtt )) %>% filter(!is.na(TypeOfContact)) %>% filter(TypeOfContact %in% c("Work", "Leisure")) %>%
    filter(value > -50) %>% filter(value < 150) %>%  
    filter(!is.na(TypeOfContact)), aes(RiskyCarefulAtt , value)) +
  geom_violin(aes(fill = RiskyCarefulAtt , color = RiskyCarefulAtt ), scale = "area", trim = TRUE) + 
  stat_summary(aes(color=RiskyCarefulAtt ), fun.data=mean_sdl, fun.args = list(mult=1), 
                 geom="pointrange", linewidth = 1) +
  #geom_violin(aes(color=WhoseContacts), size = 1.3) +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-50, 0,50, 100)) +
  facet_grid(rows = vars(TypeOfContact), cols= vars(time)) +
  theme_minimal() +
    ylab("Reduction Of Contacts [Percentage]") +
  theme(text = element_text(size = 30)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "none") +
  #labs(color ="Attitude Score ") +
  theme(panel.spacing = unit(0.8, "cm", data = NULL))
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("CollectionViolinplots_AttCarefulnessScore.pdf", p1, dpi = 500, w = 12, h = 9)
ggsave("CollectionViolinplots_AttCarefulnessScore.png", p1, dpi = 500, w = 12, h = 9)

### Tesing for statistically significant difference in contact reduction 

data_reduced <- data_reduced %>% filter(respondent_work_rel_2019_2020 > -1000) %>%
                filter(respondent_work_rel_2019_2021 > -1000) %>%
                filter(respondent_work_rel_2019_2023 > -1000) %>%
                filter(respondent_leisure_rel_2019_2020 > -1000) %>%
                filter(respondent_leisure_rel_2019_2020 > -1000) %>%
                filter(respondent_leisure_rel_2019_2020 > -1000) 

Careful <- data_reduced %>% filter(RiskyCarefulAtt == "Careful")
Risky <- data_reduced %>% filter(RiskyCarefulAtt == "Risky")

#Work --> Testing for statistically significant difference in mean and median
t.test(Careful$respondent_work_rel_2019_2020, Risky$respondent_work_rel_2019_2020, alternative = "greater")
t.test(Careful$respondent_work_rel_2019_2021, Risky$respondent_work_rel_2019_2021, alternative = "greater")
t.test(Careful$respondent_work_rel_2019_2023, Risky$respondent_work_rel_2019_2023, alternative = "greater")

t.test(Careful$respondent_leisure_rel_2019_2020, Risky$respondent_leisure_rel_2019_2020, alternative = "greater")
t.test(Careful$respondent_leisure_rel_2019_2021, Risky$respondent_leisure_rel_2019_2021, alternative = "greater")
t.test(Careful$respondent_leisure_rel_2019_2023, Risky$respondent_leisure_rel_2019_2023, alternative = "greater")


p2 <- ggplot(data_reduced %>% filter(!is.na(RiskyCarefulAtt)), aes(date_f1_inf, color = RiskyCarefulAtt)) +
stat_ecdf(geom="smooth", size = 2) +
theme_minimal() +
ylab("Empirical Cumulative \n Distribution Function") +
xlab("Date Of 1st Infection") +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01"))) +
theme(text = element_text(size = 30)) +
scale_color_manual(values = palette()) +
theme(legend.title = element_blank(), legend.position = "bottom") +
guides(color = guide_legend(nrow = 2))

ggsave("ECDF_AttCarefulnessScore.pdf", p2, dpi = 500, w = 9, h = 9)
ggsave("ECDF_AttCarefulnessScore.png", p2, dpi = 500, w = 9, h = 9)

p3 <- data_reduced %>%
  count(RiskyCarefulAtt, num_c19_infs_eng) %>%
  mutate(percent = 100 * n / sum(n), .by = RiskyCarefulAtt) %>%
  mutate(lci = case_when(RiskyCarefulAtt == "Risky" ~ n - 1.96*(n*(n-1)/39)^0.5,
                          RiskyCarefulAtt == "Careful" ~ n - 1.96*(n*(n-1)/293)^0.5)) %>%#
  mutate(lci = case_when(RiskyCarefulAtt == "Risky" ~ 100/39*lci,
                         RiskyCarefulAtt == "Careful" ~ 100/293*lci)) %>%
  mutate(uci = case_when(RiskyCarefulAtt == "Risky" ~ n + 1.96*(n*(n-1)/39)^0.5,
                          RiskyCarefulAtt == "Careful" ~ n + 1.96*(n*(n-1)/293)^0.5)) %>%
  mutate(uci = case_when(RiskyCarefulAtt == "Risky" ~ 100/39*uci,
                          RiskyCarefulAtt == "Careful" ~ 100/293*uci)) %>%
  filter(!is.na(RiskyCarefulAtt)) %>% filter(num_c19_infs_eng %in% c("Never", "Once", "Twice")) %>%
  ggplot(aes(num_c19_infs_eng, percent, fill = RiskyCarefulAtt)) +
  #geom_col(position = position_dodge(preserve = 'single')) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=num_c19_infs_eng, ymin=lci, ymax=uci, color = RiskyCarefulAtt), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  ylab("Share [Percentage]") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50)) +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  #labs(fill="Behavioral Group") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2())

ggsave("NoInfections_AttCarefulnessScore.pdf", p3, dpi = 500, w = 9, h = 9)
ggsave("NoInfections_AttCarefulnessScore.png", p3, dpi = 500, w = 9, h = 9)

patch <- (p3/p2) +  plot_annotation(tag_levels = "A") 
p4 <- p1  + plot_spacer() + patch + plot_layout(widths = c(13, 0.5, 4.5)) +  plot_annotation(tag_levels = "A") 
ggsave("BoxplotNoInfectionsECDF_attitudeScore.pdf", p4, dpi = 500, w = 18, h = 14)
ggsave("BoxplotNoInfectionsECDF_attitudeScore.png", p4, dpi = 500, w = 18, h = 14)



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
  c("#998ec3", "#f1a340")
}

palette2 <- function() {
  c("#542788", "#b35806")
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
#data_reduced <- data_reduced %>% mutate(cc_change_during_pandemic = case_when(cc_change_during_pandemic == "Ja" ~ "Changed CC",
                                                                             #   cc_change_during_pandemic == "Nein" ~ "Did Not Change CC"))

palette <- function() {
  c("#998ec3", "#f1a340")
}

palette2 <- function() {
  c("#542788", "#b35806")
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