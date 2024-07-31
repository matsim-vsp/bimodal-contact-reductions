library(tidyverse)
library(MMWRweek)
library(see)
library(RColorBrewer)

# Author: S. Paltra, contact: paltra@tu-berlin.de

#In the first part of this script we compare the incidence of the respondents of the survey to the official incidence reported by RKI

raw_data <- readRDS(file = "/Users/sydney/Desktop/cleaned_data.rds")

reduced_data <- raw_data %>% select(num_c19_infs, date_f1_inf, date_s2_inf, date_t3_inf) %>% 
filter(!is.na(num_c19_infs)) %>% 
select(date_f1_inf, date_s2_inf, date_t3_inf)

no_time_infections <- reduced_data %>% pivot_longer(cols=c("date_f1_inf", "date_s2_inf", "date_t3_inf"))
no_time_infections <- no_time_infections %>% 
filter(!is.na(value)) %>%
filter(value > "2020-01-01")
colnames(no_time_infections)[1] <- "CounterInfection"
colnames(no_time_infections)[2] <- "DateInfection"

no_time_infections <- no_time_infections %>% 
mutate(week = isoweek(DateInfection), year = year(DateInfection)) %>%
mutate(date = MMWRweek2Date(MMWRyear = year,
                        MMWRweek = week+1,
                        MMWRday = 1))


count_no_infections <- no_time_infections %>% group_by(date) %>% count()
colnames(count_no_infections) <- c("Date", "CountPer1096")
count_no_infections <- count_no_infections %>% 
                        ungroup() %>%
                        mutate(Incidence100000 = CountPer1096 / nrow(reduced_data)*100000)

DatesInfections <- unique(count_no_infections$Date)
dates <- c()
date <- as.Date("2020-01-05")
while(date < "2023-10-01"){
    dates <- append(dates, date)
    date <- date + 7
}
for(date in dates){
    date <- as.Date(date)
    if(date %in% unique(count_no_infections$Date)){
    }else{
        row <- c(date, 0, 0)
        count_no_infections <- rbind(count_no_infections, row)
    }
}

rkidata <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Deutschland.csv")
rkidata <- rkidata %>% 
        filter(Altersgruppe == "00+") %>% 
        filter(Meldedatum < "2023-10-01") %>%
        mutate(weekday = wday(Meldedatum)) %>%
        filter(weekday == 1)

rkidata <- rkidata %>% select(Meldedatum, `Inzidenz_7-Tage`)
colnames(rkidata) <- c("Date", "Incidence100000")
rkidata <- rkidata %>% mutate(DataSet = "RKI")
count_no_infections <- count_no_infections %>% 
                        select(Date, Incidence100000) %>%
                        mutate(DataSet = "Survey")
count_no_infections <- rbind(count_no_infections, rkidata)

palette <- function() {
  c("#8F2D56", "#006BA6")
}

ggplot() +
    geom_point(data = count_no_infections, aes(x = Date, y = Incidence100000, color =DataSet), size = 2) +
    #geom_line(data=count_no_infections,  aes(x = Date, y = Incidence100000, color =DataSet), alpha = 0.2, size = 1.2) +
    theme_minimal() +
    xlab("Date") +
    scale_color_manual(values = palette()) +
    ylab("7-Day-Incidence \n per 100,000") +
    theme(text = element_text(size = 30)) +
    scale_y_log10(breaks=c(1,10,100,1000)) +
    theme(legend.position = "bottom", legend.title = element_blank())

ggsave("VizComparisonIncidenceSurveyRKILog.pdf", dpi = 500, w = 15, h = 5)
ggsave("VizComparisonIncidenceSurveyRKILog.png", dpi = 500, w = 15, h = 5)

ggplot() +
    geom_point(data = count_no_infections, aes(x = Date, y = Incidence100000, color = DataSet), size = 1.5) +
    #geom_line(data=count_no_infections,  aes(x = Date, y = Incidence100000, color =DataSet), alpha = 0.2, size = 1.2) +
    theme_minimal() +
    xlab("Date") +
    scale_color_manual(values = palette()) +
    ylab("7-Day-Incidence \n per 100,000") +
    theme(text = element_text(size = 35)) +
    #scale_y_log10(breaks=c(1,10,100,1000)) +
    theme(legend.position = "bottom", legend.title = element_blank())

ggsave("VizComparisonIncidenceSurveyRKILin.pdf", dpi = 500, w = 15, h = 5)
ggsave("VizComparisonIncidenceSurveyRKILin.png", dpi = 500, w = 15, h = 5)

# Did careful attitudes/behaviors result in less/later infections? --------

reduced_data <- raw_data %>% select(num_c19_infs, date_f1_inf, date_s2_inf, date_t3_inf, cc_change_during_pandemic,
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

palette <- function() {
  c("#8F2D56", "#006BA6")
}

reduced_data$cc_change_during_pandemic <- factor(reduced_data$cc_change_during_pandemic, levels = c("Nein", "Ja"))

for(attBeh in attitudesAndBehaviors){
reduced_data %>% filter(!is.na(cc_change_during_pandemic)) %>% filter(!!sym(attBeh) != "trifft nicht zu") %>%
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

reduced_data <- reduced_data %>% mutate(attitudes_precautions_mar2020_low_infection_risk_perception = case_when(attitudes_precautions_mar2020_low_infection_risk_perception %in% c("viel weniger", "weniger", "etwas weniger") ~ "Careful",
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

#reduced_data <- reduced_data %>% mutate(cc_change_during_pandemic = case_when(cc_change_during_pandemic == "Ja" ~ "Changed CC",
                                                                             #   cc_change_during_pandemic == "Nein" ~ "Did Not Change CC"))

palette <- function() {
  c("#8F2D56", "#006BA6")
}

for(attBeh in attitudesAndBehaviors){
reduced_data %>% filter(!is.na(cc_change_during_pandemic)) %>% filter(!!sym(attBeh) != "trifft nicht zu") %>%
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

reduced_data <- reduced_data %>% mutate(date_f1_inf = case_when(is.na(date_f1_inf) ~ as.Date("2025-01-01"),
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
    reduced_data_filtered <- reduced_data %>% filter(!!sym(attBeh) == avg)
    reduced_data_noInf <- reduced_data_filtered %>% filter(num_c19_infs == "Nie")
    reduced_data_noInf <- nrow(reduced_data_noInf)
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
  c("#006BA6", "#FFBC42", "#8F2D56")
}

reduced_data <- reduced_data %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "Never",
                                                                    num_c19_infs == "Einmal" ~ "Once",
                                                                    num_c19_infs == "Zweimal" ~ "Twice",
                                                                    num_c19_infs == "Dreimal" ~ "Three Times",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "More Than Three Times",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

reduced_data$num_c19_infs_eng <- factor(reduced_data$num_c19_infs_eng, levels = c("Never", "Once", "Twice", "Three Times", "More Than Three Times", "I Don't Want To Answer"))


for(attBeh in attitudesAndBehaviors){
ggplot(reduced_data %>% filter(!is.na(!!sym(attBeh))), aes(date_f1_inf, color = !!sym(attBeh))) +
stat_ecdf(geom="smooth", size = 2) +
theme_minimal() +
ggtitle(attBeh) +
ylab("Empirical Cumulative \n Density Function") +
xlab("Date Of 1st Infection") +
scale_color_manual(values = palette()) +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01"))) +
theme(text = element_text(size = 22)) +
theme(legend.title = element_blank(), legend.position = "bottom") +
guides(color = guide_legend(nrow=2))
ggsave(paste0(attBeh, "ECDF.pdf"), dpi = 500, w = 9, h = 9)
ggsave(paste0(attBeh, "ECDF.png"), dpi = 500, w = 9, h = 9)

reduced_data %>%
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
reduced_data <- reduced_data %>% mutate(attitudes_precautions_mar2020_low_infection_risk_perception = case_when(attitudes_precautions_mar2020_low_infection_risk_perception == "Careful" ~ 1,
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

reduced_data$attitudes_precautions_mar2020_low_infection_risk_perception  <- as.integer(reduced_data$attitudes_precautions_mar2020_low_infection_risk_perception)
reduced_data$attitudes_precautions_mar2020_risky_infection_course_assessment <- as.integer(reduced_data$attitudes_precautions_mar2020_risky_infection_course_assessment)
reduced_data$attitudes_precautions_mar2020_high_risk_perception <- as.integer(reduced_data$attitudes_precautions_mar2020_high_risk_perception)
reduced_data$attitudes_precautions_mar2020_avoided_risky_situations <- as.integer(reduced_data$attitudes_precautions_mar2020_avoided_risky_situations)
reduced_data$attitudes_precautions_mar2020_aware_distance_rule_effectiveness <- as.integer(reduced_data$attitudes_precautions_mar2020_aware_distance_rule_effectiveness)     
reduced_data$attitudes_precautions_mar2020_understood_mask_reduces_risk <- as.integer(reduced_data$attitudes_precautions_mar2020_understood_mask_reduces_risk)
reduced_data$attitudes_precautions_mar2020_followed_measures <- as.integer(reduced_data$attitudes_precautions_mar2020_followed_measures)
reduced_data$attitudes_precautions_mar2020_felt_restricted_by_measures <- as.integer(reduced_data$attitudes_precautions_mar2020_felt_restricted_by_measures)
reduced_data$attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical <- as.integer(reduced_data$attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical)
reduced_data$beh_change_start_pandemic_avoid_in_person <- as.integer(reduced_data$beh_change_start_pandemic_avoid_in_person)
reduced_data$beh_change_start_pandemic_avoid_careless_contacts <- as.integer(reduced_data$beh_change_start_pandemic_avoid_careless_contacts)
reduced_data$beh_change_start_pandemic_contact_cautious_people <- as.integer(reduced_data$beh_change_start_pandemic_contact_cautious_people)
reduced_data$beh_change_start_pandemic_avoid_peak_hours <- as.integer(reduced_data$beh_change_start_pandemic_avoid_peak_hours)
reduced_data$beh_change_start_pandemic_maintain_distance <- as.integer(reduced_data$beh_change_start_pandemic_maintain_distance)
reduced_data$beh_change_start_pandemic_outdoor_only <- as.integer(reduced_data$beh_change_start_pandemic_outdoor_only)
reduced_data$beh_change_start_pandemic_no_visit_high_risk <- as.integer(reduced_data$beh_change_start_pandemic_no_visit_high_risk)
reduced_data$beh_change_start_pandemic_avoid_busy_places <- as.integer(reduced_data$beh_change_start_pandemic_avoid_busy_places)
reduced_data$beh_change_start_pandemic_avoid_public_trans <- as.integer(reduced_data$beh_change_start_pandemic_avoid_public_trans)
reduced_data$beh_change_start_pandemic_mask_public_trans <- as.integer(reduced_data$beh_change_start_pandemic_mask_public_trans)
reduced_data$beh_change_start_pandemic_mask_supermarket <- as.integer(reduced_data$beh_change_start_pandemic_mask_supermarket)
reduced_data$beh_change_start_pandemic_work_from_home <- as.integer(reduced_data$beh_change_start_pandemic_work_from_home)
reduced_data$beh_change_start_pandemic_children_limited_contacts <- as.integer(reduced_data$beh_change_start_pandemic_children_limited_contacts)
reduced_data$beh_change_start_pandemic_meet_close_despite_restrict <- as.integer(reduced_data$beh_change_start_pandemic_meet_close_despite_restrict)

reduced_data <- reduced_data %>% mutate(attitudeScore = attitudes_precautions_mar2020_low_infection_risk_perception +                
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

reduced_data$attitudeScore <- factor(reduced_data$attitudeScore)

reduced_data <- reduced_data %>% mutate(RiskyCarefulAtt = case_when(attitudeScore %in% c(-9,-8,-7,-6,-5,-4,-3,-2,-1) ~ "Risky",
                                                                attitudeScore %in% c(1,2,3,4,5,6,7,8,9) ~ "Careful")) %>%
                                mutate(RiskyCarefulBeh = case_when(behaviorChangeScore %in% c(-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1) ~ "Risky",
                                                                    behaviorChangeScore %in% c(14,13,12,11,10,9,8,7,6,5,4,3,2,1) ~ "Careful"))

reduced_data$RiskyCarefulAtt <- factor(reduced_data$RiskyCarefulAtt, levels = c("Risky", "Careful"))
reduced_data$RiskyCarefulBeh <- factor(reduced_data$RiskyCarefulBeh, levels = c("Risky", "Careful"))

palette <- function() {
  c("#8F2D56", "#006BA6")
}

ggplot(reduced_data %>% filter(!is.na(RiskyCarefulAtt)), aes(date_f1_inf, color = RiskyCarefulAtt)) +
stat_ecdf(geom="smooth", size = 2) +
theme_minimal() +
ylab("Empirical Cumulative \n Density Function") +
xlab("Date Of 1st Infection") +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01"))) +
theme(text = element_text(size = 30)) +
scale_color_manual(values = palette()) +
theme(legend.title = element_blank(), legend.position = "bottom") +
guides(color = guide_legend(nrow = 2))

ggsave("TimingOfInfectionAttCarefulnessScore.pdf", dpi = 500, w = 9, h = 9)
ggsave("TimingOfInfectionAttCarefulnessScore.png", dpi = 500, w = 9, h = 9)

reduced_data %>%
  count(RiskyCarefulAtt, num_c19_infs_eng) %>%
  mutate(n = n / sum(n), .by = RiskyCarefulAtt) %>%
  filter(!is.na(RiskyCarefulAtt)) %>% 
  ggplot(aes(num_c19_infs_eng, n, fill = RiskyCarefulAtt)) +
  geom_col(position = position_dodge(preserve = 'single')) +
  theme_minimal() +
  ylab("Relative Frequency") +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom") +
  labs(fill="Behavioral Group") +
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette())

ggsave("NumberOfInfectionAttCarefulnessScore.pdf", dpi = 500, w = 9, h = 12)
ggsave("NumberOfInfectionAttCarefulnessScore.png", dpi = 500, w = 9, h = 12)

ggplot(reduced_data %>% filter(!is.na(RiskyCarefulBeh)), aes(date_f1_inf, color = RiskyCarefulBeh)) +
stat_ecdf(geom="smooth", size = 2) +
theme_minimal() +
ylab("Empirical Cumulative \n Density Function") +
xlab("Date Of 1st Infection") +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01"))) +
theme(text = element_text(size = 30)) +
scale_color_manual(values = palette()) +
theme(legend.title = element_blank(), legend.position = "bottom") +
guides(color = guide_legend(nrow = 2))

ggsave("TimingOfInfectionBehCarefulnessScore.pdf", dpi = 500, w = 9, h = 9)
ggsave("TimingOfInfectionBehCarefulnessScore.png", dpi = 500, w = 9, h = 9)

reduced_data %>%
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
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette())

ggsave("NumberOfInfectionBehCarefulnessScore.pdf", dpi = 500, w = 9, h = 12)
ggsave("NumberOfInfectionBehCarefulnessScore.png", dpi = 500, w = 9, h = 12)
                                                                                 cc_change_during_pandemic == "Nein" ~ "Did Not Change CC"))

palette <- function() {
  c("#8F2D56", "#006BA6")
}

reduced_data %>% filter(!is.na(cc_change_during_pandemic)) %>% filter(attitudeScore != "trifft nicht zu") %>%
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


reduced_data %>% filter(!is.na(cc_change_during_pandemic)) %>% filter(behaviorChangeScore != "trifft nicht zu") %>%
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
reduced_data <- reduced_data %>% mutate(noInfInt = case_when(num_c19_infs == "Nie" ~ 0,
                                                            num_c19_infs == "Einmal" ~ 1,
                                                            num_c19_infs == "Zweimal" ~ 2,
                                                            num_c19_infs == "Dreimal" ~ 3,
                                                            num_c19_infs == "Mehr als dreimal" ~ 10))


for(attBeh in attitudesAndBehaviors){
ggplot(reduced_data %>% filter(!is.na(!!sym(attBeh)))) +
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


reduced_data <- reduced_data %>% mutate(noInfInt = case_when(num_c19_infs == "Nie" ~ 0,
                                                            num_c19_infs == "Einmal" ~ 1,
                                                            num_c19_infs == "Zweimal" ~ 2,
                                                            num_c19_infs == "Dreimal" ~ 3,
                                                            num_c19_infs == "Mehr als dreimal" ~ 4))

summaryStats <- data.frame(matrix(nrow = 0, ncol = 8))
colnames(summaryStats) <- c("attitude", "average", "min", "firstquartile", "median", "mean", "thirdquartile", "max")
for(attBeh in attitudesAndBehaviors){
    for(avg in comparedToAverage){
    reduced_data_filtered <- reduced_data %>% filter(!!sym(attBeh) == avg)
    summaryStats[nrow(summaryStats) + 1, 1] <- attBeh
    summaryStats[nrow(summaryStats), 2] <- avg
    summaryStats[nrow(summaryStats), 3] <- min(reduced_data_filtered$noInfInt, na.rm=TRUE)
    summaryStats[nrow(summaryStats), 4] <- unname(quantile(reduced_data_filtered$noInfInt, type=1, na.rm=TRUE))[2]
    summaryStats[nrow(summaryStats), 5] <- unname(quantile(reduced_data_filtered$noInfInt, type=1, na.rm=TRUE))[3]
    summaryStats[nrow(summaryStats), 6] <- mean(reduced_data_filtered$noInfInt, na.rm=TRUE)
    summaryStats[nrow(summaryStats), 7] <- unname(quantile(reduced_data_filtered$noInfInt, type=1, na.rm=TRUE))[4]
    summaryStats[nrow(summaryStats), 8] <- max(reduced_data_filtered$noInfInt, na.rm=TRUE)
    }
}

# Carefulness Due To Old Age ----------------------------------------------

reduced_data <- raw_data %>% select(num_c19_infs, date_f1_inf, date_s2_inf, date_t3_inf, year_of_birth, cond_none) %>%
                            mutate(age_group = case_when (year_of_birth <= 1960 ~ "60+",
                                                            year_of_birth > 1960 ~ "18-59"))

reduced_data <- reduced_data %>% mutate(date_f1_inf = case_when(is.na(date_f1_inf) ~ as.Date("2025-01-01"),
                                        .default = as.Date(as.character(date_f1_inf)))) %>%
                                filter(date_f1_inf != as.Date("1922-03-01")) %>%
                                filter(date_f1_inf != as.Date("1965-06-12")) %>%
                                filter(date_f1_inf != as.Date("2000-12-13")) %>%
                                filter(date_f1_inf != as.Date("2019-12-21")) 

reduced_data <- reduced_data %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "Never",
                                                                    num_c19_infs == "Einmal" ~ "Once",
                                                                    num_c19_infs == "Zweimal" ~ "Twice",
                                                                    num_c19_infs == "Dreimal" ~ "Three Times",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "More Than Three Times",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

reduced_data$num_c19_infs_eng <- factor(reduced_data$num_c19_infs_eng, levels = c("Never", "Once", "Twice", "Three Times", "More Than Three Times", "I Don't Want To Answer"))

palette <- function() {
  c("#8F2D56", "#006BA6")
}

ggplot(reduced_data %>% filter(!is.na(age_group)), aes(date_f1_inf, color = age_group)) +
stat_ecdf(geom="smooth", size = 2) +
theme_minimal() +
ylab("Empirical Cumulative \n Density Function") +
xlab("Date Of First Infection") +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01")), ylim=c(0, 0.75)) +
theme(text = element_text(size = 30)) +
theme(legend.position = "bottom") +
labs(color="Age Group") +
guides(color = guide_legend(nrow = 2)) +
scale_color_manual(values = palette())

ggsave("TimingOfInfectionByAge.pdf", dpi = 500, w = 9, h = 9)
ggsave("TimingOfInfectionByAge.png", dpi = 500, w = 9, h = 9)

reduced_data %>%
  count(age_group, num_c19_infs_eng) %>%
  mutate(n = n / sum(n), .by = 'age_group') %>%
  filter(!is.na(age_group)) %>% 
  ggplot(aes(num_c19_infs_eng, n, fill = age_group)) +
  geom_col(position = position_dodge(preserve = 'single')) +
  theme_minimal() +
  ylab("Relative Frequency") +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom") +
  labs(fill="Age Group") +
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette())

ggsave("NumberOfInfectionByAge.pdf", dpi = 500, w = 9, h = 12)
ggsave("NumberOfInfectionByAge.png", dpi = 500, w = 9, h = 12)


# Carefulness Due To Comorbidities ----------------------------------------

reduced_data <- raw_data %>% select(num_c19_infs, date_f1_inf, date_s2_inf, date_t3_inf,
                                    cond_hbp, cond_diabetes, cond_cardio, cond_resp,
                                    cond_immuno, cond_cancer, cond_post_c19, cond_none)

reduced_data <- reduced_data %>% mutate(date_f1_inf = case_when(is.na(date_f1_inf) ~ as.Date("2025-01-01"),
                                        .default = as.Date(as.character(date_f1_inf)))) %>%
                                filter(date_f1_inf != as.Date("1922-03-01")) %>%
                                filter(date_f1_inf != as.Date("1965-06-12")) %>%
                                filter(date_f1_inf != as.Date("2000-12-13")) %>%
                                filter(date_f1_inf != as.Date("2019-12-21")) 

reduced_data <- reduced_data %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "Never",
                                                                    num_c19_infs == "Einmal" ~ "Once",
                                                                    num_c19_infs == "Zweimal" ~ "Twice",
                                                                    num_c19_infs == "Dreimal" ~ "Three Times",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "More Than Three Times",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

reduced_data$num_c19_infs_eng <- factor(reduced_data$num_c19_infs_eng, levels = c("Never", "Once", "Twice", "Three Times", "More Than Three Times", "I Don't Want To Answer"))

palette <- function() {
  c("#8F2D56", "#006BA6")
}

reduced_data <- reduced_data %>% mutate(cond_  = case_when(cond_hbp == "Ja" ~ "Yes",
                                cond_hbp == "Nicht Gewählt" ~ "No")) %>% 
                                mutate(cond_diabetes = case_when(cond_diabetes == "Ja" ~ "Yes",
                                cond_diabetes == "Nicht Gewählt" ~ "No")) %>% 
                                mutate(cond_cardio = case_when(cond_cardio == "Ja" ~ "Yes",
                                cond_cardio == "Nicht Gewählt" ~ "No")) %>% 
                                mutate(cond_resp = case_when(cond_resp == "Ja" ~ "Yes",
                                cond_resp == "Nicht Gewählt" ~ "No")) %>% 
                                mutate(cond_immuno = case_when(cond_immuno == "Ja" ~ "Yes",
                                cond_immuno == "Nicht Gewählt" ~ "No")) %>% 
                                mutate(cond_cancer = case_when(cond_cancer == "Ja" ~ "Yes",
                                cond_cancer == "Nicht Gewählt" ~ "No")) %>%
                                mutate(cond_post_c19 = case_when(cond_post_c19 == "Ja" ~ "Yes",
                                cond_post_c19 == "Nicht Gewählt" ~ "No")) %>%
                                mutate(cond_none = case_when(cond_none == "Ja" ~ "No Comorbidities",
                                cond_none == "Nicht Gewählt" ~ "Comorbidities"))

reduced_data$cond_hbp <- factor(reduced_data$cond_hbp, levels = c("No", "Yes"))
reduced_data$cond_diabetes <- factor(reduced_data$cond_diabetes, levels = c("No", "Yes"))
reduced_data$cond_cardio <- factor(reduced_data$cond_cardio, levels = c("No", "Yes"))
reduced_data$cond_resp <- factor(reduced_data$cond_resp, levels = c("No", "Yes"))
reduced_data$cond_immuno <- factor(reduced_data$cond_immuno, levels = c("No", "Yes"))
reduced_data$cond_cancer <- factor(reduced_data$cond_cancer, levels = c("No", "Yes"))
reduced_data$cond_post_c_19 <- factor(reduced_data$cond_post_c19, levels = c("No", "Yes"))
reduced_data$cond_none <- factor(reduced_data$cond_none, levels = c("No Comorbidities", "Comorbidities"))

comorbidities <- c("cond_hbp", "cond_diabetes", "cond_cardio", "cond_resp",
                    "cond_immuno", "cond_cancer", "cond_post_c19", "cond_none")

for(com in comorbidities){
ggplot(reduced_data %>% filter(!is.na(!!sym(com))), aes(date_f1_inf, color = !!sym(com))) +
stat_ecdf(geom="smooth", size = 2) +
theme_minimal() +
ylab("Empirical Cumulative \n Density Function") +
xlab("Date Of First Infection") +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01")), ylim=c(0, 0.75)) +
theme(text = element_text(size = 30)) +
theme(legend.position = "bottom", legend.title = element_blank()) +
labs(color=com) +
guides(color = guide_legend(nrow = 2)) +
scale_color_manual(values = palette())

ggsave(paste0(com, "_timingOfFirstInf_ECDF.pdf"), dpi = 500, w = 9, h = 9)
ggsave(paste0(com, "_timingOfFirstInf_ECDF.png"), dpi = 500, w = 9, h = 9)

reduced_data %>%
  count(!!sym(com), num_c19_infs_eng) %>%
  mutate(n = n / sum(n), .by = !!sym(com)) %>%
  filter(!is.na(!!sym(com))) %>% 
  ggplot(aes(num_c19_infs_eng, n, fill = !!sym(com))) +
  geom_col(position = position_dodge(preserve = 'single')) +
  theme_minimal() +
  ylab("Relative Frequency") +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title=element_blank()) +
  #labs(fill=com) +
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette())

ggsave(paste0(com, "NumberOfInfection.pdf"), dpi = 500, w = 9, h = 12)
ggsave(paste0(com,"NumberOfInfection.png"), dpi = 500, w = 9, h = 12)
}


