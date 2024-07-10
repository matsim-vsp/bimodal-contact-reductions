library(tidyverse)
library(MMWRweek)

# Author: S. Paltra, contact: paltra@tu-berlin.de

#In the first part of this script we compare the incidence of the respondents of the survey to the official incidence reported by RKI

raw_data <- read_csv("/Users/sydney/Downloads/twitter_data.csv")

reduced_data <- raw_data %>% select(num_c19_infs, user_id, date_f1_inf, date_s2_inf, date_t3_inf) %>% 
filter(!is.na(num_c19_infs)) %>% 
select(user_id, date_f1_inf, date_s2_inf, date_t3_inf)

no_time_infections <- reduced_data %>% pivot_longer(cols=c("date_f1_inf", "date_s2_inf", "date_t3_inf"))
no_time_infections <- no_time_infections %>% 
filter(!is.na(value)) %>%
filter(value > "2020-01-01")
colnames(no_time_infections)[2] <- "CounterInfection"
colnames(no_time_infections)[3] <- "DateInfection"

no_time_infections <- no_time_infections %>% 
mutate(week = isoweek(DateInfection), year = year(DateInfection)) %>%
mutate(date = MMWRweek2Date(MMWRyear = year,
                        MMWRweek = week+1,
                        MMWRday = 1))


count_no_infections <- no_time_infections %>% group_by(date) %>% count()
colnames(count_no_infections) <- c("Date", "CountPer1096")
count_no_infections <- count_no_infections %>% 
                        ungroup() %>%
                        mutate(Incidence100000 = CountPer1096 / length(unique(reduced_data$user_id))*100000)

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

ggplot() +
    geom_point(data = count_no_infections, aes(x = Date, y = Incidence100000, color =DataSet), size = 1.5) +
    #geom_line(data=count_no_infections,  aes(x = Date, y = Incidence100000, color =DataSet), alpha = 0.2, size = 1.2) +
    theme_minimal() +
    xlab("Date") +
    ylab("7-Day-Incidence \n per 100,000") +
    theme(text = element_text(size = 25)) +
    scale_y_log10(breaks=c(1,10,100,1000)) +
    theme(legend.position = "bottom", legend.title = element_blank())

ggsave("VizComparisonIncidenceSurveyRKILog.pdf", dpi = 500, w = 15, h = 5)
ggsave("VizComparisonIncidenceSurveyRKILog.png", dpi = 500, w = 15, h = 5)

ggplot() +
    geom_point(data = count_no_infections, aes(x = Date, y = Incidence100000, color =DataSet), size = 1.5) +
    #geom_line(data=count_no_infections,  aes(x = Date, y = Incidence100000, color =DataSet), alpha = 0.2, size = 1.2) +
    theme_minimal() +
    xlab("Date") +
    ylab("7-Day-Incidence \n per 100,000") +
    theme(text = element_text(size = 25)) +
    #scale_y_log10(breaks=c(1,10,100,1000)) +
    theme(legend.position = "bottom", legend.title = element_blank())

ggsave("VizComparisonIncidenceSurveyRKILin.pdf", dpi = 500, w = 15, h = 5)
ggsave("VizComparisonIncidenceSurveyRKILin.png", dpi = 500, w = 15, h = 5)

# Did careful attitudes/behaviors result in less/later infections? --------

reduced_data <- raw_data %>% select(num_c19_infs, user_id, date_f1_inf, date_s2_inf, date_t3_inf,
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

reduced_data <- reduced_data %>% mutate(attitudes_precautions_mar2020_low_infection_risk_perception = case_when(attitudes_precautions_mar2020_low_infection_risk_perception %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                attitudes_precautions_mar2020_low_infection_risk_perception %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                
                        attitudes_precautions_mar2020_risky_infection_course_assessment = case_when(attitudes_precautions_mar2020_risky_infection_course_assessment %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                attitudes_precautions_mar2020_risky_infection_course_assessment %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                   
                        attitudes_precautions_mar2020_high_risk_perception = case_when(attitudes_precautions_mar2020_high_risk_perception %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                attitudes_precautions_mar2020_high_risk_perception %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                                
                        attitudes_precautions_mar2020_avoided_risky_situations = case_when(attitudes_precautions_mar2020_avoided_risky_situations %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                attitudes_precautions_mar2020_avoided_risky_situations %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                            
                        attitudes_precautions_mar2020_aware_distance_rule_effectiveness = case_when(attitudes_precautions_mar2020_aware_distance_rule_effectiveness %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                attitudes_precautions_mar2020_aware_distance_rule_effectiveness %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),     
                        attitudes_precautions_mar2020_understood_mask_reduces_risk = case_when(attitudes_precautions_mar2020_understood_mask_reduces_risk %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                attitudes_precautions_mar2020_understood_mask_reduces_risk %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                 
                        attitudes_precautions_mar2020_followed_measures = case_when(attitudes_precautions_mar2020_followed_measures %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                attitudes_precautions_mar2020_followed_measures %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                                
                        attitudes_precautions_mar2020_felt_restricted_by_measures = case_when(attitudes_precautions_mar2020_felt_restricted_by_measures %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                attitudes_precautions_mar2020_felt_restricted_by_measures %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                         
                        attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical = case_when(attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),       
                        beh_change_start_pandemic_avoid_in_person = case_when(beh_change_start_pandemic_avoid_in_person %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                beh_change_start_pandemic_avoid_in_person %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                                         
                        beh_change_start_pandemic_avoid_careless_contacts = case_when(beh_change_start_pandemic_avoid_careless_contacts %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                beh_change_start_pandemic_avoid_careless_contacts %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                              
                        beh_change_start_pandemic_contact_cautious_people = case_when(beh_change_start_pandemic_contact_cautious_people %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                beh_change_start_pandemic_contact_cautious_people %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                               
                        beh_change_start_pandemic_avoid_peak_hours = case_when(beh_change_start_pandemic_avoid_peak_hours %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                beh_change_start_pandemic_avoid_peak_hours %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                                 
                        beh_change_start_pandemic_maintain_distance = case_when(beh_change_start_pandemic_maintain_distance %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                beh_change_start_pandemic_maintain_distance %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                                       
                        beh_change_start_pandemic_outdoor_only = case_when(beh_change_start_pandemic_outdoor_only %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                beh_change_start_pandemic_outdoor_only %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                                            
                        beh_change_start_pandemic_no_visit_high_risk = case_when(beh_change_start_pandemic_no_visit_high_risk %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                beh_change_start_pandemic_no_visit_high_risk %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                                      
                        beh_change_start_pandemic_avoid_busy_places = case_when(beh_change_start_pandemic_avoid_busy_places %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                beh_change_start_pandemic_avoid_busy_places %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                                      
                        beh_change_start_pandemic_avoid_public_trans = case_when(beh_change_start_pandemic_avoid_public_trans %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                beh_change_start_pandemic_avoid_public_trans %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                                      
                        beh_change_start_pandemic_mask_public_trans = case_when(beh_change_start_pandemic_mask_public_trans %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                beh_change_start_pandemic_mask_public_trans %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                                       
                        beh_change_start_pandemic_mask_supermarket = case_when(beh_change_start_pandemic_mask_supermarket %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                beh_change_start_pandemic_mask_supermarket %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                                        
                        beh_change_start_pandemic_work_from_home = case_when(beh_change_start_pandemic_work_from_home %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                beh_change_start_pandemic_work_from_home %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                                         
                        beh_change_start_pandemic_children_limited_contacts = case_when(beh_change_start_pandemic_children_limited_contacts %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                beh_change_start_pandemic_children_limited_contacts %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"),                              
                        beh_change_start_pandemic_meet_close_despite_restrict = case_when(beh_change_start_pandemic_meet_close_despite_restrict %in% c("viel weniger", "weniger", "etwas weniger", "genauso") ~ "AverageOrLessThanAverage",
                                                                                                beh_change_start_pandemic_meet_close_despite_restrict %in% c("etwas mehr", "mehr") ~ "MoreThanAverage"))

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

comparedToAverage <- c("AverageOrLessThanAverage", "MoreThanAverage")

reduced_data <- reduced_data %>% filter(user_id != "ebd2141d-accb-48a3-80ec-0f274691f9e6") %>% # entered date of 1st inf. 1922-03-01
                                filter(user_id != "0048e8e6-56fd-4a1c-8639-3c2362afdcaa") %>% #filter(date_f1_inf != "1965-06-12") %>%
                                filter(user_id != "3f1397c7-1c31-4e5f-b510-d29eb6102609") %>% #filter(date_f1_inf != "2000-12-13") %>%
                                filter(user_id != "c02a51b2-8a80-496f-9ef8-1c4b99e025da") #date_f1_inf != "2019-12-21")

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

reduced_data <- reduced_data %>% mutate(date_f1_inf = case_when(is.na(date_f1_inf) ~ as.Date("2025-01-01"),
                                        .default = as.Date(as.character(date_f1_inf))))

for(attBeh in attitudesAndBehaviors){
ggplot(reduced_data %>% filter(!is.na(!!sym(attBeh))), aes(date_f1_inf, color = !!sym(attBeh))) +
stat_ecdf(geom="step", size = 2) +
theme_minimal() +
ggtitle(attBeh) +
ylab("Empirical Cumulative \n Density Function") +
xlab("Date Of 1st Infection") +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01"))) +
theme(text = element_text(size = 22)) +
theme(legend.title = element_blank(), legend.position = "bottom") +
guides(color = guide_legend(nrow=2))
ggsave(paste0(attBeh, "ECDF.pdf"), dpi = 500, w = 12, h = 8)
ggsave(paste0(attBeh, "ECDF.png"), dpi = 500, w = 12, h = 8)
}

# Convert Responses to Attitude Questions to Integers such that I can compute a "carefulness score"
reduced_data <- reduced_data %>% mutate(attitudes_precautions_mar2020_low_infection_risk_perception = case_when(attitudes_precautions_mar2020_low_infection_risk_perception == "AverageOrLessThanAverage" ~ 1,
                                                                                                                attitudes_precautions_mar2020_low_infection_risk_perception == "MoreThanAverage" ~ 0),
                                         attitudes_precautions_mar2020_risky_infection_course_assessment = case_when(attitudes_precautions_mar2020_risky_infection_course_assessment == "AverageOrLessThanAverage" ~ 0,
                                                    attitudes_precautions_mar2020_risky_infection_course_assessment =="MoreThanAverage" ~ 1),
                                         attitudes_precautions_mar2020_high_risk_perception = case_when(attitudes_precautions_mar2020_high_risk_perception == "AverageOrLessThanAverage" ~ 0,
                                                    attitudes_precautions_mar2020_high_risk_perception == "MoreThanAverage" ~ 1),
                                         attitudes_precautions_mar2020_avoided_risky_situations = case_when(attitudes_precautions_mar2020_avoided_risky_situations == "AverageOrLessThanAverage" ~ 0,
                                                    attitudes_precautions_mar2020_avoided_risky_situations == "MoreThanAverage" ~ 1),
                                         attitudes_precautions_mar2020_aware_distance_rule_effectiveness = case_when(attitudes_precautions_mar2020_aware_distance_rule_effectiveness == "AverageOrLessThanAverage" ~ 0,
                                                    attitudes_precautions_mar2020_aware_distance_rule_effectiveness == "MoreThanAverage" ~ 1),
                                         attitudes_precautions_mar2020_understood_mask_reduces_risk = case_when(attitudes_precautions_mar2020_understood_mask_reduces_risk == "AverageOrLessThanAverage" ~ 0,
                                                    attitudes_precautions_mar2020_understood_mask_reduces_risk == "MoreThanAverage" ~ 1),
                                         attitudes_precautions_mar2020_followed_measures = case_when(attitudes_precautions_mar2020_followed_measures == "AverageOrLessThanAverage" ~ 0,
                                                    attitudes_precautions_mar2020_followed_measures == "MoreThanAverage" ~ 1),
                                         attitudes_precautions_mar2020_felt_restricted_by_measures = case_when(attitudes_precautions_mar2020_felt_restricted_by_measures == "AverageOrLessThanAverage" ~ 1,
                                                    attitudes_precautions_mar2020_felt_restricted_by_measures == "MoreThanAverage" ~ 0),
                                         attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical = case_when(attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical == "AverageOrLessThanAverage" ~ 0,
                                                    attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical =="MoreThanAverage" ~ 1))                                                                                                             

reduced_data$attitudes_precautions_mar2020_low_infection_risk_perception  <- as.integer(reduced_data$attitudes_precautions_mar2020_low_infection_risk_perception)
reduced_data$attitudes_precautions_mar2020_risky_infection_course_assessment <- as.integer(reduced_data$attitudes_precautions_mar2020_risky_infection_course_assessment)
reduced_data$attitudes_precautions_mar2020_high_risk_perception <- as.integer(reduced_data$attitudes_precautions_mar2020_high_risk_perception)
reduced_data$attitudes_precautions_mar2020_avoided_risky_situations <- as.integer(reduced_data$attitudes_precautions_mar2020_avoided_risky_situations)
reduced_data$attitudes_precautions_mar2020_aware_distance_rule_effectiveness <- as.integer(reduced_data$attitudes_precautions_mar2020_aware_distance_rule_effectiveness)     
reduced_data$attitudes_precautions_mar2020_understood_mask_reduces_risk <- as.integer(reduced_data$attitudes_precautions_mar2020_understood_mask_reduces_risk)
reduced_data$attitudes_precautions_mar2020_followed_measures <- as.integer(reduced_data$attitudes_precautions_mar2020_followed_measures)
reduced_data$attitudes_precautions_mar2020_felt_restricted_by_measures <- as.integer(reduced_data$attitudes_precautions_mar2020_felt_restricted_by_measures)
reduced_data$attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical <- as.integer(reduced_data$attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical)

reduced_data <- reduced_data %>% mutate(attitudeScore = attitudes_precautions_mar2020_low_infection_risk_perception +                
                                    attitudes_precautions_mar2020_risky_infection_course_assessment +            
                                    attitudes_precautions_mar2020_high_risk_perception +                     
                                    attitudes_precautions_mar2020_avoided_risky_situations +                     
                                    attitudes_precautions_mar2020_aware_distance_rule_effectiveness +         
                                    attitudes_precautions_mar2020_understood_mask_reduces_risk +                
                                    attitudes_precautions_mar2020_followed_measures +                        
                                    attitudes_precautions_mar2020_felt_restricted_by_measures +                
                                    attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical)

reduced_data$attitudeScore <- factor(reduced_data$attitudeScore)

ggplot(reduced_data %>% filter(!is.na(attitudeScore)), aes(date_f1_inf, color = attitudeScore)) +
stat_ecdf(geom="step", size = 2) +
theme_minimal() +
ylab("Empirical Cumulative \n Density Function") +
xlab("Date Of 1st Infection") +
#coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01"))) +
theme(text = element_text(size = 22)) +
theme(legend.title = element_blank(), legend.position = "bottom") +
guides(color = guide_legend(nrow = 2))

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
