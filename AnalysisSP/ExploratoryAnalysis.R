library(tidyverse)
library(RColorBrewer)

raw_data <- read_csv("ENTER PATH HERE")
# Number of infections ----------------------------------------------------

no_time_infections <- raw_data %>% select(user_id, num_c19_infs, date_f1_inf, date_s2_inf, date_t3_inf) %>%
  mutate(noInfections = case_when(num_c19_infs == NA ~ NA,
                                  num_c19_infs == "Nie" ~ "0",
                                  num_c19_infs == "Einmal" ~ "1",
                                  num_c19_infs == "Zweimal" ~ "2",
                                  num_c19_infs == "Dreimal" ~ "3",
                                  num_c19_infs == "Mehr als dreimal" ~ "3+",
                                  num_c19_infs == "Ich möchte nicht antworten" ~ "Unwilling to answer"))

#Plot no. of Covid Infections
ggplot(no_time_infections %>% filter(is.na(noInfections) == FALSE)) + 
  geom_bar(aes(noInfections, y = ..prop.., group = 1), fill = "#1b9e77") +
  theme_minimal() +
  xlab("# Covid Infections") +
  ylab("Count") +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave("RelNoInfections.png", dpi = 500, w = 9, h = 4.5)

# Time of infections ------------------------------------------------------

no_time_infections <- no_time_infections %>% pivot_longer(cols=c("date_f1_inf", "date_s2_inf", "date_t3_inf"))
colnames(no_time_infections)[4] <- "CounterInfection"
colnames(no_time_infections)[5] <- "DateInfection"

no_time_infections <- no_time_infections %>% mutate(DateInfectionInterval = case_when(DateInfection < "2020-03-01" ~ "Feb2020",
                                                                                      DateInfection < "2020-04-01" ~ "Mar2020",
                                                                                      DateInfection < "2020-05-01" ~ "Apr2020",
                                                                                      DateInfection < "2020-06-01" ~ "May2020",
                                                                                      DateInfection < "2020-07-01" ~ "Jun2020",
                                                                                      DateInfection < "2020-08-01" ~ "Jul2020",
                                                                                      DateInfection < "2020-09-01" ~ "Aug2020",
                                                                                      DateInfection < "2020-10-01" ~ "Sep2020",
                                                                                      DateInfection < "2020-11-01" ~ "Oct2020",
                                                                                      DateInfection < "2020-12-01" ~ "Nov2020",
                                                                                      DateInfection < "2021-01-01" ~ "Dec2020",
                                                                                      DateInfection < "2021-02-01" ~ "Jan2021",
                                                                                      DateInfection < "2021-03-01" ~ "Feb2021",
                                                                                      DateInfection < "2021-04-01" ~ "Mar2021",
                                                                                      DateInfection < "2021-05-01" ~ "Apr2021",
                                                                                      DateInfection < "2021-06-01" ~ "May2021",
                                                                                      DateInfection < "2021-07-01" ~ "Jun2021",
                                                                                      DateInfection < "2021-08-01" ~ "Jul2021",
                                                                                      DateInfection < "2021-09-01" ~ "Aug2021",
                                                                                      DateInfection < "2021-10-01" ~ "Sep2021",
                                                                                      DateInfection < "2021-11-01" ~ "Oct2021",
                                                                                      DateInfection < "2021-12-01" ~ "Nov2021",
                                                                                      DateInfection < "2022-01-01" ~ "Dec2021",
                                                                                      DateInfection < "2022-02-01" ~ "Jan2021",
                                                                                      DateInfection < "2022-03-01" ~ "Feb2022",
                                                                                      DateInfection < "2022-04-01" ~ "Mar2022",
                                                                                      DateInfection < "2022-05-01" ~ "Apr2022",
                                                                                      DateInfection < "2022-06-01" ~ "May2022",
                                                                                      DateInfection < "2022-07-01" ~ "Jun2022",
                                                                                      DateInfection < "2022-08-01" ~ "Jul2022",
                                                                                      DateInfection < "2022-09-01" ~ "Aug2022",
                                                                                      DateInfection < "2022-10-01" ~ "Sep2022",
                                                                                      DateInfection < "2022-11-01" ~ "Oct2022",
                                                                                      DateInfection < "2022-12-01" ~ "Nov2022",
                                                                                      DateInfection < "2023-01-01" ~ "Dec2022",
                                                                                      DateInfection < "2023-02-01" ~ "Jan2023",
                                                                                      DateInfection < "2023-03-01" ~ "Feb2023",
                                                                                      DateInfection < "2023-04-01" ~ "Mar2023",
                                                                                      DateInfection < "2023-05-01" ~ "Apr2023",
                                                                                      DateInfection < "2023-06-01" ~ "May2023",
                                                                                      DateInfection < "2023-07-01" ~ "Jun2023",
                                                                                      DateInfection < "2023-08-01" ~ "Jul2023",
                                                                                      DateInfection < "2023-09-01" ~ "Aug2023",
                                                                                      DateInfection < "2023-10-01" ~ "Sep2023"))

no_time_infections$DateInfectionInterval <- factor(no_time_infections$DateInfectionInterval, levels = c("Feb2020", "Mar2020", "Apr2020", "May2020", "Jun2020", "Jul2020", "Aug2020", "Sep2020", "Oct2020", "Nov2020", "Dec2020",
                                                                                                        "Jan2021", "Feb2021", "Mar2021", "Apr2021", "May2021", "Jun2021", "Jul2021", "Aug2021", "Sep2021", "Oct2021", "Nov2021", "Dec2021",
                                                                                                        "Jan2022", "Feb2022", "Mar2022", "Apr2022", "May2022", "Jun2022", "Jul2022", "Aug2022", "Sep2022", "Oct2022", "Nov2022", "Dec2022",
                                                                                                        "Jan2023", "Feb2023", "Mar2023", "Apr2023", "May2023", "Jun2023", "Jul2023", "Aug2023", "Sep2023"))
ggplot(no_time_infections %>% filter(CounterInfection == "date_f1_inf") %>% filter(is.na(DateInfectionInterval) == FALSE)) + 
  geom_bar(aes(DateInfectionInterval), fill = "#1b9e77") +
  theme_minimal() +
  xlab("Month of First Infection") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave("TimeFirstInfection.png", dpi = 500, w = 9, h = 4.5)

# Vaccination -------------------------------------------------------------

VaccinationYesNo <- raw_data %>% select(user_id, c19_vaccination_status)
VaccinationYesNo <- VaccinationYesNo[!is.na(VaccinationYesNo$user_id),]

VaccinationYesNo$c19_vaccination_status <- factor(VaccinationYesNo$c19_vaccination_status, levels = c("Ja", "Nein", "Weiß ich nicht", "Ich möchte nicht antworten", NA))

ggplot(VaccinationYesNo) + 
  geom_bar(aes(x = c19_vaccination_status, y = ..prop.., group = 1), fill = "#1b9e77") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  theme(text = element_text(size = 15))

ggsave("FrequencyVaccination.png", dpi = 500, w = 9, h = 6)

ggplot(VaccinationYesNo %>% filter(is.na(c19_vaccination_status) == FALSE)) + 
  geom_bar(aes(x = c19_vaccination_status, y = ..prop.., group = 1), fill = "#1b9e77") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  theme(text = element_text(size = 15))

ggsave("FrequencyVaccinationNoNA.png", dpi = 500, w = 9, h = 6)

# Vaccination Supplier ----------------------------------------------------


vaccinationData <- raw_data %>% select(user_id, c19_vaccination_details_vaccine_dose_1, c19_vaccination_details_vaccine_dose_2, c19_vaccination_details_vaccine_dose_3, c19_vaccination_details_vaccine_dose_4)
vaccinationData <- na.omit(vaccinationData)
vaccinationData <- vaccinationData %>% pivot_longer(cols = c("c19_vaccination_details_vaccine_dose_1", "c19_vaccination_details_vaccine_dose_2", "c19_vaccination_details_vaccine_dose_3", "c19_vaccination_details_vaccine_dose_4"))
vaccinationData$value <- factor(vaccinationData$value, levels=c("BioNTech", "Moderna", "AstraZeneca", "Janssen/ Johnson & Johnson", "Gamaleya Sputnik V", "Andere", "Ich möchte nicht antworten", "Nicht zutreffend"))
ggplot(vaccinationData) + 
  geom_bar(aes(x = value, y = ..prop.., group = 1, fill = name)) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  theme(text = element_text(size = 15))

ggsave("FrequencyVaccinationSupplier.png", dpi = 500, w = 9, h = 6)

rkiVaccinations <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/main/Deutschland_Bundeslaender_COVID-19-Impfungen.csv")
rkiVaccinations <- rkiVaccinations %>% mutate(week = isoweek(Impfdatum)) %>%
  mutate(year = year(Impfdatum))
rkiVaccinations <- rkiVaccinations %>% group_by(Impfstoff, Impfserie) %>% summarise(Anzahl = sum(Anzahl))
rkiVaccinations <- ungroup(rkiVaccinations)
rkiVaccinations$Impfserie <- as.character(rkiVaccinations$Impfserie)
rkiVaccinations <- rkiVaccinations %>% mutate(Impfstoff = case_when(Impfstoff == "Comirnaty" ~ "BioNTech",
                                                                    Impfstoff == "Comirnaty Omicron XBB.1.5" ~ "BioNTech",
                                                                    Impfstoff == "Comirnaty Original/Omicron BA.1" ~ "BioNTech",
                                                                    Impfstoff == "Comirnaty Original/Omicron BA.4-5" ~ "BioNTech",
                                                                    Impfstoff == "Comirnaty bivalent (Original/Omikron)" ~ "BioNTech",
                                                                    Impfstoff == "Comirnaty-Kleinkinder" ~ "BioNTech",
                                                                    Impfstoff == "Jcovden" ~ "Janssen/Johnson & Johnson",
                                                                    Impfstoff == "Nuvaxovid" ~ "Andere",
                                                                    Impfstoff == "Vaxzevria" ~ "AstraZeneca",
                                                                    Impfstoff == "Spikevax" ~ "Moderna",
                                                                    Impfstoff == "Spikevax bivalent (Original/Omikron)" ~ "Moderna",
                                                                    Impfstoff == "Spikevax bivalent Original/Omicron BA.1" ~ "Moderna",
                                                                    Impfstoff == "Spikevax bivalent Original/Omicron BA.4-5" ~ "Moderna",
                                                                    Impfstoff == "Valneva" ~ "Andere",
                                                                    Impfstoff == "VidPrevtyn Beta" ~ "Andere"))

Sum <- rkiVaccinations %>% group_by(Impfserie) %>% summarise(sum = sum(Anzahl))
rkiVaccinations <- rkiVaccinations %>% mutate(relAnzahl = case_when(Impfserie == 1 ~ Anzahl/64947115,
                                                                    Impfserie == 2 ~ Anzahl/61284765,
                                                                    Impfserie == 3 ~ Anzahl/52210149,
                                                                    Impfserie == 4 ~ Anzahl/13479521,
                                                                    Impfserie == 5 ~ Anzahl/3994200,
                                                                    Impfserie == 6 ~ Anzahl/808739))
rkiVaccinations$Impfstoff <- factor(rkiVaccinations$Impfstoff, levels=c("BioNTech", "Moderna", "AstraZeneca", "Janssen/Johnson & Johnson", "Andere"))
ggplot(rkiVaccinations) + 
  geom_col(aes(x= Impfstoff, y = relAnzahl, fill = Impfserie, group = 1)) +
  theme_minimal() +
  facet_wrap(~Impfserie, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(text = element_text(size = 15))

ggsave("RKIFrequencyVaccinationSupplier.png", dpi = 500, w = 9, h = 6)

# Household Size and No. of Children --------------------------------------

HouseholdData <- raw_data %>% select(user_id, hsld_size_2019_, hsld_size_03_2020_, hsld_size_summer_2021_, hsld_size_01_2023_, total_hsld_size_persons_under_14, total_hsld_size_persons_above_14, number_of_children_under_18)
HouseholdData <- na.omit(HouseholdData)
HouseholdData <- HouseholdData %>% pivot_longer(cols = c("hsld_size_2019_", "hsld_size_03_2020_", "hsld_size_summer_2021_", "hsld_size_01_2023_", "total_hsld_size_persons_under_14", "total_hsld_size_persons_above_14", "number_of_children_under_18"))

HouseholdData$name <- factor(HouseholdData$name, levels = c("hsld_size_2019_", "hsld_size_03_2020_", "hsld_size_summer_2021_", "hsld_size_01_2023_", "total_hsld_size_persons_under_14", "total_hsld_size_persons_above_14", "number_of_children_under_18"))
HouseholdData$value <- as.integer(HouseholdData$value)
ggplot(HouseholdData %>% filter(value < 10)) + geom_bar(aes(x= value, y = ..prop.., group = 1, fill = name)) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks=c(0,2,4,6,8)) +
  theme(text = element_text(size = 15))

ggsave("HouseholdSize.png", dpi = 500, w = 9, h = 5)


# Gender ------------------------------------------------------------------

GenderData <- raw_data %>% select(user_id, gender)

GenderData <- na.omit(GenderData)
GenderData$gender <- factor(GenderData$gender, levels = c("Weiblich", "Männlich", "Divers", "Ich möchte nicht antworten"))
ggplot(GenderData) + 
  geom_bar(aes(x= gender, y = ..prop.., group = 1), fill = "#1b9e77") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  theme(text = element_text(size = 15))

ggsave("Gender.png", dpi = 500, w = 9, h = 4.5)

AgeData <- raw_data %>% select(user_id, year_of_birth)
AgeData <- AgeData %>% mutate(age = 2023-year_of_birth)
AgeData <- na.omit(AgeData)
ggplot(AgeData) + 
  geom_bar(aes(x= age, y = ..prop.., group = 1), fill = "#1b9e77") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  theme(text = element_text(size = 15))

ggsave("Age.png", dpi = 500, w = 9, h = 4.5)

# Comorbidities -----------------------------------------------------------

comorbidities <- raw_data %>% select(user_id, cond_hbp, cond_diabetes, cond_cardio, cond_resp, cond_immuno, cond_cancer, cond_post_c19, cond_none)
comorbidities <- comorbidities %>% pivot_longer(cols=c(cond_hbp, cond_diabetes, cond_cardio, cond_resp, cond_immuno, cond_cancer, cond_post_c19, cond_none))
ggplot(comorbidities  %>% filter(!is.na(value))) + 
  geom_bar(aes(x= value, y = ..prop.., group = 1), fill = "#1b9e77") +
  theme_minimal() +
  facet_wrap(~name)+
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  theme(text = element_text(size = 15)) 

smoking <- raw_data %>% select(user_id, smoking_status)
smoking$smoking_status <- factor(smoking$smoking_status, levels = c("Ich habe noch nie geraucht.", "Nein, nicht mehr", "Ja, gelegentlich", "Ja, täglich", "Ich möchte nicht antworten"))
ggplot(smoking  %>% filter(!is.na(smoking_status))) + 
  geom_bar(aes(x= smoking_status, y = ..prop.., group = 1), fill = "#1b9e77") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  theme(text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Education / Occupation --------------------------------------------------

educationLevel <- raw_data %>% select(user_id, highest_educational_qualification, current_occupation)
educationLevel$highest_educational_qualification <- factor(educationLevel$highest_educational_qualification, levels = c("Haupt-/ Volksschulabschluss", "Realschulabschluss", "Abitur / Fachhochschulabitur", "Anderer"))
ggplot(educationLevel %>% filter(!is.na(highest_educational_qualification))) + 
  geom_bar(aes(x= highest_educational_qualification, y = ..prop.., group = 1), fill = "#1b9e77") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  theme(text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

educationLevel$current_occupation <- factor(educationLevel$current_occupation, levels = c("Ich bin als Lehrer:in oder Erzieher:in tätig.", "Ich bin in einem medizinischen oder pflegerischen Beruf bei einem Gesundheitsversorger tätig.", "Ich bin im Studium oder in der Ausbildung.", "Ich bin Rentner:in oder Pensionär:in.", "Andere (z.B. Elternzeit, Sabbatical)", "Ich bin in einem anderen Beruf tätig.", "Ich möchte nicht antworten"))
ggplot(educationLevel %>% filter(!is.na(current_occupation))) + 
  geom_bar(aes(x= current_occupation, y = ..prop.., group = 1), fill = "#1b9e77") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  theme(text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Infection Context -------------------------------------------------------



# Behavior Change ---------------------------------------------------------

behaviorChange <- raw_data %>% select(user_id, beh_change_start_pandemic_avoid_in_person, beh_change_start_pandemic_avoid_careless_contacts, beh_change_start_pandemic_contact_cautious_people,
                                      beh_change_start_pandemic_avoid_peak_hours, beh_change_start_pandemic_maintain_distance,
                                      beh_change_start_pandemic_outdoor_only, beh_change_start_pandemic_no_visit_high_risk, beh_change_start_pandemic_avoid_busy_places,
                                      beh_change_start_pandemic_avoid_public_trans, beh_change_start_pandemic_mask_public_trans, beh_change_start_pandemic_mask_supermarket, beh_change_start_pandemic_work_from_home, 
                                      beh_change_start_pandemic_children_limited_contacts, beh_change_start_pandemic_meet_close_despite_restrict)
behaviorChange <- behaviorChange %>% pivot_longer(cols = c("beh_change_start_pandemic_avoid_in_person", "beh_change_start_pandemic_avoid_careless_contacts", "beh_change_start_pandemic_contact_cautious_people",
                                                           "beh_change_start_pandemic_avoid_peak_hours", "beh_change_start_pandemic_maintain_distance",
                                                           "beh_change_start_pandemic_outdoor_only", "beh_change_start_pandemic_no_visit_high_risk", "beh_change_start_pandemic_avoid_busy_places",
                                                           "beh_change_start_pandemic_avoid_public_trans", "beh_change_start_pandemic_mask_public_trans", "beh_change_start_pandemic_mask_supermarket", "beh_change_start_pandemic_work_from_home", 
                                                           "beh_change_start_pandemic_children_limited_contacts", "beh_change_start_pandemic_meet_close_despite_restrict"))

behaviorChange <- behaviorChange %>% mutate(name = case_when(name == "beh_change_start_pandemic_avoid_in_person" ~ "avoid in person meetings", 
                                                             name == "beh_change_start_pandemic_avoid_careless_contacts" ~ "avoid careless contacts",
                                                             name == "beh_change_start_pandemic_contact_cautious_people" ~ "meet similarly cautious people",
                                                             name == "beh_change_start_pandemic_avoid_peak_hours" ~ "no shopping during peak hours", 
                                                             name == "beh_change_start_pandemic_maintain_distance" ~ "keep 1.5m distance to others",
                                                             name == "beh_change_start_pandemic_outdoor_only" ~ "only meet outdoors", 
                                                             name == "beh_change_start_pandemic_no_visit_high_risk" ~ "avoid visiting of high-risk persons", 
                                                             name == "beh_change_start_pandemic_avoid_busy_places" ~ "avoid crowded places",
                                                             name == "beh_change_start_pandemic_avoid_public_trans" ~ "avoid public transport", 
                                                             name == "beh_change_start_pandemic_mask_public_trans" ~ "wear mask on public transport", 
                                                             name == "beh_change_start_pandemic_mask_supermarket" ~ "wear mask in supermarket", 
                                                             name == "beh_change_start_pandemic_work_from_home" ~ "work from home", 
                                                             name == "beh_change_start_pandemic_children_limited_contacts" ~ "limit children's contacts", 
                                                             name == "beh_change_start_pandemic_meet_close_despite_restrict" ~ "meet close contacts despite restrictions"))

behaviorChange$value <- factor(behaviorChange$value, levels = c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))

ggplot(behaviorChange %>% filter(!is.na(value))) + 
  geom_bar(aes(x= value, y = ..prop.., group = 1), fill = "#1b9e77") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("") +
  facet_wrap(~name)+
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  theme(text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("BehaviorChange.png", dpi = 500, w = 12, h = 9)
ggsave("BehaviorChange.pdf", dpi = 500, w = 12, h = 9)


# Attitudes ---------------------------------------------------------------

attitudes <- raw_data %>% select(user_id, attitudes_precautions_mar2020_low_infection_risk_perception,
                                 attitudes_precautions_mar2020_risky_infection_course_assessment, attitudes_precautions_mar2020_high_risk_perception, 
                                 attitudes_precautions_mar2020_avoided_risky_situations, attitudes_precautions_mar2020_aware_distance_rule_effectiveness, 
                                 attitudes_precautions_mar2020_understood_mask_reduces_risk, attitudes_precautions_mar2020_followed_measures, 
                                 attitudes_precautions_mar2020_felt_restricted_by_measures, attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical)

attitudes <- attitudes %>% pivot_longer(cols = c("attitudes_precautions_mar2020_low_infection_risk_perception",
                                                 "attitudes_precautions_mar2020_risky_infection_course_assessment", "attitudes_precautions_mar2020_high_risk_perception", 
                                                 "attitudes_precautions_mar2020_avoided_risky_situations", "attitudes_precautions_mar2020_aware_distance_rule_effectiveness", 
                                                 "attitudes_precautions_mar2020_understood_mask_reduces_risk", "attitudes_precautions_mar2020_followed_measures", 
                                                 "attitudes_precautions_mar2020_felt_restricted_by_measures", "attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical"))


attitudes <- attitudes %>% mutate(name = case_when(name == "attitudes_precautions_mar2020_low_infection_risk_perception" ~ "risk of infection",
                                  name == "attitudes_precautions_mar2020_risky_infection_course_assessment" ~ "assessed getting the infection to be risky", 
                                  name == "attitudes_precautions_mar2020_high_risk_perception" ~ "high risk perception", 
                                  name == "attitudes_precautions_mar2020_avoided_risky_situations" ~ "avoidance of risky situations", 
                                  name == "attitudes_precautions_mar2020_aware_distance_rule_effectiveness" ~ "awareness of effectivenss of distance-rel. NPIs", 
                                  name == "attitudes_precautions_mar2020_understood_mask_reduces_risk" ~ "awareness of effectiveness of masks", 
                                  name == "attitudes_precautions_mar2020_followed_measures" ~ "adherance to measures", 
                                  name == "attitudes_precautions_mar2020_felt_restricted_by_measures" ~ "felt restricted due to NPIs", 
                                  name == "attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical" ~ "wore FFP2/FFP3 instead of medical mask"))

attitudes$value <- factor(attitudes$value, levels = c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))

ggplot(attitudes %>% filter(!is.na(value))) + 
  geom_bar(aes(x= value, y = ..prop.., group = 1), fill = "#1b9e77") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("") +
  facet_wrap(~name)+
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  theme(text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("BehaviorChange.png", dpi = 500, w = 12, h = 9)
ggsave("BehaviorChange.pdf", dpi = 500, w = 12, h = 9)