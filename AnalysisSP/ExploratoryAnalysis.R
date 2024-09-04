library(tidyverse)
library(RColorBrewer)

# Author: S. Paltra, contact: paltra@tu-berlin.de

source("DataCleaningPrepForContactAnalysis.R")

# Number of infections ----------------------------------------------------

no_time_infections <- raw_data %>% select(num_c19_infs, date_f1_inf, date_s2_inf, date_t3_inf) %>%
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

# Vaccination -------------------------------------------------------------

VaccinationYesNo <- data_reduced %>% select(c19_vaccination_status, year_of_birth)
#VaccinationYesNo <- VaccinationYesNo[!is.na(VaccinationYesNo$user_id),]

VaccinationYesNo$c19_vaccination_status <- factor(VaccinationYesNo$c19_vaccination_status, levels = c("Ja", "Nein", "Weiß ich nicht", "Ich möchte nicht antworten", NA))

VaccinationYesNo <- VaccinationYesNo %>% mutate(c19_vaccination_status_eng = case_when(c19_vaccination_status == "Ja" ~ "Yes",
                                                                                        c19_vaccination_status == "Nein" ~ "No",
                                                                                        c19_vaccination_status == "Weiß ich nicht" ~ "I Don't Know",
                                                                                        c19_vaccination_status == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))

VaccinationYesNo$c19_vaccination_status_eng <- factor(VaccinationYesNo$c19_vaccination_status_eng, levels = c("Yes", "No", "I Don't Know", "I Don't Want To Answer"))

VaccinationYesNo %>% filter(!is.na(c19_vaccination_status_eng)) %>% count(c19_vaccination_status_eng) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  mutate(lci =  n - 1.96*(n*(n-1)/567)^0.5) %>%
  mutate(lci = 100/567*lci) %>%
  mutate(uci =  n + 1.96*(n*(n-1)/567)^0.5) %>%
  mutate(uci =  100/567*uci) %>%
  ggplot(aes(c19_vaccination_status_eng, percent)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.8, fill = "#998ec3") +
  geom_errorbar(aes(x=c19_vaccination_status_eng, ymin=lci, ymax=uci), color = "#542788", position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  ylab("Share [Percentage]") +
  xlab("Have you received any COVID-19 vaccine?") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 30)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))
ggsave("ShareVaccinated.png", dpi = 500, w = 9, h = 6)
ggsave("ShareVaccinated.pdf", dpi = 500, w = 9, h = 6)

VaccinationYesNo <- VaccinationYesNo %>% mutate(age_bracket = case_when(year_of_birth <= 1962 ~ "60+",
                                                                        year_of_birth > 1962 ~ "18-59"))


palette <- function() {
  c("#998ec3", "#f1a340")
}

palette2 <- function() {
  c("#542788", "#b35806")
}


VaccinationYesNo %>% filter(!is.na(c19_vaccination_status_eng)) %>% filter(!is.na(age_bracket)) %>% group_by(age_bracket) %>% count(c19_vaccination_status_eng) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  mutate(lci = case_when(age_bracket == "60+" ~ n - 1.96*(n*(n-1)/75)^0.5, 
                          age_bracket == "18-59" ~ n - 1.96*(n*(n-1)/484)^0.5)) %>%
  mutate(lci = case_when(age_bracket == "60+" ~ 100/75*lci,
                        age_bracket == "18-59" ~ 100/484*lci)) %>%
  mutate(uci =  case_when(age_bracket == "60+" ~ n + 1.96*(n*(n-1)/75)^0.5,
                          age_bracket == "18-59" ~ n + 1.96*(n*(n-1)/484)^0.5)) %>%
  mutate(uci =  case_when(age_bracket == "60+" ~ 100/75*uci,
                          age_bracket == "18-59" ~ 100/484*uci)) %>%
  ggplot(aes(c19_vaccination_status_eng, percent, fill = age_bracket)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8)+
  geom_errorbar(aes(x=c19_vaccination_status_eng, ymin=lci, ymax=uci, color=age_bracket), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  ylab("Share [Percentage]") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  xlab("Have you received any COVID-19 vaccine?") +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(text = element_text(size = 30)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))
ggsave("ShareVaccinated_AgeBracket.png", dpi = 500, w = 9, h = 6)
ggsave("ShareVaccinated_AgeBracket.pdf", dpi = 500, w = 9, h = 6)

# Vaccination Supplier ----------------------------------------------------

vaccinationData <- data_reduced %>% select(c19_vaccination_details_vaccine_dose_1, c19_vaccination_details_vaccine_dose_2, c19_vaccination_details_vaccine_dose_3, c19_vaccination_details_vaccine_dose_4)
vaccinationData <- na.omit(vaccinationData)
vaccinationData <- vaccinationData %>% pivot_longer(cols = c("c19_vaccination_details_vaccine_dose_1", "c19_vaccination_details_vaccine_dose_2", "c19_vaccination_details_vaccine_dose_3", "c19_vaccination_details_vaccine_dose_4"))
vaccinationData$value <- factor(vaccinationData$value, levels=c("BioNTech", "Moderna", "AstraZeneca", "Janssen/ Johnson & Johnson", "Gamaleya Sputnik V", "Andere", "Ich möchte nicht antworten", "Nicht zutreffend"))
vaccinationData <- vaccinationData %>% mutate(value_eng = case_when(value == "BioNTech" ~ "BioNTech",
                                                                    value == "Moderna" ~ "Moderna",
                                                                    value == "AstraZeneca" ~ "AstraZeneca", 
                                                                    value == "Janssen/ Johnson & Johnson" ~ "Janssen/Johnson & Johnson",
                                                                    value == "Gamaleya Sputnik V" ~ "Gamaleya Sputnik V",
                                                                    value == "Andere" ~ "Other",
                                                                    value == "Ich möchte nicht antworten" ~ "I Don't Want To Answer",
                                                                    value == "Nicht zutreffend" ~ "Does Not Apply"))
vaccinationData$value_eng <- factor(vaccinationData$value_eng, levels=c("BioNTech", "Moderna", "AstraZeneca", "Janssen/Johnson & Johnson", "Gamaleya Sputnik V", "Other", "I Don't Want To Answer", "Does Not Apply"))
vaccinationData <- vaccinationData %>% mutate(Impfserie = case_when(name == "c19_vaccination_details_vaccine_dose_1" ~ "1", 
                                                                    name == "c19_vaccination_details_vaccine_dose_2" ~ "2",
                                                                    name == "c19_vaccination_details_vaccine_dose_3" ~ "3",
                                                                    name == "c19_vaccination_details_vaccine_dose_4" ~ "4"))
vaccinationData$Impfserie <- factor(vaccinationData$Impfserie, levels = c("1", "2", "3", "4"))
vaccinationData <- vaccinationData %>% mutate(Source = "Survey")
vaccinationData<- vaccinationData %>% select(Impfserie, value_eng, Source)

# Compare to RKI vaccination data
rkiVaccinations <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/main/Deutschland_Bundeslaender_COVID-19-Impfungen.csv")
rkiVaccinations <- rkiVaccinations %>% mutate(value_eng = case_when(Impfstoff == "Comirnaty" ~ "BioNTech",
                                                                    Impfstoff == "Comirnaty Omicron XBB.1.5" ~ "BioNTech",
                                                                    Impfstoff == "Comirnaty Original/Omicron BA.1" ~ "BioNTech",
                                                                    Impfstoff == "Comirnaty Original/Omicron BA.4-5" ~ "BioNTech",
                                                                    Impfstoff == "Comirnaty bivalent (Original/Omikron)" ~ "BioNTech",
                                                                    Impfstoff == "Comirnaty-Kleinkinder" ~ "BioNTech",
                                                                    Impfstoff == "Jcovden" ~ "Janssen/Johnson & Johnson",
                                                                    Impfstoff == "Nuvaxovid" ~ "Other",
                                                                    Impfstoff == "Vaxzevria" ~ "AstraZeneca",
                                                                    Impfstoff == "Spikevax" ~ "Moderna",
                                                                    Impfstoff == "Spikevax bivalent (Original/Omikron)" ~ "Moderna",
                                                                    Impfstoff == "Spikevax bivalent Original/Omicron BA.1" ~ "Moderna",
                                                                    Impfstoff == "Spikevax bivalent Original/Omicron BA.4-5" ~ "Moderna",
                                                                    Impfstoff == "Valneva" ~ "Other",
                                                                    Impfstoff == "VidPrevtyn Beta" ~ "Other"))
rkiVaccinations <- rkiVaccinations %>% mutate(Source = "RKI") %>% filter(Impfserie %in% c(1,2,3,4))
rkiVaccinations <- rkiVaccinations %>% uncount(Anzahl)

rkiVaccinations <- rkiVaccinations %>% select(Impfserie, value_eng, Source)

vaccinationData <- rbind(vaccinationData, rkiVaccinations)

vaccinationData <- vaccinationData %>% mutate(vaccineNo = case_when(Impfserie == "1" ~ "1st COVID-19 Vaccination",
                                                                    Impfserie == "2" ~ "2nd COVID-19 Vaccination",
                                                                    Impfserie == "3" ~ "3rd COVID-19 Vaccination",
                                                                    Impfserie == "4" ~ "4th COVID-19 Vaccination"
                                                                    ))


vaccinationData %>% filter(value_eng != "Does Not Apply") %>% filter(value_eng != "I Don't Want To Answer") %>% group_by(vaccineNo, Source) %>% count(value_eng) %>%
                    mutate(percent = 100 * n / sum(n)) %>% mutate(percent = round(percent, digits = 2)) %>%
ggplot(aes(value_eng, percent)) +
  geom_bar(aes(fill = Source), stat = "identity", position = "dodge", width = 0.8) +
  geom_text(aes(label = percent, 
                  y = percent, 
                  group = Source),
              position = position_dodge(width = 0.8),
              vjust = -1, size = 6) +
  theme_minimal() +
  facet_wrap(~vaccineNo, nrow=2) +
  ylab("Share [Percentage]") +
  scale_y_continuous(limits=c(0,110), labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  xlab("Vaccination Supplier") +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(text = element_text(size = 30)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

ggsave("ShareVaccinationSupplier.pdf", dpi = 500, w = 21, h = 15)
ggsave("ShareVaccinationSupplier.png", dpi = 500, w = 21, h = 15)

# How many respondents are vaccinated? How often have they been vaccinated?

vaccinationData <- data_reduced %>% select(c19_vaccination_details_vaccine_dose_1, c19_vaccination_details_vaccine_dose_2, c19_vaccination_details_vaccine_dose_3, c19_vaccination_details_vaccine_dose_4)

vaccinationData <- vaccinationData %>% mutate(dose_1_received = case_when(c19_vaccination_details_vaccine_dose_1 == "BioNTech" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_1 == "Moderna" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_1 == "Janssen/ Johnson & Johnson" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_1 == "AstraZeneca" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_1 == "Andere" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_1 == "Nicht zutreffend" ~ "No",
                                                                          c19_vaccination_details_vaccine_dose_1 == "Ich möchte nicht antworten" ~ "I Don't Want To Answer",
                                                                          .default =  "NA")) %>%
                                        mutate(dose_2_received = case_when(c19_vaccination_details_vaccine_dose_2 == "BioNTech" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_2 == "Moderna" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_2 == "Janssen/ Johnson & Johnson" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_2 == "AstraZeneca" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_2 == "Andere" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_2 == "Nicht zutreffend" ~ "No",
                                                                          c19_vaccination_details_vaccine_dose_2 == "Ich möchte nicht antworten" ~ "I Don't Want To Answer",
                                                                          .default =  "NA")) %>%
                                        mutate(dose_3_received = case_when(c19_vaccination_details_vaccine_dose_3 == "BioNTech" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_3 == "Moderna" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_3 == "Janssen/ Johnson & Johnson" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_3 == "AstraZeneca" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_3 == "Andere" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_3 == "Nicht zutreffend" ~ "No",
                                                                          c19_vaccination_details_vaccine_dose_3 == "Ich möchte nicht antworten" ~ "I Don't Want To Answer",
                                                                          .default =  "NA")) %>%
                                        mutate(dose_4_received = case_when(c19_vaccination_details_vaccine_dose_4 == "BioNTech" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_4 == "Moderna" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_4 == "Janssen/ Johnson & Johnson" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_4 == "AstraZeneca" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_4 == "Andere" ~ "Yes",
                                                                          c19_vaccination_details_vaccine_dose_4 == "Nicht zutreffend" ~ "No",
                                                                          c19_vaccination_details_vaccine_dose_4 == "Ich möchte nicht antworten" ~ "I Don't Want To Answer",
                                                                          .default =  "NA"))

vaccinationData$dose_1_received <- factor(vaccinationData$dose_1_received, levels = c("Yes", "No", NA, "I Don't Want To Answer"))
vaccinationData$dose_2_received <- factor(vaccinationData$dose_2_received, levels = c("Yes", "No", NA, "I Don't Want To Answer"))
vaccinationData$dose_3_received <- factor(vaccinationData$dose_3_received, levels = c("Yes", "No", NA, "I Don't Want To Answer"))
vaccinationData$dose_4_received<- factor(vaccinationData$dose_4_received, levels = c("Yes", "No", NA, "I Don't Want To Answer"))

# RKI data: https://impfdashboard.de/
data1st <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(data1st) <- c("dose_1_received", "n", "percent", "source")
data1st [nrow(data1st)+1,] <- c("Yes", 64900000, 100*64900000/78000000, "RKI")
data1st [nrow(data1st)+1,] <- c("No", 13100000, 100*13100000/78000000, "RKI")
data1st [nrow(data1st)+1,] <- c("I Don't Want To Answer", 0, 0, "RKI")
data1st [nrow(data1st)+1,] <- c("No", 0, 0, "Survey")
data1st$n <- as.integer(data1st$n)
data1st$percent <- as.double(data1st$percent)

data2nd <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(data2nd) <- c("dose_2_received", "n", "percent", "source")
data2nd [nrow(data2nd)+1,] <- c("Yes", 63600000, 100*63600000/78000000, "RKI")
data2nd [nrow(data2nd)+1,] <- c("No", 14400000,100*14400000/78000000, "RKI")
data2nd [nrow(data2nd)+1,] <- c("I Don't Want To Answer", 0, 0, "RKI")
data2nd$n <- as.integer(data2nd$n)
data2nd$percent <- as.double(data2nd$percent)

data3rd <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(data3rd) <- c("dose_3_received", "n", "percent", "source")
data3rd [nrow(data3rd)+1,] <- c("Yes", 52100000, 100*52100000/78000000, "RKI")
data3rd [nrow(data3rd)+1,] <- c("No", 25900000, 100*25900000/78000000, "RKI")
data3rd [nrow(data3rd)+1,] <- c("I Don't Want To Answer", 0, 0, "RKI")
data3rd$n <- as.integer(data3rd$n)
data3rd$percent <- as.double(data3rd$percent)

data4th <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(data4th) <- c("dose_4_received", "n", "percent", "source")
data4th [nrow(data4th)+1,] <- c("Yes", 1200000, 100*1200000/78000000, "RKI")
data4th [nrow(data4th)+1,] <- c("No", 76800000, 100*76800000/78000000, "RKI")
data4th [nrow(data4th)+1,] <- c("I Don't Want To Answer", 0, 0, "RKI")
data4th$n <- as.integer(data4th$n)
data4th$percent <- as.double(data4th$percent)

doses <- c("1st", "2nd", "3rd", "4th")

palette <- function() {
  c("#998ec3", "#f1a340")
}

for(dose in doses){
  if(dose == "1st"){
    column <- "dose_1_received"
    data <- data1st
  }else if(dose == "2nd"){
    column <- "dose_2_received"
    data <- data2nd
  }else if(dose == "3rd"){
    column <- "dose_3_received"
    data <- data3rd
  }else if(dose == "4th"){
    column <- "dose_4_received"
    data <- data4th
  }

  vaccinationData %>% filter(!is.na(!!sym(column)))  %>% count(!!sym(column)) %>%
    mutate(percent = 100 * n / sum(n)) %>% mutate(source = "Survey") %>%
    rbind(data) %>%
    mutate(percent = round(percent, digits = 2)) %>%
    # mutate(lci = case_when(age_bracket == "60+" ~ n - 1.96*(n*(n-1)/75)^0.5, 
    #                         age_bracket == "18-59" ~ n - 1.96*(n*(n-1)/484)^0.5)) %>%
    # mutate(lci = case_when(age_bracket == "60+" ~ 100/75*lci,
    #                       age_bracket == "18-59" ~ 100/484*lci)) %>%
    # mutate(uci =  case_when(age_bracket == "60+" ~ n + 1.96*(n*(n-1)/75)^0.5,
    #                         age_bracket == "18-59" ~ n + 1.96*(n*(n-1)/484)^0.5)) %>%
    # mutate(uci =  case_when(age_bracket == "60+" ~ 100/75*uci,
    #                         age_bracket == "18-59" ~ 100/484*uci)) %>%
    ggplot(aes(!!sym(column), percent)) +
    geom_bar(aes(fill=source), stat = "identity", position = "dodge", width = 0.8)+
    geom_text(aes(label = percent, 
                  y = percent, 
                  group = source),
              position = position_dodge(width = 0.8),
              vjust = -1, size = 6) +
    #geom_errorbar(aes(x=dose_2_received, ymin=lci, ymax=uci, color=age_bracket), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
    theme_minimal() +
    ylab("Share [Percentage]") +
    scale_y_continuous(limits=c(0,110), labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
    xlab(paste0("Have you received a ", dose, " dose \nof a COVID-19-vaccine?")) +
    scale_fill_manual(values = palette()) +
    scale_color_manual(values = palette2()) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    theme(text = element_text(size = 30)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
    theme(axis.ticks.x = element_line(),
          axis.ticks.y = element_line(),
          axis.ticks.length = unit(5, "pt"))

    ggsave(paste0("PercentageVaccinated", dose, "dose.pdf"), dpi = 500, w = 9, h = 9)
    ggsave(paste0("PercentageVaccinated", dose, "dose.png"), dpi = 500, w = 9, h = 9)
}


# Household Size and No. of Children --------------------------------------

HouseholdData <- data_reduced %>% select(respondent_hsld_size_2019, respondent_hsld_size_03_2020, respondent_hsld_size_summer_2021, respondent_hsld_size_01_2023, respondent_hsld_size_persons_under_14, number_of_children_under_18)
HouseholdData <- HouseholdData %>% pivot_longer(cols = c("respondent_hsld_size_2019", "respondent_hsld_size_03_2020", "respondent_hsld_size_summer_2021", "respondent_hsld_size_01_2023", "respondent_hsld_size_persons_under_14", "number_of_children_under_18"))

#HouseholdData$name <- factor(HouseholdData$name, levels = c("hsld_size_2019_", "hsld_size_03_2020_", "hsld_size_summer_2021_", "hsld_size_01_2023_", "total_hsld_size_persons_under_14", "total_hsld_size_persons_above_14", "number_of_children_under_18"))
HouseholdData <- HouseholdData %>% mutate(value = case_when(value == 1 ~ "1", value == 2 ~ "2", value == 3 ~ "3", value == 4 ~ "4", value >= 5 ~ "5+"))
HouseholdData <- HouseholdData %>% mutate(name = case_when(name == "respondent_hsld_size_2019" ~ "Household size 2019",
                                                          name == "respondent_hsld_size_03_2020" ~ "Household size 3/20",
                                                          name == "respondent_hsld_size_summer_2021" ~ "Household size Summer/21",
                                                          name == "respondent_hsld_size_01_2023" ~ "Household size 1/23",
                                                          name == "respondent_hsld_size_persons_under_14" ~ "Children < 14 in household",
                                                          name == "number_of_children_under_18" ~ "# Children < 18"))
HouseholdData$name <- factor(HouseholdData$name, levels = c("Household size 2019","Household size 3/20","Household size Summer/21","Household size 1/23","Children < 14 in household","Persons > 14 in household","# Children < 18"))

#https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Haushalte-Familien/Tabellen/1-1-privathaushalte-haushaltsmitglieder.html
HouseholdDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(HouseholdDataStatBundesamt) <- c("name", "value", "n", "percent", "Source")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "1", 1, 41.1, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "1", 1, 41.1, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "1", 1, 41.1, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "1", "X", 41.1, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "2", 1, 33.5, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "2", 1, 33.5, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "2", 1, 33.5, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "2", 1, 33.5, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "3", 1, 11.9, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "3", 1, 11.9, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "3", 1, 11.9, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "3", 1, 11.9, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "4", 1, 9.5, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "4", 1, 9.5, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "4", 1, 9.5, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "4", 1, 9.5, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "5+", 1, 3.9, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "5+", 1, 3.9, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "5+", 1, 3.9, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "5+", 1, 3.9, "Statistisches Bundesamt (2023)")
HouseholdDataStatBundesamt$n <- as.integer(HouseholdDataStatBundesamt$n)
HouseholdDataStatBundesamt$percent <- as.double(HouseholdDataStatBundesamt$percent)
HouseholdDataStatBundesamt$name <- factor(HouseholdDataStatBundesamt$name, levels = c("Household size 2019","Household size 3/20","Household size Summer/21","Household size 1/23","Children < 14 in household","Persons > 14 in household","# Children < 18"))


palette <- function() {
  c("#998ec3", "#f1a340")
}

HouseholdData %>% filter(name != "Children < 14 in household") %>%
                  filter(name != "# Children < 18") %>%  filter(!is.na(name)) %>% filter(!is.na(value)) %>% 
                  group_by(name) %>% count(value) %>% mutate(percent = 100 * n / sum(n)) %>% mutate(Source = "Survey") %>% 
                  rbind(HouseholdData, HouseholdDataStatBundesamt) %>%  filter(name == "Household size 1/23") %>% filter(!is.na(Source)) %>%
ggplot(aes(value, percent)) +
  geom_bar(aes(fill=Source), stat = "identity", position = "dodge", width = 0.8) +
  theme_minimal() +
  #facet_wrap(~name, nrow=2) +
  ylab("Share [Percentage]") +
  xlab("Household size [# Members]") +
  scale_fill_manual(values = palette()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

ggsave("HouseholdSize.png", dpi = 500, w = 9, h = 6)
ggsave("HouseholdSize.pdf", dpi = 500, w = 9, h = 6)


# Gender ------------------------------------------------------------------

GenderData <- data_reduced %>% select(gender)

GenderData <- GenderData %>% mutate(gender = case_when(gender == "Weiblich" ~ "female", 
                                                        gender == "Männlich" ~ "male",
                                                        gender == "Divers" ~ "diverse",
                                                        gender == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))

GenderData$gender <- factor(GenderData$gender, levels = c("female", "male", "diverse", "I Don't Want To Answer"))

GenderDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(GenderDataStatBundesamt) <- c("gender", "n", "percent", "Source")
GenderDataStatBundesamt[nrow(GenderDataStatBundesamt) + 1, ] <- c("female",42885791, 100*42885791/84669326, "Statistisches Bundesamt (2023)")
GenderDataStatBundesamt[nrow(GenderDataStatBundesamt) + 1, ] <- c("male",	41783535, 100*41783535/84669326, "Statistisches Bundesamt (2023)")
GenderDataStatBundesamt$gender <- factor(GenderDataStatBundesamt$gender, levels = c("female", "male"))
GenderDataStatBundesamt$n <- as.integer(GenderDataStatBundesamt$n)
GenderDataStatBundesamt$percent <- as.double(GenderDataStatBundesamt$percent)

GenderData %>% filter(gender != "diverse") %>%
                  filter(gender != "I Don't Want To Answer") %>% 
                  count(gender) %>% mutate(percent = 100 * n / sum(n)) %>% mutate(Source = "Survey") %>% 
                  rbind(GenderDataStatBundesamt) %>%
ggplot(aes(gender, percent)) +
  geom_bar(aes(fill=Source), stat = "identity", position = "dodge", width = 0.8) +
  theme_minimal() +
  #facet_wrap(~name, nrow=2) +
  ylab("Share [Percentage]") +
  xlab("Gender") +
  scale_fill_manual(values = palette()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

ggsave("Gender_Comparison.pdf", dpi = 500, w = 9, h = 6)
ggsave("Gender_Comparison.png", dpi = 500, w = 9, h = 6)


ggplot(GenderData) + 
  geom_bar(aes(x= gender, y = ..prop.., group = 1), fill = "#1b9e77") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  theme(text = element_text(size = 15))

ggsave("Gender.png", dpi = 500, w = 9, h = 4.5)

# Age ---------------------------------------------------------------------

AgeData <- data_reduced %>% select(year_of_birth) %>% mutate(age = 2023-year_of_birth) %>%
          mutate(age_bracket = case_when(age < 20 ~ "Below 20 (*)",
                                        age < 40 ~ "20-39",
                                        age < 60 ~ "40-59",
                                        age < 80 ~ "60-79",
                                        age < 100 ~ "80-99")) 
AgeData$age_bracket <- factor(AgeData$age_bracket, levels = c("Below 20 (*)", "20-39", "40-59", "60-79", "80-99"))

# Data from https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/bevoelkerung-altersgruppen-deutschland.html
AgeDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(AgeDataStatBundesamt) <- c("age_bracket", "n", "percent", "source")
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("Below 20 (*)", 84669326*0.188, 18.8, "Statistisches Bundesamt (2023)")
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("20-39", 84669326*0.245, 24.5, "Statistisches Bundesamt (2023)")
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("40-59", 84669326*0.268, 26.8, "Statistisches Bundesamt (2023)")
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("60-79", 84669326*0.226, 22.6, "Statistisches Bundesamt (2023)")
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("80-99", 84669326*0.072, 7.2, "Statistisches Bundesamt (2023)")
AgeDataStatBundesamt$n <- as.integer(AgeDataStatBundesamt$n)
AgeDataStatBundesamt$percent <- as.double(AgeDataStatBundesamt$percent)
AgeDataStatBundesamt$age_bracket <- factor(AgeDataStatBundesamt$age_bracket, levels = c("Below 20 (*)", "20-39", "40-59", "60-79", "80-99"))


AgeData %>% filter(!is.na(age_bracket)) %>% count(age_bracket) %>% 
            mutate(percent = 100 * n / sum(n)) %>% 
            mutate(source = "Survey") %>%
            rbind(AgeDataStatBundesamt) %>%
ggplot(aes(age_bracket, percent)) +
  geom_bar(aes(fill=source), stat = "identity", position = "dodge", width = 0.8) +
  theme_minimal() +
  #facet_wrap(~name, nrow=2) +
  ylab("Share [Percentage]") +
  xlab("Age Bracket (2023)") +
  scale_fill_manual(values = palette()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

ggsave("Age_Comparison.pdf", dpi = 500, w = 9, h = 6)
ggsave("Age_Comparison.png", dpi = 500, w = 9, h = 6)


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

ggsave("Comorbidities.png", dpi = 500, w = 9, h = 4.5)

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

ggsave("Smoking.png", dpi = 500, w = 9, h = 4.5)

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

ggsave("Education.png", dpi = 500, w = 9, h = 4.5)

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

ggsave("CurrentOccupation.png", dpi = 500, w = 12, h = 12)

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

# Infection context -------------------------------------------------------

ownInfection <- raw_data %>% select(user_id, loc_home, loc_work, loc_school, loc_friends,
                                    loc_sport_event, loc_health_fac, loc_shopping, loc_party,
                                    loc_conference, loc_unknown, loc_no_info, loc_other)

ownInfection <- ownInfection %>% pivot_longer(cols = c("loc_home", "loc_work", "loc_school", "loc_friends",
                                                       "loc_sport_event", "loc_health_fac", "loc_shopping", "loc_party",
                                                       "loc_conference", "loc_unknown", "loc_no_info", "loc_other"))

ownInfection <- na.omit(ownInfection)
ownInfection <- ownInfection %>% filter(value != "Nein")

ownInfection %>% count(name)

householdInfections <- raw_data %>% select(user_id, inf_locs_hsld_mbrs_home, inf_locs_hsld_mbrs_workplace, 
                                           inf_locs_hsld_mbrs_school, inf_locs_hsld_mbrs_friends_meetings,
                                           inf_locs_hsld_mbrs_sports_event, inf_locs_hsld_mbrs_shopping,
                                           inf_locs_hsld_mbrs_party, inf_locs_hsld_mbrs_conference,
                                           inf_locs_hsld_mbrs_health_facility, inf_locs_hsld_mbrs_childcare,
                                           inf_locs_hsld_mbrs_vacation, inf_locs_hsld_mbrs_public_transport,
                                           inf_locs_hsld_mbrs_restaurants_bars, inf_locs_hsld_mbrs_unknown,
                                           inf_locs_hsld_mbrs_no_information, inf_locs_hsld_mbrs_other)

householdInfections <- householdInfections %>% pivot_longer(cols = c("inf_locs_hsld_mbrs_home", "inf_locs_hsld_mbrs_workplace", 
                                                                     "inf_locs_hsld_mbrs_school", "inf_locs_hsld_mbrs_friends_meetings",
                                                                     "inf_locs_hsld_mbrs_sports_event", "inf_locs_hsld_mbrs_shopping",
                                                                     "inf_locs_hsld_mbrs_party", "inf_locs_hsld_mbrs_conference",
                                                                     "inf_locs_hsld_mbrs_health_facility", "inf_locs_hsld_mbrs_childcare",
                                                                     "inf_locs_hsld_mbrs_vacation", "inf_locs_hsld_mbrs_public_transport",
                                                                     "inf_locs_hsld_mbrs_restaurants_bars", "inf_locs_hsld_mbrs_unknown",
                                                                     "inf_locs_hsld_mbrs_no_information", "inf_locs_hsld_mbrs_other"))

householdInfections <- na.omit(householdInfections)
householdInfections <- householdInfections %>% filter(value != "Nein")

householdInfections %>% count(name)


# Change of CC ------------------------------------------------------------

ChangeOfCC <- raw_data %>% select(cc_change_during_pandemic, reasons_change_conts_op)

ChangeOfCC <- ChangeOfCC %>% mutate(cc_change_during_pandemic = case_when(cc_change_during_pandemic == "Ja" ~ "Yes",
                                                                          cc_change_during_pandemic == "Nein" ~ "No"))
ChangeOfCC$cc_change_during_pandemic <- factor(ChangeOfCC$cc_change_during_pandemic, levels = c("Yes", "No"))
ggplot(ChangeOfCC %>% filter(is.na(cc_change_during_pandemic) == FALSE)) + 
  geom_bar(aes(cc_change_during_pandemic, y = ..prop.., group = 1), fill = "#1b9e77") +
  theme_minimal() +
  xlab("Did you change your CC \n during the pandemic?") +
  ylab("Share") +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave("ChangeOfCC.png", dpi = 500, w = 6, h = 3)
ggsave("ChangeOfCC.pdf", dpi = 500, w = 6, h = 3)

YesChangeOfCC <- ChangeOfCC %>% filter(cc_change_during_pandemic == "Yes")
unique(YesChangeOfCC$reasons_change_conts_op)
