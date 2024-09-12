library(tidyverse)
library(RColorBrewer)

# Author: S. Paltra, contact: paltra@tu-berlin.de

source("DataCleaningPrepForContactAnalysis.R")

MuSPAD <- read_delim("ADD PATH")
count_na <- function(row) {
  sum(grepl("^s22_", names(row)) & !is.na(row))
}
MuSPAD$count_na <- apply(MuSPAD, 1, count_na)
MuSPAD <- MuSPAD %>% filter(count_na != 0) # We are excluding all participants who did not answer anything in the s22 survey

# Number of infections ----------------------------------------------------

palette <- function() {
  c("#FFD269", "#ECA400", "#006992")
}

data_reduced <- data_reduced %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "0",
                                                                    num_c19_infs == "Einmal" ~ "1",
                                                                    num_c19_infs == "Zweimal" ~ "2+",
                                                                    num_c19_infs == "Dreimal" ~ "2+",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "2+",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

data_reduced$num_c19_infs_eng <- factor(data_reduced$num_c19_infs_eng, levels = c("0", "1", "2+"))

InfectionsMuspad <- MuSPAD %>% select(w22_positive_test) %>% count(w22_positive_test)
InfectionsMuspad <- InfectionsMuspad %>% filter(!is.na(w22_positive_test))

InfectionsDataMuspad <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(InfectionsDataMuspad) <- c("num_c19_infs_eng", "n", "percent", "Source")
InfectionsDataMuspad[nrow(InfectionsDataMuspad) + 1, ] <- c("0", (InfectionsMuspad %>% filter(w22_positive_test=="Nie"))$n, 100*(InfectionsMuspad %>% filter(w22_positive_test=="Nie"))$n/sum(InfectionsMuspad$n), "MuSPAD")
InfectionsDataMuspad[nrow(InfectionsDataMuspad) + 1, ] <- c("1",	(InfectionsMuspad %>% filter(w22_positive_test=="Einmal"))$n, 100*(InfectionsMuspad %>% filter(w22_positive_test=="Einmal"))$n/sum(InfectionsMuspad$n), "MuSPAD")
InfectionsDataMuspad[nrow(InfectionsDataMuspad) + 1, ] <- c("2+", (InfectionsMuspad %>% filter(w22_positive_test=="Zweimal"))$n+(InfectionsMuspad %>% filter(w22_positive_test=="Dreimal"))$n+(InfectionsMuspad %>% filter(w22_positive_test=="mehr als dreimal"))$n, 100*((InfectionsMuspad %>% filter(w22_positive_test=="Zweimal"))$n+(InfectionsMuspad %>% filter(w22_positive_test=="Dreimal"))$n+(InfectionsMuspad %>% filter(w22_positive_test=="mehr als dreimal"))$n)/sum(InfectionsMuspad$n), "MuSPAD")
InfectionsDataMuspad$num_c19_infs_eng <- factor(InfectionsDataMuspad$num_c19_infs_eng, levels = c("0", "1", "2+"))
InfectionsDataMuspad$n <- as.integer(InfectionsDataMuspad$n)
InfectionsDataMuspad$percent <- as.double(InfectionsDataMuspad$percent)

InfectionsDataTwitter <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(InfectionsDataTwitter) <- c("num_c19_infs_eng", "n", "percent", "Source")
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("0", 1191, 28.25, "Twitter")
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("1",	2310, 54.77, "Twitter")
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("2+", 716, 16.98, "Twitter")
InfectionsDataTwitter$num_c19_infs_eng <- factor(InfectionsDataTwitter$num_c19_infs_eng, levels = c("0", "1", "2+"))
InfectionsDataTwitter$n <- as.integer(InfectionsDataTwitter$n)
InfectionsDataTwitter$percent <- as.double(InfectionsDataTwitter$percent)

data_reduced %>% filter(num_c19_infs_eng != "I Don't Want To Answer") %>%
  count(num_c19_infs_eng) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  mutate(Source = "Survey") %>%
  rbind(InfectionsDataMuspad) %>%
  rbind(InfectionsDataTwitter) %>%
  ggplot(aes(num_c19_infs_eng, percent)) +
  geom_bar(aes(fill=factor(Source, levels = c("Twitter", "Survey", "MuSPAD"))), stat = "identity", position = "dodge", width = 0.8) +
  theme_minimal() +
  #facet_wrap(~name, nrow=2) +
  ylab("Share [Percentage]") +
  xlab("No. Of Infections") +
  scale_fill_manual(values = palette()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) 

ggsave("NoInfections_Comparison.pdf", dpi = 500, w = 9.5, h = 6)
ggsave("NoInfections_Comparison.png", dpi = 500,  w = 9.5, h = 6)


# Vaccination -------------------------------------------------------------

# VaccinationYesNo <- data_reduced %>% select(c19_vaccination_status, year_of_birth)
# #VaccinationYesNo <- VaccinationYesNo[!is.na(VaccinationYesNo$user_id),]

# VaccinationYesNo$c19_vaccination_status <- factor(VaccinationYesNo$c19_vaccination_status, levels = c("Ja", "Nein", "Weiß ich nicht", "Ich möchte nicht antworten", NA))

# VaccinationYesNo <- VaccinationYesNo %>% mutate(c19_vaccination_status_eng = case_when(c19_vaccination_status == "Ja" ~ "Yes",
#                                                                                         c19_vaccination_status == "Nein" ~ "No",
#                                                                                         c19_vaccination_status == "Weiß ich nicht" ~ "I Don't Know",
#                                                                                         c19_vaccination_status == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))

# VaccinationYesNo$c19_vaccination_status_eng <- factor(VaccinationYesNo$c19_vaccination_status_eng, levels = c("Yes", "No", "I Don't Know", "I Don't Want To Answer"))

# VaccinationYesNo %>% filter(!is.na(c19_vaccination_status_eng)) %>% count(c19_vaccination_status_eng) %>%
#   mutate(percent = 100 * n / sum(n)) %>%
#   mutate(lci =  n - 1.96*(n*(n-1)/567)^0.5) %>%
#   mutate(lci = 100/567*lci) %>%
#   mutate(uci =  n + 1.96*(n*(n-1)/567)^0.5) %>%
#   mutate(uci =  100/567*uci) %>%
#   ggplot(aes(c19_vaccination_status_eng, percent)) + 
#   geom_bar(stat = "identity", position = "dodge", width = 0.8, fill = "#998ec3") +
#   geom_errorbar(aes(x=c19_vaccination_status_eng, ymin=lci, ymax=uci), color = "#542788", position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
#   theme_minimal() +
#   ylab("Share [Percentage]") +
#   xlab("Have you received any COVID-19 vaccine?") +
#   scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
#   theme(legend.position = "none") +
#   theme(text = element_text(size = 30)) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
#   theme(axis.ticks.x = element_line(),
#         axis.ticks.y = element_line(),
#         axis.ticks.length = unit(5, "pt"))
# ggsave("ShareVaccinated.png", dpi = 500, w = 9, h = 6)
# ggsave("ShareVaccinated.pdf", dpi = 500, w = 9, h = 6)

# VaccinationYesNo <- VaccinationYesNo %>% mutate(age_bracket = case_when(year_of_birth <= 1962 ~ "60+",
#                                                                         year_of_birth > 1962 ~ "18-59"))


# palette <- function() {
#   c("#998ec3", "#f1a340")
# }

# palette2 <- function() {
#   c("#542788", "#b35806")
# }


# VaccinationYesNo %>% filter(!is.na(c19_vaccination_status_eng)) %>% filter(!is.na(age_bracket)) %>% group_by(age_bracket) %>% count(c19_vaccination_status_eng) %>%
#   mutate(percent = 100 * n / sum(n)) %>%
#   mutate(lci = case_when(age_bracket == "60+" ~ n - 1.96*(n*(n-1)/75)^0.5, 
#                           age_bracket == "18-59" ~ n - 1.96*(n*(n-1)/484)^0.5)) %>%
#   mutate(lci = case_when(age_bracket == "60+" ~ 100/75*lci,
#                         age_bracket == "18-59" ~ 100/484*lci)) %>%
#   mutate(uci =  case_when(age_bracket == "60+" ~ n + 1.96*(n*(n-1)/75)^0.5,
#                           age_bracket == "18-59" ~ n + 1.96*(n*(n-1)/484)^0.5)) %>%
#   mutate(uci =  case_when(age_bracket == "60+" ~ 100/75*uci,
#                           age_bracket == "18-59" ~ 100/484*uci)) %>%
#   ggplot(aes(c19_vaccination_status_eng, percent, fill = age_bracket)) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.8)+
#   geom_errorbar(aes(x=c19_vaccination_status_eng, ymin=lci, ymax=uci, color=age_bracket), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
#   theme_minimal() +
#   ylab("Share [Percentage]") +
#   scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
#   xlab("Have you received any COVID-19 vaccine?") +
#   scale_fill_manual(values = palette()) +
#   scale_color_manual(values = palette2()) +
#   theme(legend.position = "bottom", legend.title = element_blank()) +
#   theme(text = element_text(size = 30)) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
#   theme(text = element_text(size = 15)) +
#   theme(axis.ticks.x = element_line(),
#         axis.ticks.y = element_line(),
#         axis.ticks.length = unit(5, "pt"))
# ggsave("ShareVaccinated_AgeBracket.png", dpi = 500, w = 9, h = 6)
# ggsave("ShareVaccinated_AgeBracket.pdf", dpi = 500, w = 9, h = 6)

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

#Compare to MuSPAD data

VaccinationSupplierMuspad <- MuSPAD %>% select(w22_vacc_type_1, w22_vacc_type_2, w22_vacc_type_3, w22_vacc_type_4)
VaccinationSupplierMuspad1 <- VaccinationSupplierMuspad %>% count(w22_vacc_type_1) %>% filter(!is.na(w22_vacc_type_1)) %>%
                                                            mutate(w22_vacc_type_1 = case_when(w22_vacc_type_1 %in% c("Andere", "Novavax", "Gamaleya Sputnik V") ~ "Other", 
                                                            .default = w22_vacc_type_1))
VaccinationSupplierMuspad2 <- VaccinationSupplierMuspad %>% count(w22_vacc_type_2) %>% filter(!is.na(w22_vacc_type_2)) %>%
                                                            mutate(w22_vacc_type_2 = case_when(w22_vacc_type_2 %in% c("Andere", "Novavax", "Gamaleya Sputnik V") ~ "Other", 
                                                            .default = w22_vacc_type_2))
VaccinationSupplierMuspad3 <- VaccinationSupplierMuspad %>% count(w22_vacc_type_3) %>% filter(!is.na(w22_vacc_type_3)) %>%
                                                            mutate(w22_vacc_type_3 = case_when(w22_vacc_type_3 %in% c("Andere", "Novavax", "Gamaleya Sputnik V") ~ "Other", 
                                                            .default = w22_vacc_type_3))
VaccinationSupplierMuspad4 <- VaccinationSupplierMuspad %>% count(w22_vacc_type_4) %>% filter(!is.na(w22_vacc_type_4)) %>%
                                                            mutate(w22_vacc_type_4 = case_when(w22_vacc_type_4 %in% c("Andere", "Novavax", "Gamaleya Sputnik V") ~ "Other", 
                                                            .default = w22_vacc_type_4))


VaccinationSupplierDataMuspad <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(VaccinationSupplierDataMuspad) <- c("vaccineNo", "Source", "value_eng", "n", "percent")
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("1st COVID-19 Vaccination", "MuSPAD", "BioNTech", (VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "BioNTech"))$n, 100*(VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "BioNTech"))$n/sum(VaccinationSupplierMuspad1$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("1st COVID-19 Vaccination", "MuSPAD", "Moderna", (VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "Moderna"))$n,100*(VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "Moderna"))$n/sum(VaccinationSupplierMuspad1$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("1st COVID-19 Vaccination", "MuSPAD", "AstraZeneca", (VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "AstraZeneca"))$n, 100*(VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "AstraZeneca"))$n/sum(VaccinationSupplierMuspad1$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("1st COVID-19 Vaccination", "MuSPAD", "Janssen/Johnson & Johnson", (VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "Janssen/ Johnson & Johnson"))$n, 100*(VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "Janssen/ Johnson & Johnson"))$n/sum(VaccinationSupplierMuspad1$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("1st COVID-19 Vaccination", "MuSPAD", "Other", sum((VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "Other"))$n), 100*sum((VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "Other"))$n)/sum(VaccinationSupplierMuspad1$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("2nd COVID-19 Vaccination", "MuSPAD", "BioNTech", (VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "BioNTech"))$n, 100*(VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "BioNTech"))$n/sum(VaccinationSupplierMuspad2$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("2nd COVID-19 Vaccination", "MuSPAD", "Moderna",  (VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "Moderna"))$n, 100* (VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "Moderna"))$n/sum(VaccinationSupplierMuspad2$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("2nd COVID-19 Vaccination", "MuSPAD", "AstraZeneca", (VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "AstraZeneca"))$n, 100*(VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "AstraZeneca"))$n/sum(VaccinationSupplierMuspad2$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("2nd COVID-19 Vaccination", "MuSPAD", "Janssen/Johnson & Johnson", (VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "Janssen/ Johnson & Johnson"))$n, 100*(VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "Janssen/ Johnson & Johnson"))$n/sum(VaccinationSupplierMuspad2$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("2nd COVID-19 Vaccination", "MuSPAD", "Other", sum((VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "Other"))$n), 100*sum((VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "Other"))$n)/sum(VaccinationSupplierMuspad2$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("3rd COVID-19 Vaccination", "MuSPAD", "BioNTech", (VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "BioNTech"))$n, 100*(VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "BioNTech"))$n/sum(VaccinationSupplierMuspad3$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("3rd COVID-19 Vaccination", "MuSPAD", "Moderna",  (VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "Moderna"))$n, 100*(VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "Moderna"))$n/sum(VaccinationSupplierMuspad3$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("3rd COVID-19 Vaccination", "MuSPAD", "AstraZeneca", (VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "AstraZeneca"))$n, 100*(VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "AstraZeneca"))$n/sum(VaccinationSupplierMuspad3$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("3rd COVID-19 Vaccination", "MuSPAD", "Janssen/Johnson & Johnson", (VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "Janssen/ Johnson & Johnson"))$n, 100*(VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "Janssen/ Johnson & Johnson"))$n/sum(VaccinationSupplierMuspad3$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("3rd COVID-19 Vaccination", "MuSPAD", "Other", sum((VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "Other"))$n), 100*sum((VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "Other"))$n)/sum(VaccinationSupplierMuspad3$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("4th COVID-19 Vaccination", "MuSPAD", "BioNTech", (VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "BioNTech"))$n, 100*(VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "BioNTech"))$n/sum(VaccinationSupplierMuspad4$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("4th COVID-19 Vaccination", "MuSPAD", "Moderna",  (VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "Moderna"))$n, 100*(VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "Moderna"))$n/sum(VaccinationSupplierMuspad4$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("4th COVID-19 Vaccination", "MuSPAD", "AstraZeneca", (VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "AstraZeneca"))$n, 100*(VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "AstraZeneca"))$n/sum(VaccinationSupplierMuspad4$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("4th COVID-19 Vaccination", "MuSPAD", "Janssen/Johnson & Johnson", (VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "Janssen/ Johnson & Johnson"))$n, 100*(VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "Janssen/ Johnson & Johnson"))$n/sum(VaccinationSupplierMuspad4$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("4th COVID-19 Vaccination", "MuSPAD", "Other", sum((VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "Other"))$n), 100*sum((VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "Other"))$n)/sum(VaccinationSupplierMuspad4$n))
VaccinationSupplierDataMuspad$n <- as.integer(VaccinationSupplierDataMuspad$n)
VaccinationSupplierDataMuspad$percent <- as.double(VaccinationSupplierDataMuspad$percent)
VaccinationSupplierDataMuspad$value_eng <- factor(VaccinationSupplierDataMuspad$value_eng, levels=c("BioNTech", "Moderna", "AstraZeneca", "Janssen/Johnson & Johnson", "Gamaleya Sputnik V", "Other", "I Don't Want To Answer", "Does Not Apply"))


palette <- function() {
  c("#ECA400", "#006992", "#001D4A")
}


vaccinationData %>% filter(value_eng != "Does Not Apply") %>% filter(value_eng != "I Don't Want To Answer") %>% group_by(vaccineNo, Source) %>% count(value_eng) %>%
                    mutate(percent = 100 * n / sum(n)) %>% mutate(percent = round(percent, digits = 2)) %>%
                    rbind(VaccinationSupplierDataMuspad) %>%
ggplot(aes(value_eng, percent)) +
  geom_bar(aes(fill = factor(Source, levels = c("Survey", "MuSPAD", "RKI"))), stat = "identity", position = "dodge", width = 0.8) +
  # geom_text(aes(label = percent, 
  #                 y = percent, 
  #                 group = Source),
  #             position = position_dodge(width = 0.8),
  #             vjust = -1, size = 6) +
  theme_minimal() +
  facet_wrap(~vaccineNo, nrow=2) +
  ylab("Share [Percentage]") +
  scale_y_continuous(limits=c(0,110), labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  xlab("Vaccination Supplier") +
  scale_fill_manual(values = palette()) +
  #scale_color_manual(values = palette2()) +
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

MuSPAD1st <- MusPAD %>% count(w22_vacc_type_1)

data1st <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(data1st) <- c("dose_1_received", "n", "percent", "source")
data1st [nrow(data1st)+1,] <- c("Yes", 64900000, 100*64900000/78000000, "RKI")
data1st [nrow(data1st)+1,] <- c("No", 13100000, 100*13100000/78000000, "RKI")
data1st [nrow(data1st)+1,] <- c("I Don't Want To Answer", 0, 0, "RKI")
data1st [nrow(data1st)+1,] <- c("Yes", 5+1256+2814+1), "MuSPAD")
data1st [nrow(data1st)+1,] <- c("No", 13, 100*13/(10+2096+4823+1+270+968+16+13), "MuSPAD")
data1st [nrow(data1st)+1,] <- c("I Don't Want To Answer", 0, 0, "MuSPAD")
data1st [nrow(data1st)+1,] <- c("No", 0, 0, "Survey")
data1st$n <- as.integer(data1st$n)
data1st$percent <- as.double(data1st$percent)

data2nd <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(data2nd) <- c("dose_2_received", "n", "percent", "source")
data2nd [nrow(data2nd)+1,] <- c("Yes", 63600000, 100*63600000/78000000, "RKI")
data2nd [nrow(data2nd)+1,] <- c("No", 14400000,100*14400000/78000000, "RKI")
data2nd [nrow(data2nd)+1,] <- c("I Don't Want To Answer", 0, 0, "RKI")
data2nd [nrow(data2nd)+1,] <- c("Yes", 26+591+6062+15+1429+9, 100*(26+591+6062+15+1429+9)/(26+591+6062+15+1429+9+32), "MuSPAD")
data2nd [nrow(data2nd)+1,] <- c("No", 32,100*32/(26+591+6062+15+1429+9+32), "MuSPAD")
data2nd [nrow(data2nd)+1,] <- c("I Don't Want To Answer", 0, 0, "MuSPAD")
data2nd$n <- as.integer(data2nd$n)
data2nd$percent <- as.double(data2nd$percent)

data3rd <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(data3rd) <- c("dose_3_received", "n", "percent", "source")
data3rd [nrow(data3rd)+1,] <- c("Yes", 52100000, 100*52100000/78000000, "RKI")
data3rd [nrow(data3rd)+1,] <- c("No", 25900000, 100*25900000/78000000, "RKI")
data3rd [nrow(data3rd)+1,] <- c("I Don't Want To Answer", 0, 0, "RKI")
data3rd [nrow(data3rd)+1,] <- c("Yes", 78+44+4558+2+11+2917+13, 100*(78+44+4558+2+11+2917+13)/(78+44+4558+2+11+2917+13+252), "MuSPAD")
data3rd [nrow(data3rd)+1,] <- c("No", 252, 100*252/(78+44+4558+2+11+2917+13+252), "MuSPAD")
data3rd [nrow(data3rd)+1,] <- c("I Don't Want To Answer", 0, 0, "MuSPAD")
data3rd$n <- as.integer(data3rd$n)
data3rd$percent <- as.double(data3rd$percent)

data4th <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(data4th) <- c("dose_4_received", "n", "percent", "source")
data4th [nrow(data4th)+1,] <- c("Yes", 1200000, 100*1200000/78000000, "RKI")
data4th [nrow(data4th)+1,] <- c("No", 76800000, 100*76800000/78000000, "RKI")
data4th [nrow(data4th)+1,] <- c("I Don't Want To Answer", 0, 0, "RKI")
data4th [nrow(data4th)+1,] <- c("Yes", 54+7+1344+1+1+179+8, 100*(54+7+1344+1+1+179+8)/(54+7+1344+1+1+179+8+1105+7141), "MuSPAD")
data4th [nrow(data4th)+1,] <- c("No", 1105+7141, 100*(1105+7141)//(54+7+1344+1+1+179+8+1105+7141, "MuSPAD")
data4th [nrow(data4th)+1,] <- c("I Don't Want To Answer", 0, 0, "MuSPAD")
data4th$n <- as.integer(data4th$n)
data4th$percent <- as.double(data4th$percent)

doses <- c("1st", "2nd", "3rd", "4th")

palette <- function() {
  c("#ECA400", "#006992", "#001D4A")
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

  vaccinationData %>% filter(!is.na(!!sym(column))) %>% filter(!!sym(column) != "I Don't Want To Answer") %>% count(!!sym(column)) %>%
    mutate(percent = 100 * n / sum(n)) %>% mutate(source = "Survey") %>%
    rbind(data) %>% filter(!!sym(column) != "I Don't Want To Answer") %>%
    mutate(percent = round(percent, digits = 2)) %>%
    ggplot(aes(!!sym(column), percent)) +
    geom_bar(aes(fill=factor(source, levels = c("Survey", "MuSPAD", "RKI"))), stat = "identity", position = "dodge", width = 0.8)+
    # geom_text(aes(label = percent, 
    #               y = percent, 
    #               group = source),
    #           position = position_dodge(width = 0.8),
    #           vjust = -1, size = 6) +
    #geom_errorbar(aes(x=dose_2_received, ymin=lci, ymax=uci, color=age_bracket), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
    theme_minimal() +
    ylab("Share [Percentage]") +
    scale_y_continuous(limits=c(0,110), labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
    xlab(paste0("Have you received a ", dose, " dose \nof a COVID-19-vaccine?")) +
    scale_fill_manual(values = palette()) +
    #scale_color_manual(values = palette2()) +
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

MuSPAD$w22_contact_household_count <- as.integer(MuSPAD$w22_contact_household_count)
MuSPAD <- MuSPAD %>% mutate(HouseholdCount = case_when(!is.na(s23_household_count) ~ s23_household_count,
                                                        .default = w22_contact_household_count)) 
HouseholdMuSPAD <- MuSPAD %>% select(HouseholdCount) %>% filter(HouseholdCount <= 22) %>% filter(HouseholdCount > 0) %>%
                              mutate(HouseholdCountAggregated = case_when(HouseholdCount == 1 ~ "1",
                                                                      HouseholdCount == 2 ~ "2",
                                                                      HouseholdCount == 3 ~ "3",
                                                                      HouseholdCount == 4 ~ "4",
                                                                      HouseholdCount > 4 ~ "5+"))
HouseholdMuSPAD <- HouseholdMuSPAD %>% count(HouseholdCountAggregated)
HouseholdDataMuspad <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(HouseholdDataMuspad) <- c("name", "value", "n", "percent", "Source")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 2019", "1", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "1"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "1"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 3/20", "1", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "1"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "1"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size Summer/21", "1", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "1"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "1"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 1/23", "1", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "1"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "1"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 2019", "2", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "2"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "2"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 3/20", "2", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "2"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "2"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size Summer/21", "2", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "2"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "2"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 1/23", "2", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "2"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "2"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 2019", "3", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "3"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "3"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 3/20", "3", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "3"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "3"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size Summer/21", "3", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "3"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "3"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 1/23", "3", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "3"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "3"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 2019", "4", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "4"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "4"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 3/20", "4", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "4"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "4"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size Summer/21", "4", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "4"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "4"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 1/23", "4", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "4"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "4"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 2019", "5+", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "5+"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "5+"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 3/20", "5+", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "5+"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "5+"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size Summer/21", "5+", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "5+"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "5+"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 1/23", "5+", (HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "5+"))$n, 100*(HouseholdMuSPAD %>% filter(HouseholdCountAggregated == "5+"))$n/sum(HouseholdMuSPAD$n), "MuSPAD")
HouseholdDataMuspad$n <- as.integer(HouseholdDataMuspad$n)
HouseholdDataMuspad$percent <- as.double(HouseholdDataMuspad$percent)
HouseholdDataMuspad$name <- factor(HouseholdDataMuspad$name, levels = c("Household size 2019","Household size 3/20","Household size Summer/21","Household size 1/23","Children < 14 in household","Persons > 14 in household","# Children < 18"))


palette <- function() {
  c("#ECA400", "#006992", "#001D4A")
}

HouseholdData %>% filter(name != "Children < 14 in household") %>%
                  filter(name != "# Children < 18") %>%  filter(!is.na(name)) %>% filter(!is.na(value)) %>% 
                  group_by(name) %>% count(value) %>% mutate(percent = 100 * n / sum(n)) %>% mutate(Source = "Survey") %>% 
                  rbind(HouseholdDataStatBundesamt) %>% 
                  rbind(HouseholdDataMuspad) %>% filter(name == "Household size 1/23") %>% filter(!is.na(Source)) %>%
ggplot(aes(value, percent)) +
  geom_bar(aes(fill=factor(Source, levels = c("Survey", "MuSPAD", "Statistisches Bundesamt (2023)"))), stat = "identity", position = "dodge", width = 0.8) +
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

ggsave("HouseholdSize.png", dpi = 500, w = 9.5, h = 6)
ggsave("HouseholdSize.pdf", dpi = 500, w = 9.5, h = 6)

# children under 14 ------------------------------------------------------------------

palette <- function() {
  c("#ECA400", "#006992", "#001D4A")
}

Children <- data_reduced %>% select(respondent_hsld_size_persons_under_14) %>%
                            mutate(respondent_hsld_size_persons_under_14 = case_when(respondent_hsld_size_persons_under_14  == 0 ~ "0",
                            respondent_hsld_size_persons_under_14  == 1 ~ "1",
                            respondent_hsld_size_persons_under_14  == 2 ~ "2",
                            respondent_hsld_size_persons_under_14  == 3 ~ "3+",
                            respondent_hsld_size_persons_under_14  == 4 ~ "3+"))

MuSPAD <- MuSPAD %>% mutate(ChildrenUnder14 = case_when(!is.na(w22_kids_under14_count) ~ w22_kids_under14_count,
                                                        .default = s22_household_under14)) 


ChildrenMuspad <- MuSPAD %>% select(ChildrenUnder14) %>%
                            mutate(ChildrenUnder14 = case_when(ChildrenUnder14  == 0 ~ "0",
                            ChildrenUnder14  == 1 ~ "1",
                            ChildrenUnder14  == 2 ~ "2",
                            ChildrenUnder14  == 3 ~ "3+",
                            ChildrenUnder14  > 3 ~ "3+"))

ChildrenMuspad <- ChildrenMuspad %>% count(ChildrenUnder14) %>% filter(!is.na(ChildrenUnder14))                  

ChildrenDataMuspad <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(ChildrenDataMuspad) <- c("respondent_hsld_size_persons_under_14", "n", "percent", "Source")
ChildrenDataMuspad[nrow(ChildrenDataMuspad) + 1, ] <- c("0", (ChildrenMuspad %>% filter(ChildrenUnder14 == "0"))$n, 100*(ChildrenMuspad %>% filter(ChildrenUnder14 == "0"))$n/sum(ChildrenMuspad$n), "MuSPAD")
ChildrenDataMuspad[nrow(ChildrenDataMuspad) + 1, ] <- c("1",	(ChildrenMuspad %>% filter(ChildrenUnder14 == "1"))$n, 100*(ChildrenMuspad %>% filter(ChildrenUnder14 == "1"))$n/sum(ChildrenMuspad$n), "MuSPAD")
ChildrenDataMuspad[nrow(ChildrenDataMuspad) + 1, ] <- c("2",	(ChildrenMuspad %>% filter(ChildrenUnder14 == "2"))$n, 100*(ChildrenMuspad %>% filter(ChildrenUnder14 == "2"))$n/sum(ChildrenMuspad$n), "MuSPAD")
ChildrenDataMuspad[nrow(ChildrenDataMuspad) + 1, ] <- c("3+",	(ChildrenMuspad %>% filter(ChildrenUnder14 == "3+"))$n, 100*(ChildrenMuspad %>% filter(ChildrenUnder14 == "3+"))$n/sum(ChildrenMuspad$n), "MuSPAD")

ChildrenDataMuspad$respondent_hsld_size_persons_under_14 <- factor(ChildrenDataMuspad$respondent_hsld_size_persons_under_14, levels = c("0", "1", "2", "3+"))
ChildrenDataMuspad$n <- as.integer(ChildrenDataMuspad$n)
ChildrenDataMuspad$percent <- as.double(ChildrenDataMuspad$percent)

Children %>% filter(!is.na(respondent_hsld_size_persons_under_14)) %>% 
                  count(respondent_hsld_size_persons_under_14) %>% 
                  mutate(percent = 100 * n / sum(n)) %>% 
                  mutate(Source = "Survey") %>% 
                  rbind(ChildrenDataMuspad) %>% 
                  filter(!is.na(Source)) %>%
ggplot(aes(respondent_hsld_size_persons_under_14, percent)) +
  geom_bar(aes(fill=factor(Source, levels = c("Survey", "MuSPAD"))), stat = "identity", position = "dodge", width = 0.8) +
  theme_minimal() +
  #facet_wrap(~name, nrow=2) +
  ylab("Share [Percentage]") +
  xlab("Children Under 14") +
  scale_fill_manual(values = palette()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

ggsave("Children_Comparison.png", dpi = 500, w = 9.5, h = 6)
ggsave("Children_Comparison.pdf", dpi = 500, w = 9.5, h = 6)


# Gender ------------------------------------------------------------------

palette <- function() {
  c("#ECA400", "#006992", "#001D4A")
}

GenderData <- data_reduced %>% select(gender)

GenderData <- GenderData %>% mutate(gender = case_when(gender == "Weiblich" ~ "female", 
                                                        gender == "Männlich" ~ "male",
                                                        gender == "Divers" ~ "diverse",
                                                        gender == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))

GenderData$gender <- factor(GenderData$gender, levels = c("female", "male", "diverse", "I Don't Want To Answer"))

#https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/deutsche-nichtdeutsche-bevoelkerung-nach-geschlecht-deutschland.html
GenderDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(GenderDataStatBundesamt) <- c("gender", "n", "percent", "Source")
GenderDataStatBundesamt[nrow(GenderDataStatBundesamt) + 1, ] <- c("female",42885791, 100*42885791/84669326, "Statistisches Bundesamt (2023)")
GenderDataStatBundesamt[nrow(GenderDataStatBundesamt) + 1, ] <- c("male",	41783535, 100*41783535/84669326, "Statistisches Bundesamt (2023)")
GenderDataStatBundesamt[nrow(GenderDataStatBundesamt) + 1, ] <- c("diverse",	0, 0, "Statistisches Bundesamt (2023)")
GenderDataStatBundesamt$gender <- factor(GenderDataStatBundesamt$gender, levels = c("female", "male", "diverse"))
GenderDataStatBundesamt$n <- as.integer(GenderDataStatBundesamt$n)
GenderDataStatBundesamt$percent <- as.double(GenderDataStatBundesamt$percent)

GenderMus <- MuSPAD %>% select(s22_sex) %>% count(s22_sex) %>% filter(!is.na(s22_sex))

GenderDataMuspad <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(GenderDataMuspad) <- c("gender", "n", "percent", "Source")
GenderDataMuspad[nrow(GenderDataMuspad) + 1, ] <- c("female", (GenderMus %>% filter(s22_sex == "female"))$n, 100*(GenderMus %>% filter(s22_sex == "female"))$n/sum(GenderMus$n), "MuSPAD")
GenderDataMuspad[nrow(GenderDataMuspad) + 1, ] <- c("male",	(GenderMus %>% filter(s22_sex == "male"))$n, 100*(GenderMus %>% filter(s22_sex == "male"))$n/sum(GenderMus$n), "MuSPAD")
GenderDataMuspad[nrow(GenderDataMuspad) + 1, ] <- c("diverse",	(GenderMus %>% filter(s22_sex == "diverse"))$n, 100*(GenderMus %>% filter(s22_sex == "diverse"))$n/sum(GenderMus$n), "MuSPAD")
GenderDataMuspad$gender <- factor(GenderDataMuspad$gender, levels = c("female", "male", "diverse"))
GenderDataMuspad$n <- as.integer(GenderDataMuspad$n)
GenderDataMuspad$percent <- as.double(GenderDataMuspad$percent)

GenderData %>% filter(gender != "I Don't Want To Answer") %>% 
                  count(gender) %>% mutate(percent = 100 * n / sum(n)) %>% mutate(Source = "Survey") %>% 
                  rbind(GenderDataStatBundesamt) %>%
                  rbind(GenderDataMuspad) %>%
ggplot(aes(gender, percent)) +
  geom_bar(aes(fill=factor(Source, levels = c("Survey", "MuSPAD", "Statistisches Bundesamt (2023)"))), stat = "identity", position = "dodge", width = 0.8) +
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

ggsave("Gender_Comparison.pdf", dpi = 500, w = 9.5, h = 6)
ggsave("Gender_Comparison.png", dpi = 500, w = 9.5, h = 6)

# Age ---------------------------------------------------------------------

palette <- function() {
  c("#ECA400", "#006992", "#001D4A")
}

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

AgeMuspad <- MuSPAD %>% select(s22_birth_date_yyyy) %>% 
                        filter(s22_birth_date_yyyy > 1900) %>%
                        mutate(age_bracket = case_when(s22_birth_date_yyyy <= 1910 ~ "80-99",
                                                                s22_birth_date_yyyy <= 1953 ~ "60-79",
                                                                s22_birth_date_yyyy <= 1963 ~ "40-59",
                                                                s22_birth_date_yyyy <= 1983 ~ "20-39",
                                                                s22_birth_date_yyyy <= 2003 ~ "Below 20 (*)")) %>% count(age_bracket)

AgeDataMuspad <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(AgeDataMuspad) <- c("age_bracket", "n", "percent", "source")
AgeDataMuspad[nrow(AgeDataMuspad) + 1, ] <- c("Below 20 (*)", (AgeMuspad %>% filter(age_bracket=="Below 20 (*)"))$n, 100*(AgeMuspad %>% filter(age_bracket=="Below 20 (*)"))$n/sum(AgeMuspad$n), "MuSPAD")
AgeDataMuspad[nrow(AgeDataMuspad) + 1, ] <- c("20-39", (AgeMuspad %>% filter(age_bracket=="20-39"))$n, 100*(AgeMuspad %>% filter(age_bracket=="20-39"))$n/sum(AgeMuspad$n), "MuSPAD")
AgeDataMuspad[nrow(AgeDataMuspad) + 1, ] <- c("40-59", (AgeMuspad %>% filter(age_bracket=="40-59"))$n, 100*(AgeMuspad %>% filter(age_bracket=="40-59"))$n/sum(AgeMuspad$n), "MuSPAD")
AgeDataMuspad[nrow(AgeDataMuspad) + 1, ] <- c("60-79", (AgeMuspad %>% filter(age_bracket=="60-79"))$n, 100*(AgeMuspad %>% filter(age_bracket=="60-79"))$n/sum(AgeMuspad$n), "MuSPAD")
AgeDataMuspad[nrow(AgeDataMuspad) + 1, ] <- c("80-99", (AgeMuspad %>% filter(age_bracket=="80-99"))$n, 100*(AgeMuspad %>% filter(age_bracket=="80-99"))$n/sum(AgeMuspad$n), "MuSPAD")
AgeDataMuspad$n <- as.integer(AgeDataMuspad$n)
AgeDataMuspad$percent <- as.double(AgeDataMuspad$percent)
AgeDataMuspad$age_bracket <- factor(AgeDataMuspad$age_bracket, levels = c("Below 20 (*)", "20-39", "40-59", "60-79", "80-99"))

AgeData %>% filter(!is.na(age_bracket)) %>% count(age_bracket) %>% 
            mutate(percent = 100 * n / sum(n)) %>% 
            mutate(source = "Survey") %>%
            rbind(AgeDataStatBundesamt) %>%
            rbind(AgeDataMuspad) %>%
ggplot(aes(age_bracket, percent)) +
  geom_bar(aes(fill=factor(source, levels = c("Survey", "MuSPAD", "Statistisches Bundesamt (2023)"))), stat = "identity", position = "dodge", width = 0.8) +
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

ggsave("Age_Comparison.pdf", dpi = 500, w = 9.5, h = 6)
ggsave("Age_Comparison.png", dpi = 500, w = 9.5, h = 6)

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

#Education 

palette <- function() {
  c("#ECA400", "#006992", "#001D4A")
}

educationLevel <- data_reduced %>% select(highest_educational_qualification)

educationLevel <- educationLevel %>% mutate(highest_educational_qualification = case_when(highest_educational_qualification == "Haupt-/ Volksschulabschluss" ~ "Certification after 9 years",
                                                                                          highest_educational_qualification == "Realschulabschluss" ~ "Certification after 10 years",
                                                                                          highest_educational_qualification == "Abitur / Fachhochschulabitur" ~ "Higher Education",
                                                                                          highest_educational_qualification == "Anderer" ~ "Other"))

educationLevel$highest_educational_qualification <- factor(educationLevel$highest_educational_qualification, levels = c("Higher Education", "Certification after 10 years", "Certification after 9 years", "Other"))

#https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bildung-Forschung-Kultur/Bildungsstand/Tabellen/bildungsabschluss.html
EducationDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(EducationDataStatBundesamt ) <- c("highest_educational_qualification", "n", "percent", "source")
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Higher Education", 82000000*0.335*100, 33.5, "Statistisches Bundesamt (2019)")
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Certification after 10 years", 82000000*0.3*100, 30, "Statistisches Bundesamt (2019)")
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Certification after 9 years", 82000000*0.286*100, 28.6, "Statistisches Bundesamt (2019)")
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Other", 82000000*0.077*100, 7.7, "Statistisches Bundesamt (2019)")
EducationDataStatBundesamt $n <- as.integer(EducationDataStatBundesamt $n)
EducationDataStatBundesamt $percent <- as.double(EducationDataStatBundesamt $percent)
EducationDataStatBundesamt $highest_educational_qualification <- factor(EducationDataStatBundesamt $highest_educational_qualification, levels = c("Higher Education", "Certification after 10 years", "Certification after 9 years", "Other"))

Education <- MuSPAD %>% select(s22_education) %>% count(s22_education) %>% filter(!is.na(s22_education))

EducationDataMuspad <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(EducationDataMuspad) <- c("highest_educational_qualification", "n", "percent", "source")
EducationDataMuspad[nrow(EducationDataMuspad) + 1, ] <- c("Higher Education", 4804+279, 100*(4804+279)/(4804+279+2544+945+178+903), "MuSPAD")
EducationDataMuspad[nrow(EducationDataMuspad) + 1, ] <- c("Certification after 10 years", 2544, 100*2544/(4804+279+2544+945+178+903), "MuSPAD")
EducationDataMuspad[nrow(EducationDataMuspad) + 1, ] <- c("Certification after 9 years", 945+178, 100*(945+178)/(4804+279+2544+945+178+903), "MuSPAD")
EducationDataMuspad[nrow(EducationDataMuspad) + 1, ] <- c("Other", 903, 100*903/(4804+279+2544+945+178+903), "MuSPAD")
EducationDataMuspad[nrow(EducationDataMuspad) + 1, ] <- (c("Other", 0, 0, "Survey")) 
EducationDataMuspad$n <- as.integer(EducationDataMuspad$n)
EducationDataMuspad$percent <- as.double(EducationDataMuspad$percent)
EducationDataMuspad$highest_educational_qualification <- factor(EducationDataMuspad$highest_educational_qualification, levels = c("Higher Education", "Certification after 10 years", "Certification after 9 years", "Other"))

educationLevel %>% filter(!is.na(highest_educational_qualification)) %>% 
            filter(highest_educational_qualification != "Other") %>%
            count(highest_educational_qualification) %>% 
            mutate(percent = 100 * n / sum(n)) %>% 
            mutate(source = "Survey") %>%
            rbind(EducationDataMuspad) %>%
            rbind(EducationDataStatBundesamt) %>%
ggplot(aes(highest_educational_qualification, percent)) +
  geom_bar(aes(fill=factor(source, levels = c("Survey", "MuSPAD", "Statistisches Bundesamt (2019)"))), stat = "identity", position = "dodge", width = 0.8) +
  theme_minimal() +
  #facet_wrap(~name, nrow=2) +
  ylab("Share [Percentage]") +
  xlab("Education") +
  scale_fill_manual(values = palette()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=0.7))

ggsave("EducationLevel_Comparison.pdf", dpi = 500, w =9.5, h = 9)
ggsave("EducationLevel_Comparison.png", dpi = 500, w =9.5, h = 9)


#Occupation
currentOccupation <- data_reduced %>% select(current_occupation)

currentOccupation <- currentOccupation %>% mutate(current_occupation = case_when(current_occupation == "Ich bin in einem anderen Beruf tätig." ~ "Other",
                                                                                 current_occupation == "Ich bin als Lehrer:in oder Erzieher:in tätig." ~ "Teaching Sector", 
                                                                                 current_occupation == "Ich bin Rentner:in oder Pensionär:in." ~ "Retired",
                                                                                 current_occupation == "Ich bin arbeitssuchend." ~ "Unemployed", 
                                                                                 current_occupation == "Ich bin im Studium oder in der Ausbildung." ~ "Student",
                                                                                 current_occupation == "Ich bin in einem medizinischen oder pflegerischen Beruf bei einem Gesundheitsversorger tätig." ~ "Medical Sector", 
                                                                                 current_occupation == "Andere (z.B. Elternzeit, Sabbatical)" ~ "Other",
                                                                                 current_occupation == "Ich möchte nicht antworten" ~ "Unknown",
                                                                                 is.na(current_occupation) ~ "Unknown")) %>%
                                            filter(current_occupation != "Unknown")

OccupationDataMuspad <- data.frame(matrix(nrow = 0, ncol = 4))
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Other", 4558, 46.6, "MuSPAD")
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Teaching Sector", 486, 4.8, "MuSPAD")
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Medical Sector", 719, 7.3, "MuSPAD")
#OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Unknown", 2545, 100*0, "MuSPAD")
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Retired", 3546, 36.2, "MuSPAD")
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Unemployed", 107, 1.1, "MuSPAD")
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Student", 393, 4, "MuSPAD") 
colnames(OccupationDataMuspad) <- c("current_occupation", "n", "percent", "source")
OccupationDataMuspad$n <- as.integer(OccupationDataMuspad$n)
OccupationDataMuspad$percent <- as.double(OccupationDataMuspad$percent)

currentOccupation %>% filter(!is.na(current_occupation)) %>% 
            count(current_occupation) %>% 
            mutate(percent = 100 * n / sum(n)) %>% 
            mutate(source = "Survey") %>%
            rbind(OccupationDataMuspad) %>%
ggplot(aes(current_occupation, percent)) +
  geom_bar(aes(fill=factor(source, levels = c("Survey", "MuSPAD"))), stat = "identity", position = "dodge", width = 0.8) +
  theme_minimal() +
  #facet_wrap(~name, nrow=2) +
  ylab("Share [Percentage]") +
  xlab("Current Occupation") +
  scale_fill_manual(values = palette()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=0.7))

ggsave("Occupation_Comparison.pdf", dpi = 500, w =9.5, h = 9)
ggsave("Occupation_Comparison.png", dpi = 500, w =9.5, h = 9)


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
