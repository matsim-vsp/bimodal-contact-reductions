library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(ggpubr)
library(readxl)

setwd("./AnalysisSP") # You need to set the working directory accordingly, otherwise the cleaning script (below does not work)
source("DataCleaningPrepForContactAnalysis.R")

MuSPAD <- read_delim("/Users/sydney/Downloads/Uni_Lübeck/MuSPAD_data_subset.csv") # Enter MuSPAD path
count_na_w22 <- function(row) {
  sum(grepl("^w22_", names(row)) & !is.na(row))
}
count_na_s22 <- function(row) {
  sum(grepl("^s22_", names(row)) & !is.na(row))
}
count_na_s22w22 <- function(row) {
  sum(grepl("^w22_", names(row)) & !is.na(row)) + sum(grepl("^s22_", names(row)) & !is.na(row))
}

MuSPAD_s22 <- readRDS("/Users/sydney/Downloads/9921_dataset/muspad_22-Nov-2022.rds")
MuSPADnewplusold <- left_join(MuSPAD_s22 %>% mutate(user_id = gsub("_", "-", user_id)) %>% select(user_id), MuSPAD, by = join_by(user_id == merge_id))


# Vaccination Supplier ----------------------------------------------------

# The following section creates bar plots which show the share of the different vaccination suppliers for the different doses

vaccinationData <- data_reduced %>% select(year_of_birth, c19_vaccination_details_vaccine_dose_1, c19_vaccination_details_vaccine_dose_2, c19_vaccination_details_vaccine_dose_3, c19_vaccination_details_vaccine_dose_4)
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
vaccinationData <- vaccinationData %>% mutate(Source = "External Survey\n(data acquisition:\n2023/07/18-2023/08/30)")
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

rkiVaccinations <- rkiVaccinations %>% filter(Impfdatum < "2023-09-01") %>%
                  mutate(Source = "RKI\n(data acquisition:\n2020/12/27-2023/08/30)") %>% filter(Impfserie %in% c(1,2,3,4))
rkiVaccinations <- rkiVaccinations %>% uncount(Anzahl)

rkiVaccinations <- rkiVaccinations %>% select(Impfserie, value_eng, Source)

vaccinationData <- rbind(vaccinationData, rkiVaccinations)

vaccinationData <- vaccinationData %>% mutate(vaccineNo = case_when(Impfserie == "1" ~ "1st COVID-19 Vaccination Dosis",
                                                                    Impfserie == "2" ~ "2nd COVID-19 Vaccination Dosis",
                                                                    Impfserie == "3" ~ "3rd COVID-19 Vaccination Dosis",
                                                                    Impfserie == "4" ~ "4th COVID-19 Vaccination Dosis"
                                                                    ))

#Compare to MuSPAD data

VaccinationSupplierMuspad <- MuSPAD %>% select(w22_vacc_type_1, w22_vacc_type_2, w22_vacc_type_3, w22_vacc_type_4)
VaccinationSupplierMuspad1 <- VaccinationSupplierMuspad %>% count(w22_vacc_type_1) %>% filter(!is.na(w22_vacc_type_1)) %>%
                                                            mutate(w22_vacc_type_1 = case_when(w22_vacc_type_1 %in% c("Andere", "Novavax", "Gamaleya Sputnik V") ~ "Other", 
                                                            .default = w22_vacc_type_1))  %>% filter(w22_vacc_type_1 != "keine (weitere) Impfung erhalten")
VaccinationSupplierMuspad2 <- VaccinationSupplierMuspad %>% count(w22_vacc_type_2) %>% filter(!is.na(w22_vacc_type_2)) %>%
                                                            mutate(w22_vacc_type_2 = case_when(w22_vacc_type_2 %in% c("Andere", "Novavax", "Gamaleya Sputnik V") ~ "Other", 
                                                            .default = w22_vacc_type_2))  %>% filter(w22_vacc_type_2 != "keine (weitere) Impfung erhalten")
VaccinationSupplierMuspad3 <- VaccinationSupplierMuspad %>% count(w22_vacc_type_3) %>% filter(!is.na(w22_vacc_type_3)) %>%
                                                            mutate(w22_vacc_type_3 = case_when(w22_vacc_type_3 %in% c("Andere", "Novavax", "Gamaleya Sputnik V") ~ "Other", 
                                                            .default = w22_vacc_type_3))  %>% filter(w22_vacc_type_3 != "keine (weitere) Impfung erhalten")
VaccinationSupplierMuspad4 <- VaccinationSupplierMuspad %>% count(w22_vacc_type_4) %>% filter(!is.na(w22_vacc_type_4)) %>%
                                                            mutate(w22_vacc_type_4 = case_when(w22_vacc_type_4 %in% c("Andere", "Novavax", "Gamaleya Sputnik V") ~ "Other", 
                                                            .default = w22_vacc_type_4)) %>% filter(w22_vacc_type_4 != "keine (weitere) Impfung erhalten")


VaccinationSupplierDataMuspad <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(VaccinationSupplierDataMuspad) <- c("vaccineNo", "Source", "value_eng", "n", "percent")
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("1st COVID-19 Vaccination Dosis", "MuSPAD", "BioNTech", (VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "BioNTech"))$n, 100*(VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "BioNTech"))$n/sum(VaccinationSupplierMuspad1$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("1st COVID-19 Vaccination Dosis", "MuSPAD", "Moderna", (VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "Moderna"))$n,100*(VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "Moderna"))$n/sum(VaccinationSupplierMuspad1$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("1st COVID-19 Vaccination Dosis", "MuSPAD", "AstraZeneca", (VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "AstraZeneca"))$n, 100*(VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "AstraZeneca"))$n/sum(VaccinationSupplierMuspad1$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("1st COVID-19 Vaccination Dosis", "MuSPAD", "Janssen/Johnson & Johnson", (VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "Janssen/ Johnson & Johnson"))$n, 100*(VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "Janssen/ Johnson & Johnson"))$n/sum(VaccinationSupplierMuspad1$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("1st COVID-19 Vaccination Dosis", "MuSPAD", "Other", sum((VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "Other"))$n), 100*sum((VaccinationSupplierMuspad1 %>% filter(w22_vacc_type_1 == "Other"))$n)/sum(VaccinationSupplierMuspad1$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("2nd COVID-19 Vaccination Dosis", "MuSPAD", "BioNTech", (VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "BioNTech"))$n, 100*(VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "BioNTech"))$n/sum(VaccinationSupplierMuspad2$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("2nd COVID-19 Vaccination Dosis", "MuSPAD", "Moderna",  (VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "Moderna"))$n, 100* (VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "Moderna"))$n/sum(VaccinationSupplierMuspad2$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("2nd COVID-19 Vaccination Dosis", "MuSPAD", "AstraZeneca", (VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "AstraZeneca"))$n, 100*(VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "AstraZeneca"))$n/sum(VaccinationSupplierMuspad2$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("2nd COVID-19 Vaccination Dosis", "MuSPAD", "Janssen/Johnson & Johnson", (VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "Janssen/ Johnson & Johnson"))$n, 100*(VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "Janssen/ Johnson & Johnson"))$n/sum(VaccinationSupplierMuspad2$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("2nd COVID-19 Vaccination Dosis", "MuSPAD", "Other", sum((VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "Other"))$n), 100*sum((VaccinationSupplierMuspad2 %>% filter(w22_vacc_type_2 == "Other"))$n)/sum(VaccinationSupplierMuspad2$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("3rd COVID-19 Vaccination Dosis", "MuSPAD", "BioNTech", (VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "BioNTech"))$n, 100*(VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "BioNTech"))$n/sum(VaccinationSupplierMuspad3$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("3rd COVID-19 Vaccination Dosis", "MuSPAD", "Moderna",  (VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "Moderna"))$n, 100*(VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "Moderna"))$n/sum(VaccinationSupplierMuspad3$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("3rd COVID-19 Vaccination Dosis", "MuSPAD", "AstraZeneca", (VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "AstraZeneca"))$n, 100*(VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "AstraZeneca"))$n/sum(VaccinationSupplierMuspad3$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("3rd COVID-19 Vaccination Dosis", "MuSPAD", "Janssen/Johnson & Johnson", (VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "Janssen/ Johnson & Johnson"))$n, 100*(VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "Janssen/ Johnson & Johnson"))$n/sum(VaccinationSupplierMuspad3$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("3rd COVID-19 Vaccination Dosis", "MuSPAD", "Other", sum((VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "Other"))$n), 100*sum((VaccinationSupplierMuspad3 %>% filter(w22_vacc_type_3 == "Other"))$n)/sum(VaccinationSupplierMuspad3$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("4th COVID-19 Vaccination Dosis", "MuSPAD", "BioNTech", (VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "BioNTech"))$n, 100*(VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "BioNTech"))$n/sum(VaccinationSupplierMuspad4$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("4th COVID-19 Vaccination Dosis", "MuSPAD", "Moderna",  (VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "Moderna"))$n, 100*(VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "Moderna"))$n/sum(VaccinationSupplierMuspad4$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("4th COVID-19 Vaccination Dosis", "MuSPAD", "AstraZeneca", (VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "AstraZeneca"))$n, 100*(VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "AstraZeneca"))$n/sum(VaccinationSupplierMuspad4$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("4th COVID-19 Vaccination Dosis", "MuSPAD", "Janssen/Johnson & Johnson", (VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "Janssen/ Johnson & Johnson"))$n, 100*(VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "Janssen/ Johnson & Johnson"))$n/sum(VaccinationSupplierMuspad4$n))
VaccinationSupplierDataMuspad[nrow(VaccinationSupplierDataMuspad) + 1, ] <- c("4th COVID-19 Vaccination Dosis", "MuSPAD", "Other", sum((VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "Other"))$n), 100*sum((VaccinationSupplierMuspad4 %>% filter(w22_vacc_type_4 == "Other"))$n)/sum(VaccinationSupplierMuspad4$n))
VaccinationSupplierDataMuspad$n <- as.integer(VaccinationSupplierDataMuspad$n)
VaccinationSupplierDataMuspad$percent <- as.double(VaccinationSupplierDataMuspad$percent)

palette_surveyrkimuspad_bars <- function() {
  c("#9900CC", "#7C898B", "#990000")
}

palette_surveyrkimuspad_errorbars <- function() {
   c("#640085", "#4a5253", "#5c0000")
}


vaccinationData %>% filter(value_eng != "Does Not Apply") %>% filter(value_eng != "I Don't Want To Answer") %>% group_by(vaccineNo, Source) %>% count(value_eng) %>%
                    mutate(percent = 100 * n / sum(n)) %>% mutate(percent = round(percent, digits = 2)) %>%
                    rbind(VaccinationSupplierDataMuspad) %>% group_by(vaccineNo, Source) %>% 
                    mutate(value_eng = factor(value_eng, levels=c("BioNTech", "Moderna", "AstraZeneca", "Janssen/Johnson & Johnson", "Gamaleya Sputnik V", "Other", "I Don't Want To Answer", "Does Not Apply"))) %>%
                    mutate(lci = sum(n)*(n/sum(n) - 1.96*(((n/sum(n)*(1-n/sum(n)))/sum(n))^0.5))) %>%
                    mutate(lci = 100/sum(n)*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
                    mutate(uci = sum(n)*(n/sum(n) + 1.96*(((n/sum(n)*(1-n/sum(n)))/sum(n))^0.5))) %>%
                    mutate(uci = 100/sum(n)*uci) %>%
                    mutate(Source = factor(Source, levels = c("External Survey\n(data acquisition:\n2023/07/18-2023/08/30)", "RKI\n(data acquisition:\n2020/12/27-2023/08/30)", "MuSPAD"))) %>%
ggplot(aes(value_eng, percent)) +
  geom_bar(aes(fill = Source), stat = "identity", position = "dodge", width = 0.95) +
  geom_errorbar(aes(x=value_eng, ymin=lci, ymax=uci, colour = Source), position = position_dodge(0.95), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  facet_wrap(~vaccineNo, nrow=2) +
  ylab("Share (Percentage)") +
  scale_y_continuous(limits=c(0,110), labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  xlab("Vaccination Supplier") +
  scale_fill_manual(values = palette_surveyrkimuspad_bars()) +
  scale_color_manual(values = palette_surveyrkimuspad_errorbars()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(text = element_text(size = 37)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

ggsave("ShareVaccinationSupplier.pdf", dpi = 500, w = 21, h = 21)
ggsave("ShareVaccinationSupplier.png", dpi = 500, w = 21, h = 21)

# Vaccination -------------------------------------------------------------

vaccinationData <- data_reduced %>% select(year_of_birth, c19_vaccination_status, c19_vaccination_details_vaccine_dose_1, c19_vaccination_details_vaccine_dose_2, c19_vaccination_details_vaccine_dose_3, c19_vaccination_details_vaccine_dose_4)

vaccinationData <- vaccinationData %>%  mutate(agegroup = case_when(2023-year_of_birth >= 60 ~ "60+",
                                                                    2023-year_of_birth >= 18 ~ "18-59"
                                                                    )) %>%
                                        mutate(dose_1_received = case_when(c19_vaccination_details_vaccine_dose_1 == "BioNTech" ~ "Yes",
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

vaccinationData <- vaccinationData %>% group_by(agegroup) %>% pivot_longer(cols=c(dose_1_received, dose_2_received, dose_3_received, dose_4_received)) %>%
                                      filter(value %in% c("Yes", "Not Vaccinated")) %>% 
                                      mutate(Source = "External Survey\n(data acquisition:\n2023/07/18-2023/08/30)") %>%
                                      mutate(name = case_when(name == "dose_1_received" ~ "Received at least\n1 dose",
                                                              name == "dose_2_received" ~ "Received at least\n2 doses",
                                                              name == "dose_3_received" ~ "Received at least\n3 doses",
                                                              name == "dose_4_received" ~ "Received at least\n4 doses")) %>% count(name) %>% mutate(Source = "External Survey\n(data acquisition:\n2023/07/18-2023/08/30)") 
NotVacc <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(NotVacc) <- c("name", "n", "Source", "agegroup")
NotVacc[nrow(NotVacc) + 1, ] <- c("Received 0 doses", 9 , "External Survey\n(data acquisition:\n2023/07/18-2023/08/30)", "18-59")
NotVacc[nrow(NotVacc) + 1, ] <- c("Received 0 doses", 1 , "External Survey\n(data acquisition:\n2023/07/18-2023/08/30)", "60+") ##Use c19_vaccination_status to find unvaccinated
NotVacc$n <- as.double(NotVacc$n)                                                              

vaccinationData <- rbind(vaccinationData, NotVacc)
vaccinationData <- vaccinationData %>% filter(!is.na(agegroup)) %>%
mutate(groupsize = case_when(agegroup == "18-59" ~ 456 +9, agegroup == "60+" ~ 91)) %>%
 group_by(agegroup, name) %>% mutate(percent = n/groupsize) %>% select(n, name, percent, Source, agegroup, groupsize)


Rki <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/refs/heads/main/Archiv/2023-09-12_Deutschland_Impfquoten_COVID-19.csv") %>%
       filter(Bundesland == "Deutschland")
#RKI data: https://impfdashboard.de/ 
RkiVacc <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(RkiVacc) <- c("name", "percent", "Source", "agegroup", "groupsize")
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received 0 doses", 100-Rki$Impfquote_18bis59_min1,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "18-59", 84669326*(0.188-0.169+0.245+0.268))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received at least\n1 dose", Rki$Impfquote_18bis59_min1,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "18-59", 84669326*(0.188-0.169+0.245+0.268))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received at least\n2 doses", Rki$Impfquote_18bis59_gi,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "18-59", 84669326*(0.188-0.169+0.245+0.268))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received at least\n3 doses", Rki$Impfquote_18bis59_boost1,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "18-59", 84669326*(0.188-0.169+0.245+0.268))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received at least\n4 doses", Rki$Impfquote_18bis59_boost2,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "18-59", 84669326*(0.188-0.169+0.245+0.268))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received 0 doses", 100-Rki$Impfquote_60plus_min1,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "60+", 84669326*(0.226+0.072))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received at least\n1 dose", Rki$Impfquote_60plus_min1,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "60+", 84669326*(0.226+0.072))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received at least\n2 doses", Rki$Impfquote_60plus_gi,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "60+", 84669326*(0.226+0.072))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received at least\n3 doses", Rki$Impfquote_60plus_boost1,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "60+", 84669326*(0.226+0.072))
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received at least\n4 doses", Rki$Impfquote_60plus_boost2,"RKI\n(data acquisition:\n2020/12/27-2023/09/11)", "60+", 84669326*(0.226+0.072))
RkiVacc$percent <- as.double(RkiVacc$percent)
RkiVacc$percent <- RkiVacc$percent/100
RkiVacc$groupsize <- as.double(RkiVacc$groupsize)
RkiVacc <- RkiVacc %>% mutate(n= groupsize*percent)

#vaccinationData <- rbind(vaccinationData, RkiVacc)

#Muspad vacc data

MuSPADVacc <- MuSPADnewplusold %>% select(c(s22_birth_date_yyyy, w22_vacc, w22_vacc_type_1, w22_vacc_type_2, w22_vacc_type_3, w22_vacc_type_4))
MuSPADVacc <- MuSPADVacc %>% mutate(w22_vacc_type_1 = case_when(w22_vacc_type_1 %in% c("Moderna", "BioNTech", "AstraZeneca", "Janssen/ Johnson & Johnson", "Novavax") ~ "Yes",
                                                                w22_vacc_type_1 == NA ~ NA,
                                                                w22_vacc_type_1 == "keine (weitere) Impfung erhalten" ~ "No")) %>%
                            mutate(w22_vacc_type_2 = case_when(w22_vacc_type_2 %in% c("Moderna", "BioNTech", "AstraZeneca", "Janssen/ Johnson & Johnson", "Novavax") ~ "Yes",
                                                                w22_vacc_type_2 == NA ~ NA,
                                                                w22_vacc_type_2 == "keine (weitere) Impfung erhalten" ~ "No")) %>%
                            mutate(w22_vacc_type_3 = case_when(w22_vacc_type_3 %in% c("Moderna", "BioNTech", "AstraZeneca", "Janssen/ Johnson & Johnson", "Novavax") ~ "Yes",
                                                                w22_vacc_type_3 == NA ~ NA,
                                                                w22_vacc_type_3 == "keine (weitere) Impfung erhalten" ~ "No")) %>%
                            mutate(w22_vacc_type_4 = case_when(w22_vacc_type_4 %in% c("Moderna", "BioNTech", "AstraZeneca", "Janssen/ Johnson & Johnson", "Novavax") ~ "Yes",
                                                                w22_vacc_type_4 == NA ~ NA,
                                                                w22_vacc_type_4 == "keine (weitere) Impfung erhalten" ~ "No")) %>%
                            mutate(agegroup = case_when(2023-s22_birth_date_yyyy >= 60 ~ "60+",
                                                                    2023-s22_birth_date_yyyy >= 18 ~ "18-59"
                                                                    ))                            
MuSPADVacc <- MuSPADVacc %>% pivot_longer(cols = c(w22_vacc, w22_vacc_type_1, w22_vacc_type_2, w22_vacc_type_3, w22_vacc_type_4)) %>% 
                              group_by(name, value, agegroup) %>% 
                              count() %>% 
                              filter(value %in% c("Yes", "Nein")) %>%
                              mutate(name = case_when(name == "w22_vacc" ~ "Received 0 doses",
                              name == "w22_vacc_type_1" ~ "Received at least\n1 dose",
                              name == "w22_vacc_type_2" ~ "Received at least\n2 doses",
                              name == "w22_vacc_type_3" ~ "Received at least\n3 doses",
                              name == "w22_vacc_type_4" ~ "Received at least\n4 doses")) %>%
                              mutate(Source = "MuSPAD\n(data acquisition:\nYY/MM/DD-YY/MM/DD)")


MuSPADVacc[which(MuSPADVacc$name == "Received at least\n1 dose" & MuSPADVacc$agegroup == "60+"), 4] <- MuSPADVacc[which(MuSPADVacc$name == "Received at least\n1 dose"& MuSPADVacc$agegroup == "60+"), 4] - MuSPADVacc[which(MuSPADVacc$name == "Received at least\n2 doses"& MuSPADVacc$agegroup == "60+"), 4]
MuSPADVacc[which(MuSPADVacc$name == "Received at least\n2 doses" & MuSPADVacc$agegroup == "60+"), 4] <- MuSPADVacc[which(MuSPADVacc$name == "Received at least\n2 doses"& MuSPADVacc$agegroup == "60+"), 4] - MuSPADVacc[which(MuSPADVacc$name == "Received at least\n3doses"& MuSPADVacc$agegroup == "60+"), 4]
MuSPADVacc[which(MuSPADVacc$name == "Received at least\n3doses" & MuSPADVacc$agegroup == "60+"), 4] <- MuSPADVacc[which(MuSPADVacc$name == "Received at least\n3doses"& MuSPADVacc$agegroup == "60+"), 4] - MuSPADVacc[which(MuSPADVacc$name == "Received at least\n4doses"& MuSPADVacc$agegroup == "60+"), 4]
MuSPADVacc[which(MuSPADVacc$name == "Received at least\n1 dose" & MuSPADVacc$agegroup == "18-59"), 4] <- MuSPADVacc[which(MuSPADVacc$name == "Received at least\n1 dose"& MuSPADVacc$agegroup == "18-59"), 4] - MuSPADVacc[which(MuSPADVacc$name == "Received at least\n2 doses"& MuSPADVacc$agegroup == "18-59"), 4]
MuSPADVacc[which(MuSPADVacc$name == "Received at least\n2 doses" & MuSPADVacc$agegroup == "18-59"), 4] <- MuSPADVacc[which(MuSPADVacc$name == "Received at least\n2 doses"& MuSPADVacc$agegroup == "18-59"), 4] - MuSPADVacc[which(MuSPADVacc$name == "Received at least\n3doses"& MuSPADVacc$agegroup == "18-59+"), 4]
MuSPADVacc[which(MuSPADVacc$name == "Received at least\n3doses" & MuSPADVacc$agegroup == "18-59"), 4] <- MuSPADVacc[which(MuSPADVacc$name == "Received at least\n3doses"& MuSPADVacc$agegroup == "18-59"), 4] - MuSPADVacc[which(MuSPADVacc$name == "Received at least\n4doses"& MuSPADVacc$agegroup == "18-59"), 4]


MuSPADVacc <- MuSPADVacc %>% group_by(agegroup)
MuSPADVacc <- MuSPADVacc %>% mutate(percent = n/sum(n)) %>% select(name, percent, Source)
MuSPADVacc$percent <- as.double(MuSPADVacc$percent)
#vaccinationData <- rbind(vaccinationData, MuSPADVacc)

palette_surveyrki_bars <- function() {
  c("#9900CC", "#7C898B")
}

palette_surveyrki_errorbars <- function() {
   c("#640085", "#4a5253")
}

eighteentofiftynine <- ggplot(vaccinationData %>%
rbind(RkiVacc) %>%
#rbind(MuSPADVacc) %>% 
filter(agegroup == "18-59") %>%
filter(name != "Received 0 doses") %>%
            mutate(lci = groupsize*(n/groupsize - 1.96*(((n/groupsize*(1-n/groupsize))/groupsize)^0.5))) %>%
                    mutate(lci = lci/groupsize) %>%
                    mutate(lci = case_when(lci < 0 ~ 0, .default= lci)) %>%
                    mutate(uci = groupsize*(n/groupsize + 1.96*(((n/groupsize*(1-n/groupsize))/groupsize)^0.5))) %>%
                    mutate(uci = uci/groupsize) %>%
mutate(Source = factor(Source, levels = c("External Survey\n(data acquisition:\n2023/07/18-2023/08/30)", "MuSPAD\n(data acquisition:\nYY/MM/DD-YY/MM/DD)", "RKI\n(data acquisition:\n2020/12/27-2023/09/11)"))), aes(x = name,  y = percent)) +
  geom_bar(stat = "identity", position="dodge", aes(fill = Source), alpha = 0.8) +
  geom_errorbar(aes(x=name, ymin=lci, ymax=uci, colour = Source), position = position_dodge(0.8), width = 0.3, size=1.3) +
  theme_minimal() +
  theme(text = element_text(size = 40)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_fill_manual(values = palette_surveyrki_bars()) +
  scale_color_manual(values = palette_surveyrki_errorbars()) +
  xlab("") +
  ggtitle("Number of Vaccine Doses (18-59)") +
  ylab("Share (Percentage)") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
        theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=0.7))

sixtyplus <- ggplot(vaccinationData %>%
rbind(RkiVacc) %>%
#rbind(MuSPADVacc) %>% 
filter(agegroup == "60+") %>%
filter(name != "Received 0 doses") %>%
            mutate(lci = groupsize*(n/groupsize - 1.96*(((n/groupsize*(1-n/groupsize))/groupsize)^0.5))) %>%
                    mutate(lci = lci/groupsize) %>%
                    mutate(lci = case_when(lci < 0 ~ 0, .default= lci)) %>%
                    mutate(uci = groupsize*(n/groupsize + 1.96*(((n/groupsize*(1-n/groupsize))/groupsize)^0.5))) %>%
                    mutate(uci = uci/groupsize) %>%
mutate(Source = factor(Source, levels = c("External Survey\n(data acquisition:\n2023/07/18-2023/08/30)", "MuSPAD\n(data acquisition:\nYY/MM/DD-YY/MM/DD)", "RKI\n(data acquisition:\n2020/12/27-2023/09/11)"))), aes(x = name,  y = percent)) +
  geom_bar(stat = "identity", position="dodge", aes(fill = Source), alpha = 0.8) +
    geom_errorbar(aes(x=name, ymin=lci, ymax=uci, colour = Source), position = position_dodge(0.8), width = 0.3, size=1.3) +
  theme_minimal() +
  theme(text = element_text(size = 40)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_fill_manual(values = palette_surveyrki_bars()) +
  scale_color_manual(values = palette_surveyrki_errorbars()) +
  xlab("") +
  ggtitle("Number of Vaccine Doses (60+)") +
  ylab("Share (Percentage)") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
        theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=0.7))

ggarrange(eighteentofiftynine, sixtyplus, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")
 
ggsave("NoVaccinations_Comparison.pdf", dpi = 500, w = 24, h = 12)
ggsave("NoVaccinations_Comparison.png", dpi = 500, w = 24, h = 12)
