library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(ggpubr)

# Author: S. Paltra, contact: paltra@tu-berlin.de

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
# MuSPAD$count_na_w22 <- apply(MuSPAD, 1, count_na_w22)
# MuSPAD$count_na_s22 <- apply(MuSPAD, 1, count_na_s22)
# MuSPAD$count_na_s22w22 <- apply(MuSPAD, 1, count_na_s22w22)
# MuSPAD_w22 <- MuSPAD %>% filter(count_na_w22 != 0) # We are excluding all participants who did not answer anything in the s22 survey -> Replace to match MuSPAD's procedure
# MuSPAD_s22 <- MuSPAD %>% filter(count_na_s22 != 0) # We are excluding all participants who did not answer anything in the s22 survey -> Replace to match MuSPAD's procedure
# MuSPAD_s22w22 <- MuSPAD %>% filter(count_na_s22w22 != 0) # We are excluding all participants who did not answer anything in the s22 survey -> Replace to match MuSPAD's procedure

MuSPAD_s22 <- readRDS("/Users/sydney/Downloads/9921_dataset/muspad_22-Nov-2022.rds")
MuSPADnewplusold <- left_join(MuSPAD_s22 %>% mutate(user_id = gsub("_", "-", user_id)) %>% select(user_id), MuSPAD, by = join_by(user_id == merge_id))

# Number of infections ----------------------------------------------------

# The following section creates the bar chart depicting how often people have been infected
# To do: Include COSMO data

palette <- function() {
  c("#ffe6ab", "#FFD269", "#ECA400", "#006992", "#27476E")
}

palette2 <- function() {
  c("#FFD269", "#ECA400", "#ab7700","#27476E", "#0b2442")
}

data_reduced <- data_reduced %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "0",
                                                                    num_c19_infs == "Einmal" ~ "1",
                                                                    num_c19_infs == "Zweimal" ~ "2+",
                                                                    num_c19_infs == "Dreimal" ~ "2+",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "2+",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

data_reduced$num_c19_infs_eng <- factor(data_reduced$num_c19_infs_eng, levels = c("0", "1", "2+"))

InfectionsMuspad <- MuSPADnewplusold %>% select(w22_positive_test) %>% count(w22_positive_test)
InfectionsMuspad <- InfectionsMuspad %>% filter(!is.na(w22_positive_test))

InfectionsDataMuspad <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(InfectionsDataMuspad) <- c("num_c19_infs_eng", "n", "percent", "Source", "sum")
InfectionsDataMuspad[nrow(InfectionsDataMuspad) + 1, ] <- c("0", (InfectionsMuspad %>% filter(w22_positive_test=="Nie"))$n, 100*(InfectionsMuspad %>% filter(w22_positive_test=="Nie"))$n/sum(InfectionsMuspad$n), "MuSPAD\n(data acquisition:\nYY/MM/DD - YY/MM/DD)", sum(InfectionsMuspad$n))
InfectionsDataMuspad[nrow(InfectionsDataMuspad) + 1, ] <- c("1",	(InfectionsMuspad %>% filter(w22_positive_test=="Einmal"))$n, 100*(InfectionsMuspad %>% filter(w22_positive_test=="Einmal"))$n/sum(InfectionsMuspad$n), "MuSPAD\n(data acquisition:\nYY/MM/DD - YY/MM/DD)", sum(InfectionsMuspad$n))
InfectionsDataMuspad[nrow(InfectionsDataMuspad) + 1, ] <- c("2+", (InfectionsMuspad %>% filter(w22_positive_test=="Zweimal"))$n+(InfectionsMuspad %>% filter(w22_positive_test=="Dreimal"))$n+(InfectionsMuspad %>% filter(w22_positive_test=="mehr als dreimal"))$n, 100*((InfectionsMuspad %>% filter(w22_positive_test=="Zweimal"))$n+(InfectionsMuspad %>% filter(w22_positive_test=="Dreimal"))$n+(InfectionsMuspad %>% filter(w22_positive_test=="mehr als dreimal"))$n)/sum(InfectionsMuspad$n), "MuSPAD\n(data acquisition:\nYY/MM/DD - YY/MM/DD)", sum(InfectionsMuspad$n))
InfectionsDataMuspad$num_c19_infs_eng <- factor(InfectionsDataMuspad$num_c19_infs_eng, levels = c("0", "1", "2+"))
InfectionsDataMuspad$n <- as.integer(InfectionsDataMuspad$n)
InfectionsDataMuspad$percent <- as.double(InfectionsDataMuspad$percent)
InfectionsDataMuspad$sum <- as.double(InfectionsDataMuspad$sum)

InfectionsDataTwitter <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(InfectionsDataTwitter) <- c("num_c19_infs_eng", "n", "percent", "Source", "sum")
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("0", 1191, 28.25, "Twitter\n(data acquisition:\n2023/07/19 - 2023/07/26)", 1191+2310+716)
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("1",	2310, 54.77, "Twitter\n(data acquisition:\n2023/07/19 - 2023/07/26)", 1191+2310+716)
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("2+", 716, 16.98, "Twitter\n(data acquisition:\n2023/07/19 - 2023/07/26)", 1191+2310+716)
InfectionsDataTwitter$num_c19_infs_eng <- factor(InfectionsDataTwitter$num_c19_infs_eng, levels = c("0", "1", "2+"))
InfectionsDataTwitter$n <- as.integer(InfectionsDataTwitter$n)
InfectionsDataTwitter$percent <- as.double(InfectionsDataTwitter$percent)
InfectionsDataTwitter$sum <- as.double(InfectionsDataTwitter$sum)

InfectionsDataMastodon <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(InfectionsDataMastodon) <- c("num_c19_infs_eng", "n", "percent", "Source", "sum")
InfectionsDataMastodon[nrow(InfectionsDataMastodon) + 1, ] <- c("0", 1802*0.37, 100*1802*0.37/(1802*0.37+1802*0.49+1802*0.12), "Mastodon\n(data acquisition:\n2023/07/19 - 2023/07/26)", 1802*0.37+1802*0.49+1802*0.12)
InfectionsDataMastodon[nrow(InfectionsDataMastodon) + 1, ] <- c("1",	1802*0.49, 100*1802*0.49/(1802*0.37+1802*0.49+1802*0.12), "Mastodon\n(data acquisition:\n2023/07/19 - 2023/07/26)", 1802*0.37+1802*0.49+1802*0.12)
InfectionsDataMastodon[nrow(InfectionsDataMastodon) + 1, ] <- c("2+", 1802*0.12, 100*1802*0.12/(1802*0.37+1802*0.49+1802*0.12), "Mastodon\n(data acquisition:\n2023/07/19 - 2023/07/26)", 1802*0.37+1802*0.49+1802*0.12)
InfectionsDataMastodon$num_c19_infs_eng <- factor(InfectionsDataMastodon$num_c19_infs_eng, levels = c("0", "1", "2+"))
InfectionsDataMastodon$n <- as.integer(InfectionsDataMastodon$n)
InfectionsDataMastodon$percent <- as.double(InfectionsDataMastodon$percent)
InfectionsDataMastodon$sum <- as.double(InfectionsDataMastodon$sum)
 
# Data comes from https://projekte.uni-erfurt.de/cosmo2020/files/COSMO_W70.pdf --> DEC 2022!
InfectionsDataCOSMO <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(InfectionsDataCOSMO) <- c("num_c19_infs_eng", "n", "percent", "Source", "sum")
InfectionsDataCOSMO[nrow(InfectionsDataCOSMO) + 1, ] <- c("0", 1003*0.5, 50, "COSMO\n(data acquisition:\n2022/11/29-2022/11/30)", 1003)
InfectionsDataCOSMO[nrow(InfectionsDataCOSMO) + 1, ] <- c("1",	1003*0.42, 42, "COSMO\n(data acquisition:\n2022/11/29-2022/11/30)", 1003)
InfectionsDataCOSMO[nrow(InfectionsDataCOSMO) + 1, ] <- c("2+", 1003*0.08, 8, "COSMO\n(data acquisition:\n2022/11/29-2022/11/30)", 1003)
InfectionsDataCOSMO$num_c19_infs_eng <- factor(InfectionsDataCOSMO$num_c19_infs_eng, levels = c("0", "1", "2+"))
InfectionsDataCOSMO$n <- as.integer(InfectionsDataCOSMO$n)
InfectionsDataCOSMO$percent <- as.double(InfectionsDataCOSMO$percent)
InfectionsDataCOSMO$sum <- as.double(InfectionsDataCOSMO$sum)

#Intervals based on: http://www.stat.yale.edu/Courses/1997-98/101/catinf.htm
data_reduced %>% filter(num_c19_infs_eng != "I Don't Want To Answer") %>%
  count(num_c19_infs_eng) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  mutate(Source = "External Survey\n(data acquisition:\n2023/07/18-2023/08/30)") %>%
  mutate(sum = sum(n)) %>%
  rbind(InfectionsDataTwitter) %>%
  rbind(InfectionsDataMuspad) %>%
  rbind(InfectionsDataCOSMO) %>% 
  rbind(InfectionsDataMastodon) %>%
  mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(lci = 100/sum*lci) %>%
  mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(uci = 100/sum*uci) %>%
  mutate(Source = factor(Source, levels = c("Mastodon\n(data acquisition:\n2023/07/19 - 2023/07/26)", "Twitter\n(data acquisition:\n2023/07/19 - 2023/07/26)", "External Survey\n(data acquisition:\n2023/07/18-2023/08/30)", "MuSPAD\n(data acquisition:\nYY/MM/DD - YY/MM/DD)", "COSMO\n(data acquisition:\n2022/11/29-2022/11/30)"))) %>%
  ggplot(aes(num_c19_infs_eng, percent)) +
  geom_bar(aes(fill=Source), stat = "identity", position = "dodge", width = 0.95) +
  geom_errorbar(aes(x=num_c19_infs_eng, ymin=lci, ymax=uci, colour = Source), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  facet_wrap(~Source, nrow=1) +
  ylab("Share (Percentage)") +
  xlab("Number of Infections") +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.5), breaks = c(0,12.5,25,37.5, 50,75,100)) +
  theme(text = element_text(size = 34)) +
  theme(legend.position = "none", legend.title = element_blank()) +
   guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
      theme(axis.ticks.x = element_line(size = 1), 
                   axis.ticks.y = element_line(size = 1),
                   axis.ticks.length = unit(20, "pt"))

ggsave("NoInfections_Comparison.pdf", dpi = 500, w = 24, h = 12)
ggsave("NoInfections_Comparison.png", dpi = 500,  w = 24, h = 12)


# Infection numbers (by recruiter)

palette <- function() {
  c("#ffe6ab", "#FFD269", "#fac548", "#ECA400", "#ad8500", "#006992")
}
palette2 <- function() {
  c("#FFD269", "#fac548", "#ECA400", "#ad8500", "#6b5200", "#27476E")
}


#Priesemann
InfectionsDataTwitter <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(InfectionsDataTwitter) <- c("num_c19_infs_eng", "n", "percent", "recruiter", "sum")
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("0", 2120*(0.3030), 2120*(0.3030)/(2120*(0.3030) + 2120*(0.5270) + 2120*(0.1390)), "Priesemann", 2120*(0.3030) + 2120*(0.5270) + 2120*(0.1390))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("1", 2120*(0.5270), 2120*(0.5270)/(2120*(0.3030) + 2120*(0.5270) + 2120*(0.1390)), "Priesemann", 2120*(0.3030) + 2120*(0.5270) + 2120*(0.1390))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("2+", 2120*(0.1390), 2120*(0.1390)/(2120*(0.3030) + 2120*(0.5270) + 2120*(0.1390)), "Priesemann", 2120*(0.3030) + 2120*(0.5270) + 2120*(0.1390))
InfectionsDataTwitter$num_c19_infs_eng <- factor(InfectionsDataTwitter$num_c19_infs_eng, levels = c("0", "1", "2+"))

#Valdez
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("0", 101*(0.1780), 101*(0.1780)/(101*(0.1780) + 101*(0.5740) + 101*(0.2180)), "CaleroValdez", 101*(0.1780) + 101*(0.5740) + 101*(0.2180))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("1", 101*(0.5740), 101*(0.5740)/(101*(0.1780) + 101*(0.5740) + 101*(0.2180)), "CaleroValdez", 101*(0.1780) + 101*(0.5740) + 101*(0.2180))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("2+", 101*(0.2180), 101*(0.2180)/(101*(0.1780) + 101*(0.5740) + 101*(0.2180)), "CaleroValdez", 101*(0.1780) + 101*(0.5740) + 101*(0.2180))
InfectionsDataTwitter$num_c19_infs_eng <- factor(InfectionsDataTwitter$num_c19_infs_eng, levels = c("0", "1", "2+"))

#Franke
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("0", 111*(0.2610), 111*(0.2610)/(111*(0.2610) + 111*(0.5410) + 111*(0.1530)), "Franke", 111*(0.2610) + 111*(0.5410) + 111*(0.1530))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("1", 111*(0.5410), 111*(0.5410)/(111*(0.2610) + 111*(0.5410) + 111*(0.1530)), "Franke", 111*(0.2610) + 111*(0.5410) + 111*(0.1530))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("2+", 111*(0.1530), 111*(0.1530)/(111*(0.2610) + 111*(0.5410) + 111*(0.1530)), "Franke", 111*(0.2610) + 111*(0.5410) + 111*(0.1530))
InfectionsDataTwitter$num_c19_infs_eng <- factor(InfectionsDataTwitter$num_c19_infs_eng, levels = c("0", "1", "2+"))

#Briest
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("0", 371*(0.2210), 371*(0.2210)/(371*(0.2210) + 371*(0.5690) + 371*(0.1640)), "Briest", 371*(0.2210) + 371*(0.5690) + 371*(0.1640))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("1", 371*(0.5690), 371*(0.5690)/(371*(0.2210) + 371*(0.5690) + 371*(0.1640)), "Briest", 371*(0.2210) + 371*(0.5690) + 371*(0.1640))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("2+", 371*(0.1640), 371*(0.1640)/(371*(0.2210) + 371*(0.5690) + 371*(0.1640)), "Briest", 371*(0.2210) + 371*(0.5690) + 371*(0.1640))
InfectionsDataTwitter$num_c19_infs_eng <- factor(InfectionsDataTwitter$num_c19_infs_eng, levels = c("0", "1", "2+"))

#Bachmann
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("0", 1667*(0.2520), 1667*(0.2520)/(1667*(0.2520) + 1667*(0.5180) + 1667*(0.1930)), "Bachmann", 1667*(0.2520) + 1667*(0.5180) + 1667*(0.1930))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("1", 1667*(0.5180), 1667*(0.5180)/(1667*(0.2520) + 1667*(0.5180) + 1667*(0.1930)), "Bachmann", 1667*(0.2520) + 1667*(0.5180) + 1667*(0.1930))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("2+", 1667*(0.1930),  1667*(0.1930)/(1667*(0.2520) + 1667*(0.5180) + 1667*(0.1930)), "Bachmann", 1667*(0.2520) + 1667*(0.5180) + 1667*(0.1930))
InfectionsDataTwitter$num_c19_infs_eng <- factor(InfectionsDataTwitter$num_c19_infs_eng, levels = c("0", "1", "2+"))

#Priesemann (Mastodon)
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("0", 1802*(0.37), 1802*(0.37)/(1802*(0.37) + 1802*(0.49) + 1802*(0.1200)), "Priesemann (Mastodon)", 1802*(0.37) + 1802*(0.49) + 1802*(0.1200))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("1", 1802*(0.49), 1802*(0.49)/(1802*(0.37) + 1802*(0.49) + 1802*(0.1200)), "Priesemann (Mastodon)", 1802*(0.37) + 1802*(0.49) + 1802*(0.1200))
InfectionsDataTwitter[nrow(InfectionsDataTwitter) + 1, ] <- c("2+", 1802*(0.1200), 1802*(0.1200)/(1802*(0.37) + 1802*(0.49) + 1802*(0.1200)), "Priesemann (Mastodon)", 1802*(0.37) + 1802*(0.49) + 1802*(0.1200))
InfectionsDataTwitter$num_c19_infs_eng <- factor(InfectionsDataTwitter$num_c19_infs_eng, levels = c("0", "1", "2+"))

InfectionsDataTwitter$n <- as.integer(InfectionsDataTwitter$n)
InfectionsDataTwitter$percent <- as.double(InfectionsDataTwitter$percent)
InfectionsDataTwitter$percent <- 100*(InfectionsDataTwitter$percent)
InfectionsDataTwitter$sum <- as.double(InfectionsDataTwitter$sum)

InfectionsDataTwitter %>% group_by(recruiter) %>%
  mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(lci = 100/sum*lci) %>%
  mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(uci = 100/sum*uci) %>%
  ggplot(aes(num_c19_infs_eng, percent)) +
  geom_bar(aes(fill=factor(recruiter, levels = c("Priesemann", "CaleroValdez", "Franke", "Briest", "Bachmann", "Priesemann (Mastodon)"))), stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=num_c19_infs_eng, ymin=lci, ymax=uci, colour = factor(recruiter, levels = c("Priesemann", "CaleroValdez", "Franke", "Briest", "Bachmann", "Priesemann (Mastodon)"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  #facet_wrap(~name, nrow=2) +
  ylab("Share (Percentage)") +
  xlab("No. Of Infections") +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.5), breaks = c(0,12.5,25, 37.5, 50,75,100)) +
  theme(text = element_text(size = 37)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

ggsave("NoInfections_Comparison_Recruiter.pdf", dpi = 500, w = 10, h = 7.5)
ggsave("NoInfections_Comparison_Recruiter.png", dpi = 500,  w = 10, h = 7.5)


# Vaccination Supplier ----------------------------------------------------

# The following section creates bar plots which show the share of the different vaccination suppliers for the different doses

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
vaccinationData <- vaccinationData %>% mutate(Source = "External Survey (cutoff date: 08/30/23)")
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
  c("#ECA400", "#006992", "#27476E")
}

palette2 <- function() {
  c("#6b5200", "#27476E", "#091c33")
}


vaccinationData %>% filter(value_eng != "Does Not Apply") %>% filter(value_eng != "I Don't Want To Answer") %>% group_by(vaccineNo, Source) %>% count(value_eng) %>%
                    mutate(percent = 100 * n / sum(n)) %>% mutate(percent = round(percent, digits = 2)) %>%
                    rbind(VaccinationSupplierDataMuspad) %>%
ggplot(aes(value_eng, percent)) +
  geom_bar(aes(fill = factor(Source, levels = c("Survey", "MuSPAD", "RKI"))), stat = "identity", position = "dodge", width = 0.8) +
  theme_minimal() +
  facet_wrap(~vaccineNo, nrow=2) +
  ylab("Share (Percentage)") +
  scale_y_continuous(limits=c(0,110), labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  xlab("Vaccination Supplier") +
  scale_fill_manual(values = palette()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(text = element_text(size = 37)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

ggsave("ShareVaccinationSupplier.pdf", dpi = 500, w = 21, h = 15)
ggsave("ShareVaccinationSupplier.png", dpi = 500, w = 21, h = 15)

# Vaccination -------------------------------------------------------------

vaccinationData <- raw_data %>% select(c19_vaccination_details_vaccine_dose_1, c19_vaccination_details_vaccine_dose_2, c19_vaccination_details_vaccine_dose_3, c19_vaccination_details_vaccine_dose_4)

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

vaccinationData <- vaccinationData %>% mutate(dose_3_received = case_when(dose_4_received == "Yes" ~ NA,
                                              .default = dose_3_received)) %>%
                                        mutate(dose_2_received = case_when(dose_3_received == "Yes" ~ NA,
                                              dose_4_received == "Yes" ~ NA,
                                              .default = dose_2_received)) %>%
                                        mutate(dose_1_received = case_when(dose_2_received == "Yes" ~ NA,
                                              dose_3_received == "Yes" ~ NA,
                                              dose_4_received == "Yes" ~ NA,
                                              dose_1_received == "No" ~ "Not Vaccinated",
                                              .default = dose_1_received))

vaccinationData$dose_1_received <- factor(vaccinationData$dose_1_received, levels = c("Yes", "No", NA, "I Don't Want To Answer"))
vaccinationData$dose_2_received <- factor(vaccinationData$dose_2_received, levels = c("Yes", "No", NA, "I Don't Want To Answer"))
vaccinationData$dose_3_received <- factor(vaccinationData$dose_3_received, levels = c("Yes", "No", NA, "I Don't Want To Answer"))
vaccinationData$dose_4_received<- factor(vaccinationData$dose_4_received, levels = c("Yes", "No", NA, "I Don't Want To Answer"))

vaccinationData <- vaccinationData %>% pivot_longer(cols=c(dose_1_received, dose_2_received, dose_3_received, dose_4_received)) %>%
                                      filter(value %in% c("Yes", "Not Vaccinated")) %>% 
                                      mutate(Source = "External Survey \n(cutoff date: 08/30/23)") %>%
                                      mutate(name = case_when(name == "dose_1_received" ~ "Received 1 dose",
                                                              name == "dose_2_received" ~ "Received 2 doses",
                                                              name == "dose_3_received" ~ "Received 3 doses",
                                                              name == "dose_4_received" ~ "Received 4 doses")) %>% count(name) %>% mutate(Source = "External Survey \n(cutoff date: 08/30/23)") 
NotVacc <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(NotVacc) <- c("name", "n", "Source")
NotVacc[nrow(NotVacc) + 1, ] <- c("Received 0 doses", 10 , "External Survey \n(cutoff date: 08/30/23)")
NotVacc$n <- as.double(NotVacc$n)                                                              

vaccinationData <- rbind(vaccinationData, NotVacc)
vaccinationData <- vaccinationData %>% mutate(percent = n/sum(n)) %>% select(name, percent, Source)


Rki <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/refs/heads/main/Archiv/2023-09-12_Deutschland_Impfquoten_COVID-19.csv") %>%
       filter(Bundesland == "Deutschland")
#RKI data: https://impfdashboard.de/ 
RkiVacc <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(RkiVacc) <- c("name", "percent", "Source")
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received 0 doses", 100-Rki$Impfquote_gesamt_min1,"RKI \n(cutoff date: 09/11/23)")
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received 1 dose", Rki$Impfquote_gesamt_min1-Rki$Impfquote_gesamt_gi,"RKI \n(cutoff date: 09/11/23)")
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received 2 doses", Rki$Impfquote_gesamt_gi-Rki$Impfquote_gesamt_boost1,"RKI \n(cutoff date: 09/11/23)")
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received 3 doses", Rki$Impfquote_gesamt_boost1-Rki$Impfquote_gesamt_boost2,"RKI \n(cutoff date: 09/11/23)")
RkiVacc[nrow(RkiVacc) + 1, ] <- c("Received 4 doses", Rki$Impfquote_gesamt_boost2,"RKI \n(cutoff date: 09/11/23)")
RkiVacc$percent <- as.double(RkiVacc$percent)
RkiVacc$percent <- RkiVacc$percent/100

vaccinationData <- rbind(vaccinationData, RkiVacc)

#Muspad vacc data

MuSPADVacc <- MuSPADnewplusold %>% select(c(w22_vacc, w22_vacc_type_1, w22_vacc_type_2, w22_vacc_type_3, w22_vacc_type_4))
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
                                                                w22_vacc_type_4 == "keine (weitere) Impfung erhalten" ~ "No"))
MuSPADVacc <- MuSPADVacc %>% pivot_longer(cols = c(w22_vacc, w22_vacc_type_1, w22_vacc_type_2, w22_vacc_type_3, w22_vacc_type_4)) %>% 
                              group_by(name, value) %>% 
                              count() %>% 
                              filter(value %in% c("Yes", "Nein")) %>%
                              mutate(name = case_when(name == "w22_vacc" ~ "Received 0 doses",
                              name == "w22_vacc_type_1" ~ "Received 1 dose",
                              name == "w22_vacc_type_2" ~ "Received 2 doses",
                              name == "w22_vacc_type_3" ~ "Received 3 doses",
                              name == "w22_vacc_type_4" ~ "Received 4 doses")) %>%
                              mutate(Source = "MuSPAD")

MuSPADVacc[which(MuSPADVacc$name == "Received 1 dose"), 3] <- MuSPADVacc[which(MuSPADVacc$name == "Received 1 dose"), 3] - MuSPADVacc[which(MuSPADVacc$name == "Received 2 doses"), 3]
MuSPADVacc[which(MuSPADVacc$name == "Received 2 doses"), 3] <- MuSPADVacc[which(MuSPADVacc$name == "Received 2 doses"), 3] - MuSPADVacc[which(MuSPADVacc$name == "Received 3 doses"), 3]
MuSPADVacc[which(MuSPADVacc$name == "Received 3 doses"), 3] <- MuSPADVacc[which(MuSPADVacc$name == "Received 3 doses"), 3] - MuSPADVacc[which(MuSPADVacc$name == "Received 4 doses"), 3]

MuSPADVacc <- MuSPADVacc %>% ungroup()
MuSPADVacc <- MuSPADVacc %>% mutate(percent = n/sum(n)) %>% select(name, percent, Source)
MuSPADVacc$percent <- as.double(MuSPADVacc$percent)
vaccinationData <- rbind(vaccinationData, MuSPADVacc)

vaccinationData$Source <- factor(vaccinationData$Source, levels = c("External Survey \n(cutoff date: 08/30/23)", "MuSPAD", "RKI \n(cutoff date: 09/11/23)"))

palette <- function() {
  c("#ECA400", "#52b7de", "#1d94c2", "#006992", "#27476E")
}

ggplot(vaccinationData, aes(x = Source,  y = percent, fill = name)) +
  geom_bar(stat = "identity", position="fill") +
  theme_minimal() +
  theme(text = element_text(size = 37)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_fill_manual(values = palette()) +
  xlab("") +
  ylab("Share (Percentage)") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(nrow = 3)) + 
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

ggsave("NoVaccinations_Comparison.pdf", dpi = 500,  w = 12, h = 9)
ggsave("NoVaccinations_Comparison.png", dpi = 500,  w = 12, h = 9)

# Household Size ----------------------------------------------------

# The following section compares the household sizes for the survey, MuSPAD and the Federal Statistical Office

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
HouseholdDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(HouseholdDataStatBundesamt) <- c("name", "value", "n", "percent", "Source", "sum")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "1", 84669326*0.411, 41.1, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "1", 84669326*0.411, 41.1, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "1", 84669326*0.411, 41.1, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "1", 84669326*0.411, 41.1, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "2", 84669326*0.335, 33.5, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "2", 84669326*0.335, 33.5, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "2", 84669326*0.335, 33.5, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "2", 84669326*0.335, 33.5, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "3", 84669326*0.119, 11.9, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "3", 84669326*0.119, 11.9, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "3", 84669326*0.119, 11.9, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "3", 84669326*0.119, 11.9, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "4", 84669326*0.095, 9.5, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "4", 84669326*0.095, 9.5, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "4", 84669326*0.095, 9.5, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "4", 84669326*0.095, 9.5, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "5+", 84669326*0.039, 3.9, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "5+", 84669326*0.039, 3.9, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "5+", 84669326*0.039, 3.9, "Federal Statistical Office (2023)", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "5+", 84669326*0.039, 3.9, "Federal Statistical Office (2023)",84669326)
HouseholdDataStatBundesamt$n <- as.integer(HouseholdDataStatBundesamt$n)
HouseholdDataStatBundesamt$sum <- as.integer(HouseholdDataStatBundesamt$sum)
HouseholdDataStatBundesamt$percent <- as.double(HouseholdDataStatBundesamt$percent)
HouseholdDataStatBundesamt$name <- factor(HouseholdDataStatBundesamt$name, levels = c("Household size 2019","Household size 3/20","Household size Summer/21","Household size 1/23","Children < 14 in household","Persons > 14 in household","# Children < 18"))

MuSPADHousehold <- MuSPAD_s22 %>%  
                         select(haushalt_gesamt, haushalt_gesamt_g) %>%
                         filter(haushalt_gesamt != 0) %>%
                         filter(!is.na(haushalt_gesamt)) %>%
                         mutate(haushalt_gesamt = case_when(haushalt_gesamt == 1 ~ "1", 
                         haushalt_gesamt == 2 ~ "2",
                         haushalt_gesamt == 3 ~ "3",
                         haushalt_gesamt == 4 ~ "4",
                         haushalt_gesamt >= 5 ~ "5+")) %>%
                         count(haushalt_gesamt)

HouseholdDataMuspad <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(HouseholdDataMuspad) <- c("name", "value", "n", "percent", "Source", "sum")
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 2019", "1", (MuSPADHousehold %>% filter(haushalt_gesamt == "1"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "1"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 3/20", "1", (MuSPADHousehold %>% filter(haushalt_gesamt == "1"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "1"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size Summer/21", "1", (MuSPADHousehold %>% filter(haushalt_gesamt == "1"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "1"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 1/23", "1", (MuSPADHousehold %>% filter(haushalt_gesamt == "1"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "1"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 2019", "2", (MuSPADHousehold %>% filter(haushalt_gesamt == "2"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "2"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 3/20", "2", (MuSPADHousehold %>% filter(haushalt_gesamt == "2"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "2"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size Summer/21", "2", (MuSPADHousehold %>% filter(haushalt_gesamt == "2"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "2"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 1/23", "2", (MuSPADHousehold %>% filter(haushalt_gesamt == "2"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "2"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 2019", "3", (MuSPADHousehold %>% filter(haushalt_gesamt == "3"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "3"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 3/20", "3", (MuSPADHousehold %>% filter(haushalt_gesamt == "3"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "3"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size Summer/21", "3", (MuSPADHousehold %>% filter(haushalt_gesamt == "3"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "3"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 1/23", "3", (MuSPADHousehold %>% filter(haushalt_gesamt == "3"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "3"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 2019", "4", (MuSPADHousehold %>% filter(haushalt_gesamt == "4"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "4"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 3/20", "4", (MuSPADHousehold %>% filter(haushalt_gesamt == "4"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "4"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size Summer/21", "4", (MuSPADHousehold %>% filter(haushalt_gesamt == "4"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "4"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 1/23", "4", (MuSPADHousehold %>% filter(haushalt_gesamt == "4"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "4"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 2019", "5+", (MuSPADHousehold %>% filter(haushalt_gesamt == "5+"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "5+"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 3/20", "5+", (MuSPADHousehold %>% filter(haushalt_gesamt == "5+"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "5+"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size Summer/21", "5+", (MuSPADHousehold %>% filter(haushalt_gesamt == "5+"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "5+"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad[nrow(HouseholdDataMuspad) + 1, ] <- c("Household size 1/23", "5+", (MuSPADHousehold %>% filter(haushalt_gesamt == "5+"))$n, 100*(MuSPADHousehold %>% filter(haushalt_gesamt == "5+"))$n/sum(MuSPADHousehold$n), "MuSPAD", sum(MuSPADHousehold$n))
HouseholdDataMuspad$n <- as.integer(HouseholdDataMuspad$n)
HouseholdDataMuspad$sum <- as.integer(HouseholdDataMuspad$sum)
HouseholdDataMuspad$percent <- as.double(HouseholdDataMuspad$percent)
HouseholdDataMuspad$Source <- as.character(HouseholdDataMuspad$Source)
HouseholdDataMuspad$name <- factor(HouseholdDataMuspad$name, levels = c("Household size 2019","Household size 3/20","Household size Summer/21","Household size 1/23","Children < 14 in household","Persons > 14 in household","# Children < 18"))


palette <- function() {
  c("#ECA400", "#006992", "#27476E")
}

palette2 <- function() {
  c("#6b5200", "#27476E", "#091c33")
}

HouseholdPlot <- HouseholdData %>% filter(name != "Children < 14 in household") %>%
                  filter(name != "# Children < 18") %>%  filter(!is.na(name)) %>% filter(!is.na(value)) %>% 
                  group_by(name) %>% count(value) %>% mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% mutate(Source = "External Survey") %>% 
                  rbind(HouseholdDataStatBundesamt) %>% 
                  rbind(HouseholdDataMuspad) %>% filter(name == "Household size 1/23") %>% filter(!is.na(Source)) %>%
                  mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(lci = 100/sum*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0, .default= lci)) %>%
                    mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(uci = 100/sum*uci) %>%
 ggplot(aes(value, percent)) +
  geom_bar(aes(fill=factor(Source, levels = c("External Survey", "MuSPAD", "Federal Statistical Office (2023)"))), stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=value, ymin=lci, ymax=uci, colour = factor(Source, levels = c("External Survey", "MuSPAD", "Federal Statistical Office (2023)"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  #ylab("") +
  ylab("Share (Percentage)") +
  xlab("Household size [# Members]") +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 37)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

ggsave("HouseholdSize.png", HouseholdPlot, dpi = 500, w = 9.5, h = 6)
ggsave("HouseholdSize.pdf", HouseholdPlot, dpi = 500, w = 9.5, h = 6)

# Children under 14 ------------------------------------------------------------------

palette <- function() {
  c("#ECA400", "#006992", "#27476E")
}

palette2 <- function() {
  c("#6b5200", "#27476E", "#091c33")
}

Children <- data_reduced %>% select(respondent_hsld_size_persons_under_14) %>%
                            mutate(respondent_hsld_size_persons_under_14 = case_when(respondent_hsld_size_persons_under_14  == 0 ~ "0",
                            respondent_hsld_size_persons_under_14  == 1 ~ "1",
                            respondent_hsld_size_persons_under_14  == 2 ~ "2",
                            respondent_hsld_size_persons_under_14  == 3 ~ "3+",
                            respondent_hsld_size_persons_under_14  == 4 ~ "3+"))

MuSPAD_s22 <- MuSPAD_s22 %>% rename(ChildrenUnder14 = household_under14) 


ChildrenMuspad <- MuSPAD_s22 %>% select(ChildrenUnder14) %>%
                            mutate(ChildrenUnder14 = case_when(ChildrenUnder14  == 0 ~ "0",
                            ChildrenUnder14  == 1 ~ "1",
                            ChildrenUnder14  == 2 ~ "2",
                            ChildrenUnder14  == 3 ~ "3+",
                            ChildrenUnder14  > 3 ~ "3+"))

ChildrenMuspad <- ChildrenMuspad %>% count(ChildrenUnder14) %>% filter(!is.na(ChildrenUnder14))                  

ChildrenDataMuspad <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(ChildrenDataMuspad) <- c("respondent_hsld_size_persons_under_14", "n", "percent", "Source", "sum")
ChildrenDataMuspad[nrow(ChildrenDataMuspad) + 1, ] <- c("0", (ChildrenMuspad %>% filter(ChildrenUnder14 == "0"))$n, 100*(ChildrenMuspad %>% filter(ChildrenUnder14 == "0"))$n/sum(ChildrenMuspad$n), "MuSPAD", sum(ChildrenMuspad$n))
ChildrenDataMuspad[nrow(ChildrenDataMuspad) + 1, ] <- c("1",	(ChildrenMuspad %>% filter(ChildrenUnder14 == "1"))$n, 100*(ChildrenMuspad %>% filter(ChildrenUnder14 == "1"))$n/sum(ChildrenMuspad$n), "MuSPAD", sum(ChildrenMuspad$n))
ChildrenDataMuspad[nrow(ChildrenDataMuspad) + 1, ] <- c("2",	(ChildrenMuspad %>% filter(ChildrenUnder14 == "2"))$n, 100*(ChildrenMuspad %>% filter(ChildrenUnder14 == "2"))$n/sum(ChildrenMuspad$n), "MuSPAD", sum(ChildrenMuspad$n))
ChildrenDataMuspad[nrow(ChildrenDataMuspad) + 1, ] <- c("3+",	(ChildrenMuspad %>% filter(ChildrenUnder14 == "3+"))$n, 100*(ChildrenMuspad %>% filter(ChildrenUnder14 == "3+"))$n/sum(ChildrenMuspad$n), "MuSPAD", sum(ChildrenMuspad$n))
ChildrenDataMuspad$n <- as.integer(ChildrenDataMuspad$n)
ChildrenDataMuspad$sum <- as.integer(ChildrenDataMuspad$sum)


ChildrenDataMuspad$respondent_hsld_size_persons_under_14 <- factor(ChildrenDataMuspad$respondent_hsld_size_persons_under_14, levels = c("0", "1", "2", "3+"))
ChildrenDataMuspad$n <- as.integer(ChildrenDataMuspad$n)
ChildrenDataMuspad$percent <- as.double(ChildrenDataMuspad$percent)

ChildrenPlot <- Children %>% filter(!is.na(respondent_hsld_size_persons_under_14)) %>% 
                  count(respondent_hsld_size_persons_under_14) %>% 
                  mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% 
                  mutate(Source = "External Survey") %>% 
                  rbind(ChildrenDataMuspad) %>% 
                  filter(!is.na(Source)) %>%
                  mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(lci = 100/sum*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0, .default= lci)) %>%
                    mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(uci = 100/sum*uci) %>%
ggplot(aes(respondent_hsld_size_persons_under_14, percent)) +
  geom_bar(aes(fill=factor(Source, levels = c("External Survey", "MuSPAD"))), stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=respondent_hsld_size_persons_under_14, ymin=lci, ymax=uci, colour = factor(Source, levels = c("External Survey", "MuSPAD", "Federal Statistical Office (2023)"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  ylab("") +
  #ylab("Share (Percentage)") +
  xlab("Children Under 14") +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values=palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 37)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

ggsave("Children_Comparison.png", ChildrenPlot, dpi = 500, w = 9.5, h = 6)
ggsave("Children_Comparison.pdf", ChildrenPlot, dpi = 500, w = 9.5, h = 6)


# Gender ------------------------------------------------------------------

palette <- function() {
  c("#ECA400", "#006992", "#27476E")
}

palette2 <- function() {
  c("#6b5200", "#27476E", "#091c33")
}

GenderData <- data_reduced %>% select(gender)

GenderData <- GenderData %>% mutate(gender = case_when(gender == "Weiblich" ~ "female", 
                                                        gender == "Männlich" ~ "male",
                                                        gender == "Divers" ~ "diverse",
                                                        gender == "Ich möchte nicht antworten" ~ "I Don't Want To Answer")) %>% 
                                                        filter(gender != "I Don't Want To Answer")

GenderData$gender <- factor(GenderData$gender, levels = c("female", "male", "diverse", "I Don't Want To Answer"))

#https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/deutsche-nichtdeutsche-bevoelkerung-nach-geschlecht-deutschland.html
GenderDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(GenderDataStatBundesamt) <- c("gender", "n", "percent", "Source", "sum")
GenderDataStatBundesamt[nrow(GenderDataStatBundesamt) + 1, ] <- c("female",42885791, 100*42885791/84669326, "Federal Statistical Office (2023)", 84669326)
GenderDataStatBundesamt[nrow(GenderDataStatBundesamt) + 1, ] <- c("male",	41783535, 100*41783535/84669326, "Federal Statistical Office (2023)", 84669326)
GenderDataStatBundesamt[nrow(GenderDataStatBundesamt) + 1, ] <- c("diverse",	0, 0, "Federal Statistical Office (2023)", 84669326)
GenderDataStatBundesamt$gender <- factor(GenderDataStatBundesamt$gender, levels = c("female", "male", "diverse"))
GenderDataStatBundesamt$n <- as.integer(GenderDataStatBundesamt$n)
GenderDataStatBundesamt$sum <- as.integer(GenderDataStatBundesamt$sum)
GenderDataStatBundesamt$percent <- as.double(GenderDataStatBundesamt$percent)

GenderMus <- MuSPAD_s22 %>% select(sex) %>% count(sex) %>% filter(!is.na(sex)) %>% filter(sex != "")

GenderDataMuspad <- data.frame(matrix(nrow = 0, ncol = 5))

colnames(GenderDataMuspad) <- c("gender", "n", "percent", "Source", "sum")
GenderDataMuspad[nrow(GenderDataMuspad) + 1, ] <- c("female", (GenderMus %>% filter(sex == "female"))$n, 100*(GenderMus %>% filter(sex == "female"))$n/sum(GenderMus$n), "MuSPAD", (GenderMus %>% filter(sex == "female"))$n+(GenderMus %>% filter(sex == "male"))$n+(GenderMus %>% filter(sex == "diverse"))$n)
GenderDataMuspad[nrow(GenderDataMuspad) + 1, ] <- c("male",	(GenderMus %>% filter(sex == "male"))$n, 100*(GenderMus %>% filter(sex == "male"))$n/sum(GenderMus$n), "MuSPAD",(GenderMus %>% filter(sex == "female"))$n+(GenderMus %>% filter(sex == "male"))$n+(GenderMus %>% filter(sex == "diverse"))$n)
GenderDataMuspad[nrow(GenderDataMuspad) + 1, ] <- c("diverse",	(GenderMus %>% filter(sex == "diverse"))$n, 100*(GenderMus %>% filter(sex == "diverse"))$n/sum(GenderMus$n),"MuSPAD", (GenderMus %>% filter(sex == "female"))$n+(GenderMus %>% filter(sex == "male"))$n+(GenderMus %>% filter(sex == "diverse"))$n)
GenderDataMuspad$gender <- factor(GenderDataMuspad$gender, levels = c("female", "male", "diverse"))
GenderDataMuspad$n <- as.integer(GenderDataMuspad$n)
GenderDataMuspad$sum <- as.integer(GenderDataMuspad$sum)
GenderDataMuspad$percent <- as.double(GenderDataMuspad$percent)

GenderPlot <- GenderData %>% count(gender) %>% mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% mutate(Source = "External Survey") %>% 
                  rbind(GenderDataStatBundesamt) %>%
                  rbind(GenderDataMuspad) %>%
                    mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(lci = 100/sum*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0, .default= lci)) %>%
                    mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(uci = 100/sum*uci) %>%
ggplot(aes(gender, percent)) +
  geom_bar(aes(fill=factor(Source, levels = c("External Survey", "MuSPAD", "Federal Statistical Office (2023)"))), stat = "identity", position = "dodge", width = 0.8) +
    geom_errorbar(aes(x=gender, ymin=lci, ymax=uci, colour = factor(Source, levels = c("External Survey", "MuSPAD", "Federal Statistical Office (2023)"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  ylab("Share (Percentage)") +
  xlab("Gender") +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 37)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

ggsave("Gender_Comparison.pdf", GenderPlot, dpi = 500, w = 9.5, h = 6)
ggsave("Gender_Comparison.png", GenderPlot, dpi = 500, w = 9.5, h = 6)

# Age ---------------------------------------------------------------------

palette <- function() {
  c("#ECA400", "#006992", "#27476E")
}

palette2 <- function() {
  c("#6b5200", "#27476E", "#091c33")
}

AgeData <- data_reduced %>% select(year_of_birth) %>% mutate(age = 2023-year_of_birth) %>%
          mutate(age_bracket = case_when(age < 20 ~ "Below 20 (*)",
                                        age < 40 ~ "20-39",
                                        age < 60 ~ "40-59",
                                        age < 80 ~ "60-79",
                                        age < 100 ~ "80-99")) 
AgeData$age_bracket <- factor(AgeData$age_bracket, levels = c("Below 20 (*)", "20-39", "40-59", "60-79", "80-99"))

# Data from https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/bevoelkerung-altersgruppen-deutschland.html
AgeDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(AgeDataStatBundesamt) <- c("age_bracket", "n", "percent", "source", "sum")
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("Below 20 (*)", 84669326*0.188, 18.8, "Federal Statistical Office (2023)", 84669326)
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("20-39", 84669326*0.245, 24.5, "Federal Statistical Office (2023)", 84669326)
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("40-59", 84669326*0.268, 26.8, "Federal Statistical Office (2023)", 84669326)
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("60-79", 84669326*0.226, 22.6, "Federal Statistical Office (2023)", 84669326)
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("80-99", 84669326*0.072, 7.2, "Federal Statistical Office (2023)", 84669326)
AgeDataStatBundesamt$n <- as.integer(AgeDataStatBundesamt$n)
AgeDataStatBundesamt$sum <- as.integer(AgeDataStatBundesamt$sum)
AgeDataStatBundesamt$percent <- as.double(AgeDataStatBundesamt$percent)
AgeDataStatBundesamt$age_bracket <- factor(AgeDataStatBundesamt$age_bracket, levels = c("Below 20 (*)", "20-39", "40-59", "60-79", "80-99"))

AgeMuspad <- MuSPAD_s22 %>% select(birth_date_yyyy, age, age_floor, age_group) %>% 
                        filter(age_floor != 17) %>% filter(!is.na(age_floor)) %>% filter(is.finite(age_floor)) %>%
                        mutate(age_bracket = case_when(age_floor >= 80 ~ "80-99",
                                                                age_floor >= 60 ~ "60-79",
                                                                age_floor >= 40 ~ "40-59",
                                                                age_floor >= 20 ~ "20-39",
                                                                age_floor < 20 ~ "Below 20 (*)")) %>% count(age_bracket)

AgeDataMuspad <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(AgeDataMuspad) <- c("age_bracket", "n", "percent", "source", "sum")
AgeDataMuspad[nrow(AgeDataMuspad) + 1, ] <- c("Below 20 (*)", (AgeMuspad %>% filter(age_bracket=="Below 20 (*)"))$n, 100*(AgeMuspad %>% filter(age_bracket=="Below 20 (*)"))$n/sum(AgeMuspad$n), "MuSPAD", sum(AgeMuspad$n))
AgeDataMuspad[nrow(AgeDataMuspad) + 1, ] <- c("20-39", (AgeMuspad %>% filter(age_bracket=="20-39"))$n, 100*(AgeMuspad %>% filter(age_bracket=="20-39"))$n/sum(AgeMuspad$n), "MuSPAD", sum(AgeMuspad$n))
AgeDataMuspad[nrow(AgeDataMuspad) + 1, ] <- c("40-59", (AgeMuspad %>% filter(age_bracket=="40-59"))$n, 100*(AgeMuspad %>% filter(age_bracket=="40-59"))$n/sum(AgeMuspad$n), "MuSPAD", sum(AgeMuspad$n))
AgeDataMuspad[nrow(AgeDataMuspad) + 1, ] <- c("60-79", (AgeMuspad %>% filter(age_bracket=="60-79"))$n, 100*(AgeMuspad %>% filter(age_bracket=="60-79"))$n/sum(AgeMuspad$n), "MuSPAD", sum(AgeMuspad$n))
AgeDataMuspad[nrow(AgeDataMuspad) + 1, ] <- c("80-99", (AgeMuspad %>% filter(age_bracket=="80-99"))$n, 100*(AgeMuspad %>% filter(age_bracket=="80-99"))$n/sum(AgeMuspad$n), "MuSPAD", sum(AgeMuspad$n))
AgeDataMuspad$n <- as.integer(AgeDataMuspad$n)
AgeDataMuspad$sum <- as.integer(AgeDataMuspad$sum)
AgeDataMuspad$percent <- as.double(AgeDataMuspad$percent)
AgeDataMuspad$age_bracket <- factor(AgeDataMuspad$age_bracket, levels = c("Below 20 (*)", "20-39", "40-59", "60-79", "80-99"))

AgePlot <- AgeData %>% filter(!is.na(age_bracket)) %>% count(age_bracket) %>% 
            mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% 
            mutate(source = "External Survey") %>%
            rbind(AgeDataStatBundesamt) %>%
            rbind(AgeDataMuspad) %>%
            mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(lci = 100/sum*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0, .default= lci)) %>%
                    mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(uci = 100/sum*uci) %>%
ggplot(aes(age_bracket, percent)) +
  geom_bar(aes(fill=factor(source, levels = c("External Survey", "MuSPAD", "Federal Statistical Office (2023)"))), stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=age_bracket, ymin=lci, ymax=uci, colour = factor(source, levels = c("External Survey", "MuSPAD", "Federal Statistical Office (2023)"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  scale_color_manual(values = palette2())+
  theme_minimal() +
    theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  ylab("") +
  #ylab("Share (Percentage)") +
  xlab("Age Bracket (2023)") +
  scale_fill_manual(values = palette()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 37)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

ggsave("Age_Comparison.pdf", dpi = 500, w = 9.5, h = 6)
ggsave("Age_Comparison.png", dpi = 500, w = 9.5, h = 6)

# Education / Occupation --------------------------------------------------

#Education 

palette <- function() {
  c("#ECA400", "#006992", "#27476E")
}

palette2 <- function() {
  c("#6b5200", "#27476E", "#091c33")
}

educationLevel <- data_reduced %>% select(highest_educational_qualification)

educationLevel <- educationLevel %>% mutate(highest_educational_qualification = case_when(highest_educational_qualification == "Haupt-/ Volksschulabschluss" ~ "Certification\nafter 9 years",
                                                                                          highest_educational_qualification == "Realschulabschluss" ~ "Certification\nafter 10 years",
                                                                                          highest_educational_qualification == "Abitur / Fachhochschulabitur" ~ "Higher Education",
                                                                                          highest_educational_qualification == "Anderer" ~ "Other/None"))

educationLevel$highest_educational_qualification <- factor(educationLevel$highest_educational_qualification, levels = c("Higher Education", "Certification\nafter 10 years", "Certification\nafter 9 years", "Other/None"))

#https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bildung-Forschung-Kultur/Bildungsstand/Tabellen/bildungsabschluss.html
EducationDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(EducationDataStatBundesamt ) <- c("highest_educational_qualification", "n", "percent", "source", "sum")
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Higher Education", 82000000*0.335*100, 33.5, "Federal Statistical Office (2019)", )
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Certification\nafter 10 years", 82000000*0.3*100, 23.5+6.5, "Federal Statistical Office (2019)")
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Certification\nafter 9 years", 82000000*0.286*100, 28.6, "Federal Statistical Office (2019)")
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Other/None", 82000000*0.077*100, 3.5+0.2+4.0, "Federal Statistical Office (2019)")
EducationDataStatBundesamt$n <- as.integer(EducationDataStatBundesamt $n)
EducationDataStatBundesamt$sum  <- as.integer(EducationDataStatBundesamt$sum)
EducationDataStatBundesamt $percent <- as.double(EducationDataStatBundesamt $percent)
EducationDataStatBundesamt $highest_educational_qualification <- factor(EducationDataStatBundesamt $highest_educational_qualification, levels = c("Higher Education", "Certification\nafter 10 years", "Certification\nafter 9 years", "Other/None"))

Education <- MuSPAD_s22 %>% 
                        select(education) %>% 
                        filter(!is.na(education)) %>% 
                        filter(education != "") %>% 
                        filter(education != "Ich möchte nicht antworten") %>% 
                        mutate(education = case_when(education == "Abitur / Fachhochschulabitur" ~ "Higher Education",
                        education == "Abitur/Fachhochschulabitur" ~ "Higher Education",
                        education == "Anderer" ~ "Other/None",
                        education == "Haupt-/ Volksschulabschluss" ~ "Certification\nafter 9 years",
                        education == "Haupt-/Volksschulabschluss" ~ "Certification\nafter 9 years",
                        education == "Keinen Schulabschluss" ~ "Other/None",
                        education == "Realschulabschluss" ~ "Certification\nafter 10 years")) %>% 
                        count(education) 

EducationDataMuspad <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(EducationDataMuspad) <- c("highest_educational_qualification", "n", "percent", "source", "sum")
EducationDataMuspad[nrow(EducationDataMuspad) + 1, ] <- c("Higher Education", (Education %>% filter(education == "Higher Education"))$n, 100*(Education %>% filter(education == "Higher Education"))$n/sum(Education$n), "MuSPAD", sum(Education$n))
EducationDataMuspad[nrow(EducationDataMuspad) + 1, ] <- c("Certification\nafter 10 years", (Education %>% filter(education == "Certification\nafter 10 years"))$n, 100*(Education %>% filter(education == "Certification\nafter 10 years"))$n/sum(Education$n), "MuSPAD", sum(Education$n))
EducationDataMuspad[nrow(EducationDataMuspad) + 1, ] <- c("Certification\nafter 9 years", (Education %>% filter(education == "Certification\nafter 9 years"))$n, 100*(Education %>% filter(education == "Certification\nafter 9 years"))$n/sum(Education$n), "MuSPAD", sum(Education$n))
EducationDataMuspad[nrow(EducationDataMuspad) + 1, ] <- c("Other/None", (Education %>% filter(education == "Other/None"))$n, 100*(Education %>% filter(education == "Other/None"))$n/sum(Education$n), "MuSPAD", sum(Education$n)) 
EducationDataMuspad$n <- as.integer(EducationDataMuspad$n)
EducationDataMuspad$sum <- as.integer(EducationDataMuspad$sum)
EducationDataMuspad$percent <- as.double(EducationDataMuspad$percent)
EducationDataMuspad$highest_educational_qualification <- factor(EducationDataMuspad$highest_educational_qualification, levels = c("Higher Education", "Certification\nafter 10 years", "Certification\nafter 9 years", "Other/None"))

EducationPlot <- educationLevel %>% filter(!is.na(highest_educational_qualification)) %>% 
            filter(highest_educational_qualification != "Other") %>%
            count(highest_educational_qualification) %>% 
            mutate(percent = 100 * n / sum(n), sum=sum(n)) %>% 
            mutate(source = "External Survey") %>%
            rbind(EducationDataMuspad) %>%
            rbind(EducationDataStatBundesamt) %>%
            mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(lci = 100/sum*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0, .default= lci)) %>%
                    mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(uci = 100/sum*uci) %>%
ggplot(aes(highest_educational_qualification, percent)) +
  geom_bar(aes(fill=factor(source, levels = c("External Survey", "MuSPAD", "Federal Statistical Office (2019)"))), stat = "identity", position = "dodge", width = 0.8) +
      geom_errorbar(aes(x=highest_educational_qualification, ymin=lci, ymax=uci, colour = factor(source, levels = c("External Survey", "MuSPAD", "Federal Statistical Office (2023)"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  #ylab("") +
  ylab("Share (Percentage)") +
  xlab("Education") +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 37)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=0.7))

ggsave("EducationLevel_Comparison.pdf", EducationPlot, dpi = 500, w =9.5, h = 9)
ggsave("EducationLevel_Comparison.png", EducationPlot, dpi = 500, w =9.5, h = 9)


#Occupation

# To do: The following section needs to be updated once we've received the according data from MuSPAD

currentOccupation <- raw_data %>% select(current_occupation)

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

OccupationMuSPAD <- MuSPAD_s22 %>% count(employment) %>% filter(employment != "") %>%
                    mutate(employment = case_when(employment == "Andere (z.B. Elternzeit, Sabbatical)" ~ "Other",
                                                  employment == "Mutterschafts-, Erziehungsurlaub, Elternzeit oder sonstige Beurlaubung" ~ "Other",
                                                  employment == "Ich bin Rentner:In oder Pensionär:In" ~ "Retired",
                                                  employment == "Ich bin Rentner:in oder Pensionär:in." ~ "Retired",
                                                  employment == "Ich bin als Lehrer:In oder Erzieher:In tätig" ~ "Teaching Sector",
                                                  employment == "Ich bin als Lehrer:in oder Erzieher:in tätig." ~ "Teaching Sector",
                                                  employment == "Ich bin arbeitssuchend" ~ "Unemployed",
                                                  employment == "Ich bin arbeitssuchend." ~ "Unemployed",
                                                  employment == "Ich bin im Studium oder in der Ausbildung" ~ "Student",
                                                  employment == "Ich bin im Studium oder in der Ausbildung." ~ "Student",
                                                  employment == "Ich bin in einem anderen Beruf tätig" ~ "Other",
                                                  employment == "Ich bin in einem anderen Beruf tätig." ~ "Other",
                                                  employment == "Ich bin in einem medizinischen oder pflegerischen Beruf bei einem Gesundheitsversorger tätig" ~ "Medical Sector",
                                                  employment == "Ich bin in einem medizinischen oder pflegerischen Beruf bei einem Gesundheitsversorger tätig." ~ "Medical Sector")) %>%
                                                  group_by(employment) %>% summarise(n = sum(n))


OccupationDataMuspad <- data.frame(matrix(nrow = 0, ncol = 5))
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Other", (OccupationMuSPAD %>% filter(employment == "Other"))$n, 100*(OccupationMuSPAD %>% filter(employment == "Other"))$n/sum(OccupationMuSPAD$n), "MuSPAD", sum(OccupationMuSPAD$n))
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Teaching Sector", (OccupationMuSPAD %>% filter(employment == "Teaching Sector"))$n, 100*(OccupationMuSPAD %>% filter(employment == "Teaching Sector"))$n/sum(OccupationMuSPAD$n), "MuSPAD", sum(OccupationMuSPAD$n))
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Medical Sector", (OccupationMuSPAD %>% filter(employment == "Medical Sector"))$n, 100*(OccupationMuSPAD %>% filter(employment == "Medical Sector"))$n/sum(OccupationMuSPAD$n), "MuSPAD", sum(OccupationMuSPAD$n))
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Unemployed", (OccupationMuSPAD %>% filter(employment == "Unemployed"))$n, 100*(OccupationMuSPAD %>% filter(employment == "Unemployed"))$n/sum(OccupationMuSPAD$n), "MuSPAD", sum(OccupationMuSPAD$n))
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Student", (OccupationMuSPAD %>% filter(employment == "Student"))$n, 100*(OccupationMuSPAD %>% filter(employment == "Student"))$n/sum(OccupationMuSPAD$n), "MuSPAD", sum(OccupationMuSPAD$n))
OccupationDataMuspad[nrow(OccupationDataMuspad) + 1, ] <- c("Retired", (OccupationMuSPAD %>% filter(employment == "Retired"))$n, 100*(OccupationMuSPAD %>% filter(employment == "Retired"))$n/sum(OccupationMuSPAD$n), "MuSPAD", sum(OccupationMuSPAD$n))
colnames(OccupationDataMuspad) <- c("current_occupation", "n", "percent", "source", "sum")
OccupationDataMuspad$n <- as.integer(OccupationDataMuspad$n)
OccupationDataMuspad$sum <- as.integer(OccupationDataMuspad$sum)
OccupationDataMuspad$percent <- as.double(OccupationDataMuspad$percent)

OccupationPlot <- currentOccupation %>% filter(!is.na(current_occupation)) %>% 
            count(current_occupation) %>% 
            mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% 
            mutate(source = "External Survey") %>%
            rbind(OccupationDataMuspad) %>%
            mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(lci = 100/sum*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0, .default= lci)) %>%
                    mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(uci = 100/sum*uci) %>%
ggplot(aes(current_occupation, percent)) +
  geom_bar(aes(fill=factor(source, levels = c("External Survey", "MuSPAD"))), stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=current_occupation, ymin=lci, ymax=uci, colour = factor(source, levels = c("External Survey", "MuSPAD", "Federal Statistical Office (2023)"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  scale_color_manual(values = palette2()) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  ylab("") +
  xlab("Current Occupation") +
  scale_fill_manual(values = palette()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 37)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=0.7))

ggsave("Occupation_Comparison.pdf", OccupationPlot, dpi = 500, w =9.5, h = 9)
ggsave("Occupation_Comparison.png", OccupationPlot, dpi = 500, w =9.5, h = 9)

## All plots together
#plot_grid(GenderPlot, AgePlot, HouseholdPlot, ChildrenPlot, EducationPlot, OccupationPlot, labels = "AUTO", nrow = 3, label_size = 24, rel_heights = c(1,1,1.25))

ggarrange(GenderPlot, AgePlot, HouseholdPlot, ChildrenPlot, EducationPlot, OccupationPlot, labels = c("A", "B", "C", "D", "E", "F"), nrow = 3, ncol = 2,font.label = list(size = 37), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")

ggsave("DemographicComparison.pdf", dpi = 500, w = 24, h = 27)
ggsave("DemographicComparison.png", dpi = 500, w = 24, h = 27)


# Infection context -------------------------------------------------------

# To discuss: Would we like to include a pie chart(?) on the infection context?

ownInfection <- data_reduced %>% select(loc_home, loc_work, loc_school, loc_friends,
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
