library(tidyverse) #version 2.0.0
library(ggpubr) #version 0.6.0
library(scales) #version 1.3.0
library(ggrepel) #version 0.9.6
library(here) #version 1.0.1

here()
source("./R/DataPrep.R")
source("./R/mytheme.R")


# Gender Distribution -----------------------------------------------------

# Produces Supplementary Figure 1A

genderdf <- data_reduced %>% count(gender) %>%   
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))

genderdf <- genderdf %>% mutate(gender = case_when(gender == "Männlich" ~"male", 
                                                       gender == "Weiblich" ~"female", 
                                                       gender == "Divers" ~"diverse", 
                                                       gender == "Ich möchte nicht antworten" ~"I Don't Want To Answer"))

genderdf <- genderdf %>% filter(gender != "I Don't Want To Answer") %>% filter (gender != "diverse") %>% mutate(percent = 100 * n / sum(n), sum = sum(n))


genderdf <- genderdf %>% mutate(Source = "Survey")
genderdf <- genderdf %>% select(gender, n, percent, Source, sum)

# Processing of federal statistical office data
#https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/deutsche-nichtdeutsche-bevoelkerung-nach-geschlecht-deutschland.html [accessed: 2025-02-10]
GenderDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(GenderDataStatBundesamt) <- c("gender", "n", "percent", "Source", "sum")
GenderDataStatBundesamt[nrow(GenderDataStatBundesamt) + 1, ] <- c("female",42885791, 100*42885791/84669326, "Federal Statistical Office", 84669326)
GenderDataStatBundesamt[nrow(GenderDataStatBundesamt) + 1, ] <- c("male",	41783535, 100*41783535/84669326, "Federal Statistical Office", 84669326)
#GenderDataStatBundesamt[nrow(GenderDataStatBundesamt) + 1, ] <- c("diverse",	0, 0, "Federal Statistical Office", 84669326)
GenderDataStatBundesamt$gender <- factor(GenderDataStatBundesamt$gender, levels = c("female", "male", "diverse"))
GenderDataStatBundesamt$n <- as.integer(GenderDataStatBundesamt$n)
GenderDataStatBundesamt$sum <- as.integer(GenderDataStatBundesamt$sum)
GenderDataStatBundesamt$percent <- as.double(GenderDataStatBundesamt$percent)

ExpProportions <- GenderDataStatBundesamt$percent/sum(GenderDataStatBundesamt$percent)
chisq.test(genderdf$n, p = ExpProportions)

genderdf <- rbind(genderdf, GenderDataStatBundesamt)

genderdf$gender <- factor(genderdf$gender, levels = c("female", "male", "diverse"))
genderdf$Source <- factor(genderdf$Source, levels = c("Survey", "Federal Statistical Office"))

GenderPlot <- ggplot(genderdf %>% filter(Source == "Survey"), aes(gender, percent, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge", width = 1, fill = "#8491b4ff") +
  geom_bar(data = genderdf %>% filter(Source == "Federal Statistical Office"), stat = "identity", position = "dodge", width = 1, alpha = 0, fill = NA, color =  "#000000", linewidth = 2) +
  #geom_step(data = genderdf %>% filter(Source == "Survey"), mapping = aes(x = gender, y = percent), stat = "identity", position = "dodge", width = 0.8) +
  #geom_errorbar(aes(x=age_bracket, ymin=lci, ymax=uci, colour = factor(source, levels = c("External Survey", "Federal Statistical Office, Federal Employment Agency", "MuSPAD"))), position = position_dodge(0.8), width = 0.3, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  ylab("Share (in percent)") +
  xlab("Gender") +
  #facet_wrap(~Source) +
  scale_fill_manual(values = manual_scale) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  my_theme() 


# Age Distribution --------------------------------------------------------

# Produces Supplementary Figure 1B

AgeData <- data_reduced %>% select(year_of_birth) %>% mutate(age = 2023-year_of_birth) %>%
  mutate(age_bracket = case_when(age < 20 ~ "18-39",
                                 age < 40 ~ "18-39",
                                 age < 60 ~ "40-59",
                                 age < 80 ~ "60-79",
                                 age < 100 ~ "80-99")) 
AgeData$age_bracket <- factor(AgeData$age_bracket, levels = c("18-39", "40-59", "60-79", "80-99"))

AgeData <- AgeData %>% count(age_bracket) %>% filter(!is.na(age_bracket)) %>% mutate(percent = 100 * n / sum(n), sum = sum(n))
AgeData <- AgeData %>% mutate(Source = "Survey")

# Processing of federal statistical office data
# Data from https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/bevoelkerung-altersgruppen-deutschland.html [accessed: 2025-02-10]
# https://de.statista.com/statistik/daten/studie/1174053/umfrage/minderjaehrige-in-deutschland-nach-altersgruppen/#:~:text=Kinder%20und%20Jugendliche%20in%20Deutschland%20nach%20Altersgruppen%202023&text=Zum%2031.,sechs%20bis%20einschlie%C3%9Flich%2014%20Jahren. [accessed: 2025-02-10]
AgeDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(AgeDataStatBundesamt) <- c("age_bracket", "n", "source", "sum")
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("18-39", 84669326*0.188-14300000+84669326*0.245, "Federal Statistical Office", 84669326-14300000)
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("40-59", 84669326*0.268, "Federal Statistical Office", 84669326-14300000)
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("60-79", 84669326*0.226, "Federal Statistical Office", 84669326-14300000)
AgeDataStatBundesamt[nrow(AgeDataStatBundesamt) + 1, ] <- c("80-99", 84669326*0.072, "Federal Statistical Office", 84669326-14300000)
AgeDataStatBundesamt$n <- as.integer(AgeDataStatBundesamt$n)
AgeDataStatBundesamt$sum <- as.integer(AgeDataStatBundesamt$sum)
AgeDataStatBundesamt <- AgeDataStatBundesamt %>% mutate(percent = 100*n/sum)
AgeDataStatBundesamt$age_bracket <- factor(AgeDataStatBundesamt$age_bracket, levels = c("18-39", "40-59", "60-79", "80-99"))

ks.test(AgeData$n, AgeDataStatBundesamt$n)

AgePlot <- ggplot(AgeData, aes(age_bracket, percent)) +
  geom_bar(stat = "identity", position = "dodge", width = 1, fill = "#8491b4ff") +
  geom_bar(data = AgeDataStatBundesamt, stat = "identity", position = "dodge", width = 1, alpha = 0, fill = NA, color =  "#000000", linewidth = 2) +
  #geom_errorbar(aes(x=age_bracket, ymin=lci, ymax=uci, colour = factor(source, levels = c("External Survey", "Federal Statistical Office, Federal Employment Agency", "MuSPAD"))), position = position_dodge(0.8), width = 0.3, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  ylab("Share (in percent)") +
  xlab("Age Bracket") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  my_theme() 
ggsave(paste0("./plots/","Age.png"), AgePlot, dpi = 500, w = 7.5, h = 7.5)
ggsave(paste0("./plots/","Age.pdf"), AgePlot, dpi = 500, w = 7.5, h = 7.5)


# Household Size Distribution ---------------------------------------------

# Produces Supplementary Figure 1C

data_reduced$respondent_hsld_size_2019 <- as.character(data_reduced$respondent_hsld_size_2019)
HouseholdData <- data_reduced %>% 
  mutate(respondent_hsld_size_2019 = case_when(respondent_hsld_size_2019 %in% c("5", "6", "80", "7", "8", "11") ~ "5+", .default = respondent_hsld_size_2019)) %>% 
  count(respondent_hsld_size_2019) %>%
  filter(!is.na(respondent_hsld_size_2019))

HouseholdData <- HouseholdData %>% mutate(percent = 100 * n / sum(n), sum = sum(n)) %>%
  mutate(Source = "Survey")

# Processing of federal statistical office data
#https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Haushalte-Familien/Tabellen/1-1-privathaushalte-haushaltsmitglieder.html
HouseholdDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(HouseholdDataStatBundesamt) <- c("name", "value", "n", "percent", "Source", "sum")
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "1", 84669326*0.411, 41.1, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "1", 84669326*0.411, 41.1, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "1", 84669326*0.411, 41.1, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "1", 84669326*0.411, 41.1, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "2", 84669326*0.335, 33.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "2", 84669326*0.335, 33.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "2", 84669326*0.335, 33.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "2", 84669326*0.335, 33.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "3", 84669326*0.119, 11.9, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "3", 84669326*0.119, 11.9, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "3", 84669326*0.119, 11.9, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "3", 84669326*0.119, 11.9, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "4", 84669326*0.095, 9.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "4", 84669326*0.095, 9.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "4", 84669326*0.095, 9.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "4", 84669326*0.095, 9.5, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 2019", "5+", 84669326*0.039, 3.9, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 3/20", "5+", 84669326*0.039, 3.9, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size Summer/21", "5+", 84669326*0.039, 3.9, "Federal Statistical Office, Federal Employment Agency", 84669326)
HouseholdDataStatBundesamt[nrow(HouseholdDataStatBundesamt) + 1, ] <- c("Household size 1/23", "5+", 84669326*0.039, 3.9, "Federal Statistical Office, Federal Employment Agency",84669326)
HouseholdDataStatBundesamt$n <- as.integer(HouseholdDataStatBundesamt$n)
HouseholdDataStatBundesamt$sum <- as.integer(HouseholdDataStatBundesamt$sum)
HouseholdDataStatBundesamt$percent <- as.double(HouseholdDataStatBundesamt$percent)
HouseholdDataStatBundesamt$name <- factor(HouseholdDataStatBundesamt$name, levels = c("Household size 2019","Household size 3/20","Household size Summer/21","Household size 1/23","Children < 14 in household","Persons > 14 in household","# Children < 18"))


HouseholdPlot <- ggplot(HouseholdData, aes(respondent_hsld_size_2019, percent)) +
  geom_bar(stat = "identity", position = "dodge", width = 1, fill = "#8491b4ff") +
  geom_bar(data = HouseholdDataStatBundesamt %>% filter(name == "Household size 2019"), aes(x = value, y = percent), stat = "identity", position = "dodge", width = 1, alpha = 0, fill = NA, color =  "#000000", linewidth = 2) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  ylab("Share (in percent)") +
  xlab("Household size\n(# Members)") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  my_theme()


# Comorbidity Distribution ------------------------------------------------

# Produces Supplementary Figure 1D

comorbidities <- data_reduced %>% count(cond_none) %>%
  mutate(cond_none_eng = case_when(cond_none == "Ja" ~"No", cond_none == "Nicht Gewählt" ~ "Yes"))

ComorbiditiesPlot <- comorbidities %>% filter(!is.na(cond_none)) %>% mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% 
  ggplot(aes(cond_none_eng, percent)) +
  geom_bar(stat = "identity", position = "dodge", width = 1, fill = "#8491b4ff") +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  ylab("Share (in percent)") +
  xlab("COVID-19\ncomorbidities") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  my_theme()


# Education ---------------------------------------------------------------

# Processing of external survey data
educationData <- data_reduced %>% select(highest_educational_qualification) %>%
  mutate(highest_educational_qualification = case_when(highest_educational_qualification == "Haupt-/ Volksschulabschluss" ~ "9 years",
                                                       highest_educational_qualification == "Realschulabschluss" ~ "10 years",
                                                       highest_educational_qualification == "Abitur / Fachhochschulabitur" ~ "Higher\neducation",
                                                       highest_educational_qualification == "Anderer" ~ "Other/\nnone"))
educationData$highest_educational_qualification <- factor(educationData$highest_educational_qualification, levels = c("Higher\neducation", "10 years", "9 years", "Other/\nnone"))

educationData <- educationData %>% count(highest_educational_qualification) %>% filter(!is.na(highest_educational_qualification)) %>% mutate(percent = 100 * n / sum(n), sum = sum(n))
educationData <- educationData %>% mutate(Source = "Survey")

# Processing of federal statistical office data
# https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bildung-Forschung-Kultur/Bildungsstand/Tabellen/bildungsabschluss.html [accessed 2025-02-10]
EducationDataStatBundesamt <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(EducationDataStatBundesamt ) <- c("highest_educational_qualification", "n", "percent", "source", "sum")
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Higher\neducation", 82000000*0.335, 33.5, "Federal Statistical Office, Federal Employment Agency", 82000000)
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("10 years", 82000000*0.3, 23.5+6.5, "Federal Statistical Office, Federal Employment Agency", 82000000)
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("9 years", 82000000*0.286, 28.6, "Federal Statistical Office, Federal Employment Agency", 82000000)
EducationDataStatBundesamt [nrow(EducationDataStatBundesamt ) + 1, ] <- c("Other/\nnone", 82000000*0.077, 3.5+0.2+4.0, "Federal Statistical Office, Federal Employment Agency", 82000000)
EducationDataStatBundesamt$n <- as.integer(EducationDataStatBundesamt $n)
EducationDataStatBundesamt$sum  <- as.integer(EducationDataStatBundesamt$sum)
EducationDataStatBundesamt $percent <- as.double(EducationDataStatBundesamt $percent)
EducationDataStatBundesamt $highest_educational_qualification <- factor(EducationDataStatBundesamt $highest_educational_qualification, levels = c("Higher\neducation", "10 years", "9 years", "Other/\nnone"))

EducationPlot <- ggplot(educationData, aes(highest_educational_qualification, percent)) +
  geom_bar(stat = "identity", position = "dodge", width = 1, fill = "#8491b4ff") +
  geom_bar(data = EducationDataStatBundesamt, aes(x = highest_educational_qualification, y = percent), stat = "identity", position = "dodge", width = 1, alpha = 0, fill = NA, color =  "#000000", linewidth = 2) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  ylab("Share (in percent)") +
  xlab("Education") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  my_theme()

# Arrangement of subfigures and saving of final plot

ggarrange(GenderPlot, AgePlot, ggparagraph(text="   ", face = "italic", size = 14, color = "black"), ggparagraph(text="   ", face = "italic", size = 14, color = "black") , HouseholdPlot, EducationPlot, labels = c("A", "B", "", "", "C", "D"), nrow = 3, ncol = 2,font.label = list(size = 40), heights = c(1,0.05,1))

ggsave(paste0("./plots/","SupplementaryFigure1.pdf"), dpi = 500, w = 19, h = 18)
ggsave(paste0("./plots/","SupplementaryFigure1.png"), dpi = 500, w = 19, h = 18)

