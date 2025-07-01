library(tidyverse) #version 2.0.0
library(ggpubr) #version 0.6.0
library(scales) #version 1.3.0
library(ggrepel) #version 0.9.6
library(here) #version 1.0.1

here()
source("./R/DataCleaningPrepForContactAnalysis.R")
source("./R/mytheme.R")


# Gender Distribution -----------------------------------------------------

# Produces Supplementary Figure 1A

genderdf <- data_reduced %>% count(gender) %>%   
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))

genderdf <- genderdf %>% mutate(gender_eng = case_when(gender == "Männlich" ~"male", 
                                                       gender == "Weiblich" ~"female", 
                                                       gender == "Divers" ~"diverse", 
                                                       gender == "Ich möchte nicht antworten" ~"I Don't Want To Answer"))

genderdf <- genderdf %>% filter(gender != "Ich möchte nicht antworten") %>% mutate(percent = 100 * n / sum(n), sum = sum(n))

genderdf$gender_eng <- factor(genderdf$gender_eng, levels = c("female", "male", "diverse"))

GenderPlot <- ggplot(genderdf , aes(gender_eng, percent)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8, fill = "#3C5488FF") +
  #geom_errorbar(aes(x=age_bracket, ymin=lci, ymax=uci, colour = factor(source, levels = c("External Survey", "Federal Statistical Office, Federal Employment Agency", "MuSPAD"))), position = position_dodge(0.8), width = 0.3, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  ylab("Share (in percent)") +
  xlab("Gender") +
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

AgePlot <- ggplot(AgeData, aes(age_bracket, percent)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8, fill = "#3C5488FF") +
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

HouseholdPlot <- HouseholdData %>% mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% 
  ggplot(aes(respondent_hsld_size_2019, percent)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8, fill = "#3C5488FF") +
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
  geom_bar(stat = "identity", position = "dodge", width = 0.8, fill = "#3C5488FF") +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  ylab("Share (in percent)") +
  xlab("COVID-19\ncomorbidities") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  my_theme()

# Arrangement of subfigures and saving of final plot

ggarrange(GenderPlot, AgePlot, ggparagraph(text="   ", face = "italic", size = 14, color = "black"), ggparagraph(text="   ", face = "italic", size = 14, color = "black") , HouseholdPlot, ComorbiditiesPlot, labels = c("A", "B", "", "", "C", "D"), nrow = 3, ncol = 2,font.label = list(size = 40), heights = c(1,0.05,1))

ggsave(paste0("./plots/","SupplementaryFigure1.pdf"), dpi = 500, w = 19, h = 18)
ggsave(paste0("./plots/","SupplementaryFigure1.png"), dpi = 500, w = 19, h = 18)
