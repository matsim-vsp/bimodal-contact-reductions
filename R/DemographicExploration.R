library(tidyverse)
library(ggpubr)
library(scales)
library(ggrepel)
library(here)

here()
source("/Users/sydney/git/second-order-contacts/R/DataCleaningPrepForContactAnalysis.R")
source("/Users/sydney/git/second-order-contacts/R/mytheme.R")

genderdf <- data_reduced %>% count(gender) %>%   
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))

genderdf <- genderdf %>% mutate(gender_eng = case_when(gender == "Männlich" ~"male", 
                                                       gender == "Weiblich" ~"female", 
                                                       gender == "Divers" ~"diverse", 
                                                       gender == "Ich möchte nicht antworten" ~"I Don't Want To Answer"))

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )


palette <- function() {
  c("#B09C85FF", "#3C5488FF", "#DC0000FF")
}

GenderPlot <- ggplot(genderdf %>% filter(gender != "Ich möchte nicht antworten"), aes(x="", y=n, fill=gender_eng))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = palette()) + 
  blank_theme +
  # my_theme() +
  theme(axis.text.x=element_blank(), legend.title = element_blank()) +
  theme(legend.position = "right", legend.key.size = unit(1.2, 'cm')) +
  xlab("") +
  ylab("") +
  theme(element_text(size = 40), legend.text = element_text(size=30))
# geom_label_repel(data = gender %>% filter(gender != "Ich möchte nicht antworten"),
#                aes(y = pos, label = paste0(round(100*n/sum(n)), "%")),
#                size = 4.5, nudge_x = 1, show.legend = FALSE)

#ggsave("Gender.png", GenderPlot, dpi = 500, w = 7.5, h = 7.5)

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
ggsave("Age.png", AgePlot, dpi = 500, w = 7.5, h = 7.5)

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

ggsave("Comorbidities.png", ComorbiditiesPlot, dpi = 500, w = 7.5, h = 7.5)


ggarrange(GenderPlot, AgePlot, ggparagraph(text="   ", face = "italic", size = 14, color = "black"), ggparagraph(text="   ", face = "italic", size = 14, color = "black") , HouseholdPlot, ComorbiditiesPlot, labels = c("A", "B", "", "", "C", "D"), nrow = 3, ncol = 2,font.label = list(size = 40), heights = c(1,0.05,1))

ggsave("BasicInfoSurvey.pdf", dpi = 500, w = 19, h = 18)
ggsave("BasicInfoSurvey.png", dpi = 500, w = 19, h = 18)
