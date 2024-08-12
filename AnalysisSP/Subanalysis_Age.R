library(tidyverse)
library(MMWRweek)
library(see)
library(RColorBrewer)

# Carefulness Due To Old Age (18-59 vs 60+) ----------------------------------------------
raw_data <- readRDS(file = "/Users/sydney/Desktop/cleaned_data.rds") #Place to enter the data's path

data_reduced <- raw_data %>% select(num_c19_infs, date_f1_inf, date_s2_inf, date_t3_inf, year_of_birth, cond_none) %>%
                            mutate(age_group = case_when (year_of_birth <= 1960 ~ "60+",
                                                            year_of_birth > 1960 ~ "18-59"))

data_reduced <- data_reduced %>% mutate(date_f1_inf = case_when(is.na(date_f1_inf) ~ as.Date("2025-01-01"),
                                        .default = as.Date(as.character(date_f1_inf)))) %>%
                                filter(date_f1_inf != as.Date("1922-03-01")) %>%
                                filter(date_f1_inf != as.Date("1965-06-12")) %>%
                                filter(date_f1_inf != as.Date("2000-12-13")) %>%
                                filter(date_f1_inf != as.Date("2019-12-21")) 

data_reduced <- data_reduced %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "Never",
                                                                    num_c19_infs == "Einmal" ~ "Once",
                                                                    num_c19_infs == "Zweimal" ~ "Twice",
                                                                    num_c19_infs == "Dreimal" ~ "Three Times",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "More Than Three Times",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

data_reduced$num_c19_infs_eng <- factor(data_reduced$num_c19_infs_eng, levels = c("Never", "Once", "Twice", "Three Times", "More Than Three Times", "I Don't Want To Answer"))

palette <- function() {
  c("#fd5901", "#008083")
}

ggplot(data_reduced %>% filter(!is.na(age_group)), aes(date_f1_inf, color = age_group)) +
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

ggsave("TimingOfInfection_AggregatedAgeBrackets.pdf", dpi = 500, w = 9, h = 12)
ggsave("TimingOfInfection_AggregatedAgeBrackets.png", dpi = 500, w = 9, h = 12)

data_reduced %>%
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

ggsave("NumberOfInfection_AggregatedAgeBrackets.pdf", dpi = 500, w = 9, h = 12)
ggsave("NumberOfInfection_AggregatedAgeBrackets.png", dpi = 500, w = 9, h = 12)

# Detailed Analysis By Age ----------------------------------------------

source("DataCleaningPrepForContactAnalysis.R")

data_reduced <- data_reduced %>% mutate(age_group = case_when (year_of_birth <= 1960 ~ "60+",
                                                            year_of_birth > 1960 ~ "18-59"))

data_reduced %>% count(age_bracket)

#data_reduced_children <- data_reduced %>% filter(hhmember_school_2019 > 0)

data_reduced <- data_reduced %>% select(-c(respondent_hsld_size_persons_under_14, number_of_children_under_18)) %>%
                                  select(-contains("attitudes")) %>%
                                  select(-contains("beh_change")) %>%
                                  select(-contains("cond"))

data_reduced <- data_reduced %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "Never",
                                                                    num_c19_infs == "Einmal" ~ "Once",
                                                                    num_c19_infs == "Zweimal" ~ "Twice",
                                                                    num_c19_infs == "Dreimal" ~ "Three Times",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "More Than Three Times",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

data_reduced$num_c19_infs_eng <- factor(data_reduced$num_c19_infs_eng, levels = c("Never", "Once", "Twice", "Three Times", "More Than Three Times", "I Don't Want To Answer"))

data_reduced <- data_reduced %>% mutate(date_f1_inf = case_when(is.na(date_f1_inf) ~ as.Date("2025-01-01"),
                                        .default = as.Date(as.character(date_f1_inf)))) %>%
                                filter(date_f1_inf != as.Date("1922-03-01")) %>%
                                filter(date_f1_inf != as.Date("1965-06-12")) %>%
                                filter(date_f1_inf != as.Date("2000-12-13")) %>%
                                filter(date_f1_inf != as.Date("2019-12-21")) 

data_reduced <- data_reduced %>% pivot_longer(cols = 5:80)

data_reduced <- data_reduced  %>% mutate(time = case_when(str_detect(name, "2019") ~ "2019",
                                                          str_detect(name, "2020") ~ "03/2020",
                                                          str_detect(name, "2021") ~ "Summer 2021",
                                                          str_detect(name, "2023") ~ "01/2023")) %>%
                                  mutate(WhoseContacts = case_when(str_detect(name, "respondent") ~ "Respondent",
                                  str_detect(name, "cc_pre") ~ "Closest Contact (Pre-Covid)",
                                  str_detect(name, "cc_during") ~ "Closest Contact (During-Covid)",
                                  str_detect(name, "hhmember") ~ "Household Member")) %>%
                                  mutate(TypeOfContact = case_when(str_detect(name, "work") ~ "Work",
                                  str_detect(name, "school") ~ "School",
                                  str_detect(name, "leisure") ~ "Leisure",
                                  str_detect(name, "all") ~ "All"))

data_reduced$time <- factor(data_reduced$time, levels = c("2019", "03/2020", "Summer 2021", "01/2023"))
data_reduced$TypeOfContact <- factor(data_reduced$TypeOfContact, levels = c("Work", "Leisure", "School", "All"))
data_reduced$WhoseContacts <- factor(data_reduced$WhoseContacts, levels = c("Respondent", "Household Member", "Closest Contact (Pre-Covid)", "Closest Contact (During-Covid)"))

palette <- function() {
  c("#fd5901", "#f78104", "#faab36", "#249ea0", "#008083", "#005f60")
}

ggplot(data_reduced %>% filter(WhoseContacts == "Respondent") %>% filter(value < 100) %>% filter(!is.na(age_bracket)) %>% filter(!is.na(TypeOfContact)), aes(age_bracket, value)) +
  #geom_violin(aes(color = WhoseContacts), width = 1, trim = FALSE, position=position_dodge(0.9)) + 
  #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
  geom_boxplot(aes(color = age_bracket), size = 1.3) +
  scale_color_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols = vars(time)) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  ylab("Reported No. Of Contacts") +
  theme(text = element_text(size = 30)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom") +
  labs(color ="Age Group") +
  theme(panel.spacing = unit(0.8, "cm", data = NULL))
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("CollectionBoxplots_AgeBrackets.pdf", dpi = 500, w = 13, h = 19)
ggsave("CollectionBoxplots_AgeBrackets.png", dpi = 500, w = 13, h = 19)

#Boxplots for aggegrated age groups
data_reduced <- data_reduced %>% mutate(age_group = case_when (year_of_birth <= 1960 ~ "60+",
                                                            year_of_birth > 1960 ~ "18-59"))

palette <- function() {
  c("#fd5901", "#008083")
}

ggplot(data_reduced %>% filter(WhoseContacts == "Respondent") %>% filter(value < 100) %>% filter(!is.na(age_group)) %>% filter(!is.na(TypeOfContact)), aes(age_group, value)) +
  #geom_violin(aes(color = WhoseContacts), width = 1, trim = FALSE, position=position_dodge(0.9)) + 
  #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
  geom_boxplot(aes(color = age_group), size = 1.3) +
  scale_color_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols = vars(time)) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  ylab("Reported No. Of Contacts") +
  theme(text = element_text(size = 30)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom") +
  labs(color ="Age Group") +
  theme(panel.spacing = unit(0.8, "cm", data = NULL))

ggsave("CollectionBoxplots_AggregatedAgeBrackets.pdf", dpi = 500, w = 13, h = 19)
ggsave("CollectionBoxplots_AggregatedAgeBrackets.png", dpi = 500, w = 13, h = 19)

data_reduced %>%
  count(age_bracket, num_c19_infs_eng) %>%
  mutate(n = n / sum(n), .by = 'age_bracket') %>%
  filter(!is.na(age_bracket)) %>% 
  ggplot(aes(num_c19_infs_eng, n, fill = age_bracket)) +
  geom_col(position = position_dodge(preserve = 'single')) +
  theme_minimal() +
  ylab("Relative Frequency") +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom") +
  labs(fill="Age Group") +
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette())

ggsave("NumberOfInfection_AgeBrackets.pdf", dpi = 500, w = 9, h = 12)
ggsave("NumberOfInfection_AgeBrackets.png", dpi = 500, w = 9, h = 12)

ggplot(data_reduced %>% filter(!is.na(age_bracket)), aes(date_f1_inf, color = age_bracket)) +
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

ggsave("TimingOfInfection_AgeBrackets.pdf", dpi = 500, w = 9, h = 12)
ggsave("TimingOfInfection_AgeBrackets.png", dpi = 500, w = 9, h = 12)
