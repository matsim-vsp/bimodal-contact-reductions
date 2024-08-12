library(tidyverse)
library(MMWRweek)
library(see)
library(RColorBrewer)

# Author: S. Paltra, contact: paltra@tu-berlin.de

raw_data <- readRDS(file = "/Users/sydney/Desktop/cleaned_data.rds")

# Carefulness Due To Comorbidities ----------------------------------------

data_reduced <- raw_data %>% select(num_c19_infs, date_f1_inf, date_s2_inf, date_t3_inf,
                                    cond_hbp, cond_diabetes, cond_cardio, cond_resp,
                                    cond_immuno, cond_cancer, cond_post_c19, cond_none)

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

data_reduced <- data_reduced %>% mutate(cond_hbp  = case_when(cond_hbp == "Ja" ~ "Yes",
                                cond_hbp == "Nicht Gewählt" ~ "No")) %>% 
                                mutate(cond_diabetes = case_when(cond_diabetes == "Ja" ~ "Yes",
                                cond_diabetes == "Nicht Gewählt" ~ "No")) %>% 
                                mutate(cond_cardio = case_when(cond_cardio == "Ja" ~ "Yes",
                                cond_cardio == "Nicht Gewählt" ~ "No")) %>% 
                                mutate(cond_resp = case_when(cond_resp == "Ja" ~ "Yes",
                                cond_resp == "Nicht Gewählt" ~ "No")) %>% 
                                mutate(cond_immuno = case_when(cond_immuno == "Ja" ~ "Yes",
                                cond_immuno == "Nicht Gewählt" ~ "No")) %>% 
                                mutate(cond_cancer = case_when(cond_cancer == "Ja" ~ "Yes",
                                cond_cancer == "Nicht Gewählt" ~ "No")) %>%
                                mutate(cond_post_c19 = case_when(cond_post_c19 == "Ja" ~ "Yes",
                                cond_post_c19 == "Nicht Gewählt" ~ "No")) %>%
                                mutate(cond_none = case_when(cond_none == "Ja" ~ "No Comorbidities",
                                cond_none == "Nicht Gewählt" ~ "Comorbidities"))

data_reduced$cond_hbp <- factor(data_reduced$cond_hbp, levels = c("No", "Yes"))
data_reduced$cond_diabetes <- factor(data_reduced$cond_diabetes, levels = c("No", "Yes"))
data_reduced$cond_cardio <- factor(data_reduced$cond_cardio, levels = c("No", "Yes"))
data_reduced$cond_resp <- factor(data_reduced$cond_resp, levels = c("No", "Yes"))
data_reduced$cond_immuno <- factor(data_reduced$cond_immuno, levels = c("No", "Yes"))
data_reduced$cond_cancer <- factor(data_reduced$cond_cancer, levels = c("No", "Yes"))
data_reduced$cond_post_c_19 <- factor(data_reduced$cond_post_c19, levels = c("No", "Yes"))
data_reduced$cond_none <- factor(data_reduced$cond_none, levels = c("No Comorbidities", "Comorbidities"))

comorbidities <- c("cond_hbp", "cond_diabetes", "cond_cardio", "cond_resp",
                    "cond_immuno", "cond_cancer", "cond_post_c19", "cond_none")

for(com in comorbidities){
ggplot(data_reduced %>% filter(!is.na(!!sym(com))), aes(date_f1_inf, color = !!sym(com))) +
stat_ecdf(geom="smooth", size = 2) +
theme_minimal() +
ylab("Empirical Cumulative \n Density Function") +
xlab("Date Of First Infection") +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01")), ylim=c(0, 0.75)) +
theme(text = element_text(size = 30)) +
theme(legend.position = "bottom") +
labs(color = "Comorbidity") +
guides(color = guide_legend(nrow = 2)) +
scale_color_manual(values = palette())

ggsave(paste0("TimingOfInfection_", com, ".pdf"), dpi = 500, w = 9, h = 9)
ggsave(paste0("TimingOfInfection_", com, ".png"), dpi = 500, w = 9, h = 9)

data_reduced %>%
  count(!!sym(com), num_c19_infs_eng) %>%
  mutate(n = n / sum(n), .by = !!sym(com)) %>%
  filter(!is.na(!!sym(com))) %>% 
  ggplot(aes(num_c19_infs_eng, n, fill = !!sym(com))) +
  geom_col(position = position_dodge(preserve = 'single')) +
  theme_minimal() +
  ylab("Relative Frequency") +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom") +
  labs(fill="Comorbidity") +
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette())

ggsave(paste0("NumberOfInfection_", com, ".pdf"), dpi = 500, w = 9, h = 12)
ggsave(paste0("NumberOfInfection_", com,".png"), dpi = 500, w = 9, h = 12)
}

source("DataCleaningPrepForContactAnalysis.R")

data_reduced <- data_reduced %>% select(-c(respondent_hsld_size_persons_under_14, number_of_children_under_18)) %>%
                                  select(-contains("attitudes")) %>%
                                  select(-contains("beh_change"))


data_reduced <- data_reduced %>% pivot_longer(cols = 13:88)

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

data_reduced <- data_reduced %>% mutate(cond_hbp  = case_when(cond_hbp == "Ja" ~ "Yes",
                                cond_hbp == "Nicht Gewählt" ~ "No")) %>% 
                                mutate(cond_diabetes = case_when(cond_diabetes == "Ja" ~ "Yes",
                                cond_diabetes == "Nicht Gewählt" ~ "No")) %>% 
                                mutate(cond_cardio = case_when(cond_cardio == "Ja" ~ "Yes",
                                cond_cardio == "Nicht Gewählt" ~ "No")) %>% 
                                mutate(cond_resp = case_when(cond_resp == "Ja" ~ "Yes",
                                cond_resp == "Nicht Gewählt" ~ "No")) %>% 
                                mutate(cond_immuno = case_when(cond_immuno == "Ja" ~ "Yes",
                                cond_immuno == "Nicht Gewählt" ~ "No")) %>% 
                                mutate(cond_cancer = case_when(cond_cancer == "Ja" ~ "Yes",
                                cond_cancer == "Nicht Gewählt" ~ "No")) %>%
                                mutate(cond_post_c19 = case_when(cond_post_c19 == "Ja" ~ "Yes",
                                cond_post_c19 == "Nicht Gewählt" ~ "No")) %>%
                                mutate(cond_none = case_when(cond_none == "Ja" ~ "No Comorbidities",
                                cond_none == "Nicht Gewählt" ~ "Comorbidities"))

palette <- function() {
  c("#fd5901", "#008083")
}

for(com in comorbidities){
    ggplot(data_reduced %>% filter(WhoseContacts == "Respondent") %>% filter(value < 100) %>% filter(!is.na(!!sym(com))) %>% filter(!is.na(TypeOfContact)), aes(!!sym(com), value)) +
    #geom_violin(aes(color = WhoseContacts), width = 1, trim = FALSE, position=position_dodge(0.9)) + 
    #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
    geom_boxplot(aes(color = !!sym(com)), size = 1.3) +
    scale_color_manual(values = palette()) +
    facet_grid(rows = vars(TypeOfContact), cols = vars(time)) +
    theme_minimal() +
    ylab("Reported No. Of Contacts") +
    theme(text = element_text(size = 30)) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    #labs(color ="Comorbidity") +
    theme(panel.spacing = unit(0.8, "cm", data = NULL))

    ggsave(paste0("CollectionBoxplots_", com, ".pdf"), dpi = 500, w = 13, h = 19)
    ggsave(paste0("CollectionBoxplots_", com, ".png"), dpi = 500, w = 13, h = 19)
}
