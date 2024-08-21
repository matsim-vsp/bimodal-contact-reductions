library(tidyverse)
library(MMWRweek)
library(see)
library(RColorBrewer)
library(patchwork)

# Author: S. Paltra, contact: paltra@tu-berlin.de

source("DataCleaningPrepForContactAnalysis.R")

# Carefulness Due To Comorbidities ----------------------------------------

data_reduced <- data_reduced %>% mutate(date_f1_inf = case_when(is.na(date_f1_inf) ~ as.Date("3000-01-01"),
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

data_reduced_tidy <- data_reduced_tidy %>% mutate(cond_hbp  = case_when(cond_hbp == "Ja" ~ "Yes",
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
  c("#998ec3", "#f1a340")
}

palette2 <- function() {
  c("#542788", "#b35806")
}

for(com in comorbidities){
    p1 <- ggplot(data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% 
    filter(!is.na(!!sym(com))) %>% filter(!is.na(TypeOfContact)) %>% filter(TypeOfContact %in% c("Work", "Leisure")) %>%
    filter(value > -50) %>% filter(value < 150) %>%  
    filter(!is.na(TypeOfContact)), aes(!!sym(com), value)) +
    #geom_violin(aes(color = WhoseContacts), width = 1, trim = FALSE, position=position_dodge(0.9)) + 
    #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
    geom_violin(aes(fill = !!sym(com), color = !!sym(com)), scale = "area", trim = TRUE) + 
    stat_summary(aes(color=!!sym(com)), fun.data=mean_sdl, fun.args = list(mult=1), 
                 geom="pointrange", linewidth = 1) +
    facet_grid(rows = vars(TypeOfContact), cols = vars(time)) +
    theme_minimal() +
    scale_fill_manual(values = palette()) +
    scale_color_manual(values = palette2()) +
    ylab("Reduction Of Contacts [Percentage]") +
    theme(text = element_text(size = 30)) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    #labs(color ="Comorbidity") +
    theme(panel.spacing = unit(0.8, "cm", data = NULL))

    ggsave(paste0("CollectionViolinplots_", com, ".pdf"), p1,  dpi = 500, w = 12, h = 9)
    ggsave(paste0("CollectionViolinplots_", com, ".png"), p1, dpi = 500, w = 12, h = 9)

    p2 <- ggplot(data_reduced %>% filter(!is.na(!!sym(com))), aes(date_f1_inf, color = !!sym(com))) +
    stat_ecdf(geom="smooth", size = 2) +
    theme_minimal() +
    ylab("Empirical Cumulative \n Density Function") +
    xlab("Date Of First Infection") +
    coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01")), ylim=c(0, 0.75)) +
    theme(text = element_text(size = 30)) +
    theme(legend.position = "none") +
    #labs(color = "Comorbidity") +
    guides(color = guide_legend(nrow = 2)) +
    scale_color_manual(values = palette())

    ggsave(paste0("TimingOfInfection_", com, ".pdf"), p2, dpi = 500, w = 9, h = 9)
    ggsave(paste0("TimingOfInfection_", com, ".png"), p2, dpi = 500, w = 9, h = 9)

    p3 <- data_reduced %>%
    count(!!sym(com), num_c19_infs_eng) %>%
    mutate(n = n / sum(n), .by = !!sym(com)) %>%
    filter(!is.na(!!sym(com))) %>% 
    ggplot(aes(num_c19_infs_eng, n, fill = !!sym(com))) +
    geom_col(position = position_dodge(preserve = 'single')) +
    theme_minimal() +
    ylab("Relative Frequency") +
    xlab("") +
    theme(text = element_text(size = 30)) +
    theme(legend.position = "none") +
    #labs(fill="Comorbidity") +
    theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
    scale_fill_manual(values = palette())

    ggsave(paste0("NumberOfInfection_", com, ".pdf"), p3, dpi = 500, w = 9, h = 9)
    ggsave(paste0("NumberOfInfection_", com,".png"), p3, dpi = 500, w = 9, h = 9)


  #patch <- (p3/p2) +  plot_annotation(tag_levels = "A")
  #p4 <- p1  + plot_spacer() + patch + plot_layout(widths = c(13, 0.5, 5)) +  plot_annotation(tag_levels = "A") 
  #ggsave(paste0("BoxplotNoInfectionsECDF_", com, ".pdf"), p4, dpi = 500, w = 18.5, h = 16)
  #ggsave(paste0("BoxplotNoInfectionsECDF_", com, ".png"), p4, dpi = 500, w = 18.5, h = 16)
}


