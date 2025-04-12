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


palette <- function() {
  c("#fd5901", "#008083")
}

    p1 <- ggplot(data_reduced_tidy %>% filter(WhoseContacts == "Respondent") %>% filter(value <= 100) %>% filter(!is.na(respondent_cc_change)) %>% filter(!is.na(TypeOfContact)), aes(respondent_cc_change, value)) +
    #geom_violin(aes(color = WhoseContacts), width = 1, trim = FALSE, position=position_dodge(0.9)) + 
    #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
    geom_boxplot(aes(color = respondent_cc_change), size = 1.3) +
    scale_color_manual(values = palette()) +
    facet_grid(rows = vars(TypeOfContact), cols = vars(time)) +
    theme_minimal() +
    ylab("Reported No. Of Contacts") +
    theme(text = element_text(size = 30)) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    #labs(color ="Comorbidity") +
    theme(panel.spacing = unit(0.8, "cm", data = NULL))

    #ggsave(paste0("CollectionBoxplots_", com, ".pdf"), p1,  dpi = 500, w = 13, h = 19)
    #ggsave(paste0("CollectionBoxplots_", com, ".png"), p1, dpi = 500, w = 13, h = 19)

   p2 <- ggplot(data_reduced %>% filter(!is.na(respondent_cc_change)), aes(date_f1_inf, color = respondent_cc_change)) +
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

    #ggsave(paste0("TimingOfInfection_", com, ".pdf"), p2, dpi = 500, w = 9, h = 9)
    #ggsave(paste0("TimingOfInfection_", com, ".png"), p2, dpi = 500, w = 9, h = 9)

   p3 <- data_reduced %>%
    count(respondent_cc_change, num_c19_infs_eng) %>%
    mutate(n = n / sum(n), .by = respondent_cc_change) %>%
    filter(!is.na(respondent_cc_change)) %>% 
    ggplot(aes(num_c19_infs_eng, n, fill = respondent_cc_change)) +
    geom_col(position = position_dodge(preserve = 'single')) +
    theme_minimal() +
    ylab("Relative Frequency") +
    xlab("") +
    theme(text = element_text(size = 30)) +
    theme(legend.position = "none") +
    #labs(fill="Comorbidity") +
    theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
    scale_fill_manual(values = palette())

    #ggsave(paste0("NumberOfInfection_", com, ".pdf"), p3, dpi = 500, w = 9, h = 12)
    #ggsave(paste0("NumberOfInfection_", com,".png"), p3, dpi = 500, w = 9, h = 12)


  patch <- (p3/p2) +  plot_annotation(tag_levels = "A")
  p4 <- p1  + plot_spacer() + patch + plot_layout(widths = c(13, 0.5, 5)) +  plot_annotation(tag_levels = "A") 
  ggsave(paste0("BoxplotNoInfectionsECDF_", "CCChange", ".pdf"), p4, dpi = 500, w = 18.5, h = 16)
  ggsave(paste0("BoxplotNoInfectionsECDF_", "CCChange", ".png"), p4, dpi = 500, w = 18.5, h = 16)



