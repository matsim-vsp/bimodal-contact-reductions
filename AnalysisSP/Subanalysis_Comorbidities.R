library(tidyverse)
library(MMWRweek)
library(see)
library(RColorBrewer)
library(patchwork)
library(ggpubr)

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
                                cond_none == "Nicht Gewählt" ~ "Some Comorbidity"))

data_reduced$cond_hbp <- factor(data_reduced$cond_hbp, levels = c("No", "Yes"))
data_reduced$cond_diabetes <- factor(data_reduced$cond_diabetes, levels = c("No", "Yes"))
data_reduced$cond_cardio <- factor(data_reduced$cond_cardio, levels = c("No", "Yes"))
data_reduced$cond_resp <- factor(data_reduced$cond_resp, levels = c("No", "Yes"))
data_reduced$cond_immuno <- factor(data_reduced$cond_immuno, levels = c("No", "Yes"))
data_reduced$cond_cancer <- factor(data_reduced$cond_cancer, levels = c("No", "Yes"))
data_reduced$cond_post_c_19 <- factor(data_reduced$cond_post_c19, levels = c("No", "Yes"))
data_reduced$cond_none <- factor(data_reduced$cond_none, levels = c("No Comorbidities", "Some Comorbidity"))

comorbidities <- c("cond_hbp", "cond_diabetes", "cond_cardio", "cond_resp",
                    "cond_immuno", "cond_cancer", "cond_post_c19", "cond_none")

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% mutate(cond_hbp  = case_when(cond_hbp == "Ja" ~ "Yes",
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
                                cond_none == "Nicht Gewählt" ~ "Some Comorbidity"))

palette <- function() {
  c("#998ec3", "#f1a340")
}

palette2 <- function() {
  c("#542788", "#b35806")
}

for (com in comorbidities){
  if (com != "cond_none") {
    my_comparisons <- list(c("Yes", "No"))
  } else {
    my_comparisons <- list(c("Some Comorbidity", "No Comorbidities"))
  }
    p1 <- ggplot(data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% 
    filter(!is.na(!!sym(com))) %>% filter(!is.na(TypeOfContact)) %>% filter(TypeOfContact %in% c("Work", "Leisure")) %>%
    filter(value > -50) %>% filter(value < 150) %>%  
    filter(!is.na(TypeOfContact)), aes(!!sym(com), value)) +
    #geom_violin(aes(color = WhoseContacts), width = 1, trim = FALSE, position=position_dodge(0.9)) + 
    #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
    geom_violin(aes(fill = !!sym(com), color = !!sym(com)), scale = "area", trim = TRUE) + 
    stat_summary(aes(color=!!sym(com)), fun.data=mean_sdl, fun.args = list(mult=1), 
                 geom="pointrange", linewidth = 1) +
    stat_compare_means(comparisons = my_comparisons, method = "t.test", symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("**** (p < 0.0001)", "*** (p < 0.001)", "** (p < 0.01)", "* (p < 0.05)", "not significant (p > 0.05)")), size = 6, bracket.size = 1, tip.length = 0.01, vjust = -0.5, label.y.npc = 0)+
    facet_grid(rows = vars(TypeOfContact), cols = vars(time)) +
    theme_minimal() +
    scale_fill_manual(values = palette()) +
    scale_color_manual(values = palette2()) +
    ylab("Reduction Of Contacts [Percentage]") +
    scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-50, 0,50, 100)) +
    theme(text = element_text(size = 30)) +
    theme(panel.spacing.y = unit(3, "lines")) +
    theme(panel.spacing.x = unit(3, "lines")) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    #labs(color ="Comorbidity") +
    theme(panel.spacing = unit(0.8, "cm", data = NULL))

    ggsave(paste0("CollectionViolinplots_", com, ".pdf"), p1,  dpi = 500, w = 15, h = 10)
    ggsave(paste0("CollectionViolinplots_", com, ".png"), p1, dpi = 500, w = 15, h = 10)

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

    if(com == "cond_hbp"){
      yes <- 149
      no <- 119
    }else if(com == "cond_diabetes"){
      yes <- 27
      no <- 241
    }else if(com == "cond_cardio"){
      yes <- 25
      no <- 243
    }else if(com == "cond_resp"){
      yes <- 84
      no <- 184
    }else if(com == "cond_immuno"){
      yes <- 24
      no <- 244
    }else if(com == "cond_cancer"){
      yes <- 11
      no <- 257
    }else if(com == "cond_post_c19"){
      yes <- 33
      no <- 235
    }else if(com == "cond_none"){
      yes <- 590
      no <- 268
    }

  data <- data.frame(matrix(nrow = 0, ncol = 4))
  if(com=="cond_hbp"){
    colnames(data) <- c("cond_hbp", "num_c19_infs_eng", "n", "percent")
    data[nrow(data)+1,] <- c("No", "Three Times", 0, 0)
    data[nrow(data)+1,] <- c("Yes", "More Than Three Times", 0, 0)
    data[nrow(data)+1,] <- c("Yes", "I Don't Want To Answer", 0, 0)
  } else if(com=="cond_diabetes"){
    colnames(data) <- c("cond_diabetes", "num_c19_infs_eng", "n", "percent")
    data[nrow(data)+1,] <- c("Yes", "Three Times", 0, 0)
    data[nrow(data)+1,] <- c("Yes", "More Than Three Times", 0, 0)
    data[nrow(data)+1,] <- c("Yes", "I Don't Want To Answer", 0, 0)
  } else if(com=="cond_cardio"){
    colnames(data) <- c("cond_cardio", "num_c19_infs_eng", "n", "percent")
    data[nrow(data)+1,] <- c("Yes", "Twice", 0,0)
    data[nrow(data)+1,] <- c("No", "Three Times", 0,0)
    data[nrow(data)+1,] <- c("Yes", "More Than Three Times", 0,0)
    data[nrow(data)+1,] <- c("Yes", "I Don't Want To Answer", 0,0)
  } else if(com=="cond_resp"){
    colnames(data) <- c("cond_resp", "num_c19_infs_eng", "n", "percent")
    data[nrow(data)+1,] <- c("Yes", "Three Times", 0,0)
    data[nrow(data)+1,] <- c("Yes", "More Than Three Times", 0,0)
    data[nrow(data)+1,] <- c("No", "I Don't Want To Answer", 0,0)
  } else if(com=="cond_immuno"){
    colnames(data) <- c("cond_immuno", "num_c19_infs_eng", "n", "percent")
    data[nrow(data)+1,] <- c("Yes", "Three Times", 0,0)
    data[nrow(data)+1,] <- c("Yes", "More Than Three Times", 0,0)
    data[nrow(data)+1,] <- c("Yes", "I Don't Want To Answer", 0,0)
  } else if(com=="cond_cancer"){
    colnames(data) <- c("cond_cancer", "num_c19_infs_eng", "n", "percent")
    data[nrow(data)+1,] <- c("Yes", "Three Times", 0,0)
    data[nrow(data)+1,] <- c("Yes", "More Than Three Times", 0,0)
    data[nrow(data)+1,] <- c("Yes", "I Don't Want To Answer", 0,0)
  } else if(com == "cond_post_c19"){
    colnames(data) <- c("cond_post_c19", "num_c19_infs_eng", "n", "percent")
    data[nrow(data)+1,] <- c("Yes", "Never", 0,0)
    data[nrow(data)+1,] <- c("Yes", "Three Times", 0,0)
    data[nrow(data)+1,] <- c("No", "More Than Three Times", 0,0)
    data[nrow(data)+1,] <- c("Yes", "I Don't Want To Answer", 0,0)
  } else if(com=="cond_none"){
    colnames(data) <- c("cond_none", "num_c19_infs_eng", "n", "percent")
    data[nrow(data)+1,] <- c("No Comorbidities", "I Don't Want To Answer", 0,0)
  }
  data$n <- as.integer(data$n)
  data$percent <- as.integer(data$percent)

    p3 <- data_reduced %>%
    count(!!sym(com), num_c19_infs_eng) %>%
    mutate(percent = 100 * n / sum(n), .by = !!sym(com)) %>% rbind(data) %>%
    mutate(lci = case_when(!!sym(com) == "Yes" ~ n - 1.96*(n*(n-1)/yes)^0.5,
                          !!sym(com) == "No" ~ n - 1.96*(n*(n-1)/no)^0.5,
                          !!sym(com) == "No Comorbidities" ~ n - 1.96*(n*(n-1)/yes)^0.5,
                          !!sym(com) == "Some Comorbidity" ~ n - 1.96*(n*(n-1)/no)^0.5)) %>%#
    mutate(lci = case_when(!!sym(com) == "Yes" ~ 100/yes*lci,
                         !!sym(com) == "No" ~ 100/no*lci,
                         !!sym(com) == "No Comorbidities" ~ 100/yes*lci,
                         !!sym(com) == "Some Comorbidity" ~ 100/no*lci)) %>%
    mutate(uci = case_when(!!sym(com) == "Yes" ~ n + 1.96*(n*(n-1)/yes)^0.5,
                          !!sym(com) == "No" ~ n + 1.96*(n*(n-1)/no)^0.5,
                          !!sym(com) == "No Comorbidities" ~ n + 1.96*(n*(n-1)/yes)^0.5,
                          !!sym(com) == "Some Comorbidity" ~ n + 1.96*(n*(n-1)/no)^0.5)) %>%
    mutate(uci = case_when(!!sym(com) == "Yes" ~ 100/yes*uci,
                          !!sym(com) == "No" ~ 100/no*uci,
                          !!sym(com) == "No Comorbidities" ~ 100/yes*uci,
                          !!sym(com) == "Some Comorbidity" ~ 100/no*uci)) %>%
    filter(!is.na(!!sym(com))) %>% 
    ggplot(aes(num_c19_infs_eng, percent, fill = !!sym(com))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.8) +
    scale_x_discrete(limits = c("Never", "Once", "Twice", "Three Times", "More Than Three Times", "I Don't Want To Answer")) +
    geom_errorbar(aes(x=num_c19_infs_eng, ymin=lci, ymax=uci, color = !!sym(com)), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
    theme_minimal() +
    ylab("Share [Percentage]") +
    scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50)) +
    xlab("") +
    theme(text = element_text(size = 30)) +
    theme(legend.position = "bottom") +
    #labs(fill="Comorbidity") +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    scale_fill_manual(values = palette()) +
    scale_color_manual(values = palette2())

    ggsave(paste0("NumberOfInfection_", com, ".pdf"), p3, dpi = 500, w = 9, h = 9)
    ggsave(paste0("NumberOfInfection_", com,".png"), p3, dpi = 500, w = 9, h = 9)


  #patch <- (p3/p2) +  plot_annotation(tag_levels = "A")
  #p4 <- p1  + plot_spacer() + patch + plot_layout(widths = c(13, 0.5, 5)) +  plot_annotation(tag_levels = "A") 
  #ggsave(paste0("BoxplotNoInfectionsECDF_", com, ".pdf"), p4, dpi = 500, w = 18.5, h = 16)
  #ggsave(paste0("BoxplotNoInfectionsECDF_", com, ".png"), p4, dpi = 500, w = 18.5, h = 16)
}


