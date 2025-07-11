library(tidyverse) #version 2.0.0.
library(MMWRweek) #version 0.1.3
library(see) #version 0.8.4
library(RColorBrewer) #version 1.1.3
library(patchwork) #version 1.2.0
library(ggpubr) #version 0.6.0
library(ggh4x) #version 0.3.1
library(here) #version 1.0.1

# Author: S. Paltra, contact: paltra@tu-berlin.de

here()
source("./R/DataCleaningPrepForContactAnalysis.R")
source("./R/mytheme.r")

palette <- function() {
  c("#3C5488FF", "#DC0000FF")
}

palette2 <- function() {
  c("#1d2942", "#870000")
}

#Data prep
data_reduced <- data_reduced %>% mutate(date_f1_inf = case_when(is.na(date_f1_inf) ~ as.Date("3000-01-01"),
                                        .default = as.Date(as.character(date_f1_inf)))) %>%
                                filter(date_f1_inf != as.Date("1922-03-01")) %>%
                                filter(date_f1_inf != as.Date("1965-06-12")) %>%
                                filter(date_f1_inf != as.Date("2000-12-13")) %>%
                                filter(date_f1_inf != as.Date("2019-12-21")) 

data_reduced <- data_reduced %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "0",
                                                                    num_c19_infs == "Einmal" ~ "1",
                                                                    num_c19_infs == "Zweimal" ~ "2",
                                                                    num_c19_infs == "Dreimal" ~ "3+",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "3+",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

data_reduced$num_c19_infs_eng <- factor(data_reduced$num_c19_infs_eng, levels = c("0", "1", "2", "3+", "I Don't Want To Answer"))

data_reduced <- data_reduced %>%
                                mutate(cond_none = case_when(cond_none == "Ja" ~ "No Comorbidities",
                                cond_none == "Nicht Gewählt" ~ "Some Comorbidity"))

data_reduced$cond_none <- factor(data_reduced$cond_none, levels = c("No Comorbidities", "Some Comorbidity"))

comorbidities <- c("cond_none")

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% 
                                mutate(cond_none = case_when(cond_none == "Ja" ~ "No Comorbidities",
                                cond_none == "Nicht Gewählt" ~ "Some Comorbidity"))

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% mutate(time = case_when(time == "Summer 2021" ~ "Summer\n2021", .default = time))
data_reduced_tidy_rel$time <- factor(data_reduced_tidy_rel$time, levels = c("03/2020", "Summer\n2021", "01/2023"))

# Mean Reduction ----------------------------------------------------------

print(data_reduced_tidy_rel %>%  
        filter(!is.na(value)) %>% 
        filter(value > -150) %>% filter(value < 100) %>% filter(WhoseContacts == "Respondent") %>%
        filter(!is.na(cond_none)) %>% group_by(TypeOfContact, cond_none, time) %>%
        summarise(meanRed = mean(value)), n = 100)

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% mutate(time = case_when(time == "Summer 2021" ~ "Summer\n2021", .default = time))
data_reduced_tidy_rel$time <- factor(data_reduced_tidy_rel$time, levels = c("03/2020", "Summer\n2021", "01/2023"))

for (com in comorbidities){
  if (com != "cond_none") {
    my_comparisons <- list(c("Yes", "No"))
  } else {
    my_comparisons <- list(c("Some Comorbidity", "No Comorbidities"))
  }
  
  data_reduced_tidy_rel$combined = interaction(data_reduced_tidy_rel$cond_none, data_reduced_tidy_rel$time)
  combined_levels <- levels(interaction(data_reduced_tidy_rel$cond_none, data_reduced_tidy_rel$time))
  A_values <- data_reduced_tidy_rel$time[match(combined_levels, data_reduced_tidy_rel$combined)]
  
    #Produces Supplementary Figure 16B
    p1_leisure <- ggplot(data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% 
    filter(!is.na(!!sym(com))) %>%
    filter(!is.na(TypeOfContact)) %>% 
    filter(TypeOfContact %in% c("Leisure")) %>%
    filter(value > -150) %>%  filter(value < 100) %>%
    filter(!is.na(TypeOfContact)) %>% group_by(!!sym(com), TypeOfContact, time), aes(combined, value, color = !!sym(com), fill = !!sym(com))) +
    sm_raincloud(aes(stat = median_cl), 
                   point.params = list(size = 3, shape = 21, alpha = 0.4, position = sdamr::position_jitternudge(
                     nudge.x = -0.1,
                     jitter.width = 0.1, jitter.height = 0.01      
                   )), 
                   boxplot.params =  list(alpha = 0.0, width = 0.0, notch = TRUE), 
                   violin.params = list(width = 1),
                   shape = 21, sep_level = 2)  +
      scale_fill_manual(values = palette()) +
      scale_color_manual(values = palette2()) +
      scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-100, -50, 0,50, 100)) +
      #facet_grid(~(time), switch="both")+
      ggtitle("Leisure") +
      theme_minimal() +
      theme(panel.spacing = unit(4, "lines")) +
      ylab("Change of No. of \n Contacts (in percent)") +
      my_theme() +
      theme(axis.title.x = element_blank(), plot.title = element_text(hjust=0.5)) +
      theme(axis.ticks.x = element_line(size = 0)) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      theme(axis.text.x = element_text(hjust=-0.0001))  +
      scale_x_discrete(
        breaks = combined_levels[unique_positions],  # Only put breaks at selected positions
        labels = unique_A_values                     # Use corresponding unique A values as labels
      )
    
    #Produces Supplementary Figure 16A
    p1_work <- ggplot(data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% 
                                 filter(!is.na(!!sym(com))) %>%
                                 filter(!is.na(TypeOfContact)) %>% 
                                 filter(TypeOfContact %in% c("Work")) %>%
                                 filter(value > -150) %>%  filter(value < 100) %>%
                                 filter(!is.na(TypeOfContact)) %>% group_by(!!sym(com), TypeOfContact, time), aes(combined, value, color = !!sym(com), fill = !!sym(com))) +
      sm_raincloud(aes(stat = median_cl), 
                   point.params = list(size = 3, shape = 21, alpha = 0.4, position = sdamr::position_jitternudge(
                     nudge.x = -0.1,
                     jitter.width = 0.1, jitter.height = 0.01      
                   )), 
                   boxplot.params =  list(alpha = 0.0, width = 0.0, notch = TRUE), 
                   violin.params = list(width = 1),
                   shape = 21, sep_level = 2)  +
      scale_fill_manual(values = palette()) +
      scale_color_manual(values = palette2()) +
      scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-100, -50, 0,50, 100)) +
      #facet_grid(~(time), switch="both")+
      ggtitle("Work") +
      theme_minimal() +
      theme(panel.spacing = unit(4, "lines")) +
      ylab("Change of No. of \n Contacts (in percent)") +
      my_theme() +
      theme(axis.title.x = element_blank(), plot.title = element_text(hjust=0.5)) +
      theme(axis.ticks.x = element_line(size = 0)) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      theme(axis.text.x = element_text(hjust=-0.0001))  +
      scale_x_discrete(
        breaks = combined_levels[unique_positions],  # Only put breaks at selected positions
        labels = unique_A_values                     # Use corresponding unique A values as labels
      )
    ggarrange(p1_work, p1_leisure, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")

    ggsave(paste0("./plots/","CollectionViolinplots_", com, ".pdf"),  dpi = 500, w = 24, h = 9)
    ggsave(paste0("./plots/","CollectionViolinplots_", com, ".png"), dpi = 500, w = 24, h = 9)

    #Produces Supplementary Figure 17B
    ecdf_comp <- data_reduced %>% filter(num_c19_infs_eng != "I Don't Want To Answer") %>% group_by(!!sym(com)) %>% 
  count(date_f1_inf) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
    mutate(ecdf = cum/sum) %>% ungroup()
  ecdf_comp <- ecdf_comp %>% mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
    mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
    mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
    mutate(uci = case_when(uci > 1 ~ 1, .default = uci))

  ecdf <- ggplot(ecdf_comp %>% filter(!is.na(!!sym(com))), aes(date_f1_inf)) +
  geom_ribbon(aes(ymin = lci, ymax = uci, x = date_f1_inf, , fill = !!sym(com)), alpha = 0.1)+
  geom_line(aes(y=ecdf,  color = !!sym(com)), size = 2) +
  theme_minimal() +
  ylab("Empirical Cumulative \n Distribution Function") +
  xlab("Date of 1st Infection") +
  coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01"))) +
  theme(text = element_text(size = 30)) +
  scale_color_manual(values = palette()) +
  my_theme() 

    if(com == "cond_none"){
      yes <- 593
      no <- 269
    }

  data <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(data) <- c("cond_none", "num_c19_infs_eng", "n", "percent")
  data$n <- as.integer(data$n)
  data$percent <- as.integer(data$percent)
  
  #Produces Supplementary Figure 17A
  
    p3 <- data_reduced %>% filter(!is.na(!!sym(com))) %>%
    filter(num_c19_infs_eng != "I Don't Want To Answer") %>%
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
    scale_x_discrete(limits = c("0", "1", "2", "3+")) +
    geom_errorbar(aes(x=num_c19_infs_eng, ymin=lci, ymax=uci, color = !!sym(com)), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=2.5) +
    theme_minimal() +
    ylab("Share of Respondents\n(in percent)") +
    scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50)) +
    xlab("Number of Infections") +
    theme(text = element_text(size = 30)) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    #labs(fill="Comorbidity") +
    scale_fill_manual(values = palette()) +
    scale_color_manual(values = palette2()) +
    theme(legend.title = element_blank(), legend.position = "bottom")  +
    my_theme()

    #ggsave(paste0("./plots/","NumberOfInfection_", com, ".pdf"), p3, dpi = 500, w = 9, h = 9)
    #ggsave(paste0("./plots/","NumberOfInfection_", com,".png"), p3, dpi = 500, w = 9, h = 9)


  ggarrange(p3, ecdf, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")

  ggsave(paste0("./plots/","NoInfectionsECDF_cond_none_Suppl17.pdf"), dpi = 500, w = 22, h = 9) 
  ggsave(paste0("./plots/","NoInfectionsECDF_cond_none_Suppl17.png"), dpi = 500, w = 22, h = 9) 
}
