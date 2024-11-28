
library(tidyverse)
library(viridis)

source("DataCleaningPrepForContactAnalysis.R")


### COMPUTATION OF CORRELATION MATRIX (ALL RESPONDENTS)

correlation_matrix <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(correlation_matrix) <- c("abs_or_rel", "context", "year", "change_of_cc", "pre_or_during", "correlation_coefficient")

times <- c("2019", "03_2020", "summer_2021", "01_2023")
riskperceptions <- c("all", "Risk-tolerant", "Risk-averse")

for(riskperception in riskperceptions){
  for(time in times){
    respname <- paste0("respondent_work_", time)
    ccname <- paste0("cc_pre_work_", time)
    data_reduced_RemoveNAs <- data_reduced %>% filter(!!respname != -1000) %>% filter(!!ccname != -1000)
    if(riskperception != "all"){
      data_reduced_RemoveNAs <- data_reduced_RemoveNAs %>% filter(RiskyCarefulAtt == riskperception)
    }
    correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("abs", "work", time, riskperception, "pre", cor(data_reduced_RemoveNAs[respname], data_reduced_RemoveNAs[ccname], use = "pairwise.complete.obs", method = "pearson"))

    respname <- paste0("respondent_leisure_", time)
    ccname <- paste0("cc_pre_leisure_", time)
    data_reduced_RemoveNAs <- data_reduced %>% filter(!!respname != -1000) %>% filter(!!ccname != -1000)
    if(riskperception != "all"){
      data_reduced_RemoveNAs <- data_reduced_RemoveNAs %>% filter(RiskyCarefulAtt == riskperception)
    }
    correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("abs", "leisure", time, riskperception, "pre", cor(data_reduced_RemoveNAs[respname], data_reduced_RemoveNAs[ccname], use = "pairwise.complete.obs", method = "pearson"))
  }
}





# Correlation Analysis ----------------------------------------------------

# Aim of SECOND PART of this script: Try to understand if there's a correlation between the no of contacts of the respondent and the no. of contacts of their cc
#data_reduced <- data_reduced %>% filter(!is.na(user_id))

data_reduced_no_out_work <- data_reduced %>% 

data_reduced_no_out_work_rel <- data_reduced %>% filter(respondent_work_rel_2019_2020 >= 0) %>% filter(respondent_work_rel_2019_2020 <= 100) %>% 
                                             filter(cc_pre_work_rel_2019_2020 >= 0 ) %>% filter(cc_pre_work_rel_2019_2020 <= 100) %>% filter(respondent_work_rel_2019_2021 >=0 ) %>% filter(respondent_work_rel_2019_2021 <= 100) %>%
                                        filter(cc_pre_work_rel_2019_2021 >= 0) %>% filter(cc_pre_work_rel_2019_2021 <= 100) %>% filter(respondent_work_rel_2019_2023 >= 0 ) %>% filter(respondent_work_rel_2019_2023 <= 100) %>%
                                        filter(cc_pre_work_rel_2019_2023 >= 0) %>% filter(cc_pre_work_rel_2019_2023 <= 100) 

data_reduced_no_out_leisure <- data_reduced %>% filter(RiskyCarefulAtt == "Risky") %>% filter(respondent_leisure_2019 != -1000) %>% filter(respondent_leisure_2019 <= 200) %>%
                                        filter(cc_pre_leisure_2019 != -1000) %>% filter(cc_pre_leisure_2019 <= 200) %>% filter(respondent_leisure_03_2020 != -1000) %>% filter(respondent_leisure_03_2020 <= 200) %>%
                                        filter(cc_pre_leisure_03_2020 != -1000) %>% filter(cc_pre_leisure_03_2020 <= 200) %>% filter(respondent_leisure_summer_2021 != -1000) %>% filter(respondent_leisure_summer_2021 <= 200) %>%
                                        filter(cc_pre_leisure_summer_2021 != -1000) %>% filter(cc_pre_leisure_summer_2021 <= 200) %>% filter(respondent_leisure_01_2023 != -1000) %>% filter(respondent_leisure_01_2023 <= 200) %>%
                                        filter(cc_pre_leisure_01_2023 != -1000) %>% filter(cc_pre_leisure_01_2023 <= 200) 

data_reduced_no_out_leisure_rel <- data_reduced %>% filter(respondent_leisure_rel_2019_2020 <= 100) %>% filter(respondent_leisure_rel_2019_2020 >= -100) %>% 
                                             filter(cc_pre_leisure_rel_2019_2020 < 100 ) %>% filter(cc_pre_leisure_rel_2019_2020 <= 100) %>% filter(respondent_leisure_rel_2019_2021 >= -100 ) %>% filter(respondent_leisure_rel_2019_2021 <= 100) %>%
                                        filter(cc_pre_leisure_rel_2019_2021 >= -100) %>% filter(cc_pre_leisure_rel_2019_2021 <= 100) %>% filter(respondent_leisure_rel_2019_2023 >= -100 ) %>% filter(respondent_leisure_rel_2019_2023 <= 100) %>%
                                        filter(cc_pre_leisure_rel_2019_2023 >= -100) %>% filter(cc_pre_leisure_rel_2019_2023 <= 100) 
 

ggplot(data_reduced) + 
  geom_jitter(aes(x=respondent_leisure_03_2020, y = cc_pre_leisure_03_2020), color = "#998ec3", size = 3, alpha = 0.7) +
  theme_minimal() +
  ggtitle("Leisure Contacts (03/2020)") +
  xlab("Respondent's # of Contacts") +
  ylab("CC's # of Contacts") +
  theme(text = element_text(size = 30)) +
  scale_color_manual(values = palette()) +
  scale_x_log10() +
  scale_y_log10()+
  theme(legend.position = "bottom", legend.title=element_blank())+
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

    ggsave(paste0("CorrelationPlotRespCC_", "leisure_0320" , ".pdf"), dpi = 500, w = 8, h = 8)
    ggsave(paste0("CorrelationPlotRespCC_", "leisure", "_0320", ".png"), dpi = 500, w = 8, h = 8)

ggplot(data_reduced) + 
  geom_point(aes(x=respondent_leisure_2019 - respondent_leisure_summer_2021, y = cc_pre_leisure_2019 - cc_pre_leisure_summer_2021), color = "#998ec3", size = 3) +
  theme_minimal() +
  scale_x_continuous(limits=c(0,60)) +
  scale_y_continuous(limits=c(0,60)) +
  ggtitle("Leisure Contacts Reduction (2021)") +
  xlab("Respondent's Reduction of \n Contacts (Absolute)") +
  ylab("CC's Reduction of \n Contacts (Absolute)") +
  theme(text = element_text(size = 30)) +
  scale_color_manual(values = palette()) +
  theme(legend.position = "bottom", legend.title=element_blank())+
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

    ggsave(paste0("CorrelationPlotRespCC_", "leisure", "_Abs1921", ".pdf"), dpi = 500, w = 9.5, h = 9)
    ggsave(paste0("CorrelationPlotRespCC_", "leisure", "_Abs1921", ".png"), dpi = 500, w = 9.5, h = 9)


data_reduced_no_out_all <- data_reduced %>% filter(respondent_all_2019 != -1000) %>% filter(respondent_all_2019 <= 250) %>%
                                        filter(cc_pre_all_2019 != -1000) %>% filter(cc_pre_all_2019 <= 250) %>% filter(respondent_all_03_2020 != -1000) %>% filter(respondent_all_03_2020 <= 250) %>%
                                        filter(cc_pre_all_03_2020 != -1000) %>% filter(cc_pre_all_03_2020 <= 250) %>% filter(respondent_all_summer_2021 != -1000) %>% filter(respondent_all_summer_2021 <= 250) %>%
                                        filter(cc_pre_all_summer_2021 != -1000) %>% filter(cc_pre_all_summer_2021 <= 250) %>% filter(respondent_all_01_2023 != -1000) %>% filter(respondent_all_01_2023 <= 250) %>%
                                        filter(cc_pre_all_01_2023 != -1000) %>% filter(cc_pre_all_01_2023 <= 250) 

data_reduced_no_out_all_rel <- data_reduced %>% filter(respondent_all_rel_2019_2020 >= 0) %>% filter(respondent_all_rel_2019_2020 <= 100) %>% 
                                             filter(cc_pre_all_rel_2019_2020 >= 0 ) %>% filter(cc_pre_all_rel_2019_2020 <= 100) %>% filter(respondent_all_rel_2019_2021 >=0 ) %>% filter(respondent_all_rel_2019_2021 <= 100) %>%
                                        filter(cc_pre_all_rel_2019_2021 >= 0) %>% filter(cc_pre_all_rel_2019_2021 <= 100) %>% filter(respondent_all_rel_2019_2023 >= 0 ) %>% filter(respondent_all_rel_2019_2023 <= 100) %>%
                                        filter(cc_pre_all_rel_2019_2023 >= 0) %>% filter(cc_pre_all_rel_2019_2023 <= 100) 

palette <- function() {
  c("#f1a340", "#998ec3")
}

data_reduced <- data_reduced %>% mutate(respondent_work_2019 = case_when(respondent_work_2019 == 0 ~ 0.1,
                                         .default = respondent_work_2019)) %>%
                                  mutate(cc_pre_work_2019 = case_when( cc_pre_work_2019 == 0 ~ 0.1,
                                         .default = cc_pre_work_2019)) %>% 
                                  mutate(respondent_work_03_2020 = case_when(respondent_work_03_2020 == 0 ~ 0.1,
                                         .default = respondent_work_03_2020)) %>%
                                  mutate(cc_pre_work_03_2020 = case_when( cc_pre_work_03_2020 == 0 ~ 0.1,
                                         .default = cc_pre_work_03_2020)) %>% 
                                  mutate(respondent_work_summer_2021 = case_when(respondent_work_summer_2021 == 0 ~ 0.1,
                                         .default = respondent_work_summer_2021)) %>%
                                  mutate(cc_pre_work_summer_2021 = case_when( cc_pre_work_summer_2021 == 0 ~ 0.1,
                                         .default = cc_pre_work_summer_2021)) %>% 
                                  mutate(respondent_work_01_2023 = case_when(respondent_work_01_2023 == 0 ~ 0.1,
                                         .default = respondent_work_01_2023)) %>%
                                  mutate(cc_pre_work_01_2023 = case_when( cc_pre_work_01_2023 == 0 ~ 0.1,
                                         .default = cc_pre_work_01_2023)) %>%
                                  mutate(respondent_leisure_2019 = case_when(respondent_leisure_2019 == 0 ~ 0.1,
                                         .default = respondent_leisure_2019)) %>%
                                  mutate(cc_pre_leisure_2019 = case_when(cc_pre_leisure_2019 == 0 ~ 0.1,
                                         .default = cc_pre_leisure_2019)) %>% 
                                  mutate(respondent_leisure_03_2020 = case_when(respondent_leisure_03_2020 == 0 ~ 0.1,
                                         .default = respondent_leisure_03_2020)) %>%
                                  mutate(cc_pre_leisure_03_2020 = case_when(cc_pre_leisure_03_2020 == 0 ~ 0.1,
                                         .default = cc_pre_leisure_03_2020)) %>%
                                  mutate(respondent_leisure_summer_2021 = case_when(respondent_leisure_summer_2021 == 0 ~ 0.1,
                                         .default = respondent_leisure_summer_2021)) %>%
                                  mutate(cc_pre_leisure_summer_2021 = case_when(cc_pre_leisure_summer_2021 == 0 ~ 0.1,
                                         .default = cc_pre_leisure_summer_2021)) %>%
                                  mutate(respondent_leisure_01_2023 = case_when(respondent_leisure_01_2023 == 0 ~ 0.1,
                                         .default = respondent_leisure_01_2023)) %>%
                                  mutate(cc_pre_leisure_01_2023 = case_when(cc_pre_leisure_01_2023 == 0 ~ 0.1,
                                         .default = cc_pre_leisure_01_2023)) 

contexts <- c("work", "leisure")
times <- c("2019", "03_2020", "summer_2021", "01_2023")

options(scipen = 999)

for(context in contexts){
  for(time in times){
    resp <- paste0("respondent_", context, "_", time)
    cc <- paste0("cc_pre_", context, "_", time)

    ggplot(data_reduced %>% filter(!is.na(RiskyCarefulAtt)) %>% filter(!!sym(resp) < 100) %>% filter(!!sym(resp) > -1000) %>% filter(!!sym(cc) < 100)%>% filter(!!sym(cc) > -1000)) + 
    #geom_point(aes(x=respondent_work_2019, y = cc_pre_work_2019), color = "#008083", size = 3) +
    geom_jitter(aes(x = !!sym(resp), y = !!sym(cc)), color = "#998ec3", size = 3, alpha = 0.8) +
    scale_size_manual(values=c(6,3))+
    theme_minimal() +
    #facet_grid(~RiskyCarefulAtt)+
    scale_x_log10(limits = c(0.005, 100), breaks = c(0.01, 0.1, 1, 10, 100)) +
    scale_y_log10(limits = c(0.005, 100), breaks = c(0.01, 0.1, 1, 10, 100)) +
    ggtitle(paste(context, "Contacts", time)) +
    xlab("No. of Contacts (Respondent)") +
    ylab("No. of Contacts (CC)") +
    theme(text = element_text(size = 30)) +
    scale_color_manual(values = palette()) +
    theme(legend.position = "bottom", legend.title=element_blank()) +
    theme(axis.ticks.x = element_line(),
          axis.ticks.y = element_line(),
          axis.ticks.length = unit(5, "pt"))

    ggsave(paste0("CorrelationPlotRespCC_", context, "_", time, ".pdf"), dpi = 500, w = 9, h = 9)
    ggsave(paste0("CorrelationPlotRespCC_", context, "_", time, ".png"), dpi = 500, w = 9, h = 9)
  }
}


p1_2019 <- ggplot(data_reduced %>% filter(!is.na(age_group))) + 
  #geom_point(aes(x=respondent_work_2019, y = cc_pre_work_2019), color = "#008083", size = 3) +
  geom_point(aes(x=respondent_work_2019, y = cc_pre_work_2019, color = age_group), size = 3) +
  theme_minimal() +
  scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100) )+
  scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100) )+
  ggtitle("Work Contacts (2019)") +
  xlab("No. of Contacts (Respondent)") +
  ylab("No. of Contacts (CC)") +
  theme(text = element_text(size = 30)) +
  scale_color_manual(values = palette()) +
  theme(legend.position = "bottom", legend.title=element_blank())+
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2019 <- ggplot(data_reduced_no_out_leisure  %>% filter(!is.na(age_group))) + 
  #geom_point(aes(x=respondent_leisure_2019, y = cc_pre_leisure_2019), color = "#008083", size = 3) +
   geom_point(aes(x=respondent_leisure_2019, y = cc_pre_leisure_2019, color = age_group), size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Leisure Contacts (2019)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "none", legend.title=element_blank())+
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2019 <- ggplot(data_reduced_no_out_all) + 
  geom_point(aes(x=respondent_all_2019, y = cc_pre_all_2019), color = "#008083", size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("All Contacts (2019)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p1_2020 <- ggplot(data_reduced_no_out_work %>% filter(!is.na(age_group))) + 
# geom_point(aes(x=respondent_work_03_2020, y = cc_pre_work_03_2020), color = "#008083", size = 3) +
  geom_point(aes(x=respondent_work_03_2020, y = cc_pre_work_03_2020, color = age_group), size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Work Contacts (2020)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
    theme(legend.position = "none", legend.title=element_blank())+
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2020 <- ggplot(data_reduced_no_out_leisure  %>% filter(!is.na(age_group))) + 
  geom_point(aes(x=respondent_leisure_03_2020, y = cc_pre_leisure_03_2020), color = "#008083", size = 3) +
  # geom_point(aes(x=respondent_leisure_03_2020, y = cc_pre_leisure_03_2020, color =age_group), size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Leisure Contacts (2020)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
    theme(legend.position = "none", legend.title=element_blank())+
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2020 <- ggplot(data_reduced_no_out_all) + 
  geom_point(aes(x=respondent_all_03_2020, y = cc_pre_all_03_2020), color = "#008083", size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("All Contacts (2020)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p1_2021 <- ggplot(data_reduced_no_out_work  %>% filter(!is.na(age_group))) + 
#  geom_point(aes(x=respondent_work_summer_2021, y = cc_pre_work_summer_2021), color = "#008083", size = 3) +
   geom_point(aes(x=respondent_work_summer_2021, y = cc_pre_work_summer_2021, color = age_group), size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Work Contacts (2021)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
    theme(legend.position = "none", legend.title=element_blank())+
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2021 <- ggplot(data_reduced_no_out_leisure  %>% filter(!is.na(age_group))) + 
#  geom_point(aes(x=respondent_leisure_summer_2021, y = cc_pre_leisure_summer_2021), color = "#008083", size = 3) +
   geom_point(aes(x=respondent_leisure_summer_2021, y = cc_pre_leisure_summer_2021, color =age_group), size = 3) +
  theme_minimal() +
  ggtitle("Leisure Contacts (2021)") +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
    theme(legend.position = "none", legend.title=element_blank())+
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2021 <- ggplot(data_reduced_no_out_all) + 
  geom_point(aes(x=respondent_all_summer_2021, y = cc_pre_all_summer_2021), color = "#008083", size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("All Contacts (2021)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p1_2023 <- ggplot(data_reduced_no_out_work  %>% filter(!is.na(age_group))) + 
  #geom_point(aes(x=respondent_work_01_2023, y = cc_pre_work_01_2023), color = "#008083", size = 3) +
  geom_point(aes(x=respondent_work_01_2023, y = cc_pre_work_01_2023, color = age_group), size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Work Contacts (2023)") +
  xlab("No. of Contacts \n of Respondent") +
    theme(legend.position = "none", legend.title=element_blank())+
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2023 <- ggplot(data_reduced_no_out_leisure  %>% filter(!is.na(age_group))) + 
 # geom_point(aes(x=respondent_leisure_01_2023, y = cc_pre_leisure_01_2023), color = "#008083", size = 3) +
   geom_point(aes(x=respondent_leisure_01_2023, y = cc_pre_leisure_01_2023, color = age_group), size = 3) +
  theme_minimal() +
  ggtitle("Leisure Contacts (2023)") +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
    theme(legend.position = "none", legend.title=element_blank())+
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2023 <- ggplot(data_reduced_not_out_all ) + 
  geom_point(aes(x=respondent_all_01_2023, y = cc_pre_all_01_2023), color = "#008083", size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("All Contacts (2023)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p <- arrangeGrob(p1_2019, p1_2020, p1_2021, p1_2023,
                  p2_2019, p2_2020, p2_2021, p2_2023, nrow = 2)
#                  p3_2019, p3_2020, p3_2021, p3_2023,
#                 nrow = 3)

ggsave("CorrelationPlotsAge.pdf", p, dpi = 500, w = 30, h = 14)
ggsave("CorrelationPlotsAge.png", p, dpi = 500, w = 30, h = 14)


p1_2020 <- ggplot(data_reduced_no_out_work_rel) + 
  geom_point(aes(x=respondent_work_rel_2019_2020, y = cc_pre_work_rel_2019_2020), color = "#008083", size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,110))+
  scale_y_log10(limits=c(1,110))+
  ggtitle("Work Contacts (2020)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2020 <- ggplot(data_reduced_no_out_leisure_rel) + 
  geom_point(aes(x=respondent_leisure_rel_2019_2020, y = cc_pre_leisure_rel_2019_2020), color = "#008083", size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,110))+
  scale_y_log10(limits=c(1,110))+
  ggtitle("Leisure Contacts (2020)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2020 <- ggplot(data_reduced_no_out_all_rel) + 
  geom_point(aes(x=respondent_all_rel_2019_2020, y = cc_pre_all_rel_2019_2020), color = "#008083", size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,110))+
  scale_y_log10(limits=c(1,110))+
  ggtitle("All Contacts (2020)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p1_2021 <- ggplot(data_reduced_no_out_work_rel) + 
  geom_point(aes(x=respondent_work_rel_2019_2021, y = cc_pre_work_rel_2019_2021), color = "#008083", size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,110))+
  scale_y_log10(limits=c(1,100))+
  ggtitle("Work Contacts (2021)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2021 <- ggplot(data_reduced_no_out_leisure_rel) + 
  geom_point(aes(x=respondent_leisure_rel_2019_2021, y = cc_pre_leisure_rel_2019_2021), color = "#008083", size = 3) +
  theme_minimal() +
  ggtitle("Leisure Contacts (2021)") +
  scale_x_log10(limits=c(1,110))+
  scale_y_log10(limits=c(1,110))+
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2021 <- ggplot(data_reduced_no_out_all_rel) + 
  geom_point(aes(x=respondent_all_rel_2019_2021, y = cc_pre_all_rel_2019_2021), color = "#008083", size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,110))+
  scale_y_log10(limits=c(1,110))+
  ggtitle("All Contacts (2021)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p1_2023 <- ggplot(data_reduced_no_out_work_rel) + 
  geom_point(aes(x=respondent_work_rel_2019_2023, y = cc_pre_work_rel_2019_2023), color = "#008083", size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,110))+
  scale_y_log10(limits=c(1,110))+
  ggtitle("Work Contacts (2023)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2023 <- ggplot(data_reduced_no_out_leisure_rel) + 
  geom_point(aes(x=respondent_leisure_rel_2019_2023, y = cc_pre_leisure_rel_2019_2023), color = "#008083", size = 3) +
  theme_minimal() +
  ggtitle("Leisure Contacts (2023)") +
  scale_x_log10(limits=c(1,110))+
  scale_y_log10(limits=c(1,110))+
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2023 <- ggplot(data_reduced_not_out_all_rel) + 
  geom_point(aes(x=respondent_all_rel_2019_2023, y = cc_pre_all_rel_2019_2023), color = "#008083", size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,110))+
  scale_y_log10(limits=c(1,110))+
  ggtitle("All Contacts (2023)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (pre-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p <- arrangeGrob(p1_2020, p1_2021, p1_2023,
                p2_2020, p2_2021, p2_2023, nrow = 2)

ggsave("CorrelationPlotsRespondent_REL.pdf", p, dpi = 500, w = 21, h = 14)
ggsave("CorrelationPlotsRespondent_REL.png", p, dpi = 500, w = 21, h = 14)


correlation_matrix$correlation_coefficient <- as.double(correlation_matrix$correlation_coefficient)
correlation_matrix$year <- factor(correlation_matrix$year, levels = c("2019", "03_2020", "summer_2021", "01_2023", "2019_2020", "2019_2021", "2019_2023"))

ggplot(correlation_matrix %>% filter(abs_or_rel == "abs")) +
geom_point(aes(x=year, y =context, size = correlation_coefficient, color = correlation_coefficient)) +
theme_minimal() +
  theme(text = element_text(size = 30)) +
  scale_color_viridis(option = "A", direction=-1) +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "left", legend.title = element_blank()) +
  theme(panel.spacing = unit(0.8, "cm", data = NULL)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  xlab("Point In Time") +
  ylab("Context")

ggsave("CorrelationTest.pdf", dpi = 500, w = 12, h = 3.4)
ggsave("CorrelationTest.png", dpi = 500, w = 12, h = 3.4)
  

p1_2019 <-ggplot(data_reduced_yes_work) + 
  geom_point(aes(x=respondent_work_2019, y = cc_during_work_2019), color = "#008083", size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Work Contacts (2019)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (during-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2019 <- ggplot(data_reduced_yes_leisure) + 
  geom_point(aes(x=respondent_leisure_2019, y = cc_during_leisure_2019), color = "#008083", size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Leisure Contacts (2019)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (during-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2019 <- ggplot(data_reduced_yes_all) + 
  geom_point(aes(x=respondent_all_2019, y = cc_during_all_2019), color = "#008083", size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("All Contacts (2019)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (during-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p1_2020 <- ggplot(data_reduced_yes_work) + 
  geom_point(aes(x=respondent_work_03_2020, y = cc_during_work_03_2020), color = "#008083", size = 3) +
  theme_minimal() +
  #xlim(0,60) +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Work Contacts (2020)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (during-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2020 <- ggplot(data_reduced_yes_leisure) + 
  geom_point(aes(x=respondent_leisure_03_2020, y = cc_during_leisure_03_2020), color = "#008083", size = 3) +
  theme_minimal() +
  #xlim(0,25) +
  #ylim(0,25) +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Leisure Contacts (2020)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (during-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2020 <- ggplot(data_reduced_yes_all) + 
  geom_point(aes(x=respondent_all_03_2020, y = cc_during_all_03_2020), color = "#008083", size = 3) +
  theme_minimal() +
  #xlim(0,100) +
  #ylim(0,100) +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("All Contacts (2020)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (during-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p1_2021 <- ggplot(data_reduced_yes_work) + 
  geom_point(aes(x=respondent_work_summer_2021, y = cc_during_work_summer_2021), color = "#008083", size = 3) +
  theme_minimal() +
  #xlim(0,110) +
  #ylim(0,110) +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Work Contacts (2021)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (during-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2021 <- ggplot(data_reduced_yes_leisure) + 
  geom_point(aes(x=respondent_leisure_summer_2021, y = cc_during_leisure_summer_2021), color = "#008083", size = 3) +
  theme_minimal() +
  ggtitle("Leisure Contacts (2021)") +
  #  xlim(0,60) +
  # ylim(0,100) +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (during-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2021 <- ggplot(data_reduced_yes_all) + 
  geom_point(aes(x=respondent_all_summer_2021, y = cc_during_all_summer_2021), color = "#008083", size = 3) +
  theme_minimal() +
  #xlim(0,150) +
  #ylim(0,150) +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("All Contacts (2021)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (during-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p1_2023 <- ggplot(data_reduced_yes_work) + 
  geom_point(aes(x=respondent_work_01_2023, y = cc_during_work_01_2023), color = "#008083", size = 3) +
  theme_minimal() +
  #xlim(0,150) +
  #ylim(0,150) +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Work Contacts (2023)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (during-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2023 <- ggplot(data_reduced_yes_leisure) + 
  geom_point(aes(x=respondent_leisure_01_2023, y = cc_during_leisure_01_2023), color = "#008083", size = 3) +
  theme_minimal() +
  ggtitle("Leisure Contacts (2023)") +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (during-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2023 <- ggplot(data_reduced_yes_all) + 
  geom_point(aes(x=respondent_all_01_2023, y = cc_during_all_01_2023), color = "#008083", size = 3) +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("All Contacts (2023)") +
  xlab("No. of Contacts \n of Respondent") +
  ylab("No. of Contacts \n of CC (during-pandemic)") +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p <- arrangeGrob(p1_2019, p1_2020, p1_2021, p1_2023,
                 p2_2019, p2_2020, p2_2021, p2_2023,
                 p3_2019, p3_2020, p3_2021, p3_2023,
                 nrow = 3)

ggsave("CorrelationPlotsChangedCCRespondentsDuring.pdf", p, dpi = 500, w = 30, h = 21)
ggsave("CorrelationPlotsChangedCCRespondentsDuring.png", p, dpi = 500, w = 30, h = 21)
