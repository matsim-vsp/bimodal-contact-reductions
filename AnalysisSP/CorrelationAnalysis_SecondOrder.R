
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

palette <- function() {
  c("#542788", "#998ec3", "#f1a340")
}

ggplot(data_reduced %>% 
mutate(respondent_leisure_2019 = case_when(respondent_leisure_2019 == 0 ~ 0.01, .default = respondent_leisure_2019)) %>%
mutate(cc_pre_leisure_2019 = case_when(cc_pre_leisure_2019 == 0 ~ 0.01, .default = cc_pre_leisure_2019)) %>%
mutate(RiskyCarefulAtt = case_when(is.na(RiskyCarefulAtt) ~ "No Risk Perception Score", 
.default = RiskyCarefulAtt)) %>%
filter(respondent_leisure_2019 != -1000) %>% 
filter(cc_pre_leisure_2019 != -1000)) + 
  geom_jitter(aes(x=respondent_leisure_2019, y = cc_pre_leisure_2019, color = RiskyCarefulAtt), size = 3, alpha = 0.7) +
  theme_minimal() +
  xlab("Number of Participant's Leisure Contacts") +
  ylab("Number of CC's Leisure Contacts") +
  ggtitle("Leisure - 2019") +
  scale_color_manual(values = palette()) +
  scale_x_log10(breaks=c(0.01, 0.1, 1,10,100)) +
  scale_y_log10(breaks=c(0.01, 0.1, 1,10,100)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  guides(color = guide_legend(nrow=2)) +
  theme(plot.title = element_text(hjust = 0.5)) 
  
  ggsave(paste0("CorrelationPlotRespCC_", "leisure_2019" , ".pdf"), dpi = 500, w = 9.5, h = 9.5)
  ggsave(paste0("CorrelationPlotRespCC_", "leisure", "_2019", ".png"), dpi = 500, w = 9.5, h = 9.5)

  ggplot(data_reduced %>% 
mutate(respondent_leisure_03_2020 = case_when(respondent_leisure_03_2020 == 0 ~ 0.01, .default = respondent_leisure_03_2020)) %>%
mutate(cc_pre_leisure_03_2020 = case_when(cc_pre_leisure_03_2020 == 0 ~ 0.01, .default = cc_pre_leisure_03_2020)) %>%
mutate(RiskyCarefulAtt = case_when(is.na(RiskyCarefulAtt) ~ "No Risk Perception Score", 
.default = RiskyCarefulAtt)) %>%
filter(respondent_leisure_03_2020 != -1000) %>% 
filter(cc_pre_leisure_03_2020 != -1000)) + 
  geom_jitter(aes(x=respondent_leisure_03_2020, y = cc_pre_leisure_03_2020, color = RiskyCarefulAtt), size = 3, alpha = 0.7) +
  theme_minimal() +
  xlab("Number of Participant's Leisure Contacts") +
  ylab("Number of CC's Leisure Contacts") +
  ggtitle("Leisure - March 2020") +
  scale_color_manual(values = palette()) +
  scale_x_log10(breaks=c(0.01, 0.1, 1,10,100)) +
  scale_y_log10(breaks=c(0.01, 0.1, 1,10,100)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  guides(color = guide_legend(nrow=2)) +
  theme(plot.title = element_text(hjust = 0.5)) 
  
  ggsave(paste0("CorrelationPlotRespCC_", "leisure_0320" , ".pdf"), dpi = 500, w = 9.5, h = 9.5)
  ggsave(paste0("CorrelationPlotRespCC_", "leisure", "_0320", ".png"), dpi = 500, w = 9.5, h = 9.5)

  ggplot(data_reduced %>% 
mutate(respondent_leisure_summer_2021 = case_when(respondent_leisure_summer_2021 == 0 ~ 0.01, .default = respondent_leisure_summer_2021)) %>%
mutate(cc_pre_leisure_summer_2021 = case_when(cc_pre_leisure_summer_2021 == 0 ~ 0.01, .default = cc_pre_leisure_summer_2021)) %>%
mutate(RiskyCarefulAtt = case_when(is.na(RiskyCarefulAtt) ~ "No Risk Perception Score", 
.default = RiskyCarefulAtt)) %>%
filter(respondent_leisure_summer_2021 != -1000) %>% 
filter(cc_pre_leisure_summer_2021 != -1000)) + 
  geom_jitter(aes(x=respondent_leisure_summer_2021, y = cc_pre_leisure_summer_2021, color = RiskyCarefulAtt), size = 3, alpha = 0.7) +
  theme_minimal() +
  xlab("Number of Participant's Leisure Contacts") +
  ylab("Number of CC's Leisure Contacts") +
  ggtitle("Leisure - Summer 2021") +
  scale_color_manual(values = palette()) +
  scale_x_log10(breaks=c(0.01, 0.1, 1,10,100)) +
  scale_y_log10(breaks=c(0.01, 0.1, 1,10,100)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  guides(color = guide_legend(nrow=2)) +
  theme(plot.title = element_text(hjust = 0.5)) 
  
  ggsave(paste0("CorrelationPlotRespCC_", "leisure_summer21" , ".pdf"), dpi = 500, w = 9.5, h = 9.5)
  ggsave(paste0("CorrelationPlotRespCC_", "leisure", "_summer21", ".png"), dpi = 500, w = 9.5, h = 9.5)

ggplot(data_reduced %>% 
mutate(respondent_leisure_01_2023 = case_when(respondent_leisure_01_2023 == 0 ~ 0.01, .default = respondent_leisure_01_2023)) %>%
mutate(cc_pre_leisure_01_2023 = case_when(cc_pre_leisure_01_2023 == 0 ~ 0.01, .default = cc_pre_leisure_01_2023)) %>%
mutate(RiskyCarefulAtt = case_when(is.na(RiskyCarefulAtt) ~ "No Risk Perception Score", 
.default = RiskyCarefulAtt)) %>%
filter(respondent_leisure_01_2023 != -1000) %>% 
filter(cc_pre_leisure_01_2023 != -1000)) + 
  geom_jitter(aes(x=respondent_leisure_01_2023, y = cc_pre_leisure_01_2023, color = RiskyCarefulAtt), size = 3, alpha = 0.7) +
  theme_minimal() +
  xlab("Number of Participant's Leisure Contacts") +
  ylab("Number of CC's Leisure Contacts") +
  ggtitle("Leisure - January 2023") +
  scale_color_manual(values = palette()) +
  scale_x_log10(breaks=c(0.01, 0.1, 1,10,100)) +
  scale_y_log10(breaks=c(0.01, 0.1, 1,10,100)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  guides(color = guide_legend(nrow=2)) +
  theme(plot.title = element_text(hjust = 0.5)) 

  ggsave(paste0("CorrelationPlotRespCC_", "leisure_0123" , ".pdf"), dpi = 500, w = 9.5, h = 9.5)
  ggsave(paste0("CorrelationPlotRespCC_", "leisure", "_0123", ".png"), dpi = 500, w = 9.5, h = 9.5)


ggplot(data_reduced %>% 
mutate(respondent_work_2019 = case_when(respondent_work_2019 == 0 ~ 0.01, .default = respondent_work_2019)) %>%
mutate(cc_pre_work_2019 = case_when(cc_pre_work_2019 == 0 ~ 0.01, .default = cc_pre_work_2019)) %>%
mutate(RiskyCarefulAtt = case_when(is.na(RiskyCarefulAtt) ~ "No Risk Perception Score", 
.default = RiskyCarefulAtt)) %>%
filter(respondent_work_2019 != -1000) %>% 
filter(cc_pre_work_2019 != -1000)) + 
  geom_jitter(aes(x=respondent_work_2019, y = cc_pre_work_2019, color = RiskyCarefulAtt), size = 3, alpha = 0.7) +
  theme_minimal() +
  xlab("Number of Participant's Work Contacts") +
  ylab("Number of CC's Work Contacts") +
  ggtitle("Work - 2019") +
  scale_color_manual(values = palette()) +
  scale_x_log10(breaks=c(0.01, 0.1, 1,10,100,1000)) +
  scale_y_log10(breaks=c(0.01, 0.1, 1,10,100,1000)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  guides(color = guide_legend(nrow=2)) +
  theme(plot.title = element_text(hjust = 0.5)) 
  
  ggsave(paste0("CorrelationPlotRespCC_", "work_2019" , ".pdf"), dpi = 500, w = 9.5, h = 9.5)
  ggsave(paste0("CorrelationPlotRespCC_", "work", "_2019", ".png"), dpi = 500, w = 9.5, h = 9.5)

  ggplot(data_reduced %>% 
mutate(respondent_work_03_2020 = case_when(respondent_work_03_2020 == 0 ~ 0.01, .default = respondent_work_03_2020)) %>%
mutate(cc_pre_work_03_2020 = case_when(cc_pre_work_03_2020 == 0 ~ 0.01, .default = cc_pre_work_03_2020)) %>%
mutate(RiskyCarefulAtt = case_when(is.na(RiskyCarefulAtt) ~ "No Risk Perception Score", 
.default = RiskyCarefulAtt)) %>%
filter(respondent_work_03_2020 != -1000) %>% 
filter(cc_pre_work_03_2020 != -1000)) + 
  geom_jitter(aes(x=respondent_work_03_2020, y = cc_pre_work_03_2020, color = RiskyCarefulAtt), size = 3, alpha = 0.7) +
  theme_minimal() +
  xlab("Number of Participant's Work Contacts") +
  ylab("Number of CC's Work Contacts") +
  ggtitle("Work - March 2020") +
  scale_color_manual(values = palette()) +
  scale_x_log10(breaks=c(0.01, 0.1, 1,10,100,1000)) +
  scale_y_log10(breaks=c(0.01, 0.1, 1,10,100,1000)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  guides(color = guide_legend(nrow=2)) +
  theme(plot.title = element_text(hjust = 0.5)) 
  
  ggsave(paste0("CorrelationPlotRespCC_", "work_0320" , ".pdf"), dpi = 500, w = 9.5, h = 9.5)
  ggsave(paste0("CorrelationPlotRespCC_", "work", "_0320", ".png"), dpi = 500, w = 9.5, h = 9.5)

  ggplot(data_reduced %>% 
mutate(respondent_work_summer_2021 = case_when(respondent_work_summer_2021 == 0 ~ 0.01, .default = respondent_work_summer_2021)) %>%
mutate(cc_pre_work_summer_2021 = case_when(cc_pre_work_summer_2021 == 0 ~ 0.01, .default = cc_pre_work_summer_2021)) %>%
mutate(RiskyCarefulAtt = case_when(is.na(RiskyCarefulAtt) ~ "No Risk Perception Score", 
.default = RiskyCarefulAtt)) %>%
filter(respondent_work_summer_2021 != -1000) %>% 
filter(cc_pre_work_summer_2021 != -1000)) + 
  geom_jitter(aes(x=respondent_work_summer_2021, y = cc_pre_work_summer_2021, color = RiskyCarefulAtt), size = 3, alpha = 0.7) +
  theme_minimal() +
  xlab("Number of Participant's Work Contacts") +
  ylab("Number of CC's Work Contacts") +
  ggtitle("Work - Summer 2021") +
  scale_color_manual(values = palette()) +
  scale_x_log10(breaks=c(0.01, 0.1, 1,10,1000)) +
  scale_y_log10(breaks=c(0.01, 0.1, 1,10,1000)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  guides(color = guide_legend(nrow=2)) +
  theme(plot.title = element_text(hjust = 0.5)) 
  
  ggsave(paste0("CorrelationPlotRespCC_", "work_summer21" , ".pdf"), dpi = 500, w = 9.5, h = 9.5)
  ggsave(paste0("CorrelationPlotRespCC_", "work", "_summer21", ".png"), dpi = 500, w = 9.5, h = 9.5)

ggplot(data_reduced %>% 
mutate(respondent_work_01_2023 = case_when(respondent_work_01_2023 == 0 ~ 0.01, .default = respondent_work_01_2023)) %>%
mutate(cc_pre_work_01_2023 = case_when(cc_pre_work_01_2023 == 0 ~ 0.01, .default = cc_pre_work_01_2023)) %>%
mutate(RiskyCarefulAtt = case_when(is.na(RiskyCarefulAtt) ~ "No Risk Perception Score", 
.default = RiskyCarefulAtt)) %>%
filter(respondent_work_01_2023 != -1000) %>% 
filter(cc_pre_work_01_2023 != -1000)) + 
  geom_jitter(aes(x=respondent_work_01_2023, y = cc_pre_work_01_2023, color = RiskyCarefulAtt), size = 3, alpha = 0.7) +
  theme_minimal() +
  xlab("Number of Participant's Work Contacts") +
  ylab("Number of CC's Work Contacts") +
  ggtitle("Work - January 2023") +
  scale_color_manual(values = palette()) +
  scale_x_log10(breaks=c(0.01, 0.1, 1,10,1000)) +
  scale_y_log10(breaks=c(0.01, 0.1, 1,10,1000)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  guides(color = guide_legend(nrow=2)) +
  theme(plot.title = element_text(hjust = 0.5)) 

  ggsave(paste0("CorrelationPlotRespCC_", "work_0123" , ".pdf"), dpi = 500, w = 9.5, h = 9.5)
  ggsave(paste0("CorrelationPlotRespCC_", "work", "_0123", ".png"), dpi = 500, w = 9.5, h = 9.5)

