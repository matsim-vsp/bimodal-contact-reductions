
library(tidyverse) #version 2.0.0
library(viridis) #version 0.6.5
library(here) #version 1.0.1

here()
source("./R/DataCleaningPrepForContactAnalysis.R")


# Creation of Correlation Matrix ------------------------------------------

# Computation of Pearson's correlation coefficient of participants' and their CCs' number of contacts.

correlation_matrix <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(correlation_matrix) <- c("abs_or_rel", "context", "year", "change_of_cc", "pre_or_during", "correlation_coefficient", "p-value")

times <- c("2019", "03_2020", "summer_2021", "01_2023")
riskperceptions <- c("all", "Risk-tolerant", "Risk-averse")

for(riskperception in riskperceptions){
  for(time in times){
    respname <- paste0("respondent_work_", time)
    ccname <- paste0("cc_pre_work_", time)
    data_reduced_RemoveNAs <- data_reduced %>% filter(!!respname != -1000) %>% filter(!!ccname != -1000) #In the cleaning NAs are mapped to -1000 --> Need to be removed for correlation analysis
    if(riskperception != "all"){
      data_reduced_RemoveNAs <- data_reduced_RemoveNAs %>% filter(RiskyCarefulAtt == riskperception)
    }
    correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("abs", "work", time, riskperception, "pre", cor(data_reduced_RemoveNAs[respname], data_reduced_RemoveNAs[ccname], use = "pairwise.complete.obs", method = "pearson"), cor.test(data_reduced_RemoveNAs %>% pull(!!respname), data_reduced_RemoveNAs %>% pull(!!ccname))$p.value)

    respname <- paste0("respondent_leisure_", time)
    ccname <- paste0("cc_pre_leisure_", time)
    data_reduced_RemoveNAs <- data_reduced %>% filter(!!respname != -1000) %>% filter(!!ccname != -1000)
    if(riskperception != "all"){
      data_reduced_RemoveNAs <- data_reduced_RemoveNAs %>% filter(RiskyCarefulAtt == riskperception)
    }
    correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("abs", "leisure", time, riskperception, "pre", cor(data_reduced_RemoveNAs[respname], data_reduced_RemoveNAs[ccname], use = "pairwise.complete.obs", method = "pearson"), cor.test(data_reduced_RemoveNAs %>% pull(!!respname), data_reduced_RemoveNAs %>% pull(!!ccname))$p.value)
  }
}

# Computation of Pearson's correlation coefficient for different risk perception groups

times <- c("2020", "2021", "2023")
riskperceptions <- c("all", "Risk-tolerant", "Risk-averse")

for(riskperception in riskperceptions){
  for(time in times){
    respname <- paste0("respondent_work_rel_2019_", time)
    ccname <- paste0("cc_pre_work_rel_2019_", time)
    data_reduced[sapply(data_reduced, is.infinite)] <- NA
    data_reduced_RemoveNAs <- data_reduced %>% filter(!is.na(!!respname)) %>% filter(!is.na(!!ccname))
    if(riskperception != "all"){
      data_reduced_RemoveNAs <- data_reduced_RemoveNAs %>% filter(RiskyCarefulAtt == riskperception)
    }
    correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("rel", "work", time, riskperception, "pre", cor(data_reduced_RemoveNAs[respname], data_reduced_RemoveNAs[ccname], use = "pairwise.complete.obs", method = "pearson"), cor.test(data_reduced_RemoveNAs %>% pull(!!respname), data_reduced_RemoveNAs %>% pull(!!ccname))$p.value)
    
    respname <- paste0("respondent_leisure_rel_2019_", time)
    ccname <- paste0("cc_pre_leisure_rel_2019_", time)
    data_reduced[sapply(data_reduced, is.infinite)] <- NA
    data_reduced_RemoveNAs <- data_reduced %>% filter(!is.na(!!respname)) %>% filter(!is.na(!!ccname))
    if(riskperception != "all"){
      data_reduced_RemoveNAs <- data_reduced_RemoveNAs %>% filter(RiskyCarefulAtt == riskperception)
    }
    correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("rel", "leisure", time, riskperception, "pre", cor(data_reduced_RemoveNAs[respname], data_reduced_RemoveNAs[ccname], use = "pairwise.complete.obs", method = "pearson"),  cor.test(data_reduced_RemoveNAs %>% pull(!!respname), data_reduced_RemoveNAs %>% pull(!!ccname))$p.value)
  }
}


# Correlation Plot --------------------------------------------------------

# Produces Figure 6

palette <- function() {
    c("#B09C85FF", "#3C5488FF", "#DC0000FF")
}

ggplot(data_reduced %>% 
mutate(RiskyCarefulAtt = case_when(is.na(RiskyCarefulAtt) ~ "No Risk Perception Score Available", 
.default = RiskyCarefulAtt)) %>%
filter(respondent_leisure_01_2023 != -1000) %>% #Again, in the data cleaning process NAs are mapped to -1000 and need to be removed here
filter(cc_pre_leisure_01_2023 != -1000)) +  #If one's interested in the correlation plot of another time/context, "respondent_leisure_01_2023" and "cc_pre_leisure_01_2023" need to be changedc accordingly
  geom_jitter(aes(x=respondent_leisure_01_2023, y = cc_pre_leisure_01_2023, color = RiskyCarefulAtt), size = 3, alpha = 0.7) +
  theme_minimal() +
  xlab("Number of Participant's\nLeisure Contacts") +
  ylab("Number of CC's\nLeisure Contacts") +
  scale_color_manual(values = palette()) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,1,3,10,30,100)) +
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,1,3,10,30,100)) +
  my_theme() +
  guides(color = guide_legend(nrow=3)) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 

  
  #ggsave(paste0("./plots/","CorrelationPlotRespCC_", "leisure_012023" , ".pdf"), dpi = 500, w = 9, h = 10)
  #ggsave(paste0("./plots/","CorrelationPlotRespCC_", "leisure", "_0123", ".png"), dpi = 500, w =  9, h = 10)


