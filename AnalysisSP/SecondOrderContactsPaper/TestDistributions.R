library(tidyverse)
library(stats)

here()
source("./AnalysisSP/SecondOrderContactsPaper/DataCleaningPrepForContactAnalysis.R")
source("./AnalysisSP/SecondOrderContactsPaper/mytheme.r")

colnames(data_reduced)

# Work --------------------------------------------------------------------

#03/2020 vs Summer 2021

data_reducedWork <- data_reduced %>% filter(respondent_work_rel_2019_2020 < 150) %>%
                              filter(respondent_work_rel_2019_2021 < 150) %>%
                              filter(respondent_work_rel_2019_2023 < 150)

ks.test(data_reducedWork$respondent_work_rel_2019_2020, data_reducedWork$respondent_work_rel_2019_2021)

#03/2020 vs 01/2023
ks.test(data_reducedWork$respondent_work_rel_2019_2020, data_reducedWork$respondent_work_rel_2019_2023)

#Summer 2021 vs 01/2023
ks.test(data_reducedWork$respondent_work_rel_2019_2021, data_reducedWork$respondent_work_rel_2019_2023)

ecdf_comp2020 <- data_reducedWork %>% count(respondent_work_rel_2019_2020) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
  mutate(ecdf = cum/sum) %>%
  mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(uci = case_when(uci > 1 ~ 1, .default = uci))

ecdf_comp2021 <- data_reducedWork %>% count(respondent_work_rel_2019_2021) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
  mutate(ecdf = cum/sum) %>%
  mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(uci = case_when(uci > 1 ~ 1, .default = uci))

ecdf_comp2023 <- data_reducedWork %>% count(respondent_work_rel_2019_2023) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
  mutate(ecdf = cum/sum) %>%
  mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(uci = case_when(uci > 1 ~ 1, .default = uci))

ggplot() +
  geom_ribbon(data = ecdf_comp2021, aes(ymin = lci, ymax = uci, x = respondent_work_rel_2019_2021), alpha = 0.3, fill = "red")+
  geom_line(data = ecdf_comp2021, aes(y=ecdf, x = respondent_work_rel_2019_2021), color = "red", size = 1) +
  geom_ribbon(data = ecdf_comp2020, aes(ymin = lci, ymax = uci, x = respondent_work_rel_2019_2020), alpha = 0.3, fill = "#542788")+
  geom_line(data = ecdf_comp2020, aes(y=ecdf, x = respondent_work_rel_2019_2020), color = "#542788", size = 1) +
  geom_ribbon(data = ecdf_comp2023, aes(ymin = lci, ymax = uci, x = respondent_work_rel_2019_2023), alpha = 0.3, fill = "green")+
  geom_line(data = ecdf_comp2023, aes(y=ecdf, x = respondent_work_rel_2019_2023), color = "green", size = 1) +
  xlab("Blue = 03/2020, Red = Summer2021,\nGreen = 01/2023") +
  xlim(-100,150) +
  my_theme() 

ggsave("ECDFWork.png", dpi = 500, w = 12, h = 6)


# Leisure --------------------------------------------------------------------

#03/2020 vs Summer 2021

data_reducedLeisure <- data_reduced %>% filter(respondent_leisure_rel_2019_2020 < 150) %>%
                filter(respondent_leisure_rel_2019_2021 < 150) %>%
                filter(respondent_leisure_rel_2019_2023 < 150)

ks.test(data_reducedLeisure$respondent_leisure_rel_2019_2020, data_reducedLeisure$respondent_leisure_rel_2019_2021)

#03/2020 vs 01/2023
ks.test(data_reducedLeisure$respondent_leisure_rel_2019_2020, data_reducedLeisure$respondent_leisure_rel_2019_2023)

#Summer 2021 vs 01/2023
ks.test(data_reducedLeisure$respondent_leisure_rel_2019_2021, data_reducedLeisure$respondent_leisure_rel_2019_2023)

ecdf_comp2020 <- data_reducedLeisure %>% count(respondent_leisure_rel_2019_2020) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
  mutate(ecdf = cum/sum) %>%
  mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(uci = case_when(uci > 1 ~ 1, .default = uci))

ecdf_comp2021 <- data_reducedLeisure %>% count(respondent_leisure_rel_2019_2021) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
  mutate(ecdf = cum/sum) %>%
  mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(uci = case_when(uci > 1 ~ 1, .default = uci))

ecdf_comp2023 <- data_reducedLeisure %>% count(respondent_leisure_rel_2019_2023) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
  mutate(ecdf = cum/sum) %>%
  mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(uci = case_when(uci > 1 ~ 1, .default = uci))

ggplot() +
  geom_ribbon(data = ecdf_comp2021, aes(ymin = lci, ymax = uci, x = respondent_leisure_rel_2019_2021), alpha = 0.3, fill = "red")+
  geom_line(data = ecdf_comp2021, aes(y=ecdf, x = respondent_leisure_rel_2019_2021), color = "red", size = 1) +
  geom_ribbon(data = ecdf_comp2020, aes(ymin = lci, ymax = uci, x = respondent_leisure_rel_2019_2020), alpha = 0.3, fill = "#542788")+
  geom_line(data = ecdf_comp2020, aes(y=ecdf, x = respondent_leisure_rel_2019_2020), color = "#542788", size = 1) +
  geom_ribbon(data = ecdf_comp2023, aes(ymin = lci, ymax = uci, x = respondent_leisure_rel_2019_2023), alpha = 0.3, fill = "green")+
  geom_line(data = ecdf_comp2023, aes(y=ecdf, x = respondent_leisure_rel_2019_2023), color = "green", size = 1) +
  xlab("Blue = 03/2020, Red = Summer2021,\nGreen = 01/2023") +
  xlim(-100,150) +
  my_theme() 

ggsave("ECDFLeisure.png", dpi = 500, w = 12, h = 6)


# DIFFERENCES BETWEEN RISK-AVERSE AND RISK-TOLERANT PARTICIPANTS ----------

# Work Risk-Averse vs Risk-Tolerant ---------------------------------------

data_reducedWork <- data_reduced %>% filter(respondent_work_rel_2019_2020 < 150) %>%
  filter(respondent_work_rel_2019_2021 < 150) %>%
  filter(respondent_work_rel_2019_2023 < 150)

#03/2020
data_reducedWorkAverse <- data_reducedWork %>% filter(RiskyCarefulAtt == "Risk-averse")
data_reducedWorkTolerant <- data_reducedWork %>% filter(RiskyCarefulAtt == "Risk-tolerant")

ks.test(data_reducedWorkAverse$respondent_work_rel_2019_2020, data_reducedWorkTolerant$respondent_work_rel_2019_2020)

#Summer 2021
ks.test(data_reducedWorkAverse$respondent_work_rel_2019_2021, data_reducedWorkTolerant$respondent_work_rel_2019_2021)

#01/2023
ks.test(data_reducedWorkAverse$respondent_work_rel_2019_2023, data_reducedWorkTolerant$respondent_work_rel_2019_2023)

# Leisure Risk-Averse vs Risk-Tolerant ---------------------------------------

#03/2020

data_reducedLeisure <- data_reduced %>% filter(respondent_leisure_rel_2019_2020 < 150) %>%
  filter(respondent_leisure_rel_2019_2021 < 150) %>%
  filter(respondent_leisure_rel_2019_2023 < 150)

data_reducedLeisureAverse <- data_reducedLeisure %>% filter(RiskyCarefulAtt == "Risk-averse")
data_reducedLeisureTolerant <- data_reducedLeisure %>% filter(RiskyCarefulAtt == "Risk-tolerant")

ks.test(data_reducedLeisureAverse$respondent_leisure_rel_2019_2020, data_reducedLeisureTolerant$respondent_leisure_rel_2019_2020)

#Summer 2021
ks.test(data_reducedLeisureAverse$respondent_leisure_rel_2019_2021, data_reducedLeisureTolerant$respondent_leisure_rel_2019_2021)

#01/2023
ks.test(data_reducedLeisureAverse$respondent_leisure_rel_2019_2023, data_reducedLeisureTolerant$respondent_leisure_rel_2019_2023)

times <- c("2020", "2021", "2023")
contexts <- c("work", "leisure")

for(context in contexts){
  for(time in times){
    column <- paste0("respondent_", context, "_rel_2019_", time)
    
    if(context == "work"){
      datafAverse <- data_reducedWorkAverse
      datafTolerant <- data_reducedWorkTolerant
    }else if(context == "leisure"){
      datafAverse <- data_reducedLeisureAverse
      datafTolerant <- data_reducedLeisureTolerant
    }
    
    ecdf_compAverse <- datafAverse %>% count(!!sym(column)) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
      mutate(ecdf = cum/sum) %>%
      mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
      mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
      mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
      mutate(uci = case_when(uci > 1 ~ 1, .default = uci))
    
    ecdf_compTolerant <- datafTolerant %>% count(!!sym(column)) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
      mutate(ecdf = cum/sum) %>%
      mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
      mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
      mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
      mutate(uci = case_when(uci > 1 ~ 1, .default = uci))
    
    if(time == "2020"){
      pointintime <- "03/2020"
    }else if(time == "2021"){
      pointintime <- "Summer 2021"
    }else if (time == "2023"){
      pointintime <- "01/2023"
    }
    
    plot <- ggplot() +
      geom_ribbon(data = ecdf_compTolerant, aes(ymin = lci, ymax = uci, x = !!sym(column), fill = "Risk-Tolerant"), alpha = 0.3)+
      geom_line(data = ecdf_compTolerant, aes(y=ecdf, x = !!sym(column), color = "Risk-Tolerant"), size = 1) +
      geom_ribbon(data = ecdf_compAverse, aes(ymin = lci, ymax = uci, x = !!sym(column), fill = "Risk-Averse"), alpha = 0.3)+
      geom_line(data = ecdf_compAverse, aes(y=ecdf, x = !!sym(column), color = "Risk-Averse"), size = 1) +
      xlab("Change of No. of Contacts (percent)") +
      ylab("ECDF") +
      xlim(-100,100) +
      my_theme()  +
      ggtitle(paste(tools::toTitleCase(context), pointintime)) +
      theme(legend.position = "bottom") +
      scale_color_manual(values=c("Risk-Tolerant" = "#DC0000FF", "Risk-Averse" = "#3C5488FF")) +
      scale_fill_manual(values=c("Risk-Tolerant" = "#DC0000FF", "Risk-Averse" = "#3C5488FF"),
                        guide = "none")
    
    if(context == "work" & time == "2020"){
      work2020 <- plot
    }else if(context == "work" & time == "2021"){
      work2021 <- plot
    }else if (context == "work" & time == "2023"){
      work2023 <- plot
    }else if(context == "leisure" & time == "2020"){
      leisure2020 <- plot  
    }else if(context == "leisure" & time == "2021"){
      leisure2021 <- plot
    }else if(context == "leisure" & time == "2023"){
      leisure2023 <- plot
    }
  
    ggsave(paste0("KolmogorovTestRiskPerc", context, time, ".pdf"), plot, dpi = 500, w = 12, h = 6)
    ggsave(paste0("KolmogorovTestRiskPerc", context, time, ".png"), dpi = 500, w = 12, h = 6)
  }
}

ggarrange(work2020, work2021, work2023, leisure2020, leisure2021, leisure2023, labels = c("A", "B", "C", "D", "E", "F"), nrow = 3, ncol = 2,font.label = list(size = 37), common.legend=TRUE, legend = "bottom")
ggsave("KolmogorovTestRiskPerc.pdf", dpi = 5, w = 24, h = 18)
ggsave("KolmogorovTestRiskPerc.png", dpi = 5, w = 24, h = 18)

# Difference Number of Infections -----------------------------------------

data_reduced <- data_reduced %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "0",
                                                                     num_c19_infs == "Einmal" ~ "1",
                                                                     num_c19_infs == "Zweimal" ~ "2",
                                                                     num_c19_infs == "Dreimal" ~ "3",
                                                                     num_c19_infs == "Mehr als dreimal" ~ "3",
                                                                     num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

data_reduced$num_c19_infs_eng <- factor(data_reduced$num_c19_infs_eng, levels = c("0", "1", "2", "3", "I Don't Want To Answer"))

data_reducedAverse <- data_reduced %>% filter(RiskyCarefulAtt == "Risk-averse") %>% filter(num_c19_infs_eng != "I Don't Want To Answer") %>% mutate(num_c19_infs_eng = as.numeric(num_c19_infs_eng)-1)
data_reducedTolerant <- data_reduced %>% filter(RiskyCarefulAtt == "Risk-tolerant") %>% filter(num_c19_infs_eng != "I Don't Want To Answer") %>% mutate(num_c19_infs_eng = as.integer(num_c19_infs_eng)-1)
ks.test(data_reducedAverse$num_c19_infs_eng, data_reducedTolerant$num_c19_infs_eng)

ecdf_compTolerant <- data_reducedTolerant %>% count(num_c19_infs_eng) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
  mutate(ecdf = cum/sum) %>%
  mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(uci = case_when(uci > 1 ~ 1, .default = uci))

ecdf_compAverse <- data_reducedAverse %>% count(num_c19_infs_eng) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
  mutate(ecdf = cum/sum) %>%
  mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(uci = case_when(uci > 1 ~ 1, .default = uci))

ggplot() +
  geom_ribbon(data = ecdf_compTolerant, aes(ymin = lci, ymax = uci, x = num_c19_infs_eng, fill = "Risk-Tolerant"), alpha = 0.3)+
  geom_line(data = ecdf_compTolerant, aes(y=ecdf, x = num_c19_infs_eng, color = "Risk-Tolerant"), size = 1) +
 geom_ribbon(data = ecdf_compAverse, aes(ymin = lci, ymax = uci, x = num_c19_infs_eng, fill = "Risk-Averse"), alpha = 0.3)+
geom_line(data = ecdf_compAverse, aes(y=ecdf, x = num_c19_infs_eng, color = "Risk-Averse"), size = 1) +
  xlab("Number of Infections") +
  ylab("ECDF") +
  xlim(0,4) +
  my_theme()  +
  theme(legend.position = "bottom") +
  scale_color_manual(values=c("Risk-Tolerant" = "#DC0000FF", "Risk-Averse" = "#3C5488FF")) +
  scale_fill_manual(values=c("Risk-Tolerant" = "#DC0000FF", "Risk-Averse" = "#3C5488FF"),
                    guide = "none")



# Difference date of first infection --------------------------------------

data_reduced <- data_reduced %>% mutate(date_f1_inf = case_when(is.na(date_f1_inf) ~ as.Date("3000-01-01"),
                                                                .default = as.Date(as.character(date_f1_inf)))) %>%
  filter(date_f1_inf != as.Date("1922-03-01")) %>%
  filter(date_f1_inf != as.Date("1965-06-12")) %>%
  filter(date_f1_inf != as.Date("2000-12-13")) %>%
  filter(date_f1_inf != as.Date("2019-12-21")) 

ecdf_comp <- data_reduced %>% filter(!is.na(RiskyCarefulAtt)) %>% group_by(RiskyCarefulAtt) %>% 
  count(date_f1_inf) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
  mutate(ecdf = cum/sum) %>% 
  mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(uci = case_when(uci > 1 ~ 1, .default = uci)) %>% ungroup()
ecdf_comp$date_f1_inf <- as.numeric(as.Date(ecdf_comp$date_f1_inf))

ecdf_compAverse <- ecdf_comp %>% filter(RiskyCarefulAtt == "Risk-averse")
ecdf_compTolerant <- ecdf_comp %>% filter(RiskyCarefulAtt == "Risk-tolerant")

ggplot() +
  geom_ribbon(data = ecdf_compTolerant, aes(ymin = lci, ymax = uci, x = as.Date(date_f1_inf), fill = "Risk-Tolerant"), alpha = 0.3)+
  geom_line(data = ecdf_compTolerant, aes(y=ecdf, x = as.Date(date_f1_inf), color = "Risk-Tolerant"), size = 1) +
  geom_ribbon(data = ecdf_compAverse, aes(ymin = lci, ymax = uci, x = as.Date(date_f1_inf), fill = "Risk-Averse"), alpha = 0.3)+
  geom_line(data = ecdf_compAverse, aes(y=ecdf, x = as.Date(date_f1_inf), color = "Risk-Averse"), size = 1) +
  xlab("Timing of First Infection") +
  ylab("ECDF") +
  xlim(as.Date(18322),as.Date(19539)) +
  my_theme()  +
  theme(legend.position = "bottom") +
  scale_color_manual(values=c("Risk-Tolerant" = "#DC0000FF", "Risk-Averse" = "#3C5488FF")) +
  scale_fill_manual(values=c("Risk-Tolerant" = "#DC0000FF", "Risk-Averse" = "#3C5488FF"),
                    guide = "none")

ks.test(ecdf_compAverse$date_f1_inf, ecdf_compTolerant$date_f1_inf)

# DIFFERENCES BETWEEN DIFFERENT AGE GROUPS ----------

data_reducedWork1839<- data_reducedWork %>% filter(age_bracket == "18-39")
data_reducedWork4059 <- data_reducedWork %>% filter(age_bracket == "40-59")
data_reducedWork60 <- data_reducedWork %>% filter(age_bracket == "60+")

#Work

#03/2020
ks.test(data_reducedWork1839$respondent_work_rel_2019_2020, data_reducedWork4059$respondent_work_rel_2019_2020)
ks.test(data_reducedWork1839$respondent_work_rel_2019_2020, data_reducedWork60$respondent_work_rel_2019_2020)
ks.test(data_reducedWork60$respondent_work_rel_2019_2020, data_reducedWork4059$respondent_work_rel_2019_2020)

#Summer 2021
ks.test(data_reducedWork1839$respondent_work_rel_2019_2021, data_reducedWork4059$respondent_work_rel_2019_2021)
ks.test(data_reducedWork1839$respondent_work_rel_2019_2021, data_reducedWork60$respondent_work_rel_2019_2021)
ks.test(data_reducedWork60$respondent_work_rel_2019_2021, data_reducedWork4059$respondent_work_rel_2019_2021)

#01/2023
ks.test(data_reducedWork1839$respondent_work_rel_2019_2023, data_reducedWork4059$respondent_work_rel_2019_2023)
ks.test(data_reducedWork1839$respondent_work_rel_2019_2023, data_reducedWork60$respondent_work_rel_2019_2023)
ks.test(data_reducedWork60$respondent_work_rel_2019_2023, data_reducedWork4059$respondent_work_rel_2019_2023)

#Leisure
data_reducedLeisure1839<- data_reducedLeisure %>% filter(age_bracket == "18-39")
data_reducedLeisure4059 <- data_reducedLeisure %>% filter(age_bracket == "40-59")
data_reducedLeisure60 <- data_reducedLeisure %>% filter(age_bracket == "60+")


#03/2020
ks.test(data_reducedLeisure1839$respondent_leisure_rel_2019_2020, data_reducedLeisure4059$respondent_leisure_rel_2019_2020)
ks.test(data_reducedLeisure1839$respondent_leisure_rel_2019_2020, data_reducedLeisure60$respondent_leisure_rel_2019_2020)
ks.test(data_reducedLeisure60$respondent_leisure_rel_2019_2020, data_reducedLeisure4059$respondent_leisure_rel_2019_2020)

#Summer 2021
ks.test(data_reducedLeisure1839$respondent_leisure_rel_2019_2021, data_reducedLeisure4059$respondent_leisure_rel_2019_2021)
ks.test(data_reducedLeisure1839$respondent_leisure_rel_2019_2021, data_reducedLeisure60$respondent_leisure_rel_2019_2021)
ks.test(data_reducedLeisure60$respondent_leisure_rel_2019_2021, data_reducedLeisure4059$respondent_leisure_rel_2019_2021)

#01/2023
ks.test(data_reducedLeisure1839$respondent_leisure_rel_2019_2023, data_reducedLeisure4059$respondent_leisure_rel_2019_2023)
ks.test(data_reducedLeisure1839$respondent_leisure_rel_2019_2023, data_reducedLeisure60$respondent_leisure_rel_2019_2023)
ks.test(data_reducedLeisure60$respondent_leisure_rel_2019_2023, data_reducedLeisure4059$respondent_leisure_rel_2019_2023)


# DIFFERENCES BETWEEN GENDERS ----------

#Work

data_reducedWorkFemale <- data_reducedWork %>% filter(gender == "Weiblich")
data_reducedWorkMale <- data_reducedWork %>% filter(gender == "Männlich")

ks.test(data_reducedWorkMale$respondent_work_rel_2019_2020, data_reducedWorkFemale$respondent_work_rel_2019_2020)

#Summer 2021
ks.test(data_reducedWorkMale$respondent_work_rel_2019_2021, data_reducedWorkFemale$respondent_work_rel_2019_2021)

#01/2023
ks.test(data_reducedWorkMale$respondent_work_rel_2019_2023, data_reducedWorkFemale$respondent_work_rel_2019_2023)

#Leisure

data_reducedLeisureFemale <- data_reducedLeisure %>% filter(gender == "Weiblich")
data_reducedLeisureMale <- data_reducedLeisure %>% filter(gender == "Männlich")

ks.test(data_reducedLeisureMale$respondent_leisure_rel_2019_2020, data_reducedLeisureFemale$respondent_leisure_rel_2019_2020)

#Summer 2021
ks.test(data_reducedLeisureMale$respondent_leisure_rel_2019_2021, data_reducedLeisureFemale$respondent_leisure_rel_2019_2021)

#01/2023
ks.test(data_reducedLeisureMale$respondent_leisure_rel_2019_2023, data_reducedLeisureFemale$respondent_leisure_rel_2019_2023)

# Difference Number of Infections -----------------------------------------

data_reduced <- data_reduced %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "0",
                                                                     num_c19_infs == "Einmal" ~ "1",
                                                                     num_c19_infs == "Zweimal" ~ "2",
                                                                     num_c19_infs == "Dreimal" ~ "3",
                                                                     num_c19_infs == "Mehr als dreimal" ~ "3",
                                                                     num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

data_reduced$num_c19_infs_eng <- factor(data_reduced$num_c19_infs_eng, levels = c("0", "1", "2", "3", "I Don't Want To Answer"))

data_reducedFemale <- data_reduced %>% filter(gender == "female") %>% filter(num_c19_infs_eng != "I Don't Want To Answer") %>% mutate(num_c19_infs_eng = as.numeric(num_c19_infs_eng)-1)
data_reducedMale <- data_reduced %>% filter(gender == "male") %>% filter(num_c19_infs_eng != "I Don't Want To Answer") %>% mutate(num_c19_infs_eng = as.integer(num_c19_infs_eng)-1)
ks.test(data_reducedFemale$num_c19_infs_eng, data_reducedMale$num_c19_infs_eng)

ecdf_compFemale <- data_reducedFemale %>% count(num_c19_infs_eng) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
  mutate(ecdf = cum/sum) %>%
  mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(uci = case_when(uci > 1 ~ 1, .default = uci))

ecdf_compMale <- data_reducedMale %>% count(num_c19_infs_eng) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
  mutate(ecdf = cum/sum) %>%
  mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(uci = case_when(uci > 1 ~ 1, .default = uci))

ggplot() +
  geom_ribbon(data = ecdf_compFemale, aes(ymin = lci, ymax = uci, x = num_c19_infs_eng, fill = "Female"), alpha = 0.3)+
  geom_line(data = ecdf_compFemale, aes(y=ecdf, x = num_c19_infs_eng, color = "Female"), size = 1) +
  geom_ribbon(data = ecdf_compMale, aes(ymin = lci, ymax = uci, x = num_c19_infs_eng, fill = "Male"), alpha = 0.3)+
  geom_line(data = ecdf_compMale, aes(y=ecdf, x = num_c19_infs_eng, color = "Male"), size = 1) +
  xlab("Number of Infections") +
  ylab("ECDF") +
  xlim(0,4) +
  my_theme()  +
  theme(legend.position = "bottom") +
  scale_color_manual(values=c("Male" = "#DC0000FF", "Female" = "#3C5488FF")) +
  scale_fill_manual(values=c("Male" = "#DC0000FF", "Female" = "#3C5488FF"),
                    guide = "none")

# Difference date of first infection --------------------------------------

data_reduced <- data_reduced %>% mutate(date_f1_inf = case_when(is.na(date_f1_inf) ~ as.Date("3000-01-01"),
                                                                .default = as.Date(as.character(date_f1_inf)))) %>%
  filter(date_f1_inf != as.Date("1922-03-01")) %>%
  filter(date_f1_inf != as.Date("1965-06-12")) %>%
  filter(date_f1_inf != as.Date("2000-12-13")) %>%
  filter(date_f1_inf != as.Date("2019-12-21")) 

ecdf_comp <- data_reduced %>% filter(!is.na(gender)) %>% group_by(gender) %>% 
  count(date_f1_inf) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
  mutate(ecdf = cum/sum) %>% 
  mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(uci = case_when(uci > 1 ~ 1, .default = uci)) %>% ungroup()
ecdf_comp$date_f1_inf <- as.numeric(as.Date(ecdf_comp$date_f1_inf))

ecdf_compFemale <- ecdf_comp %>% filter(gender == "female")
ecdf_compMale <- ecdf_comp %>% filter(gender == "male")

ggplot() +
  geom_ribbon(data = ecdf_compFemale, aes(ymin = lci, ymax = uci, x = as.Date(date_f1_inf), fill = "Female"), alpha = 0.3)+
  geom_line(data = ecdf_compFemale, aes(y=ecdf, x = as.Date(date_f1_inf), color = "Female"), size = 1) +
  geom_ribbon(data = ecdf_compMale, aes(ymin = lci, ymax = uci, x = as.Date(date_f1_inf), fill = "Male"), alpha = 0.3)+
  geom_line(data = ecdf_compMale, aes(y=ecdf, x = as.Date(date_f1_inf), color = "Male"), size = 1) +
  xlab("Timing of First Infection") +
  ylab("ECDF") +
  xlim(as.Date(18322),as.Date(19539)) +
  my_theme()  +
  theme(legend.position = "bottom") +
  scale_color_manual(values=c("Male" = "#DC0000FF", "Female" = "#3C5488FF")) +
  scale_fill_manual(values=c("Male" = "#DC0000FF", "Female" = "#3C5488FF"),
                    guide = "none")

ks.test(ecdf_compFemale$date_f1_inf, ecdf_compMale$date_f1_inf)
