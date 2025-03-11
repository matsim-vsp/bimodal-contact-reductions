library(tidyverse)
library(MMWRweek)
library(see)
library(RColorBrewer)
library(patchwork)
library(ggpubr)
library(ggh4x)
library(smplot2)
library(scales)
library(ggbreak)

# Author: S. Paltra, contact: paltra@tu-berlin.de

#In the first part of this script we compare the incidence of the respondents of the survey to the official incidence reported by RKI

here()
source("./AnalysisSP/SecondOrderContactsPaper/DataCleaningPrepForContactAnalysis.R")

## RISK-PERCEPTION SCORE

data_reduced <- data_reduced %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "0",
                                                                    num_c19_infs == "Einmal" ~ "1",
                                                                    num_c19_infs == "Zweimal" ~ "2",
                                                                    num_c19_infs == "Dreimal" ~ "3",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "3+",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

data_reduced$num_c19_infs_eng <- factor(data_reduced$num_c19_infs_eng, levels = c("0", "1", "2", "3+", "I Don't Want To Answer"))


palette <- function() {
  c("#3C5488FF", "#DC0000FF")
}

palette2 <- function() {
  c("#2C3E65FF", "#A90000FF")
}


group_sizes <- data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% 
    filter(!is.na(RiskyCarefulAtt )) %>% 
    filter(!is.na(TypeOfContact)) %>% 
    filter(TypeOfContact %in% c("Leisure")) %>%
    filter(value > -150) %>%  filter(value < 100) %>%
    filter(!is.na(TypeOfContact)) %>% group_by(RiskyCarefulAtt, TypeOfContact, time) %>%
    mutate(group = case_when(value < -75 ~ "CutAll", value < -25 ~ "Middle", .default = "CutNone"))

group_sizes <- group_sizes %>% group_by(TypeOfContact, RiskyCarefulAtt, time) %>% count(group)
group_sizes <- group_sizes %>% mutate(share = n/sum(n))

data_reduced_tidy_rel$RiskyCarefulAtt <- factor(data_reduced_tidy_rel$RiskyCarefulAtt, levels = c("Risk-averse", "Risk-tolerant"))

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% mutate(time = case_when(time == "Summer 2021" ~ "Summer\n2021", .default = time))
data_reduced_tidy_rel$time <- factor(data_reduced_tidy_rel$time, levels = c("03/2020", "Summer\n2021", "01/2023"))

p1_work <- ggplot(data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% 
    filter(!is.na(RiskyCarefulAtt)) %>% 
    filter(!is.na(TypeOfContact)) %>% 
    filter(TypeOfContact %in% c("Work")) %>%
    filter(value > -150) %>%  filter(value < 100) %>%
    filter(!is.na(TypeOfContact)) %>% group_by(RiskyCarefulAtt, TypeOfContact, time), aes(RiskyCarefulAtt, value, color = RiskyCarefulAtt, fill = RiskyCarefulAtt)) +
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
  facet_grid(~(time), switch="both")+
  ggtitle("Work") +
  theme_minimal() +
  theme(panel.spacing = unit(4, "lines")) +
  ylab("Change of No. of \n Contacts (in percent)") +
  my_theme() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust=0.5)) +
  theme(axis.ticks.x = element_line(size = 0)) +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(p1_work, p1_leisure, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")

ggsave("CollectionViolinplots_AttCarefulnessScore.pdf",  dpi = 500, w = 24, h = 9)
ggsave("CollectionViolinplots_AttCarefulnessScore.png", dpi = 500, w = 24, h = 9)

data_reduced_tidy %>% filter(WhoseContacts == "Respondent") %>% 
    filter(!is.na(RiskyCarefulAtt)) %>% 
    filter(!is.na(TypeOfContact)) %>% filter(time ==  "2019") %>% 
    filter(!is.na(value)) %>% 
    filter(value < 500) %>%
    filter(TypeOfContact %in% c("Work", "Leisure"))  %>%  
    group_by(RiskyCarefulAtt, TypeOfContact) %>% summarise(median = median(value), twentyfive = quantile(value, 0.25), seventyfive = quantile(value, 0.75))

p4 <- ggplot(data_reduced_tidy %>% filter(WhoseContacts == "Respondent") %>% 
    filter(!is.na(RiskyCarefulAtt)) %>% 
    filter(!is.na(TypeOfContact)) %>% filter(time ==  "2019") %>% 
    filter(!is.na(value)) %>% 
    filter(value < 500) %>%
    filter(TypeOfContact %in% c("Work", "Leisure"))  %>%  
    group_by(RiskyCarefulAtt, TypeOfContact), aes(RiskyCarefulAtt, value, fill = RiskyCarefulAtt, color = RiskyCarefulAtt)) +
        sm_raincloud(aes(stat = median_cl), boxplot.params =  list(alpha = 0.0, width = 0.0, notch = TRUE), 
              violin.params = list(width = 1.3), point.params = list(size = 3, shape = 21, alpha = 0.4, position = sdamr::position_jitternudge(
        nudge.x = -0.1,
        jitter.width = 0.1, jitter.height = 0.01      
      )), 
              shape = 21, sep_level = 2)  +
  # geom_violin(aes(fill = RiskyCarefulAtt, color = RiskyCarefulAtt), scale = "area", trim = TRUE) +  
  # stat_summary(aes(color=RiskyCarefulAtt ), fun.data=mean_sdl, fun.args = list(mult=1), 
  #                geom="pointrange", linewidth = 1) +
  # stat_compare_means(comparisons = my_comparisons, method = "t.test", symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("**** (p < 0.0001)", "*** (p < 0.001)", "** (p < 0.01)", "* (p < 0.05)", "not significant (p > 0.05)")), size = 6, bracket.size = 1, tip.length = 0.01, vjust = 0, label.y.npc = 0)+
  #geom_violin(aes(color=WhoseContacts), size = 1.3) +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
  facet_wrap(vars(TypeOfContact)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,1,3,10,30,100,300)) +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines")) +
  ylab("Number of\nContacts (2019)") +
  my_theme() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank())
  #guides(fill=guide_legend(nrow=2,byrow=TRUE))

ggsave("ViolinplotsPrePandemic_AttCarefulnessScore.pdf", p4, dpi = 500, w = 9, h = 9)
ggsave("ViolinplotsPrePandemic_AttCarefulnessScore.png", p4, dpi = 500, w = 7.5, h = 9)


mean <- data_reduced_tidy %>% filter(WhoseContacts == "Respondent") %>% 
    filter(!is.na(TypeOfContact)) %>% 
    filter(TypeOfContact %in% c("Leisure", "Work")) %>%
    filter(value < 500) %>%  
    filter(!is.na(TypeOfContact)) %>% group_by(TypeOfContact, RiskyCarefulAtt, time) %>% summarise(mean =mean(value), sd = sd(value))



## Are respondents sociable across categories?

options(scipen = 999)

 ggplot(data_reduced %>% mutate(respondent_work_2019 = case_when(respondent_work_2019 == 0 ~ 0.01,
                                         .default = respondent_work_2019)) %>%
                        mutate(respondent_leisure_2019 = case_when(respondent_leisure_2019 == 0 ~ 0.01,
                                         .default = respondent_leisure_2019)) %>%
        filter(!is.na(RiskyCarefulAtt)) %>%
        filter(respondent_work_2019 > -1000) %>%
        filter(respondent_work_2019 < 100) %>%
        filter(respondent_leisure_2019 > -1000) %>%
        filter(respondent_leisure_2019 <= 50)) +
#geom_smooth(aes(x=respondent_work_2019, y = respondent_leisure_2019, color = RiskyCarefulAtt, size = RiskyCarefulAtt), method = 'lm', alpha = 0.7, se = FALSE) +
#geom_jitter(aes(x=respondent_work_2019, y = respondent_leisure_2019, color = RiskyCarefulAtt, size = RiskyCarefulAtt), alpha = 0.7) +
geom_smooth(aes(x=(respondent_work_2019 - min(respondent_work_2019)) / (max(respondent_work_2019)-min(respondent_work_2019)),
 y = (respondent_leisure_2019 - min(respondent_leisure_2019)) / (max(respondent_leisure_2019)-min(respondent_leisure_2019)), color = RiskyCarefulAtt, size = RiskyCarefulAtt), method = 'lm', se = FALSE) +
geom_jitter(aes(x=(respondent_work_2019 - min(respondent_work_2019)) / (max(respondent_work_2019)-min(respondent_work_2019)),
 y = (respondent_leisure_2019 - min(respondent_leisure_2019)) / (max(respondent_leisure_2019)-min(respondent_leisure_2019)), color = RiskyCarefulAtt, size = RiskyCarefulAtt), alpha = 0.7) +
theme_minimal() + 
scale_color_manual(values = palette()) +
scale_size_manual(values=c(7,3))+
theme(legend.position = "bottom", legend.title = element_blank()) +
#scale_y_log10() +
#scale_x_log10() +
#scale_x_continuous(limits = c(0,100), breaks=c(0,20,40,60,80)) +
#scale_y_continuous(limits = c(0,100), breaks=c(0,20,40,60,80)) +
xlab("# Work Contacts (2019)") +
ylab("# Leisure Contacts (2019)") +
theme(text = element_text(size = 30))

ggsave("WorkvsLeisureContacts_AttitudeScore.pdf", dpi = 500, w = 9, h = 9)
ggsave("WorkvsLeisureContacts_AttitudeScore.png", dpi = 500, w = 9, h = 9)

options(scipen = 999)


risktolerant <- data_reduced %>% filter(RiskyCarefulAtt == "Risk-tolerant")
cor(risktolerant$respondent_work_2019, risktolerant$respondent_leisure_2019)
riskaverse <- data_reduced %>% filter(RiskyCarefulAtt == "Risk-averse")
cor(riskaverse$respondent_work_2019, riskaverse$respondent_leisure_2019)

p5 <- ggplot(data_reduced %>% mutate(respondent_work_2019 = case_when(respondent_work_2019 == 0 ~ 0.1,
                                         .default = respondent_work_2019)) %>%
                        mutate(respondent_leisure_2019 = case_when(respondent_leisure_2019 == 0 ~ 0.1,
                                         .default = respondent_leisure_2019)) %>% 
        filter(respondent_work_2019 > -1) %>% 
        filter(respondent_leisure_2019 > -1) %>%
        filter(respondent_work_2019 < 300) %>% filter(respondent_leisure_2019 < 500) %>%
        filter(!is.na(RiskyCarefulAtt))) +
geom_jitter(aes(x = respondent_work_2019, y = respondent_leisure_2019, color = RiskyCarefulAtt,  alpha = 0.8), size = 3) +
#geom_jitter(aes(x=(respondent_work_2019 - min(respondent_work_2019)) / (max(respondent_work_2019)-min(respondent_work_2019)),
theme_minimal() + 
scale_color_manual(values = palette()) +
scale_size_manual(values=c(7,3))+
theme(legend.position = "bottom", legend.title = element_blank()) +
scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,1,3,10,30,100)) +
scale_x_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,1,3,10,30,100)) +
xlab("Number of Work Contacts(2019)") +
ylab("Number of Leisure Contacts (2019)") +
my_theme() +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("WorkvsLeisureContacts_AttitudeScore.pdf", p5, dpi = 500, w = 9, h = 9)
ggsave("WorkvsLeisureContacts_AttitudeScore.png", p5, dpi = 500, w = 9, h = 9)

ggarrange(p4, p5, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 34), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")

ggsave("WorkvsLeisureViolin_AttitudeScore2019.pdf", dpi = 500, w = 21.5, h = 12.25) 
ggsave("WorkvsLeisureViolin_AttitudeScore2019.png", dpi = 500, w = 21.5, h = 12.25) 


## 2019 Contacts vs Reduction [Percentage]

timepointOfComparison <- c("respondent_work_rel_2019_2020", "respondent_work_rel_2019_2021", "respondent_work_rel_2019_2023",
                          "respondent_leisure_rel_2019_2020", "respondent_leisure_rel_2019_2021", "respondent_leisure_rel_2019_2023")

for (timepoint in timepointOfComparison){

  if (timepoint == "respondent_work_rel_2019_2020"){
    context <- "Work"
    time <- "2020"
    context2019 <- "respondent_work_2019"
  } else if (timepoint == "respondent_work_rel_2019_2021"){
    context <- "Work"
    time <- "2021"
    context2019 <- "respondent_work_2019"
  } else if (timepoint == "respondent_work_rel_2019_2023"){
    context <- "Work"
    time <- "2023"
    context2019 <- "respondent_work_2019"
  } else if(timepoint == "respondent_leisure_rel_2019_2020"){
    context <- "Leisure"
    time <- "2020"
    context2019 <- "respondent_leisure_2019" 
  } else if(timepoint == "respondent_leisure_rel_2019_2021"){
    context <- "Leisure"
    time <- "2021"
    context2019 <- "respondent_leisure_2019"
  } else if(timepoint == "respondent_leisure_rel_2019_2023"){
    context <- "Leisure"
    time <- "2023"
    context2019 <- "respondent_leisure_2019"
  }

  ggplot(data_reduced %>% filter(!is.na(RiskyCarefulAtt)) %>%
          filter(!is.na(RiskyCarefulAtt)) %>%
          filter(!!sym(context2019) < 100) %>%
          filter(!!sym(context2019) > -10) %>%
          filter(!!sym(timepoint) > -25) %>%
          filter(!!sym(timepoint) < 150)) +
  geom_jitter(aes(x=!!sym(context2019), y = !!sym(timepoint), color = RiskyCarefulAtt, size = RiskyCarefulAtt), alpha = 0.7) +
  theme_minimal() + 
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(-2, 100)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0, 20, 40, 60, 80, 100), limits = c(-2, 100)) +
  scale_color_manual(values = palette()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  #scale_y_log10() +
  #scale_x_log10() +
  xlab(paste0("# ", context , "Contacts (2019)")) +
  scale_size_manual(values=c(8,3)) +
  ylab(paste0(time, " Reduction Of ", context, " Contacts [Percentage]")) +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
          axis.ticks.y = element_line(),
          axis.ticks.length = unit(5, "pt"))

  ggsave(paste0(context, "2019vsReduction", time, ".png"), dpi = 500, w = 12, h = 12)
  ggsave(paste0(context, "2019vsReduction", time, ".pdf"), dpi = 500, w = 12, h = 12)
}

#2019 Contacts vs Reduction [Absolute]

data_reduced <- data_reduced %>% mutate(
        respondent_work_absred_2019_2020 = respondent_work_2019 - respondent_work_03_2020,
        respondent_work_absred_2019_2021 = respondent_work_2019 - respondent_work_summer_2021,
        respondent_work_absred_2019_2023 = respondent_work_2019 - respondent_work_01_2023,
        respondent_leisure_absred_2019_2020 = respondent_leisure_2019 - respondent_leisure_03_2020,
        respondent_leisure_absred_2019_2021 = respondent_leisure_2019 - respondent_leisure_summer_2021,
        respondent_leisure_absred_2019_2023 = respondent_leisure_2019 - respondent_leisure_01_2023
)

timepointOfComparison <- c("respondent_work_absred_2019_2020", "respondent_work_absred_2019_2021", "respondent_work_absred_2019_2023",
                            "respondent_leisure_absred_2019_2020", "respondent_leisure_absred_2019_2021", "respondent_leisure_absred_2019_2023")

for (timepoint in timepointOfComparison){

  if (timepoint == "respondent_work_absred_2019_2020"){
    context <- "Work"
    time <- "2020"
    context2019 <- "respondent_work_2019"
  } else if (timepoint == "respondent_work_absred_2019_2021"){
    context <- "Work"
    time <- "2021"
    context2019 <- "respondent_work_2019"
  } else if (timepoint == "respondent_work_absred_2019_2023"){
    context <- "Work"
    time <- "2023"
    context2019 <- "respondent_work_2019"
  } else if(timepoint == "respondent_leisure_absred_2019_2020"){
    context <- "Leisure"
    time <- "2020"
    context2019 <- "respondent_leisure_2019" 
  } else if(timepoint == "respondent_leisure_absred_2019_2021"){
    context <- "Leisure"
    time <- "2021"
    context2019 <- "respondent_leisure_2019"
  } else if(timepoint == "respondent_leisure_absred_2019_2023"){
    context <- "Leisure"
    time <- "2023"
    context2019 <- "respondent_leisure_2019"
  }

  ggplot(data_reduced %>% filter(!is.na(RiskyCarefulAtt)) %>%
          filter(!is.na(RiskyCarefulAtt)) %>%
          filter(!!sym(context2019) < 100) %>%
          filter(!!sym(context2019) > -10) %>%
          filter(!!sym(timepoint) > -25) %>%
          filter(!!sym(timepoint) < 150)) +
  geom_jitter(aes(x=!!sym(context2019), y = !!sym(timepoint), color = RiskyCarefulAtt, size = RiskyCarefulAtt), alpha = 0.7) +
  theme_minimal() + 
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(-2, 60)) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(-2, 60)) +
  scale_color_manual(values = palette()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  #scale_y_log10() +
  #scale_x_log10() +
  xlab(paste0("# ", context , "Contacts (2019)")) +
  scale_size_manual(values=c(8,3)) +
  ylab(paste0(time, " Reduction Of ", context, " Contacts [Absolute]")) +
  theme(text = element_text(size = 30)) +
  theme(axis.ticks.x = element_line(),
          axis.ticks.y = element_line(),
          axis.ticks.length = unit(5, "pt"))

  ggsave(paste0(context, "2019vsAbsReduction", time, ".png"), dpi = 500, w = 12, h = 12)
  ggsave(paste0(context, "2019vsAbsReduction", time, ".pdf"), dpi = 500, w = 12, h = 12)
}

data_reduced <- data_reduced %>% mutate(date_f1_inf = case_when(is.na(date_f1_inf) ~ as.Date("3000-01-01"),
                                        .default = as.Date(as.character(date_f1_inf)))) %>%
                                filter(date_f1_inf != as.Date("1922-03-01")) %>%
                                filter(date_f1_inf != as.Date("1965-06-12")) %>%
                                filter(date_f1_inf != as.Date("2000-12-13")) %>%
                                filter(date_f1_inf != as.Date("2019-12-21")) 

ecdf_comp <- data_reduced %>% filter(!is.na(RiskyCarefulAtt)) %>% group_by(RiskyCarefulAtt) %>% 
count(date_f1_inf) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
  mutate(ecdf = cum/sum) %>% ungroup()
ecdf_comp <- rbind(ecdf_comp[1,], ecdf_comp)
ecdf_comp[1,2] <- as.Date("2020-05-01")
ecdf_comp[1,3] <- 0
ecdf_comp[1,4] <- 0
ecdf_comp[1,6] <- 0
ecdf_comp <- ecdf_comp %>% mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(uci = case_when(uci > 1 ~ 1, .default = uci))

p2 <- ggplot(ecdf_comp, aes(date_f1_inf)) +
geom_ribbon(aes(ymin = lci, ymax = uci, x = date_f1_inf, , fill = RiskyCarefulAtt), alpha = 0.3)+
geom_line(aes(y=ecdf,  color = RiskyCarefulAtt), size = 2) +
theme_minimal() +
ylab("Empirical Cumulative \n Distribution Function") +
xlab("Date of\n1st Infection") +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01"))) +
scale_color_manual(values = palette()) +
scale_x_date(date_labels = "'%y")+
#scale_y_continuous(labels=percent) +
my_theme()


ggsave("ECDF_AttCarefulnessScore.pdf", p2, dpi = 500, w = 9, h = 9)
ggsave("ECDF_AttCarefulnessScore.png", p2, dpi = 500, w = 7.5, h = 8)


RemainingAnswers <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(RemainingAnswers) <- c("RiskyCarefulAtt", "num_c19_infs_eng", "n", "percent")
RemainingAnswers[nrow(RemainingAnswers)+1, ] <- c("Risk-tolerant", "3+", 0, 0)
RemainingAnswers$n <- as.integer(RemainingAnswers$n)
RemainingAnswers$percent <- as.integer(RemainingAnswers$percent)


p3 <- data_reduced %>% filter(num_c19_infs_eng != "I Don't Want To Answer") %>%
  filter(!is.na(RiskyCarefulAtt)) %>%
  count(RiskyCarefulAtt, num_c19_infs_eng) %>%
  mutate(percent = 100 * n / sum(n), .by = RiskyCarefulAtt) %>% rbind(RemainingAnswers) %>%
  mutate(lci = case_when(RiskyCarefulAtt == "Risk-tolerant" ~ 43*(n/43 - 1.96*(((n/43*(1-n/43))/43)^0.5)),
                          RiskyCarefulAtt == "Risk-averse" ~ 350*(n/350 - 1.96*(((n/350*(1-n/350))/350)^0.5)))) %>%#
  mutate(lci = case_when(RiskyCarefulAtt == "Risk-tolerant" ~ 100/43*lci,
                         RiskyCarefulAtt == "Risk-averse" ~ 100/350*lci)) %>%
  mutate(lci = case_when (lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = case_when(RiskyCarefulAtt == "Risk-tolerant" ~ 43*(n/43 + 1.96*(((n/43*(1-n/43))/43)^0.5)),
                          RiskyCarefulAtt == "Risk-averse" ~ 350*(n/350 + 1.96*(((n/350*(1-n/350))/350)^0.5)))) %>%
  mutate(uci = case_when(RiskyCarefulAtt == "Risk-tolerant" ~ 100/43*uci,
                          RiskyCarefulAtt == "Risk-averse" ~ 100/350*uci)) %>%
  ggplot(aes(num_c19_infs_eng, percent/100, fill = RiskyCarefulAtt)) +
  #geom_col(position = position_dodge(preserve = 'single')) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=num_c19_infs_eng, ymin=lci/100, ymax=uci/100, color = RiskyCarefulAtt), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=2.5) +
  theme_minimal() +
  ylab("Share\n(percent)") +
  xlab("Number of Infections") +
  scale_y_continuous(breaks = c(0,0.25, 0.50)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  #labs(fill="Behavioral Group") +
  #theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
    my_theme()

ggsave("NoInfections_AttCarefulnessScore.pdf", p3, dpi = 500, w = 9, h = 9)
ggsave("NoInfections_AttCarefulnessScore.png", p3, dpi = 500, w = 9, h = 9)

ggarrange(p3, p2, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")

ggsave("NoInfectionsECDF_AttCarefulnessScore.pdf", dpi = 500, w = 21, h = 12) 
ggsave("NoInfectionsECDF_AttCarefulnessScore.png", dpi = 500, w = 21, h = 12) 



patch <- (p3/p2) +  plot_annotation(tag_levels = "A") 
p4 <- p1  + plot_spacer() + patch + plot_layout(widths = c(13, 0.5, 4.5)) +  plot_annotation(tag_levels = "A") 
ggsave("BoxplotNoInfectionsECDF_attitudeScore.pdf", p4, dpi = 500, w = 18, h = 14)
ggsave("BoxplotNoInfectionsECDF_attitudeScore.png", p4, dpi = 500, w = 18, h = 14)


# Did careful attitudes/behaviors result in less/later infections? --------

attitudesAndBehaviors <- c("attitudes_precautions_mar2020_low_infection_risk_perception",                
                                    "attitudes_precautions_mar2020_risky_infection_course_assessment",            
                                    "attitudes_precautions_mar2020_high_risk_perception",                         
                                    "attitudes_precautions_mar2020_avoided_risky_situations",                     
                                    "attitudes_precautions_mar2020_aware_distance_rule_effectiveness",         
                                    "attitudes_precautions_mar2020_understood_mask_reduces_risk",                
                                    "attitudes_precautions_mar2020_followed_measures",                         
                                    "attitudes_precautions_mar2020_felt_restricted_by_measures",                  
                                    "attitudes_precautions_mar2020_wore_ffp2_ffp3_over_medical",
                                    "beh_change_start_pandemic_avoid_in_person",                                  
                                    "beh_change_start_pandemic_avoid_careless_contacts",                       
                                    "beh_change_start_pandemic_contact_cautious_people",                        
                                    "beh_change_start_pandemic_avoid_peak_hours",                          
                                    "beh_change_start_pandemic_maintain_distance",                                
                                    "beh_change_start_pandemic_outdoor_only",                                     
                                    "beh_change_start_pandemic_no_visit_high_risk",                               
                                    "beh_change_start_pandemic_avoid_busy_places",                               
                                    "beh_change_start_pandemic_avoid_public_trans",                               
                                    "beh_change_start_pandemic_mask_public_trans",                                
                                    "beh_change_start_pandemic_mask_supermarket",                                 
                                    "beh_change_start_pandemic_work_from_home",                                  
                                    "beh_change_start_pandemic_children_limited_contacts",                       
                                    "beh_change_start_pandemic_meet_close_despite_restrict")

data_reduced <- data_reduced %>% select(-contains("cond")) %>% 
                select(-respondent_hsld_size_persons_under_14) %>% 
                select(-number_of_children_under_18)

data_reduced$respondent_cc_change <- factor(data_reduced$respondent_cc_change, levels = c("Nein", "Ja"))

for(attBeh in attitudesAndBehaviors){
data_reduced %>% filter(!is.na(cc_change_during_pandemic)) %>% filter(!!sym(attBeh) != "trifft nicht zu") %>%
filter(!!sym(attBeh) != "keine Angabe") %>%
  count(!!sym(attBeh), cc_change_during_pandemic) %>%
  mutate(n = n / sum(n), .by = !!sym(attBeh)) %>% ungroup() %>%
  filter(!is.na(!!sym(attBeh))) %>% 
  ggplot(aes(!!sym(attBeh), n, fill = cc_change_during_pandemic)) +
  #geom_col(position = position_dodge(preserve = 'single')) +
  geom_col(position = "fill") +
  ggtitle(attBeh) +
  theme_minimal() +
  ylab("Relative Frequency") +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = palette()) +
  #theme(legend.title=element_blank()) +
  theme(panel.spacing.x = unit(2, "lines")) +
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) 
  facet_wrap(~ cc_change_during_pandemic)

ggsave(paste0(attBeh, "_changeCC-fullResponses.pdf"), dpi = 500, w = 12, h = 9)
ggsave(paste0(attBeh, "_changeCC-fullResponses.png"), dpi = 500, w = 12, h = 9)
}
#data_reduced <- data_reduced %>% mutate(cc_change_during_pandemic = case_when(cc_change_during_pandemic == "Ja" ~ "Changed CC",
                                                                             #   cc_change_during_pandemic == "Nein" ~ "Did Not Change CC"))

for(attBeh in attitudesAndBehaviors){
data_reduced %>% filter(!is.na(cc_change_during_pandemic)) %>% filter(!!sym(attBeh) != "trifft nicht zu") %>%
filter(!!sym(attBeh) != "keine Angabe") %>%
  count(!!sym(attBeh) , cc_change_during_pandemic) %>%
  mutate(n = n / sum(n), .by = !!sym(attBeh)) %>%
  filter(!is.na(!!sym(attBeh))) %>% 
  ggplot(aes(!!sym(attBeh), n, fill = cc_change_during_pandemic)) +
  #geom_col(position = position_dodge(preserve = 'single')) +
  geom_col(position = "fill") +
  theme_minimal() +
  ggtitle(attBeh) +
  #facet_wrap(~cc_change_during_pandemic)
  theme(panel.spacing.x = unit(2, "lines")) +
  ylab("Relative Frequency") +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = palette()) +
  #theme(legend.title=element_blank()) +
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1))

ggsave(paste0(attBeh, "_changeCC.pdf"), dpi = 500, w = 9, h = 9)
ggsave(paste0(attBeh, "_changeCC.png"), dpi = 500, w = 9, h = 9)
}

palette <- function() {
  c("#008083", "#FFBC42", "#fd5901")
}

for(attBeh in attitudesAndBehaviors){
ggplot(data_reduced %>% filter(!is.na(!!sym(attBeh))), aes(date_f1_inf, color = !!sym(attBeh))) +
stat_ecdf(geom="smooth", size = 2) +
theme_minimal() +
ggtitle(attBeh) +
ylab("Empirical Cumulative \n Distribution Function") +
xlab("Date Of 1st Infection") +
scale_color_manual(values = palette()) +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01"))) +
theme(text = element_text(size = 22)) +
theme(legend.title = element_blank(), legend.position = "bottom") +
guides(color = guide_legend(nrow=2))
ggsave(paste0(attBeh, "ECDF.pdf"), dpi = 500, w = 9, h = 9)
ggsave(paste0(attBeh, "ECDF.png"), dpi = 500, w = 9, h = 9)

data_reduced %>%
  count(!!sym(attBeh), num_c19_infs_eng) %>%
  mutate(n = n / sum(n), .by = !!sym(attBeh)) %>%
  filter(!is.na(!!sym(attBeh))) %>% 
  ggplot(aes(num_c19_infs_eng, n, fill = !!sym(attBeh))) +
  geom_col(position = position_dodge(preserve = 'single')) +
  theme_minimal() +
  ggtitle(attBeh) +
  ylab("Relative Frequency") +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom") +
  labs(fill="Attitude/Beh. Change") +
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette())

ggsave(paste0(attBeh, "NumberOfInfections.pdf"), dpi = 500, w = 9, h = 12)
ggsave(paste0(attBeh, "NumberOfInfection.png"), dpi = 500, w = 9, h = 12)
}

cor(data_reduced$attitudeScore, data_reduced$behaviorChangeScore, use ="pairwise.complete.obs")

ggplot(data_reduced) +
geom_point(aes(x = attitudeScore, y =behaviorChangeScore))

ggplot(data_reduced) + 
geom_histogram(aes(attitudeScore))

data_reduced %>% count(attitudeScore)
data_reduced %>% count(behaviorChangeScore)

data_reduced$attitudeScore <- factor(data_reduced$attitudeScore)

data_reduced <- data_reduced %>% mutate(RiskyCarefulAtt = case_when(attitudeScore %in% c(-9,-8,-7,-6,-5,-4,-3,-2,-1, 0,1,2,3) ~ "Risk-tolerant",
                                                                attitudeScore %in% c(4,5,6,7,8,9) ~ "Risk-averse")) %>%
                                mutate(RiskyCarefulBeh = case_when(behaviorChangeScore %in% c(-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1) ~ "Risky",
                                                                    behaviorChangeScore %in% c(14,13,12,11,10,9,8,7,6,5,4,3,2,1) ~ "Risk-averse"))

data_reduced$RiskyCarefulAtt <- factor(data_reduced$RiskyCarefulAtt, levels = c("Risk-tolerant", "Risk-averse"))
data_reduced$RiskyCarefulBeh <- factor(data_reduced$RiskyCarefulBeh, levels = c("Risk-tolerant", "Risk-averse"))




data_reduced %>% filter(!is.na(cc_change_during_pandemic)) %>% filter(behaviorChangeScore != "trifft nicht zu") %>%
filter(behaviorChangeScore != "keine Angabe") %>%
  count(behaviorChangeScore , cc_change_during_pandemic) %>%
  mutate(n = n / sum(n), .by = behaviorChangeScore) %>%
  filter(!is.na(behaviorChangeScore)) %>% 
  ggplot(aes(as.factor(behaviorChangeScore), n, fill = cc_change_during_pandemic)) +
  geom_col(position = "fill") +
  theme_minimal() +
  ylab("Relative Frequency") +
  xlab("Carefulness Score") +
  theme(text = element_text(size = 25)) +
  theme(legend.position = "bottom") + 
  theme(panel.spacing.x = unit(2, "lines")) +
  scale_fill_manual(values = palette()) +
  theme(legend.title=element_blank())

ggsave("WillingnessChangeCC_behScore.png", dpi = 500, w = 13, h =9)
ggsave("WillingnessChangeCC_behScore.pdf", dpi = 500, w = 13, h =9)