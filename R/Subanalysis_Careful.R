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
library(here)

# Author: S. Paltra, contact: paltra@tu-berlin.de

#In the first part of this script we compare the incidence of the respondents of the survey to the official incidence reported by RKI

here()
<<<<<<< HEAD
source("/Users/sydney/git/second-order-contacts/R/DataCleaningPrepForContactAnalysis.R")
=======
source("./git/second-order-contacts/R/DataCleaningPrepForContactAnalysis.R")
>>>>>>> refs/remotes/origin/main

## RISK-PERCEPTION SCORE

data_reduced <- data_reduced %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "0",
                                                                    num_c19_infs == "Einmal" ~ "1",
                                                                    num_c19_infs == "Zweimal" ~ "2",
                                                                    num_c19_infs == "Dreimal" ~ "3",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "3+",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

data_reduced$num_c19_infs_eng <- factor(data_reduced$num_c19_infs_eng, levels = c("0", "1", "2", "3+", "I Don't Want To Answer"))


palette <- function() {
  c("#DC0000FF", "#3C5488FF")
}

palette2 <- function() {
  c("#A90000FF", "#2C3E65FF")
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

data_reduced_tidy_rel$combined = interaction(data_reduced_tidy_rel$RiskyCarefulAtt, data_reduced_tidy_rel$time)
combined_levels <- levels(interaction(data_reduced_tidy_rel$RiskyCarefulAtt, data_reduced_tidy_rel$time))
A_values <- data_reduced_tidy_rel$time[match(combined_levels, data_reduced_tidy_rel$combined)]

unique_A_values <- unique(A_values)
unique_positions <- sapply(unique_A_values, function(a) {
  # For each unique A value, find the first position where it appears
  which(A_values == a)[1]
})

p1_work <- ggplot(data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% 
    filter(!is.na(RiskyCarefulAtt)) %>% 
    filter(!is.na(TypeOfContact)) %>% 
    filter(TypeOfContact %in% c("Work")) %>%
    filter(value > -150) %>%  filter(value < 100) %>%
    filter(!is.na(TypeOfContact)), aes(combined, value, color = RiskyCarefulAtt, fill = RiskyCarefulAtt)) +
    sm_raincloud(aes(stat = median_cl), 
    point.params = list(size = 3, shape = 21, alpha = 0.4, position = sdamr::position_jitternudge(
        nudge.x = -0.12,
        jitter.width = 0.1, jitter.height = 0.01      
      )), 
    boxplot.params =  list(alpha = 0.0, width = 0.0, notch = TRUE), 
              violin.params = list(width = 1.4, scale = "area"),
              shape = 21, sep_level = 2)  +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-100, -50, 0,50, 100), limits = c(-100,100)) +
  #ylim(c(-100,100)) +
  #facet_grid(~(time), switch="both")+
  ggtitle("Work") +
  theme_minimal() +
  #theme(panel.spacing = unit(4, "lines")) +
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

my_comparisons <- list( c("Risk-averse.03/2020", "Risk-tolerant.03/2020"), 
                        c("Risk-averse.Summer\n2021", "Risk-tolerant.Summer\n2021"),
                        c("Risk-averse.01/2023", "Risk-tolerant.01/2023"))
p1_leisureb <- p1_leisure +  
  stat_compare_means(comparisons = my_comparisons, label.y = c(80), symnum.args = list(cutpoints = c(0, 0.01, 0.05, 0.1, Inf), symbols = c("***", "**", "*", "ns")), bracket.size=1, size = 10)

my_comparisons <- list( c("Risk-averse.03/2020", "Risk-tolerant.03/2020"))
p1_workb <- p1_work +  
  stat_compare_means(comparisons = my_comparisons, label.y = c(80), symnum.args = list(cutpoints = c(0, 0.01, 0.05, 0.1, Inf), symbols = c("***", "**", "*", "ns")), bracket.size=1, size = 10)


ggarrange(p1_work, p1_leisure, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")

ggsave("CollectionViolinplots_AttCarefulnessScoreLeisure.png",  p1_leisureb, dpi = 500, w = 12, h = 9)
ggsave("CollectionViolinplots_AttCarefulnessScoreLeisure.pdf", p1_leisureb, dpi = 500, w = 12, h = 9)
ggsave("CollectionViolinplots_AttCarefulnessScoreWork.png",  p1_workb, dpi = 500, w = 12, h = 9)
ggsave("CollectionViolinplots_AttCarefulnessScoreWork.pdf", p1_workb, dpi = 500, w = 12, h = 9)


# Pre-Pandemic Contacts ---------------------------------------------------

data_reduced_tidy %>% filter(WhoseContacts == "Respondent") %>% 
    filter(!is.na(RiskyCarefulAtt)) %>% 
    filter(!is.na(TypeOfContact)) %>% filter(time ==  "2019") %>% 
    filter(!is.na(value)) %>% 
    filter(value < 500) %>%
    filter(TypeOfContact %in% c("Work", "Leisure"))  %>%  
    group_by(RiskyCarefulAtt, TypeOfContact) %>% summarise(mean = mean(value), median = median(value), twentyfive = quantile(value, 0.25), seventyfive = quantile(value, 0.75))

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
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank())
  #guides(fill=guide_legend(nrow=2,byrow=TRUE))

ggsave("WorkvsLeisureViolin_AttitudeScore2019.pdf", p4, dpi = 500, w = 9, h = 9)
ggsave("WorkvsLeisureViolin_AttitudeScore2019.png", p4, dpi = 500, w = 9, h = 9)

data2019work <- data_reduced_tidy %>% filter(TypeOfContact == "Work") %>% 
  filter(time %in% c("2019")) %>%
  filter(value < 500) %>%
  filter(WhoseContacts=="Respondent") %>% filter(RiskyCarefulAtt == "Risk-tolerant")
round(quantile(data2019work$value, na.rm=TRUE))

data2019leisure <- data_reduced_tidy %>% filter(TypeOfContact == "Leisure") %>% 
  filter(time %in% c("2019")) %>%
  filter(value < 500) %>%
  filter(WhoseContacts=="Respondent") %>% filter(RiskyCarefulAtt == "Risk-averse")
round(quantile(data2019leisure$value, na.rm=TRUE))

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

palette <- function() {
  c("#DC0000FF", "#3C5488FF")
}


p5 <- ggplot(data_reduced %>% 
               #mutate(respondent_work_2019 = case_when(respondent_work_2019 == 0 ~ 0.1,
                #                         .default = respondent_work_2019)) %>%
                 #       mutate(respondent_leisure_2019 = case_when(respondent_leisure_2019 == 0 ~ 0.1,
                  #                       .default = respondent_leisure_2019)) %>% 
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
xlab("Weekly Work Contacts(2019)") +
ylab("Weekly Leisure Contacts (2019)") +
my_theme() +
  theme(legend.position = "bottom", legend.title = element_blank())

data_reduced_forCorrelation <- data_reduced %>% filter(respondent_work_2019 > -1) %>% 
  filter(respondent_leisure_2019 > -1) %>%
  filter(respondent_work_2019 < 300) %>% filter(respondent_leisure_2019 < 500) %>%
  filter(!is.na(RiskyCarefulAtt)) %>%
  filter(RiskyCarefulAtt == "Risk-tolerant")

cor(data_reduced_forCorrelation$respondent_leisure_2019, data_reduced_forCorrelation$respondent_work_2019)

#ggsave("WorkvsLeisureContacts_AttitudeScore.pdf", p5, dpi = 500, w = 9, h = 9)
#ggsave("WorkvsLeisureContacts_AttitudeScore.png", p5, dpi = 500, w = 9, h = 9)

ggarrange(p4, p5, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 34), widths = c(1,1.1), common.legend = TRUE, legend = "bottom")

ggsave("WorkvsLeisureViolin_AttitudeScore2019.pdf", dpi = 500, w = 21, h = 12) 
ggsave("WorkvsLeisureViolin_AttitudeScore2019.png", dpi = 500, w = 21, h = 12) 


# ECDF --------------------------------------------------------------------

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


# Number of Infections ----------------------------------------------------

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