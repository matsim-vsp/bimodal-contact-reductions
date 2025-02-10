library(tidyverse)
library(MMWRweek)
library(see)
library(RColorBrewer)
library(patchwork)
library(ggpubr)
library(ggh4x)
library(scales)
library(smplot2)

# Author: S. Paltra, contact: paltra@tu-berlin.de

#raw_data <- readRDS(file = "/Users/sydney/Desktop/cleaned_data.rds")

source("DataCleaningPrepForContactAnalysis.R")


data_reduced <- data_reduced %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "0",
                                                                    num_c19_infs == "Einmal" ~ "1",
                                                                    num_c19_infs == "Zweimal" ~ "2",
                                                                    num_c19_infs == "Dreimal" ~ "3+",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "3+",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

data_reduced$num_c19_infs_eng <- factor(data_reduced$num_c19_infs_eng, levels = c("0", "1", "2", "3+", "I Don't Want To Answer"))


palette <- function() {
  c("#f1a340", "#998ec3")
}

palette2 <- function() {
  c("#b35806", "#542788")
}

my_comparisons <- list(c("female", "male"))

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% mutate(gender = case_when(gender == "Weiblich" ~ "female",
                                                        gender == "Männlich" ~ "male", 
                                                        gender == "Ich möchte nicht antworten" ~ "No Answer",
                                                        gender == "Divers" ~ "diverse"))

p1 <- ggplot(data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% 
    filter(!is.na(TypeOfContact)) %>% 
    filter(gender %in% c("female", "male")) %>% filter(TypeOfContact %in% c("Leisure", "Work")) %>%
    filter(value < 100) %>% filter(value > -150) %>%  
    group_by(gender, time), aes(gender, value, color = gender, fill = gender)) +
     sm_raincloud(aes(stat = median_cl), boxplot.params =  list(alpha = 0.6, width = 0.15, notch = TRUE), 
               violin.params = list(width = 1.6),
               shape = 21, sep_level = 2)  +
  # geom_violin(aes(fill = gender, color = gender), scale = "area", trim = TRUE) +  
  # stat_summary(aes(color=gender), fun.data=mean_sdl, fun.args = list(mult=1), 
  #                geom="pointrange", linewidth = 1) +
  # stat_compare_means(comparisons = my_comparisons, method = "t.test", symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("**** (p < 0.0001)", "*** (p < 0.001)", "** (p < 0.01)", "* (p < 0.05)", "not significant (p > 0.05)")), size = 6, bracket.size = 1, tip.length = 0.01, vjust = 0, label.y.npc = 0)+
  #geom_violin(aes(color=WhoseContacts), size = 1.3) +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-100, -50, 0,50, 100)) +
  facet_nested(~ TypeOfContact + time) +
  #facet_grid(cols = vars(time), rows = vars(TypeOfContact), switch = "x") +
  theme_minimal() +
  xlab("Point In Time") +
  theme(panel.spacing = unit(4, "lines")) +
  ylab("Change of No. of \n Contacts (in percent)") +
  theme(text = element_text(size = 35)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(panel.spacing.x = unit(c(rep(0,2),4, 0,0), "lines")) +
      theme(axis.ticks.x = element_line(size = 1), 
                   axis.ticks.y = element_line(size = 1),
                   axis.ticks.length = unit(20, "pt"))
ggsave("CollectionViolinplots_Gender.pdf", p1, dpi = 500, w = 22, h = 9)
ggsave("CollectionViolinplots_Gender.png", p1, dpi = 500, w = 22, h = 9)

mean <- data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% 
    filter(!is.na(TypeOfContact)) %>% 
    filter(gender %in% c("female", "male")) %>% filter(TypeOfContact %in% c("Leisure", "Work")) %>%
    filter(value < 100) %>% filter(value > -150) %>%  
    filter(!is.na(TypeOfContact)) %>% group_by(TypeOfContact, gender, time) %>% summarise(mean =mean(value))


data_reduced <- data_reduced %>% mutate(gender = case_when(gender == "Weiblich" ~ "female",
                                                        gender == "Männlich" ~ "male", 
                                                        gender == "Ich möchte nicht antworten" ~ "No Answer",
                                                        gender == "Divers" ~ "diverse"))


p3 <- data_reduced %>% filter(num_c19_infs_eng != "I Don't Want To Answer") %>%
  filter(!is.na(gender)) %>% filter(gender %in% c("male", "female")) %>%
  count(gender, num_c19_infs_eng) %>%
  mutate(percent = 100 * n / sum(n), .by = gender) %>%
  mutate(lci = case_when(gender == "male" ~ 386*(n/386 - 1.96*(((n/386*(1-n/386))/386)^0.5)),
                          gender == "female" ~ 464*(n/464 - 1.96*(((n/464*(1-n/464))/464)^0.5)))) %>%#
  mutate(lci = case_when(gender == "male" ~ 100/386*lci,
                         gender == "female" ~ 100/464*lci)) %>%
  mutate(lci = case_when (lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = case_when(gender == "male" ~ 386*(n/386 + 1.96*(((n/386*(1-n/386))/386)^0.5)),
                          gender == "female" ~ 464*(n/464 + 1.96*(((n/464*(1-n/464))/464)^0.5)))) %>%
  mutate(uci = case_when(gender == "male" ~ 100/386*uci,
                          gender == "female" ~ 100/464*uci)) %>%
  ggplot(aes(num_c19_infs_eng, percent, fill = gender)) +
  #geom_col(position = position_dodge(preserve = 'single')) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=num_c19_infs_eng, ymin=lci, ymax=uci, color = gender), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  ylab("Share of Respondents\n(in percent)") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50)) +
  xlab("Number of Infections") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  #labs(fill="Behavioral Group") +
  #theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
      theme(axis.ticks.x = element_line(size = 1), 
                   axis.ticks.y = element_line(size = 1),
                   axis.ticks.length = unit(20, "pt"))

ggsave("NoInfections_Gender.pdf", p3, dpi = 500, w = 9, h = 9)
ggsave("NoInfections_Gender.png", p3, dpi = 500, w = 9, h = 9)

data_reduced <- data_reduced %>% mutate(date_f1_inf = case_when(is.na(date_f1_inf) ~ as.Date("3000-01-01"),
                                        .default = as.Date(as.character(date_f1_inf)))) %>%
                                filter(date_f1_inf != as.Date("1922-03-01")) %>%
                                filter(date_f1_inf != as.Date("1965-06-12")) %>%
                                filter(date_f1_inf != as.Date("2000-12-13")) %>%
                                filter(date_f1_inf != as.Date("2019-12-21")) 

ecdf_comp <- data_reduced %>% filter(!is.na(gender)) %>% filter(gender %in% c("male", "female")) %>% group_by(gender) %>% 
count(date_f1_inf) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
  mutate(ecdf = cum/sum) %>% ungroup()
ecdf_comp <- rbind(ecdf_comp[1,], ecdf_comp)
ecdf_comp <- ecdf_comp %>% mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(uci = case_when(uci > 1 ~ 1, .default = uci))

p2 <- ggplot(ecdf_comp, aes(date_f1_inf)) +
geom_ribbon(aes(ymin = lci, ymax = uci, x = date_f1_inf, , fill = gender), alpha = 0.1)+
geom_line(aes(y=ecdf,  color =gender), size = 2) +
theme_minimal() +
ylab("Empirical Cumulative \n Distribution Function") +
xlab("Date of 1st Infection") +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01"))) +
theme(text = element_text(size = 30)) +
scale_color_manual(values = palette()) +
#scale_y_continuous(labels=percent) +
theme(legend.title = element_blank(), legend.position = "bottom")  +
    theme(axis.ticks.x = element_line(size = 1), 
                   axis.ticks.y = element_line(size = 1),
                   axis.ticks.length = unit(15, "pt"))

ggsave("ECDF_Gender.pdf", p2, dpi = 500, w = 9, h = 9)
ggsave("ECDF_Gender.png", p2, dpi = 500, w = 9, h = 9)

ggarrange(p3, p2, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")

ggsave("NoInfectionsECDF_Gender.pdf", dpi = 500, w = 18, h = 9) 
ggsave("NoInfectionsECDF_Gender.png", dpi = 500, w = 18, h = 9) 
