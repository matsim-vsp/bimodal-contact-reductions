library(tidyverse)
library(MMWRweek)
library(see)
library(RColorBrewer)
library(patchwork)

source("DataCleaningPrepForContactAnalysis.R")

data_reduced <- data_reduced %>% mutate(date_f1_inf = case_when(is.na(date_f1_inf) ~ as.Date("2025-01-01"),
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


# Detailed Analysis By Age ----------------------------------------------

data_reduced %>% count(age_bracket)

#palette <- function() {
#  c("#fd5901", "#f78104", "#faab36", "#249ea0", "#008083", "#005f60")
#}

palette <- function() {
  c("#542788", "#998ec3", "#d8daeb", "#fee0b6", "#f1a340", "#b35806")
}

p1 <- ggplot(data_reduced_tidy %>% filter(WhoseContacts == "Respondent") %>% 
filter(value > -150) %>% filter(value < 100) %>% 
filter(!is.na(age_bracket)) %>%
 filter(!is.na(TypeOfContact)), aes(age_bracket, value)) +
  #geom_violin(aes(color = WhoseContacts), width = 1, trim = FALSE, position=position_dodge(0.9)) + 
  #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
  geom_boxplot(aes(color = age_bracket), size = 1.3) +
  scale_color_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols = vars(time)) +
  theme_minimal() +
  ylab("Reported No. Of Contacts") +
  theme(text = element_text(size = 30)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom") +
  labs(color ="Age Group") +
  theme(panel.spacing = unit(0.8, "cm", data = NULL))
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#ggsave("CollectionBoxplots_AgeBrackets.pdf", p1, dpi = 500, w = 13, h = 19)
#ggsave("CollectionBoxplots_AgeBrackets.png", p1, dpi = 500, w = 13, h = 19)

# palette <- function() {
#   c("#fd5901", "#f78104", "#faab36", "#249ea0", "#008083", "#005f60")
# }

palette <- function() {
  c("#542788", "#998ec3", "#d8daeb", "#fee0b6", "#f1a340", "#b35806")
}

palette2 <- function() {
  c("#1a0a2b", "#542788", "#998ec3", "#f1a340", "#b35806", "#713500")
}

my_comparisons <- list(c("18-30", "30-40"), c("18-30", "40-50"), c("18-30", "50-60"), c("18-30", "60-70"), c("18-30", "70+"))

ggplot(data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% 
filter(!is.na(age_bracket)) %>% filter(!is.na(TypeOfContact)) %>% filter(TypeOfContact %in% c("Work")) %>%
    filter(value > -150) %>% filter(value < 100)  %>%
    filter(!is.na(TypeOfContact)), aes(age_bracket, value)) +
  geom_violin(aes(fill = age_bracket, color = age_bracket), scale = "area", trim = TRUE) + 
  stat_summary(aes(color=age_bracket), fun.data=mean_sdl, fun.args = list(mult=1), 
                 geom="pointrange", linewidth = 1) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test", symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("**** (p < 0.0001)", "*** (p < 0.001)", "** (p < 0.01)", "* (p < 0.05)", "not significant (p > 0.05)")), size = 6, bracket.size = 1, tip.length = 0.01, vjust = -0.2, label.y.npc = 0)+
  #geom_violin(aes(color=WhoseContacts), size = 1.3) +
  scale_fill_manual(values = palette()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-100,-50,0,50, 100)) +
  facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette2()) +
  ylab("Change of No. of \n Contacts (in percent)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Work") +
  theme(text = element_text(size = 30)) +
  theme(panel.spacing.y = unit(3, "lines")) +
  theme(panel.spacing.x = unit(3, "lines")) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
      theme(axis.ticks.x = element_line(size = 1), 
                   axis.ticks.y = element_line(size = 1),
                   axis.ticks.length = unit(20, "pt"))
 
ggsave("CollectionViolinplots_Work_AgeGroups.pdf", dpi = 500, w = 18, h = 9)
ggsave("CollectionViolinplots_Work_AgeGroups.png", dpi = 500, w = 18, h = 9)

data <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(data) <- c("age_bracket", "num_c19_infs_eng", "n", "percent")
data[nrow(data)+1,] <- c("60-70", "3+", 0,0)
data[nrow(data)+1,] <- c("70+", "3+", 0,0)
data$n <- as.integer(data$n)
data$percent <- as.integer(data$percent)

p3 <- data_reduced %>% group_by(age_bracket)  %>%
  count(num_c19_infs_eng) %>% 
  filter(!is.na(age_bracket)) %>% 
  filter(num_c19_infs_eng != "I Don't Want To Answer") %>%
  mutate(percent = 100 * n / sum(n)) %>%
  mutate(lci = case_when(age_bracket == "18-30" ~ 30*(n/30 - 1.96*(((n/30*(1-n/30))/30)^0.5)),
                          age_bracket == "30-40" ~ 125*(n/125 - 1.96*(((n/125*(1-n/125))/125)^0.5)),
                          age_bracket == "40-50" ~ 291*(n/291 - 1.96*(((n/291*(1-n/291))/291)^0.5)),
                          age_bracket == "50-60" ~ 275*(n/275 - 1.96*(((n/275*(1-n/275))/275)^0.5)),
                          age_bracket == "60-70" ~ 109*(n/109 - 1.96*(((n/109*(1-n/109))/109)^0.5)),
                          age_bracket == "70+" ~ 20*(n/20 - 1.96*(((n/20*(1-n/20))/20)^0.5)))) %>%#
  mutate(lci = case_when(age_bracket == "18-30" ~ 100/30*lci,
                         age_bracket == "30-40" ~ 100/125*lci,
                         age_bracket == "40-50" ~ 100/291*lci,
                         age_bracket == "50-60" ~ 100/275*lci,
                         age_bracket == "60-70" ~ 100/109*lci,
                         age_bracket == "70+" ~ 100/20*lci)) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = case_when(age_bracket == "18-30" ~ 30*(n/30 + 1.96*(((n/30*(1-n/30))/30)^0.5)),
                          age_bracket == "30-40" ~ 125*(n/125 + 1.96*(((n/125*(1-n/125))/125)^0.5)),
                          age_bracket == "40-50" ~ 291*(n/291 + 1.96*(((n/291*(1-n/291))/291)^0.5)),
                          age_bracket == "50-60" ~ 275*(n/275 + 1.96*(((n/275*(1-n/275))/275)^0.5)),
                          age_bracket == "60-70" ~ 109*(n/109 + 1.96*(((n/109*(1-n/109))/109)^0.5)),
                          age_bracket == "70+" ~ 20*(n/20 + 1.96*(((n/20*(1-n/20))/20)^0.5)))) %>%
  mutate(uci = case_when(age_bracket == "18-30" ~ 100/30*uci,
                          age_bracket == "30-40" ~ 100/125*uci,
                          age_bracket == "40-50" ~ 100/291*uci,
                          age_bracket == "50-60" ~ 100/275*uci,
                          age_bracket == "60-70" ~ 100/109*uci,
                          age_bracket == "70+" ~ 100/20*uci)) %>%
  ggplot(aes(num_c19_infs_eng, percent, fill = age_bracket)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  scale_x_discrete(limits = c("0", "1", "2", "3+")) +
  geom_errorbar(aes(x=num_c19_infs_eng, ymin=lci, ymax=uci, color = age_bracket), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  ylab("Share (in percent)") +
  xlab("Number of Infections") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
      theme(axis.ticks.x = element_line(size = 1), 
                   axis.ticks.y = element_line(size = 1),
                   axis.ticks.length = unit(20, "pt"))

ggsave("NoInfections_AgeBrackets.pdf", p3, dpi = 500, w = 9, h = 9)
ggsave("NoInfections_AgeBrackets.png", p3, dpi = 500, w = 9, h = 9)

p2 <- ggplot(data_reduced %>% filter(!is.na(age_bracket)), aes(date_f1_inf, color = age_bracket)) +
stat_ecdf(geom="smooth", size = 2) +
theme_minimal() +
ylab("Empirical Cumulative \n Density Function") +
xlab("Date Of First Infection") +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01")), ylim=c(0, 0.75)) +
theme(text = element_text(size = 30)) +
theme(legend.position = "bottom", legend.title = element_blank()) +
guides(color = guide_legend(nrow = 2)) +
scale_color_manual(values = palette()) +
    theme(axis.ticks.x = element_line(size = 1), 
                   axis.ticks.y = element_line(size = 1),
                   axis.ticks.length = unit(20, "pt"))

ggsave("ECDF_AgeBrackets.pdf", p2, dpi = 500, w = 9, h = 9)
ggsave("ECDF_AgeBrackets.png", p2, dpi = 500, w = 9, h = 9)

patch <- (p3/p2) +  plot_annotation(tag_levels = "A") 
p4 <- p1  + plot_spacer() + patch + plot_layout(widths = c(13, 0.5, 5)) +  plot_annotation(tag_levels = "A") 
ggsave("BoxplotNoInfectionsECDF_AgeBrackets.pdf", p4, dpi = 500, w = 18.5, h = 16)
ggsave("BoxplotNoInfectionsECDF_AgeBrackets.png", p4, dpi = 500, w = 18.5, h = 16)

## AGGREGATION OF AGE GROUPS
# The following sub-analysis only considers two age groups: 18-59 and 60+

data_reduced <- data_reduced %>% mutate(age_group = case_when (year_of_birth <= 1960 ~ "60+",
                                                            year_of_birth > 1960 ~ "18-59"))

palette <- function() {
  c("#fd5901", "#008083")
}

p2 <- ggplot(data_reduced %>% filter(!is.na(age_group)), aes(date_f1_inf, color = age_group)) +
stat_ecdf(geom="smooth", size = 2) +
theme_minimal() +
ylab("Empirical Cumulative \n Density Function") +
xlab("Date Of First Infection") +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01")), ylim=c(0, 0.75)) +
theme(text = element_text(size = 30)) +
theme(legend.position = "none") +
labs(color="Age Group") +
guides(color = guide_legend(nrow = 2)) +
scale_color_manual(values = palette())

#ggsave("TimingOfInfection_AggregatedAgeBrackets.pdf", p2, dpi = 500, w = 9, h = 12)
#ggsave("TimingOfInfection_AggregatedAgeBrackets.png", p2, dpi = 500, w = 9, h = 12)

p3 <- data_reduced %>%
  count(age_group, num_c19_infs_eng) %>%
  mutate(n = n / sum(n), .by = 'age_group') %>%
  filter(!is.na(age_group)) %>% 
  ggplot(aes(num_c19_infs_eng, n, fill = age_group)) +
  geom_col(position = position_dodge(preserve = 'single')) +
  theme_minimal() +
  ylab("Relative Frequency") +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "none") +
  #labs(fill="Age Group") +
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette())

#ggsave("NumberOfInfection_AggregatedAgeBrackets.pdf", p3, dpi = 500, w = 9, h = 12)
#ggsave("NumberOfInfection_AggregatedAgeBrackets.png", p3, dpi = 500, w = 9, h = 12)

#Boxplots for aggegrated age groups
data_reduced_tidy <- data_reduced_tidy %>% mutate(age_group = case_when (year_of_birth <= 1960 ~ "60+",
                                                            year_of_birth > 1960 ~ "18-59"))

palette <- function() {
  c("#fd5901", "#008083")
}

p1 <- ggplot(data_reduced_tidy %>% filter(WhoseContacts == "Respondent") %>% filter(value < 100) %>% filter(!is.na(age_group)) %>% filter(!is.na(TypeOfContact)), aes(age_group, value)) +
  #geom_violin(aes(color = WhoseContacts), width = 1, trim = FALSE, position=position_dodge(0.9)) + 
  #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
  geom_boxplot(aes(color = age_group), size = 1.3) +
  scale_color_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols = vars(time)) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  ylab("Reported No. Of Contacts") +
  theme(text = element_text(size = 30)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom") +
  labs(color ="Age Group") +
  theme(panel.spacing = unit(0.8, "cm", data = NULL))

#ggsave("CollectionBoxplots_AggregatedAgeBrackets.pdf", p1, dpi = 500, w = 13, h = 19)
#ggsave("CollectionBoxplots_AggregatedAgeBrackets.png", p1, dpi = 500, w = 13, h = 19)

patch <- (p3/p2) +  plot_annotation(tag_levels = "A")
p4 <- p1  + plot_spacer() + patch + plot_layout(widths = c(13, 0.5, 5)) +  plot_annotation(tag_levels = "A") 
ggsave("BoxplotNoInfectionsECDF_AggregatedAgeBrackets.pdf", p4, dpi = 500, w = 18.5, h = 16)
ggsave("BoxplotNoInfectionsECDF_AggregatedAgeBrackets.png", p4, dpi = 500, w = 18.5, h = 16)
