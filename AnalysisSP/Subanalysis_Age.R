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

data_reduced <- data_reduced %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "Never",
                                                                    num_c19_infs == "Einmal" ~ "Once",
                                                                    num_c19_infs == "Zweimal" ~ "Twice",
                                                                    num_c19_infs == "Dreimal" ~ "Three Times",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "More Than Three Times",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

data_reduced$num_c19_infs_eng <- factor(data_reduced$num_c19_infs_eng, levels = c("Never", "Once", "Twice", "Three Times", "More Than Three Times", "I Don't Want To Answer"))


# Detailed Analysis By Age ----------------------------------------------

data_reduced %>% count(age_bracket)

palette <- function() {
  c("#fd5901", "#f78104", "#faab36", "#249ea0", "#008083", "#005f60")
}

p1 <- ggplot(data_reduced_tidy %>% filter(WhoseContacts == "Respondent") %>% filter(value < 100) %>% filter(!is.na(age_bracket)) %>% filter(!is.na(TypeOfContact)), aes(age_bracket, value)) +
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

palette <- function() {
  c("#fd5901", "#f78104", "#faab36", "#249ea0", "#008083", "#005f60")
}

ggplot(data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% filter(!is.na(age_bracket)) %>% filter(age_bracket != "80-90") %>% filter(value < 200) %>% filter(!is.na(TypeOfContact)), aes(age_bracket, value)) +
  geom_boxplot(aes(color = age_bracket), size = 1.3) +
  #geom_violin(aes(color=WhoseContacts), size = 1.3) +
  scale_color_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols= vars(time)) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  ylab("Relative Reported # Of Contacts") +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank())

#ggsave("CollectionBoxplotsRelativeAgeBrackets.pdf", dpi = 500, w = 13, h = 16)
#ggsave("CollectionBoxplotsRelativeAgeBrackets.png", dpi = 500, w = 13, h = 16)

p3 <- data_reduced %>%
  count(age_bracket, num_c19_infs_eng) %>%
  mutate(n = n / sum(n), .by = 'age_bracket') %>%
  filter(!is.na(age_bracket)) %>% 
  ggplot(aes(num_c19_infs_eng, n, fill = age_bracket)) +
  geom_col(position = position_dodge(preserve = 'single')) +
  theme_minimal() +
  ylab("Relative Frequency") +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "none") +
  labs(fill="Age Group") +
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette())

#ggsave("NumberOfInfection_AgeBrackets.pdf", p3, dpi = 500, w = 9, h = 12)
#ggsave("NumberOfInfection_AgeBrackets.png", p3, dpi = 500, w = 9, h = 12)

p2 <- ggplot(data_reduced %>% filter(!is.na(age_bracket)), aes(date_f1_inf, color = age_bracket)) +
stat_ecdf(geom="smooth", size = 2) +
theme_minimal() +
ylab("Empirical Cumulative \n Density Function") +
xlab("Date Of First Infection") +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01")), ylim=c(0, 0.75)) +
theme(text = element_text(size = 30)) +
theme(legend.position = "none") +
#labs(color="Age Group") +
guides(color = guide_legend(nrow = 2)) +
scale_color_manual(values = palette())

#ggsave("TimingOfInfection_AgeBrackets.pdf", p2, dpi = 500, w = 9, h = 12)
#ggsave("TimingOfInfection_AgeBrackets.png", p2, dpi = 500, w = 9, h = 12)

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
