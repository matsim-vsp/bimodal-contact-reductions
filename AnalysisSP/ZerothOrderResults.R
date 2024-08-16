library(tidyverse)
library(igraph)
library(gridExtra)
library(ggiraphExtra)
library(Hmisc)

# 0th order Results -------------------------------------------------------

source("DataCleaningPrepForContactAnalysis.R")


p1_zeroth_order_absolut <- ggplot(data_reduced_tidy %>% filter((TypeOfContact %in% c("Work", "Leisure", "School") & value <= 100)) %>% 
filter(!is.na(TypeOfContact)) %>% filter(WhoseContacts=="Respondent"), aes(WhoseContacts, value)) +
  geom_violin(aes(fill = WhoseContacts), width = 1, trim = FALSE) + 
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                 geom="pointrange", color="black", linewidth = 1.2) +
  #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
  #geom_boxplot(aes(color = WhoseContacts), size = 1.3) +
  scale_fill_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols= vars(time)) +
  theme_minimal() +
  ylab("Reported Number Of Contacts") +
  theme(text = element_text(size = 30)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(panel.spacing = unit(0.8, "cm", data = NULL)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) 
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("CollectionViolinplots_Respondent.pdf", p1_zeroth_order_absolut, dpi = 500, w = 15, h = 18)
ggsave("CollectionViolinplots_Respondent.png", p1_zeroth_order_absolut, dpi = 500, w = 15, h = 18)

# palette <- function() {
#   c("#006BA6", "#FFBC42", "#8F2D56", "#C93E78")
# }

palette <- function() {
  c("#998ec3", "#d8daeb", "#f1a340", "#b35806")
}

p1_first_order_absolut <- ggplot(data_reduced_tidy %>% filter((TypeOfContact %in% c("Work", "Leisure", "School") & value <= 100)) %>% filter(!is.na(TypeOfContact)), aes(WhoseContacts, value)) +
  geom_violin(aes(fill = WhoseContacts), width = 1, trim = FALSE) + 
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                 geom="pointrange", color="black", linewidth = 1.2) +
  #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
  #geom_boxplot(aes(color = WhoseContacts), size = 1.3) +
  scale_fill_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols= vars(time)) +
  theme_minimal() +
  ylab("Reported Number Of Contacts") +
  theme(text = element_text(size = 30)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(panel.spacing = unit(0.8, "cm", data = NULL)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) 
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("CollectionViolinplots_Absolute.pdf", p1_first_order_absolut, dpi = 500, w = 15, h = 18)
ggsave("CollectionViolinplots_Absolute.png", p1_first_order_absolut, dpi = 500, w = 15, h = 18)

# Relative no. of contacts

p1_zeroth_order_percred <- ggplot(data_reduced_tidy_rel %>%  filter((TypeOfContact %in% c("Work", "Leisure"))) %>% 
    filter(WhoseContacts == "Respondent") %>% filter(!is.na(value)) %>%
    filter(value > -50) %>% filter(value < 150) %>%  
    filter(!is.na(TypeOfContact)), aes(WhoseContacts, value)) +
  geom_violin(aes(fill = WhoseContacts), scale = "area", trim = TRUE,  color="#542788") + 
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                 geom="pointrange", color="#542788", linewidth = 1) +
  #geom_violin(aes(color=WhoseContacts), size = 1.3) +
  scale_fill_manual(values = palette()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-50, 0,50, 100)) +
  facet_grid(rows = vars(TypeOfContact), cols = vars(time)) +
  theme_minimal() +
  ylab("Reduction Of Contacts [Percentage]") +
  theme(text = element_text(size = 30)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

ggsave("CollectionViolinplots_RemainingRespondent.pdf", p1_zeroth_order_percred, dpi = 500, w = 9, h = 8)
ggsave("CollectionViolinplots_RemainingRespondent.png", p1_zeroth_order_percred, dpi = 500, w = 9, h = 8)


# palette <- function() {
#   c("#006BA6", "#FFBC42", "#8F2D56", "#C93E78")
# }

palette <- function() {
  c("#542788", "#998ec3", "#f1a340", "#b35806")
}

p1_first_order_remaining <- ggplot(data_reduced_tidy_rel %>%  filter((TypeOfContact %in% c("Work", "Leisure", "School"))) %>% 
    #filter(attitudeScore > 6) %>% 
    filter(value >= -250) %>% filter(value < 150) %>%  
    filter(!is.na(TypeOfContact)), aes(WhoseContacts, value)) +
    geom_violin(aes(fill = WhoseContacts), width = 1, trim = TRUE) + 
    stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                 geom="pointrange", color="black", linewidth = 1.2) +
  #geom_violin(aes(color=WhoseContacts), size = 1.3) +
  scale_fill_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols = vars(time)) +
  theme_minimal() +
  ylab("Remaining % Of Contacts (Relative to 2019)") +
  theme(text = element_text(size = 30)) +
  scale_y_continuous(limits=c(-10,100)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

ggsave("CollectionBoxplots_Remaining.pdf", p1_first_order_remaining, dpi = 500, w = 13, h = 13)
ggsave("CollectionBoxplots_Remaining.png", p1_first_order_remaining, dpi = 500, w = 13, h = 13)

## ECDF 
data_reduced <- data_reduced %>% mutate(date_f1_inf = case_when(is.na(date_f1_inf) ~ as.Date("3000-01-01"),
                                        .default = as.Date(as.character(date_f1_inf)))) %>%
                                filter(date_f1_inf != as.Date("1922-03-01")) %>%
                                filter(date_f1_inf != as.Date("1965-06-12")) %>%
                                filter(date_f1_inf != as.Date("2000-12-13")) %>%
                                filter(date_f1_inf != as.Date("2019-12-21")) 

ggplot(data_reduced, aes(date_f1_inf)) +
stat_ecdf(geom="smooth", size = 2, color = "#998ec3") +
  theme_minimal() +
  ylab("Relative Frequency") +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette()) +
theme_minimal() +
ylab("Empirical Cumulative \n Distribution Function") +
xlab("Date Of First Infection") +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01")), ylim=c(0, 0.75)) +
theme(text = element_text(size = 30)) +
theme(legend.position = "none") +
guides(color = guide_legend(nrow = 2)) +
scale_color_manual(values = palette()) +
theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

ggsave("ECDF_Respondents.pdf", dpi = 500, w = 9, h = 9)
ggsave("ECDF_Respondents.png", dpi = 500, w = 9, h = 9)

## NO OF INFECTIONS

data_reduced <- data_reduced %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "Never",
                                                                    num_c19_infs == "Einmal" ~ "Once",
                                                                    num_c19_infs == "Zweimal" ~ "Twice",
                                                                    num_c19_infs == "Dreimal" ~ "Three Times",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "More Than Three Times",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

data_reduced$num_c19_infs_eng <- factor(data_reduced$num_c19_infs_eng, levels = c("Never", "Once", "Twice", "Three Times", "More Than Three Times", "I Don't Want To Answer"))

data_reduced %>%
  count(num_c19_infs_eng) %>%
  mutate(n = 100 * n / sum(n)) %>%  
  ggplot(aes(num_c19_infs_eng, n)) +
  geom_col(position = position_dodge(preserve = 'single'), fill = "#998ec3", color = "#998ec3") +
  theme_minimal() +
  ylab("Share [Percentage]") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50)) +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "none") +
  labs(fill="Age Group") +
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette())

ggsave("NoInfections_Respondents.pdf", dpi = 500, w = 9, h = 9)
ggsave("NoInfections_Respondents.png", dpi = 500, w = 9, h = 9)
