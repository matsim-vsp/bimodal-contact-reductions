library(tidyverse) #version 2.0.0
library(MMWRweek) #version 0.1.3
library(see) #version 0.8.4 
library(RColorBrewer) #version 1.1.3
library(patchwork) #version 1.2.0

here()
source("./R/DataCleaningPrepForContactAnalysis.R")


# Data prep ---------------------------------------------------------------

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


# Work and Leisure Contact Reductions -------------------------------------

# Produces Supplementary Figures 10 and 11

palette <- function() {
  c("#3C5488FF", "#f39b7f", "#DC0000FF")
}

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% mutate(time = case_when(time == "Summer 2021" ~ "Summer\n2021", .default = time))
data_reduced_tidy_rel$time <- factor(data_reduced_tidy_rel$time, levels = c("03/2020", "Summer\n2021", "01/2023"))

data_reduced_tidy_rel$combined = interaction(data_reduced_tidy_rel$age_bracket, data_reduced_tidy_rel$time)

combined_levels <- levels(interaction(data_reduced_tidy_rel$age_bracket, data_reduced_tidy_rel$time))
A_values <- data_reduced_tidy_rel$time[match(combined_levels, interaction(data_reduced_tidy_rel$age_bracket, data_reduced_tidy_rel$time))]

unique_A_values <- unique(A_values)
unique_positions <- sapply(unique_A_values, function(a) {
  # For each unique A value, find the first position where it appears
  which(A_values == a)[2]
})

agegroups_work <- ggplot(data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% 
                       filter(!is.na(age_bracket)) %>% 
                       filter(!is.na(TypeOfContact)) %>% 
                       filter(TypeOfContact %in% c("Work")) %>% #Change "Work" to "Leisure" if you want to recreate Suppl. Fig 11.
                       filter(value > -150) %>%  filter(value < 100) %>%
                       filter(!is.na(TypeOfContact)) %>% group_by(age_bracket, TypeOfContact, time), aes(combined, value, color = age_bracket, fill = age_bracket)) +
  sm_raincloud(aes(stat = median_cl), 
               point.params = list(size = 3, shape = 21, alpha = 0.5, position = sdamr::position_jitternudge(
                 nudge.x = -0.1,
                 jitter.width = 0.1, jitter.height = 0.01      
               )), 
               boxplot.params =  list(alpha = 0.0, width = 0.0, notch = TRUE), 
               violin.params = list(width = 1.5, scale = "area"),
               shape = 21, sep_level = 2)  +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-100, -50, 0,50, 100)) +
  ggtitle("Work") + #Change "Work" to "Leisure" if you want to recreate Suppl. Fig 11.
  theme_minimal() +
  theme(panel.spacing = unit(4, "lines")) +
  ylab("Change of No. of \n Contacts (in percent)") +
  my_theme() +
  theme(axis.title.x = element_blank(), plot.title = element_text(hjust=0.5)) +
  theme(axis.ticks.x = element_line(size = 0)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_discrete(
    breaks = combined_levels[unique_positions],  # Only put breaks at selected positions
    labels = unique_A_values                     # Use corresponding unique A values as labels
  )

ggarrange(agegroups_work, agegroups_leisure, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")

ggsave(paste0("./plots/", "SupplementaryFigure10.pdf"),  dpi = 500, w = 24, h = 9)
ggsave(paste0("./plots/","SupplementaryFigure10.png"), dpi = 500, w = 24, h = 9)

# Mean Reduction ----------------------------------------------------------

print(data_reduced_tidy_rel %>%  
        filter(!is.na(value)) %>% 
        filter(value > -150) %>% filter(value < 100) %>% filter(WhoseContacts == "Respondent") %>%
        filter(!is.na(gender)) %>% group_by(TypeOfContact, age_bracket, time) %>%
        summarise(meanRed = mean(value)), n = 100)

# Number of Infections ----------------------------------------------------

# Produces Supplementary Figure 12A

data <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(data) <- c("age_bracket", "num_c19_infs_eng", "n", "percent")
data[nrow(data)+1,] <- c("60+", "3+", 0,0)
data$n <- as.integer(data$n)
data$percent <- as.integer(data$percent)

palette <- function() {
  c("#3C5488FF", "#f39b7f", "#DC0000FF")
}

palette2 <- function() {
  c("#253353", "#c63e13", "#900000")
}

p3 <- data_reduced %>% group_by(age_bracket)  %>%
  count(num_c19_infs_eng) %>% 
  filter(!is.na(age_bracket)) %>% 
  filter(num_c19_infs_eng != "I Don't Want To Answer") %>%
  mutate(percent = 100 * n / sum(n)) %>%
  rbind(data) %>%
  mutate(lci = case_when(age_bracket == "18-39" ~ 155*(n/155 - 1.96*(((n/155*(1-n/155))/155)^0.5)),
                          age_bracket == "40-59" ~ 566*(n/566 - 1.96*(((n/566*(1-n/566))/566)^0.5)),
                          age_bracket == "60+" ~ 129*(n/129 - 1.96*(((n/129*(1-n/129))/129)^0.5)))) %>%#
  mutate(lci = case_when(age_bracket == "18-39" ~ 100/155*lci,
                         age_bracket == "40-59" ~ 100/566*lci,
                         age_bracket == "60+" ~ 100/129*lci)) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = case_when(age_bracket == "18-39" ~ 155*(n/155 + 1.96*(((n/155*(1-n/155))/155)^0.5)),
                          age_bracket == "40-59" ~ 566*(n/566 + 1.96*(((n/566*(1-n/566))/566)^0.5)),
                          age_bracket == "60+" ~ 130*(n/129 + 1.96*(((n/129*(1-n/129))/129)^0.5)))) %>%
  mutate(uci = case_when(age_bracket == "18-39" ~ 100/155*uci,
                          age_bracket == "40-59" ~ 100/566*uci,
                          age_bracket == "60+" ~ 100/129*uci)) %>%
  ggplot(aes(num_c19_infs_eng, percent, fill = age_bracket)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  scale_x_discrete(limits = c("0", "1", "2", "3+")) +
  geom_errorbar(aes(x=num_c19_infs_eng, ymin=lci, ymax=uci, color = age_bracket), position = position_dodge(0.8), width = 0.4, alpha=1, size=1.3) +
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
                   axis.ticks.length = unit(20, "pt")) +
  my_theme()

#ggsave(paste0("./plots/","SupplementaryFigure12A.pdf"), p3, dpi = 500, w = 9, h = 9)
#ggsave(paste0("./plots/","SupplementaryFigure12A.png"), p3, dpi = 500, w = 9, h = 9)


# ECDF Timing of First Infection ------------------------------------------

# Produces Supplementary Figure 12A

ecdf_comp <- data_reduced %>% filter(!is.na(age_bracket)) %>% group_by(age_bracket) %>% 
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
  geom_ribbon(aes(ymin = lci, ymax = uci, x = date_f1_inf, , fill = age_bracket), alpha = 0.3)+
  geom_line(aes(y=ecdf,  color = age_bracket), size = 2) +
  theme_minimal() +
  ylab("Empirical Cumulative \n Distribution Function") +
  xlab("Date of\n1st Infection") +
  coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01"))) +
  scale_color_manual(values = palette()) +
  scale_x_date(date_labels = "'%y")+
  #scale_y_continuous(labels=percent) +
  my_theme()

# Arrangement of subfigures and saving of Figure 12

ggarrange(p3, p2, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")

ggsave(paste0("./plots/","SupplementaryFigure12.pdf"), dpi = 500, w = 21, h = 12) 
ggsave(paste0("./plots/","SupplementaryFigure12.png"), dpi = 500, w = 21, h = 12) 
