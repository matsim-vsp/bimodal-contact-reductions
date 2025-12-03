library(tidyverse) #version 2.0.0
library(MMWRweek) #version 0.1.3
library(see) #version 0.8.4
library(RColorBrewer) #version 1.1.3
library(patchwork) #version 1.2.0
library(ggpubr) #version 0.6.0
library(ggh4x) #version 0.3.1
library(scales) #version 1.3.0
library(smplot2) #version 0.2.4
library(here) #version 1.0.1

# Author: S. Paltra, contact: paltra@tu-berlin.de

here()
source("./R/DataPrep.R")
source("./R/mytheme.R")

palette <- function() {
  c("#3C5488FF", "#DC0000FF")
}

palette2 <- function() {
  c("#1d2942", "#870000")
}

# Contact Comparison -------------------------------------------------------

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% mutate(gender = case_when(gender == "Weiblich" ~ "female",
                                                                             gender == "Männlich" ~ "male", 
                                                                             gender == "Ich möchte nicht antworten" ~ "No Answer",
                                                                             gender == "Divers" ~ "diverse"))

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% mutate(time = case_when(time == "Summer 2021" ~ "Summer\n2021", .default = time))
data_reduced_tidy_rel$time <- factor(data_reduced_tidy_rel$time, levels = c("03/2020", "Summer\n2021", "01/2023"))

data_reduced_tidy_rel <- data_reduced_tidy_rel %>% filter(gender %in%  c("male", "female"))
data_reduced_tidy_rel$combined = interaction(data_reduced_tidy_rel$gender, data_reduced_tidy_rel$time)
combined_levels <- levels(interaction(data_reduced_tidy_rel$gender, data_reduced_tidy_rel$time))
A_values <- data_reduced_tidy_rel$time[match(combined_levels, data_reduced_tidy_rel$combined)]

unique_A_values <- unique(A_values)
unique_positions <- sapply(unique_A_values, function(a) {
  # For each unique A value, find the first position where it appears
  which(A_values == a)[1]
})


# Produces Supplementary Figure 14 ----------------------------------------

p1_work <- ggplot(data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% 
    filter(!is.na(gender)) %>% filter(gender %in% c("male", "female")) %>%
    filter(!is.na(TypeOfContact)) %>% 
    filter(TypeOfContact %in% c("Work")) %>% 
    filter(value > -150) %>%  filter(value < 100) %>%
    filter(!is.na(TypeOfContact)) %>% group_by(gender, TypeOfContact, time), aes(combined, value, color = gender, fill = gender)) +
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
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-100, -50, 0,50, 100)) +
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

p1_leisure <- ggplot(data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% 
                    filter(!is.na(gender)) %>% filter(gender %in% c("male", "female")) %>%
                    filter(!is.na(TypeOfContact)) %>% 
                    filter(TypeOfContact %in% c("Leisure")) %>% 
                    filter(value > -150) %>%  filter(value < 100) %>%
                    filter(!is.na(TypeOfContact)) %>% group_by(gender, TypeOfContact, time), aes(combined, value, color = gender, fill = gender)) +
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
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-100, -50, 0,50, 100)) +
  #facet_grid(~(time), switch="both")+
  ggtitle("Leisure") +
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

ggarrange(p1_work, p1_leisure, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")

#ggsave(paste0("./plots/","CollectionViolinplots_Gender.pdf"), dpi = 500, w = 24, h = 9)
#ggsave(paste0("./plots/","CollectionViolinplots_Gender.png"), dpi = 500, w = 24, h = 9)


# Mean Reduction ----------------------------------------------------------

print(data_reduced_tidy_rel %>%  
        filter(!is.na(value)) %>% 
        filter(value > -150) %>% filter(value < 100) %>% filter(WhoseContacts == "Respondent") %>%
        filter(!is.na(gender)) %>% group_by(TypeOfContact, gender, time) %>%
        summarise(meanRed = mean(value)), n = 100)

# Number of Infections -------------------------------------------------------

# Produces Supplementary Figure 15A

data_reduced <- data_reduced %>% mutate(gender = case_when(gender == "Weiblich" ~ "female",
                                                           gender == "Männlich" ~ "male", 
                                                           gender == "Ich möchte nicht antworten" ~ "No Answer",
                                                           gender == "Divers" ~ "diverse"))


data_reduced <- data_reduced %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "0",
                                                                     num_c19_infs == "Einmal" ~ "1",
                                                                     num_c19_infs == "Zweimal" ~ "2",
                                                                     num_c19_infs == "Dreimal" ~ "3+",
                                                                     num_c19_infs == "Mehr als dreimal" ~ "3+",
                                                                     num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

data_reduced$num_c19_infs_eng <- factor(data_reduced$num_c19_infs_eng, levels = c("0", "1", "2", "3+", "I Don't Want To Answer"))


no_infections <- data_reduced %>% filter(num_c19_infs_eng != "I Don't Want To Answer") %>%
  filter(!is.na(gender)) %>% filter(gender %in% c("male", "female")) %>%
  count(gender,num_c19_infs_eng) %>%
  mutate(percent = 100 * n / sum(n), .by = gender) %>%
  mutate(lci = case_when(gender == "male" ~ 385*(n/385 - 1.96*(((n/385*(1-n/385))/385)^0.5)),
                          gender == "female" ~ 464*(n/464 - 1.96*(((n/464*(1-n/464))/464)^0.5)))) %>%#
  mutate(lci = case_when(gender == "male" ~ 100/385*lci,
                         gender == "female" ~ 100/464*lci)) %>%
  mutate(lci = case_when (lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = case_when(gender == "male" ~ 385*(n/385 + 1.96*(((n/385*(1-n/385))/385)^0.5)),
                          gender == "female" ~ 464*(n/464 + 1.96*(((n/464*(1-n/464))/464)^0.5)))) %>%
  mutate(uci = case_when(gender == "male" ~ 100/385*uci,
                          gender == "female" ~ 100/464*uci)) %>%
  ggplot(aes(num_c19_infs_eng, percent, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=num_c19_infs_eng, ymin=lci, ymax=uci, color = gender),  position = position_dodge(0.8), width = 0.3, alpha=0.9, size=2.5) +
  theme_minimal() +
  ylab("Share (in percent)") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50)) +
  xlab("Number of Infections") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  #labs(fill="Behavioral Group") +
  #theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
  my_theme()

#ggsave(paste0("./plots/","NoInfections_Gender.pdf"), p3, dpi = 500, w = 9, h = 9)
#ggsave(paste0("./plots/","NoInfections_Gender.png"), p3, dpi = 500, w = 9, h = 9)

# ECDF -------------------------------------------------------

# Produces Supplementary Figure 15B

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

ecdf <- ggplot(ecdf_comp, aes(date_f1_inf)) +
geom_ribbon(aes(ymin = lci, ymax = uci, x = date_f1_inf, , fill = gender), alpha = 0.1)+
geom_line(aes(y=ecdf,  color =gender), size = 2) +
theme_minimal() +
ylab("Empirical Cumulative \n Distribution Function") +
xlab("Date of 1st Infection") +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01"))) +
theme(text = element_text(size = 30)) +
scale_color_manual(values = palette()) +
my_theme()

#ggsave(paste0("./plots/","ECDF_Gender.pdf"), p2, dpi = 500, w = 9, h = 9)
#ggsave(paste0("./plots/","ECDF_Gender.png"), p2, dpi = 500, w = 9, h = 9)

# Arrangement of subfigures and saving of Supplementary Figure 15

ggarrange(no_infections, ecdf, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")

ggsave(paste0("./plots/","NoInfectionsECDF_Gender_Suppl15.pdf"), dpi = 500, w = 22, h = 9) 
ggsave(paste0("./plots/","NoInfectionsECDF_Gender_Suppl15.png"), dpi = 500, w = 22, h = 9) 
