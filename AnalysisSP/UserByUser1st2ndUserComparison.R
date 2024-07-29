library(tidyverse)
library(igraph)
library(gridExtra)
library(ggiraphExtra)

# Author: S. Paltra, contact: paltra@tu-berlin.de

# Aim of FIRST PART of this script:
# Survey respondents could forward the survey to their CC
# This script compares the answers of the secondary respondents (referred to as 'second responds')
# to the answers of the initial respondents (referred to as 'first respondent')
# (Internal node: Slide 52 f.)

source("DataCleaningPrepForContactAnalysis.R")

# Respondents who were forwarded the survey -------------------------------
data_reduced_second_respondents <- data_reduced %>% filter(!is.na(ref)) %>% 
  filter(!is.na(user_id)) %>%
  # Removal of respondets who's reference is one of the 5 people who share the link on twitter
  filter(!(ref %in% c("4a76b", "dec9d", "7b598", "008b5", "6c8d7"))) %>%
  # Removal of respondents who clicked on the link they were forwarded, but did not fill anything in 
  filter(!(user_id %in% c("5c0412b6-edfd-4eb8-84df-f8fe25b8c208", "b8bf1416-b83f-4b1b-a411-e715a6437cd2", "caba33fe-d132-4f6c-8dea-c059392888d4")))
# Manually removing the second to last row as this user started the survey twice
data_reduced_second_respondents <- data_reduced_second_respondents[-22,]   

data_reduced_second_respondents <- data_reduced_second_respondents %>% select(user_id, ref, cc_change_during_pandemic,  
                                                                              total_hsld_size_persons_under_14, hsld_size_2019_, hsld_size_03_2020_, hsld_size_summer_2021_, hsld_size_01_2023_,
                                                                              wkly_cont_2019_work_uni, wkly_cont_03_2020_work_uni, wkly_cont_summer_2021_work_uni, wkly_cont_01_2023_work_uni, 
                                                                              wkly_cont_2019_school_kinder, wkly_cont_03_2020_school_kinder, wkly_cont_summer_2021_school_kinder, wkly_cont_01_2023_school_kinder,
                                                                              wkly_cont_2019_leisure, wkly_cont_03_2020_leisure, wkly_cont_summer_2021_leisure, wkly_cont_01_2023_leisure)

colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "user_id")] <- "id_second_respondent"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "ref")] <- "id_original_respondent"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "cc_change_during_pandemic")] <- "cc_change_during_pandemic_second_respondent"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "total_hsld_size_persons_under_14")] <- "hsldsize_personsunder14"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "hsld_size_2019_")] <- "hsldsize_2019"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "hsld_size_03_2020_")] <- "hsldsize_03_2020"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "hsld_size_summer_2021_")] <- "hsldsize_summer_2021"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "hsld_size_01_2023_")] <- "hsldsize_01_2023"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_2019_work_uni")] <- "work_2019"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_03_2020_work_uni")] <- "work_03_2020"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_summer_2021_work_uni")] <- "work_summer_2021"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_01_2023_work_uni")] <- "work_01_2023"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_2019_school_kinder")] <- "school_2019"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_03_2020_school_kinder")] <- "school_03_2020"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_summer_2021_school_kinder")] <- "school_summer_2021"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_01_2023_school_kinder")] <- "school_01_2023"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_2019_leisure")] <- "leisure_2019"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_03_2020_leisure")] <- "leisure_03_2020"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_summer_2021_leisure")] <- "leisure_summer_2021"
colnames(data_reduced_second_respondents)[which(names(data_reduced_second_respondents) == "wkly_cont_01_2023_leisure")] <- "leisure_01_2023"

#Setting no of contacts equal to 0 if person DID not answer 
#TODO: How do I differentiate real zeros from introduced 0? Are there even "real zeros"? Check!
data_reduced_second_respondents <- as.data.frame(data_reduced_second_respondents)
data_reduced_second_respondents[is.na(data_reduced_second_respondents)] <- 0

data_reduced_second_respondents <- data_reduced_second_respondents %>% pivot_longer(cols = c("hsldsize_personsunder14", "hsldsize_2019", "hsldsize_03_2020", "hsldsize_summer_2021", "hsldsize_01_2023", "work_2019", "work_03_2020", "work_summer_2021", "work_01_2023", "school_2019", "school_03_2020", "school_summer_2021", "school_01_2023", "leisure_2019", "leisure_03_2020", "leisure_summer_2021", "leisure_01_2023"), names_to ="category", values_to="value_second_respondent")

data_reduced_second_respondents <- data_reduced_second_respondents %>% separate(category, c("category", "time"), "_", extra = "merge")

data_reduced_second_respondents <- data_reduced_second_respondents  %>% filter(id_original_respondent != "3f12f149-7326-453b-8628-edc7ccd046e8") %>% 
  filter(id_original_respondent != "5a13ab20-790f-4f32-83e1-b21e55c45a93") %>%
  filter(cc_change_during_pandemic_second_respondent == "Nein") %>%
  filter(time != "personsunder14")

# Respondents who originally filled out the survey ------------------------

data_reduced_original_respondents <- data_reduced %>% filter(user_id %in% unique(data_reduced_second_respondents$id_original_respondent))

data_reduced_original_respondents  <- data_reduced_original_respondents  %>% select(user_id, cc_change_during_pandemic, 
                                                                                    cc_hsld_size_pre_pandemic_2019_num_hsld_members, cc_hsld_size_pre_pandemic_03_2020_num_hsld_members, cc_hsld_size_pre_pandemic_summer_2021_num_hsld_members, cc_hsld_size_pre_pandemic_01_2023_num_hsld_members,
                                                                                    #cc_hsld_size_during_pandemic_2019_num_hsld_members, cc_hsld_size_during_pandemic_03_2020_num_hsld_members, cc_hsld_size_during_pandemic_summer_2021_num_hsld_members, cc_hsld_size_during_pandemic_01_2023_num_hsld_members,
                                                                                    cc_weekly_contacts_2019_work_uni_cont, cc_weekly_contacts_03_2020_work_uni_cont, cc_weekly_contacts_summer_2021_work_uni_cont, cc_weekly_contacts_01_2023_work_uni_cont,
                                                                                    #cc_weekly_cont_during_pandemic_2019_work_uni_cont, cc_weekly_cont_during_pandemic_03_2020_work_uni_cont, cc_weekly_cont_during_pandemic_summer_2021_work_uni_cont, cc_weekly_cont_during_pandemic_01_2023_work_uni_cont,
                                                                                    cc_weekly_contacts_2019_school_kinder_cont, cc_weekly_contacts_03_2020_school_kinder_cont, cc_weekly_contacts_summer_2021_school_kinder_cont, cc_weekly_contacts_01_2023_school_kinder_cont,
                                                                                    #cc_weekly_cont_during_pandemic_2019_school_kg_cont, cc_weekly_cont_during_pandemic_03_2020_school_kg_cont, cc_weekly_cont_during_pandemic_summer_2021_school_kg_cont, cc_weekly_cont_during_pandemic_01_2023_school_kg_cont,
                                                                                    cc_weekly_contacts_2019_leisure_cont, cc_weekly_contacts_03_2020_leisure_cont, cc_weekly_contacts_summer_2021_leisure_cont, cc_weekly_contacts_01_2023_leisure_cont)
#cc_weekly_cont_during_pandemic_2019_leisure_cont, cc_weekly_cont_during_pandemic_03_2020_leisure_cont, cc_weekly_cont_during_pandemic_summer_2021_leisure_cont, cc_weekly_cont_during_pandemic_01_2023_leisure_cont)

colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "user_id")] <- "id_original_respondent"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_change_during_pandemic")] <- "cc_change_during_pandemic_original_respondent"

colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_hsld_size_pre_pandemic_2019_num_hsld_members")] <- "hsldsize_2019"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_hsld_size_pre_pandemic_03_2020_num_hsld_members")] <- "hsldsize_03_2020"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_hsld_size_pre_pandemic_summer_2021_num_hsld_members")] <- "hsldsize_summer_2021"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_hsld_size_pre_pandemic_01_2023_num_hsld_members")] <- "hsldsize_01_2023"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_2019_work_uni_cont")] <- "work_2019"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_03_2020_work_uni_cont")] <- "work_03_2020"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_summer_2021_work_uni_cont")] <- "work_summer_2021"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_01_2023_work_uni_cont")] <- "work_01_2023"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_2019_school_kinder_cont")] <- "school_2019"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_03_2020_school_kinder_cont")] <- "school_03_2020"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_summer_2021_school_kinder_cont")] <- "school_summer_2021"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_01_2023_school_kinder_cont")] <- "school_01_2023"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_2019_leisure_cont")] <- "leisure_2019"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_03_2020_leisure_cont")] <- "leisure_03_2020"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_summer_2021_leisure_cont")] <- "leisure_summer_2021"
colnames(data_reduced_original_respondents)[which(names(data_reduced_original_respondents) == "cc_weekly_contacts_01_2023_leisure_cont")] <- "leisure_01_2023"

data_reduced_original_respondents <- data_reduced_original_respondents %>% pivot_longer(cols = c("hsldsize_2019", "hsldsize_03_2020", "hsldsize_summer_2021", "hsldsize_01_2023", "work_2019", "work_03_2020", "work_summer_2021", "work_01_2023", "school_2019", "school_03_2020", "school_summer_2021", "school_01_2023", "leisure_2019", "leisure_03_2020", "leisure_summer_2021", "leisure_01_2023"), names_to ="category", values_to="value_first_respondent")

data_reduced_original_respondents <- data_reduced_original_respondents %>% separate(category, c("category", "time"), "_", extra = "merge")

data_both <- left_join(data_reduced_original_respondents, data_reduced_second_respondents, by = c("id_original_respondent", "category", "time"))

data_both <- data_both %>% mutate(Diff = value_first_respondent - value_second_respondent)

data_both$time <- factor(data_both$time, levels = c("2019", "03_2020", "summer_2021", "01_2023"))

palette <- function() {
  c("#E4572E", "#29335C", "#F3A712", "#A8C686", "#669BBC")
}

#Difference all categories
ggplot(data_both %>% filter(id_second_respondent != "c34b67f3-53ca-47e4-82b2-56a7bc7f2c44")) + 
  geom_point(aes(x = time, y = Diff, colour = factor(category)), size = 3) +
  theme_linedraw() +
  facet_wrap(~id_second_respondent, nrow=3) +
  scale_color_manual(values = palette()) +
  ylab("Assumed # Contacts CC - #Actual Contacts CC") +
  xlab("Point In Time") +
  theme(legend.position = "bottom", legend.title=element_blank()) +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_text(angle = 90))

ggsave("AssumedMinusActual.pdf", dpi = 500, w = 16, h = 10)
ggsave("AssumedMinusActual.png", dpi = 500, w = 21, h = 9)


#Difference household
ggplot(data_both %>% filter(category == "hsldsize")) + 
  geom_point(aes(x = time, y = Diff, colour = factor(category)), size = 3) +
  theme_linedraw() +
  facet_wrap(~id_second_respondent, nrow=2) +
  scale_color_manual(values = palette()) +
  ylab("Assumed # Contacts CC - #Actual Contacts CC") +
  xlab("") +
  theme(legend.position = "bottom", legend.title=element_blank()) +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_text(angle = 90)) 

ggsave("AssumedMinusActualHousehold.pdf", dpi = 500, w = 21, h = 9)
ggsave("AssumedMinusActualHousehold.png", dpi = 500, w = 21, h = 9)

p1 <- ggplot(data_both %>% filter(category == "hsldsize"), aes(time, Diff)) +
  geom_boxplot(color = "#E4572E", linewidth = 1.1) +
  theme_minimal() +
  ggtitle("Household Size") +
  xlab("Point In Time") +
  ylab("Assumed # Contacts CC \n Minus #Actual Contacts CC") +
  theme(text = element_text(size = 22))

ggsave("AssumedMinusActualHouseholdBoxPlot.pdf", dpi = 500, w = 8, h = 4.5)
ggsave("AssumedMinusActualHouseholdBoxPlot.png", dpi = 500, w = 8, h = 4.5)

#Difference work
ggplot(data_both %>% filter(category == "work")) + 
  geom_point(aes(x = time, y = Diff, colour = factor(category)), size = 3) +
  theme_linedraw() +
  facet_wrap(~id_second_respondent, nrow=2) +
  scale_color_manual(values = palette()) +
  ylab("Assumed # Contacts CC - #Actual Contacts CC") +
  xlab("Assumed # Contacts CC \n Minus #Actual Contacts CC") +
  theme(legend.position = "bottom", legend.title=element_blank()) +
  theme(text = element_text(size = 25)) +
  theme(axis.text.x = element_text(angle = 90)) 

ggsave("AssumedMinusActualWork.pdf", dpi = 500, w = 21, h = 9)
ggsave("AssumedMinusActualWork.png", dpi = 500, w = 21, h = 9)

p2 <- ggplot(data_both %>% filter(category == "work"), aes(time, Diff)) +
  geom_boxplot(color = "#E4572E", linewidth = 1.1) +
  theme_minimal() +
  xlab("Time") +
  ggtitle("Work Contacts") +
  ylab("Assumed # Contacts CC \n Minus #Actual Contacts CC") +
  theme(text = element_text(size = 25))

ggsave("AssumedMinusActualWorkBoxPlot.pdf", dpi = 500, w = 8, h = 4.5)
ggsave("AssumedMinusActualWorkBoxPlot.png", dpi = 500, w = 8, h = 4.5)

#Difference school
ggplot(data_both %>% filter(category == "school")) + 
  geom_point(aes(x = time, y = Diff, colour = factor(category)), size = 3) +
  theme_linedraw() +
  facet_wrap(~id_second_respondent, nrow=2) +
  scale_color_manual(values = palette()) +
  ylab("Assumed # Contacts CC - #Actual Contacts CC") +
  xlab("") +
  theme(legend.position = "bottom", legend.title=element_blank()) +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_text(angle = 90)) 

ggsave("AssumedMinusActualSchool.pdf", dpi = 500, w = 21, h = 9)
ggsave("AssumedMinusActualSchool.png", dpi = 500, w = 21, h = 9)

ggplot(data_both %>% filter(category == "school"), aes(time, Diff)) +
  geom_boxplot() +
  theme_minimal() +
  xlab("Time") +
  ylab("Difference") +
  theme(text = element_text(size = 22))

ggsave("AssumedMinusActualSchoolBoxPlot.pdf", dpi = 500, w = 8, h = 4)
ggsave("AssumedMinusActualSchoolBoxPlot.png", dpi = 500, w = 8, h = 4)

#Difference leisure
ggplot(data_both %>% filter(category == "leisure")) + 
  geom_point(aes(x = time, y = Diff, colour = factor(category)), size = 3) +
  theme_linedraw() +
  facet_wrap(~id_second_respondent, nrow=2) +
  scale_color_manual(values = palette()) +
  ylab("Assumed # Contacts CC - #Actual Contacts CC") +
  xlab("") +
  theme(legend.position = "bottom", legend.title=element_blank()) +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_text(angle = 90)) 

ggsave("AssumedMinusActualLeisure.pdf", dpi = 500, w = 21, h = 9)
ggsave("AssumedMinusActualLeisure.png", dpi = 500, w = 21, h = 9)

p3 <- ggplot(data_both %>% filter(category == "leisure"), aes(time, Diff)) +
  geom_boxplot(color = "#E4572E", linewidth = 1.1) +
  theme_minimal() +
  xlab("Time") +
  ggtitle("Leisure Contacts") +
  ylab("") +
  theme(text = element_text(size = 25))

p <- arrangeGrob(p2,p3, nrow=1)
ggsave("AssumedMinusActualBoxPlot.pdf", p, dpi = 500, w = 24, h = 5)
ggsave("AssumedMinusActualBoxPlot.png", p, dpi = 500, w = 24, h = 5)

ggsave("AssumedMinusActualLeisureBoxPlot.pdf", dpi = 500, w = 8, h = 4.5)
ggsave("AssumedMinusActualLeisureBoxPlot.png", dpi = 500, w = 8, h = 4.5)

# Correlation Analysis ----------------------------------------------------

# Aim of SECOND PART of this script: Try to understand if there's a correlation between the no of contacts of the respondent and the no. of contacts of their cc
#data_reduced <- data_reduced %>% filter(!is.na(user_id))

correlation_matrix <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(correlation_matrix) <- c("context", "year", "change_of_cc", "pre_or_during", "correlation_coefficient")

#Not considering if CC was changed
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "2019", "ALL", "pre", cor(data_reduced$respondent_work_2019, data_reduced$cc_pre_work_2019, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "2019", "ALL", "pre", cor(data_reduced$respondent_leisure_2019, data_reduced$cc_pre_leisure_2019, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "2019", "ALL", "pre", cor(data_reduced$respondent_all_2019, data_reduced$cc_pre_all_2019, use = "pairwise.complete.obs", method = "pearson"))

p1_2019 <-ggplot(data_reduced) + 
  geom_point(aes(x=respondent_work_2019, y = cc_pre_work_2019), color = "#006BA6") +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Work Contacts (2019)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (pre)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2019 <- ggplot(data_reduced) + 
  geom_point(aes(x=respondent_leisure_2019, y = cc_pre_leisure_2019), color = "#006BA6") +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Leisure Contacts (2019)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (pre)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2019 <- ggplot(data_reduced) + 
  geom_point(aes(x=respondent_all_2019, y = cc_pre_all_2019), color = "#006BA6") +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("All Contacts (2019)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (pre)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "2019", "ALL", "during", cor(data_reduced$respondent_work_2019, data_reduced$cc_during_work_2019, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "2019", "ALL", "during", cor(data_reduced$respondent_leisure_2019, data_reduced$cc_during_leisure_2019, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "2019", "ALL", "during", cor(data_reduced$respondent_all_2019, data_reduced$cc_during_all_2019, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "03_2020", "ALL", "pre", cor(data_reduced$respondent_work_03_2020, data_reduced$cc_pre_work_03_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "03_2020", "ALL", "pre", cor(data_reduced$respondent_leisure_03_2020, data_reduced$cc_pre_leisure_03_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "03_2020", "ALL", "pre", cor(data_reduced$respondent_all_03_2020, data_reduced$cc_pre_all_03_2020, use = "pairwise.complete.obs", method = "pearson"))

p1_2020 <- ggplot(data_reduced) + 
  geom_point(aes(x=respondent_work_03_2020, y = cc_pre_work_03_2020), color = "#006BA6") +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Work Contacts (2020)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (pre)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2020 <- ggplot(data_reduced) + 
  geom_point(aes(x=respondent_leisure_03_2020, y = cc_pre_leisure_03_2020), color = "#006BA6") +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Leisure Contacts (2020)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (pre)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2020 <- ggplot(data_reduced) + 
  geom_point(aes(x=respondent_all_03_2020, y = cc_pre_all_03_2020), color = "#006BA6") +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("All Contacts (2020)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (pre)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "03_2020", "ALL", "during", cor(data_reduced$respondent_work_03_2020, data_reduced$cc_during_work_03_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "03_2020", "ALL", "during", cor(data_reduced$respondent_leisure_03_2020, data_reduced$cc_during_leisure_03_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "03_2020", "ALL", "during", cor(data_reduced$respondent_all_03_2020, data_reduced$cc_during_all_03_2020, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "summer_2021", "ALL", "pre", cor(data_reduced$respondent_work_summer_2021, data_reduced$cc_pre_work_summer_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "summer_2021", "ALL", "pre", cor(data_reduced$respondent_leisure_summer_2021, data_reduced$cc_pre_leisure_summer_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "summer_2021", "ALL", "pre", cor(data_reduced$respondent_all_summer_2021, data_reduced$cc_pre_all_summer_2021, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "summer_2021", "ALL", "during", cor(data_reduced$respondent_work_summer_2021, data_reduced$cc_during_work_summer_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "summer_2021", "ALL", "during", cor(data_reduced$respondent_leisure_summer_2021, data_reduced$cc_during_leisure_summer_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "summer_2021", "ALL", "during", cor(data_reduced$respondent_all_summer_2021, data_reduced$cc_during_all_summer_2021, use = "pairwise.complete.obs", method = "pearson"))

p1_2021 <- ggplot(data_reduced) + 
  geom_point(aes(x=respondent_work_summer_2021, y = cc_pre_work_summer_2021), color = "#006BA6") +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Work Contacts (2021)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (pre)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2021 <- ggplot(data_reduced) + 
  geom_point(aes(x=respondent_leisure_summer_2021, y = cc_pre_leisure_summer_2021), color = "#006BA6") +
  theme_minimal() +
  ggtitle("Leisure Contacts (2021)") +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (pre)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2021 <- ggplot(data_reduced) + 
  geom_point(aes(x=respondent_all_summer_2021, y = cc_pre_all_summer_2021), color = "#006BA6") +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("All Contacts (2021)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (pre)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "01_2023", "ALL", "pre", cor(data_reduced$respondent_work_01_2023, data_reduced$cc_pre_work_01_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "01_2023", "ALL", "pre", cor(data_reduced$respondent_leisure_01_2023, data_reduced$cc_pre_leisure_01_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "01_2023", "ALL", "pre", cor(data_reduced$respondent_all_01_2023, data_reduced$cc_pre_all_01_2023, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "01_2023", "ALL", "during", cor(data_reduced$respondent_work_01_2023, data_reduced$cc_during_work_01_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "01_2023", "ALL", "during", cor(data_reduced$respondent_leisure_01_2023, data_reduced$cc_during_leisure_01_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "01_2023", "ALL", "during", cor(data_reduced$respondent_all_01_2023, data_reduced$cc_during_all_01_2023, use = "pairwise.complete.obs", method = "pearson"))


p1_2023 <- ggplot(data_reduced) + 
  geom_point(aes(x=respondent_work_01_2023, y = cc_pre_work_01_2023), color = "#006BA6") +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Work Contacts (2023)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (pre)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2023 <- ggplot(data_reduced) + 
  geom_point(aes(x=respondent_leisure_01_2023, y = cc_pre_leisure_01_2023), color = "#006BA6") +
  theme_minimal() +
  ggtitle("Leisure Contacts (2023)") +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (pre)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2023 <- ggplot(data_reduced) + 
  geom_point(aes(x=respondent_all_01_2023, y = cc_pre_all_01_2023), color = "#006BA6") +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("All Contacts (2023)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (pre)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p <- arrangeGrob(p1_2019, p1_2020, p1_2021, p1_2023,
                  p2_2019, p2_2020, p2_2021, p2_2023,
                  p3_2019, p3_2020, p3_2021, p3_2023,
                 nrow = 3)

ggsave("CorrelationPlotsRespondent.pdf", p, dpi = 500, w = 12, h = 9)
ggsave("CorrelationPlotsRespondent.png", p, dpi = 500, w = 12, h = 9)

#Respondents who CHANGED their cc
data_reduced_yes <- data_reduced %>% filter(respondent_cc_change == "Ja")
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "2019", "Yes", "pre", cor(data_reduced_yes$respondent_work_2019, data_reduced_yes$cc_pre_work_2019, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "2019", "Yes", "pre", cor(data_reduced_yes$respondent_leisure_2019, data_reduced_yes$cc_pre_leisure_2019, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "2019", "Yes", "pre", cor(data_reduced_yes$respondent_all_2019, data_reduced_yes$cc_pre_all_2019, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "2019", "Yes", "during", cor(data_reduced_yes$respondent_work_2019, data_reduced_yes$cc_during_work_2019, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "2019", "Yes", "during", cor(data_reduced_yes$respondent_leisure_2019, data_reduced_yes$cc_during_leisure_2019, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "2019", "Yes", "during", cor(data_reduced_yes$respondent_all_2019, data_reduced_yes$cc_during_all_2019, use = "pairwise.complete.obs", method = "pearson"))

p1_2019 <-ggplot(data_reduced_yes) + 
  geom_point(aes(x=respondent_work_2019, y = cc_during_work_2019), color = "#006BA6") +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Work Contacts (2019)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (during)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2019 <- ggplot(data_reduced_yes) + 
  geom_point(aes(x=respondent_leisure_2019, y = cc_during_leisure_2019), color = "#006BA6") +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Leisure Contacts (2019)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (during)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2019 <- ggplot(data_reduced_yes) + 
  geom_point(aes(x=respondent_all_2019, y = cc_during_all_2019), color = "#006BA6") +
  theme_minimal() +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("All Contacts (2019)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (during)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "03_2020", "Yes", "pre", cor(data_reduced_yes$respondent_work_03_2020, data_reduced_yes$cc_pre_work_03_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "03_2020", "Yes", "pre", cor(data_reduced_yes$respondent_leisure_03_2020, data_reduced_yes$cc_pre_leisure_03_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "03_2020", "Yes", "pre", cor(data_reduced_yes$respondent_all_03_2020, data_reduced_yes$cc_pre_all_03_2020, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "03_2020", "Yes", "during", cor(data_reduced_yes$respondent_work_03_2020, data_reduced_yes$cc_during_work_03_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "03_2020", "Yes", "during", cor(data_reduced_yes$respondent_leisure_03_2020, data_reduced_yes$cc_during_leisure_03_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "03_2020", "Yes", "during", cor(data_reduced_yes$respondent_all_03_2020, data_reduced_yes$cc_during_all_03_2020, use = "pairwise.complete.obs", method = "pearson"))

p1_2020 <- ggplot(data_reduced_yes) + 
  geom_point(aes(x=respondent_work_03_2020, y = cc_during_work_03_2020), color = "#006BA6") +
  theme_minimal() +
  #xlim(0,60) +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Work Contacts (2020)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (during)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2020 <- ggplot(data_reduced_yes) + 
  geom_point(aes(x=respondent_leisure_03_2020, y = cc_during_leisure_03_2020), color = "#006BA6") +
  theme_minimal() +
  #xlim(0,25) +
  #ylim(0,25) +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Leisure Contacts (2020)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (during)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2020 <- ggplot(data_reduced_yes) + 
  geom_point(aes(x=respondent_all_03_2020, y = cc_during_all_03_2020), color = "#006BA6") +
  theme_minimal() +
  #xlim(0,100) +
  #ylim(0,100) +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("All Contacts (2020)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (during)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))


correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "summer_2021", "Yes", "pre", cor(data_reduced_yes$respondent_work_summer_2021, data_reduced_yes$cc_pre_work_summer_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "summer_2021", "Yes", "pre", cor(data_reduced_yes$respondent_leisure_summer_2021, data_reduced_yes$cc_pre_leisure_summer_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "summer_2021", "Yes", "pre", cor(data_reduced_yes$respondent_all_summer_2021, data_reduced_yes$cc_pre_all_summer_2021, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "summer_2021", "Yes", "during", cor(data_reduced_yes$respondent_work_summer_2021, data_reduced_yes$cc_during_work_summer_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "summer_2021", "Yes", "during", cor(data_reduced_yes$respondent_leisure_summer_2021, data_reduced_yes$cc_during_leisure_summer_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "summer_2021", "Yes", "during", cor(data_reduced_yes$respondent_all_summer_2021, data_reduced_yes$cc_during_all_summer_2021, use = "pairwise.complete.obs", method = "pearson"))


p1_2021 <- ggplot(data_reduced_yes) + 
  geom_point(aes(x=respondent_work_summer_2021, y = cc_during_work_summer_2021), color = "#006BA6") +
  theme_minimal() +
  #xlim(0,110) +
  #ylim(0,110) +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Work Contacts (2021)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (during)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2021 <- ggplot(data_reduced_yes) + 
  geom_point(aes(x=respondent_leisure_summer_2021, y = cc_during_leisure_summer_2021), color = "#006BA6") +
  theme_minimal() +
  ggtitle("Leisure Contacts (2021)") +
  #  xlim(0,60) +
  # ylim(0,100) +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (during)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2021 <- ggplot(data_reduced_yes) + 
  geom_point(aes(x=respondent_all_summer_2021, y = cc_during_all_summer_2021), color = "#006BA6") +
  theme_minimal() +
  #xlim(0,150) +
  #ylim(0,150) +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("All Contacts (2021)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (during)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "01_2023", "Yes", "pre", cor(data_reduced_yes$respondent_work_01_2023, data_reduced_yes$cc_pre_work_01_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "01_2023", "Yes", "pre", cor(data_reduced_yes$respondent_leisure_01_2023, data_reduced_yes$cc_pre_leisure_01_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "01_2023", "Yes", "pre", cor(data_reduced_yes$respondent_all_01_2023, data_reduced_yes$cc_pre_all_01_2023, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "01_2023", "Yes", "during", cor(data_reduced_yes$respondent_work_01_2023, data_reduced_yes$cc_during_work_01_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "01_2023", "Yes", "during", cor(data_reduced_yes$respondent_leisure_01_2023, data_reduced_yes$cc_during_leisure_01_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "01_2023", "Yes", "during", cor(data_reduced_yes$respondent_all_01_2023, data_reduced_yes$cc_during_all_01_2023, use = "pairwise.complete.obs", method = "pearson"))

p1_2023 <- ggplot(data_reduced_yes) + 
  geom_point(aes(x=respondent_work_01_2023, y = cc_during_work_01_2023), color = "#006BA6") +
  theme_minimal() +
  #xlim(0,150) +
  #ylim(0,150) +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("Work Contacts (2023)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (during)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p2_2023 <- ggplot(data_reduced_yes) + 
  geom_point(aes(x=respondent_leisure_01_2023, y = cc_during_leisure_01_2023), color = "#006BA6") +
  theme_minimal() +
  ggtitle("Leisure Contacts (2023)") +
  #xlim(0,60) +
  #ylim(0,60) +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  #xlim(1, 1000) +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (during)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p3_2023 <- ggplot(data_reduced_yes) + 
  geom_point(aes(x=respondent_all_01_2023, y = cc_during_all_01_2023), color = "#006BA6") +
  theme_minimal() +
  #xlim(0,150) +
  #ylim(0,150) +
  scale_x_log10(limits=c(1,1100))+
  scale_y_log10(limits=c(1,1100))+
  ggtitle("All Contacts (2023)") +
  xlab("#Contacts Respondent") +
  ylab("#Contacts CC (during)") +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

p <- arrangeGrob(p1_2019, p1_2020, p1_2021, p1_2023,
                 p2_2019, p2_2020, p2_2021, p2_2023,
                 p3_2019, p3_2020, p3_2021, p3_2023,
                 nrow = 3)

ggsave("CorrelationPlotsChangedCCRespondentsDuring.pdf", p, dpi = 500, w = 12, h = 9)
ggsave("CorrelationPlotsChangedCCRespondentsDuring.png", p, dpi = 500, w = 12, h = 9)

#Respondents who did NOT change their CC
data_reduced_no <- data_reduced %>% filter(respondent_cc_change == "Nein")
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "2019", "No", "pre", cor(data_reduced_no$respondent_work_2019, data_reduced_no$cc_pre_work_2019, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "2019", "No", "pre", cor(data_reduced_no$respondent_leisure_2019, data_reduced_no$cc_pre_leisure_2019, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "2019", "No", "pre", cor(data_reduced_no$respondent_all_2019, data_reduced_no$cc_pre_all_2019, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "2019", "No", "during", cor(data_reduced_no$respondent_work_2019, data_reduced_no$cc_during_work_2019, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "2019", "No", "during", cor(data_reduced_no$respondent_leisure_2019, data_reduced_no$cc_during_leisure_2019, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "2019", "No", "during", cor(data_reduced_no$respondent_all_2019, data_reduced_no$cc_during_all_2019, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "03_2020", "No", "pre", cor(data_reduced_no$respondent_work_03_2020, data_reduced_no$cc_pre_work_03_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "03_2020", "No", "pre", cor(data_reduced_no$respondent_leisure_03_2020, data_reduced_no$cc_pre_leisure_03_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "03_2020", "No", "pre", cor(data_reduced_no$respondent_all_03_2020, data_reduced_no$cc_pre_all_03_2020, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "03_2020", "No", "during", cor(data_reduced_no$respondent_work_03_2020, data_reduced_no$cc_during_work_03_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "03_2020", "No", "during", cor(data_reduced_no$respondent_leisure_03_2020, data_reduced_no$cc_during_leisure_03_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "03_2020", "No", "during", cor(data_reduced_no$respondent_all_03_2020, data_reduced_no$cc_during_all_03_2020, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "summer_2021", "No", "pre", cor(data_reduced_no$respondent_work_summer_2021, data_reduced_no$cc_pre_work_summer_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "summer_2021", "No", "pre", cor(data_reduced_no$respondent_leisure_summer_2021, data_reduced_no$cc_pre_leisure_summer_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "summer_2021", "No", "pre", cor(data_reduced_no$respondent_all_summer_2021, data_reduced_no$cc_pre_all_summer_2021, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "summer_2021", "No", "during", cor(data_reduced_no$respondent_work_summer_2021, data_reduced_no$cc_during_work_summer_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "summer_2021", "No", "during", cor(data_reduced_no$respondent_leisure_summer_2021, data_reduced_no$cc_during_leisure_summer_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "summer_2021", "No", "during", cor(data_reduced_no$respondent_all_summer_2021, data_reduced_no$cc_during_all_summer_2021, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "01_2023", "No", "pre", cor(data_reduced_no$respondent_work_01_2023, data_reduced_no$cc_pre_work_01_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "01_2023", "No", "pre", cor(data_reduced_no$respondent_leisure_01_2023, data_reduced_no$cc_pre_leisure_01_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "01_2023", "No", "pre", cor(data_reduced_no$respondent_all_01_2023, data_reduced_no$cc_pre_all_01_2023, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("work", "01_2023", "No", "during", cor(data_reduced_no$respondent_work_01_2023, data_reduced_no$cc_during_work_01_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("leisure", "01_2023", "No", "during", cor(data_reduced_no$respondent_leisure_01_2023, data_reduced_no$cc_during_leisure_01_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix[nrow(correlation_matrix) + 1, ] <- c("all", "01_2023", "No", "during", cor(data_reduced_no$respondent_all_01_2023, data_reduced_no$cc_during_all_01_2023, use = "pairwise.complete.obs", method = "pearson"))

#Computation of remaining % of contacts for respondent, pre-pandemic and during-pandemic CC
# Respondent
data_reduced <- data_reduced %>% mutate(red_respondent_work_2019_2020 = 100-(100/respondent_work_2019)*(respondent_work_2019-respondent_work_03_2020)) %>%
  mutate(red_respondent_work_2019_2021 = 100-(100/respondent_work_2019)*(respondent_work_2019-respondent_work_summer_2021)) %>%
  mutate(red_respondent_work_2019_2023 = 100-(100/respondent_work_2019)*(respondent_work_2019-respondent_work_01_2023)) %>%
  mutate(red_respondent_all_2019_2020 = 100-(100/respondent_all_2019)*(respondent_all_2019-respondent_all_03_2020)) %>%
  mutate(red_respondent_all_2019_2021 = 100-(100/respondent_all_2019)*(respondent_all_2019-respondent_all_summer_2021)) %>%
  mutate(red_respondent_all_2019_2023 = 100-(100/respondent_all_2019)*(respondent_all_2019-respondent_all_01_2023)) %>%
  mutate(red_respondent_leisure_2019_2020 = 100-(100/respondent_leisure_2019)*(respondent_leisure_2019-respondent_leisure_03_2020)) %>%
  mutate(red_respondent_leisure_2019_2021 = 100-(100/respondent_leisure_2019)*(respondent_leisure_2019-respondent_leisure_summer_2021)) %>%
  mutate(red_respondent_leisure_2019_2023 = 100-(100/respondent_leisure_2019)*(respondent_leisure_2019-respondent_leisure_01_2023))
# Pre-Pandemic CC
data_reduced <- data_reduced %>% mutate(red_cc_pre_work_2019_2020 = 100-(100/cc_pre_work_2019)*(cc_pre_work_2019-cc_pre_work_03_2020)) %>%
  mutate(red_cc_pre_work_2019_2021 = 100-(100/cc_pre_work_2019)*(cc_pre_work_2019-cc_pre_work_summer_2021)) %>%
  mutate(red_cc_pre_work_2019_2023 = 100-(100/cc_pre_work_2019)*(cc_pre_work_2019-cc_pre_work_01_2023)) %>%
  mutate(red_cc_pre_all_2019_2020 = 100-(100/cc_pre_all_2019)*(cc_pre_all_2019-cc_pre_all_03_2020)) %>%
  mutate(red_cc_pre_all_2019_2021 = 100-(100/cc_pre_all_2019)*(cc_pre_all_2019-cc_pre_all_summer_2021)) %>%
  mutate(red_cc_pre_all_2019_2023 = 100-(100/cc_pre_all_2019)*(cc_pre_all_2019-cc_pre_all_01_2023)) %>%
  mutate(red_cc_pre_leisure_2019_2020 = 100-(100/cc_pre_leisure_2019)*(cc_pre_leisure_2019-cc_pre_leisure_03_2020)) %>%
  mutate(red_cc_pre_leisure_2019_2021 = 100-(100/cc_pre_leisure_2019)*(cc_pre_leisure_2019-cc_pre_leisure_summer_2021)) %>%
  mutate(red_cc_pre_leisure_2019_2023 = 100-(100/cc_pre_leisure_2019)*(cc_pre_leisure_2019-cc_pre_leisure_01_2023))
# During-Pandemic CC 
data_reduced <- data_reduced %>% mutate(red_cc_during_work_2019_2020 = 100-(100/cc_during_work_2019)*(cc_during_work_2019-cc_during_work_03_2020)) %>%
  mutate(red_cc_during_work_2019_2021 = 100-(100/cc_during_work_2019)*(cc_during_work_2019-cc_during_work_summer_2021)) %>%
  mutate(red_cc_during_work_2019_2023 = 100-(100/cc_during_work_2019)*(cc_during_work_2019-cc_during_work_01_2023)) %>%
  mutate(red_cc_during_all_2019_2020 = 100-(100/cc_during_all_2019)*(cc_during_all_2019-cc_during_all_03_2020)) %>%
  mutate(red_cc_during_all_2019_2021 = 100-(100/cc_during_all_2019)*(cc_during_all_2019-cc_during_all_summer_2021)) %>%
  mutate(red_cc_during_all_2019_2023 = 100-(100/cc_during_all_2019)*(cc_during_all_2019-cc_during_all_01_2023)) %>%
  mutate(red_cc_during_leisure_2019_2020 = 100-(100/cc_during_leisure_2019)*(cc_during_leisure_2019-cc_during_leisure_03_2020)) %>%
  mutate(red_cc_during_leisure_2019_2021 = 100-(100/cc_during_leisure_2019)*(cc_during_leisure_2019-cc_during_leisure_summer_2021)) %>%
  mutate(red_cc_during_leisure_2019_2023 = 100-(100/cc_during_leisure_2019)*(cc_during_leisure_2019-cc_during_leisure_01_2023))




correlation_matrix_relative <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(correlation_matrix_relative) <- c("context", "year", "change_of_cc", "pre_or_during", "correlation_coefficient")

#Not considering if CC was changed
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "03_2020", "ALL", "pre", cor(data_reduced$red_respondent_work_2019_2020, data_reduced$red_cc_pre_work_2019_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "03_2020", "ALL", "pre", with(subset(data_reduced, (is.finite(red_respondent_leisure_2019_2020)) & (is.finite(data_reduced$red_cc_pre_leisure_2019_2020))), cor(red_respondent_leisure_2019_2020, red_cc_pre_leisure_2019_2020, method = "pearson", use = "pairwise.complete.obs")))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "03_2020", "ALL", "pre", cor(data_reduced$red_respondent_all_2019_2020, data_reduced$red_cc_pre_all_2019_2020, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "03_2020", "ALL", "during", cor(data_reduced$red_respondent_work_2019_2020, data_reduced$red_cc_during_work_2019_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "03_2020", "ALL", "during", cor(data_reduced$red_respondent_leisure_2019_2020, data_reduced$red_cc_during_leisure_2019_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "03_2020", "ALL", "during", cor(data_reduced$red_respondent_all_2019_2020, data_reduced$red_cc_during_all_2019_2020, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "summer_2021", "ALL", "pre", with(subset(data_reduced, (is.finite(red_respondent_work_2019_2021)) & (is.finite(data_reduced$red_cc_pre_work_2019_2021))), cor(red_respondent_work_2019_2021, red_cc_pre_work_2019_2021, method = "pearson", use = "pairwise.complete.obs")))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "summer_2021", "ALL", "pre", with(subset(data_reduced, (is.finite(red_respondent_leisure_2019_2021)) & (is.finite(data_reduced$red_cc_pre_leisure_2019_2021))), cor(red_respondent_leisure_2019_2021, red_cc_pre_leisure_2019_2021, method = "pearson", use = "pairwise.complete.obs")))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "summer_2021", "ALL", "pre", cor(data_reduced$red_respondent_all_2019_2021, data_reduced$red_cc_pre_all_2019_2021, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "summer_2021", "ALL", "during", cor(data_reduced$red_respondent_work_2019_2021, data_reduced$red_cc_during_work_2019_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "summer_2021", "ALL", "during", cor(data_reduced$red_respondent_leisure_2019_2021, data_reduced$red_cc_during_leisure_2019_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "summer_2021", "ALL", "during", cor(data_reduced$red_respondent_all_2019_2021, data_reduced$red_cc_during_all_2019_2021, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "01_2023", "ALL", "pre", with(subset(data_reduced, (is.finite(red_respondent_work_2019_2023)) & (is.finite(data_reduced$red_cc_pre_work_2019_2023))), cor(red_respondent_work_2019_2023, red_cc_pre_work_2019_2023, method = "pearson", use = "pairwise.complete.obs")))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "01_2023", "ALL", "pre", with(subset(data_reduced, (is.finite(red_respondent_leisure_2019_2023)) & (is.finite(data_reduced$red_cc_pre_leisure_2019_2023))), cor(red_respondent_leisure_2019_2023, red_cc_pre_leisure_2019_2023, method = "pearson", use = "pairwise.complete.obs")))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "01_2023", "ALL", "pre", cor(data_reduced$red_respondent_all_2019_2023, data_reduced$red_cc_pre_all_2019_2023, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "01_2023", "ALL", "during", cor(data_reduced$red_respondent_work_2019_2023, data_reduced$red_cc_during_work_2019_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "01_2023", "ALL", "during", cor(data_reduced$red_respondent_leisure_2019_2023, data_reduced$red_cc_during_leisure_2019_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "01_2023", "ALL", "during", cor(data_reduced$red_respondent_all_2019_2023, data_reduced$red_cc_during_all_2019_2023, use = "pairwise.complete.obs", method = "pearson"))

#Respondents who CHANGED their cc
data_reduced_yes <- data_reduced %>% filter(respondent_cc_change == "Ja")

correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "03_2020", "Yes", "pre", cor(data_reduced_yes$red_respondent_work_2019_2020, data_reduced_yes$red_cc_pre_work_2019_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "03_2020", "Yes", "pre", cor(data_reduced_yes$red_respondent_leisure_2019_2020, data_reduced_yes$red_cc_pre_leisure_2019_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "03_2020", "Yes", "pre", cor(data_reduced_yes$red_respondent_all_2019_2020, data_reduced_yes$red_cc_pre_all_2019_2020, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "03_2020", "Yes", "during", cor(data_reduced_yes$red_respondent_work_2019_2020, data_reduced_yes$red_cc_during_work_2019_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "03_2020", "Yes", "during", cor(data_reduced_yes$red_respondent_leisure_2019_2020, data_reduced_yes$red_cc_during_leisure_2019_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "03_2020", "Yes", "during", cor(data_reduced_yes$red_respondent_all_2019_2020, data_reduced_yes$red_cc_during_all_2019_2020, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "summer_2021", "Yes", "pre", cor(data_reduced_yes$red_respondent_work_2019_2021, data_reduced_yes$red_cc_pre_work_2019_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "summer_2021", "Yes", "pre", cor(data_reduced_yes$red_respondent_leisure_2019_2021, data_reduced_yes$red_cc_pre_leisure_2019_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "summer_2021", "Yes", "pre", cor(data_reduced_yes$red_respondent_all_2019_2021, data_reduced_yes$red_cc_pre_all_2019_2021, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "summer_2021", "Yes", "during", cor(data_reduced_yes$red_respondent_work_2019_2021, data_reduced_yes$red_cc_during_work_2019_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "summer_2021", "Yes", "during", cor(data_reduced_yes$red_respondent_leisure_2019_2021, data_reduced_yes$red_cc_during_leisure_2019_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "summer_2021", "Yes", "during", cor(data_reduced_yes$red_respondent_all_2019_2021, data_reduced_yes$red_cc_during_all_2019_2021, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "01_2023", "Yes", "pre", cor(data_reduced_yes$red_respondent_work_2019_2023, data_reduced_yes$red_cc_pre_work_2019_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "01_2023", "Yes", "pre", cor(data_reduced_yes$red_respondent_leisure_2019_2023, data_reduced_yes$red_cc_pre_leisure_2019_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "01_2023", "Yes", "pre", cor(data_reduced_yes$red_respondent_all_2019_2023, data_reduced_yes$red_cc_pre_all_2019_2023, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "01_2023", "Yes", "during", cor(data_reduced_yes$red_respondent_work_2019_2023, data_reduced_yes$red_cc_during_work_2019_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "01_2023", "Yes", "during", cor(data_reduced_yes$red_respondent_leisure_2019_2023, data_reduced_yes$red_cc_during_leisure_2019_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "01_2023", "Yes", "during", cor(data_reduced_yes$red_respondent_all_2019_2023, data_reduced_yes$red_cc_during_all_2019_2023, use = "pairwise.complete.obs", method = "pearson"))

#respondents who did NOT change their CC
data_reduced_no <- data_reduced %>% filter(respondent_cc_change == "Nein")

correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "03_2020", "No", "pre", cor(data_reduced_no$red_respondent_work_2019_2020, data_reduced_no$red_cc_pre_work_2019_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "03_2020", "No", "pre", with(subset(data_reduced_no, (is.finite(red_respondent_leisure_2019_2020)) & (is.finite(red_cc_pre_leisure_2019_2020))), cor(red_respondent_leisure_2019_2020, red_cc_pre_leisure_2019_2020, method = "pearson", use = "pairwise.complete.obs")))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "03_2020", "No", "pre", cor(data_reduced_no$red_respondent_all_2019_2020, data_reduced_no$red_cc_pre_all_2019_2020, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "03_2020", "No", "during", cor(data_reduced_no$red_respondent_work_2019_2020, data_reduced_no$red_cc_during_work_2019_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "03_2020", "No", "during", cor(data_reduced_no$red_respondent_leisure_2019_2020, data_reduced_no$red_cc_during_leisure_2019_2020, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "03_2020", "No", "during", cor(data_reduced_no$red_respondent_all_2019_2020, data_reduced_no$red_cc_during_all_2019_2020, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "summer_2021", "No", "pre", with(subset(data_reduced_no, (is.finite(red_respondent_work_2019_2021)) & (is.finite(red_cc_pre_work_2019_2021))), cor(red_respondent_work_2019_2021, red_cc_pre_work_2019_2021, method = "pearson", use = "pairwise.complete.obs")))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "summer_2021", "No", "pre", with(subset(data_reduced_no, (is.finite(red_respondent_leisure_2019_2021)) & (is.finite(red_cc_pre_leisure_2019_2021))), cor(red_respondent_leisure_2019_2021, red_cc_pre_leisure_2019_2021, method = "pearson", use = "pairwise.complete.obs")))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "summer_2021", "No", "pre", cor(data_reduced_no$red_respondent_all_2019_2021, data_reduced_no$red_cc_pre_all_2019_2021, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "summer_2021", "No", "during", cor(data_reduced_no$red_respondent_work_2019_2021, data_reduced_no$red_cc_during_work_2019_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "summer_2021", "No", "during", cor(data_reduced_no$red_respondent_leisure_2019_2021, data_reduced_no$red_cc_during_leisure_2019_2021, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "summer_2021", "No", "during", cor(data_reduced_no$red_respondent_all_2019_2021, data_reduced_no$red_cc_during_all_2019_2021, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "01_2023", "No", "pre", with(subset(data_reduced_no, (is.finite(red_respondent_work_2019_2023)) & (is.finite(red_cc_pre_work_2019_2023))), cor(red_respondent_work_2019_2023, red_cc_pre_work_2019_2023, method = "pearson", use = "pairwise.complete.obs")))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "01_2023", "No", "pre", with(subset(data_reduced_no, (is.finite(red_respondent_leisure_2019_2023)) & (is.finite(red_cc_pre_leisure_2019_2023))), cor(red_respondent_leisure_2019_2023, red_cc_pre_leisure_2019_2023, method = "pearson", use = "pairwise.complete.obs")))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "01_2023", "No", "pre", cor(data_reduced_no$red_respondent_all_2019_2023, data_reduced_no$red_cc_pre_all_2019_2023, use = "pairwise.complete.obs", method = "pearson"))

correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("work", "01_2023", "No", "during", cor(data_reduced_no$red_respondent_work_2019_2023, data_reduced_no$red_cc_during_work_2019_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("leisure", "01_2023", "No", "during", cor(data_reduced_no$red_respondent_leisure_2019_2023, data_reduced_no$red_cc_during_leisure_2019_2023, use = "pairwise.complete.obs", method = "pearson"))
correlation_matrix_relative[nrow(correlation_matrix_relative) + 1, ] <- c("all", "01_2023", "No", "during", cor(data_reduced_no$red_respondent_all_2019_2023, data_reduced_no$red_cc_during_all_2019_2023, use = "pairwise.complete.obs", method = "pearson"))



# Correlation Analysis - Separated By Attitudes ---------------------------

attitudes <- c("attitudes_precautions_mar2020_low_infection_risk_perception",                
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

possible_attitudes <- c("viel weniger",                                              
                        "genauso",                                                    
                         "weniger",                                                    
                         "viel mehr",                                                  
                         "trifft nicht zu",                                            
                         "etwas mehr",                                                 
                         "etwas weniger",                                              
                         "mehr",                                                       
                         "keine Angabe")

correlation_matrix <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(correlation_matrix) <- c("attitude", "answer", "group_size(n)", "year", "context", "correlation_coefficient")

for(i in 1:length(attitudes)){
  attitude <- attitudes[i]
  for(possible_attitude in possible_attitudes){
    grouped_data <- data_reduced %>% filter(!!sym(attitude) == possible_attitude)
    if(nrow(grouped_data) > 0){
    times <- c("2019", "03_2020", "summer_2021", "01_2023")
    for(time in times){
      correlation_matrix[nrow(correlation_matrix) + 1, ] <- c(attitude,
                                                              possible_attitude,
                                                              nrow(grouped_data),
                                                              time,
                                                              "work",
                                                              cor(grouped_data[,paste0("respondent_work_", time)], grouped_data[,paste0("cc_pre_work_", time)], use = "pairwise.complete.obs"))
      correlation_matrix[nrow(correlation_matrix) + 1, ] <- c(attitude,
                                                              possible_attitude,
                                                              nrow(grouped_data),
                                                              time,
                                                              "leisure",
                                                              cor(grouped_data[,paste0("respondent_leisure_", time)], grouped_data[,paste0("cc_pre_leisure_", time)], use = "pairwise.complete.obs"))
      correlation_matrix[nrow(correlation_matrix) + 1, ] <- c(attitude,
                                                              possible_attitude,
                                                              nrow(grouped_data),
                                                              time,
                                                              "all",
                                                              cor(grouped_data[,paste0("respondent_all_", time)], grouped_data[,paste0("cc_pre_all_", time)], use = "pairwise.complete.obs"))
       }
    }
  }
}

correlation_matrix$answer <- factor(correlation_matrix$answer, levels = c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"), ordered = TRUE)
correlation_matrix <- as.data.frame(correlation_matrix)
correlation_matrix <- correlation_matrix[order(correlation_matrix$answer), ]

