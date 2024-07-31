library(tidyverse)
library(igraph)
library(gridExtra)
library(ggiraphExtra)

source("DataCleaningPrepForContactAnalysis.R")

# 0th order Results -------------------------------------------------------
## RESPONDENT'S CONTACTS
# WORK CONTACTS
summary(data_reduced$respondent_work_2019)
sum(!is.na(data_reduced$respondent_work_2019))

summary(data_reduced$respondent_work_03_2020)
sum(!is.na(data_reduced$respondent_work_03_2020))
 
summary(data_reduced$respondent_work_summer_2021)
sum(!is.na(data_reduced$respondent_work_summer_2021))

summary(data_reduced$respondent_work_01_2023)
sum(!is.na(data_reduced$respondent_work_01_2023))

# Leisure CONTACTS
summary(data_reduced$respondent_leisure_2019)
sum(!is.na(data_reduced$respondent_leisure_2019))

summary(data_reduced$respondent_leisure_03_2020)
sum(!is.na(data_reduced$respondent_leisure_03_2020))

summary(data_reduced$respondent_leisure_summer_2021)
sum(!is.na(data_reduced$respondent_leisure_summer_2021))

summary(data_reduced$respondent_leisure_01_2023)
sum(!is.na(data_reduced$respondent_leisure_01_2023))

# ALL CONTACTS
summary(data_reduced$respondent_all_2019)
sum(!is.na(data_reduced$respondent_all_2019))

summary(data_reduced$respondent_all_03_2020)
sum(!is.na(data_reduced$respondent_all_03_2020))
 
summary(data_reduced$respondent_all_summer_2021)
sum(!is.na(data_reduced$respondent_all_summer_2021))

summary(data_reduced$respondent_all_01_2023)
sum(!is.na(data_reduced$respondent_all_01_2023))


# Boxplots ----------------------------------------------------------------

all_contacts <- raw_data %>% select(hsld_size_2019_, hsld_size_03_2020_, hsld_size_summer_2021_, hsld_size_01_2023_, 
                                    wkly_cont_2019_work_uni, wkly_cont_03_2020_work_uni, wkly_cont_summer_2021_work_uni, wkly_cont_01_2023_work_uni,
                                    wkly_cont_2019_school_kinder, wkly_cont_03_2020_school_kinder, wkly_cont_summer_2021_school_kinder, wkly_cont_01_2023_school_kinder,
                                    wkly_cont_2019_leisure, wkly_cont_03_2020_leisure, wkly_cont_summer_2021_leisure, wkly_cont_01_2023_leisure)

all_contacts <- all_contacts %>% mutate(all2019 = hsld_size_2019_ +  wkly_cont_2019_work_uni + wkly_cont_2019_school_kinder + wkly_cont_2019_leisure,
                                        all032020 = hsld_size_03_2020_ + wkly_cont_03_2020_work_uni + wkly_cont_03_2020_school_kinder + wkly_cont_03_2020_leisure,
                                        allsummer21 = hsld_size_summer_2021_ + wkly_cont_summer_2021_work_uni + wkly_cont_summer_2021_school_kinder + wkly_cont_summer_2021_leisure,
                                        all23 = hsld_size_01_2023_ + wkly_cont_01_2023_work_uni + wkly_cont_01_2023_school_kinder + wkly_cont_01_2023_leisure)

all_contacts <- all_contacts %>% select(all2019, all032020, allsummer21, all23) %>% pivot_longer(cols = c("all2019", "all032020", "allsummer21", "all23"))


all_contacts <- all_contacts %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "all2019" ~ "2019",
                                                                                               name == "all032020" ~ "03/2020",
                                                                                               name == "allsummer21" ~ "Summer 21",
                                                                                               name == "all23" ~ "01/2023")) %>% mutate(context = "all Contacts")
          

## BOXPLOTS RESPONDENT
WorkDataRespondent <- data_reduced %>% select(respondent_work_2019, respondent_work_03_2020, respondent_work_summer_2021, respondent_work_01_2023) %>%
  pivot_longer(cols = c("respondent_work_2019", "respondent_work_03_2020", "respondent_work_summer_2021", "respondent_work_01_2023"))
WorkDataRespondent$name <- factor(WorkDataRespondent$name, levels = c("respondent_work_2019", "respondent_work_03_2020", "respondent_work_summer_2021", "respondent_work_01_2023"))
WorkDataRespondent$value <- as.integer(WorkDataRespondent$value)

WorkDataRespondent <- WorkDataRespondent %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "respondent_work_2019" ~ "2019",
                                                                                              name == "respondent_work_03_2020" ~ "03/2020",
                                                                                              name == "respondent_work_summer_2021" ~ "Summer 21",
                                                                                              name == "respondent_work_01_2023" ~ "01/2023")) %>%
                                                                      mutate(context = "work")

LeisureDataRespondent <- data_reduced %>% select(respondent_leisure_2019, respondent_leisure_03_2020, respondent_leisure_summer_2021, respondent_leisure_01_2023) %>% 
pivot_longer(cols = c("respondent_leisure_2019", "respondent_leisure_03_2020", "respondent_leisure_summer_2021", "respondent_leisure_01_2023"))
LeisureDataRespondent$name <- factor(LeisureDataRespondent$name, levels = c("respondent_leisure_2019", "respondent_leisure_03_2020", "respondent_leisure_summer_2021", "respondent_leisure_01_2023"))
LeisureDataRespondent$value <- as.integer(LeisureDataRespondent$value)

LeisureDataRespondent <- LeisureDataRespondent %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "respondent_leisure_2019" ~ "2019",
                                                                                               name == "respondent_leisure_03_2020" ~ "03/2020",
                                                                                               name == "respondent_leisure_summer_2021" ~ "Summer 21",
                                                                                               name == "respondent_leisure_01_2023" ~ "01/2023")) %>% 
                                                                                               mutate(context = "leisure")

allDataRespondent <- data_reduced %>% select(respondent_all_2019, respondent_all_03_2020, respondent_all_summer_2021, respondent_all_01_2023) %>% 
pivot_longer(cols = c("respondent_all_2019", "respondent_all_03_2020", "respondent_all_summer_2021", "respondent_all_01_2023"))
allDataRespondent$name <- factor(allDataRespondent$name, levels = c("respondent_all_2019", "respondent_all_03_2020", "respondent_all_summer_2021", "respondent_all_01_2023"))
allDataRespondent$value <- as.integer(allDataRespondent$value)

allDataRespondent <- allDataRespondent %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "respondent_all_2019" ~ "2019",
                                                                                               name == "respondent_all_03_2020" ~ "03/2020",
                                                                                               name == "respondent_all_summer_2021" ~ "Summer 21",
                                                                                               name == "respondent_all_01_2023" ~ "01/2023")) %>% 
                                                                                               mutate(context = "all")

data_full <- rbind(WorkDataRespondent, LeisureDataRespondent) 
data_full <- rbind(data_full, allDataRespondent)

data_full$time <- factor(data_full$time, levels = c("2019", "03/2020", "Summer 21", "01/2023"))
data_full <- data_full[order(data_full$time, decreasing = TRUE), ]

palette <- function() {
  c("#E4572E", "#29335C", "#F3A712", "#A8C686", "#669BBC")
}

leisure <- ggplot(data_full %>% filter(value < 200) %>% filter(context == "leisure"), aes(time, value)) +
  geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
   geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  #facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Reported # Of Contacts") +
  theme(text = element_text(size = 22))
  
ggsave("LeisureBoxplot.pdf", leisure, dpi = 500, w = 9, h = 9)
ggsave("LeisureBoxplot.png", leisure, dpi = 500, w = 9, h = 9)


work <- ggplot(data_full %>% filter(value < 200) %>% filter(context == "work"), aes(time, value)) +
  geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
   geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  #facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Reported # Of Contacts") +
  theme(text = element_text(size = 22))

ggsave("WorkBoxplot.pdf", work, dpi = 500, w = 9, h = 9)
ggsave("WorkBoxplot.png", work, dpi = 500, w = 9, h = 9)

all <- ggplot(data_full %>% filter(value < 250) %>% filter(context == "all") %>% filter(!is.na(time)), aes(time, value)) +
  geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
  geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  #facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Reported # Of Contacts") +
  theme(text = element_text(size = 22))

ggsave("AllBoxplot.pdf", all, dpi = 500, w = 9, h = 9)
ggsave("AllBoxplot.png", all, dpi = 500, w = 9, h = 9)

## Contact reductions of respondent
data_reduced <- data_reduced %>% mutate(respondent_work_rel_2019_2020 = 100/respondent_work_2019*respondent_work_03_2020) %>%
                                  mutate(respondent_work_rel_2019_2021 = 100/respondent_work_2019*respondent_work_summer_2021) %>%
                                  mutate(respondent_work_rel_2019_2023 = 100/respondent_work_2019*respondent_work_01_2023) %>%
                                  mutate(respondent_leisure_rel_2019_2020 = 100/respondent_leisure_2019*respondent_leisure_03_2020) %>%
                                  mutate(respondent_leisure_rel_2019_2021 = 100/respondent_leisure_2019*respondent_leisure_summer_2021) %>%
                                  mutate(respondent_leisure_rel_2019_2023 = 100/respondent_leisure_2019*respondent_leisure_01_2023) %>%
                                  mutate(respondent_all_rel_2019_2020 = 100/respondent_all_2019*respondent_all_03_2020) %>%
                                  mutate(respondent_all_rel_2019_2021 = 100/respondent_all_2019*respondent_all_summer_2021) %>%
                                  mutate(respondent_all_rel_2019_2023 = 100/respondent_all_2019*respondent_all_01_2023)
workedIn2019 <- data_reduced %>% filter(!(respondent_work_2019 == 0 & respondent_work_03_2020 > 0))
summary(workedIn2019$respondent_work_rel_2019_2020)
sum(!is.na(workedIn2019$respondent_work_rel_2019_2020))
workedIn2019 <- data_reduced %>% filter(!(respondent_work_2019 == 0 & respondent_work_summer_2021 > 0))
summary(workedIn2019$respondent_work_rel_2019_2021)
sum(!is.na(workedIn2019$respondent_work_rel_2019_2021))
workedIn2019 <- data_reduced %>% filter(!(respondent_work_2019 == 0 & respondent_work_01_2023 > 0))
summary(workedIn2019$respondent_work_rel_2019_2023)
sum(!is.na(workedIn2019$respondent_work_rel_2019_2023))

# Leisure CONTACTS
leisureIn2019 <- data_reduced %>% filter(!(respondent_leisure_2019 == 0 & respondent_leisure_03_2020 > 0))
summary(leisureIn2019$respondent_leisure_rel_2019_2020)
sum(!is.na(leisureIn2019$respondent_leisure_rel_2019_2020))
leisureIn2019 <- data_reduced %>% filter(!(respondent_leisure_2019 == 0 & respondent_leisure_summer_2021 > 0))
summary(leisureIn2019$respondent_leisure_rel_2019_2021)
sum(!is.na(leisureIn2019$respondent_leisure_rel_2019_2021))
leisureIn2019 <- data_reduced %>% filter(!(respondent_leisure_2019 == 0 & respondent_leisure_01_2023 > 0))
summary(leisureIn2019$respondent_leisure_rel_2019_2023)
sum(!is.na(leisureIn2019$respondent_leisure_rel_2019_2023))

# ALL CONTACTS
allIn2019 <- data_reduced %>% filter(!(respondent_all_2019 == 0 & respondent_all_03_2020 > 0))
summary(allIn2019$respondent_all_rel_2019_2020)
sum(!is.na(allIn2019$respondent_all_rel_2019_2020))
allIn2019 <- data_reduced %>% filter(!(respondent_all_2019 == 0 & respondent_all_summer_2021 > 0))
summary(allIn2019$respondent_all_rel_2019_2021)
sum(!is.na(allIn2019$respondent_all_rel_2019_2021))
allIn2019 <- data_reduced %>% filter(!(respondent_all_2019 == 0 & respondent_all_01_2023 > 0))
summary(allIn2019$respondent_all_rel_2019_2023)
sum(!is.na(allIn2019$respondent_all_rel_2019_2023))


WorkDataRespondent <- data_reduced %>% select(user_id, respondent_work_rel_2019_2020, respondent_work_rel_2019_2021, respondent_work_rel_2019_2023) %>%
  pivot_longer(cols = c("respondent_work_rel_2019_2020", "respondent_work_rel_2019_2021", "respondent_work_rel_2019_2023"))
WorkDataRespondent$name <- factor(WorkDataRespondent$name, levels = c("respondent_work_rel_2019_2020", "respondent_work_rel_2019_2021", "respondent_work_rel_2019_2023"))
WorkDataRespondent$value <- as.integer(WorkDataRespondent$value)

WorkDataRespondent <- WorkDataRespondent %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "respondent_work_rel_2019_2020" ~ "2020 (rel. to 2019)",
                                                                                              name == "respondent_work_rel_2019_2021" ~ "2021 (rel. to 2019)",
                                                                                              name == "respondent_work_rel_2019_2023" ~ "2023 (rel. to 2019)")) %>%
                                                                      mutate(context = "work")

work <-ggplot(WorkDataRespondent %>% filter(value < 1000), aes(time, value)) +
   geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
   geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  #facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Rel. # Of Work Contacts") +
  theme(text = element_text(size = 22))

ggsave("WorkBoxplotRel.pdf", work, dpi = 500, w = 9, h = 9)
ggsave("WorkBoxplotRel.png", work, dpi = 500, w = 9, h = 9)

LeisureDataRespondent <- data_reduced %>% select(user_id, respondent_leisure_rel_2019_2020, respondent_leisure_rel_2019_2021, respondent_leisure_rel_2019_2023) %>%
  pivot_longer(cols = c("respondent_leisure_rel_2019_2020", "respondent_leisure_rel_2019_2021", "respondent_leisure_rel_2019_2023"))
LeisureDataRespondent$name <- factor(LeisureDataRespondent$name, levels = c("respondent_leisure_rel_2019_2020", "respondent_leisure_rel_2019_2021", "respondent_leisure_rel_2019_2023"))
LeisureDataRespondent$value <- as.integer(LeisureDataRespondent$value)

LeisureDataRespondent <- LeisureDataRespondent %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "respondent_leisure_rel_2019_2020" ~ "2020 (rel. to 2019)",
                                                                                              name == "respondent_leisure_rel_2019_2021" ~ "2021 (rel. to 2019)",
                                                                                              name == "respondent_leisure_rel_2019_2023" ~ "2023 (rel. to 2019)")) %>%
                                                                      mutate(context = "leisure")

leisure <- ggplot(LeisureDataRespondent %>% filter(value < 1000), aes(time, value)) +
   geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
   geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  #facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Relative # Of Leisure Contacts") +
  theme(text = element_text(size = 22))

ggsave("LeisureBoxplotRel.pdf", leisure, dpi = 500, w = 9, h = 9)
ggsave("LeisureBoxplotRel.png", leisure, dpi = 500, w = 9, h = 9)

AllDataRespondent <- data_reduced %>% select(user_id, respondent_all_rel_2019_2020, respondent_all_rel_2019_2021, respondent_all_rel_2019_2023) %>%
  pivot_longer(cols = c("respondent_all_rel_2019_2020", "respondent_all_rel_2019_2021", "respondent_all_rel_2019_2023"))
AllDataRespondent$name <- factor(AllDataRespondent$name, levels = c("respondent_all_rel_2019_2020", "respondent_all_rel_2019_2021", "respondent_all_rel_2019_2023"))
AllDataRespondent$value <- as.integer(AllDataRespondent$value)

AllDataRespondent <- AllDataRespondent %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "respondent_all_rel_2019_2020" ~ "2020 (rel. to 2019)",
                                                                                              name == "respondent_all_rel_2019_2021" ~ "2021 (rel. to 2019)",
                                                                                              name == "respondent_all_rel_2019_2023" ~ "2023 (rel. to 2019)")) %>%
                                                                      mutate(context = "all")

all <- ggplot(AllDataRespondent %>% filter(value < 1000), aes(time, value)) +
  geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
  geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  #facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Relative # Of All Contacts") +
  theme(text = element_text(size = 22))

ggsave("AllBoxplotRel.pdf", all, dpi = 500, w = 9, h = 9)
ggsave("AllBoxplotRel.png", all, dpi = 500, w = 9, h = 9)

sum_stat_reductions <- data.frame(matrix(0, ncol = 5, nrow = 0))
colnames(sum_stat_reductions) <- c("RespondentHHCC", "Category", "PointInTime", "NoWhoReduced", "NoWhoAnswered")
sum_stat_reductions[nrow(sum_stat_reductions ) + 1, ] <- c("Respondent", "Work", 2020, length(which(data_reduced$respondent_work_rel_2019_2020 <= 100)), length(which(!is.na(data_reduced$respondent_work_rel_2019_2020))))
sum_stat_reductions[nrow(sum_stat_reductions ) + 1, ] <- c("Respondent", "Work", 2021, length(which(data_reduced$respondent_work_rel_2019_2021 <= 100)), length(which(!is.na(data_reduced$respondent_work_rel_2019_2021))))
sum_stat_reductions[nrow(sum_stat_reductions ) + 1, ] <- c("Respondent", "Work", 2023, length(which(data_reduced$respondent_work_rel_2019_2023 <= 100)), length(which(!is.na(data_reduced$respondent_work_rel_2019_2023))))
sum_stat_reductions[nrow(sum_stat_reductions ) + 1, ] <- c("Respondent", "Leisure", 2020, length(which(data_reduced$respondent_leisure_rel_2019_2020 <= 100)), length(which(!is.na(data_reduced$respondent_leisure_rel_2019_2020))))
sum_stat_reductions[nrow(sum_stat_reductions ) + 1, ] <- c("Respondent", "Leisure", 2021, length(which(data_reduced$respondent_leisure_rel_2019_2021 <= 100)), length(which(!is.na(data_reduced$respondent_leisure_rel_2019_2021))))
sum_stat_reductions[nrow(sum_stat_reductions ) + 1, ] <- c("Respondent", "Leisure", 2023, length(which(data_reduced$respondent_leisure_rel_2019_2023 <= 100)), length(which(!is.na(data_reduced$respondent_leisure_rel_2019_2023))))
sum_stat_reductions[nrow(sum_stat_reductions ) + 1, ] <- c("Respondent", "All", 2020, length(which(data_reduced$respondent_all_rel_2019_2020 <= 100)), length(which(!is.na(data_reduced$respondent_all_rel_2019_2020))))
sum_stat_reductions[nrow(sum_stat_reductions ) + 1, ] <- c("Respondent", "All", 2021, length(which(data_reduced$respondent_all_rel_2019_2021 <= 100)), length(which(!is.na(data_reduced$respondent_all_rel_2019_2021))))
sum_stat_reductions[nrow(sum_stat_reductions ) + 1, ] <- c("Respondent", "All", 2023, length(which(data_reduced$respondent_all_rel_2019_2023 <= 100)), length(which(!is.na(data_reduced$respondent_all_rel_2019_2023))))

sum_stat_reductions$NoWhoReduced <- as.integer(sum_stat_reductions$NoWhoReduced)
sum_stat_reductions$NoWhoAnswered <- as.integer(sum_stat_reductions$NoWhoAnswered)
sum_stat_reductions <- sum_stat_reductions %>% mutate(ShareWhoReduced = NoWhoReduced/NoWhoAnswered)

## CC'S CONTACTS
data_reduced <- data_reduced %>% mutate(cc_pre_all_2019 = cc_pre_hsld_size_2019 + cc_pre_school_2019 + cc_pre_work_2019 + cc_pre_leisure_2019) %>% 
  mutate(cc_pre_all_03_2020 = cc_pre_hsld_size_03_2020 + cc_pre_school_03_2020 + cc_pre_work_03_2020 + cc_pre_leisure_03_2020) %>%
  mutate(cc_pre_all_summer_2021 = cc_pre_hsld_size_summer_2021 + cc_pre_school_summer_2021 +cc_pre_work_summer_2021 + cc_pre_leisure_summer_2021) %>%
  mutate(cc_pre_all_01_2023 = cc_pre_hsld_size_01_2023 + cc_pre_school_01_2023 +cc_pre_work_01_2023 + cc_pre_leisure_01_2023)

# WORK CONTACTS
summary(data_reduced$cc_pre_work_2019)
sum(!is.na(data_reduced$cc_pre_work_2019))

summary(data_reduced$cc_pre_work_03_2020)
sum(!is.na(data_reduced$cc_pre_work_03_2020))
 
summary(data_reduced$cc_pre_work_summer_2021)
sum(!is.na(data_reduced$cc_pre_work_summer_2021))

summary(data_reduced$cc_pre_work_01_2023)
sum(!is.na(data_reduced$cc_pre_work_01_2023))

# Leisure CONTACTS
summary(data_reduced$cc_pre_leisure_2019)
sum(!is.na(data_reduced$cc_pre_leisure_2019))

summary(data_reduced$cc_pre_leisure_03_2020)
sum(!is.na(data_reduced$cc_pre_leisure_03_2020))
 
summary(data_reduced$cc_pre_leisure_summer_2021)
sum(!is.na(data_reduced$cc_pre_leisure_summer_2021))

summary(data_reduced$cc_pre_leisure_01_2023)
sum(!is.na(data_reduced$cc_pre_leisure_01_2023))

# ALL CONTACTS
summary(data_reduced$cc_pre_all_2019)
sum(!is.na(data_reduced$cc_pre_all_2019))

summary(data_reduced$cc_pre_all_03_2020)
sum(!is.na(data_reduced$cc_pre_all_03_2020))
 
summary(data_reduced$cc_pre_all_summer_2021)
sum(!is.na(data_reduced$cc_pre_all_summer_2021))

summary(data_reduced$cc_pre_all_01_2023)
sum(!is.na(data_reduced$cc_pre_all_01_2023))

## BOXPLOTS CC
WorkDatacc_pre <- data_reduced %>% select(user_id, cc_pre_work_2019, cc_pre_work_03_2020, cc_pre_work_summer_2021, cc_pre_work_01_2023) %>%
  pivot_longer(cols = c("cc_pre_work_2019", "cc_pre_work_03_2020", "cc_pre_work_summer_2021", "cc_pre_work_01_2023"))
WorkDatacc_pre$name <- factor(WorkDatacc_pre$name, levels = c("cc_pre_work_2019", "cc_pre_work_03_2020", "cc_pre_work_summer_2021", "cc_pre_work_01_2023"))
WorkDatacc_pre$value <- as.integer(WorkDatacc_pre$value)

WorkDatacc_pre <- WorkDatacc_pre %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "cc_pre_work_2019" ~ "2019",
                                                                                              name == "cc_pre_work_03_2020" ~ "03/2020",
                                                                                              name == "cc_pre_work_summer_2021" ~ "Summer 21",
                                                                                              name == "cc_pre_work_01_2023" ~ "01/2023")) %>%
                                                                      mutate(context = "work")

LeisureDatacc_pre <- data_reduced %>% select(user_id, cc_pre_leisure_2019, cc_pre_leisure_03_2020, cc_pre_leisure_summer_2021, cc_pre_leisure_01_2023) %>% 
pivot_longer(cols = c("cc_pre_leisure_2019", "cc_pre_leisure_03_2020", "cc_pre_leisure_summer_2021", "cc_pre_leisure_01_2023"))
LeisureDatacc_pre$name <- factor(LeisureDatacc_pre$name, levels = c("cc_pre_leisure_2019", "cc_pre_leisure_03_2020", "cc_pre_leisure_summer_2021", "cc_pre_leisure_01_2023"))
LeisureDatacc_pre$value <- as.integer(LeisureDatacc_pre$value)

LeisureDatacc_pre <- LeisureDatacc_pre %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "cc_pre_leisure_2019" ~ "2019",
                                                                                               name == "cc_pre_leisure_03_2020" ~ "03/2020",
                                                                                               name == "cc_pre_leisure_summer_2021" ~ "Summer 21",
                                                                                               name == "cc_pre_leisure_01_2023" ~ "01/2023")) %>% 
                                                                                               mutate(context = "leisure")

allDatacc_pre <- data_reduced %>% select(user_id, cc_pre_all_2019, cc_pre_all_03_2020, cc_pre_all_summer_2021, cc_pre_all_01_2023) %>% 
pivot_longer(cols = c("cc_pre_all_2019", "cc_pre_all_03_2020", "cc_pre_all_summer_2021", "cc_pre_all_01_2023"))
allDatacc_pre$name <- factor(allDatacc_pre$name, levels = c("cc_pre_all_2019", "cc_pre_all_03_2020", "cc_pre_all_summer_2021", "cc_pre_all_01_2023"))
allDatacc_pre$value <- as.integer(allDatacc_pre$value)

allDatacc_pre <- allDatacc_pre %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "cc_pre_all_2019" ~ "2019",
                                                                                               name == "cc_pre_all_03_2020" ~ "03/2020",
                                                                                               name == "cc_pre_all_summer_2021" ~ "Summer 21",
                                                                                               name == "cc_pre_all_01_2023" ~ "01/2023")) %>% 
                                                                                               mutate(context = "all")

data_full <- rbind(WorkDatacc_pre, LeisureDatacc_pre) 
data_full <- rbind(data_full, allDatacc_pre)

data_full$time <- factor(data_full$time, levels = c("2019", "03/2020", "Summer 21", "01/2023"))
data_full <- data_full[order(data_full$time, decreasing = TRUE), ]

palette <- function() {
  c("#E4572E", "#29335C", "#F3A712", "#A8C686", "#669BBC")
}

leisure <- ggplot(data_full %>% filter(value < 200) %>% filter(context == "leisure"), aes(time, value)) +
  geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
  geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  #facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Reported # Of Contacts") +
  theme(text = element_text(size = 22))
  
ggsave("LeisureBoxplotCCPre.pdf", leisure, dpi = 500, w = 9, h = 9)
ggsave("LeisureBoxplotCCPre.png", leisure, dpi = 500, w = 9, h = 9)


work <- ggplot(data_full %>% filter(value < 200) %>% filter(context == "work"), aes(time, value)) +
  geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
  geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  #facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Reported # Of Contacts") +
  theme(text = element_text(size = 22))

ggsave("WorkBoxplotCCPre.pdf", work, dpi = 500, w = 9, h = 9)
ggsave("WorkBoxplotCCPre.png", work, dpi = 500, w = 9, h = 9)

all <- ggplot(data_full %>% filter(value < 250) %>% filter(context == "all") %>% filter(!is.na(time)), aes(time, value)) +
  geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
  geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  #facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Reported # Of Contacts") +
  theme(text = element_text(size = 22))

ggsave("AllBoxplotCCPre.pdf", all, dpi = 500, w = 9, h = 9)
ggsave("AllBoxplotCCPre.png", all, dpi = 500, w = 9, h = 9)

## Contact reductions of CC (pre)
data_reduced <- data_reduced %>% mutate(cc_pre_work_rel_2019_2020 = 100/cc_pre_work_2019*cc_pre_work_03_2020) %>%
                                  mutate(cc_pre_work_rel_2019_2021 = 100/cc_pre_work_2019*cc_pre_work_summer_2021) %>%
                                  mutate(cc_pre_work_rel_2019_2023 = 100/cc_pre_work_2019*cc_pre_work_01_2023) %>%
                                  mutate(cc_pre_leisure_rel_2019_2020 = 100/cc_pre_leisure_2019*cc_pre_leisure_03_2020) %>%
                                  mutate(cc_pre_leisure_rel_2019_2021 = 100/cc_pre_leisure_2019*cc_pre_leisure_summer_2021) %>%
                                  mutate(cc_pre_leisure_rel_2019_2023 = 100/cc_pre_leisure_2019*cc_pre_leisure_01_2023) %>%
                                  mutate(cc_pre_all_rel_2019_2020 = 100/cc_pre_all_2019*cc_pre_all_03_2020) %>%
                                  mutate(cc_pre_all_rel_2019_2021 = 100/cc_pre_all_2019*cc_pre_all_summer_2021) %>%
                                  mutate(cc_pre_all_rel_2019_2023 = 100/cc_pre_all_2019*cc_pre_all_01_2023)

workedIn2019 <- data_reduced %>% filter(!(cc_pre_work_2019 == 0 & cc_pre_work_03_2020 > 0))
summary(workedIn2019$cc_pre_work_rel_2019_2020)
sum(!is.na(workedIn2019$cc_pre_work_rel_2019_2020))
workedIn2019 <- data_reduced %>% filter(!(cc_pre_work_2019 == 0 & cc_pre_work_summer_2021 > 0))
summary(workedIn2019$cc_pre_work_rel_2019_2021)
sum(!is.na(workedIn2019$cc_pre_work_rel_2019_2021))
workedIn2019 <- data_reduced %>% filter(!(cc_pre_work_2019 == 0 & cc_pre_work_01_2023 > 0))
summary(workedIn2019$cc_pre_work_rel_2019_2023)
sum(!is.na(workedIn2019$cc_pre_work_rel_2019_2023))

# Leisure CONTACTS
leisureIn2019 <- data_reduced %>% filter(!(cc_pre_leisure_2019 == 0 & cc_pre_leisure_03_2020 > 0))
summary(leisureIn2019$cc_pre_leisure_rel_2019_2020)
sum(!is.na(leisureIn2019$cc_pre_leisure_rel_2019_2020))
leisureIn2019 <- data_reduced %>% filter(!(cc_pre_leisure_2019 == 0 & cc_pre_leisure_summer_2021 > 0))
summary(leisureIn2019$cc_pre_leisure_rel_2019_2021)
sum(!is.na(leisureIn2019$cc_pre_leisure_rel_2019_2021))
leisureIn2019 <- data_reduced %>% filter(!(cc_pre_leisure_2019 == 0 & cc_pre_leisure_01_2023 > 0))
summary(leisureIn2019$cc_pre_leisure_rel_2019_2023)
sum(!is.na(leisureIn2019$cc_pre_leisure_rel_2019_2023))

# ALL CONTACTS
allIn2019 <- data_reduced %>% filter(!(cc_pre_all_2019 == 0 & cc_pre_all_03_2020 > 0))
summary(allIn2019$cc_pre_all_rel_2019_2020)
sum(!is.na(allIn2019$cc_pre_all_rel_2019_2020))
allIn2019 <- data_reduced %>% filter(!(cc_pre_all_2019 == 0 & cc_pre_all_summer_2021 > 0))
summary(allIn2019$cc_pre_all_rel_2019_2021)
sum(!is.na(allIn2019$cc_pre_all_rel_2019_2021))
allIn2019 <- data_reduced %>% filter(!(cc_pre_all_2019 == 0 & cc_pre_all_01_2023 > 0))
summary(allIn2019$cc_pre_all_rel_2019_2023)
sum(!is.na(allIn2019$cc_pre_all_rel_2019_2023))

WorkDatacc_pre <- data_reduced %>% select(user_id, cc_pre_work_rel_2019_2020, cc_pre_work_rel_2019_2021, cc_pre_work_rel_2019_2023) %>%
  pivot_longer(cols = c("cc_pre_work_rel_2019_2020", "cc_pre_work_rel_2019_2021", "cc_pre_work_rel_2019_2023"))
WorkDatacc_pre$name <- factor(WorkDatacc_pre$name, levels = c("cc_pre_work_rel_2019_2020", "cc_pre_work_rel_2019_2021", "cc_pre_work_rel_2019_2023"))
WorkDatacc_pre$value <- as.integer(WorkDatacc_pre$value)

WorkDatacc_pre <- WorkDatacc_pre %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "cc_pre_work_rel_2019_2020" ~ "2020 (rel. to 2019)",
                                                                                              name == "cc_pre_work_rel_2019_2021" ~ "2021 (rel. to 2019)",
                                                                                              name == "cc_pre_work_rel_2019_2023" ~ "2023 (rel. to 2019)")) %>%
                                                                      mutate(context = "work")

LeisureDatacc_pre <- data_reduced %>% select(user_id, cc_pre_leisure_rel_2019_2020, cc_pre_leisure_rel_2019_2021, cc_pre_leisure_rel_2019_2023) %>% 
pivot_longer(cols = c("cc_pre_leisure_rel_2019_2020", "cc_pre_leisure_rel_2019_2021", "cc_pre_leisure_rel_2019_2023"))
LeisureDatacc_pre$name <- factor(LeisureDatacc_pre$name, levels = c("cc_pre_leisure_rel_2019_2020", "cc_pre_leisure_rel_2019_2021", "cc_pre_leisure_rel_2019_2023"))
LeisureDatacc_pre$value <- as.integer(LeisureDatacc_pre$value)

LeisureDatacc_pre <- LeisureDatacc_pre %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "cc_pre_leisure_rel_2019_2020" ~ "2020 (rel. to 2019)",
                                                                                               name == "cc_pre_leisure_rel_2019_2021" ~ "2021 (rel. to 2019)",
                                                                                               name == "cc_pre_leisure_rel_2019_2023" ~ "2023 (rel. to 2019)")) %>% 
                                                                                               mutate(context = "leisure")

allDatacc_pre <- data_reduced %>% select(user_id, cc_pre_all_rel_2019_2020, cc_pre_all_rel_2019_2021, cc_pre_all_rel_2019_2023) %>% 
pivot_longer(cols = c("cc_pre_all_rel_2019_2020", "cc_pre_all_rel_2019_2021", "cc_pre_all_rel_2019_2023"))
allDatacc_pre$name <- factor(allDatacc_pre$name, levels = c("cc_pre_all_rel_2019_2020", "cc_pre_all_rel_2019_2021", "cc_pre_all_rel_2019_2023"))
allDatacc_pre$value <- as.integer(allDatacc_pre$value)

allDatacc_pre <- allDatacc_pre %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "cc_pre_all_rel_2019_2020" ~ "2020 (rel. to 2019)",
                                                                                               name == "cc_pre_all_rel_2019_2021" ~ "2021 (rel. to 2019)",
                                                                                               name == "cc_pre_all_rel_2019_2023" ~ "2023 (rel. to 2019)")) %>% 
                                                                                               mutate(context = "all")

data_full <- rbind(WorkDatacc_pre, LeisureDatacc_pre) 
data_full <- rbind(data_full, allDatacc_pre)

data_full$time <- factor(data_full$time, levels = c("2020 (rel. to 2019)", "2021 (rel. to 2019)", "2023 (rel. to 2019)"))
data_full <- data_full[order(data_full$time, decreasing = TRUE), ]

palette <- function() {
  c("#E4572E", "#29335C", "#F3A712", "#A8C686", "#669BBC")
}

leisure <- ggplot(data_full %>% filter(value < 200) %>% filter(context == "leisure"), aes(time, value)) +
  geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
  geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  #facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Rel. # Of Work\nContacts (CC, pre)") +
  theme(text = element_text(size = 22))
  
ggsave("LeisureBoxplotCCPreRel.pdf", leisure, dpi = 500, w = 9, h = 9)
ggsave("LeisureBoxplotCCPreRel.png", leisure, dpi = 500, w = 9, h = 9)


work <- ggplot(data_full %>% filter(value < 200) %>% filter(context == "work"), aes(time, value)) +
  geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
  geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  #facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Rel. # Of Work\nContacts (CC, pre)") +
  theme(text = element_text(size = 22))

ggsave("WorkBoxplotCCPreRel.pdf", work, dpi = 500, w = 9, h = 9)
ggsave("WorkBoxplotCCPreRel.png", work, dpi = 500, w = 9, h = 9)

all <- ggplot(data_full %>% filter(value < 250) %>% filter(context == "all") %>% filter(!is.na(time)), aes(time, value)) +
  geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
  geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  #facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Rel. # Of All \nContacts (CC, pre)") +
  theme(text = element_text(size = 22)) +

ggsave("AllBoxplotCCPreRel.pdf", all, dpi = 500, w = 9, h = 9)
ggsave("AllBoxplotCCPreRel.png", all, dpi = 500, w = 9, h = 9)


## Household members's CONTACTS
# WORK CONTACTS
summary(data_reduced$hhmember_work_2019)
sum(!is.na(data_reduced$hhmember_work_2019))

summary(data_reduced$hhmember_work_03_2020)
sum(!is.na(data_reduced$hhmember_work_03_2020))
 
summary(data_reduced$hhmember_work_summer_2021)
sum(!is.na(data_reduced$hhmember_work_summer_2021))

summary(data_reduced$hhmember_work_01_2023)
sum(!is.na(data_reduced$hhmember_work_01_2023))

# Leisure CONTACTS
summary(data_reduced$hhmember_leisure_2019)
sum(!is.na(data_reduced$hhmember_leisure_2019))

summary(data_reduced$hhmember_leisure_03_2020)
sum(!is.na(data_reduced$hhmember_leisure_03_2020))
 
summary(data_reduced$hhmember_leisure_summer_2021)
sum(!is.na(data_reduced$hhmember_leisure_summer_2021))

summary(data_reduced$hhmember_leisure_01_2023)
sum(!is.na(data_reduced$hhmember_leisure_01_2023))

# ALL CONTACTS
summary(data_reduced$hhmember_all_2019)
sum(!is.na(data_reduced$hhmember_all_2019))

summary(data_reduced$hhmember_all_03_2020)
sum(!is.na(data_reduced$hhmember_all_03_2020))
 
summary(data_reduced$hhmember_all_summer_2021)
sum(!is.na(data_reduced$hhmember_all_summer_2021))

summary(data_reduced$hhmember_all_01_2023)
sum(!is.na(data_reduced$hhmember_all_01_2023))

## BOXPLOTS CC
WorkDatahhmember <- data_reduced %>% select(hhmember_work_2019, hhmember_work_03_2020, hhmember_work_summer_2021, hhmember_work_01_2023) %>%
  pivot_longer(cols = c("hhmember_work_2019", "hhmember_work_03_2020", "hhmember_work_summer_2021", "hhmember_work_01_2023"))
WorkDatahhmember$name <- factor(WorkDatahhmember$name, levels = c("hhmember_work_2019", "hhmember_work_03_2020", "hhmember_work_summer_2021", "hhmember_work_01_2023"))
WorkDatahhmember$value <- as.integer(WorkDatahhmember$value)

WorkDatahhmember <- WorkDatahhmember %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "hhmember_work_2019" ~ "2019",
                                                                                              name == "hhmember_work_03_2020" ~ "03/2020",
                                                                                              name == "hhmember_work_summer_2021" ~ "Summer 21",
                                                                                              name == "hhmember_work_01_2023" ~ "01/2023")) %>%
                                                                      mutate(context = "work")

LeisureDatahhmember <- data_reduced %>% select(user_id, hhmember_leisure_2019, hhmember_leisure_03_2020, hhmember_leisure_summer_2021, hhmember_leisure_01_2023) %>% 
pivot_longer(cols = c("hhmember_leisure_2019", "hhmember_leisure_03_2020", "hhmember_leisure_summer_2021", "hhmember_leisure_01_2023"))
LeisureDatahhmember$name <- factor(LeisureDatahhmember$name, levels = c("hhmember_leisure_2019", "hhmember_leisure_03_2020", "hhmember_leisure_summer_2021", "hhmember_leisure_01_2023"))
LeisureDatahhmember$value <- as.integer(LeisureDatahhmember$value)

LeisureDatahhmember <- LeisureDatahhmember %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "hhmember_leisure_2019" ~ "2019",
                                                                                               name == "hhmember_leisure_03_2020" ~ "03/2020",
                                                                                               name == "hhmember_leisure_summer_2021" ~ "Summer 21",
                                                                                               name == "hhmember_leisure_01_2023" ~ "01/2023")) %>% 
                                                                                               mutate(context = "leisure")

allDatahhmember <- data_reduced %>% select(user_id, hhmember_all_2019, hhmember_all_03_2020, hhmember_all_summer_2021, hhmember_all_01_2023) %>% 
pivot_longer(cols = c("hhmember_all_2019", "hhmember_all_03_2020", "hhmember_all_summer_2021", "hhmember_all_01_2023"))
allDatahhmember$name <- factor(allDatahhmember$name, levels = c("hhmember_all_2019", "hhmember_all_03_2020", "hhmember_all_summer_2021", "hhmember_all_01_2023"))
allDatahhmember$value <- as.integer(allDatahhmember$value)

allDatahhmember <- allDatahhmember %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "hhmember_all_2019" ~ "2019",
                                                                                               name == "hhmember_all_03_2020" ~ "03/2020",
                                                                                               name == "hhmember_all_summer_2021" ~ "Summer 21",
                                                                                               name == "hhmember_all_01_2023" ~ "01/2023")) %>% 
                                                                                               mutate(context = "all")

data_full <- rbind(WorkDatahhmember, LeisureDatahhmember) 
data_full <- rbind(data_full, allDatahhmember)

data_full$time <- factor(data_full$time, levels = c("2019", "03/2020", "Summer 21", "01/2023"))
data_full <- data_full[order(data_full$time, decreasing = TRUE), ]

palette <- function() {
  c("#E4572E", "#29335C", "#F3A712", "#A8C686", "#669BBC")
}

leisure <- ggplot(data_full %>% filter(value < 200) %>% filter(context == "leisure"), aes(time, value)) +
  geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
  geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  #facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Reported # Of Contacts") +
  theme(text = element_text(size = 22))
  
ggsave("LeisureBoxplotHHMember.pdf", leisure, dpi = 500, w = 9, h = 9)
ggsave("LeisureBoxplotHHMember.png", leisure, dpi = 500, w = 9, h = 9)


work <- ggplot(data_full %>% filter(value < 200) %>% filter(context == "work"), aes(time, value)) +
  geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
  geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  #facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Reported # Of Contacts") +
  theme(text = element_text(size = 22))

ggsave("WorkBoxplotHHMember.pdf", work, dpi = 500, w = 9, h = 9)
ggsave("WorkBoxplotHHMember.png", work, dpi = 500, w = 9, h = 9)

all <- ggplot(data_full %>% filter(value < 250) %>% filter(context == "all") %>% filter(!is.na(time)), aes(time, value)) +
  geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
  geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Reported # Of Contacts") +
  theme(text = element_text(size = 22))

ggsave("AllBoxplotHHMember.pdf", all, dpi = 500, w = 9, h = 9)
ggsave("AllBoxplotHHMember.png", all, dpi = 500, w = 9, h = 9)

## Contact reductions of household member
data_reduced <- data_reduced %>% mutate(hhmember_work_rel_2019_2020 = 100/hhmember_work_2019*hhmember_work_03_2020) %>%
                                  mutate(hhmember_work_rel_2019_2021 = 100/hhmember_work_2019*hhmember_work_summer_2021) %>%
                                  mutate(hhmember_work_rel_2019_2023 = 100/hhmember_work_2019*hhmember_work_01_2023) %>%
                                  mutate(hhmember_leisure_rel_2019_2020 = 100/hhmember_leisure_2019*hhmember_leisure_03_2020) %>%
                                  mutate(hhmember_leisure_rel_2019_2021 = 100/hhmember_leisure_2019*hhmember_leisure_summer_2021) %>%
                                  mutate(hhmember_leisure_rel_2019_2023 = 100/hhmember_leisure_2019*hhmember_leisure_01_2023) %>%
                                  mutate(hhmember_all_rel_2019_2020 = 100/hhmember_all_2019*hhmember_all_03_2020) %>%
                                  mutate(hhmember_all_rel_2019_2021 = 100/hhmember_all_2019*hhmember_all_summer_2021) %>%
                                  mutate(hhmember_all_rel_2019_2023 = 100/hhmember_all_2019*hhmember_all_01_2023)

workedIn2019 <- data_reduced %>% filter(!(hhmember_work_2019 == 0 & hhmember_work_03_2020 > 0))
summary(workedIn2019$hhmember_work_rel_2019_2020)
sum(!is.na(workedIn2019$hhmember_work_rel_2019_2020))
workedIn2019 <- data_reduced %>% filter(!(hhmember_work_2019 == 0 & hhmember_work_summer_2021 > 0))
summary(workedIn2019$hhmember_work_rel_2019_2021)
sum(!is.na(workedIn2019$hhmember_work_rel_2019_2021))
workedIn2019 <- data_reduced %>% filter(!(hhmember_work_2019 == 0 & hhmember_work_01_2023 > 0))
summary(workedIn2019$hhmember_work_rel_2019_2023)
sum(!is.na(workedIn2019$hhmember_work_rel_2019_2023))

# Leisure CONTACTS
leisureIn2019 <- data_reduced %>% filter(!(hhmember_leisure_2019 == 0 & hhmember_leisure_03_2020 > 0))
summary(leisureIn2019$hhmember_leisure_rel_2019_2020)
sum(!is.na(leisureIn2019$hhmember_leisure_rel_2019_2020))
leisureIn2019 <- data_reduced %>% filter(!(hhmember_leisure_2019 == 0 & hhmember_leisure_summer_2021 > 0))
summary(leisureIn2019$hhmember_leisure_rel_2019_2021)
sum(!is.na(leisureIn2019$hhmember_leisure_rel_2019_2021))
leisureIn2019 <- data_reduced %>% filter(!(hhmember_leisure_2019 == 0 & hhmember_leisure_01_2023 > 0))
summary(leisureIn2019$hhmember_leisure_rel_2019_2023)
sum(!is.na(leisureIn2019$hhmember_leisure_rel_2019_2023))

# ALL CONTACTS
allIn2019 <- data_reduced %>% filter(!(hhmember_all_2019 == 0 & hhmember_all_03_2020 > 0))
summary(allIn2019$hhmember_all_rel_2019_2020)
sum(!is.na(allIn2019$hhmember_all_rel_2019_2020))
allIn2019 <- data_reduced %>% filter(!(hhmember_all_2019 == 0 & hhmember_all_summer_2021 > 0))
summary(allIn2019$hhmember_all_rel_2019_2021)
sum(!is.na(allIn2019$hhmember_all_rel_2019_2021))
allIn2019 <- data_reduced %>% filter(!(hhmember_all_2019 == 0 & hhmember_all_01_2023 > 0))
summary(allIn2019$hhmember_all_rel_2019_2023)
sum(!is.na(allIn2019$hhmember_all_rel_2019_2023))

WorkDatahhmember <- data_reduced %>% select(user_id, hhmember_work_rel_2019_2020, hhmember_work_rel_2019_2021, hhmember_work_rel_2019_2023) %>%
  pivot_longer(cols = c("hhmember_work_rel_2019_2020", "hhmember_work_rel_2019_2021", "hhmember_work_rel_2019_2023"))
WorkDatahhmember$name <- factor(WorkDatahhmember$name, levels = c("hhmember_work_rel_2019_2020", "hhmember_work_rel_2019_2021", "hhmember_work_rel_2019_2023"))
WorkDatahhmember$value <- as.integer(WorkDatahhmember$value)

WorkDatahhmember <- WorkDatahhmember %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "hhmember_work_rel_2019_2020" ~ "2020 (rel. to 2019)",
                                                                                              name == "hhmember_work_rel_2019_2021" ~ "2021 (rel. to 2019)",
                                                                                              name == "hhmember_work_rel_2019_2023" ~ "2023 (rel. to 2019)")) %>%
                                                                      mutate(context = "work")

LeisureDatahhmember <- data_reduced %>% select(user_id, hhmember_leisure_rel_2019_2020, hhmember_leisure_rel_2019_2021, hhmember_leisure_rel_2019_2023) %>% 
pivot_longer(cols = c("hhmember_leisure_rel_2019_2020", "hhmember_leisure_rel_2019_2021", "hhmember_leisure_rel_2019_2023"))
LeisureDatahhmember$name <- factor(LeisureDatahhmember$name, levels = c("hhmember_leisure_rel_2019_2020", "hhmember_leisure_rel_2019_2021", "hhmember_leisure_rel_2019_2023"))
LeisureDatahhmember$value <- as.integer(LeisureDatahhmember$value)

LeisureDatahhmember <- LeisureDatahhmember %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "hhmember_leisure_rel_2019_2020" ~ "2020 (rel. to 2019)",
                                                                                               name == "hhmember_leisure_rel_2019_2021" ~ "2021 (rel. to 2019)",
                                                                                               name == "hhmember_leisure_rel_2019_2023" ~ "2023 (rel. to 2019)")) %>% 
                                                                                               mutate(context = "leisure")

allDatahhmember <- data_reduced %>% select(user_id, hhmember_all_rel_2019_2020, hhmember_all_rel_2019_2021, hhmember_all_rel_2019_2023) %>% 
pivot_longer(cols = c("hhmember_all_rel_2019_2020", "hhmember_all_rel_2019_2021", "hhmember_all_rel_2019_2023"))
allDatahhmember$name <- factor(allDatahhmember$name, levels = c("hhmember_all_rel_2019_2020", "hhmember_all_rel_2019_2021", "hhmember_all_rel_2019_2023"))
allDatahhmember$value <- as.integer(allDatahhmember$value)

allDatahhmember <- allDatahhmember %>% filter(!is.na(value)) %>% mutate(time = case_when(name == "hhmember_all_rel_2019_2020" ~ "2020 (rel. to 2019)",
                                                                                               name == "hhmember_all_rel_2019_2021" ~ "2021 (rel. to 2019)",
                                                                                               name == "hhmember_all_rel_2019_2023" ~ "2023 (rel. to 2019)")) %>% 
                                                                                               mutate(context = "all")

data_full <- rbind(WorkDatahhmember, LeisureDatahhmember) 
data_full <- rbind(data_full, allDatahhmember)

data_full$time <- factor(data_full$time, levels = c("2020 (rel. to 2019)", "2021 (rel. to 2019)", "2023 (rel. to 2019)"))
data_full <- data_full[order(data_full$time, decreasing = TRUE), ]

palette <- function() {
  c("#E4572E", "#29335C", "#F3A712", "#A8C686", "#669BBC")
}

leisure <- ggplot(data_full %>% filter(value < 200) %>% filter(context == "leisure"), aes(time, value)) +
  geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
  geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  #facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Rel. # Of Work\nContacts (CC, pre)") +
  theme(text = element_text(size = 22))
  
ggsave("LeisureBoxplotHHMemberRel.pdf", leisure, dpi = 500, w = 9, h = 9)
ggsave("LeisureBoxplotHHmemberRel.png", leisure, dpi = 500, w = 9, h = 9)


work <- ggplot(data_full %>% filter(value < 200) %>% filter(context == "work"), aes(time, value)) +
  geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
  geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  #facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Rel. # Of Work\nContacts (CC, pre)") +
  theme(text = element_text(size = 22))

ggsave("WorkBoxplotHHmemberRel.pdf", work, dpi = 500, w = 9, h = 9)
ggsave("WorkBoxplotHHmemberRel.png", work, dpi = 500, w = 9, h = 9)

all <- ggplot(data_full %>% filter(value < 250) %>% filter(context == "all") %>% filter(!is.na(time)), aes(time, value)) +
  geom_violin(color = "#29335C", width = 1.3, trim = FALSE, position=position_dodge(0.9)) + 
  geom_boxplot(color = "#29335C", width = 0.03, position = position_dodge(0.9)) +
  #facet_wrap(~time) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  xlab("Point In Time") +
  ylab("Rel. # Of All \nContacts (CC, pre)") +
  theme(text = element_text(size = 22))

ggsave("AllBoxplotHHmemberRel.pdf", all, dpi = 500, w = 9, h = 9)
ggsave("AllBoxplotHHmemberRel.png", all, dpi = 500, w = 9, h = 9)


# --- Turning data into tidy format

source("DataCleaningPrepForContactAnalysis.R")

data_reduced %>% count(age_bracket)

#data_reduced_children <- data_reduced %>% filter(hhmember_school_2019 > 0)

data_reduced <- data_reduced %>% select(-c(respondent_hsld_size_persons_under_14, number_of_children_under_18)) %>%
                                  select(-contains("attitudes")) %>%
                                  select(-contains("beh_change"))

data_reduced <- data_reduced %>% pivot_longer(cols = 4:78)

data_reduced <- data_reduced  %>% mutate(time = case_when(str_detect(name, "2019") ~ "2019",
                                                          str_detect(name, "2020") ~ "03/2020",
                                                          str_detect(name, "2021") ~ "Summer 2021",
                                                          str_detect(name, "2023") ~ "01/2023")) %>%
                                  mutate(WhoseContacts = case_when(str_detect(name, "respondent") ~ "Respondent",
                                  str_detect(name, "cc_pre") ~ "Closest Contact (Pre-Covid)",
                                  str_detect(name, "cc_during") ~ "Closest Contact (During-Covid)",
                                  str_detect(name, "hhmember") ~ "Household Member")) %>%
                                  mutate(TypeOfContact = case_when(str_detect(name, "work") ~ "Work",
                                  str_detect(name, "school") ~ "School",
                                  str_detect(name, "leisure") ~ "Leisure",
                                  str_detect(name, "all") ~ "All"))

data_reduced$time <- factor(data_reduced$time, levels = c("2019", "03/2020", "Summer 2021", "01/2023"))
data_reduced$TypeOfContact <- factor(data_reduced$TypeOfContact, levels = c("Work", "Leisure", "School", "All"))
data_reduced$WhoseContacts <- factor(data_reduced$WhoseContacts, levels = c("Respondent", "Household Member", "Closest Contact (Pre-Covid)", "Closest Contact (During-Covid)"))

palette <- function() {
  c("#006BA6", "#FFBC42", "#8F2D56", "#C93E78")
}

ggplot(data_reduced %>% filter(value < 100) %>% filter(!is.na(TypeOfContact)), aes(WhoseContacts, value)) +
  #geom_violin(aes(color = WhoseContacts), width = 1, trim = FALSE, position=position_dodge(0.9)) + 
  #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
  geom_boxplot(aes(color = WhoseContacts), size = 1.3) +
  scale_color_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols= vars(time)) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  ylab("Reported # Of Contacts") +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(panel.spacing = unit(0.8, "cm", data = NULL))
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("CollectionBoxplots.pdf", dpi = 500, w = 13, h = 16)
ggsave("CollectionBoxplots.png", dpi = 500, w = 13, h = 16)

palette <- function() {
  c("#fd5901", "#f78104", "#faab36", "#249ea0", "#008083", "#005f60")
}

ggplot(data_reduced %>% filter(WhoseContacts == "Respondent") %>% filter(value < 100) %>% filter(age_bracket != "80-90") %>% filter(!is.na(age_bracket)) %>% filter(!is.na(TypeOfContact)), aes(age_bracket, value)) +
  #geom_violin(aes(color = WhoseContacts), width = 1, trim = FALSE, position=position_dodge(0.9)) + 
  #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
  geom_boxplot(aes(color = age_bracket), size = 1.3) +
  scale_color_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols= vars(time)) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  ylab("Reported # Of Contacts") +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(panel.spacing = unit(0.8, "cm", data = NULL))
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("CollectionBoxplotsAgeBrackets.pdf", dpi = 500, w = 13, h = 16)
ggsave("CollectionBoxplotsAgeBrackets.png", dpi = 500, w = 13, h = 16)

#Subanalysis for school children
data_reduced_children <- data_reduced_children %>% select(-c(respondent_hsld_size_persons_under_14, number_of_children_under_18)) %>%
                                  select(-contains("attitudes")) %>%
                                  select(-contains("beh_change"))

data_reduced_children <- data_reduced_children %>% pivot_longer(cols = 2:76)

data_reduced_children <- data_reduced_children  %>% mutate(time = case_when(str_detect(name, "2019") ~ "2019",
                                                          str_detect(name, "2020") ~ "03/2020",
                                                          str_detect(name, "2021") ~ "Summer 2021",
                                                          str_detect(name, "2023") ~ "01/2023")) %>%
                                  mutate(WhoseContacts = case_when(str_detect(name, "respondent") ~ "Respondent",
                                  str_detect(name, "cc_pre") ~ "Closest Contact (Pre-Covid)",
                                  str_detect(name, "cc_during") ~ "Closest Contact (During-Covid)",
                                  str_detect(name, "hhmember") ~ "Household Member")) %>%
                                  mutate(TypeOfContact = case_when(str_detect(name, "work") ~ "Work",
                                  str_detect(name, "school") ~ "School",
                                  str_detect(name, "leisure") ~ "Leisure",
                                  str_detect(name, "all") ~ "All"))

data_reduced_children$time <- factor(data_reduced_children$time, levels = c("2019", "03/2020", "Summer 2021", "01/2023"))
data_reduced_children$TypeOfContact <- factor(data_reduced_children$TypeOfContact, levels = c("Work", "Leisure", "School", "All"))
data_reduced_children$WhoseContacts <- factor(data_reduced_children$WhoseContacts, levels = c("Respondent", "Household Member", "Closest Contact (Pre-Covid)", "Closest Contact (During-Covid)"))

ggplot(data_reduced_children %>% filter(WhoseContacts == "Household Member") %>% filter(value < 100) %>% filter(!is.na(TypeOfContact)), aes(WhoseContacts, value)) +
  #geom_violin(aes(color = WhoseContacts), width = 1, trim = FALSE, position=position_dodge(0.9)) + 
  #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
  geom_boxplot(color = "#FFBC42", size = 1.3) +
  scale_color_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols= vars(time)) +
  theme_minimal() +
  ylab("Reported # Of Contacts") +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(panel.spacing = unit(0.8, "cm", data = NULL))
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("CollectionBoxplotsChildren.pdf", dpi = 500, w = 8.5, h = 12)
ggsave("CollectionBoxplotsChildren.png", dpi = 500, w = 8.5, h = 12)

# Relative no. of contacts

source("DataCleaningPrepForContactAnalysis.R")

#data_reduced <- data_reduced %>% filter(hhmember_school_2019 > 0)

data_reduced <- data_reduced %>% mutate(respondent_work_rel_2019_2020 = 100/respondent_work_2019*respondent_work_03_2020) %>%
                                  mutate(respondent_work_rel_2019_2021 = 100/respondent_work_2019*respondent_work_summer_2021) %>%
                                  mutate(respondent_work_rel_2019_2023 = 100/respondent_work_2019*respondent_work_01_2023) %>%
                                  mutate(respondent_school_rel_2019_2020 = 100/respondent_school_2019*respondent_school_03_2020) %>%
                                  mutate(respondent_school_rel_2019_2021 = 100/respondent_school_2019*respondent_school_summer_2021) %>%
                                  mutate(respondent_school_rel_2019_2023 = 100/respondent_school_2019*respondent_school_01_2023) %>%
                                  mutate(respondent_leisure_rel_2019_2020 = 100/respondent_leisure_2019*respondent_leisure_03_2020) %>%
                                  mutate(respondent_leisure_rel_2019_2021 = 100/respondent_leisure_2019*respondent_leisure_summer_2021) %>%
                                  mutate(respondent_leisure_rel_2019_2023 = 100/respondent_leisure_2019*respondent_leisure_01_2023) %>%
                                  mutate(respondent_all_rel_2019_2020 = 100/respondent_all_2019*respondent_all_03_2020) %>%
                                  mutate(respondent_all_rel_2019_2021 = 100/respondent_all_2019*respondent_all_summer_2021) %>%
                                  mutate(respondent_all_rel_2019_2023 = 100/respondent_all_2019*respondent_all_01_2023)

data_reduced <- data_reduced %>% mutate(hhmember_work_rel_2019_2020 = 100/hhmember_work_2019*hhmember_work_03_2020) %>%
                                  mutate(hhmember_work_rel_2019_2021 = 100/hhmember_work_2019*hhmember_work_summer_2021) %>%
                                  mutate(hhmember_work_rel_2019_2023 = 100/hhmember_work_2019*hhmember_work_01_2023) %>%
                                  mutate(hhmember_school_rel_2019_2020 = 100/hhmember_school_2019*hhmember_school_03_2020) %>%
                                  mutate(hhmember_school_rel_2019_2021 = 100/hhmember_school_2019*hhmember_school_summer_2021) %>%
                                  mutate(hhmember_school_rel_2019_2023 = 100/hhmember_school_2019*hhmember_school_01_2023) %>%
                                  mutate(hhmember_leisure_rel_2019_2020 = 100/hhmember_leisure_2019*hhmember_leisure_03_2020) %>%
                                  mutate(hhmember_leisure_rel_2019_2021 = 100/hhmember_leisure_2019*hhmember_leisure_summer_2021) %>%
                                  mutate(hhmember_leisure_rel_2019_2023 = 100/hhmember_leisure_2019*hhmember_leisure_01_2023) %>%
                                  mutate(hhmember_all_rel_2019_2020 = 100/hhmember_all_2019*hhmember_all_03_2020) %>%
                                  mutate(hhmember_all_rel_2019_2021 = 100/hhmember_all_2019*hhmember_all_summer_2021) %>%
                                  mutate(hhmember_all_rel_2019_2023 = 100/hhmember_all_2019*hhmember_all_01_2023)

data_reduced <- data_reduced %>% mutate(cc_pre_work_rel_2019_2020 = 100/cc_pre_work_2019*cc_pre_work_03_2020) %>%
                                  mutate(cc_pre_work_rel_2019_2021 = 100/cc_pre_work_2019*cc_pre_work_summer_2021) %>%
                                  mutate(cc_pre_work_rel_2019_2023 = 100/cc_pre_work_2019*cc_pre_work_01_2023) %>%
                                  mutate(cc_pre_school_rel_2019_2020 = 100/cc_pre_school_2019*cc_pre_school_03_2020) %>%
                                  mutate(cc_pre_school_rel_2019_2021 = 100/cc_pre_school_2019*cc_pre_school_summer_2021) %>%
                                  mutate(cc_pre_school_rel_2019_2023 = 100/cc_pre_school_2019*cc_pre_school_01_2023) %>%
                                  mutate(cc_pre_leisure_rel_2019_2020 = 100/cc_pre_leisure_2019*cc_pre_leisure_03_2020) %>%
                                  mutate(cc_pre_leisure_rel_2019_2021 = 100/cc_pre_leisure_2019*cc_pre_leisure_summer_2021) %>%
                                  mutate(cc_pre_leisure_rel_2019_2023 = 100/cc_pre_leisure_2019*cc_pre_leisure_01_2023) %>%
                                  mutate(cc_pre_all_rel_2019_2020 = 100/cc_pre_all_2019*cc_pre_all_03_2020) %>%
                                  mutate(cc_pre_all_rel_2019_2021 = 100/cc_pre_all_2019*cc_pre_all_summer_2021) %>%
                                  mutate(cc_pre_all_rel_2019_2023 = 100/cc_pre_all_2019*cc_pre_all_01_2023)

data_reduced <- data_reduced %>% mutate(cc_during_work_rel_2019_2020 = 100/cc_during_work_2019*cc_during_work_03_2020) %>%
                                  mutate(cc_during_work_rel_2019_2021 = 100/cc_during_work_2019*cc_during_work_summer_2021) %>%
                                  mutate(cc_during_work_rel_2019_2023 = 100/cc_during_work_2019*cc_during_work_01_2023) %>%
                                  mutate(cc_during_school_rel_2019_2020 = 100/cc_during_school_2019*cc_during_school_03_2020) %>%
                                  mutate(cc_during_school_rel_2019_2021 = 100/cc_during_school_2019*cc_during_school_summer_2021) %>%
                                  mutate(cc_during_school_rel_2019_2023 = 100/cc_during_school_2019*cc_during_school_01_2023) %>%
                                  mutate(cc_during_leisure_rel_2019_2020 = 100/cc_during_leisure_2019*cc_during_leisure_03_2020) %>%
                                  mutate(cc_during_leisure_rel_2019_2021 = 100/cc_during_leisure_2019*cc_during_leisure_summer_2021) %>%
                                  mutate(cc_during_leisure_rel_2019_2023 = 100/cc_during_leisure_2019*cc_during_leisure_01_2023) %>%
                                  mutate(cc_during_all_rel_2019_2020 = 100/cc_during_all_2019*cc_during_all_03_2020) %>%
                                  mutate(cc_during_all_rel_2019_2021 = 100/cc_during_all_2019*cc_during_all_summer_2021) %>%
                                  mutate(cc_during_all_rel_2019_2023 = 100/cc_during_all_2019*cc_during_all_01_2023)

### SUB ANALYSIS A
data_reduced <- data_reduced %>% select(contains(c("respondent_cc_change", "age_bracket", "rel_", "Score")))

data_reduced <- data_reduced %>% mutate_all(~ifelse(is.nan(.), NA, .))
data_reduced <- data_reduced %>% mutate_all(~ifelse(is.infinite(.), NA, .))

data_reduced_analysis_a <- data_reduced %>% mutate(resp_minus_cc_pre_work_rel_2019_2020 = respondent_work_rel_2019_2020 - cc_pre_work_rel_2019_2020,
                                        resp_minus_cc_pre_work_rel_2019_2021 = respondent_work_rel_2019_2021 - cc_pre_work_rel_2019_2021,
                                        resp_minus_cc_pre_work_rel_2019_2023 = respondent_work_rel_2019_2023 - cc_pre_work_rel_2019_2023,
                                        resp_minus_cc_pre_leisure_rel_2019_2020 = respondent_leisure_rel_2019_2020 - cc_pre_leisure_rel_2019_2020,
                                        resp_minus_cc_pre_leisure_rel_2019_2021 = respondent_leisure_rel_2019_2021 - cc_pre_leisure_rel_2019_2021,
                                        resp_minus_cc_pre_leisure_rel_2019_2023 = respondent_leisure_rel_2019_2023 - cc_pre_leisure_rel_2019_2023,
                                        resp_minus_cc_pre_all_rel_2019_2020 = respondent_all_rel_2019_2020 - cc_pre_leisure_rel_2019_2020,
                                        resp_minus_cc_pre_all_rel_2019_2021 = respondent_all_rel_2019_2021 - cc_pre_all_rel_2019_2021,
                                        resp_minus_cc_pre_all_rel_2019_2023 = respondent_all_rel_2019_2023 - cc_pre_all_rel_2019_2023)

data_reduced_analysis_a <- data_reduced_analysis_a %>% mutate(mean_minus_cc_pre_work_rel_2019_2020 = mean(respondent_work_rel_2019_2020, na.rm=TRUE) - cc_pre_work_rel_2019_2020,
                                        mean_minus_cc_pre_work_rel_2019_2021 = mean(respondent_work_rel_2019_2021, na.rm = TRUE) - cc_pre_work_rel_2019_2021,
                                        mean_minus_cc_pre_work_rel_2019_2023 = mean(respondent_work_rel_2019_2023, na.rm = TRUE) - cc_pre_work_rel_2019_2023,
                                        mean_minus_cc_pre_leisure_rel_2019_2020 = mean(respondent_leisure_rel_2019_2020, na.rm = TRUE) - cc_pre_leisure_rel_2019_2020,
                                        mean_minus_cc_pre_leisure_rel_2019_2021 = mean(respondent_leisure_rel_2019_2021, na.rm = TRUE) - cc_pre_leisure_rel_2019_2021,
                                        mean_minus_cc_pre_leisure_rel_2019_2023 = mean(respondent_leisure_rel_2019_2023, na.rm = TRUE) - cc_pre_leisure_rel_2019_2023,
                                        mean_minus_cc_pre_all_rel_2019_2020 = mean(respondent_all_rel_2019_2020, na.rm = TRUE) - cc_pre_leisure_rel_2019_2020,
                                        mean_minus_cc_pre_all_rel_2019_2021 = mean(respondent_all_rel_2019_2021, na.rm = TRUE) - cc_pre_all_rel_2019_2021,
                                        mean_minus_cc_pre_all_rel_2019_2023 = mean(respondent_all_rel_2019_2023, na.rm = TRUE) - cc_pre_all_rel_2019_2023)

data_reduced_analysis_a <- data_reduced_analysis_a %>% select(contains(c("age_bracket", "Score", "minus")))

data_reduced_analysis_a <- data_reduced_analysis_a %>% pivot_longer(cols = 4:2)

data_reduced_analysis_a <- data_reduced_analysis_a  %>% mutate(time = case_when(str_detect(name, "2020") ~ "03/2020",
                                                          str_detect(name, "2021") ~ "Summer 2021",
                                                          str_detect(name, "2023") ~ "01/2023")) %>%
                                  mutate(RespOrMeanOrMedian = case_when(str_detect(name, "resp") ~ "Resp",
                                                                        str_detect(name, "mean") ~  "Mean",
                                                                        str_detect(name, "median") ~  "Median")) %>%
                                  mutate(TypeOfContact = case_when(str_detect(name, "work") ~ "Work",
                                  str_detect(name, "school") ~ "School",
                                  str_detect(name, "leisure") ~ "Leisure",
                                  str_detect(name, "all") ~ "All"))

palette <- function() {
  c("#006BA6", "#FFBC42", "#8F2D56", "#C93E78")
}

data_reduced_analysis_a$time <- factor(data_reduced_analysis_a$time, levels = c("2019", "03/2020", "Summer 2021", "01/2023"))
data_reduced_analysis_a$TypeOfContact <- factor(data_reduced_analysis_a$TypeOfContact, levels = c("Work", "Leisure", "All"))

ggplot(data_reduced_analysis_a %>% filter(RespOrMeanOrMedian == "Mean") %>%
    filter(attitudeScore > 6) %>% 
    filter(value > -100) %>% filter(value < 100) %>% 
    filter(TypeOfContact != "School") %>% 
    filter(!is.na(TypeOfContact)), aes(time, value)) +
  geom_boxplot(color = "#006BA6", size = 1.3) +
  #geom_violin(aes(color=WhoseContacts), size = 1.3) +
  scale_color_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols= vars(time)) +
  theme_minimal() +
  ylab("Reported # Of Contacts") +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank())

### SUB ANALYSIS B

data_reduced <- data_reduced %>% select(contains(c("age_bracket", "respondent_cc_change", "rel_")))

data_reduced <- data_reduced %>% pivot_longer(cols = 3:50)

data_reduced <- data_reduced  %>% mutate(time = case_when(str_detect(name, "2020") ~ "03/2020",
                                                          str_detect(name, "2021") ~ "Summer 2021",
                                                          str_detect(name, "2023") ~ "01/2023")) %>%
                                  mutate(WhoseContacts = case_when(str_detect(name, "respondent") ~ "Respondent",
                                  str_detect(name, "cc_pre") ~ "Closest Contact (Pre-Covid)",
                                  str_detect(name, "cc_during") ~ "Closest Contact (During-Covid)",
                                  str_detect(name, "hhmember") ~ "Household Member")) %>%
                                  mutate(TypeOfContact = case_when(str_detect(name, "work") ~ "Work",
                                  str_detect(name, "school") ~ "School",
                                  str_detect(name, "leisure") ~ "Leisure",
                                  str_detect(name, "all") ~ "All"))

data_reduced$time <- factor(data_reduced$time, levels = c("2019", "03/2020", "Summer 2021", "01/2023"))
data_reduced$TypeOfContact <- factor(data_reduced$TypeOfContact, levels = c("Work", "Leisure", "School", "All"))
data_reduced$WhoseContacts <- factor(data_reduced$WhoseContacts, levels = c("Respondent", "Household Member", "Closest Contact (Pre-Covid)", "Closest Contact (During-Covid)"))


palette <- function() {
  c("#006BA6", "#FFBC42", "#8F2D56", "#C93E78")
}

ggplot(data_reduced %>% filter(value < 200) %>% filter(!is.na(TypeOfContact)), aes(WhoseContacts, value)) +
  geom_boxplot(aes(color = WhoseContacts), size = 1.3) +
  #geom_violin(aes(color=WhoseContacts), size = 1.3) +
  scale_color_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols= vars(time)) +
  theme_minimal() +
  scale_color_manual(values = palette()) +
  ylab("Relative Reported # Of Contacts") +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("CollectionBoxplotsRelative.pdf", dpi = 500, w = 13, h = 16)
ggsave("CollectionBoxplotsRelative.png", dpi = 500, w = 13, h = 16)

ggplot(data_reduced %>% filter(WhoseContacts == "Household Member") %>% filter(value < 100) %>% filter(!is.na(TypeOfContact)), aes(time, value)) +
  #geom_violin(aes(color = WhoseContacts), width = 1, trim = FALSE, position=position_dodge(0.9)) + 
  #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
  geom_boxplot(color = "#FFBC42", size = 1.3) +
  scale_color_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols= vars(time)) +
  theme_minimal() +
  ylab("Reported # Of Contacts") +
  theme(text = element_text(size = 22)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(panel.spacing = unit(0.8, "cm", data = NULL))
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("CollectionBoxplotsChildrenRelative.pdf", dpi = 500, w = 8.5, h = 12)
ggsave("CollectionBoxplotsChildrenRelative.png", dpi = 500, w = 8.5, h = 12)

palette <- function() {
  c("#fd5901", "#f78104", "#faab36", "#249ea0", "#008083", "#005f60")
}

ggplot(data_reduced %>% filter(WhoseContacts == "Respondent") %>% filter(!is.na(age_bracket)) %>% filter(age_bracket != "80-90") %>% filter(value < 200) %>% filter(!is.na(TypeOfContact)), aes(age_bracket, value)) +
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

ggsave("CollectionBoxplotsRelativeAgeBrackets.pdf", dpi = 500, w = 13, h = 16)
ggsave("CollectionBoxplotsRelativeAgeBrackets.png", dpi = 500, w = 13, h = 16)
