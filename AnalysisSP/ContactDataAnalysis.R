library(tidyverse)

raw_data <- read_csv("ENTER PATH HERE")

# Households --------------------------------------------------------------

HouseholdData <- raw_data %>% select(user_id, hsld_size_2019_, hsld_size_03_2020_, hsld_size_summer_2021_, hsld_size_01_2023_, 
                                     cc_hsld_size_pre_pandemic_2019_num_hsld_members, cc_hsld_size_pre_pandemic_03_2020_num_hsld_members, cc_hsld_size_pre_pandemic_summer_2021_num_hsld_members, cc_hsld_size_pre_pandemic_01_2023_num_hsld_members,
                                     cc_hsld_size_during_pandemic_2019_num_hsld_members, cc_hsld_size_during_pandemic_03_2020_num_hsld_members, cc_hsld_size_during_pandemic_summer_2021_num_hsld_members, cc_hsld_size_during_pandemic_01_2023_num_hsld_members)
#HouseholdData <- na.omit(HouseholdData)
HouseholdDataRespondent <- HouseholdData %>% pivot_longer(cols = c("hsld_size_2019_", "hsld_size_03_2020_", "hsld_size_summer_2021_", "hsld_size_01_2023_"))
HouseholdDataRespondent$name <- factor(HouseholdDataRespondent$name, levels = c("hsld_size_2019_", "hsld_size_03_2020_", "hsld_size_summer_2021_", "hsld_size_01_2023_"))
HouseholdDataRespondent <- HouseholdDataRespondent %>% mutate(name = case_when(name == "hsld_size_2019_" ~ "Household size 2019",
                                                                     name == "hsld_size_03_2020_"~ "Household size 03/20",
                                                                     name == "hsld_size_summer_2021_"~ "Household size summer 21",
                                                                     name == "hsld_size_01_2023_"~ "Household size 01/23"))
HouseholdDataRespondent$name <- factor(HouseholdDataRespondent$name, levels = c("Household size 2019", "Household size 03/20", "Household size summer 21", "Household size 01/23"))
HouseholdDataRespondent$value <- as.integer(HouseholdDataRespondent$value)
ggplot(HouseholdDataRespondent %>% filter(value <= 10)) + geom_bar(aes(x= value, y = ..prop.., group = 1, fill = name)) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks=c(0,2,4,6,8)) +
  theme(text = element_text(size = 15)) +
  labs(title=paste0("Filtered for households with <= 10 members")) +
  labs(subtitle=paste0("#Households with more than 10 members: ", as.character(nrow(HouseholdDataRespondent %>% filter(value > 10))), "\n#Members in largest household (in 2019): ", as.character(max(HouseholdDataRespondent$value, na.rm=TRUE))))
ggsave("HouseholdSizeRespondent.png", dpi = 500, w = 9, h = 5)

HouseholdDataCloseContactPre <- HouseholdData %>% pivot_longer(cols = c("cc_hsld_size_pre_pandemic_2019_num_hsld_members", "cc_hsld_size_pre_pandemic_03_2020_num_hsld_members", "cc_hsld_size_pre_pandemic_summer_2021_num_hsld_members", "cc_hsld_size_pre_pandemic_01_2023_num_hsld_members"))
HouseholdDataCloseContactPre$name <- factor(HouseholdDataCloseContactPre$name, levels = c("cc_hsld_size_pre_pandemic_2019_num_hsld_members", "cc_hsld_size_pre_pandemic_03_2020_num_hsld_members", "cc_hsld_size_pre_pandemic_summer_2021_num_hsld_members", "cc_hsld_size_pre_pandemic_01_2023_num_hsld_members"))
HouseholdDataCloseContactPre <- HouseholdDataCloseContactPre %>% mutate(name = case_when(name == "cc_hsld_size_pre_pandemic_2019_num_hsld_members" ~ "Household size 2019",
                                                                               name == "cc_hsld_size_pre_pandemic_03_2020_num_hsld_members"~ "Household size 03/20",
                                                                               name == "cc_hsld_size_pre_pandemic_summer_2021_num_hsld_members"~ "Household size summer 21",
                                                                               name == "cc_hsld_size_pre_pandemic_01_2023_num_hsld_members"~ "Household size 01/23"))
HouseholdDataCloseContactPre$name <- factor(HouseholdDataCloseContactPre$name, levels = c("Household size 2019", "Household size 03/20", "Household size summer 21", "Household size 01/23"))
HouseholdDataCloseContactPre$value <- as.integer(HouseholdDataCloseContactPre$value)
ggplot(HouseholdDataCloseContactPre %>% filter(value < 10)) + geom_bar(aes(x= value, y = ..prop.., group = 1, fill = name)) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks=c(0,2,4,6,8)) +
  theme(text = element_text(size = 15)) +
  labs(title=paste0("Filtered for households with <= 10 members")) +
  labs(subtitle=paste0("#Households with more than 10 members: ", as.character(nrow(HouseholdDataCloseContact %>% filter(value > 10))), "\n#Members in largest household (in 2019): ", as.character(max(HouseholdDataCloseContact$value, na.rm=TRUE))))
ggsave("HouseholdSizeCCPre.png", dpi = 500, w = 9, h = 5)

HouseholdDataCloseContactDuring <- HouseholdData %>% pivot_longer(cols = c("cc_hsld_size_during_pandemic_2019_num_hsld_members", "cc_hsld_size_during_pandemic_03_2020_num_hsld_members", "cc_hsld_size_during_pandemic_summer_2021_num_hsld_members", "cc_hsld_size_during_pandemic_01_2023_num_hsld_members"))
#HouseholdDataCloseContactDuring$name <- factor(HouseholdDataCloseContactPre$name, levels = c("cc_hsld_size_during_pandemic_2019_num_hsld_members", "cc_hsld_size_during_pandemic_03_2020_num_hsld_members", "cc_hsld_size_during_pandemic_summer_2021_num_hsld_members", "cc_hsld_size_during_pandemic_01_2023_num_hsld_members"))
HouseholdDataCloseContactDuring <- HouseholdDataCloseContactDuring %>% mutate(name = case_when(name == "cc_hsld_size_during_pandemic_2019_num_hsld_members" ~ "Household size 2019",
                                                                                         name == "cc_hsld_size_during_pandemic_03_2020_num_hsld_members"~ "Household size 03/20",
                                                                                         name == "cc_hsld_size_during_pandemic_summer_2021_num_hsld_members"~ "Household size summer 21",
                                                                                         name == "cc_hsld_size_during_pandemic_01_2023_num_hsld_members"~ "Household size 01/23"))
HouseholdDataCloseContactDuring$name <- factor(HouseholdDataCloseContactDuring$name, levels = c("Household size 2019", "Household size 03/20", "Household size summer 21", "Household size 01/23"))
HouseholdDataCloseContactDuring$value <- as.integer(HouseholdDataCloseContactDuring$value)
ggplot(HouseholdDataCloseContactDuring %>% filter(value < 10)) + geom_bar(aes(x= value, y = ..prop.., group = 1, fill = name)) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks=c(0,2,4,6,8)) +
  theme(text = element_text(size = 15)) +
  labs(title=paste0("Filtered for households with <= 10 members")) +
  labs(subtitle=paste0("#Households with more than 10 members: ", as.character(nrow(HouseholdDataCloseContact %>% filter(value > 10))), "\n#Members in largest household (in 2019): ", as.character(max(HouseholdDataCloseContact$value, na.rm=TRUE))))
ggsave("HouseholdSizeCCDuring.png", dpi = 500, w = 9, h = 5)

# Work setting ------------------------------------------------------------

WorkData <- raw_data %>% select(user_id, wkly_cont_2019_work_uni, wkly_cont_03_2020_work_uni, wkly_cont_summer_2021_work_uni, wkly_cont_01_2023_work_uni,
                                hsld_cont__2019_work_uni, hsld_cont__03_2020_work_uni, hsld_cont__summer_2021_work_uni, hsld_cont__01_2023_work_uni,
                                cc_weekly_contacts_2019_work_uni_cont, cc_weekly_contacts_03_2020_work_uni_cont, cc_weekly_contacts_summer_2021_work_uni_cont, cc_weekly_contacts_01_2023_work_uni_cont,
                                cc_weekly_cont_during_pandemic_2019_work_uni_cont, cc_weekly_cont_during_pandemic_03_2020_work_uni_cont, cc_weekly_cont_during_pandemic_summer_2021_work_uni_cont, cc_weekly_cont_during_pandemic_01_2023_work_uni_cont)

#WorkData <- na.omit(WorkData)

WorkDataRespondent <- WorkData %>% select(user_id, wkly_cont_2019_work_uni, wkly_cont_03_2020_work_uni, wkly_cont_summer_2021_work_uni, wkly_cont_01_2023_work_uni) %>%
                                             pivot_longer(cols = c("wkly_cont_2019_work_uni", "wkly_cont_03_2020_work_uni", "wkly_cont_summer_2021_work_uni", "wkly_cont_01_2023_work_uni"))
#WorkDataRespondent$name <- factor(WorkDataRespondent$name, levels = c("wkly_cont_2019_work_uni", "wkly_cont_03_2020_work_uni", "wkly_cont_summer_2021_work_uni", "wkly_cont_01_2023_work_uni"))
WorkDataRespondent <- WorkDataRespondent %>% mutate(name = case_when(name == "wkly_cont_2019_work_uni" ~ "weekly Work/Uni contacts 2019",
                                                                     name == "wkly_cont_03_2020_work_uni"~ "weekly Work/Uni contacts 03/20",
                                                                     name == "wkly_cont_summer_2021_work_uni"~ "weekly Work/Uni contacts summer 21",
                                                                     name == "wkly_cont_01_2023_work_uni"~ "weekly Work/Uni contacts 01/23"))
WorkDataRespondent$name <- factor(WorkDataRespondent$name, levels = c("weekly Work/Uni contacts 2019", "weekly Work/Uni contacts 03/20", "weekly Work/Uni contacts summer 21", "weekly Work/Uni contacts 01/23"))
WorkDataRespondent$value <- as.integer(WorkDataRespondent$value)
ggplot(WorkDataRespondent %>% filter(value <= 100)) + geom_histogram(aes(x = value, y = after_stat(count / sum(count)), fill = name), bins = 10) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks=c(0,20,40,60,80,100)) +
  theme(text = element_text(size = 15)) +
  labs(title=paste0("Filtered for respondents with <= 100 work contacts")) +
  labs(subtitle=paste0("#Respondents with more than 100 work contacts: ", as.character(nrow(WorkDataRespondent %>% filter(value > 100))), "\nMax # of work contacts (in 2019): ", as.character(max(WorkDataRespondent$value, na.rm=TRUE))))
ggsave("WorkContactsRespondent.png", dpi = 500, w = 9, h = 5)

WorkDataHouseholdMember <- WorkData %>% select(user_id, hsld_cont__2019_work_uni, hsld_cont__03_2020_work_uni, hsld_cont__summer_2021_work_uni, hsld_cont__01_2023_work_uni) %>%
                          pivot_longer(cols = c("hsld_cont__2019_work_uni", "hsld_cont__03_2020_work_uni", "hsld_cont__summer_2021_work_uni", "hsld_cont__01_2023_work_uni"))
#WorkDataHouseholdMember$name <- factor(WorkDataHouseholdMember$name, levels = c("hsld_cont__2019_work_uni", "hsld_cont__03_2020_work_uni", "hsld_cont__summer_2021_work_uni", "hsld_cont__01_2023_work_uni"))
WorkDataHouseholdMember <- WorkDataHouseholdMember %>% mutate(name = case_when(name == "hsld_cont__2019_work_uni" ~ "Household's Work/Uni contacts 2019",
                                                                     name == "hsld_cont__03_2020_work_uni"~ "Household's Work/Uni contacts 03/20",
                                                                     name == "hsld_cont__summer_2021_work_uni"~ "Household's Work/Uni contacts summer 21",
                                                                     name == "hsld_cont__01_2023_work_uni"~ "Household's Work/Uni contacts 01/23"))
WorkDataHouseholdMember$name <- factor(WorkDataHouseholdMember$name, levels = c("Household's Work/Uni contacts 2019", "Household's Work/Uni contacts 03/20", "Household's Work/Uni contacts summer 21", "Household's Work/Uni contacts 01/23"))
WorkDataHouseholdMember$value <- as.integer(WorkDataHouseholdMember$value)
ggplot(WorkDataHouseholdMember %>% filter(value <= 100)) + geom_histogram(aes(x = value, y = after_stat(count / sum(count)), fill = name), bins = 10) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks=c(0,20,40,60,80,100)) +
  theme(text = element_text(size = 15)) +
  labs(title=paste0("Filtered for HH members with <= 100 work contacts")) +
  labs(subtitle=paste0("#HH members with more than 100 work contacts: ", as.character(nrow(WorkDataHouseholdMember %>% filter(value > 100))), "\nMax # of work contacts (in 2019): ", as.character(max(WorkDataHouseholdMember$value, na.rm=TRUE))))
ggsave("WorkContactsHouseholdMember.png", dpi = 500, w = 9, h = 5)

WorkDataCCPre <- WorkData %>% select(user_id, cc_weekly_contacts_2019_work_uni_cont, cc_weekly_contacts_03_2020_work_uni_cont, cc_weekly_contacts_summer_2021_work_uni_cont, cc_weekly_contacts_01_2023_work_uni_cont) %>%
                pivot_longer(cols = c("cc_weekly_contacts_2019_work_uni_cont", "cc_weekly_contacts_03_2020_work_uni_cont", "cc_weekly_contacts_summer_2021_work_uni_cont", "cc_weekly_contacts_01_2023_work_uni_cont"))
#WorkDataCCPre$name <- factor(WorkDataCCPre$name, levels = c("cc_weekly_contacts_2019_work_uni_cont", "cc_weekly_contacts_03_2020_work_uni_cont", "cc_weekly_contacts_summer_2021_work_uni_cont", "cc_weekly_contacts_01_2023_work_uni_cont"))
WorkDataCCPre <- WorkDataCCPre %>% mutate(name = case_when(name == "cc_weekly_contacts_2019_work_uni_cont" ~ "CC's Work/Uni contacts 2019",
                                                                               name == "cc_weekly_contacts_03_2020_work_uni_cont"~ "CC's Work/Uni contacts 03/20",
                                                                               name == "cc_weekly_contacts_summer_2021_work_uni_cont"~ "CC's Work/Uni contacts summer 21",
                                                                               name == "cc_weekly_contacts_01_2023_work_uni_cont"~ "CC's Work/Uni contacts 01/23"))
WorkDataCCPre$name <- factor(WorkDataCCPre$name, levels = c("CC's Work/Uni contacts 2019", "CC's Work/Uni contacts 03/20", "CC's Work/Uni contacts summer 21", "CC's Work/Uni contacts 01/23"))
WorkDataCCPre$value <- as.integer(WorkDataCCPre$value)
ggplot(WorkDataCCPre %>% filter(value <= 100)) + geom_histogram(aes(x = value, y = after_stat(count / sum(count)), fill = name), bins = 10) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks=c(0,20,40,60,80,100)) +
  theme(text = element_text(size = 15)) +
  labs(title=paste0("Filtered for close contacts with <= 100 work contacts")) +
  labs(subtitle=paste0("#close contacts with more than 100 work contacts: ", as.character(nrow(WorkDataCCPre %>% filter(value > 100))), "\nMax # of work contacts (in 2019): ", as.character(max(WorkDataCCPre$value, na.rm=TRUE))))
ggsave("WorkContactsCCPre.png", dpi = 500, w = 9, h = 5)

WorkDataCCDuring <- WorkData %>% select(user_id, cc_weekly_cont_during_pandemic_2019_work_uni_cont, cc_weekly_cont_during_pandemic_03_2020_work_uni_cont, cc_weekly_cont_during_pandemic_summer_2021_work_uni_cont, cc_weekly_cont_during_pandemic_01_2023_work_uni_cont) %>%
                  pivot_longer(cols = c("cc_weekly_cont_during_pandemic_2019_work_uni_cont", "cc_weekly_cont_during_pandemic_03_2020_work_uni_cont", "cc_weekly_cont_during_pandemic_summer_2021_work_uni_cont", "cc_weekly_cont_during_pandemic_01_2023_work_uni_cont"))
#WorkDataCCDuring$name <- factor(WorkDataCCDuring$name, levels = c("cc_weekly_cont_during_pandemic_2019_work_uni_cont", "cc_weekly_cont_during_pandemic_03_2020_work_uni_cont", "cc_weekly_cont_during_pandemic_summer_2021_work_uni_cont", "cc_weekly_cont_during_pandemic_01_2023_work_uni_cont"))
WorkDataCCDuring <- WorkDataCCDuring %>% mutate(name = case_when(name == "cc_weekly_cont_during_pandemic_2019_work_uni_cont" ~ "CC's Work/Uni contacts 2019",
                                                           name == "cc_weekly_cont_during_pandemic_03_2020_work_uni_cont"~ "CC's Work/Uni contacts 03/20",
                                                           name == "cc_weekly_cont_during_pandemic_summer_2021_work_uni_cont"~ "CC's Work/Uni contacts summer 21",
                                                           name == "cc_weekly_cont_during_pandemic_01_2023_work_uni_cont"~ "CC's Work/Uni contacts 01/23"))
WorkDataCCDuring$name <- factor(WorkDataCCDuring$name, levels = c("CC's Work/Uni contacts 2019", "CC's Work/Uni contacts 03/20", "CC's Work/Uni contacts summer 21", "CC's Work/Uni contacts 01/23"))
WorkDataCCDuring$value <- as.integer(WorkDataCCDuring$value)
ggplot(WorkDataCCDuring %>% filter(value <= 100)) + geom_histogram(aes(x = value, y = after_stat(count / sum(count)), fill = name), bins = 10) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks=c(0,20,40,60,80,100)) +
  theme(text = element_text(size = 15)) +
  labs(title=paste0("Filtered for close contacts with <= 100 work contacts")) +
  labs(subtitle=paste0("#close contacts with more than 100 work contacts: ", as.character(nrow(WorkDataCCDuring %>% filter(value > 100))), "\nMax # of work contacts (in 2019): ", as.character(max(WorkDataCCDuring$value, na.rm=TRUE))))
ggsave("WorkContactsCCDuring.png", dpi = 500, w = 9, h = 5)

# School ------------------------------------------------------------------

SchoolData <- raw_data %>% select(user_id, wkly_cont_2019_school_kinder, wkly_cont_03_2020_school_kinder, wkly_cont_summer_2021_school_kinder, wkly_cont_01_2023_school_kinder,
                                  hsld_cont__2019_school_kinder, hsld_cont__03_2020_school_kinder, hsld_cont__summer_2021_school_kinder, hsld_cont__01_2023_school_kinder,
                                  cc_weekly_contacts_2019_school_kinder_cont, cc_weekly_contacts_03_2020_school_kinder_cont, cc_weekly_contacts_summer_2021_school_kinder_cont, cc_weekly_contacts_01_2023_school_kinder_cont,
                                  cc_weekly_cont_during_pandemic_2019_school_kg_cont, cc_weekly_cont_during_pandemic_03_2020_school_kg_cont, cc_weekly_cont_during_pandemic_summer_2021_school_kg_cont, cc_weekly_cont_during_pandemic_01_2023_school_kg_cont)

#SchoolData <- na.omit(SchoolData)

SchoolDataRespondent <- SchoolData %>% select(user_id, wkly_cont_2019_school_kinder, wkly_cont_03_2020_school_kinder, wkly_cont_summer_2021_school_kinder, wkly_cont_01_2023_school_kinder) %>%
                    pivot_longer(cols = c("wkly_cont_2019_school_kinder", "wkly_cont_03_2020_school_kinder", "wkly_cont_summer_2021_school_kinder", "wkly_cont_01_2023_school_kinder"))
#SchoolDataRespondent$name <- factor(SchoolDataRespondent$name, levels = c("wkly_cont_2019_school_kinder", "wkly_cont_03_2020_school_kinder", "wkly_cont_summer_2021_school_kinder", "wkly_cont_01_2023_school_kinder"))
SchoolDataRespondent <- SchoolDataRespondent %>% mutate(name = case_when(name == "wkly_cont_2019_school_kinder" ~ "weekly School contacts 2019",
                                                                     name == "wkly_cont_03_2020_school_kinder"~ "weekly School contacts 03/20",
                                                                     name == "wkly_cont_summer_2021_school_kinder"~ "weekly School contacts summer 21",
                                                                     name == "wkly_cont_01_2023_school_kinder"~ "weekly School contacts 01/23"))
SchoolDataRespondent$name <- factor(SchoolDataRespondent$name, levels = c("weekly School contacts 2019", "weekly School contacts 03/20", "weekly School contacts summer 21", "weekly School contacts 01/23"))
SchoolDataRespondent$value <- as.integer(SchoolDataRespondent$value)
ggplot(SchoolDataRespondent %>% filter(value <= 100)) + geom_histogram(aes(x = value, y = after_stat(count / sum(count)), fill = name), bins = 10) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks=c(0,20,40,60,80,100)) +
  theme(text = element_text(size = 15)) +
  labs(title=paste0("Filtered for respondents with <= 100 school contacts")) +
  labs(subtitle=paste0("#Respondents with more than 100 school contacts: ", as.character(nrow(SchoolDataRespondent %>% filter(value > 100))), "\nMax # of work contacts (in 2019): ", as.character(max(SchoolDataRespondent$value, na.rm=TRUE))))
ggsave("SchoolContactsRespondent.png", dpi = 500, w = 9, h = 5)

SchoolDataHouseholdMember <- SchoolData %>% select(user_id, hsld_cont__2019_school_kinder, hsld_cont__03_2020_school_kinder, hsld_cont__summer_2021_school_kinder, hsld_cont__01_2023_school_kinder) %>%
                                                   pivot_longer(cols = c("hsld_cont__2019_school_kinder", "hsld_cont__03_2020_school_kinder", "hsld_cont__summer_2021_school_kinder",  "hsld_cont__01_2023_school_kinder"))
#SchoolDataHouseholdMember$name <- factor(SchoolDataHouseholdMember$name, levels = c("hsld_cont__2019_school_kinder", "hsld_cont__03_2020_school_kinder", "hsld_cont__summer_2021_school_kinder",  "hsld_cont__01_2023_school_kinder"))
SchoolDataHouseholdMember$value <- as.integer(SchoolDataHouseholdMember$value)
SchoolDataHouseholdMember <- SchoolDataHouseholdMember %>% mutate(name = case_when(name == "hsld_cont__2019_school_kinder" ~ "HH's School contacts 2019",
                                                                         name == "hsld_cont__03_2020_school_kinder"~ "HH's School contacts 03/20",
                                                                         name == "hsld_cont__summer_2021_school_kinder"~ "HH's School contacts summer 21",
                                                                         name == "hsld_cont__01_2023_school_kinder"~ "HH's School contacts 01/23"))
SchoolDataHouseholdMember$name <- factor(SchoolDataHouseholdMember$name, levels = c("HH's School contacts 2019", "HH's School contacts 03/20", "HH's School contacts summer 21", "HH's School contacts 01/23"))
ggplot(SchoolDataHouseholdMember %>% filter(value <= 100)) + geom_histogram(aes(x = value, y = after_stat(count / sum(count)), fill = name), bins = 10) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks=c(0,20,40,60,80,100)) +
  theme(text = element_text(size = 15)) +
  labs(title=paste0("Filtered for HH members with <= 100 school contacts")) +
  labs(subtitle=paste0("#HH members with more than 100 school contacts: ", as.character(nrow(SchoolDataHouseholdMember %>% filter(value > 100))), "\nMax # of school contacts (in 2019): ", as.character(max(SchoolDataHouseholdMember$value, na.rm=TRUE))))
#ggsave("SchoolContactsHouseholdMember.png", dpi = 500, w = 9, h = 5)

SchoolDataCCPre <- SchoolData %>% select(user_id, cc_weekly_contacts_2019_school_kinder_cont, cc_weekly_contacts_03_2020_school_kinder_cont, cc_weekly_contacts_summer_2021_school_kinder_cont, cc_weekly_contacts_01_2023_school_kinder_cont) %>%
  pivot_longer(cols = c("cc_weekly_contacts_2019_school_kinder_cont", "cc_weekly_contacts_03_2020_school_kinder_cont", "cc_weekly_contacts_summer_2021_school_kinder_cont", "cc_weekly_contacts_01_2023_school_kinder_cont"))
#SchoolDataCCPre$name <- factor(SchoolDataCCPre$name, levels = c("cc_weekly_contacts_2019_school_kinder_cont", "cc_weekly_contacts_03_2020_school_kinder_cont", "cc_weekly_contacts_summer_2021_school_kinder_cont", "cc_weekly_contacts_01_2023_school_kinder_cont"))
SchoolDataCCPre <- SchoolDataCCPre %>% mutate(name = case_when(name == "cc_weekly_contacts_2019_school_kinder_cont" ~ "CC's School contacts 2019",
                                                                                   name == "cc_weekly_contacts_03_2020_school_kinder_cont"~ "CC's School contacts 03/20",
                                                                                   name == "cc_weekly_contacts_summer_2021_school_kinder_cont"~  "CC's School contacts summer 21",
                                                                                   name == "cc_weekly_contacts_01_2023_school_kinder_cont"~ "CC's School contacts 01/23"))
SchoolDataCCPre$name <- factor(SchoolDataCCPre$name, levels = c("CC's School contacts 2019", "CC's School contacts 03/20", "CC's School contacts summer 21", "CC's School contacts 01/23"))
SchoolDataCCPre$value <- as.integer(SchoolDataCCPre$value)
ggplot(SchoolDataCCPre %>% filter(value <= 100)) + geom_histogram(aes(x = value, y = after_stat(count / sum(count)), fill = name), bins = 10) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks=c(0,20,40,60,80,100)) +
  theme(text = element_text(size = 15)) +
  labs(title=paste0("Filtered for close contacts with <= 100 school contacts")) +
  labs(subtitle=paste0("#close contacts with more than 100 school contacts: ", as.character(nrow(SchoolDataCCPre %>% filter(value > 100))), "\nMax # of school contacts (in 2019): ", as.character(max(SchoolDataCCPre$value, na.rm=TRUE))))
ggsave("SchoolContactsCCPre.png", dpi = 500, w = 9, h = 5)

SchoolDataCCDuring <- SchoolData %>% select(user_id, cc_weekly_cont_during_pandemic_2019_school_kg_cont, cc_weekly_cont_during_pandemic_03_2020_school_kg_cont, cc_weekly_cont_during_pandemic_summer_2021_school_kg_cont, cc_weekly_cont_during_pandemic_01_2023_school_kg_cont) %>%
                                    pivot_longer(cols = c("cc_weekly_cont_during_pandemic_2019_school_kg_cont", "cc_weekly_cont_during_pandemic_03_2020_school_kg_cont", "cc_weekly_cont_during_pandemic_summer_2021_school_kg_cont", "cc_weekly_cont_during_pandemic_01_2023_school_kg_cont"))
#SchoolDataCCDuring$name <- factor(SchoolDataCCDuring$name, levels = c("cc_weekly_cont_during_pandemic_2019_school_kg_cont", "cc_weekly_cont_during_pandemic_03_2020_school_kg_cont", "cc_weekly_cont_during_pandemic_summer_2021_school_kg_cont", "cc_weekly_cont_during_pandemic_01_2023_school_kg_cont"))
SchoolDataCCDuring <- SchoolDataCCDuring %>% mutate(name = case_when(name == "cc_weekly_cont_during_pandemic_2019_school_kg_cont" ~ "CC's School contacts 2019",
                                                               name == "cc_weekly_cont_during_pandemic_03_2020_school_kg_cont"~ "CC's School contacts 03/20",
                                                               name == "cc_weekly_cont_during_pandemic_summer_2021_school_kg_cont"~  "CC's School contacts summer 21",
                                                               name == "cc_weekly_cont_during_pandemic_01_2023_school_kg_cont"~ "CC's School contacts 01/23"))
SchoolDataCCDuring$name <- factor(SchoolDataCCDuring$name, levels = c("CC's School contacts 2019", "CC's School contacts 03/20", "CC's School contacts summer 21", "CC's School contacts 01/23"))
SchoolDataCCDuring$value <- as.integer(SchoolDataCCDuring$value)
ggplot(SchoolDataCCDuring %>% filter(value <= 100)) + geom_histogram(aes(x = value, y = after_stat(count / sum(count)), fill = name), bins = 10) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks=c(0,20,40)) +
  theme(text = element_text(size = 15))  +
  labs(title=paste0("Filtered for close contacts with <= 100 school contacts")) +
  labs(subtitle=paste0("#close contacts with more than 100 school contacts: ", as.character(nrow(SchoolDataCCDuring %>% filter(value > 100))), "\nMax # of school contacts (in 2019): ", as.character(max(SchoolDataCCDuring$value, na.rm=TRUE))))
ggsave("SchoolContactsCCDuring.png", dpi = 500, w = 9, h = 5)

# Leisure -----------------------------------------------------------------

LeisureData <- raw_data %>% select(user_id, wkly_cont_2019_leisure, wkly_cont_03_2020_leisure, wkly_cont_summer_2021_leisure, wkly_cont_01_2023_leisure,
                                   hsld_cont__2019_leisure, hsld_cont__03_2020_leisure, hsld_cont__summer_2021_leisure, hsld_cont__01_2023_leisure,
                                   cc_weekly_contacts_2019_leisure_cont, cc_weekly_contacts_03_2020_leisure_cont, cc_weekly_contacts_summer_2021_leisure_cont, cc_weekly_contacts_01_2023_leisure_cont,
                                   cc_weekly_cont_during_pandemic_2019_leisure_cont, cc_weekly_cont_during_pandemic_03_2020_leisure_cont, cc_weekly_cont_during_pandemic_summer_2021_leisure_cont, cc_weekly_cont_during_pandemic_01_2023_leisure_cont)

#LeisureData <- na.omit(LeisureData)

LeisureDataRespondent <- LeisureData %>% pivot_longer(cols = c("wkly_cont_2019_leisure", "wkly_cont_03_2020_leisure", "wkly_cont_summer_2021_leisure", "wkly_cont_01_2023_leisure"))
#LeisureDataRespondent$name <- factor(LeisureDataRespondent$name, levels = c("wkly_cont_2019_leisure", "wkly_cont_03_2020_leisure", "wkly_cont_summer_2021_leisure", "wkly_cont_01_2023_leisure"))
LeisureDataRespondent <- LeisureDataRespondent %>% mutate(name = case_when(name == "wkly_cont_2019_leisure" ~ "weekly Leisure contacts 2019",
                                                                         name == "wkly_cont_03_2020_leisure"~ "weekly Leisure contacts 03/20",
                                                                         name == "wkly_cont_summer_2021_leisure"~ "weekly Leisure contacts summer 21",
                                                                         name == "wkly_cont_01_2023_leisure"~ "weekly Leisure contacts 01/23"))
LeisureDataRespondent$name <- factor(LeisureDataRespondent$name, levels = c("weekly Leisure contacts 2019", "weekly Leisure contacts 03/20", "weekly Leisure contacts summer 21", "weekly Leisure contacts 01/23"))
LeisureDataRespondent$value <- as.integer(LeisureDataRespondent$value)
ggplot(LeisureDataRespondent %>% filter(value <= 50)) + geom_histogram(aes(x = value, y = after_stat(count / sum(count)), fill = name), bins = 10) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks=c(0,10,20,30,40,50)) +
  theme(text = element_text(size = 15)) +
  labs(title=paste0("Filtered for respondents with <= 50 leisure contacts")) +
  labs(subtitle=paste0("#Respondents with more than 50 leisure contacts: ", as.character(nrow(LeisureDataRespondent %>% filter(value > 50))), "\nMax # of leisure contacts (in 2019): ", as.character(max(LeisureDataRespondent$value, na.rm=TRUE))))
ggsave("LeisureContactsRespondent.png", dpi = 500, w = 9, h = 5)

LeisureDataHouseholdMember <- LeisureData %>% select(user_id, hsld_cont__2019_leisure, hsld_cont__03_2020_leisure, hsld_cont__summer_2021_leisure, hsld_cont__01_2023_leisure) %>%
                                                     pivot_longer(cols = c("hsld_cont__2019_leisure", "hsld_cont__03_2020_leisure", "hsld_cont__summer_2021_leisure", "hsld_cont__01_2023_leisure"))
#LeisureDataHouseholdMember$name <- factor(LeisureDataHouseholdMember$name, levels = c("hsld_cont__2019_leisure", "hsld_cont__03_2020_leisure", "hsld_cont__summer_2021_leisure", "hsld_cont__01_2023_leisure"))
LeisureDataHouseholdMember <- LeisureDataHouseholdMember %>% mutate(name = case_when(name == "hsld_cont__2019_leisure" ~ "HH's Leisure contacts 2019",
                                                                           name == "hsld_cont__03_2020_leisure"~ "HH's Leisure contacts 03/20",
                                                                           name == "hsld_cont__summer_2021_leisure"~ "HH's Leisure contacts summer 21",
                                                                           name == "hsld_cont__01_2023_leisure"~ "HH's Leisure contacts 01/23"))
LeisureDataHouseholdMember$name <- factor(LeisureDataHouseholdMember$name, levels = c("HH's Leisure contacts 2019", "HH's Leisure contacts 03/20", "HH's Leisure contacts summer 21", "HH's Leisure contacts 01/23"))
LeisureDataHouseholdMember$value <- as.integer(LeisureDataHouseholdMember$value)
ggplot(LeisureDataHouseholdMember %>% filter(value <= 50)) + geom_histogram(aes(x = value, y = after_stat(count / sum(count)), fill = name), bins = 10) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks=c(0,10,20,30,40,50)) +
  theme(text = element_text(size = 15)) +
  labs(title=paste0("Filtered for HH members with <= 50 leisure contacts")) +
  labs(subtitle=paste0("#HH members with more than 50 leisure contacts: ", as.character(nrow(LeisureDataHouseholdMember %>% filter(value > 50))), "\nMax # of leisure contacts (in 2019): ", as.character(max(LeisureDataHouseholdMember$value, na.rm=TRUE))))
ggsave("LeisureContactsHouseholdMember.png", dpi = 500, w = 9, h = 5)

LeisureDataCCPre <- LeisureData %>% select(user_id, cc_weekly_contacts_2019_leisure_cont, cc_weekly_contacts_03_2020_leisure_cont, cc_weekly_contacts_summer_2021_leisure_cont, cc_weekly_contacts_01_2023_leisure_cont) %>%
                                           pivot_longer(cols = c("cc_weekly_contacts_2019_leisure_cont", "cc_weekly_contacts_03_2020_leisure_cont", "cc_weekly_contacts_summer_2021_leisure_cont", "cc_weekly_contacts_01_2023_leisure_cont"))
#LeisureDataCCPre$name <- factor(LeisureDataCCPre$name, levels = c("cc_weekly_contacts_2019_leisure_cont", "cc_weekly_contacts_03_2020_leisure_cont", "cc_weekly_contacts_summer_2021_leisure_cont", "cc_weekly_contacts_01_2023_leisure_cont"))
LeisureDataCCPre <- LeisureDataCCPre %>% mutate(name = case_when(name == "cc_weekly_contacts_2019_leisure_cont" ~ "CC's Leisure contacts 2019",
                                                                                     name == "cc_weekly_contacts_03_2020_leisure_cont"~ "CC's Leisure contacts 03/20",
                                                                                     name == "cc_weekly_contacts_summer_2021_leisure_cont"~ "CC's Leisure contacts summer 21",
                                                                                     name == "cc_weekly_contacts_01_2023_leisure_cont"~ "CC's Leisure contacts 01/23"))
LeisureDataCCPre$name <- factor(LeisureDataCCPre$name, levels = c("CC's Leisure contacts 2019", "CC's Leisure contacts 03/20", "CC's Leisure contacts summer 21", "CC's Leisure contacts 01/23"))
LeisureDataCCPre$value <- as.integer(LeisureDataCCPre$value)
ggplot(LeisureDataCCPre %>% filter(value <= 50)) + geom_histogram(aes(x = value, y = after_stat(count / sum(count)), fill = name), bins = 10) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks=c(0,10,20,30,40,50)) +
  theme(text = element_text(size = 15)) +
  labs(title=paste0("Filtered for CC with <= 50 leisure contacts")) +
  labs(subtitle=paste0("#CC with more than 50 leisure contacts: ", as.character(nrow(LeisureDataCCPre %>% filter(value > 50))), "\nMax # of leisure contacts (in 2019): ", as.character(max(LeisureDataCCPre$value, na.rm=TRUE))))
ggsave("LeisureContactsCCPre.png", dpi = 500, w = 9, h = 5)

LeisureDataCCDuring <- LeisureData %>% select(user_id, cc_weekly_cont_during_pandemic_2019_leisure_cont, cc_weekly_cont_during_pandemic_03_2020_leisure_cont, cc_weekly_cont_during_pandemic_summer_2021_leisure_cont, cc_weekly_cont_during_pandemic_01_2023_leisure_cont) %>%
                                              pivot_longer(cols = c("cc_weekly_cont_during_pandemic_2019_leisure_cont", "cc_weekly_cont_during_pandemic_03_2020_leisure_cont", "cc_weekly_cont_during_pandemic_summer_2021_leisure_cont", "cc_weekly_cont_during_pandemic_01_2023_leisure_cont"))
#LeisureDataCCDuring$name <- factor(LeisureDataCCDuring$name, levels = c("cc_weekly_cont_during_pandemic_2019_leisure_cont", "cc_weekly_cont_during_pandemic_03_2020_leisure_cont", "cc_weekly_cont_during_pandemic_summer_2021_leisure_cont", "cc_weekly_contacts_01_2023_leisure_cont"))
LeisureDataCCDuring <- LeisureDataCCDuring %>% mutate(name = case_when(name == "cc_weekly_cont_during_pandemic_2019_leisure_cont" ~ "CC's Leisure contacts 2019",
                                                                 name == "cc_weekly_cont_during_pandemic_03_2020_leisure_cont"~ "CC's Leisure contacts 03/20",
                                                                 name == "cc_weekly_cont_during_pandemic_summer_2021_leisure_cont"~ "CC's Leisure contacts summer 21",
                                                                 name == "cc_weekly_cont_during_pandemic_01_2023_leisure_cont"~ "CC's Leisure contacts 01/23"))
LeisureDataCCDuring$name <- factor(LeisureDataCCDuring$name, levels = c("CC's Leisure contacts 2019", "CC's Leisure contacts 03/20", "CC's Leisure contacts summer 21", "CC's Leisure contacts 01/23"))
LeisureDataCCDuring$value <- as.integer(LeisureDataCCDuring$value)
ggplot(LeisureDataCCDuring %>% filter(value <= 50)) + geom_histogram(aes(x = value, y = after_stat(count / sum(count)), fill = name), bins = 10) +
  theme_minimal() +
  facet_wrap(~name, nrow=2) +
  ylab("Frequency") +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks=c(0,10,20,30,40,50)) +
  theme(text = element_text(size = 15)) +
  labs(title=paste0("Filtered for CC with <= 50 leisure contacts")) +
  labs(subtitle=paste0("#CC with more than 50 leisure contacts: ", as.character(nrow(LeisureDataCCDuring %>% filter(value > 50))), "\nMax # of leisure contacts (in 2019): ", as.character(max(LeisureDataCCDuring$value, na.rm=TRUE))))
ggsave("LeisureContactsCCDuring.png", dpi = 500, w = 9, h = 5)
