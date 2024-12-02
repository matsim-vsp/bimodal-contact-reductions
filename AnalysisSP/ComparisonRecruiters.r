library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(ggpubr)

data_reduced <- read_csv(file = "/Users/sydney/Desktop/TwitterLimeSurvey/twitter_data.csv")

scenario <- "Twitter" #Alternative: "Twitter/Mastodon"

if(scenario == "Twitter"){
palette <- function() {
  c("#ffe6ab", "#FFD269", "#fac548", "#ECA400", "#ad8500")
}
palette2 <- function() {
  c("#FFD269", "#fac548", "#ECA400", "#ad8500", "#6b5200")
}
}else if(scenario == "Twitter/Mastodon"){
palette <- function() {
  c("#FFD269", "#006992")
}
palette2 <- function() {
  c("#ECA400", "#27476E")
}
}


# Gender ------------------------------------------------------------------

if(scenario == "Twitter"){
    GenderData <- data_reduced %>% select(gender, ref, origin) %>% 
    filter(ref %in% c("dec9d", "6c8d7", "7b598", "008b5", "4a76b")) %>%
    mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Priesemann",
                            ref == "4a76b" ~ "Bachmann",
                            ref == "008b5" ~ "Briest", 
                            ref == "7b598" ~ "Franke", 
                            ref == "6c8d7" ~ "CaleroValdez"))
} else if (scenario == "Twitter/Mastodon"){
    GenderData <- data_reduced %>% select(gender, ref, origin) %>% 
    filter(ref %in% c("dec9d")) %>%
    mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Priesemann",
                            ref == "dec9d" & origin == "6080d" ~ "Priesemann (Mastodon)"))    
}

GenderData <- GenderData %>% mutate(gender = case_when(gender == "Weiblich" ~ "female", 
                                                        gender == "Männlich" ~ "male",
                                                        gender == "Divers" ~ "diverse",
                                                        gender == "Ich möchte nicht antworten" ~ "I Don't Want To Answer")) %>% 
                                                        filter(gender != "I Don't Want To Answer")

GenderData$gender <- factor(GenderData$gender, levels = c("female", "male", "diverse", "I Don't Want To Answer"))


GenderAdd <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(GenderAdd) <- c("ref", "gender", "n", "percent")
    if(scenario == "Twitter"){
    GenderAdd[nrow(GenderAdd) + 1, ] <- c("Bachmann", "diverse", 0, 0)
    GenderAdd[nrow(GenderAdd) + 1, ] <- c("Briest", "diverse", 0, 0)
    GenderAdd[nrow(GenderAdd) + 1, ] <- c("CaleroValdez", "diverse", 0, 0)
    GenderAdd[nrow(GenderAdd) + 1, ] <- c("Franke", "diverse", 0, 0)
}

GenderAdd$n <- as.double(GenderAdd$n)
GenderAdd$percent <- as.double(GenderAdd$percent)
GenderAdd$ref <- as.character(GenderAdd$ref)
GenderAdd$gender <- as.character(GenderAdd$gender)

GenderPlot <- GenderData %>% group_by(ref) %>% filter(!is.na(ref)) %>% count(gender) %>% mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% 
rbind(GenderAdd) %>%
  mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(lci = 100/sum*lci) %>%
  mutate(lci = case_when(lci < 0 ~ 0,
                .default = lci)) %>%
  mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
  mutate(uci = 100/sum*uci) %>%
  mutate(gender = factor(gender, levels = c("female", "male", "diverse"))) %>%
ggplot(aes(gender, percent)) +
  geom_bar(aes(fill=factor(ref, levels = c("Priesemann", "Bachmann", "Briest", "Franke", "CaleroValdez", "Priesemann (Mastodon)"))), stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=gender, ymin=lci, ymax=uci, colour = factor(ref, levels = c("Priesemann", "Bachmann", "Briest", "Franke", "CaleroValdez", "Priesemann (Mastodon)"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  ylab("Share (Percentage)") +
  xlab("Gender") +
  scale_fill_manual(values = palette()) +
  scale_colour_manual(values = palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

# Age ---------------------------------------------------------------------

if(scenario == "Twitter"){
    AgeData <- data_reduced %>% select(year_of_birth, ref, origin) %>% mutate(age = 2023-year_of_birth) %>%
            mutate(age_bracket = case_when(age < 20 ~ "Below 20 (*)",
                                            age < 40 ~ "20-39",
                                            age < 60 ~ "40-59",
                                            age < 80 ~ "60-79",
                                            age < 100 ~ "80-99"))  %>%
                filter(ref %in% c("dec9d", "6c8d7", "7b598", "008b5", "4a76b")) %>%
                mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Priesemann",
                            ref ==  "6c8d7" ~ "CaleroValdez", 
                            ref == "7b598" ~ "Franke", 
                            ref == "008b5" ~ "Briest", 
                            ref == "4a76b" ~ "Bachmann"))
}else if(scenario == "Twitter/Mastodon"){
    AgeData <- data_reduced %>% select(year_of_birth, ref, origin) %>% mutate(age = 2023-year_of_birth) %>%
            mutate(age_bracket = case_when(age < 20 ~ "Below 20 (*)",
                                            age < 40 ~ "20-39",
                                            age < 60 ~ "40-59",
                                            age < 80 ~ "60-79",
                                            age < 100 ~ "80-99"))  %>%
                filter(ref %in% c("dec9d", "6c8d7", "7b598", "008b5", "4a76b")) %>%
                mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Priesemann",
                            ref == "dec9d" & origin == "6080d" ~ "Priesemann (Mastodon)"))
}

AgeAdd <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(AgeAdd) <- c("ref", "age_bracket", "n", "percent")
if(scenario == "Twitter"){
    AgeAdd[nrow(AgeAdd) + 1, ] <- c("Briest", "Below 20 (*)", 0, 0)
    AgeAdd[nrow(AgeAdd) + 1, ] <- c("Briest", "80-99", 0, 0)
    AgeAdd[nrow(AgeAdd) + 1, ] <- c("CaleroValdez", "Below 20 (*)", 0, 0)
    AgeAdd[nrow(AgeAdd) + 1, ] <- c("CaleroValdez", "60-79", 0, 0)
    AgeAdd[nrow(AgeAdd) + 1, ] <- c("CaleroValdez", "80-99", 0, 0)
    AgeAdd[nrow(AgeAdd) + 1, ] <- c("Franke", "Below 20 (*)", 0, 0)
    AgeAdd[nrow(AgeAdd) + 1, ] <- c("Franke", "60-79", 0, 0)
    AgeAdd[nrow(AgeAdd) + 1, ] <- c("Franke", "80-99", 0, 0)
}else if(scenario == "Twitter/Mastodon"){
    AgeAdd[nrow(AgeAdd) + 1, ] <- c("Priesemann (Mastodon)", "Below 20 (*)", 0, 0)
    AgeAdd[nrow(AgeAdd) + 1, ] <- c("Priesemann (Mastodon)", "80-99", 0, 0)
}

AgeAdd$n <- as.double(AgeAdd$n)
AgeAdd$percent <- as.double(AgeAdd$percent)
AgeAdd$ref <- as.character(AgeAdd$ref)
AgeAdd$age_bracket <- as.character(AgeAdd$age_bracket)
 
AgePlot <- AgeData %>% filter(!is.na(age_bracket)) %>% filter(!is.na(ref)) %>% group_by(ref) %>% count(age_bracket) %>% 
            mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% rbind(AgeAdd) %>%
              mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                mutate(lci = 100/sum*lci) %>%
                mutate(lci = case_when(lci < 0 ~ 0,
                                .default = lci)) %>%
                mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                mutate(uci = 100/sum*uci) %>%
ggplot(aes(factor(age_bracket, levels = c("Below 20 (*)", "20-39", "40-59", "60-79", "80-99")), percent)) +
  geom_bar(aes(fill=factor(ref, levels = c("Priesemann", "Bachmann", "Briest", "Franke", "CaleroValdez", "Priesemann (Mastodon)"))), stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=age_bracket, ymin=lci, ymax=uci, colour = factor(ref, levels = c("Priesemann", "Bachmann", "Briest", "Franke", "CaleroValdez", "Priesemann (Mastodon)"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
    theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  ylab("") +
  #ylab("Share (Percentage)") +
  xlab("Age Bracket (2023)") +
  scale_fill_manual(values = palette()) +
  scale_colour_manual(values = palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

# Household Size ----------------------------------------------------

# The following section compares the household sizes for the survey, MuSPAD and the Federal Statistical Office

if(scenario == "Twitter"){
    HouseholdData <- data_reduced %>% select(ref, hsld_size_01_2023_, origin) %>%
                filter(ref %in% c("dec9d", "6c8d7", "7b598", "008b5", "4a76b")) %>%
                mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Priesemann",
                            ref ==  "6c8d7" ~ "CaleroValdez", 
                            ref == "7b598" ~ "Franke", 
                            ref == "008b5" ~ "Briest", 
                            ref == "4a76b" ~ "Bachmann"))
}else if(scenario == "Twitter/Mastodon"){
    HouseholdData <- data_reduced %>% select(ref, hsld_size_01_2023_, origin) %>%
                filter(ref %in% c("dec9d", "6c8d7", "7b598", "008b5", "4a76b")) %>%
                mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Priesemann",
                            ref == "dec9d" & origin == "6080d" ~ "Priesemann (Mastodon)"))
}

HouseholdData <- HouseholdData %>% mutate(hsld_size_01_2023_ = case_when(hsld_size_01_2023_ == 1 ~ "1", hsld_size_01_2023_ == 2 ~ "2", hsld_size_01_2023_ == 3 ~ "3", hsld_size_01_2023_ == 4 ~ "4", hsld_size_01_2023_ >= 5 ~ "5+"))

HouseholdAdd <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(HouseholdAdd) <- c("ref", "hsld_size_01_2023_", "n", "percent")
if(scenario == "Twitter"){
    HouseholdAdd[nrow(HouseholdAdd) + 1, ] <- c("CaleroValdez", "3", 0, 0)
    HouseholdAdd[nrow(HouseholdAdd) + 1, ] <- c("CaleroValdez", "4", 0, 0)
    HouseholdAdd[nrow(HouseholdAdd) + 1, ] <- c("Franke", "5+", 0, 0)
}

HouseholdAdd$ref <- as.character(HouseholdAdd$ref)
HouseholdAdd$hsld_size_01_2023_ <- as.character(HouseholdAdd$hsld_size_01_2023_)
HouseholdAdd$n <- as.double(HouseholdAdd$n)
HouseholdAdd$percent <- as.double(HouseholdAdd$percent)

HouseholdPlot <- HouseholdData %>% filter(!is.na(ref))  %>% group_by(ref) %>% filter(!is.na(hsld_size_01_2023_ )) %>% 
                  count(hsld_size_01_2023_ ) %>% mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% rbind(HouseholdAdd) %>%
                    mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(lci = 100/sum*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0,
                                    .default = lci)) %>%
                    mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(uci = 100/sum*uci) %>%
 ggplot(aes(hsld_size_01_2023_ , percent)) +
  geom_bar(aes(fill=factor(ref, levels = c("Priesemann", "Bachmann", "Briest", "Franke", "CaleroValdez", "Priesemann (Mastodon)"))), stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=hsld_size_01_2023_, ymin=lci, ymax=uci, colour = factor(ref, levels = c("Priesemann", "Bachmann", "Briest", "Franke", "CaleroValdez", "Priesemann (Mastodon)"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  #ylab("") +
  ylab("Share (Percentage)") +
  xlab("Household size (# Members)") +
  scale_fill_manual(values = palette()) +
  scale_colour_manual(values = palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

# Children under 14 ------------------------------------------------------------------

if(scenario == "Twitter"){
Children <- data_reduced  %>% select(ref, total_hsld_size_persons_under_14, origin) %>%
                        filter(ref %in% c("dec9d", "6c8d7", "7b598", "008b5", "4a76b")) %>%
                mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Priesemann",
                        ref ==  "6c8d7" ~ "CaleroValdez", 
                        ref == "7b598" ~ "Franke", 
                        ref == "008b5" ~ "Briest", 
                        ref == "4a76b" ~ "Bachmann")) %>%
                            mutate(total_hsld_size_persons_under_14 = case_when(total_hsld_size_persons_under_14  == 0 ~ "0",
                            total_hsld_size_persons_under_14  == 1 ~ "1",
                            total_hsld_size_persons_under_14  == 2 ~ "2",
                            total_hsld_size_persons_under_14  == 3 ~ "3+",
                            total_hsld_size_persons_under_14  == 4 ~ "3+"))
}else if(scenario == "Twitter/Mastodon"){
Children <- data_reduced %>% select(ref, total_hsld_size_persons_under_14, origin) %>%
                        filter(ref %in% c("dec9d", "6c8d7", "7b598", "008b5", "4a76b")) %>%
                mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Priesemann",
                        ref == "dec9d" & origin == "6080d" ~ "Priesemann (Mastodon)")) %>%
                            mutate(total_hsld_size_persons_under_14 = case_when(total_hsld_size_persons_under_14  == 0 ~ "0",
                            total_hsld_size_persons_under_14  == 1 ~ "1",
                            total_hsld_size_persons_under_14  == 2 ~ "2",
                            total_hsld_size_persons_under_14  == 3 ~ "3+",
                            total_hsld_size_persons_under_14  == 4 ~ "3+"))    
}

ChildrenAdd <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(ChildrenAdd) <- c("ref", "total_hsld_size_persons_under_14", "n", "percent")
if(scenario == "Twitter"){
    ChildrenAdd[nrow(ChildrenAdd) + 1, ] <- c("Briest", "3+", 0, 0)
    ChildrenAdd[nrow(ChildrenAdd) + 1, ] <- c("CaleroValdez", "1", 0, 0)
    ChildrenAdd[nrow(ChildrenAdd) + 1, ] <- c("CaleroValdez", "2", 0, 0)
    ChildrenAdd[nrow(ChildrenAdd) + 1, ] <- c("CaleroValdez", "3+", 0, 0)
    ChildrenAdd[nrow(ChildrenAdd) + 1, ] <- c("Franke", "3+", 0, 0)
}

ChildrenAdd$ref <- as.character(ChildrenAdd$ref)
ChildrenAdd$total_hsld_size_persons_under_14 <- as.character(ChildrenAdd$total_hsld_size_persons_under_14)
ChildrenAdd$n <- as.double(ChildrenAdd$n)
ChildrenAdd$percent <- as.double(ChildrenAdd$percent)

ChildrenPlot <- Children %>% filter(!is.na(ref))  %>% group_by(ref) %>% filter(!is.na(total_hsld_size_persons_under_14)) %>% 
                  count(total_hsld_size_persons_under_14) %>% filter(!is.na(total_hsld_size_persons_under_14)) %>%
                  mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% rbind(ChildrenAdd) %>%
                    mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                    mutate(lci = 100/sum*lci) %>%
                    mutate(lci = case_when(lci < 0 ~ 0,
                .default = lci)) %>%
                 mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
                 mutate(uci = 100/sum*uci) %>%
ggplot(aes(total_hsld_size_persons_under_14, percent)) +
  geom_bar(aes(fill=factor(ref, levels = c("Priesemann", "Bachmann", "Briest", "Franke", "CaleroValdez", "Priesemann (Mastodon)"))), stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=total_hsld_size_persons_under_14, ymin=lci, ymax=uci, colour = factor(ref, levels = c("Priesemann", "Bachmann", "Briest", "Franke", "CaleroValdez", "Priesemann (Mastodon)"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  ylab("") +
  #ylab("Share (Percentage)") +
  xlab("Children Under 14") +
  scale_fill_manual(values = palette()) +
  scale_colour_manual(values = palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

# Education / Occupation --------------------------------------------------

#Education 

if(scenario == "Twitter"){
    educationLevel <- data_reduced %>% filter(!is.na(ref))  %>% select(highest_educational_qualification, ref, origin) %>%
                            filter(ref %in% c("dec9d", "6c8d7", "7b598", "008b5", "4a76b")) %>%
                        mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Priesemann",
                            ref ==  "6c8d7" ~ "CaleroValdez", 
                            ref == "7b598" ~ "Franke", 
                            ref == "008b5" ~ "Briest", 
                            ref == "4a76b" ~ "Bachmann"))
} else if(scenario == "Twitter/Mastodon"){
    educationLevel <- data_reduced %>% select(highest_educational_qualification, ref, origin) %>%
                            filter(ref %in% c("dec9d", "6c8d7", "7b598", "008b5", "4a76b")) %>%
                        mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Priesemann",
                            ref == "dec9d" & origin == "6080d" ~ "Priesemann (Mastodon)"))    
}

educationLevel <- educationLevel %>% mutate(highest_educational_qualification = case_when(highest_educational_qualification == "Haupt-/ Volksschulabschluss" ~ "Certification\nafter 9 years",
                                                                                          highest_educational_qualification == "Realschulabschluss" ~ "Certification\nafter 10 years",
                                                                                          highest_educational_qualification == "Abitur / Fachhochschulabitur" ~ "Higher Education",
                                                                                          highest_educational_qualification == "Anderer" ~ "Other/None"))

EducationAdd <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(EducationAdd) <- c("ref", "highest_educational_qualification", "n", "percent")
if(scenario == "Twitter"){
    EducationAdd[nrow(EducationAdd) + 1, ] <- c("Bachmann", "Certification\nafter 9 years", 0, 0)
    EducationAdd[nrow(EducationAdd) + 1, ] <- c("Briest", "Certification\nafter 9 years", 0, 0)
    EducationAdd[nrow(EducationAdd) + 1, ] <- c("CaleroValdez", "Certification\nafter 10 years", 0, 0)
    EducationAdd[nrow(EducationAdd) + 1, ] <- c("CaleroValdez", "Certification\nafter 9 years", 0, 0)
    EducationAdd[nrow(EducationAdd) + 1, ] <- c("Franke", "Certification\nafter 9 years", 0, 0)
}

EducationAdd$ref <- as.character(EducationAdd$ref)
EducationAdd$highest_educational_qualification <- as.character(EducationAdd$highest_educational_qualification)
EducationAdd$n <- as.double(EducationAdd$n)
EducationAdd$percent <- as.double(EducationAdd$percent)

EducationPlot <- educationLevel %>% filter(!is.na(ref))  %>% group_by(ref) %>% filter(!is.na(highest_educational_qualification)) %>% 
            count(highest_educational_qualification) %>% 
            mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% rbind(EducationAdd) %>%
            mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
            mutate(lci = 100/sum*lci) %>%
            mutate(lci = case_when(lci < 0 ~ 0,
                .default = lci)) %>%
            mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
            mutate(uci = 100/sum*uci) %>%
ggplot(aes(factor(highest_educational_qualification, levels = c("Higher Education", "Certification\nafter 10 years", "Certification\nafter 9 years", "Other/None")), percent)) +
  geom_bar(aes(fill=factor(ref, levels = c("Priesemann", "Bachmann", "Briest", "Franke", "CaleroValdez", "Priesemann (Mastodon)"))), stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=highest_educational_qualification, ymin=lci, ymax=uci, colour = factor(ref, levels = c("Priesemann", "Bachmann", "Briest", "Franke", "CaleroValdez", "Priesemann (Mastodon)"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  #ylab("") +
  ylab("Share (Percentage)") +
  xlab("Education") +
  scale_fill_manual(values = palette()) +
  scale_colour_manual(values = palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=0.7))

#Occupation

# To do: The following section needs to be updated once we've received the according data from MuSPAD

if(scenario == "Twitter"){
    currentOccupation <- data_reduced %>% filter(!is.na(ref))  %>% select(ref, current_occupation, origin) %>%
                            filter(ref %in% c("dec9d", "6c8d7", "7b598", "008b5", "4a76b")) %>%
                        mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Priesemann",
                            ref ==  "6c8d7" ~ "CaleroValdez", 
                            ref == "7b598" ~ "Franke", 
                            ref == "008b5" ~ "Briest", 
                            ref == "4a76b" ~ "Bachmann"))
}else if(scenario == "Twitter/Mastodon"){
    currentOccupation <- data_reduced %>% select(ref, current_occupation, origin) %>%
                            filter(ref %in% c("dec9d", "6c8d7", "7b598", "008b5", "4a76b")) %>%
                        mutate(ref = case_when(ref == "dec9d" & origin == "b73c2" ~ "Priesemann",
                            ref == "dec9d" & origin == "6080d" ~ "Priesemann (Mastodon)"))
}

currentOccupation <- currentOccupation %>% mutate(current_occupation = case_when(current_occupation == "Ich bin in einem anderen Beruf tätig." ~ "Other",
                                                                                 current_occupation == "Ich bin als Lehrer:in oder Erzieher:in tätig." ~ "Teaching Sector", 
                                                                                 current_occupation == "Ich bin Rentner:in oder Pensionär:in." ~ "Retired",
                                                                                 current_occupation == "Ich bin arbeitssuchend." ~ "Unemployed", 
                                                                                 current_occupation == "Ich bin im Studium oder in der Ausbildung." ~ "Student",
                                                                                 current_occupation == "Ich bin in einem medizinischen oder pflegerischen Beruf bei einem Gesundheitsversorger tätig." ~ "Medical Sector", 
                                                                                 current_occupation == "Andere (z.B. Elternzeit, Sabbatical)" ~ "Other",
                                                                                 current_occupation == "Ich möchte nicht antworten" ~ "Unknown",
                                                                                 is.na(current_occupation) ~ "Unknown")) %>%
                                            filter(current_occupation != "Unknown")

OccupationAdd <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(OccupationAdd) <- c("ref", "current_occupation", "n", "percent")
if(scenario == "Twitter"){
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("Briest", "Unemployed", 0, 0)
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("CaleroValdez", "Medical Sector", 0, 0)
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("CaleroValdez", "Retired", 0, 0)
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("CaleroValdez", "Student", 0, 0)
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("CaleroValdez", "Teaching Sector", 0, 0)
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("CaleroValdez", "Unemployed", 0, 0)
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("Franke", "Medical Sector", 0, 0)
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("Franke", "Student", 0, 0)
    OccupationAdd[nrow(OccupationAdd) + 1, ] <- c("Franke", "Unemployed", 0, 0)
}

OccupationAdd$ref <- as.character(OccupationAdd$ref)
OccupationAdd$current_occupation <- as.character(OccupationAdd$current_occupation)
OccupationAdd$n <- as.double(OccupationAdd$n)
OccupationAdd$percent <- as.double(OccupationAdd$percent)

OccupationPlot <- currentOccupation %>% filter(!is.na(ref))  %>% group_by(ref) %>% filter(!is.na(current_occupation)) %>% 
            count(current_occupation) %>% filter(!is.na(current_occupation)) %>%
            mutate(percent = 100 * n / sum(n), sum = sum(n)) %>% 
            rbind(OccupationAdd) %>%
              mutate(lci = sum*(n/sum - 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
            mutate(lci = 100/sum*lci) %>%
            mutate(lci = case_when(lci <= 0 ~ 0,
                lci == 100 ~ 0,
                .default = lci)) %>%
            mutate(uci = sum*(n/sum + 1.96*(((n/sum*(1-n/sum))/sum)^0.5))) %>%
            mutate(uci = 100/sum*uci) %>%
ggplot(aes(current_occupation, percent)) +
  geom_bar(aes(fill=factor(ref, levels = c("Priesemann", "Bachmann", "Briest", "Franke", "CaleroValdez", "Priesemann (Mastodon)"))), stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(x=current_occupation, ymin=lci, ymax=uci, colour = factor(ref, levels = c("Priesemann", "Bachmann", "Briest", "Franke", "CaleroValdez", "Priesemann (Mastodon)"))), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  theme(plot.margin=unit(c(1,1,1,1), 'cm')) +
  #facet_wrap(~name, nrow=2) +
  ylab("") +
  xlab("Current Occupation") +
  scale_fill_manual(values = palette()) +
  scale_colour_manual(values = palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50,75,100)) +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=0.7))


## All plots together
#plot_grid(GenderPlot, AgePlot, HouseholdPlot, ChildrenPlot, EducationPlot, OccupationPlot, labels = "AUTO", nrow = 3, label_size = 24, rel_heights = c(1,1,1.25))

ggarrange(GenderPlot, AgePlot, HouseholdPlot, ChildrenPlot, EducationPlot, OccupationPlot, labels = c("A", "B", "C", "D", "E", "F"), nrow = 3, ncol = 2,font.label = list(size = 30), heights = c(1,1,1.25), common.legend = TRUE, legend = "bottom")

if(scenario == "Twitter"){
    ggsave("DemographicComparison_TwitterRecruiter.pdf", dpi = 500, w = 21, h = 24)
    ggsave("DemographicComparison_TwitterRecruiter.png", dpi = 500, w = 21, h = 24)
}else if(scenario == "Twitter/Mastodon"){
    ggsave("DemographicComparison_TwvsMastRecruiter.pdf", dpi = 500, w = 21, h = 24)
    ggsave("DemographicComparison_TwvsMastRecruiter.png", dpi = 500, w = 21, h = 24)
}






