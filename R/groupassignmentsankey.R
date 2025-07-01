library(tidyverse)
library(ggsankey)
library(here)
library(ggsankeyfier)
library(ggpubr)

# This script assigns the contact reductions of the participants to the groups "Strong Reduction", "Intermediate Reduction", and "Little Change".
# The output of this script is used for the Sankey plot.

here()
source("./R/mytheme.r")
source("./R/DataPrep.R")

data_reduced_tidy_rel %>%  
  filter(!is.na(value)) %>% 
  filter(value > -150) %>% filter(value < 100) %>%    
  filter(!is.na(TypeOfContact)) %>% group_by(TypeOfContact, WhoseContacts, time) %>%
  summarise(meanRed = mean(value))

## Group Sizes When Fitting 2 Distributions to the Data

#fileNames <- list.files("./data/BimodalGroups/", pattern = "*.csv")

# 
# dataf <- data.frame(matrix(nrow=0, ncol=4))
# colnames(dataf) <- c("value", "group_0", "group_1", "group")
# 
#   for(file in fileNames){
#   
#   group_bel <- paste0("./data/BimodalGroups/", file)
#   
#   group_belonging <- read_csv(paste0("./data/BimodalGroups/", group_bel))
#   group_belonging <- group_belonging %>% mutate(value = seq(-100,75,length.out=500))
#   group_belonging$value <- round(group_belonging$value)
#   group_belonging <- group_belonging %>% group_by(value) %>% 
#     summarise(group_0 = mean(first_group), group_1 = mean(second_group))
#   
#   filename <- paste0(str_split(file, "csv")[[1]][1], "csv")
#   red <- paste0(getwd(), "/data/", filename)
#   reductions <- read_csv(red)
#   reductions <- reductions %>% dplyr::select(value)
#   reductions$value <- round(reductions$value)
#   reductions <- reductions %>% left_join(group_belonging, by = "value") %>%
#     mutate(group = case_when(
#       runif(n()) < group_0 ~ 0,
#       TRUE ~ 1
#     ))
#   reductions <- reductions %>% mutate(filename = file)
#   dataf <- rbind(dataf, reductions)
# }
# 
# print(dataf %>% group_by(filename) %>% count(group) %>% mutate(perc = n/sum(n)), n = 100)

## Group Sizes When Fitting 3 Distributions to the Data

dataf <- data.frame(matrix(nrow=0, ncol=5))
colnames(dataf) <- c("value", "group_0", "group_1", "group_2", "group")

group_bel_long <- data.frame(matrix(nrow=0, ncol=7))
colnames(group_bel_long) <- c("value", "ID", "group_0", "group_1", "group_2", "TypeOfContact", "time")

fileNames <- list.files("./data/TrimodalGroups/", pattern = "*.csv")

for(file in fileNames){
  
  group_bel <- paste0(file)
  group_belonging <- read_csv(paste0("./data/TrimodalGroups/", group_bel))
  group_belonging <- group_belonging %>% mutate(value = seq(-100,75,length.out=500))
  group_belonging$value <- round(group_belonging$value)
  group_belonging <- group_belonging %>% group_by(value) %>% 
    summarise(group_0 = mean(first_group), group_1 = mean(second_group), group_2 = mean(third_group))
  
  filename <- paste0(str_split(file, "csv")[[1]][1], "csv")
  red <- paste0(getwd(), "/data/", filename)
  reductions <- read_csv(red)
  reductions <- reductions %>%
    #filter(cond_none == "Ja") %>%
    #filter(gender == "Männlich") %>%
    #filter(age_bracket == "60+") %>%
    dplyr::select(c(value, ID)) 
  reductions$value <- round(reductions$value)
  reductions <- reductions %>% left_join(group_belonging, by = "value") %>%
    mutate(group = case_when(
      runif(n()) < group_0 ~ 0,
      runif(n()) < group_0 + group_1 ~ 1,
      TRUE ~ 2
    ))
  reductions <- reductions %>% mutate(group = case_when((value > 4 & group == 1) ~ NA, .default=group))
  reductions <- reductions %>% mutate(filename = file)
  dataf <- rbind(dataf, reductions)
  
  group_belonging <- group_belonging %>% mutate(time = case_when(grepl("032020", file) ~ "03/2020",
                                                                 grepl("Summer", file) ~ "Summer 2021",
                                                                 grepl("012023", file) ~ "01/2023")) %>%
    mutate(TypeOfContact = case_when(grepl("Work", file) ~ "Work", grepl("Leisure", file) ~ "Leisure"))
  
  if(grepl("032020", file) & grepl("Work", file)){
    Work032020 <- reductions
  } else if(grepl("032020", file) & grepl("Leisure", file)){
    Leisure032020 <- reductions
  } else if(grepl("Summer", file) & grepl("Work", file)){
    WorkSummer2021 <- reductions
  } else  if(grepl("Summer", file) & grepl("Leisure", file)){
    LeisureSummer2021 <- reductions
  } else  if(grepl("012023", file) & grepl("Work", file)){
    Work012023 <- reductions
  } else if(grepl("012023", file) & grepl("Leisure", file)){
    Leisure012023 <- reductions
  }
  
  group_bel_long <- rbind(group_bel_long, group_belonging)
}

print(dataf %>% group_by(filename) %>% count(group) %>% mutate(perc = n/sum(n)), n = 100)

## Group Sizes When Fitting 4 Distributions to the Data

#fileNames <- list.files("./data/QuatromodalGroups/", pattern = "*.csv")
# 
# 
# dataf <- data.frame(matrix(nrow=0, ncol=6))
# colnames(dataf) <- c("value", "group_0", "group_1", "group_2", "group_3", "group")
# 
# for(file in fileNames){
#   
#  group_belonging <- read_csv(paste0("./data/QuatromodalGroups/", group_bel))
#   
#   group_belonging <- read_csv(group_bel)
#   group_belonging <- group_belonging %>% mutate(value = seq(-100,75,length.out=500))
#   group_belonging$value <- round(group_belonging$value)
#   group_belonging <- group_belonging %>% group_by(value) %>% 
#     summarise(group_0 = mean(first_group), group_1 = mean(second_group), group_2 = mean(third_group), group_3=mean(fourth_group))
#   
#   filename <- paste0(str_split(file, "csv")[[1]][1], "csv")
#   red <- paste0(getwd(), "/data/", filename)
#   reductions <- read_csv(red)
#   reductions <- reductions %>% dplyr::select(value)
#   reductions$value <- round(reductions$value)
#   reductions <- reductions %>% left_join(group_belonging, by = "value") %>%
#     mutate(group = case_when(
#       runif(n()) < group_0 ~ 0,
#       runif(n()) < group_0 + group_1 ~ 1,
#       runif(n()) < group_0 + group_1 + group_2 ~ 2,
#       TRUE ~ 3
#     ))
#   reductions <- reductions %>% mutate(filename = file)
#   dataf <- rbind(dataf, reductions)
# }
# 
# print(dataf %>% group_by(filename) %>% count(group) %>% mutate(perc = n/sum(n)), n = 100)
