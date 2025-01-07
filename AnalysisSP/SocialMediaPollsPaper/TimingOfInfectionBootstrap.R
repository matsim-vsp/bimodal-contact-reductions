
library(tidyverse)

# Author: S. Paltra, contact: paltra@tu-berlin.de

setwd("./SocialMediaPollsPaper") # You need to set the working directory accordingly, otherwise the cleaning script (below does not work)
source("TimingOfInfection.R")
source("MuSPAD.R")

eighteen_thirtynine <- 31.9
fourty_fitynine <- 32.2
sixty_seventynine <- 27.2 + 8.7 #Moved 80+ year olds to 60-79 year olds as there are so few (only 2) that they weirdly influence the sample others

age_groups <- c("18-39", "40-59", "60+")

data <- data.frame(matrix(nrow = 0, ncol = 13))
colnames(data) <- c("date_f1_inf","f1_pcr_doc","f1_pcr_center","date_s2_inf","s2_pcr_doc","s2_pcr_center","date_t3_inf","t3_pcr_doc","t3_pcr_center","year_of_birth","age","age_bracket", "iteration")

for(i in 1:200){
    for(age_group in age_groups){

        raw_data <- readRDS(file = "/Users/sydney/Desktop/cleaned_data.rds")
        data_reduced <- raw_data %>% select(num_c19_infs, date_f1_inf, f1_pcr_doc, f1_pcr_center, date_s2_inf, s2_pcr_doc, s2_pcr_center, date_t3_inf, t3_pcr_doc, t3_pcr_center, year_of_birth) %>% 
                    mutate(age = 2023-year_of_birth) %>%
                    mutate(age_bracket = case_when(age < 39 ~ "18-39",
                                                age < 60 ~ "40-59",
                                                age < 100 ~ "60+")) 
        data_reduced <- data_reduced %>% filter(!is.na(num_c19_infs)) %>%
        select(-num_c19_infs) %>% filter(age_bracket == age_group) %>% mutate(iteration = i)

        if(age_group == "18-39"){
        size = eighteen_thirtynine * 10
        }
        if(age_group == "40-59"){
        size = fourty_fitynine * 10
        }
        if(age_group == "60+"){
        size = sixty_seventynine * 10
        }
        if(age_group == "80+"){
        size = eightyplus * 10
        }

        slices <- slice_sample(data_reduced, n = size, replace = TRUE)

        data <- rbind(data, slices)
    }
}

data_reduced <- data

data <- data.frame(matrix(nrow = 0, ncol = ncol(MuSPADnewplusold)))
colnames(data) <- colnames(MuSPADnewplusold)

for(i in 1:200){
    MuSPAD <- MuSPADnewplusold %>% 
    mutate(firstinfection = make_date(MuSPADnewplusold$s22_positive_PCR_year_1, MuSPADnewplusold$s22_positive_PCR_month_1, MuSPADnewplusold$s22_positive_PCR_day_1)) %>%
    mutate(secondinfection = make_date(MuSPADnewplusold$s22_positive_PCR_year_2, MuSPADnewplusold$s22_positive_PCR_month_2, MuSPADnewplusold$s22_positive_PCR_day_2)) %>%
    mutate(thirdinfection = make_date(MuSPADnewplusold$s22_positive_PCR_year_3, MuSPADnewplusold$s22_positive_PCR_month_3, MuSPADnewplusold$s22_positive_PCR_day_3)) %>%
    select(s22_birth_date_yyyy, firstinfection, secondinfection, thirdinfection, s23_test_covid_2023, w22_positive_PCR_day_1) %>%
    mutate(age = 2023-s22_birth_date_yyyy) %>%
              mutate(age_bracket = case_when(age < 39 ~ "18-39",
                                             age < 60 ~ "40-59",
                                             age < 100 ~ "60+")) %>% mutate(iteration = i)

        if(age_group == "18-39"){
        size = eighteen_thirtynine * 10
        }
        if(age_group == "40-59"){
        size = fourty_fitynine * 10
        }
        if(age_group == "60+"){
        size = sixty_seventynine * 10
        }
        if(age_group == "80+"){
        size = eightyplus * 10
        }

        slices <- slice_sample(MuSPAD, n = size, replace = TRUE)

        data <- rbind(data, slices)
}

MuSPAD <- data 

timingOfInfection(bootstrapping = "yes")
