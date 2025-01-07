library(tidyverse)
library(MMWRweek)
library(see)
library(RColorBrewer)

# Author: S. Paltra, contact: paltra@tu-berlin.de

# This script consists of two parts
# 1: Bar chart depicting the answer to the 2nd Twitter poll (timing of infection)
# 2: Comparison the survey respondents' incidence to the official incidence by RKI and incidence according to MuSPAD study


# 1st Part ----------------------------------------------------------------

InfectionTimingTwitter <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(InfectionTimingTwitter) <- c("TimeFrame", "Share", "OverallNoParticipants", "Recruiter")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Jan - Apr", 0.227, 1131, "Recruiter 1 (Twitter)")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("May - Aug", 0.228, 1131, "Recruiter 1 (Twitter)")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Sep - Dec", 0.192, 1131, "Recruiter 1 (Twitter)")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Show Me The Answer", 0.352, 1131, "Recruiter 1 (Twitter)")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Jan - Apr", 0.14, 738, "Recruiter 1 (Mastodon)")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("May - Aug", 0.18, 738, "Recruiter 1 (Mastodon)")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Sep - Dec", 0.15, 738, "Recruiter 1 (Mastodon)")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Show Me The Answer", 0.54, 738, "Recruiter 1 (Mastodon)")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Jan - Apr", 0.13, 23, "Recruiter 5")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("May - Aug", 0.174, 23, "Recruiter 5")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Sep - Dec", 0.261, 23, "Recruiter 5")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Show Me The Answer", 0.435, 23, "Recruiter 5")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Jan - Apr", 0.231, 39, "Recruiter 4")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("May - Aug", 0.333, 39, "Recruiter 4")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Sep - Dec", 0.051, 39, "Recruiter 4")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Show Me The Answer", 0.385, 39, "Recruiter 4")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Jan - Apr", 0.355, 172, "Recruiter 3")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("May - Aug", 0.151, 172, "Recruiter 3")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Sep - Dec", 0.076, 172, "Recruiter 3")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Show Me The Answer", 0.419, 172, "Recruiter 3")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Jan - Apr", 0.196, 764, "Recruiter 2")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("May - Aug", 0.204, 764, "Recruiter 2")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Sep - Dec", 0.147, 764, "Recruiter 2")
InfectionTimingTwitter[nrow(InfectionTimingTwitter) + 1, ] <- c("Show Me The Answer", 0.453, 764, "Recruiter 2")
InfectionTimingTwitter$Share <- as.double(InfectionTimingTwitter$Share)
InfectionTimingTwitter$OverallNoParticipants <- as.double(InfectionTimingTwitter$OverallNoParticipants)
InfectionTimingTwitter <- InfectionTimingTwitter %>% mutate(NoVoted = Share * OverallNoParticipants)

#Adding MuSPAD data
MuSPAD <- MuSPADnewplusold %>% 
mutate(firstinfection = make_date(MuSPADnewplusold$s22_positive_PCR_year_1, MuSPADnewplusold$s22_positive_PCR_month_1, MuSPADnewplusold$s22_positive_PCR_day_1)) %>%
mutate(secondinfection = make_date(MuSPADnewplusold$s22_positive_PCR_year_2, MuSPADnewplusold$s22_positive_PCR_month_2, MuSPADnewplusold$s22_positive_PCR_day_2)) %>%
mutate(thirdinfection = make_date(MuSPADnewplusold$s22_positive_PCR_year_3, MuSPADnewplusold$s22_positive_PCR_month_3, MuSPADnewplusold$s22_positive_PCR_day_3)) %>%
select(firstinfection, secondinfection, thirdinfection, w22_positive_PCR_day_1, s23_test_covid_2023)

MuSPADplot <- MuSPAD %>% mutate(test = case_when(abs(as.Date(w22_positive_PCR_day_1) - as.Date(firstinfection)) < 14 ~ NA,
                                                  abs(as.Date(w22_positive_PCR_day_1) - as.Date(secondinfection)) < 14 ~ NA,
                                                  abs(as.Date(w22_positive_PCR_day_1) - as.Date(thirdinfection)) < 14 ~ NA,
                                                  abs(as.Date(w22_positive_PCR_day_1) - as.Date(s23_test_covid_2023)) < 14 ~ NA,
                                                  .default = w22_positive_PCR_day_1
                                                  ))

no_time_infections <- MuSPADplot %>% pivot_longer(cols=c("firstinfection", "secondinfection", "thirdinfection", "w22_positive_PCR_day_1", "s23_test_covid_2023"))
no_time_infections <- no_time_infections %>%
filter(!is.na(value)) %>%
filter(value > as.Date("2021/12/31")) %>%
filter(value < as.Date("2023/01/01")) %>%
mutate(TimeFrame= case_when(value <  as.Date("2022/04/01") ~ "Jan - Apr",
                          value < as.Date("2022/09/01") ~ "May - Aug",
                          .default = "Sep - Dec"))
colnames(no_time_infections)[1] <- "CounterInfection"
colnames(no_time_infections)[2] <- "DateInfection"

no_time_infections  <- no_time_infections %>% count(TimeFrame) %>% mutate(Share = 100*n/(sum(n))) %>% mutate(Recruiter = "MuSPAD") %>% mutate(OverallNoParticipants = sum(n))
colnames(no_time_infections)[2] <- "NoVoted"
InfectionTimingTwitter <- rbind(InfectionTimingTwitter, no_time_infections)

#Adding rki data
rkidata <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Deutschland.csv")
rkidata <- rkidata %>% 
        filter(Altersgruppe == "00+") %>% 
        filter(Meldedatum < "2023-10-01") %>%
        mutate(weekday = wday(Meldedatum)) %>%
        filter(weekday == 1)

rkidata <- rkidata %>% select(Meldedatum, `Faelle_7-Tage`)
rkidata <- rkidata %>% filter(Meldedatum > as.Date("2021/12/31")) %>%
filter(Meldedatum < as.Date("2023/01/01")) %>%
mutate(TimeFrame = case_when(Meldedatum <  as.Date("2022/04/01") ~ "Jan - Apr",
                          Meldedatum < as.Date("2022/09/01") ~ "May - Aug",
                          .default = "Sep - Dec"))
colnames(rkidata)[2] <- "CounterInfection"
colnames(rkidata)[1] <- "DateInfection"

rkidata <- rkidata %>% group_by(TimeFrame) %>% summarise(NoVoted = sum(CounterInfection)) %>% ungroup() %>% mutate(Share = 100*NoVoted/(sum(NoVoted))) %>% mutate(OverallNoParticipants = sum(NoVoted)) %>% mutate(Recruiter = "RKI")
colnames(rkidata)[2] <- "NoVoted"

InfectionTimingTwitter <- rbind(InfectionTimingTwitter, rkidata)

palette_recruiters_bars <- function(){
  c("#253494", "#ffffcc", "#7fcdbb", "#2c7fb8", "#c7e9b4", "#663300", "#990000", "#9fadaf")
}
palette_recruiters_errorbars <- function(){
  c("#1a2569", "#c9c99f", "#5e978a", "#1d577d", "#8ba37d", "#261300", "#5c0000", "#515859")
}

twitterplot <- InfectionTimingTwitter %>% filter(TimeFrame != "Show Me The Answer")  %>%
                            group_by(TimeFrame, Recruiter) %>% summarise(n = sum(NoVoted)) %>% group_by(Recruiter) %>%
                            mutate(percent = 100 * n / sum(n)) %>%
                            mutate(lci = sum(n)*(n/sum(n) - 1.96*(((n/sum(n)*(1-n/sum(n)))/sum(n))^0.5))) %>%
                            mutate(lci = 100/sum(n)*lci) %>%
                            mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
                            mutate(uci = sum(n)*(n/sum(n) + 1.96*(((n/sum(n)*(1-n/sum(n)))/sum(n))^0.5))) %>%
                            mutate(uci = 100/sum(n)*uci) %>%
                            mutate(Recruiter = factor(Recruiter, levels = c("Recruiter 1 (Twitter)", "Recruiter 2", "Recruiter 3", "Recruiter 4", "Recruiter 5", "Recruiter 1 (Mastodon)", "MuSPAD", "RKI"))) %>%
ggplot(aes(x = TimeFrame, y = percent)) +
geom_bar(aes(fill=Recruiter), stat = "identity", position = "dodge", width = 0.8) +
geom_errorbar(aes(x=TimeFrame, ymin=lci, ymax=uci, colour = Recruiter), position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
theme_minimal() +
scale_fill_manual(values = palette_recruiters_bars()) +
scale_color_manual(values = palette_recruiters_errorbars()) +
scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,10,20,30,40,50, 60,70,80), limits = c(0,80)) +
ylab("Share (Percentage)") +
xlab("Timing of Infection (2022)") +
theme(text = element_text(size = 33)) +
theme(legend.position = "bottom", legend.title = element_blank()) +
theme(panel.spacing = unit(4, "cm", data = NULL)) +
guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE))

ggsave("TimingOfInfections_Twitter.pdf", dpi = 500, w = 12, h = 7.5)
ggsave("TimingOfInfections_Twitter.png", dpi = 500, w = 12, h = 7.5)

# 2nd Part ----------------------------------------------------------------

    raw_data <- readRDS(file = "/Users/sydney/Desktop/cleaned_data.rds")
    data_reduced <- raw_data %>% select(num_c19_infs, date_f1_inf, f1_pcr_doc, f1_pcr_center, date_s2_inf, s2_pcr_doc, s2_pcr_center, date_t3_inf, t3_pcr_doc, t3_pcr_center, year_of_birth) %>% 
              mutate(age = 2023-year_of_birth) %>%
              mutate(age_bracket = case_when(age < 35 ~ "15-34",
                                            age < 60 ~ "35-59",
                                            age < 80 ~ "60-79",
                                            age < 100 ~ "80+")) 
    if(age_group != "00+"){
      data_reduced <- data_reduced %>% filter(age_bracket == age_group)
    }
    data_reduced <- data_reduced %>% filter(!is.na(num_c19_infs)) %>%
    select(-num_c19_infs)

timingOfInfection <- function(bootstrapping = "no"){
  #age_groups <- c("15-34", "35-59", "60-79", "80+", "00+")
  age_groups <- c("00+")
  for(age_group in age_groups){
    no_time_infections <- data_reduced %>% pivot_longer(cols=c("date_f1_inf", "date_s2_inf", "date_t3_inf"))
    no_time_infections <- no_time_infections %>% filter((name == "date_f1_inf" & f1_pcr_doc == "Ja") | (name == "date_f1_inf" & f1_pcr_center == "Ja") | (name == "date_s2_inf" & s2_pcr_doc == "Ja") | (name == "date_s2_inf" & s2_pcr_center == "Ja") | (name == "date_t3_inf" & t3_pcr_doc == "Ja") | (name == "date_t3_inf" & t3_pcr_center == "Ja"))
    no_time_infections <- no_time_infections %>%
    filter(!is.na(value)) %>%
    filter(value > "2020-01-01")
    no_time_infections <- no_time_infections %>% rename(
                                   "CounterInfection" = "name",
                                   "DateInfection" = "value")

    no_time_infections <- no_time_infections %>%
    mutate(week = isoweek(DateInfection), year = year(DateInfection)) %>%
    mutate(date = MMWRweek2Date(MMWRyear = year,
                            MMWRweek = week+1,
                            MMWRday = 1))

    if(bootstrapping == "no"){
      count_no_infections <- no_time_infections %>% group_by(date) %>% count()
      colnames(count_no_infections) <- c("Date", "CountPer1096")
      count_no_infections <- count_no_infections %>% 
                            ungroup() %>% # https://epid.blogspot.com/2012/08/how-to-calculate-confidence-interval-of.html
                            mutate(Incidence100000 = CountPer1096 / nrow(data_reduced)*100000) %>%
                            mutate(lci = nrow(data_reduced)*(CountPer1096/nrow(data_reduced) - 1.96*(((CountPer1096/nrow(data_reduced)*(1-CountPer1096/nrow(data_reduced)))/nrow(data_reduced))^0.5))) %>%
                            mutate(lci = lci/nrow(data_reduced)*100000) %>%
                            mutate(lci = case_when(lci < 0 ~ 0, 
                                                      .default = lci)) %>%
                            mutate(uci = nrow(data_reduced)*(CountPer1096/nrow(data_reduced) + 1.96*(((CountPer1096/nrow(data_reduced)*(1-CountPer1096/nrow(data_reduced)))/nrow(data_reduced))^0.5))) %>%
                            mutate(uci = uci/nrow(data_reduced)*100000) %>%
                            mutate(Incidence100000 = CountPer1096 / nrow(data_reduced)*100000)
      DatesInfections <- unique(count_no_infections$Date)
      dates <- c()
      date <- as.Date("2020-01-05")
      while(date < "2023-10-01"){
          dates <- append(dates, date)
          date <- date + 7
      }
      for(date in dates){ 
            date <- as.Date(date)
            if(date %in% unique(count_no_infections_reduced$Date)){
            }else{
                row <- c(date, 0, 0, 0, 0)
                count_no_infections <- rbind(count_no_infections, row)
            }
        }
    }
    if(bootstrapping == "yes"){
      count_no_infections <- no_time_infections %>% group_by(date, iteration) %>% count()
      colnames(count_no_infections) <- c("Date", "iteration", "CountPer1096")
      count_no_infections <- count_no_infections %>% 
                            ungroup() %>% 
                            mutate(Incidence100000 = CountPer1096 /1000*100000)
      DatesInfections <- unique(count_no_infections$Date)
      dates <- c()
      date <- as.Date("2020-01-05")
      while(date < "2023-10-01"){
          dates <- append(dates, date)
          date <- date + 7
      }
      for(i in 1:200){
        for(date in dates){ 
            count_no_infections_reduced <- count_no_infections %>% filter(iteration == i)
            date <- as.Date(date)
            if(date %in% unique(count_no_infections_reduced$Date)){
            }else{
                row <- c(date, i, 0, 0, 0, 0)
                count_no_infections <- rbind(count_no_infections, row)
            }
        }
      }
        count_no_infections <- count_no_infections %>% group_by(Date) %>% summarize(standard_dev = sd(Incidence100000), Incidence100000 = mean(Incidence100000))
        count_no_infections <- count_no_infections %>% mutate(lci = Incidence100000 - 2*standard_dev, uci = Incidence100000 + 2*standard_dev)
    } 
   
    rkidata <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Deutschland.csv")
    rkidata <- rkidata %>% 
            filter(Altersgruppe == age_group) %>% 
            filter(Meldedatum < "2023-08-30") %>%
            mutate(weekday = wday(Meldedatum)) %>%
            filter(weekday == 1)

    rkidata <- rkidata %>% select(Meldedatum, `Inzidenz_7-Tage`)
    colnames(rkidata) <- c("Date", "Incidence100000")
    rkidata <- rkidata %>% mutate(lci = Incidence100000, uci = Incidence100000, DataSet = "RKI")
    
    count_no_infections <- count_no_infections %>% 
                            select(Date, Incidence100000, lci, uci) %>%
                            mutate(DataSet = "External Survey")
    count_no_infections <- rbind(count_no_infections, rkidata)

    #No of infections MuSPAD
    if(bootstrapping == "no"){
      MuSPAD <- MuSPADnewplusold %>% 
      mutate(firstinfection = make_date(MuSPADnewplusold$s22_positive_PCR_year_1, MuSPADnewplusold$s22_positive_PCR_month_1, MuSPADnewplusold$s22_positive_PCR_day_1)) %>%
      mutate(secondinfection = make_date(MuSPADnewplusold$s22_positive_PCR_year_2, MuSPADnewplusold$s22_positive_PCR_month_2, MuSPADnewplusold$s22_positive_PCR_day_2)) %>%
      mutate(thirdinfection = make_date(MuSPADnewplusold$s22_positive_PCR_year_3, MuSPADnewplusold$s22_positive_PCR_month_3, MuSPADnewplusold$s22_positive_PCR_day_3)) %>%
      select(s22_birth_date_yyyy, firstinfection, secondinfection, thirdinfection, s23_test_covid_2023, w22_positive_PCR_day_1) %>%
      mutate(age = 2023-s22_birth_date_yyyy) %>%
                mutate(age_bracket = case_when(age < 35 ~ "15-34",
                                              age < 60 ~ "35-59",
                                              age < 80 ~ "60-79",
                                              age < 100 ~ "80+"))
    }

    if(age_group != "00+"){
      MuSPAD <- MuSPAD %>% filter(age_bracket == age_group)
    }

    # MuSPAD <- MuSPAD %>% mutate(test = case_when(abs(as.Date(w22_positive_PCR_day_1) - as.Date(firstinfection)) < 14 ~ NA,
    #                                                   abs(as.Date(w22_positive_PCR_day_1) - as.Date(secondinfection)) < 14 ~ NA,
    #                                                   abs(as.Date(w22_positive_PCR_day_1) - as.Date(thirdinfection)) < 14 ~ NA,
    #                                                   abs(as.Date(w22_positive_PCR_day_1) - as.Date(s23_test_covid_2023)) < 14 ~ NA,
    #                                                   .default = w22_positive_PCR_day_1
    #                                                   ))

    no_time_infections <- MuSPAD %>% pivot_longer(cols=c("firstinfection", "secondinfection", "thirdinfection", "w22_positive_PCR_day_1", "s23_test_covid_2023"))
    no_time_infections <- no_time_infections %>%
    filter(!is.na(value)) %>%
    filter(value > "2020-01-01")
    no_time_infections <- no_time_infections %>% rename("CounterInfection" = "name",
                                          "DateInfection" = "value")

    no_time_infections <- no_time_infections %>% filter(DateInfection > "2020-01-01") %>% filter(DateInfection < "2023-08-31") %>%
    mutate(week = isoweek(DateInfection), year = year(DateInfection)) %>%
    mutate(date = MMWRweek2Date(MMWRyear = year,
                            MMWRweek = week,
                            MMWRday = 1))


    if(age_group == "15-34"){
      group_size <- 1186
    } 
    if(age_group == "35-59"){
      group_size <- 3853
    }
    if(age_group ==  "60-79"){
      group_size <- 3892
    }
    if(age_group == "80+"){
      group_size <- 745
    }
    if(age_group == "00+"){
      if(bootstrapping == "no"){
        group_size <- 9921
      }
      if(bootstrapping == "yes"){
        group_size <- 1000
      }
    }
    if(bootstrapping == "no"){
      MuSPAD_time_inf <- no_time_infections %>% group_by(date) %>% count()
      colnames(MuSPAD_time_inf) <- c("Date", "CountPer1096")
      MuSPAD_time_inf <- MuSPAD_time_inf %>% 
                              ungroup() %>%
                              mutate(Incidence100000 = CountPer1096/group_size*100000) %>%
                              mutate(lci = group_size*(CountPer1096/group_size - 1.96*(((CountPer1096/group_size*(1-CountPer1096/group_size))/group_size)^0.5))) %>%
                              mutate(lci = lci/group_size*100000) %>%
                              mutate(uci = group_size*(CountPer1096/group_size + 1.96*(((CountPer1096/group_size*(1-CountPer1096/group_size))/group_size)^0.5))) %>%
                              mutate(uci = uci/group_size*100000) %>%
                              mutate(Incidence100000 = CountPer1096/group_size*100000) %>%
                              select(Date, Incidence100000, lci, uci) %>% mutate(DataSet = "MuSPAD")
        DatesInfections <- unique(MuSPAD_time_inf$Date)
        dates <- c()
        date <- as.Date("2020-01-05")
        while(date < "2023-08-31"){
            dates <- append(dates, date)
            date <- date + 7
        }
        for(date in dates){
            date <- as.Date(date)
            if(date %in% unique(MuSPAD_time_inf$Date)){
            }else{
                row <- c(date, 0, 0, 0, "MuSPAD")
                MuSPAD_time_inf <- rbind(MuSPAD_time_inf, row)
            }
        }
    } 
    if(bootstrapping == "yes"){
      MuSPAD_time_inf <- no_time_infections %>% group_by(date, iteration) %>% count()
      colnames(MuSPAD_time_inf) <- c("Date", "iteration", "CountPer1096")
      MuSPAD_time_inf <- MuSPAD_time_inf %>% 
                              ungroup() %>%
                              mutate(Incidence100000 = CountPer1096/group_size*100000) %>%
                              mutate(DataSet = "MuSPAD")
          DatesInfections <- unique(MuSPAD_time_inf$Date)
          dates <- c()
          date <- as.Date("2020-01-05")
          while(date < "2023-08-31"){
              dates <- append(dates, date)
              date <- date + 7
          }
          for(i in 1:200){
            for(date in dates){
                date <- as.Date(date)
                MuSPAD_time_inf_reduced <- MuSPAD_time_inf %>% filter(iteration == i)
                if(date %in% unique(MuSPAD_time_inf_reduced$Date)){
                }else{
                    row <- c(date, i, 0, 0, 0, "MuSPAD")
                    MuSPAD_time_inf <- rbind(MuSPAD_time_inf, row)
                }
            }
          }
      MuSPAD_time_inf <- MuSPAD_time_inf %>% group_by(Date) %>% summarize(standard_dev = sd(Incidence100000), Incidence100000 = mean(Incidence100000))
      MuSPAD_time_inf <- MuSPAD_time_inf %>% mutate(lci = Incidence100000 - 2*standard_dev, uci = Incidence100000 + 2*standard_dev)
      MuSPAD_time_inf <- MuSPAD_time_inf %>% mutate(DataSet = "MuSPAD")
    }

    MuSPAD_time_inf <- MuSPAD_time_inf %>% mutate(Incidence100000 = case_when(is.na(Incidence100000) ~ 0, .default = Incidence100000))

    MuSPAD_time_inf <- MuSPAD_time_inf %>% select(Date, Incidence100000, lci, uci, DataSet)

    MuSPAD_time_inf$Date <- as.Date(MuSPAD_time_inf$Date)
    MuSPAD_time_inf <- MuSPAD_time_inf %>% mutate(DataSet = "MuSPAD") # %>% filter(Date < "2023-07-01")
    count_no_infections <- rbind(count_no_infections, MuSPAD_time_inf)

    palette_surveyrki_bars <- function() {
      c("#9900CC", "#9fadaf", "#990000")
    }

    palette_surveyrki_errorbars <- function() {
      c("#640085", "#4a5253", "#5c0000")
    }

    count_no_infections$DataSet <- factor(count_no_infections$DataSet, levels = c("External Survey", "MuSPAD", "RKI"))

    StopMusPadData <- data.frame(date=as.Date(c("2022-12-13")), 
                                   event=c("Last Infection\nreported to MuSPAD"))

    ComSurveyRki <- ggplot(data = count_no_infections %>% filter(Date > "2020-03-01") %>% filter(Date < "2023-09-01") %>% mutate(DataSet = factor(DataSet, levels = c("External Survey", "RKI", "MuSPAD")))) +
        geom_point (aes(x = Date, y = Incidence100000, color = DataSet), size = 4) +
        geom_ribbon(aes(ymin = lci, ymax = uci, x = Date, fill = DataSet), alpha = 0.15)+
        #geom_line(data=count_no_infections,  aes(x = Date, y = Incidence100000, color =DataSet), alpha = 0.2, size = 1.2) +
        theme_minimal() +
        xlab("Date") +
        facet_wrap(~DataSet, nrow = 3) +
        geom_vline(data = count_no_infections, mapping = aes(xintercept = as.Date("2023-05-14")), color = "#990000", size = 1.5) +
        annotate("text", x=as.Date("2023-05-14"), y=2500, label="Last infection reported\nto MuSPAD", angle=90, size =10) +
        geom_vline(data = count_no_infections, mapping = aes(xintercept = as.Date("2023-08-13")), color = "#9900CC", size = 1.5) +
        annotate("text", x=as.Date("2023-08-13"), y=2400, label="Last infections reported\nto external survey", angle=90, size = 10) +
        scale_color_manual(values = palette_surveyrki_bars()) +
        scale_fill_manual(values = palette_surveyrki_errorbars()) +
        ggtitle("7-Day-Incidence per 100,000") +
        #geom_vline(data=StopMusPadData, mapping=aes(xintercept=date), color="#666666", size = 1.5) +
        #geom_text(data=StopMusPadData, mapping=aes(x=date, y=0.04, label=event), size=10, angle=90, nudge_y = 2600, hjust = 0.5) +
        ylab("7-Day-Incidence per 100,000") +
        ggtitle(age_group) +
        theme(text = element_text(size = 55)) +
        scale_y_continuous(breaks = c (0,500,1000,1500,2000,2500,3000)) +
        #scale_x_date(date_breaks = "6 months", date_labels = "%Y/%m")+
        scale_x_date(breaks= seq(min(count_no_infections$Date), as.Date("2023-08-31"), by = "6 months"), date_labels = "%Y/%m")+
        theme(legend.position = "none", legend.title = element_blank()) +
          theme(axis.ticks.x = element_line(),
            axis.ticks.y = element_line(),
            axis.ticks.length = unit(12, "pt")) +
        theme(plot.title = element_text(hjust = 0.5)) 

    #ggarrange(ComSurveyRki, ggparagraph(text="   ", face = "italic", size = 14, color = "black"), timelineplot2, labels = c("A", "", "B"), nrow = 3, ncol = 1,  align = "v", font.label = list(size = 37), heights = c(1.1,0.05,0.22))
    
    ggsave(paste0("VizComparisonIncidenceSurveyRKI", age_group, "bootstrap", bootstrapping, ".pdf"), dpi = 500, w = 24, h = 30)
    ggsave(paste0("VizComparisonIncidenceSurveyRKI", age_group, "bootstrap", bootstrapping, ".png"), dpi = 500, w = 24, h = 30)
  }

}


ggplot() +
    geom_point(data = count_no_infections, aes(x = Date, y = Incidence100000, color = DataSet), size = 1.5) +
    #geom_line(data=count_no_infections,  aes(x = Date, y = Incidence100000, color =DataSet), alpha = 0.2, size = 1.2) +
    theme_minimal() +
    xlab("Date") +
    scale_color_manual(values = palette()) +
    ylab("7-Day-Incidence \n per 100,000") +
    theme(text = element_text(size = 35)) +
    #scale_y_log10(breaks=c(1,10,100,1000)) +
    theme(legend.position = "bottom", legend.title = element_blank())

ggsave("VizComparisonIncidenceSurveyRKILin.pdf", dpi = 500, w = 15, h = 5)
ggsave("VizComparisonIncidenceSurveyRKILin.png", dpi = 500, w = 15, h = 5)