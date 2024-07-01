library(tidyverse)
library(MMWRweek)

# Author: S. Paltra, contact: paltra@tu-berlin.de

raw_data <- read_csv("/Users/sydney/Downloads/twitter_data.csv")

reduced_data <- raw_data %>% select(num_c19_infs, user_id, date_f1_inf, date_s2_inf, date_t3_inf) %>% 
filter(!is.na(num_c19_infs)) %>% 
select( user_id, date_f1_inf, date_s2_inf, date_t3_inf)

no_time_infections <- reduced_data %>% pivot_longer(cols=c("date_f1_inf", "date_s2_inf", "date_t3_inf"))
no_time_infections <- no_time_infections %>% 
filter(!is.na(value)) %>%
filter(value > "2020-01-01")
colnames(no_time_infections)[2] <- "CounterInfection"
colnames(no_time_infections)[3] <- "DateInfection"

no_time_infections <- no_time_infections %>% 
mutate(week = isoweek(DateInfection), year = year(DateInfection)) %>%
mutate(date = MMWRweek2Date(MMWRyear = year,
                        MMWRweek = week+1,
                        MMWRday = 1))


count_no_infections <- no_time_infections %>% group_by(date) %>% count() 
colnames(count_no_infections) <- c("Date", "CountPer1096")
count_no_infections <- count_no_infections %>% 
                        ungroup() %>%
                        mutate(Incidence100000 = CountPer1096 / length(unique(reduced_data$user_id))*100000)

DatesInfections <- unique(count_no_infections$Date)
dates <- c()
date <- as.Date("2020-01-05")
while(date < "2023-10-01"){
    dates <- append(dates, date)
    date <- date + 7
}
for(date in dates){
    date <- as.Date(date)
    if(date %in% unique(count_no_infections$Date)){
    }else{
        row <- c(date, 0, 0)
        count_no_infections <- rbind(count_no_infections, row)
    }
}

rkidata <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Deutschland.csv")
rkidata <- rkidata %>% 
        filter(Altersgruppe == "00+") %>% 
        filter(Meldedatum < "2023-10-01") %>%
        mutate(weekday = wday(Meldedatum)) %>%
        filter(weekday == 1)

rkidata <- rkidata %>% select(Meldedatum, `Inzidenz_7-Tage`)
colnames(rkidata) <- c("Date", "Incidence100000")
rkidata <- rkidata %>% mutate(DataSet = "RKI")
count_no_infections <- count_no_infections %>% 
                        select(Date, Incidence100000) %>%
                        mutate(DataSet = "Survey")
count_no_infections <- rbind(count_no_infections, rkidata)

ggplot() +
    geom_point(data = count_no_infections, aes(x = Date, y = Incidence100000, color =DataSet), size = 1.5) +
    #geom_line(data=count_no_infections,  aes(x = Date, y = Incidence100000, color =DataSet), alpha = 0.2, size = 1.2) +
    theme_minimal() +
    xlab("Date") +
    ylab("7-Day-Incidence \n per 100,000") +
    theme(text = element_text(size = 25)) +
    scale_y_log10(breaks=c(1,10,100,1000)) +
    theme(legend.position = "bottom", legend.title = element_blank())

ggsave("VizComparisonIncidenceSurveyRKILog.pdf", dpi = 500, w = 15, h = 5)
ggsave("VizComparisonIncidenceSurveyRKILog.png", dpi = 500, w = 15, h = 5)

ggplot() +
    geom_point(data = count_no_infections, aes(x = Date, y = Incidence100000, color =DataSet), size = 1.5) +
    #geom_line(data=count_no_infections,  aes(x = Date, y = Incidence100000, color =DataSet), alpha = 0.2, size = 1.2) +
    theme_minimal() +
    xlab("Date") +
    ylab("7-Day-Incidence \n per 100,000") +
    theme(text = element_text(size = 25)) +
    #scale_y_log10(breaks=c(1,10,100,1000)) +
    theme(legend.position = "bottom", legend.title = element_blank())

ggsave("VizComparisonIncidenceSurveyRKILin.pdf", dpi = 500, w = 15, h = 5)
ggsave("VizComparisonIncidenceSurveyRKILin.png", dpi = 500, w = 15, h = 5)
