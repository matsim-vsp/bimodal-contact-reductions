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

palette <- function() {
  c("#ffe6ab", "#FFD269", "#fac548", "#ECA400", "#ad8500", "#006992")
}
palette2 <- function() {
  c("#FFD269", "#fac548", "#ECA400", "#ad8500", "#6b5200", "#27476E")
}

InfectionTimingTwitter %>% filter(TimeFrame != "Show Me The Answer")  %>%
                            group_by(TimeFrame, Recruiter) %>% summarise(n = sum(NoVoted)) %>% group_by(Recruiter) %>%
                            mutate(percent = 100 * n / sum(n)) %>%
                            mutate(lci = sum(n)*(n/sum(n) - 1.96*(((n/sum(n)*(1-n/sum(n)))/sum(n))^0.5))) %>%
                            mutate(lci = 100/sum(n)*lci) %>%
                            mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
                            mutate(uci = sum(n)*(n/sum(n) + 1.96*(((n/sum(n)*(1-n/sum(n)))/sum(n))^0.5))) %>%
                            mutate(uci = 100/sum(n)*uci) %>%
                            mutate(Recruiter = factor(Recruiter, levels = c("Recruiter 1 (Twitter)", "Recruiter 2", "Recruiter 3", "Recruiter 4", "Recruiter 5", "Recruiter 1 (Mastodon)"))) %>%
ggplot(aes(x = TimeFrame, y = percent)) +
geom_bar(aes(fill=Recruiter), stat = "identity", position = "dodge", width = 0.95) +
geom_errorbar(aes(x=TimeFrame, ymin=lci, ymax=uci, colour = Recruiter), position = position_dodge(0.95), width = 0.3, alpha=0.9, size=1.3) +
theme_minimal() +
scale_fill_manual(values = palette()) +
scale_color_manual(values = palette2()) +
scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,10,20,30,40,50)) +
ylab("Share (Percentage)") +
xlab("Timing of Infection (2022)") +
theme(text = element_text(size = 33)) +
theme(legend.position = "bottom", legend.title = element_blank()) +
theme(panel.spacing = unit(0.8, "cm", data = NULL)) +
guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE))

ggsave("TimingOfInfections_Twitter.pdf", dpi = 500, w = 10, h = 7.5)
ggsave("TimingOfInfections_Twitter.png", dpi = 500, w = 10, h = 7.5)

MuSPAD <- MuSPADnewplusold %>% 
mutate(firstinfection = make_date(MuSPADnewplusold$s22_positive_PCR_year_1, MuSPADnewplusold$s22_positive_PCR_month_1, MuSPADnewplusold$s22_positive_PCR_day_1)) %>%
mutate(secondinfection = make_date(MuSPADnewplusold$s22_positive_PCR_year_2, MuSPADnewplusold$s22_positive_PCR_month_2, MuSPADnewplusold$s22_positive_PCR_day_2)) %>%
mutate(thirdinfection = make_date(MuSPADnewplusold$s22_positive_PCR_year_3, MuSPADnewplusold$s22_positive_PCR_month_3, MuSPADnewplusold$s22_positive_PCR_day_3)) %>%
select(firstinfection, secondinfection, thirdinfection)

no_time_infections <- MuSPAD %>% pivot_longer(cols=c("firstinfection", "secondinfection", "thirdinfection"))
no_time_infections <- no_time_infections %>%
filter(!is.na(value)) %>%
filter(value > as.Date("2021/12/31")) %>%
filter(value < as.Date("2023/01/01")) %>%
mutate(Timing = case_when(value <  as.Date("2022/04/01") ~ "Jan - Apr",
                          value < as.Date("2022/09/01") ~ "May - Aug",
                          .default = "Sep - Dec"))
colnames(no_time_infections)[1] <- "CounterInfection"
colnames(no_time_infections)[2] <- "DateInfection"

no_time_infections %>% count(Timing) %>% mutate(percent = 100*n/(sum(n))) %>%
                            mutate(lci = sum(n)*(n/sum(n) - 1.96*(((n/sum(n)*(1-n/sum(n)))/sum(n))^0.5))) %>%
                            mutate(lci = 100/sum(n)*lci) %>%
                            mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
                            mutate(uci = sum(n)*(n/sum(n) + 1.96*(((n/sum(n)*(1-n/sum(n)))/sum(n))^0.5))) %>%
                            mutate(uci = 100/sum(n)*uci) %>%
ggplot(aes(x = Timing, y = percent)) +
geom_bar(stat = "identity", position = "dodge", fill = "#006992", width = 0.95) +
geom_errorbar(aes(x=Timing, ymin=lci, ymax=uci), color = "#27476E",  position = position_dodge(0.95), width = 0.3, alpha=0.9, size=1.3) +
theme_minimal() +
scale_fill_manual(values = palette()) +
scale_color_manual(values = palette2()) +
scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,10,20,30,40,50)) +
ylab("Share (Percentage)") +
xlab("Timing of Infection (2022)") +
theme(text = element_text(size = 33)) +
theme(legend.position = "bottom", legend.title = element_blank()) +
theme(panel.spacing = unit(0.8, "cm", data = NULL)) +
guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE))

ggsave("TimingOfInfections_MuSPAD.pdf", dpi = 500, w = 10, h = 7.5)
ggsave("TimingOfInfections_MuSPAD.png", dpi = 500, w = 10, h = 7.5)

# 2nd Part ----------------------------------------------------------------

raw_data <- readRDS(file = "/Users/sydney/Desktop/cleaned_data.rds")

data_reduced <- raw_data %>% select(num_c19_infs, date_f1_inf, date_s2_inf, date_t3_inf) %>% 
filter(!is.na(num_c19_infs)) %>%
select(date_f1_inf, date_s2_inf, date_t3_inf)

no_time_infections <- data_reduced %>% pivot_longer(cols=c("date_f1_inf", "date_s2_inf", "date_t3_inf"))
no_time_infections <- no_time_infections %>%
filter(!is.na(value)) %>%
filter(value > "2020-01-01")
colnames(no_time_infections)[1] <- "CounterInfection"
colnames(no_time_infections)[2] <- "DateInfection"

no_time_infections <- no_time_infections %>%
mutate(week = isoweek(DateInfection), year = year(DateInfection)) %>%
mutate(date = MMWRweek2Date(MMWRyear = year,
                        MMWRweek = week+1,
                        MMWRday = 1))


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
    if(date %in% unique(count_no_infections$Date)){
    }else{
        row <- c(date, 0, 0, 0, 0)
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
rkidata <- rkidata %>% mutate(lci = Incidence100000, uci = Incidence100000, DataSet = "RKI")
count_no_infections <- count_no_infections %>% 
                        select(Date, Incidence100000, lci, uci) %>%
                        mutate(DataSet = "External Survey")
count_no_infections <- rbind(count_no_infections, rkidata)

#No of infections MuSPAD
MuSPAD <- MuSPADnewplusold %>% 
mutate(firstinfection = make_date(MuSPADnewplusold$s22_positive_PCR_year_1, MuSPADnewplusold$s22_positive_PCR_month_1, MuSPADnewplusold$s22_positive_PCR_day_1)) %>%
mutate(secondinfection = make_date(MuSPADnewplusold$s22_positive_PCR_year_2, MuSPADnewplusold$s22_positive_PCR_month_2, MuSPADnewplusold$s22_positive_PCR_day_2)) %>%
mutate(thirdinfection = make_date(MuSPADnewplusold$s22_positive_PCR_year_3, MuSPADnewplusold$s22_positive_PCR_month_3, MuSPADnewplusold$s22_positive_PCR_day_3)) %>%
select(firstinfection, secondinfection, thirdinfection)

no_time_infections <- MuSPAD %>% pivot_longer(cols=c("firstinfection", "secondinfection", "thirdinfection"))
no_time_infections <- no_time_infections %>%
filter(!is.na(value)) %>%
filter(value > "2020-01-01")
colnames(no_time_infections)[1] <- "CounterInfection"
colnames(no_time_infections)[2] <- "DateInfection"

no_time_infections <- no_time_infections %>% filter(DateInfection > "2020-01-01") %>% filter(DateInfection < "2024-01-01") %>%
mutate(week = isoweek(DateInfection), year = year(DateInfection)) %>%
mutate(date = MMWRweek2Date(MMWRyear = year,
                        MMWRweek = week,
                        MMWRday = 1))

MuSPAD_time_inf <- no_time_infections %>% group_by(date) %>% count()
colnames(MuSPAD_time_inf) <- c("Date", "CountPer1096")
MuSPAD_time_inf <- MuSPAD_time_inf %>% 
                        ungroup() %>%
                        mutate(Incidence100000 = CountPer1096/9921*100000) %>%
                        mutate(lci = 9921*(CountPer1096/9921 - 1.96*(((CountPer1096/9921*(1-CountPer1096/9921))/9921)^0.5))) %>%
                        mutate(lci = lci/9921*100000) %>%
                        mutate(uci = 9921*(CountPer1096/9921 + 1.96*(((CountPer1096/9921*(1-CountPer1096/9921))/9921)^0.5))) %>%
                        mutate(uci = uci/9921*100000) %>%
                        mutate(Incidence100000 = CountPer1096/9921*100000) %>%
                        select(Date, Incidence100000, lci, uci) %>% mutate(DataSet = "MuSPAD")

DatesInfections <- unique(MuSPAD_time_inf$Date)
dates <- c()
date <- as.Date("2020-01-05")
while(date < "2023-10-01"){
    dates <- append(dates, date)
    date <- date + 7
}
for(date in dates){
    date <- as.Date(date)
    if(date %in% unique(MuSPAD_time_inf$Date)){
    }else{
        row <- c(date, NA, 0, 0, 0)
        MuSPAD_time_inf <- rbind(MuSPAD_time_inf, row)
    }
}

MuSPAD_time_inf$Date <- as.Date(MuSPAD_time_inf$Date)
MuSPAD_time_inf <- MuSPAD_time_inf %>% mutate(DataSet = "MuSPAD") %>% filter(Date <= "2022-12-13")
count_no_infections <- rbind(count_no_infections, MuSPAD_time_inf)

palette <- function() {
  c("#ECA400", "#006992", "#27476E")
}

palette2 <- function() {
  c("#FFD269", "#009cd9", "#006992")
}

count_no_infections$DataSet <- factor(count_no_infections$DataSet, levels = c("External Survey", "MuSPAD", "RKI"))

StopMusPadData <- data.frame(date=as.Date(c("2022-12-13")), 
                              event=c("Last Infection\nreported to MuSPAD"))


ggplot(data = count_no_infections %>% filter(Date < "2024-01-01")) +
    geom_point (aes(x = Date, y = Incidence100000, color = DataSet), size = 2) +
    geom_ribbon(aes(ymin = lci, ymax = uci, x = Date, fill = DataSet), alpha = 0.25)+
    #geom_line(data=count_no_infections,  aes(x = Date, y = Incidence100000, color =DataSet), alpha = 0.2, size = 1.2) +
    theme_minimal() +
    xlab("Date") +
    scale_color_manual(values = palette()) +
    scale_fill_manual(values = palette()) +
    geom_vline(data=StopMusPadData, mapping=aes(xintercept=date), color="#666666", size = 1.5) +
    geom_text(data=StopMusPadData, mapping=aes(x=date, y=0.04, label=event), size=10, angle=90, nudge_y = 2600, hjust = 0.5) +
    ylab("7-Day-Incidence \n per 100,000") +
    theme(text = element_text(size = 45)) +
    scale_y_continuous(breaks = c (0,500,1000,1500,2000,2500,3000)) +
    scale_x_date(date_breaks = "6 months", date_labels = "%Y/%m")+
    theme(legend.position = "bottom", legend.title = element_blank()) +
      theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(12, "pt"))

ggsave("VizComparisonIncidenceSurveyRKI.pdf", dpi = 500, w = 23, h = 10)
ggsave("VizComparisonIncidenceSurveyRKI.png", dpi = 500, w = 23, h = 10)

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