library(tidyverse)
library(igraph)
library(gridExtra)
library(ggiraphExtra)
library(Hmisc)
library(ggpubr)
library(smplot2)

# 0th order Results -------------------------------------------------------

source("DataCleaningPrepForContactAnalysis.R")

palette <- function() {
  c("#998ec3", "#998ec3",  "#998ec3",   "#998ec3")
}

palette2 <- function() {
  c("#542788", "#542788", "#542788" , "#542788")
}

quantile_cl <- function(y, q=0.5, conf.level = 0.95, na.rm=TRUE) {
  alpha <- 1 - conf.level
  if (na.rm) y <- y[!is.na(y)]
  n <- length(y)
  l <- qbinom(alpha/2, size=n, prob = q)
  u <- 1 + n - l
  ys <- sort.int(c(-Inf, y, Inf), partial = c(1 + l, 1 + u))
  data.frame(
    y = quantile(y, probs = q, na.rm=na.rm, type = 8),
    ymin = ys[1 + l],
    ymax = ys[1 + u]
  )
}

median_cl2 <- function(y, conf.level=0.95, na.rm=TRUE) quantile_cl(y, q=0.5, conf.level=conf.level, na.rm=na.rm)

p1_zeroth_order_absolut <- ggplot(data_reduced_tidy %>% 
filter((TypeOfContact %in% c("Work", "Leisure"))) %>% 
filter(!is.na(TypeOfContact)) %>% filter(value < 500) %>%
filter(WhoseContacts=="Respondent"), aes(time, value, fill = time, color = time)) +
  sm_raincloud(aes(stat = median_cl2), 
      point.params = list(size = 2, shape = 21, alpha = 0.3, position = sdamr::position_jitternudge(
        nudge.x = -0.1,
        jitter.width = 0.1, jitter.height = 0.01
      )), 
      boxplot.params =  list(alpha = 0.0, width = 0.0), 
              violin.params = list(width = 1),
              shape = 21, sep_level = 2)  +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
  facet_grid(~TypeOfContact) +
  theme_minimal() +
  ylab("Reported Number Of Contacts") +
  xlab("") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,1,3,10,30,100,300)) +
  theme(panel.spacing = unit(1, "lines")) +
  theme(text = element_text(size = 25)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(size = 1), 
                   axis.ticks.y = element_line(size = 1),
                   axis.ticks.length = unit(15, "pt"))

ggsave("CollectionViolinplots_Respondent.pdf", p1_zeroth_order_absolut, dpi = 500, w = 15, h = 6)
ggsave("CollectionViolinplots_Respondent.png", p1_zeroth_order_absolut, dpi = 500, w = 15, h = 6)


p1_zeroth_order_absolut <- ggplot(data_reduced_tidy_ln %>% 
filter((TypeOfContact %in% c("Work", "Leisure"))) %>% 
filter(!is.na(TypeOfContact)) %>% filter(value < 500) %>% filter(value != -1000) %>%
filter(WhoseContacts=="Respondent"), aes(time, value, fill = time, color = time)) +
  sm_raincloud(aes(stat = median_cl2), 
      point.params = list(size = 2, shape = 21, alpha = 0.3, position = sdamr::position_jitternudge(
        nudge.x = -0.1,
        jitter.width = 0.1, jitter.height = 0.01
      )), 
      boxplot.params =  list(alpha = 0.0, width = 0.0), 
              violin.params = list(width = 1),
              shape = 21, sep_level = 2)  +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
  facet_grid(~TypeOfContact) +
  theme_minimal() +
  ylab("Logarithmic Change of Contacts") +
  xlab("") +
  #scale_y_continuous(limits = c(-10,2))+
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,1,3,10,30,100,300)) +
  theme(panel.spacing = unit(1, "lines")) +
  theme(text = element_text(size = 25)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(size = 1), 
                   axis.ticks.y = element_line(size = 1),
                   axis.ticks.length = unit(15, "pt"))

ggsave("CollectionViolinplotsln.pdf", p1_zeroth_order_absolut, dpi = 500, w = 15, h = 6)
ggsave("CollectionViolinplotsln.png", p1_zeroth_order_absolut, dpi = 500, w = 15, h = 6)

# palette <- function() {
#   c("#006BA6", "#FFBC42", "#8F2D56", "#C93E78")
# }


p1_first_order_absolut <- ggplot(data_reduced_tidy %>% filter((TypeOfContact %in% c("Work", "Leisure", "School") & value <= 100)) %>% filter(!is.na(TypeOfContact)), aes(WhoseContacts, value)) +
  geom_violin(aes(fill = WhoseContacts), width = 1, trim = FALSE) + 
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                 geom="pointrange", color="black", linewidth = 1.2) +
  #geom_boxplot(aes(color = WhoseContacts), width = 0.1, position = position_dodge(0.9)) +
  #geom_boxplot(aes(color = WhoseContacts), size = 1.3) +
  scale_fill_manual(values = palette()) +
  facet_grid(rows = vars(TypeOfContact), cols= vars(time)) +
  theme_minimal() +
  ylab("Reported Number Of Contacts") +
  theme(text = element_text(size = 30)) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(panel.spacing = unit(0.8, "cm", data = NULL)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) 
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("CollectionViolinplots_Absolute.pdf", p1_first_order_absolut, dpi = 500, w = 15, h = 18)
ggsave("CollectionViolinplots_Absolute.png", p1_first_order_absolut, dpi = 500, w = 15, h = 18)


meadian <- data_reduced_tidy %>%  filter((TypeOfContact %in% c("Work", "Leisure"))) %>% 
   filter(WhoseContacts == "Respondent") %>% filter(!is.na(value)) %>%
   filter(value < 100) %>%  
   filter(!is.na(TypeOfContact)) %>% group_by(TypeOfContact, time) %>% summarise(mean =mean(value), lower = smean.sdl(value)["Lower"], upper = smean.sdl(value)["Upper"])

# Relative no. of contacts

my_comparisons <- list(c("03/2020", "Summer 2021"), c("Summer 2021", "01/2023"))

palette <- function() {
  c("#8491B4FF", "#8491B4FF",  "#8491B4FF")
}

palette2 <- function() {
  c("#515972", "#515972", "#515972")
}

median_cl <- function(y, conf.level=0.95, na.rm=TRUE) quantile_cl(y, q=0.5, conf.level=conf.level, na.rm=na.rm)

first_boxplot<-structure(list(
  Median = c(-96, -94, -96, -94, -98, -96), 
  FirstQuartile = c(`25%` = -97.471, `25%` = -95, `25%` =-97.32, `25%` = -94.97, `25%` = -99.77,  `25%` =-97.32), 
  ThirdQuartile = c(`75%` = -96.15, `75%` = -92.88, `75%` =-94.57, `75%` =-93.27, `75%` = -96.04, `75%` =-94.57), 
  time = c("03/2020", "Summer 2021", "01/2023", "03/2020", "Summer 2021", "01/2023"), 
TypeOfContact = c("Work", "Work", "Work", "Leisure", "Leisure", "Leisure")), 
row.names = c(NA, -6L), class = c("tbl_df", "tbl", "data.frame"
))
first_boxplot <- as.data.frame(first_boxplot)

second_boxplot<-structure(list(
  Median = c(-52, -51, -50, -52.69, -51.85, -50.43), 
  FirstQuartile = c(`25%` =-54.32,`25%` =-52.22, `25%` = -52.05, `25%` = -54.12, `25%`  = -53.49,  `25%` =-52.05), 
  ThirdQuartile = c(`75%` =-51.16, `75%` =-49.27, `75%` = -48.82, `75%` = -51.09, `75%` = -50.17, `75%` =-48.82), 
  time = c("03/2020", "Summer 2021", "01/2023","03/2020", "Summer 2021", "01/2023"), 
TypeOfContact = c("Work", "Work", "Work", "Leisure", "Leisure", "Leisure")), 
row.names = c(NA, -6L), class = c("tbl_df", "tbl", "data.frame"
))
second_boxplot <- as.data.frame(second_boxplot)

third_boxplot<-structure(list(
  Median = c(0, 0, 1.04, -0.43, 1.2, 1.04), 
  FirstQuartile = c(`25%` = -0.72, `25%` = -1.23, `25%` = -0.273, `25%` = -1.80, `25%` =-0.36,  `25%` =-0.27), 
  ThirdQuartile = c(`75%` =1.48, `75%` =1.5, `75%` =2.28 , `75%` = 1.01, `75%` =2.47, `75%` =2.28), 
  time = c("03/2020", "Summer 2021", "01/2023", "03/2020", "Summer 2021", "01/2023"), 
TypeOfContact = c("Work", "Work", "Work", "Leisure", "Leisure", "Leisure")), 
row.names = c(NA, -6L), class = c("tbl_df", "tbl", "data.frame"
))
third_boxplot <- as.data.frame(third_boxplot)

p1_zeroth_order_percred <- ggplot(data_reduced_tidy_rel %>%  filter((TypeOfContact %in% c("Work", "Leisure"))) %>% 
    filter(WhoseContacts == "Respondent") %>% filter(!is.na(value)) %>%
   filter(value > -150) %>% filter(value < 100) %>%  
    filter(!is.na(TypeOfContact))) +
  sm_raincloud(mapping=aes(x=time, y=value, fill = time, color = time), 
      point.params = list(size = 2, shape = 21, alpha = 0.3, position = sdamr::position_jitternudge(
        nudge.x = -0.1,
        jitter.width = 0.1, jitter.height = 0.01
      )), 
      boxplot.params =  list(alpha = 0.0, width = 0.0), 
              violin.params = list(width = 1),
              shape = 21, sep_level = 2)  +
  geom_boxplot(data = first_boxplot,
    stat = "identity",
    mapping = aes(x = time, fill = time, lower  = FirstQuartile,
        upper  = ThirdQuartile,
        middle = Median,
        ymin   = -100, # optional
        ymax   = 0), width = 0.1 # optional
  ) +
  geom_boxplot(data = second_boxplot,
    stat = "identity",
    mapping = aes(x = time, fill = time, lower  = FirstQuartile,
        upper  = ThirdQuartile,
        middle = Median,
        ymin   = -100, # optional
        ymax   = 0), width = 0.1 # optional
  ) +
  geom_boxplot(data = third_boxplot,
    stat = "identity",
    mapping = aes(x = time, fill = time, lower  = FirstQuartile, 
        upper  = ThirdQuartile,
        middle = Median,
        ymin   = -100, # optional
        ymax   = 0), width = 0.1 # optional
  ) +
  #geom_violin(aes(fill = time, color = time), scale = "area", trim = TRUE) + 
  #stat_compare_means(comparisons = my_comparisons, method = "t.test", symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("**** (p < 0.0001)", "*** (p < 0.001)", "** (p < 0.01)", "* (p < 0.05)", "not significant (p > 0.05)")), size = 6, bracket.size = 1, tip.length = 0.01, vjust = -0.2, label.y.npc = 0)+
  #stat_summary(aes(color= time), fun.data=mean_sdl, fun.args = list(mult=1), 
                 #geom="pointrange", linewidth = 1) +
  #geom_violin(aes(color=WhoseContacts), size = 1.3) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-100, -50, 0,50, 100)) +
  scale_color_manual(values = palette2()) +
  scale_fill_manual(values = palette()) +
  facet_grid(~(TypeOfContact)) +
  theme_minimal() +
  xlab("") +
  theme(panel.spacing = unit(1, "lines")) +
  ylab("Change of No. of \n Contacts (in percent)") +
  theme(text = element_text(size = 25)) +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(size = 1), 
                   axis.ticks.y = element_line(size = 1),
                   axis.ticks.length = unit(15, "pt"))

ggsave("CollectionViolinplots_RemainingRespondent.pdf", p1_zeroth_order_percred, dpi = 500, w = 15, h = 7.5)
ggsave("CollectionViolinplots_RemainingRespondent.png", p1_zeroth_order_percred, dpi = 500, w = 15, h = 7.5)


data_reduced_tidy_rel_fil <- data_reduced_tidy_rel %>% filter(WhoseContacts == "Respondent") %>% 
filter(TypeOfContact == "Leisure") %>% filter(time %in% c("03/2020", "Summer 2021")) %>%
filter(is.finite(value)) %>% filter(value > -1000)

#Permutation test
outcome <- data_reduced_tidy_rel_fil$value
treatment <- data_reduced_tidy_rel_fil$time
treatment <- as.character(treatment)

#Difference in means
original <- diff(tapply(outcome, treatment, mean))
mean(outcome[treatment=="03/2020"])-mean(outcome[treatment=="Summer 2021"])

permutation.test <- function(treatment, outcome, n){
  distribution=c()
  result=0
  for(i in 1:n){
    distribution[i]=diff(by(outcome, sample(treatment, length(treatment), FALSE), mean))
  }
  result=sum(abs(distribution) >= abs(original))/(n)
  return(list(result, distribution))
}

test1 <- permutation.test(treatment, outcome, 10000)

sum(abs(unlist(test1)) > abs(diff(by(outcome, treatment, median))))/2000 

hist(test1[[2]], breaks=100, col='grey', main="Permutation Distribution", las=1, xlab='')
abline(v=original, lwd=3, col="red")



y <- data_reduced$respondent_work_rel_2019_2020
tr <- data_reduced$respondent_work_rel_2019_2021

diff(by(data_reduced$respondent_work_rel_2019_2020, data_reduced$respondent_work_rel_2019_2021, median))

dist <- replicate(2000, diff(by(data_reduced$respondent_work_rel_2019_2020, sample(data_reduced$respondent_work_rel_2019_2021, length(data_reduced$respondent_work_rel_2019_2021), FALSE), median)))
hist(dist, xlim = c(-100, 100), col = "black", breaks = 1000)
abline(v = diff(by(data_reduced$respondent_work_rel_2019_2020,data_reduced$respondent_work_rel_2019_2021, median)), col = "blue", lwd = 2)

sum(abs(dist) > abs(diff(by(y, tr, median))))/2000 

set.seed(1)
n <- 100
data <- data_reduced %>% filter(!is.na(respondent_work_rel_2019_2021)) %>% filter(!is.na(respondent_work_rel_2019_2020))%>% 
filter(respondent_work_rel_2019_2021 != -1000) %>% filter(respondent_work_rel_2019_2020!= -1000) %>%
filter(is.finite(respondent_work_rel_2019_2021)) %>% filter(is.finite(respondent_work_rel_2019_2020))
tr <- data$respondent_work_rel_2019_2021
y <- data$respondent_work_rel_2019_2020

dist <- replicate(2000, diff(by(y, sample(tr, length(tr), FALSE), mean)))
hist(dist, xlim = c(-100, 100), col = "black", breaks = 1000)
abline(v = diff(by(y,tr, median)), col = "blue", lwd = 2)

sum(abs(dist) > abs(diff(by(y, tr, mean))))/2000 

n <- 721
tr <- rbinom(723, 1, 0.5)
y <- 1 + tr + rnorm(n, 0, 3)
dist <- replicate(2000, diff(by(y, sample(tr, length(tr), FALSE), mean)))
hist(dist, xlim = c(-3, 3), col = "black", breaks = 100)
abline(v = diff(by(y, tr, mean)), col = "blue", lwd = 2)

mean <- data_reduced_tidy_rel %>%  filter((TypeOfContact %in% c("Work", "Leisure"))) %>% 
   filter(WhoseContacts == "Respondent") %>% filter(!is.na(value)) %>%
   filter(value > -150) %>% filter(value < 100) %>%  
   filter(!is.na(TypeOfContact)) %>% group_by(TypeOfContact, time) %>% summarise(mean =mean(value), lower = smean.sdl(value)["Lower"], upper = smean.sdl(value)["Upper"])


palette <- function() {
  c("#998ec3", "#d8daeb", "#f1a340", "#b35806")
}

palette2 <- function() {
  c("#542788", "#998ec3", "#b35806", "#713500")
}

  my_comparisons <- list(c("Respondent", "Household Member"),
  c("Respondent", "Closest Contact (Pre-Covid)"), 
  c("Respondent", "Closest Contact (During-Covid)"))

p1_zeroth_order_percred_all <- ggplot(data_reduced_tidy_rel %>%  
    filter((TypeOfContact %in% c("Work"))) %>% 
    filter(!is.na(value)) %>% filter(WhoseContacts != "Closest Contact (During-Covid)") %>%
       filter(value > -150) %>% filter(value < 100) %>%    
    filter(!is.na(TypeOfContact)), aes(WhoseContacts, value, color = WhoseContacts, fill = WhoseContacts)) +
      sm_raincloud(aes(stat = median_cl), 
      point.params = list(size = 2, shape = 21, alpha = 0.3), 
      boxplot.params =  list(alpha = 0.0, width = 0.2, notch = TRUE), 
              violin.params = list(width = 1.25),
              shape = 21, sep_level = 2)  +
  # geom_violin(aes(fill = WhoseContacts, color= WhoseContacts), scale = "area", trim = TRUE,  ) + 
  # stat_summary(aes(color = WhoseContacts), fun.data=mean_sdl, fun.args = list(mult=1), 
  #                geom="pointrange", linewidth = 1) +
  # stat_compare_means(comparisons = my_comparisons, test = "t.test", symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("**** (p < 0.0001)", "*** (p < 0.001)", "** (p < 0.01)", "* (p < 0.05)", "not significant (p > 0.05)")), size = 6, bracket.size = 1, tip.length = 0.01, vjust = -0.2, label.y.npc = 0)+
  #geom_violin(aes(color=WhoseContacts), size = 1.3) +
  scale_fill_manual(values = palette()) +
  scale_color_manual(values = palette2()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(-100, -50, 0,50, 100)) +
  facet_wrap(~time) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Work") +
  ylab("Change of No. of \n Work Contacts (in percent)") +
  theme(text = element_text(size = 30)) +
  theme(panel.spacing.y = unit(3, "lines")) +
  theme(panel.spacing.x = unit(3, "lines")) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
      theme(axis.ticks.x = element_line(size = 1), 
                   axis.ticks.y = element_line(size = 1),
                   axis.ticks.length = unit(20, "pt"))

ggsave("CollectionViolinplots_Work_All.pdf", p1_zeroth_order_percred_all, dpi = 500, w = 18, h = 9)
ggsave("CollectionViolinplots_Work_All.png", p1_zeroth_order_percred_all, dpi = 500, w = 18, h = 9)


## ECDF 
data_reduced <- data_reduced %>% mutate(date_f1_inf = case_when(is.na(date_f1_inf) ~ as.Date("3000-01-01"),
                                        .default = as.Date(as.character(date_f1_inf)))) %>%
                                filter(date_f1_inf != as.Date("1922-03-01")) %>%
                                filter(date_f1_inf != as.Date("1965-06-12")) %>%
                                filter(date_f1_inf != as.Date("2000-12-13")) %>%
                                filter(date_f1_inf != as.Date("2019-12-21"))


ecdf_comp <- data_reduced %>% count(date_f1_inf) %>% mutate(cum = cumsum(n)) %>% mutate(sum = sum(n)) %>%
  mutate(ecdf = cum/sum) %>%
  mutate(lci = (cum/sum - 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(lci = case_when(lci < 0 ~ 0, .default = lci)) %>%
  mutate(uci = (cum/sum + 1.96*(((cum/sum*(1-cum/sum))/sum)^0.5))) %>%
  mutate(uci = case_when(uci > 1 ~ 1, .default = uci))

ggplot(ecdf_comp, aes(date_f1_inf)) +
geom_ribbon(aes(ymin = lci, ymax = uci, x = date_f1_inf), alpha = 0.1, fill = "#542788")+
geom_line(aes(y=ecdf), color = "#998ec3", size = 2) +
#stat_ecdf(geom="step", size = 2, color = "#998ec3") +
  theme_minimal() +
  ylab("Relative Frequency") +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette()) +
theme_minimal() +
ylab("Empirical Cumulative \n Distribution Function") +
xlab("Date Of First Infection") +
coord_cartesian(xlim=c(as.Date("2020-03-01"), as.Date("2023-08-01")), ylim=c(0, 0.75)) +
theme(text = element_text(size = 30)) +
theme(legend.position = "none") +
guides(color = guide_legend(nrow = 2)) +
scale_color_manual(values = palette()) +
theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt"))

ggsave("ECDF_Respondents.pdf", dpi = 500, w = 9, h = 9)
ggsave("ECDF_Respondents.png", dpi = 500, w = 9, h = 9)

## NO OF INFECTIONS

data_reduced <- data_reduced %>% mutate(num_c19_infs_eng = case_when(num_c19_infs == "Nie" ~ "Never",
                                                                    num_c19_infs == "Einmal" ~ "Once",
                                                                    num_c19_infs == "Zweimal" ~ "Twice",
                                                                    num_c19_infs == "Dreimal" ~ "Three Times",
                                                                    num_c19_infs == "Mehr als dreimal" ~ "More Than Three Times",
                                                                    num_c19_infs == "Ich möchte nicht antworten" ~ "I Don't Want To Answer"))                              

data_reduced$num_c19_infs_eng <- factor(data_reduced$num_c19_infs_eng, levels = c("Never", "Once", "Twice", "Three Times", "More Than Three Times", "I Don't Want To Answer"))

data_reduced %>%
  count(num_c19_infs_eng) %>%
  mutate(percent = 100 * n / sum(n)) %>%  
  mutate(lci =  n - 1.96*(n*(n-1)/704)^0.5) %>%#
  mutate(lci = 100/704*lci) %>%
  mutate(uci = n + 1.96*(n*(n-1)/704)^0.5) %>%
  mutate(uci = 100/704*uci) %>%
  ggplot(aes(num_c19_infs_eng, percent)) +
  geom_bar(stat = "identity", width = 0.8, fill = "#998ec3") +
  geom_errorbar(aes(x=num_c19_infs_eng, ymin=lci, ymax=uci), colour = "#542788", position = position_dodge(0.8), width = 0.3, alpha=0.9, size=1.3) +
  theme_minimal() +
  ylab("Share [Percentage]") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1), breaks = c(0,25, 50)) +
  xlab("") +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "none") +
  labs(fill="Age Group") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  scale_fill_manual(values = palette())

ggsave("NoInfections_Respondents.pdf", dpi = 500, w = 9, h = 9)
ggsave("NoInfections_Respondents.png", dpi = 500, w = 9, h = 9)

palette <- function() {
  c("#8F2D56", "#006BA6")
}

ggplot(data_reduced %>% filter(num_c19_infs_eng %in% c("Never", "Once", "Twice", "Three Times")) %>% filter(!is.na(attitudeScore))) +
  geom_jitter(aes(num_c19_infs_eng, attitudeScore, color = RiskyCarefulAtt),height = 0.2) +
  theme_minimal() +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  scale_color_manual(values = palette())


ggplot(data_reduced %>% filter(num_c19_infs_eng %in% c("Never", "Once", "Twice", "Three Times")) %>% filter(!is.na(attitudeScore))) +
  geom_jitter(aes(as.Date(date_f1_inf), attitudeScore, color = RiskyCarefulAtt),height = 0.2) +
  theme_minimal() +
  theme(text = element_text(size = 30)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  scale_color_manual(values = palette())
