library(tidyverse)
library(ggsankeyfier)
library(ggsankey)

data_reduced_tidy_rel %>%  
  #filter((TypeOfContact %in% c("Leisure"))) %>% 
  filter(!is.na(value)) %>% 
  #filter(WhoseContacts %in% c("Respondent", "Household Members")) %>%
  filter(value > -150) %>% filter(value < 100) %>%    
  filter(!is.na(TypeOfContact)) %>% group_by(TypeOfContact, WhoseContacts, time) %>%
  summarise(meanRed = mean(value))

setwd("/Users/sydney/git/second-order-contacts/AnalysisSP/BimodalGroups")
fileNames <- Sys.glob("*.csv")

## BIMODAL

dataf <- data.frame(matrix(nrow=0, ncol=4))
colnames(dataf) <- c("value", "group_0", "group_1", "group")

for(file in fileNames){
  
  group_bel <- paste0("/Users/sydney/git/second-order-contacts/AnalysisSP/BimodalGroups/", file)
  
  group_belonging <- read_csv(group_bel)
  group_belonging <- group_belonging %>% mutate(value = seq(-100,75,length.out=500))
  group_belonging$value <- round(group_belonging$value)
  group_belonging <- group_belonging %>% group_by(value) %>% 
    summarise(group_0 = mean(first_group), group_1 = mean(second_group))
  
  filename <- paste0(str_split(file, "csv")[[1]][1], "csv")
  red <- paste0("/Users/sydney/git/second-order-contacts/AnalysisSP/SecondOrderContactsPaper/DataBimodalFit/", filename)
  reductions <- read_csv(red)
  reductions <- reductions %>% dplyr::select(value)
  reductions$value <- round(reductions$value)
  reductions <- reductions %>% left_join(group_belonging, by = "value") %>%
    mutate(group = case_when(
      runif(n()) < group_0 ~ 0,
      TRUE ~ 1
    ))
  reductions <- reductions %>% mutate(filename = file)
  dataf <- rbind(dataf, reductions)
}

print(dataf %>% group_by(filename) %>% count(group) %>% mutate(perc = n/sum(n)), n = 100)

## TRIMODAL

dataf <- data.frame(matrix(nrow=0, ncol=5))
colnames(dataf) <- c("value", "group_0", "group_1", "group_2", "group")

group_bel_long <- data.frame(matrix(nrow=0, ncol=7))
colnames(group_bel_long) <- c("value", "ID", "group_0", "group_1", "group_2", "TypeOfContact", "time")

setwd("/Users/sydney/git/second-order-contacts/AnalysisSP/TrimodalGroups")
fileNames <- Sys.glob("*.csv")

for(file in fileNames){
  
  group_bel <- paste0("/Users/sydney/git/second-order-contacts/AnalysisSP/TrimodalGroups/", file)
  
  group_belonging <- read_csv(group_bel)
  group_belonging <- group_belonging %>% mutate(value = seq(-100,75,length.out=500))
  group_belonging$value <- round(group_belonging$value)
  group_belonging <- group_belonging %>% group_by(value) %>% 
    summarise(group_0 = mean(first_group), group_1 = mean(second_group), group_2 = mean(third_group))
  
  filename <- paste0(str_split(file, "csv")[[1]][1], "csv")
  red <- paste0("/Users/sydney/git/second-order-contacts/AnalysisSP/SecondOrderContactsPaper/DataBimodalFit/", filename)
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

Work <- left_join(Work032020, WorkSummer2021, by = join_by(ID))
Work <- left_join(Work, Work012023, by = join_by(ID))
names(Work)[names(Work) == 'group.x'] <- '03/2020'
names(Work)[names(Work) == 'group.y'] <- 'Summer 2021'
names(Work)[names(Work) == 'group'] <- '01/2023'

Work %>% count(`01/2023`)
Work %>% select(`Summer 2021`, `01/2023`) %>% group_by(`Summer 2021`) %>% count(`01/2023`)


Worktest <- Work %>% select(`03/2020`, `Summer 2021`) %>% group_by(`03/2020`) %>% count(`Summer 2021`)

Worktest1 <- Work %>% select(`03/2020`, `Summer 2021`) %>% pivot_longer(
  cols=c(`03/2020`, `Summer 2021`),
  names_to = "stage",
  values_to = "node"
)

Worktest2 <- Work %>% select(`Summer 2021`, `01/2023`) %>% pivot_longer(
  cols=c(`Summer 2021`, `01/2023`),
  names_to = "stage",
  values_to = "node"
)

Work <- rbind(Worktest1, Worktest2)
Work <- Work %>% mutate(edge_id = ceiling(seq_len(nrow((Work)))/2)) %>%
  mutate(connector = if_else(row_number() %% 2 == 1, "from", "to")) 

Work$stage <- factor(Work$stage, levels = c("03/2020", "Summer 2021", "01/2023"))
Work <- Work[order(Work$node),]
Work <- Work[order(Work$stage),]

Work %>% filter(is.na(node)) %>% group_by(stage) %>% count(node)


Work <- Work %>% filter(!is.na(node)) %>% 
  
  
  
  mutate(y_axis = row_number())
  mutate(y_axis = case_when(node == 0 ~ 0, 
         node == 1 ~ 1, node == 2 ~ 2))
  
  mutate (y_axs = case_when(stage == "03/2020"  ~ row_number()/(401+203+118),
          stage == "Summer 2021" ~ (row_number()-(401+203+118))/(608+550+246),
          stage == "01/2023" ~ (row_number()-(401+203+118+608+550+246))/(166+279+226)))

Work <-   
  
  



Work$node <- as.character(Work$node)


Worktest <- read_csv("/Users/sydney/Desktop/Worktest.csv")
Worktest$node <- as.character(Worktest$node)
Worktest$stage <- factor(Worktest$stage, levels = c("03/2020", "Summer 2021", "01/2023"))


pos <- position_sankey(split_nodes = FALSE, align = "center",
                       width = 0.2, v_space = 0.15, h_space = 0.25, order = "as_is")

pos <-  position_sankey(v_space = "auto", order = "as_is", align = "top")


ggplot(Worktest,
         aes(x = stage, y = y_axs, group = node, connector = connector,
             edge_id = edge_id, fill = node)) +
  geom_sankeyedge(position = pos) +
  geom_sankeynode(position = pos) +
  theme_minimal() +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
  my_theme() +
  theme(legend.position = "bottom")


ggplot(ecosystem_services_pivot1, aes(x = stage, y = RCSES, group = node,
                                      connector = connector, edge_id = edge_id,
                                      fill = node)) +
  geom_sankeynode(v_space = "auto") +
  geom_sankeyedge(v_space = "auto")

Leisure <- left_join(Leisure032020, LeisureSummer2021, by = join_by(ID))
Leisure <- left_join(Leisure, Leisure012023, by = join_by(ID))

names(Leisure)[names(Leisure) == 'group.x'] <- '03/2020'
names(Leisure)[names(Leisure) == 'group.y'] <- 'Summer 2021'
names(Leisure)[names(Leisure) == 'group'] <- '01/2023'


Leisure %>% count(`Summer 2021`)
Leisure %>% select(`Summer 2021`,`01/2023`) %>% group_by(`Summer 2021`) %>% count(`01/2023`)


Work <- Work %>% ggsankey::make_long(`03/2020`, `Summer 2021`, `01/2023`)

Work <- Work %>% mutate(node = case_when(node == 0 ~ "Strong\nReduction", node == 1 ~ "Intermediate\nReduction", node == 2 ~ "Little\nChange"))
Work <- Work %>% mutate(next_node = case_when(next_node == 0 ~ "Strong\nReduction", next_node == 1 ~ "Intermediate\nReduction", next_node == 2 ~ "Little\nChange"))

Work$node <- factor(Work$node, levels = c(NA, "Strong\nReduction", "Intermediate\nReduction", "Little\nChange"))

pos <- position_sankey(split_nodes = TRUE, align = "top",
                       width = 0.2, v_space = 0.15, h_space = 0.25)

WorkSankey <- ggplot(Work %>% filter(!is.na(node)), aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
  ggsankey::geom_sankey(flow.alpha = 0.5, node.color = 1, show.legend=T) +
  ggsankey::geom_sankey_label(size = 7, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  ggsankey::theme_sankey(base_size = 25) +
  guides(fill = guide_legend(title = "")) +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c"))

Leisure <- Leisure %>% ggsankey::make_long(`03/2020`, `Summer 2021`, `01/2023`)

Leisure <- Leisure %>% mutate(node = case_when(node == 0 ~ "Strong\nReduction", node == 1 ~ "Intermediate\nReduction", node == 2 ~ "Little\nChange"))
Leisure <- Leisure%>% mutate(next_node = case_when(next_node == 0 ~ "Strong\nReduction", next_node == 1 ~ "Intermediate\nReduction", next_node == 2 ~ "Little\nChange"))

Leisure$node <- factor(Leisure$node, levels = c("Strong\nReduction", "Intermediate\nReduction", "Little\nChange"))

LeisureSankey <- ggplot(Leisure %>% filter(!is.na(node)), aes(x = x, 
                                          next_x = next_x, 
                                          node = node, 
                                          next_node = next_node,
                                          fill = factor(node),
                                          label = node)) +
  ggsankey::geom_sankey(flow.alpha = 0.5, node.color = 1) +
  ggsankey::geom_sankey_label(size = 7, color = 1, fill = "white") +
  scale_fill_viridis_d(drop = FALSE) +
  ggsankey::theme_sankey(base_size = 25) +
  guides(fill = guide_legend(title = "")) +
  xlab("") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c"))

ggarrange(WorkSankey, LeisureSankey, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 37), heights = c(1,1))

ggsave("Sankey.pdf", dpi = 500, w = 15, h = 6) 
ggsave("Sankey.png", dpi = 500, w = 15, h = 6)

## QUATROMODAL

setwd("/Users/sydney/git/second-order-contacts/AnalysisSP/QuatromodalGroups")
fileNames <- Sys.glob("*.csv")


dataf <- data.frame(matrix(nrow=0, ncol=6))
colnames(dataf) <- c("value", "group_0", "group_1", "group_2", "group_3", "group")

for(file in fileNames){
    
  group_bel <- paste0("/Users/sydney/git/second-order-contacts/AnalysisSP/QuatromodalGroups/", file)
  
  group_belonging <- read_csv(group_bel)
  group_belonging <- group_belonging %>% mutate(value = seq(-100,75,length.out=500))
  group_belonging$value <- round(group_belonging$value)
  group_belonging <- group_belonging %>% group_by(value) %>% 
    summarise(group_0 = mean(first_group), group_1 = mean(second_group), group_2 = mean(third_group), group_3=mean(fourth_group))
  
  filename <- paste0(str_split(file, "csv")[[1]][1], "csv")
  red <- paste0("/Users/sydney/git/second-order-contacts/AnalysisSP/SecondOrderContactsPaper/DataBimodalFit/", filename)
  reductions <- read_csv(red)
  reductions <- reductions %>% dplyr::select(value)
  reductions$value <- round(reductions$value)
  reductions <- reductions %>% left_join(group_belonging, by = "value") %>%
    mutate(group = case_when(
      runif(n()) < group_0 ~ 0,
      runif(n()) < group_0 + group_1 ~ 1,
      runif(n()) < group_0 + group_1 + group_2 ~ 2,
      TRUE ~ 3
    ))
  reductions <- reductions %>% mutate(filename = file)
  dataf <- rbind(dataf, reductions)
}

print(dataf %>% group_by(filename) %>% count(group) %>% mutate(perc = n/sum(n)), n = 100)
  