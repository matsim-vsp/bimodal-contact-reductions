library(tidyverse)
library(ggsankey)
library(here)
library(ggsankeyfier)
library(ggpubr)

here()
source("./R/mytheme.r")
source("./R/DataCleaningPrepForContactAnalysis.R")
source("./R/GroupAssignmentSankey.R")

# Work Sankey Plots -------------------------------------------------------

Work <- left_join(Work032020, WorkSummer2021, by = join_by(ID))
Work <- left_join(Work, Work012023, by = join_by(ID))
names(Work)[names(Work) == 'group.x'] <- '03/2020'
names(Work)[names(Work) == 'group.y'] <- 'Summer 2021'
names(Work)[names(Work) == 'group'] <- '01/2023'

Work <- Work %>% ggsankey::make_long(`03/2020`, `Summer 2021`, `01/2023`)

Work <- Work %>% mutate(node = case_when(node == 0 ~ "Strong\nReduction", node == 1 ~ "Intermediate\nReduction", node == 2 ~ "Little\nChange"))
Work <- Work %>% mutate(next_node = case_when(next_node == 0 ~ "Strong\nReduction", next_node == 1 ~ "Intermediate\nReduction", next_node == 2 ~ "Little\nChange"))

Work$node <- factor(Work$node, levels = c(NA, "Strong\nReduction", "Intermediate\nReduction", "Little\nChange"))

WorkSankey <- Work %>% filter(!is.na(node)) %>% filter(!is.na(next_node)) %>% 
  count(x,node,next_x,next_node) %>% 
  group_by(x) %>% 
  summarise(node=node,next_node=next_node, n=n, sumTime=sum(n), y_axs = n/sumTime) %>% 
  #group_by(x,node) %>%
  #summarise(next_node=next_node, n=n, sumTime=sumTime, sumGroup = (sum(n))) %>%
  mutate(node = case_when(node == "Strong\nReduction" ~ 0, node == "Intermediate\nReduction" ~ 1, node == "Little\nChange" ~ 2)) %>%
  mutate(next_node = case_when(next_node == "Strong\nReduction" ~ 0, next_node == "Intermediate\nReduction" ~ 1, next_node == "Little\nChange" ~ 2)) 

WorkSankey <- WorkSankey[order(WorkSankey$x),]
WorkSankey <- WorkSankey[order(WorkSankey$next_node),]
WorkSankey <- WorkSankey[order(WorkSankey$node),]
WorkSankey <- WorkSankey[order(WorkSankey$x),]
colnames(WorkSankey)[1] <- "stage"

WorkSankey$edge_id <- seq.int(nrow(WorkSankey)) 

WorkSankeyfrom <- WorkSankey %>% mutate(connector = "from") %>% select(c("stage", "node", "y_axs", "edge_id", "connector"))
WorkSankeyto <- WorkSankey %>% mutate(connector = "to") %>% select(c("stage", "next_node", "y_axs", "edge_id", "connector")) %>%
  mutate(stage = case_when(stage == "03/2020" ~ "Summer 2021", stage == "Summer 2021" ~ "01/2023"))
colnames(WorkSankeyto)[2] <- "node"

WorkSankey <- rbind(WorkSankeyfrom, WorkSankeyto)

WorkSankey <- WorkSankey[order(WorkSankey$edge_id),]

WorkSankey$node <- as.character(WorkSankey$node)
WorkSankey$stage <- factor(WorkSankey$stage, levels = c("03/2020", "Summer 2021", "01/2023"))

pos <- position_sankey(split_nodes = FALSE, align = "top", order = "as_is",
                       width = 0.2, v_space = 0.15, h_space = 0.25)

WorkSankey <- WorkSankey %>% mutate(node = case_when(node == "0" ~ "Strong Reduction", node  == "1" ~ "Intermediate Reduction", node == "2" ~ "Little Change"))

WorkSankey$node <- factor(WorkSankey$node, levels = c("Strong Reduction", "Intermediate Reduction", "Little Change"))

WorkSankey$y_axs <- round(WorkSankey$y_axs, 2)

WorkSankey <- WorkSankey %>% mutate(y_axs = case_when((node == "Intermediate Reduction" & stage == "Summer 2021" & y_axs == 0.03) ~ 0.04,
                                                  (node == "Strong Reduction" & stage == "Summer 2021"& y_axs == 0.05) ~ 0.04,
                                                  .default = y_axs))

Workplot <- ggplot(WorkSankey,
                   aes(x = stage, y = y_axs, group = node, connector = connector,
                       edge_id = edge_id, fill = node)) +
  geom_sankeyedge(position = pos) +
  geom_sankeynode(position = pos) +
  theme_minimal() +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
  my_theme() +
  ylab("") +
  xlab("") +
  theme(legend.position = "bottom") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y =element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

# Leisure Sankey Plot -----------------------------------------------------

Leisure <- left_join(Leisure032020, LeisureSummer2021, by = join_by(ID))
Leisure <- left_join(Leisure, Leisure012023, by = join_by(ID))

names(Leisure)[names(Leisure) == 'group.x'] <- '03/2020'
names(Leisure)[names(Leisure) == 'group.y'] <- 'Summer 2021'
names(Leisure)[names(Leisure) == 'group'] <- '01/2023'

Leisure <- Leisure %>% ggsankey::make_long(`03/2020`, `Summer 2021`, `01/2023`)

Leisure <- Leisure %>% mutate(node = case_when(node == 0 ~ "Strong\nReduction", node == 1 ~ "Intermediate\nReduction", node == 2 ~ "Little\nChange"))
Leisure <- Leisure %>% mutate(next_node = case_when(next_node == 0 ~ "Strong\nReduction", next_node == 1 ~ "Intermediate\nReduction", next_node == 2 ~ "Little\nChange"))

Leisure$node <- factor(Leisure$node, levels = c(NA, "Strong\nReduction", "Intermediate\nReduction", "Little\nChange"))

LeisureSankey <- Leisure %>% filter(!is.na(node)) %>% filter(!is.na(next_node)) %>% 
  count(x,node,next_x,next_node) %>% 
  group_by(x) %>% 
  summarise(node=node,next_node=next_node, n=n, sumTime=sum(n), y_axs = n/sumTime) %>% 
  #group_by(x,node) %>%
  #summarise(next_node=next_node, n=n, sumTime=sumTime, sumGroup = (sum(n))) %>%
  mutate(node = case_when(node == "Strong\nReduction" ~ 0, node == "Intermediate\nReduction" ~ 1, node == "Little\nChange" ~ 2)) %>%
  mutate(next_node = case_when(next_node == "Strong\nReduction" ~ 0, next_node == "Intermediate\nReduction" ~ 1, next_node == "Little\nChange" ~ 2)) 

LeisureSankey <- LeisureSankey[order(LeisureSankey$x),]
LeisureSankey <- LeisureSankey[order(LeisureSankey$next_node),]
LeisureSankey <- LeisureSankey[order(LeisureSankey$node),]
LeisureSankey <- LeisureSankey[order(LeisureSankey$x),]
colnames(LeisureSankey)[1] <- "stage"

LeisureSankey$edge_id <- seq.int(nrow(LeisureSankey)) 

LeisureSankeyfrom <- LeisureSankey %>% mutate(connector = "from") %>% select(c("stage", "node", "y_axs", "edge_id", "connector"))
LeisureSankeyto <- LeisureSankey %>% mutate(connector = "to") %>% select(c("stage", "next_node", "y_axs", "edge_id", "connector")) %>%
  mutate(stage = case_when(stage == "03/2020" ~ "Summer 2021", stage == "Summer 2021" ~ "01/2023"))
colnames(LeisureSankeyto)[2] <- "node"

LeisureSankey <- rbind(LeisureSankeyfrom, LeisureSankeyto)

LeisureSankey <- LeisureSankey[order(LeisureSankey$edge_id),]

LeisureSankey$node <- as.character(LeisureSankey$node)
LeisureSankey$stage <- factor(LeisureSankey$stage, levels = c("03/2020", "Summer 2021", "01/2023"))

LeisureSankey <- LeisureSankey %>% mutate(node = case_when(node == "0" ~ "Strong Reduction", node  == "1" ~ "Intermediate Reduction", node == "2" ~ "Little Change"))

LeisureSankey$node <- factor(LeisureSankey$node, levels = c("Strong Reduction", "Intermediate Reduction", "Little Change"))

LeisureSankey$y_axs <- round(LeisureSankey$y_axs, 2)


LeisureSankey <- LeisureSankey %>% mutate(y_axs = case_when((stage == "Summer 2021" & node == "Strong Reduction" & y_axs == 0.01) ~ 0.02,
                                                  (node == "Intermediate Reduction" & stage == "Summer 2021"& y_axs == 0.30) ~ 0.29,
                                                  (node == "Intermediate Reduction" & stage == "Summer 2021"& y_axs == 0.33) ~ 0.32,
                                                  (node == "Little Change" & stage == "Summer 2021"& y_axs == 0.07) ~ 0.06,
                                                  (node == "Little Change" & stage == "Summer 2021"& y_axs == 0.08) ~ 0.07,
                                                  .default = y_axs))




Leisureplot <- ggplot(LeisureSankey, 
                      aes(x = stage, y = y_axs, group = node, connector = connector,
                          edge_id = edge_id, fill = node)) +
  geom_sankeyedge(position = pos) +
  geom_sankeynode(position = pos) +
  theme_minimal() +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
  my_theme() +
  ylab("") +
  xlab("") +
  theme(legend.position = "bottom") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y =element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

# Arranging and saving the two plots

ggarrange(Workplot, Leisureplot, labels = c("A", "B"), nrow = 1, ncol = 2,font.label = list(size = 45), heights = c(1,1), common.legend = TRUE, legend="bottom")

#ggsave("Sankey.pdf", dpi = 500, w = 20, h = 7.5) 
#ggsave("Sankey.png", dpi = 500, w = 20, h = 7.5)