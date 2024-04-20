library(tidyverse)
library(igraph)

# Author: S. Paltra, contact: paltra@tu-berlin.de

raw_data <- read_csv("ENTER PATH HERE")

# Reducing data frame to the variables of interest ------------------------

data_reduced <- raw_data %>% select(user_id, ref, cc_change_during_pandemic, total_hsld_size_persons_under_14,
                                    hsld_size_2019_, hsld_size_03_2020_, hsld_size_summer_2021_, hsld_size_01_2023_, 
                                    cc_hsld_size_pre_pandemic_2019_num_hsld_members, cc_hsld_size_pre_pandemic_03_2020_num_hsld_members, cc_hsld_size_pre_pandemic_summer_2021_num_hsld_members, cc_hsld_size_pre_pandemic_01_2023_num_hsld_members,
                                    wkly_cont_2019_work_uni, wkly_cont_03_2020_work_uni, wkly_cont_summer_2021_work_uni, wkly_cont_01_2023_work_uni,
                                    hsld_cont__2019_work_uni, hsld_cont__03_2020_work_uni, hsld_cont__summer_2021_work_uni, hsld_cont__01_2023_work_uni,
                                    cc_weekly_contacts_2019_work_uni_cont, cc_weekly_contacts_03_2020_work_uni_cont, cc_weekly_contacts_summer_2021_work_uni_cont, cc_weekly_contacts_01_2023_work_uni_cont,
                                    cc_weekly_cont_during_pandemic_2019_work_uni_cont, cc_weekly_cont_during_pandemic_03_2020_work_uni_cont, cc_weekly_cont_during_pandemic_summer_2021_work_uni_cont, cc_weekly_cont_during_pandemic_01_2023_work_uni_cont,
                                    wkly_cont_2019_school_kinder, wkly_cont_03_2020_school_kinder, wkly_cont_summer_2021_school_kinder, wkly_cont_01_2023_school_kinder,
                                    hsld_cont__2019_school_kinder, hsld_cont__03_2020_school_kinder, hsld_cont__summer_2021_school_kinder, hsld_cont__01_2023_school_kinder,
                                    cc_weekly_contacts_2019_school_kinder_cont, cc_weekly_contacts_03_2020_school_kinder_cont, cc_weekly_contacts_summer_2021_school_kinder_cont, cc_weekly_contacts_01_2023_school_kinder_cont,
                                    cc_weekly_cont_during_pandemic_2019_school_kg_cont, cc_weekly_cont_during_pandemic_03_2020_school_kg_cont, cc_weekly_cont_during_pandemic_summer_2021_school_kg_cont, cc_weekly_cont_during_pandemic_01_2023_school_kg_cont,
                                    wkly_cont_2019_leisure, wkly_cont_03_2020_leisure, wkly_cont_summer_2021_leisure, wkly_cont_01_2023_leisure,
                                    hsld_cont__2019_leisure, hsld_cont__03_2020_leisure, hsld_cont__summer_2021_leisure, hsld_cont__01_2023_leisure,
                                    cc_weekly_contacts_2019_leisure_cont, cc_weekly_contacts_03_2020_leisure_cont, cc_weekly_contacts_summer_2021_leisure_cont, cc_weekly_contacts_01_2023_leisure_cont,
                                    cc_weekly_cont_during_pandemic_2019_leisure_cont, cc_weekly_cont_during_pandemic_03_2020_leisure_cont, cc_weekly_cont_during_pandemic_summer_2021_leisure_cont, cc_weekly_cont_during_pandemic_01_2023_leisure_cont)

# Respondents who did NOT change their CC during pandemic -----------------

data_reduced <- data_reduced %>% filter(cc_change_during_pandemic == "Nein")

# Respondents who were forwarded the survey -------------------------------

data_reduced <- data_reduced %>% filter(!is.na(ref)) %>% 
                                  filter(!is.na(user_id)) %>%
                                  # Removal of respondets who's reference is one of the 5 people who share the link on twitter
                                  filter(!(ref %in% c("4a76b", "dec9d", "7b598", "008b5", "6c8d7"))) %>%
                                  # Removal of respondents who clicked on the link they were forwarded, but did not fill anything in 
                                  filter(!(user_id %in% c("5c0412b6-edfd-4eb8-84df-f8fe25b8c208", "b8bf1416-b83f-4b1b-a411-e715a6437cd2", "caba33fe-d132-4f6c-8dea-c059392888d4")))
# Manually removing the second to last row as this user started the survey twice
data_reduced <- data_reduced[-21,]

#Setting no of contacts equal to 0 if person DID not answer
#TODO: How do I differentiate real zeros from introduced 0? Are there even "real zeros"? Check!
data_reduced <- as.data.frame(data_reduced)
data_reduced[is.na(data_reduced)] <- 0

#create_network_plots(data_reduced, "2019")


# Function that creates ego networks --------------------------------------

create_network_plots <- function(data_reduced, time){ #time options: "2019", "03_2020", "summer_2021", "01_2023"

  #Loop over all respondents
  for(chosen_user_id in unique(data_reduced$user_id)){
    # one_respondent <- data_reduced_full_records[7,]
    one_respondent <- data_reduced %>% filter(user_id == chosen_user_id)
    
    resp_hsld <- paste0("hsld_size_", time, "_")
    resp_under_14 <- "total_hsld_size_persons_under_14"
    resp_work <- paste0("wkly_cont_", time, "_work_uni")
    resp_school <- paste0("wkly_cont_", time, "_school_kinder")
    resp_leisure <- paste0("wkly_cont_", time, "_leisure")
    
    hsld_work <- paste0("hsld_cont__", time, "_work_uni")
    hsld_school <- paste0("hsld_cont__", time, "_school_kinder")
    hsld_leisure<- paste0("hsld_cont__", time, "_leisure")
    
    cc_pre_hsld <- paste0("cc_hsld_size_pre_pandemic_", time, "_num_hsld_members")
    cc_pre_work <- paste0("cc_weekly_contacts_", time, "_work_uni_cont")
    cc_pre_school <- paste0("cc_weekly_contacts_", time, "_school_kinder_cont")
    cc_pre_leisure <- paste0("cc_weekly_contacts_", time, "_leisure_cont")
    
    cc_during_hsld <- paste0("cc_hsld_size_during_pandemic_", time , "_num_hsld_members")
    cc_during_work <- paste0("cc_weekly_cont_during_pandemic_", time, "_work_uni_cont")
    cc_during_school <- paste0("cc_weekly_cont_during_pandemic_", time, "_school_kinder_cont")
    cc_during_leisure <- paste0("cc_weekly_cont_during_pandemic_", time, "_leisure_cont")
  
      if(one_respondent[[resp_under_14]] != one_respondent[[resp_hsld]]){
      
      listofnodes <- data.frame(matrix(ncol = 2, nrow = 0))
      colnames(listofnodes) <- c("node", "color")
      listofnodes[nrow(listofnodes)+1,] <- c("ego", "#7b4173")
      matrixedges <- data.frame(matrix(ncol = 3, nrow = 0))
      colnames(matrixedges) <- c("from", "to", "weight")
      
    
    # Adding contacts of respondent -------------------------------------------
      # Adding household members
      i <- 1
      if(!is.na(one_respondent[[resp_hsld]]) && one_respondent[[resp_hsld]] > 1){
        while(i < one_respondent[[resp_hsld]]){
          listofnodes[nrow(listofnodes)+1,] <- c(paste0("hh", as.character(i)), "#393b79")
          matrixedges[nrow(matrixedges)+1,] <- c("ego", paste0("hh", as.character(i)), 1)
          i <- i +1
        }
      
        noOfHouseholdMembers <- one_respondent[[resp_hsld]] - 1
        if(noOfHouseholdMembers > 0){
          for(i in 1:noOfHouseholdMembers){
            for(j in 1:noOfHouseholdMembers){
              if(i < j){
            matrixedges[nrow(matrixedges)+1,] <- c(paste0("hh", as.character(i)), paste0("hh", as.character(j)), 1)
              }
            }
          }
        }
      }
      
      #Adding work contacts
      i <- 1
      if(!is.na(one_respondent[[resp_work]])){
        while(i <= one_respondent[[resp_work]]){
          listofnodes[nrow(listofnodes)+1,] <-c(paste0("work", as.character(i)), "#637939")
          matrixedges[nrow(matrixedges)+1,] <- c("ego", paste0("work", as.character(i)), 1)
          i <- i +1
        }
      }
      # Adding school contacts
      i <- 1
      if(!is.na(one_respondent[[resp_school]])){
        while(i <= one_respondent[[resp_school]]){
          listofnodes[nrow(listofnodes)+1,] <- c(paste0("school", as.character(i)), "#8c6d31")
          matrixedges[nrow(matrixedges)+1,] <- c("ego", paste0("school", as.character(i)), 1)
          i <- i +1
        }
      }
      # Adding leisure contacts
      i <- 1
      if(!is.na(one_respondent[[resp_leisure]])){
        while(i <= one_respondent[[resp_leisure]]){
          if(i == 1){
          listofnodes[nrow(listofnodes)+1,] <- c("cc", "#ad494a")
          matrixedges[nrow(matrixedges)+1,] <- c("ego", "cc", 1)
          } else{
          listofnodes[nrow(listofnodes)+1,] <- c(paste0("leisure", as.character(i)), "#843c39")
          matrixedges[nrow(matrixedges)+1,] <- c("ego", paste0("leisure", as.character(i)), 1)
          }
          i <- i +1
        }
      }
      
    
    # Adding contacts of household member -------------------------------------
      # Adding work contacts
      workCounter <- one_respondent[[resp_work]] + 1
      if(!is.na(one_respondent[[hsld_work]]) && one_respondent[[hsld_work]] > 0 && (one_respondent[[resp_hsld]] - one_respondent[[resp_under_14]] - 1 > 0)){ ## TODO: Explain if
        for(j in 1:((one_respondent[[resp_hsld]]) - one_respondent[[resp_under_14]] - 1)){
          i <- 1
          maxi <- one_respondent$hsld_work/(one_respondent[[resp_hsld]] - one_respondent[[resp_under_14]] - 1) #TODO: write comment
          rest <- one_respondent$hsld_work%%(one_respondent[[resp_hsld]] - one_respondent[[resp_under_14]] - 1)
          if(length(rest) == 0){
            rest <- 0
          }
          if(rest == 1 && j == 1){
            maxi <- ceiling(maxi)
          } else if(rest == 2 && (j == 1 | j == 2)){ #TODO: Can this be done in a smarter way?
            maxi <- ceiling(maxi)
          } else if(rest == 3 && (j == 1 | j == 2 | j == 3)){
            maxi <- ceiling(maxi)
          } else if(rest == 4 && (j == 1 | j == 2 | j == 3 | j == 4)){
            maxi <- ceiling(maxi)
          } else if(rest == 5 && (j == 1 | j == 2 | j == 3| j == 4| j == 5)){
            maxi <- ceiling(maxi)
          } else if(rest == 6 && (j == 1 | j == 2 | j == 3| j == 4| j == 5| j == 6)){
            maxi <- ceiling(maxi)
          } else {
            maxi <- floor(maxi)
          }
          if(length(maxi) > 0){
            while(i <= maxi){
              listofnodes[nrow(listofnodes)+1,] <-c(paste0("hh", as.character(j), "_work", as.character(workCounter)), "#8ca252")
              matrixedges[nrow(matrixedges)+1,] <- c(paste0("hh", as.character(j)), paste0("hh", as.character(j), "_work", as.character(workCounter)), 1)
              i <- i + 1
              workCounter <- workCounter + 1
            }
          }
        }
      }
      
      # Adding school contacts
      schoolCounter <- one_respondent[[resp_school]] + 1
      j <- 0
      if(one_respondent[[resp_under_14]] > 0 && !is.na(one_respondent[[hsld_school]]) && one_respondent[[hsld_school]] > 0){ #TODO: Explain if
        for(j in 1:one_respondent[[resp_under_14]]){
          i <- 1
          maxi <- one_respondent[[hsld_school]]/one_respondent[[resp_under_14]] #TODO: write comment
          rest <- one_respondent[[hsld_school]]%%one_respondent[[resp_under_14]]
          if(length(rest) == 0){
            rest <- 0
          }
          if(rest == 1 && j == 1){
            maxi <- ceiling(maxi)
          } else if(rest == 2 && (j == 1 | j == 2)){ #TODO: Can this be done in a smarter way?
            maxi <- ceiling(maxi)
          } else if(rest == 3 && (j == 1 | j == 2 | j == 3)){
            maxi <- ceiling(maxi)
          } else if(rest == 4 && (j == 1 | j == 2 | j == 3 | j == 4)){
            maxi <- ceiling(maxi)
          } else if(rest == 5 && (j == 1 | j == 2 | j == 3| j == 4| j == 5)){
            maxi <- ceiling(maxi)
          } else if(rest == 6 && (j == 1 | j == 2 | j == 3| j == 4| j == 5| j == 6)){
            maxi <- ceiling(maxi)
          } else {
            maxi <- maxi
          }
          if(length(maxi) > 0){
            while(i <= maxi){
              kidCounter <- one_respondent[[resp_hsld]] - one_respondent[[resp_under_14]] + j - 1 
              listofnodes[nrow(listofnodes)+1,] <-c(paste0("hh", as.character(kidCounter), "_school", as.character(schoolCounter)), "#bd9e39")
              matrixedges[nrow(matrixedges)+1,] <- c(paste0("hh", as.character(kidCounter)), paste0("hh", as.character(kidCounter), "_school", as.character(schoolCounter)), 1)
              i <- i + 1
              schoolCounter <- schoolCounter + 1
            }
          }
        }
      }
      # Adding leisure contacts
      leisureCounter <- one_respondent[[resp_leisure]] + 1
      j <- 1
      if(!is.na(one_respondent[[hsld_leisure]]) && one_respondent[[hsld_leisure]] > 0){ #TODO: Explain if
        for(j in 1:((one_respondent[[resp_hsld]]) - 1)){
          i <- 1
          maxi <- one_respondent[[hsld_leisure]]/(one_respondent[[resp_hsld]] - 1) #TODO: write comment
          rest <- one_respondent[[hsld_leisure]]%%(one_respondent[[resp_hsld]] - 1)
          if(length(rest) == 0){
            rest <- 0
          }
          if(rest == 1 && j == 1){
            maxi <- ceiling(maxi)
          } else if(rest == 2 && (j == 1 | j == 2)){ #TODO: Can this be done in a smarter way?
            maxi <- ceiling(maxi)
          } else if(rest == 3 && (j == 1 | j == 2 | j == 3)){
            maxi <- ceiling(maxi)
          } else if(rest == 4 && (j == 1 | j == 2 | j == 3 | j == 4)){
            maxi <- ceiling(maxi)
          } else if(rest == 5 && (j == 1 | j == 2 | j == 3| j == 4| j == 5)){
            maxi <- ceiling(maxi)
          } else if(rest == 6 && (j == 1 | j == 2 | j == 3| j == 4| j == 5| j == 6)){
            maxi <- ceiling(maxi)
          } else {
            maxi <- maxi
          }
          if(length(maxi) > 0){
            while(i <= maxi){
              listofnodes[nrow(listofnodes)+1,] <-c(paste0("hh", as.character(j), "_leisure", as.character(leisureCounter)), "#ad494a")
              matrixedges[nrow(matrixedges)+1,] <- c(paste0("hh", as.character(j)), paste0("hh", as.character(j), "_leisure", as.character(leisureCounter)), 1)
              i <- i + 1
              leisureCounter <- leisureCounter + 1
            }
          }
        }
      }
    
    
    # Adding contacts of CC ---------------------------------------------------
      # Adding household contacts
      householdCounter <- one_respondent[[resp_hsld]] + 1
      i <- 1
      maxi <- one_respondent[[cc_pre_hsld]] - 1
      if(!is.na(one_respondent[[cc_pre_hsld]]) && one_respondent[[cc_pre_hsld]] > 0){ #TODO: Explain if
        while(i <= maxi){
          listofnodes[nrow(listofnodes)+1,] <-c(paste0("cc_hh", as.character(householdCounter)), "#5254a3")
          matrixedges[nrow(matrixedges)+1,] <- c("cc", paste0("cc_hh", as.character(householdCounter)), 1)
          i <- i + 1
          householdCounter <- householdCounter + 1
        }
      }
      # Adding work contacts
      workCounter <- one_respondent[[resp_work]] + one_respondent[[hsld_work]] + 1
        i <- 1
        maxi <- one_respondent[[cc_pre_work]]
      if(!is.na(one_respondent[[resp_work]]) && one_respondent[[resp_work]] > 0 && one_respondent[[cc_pre_work]] > 0){
        while(i <= maxi){
          listofnodes[nrow(listofnodes)+1,] <-c(paste0("cc_work", as.character(workCounter)), "#b5cf6b")
          matrixedges[nrow(matrixedges)+1,] <- c("cc", paste0("cc_work", as.character(workCounter)), 1)
          i <- i + 1
          workCounter <- workCounter + 1
        }
      }
      
      # Adding school contacts
      schoolCounter <- one_respondent[[resp_school]] + one_respondent[[hsld_school]] + 1
          i <- 1
          maxi <- one_respondent[[cc_pre_school]]
        if(!is.na(one_respondent[[cc_pre_school]]) && one_respondent[[cc_pre_school]] > 0){
          while(i <= maxi){
            listofnodes[nrow(listofnodes)+1,] <-c(paste0("cc_school", as.character(schoolCounter)), "#e7ba52")
            matrixedges[nrow(matrixedges)+1,] <- c("cc", paste0("cc_school", as.character(schoolCounter)), 1)
            i <- i + 1
            schoolCounter <- schoolCounter + 1
          }
        }
        
      # Adding leisure contacts
      leisureCounter <- one_respondent[[resp_leisure]] + one_respondent[[hsld_leisure]] + 1
        i <- 1
        maxi <- one_respondent[[cc_pre_leisure]]
      if(!is.na(one_respondent[[cc_pre_leisure]]) && one_respondent[[cc_pre_leisure]] > 0){
        while(i <= maxi){
          listofnodes[nrow(listofnodes)+1,] <-c(paste0("cc_leisure", as.character(leisureCounter)), "#d6616b")
          matrixedges[nrow(matrixedges)+1,] <- c("cc", paste0("cc_leisure", as.character(leisureCounter)), 1)
          i <- i + 1
          leisureCounter <- leisureCounter + 1
        }
      }
      
      if(nrow(listofnodes) > 20){
          net.igraph <- graph_from_data_frame(
            d = matrixedges, vertices = listofnodes$node, 
            directed = FALSE
          )
          
          set.seed(124)
          
          comm=leading.eigenvector.community(net.igraph)
          
          edge.weights <- function(community, network, weight.within = 0.5, weight.between = 1) {
            bridges <- crossing(communities = community, graph = network)
            weights <- ifelse(test = bridges, yes = weight.between, no = weight.within)
            return(weights) 
          }
          
          E(net.igraph)$weight <- edge.weights(comm, net.igraph)
          karateLayout <- layout_with_fr(net.igraph)
          par(mar = c(0,0,5,0))
          filename = paste0(chosen_user_id, "_", time, "_networkPlot.pdf")
          pdf(filename, width = 300, height = 200)
          plot(net.igraph, layout = karateLayout, vertex.label.cex = 0.27, vertex.label.color="white", vertex.label.family = 'sans', vertex.label.font=2, edge.width = 1, vertex.size = 10, frame.color = "black", vertex.label.color="black", edge.color="black", vertex.color = listofnodes$color)
          dev.off()
          ## TODO: WHEN THE PLOT IS SAVED, THE VERTEX LABELS ARE NOT SAVED. WHY???
        }
      }
  }

}
