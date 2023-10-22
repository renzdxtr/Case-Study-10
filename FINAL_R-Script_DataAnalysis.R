library(readxl)
library(ggplot2)
library(dplyr)

# LOADING THE DATASET
dataset <- read_excel("dataset_specific_region.xlsx", sheet = "Mimaropa")


# BASIC SUMMARY STATISTICS
summary(dataset)

# LINEAR MODEL
result <- lm(Extinction_Risk_Rate ~ Average_Temperature_Increase + Deforestation_Rate ,
   data = dataset)

# BASIC SUMMARY STATISTICS
summary(result)

# CREATES A LINE PLOT TO VISUALIZE DATA
linePlot <- ggplot(dataset, aes(x = Year)) +
  geom_line(aes(y = Deforestation_Rate, color = "Deforestation Rate")) +
  geom_line(aes(y = Average_Temperature_Increase, color = "Average Temp Increase")) +
  geom_line(aes(y = Reforestation_Rate, color = "Reforestation Rate")) +
  geom_line(aes(y = Extinction_Risk_Rate, color = "Extinction Risk Rate")) +
  labs(title = "Environmental Data Trends Over Time",
       x = "Year",
       y = "Values") +
  scale_color_manual(values = c("Deforestation Rate" = "red", 
                                "Average Temp Increase" = "blue",
                                "Reforestation Rate" = "green", 
                                "Extinction Risk Rate" = "purple")) +
  theme_minimal()

# DISPLAYS THE PLOT
print(linePlot)

# -----------------------------------------------------------------------------#

# ESTABLISHING THE KNOWLEDGE BASE / FACTS
SpeciesData <- read_excel("dataset_specific_region.xlsx", sheet = "SpeciesData")
ForestData <- read_excel("dataset_specific_region.xlsx", sheet = "ForestData")
OrganizationData <- read_excel("dataset_specific_region.xlsx", sheet = "OrganizationData")
IUCNRedList <- read_excel("dataset_specific_region.xlsx", sheet = "IUCNRedList")
ForestIssues <- read_excel("dataset_specific_region.xlsx", sheet = "ForestIssues")
IssueData <- read_excel("dataset_specific_region.xlsx", sheet = "IssueData")

# ESTABLISHING THE RELATIONSHIPS
SpeciesCategory <- SpeciesData %>%
  left_join(IUCNRedList, by = c("Category" = "Abbreviation")) %>% 
  select(Species, IUCN_Red_List_Category = FullForm) %>% as.data.frame()

ForestIssueData <- ForestIssues %>%
  left_join(IssueData, by = c("Issue" = "Issue")) %>% as.data.frame()

# -----------------------------------------------------------------------------#

# RELATIONSHIPS

categorizedAs <- function(species, category){
  any(SpeciesCategory$Species == species & SpeciesCategory$IUCN_Red_List_Category == category)
}

locatedIn <- function(forest, location){
  any(ForestData$Forest == forest & ForestData$Location == location)
}

canBeFoundIn <- function(species, location){
  any(ForestData$Species == species & ForestData$Location == location)
}

inhabits <- function(forest, species){
  any(ForestData$Forest == forest & ForestData$Species == species)
}

headquarteredIn <- function(organization, location){
  any(OrganizationData$Organization == organization & OrganizationData$Location == location)
}

affects <- function(issue, forest){
  any(ForestIssueData$Issue == issue & ForestIssueData$Forest == forest)
}

adverselyAffects <- function(issue, species){
  data <- ForestIssueData[1:2] %>% left_join(ForestData[2:3], by = c("Forest" = "Forest"), relationship = "many-to-many") %>%
    select(Issue, Species) %>% distinct() %>% as.data.frame()
  
  any(data$Issue == issue & data$Species == species)
}

# -----------------------------------------------------------------------------#

# PREDICTIVE FUNCTIONS

predictImpact <- function(issue = "NA"){
  
  if(issue %in% ForestIssueData$Issue){
    forestImpact <- ForestIssueData[-c(1, 4)] %>% filter(Issue == issue) %>% distinct %>% select(Impact) %>% as.data.frame()
    
    cat("\n\nIt has been predicted that", issue, "will bring forth the impact:\n\n", unique(forestImpact$Impact))
    predictAffectedForestInLocation(issue)
  } else if (issue == "NA") {cat("\nPlease enter an issue!\n")} else {cat("\nIssue not found in the database!\n")}
}

predictAffectedForestInLocation <- function(issue = "NA"){
  if(issue %in% ForestIssueData$Issue){
    affectedForestInLocation <- ForestIssueData %>% filter(Issue == issue) %>% 
      left_join(ForestData[1:2], by = c("Forest" = "Forest")) %>%
      distinct() %>% select(Affected_Forest = Forest, Location, Practice) %>% as.data.frame()
    
    cat("\n\nIt has been predicted that", issue, "will adversely affect the following Forest in location:\n\n")
    print(affectedForestInLocation %>% select(-Practice))
    
    cat("\n\nTo fight", issue, "It is suggested to perform the conservation practice: \n\n", unique(affectedForestInLocation$Practice))
    
  } else if (issue == "NA") {cat("\nPlease enter an issue!\n")} else {cat("\nIssue not found in the database!\n")}
}

predictAllAffectedForestInLocation <- function(location = "NA"){
  if (location %in% unique(ForestData$Location)){
    affectedForests <- ForestData %>% filter(Location == location) %>% select(Forest) %>% distinct() %>%as.data.frame()
    cat("\nIf an environmental issue arises in", location, "it has been predicted that the following forests will be put to risk:\n\n")
    print(affectedForests)
  } else if (location == "NA") {cat("\nPlease enter a location!\n")} else {cat("\nLocation not found in the database!\n")}
}

predictAffectedSpecies <- function(issue = "NA"){
  if(issue %in% ForestIssueData$Issue){
    affectedSpecies <- ForestIssueData %>% filter(Issue == issue) %>% 
      left_join(ForestData, by = c("Forest" = "Forest")) %>%
      select(Species, Location, Forest) %>% distinct() %>% as.data.frame()
    
    cat("\n\nIt has been predicted that", issue, "will adversely affect the following Species:\n\n")
    print(affectedSpecies)
  } else if (issue == "NA") {cat("\nPlease enter an issue!\n")} else {cat("\nIssue not found in the database!\n")}
}

predictAffectedSpeciesPerForestLocation <- function(forest = "NA", location = "NA"){
  if(locatedIn(forest, location)){
    affectedSpecies <- ForestData %>% filter(Forest == forest, Location == location) %>% select(Species) %>% as.data.frame()
    
    cat("\n\nIf an environmental crisis arises in", forest, "in", location,
        ", it has been predicted that the following species below will be at risk:\n\n")
    print(affectedSpecies)
  } else if ((forest == "NA") | (location == "NA")) {cat("\nPlease enter a forest and an issue!\n")} 
  else {cat("\n", forest, "is not located in", location, "!\n")}
}

suggestOrganization <- function(location = "NA"){
  if (!location %in% unique(OrganizationData$Location)){cat("\nThe location", location, "isn't in the database!\n")}
  else if (location == "NA"){cat("\nPlease enter a location!\n")}
  else {print(OrganizationData %>% filter(Location == location) %>% select(-Location) %>% distinct() %>% as.data.frame())}
}

identifyThreatenedSpecies <- function(location = "NA"){
  threatenedSpecies <- ForestData %>% left_join(SpeciesData, by = c("Species" = "Species"), relationship = "many-to-many") %>% 
    filter(Location == location) %>% left_join(IUCNRedList, by = c("Category" = "Abbreviation")) %>% 
    filter(FullForm %in% c("Vulnerable", "Endangered", "Critically Endangered")) %>%
    select(Forest, Species, Category = FullForm) %>% as.data.frame()
  
  if (location == "NA"){cat("\nPlease enter a location!\n")}
  if (nrow(threatenedSpecies) == 0){cat("There is no threatened species in", lcoation)}
  else if (!location %in% unique(ForestData$Location)){cat("\nThe location", location, "isn't in the database!\n")}
  else{
    cat("\nWarning! A threatened species has been identified in", location, "!\n\n Prioritize Utmost Conservation!\n\n")
    print(threatenedSpecies)
    
    cat("\nIt is suggested to Contact the organization below:\n\n")
    suggestOrganization(location)
  }
}

# -----------------------------------------------------------------------------#

while (TRUE){
  cat("\n\nMain Menu\n",
  "1. Predict Impact\n",
  "2. Predict Affected Forest In Location\n",
  "3. Predict All Affected Forest In Location\n",
  "4. Predict Affected Species\n",
  "5. Predict Affected Species Per Forest Location\n",
  "6. Suggest Organization\n",
  "7. Identify Threatened Species\n",
  "8. Exit\n\n")
  
  case <- readline("> ")
  
  cat("\n")
  
  switch (
    case,
    "1" = predictImpact(readline("Input an issue: ")),
    "2" = predictAffectedForestInLocation(readline("Input an issue: ")),
    "3" = predictAllAffectedForestInLocation(readline("Input a location: ")),
    "4" = predictAffectedSpecies(readline("Input an issue: ")),
    "5" = predictAffectedSpeciesPerForestLocation(readline("Input a forest: "), readline("Input a location: ")),
    "6" = suggestOrganization(readline("Input a location: ")),
    "7" = identifyThreatenedSpecies(readline("Input a location: ")),
    "8" = break
  )
}
