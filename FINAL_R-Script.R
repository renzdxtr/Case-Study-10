# ---------------------------------------------------------------------------- #

if(!require('readxl')) {
  install.packages('readxl')
  library(readxl)
}

if(!require('dplyr')) {
  install.packages('dplyr')
  library(dplyr)
}

# ---------------------------------------------------------------------------- #

# ESTABLISHING THE KNOWLEDGE BASE / FACTS

ForestType <- read_excel("dataset.xlsx", sheet = "ForestType")

Species <- read_excel("dataset.xlsx", sheet = "Species")

ClimateData <- read_excel("dataset.xlsx", sheet = "ClimateData")

ConservationPractice <- read_excel("dataset.xlsx", sheet = "ConservationPractice")

ForestInLocation <- read_excel("dataset.xlsx", sheet = "ForestInLocation")

SpeciesInLocation <- read_excel("dataset.xlsx", sheet = "SpeciesInLocation")

EnvironmentalOrganization <- read_excel("dataset.xlsx", sheet = "EnvironmentalOrganization")

# https://www.iucnredlist.org/#:~:text=It%20divides%20species%20into%20nine,in%20the%20Wild%20and%20Extinct.
IUCNRedList <- read_excel("dataset.xlsx", sheet = "IUCNRedList")

SpeciesType <- read_excel("dataset.xlsx", sheet = "SpeciesType")

OrgType <- read_excel("dataset.xlsx", sheet = "OrgType")

Locations <- read_excel("dataset.xlsx", sheet = "Locations")

SpeciesData <- read_excel("dataset.xlsx", sheet = "SpeciesData")

EnvironmentalData <- read_excel("dataset.xlsx", sheet = "EnvironmentalData")

EnvironmentalIssues <- read_excel("dataset.xlsx", sheet = "EnvironmentalIssues")

Impacts <- read_excel("dataset.xlsx", sheet = "Impacts")

ConservationPractice <- read_excel("dataset.xlsx", sheet = "ConservationPractice")

ForestConcernedByIssues <- read_excel("dataset.xlsx", sheet = "ForestConcernedByIssues")

IssuesInLocationsPerForestType <- read_excel("dataset.xlsx", sheet = "IssuesInLocationsPerForestType")

# ---------------------------------------------------------------------------- #

# LIST ALL THE CLASSES AND THEIR RESPECTIVE PROPERTIES

listClassesAndProperties <- function(){
  cat("List of Classes and their Properties:\n",
      "\n[1] ForestType:\n\tProperties:", names(ForestType),
      "\n[2] Species:\n\tProperties:", names(Species),
      "\n[3] ClimateData:\n\tProperties:", names(ClimateData),
      "\n[4] ConservationPractice:\n\tProperties:", names(ConservationPractice),
      "\n[5] ForestInLocation:\n\tProperties:", names(ForestInLocation),
      "\n[6] SpeciesInLocation:\n\tProperties:", names(SpeciesInLocation),
      "\n[7] EnvironmentalOrganization:\n\tProperties:", names(EnvironmentalOrganization),
      "\n[8] IUCNRedList:\n\tProperties:", names(IUCNRedList),
      "\n[9] SpeciesType:\n\tProperties:", names(SpeciesType),
      "\n[10] OrgType:\n\tProperties:", names(OrgType),
      "\n[11] Locations:\n\tProperties:", names(Locations),
      "\n[11] SpeciesData:\n\tProperties:", names(SpeciesData),
      "\n[13] CP_Impacts:\n\tProperties:", names(Impacts),
      "\n[14] EnvironmentalData:\n\tProperties:", names(EnvironmentalData),
      "\n[15] EnvironmentalIssues:\n\tProperties:", names(EnvironmentalIssues),
      "\n[15] EnvironmentalIssues:\n\tProperties:", names(ConservationPractice),
      "\n[15] EnvironmentalIssues:\n\tProperties:", names(ForestConcernedByIssues),
      "\n[15] EnvironmentalIssues:\n\tProperties:", names(IssuesInLocationsPerForestType)
  )
}

# ---------------------------------------------------------------------------- #

# ESTABLISHING THE RELATIONSHIPS

forestTypeInLocation <- ForestInLocation %>%
  left_join(ForestType, by = c("ForestType" = "Abbreviation")) %>%
  left_join(Locations, by = c("Location" = "Abbreviation")) %>%
  select(Forest_Type = TypeName,
         Location = Location.y) %>%
  as.data.frame()

forestProtectedBy <- EnvironmentalOrganization[c(2, 3, 4, 6)] %>%
  left_join(Locations, by = c("Location" = "Abbreviation")) %>%
  left_join(ForestType[1:2], by = c("ForestType" = "Abbreviation")) %>%
  select(Forest_Type = TypeName,
         Location = Location.y,
         Organization = OrgName) %>%
  as.data.frame()

supportsSpecies <- EnvironmentalOrganization[c(2, 4, 6)] %>%
  left_join(Locations, by = c("Location" = "Abbreviation")) %>%
  left_join(Locations, by = c("Location" = "Location")) %>%
  left_join(SpeciesInLocation, by = c("Location" = "Location"), relationship = "many-to-many") %>%
  left_join(Species, by = c("SpeciesID" = "SpeciesID")) %>%
  select(Organization = OrgName,
         Location = Location.y,
         Species = CommonName) %>%
  as.data.frame()
  
climateDataInLocation <- ClimateData %>%
  left_join(Locations, by = c("Location" = "Abbreviation")) %>%
  select(Location = Location.y,
         3:6) %>%
  as.data.frame()

speciesHabitat <- SpeciesData[1:2] %>%
  left_join(ForestType[1:2], by = c("Habitat" = "Abbreviation")) %>%
  left_join(Species, by = c("SpeciesID" = "SpeciesID")) %>%
  select(Species = CommonName,
         Habitat = TypeName) %>%
  as.data.frame()

# *

environmetalIssuesImpact <- EnvironmentalIssues[2:3] %>%
  left_join(Impacts,
            by = c("Abbreviation" = "Abbreviation"),
            relationship = "many-to-many") %>%
  select(Issue,
         Impact) %>%
  as.data.frame()

conservationPractices <- EnvironmentalIssues[2:3] %>%
  left_join(ConservationPractice[2:3],
            by = c("Abbreviation" = "Abbreviation"), 
            relationship = "many-to-many") %>%
  select(Issue,
         Practice) %>%
  as.data.frame()
  
speciesInLocation <- SpeciesInLocation %>%
  left_join(Species, by = c("SpeciesID" = "SpeciesID")) %>%
  left_join(Locations, by = c("Location" = "Abbreviation")) %>%
  select(Species = CommonName,
         Location = Location.y) %>%
  as.data.frame()

forestConcernedByIssue <- EnvironmentalIssues[2:3] %>%
  left_join(ForestConcernedByIssues,
            by = c("Abbreviation" = "Issue"), 
            relationship = "many-to-many") %>%
  left_join(ForestType[1:2],
            by = c("ForestConcerned" = "Abbreviation")) %>%
  select(Issue,
         Concerned_Forest = TypeName) %>%
   as.data.frame()

issuesInLocationPerForestType <- IssuesInLocationsPerForestType %>%
  left_join(EnvironmentalIssues[2:3],
            by = c("Issue" = "Abbreviation"),
            relationship = "many-to-many") %>%
  left_join(Locations,
            by = c("Location" = "Abbreviation")) %>%
  left_join(ForestType[1:2],
            by = c("Forest_Type" = "Abbreviation")) %>%
  select(Location = Location.y,
         Forest_Type = TypeName,
         Issue = Issue.y) %>%
  as.data.frame()

redListOfSpecies <- SpeciesData[c(1, 4)] %>%
  left_join(Species[1:2], by = c("SpeciesID" = "SpeciesID")) %>%
  left_join(IUCNRedList, by = c("IUCNRedList" = "Abbreviation")) %>%
  select(Species = CommonName,
         IUCN_Red_List = FullForm) %>%
  as.data.frame()

contactsConcernedForestPerLocation <- EnvironmentalOrganization[4:6] %>%
  left_join(ForestType[1:2], b = c("ForestType" = "Abbreviation")) %>%
  left_join(Locations, by = c("Location" = "Abbreviation")) %>%
  select(Location = Location.y,
         Forest_Type = TypeName,
         Contact_Information= ContactInformation) %>%
  as.data.frame()


environmentalIssuesImpactInForestTypePerLocation <- IssuesInLocationsPerForestType %>%
  left_join(EnvironmentalIssues[2:3],
            by = c("Issue" = "Abbreviation"),
            relationship = "many-to-many") %>%
  left_join(Locations,
            by = c("Location" = "Abbreviation")) %>%
  left_join(ForestType[1:2],
            by = c("Forest_Type" = "Abbreviation")) %>%
  left_join(Impacts,
            by = c("Issue" = "Abbreviation"),
            relationship = "many-to-many") %>%
  select(Forest_Type = TypeName,
         Location = Location.y,
         Issue = Issue.y,
         Impact) %>%
  as.data.frame()

organizationData <- EnvironmentalOrganization[c(2, 3, 4, 6)] %>%
  left_join(ForestType[1:2], b = c("ForestType" = "Abbreviation")) %>%
  left_join(OrgType, by = c("OrgType" = "Abbreviation")) %>%
  left_join(Locations, by = c("Location" = "Abbreviation")) %>%
  select(Organization = OrgName,
         Location = Location.y,
         Forest_Type = TypeName,
         Classification = FullForm) %>%
  as.data.frame()

speciesType <- Species[c(1, 2)] %>%
  left_join(SpeciesData[c(1, 3)], by = c("SpeciesID" = "SpeciesID")) %>%
  left_join(SpeciesType, by = c("Type" = "Abbreviation")) %>%
  select(Species = CommonName,
         Type = TypeName) %>%
  as.data.frame()

# ---------------------------------------------------------------------------- #
# RELATIONSHIPS

# forestTypeInLocation
hasA <- function(location, forest){
  if (any(forestTypeInLocation$Forest_Type == forest & 
          forestTypeInLocation$Location == location)){
    cat(location, "has a", forest)
  } else { cat(location, "has no", forest) }
}

# forestProtectedBy
protectedBy <- function(forest, organization){
  if (any(forestProtectedBy$Forest_Type == forest &
          forestProtectedBy$Organization == organization)){
    cat(forest, "is protected by", organization)
  } else { cat(forest, "is not protected by", organization) }
}

# supportsSpecies
supportedBy <- function(species, organization){
  if (any(supportsSpecies$Species == species &
          supportsSpecies$Organization == organization)){
    cat(species, "is supported by", organization)
  } else { cat(species, "is not supported by", organization) }
}

# speciesHabitat
inhabitantOf <- function(species, forest){
  if (any(speciesHabitat$Species == species &
          speciesHabitat$Habitat == forest)){
    cat(species, "is inhabitant of", forest)
  } else { cat(species, "is not inhabitant of", forest) }
}

# speciesInLocation
canBeFoundIn <- function(species, location){
  if (any(speciesInLocation$Species == species &
          speciesInLocation$Location == location)){
    cat(species, "can be found in", location)
  } else { cat(species, "cannot be found in", location) }
}

# redListOfSpecies
categorizedAs <- function(species, category){
  if (any(redListOfSpecies$Species == species &
          redListOfSpecies$IUCN_Red_List == category)){
    cat(species, "is categorized as", category)
  } else { cat(species, "is not categorized as", category) }
}

# organizationData
oversees <- function(organization, forest){
  if (any(organizationData$Organization == organization &
          organizationData$Forest_Type == forest)){
    cat(organization, "oversees", forest)
  } else { cat(organization, "does not oversees", forest) }
}

# organizationData
isFor <- function(organization, classification){
  if (any(organizationData$Organization == organization &
          organizationData$Classification == classification)){
    cat(organization, "is for", classification)
  } else { cat(organization, "is not for", classification) }
}

# organizationData
stationedAt <- function(organization, location){
  if (any(organizationData$Organization == organization &
          organizationData$Location == location)){
    cat(organization, "is stationed at", location)
  } else { cat(organization, "is not stationed at", location) }
}

# organizationData
hasOrgFor <- function(location, forest){
  if (any(organizationData$Location == location &
          organizationData$Forest_Type == forest)){
    cat(location, "has an organization for", forest)
  } else { cat(location, "has an organization for", forest) }
}

# speciesType
isA <- function(species, type){
  if (any(speciesType$Species == species &
          speciesType$Type == type)){
    cat(species, "is a", type)
  } else { cat(species, "is not a", type) }
}

# environmentalIssuesImpactInForestTypePerLocation
impacts <- function(issue, location){
  if (any(environmentalIssuesImpactInForestTypePerLocation$Issue == issue &
          environmentalIssuesImpactInForestTypePerLocation$Location == location)){
    cat(issue, "impacts", location)
  } else { cat(issue, "does not impacts", location) }
}

# forestConcernedByIssue
influences <- function(issue, forest){
  if (any(forestConcernedByIssue$Issue == issue &
          forestConcernedByIssue$Concerned_Forest == forest)){
    cat(issue, "influences", forest)
  } else { cat(issue, "does not influences", forest) }
}

# climateDataInLocation
recordedIn <- function(climate_data, location){
  if (any(climateDataInLocation$Temperature == climate_data[1] &
          climateDataInLocation$Precipitation == climate_data[2] &
          climateDataInLocation$Humidity == climate_data[3] &
          climateDataInLocation$Location == location)) {
    cat(climate_data, "recorded in", location)
  } else { cat(climate_data, "was not recorded in", location) }
}

# ---------------------------------------------------------------------------- #

climateDataInSpecificLocation <- function(location){
  climateDataInLocation %>%
    filter(Location == location)
}

climateChange <- function(location){
  initialData <- climateDataInSpecificLocation(location)[-2]
  
  newData <- initialData %>%
    mutate(Temperature = Temperature + runif(1, min = 0, max = 2))
  
  tempDiff <- newData$Temperature - initialData$Temperature
  
  # https://www.climatesignals.org/scientific-reports/decade-weather-extremes
  newData <- newData %>%
    mutate(Humidity = Humidity + (initialData$Humidity * (0.07 * tempDiff)))
  
  return(newData)
}

BiodiversityInLocation <- function(location){
  speciesInLocation %>%
    filter(Location == location)
}

# speciesHabitat
BiodiversityInForest <- function(forest){
  speciesHabitat %>% 
    filter(Habitat == forest)
}

changeInForestBiodiversityByClimateChange <- function(location){
  climateData <- climateChange(location)
  
  tempDiff <- round(climateData$Temperature - climateDataInSpecificLocation(location)$Temperature, digits = 2)
  
  if (tempDiff < 1){
    cat("A", tempDiff, "°C temperature increase has no significant effect that will adversely affect in Forest Biodiversity in", location,"\n\n")
  } else if (tempDiff <= 2) {
    cat("It has been forecasted that a", tempDiff, "°C temperature increase has a significant adverse effect in Forest Biodiversity in", location,"\n\n")
    
    cat("With this excessive increase in temperature, it has been predicted that the following Forest Types would be negatively affected:\n\n")
    
    numberOfForestTypes <- filter(forestTypeInLocation, Location == location)
    print(numberOfForestTypes)
    
    cat("\n\nThe prediction also identified that the following Species in", location, "would also be adversely affected:\n\n")
    print(BiodiversityInLocation(location)[1])
    
    cat("\n\nIt is advisable to reach the following organization(s):\n\n")
    
    orgData <- organizationData %>%
                  filter(Location == location) %>%
                  select(-c("Location", "Classification"))
    print(orgData)
    
    lackOrg <- numberOfForestTypes %>%
                filter(!(Forest_Type %in% orgData$Forest_Type))
    
    if(NROW(numberOfForestTypes$Forest_Type) > NROW(orgData$Organization)){
      cat("\n\nIt has been detected that", location, "has no Organization for the following Forest Types:\n\n")
      print(lackOrg %>% select(-Location))
      
      cat("\n\nSince there is no Organization for the aforestated Forest Type in", location,
          "it has been predicted that the following species will be threatened:\n\n")
      
      affectedSpecies <- BiodiversityInLocation(location) %>%
        left_join(BiodiversityInForest(lackOrg$Forest_Type),
                  by = c("Species" = "Species")) %>% 
        filter(!is.na(Habitat)) %>%
        as.data.frame()
      
      print(affectedSpecies)
    }
  }
}

# ---------------------------------------------------------------------------- #

suggestPracticeInForestIssue <- function(forest){
  forestConcernedByIssue %>%
    filter(Concerned_Forest == forest) %>%
    left_join(conservationPractices,
              by = c("Issue" = "Issue")) %>%
    select(-Concerned_Forest) %>%
    as.data.frame()
}

speciesPopulationPerForestType <- function(forest){
  SpeciesData[c(1, 2, 5)] %>%
    left_join(ForestType[1:2],
            by = c("Habitat" = "Abbreviation")) %>%
    filter(TypeName == forest) %>%
    left_join(Species[1:2],
              by = c("SpeciesID" = "SpeciesID")) %>%
    select(Forest_Type = TypeName,
           Species = CommonName,
           Population)
}

populationLostInForestBiodiversity <- function(forest){
  biodiversity <- speciesPopulationPerForestType(forest)
  totalDiversity <- nrow(biodiversity)
  
  biodiversity$RemainingPopulation <- biodiversity$Population
  
  i <- 1
  while (i <= totalDiversity) {
    biodiversity$RemainingPopulation[i] - runif(1, 0, biodiversity$Population[i])

    # https://ourworldindata.org/extinction-risk-definition#:~:text=Take%20metric%20(1)%3A%20the,there%20are%20fewer%20than%201%2C000.
    if (biodiversity$RemainingPopulation[i] < 50){
      cat("The population of species \"", biodiversity$Species[i], "\" is critically low!\n",
      "It has been identified that the aforesaid species is Critically Endangered\n",
      "To prevent it from being Extinct, administer Conservation Practices from below:\n\n")
      
      print(suggestPracticeInForestIssue(forest))
    }
    else if (biodiversity$RemainingPopulation[i] < 250){
      cat("The population of species \"", biodiversity$Species[i], "\" is low\n",
          "It has been identified that the aforesaid species is Endangered\n",
          "To prevent it from being Critically Endangered, administer Conservation Practices from below:\n\n")
      print(suggestPracticeInForestIssue(forest))
    }
    else if (biodiversity$RemainingPopulation[i] < 1000){
      cat("The population of species \"", biodiversity$Species[i], "\" is in medium scale.\n",
          "It has been identified that the aforesaid species is Vulnerable\n",
          "To prevent it from being Endangered, administer Conservation Practices from below:\n\n")
      print(suggestPracticeInForestIssue(forest))
    }
    else {
      cat("The population of species \"", biodiversity$Species[i], "\" is good.\n",
          "It has been identified that the aforesaid species is not threatened.\n")
    }
    
    cat("\n\n")
    
    i <- i + 1
  }
}

# ---------------------------------------------------------------------------- #

identifyPriorityConservationForestLocation <- function(){
  # https://global.si.edu/projects/protecting-endangered-mangroves-around-world
  forestType <- "Mangrove Forest"
  
  cat("According to global.si.edu \"Mangroves are threatened throughout the world and disappearing at an alarming rate.\" 
Thus, the following Locations below that contains this forest type are advised to Prioritize Conservation:\n\n")
  prioritizedForestLocation <- forestTypeInLocation %>%
                                filter(Forest_Type == forestType)
  
  print(prioritizedForestLocation)
  
  cat("\n\nWith that, it is predicted that the following inhabitants (species) of Mangrove Forest 
in the abovementioned Locations will be preserved should a conservation practice is put to action:\n\n")
  print(speciesHabitat %>%
    filter(Habitat == forestType) %>%
      left_join(speciesInLocation,
                by = c("Species" = "Species")) %>%
      filter(Location %in% prioritizedForestLocation$Location) %>%
      select(Species))
  
  cat("\n\nHere are the list of relevant organizations for each location that oversees Mangrove Forests:\n\n")
  organizationData %>%
    filter(Location %in% prioritizedForestLocation$Location,
           Forest_Type == "Mangrove Forest") %>%
    select(-c("Location", "Forest_Type"))
}

# ---------------------------------------------------------------------------- #

# redListOfSpecies
forestConservationPrioritiesRedList <- function(){
  redListOfSpecies %>%
    filter(IUCN_Red_List %in% c("Vulnerable", "Endangered", "Critically Endangered")) %>%
    left_join(speciesHabitat,
              by = c("Species" = "Species")) %>%
    left_join(forestTypeInLocation,
              by = c("Habitat" = "Forest_Type"),
              relationship = "many-to-many") %>%
    select(Species,
           Category = IUCN_Red_List,
           Habitat,
           Location)
}

# ---------------------------------------------------------------------------- #

hasA <- function(location, forest){
  any(forestTypeInLocation$Forest_Type == forest & 
          forestTypeInLocation$Location == location)
}

predictAffectedSpecies <- function(location, forest_type){
  if (hasA(location, forest_type)){
    
    cat("\n\nIf an environmental issue arises in", forest_type, "in", location,
        ", it is predicted that the following species will be affected:\n\n")
    
    speciesInLocation %>%
      filter(Location %in% location) %>%
      left_join(speciesHabitat, by = c("Species" = "Species")) %>%
      filter(Habitat == forest_type) %>%
      select(Species)
  } else {
    cat("\n\n", location, "has no", forest_type,".\n\n")
  }
}

# ---------------------------------------------------------------------------- #

changeSpecificToForestType <- function(forest_type){
  cat("If an environmental issue specifically targets", forest_type,
      "\nit is suggested that the following location enhance their conservation and research priorities\n\n")
  
  locations <- ForestInLocation[2:3] %>%
                  left_join(ForestType[1:2],
                            by = c("ForestType" = "Abbreviation")) %>%
                  left_join(Locations,
                            by = c("Location" = "Abbreviation")) %>%
                  filter(TypeName == forest_type) %>%
                  select(Location = Location.y) %>%
                  as.data.frame()
  
  print(locations)
  
  cat("\nThe following organizations should also act accordingly:\n\n")
  print(organizationData %>%
    filter(Location %in% locations$Location &
           Forest_Type == forest_type) %>%
    select(Organization))
  
  cat("\nIf no respective environmental practice were put on action, it is predicted that the following species will be at risk:\n\n")
  speciesInLocation %>%
    filter(Location %in% locations$Location) %>%
    select(Species)
}

# ---------------------------------------------------------------------------- #
# Predictive Functions

changeInForestBiodiversityByClimateChange(location = )
populationLostInForestBiodiversity(forest = )
identifyPriorityConservationForestLocation()
forestConservationPrioritiesRedList()
predictAffectedSpecies(location = , forest_type = )
changeSpecificToForestType(forest_type = )

# ---------------------------------------------------------------------------- #
