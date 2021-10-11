library(tidyverse)
library(censusapi)
library(tidycensus)

# metadata: https://api.census.gov/data/2019/acs/acs1/pums/variables.html
# < 2016: https://api.census.gov/data/2015/acs/acs1/pumspr/variables.html

rep_weights <- paste0("PWGTP", 1:80)

years <- as.character(2018:2019)


# Gets the  public microdata for non-US citizens only

for(i in 1:length(years)){
  print(years[i])
  lang_var <- ifelse(as.numeric(years[i])<2016, "HHL", "HHLANP")
  if(years[i]>2007){
    df <- getCensus(
      name = "acs/acs1/pums/",
      vintage = years[i],
      vars = c("PWGTP", #weight
               rep_weights,
               "AGEP", # age
               #"ENG", # ability to speak english
               "NATIVITY", # nativity
               "NOP", # nativity of parents
               "SEX", #sex
               "ANC1P", # ancestory
               lang_var, ## HH language
               "YOEP", ## year of entry
               "POBP"), # place of birth
      region = "state:*", 
      CIT="2,3,5")
  }
  else{
    df <- getCensus(
      name = "acs/acs1/pums/",
      vintage = years[i],
      vars = c("PWGTP", #weight
               rep_weights,
               "AGEP", # age
               #"ENG", # ability to speak english
               "NATIVITY", # nativity
               #"NOP", # nativity of parents
               "SEX", #sex
               "ANC1P", # ancestory
               lang_var, ## HH language
               "YOEP", ## year of entry
               "POBP"), # place of birth
      region = "state:*", 
      CIT="2,3,5")
  }
  
  
  
  
  write_rds(df, paste0("data/acs/acs1_", years[i], ".RDS"))
  
  
  
}
