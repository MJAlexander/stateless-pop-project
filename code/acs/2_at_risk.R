## Extract at-risk populations from ACS

library(tidyverse)
library(janitor)

years <- 2005:2019

for(i in 1:length(years)){
  
  year <- years[i]
  print(year)
  # read in and clean up
  
  d <- read_rds(paste0("data/acs/acs1_", year,".RDS"))
  if(("nop" %in% colnames(d))==FALSE){
    d$nop <- NA
  }
  
  d <- d %>% 
    clean_names() %>% 
    mutate(nativity = ifelse(nativity ==1, "native", "foreign born")) %>% 
    mutate(citizen = ifelse(cit == 2, "born in territories", 
                            ifelse(cit==3, "born abroad US parents", "non-citizen"))) %>% 
  mutate(nativity_parents = case_when(
    nop==0 ~ "NA",
    nop ==1 ~ "Living with two parents: Both parents NATIVE",
    nop == 5 ~ "Living with father only: Father NATIVE",
    nop == 8 ~ "Living with mother only: Mother FOREIGN BORN",
    nop == 7 ~ "Living with mother only: Mother NATIVE",
    nop == 3 ~ "Living with two parents: Mother only FOREIGN BORN",
    nop == 4 ~ "Living with two parents: BOTH parents FOREIGN BORN",
    nop == 6 ~ "Living with father only: Father FOREIGN BORN",
    nop == 2 ~ "Living with two parents: Father only FOREIGN BORN",
    TRUE ~ "NA"
  )) 
  #mutate(speaks_only_english = ifelse(eng == 0, 1, 0)) %>% 
  #mutate(english_only_hh = ifelse(hhl==1, 1, 0))
  
  d <- d %>% 
    mutate(yob = year - as.numeric(agep))
  
  d <- d %>% filter(nativity == "foreign born")
  
  # Extract populations -----------------------------------------------------
  
  # empty tibble to save
  ar_all <- tibble()
  
  ############ Former soviet countries arrived before 1992
  # no tajkistan, turkmenistan, estonia
  
  ar_all <- bind_rows(ar_all, d %>% 
                        filter(pobp %in% c(156, 165, 160, 162, 164, 158, 159, 161, 218, 246, 157)) %>% 
                        filter(yoep<1992) %>% 
                        mutate(group = "Former Soviet Union") %>% 
                        mutate(region = "Europe and Eurasia"))
  
  
  # Estonia (No data)
  
  # Latvia: ancestory russia, belarus, polish, born before 1991
  ar_all <- bind_rows(ar_all, d %>% 
                        filter(pobp == 156) %>% 
                        filter(anc1p==148|anc1p==102|anc1p==142)%>% 
                        mutate(group = "Ethnic Russians, Belarussians, and Poles
from Latvia")%>% 
                        mutate(region = "Europe and Eurasia"))
  
  # Lithuania:
  
  ar_all <- bind_rows(ar_all, d %>% 
                        filter(pobp==157) %>% 
                        filter(anc1p==148|anc1p==102|anc1p==142)%>% 
                        mutate(group = "Members of Ethnic Minority Groups from
Lithuania")%>% 
                        mutate(region = "Europe and Eurasia"))
  
  # Ethnic Armenians from Azerbaijan and Georgia 
  
  ar_all <- bind_rows(ar_all,d %>% 
                        filter((pobp==159& anc1p==431)|(pobp==161 & anc1p==431))%>% 
                        mutate(group = "Ethnic Armenians from Azerbaijan and Georgia") %>% 
                        mutate(region = "Europe and Eurasia"))
  
  # ethnic Azerbaijanis from Georgia 
  # no azerbaijan ethnicity
  
  # Meskhetian Turks born in georgia, russia, USSR, Uzbekistan
  
  # ar_all <- bind_rows(ar_all,d %>% 
  #   filter(pobp %in% c(161, 163, 165, 246)) %>% 
  #   filter(hhlanp==1675) )
  
  # OR
  
  ar_all <- bind_rows(ar_all, d %>%
                        filter(pobp %in% c(161, 163, 165, 246)) %>%
                        filter(anc1p==434)%>% 
                        mutate(group = "Meskhetian Turks born in Georgia, Russia, USSR, Uzbekistan") %>% 
                        mutate(region = "Europe and Eurasia"))
  
  ### Yugoslavia
  
  # Roma
  
  ar_all <- bind_rows(ar_all, d %>% filter(anc1p==124) %>% 
                        filter(pobp %in% c(100, 151, 154, 168, 152, 147, 150)) %>% 
                        mutate(group = "Roma")%>% 
                        mutate(region = "Europe and Eurasia"))
  
  # Balkan egyptians
  
  ar_all <- bind_rows(ar_all,d %>% filter(anc1p==402) %>% 
                        filter(pobp %in% c(100, 154, 168, 152, 147))%>% 
                        mutate(group = "Balkan Egyptians")%>% 
                        mutate(region = "Europe and Eurasia"))
  
  # Yugoslavian passports
  
  ar_all <- bind_rows(ar_all,d %>% 
                        filter(anc1p== 176) %>% 
                        filter(yoep<1992) %>% 
                        mutate(group = "Yugoslavian passports")%>% 
                        mutate(region = "Europe and Eurasia"))
  
  # Born in North Macedonia, Other Ex-Yugoslav Descent
  
  ar_all <- bind_rows(ar_all,d %>% 
                        filter(pobp==152) %>% 
                        filter(anc1p %in% c(109, 154, 177, 152, 131)) %>% 
                        mutate(group = "Born in North Macedonia, Other Ex-Yugoslav Descent")%>% 
                        mutate(region = "Europe and Eurasia"))
  
  # Born in Croatia, Serbian Descent
  
  ar_all <- bind_rows(ar_all,d %>% 
                        filter(pobp==151) %>% 
                        filter(anc1p==152) %>% 
                        mutate(group = "Born in Croatia, Serbian Descent")%>% 
                        mutate(region = "Europe and Eurasia"))
  
  # roma in italy and germany
  
  ar_all <- bind_rows(ar_all,d %>% filter(anc1p==124) %>% 
                        filter(pobp %in% c(110, 120)) %>% 
                        mutate(group = "Roma Born in Italy and Germany")%>% 
                        mutate(region = "Europe and Eurasia"))
  
  
  ## Middle east and north africa
  
  # Syrian Refugee Children Born Abroad
  
  ar_all <- bind_rows(ar_all, d %>% 
                        filter(anc1p==429) %>% 
                        filter(pobp %in% c(110, 216, 224, 136, 243, 134, 139, 119, 140)) %>% 
                        mutate(group = "Syrian Refugee Children Born Abroad") %>% 
                        mutate(region = "Middle East and North Africa"))
  
  # Feyli Kurds from Iraq
  
  ar_all <- bind_rows(ar_all,d %>% filter(pobp==213) %>% 
                        filter(anc1p==442) %>% 
                        filter(yoep>1980) %>% 
                        mutate(group = "Feyli Kurds from Iraq")%>% 
                        mutate(region = "Middle East and North Africa")) 
  
  # Syrian Kurds
  
  ar_all <- bind_rows(ar_all,d %>% filter(pobp==239) %>% 
                        filter(anc1p==442) %>% 
                        mutate(group = "Syrian Kurds")%>% 
                        mutate(region = "Middle East and North Africa")) 
  
  # Lebanese Kurds and Bedouin (can't identify)
  
  ar_all <- bind_rows(ar_all, d %>% filter(pobp==224) %>% 
                        filter(anc1p==442)%>% 
                        mutate(group = "Lebanese Kurds")%>% 
                        mutate(region = "Middle East and North Africa") ) 
  
  # Palestinians
  
  ar_all <- bind_rows(ar_all,d %>% 
                        filter(anc1p == 465) %>% 
                        filter(pobp %in% c(216, 224, 239, 214, 222, 235, 245, 414, 248, 254)) %>% 
                        mutate(group = "Palestinians")%>% 
                        mutate(region = "Middle East and North Africa"))
  
  # Bidoon 
  # Iraqi and Saudi descent in Kuwait
  
  ar_all <- bind_rows(ar_all,d %>% filter(pobp==222) %>% 
                        filter(anc1p==427|anc1p==417) %>% 
                        mutate(group ="Iraqi and Saudi descent in Kuwait" )%>% 
                        mutate(region = "Middle East and North Africa")) 
  
  ## Asia and South Pacific
  
  # Nepal born after 1990
  
  ar_all <- bind_rows(ar_all, d %>% filter(pobp==229) %>% 
                        filter(yob>1990) %>% 
                        mutate(group = "Nepal born after 1990")%>% 
                        mutate(region = "Asia and South Pacific"))
  
  # Nepalis born in Bhutan
  
  ar_all <- bind_rows(ar_all, d %>% 
                        filter(anc1p==609) %>% 
                        filter(pobp==203) %>% 
                        mutate(group = "Nepalis born in Bhutan")%>% 
                        mutate(region = "Asia and South Pacific"))
  
  # Nothing on Rohingya
  
  # Other Minorities from Myanmar
  
  ar_all <- bind_rows(ar_all, d %>% filter(pobp==205) %>% 
                        filter(anc1p %in% c(918, 706, 609)) %>% 
                        mutate(group = "Other Minorities from Myanmar")%>% 
                        mutate(region = "Asia and South Pacific"))
  
  # Hmong from Laos
  
  ar_all <- bind_rows(ar_all,d %>% 
                        filter(anc1p==768) %>% 
                        filter(pobp==223) %>% 
                        mutate(group = "Hmong from Laos")%>% 
                        mutate(region = "Asia and South Pacific"))
  
  # Hmong from Thailand
  
  ar_all <- bind_rows(ar_all,d %>% 
                        filter(anc1p==768) %>% 
                        filter(pobp==242) %>% 
                        mutate(group = "Hmong from Thailand")%>% 
                        mutate(region = "Asia and South Pacific"))
  
  # Thai-Born Children of Burmese Refugees
  
  ar_all <- bind_rows(ar_all,d %>% 
                        filter(anc1p==700)%>% 
                        filter(pobp==242)%>% 
                        filter(yoep>1982) %>% 
                        mutate(group = "Thai-Born Children of Burmese Refugees")%>% 
                        mutate(region = "Asia and South Pacific"))
  
  # Tibetans
  
  ar_all <- bind_rows(ar_all,d %>% 
                        filter(anc1p==714) %>% 
                        filter(pobp %in% c(207, 210, 229)) %>% 
                        mutate(group = "Tibetans")%>% 
                        mutate(region = "Asia and South Pacific"))
  
  # Stateless Groups from India
  
  # Indian-born persons with Sri Lankan Tamil Origin
  
  ar_all <- bind_rows(ar_all,d %>% 
                        filter(pobp==210) %>% 
                        filter(anc1p == 690) %>% 
                        mutate(group = "Indian-born persons with Sri Lankan Tamil Origin")%>% 
                        mutate(region = "Asia and South Pacific"))
  
  # Indian-Born Persons of Bhutanese Origin, including Lhotshampas
  
  ar_all <- bind_rows(ar_all, d %>% 
                        filter(pobp==210) %>% 
                        filter(anc1p == 607) %>% 
                        mutate(group = "Indian-Born Persons of Bhutanese Origin")%>% 
                        mutate(region = "Asia and South Pacific"))
  
  # Bengalis from Pakistan
  
  ar_all <- bind_rows(ar_all,d %>% 
                        filter(pobp==231) %>% 
                        filter(anc1p == 618) %>% 
                        mutate(group = "Bengalis from Pakistan")%>% 
                        mutate(region = "Asia and South Pacific"))
  
  # Malaysian ethnic minorities
  ar_all <- bind_rows(ar_all,d %>% 
                        filter(pobp==226) %>% 
                        filter(anc1p %in% c(918, 730, 720)) %>% 
                        mutate(group = "Malaysian ethnic minorities")%>% 
                        mutate(region = "Asia and South Pacific"))
  
  # Ethnic Vietnamese and Khmer Krom from Cambodia  
  
  ar_all <- bind_rows(ar_all,d %>% 
                        filter(pobp==206) %>% 
                        filter(anc1p == 785) %>% 
                        mutate(group = "Ethnic Vietnamese and Khmer Krom from Cambodia")%>% 
                        mutate(region = "Asia and South Pacific"))
  
  # SSA
  
  # KENYA
  
  # Somalian ancestry, including Galjeel
  
  ar_all <- bind_rows(ar_all,d %>% 
                        filter(pobp==427) %>% 
                        filter(anc1p==568) %>% 
                        mutate(group = "Kenya, Somalian ancestry, including Galjeel")%>% 
                        mutate(region = "Sub-Saharan Africa"))
  
  # Shona speakers
  if(year>=2016){
    ar_all <- bind_rows(ar_all,d %>%
                          filter(pobp==427) %>%
                          filter(hhlanp==5525) %>%
                          mutate(group = "Shona")%>% 
                          mutate(region = "Sub-Saharan Africa"))
  }
  
  
  # Ethiopians with Eritrean Ancestry
  
  ar_all <- bind_rows(ar_all,d %>% 
                        filter(pobp==416) %>% 
                        filter(anc1p==523)%>% 
                        filter(yoep>=1998) %>% 
                        mutate(group = "Ethiopians with Eritrean Ancestry")%>% 
                        mutate(region = "Sub-Saharan Africa")) 
  
  # Eritreans with Ethiopian Ancestry
  
  ar_all <- bind_rows(ar_all,d %>% 
                        filter(pobp==417) %>% 
                        filter(anc1p==522)%>% 
                        filter(yoep>=1998) %>% 
                        mutate(group = "Eritreans with Ethiopian Ancestry")%>% 
                        mutate(region = "Sub-Saharan Africa")) 
  
  # Banyarwanda and Banyamulenge from the Democratic Republic of Congo
  
  # Americas
  
  # Dominicans (Dominican Republic) of Haitian Ancestry
  
  ar_all <- bind_rows(ar_all,d %>% 
                        filter(pobp==329) %>% 
                        filter(anc1p==336) %>% 
                        mutate(group = "Dominicans (Dominican Republic) of Haitian Ancestry")%>% 
                        mutate(region = "Americas"))
  
  # Bahamians of Haitian Ancestry
  
  ar_all <- bind_rows(ar_all,d %>% 
                        filter(pobp==323) %>% 
                        filter(anc1p==336) %>% 
                        mutate(group = "Bahamians of Haitian Ancestry")%>% 
                        mutate(region = "Americas"))
  
  write_rds(ar_all, paste0("data/acs/acs1_atrisk_", year, ".RDS"))
  
  
  
}

