library(tidyverse)
library(here)

load(here("data/unhcr/civ/civ2019_statelessness_igse.rdata"))

weights <- dat$POND

df <- dat %>%
  rename(
#    ZD,
#    CODE_LOCALITE,
    sex = QA3,
    age = AGECONTROL, # there's also Q8_AGE, but most of those are NA
    resident = QA6, # whether the individual is a resident, absent, or visitor
#    QA9A, # whether born in CIV -- to be recoded below
    birthplace = QA9B, # if born in CIV, specify further -- if not, specify birth country
    year_entry = QA10, # year of first entry if not born in CIV
    declared_etat_civil = QB1, # unsure of translation here
    declared_etat_civil_doc = QB2, 
    other_residence = QB10, # country of other residence (NA = none),
    nationality = QB32, #  self-reported nationality
 #   QB33A, # nationality doc -- to be recoded below 
    began_procedure = QB33AC, # whether they began a procedure to obtain a nationality
    procedure_success = QB33AE # whether the procedure was successful
  ) %>%
  mutate(
    born_in_civ = QA9A == 1, # to match "Né en Côte d'Ivoire" without accent trouble
    has_nationality_doc = !(QB33A == 1)
  ) %>%
  select(-QB33A, -QA9A) %>% 
  mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
  mutate(birth_country = ifelse(
    born_in_civ,
    "COTE D'IVOIRE", 
    as.character(birthplace))) %>%
  mutate(wt = weights) %>%
  mutate(began_procedure = case_when(
    began_procedure == "Oui" ~ TRUE,
    began_procedure == "Non" ~ FALSE,
    began_procedure == "Ne sait pas" ~ NA)
  ) %>%
  mutate(procedure_success = case_when(
    procedure_success == "OUI" ~ TRUE,
    procedure_success == "NON" ~ FALSE
  )) %>%
  mutate(began_civ_process = case_when(
    QB34 == "Oui" ~ TRUE,
    QB34 == "Non" ~ FALSE,
    TRUE ~ NA
  )) |>
  mutate(civ_process_success = case_when(
    QB36 == "Oui" ~ TRUE,
    QB36 == "Non" ~ FALSE,
    TRUE ~ NA
  )) |>
  mutate(began_naturalisation = case_when(
    QB38 == "Oui" ~ TRUE,
    QB38 == "Non" ~ FALSE,
    TRUE ~ NA
  )) |>
  mutate(naturalisation_success = case_when(
    QB40 == "Oui" ~ TRUE,
    QB40 == "Non" ~ FALSE,
    TRUE ~ NA
  )) |>
  mutate(declared_etat_civil = case_when(
    declared_etat_civil == "Oui" ~ TRUE,
    declared_etat_civil %in% c("Non", "Ne sait pas") ~ FALSE
  )) %>%
  mutate(
    parent_docs = case_when(
      QB17A == "Oui" | QB26A == "Oui" ~ TRUE,
      QB17A == "Non" & QB26A == "Non" ~ FALSE
  )) %>%
  mutate(father_known_alive  = case_when(
    QB14 == "Oui" & QB15 == "Oui" ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(mother_known_alive = case_when(
    QB23 == "Oui" & QB24 == "Oui" ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(parent_known_alive = mother_known_alive | father_known_alive) %>%
  mutate(jus_soli = birth_country %in% c("LIBERIA", "CHAD")) %>%
  # Code up the flow diagram
  mutate(risk_level = case_when(
    has_nationality_doc ~ "none",
    born_in_civ & declared_etat_civil & parent_docs ~ "none",
    born_in_civ & declared_etat_civil & !parent_docs & parent_known_alive ~ "low",
    born_in_civ & declared_etat_civil & !parent_docs & !parent_known_alive ~ "high",
    born_in_civ & !declared_etat_civil & parent_docs & parent_known_alive ~ "none",
    born_in_civ & !declared_etat_civil & parent_docs & !parent_known_alive ~ "low",
    born_in_civ & !declared_etat_civil & !parent_docs & parent_known_alive ~ "low",
    born_in_civ & !declared_etat_civil & !parent_docs & !parent_known_alive ~ "high",
    !born_in_civ & !declared_etat_civil & parent_docs & parent_known_alive ~ "none",
    !born_in_civ & !declared_etat_civil & parent_docs & !parent_known_alive ~ "low",
    !born_in_civ & !declared_etat_civil & !parent_docs & parent_known_alive ~ "low",
    !born_in_civ & !declared_etat_civil & !parent_docs & !parent_known_alive ~ "high",
    !born_in_civ & declared_etat_civil & jus_soli ~ "none",
    !born_in_civ & declared_etat_civil & !jus_soli & parent_docs ~ "none",
    !born_in_civ & declared_etat_civil & !jus_soli & !parent_docs & parent_known_alive ~ "low",
    !born_in_civ & declared_etat_civil & !jus_soli & !parent_docs & !parent_known_alive ~ "high"
  ))

df <- df %>%
  select(
    wt,
    zone = ZD, 
    locality = CODE_LOCALITE,
    region = REGION,
    sex,
    age, 
    resident,
    other_residence,
    birth_country, 
    year_entry, 
    nationality, 
    began_procedure,
    procedure_success,
    began_civ_process,
    civ_process_success,
    began_naturalisation,
    naturalisation_success,
    risk_level
  )

write_rds(df, here("data/unhcr/civ/survey2019_processed.rds"))


