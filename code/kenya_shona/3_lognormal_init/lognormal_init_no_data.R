library(tidyverse)
library(here)
library(cmdstanr)
library(readxl)
library(posterior)

# Prepare Shona data
load(here("data/shona2019_toronto.rdata"))

all_pop <- read_excel(here("data/wpp/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx"), skip = 16) %>%
  rename(
    region = `Region, subregion, country or area *`,
    period = `Reference date (as of 1 July)`
  ) %>%
  select(-Index, -Variant, -Notes, -`Country code`, -`Type`, -`Parent code`) %>% 
  pivot_longer(names_to = "age", values_to = "pop", cols = -c("region", "period")) %>%
  mutate(age = as.numeric(str_extract(age, ".+?(?=[-+])")), pop = as.numeric(pop)) 

# prepare fertility and mortality rates from WPP
all_fert <- read_excel(here("data/wpp/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx"), skip = 16) %>%
  rename(region = `Region, subregion, country or area *`, period = Period) %>% 
  select(-Index, -Variant, -Notes, -`Country code`, -`Parent code`, -`Type`) %>% 
  mutate(year = as.numeric(str_extract(period, ".+?(?=-)"))) %>% # match everything before hyphen or plus
  pivot_longer(names_to = "age", values_to = "fert", cols = -c("region", "period", "year")) %>% 
  mutate(age = as.numeric(str_extract(age, ".+?(?=-)")), fert = as.numeric(fert)/1000) %>%
  select(-period)

all_surv <- read_excel(here("data/wpp/WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.xlsx"), skip = 16) %>%
  transmute(
    region = `Region, subregion, country or area *`,
    age = `Age (x)`,
    period = Period,
    Lx = as.numeric(`Number of person-years lived L(x,n)`)
  ) %>%
  group_by(region, period) %>%
  mutate(surv = Lx / lag(Lx)) %>%
  mutate(year = as.numeric(str_extract(period, ".+?(?=-)")))  %>%
  group_by(age = ifelse(age == 1, 0, age)) %>%
  group_by(region, year, age) %>%
  summarise(Lx = sum(Lx)) %>%
  mutate(surv = lead(Lx) / Lx) %>%
  ungroup()

ZWE_init <- all_pop %>%
  filter(region == "Zimbabwe") %>%
  group_by(period) %>%
  mutate(pop_prop = pop/sum(pop)) %>%
  ungroup() %>%
  filter(period == min(period), age != max(age)) %>%
  pull(pop_prop)


kenya_surv <- all_surv %>%
  filter(region == "Kenya") %>%
  filter(!is.na(surv))

kenya_fert <- all_fert %>%
  filter(region == "Kenya") %>%
  right_join(select(kenya_surv, -surv, -Lx)) %>%
  arrange(region, year, age) %>%
  mutate(fert = ifelse(is.na(fert), 0, fert))

# create constants
n_ages <- length(unique(kenya_fert$age))
n_periods <- length(unique(kenya_fert$year))
n_obs <- 1 # number of population observations (min. 1 for model to work)
# note: set t_obs = as.array(0) below if no actual observations

# put rates into matrix format for stan
kenya_surv_mat <- matrix(kenya_surv$surv, nrow = n_periods, ncol = n_ages, byrow = TRUE) 
kenya_fert_mat <- matrix(kenya_fert$fert, nrow = n_periods, ncol = n_ages, byrow = TRUE) 

# some additional objects to help construct fertility matrix in Stan
fert_ages_bool <- apply(kenya_fert_mat, 2, function(x) any(x > 0))
ages_before_fert = which(fert_ages_bool)[1] - 1
ages_after_fert = n_ages - last(which(fert_ages_bool))

# create mappings between indices and actual values
age_mapping <- tibble(
  number = 1:(n_ages + 1),
  value = c(unique(kenya_fert$age), max(kenya_fert$age) + 5)
)

year_mapping <- data.frame(
  number = 1:n_periods,
  value = unique(kenya_fert$year)
)

# put data in list to pass to Stan
data_list <- list(
  K = n_ages,
  Time = n_periods,
  prop_f = 1/(1 + 1.05),
  pop_0_prop_data = ZWE_init,
  surv_mean = kenya_surv_mat,
  fert_mean = kenya_fert_mat[, fert_ages_bool],
  n_fert_ages = sum(fert_ages_bool),
  ages_before_fert = ages_before_fert,
  ages_after_fert = ages_after_fert,
  t_obs = as.array(0),
  n_obs = n_obs,
  pop_obs = matrix(0, nrow = 1, ncol = n_ages)
)

mod <- cmdstan_model(here("code/kenya_shona/3_lognormal_init/projection3.stan"))

fit <- mod$sample(
  data = data_list,
  seed = 12,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 3000,
  iter_sampling = 2000,
  max_treedepth = 20,
  adapt_delta = 0.995
)

# calculate summary statistics
q_probs <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)

summary_table <- fit$summary(
  variables = NULL,
  mean, sd, rhat, ~quantile2(.x, probs = q_probs, na.rm = TRUE), ess_bulk, ess_tail
)

# save results
to_save <- list(
  data = data_list,
  table = summary_table,
  mappings = list(age = age_mapping, year = year_mapping)
)

write_rds(to_save, here("output/intermediate/kenya_shona_projection3_no_data.rds"))

