library(tidyverse)
library(here)
library(cmdstanr)
library(posterior)

survey <- read_rds(here("data/unhcr/civ/survey2019_processed.rds"))

df_obs <- survey %>%
  filter(!is.na(procedure_success) | civ_process_success | naturalisation_success) |> 
  rowwise() |> 
  mutate(any_success = any(
    procedure_success, 
    civ_process_success, 
    naturalisation_success,
    na.rm = TRUE
  )
  )

# Dictionaries to map back and forth between numeric indices and labels
birthplace_dict <- tibble(
  number = 1:n_distinct(survey$birth_country),
  value = sort(unique(survey$birth_country))
)

region_dict <- tibble(
  number = 1:n_distinct(survey$region),
  value = sort(unique(survey$region))
)  

risk_level_dict <- tibble(
  number = 1:n_distinct(survey$risk_level),
  value = sort(unique(survey$risk_level))
)

# Map data to numeric indices for success/failure model
risk_levels_y <- plyr::mapvalues(
  df_obs$risk_level,
  from = risk_level_dict$value,
  to = risk_level_dict$number
) 

risk_levels_binary_y <- risk_levels_y %>%
  replace(which(risk_levels_y == "3"), "2")

birthplaces_y <- plyr::mapvalues(
  df_obs$birth_country,
  from = birthplace_dict$value,
  to = birthplace_dict$number
) 

regions_y <- plyr::mapvalues(
  df_obs$region,
  from = region_dict$value,
  to = region_dict$number
)

# Prepare data for risk level model
risk_df <- survey %>%
  count(birth_country, region, risk_level) %>%
  pivot_wider(
    id_cols = c("birth_country", "region"), 
    names_from = "risk_level", 
    values_from = "n", 
    values_fill = 0
  ) 

# convert to matrix for Stan
r_data <- risk_df %>%
  select("high", "low", "none") %>%
  as.matrix()

# map to numbers
birthplaces_r <- plyr::mapvalues(
  risk_df$birth_country,
  from = birthplace_dict$value,
  to = birthplace_dict$number
) 

regions_r <- plyr::mapvalues(
  risk_df$region,
  from = region_dict$value,
  to = region_dict$number
) 

# Prepare data for full set of predictions 
pred_df <- survey %>%
  group_by(birth_country, region) %>%
  summarize(n = sum(wt)) %>%
  ungroup() %>%
  complete(birth_country, region, fill = list(n=0)) %>%
  arrange(region, birth_country)

bp_pred <- plyr::mapvalues(
  pred_df$birth_country,
  from = birthplace_dict$value,
  to = birthplace_dict$number
) 

a_pred <- plyr::mapvalues(
  pred_df$region,
  from = region_dict$value,
  to = region_dict$number
) 

data_list <- list(
  
  # For successes model 
  K_r = nrow(risk_level_dict), 
  K_bp = nrow(birthplace_dict),
  K_a = nrow(region_dict),
  
  N_y = nrow(df_obs),
  y = 1 - 1*df_obs$any_success,
  
  r_y = risk_levels_y,
  r_binary_y = risk_levels_binary_y,
  bp_y = birthplaces_y,
  X_bp = model.matrix(~0 + birth_country, data = df_obs %>% mutate(birth_country = factor(birth_country, levels = birthplace_dict$value))),
  a_y = regions_y, 
  
  scale_b_0 = 10,
  
  # For risk level model 
  N_r = nrow(risk_df),
  r = r_data,
  
  bp_r = birthplaces_r,
  a_r = regions_r,
  
  scale_u_0 = 10,
  
  # For prediction
  N_pred = nrow(pred_df),
  bp_pred = bp_pred,
  a_pred = a_pred,
  pop_pred = pred_df$n
)

# Run Stan via cmdstanr
file <- here("script/civ/civ.stan")

mod <- cmdstan_model(file, pedantic = TRUE)

time_to_fit <- system.time(
  fit <- mod$sample(
    data = data_list,
    seed = 2220,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000,
    
    max_treedepth = 15
  )
)


q_probs <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)

# Calculate summary statistics for parameters
summary_table <- fit$summary(
  variables = NULL,
  mean, sd, rhat, ~quantile2(.x, probs = q_probs), ess_bulk, ess_tail
)

# Save results
to_save <- list(
  time_to_fit = time_to_fit,
  summary_table = summary_table,
  data = data_list,
  region_dict = region_dict,
  birthplace_dict = birthplace_dict,
  pred_df = pred_df,
  draws_location = fit$output_files(),
  diagnostic_summary = fit$diagnostic_summary()
)

write_rds(to_save, here("output/intermediate/civ.rds"))



