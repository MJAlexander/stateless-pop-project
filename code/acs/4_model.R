### Modeling ACS data over time, and incorporating probability of statelessness


library(tidycensus)
library(srvyr)
library(ggrepel)
library(rstan)
library(tidybayes)
library(tidyverse)

years <- 2005:2019


df <- tibble()

for(i in 1:length(years)){
  d <- read_rds(paste0("data/acs/acs1_atrisk_", years[i], ".RDS"))
  d$year <- years[i]
  df <- bind_rows(df, d)
}

colnames(df)[str_detect(colnames(df), "pwgt")] <- str_to_upper(colnames(df)[str_detect(colnames(df), "pwgt")])
df <- df %>% mutate(PWGTP = as.numeric(PWGTP))


df$age_group <- cut(as.numeric(df$agep),
                    breaks= c(seq(0, 85, by = 5), Inf),
                    labels = seq(0, 85, by = 5),
                    right = FALSE)
df$age_group <- as.numeric(as.character(df$age_group))

df <- df %>% 
  filter(group!="Nepal born after 1990"|(group=="Nepal born after 1990"&(nativity_parents=="NA"|nativity_parents=="Living with mother only: Mother FOREIGN BORN")))

df_survey <- to_survey(df)

group_counts <- df_survey %>% 
  survey_count(group, year) %>% 
  filter(group!="Roma"&group!="Shona"&group!="Balkan Egyptians") 

y <- group_counts %>% 
  select(-n_se) %>% 
  pivot_wider(names_from = "group", values_from = "n") %>%
  select(-year) %>% 
  replace(is.na(.), 0.1) %>% 
  as.matrix()

se_y <- group_counts %>% 
  select(-n) %>% 
  pivot_wider(names_from = "group", values_from = "n_se") %>%
  select(-year) %>% 
  replace(is.na(.), 1) %>% 
  as.matrix()

groups <- colnames(y)

stan_data <- list(y = log(y), 
                  se_y = ifelse(y>0, se_y/y,1),
                  P = 5,
                 N = nrow(y), S = ncol(y))

mod <- stan(data = stan_data, file = "code/models/1_rw2.stan")

yhat <- mod %>% 
  gather_draws(lambda[i,s]) %>% 
  mutate(fit =exp(.value)) %>% 
  median_qi() %>% 
  mutate(year = years[i], group = groups[s])

proj <- mod %>% 
  gather_draws(y_p[i,s]) %>% 
  mutate(proj = exp(.value)) %>% 
  median_qi(.width = 0.5) %>% 
  mutate(year = 2019+i, group = groups[s])


selected <- c("Hmong from Thailand", 
              "Dominicans (Dominican Republic) of Haitian Ancestry",
              "Iraqi and Saudi descent in Kuwait",
              "Nepal born after 1990",
              "Syrian Kurds",
              "Former Soviet Union")
group_counts %>% 
  filter(group %in% selected) %>% 
  #mutate(group = ifelse(group=="Dominicans (Dominican Republic) of Haitian Ancestry", "Dominicans (Dominican Republic) \n of Haitian Ancestry", group)) %>% 
  ggplot(aes(year, n)) + geom_point() + 
  facet_wrap(~group, scales = "free_y") + 
  geom_line(data = yhat %>%     filter(group %in% selected) , aes(year, fit))+
  geom_line(data = proj%>% 
              filter(year<2022) %>%     filter(group %in% selected) , aes(year, proj), color = 2) +
  geom_ribbon(data = yhat %>%     filter(group %in% selected) , aes(year, y = fit, ymin = exp(.value.lower), ymax = exp(.value.upper)), alpha = 0.2)+
  geom_ribbon(data = proj%>% 
                filter(year<2022) %>%     filter(group %in% selected) , aes(year, y = proj, ymin = exp(.value.lower), ymax = exp(.value.upper)), alpha = 0.2, fill = 2)+
  ggtitle("Estimated and projected at-risk stateless profiles")
ggsave("notes/rw2_examples.pdf", width = 8, height = 4)


fits <- extract(mod, "lambda")
sum_fits <- apply(exp(fits[[1]]), 1:2, sum)
apply(sum_fits, 2, median)
apply(sum_fits, 2, quantile, 0.1)
apply(sum_fits, 2, quantile, 0.9)

total_fit <- tibble(year = years, estimate = apply(sum_fits, 2, median),
                    lower = apply(sum_fits, 2, quantile, 0.025), upper = apply(sum_fits, 2, quantile, 0.975))


projs <- extract(mod, "y_p")
sum_projs <- apply(projs[[1]], 1:2, sum)
total_proj <- tibble(year = 2020:2024, estimate = apply(sum_projs, 2, median),
                    lower = apply(sum_projs, 2, quantile, 0.025),
                    upper = apply(sum_projs, 2, quantile, 0.975))


total_fit %>% 
  #filter(year<2018) %>% 
  ggplot(aes(year, estimate)) + geom_line() +
  geom_point(data = group_counts %>% group_by(year) %>% summarize(n = sum(n)) , aes(year,n))  +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + 
  geom_line(data = total_proj %>% filter(year<2022), aes(year, estimate), col = 2)

### totals

total_counts <- df_survey %>% 
  survey_count(year) 

y <- total_counts %>% 
  select(-n_se) %>% 
  select(-year) %>% 
  replace(is.na(.), 0.1) %>% 
  as.matrix()

se_y <- total_counts %>% 
  select(-n) %>% 
  select(-year) %>% 
  replace(is.na(.), 1) %>% 
  as.matrix()

groups <- colnames(y)

stan_data <- list(y = log(y), 
                  se_y = ifelse(y>0, se_y/y,1),
                  P = 5,
                  N = nrow(y), S = ncol(y))

mod2 <- stan(data = stan_data, file = "code/models/1_rw2.stan")

yhat2 <- mod2 %>% 
  gather_draws(lambda[i,s]) %>% 
  mutate(fit =exp(.value)) %>% 
  median_qi() %>% 
  mutate(year = years[i])

proj2 <- mod2 %>% 
  gather_draws(y_p[i,s]) %>% 
  mutate(proj = exp(.value)) %>% 
  median_qi(.width = 0.5) %>% 
  mutate(year = 2019+i)

total_counts %>% 
  ggplot(aes(year, n)) + geom_point() + 
  geom_line(data = yhat2, aes(year, fit))+
  geom_line(data = proj2%>% 
              filter(year<2023), aes(year, proj), color = 2) +
  geom_ribbon(data = yhat2, aes(year, y = fit, ymin = exp(.value.lower), ymax = exp(.value.upper)), alpha = 0.2)+
  geom_ribbon(data = proj2%>% 
                filter(year<2023), aes(year, y = proj, ymin = exp(.value.lower), ymax = exp(.value.upper)), alpha = 0.2, fill = 2) + 
  ggtitle("Estimated and projected populations at risk of statelessness")
ggsave("notes/total_rw2.pdf", width = 6, height = 4)

#############

### Incorporating probability of statelessness (example: Nepal)

df %>% 
  filter(group=="Nepal born after 1990") %>% 
  group_by(yoep) %>% 
  tally() %>% 
  ggplot(aes(yoep, n)) + geom_bar(stat = "identity")


df_nepal %>% select(year, year_diff, prob) %>% 
  group_by(year, year_diff, prob) %>% 
  tally()

df_nepal_survey <- to_survey(df_nepal)

df_nepal_summary <- df_nepal_survey %>% 
  select(year, year_diff, prob) %>% 
  survey_count(year, year_diff, prob) %>% 
  mutate(prob_stateless = 1- prob,
         n_stateless = n*prob_stateless,
         se_y = n_se) %>% 
  mutate(var_binomial = n_stateless*prob*(1-prob)) %>% 
  mutate(mu_comb = (var_binomial*n_stateless+se_y^2*n_stateless)/(var_binomial+se_y^2),
         se_comb = sqrt(1/(1/var_binomial+1/se_y^2))) %>% 
  group_by(year) %>% 
  summarize(n = sum(n_stateless), se_y = sqrt(sum(se_y^2)))

stan_data <- list(y = log(df_nepal_summary$n), 
                  se_y = ifelse(df_nepal_summary$n>0, df_nepal_summary$se_y/df_nepal_summary$n,1),
                  N = length(years),
                  P = 5)

mod <- stan(data = stan_data, file = "code/models/2_rw2_prob.stan")

yhat <- mod %>% 
  gather_draws(eps[i]) %>% 
  mutate(fit =exp(.value)) %>% 
  median_qi() %>% 
  mutate(year = years[i])

proj <- mod %>% 
  gather_draws(y_p[i]) %>% 
  mutate(proj = exp(.value)) %>% 
  median_qi(.width = 0.5) %>% 
  mutate(year = 2019+i)

group_counts %>% 
  filter(group=="Nepal born after 1990") %>% 
  ggplot(aes(year, n)) + geom_point() + 
  #facet_wrap(~group, scales = "free_y") + 
  geom_line(data = yhat, aes(year, fit))+
  geom_line(data = proj%>% 
              filter(year<2022), aes(year, proj), color = 2) +
  geom_ribbon(data = yhat, aes(year, y = fit, ymin = exp(.value.lower), ymax = exp(.value.upper)), alpha = 0.2)+
  geom_ribbon(data = proj%>% 
                filter(year<2022), 
              aes(year, y = proj, ymin = exp(.value.lower), ymax = exp(.value.upper)), alpha = 0.2, fill = 2)+
  ggtitle("Probability-adjusted estimates for Nepal")
ggsave("notes/nepal_prob.pdf", width = 6, height = 4)
