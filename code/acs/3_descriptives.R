#### Descriptive charts of at risk populations based on ACS data in USA

library(tidycensus)
library(srvyr)
library(ggrepel)

years <- 2005:2019


df <- tibble()

for(i in 1:length(years)){
  d <- read_rds(paste0("data/acs/acs1_atrisk_", years[i], ".RDS"))
  d$year <- years[i]
  df <- bind_rows(df, d)
}

colnames(df)[str_detect(colnames(df), "pwgt")] <- str_to_upper(colnames(df)[str_detect(colnames(df), "pwgt")])
df <- df %>% mutate(PWGTP = as.numeric(PWGTP))

df %>% 
  group_by(year) %>%
  tally(wt = as.numeric(PWGTP)) 

df$age_group <- cut(as.numeric(df$agep),
                                 breaks= c(seq(0, 85, by = 5), Inf),
                                 labels = seq(0, 85, by = 5),
                                 right = FALSE)
df$age_group <- as.numeric(as.character(df$age_group))

df %>% 
  group_by(age_group, sex, year) %>% 
  tally(wt = as.numeric(PWGTP)) %>% 
  ggplot(aes(age_group, n, fill = sex)) + 
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~year)



df_survey <- to_survey(df)

df_survey %>% 
  survey_count(vartype = "ci")

df_survey %>% 
  filter(year %in% c(2005, 2010, 2019)) %>% 
  survey_count(group, year, vartype = "ci") %>% 
  mutate(group = fct_reorder(group, n)) %>% 
  ggplot(aes(group, n, fill = group)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = n_low, ymax = n_upp)) + 
  coord_flip() + 
  theme(legend.position = "none")+
  facet_wrap(~year)

df_survey %>% 
  survey_count(age_group, sex, year) %>% 
  ggplot(aes(age_group, n, fill = sex)) + 
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = n - n_se, ymax = n + n_se), position = "dodge")+
  facet_wrap(~year)

df_survey %>% 
  survey_count(group, year, vartype = "ci") %>% 
  #mutate(group = fct_reorder(group, n)) %>% 
  ggplot(aes(year, n, color = group)) + geom_line()+
  theme(legend.position = "none") +
  facet_wrap(~group, scales = "free_y") +
  geom_ribbon(aes(ymin = n_low, ymax = n_upp), alpha = 0.2)

df_survey %>% 
  survey_count(group, region, year, vartype = "ci") %>%
  rowwise() %>% 
  mutate(n_low = max(0, n_low)) %>% 
  mutate(label = ifelse(year==2019, group, NA)) %>% 
  ggplot(aes(year, n)) + geom_line(aes(color = group))+
  theme(legend.position = "none") +
  facet_wrap(~region, scales = "free_y") +
  geom_ribbon(aes(ymin = n_low, ymax = n_upp, fill = group), alpha = 0.2)+
  geom_label_repel(aes(label = label),
                   size = 2,
                   nudge_x = 1,
                   na.rm = TRUE, max.overlaps = 8)
  

dt <- df_survey %>% 
  survey_count(group, region, year) %>% 
  filter(year==2019)

regions <- unique(df$region)
for(i in 1:length(regions)){
  p <- dt %>%
    filter(region == regions[i]) %>% 
    ggplot(aes(group, n, fill = group)) + 
    geom_bar(stat = "identity") + 
    facet_wrap(~region, scales = "free") + 
    theme(legend.position = "none")+
    geom_errorbar(aes(ymin = n - n_se, ymax = n + n_se))+
    coord_flip()
  print(p)
}

df_survey %>% 
  filter(group == "Nepal born after 1990") %>% 
  survey_count(year, age_group)  %>% 
  ggplot(aes(year, n, color = factor(age_group))) + geom_line()


df_survey %>% 
  filter(group == "Nepal born after 1990") %>% 
  survey_count(year, age_group) %>% 
  group_by(year) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(age_group, n, color = year, group= year)) + 
  geom_point()+
  geom_line() + 
  scale_color_viridis_c()

df_survey %>% 
  filter(group == "Former Soviet Union") %>% 
  survey_count(year, age_group) %>% 
  group_by(year) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(age_group, n, color = year, group= year)) + 
  #geom_point()+
  #geom_line() + 
  scale_color_viridis_c()+
  geom_smooth(se = FALSE)

df_survey %>% 
  filter(group == "Former Soviet Union") %>% 
  survey_count(year, age_group)  %>% 
  ggplot(aes(year, n, color = factor(age_group))) + geom_line()

df_survey %>% 
  survey_count(region, age_group, year) %>% 
  filter(year %in% c(2005, 2010, 2019)) %>% 
  ggplot(aes(age_group, n, fill = factor(year))) + geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~region, scales = "free_y")
 


