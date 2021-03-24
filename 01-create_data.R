library("tidyverse")
library("haven")
library("stargazer")
library("conflicted")
library("lubridate")
library("DescTools")
library("labelled")
library("ggthemes")

# Prefer select and filter from dplyr
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")


# Load data - can be found at: https://osf.io/3sn2k/files/
gbp_raw <- read_dta("GlobalBehaviorsPerceptions_Data_May21_2020.dta") 


# Set theme
theme_set(theme_bw())

# Number of countries and median number of participants in full sample 
NROW(gbp_raw)
NROW(unique(gbp_raw$country))

# Recode variables
gbp_full <- gbp_raw %>% 
  
  # Recode variables
  mutate(extra1 = personality_b5_1,
         agree1 = 8 - personality_b5_2,
         consci1 = personality_b5_3,
         neuro1 =  personality_b5_4,
         open1 = personality_b5_5,
         extra2 = 8 - personality_b5_6,
         agree2 = personality_b5_7,
         consci2 = 8 - personality_b5_8,
         neuro2 = 8 - personality_b5_9,
         open2 = 8 - personality_b5_10,
         
         inc = log(Winsorize(income, na.rm = TRUE)),
         lan = UserLanguage,
         
         othersbelief = (sob_social + sob_handshake + sob_stores + sob_curfew)/4
         
         ) %>% 
  
  # Create data variable
  mutate(date = ymd(paste(year, "-", month, "_", day))) %>% 
  
  # Recode DV and Big Five traits
  mutate(
    outcome_social = (beh_stayhome + beh_socgathering + beh_distance)/3,
    
    trait_O = open1 + open2,
    trait_C = consci1 + consci2,
    trait_E = extra1 + extra2,
    trait_A = agree1 + agree2,
    trait_N = neuro1 + neuro2) %>% 
  
  # Get labels for country and language
  mutate(country = as.character(to_factor(country)),
         language = as.character(to_factor(lan))) %>%
  
  # Fix country labels
  mutate(country = case_when(
    country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
    country == "Russian Federation" ~ "Russia",
    country == "United States of America" ~ "United States",
    country == "Venezuela, Bolivarian Republic of..." ~ "Venezuela",
    country == "Viet Nam" ~ "Vietnam",
    TRUE ~ country
  )) %>% 
  
  # Gather Spanish language respondents in one category
  mutate(language = ifelse(language == "ES-ES", "ES", language)) %>% 
  
  # Remove participants with missing values on gender
  filter(gender != 3) %>% 
  
  # Create male variable
  mutate(male = ifelse(gender == 1, 1, 0))


# Have at least 250 participants within each country-language group
gbp_250 <- gbp_full %>% 
  group_by(country, language) %>% 
  add_count(n = n(), name = "cl_obs") %>% 
  filter(cl_obs > 250) %>% 
  ungroup() %>% 
  select(-StringencyIndex)

# Get item-item correlations
gbp_itemlan <- gbp_250 %>% 
  group_by(country, language) %>% 
  summarise(
    Participants = n(),
    Openness = cor(open1, open2, use = "pairwise.complete.obs"),
    Conscientiousness = cor(consci1, consci2, use = "pairwise.complete.obs"),
    Extraversion = cor(extra1, extra2, use = "pairwise.complete.obs"),
    Agreeableness = cor(agree1, agree2, use = "pairwise.complete.obs"),
    Neuroticism = cor(neuro1, neuro2, use = "pairwise.complete.obs"),
    .groups = "drop") %>% 
  drop_na(.) %>% 
  mutate(exclude = case_when(
    Openness < 0.05 ~ 1,
    Conscientiousness < 0.05 ~ 1,
    Extraversion < 0.05 ~ 1,
    Agreeableness < 0.05 ~ 1,
    Neuroticism < 0.05 ~ 1,
    TRUE ~ 0
  ))

# Import stringency data from: https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker#data
# CSV file available at: https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv
s_data_raw <- read_csv("OxCGRT_latest.csv") %>% 
  select(country = CountryName, date = Date, RestrictionIndex = StringencyIndex) %>% 
  drop_na(RestrictionIndex) %>% 
  mutate(date = ymd(date),
         country = ifelse(country == "Slovak Republic", "Slovakia", country)) %>% 
  filter(country %in% unique(gbp_250$country)) %>% 
  filter(date %in% unique(gbp_250$date)) %>% 
  distinct(country, date, .keep_all = "true")


gbp_all <- left_join(gbp_250, s_data_raw, by = c("country", "date"))

# Merge individual-level data with item-item correlations data
gbp <- left_join(gbp_all, gbp_itemlan, by = c("country", "language")) %>% 
  filter(exclude == 0) %>% 
  select(-exclude) %>% 
  group_by(country) %>% 
  mutate(O_mean = mean(trait_O),
         C_mean = mean(trait_C),
         E_mean = mean(trait_E),
         A_mean = mean(trait_A),
         N_mean = mean(trait_N),
         O_sd = sd(trait_O),
         C_sd = sd(trait_C),
         E_sd = sd(trait_E),
         A_sd = sd(trait_A),
         N_sd = sd(trait_N)) %>% 
  ungroup() %>% 
  mutate(trait_O = (trait_O - O_mean) / O_sd,
         trait_C = (trait_C - C_mean) / C_sd,
         trait_E = (trait_E - E_mean) / E_sd,
         trait_A = (trait_A - A_mean) / A_sd,
         trait_N = (trait_N - N_mean) / N_sd) %>% 
  mutate(RestrictionIndex = ((RestrictionIndex - min(RestrictionIndex, na.rm = TRUE))/ (max(RestrictionIndex, na.rm = TRUE) - min(RestrictionIndex, na.rm = TRUE)))*100 )


gbp %>% 
  group_by(country, language) %>% 
  summarise(
    Participants = n(),
    Openness = cor(open1, open2, use = "pairwise.complete.obs"),
    Conscientiousness = cor(consci1, consci2, use = "pairwise.complete.obs"),
    Extraversion = cor(extra1, extra2, use = "pairwise.complete.obs"),
    Agreeableness = cor(agree1, agree2, use = "pairwise.complete.obs"),
    Neuroticism = cor(neuro1, neuro2, use = "pairwise.complete.obs"),
    .groups = "drop") %>% 
  pivot_longer(-c("country", "language", "Participants"), values_to = "Correlation", names_to = "Trait") %>% 
  ggplot(aes(x = country, y = Correlation, colour = language)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  geom_point(size = 2.5, alpha = .5) + 
  facet_wrap(~ Trait, ncol = 5, scales = "free_x") +
  coord_flip() +
  scale_colour_pander() +
  labs(y = "Item-item correlation",
       x = NULL,
       colour = "Language") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow = 2))

ggsave("fig-itemitem.png", width = 7, height = 7)


# See correlations for all items (full sample)
gbp %>% 
  select(extra1:open2) %>% 
  cor(use="pairwise.complete.obs") %>%
  as.data.frame() %>% 
  `rownames<-`(
    c("(1) Extroverted, enthusiastic",
      "(2) Critical, quarrelsome",
      "(3) Dependable, self-disciplined",
      "(4) Anxious, easily upset",
      "(5) Open to new experiences, complex",
      "(6) Reserved, quiet",
      "(7) Sympathetic, warm",
      "(8) Disorganized, careless",
      "(9) Calm, emotionally stable",
      "(10) Conventional, uncreative")
  ) %>% 
  as.matrix() %>% 
  stargazer(type = "text", digits = 2, covariate.labels = c("", paste0("(", 1:10, ")")),
            out = "big5-correlations.htm")


gbp %>% 
  select(country, language, date, beh_stayhome, beh_socgathering, beh_distance, 
         RestrictionIndex, othersbelief, starts_with("trait_"), male, age, 
         educ, inc, marital_status, outcome_social) %>% 
  write_csv("data_gbp.csv")

