library("tidyverse")
library("broom")
library("interplot")
library("conflicted")
library("stargazer")
library("ggthemes")
library("bestNormalize")

# Prefer select and filter from dplyr
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

# Set theme
theme_set(theme_bw())

gbp <- read_csv("data_gbp.csv")

gbp %>%
  select(beh_stayhome, beh_socgathering, beh_distance) %>%
  psych::alpha()

median(gbp$RestrictionIndex)
quantile(gbp$RestrictionIndex, c(.05, .5, .95)) 
quantile(gbp$othersbelief, c(.05, .5, .95)) 

# Get number of observations
NROW(gbp)
# Get number of countries
NROW(unique(gbp$country))
# Print countries
unique(gbp$country)

# Find best normalization strategy
findBestNormalize <- function(x){
  norm_data <- gbp %>% filter(country == x)
  
  attr(bestNormalize(norm_data$outcome_social, warn = FALSE, out_of_sample = FALSE, allow_orderNorm = FALSE)$chosen_transform, "class")
}

df_normalize <- data.frame(country = unique(gbp$country), test = NA)

for (i in df_normalize$country) {
  df_normalize$test[df_normalize$country == i] <- findBestNormalize(i)
}

# yeojohnson is the best normalization (in 37 out of 39 samples)
table(df_normalize$test)

data_normalize <- bestNormalize(gbp$outcome_social, warn = FALSE, out_of_sample = FALSE, allow_orderNorm = FALSE)
gbp$outcome_social <- data_normalize$x.t

corr_data <- gbp %>% 
  group_by(country) %>%
  summarise(
    n = n(),
    trait_O = cor(outcome_social, trait_O, use = "pairwise.complete.obs"),
    trait_C = cor(outcome_social, trait_C, use = "pairwise.complete.obs"),
    trait_E = cor(outcome_social, trait_E, use = "pairwise.complete.obs"),
    trait_A = cor(outcome_social, trait_A, use = "pairwise.complete.obs"),
    trait_N = cor(outcome_social, trait_N, use = "pairwise.complete.obs"),
    .groups = "drop"
  ) %>% 
  pivot_longer(starts_with("trait_")) %>% 
  mutate(name = case_when(
    name == "trait_O" ~ "Openness", 
    name == "trait_C" ~ "Conscientiousness", 
    name == "trait_E" ~ "Extraversion",
    name == "trait_A" ~  "Agreeableness", 
    name == "trait_N" ~ "Neuroticism"
  ))

corr_data %>% 
  group_by(name) %>% 
  summarise(value = median(abs(value)))

corr_data %>% 
  mutate(value = abs(value)) %>% 
  arrange(desc(value)) %>% 
  print(n=30)

get_descriptives <- function(x) {
  gbp %>% 
    filter(country == !!x) %>% 
    select(beh_stayhome, beh_socgathering, beh_distance, outcome_social, starts_with("trait_"), othersbelief, male, age, educ, inc, marital_status) %>% 
    data.frame() %>%  
    stargazer(type = "text",
              digits = 1,
              covariate.labels = c(
                "Stayed home", "Not attend gatherings", "Kept distance", "All outcomes", 
                "Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", 
                "Others beliefs", "Male", "Age", "Education", "Income", "Marital status"
              ),
              title = paste0("Country: ", x),
              # Create country/ folder before running
              out = paste0("country/", x, ".htm"))
}

map(unique(gbp$country), get_descriptives)

gbp <- gbp %>% 
  mutate_at(vars(outcome_social,
                 beh_stayhome,
                 beh_socgathering,
                 beh_distance), ~ 
              (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE))


# Get list of unique countries
unique(gbp$country)
NROW(unique(gbp$country))

cor(gbp$outcome_social, gbp$trait_O)
cor(gbp$outcome_social, gbp$trait_C)
cor(gbp$outcome_social, gbp$trait_E)
cor(gbp$outcome_social, gbp$trait_A)
cor(gbp$outcome_social, gbp$trait_N)

summary(gbp$othersbelief)
summary(gbp$outcome_social)


gbp$lan <- gbp$language


reg_both <- lm(outcome_social ~ factor(country) + factor(date) + othersbelief + trait_O + trait_C + trait_E + trait_A + trait_N + trait_O*othersbelief + trait_C*othersbelief + trait_E*othersbelief + trait_A*othersbelief + trait_N*othersbelief +  RestrictionIndex*trait_O + trait_C*RestrictionIndex + trait_E*RestrictionIndex + trait_A*RestrictionIndex + trait_N*RestrictionIndex + male + age + educ + inc + marital_status, data = gbp)
reg_othersbelief <- lm(outcome_social ~ factor(country) + factor(date) + othersbelief + trait_O + trait_C + trait_E + trait_A + trait_N + trait_O*othersbelief + trait_C*othersbelief + trait_E*othersbelief + trait_A*othersbelief + trait_N*othersbelief + RestrictionIndex + male + age + educ + inc + marital_status, data = gbp)
reg_RestrictionIndex <- lm(outcome_social ~ factor(country) + factor(date) + othersbelief + trait_O + trait_C + trait_E + trait_A + trait_N + RestrictionIndex*trait_O + trait_C*RestrictionIndex + trait_E*RestrictionIndex + trait_A*RestrictionIndex + trait_N*RestrictionIndex + male + age + educ + inc + marital_status, data = gbp)

stargazer(reg_othersbelief, reg_both,
          keep = c("male", "age", "educ", "inc", "marital_status", "trait_O", "trait_C", "trait_E", "trait_A", "trait_N", "othersbelief", "RestrictionIndex"),
          digits = 2,
          covariate.labels = c("Others beliefs", "Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", "Restriction Index", "Male", "Age", "Education", "Income", "Marital status",
                               "Openness * Others beliefs", "Conscientiousness * Others beliefs", "Extraversion * Others beliefs", "Agreeableness * Others beliefs", "Neuroticism * Others beliefs",
                               "Openness * Restriction Index", "Conscientiousness * Restriction Index", "Extraversion * Restriction Index", "Agreeableness * Restriction Index", "Neuroticism * Restriction Index"),
          keep.stat = c("n", "rsq"),
          single.row = TRUE,
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p < .1; * p < .05; ** p < .01; *** p < .001"), 
          notes.append = FALSE,
          out = "table1.htm",
          type = "text")


reg_othersbelief <- lm(outcome_social ~ factor(country) + factor(date) + othersbelief + trait_O*othersbelief + trait_C*othersbelief + trait_E*othersbelief + trait_A*othersbelief + trait_N*othersbelief + male + age + educ + inc + marital_status + RestrictionIndex, data = gbp) 

reg_marg_othersbelief_O <- interplot(m = reg_othersbelief, var1 = "trait_O", var2 = "othersbelief", plot=FALSE) %>% 
  mutate(trait = "Openness")
reg_marg_othersbelief_C <- interplot(m = reg_othersbelief, var1 = "trait_C", var2 = "othersbelief", plot=FALSE) %>% 
  mutate(trait = "Conscientiousness")
reg_marg_othersbelief_E <- interplot(m = reg_othersbelief, var1 = "trait_E", var2 = "othersbelief", plot=FALSE) %>% 
  mutate(trait = "Extraversion")
reg_marg_othersbelief_A <- interplot(m = reg_othersbelief, var1 = "trait_A", var2 = "othersbelief", plot=FALSE) %>% 
  mutate(trait = "Agreeableness")
reg_marg_othersbelief_N <- interplot(m = reg_othersbelief, var1 = "trait_N", var2 = "othersbelief", plot=FALSE) %>% 
  mutate(trait = "Neuroticism")

reg_marg_othersbelief <- bind_rows(
  reg_marg_othersbelief_O,
  reg_marg_othersbelief_C,
  reg_marg_othersbelief_E,
  reg_marg_othersbelief_A,
  reg_marg_othersbelief_N
) %>% 
  mutate(trait = fct_relevel(trait, c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism"))) 


ggplot(reg_marg_othersbelief, aes(x = othersbelief, group = trait, colour = trait, fill = trait)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_line(aes(y = coef)) +
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.2, linetype = 2) +
  scale_colour_tableau() +
  scale_fill_tableau() +
  facet_wrap(~ trait, ncol = 5) +
  theme(legend.position = "none") +
  labs(y = "Marginal effect of trait\non social distancing",
       x = "Perceived national attitudes on social distancing")

ggsave("figure1.png", width = 8, height = 3)
ggsave("figure1.tiff", width = 8, height = 3)
ggsave("figure1.pdf", width = 8, height = 3)


reg_both_std <- lm(outcome_social ~ factor(country) + factor(date) + scale(othersbelief) + scale(trait_O) + scale(trait_C) + scale(trait_E) + scale(trait_A) + scale(trait_N) + scale(trait_O)*scale(othersbelief) + scale(trait_C)*scale(othersbelief) + scale(trait_E)*scale(othersbelief) + scale(trait_A)*scale(othersbelief) + scale(trait_N)*scale(othersbelief) + scale(RestrictionIndex)*scale(trait_O) + scale(trait_C)*scale(RestrictionIndex) + scale(trait_E)*scale(RestrictionIndex) + scale(trait_A)*scale(RestrictionIndex) + scale(trait_N)*scale(RestrictionIndex) + scale(male) + scale(age) + scale(educ) + scale(inc) + scale(marital_status), data = gbp)
reg_othersbelief_std <- lm(outcome_social ~ factor(country) + factor(date) + scale(othersbelief) + scale(trait_O) + scale(trait_C) + scale(trait_E) + scale(trait_A) + scale(trait_N) + scale(trait_O)*scale(othersbelief) + scale(trait_C)*scale(othersbelief) + scale(trait_E)*scale(othersbelief) + scale(trait_A)*scale(othersbelief) + scale(trait_N)*scale(othersbelief) + scale(RestrictionIndex) + scale(male) + scale(age) + scale(educ) + scale(inc) + scale(marital_status), data = gbp)

stargazer(reg_othersbelief_std, reg_both_std,
          keep = c("male", "age", "educ", "inc", "marital_status", "trait_O", "trait_C", "trait_E", "trait_A", "trait_N", "othersbelief", "RestrictionIndex"),
          digits = 2,
          covariate.labels = c("Others beliefs", "Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", "Restriction Index", "Male", "Age", "Education", "Income", "Marital status",
                               "Openness * Others beliefs", "Conscientiousness * Others beliefs", "Extraversion * Others beliefs", "Agreeableness * Others beliefs", "Neuroticism * Others beliefs",
                               "Openness * Restriction Index", "Conscientiousness * Restriction Index", "Extraversion * Restriction Index", "Agreeableness * Restriction Index", "Neuroticism * Restriction Index"),
          keep.stat = c("n", "rsq"),
          single.row = TRUE,
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p < .1; * p < .05; ** p < .01; *** p < .001"), 
          notes.append = FALSE,
          out = "table_std.htm",
          type = "text")


covid_formula <- "factor(country) + factor(lan) + factor(date) + trait_O + trait_C + trait_E + trait_A + trait_N + male + age + educ + inc + marital_status"

reg_all <- lm(as.formula(paste0("outcome_social ~", covid_formula)), data = gbp)
reg_stayhome <- lm(as.formula(paste0("beh_stayhome ~", covid_formula)), data = gbp)
reg_socgathering <- lm(as.formula(paste0("beh_socgathering ~", covid_formula)), data = gbp)
reg_distance <- lm(as.formula(paste0("beh_distance ~", covid_formula)), data = gbp)

stargazer(reg_stayhome, reg_socgathering, reg_distance, reg_all,
          keep = c("male", "age", "educ", "inc", "marital_status", "trait_O", "trait_C", "trait_E", "trait_A", "trait_N"),
          omit = "language",
          digits = 2,
          covariate.labels = c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", "Male", "Age", "Education", "Income", "Marital status"),
          keep.stat = c("n", "rsq"),
          dep.var.labels = c("Stayed home (A)", "Not attend gatherings (B)", "Kept distance (C)", "All (A+B+C)"),
          out = "tableD1.htm",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p .05; ** p < .01; *** p < .001"), 
          notes.append = FALSE,
          type = "text")

