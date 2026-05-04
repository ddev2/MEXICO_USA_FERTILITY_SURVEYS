#### BEFORE ####
path_MEXICO_ENADID <- paste0(rootPath, "/INEGI/Encuestas/ENADID/MEXICO_ENADID.Rdat")
load(file=path_MEXICO_ENADID)
wfs=subset(MEXICO_ENADID,survey=="WFS")

wfs <- wfs %>%
  mutate(woman_id = row_number()) %>%
  relocate(woman_id, .after = 2)

fert_data <- wfs %>%
  select(woman_id, indiv_dob_cmc, indiv_age_survey,matches("^(sex|dob_cmc)\\d+$")) %>%
  pivot_longer(
    cols = matches("^(sex|dob_cmc)\\d+$"),
    names_to = c(".value", "birth_order"),
    names_pattern = "(sex|dob_cmc)(\\d+)",
    values_drop_na = TRUE)  # We remove the NAs so that the order is clean.

fert_data <- fert_data %>%
  mutate(age_at_birth=floor((dob_cmc-indiv_dob_cmc) / 12)) %>%
  mutate(birth_order=as.integer(birth_order)) %>%
  rename(
    current_age = indiv_age_survey
  )

#### Adjust births for women under age 20 ####
library(dplyr)
library(tidyr)
library(ggplot2)


# --- 1. CALCULATE SECULAR TREND (Women 30-50) ---
# We want to see how fertility at young ages has dropped over time
trend_data <- fert_data %>%
  filter(current_age >= 40) %>%
  filter(age_at_birth < 20) %>%
  group_by(current_age) %>%
  summarise(avg_births_under_20 = n() / n_distinct(woman_id), .groups = 'drop')

trend_model <- lm(log(avg_births_under_20 + 0.001) ~ current_age, data = trend_data)
annual_decline <- 1 - exp(coef(trend_model)[2]) # Estimated annual drop

# Count unique women in the reference group
n_women_20_34 <- fert_data %>%
  filter(current_age >= 20 & current_age <= 34) %>%
  distinct(woman_id) %>%
  nrow()

# Count unique women in the problematic young group
n_women_under_20 <- fert_data %>%
  filter(current_age < 20) %>%
  distinct(woman_id) %>%
  nrow()

# --- 1. SET THE ANNUAL DECLINE ---
# Since your calculated -0.003 is very low, let's use a 
# standard 2% (0.02) to test the impact, or stick to your value.
#### critical value ####
adj_annual_decline <- 0.04 

# --- 2. PRE-CALCULATE TARGET RATES (Reference: 20-34 cohort) ---
target_rates <- fert_data %>%
  filter(current_age >= 20 & current_age <= 34) %>%
  filter(age_at_birth < 20) %>%
  group_by(age_at_birth, birth_order) %>%
  summarise(
    # Rate of this specific birth event per woman in the ref group
    target_rate = n() / n_women_20_34, 
    .groups = 'drop'
  )

# --- 3. PRE-CALCULATE OBSERVED RATES (Problematic < 20 group) ---
observed_rates_young <- fert_data %>%
  filter(current_age < 20) %>%
  group_by(age_at_birth, birth_order) %>%
  summarise(
    observed_rate = n() / n_women_under_20,
    .groups = 'drop'
  )

# --- 4. APPLY THE STOCHASTIC ADJUSTMENT ---
adjusted_young <- fert_data %>%
  filter(current_age < 20) %>%
  # Join both rates to the individual records
  left_join(target_rates, by = c("age_at_birth", "birth_order")) %>%
  left_join(observed_rates_young, by = c("age_at_birth", "birth_order")) %>%
  mutate(
    target_rate = replace_na(target_rate, 0),
    observed_rate = replace_na(observed_rate, 0),
    
    # Delta_t: Gap between the young woman and the midpoint of the ref group (27)
    delta_t = 27 - current_age,
    
    # Calculate the Adjusted Target (the 'Goal' frequency)
    adj_target_rate = target_rate * (1 - adj_annual_decline)^delta_t,
    
    # THE PROBABILITY OF KEEPING THE RECORD:
    # If the goal is 0.02 and we have 0.06, keep_prob is 0.33 (33% kept, 67% deleted)
    keep_prob = ifelse(observed_rate > 0, 
                       pmin(1, adj_target_rate / observed_rate), 
                       0)
  ) %>%
  ungroup() %>%
  # Randomly filter rows based on the calculated probability
  filter(runif(n()) <= keep_prob)

# --- 4.5 RE-SEQUENCE BIRTH ORDER ---
adjusted_young <- adjusted_young %>%
  # Crucial: Sort by age_at_birth first to ensure chronological order
  arrange(woman_id, age_at_birth) %>%
  group_by(woman_id) %>%
  mutate(
    # re-calculate birth_order based on remaining rows
    birth_order = row_number() 
  ) %>%
  ungroup()

# --- 5. FINAL ASSEMBLY ---
final_dataset <- bind_rows(
  fert_data %>% filter(current_age >= 20),
  adjusted_young %>% select(woman_id, indiv_dob_cmc, current_age, sex, dob_cmc, age_at_birth, birth_order)
)
# --- 7. VISUAL VERIFICATION ---
# Let's see the distribution of Age at First Birth before and after
comparison <- bind_rows(
  fert_data %>% filter(current_age < 20, birth_order == 1) %>% mutate(type = "Original <20"),
  adjusted_young %>% filter(birth_order == 1) %>% mutate(type = "Adjusted <20")
)

ggplot(comparison, aes(x = age_at_birth, fill = type)) +
  geom_histogram(binwidth = 1, position = "dodge") + 
  theme_minimal() +
  labs(title = "Effect of Adjustment on Age at First Birth")

comparison <- bind_rows(
  fert_data %>% filter(current_age < 20, birth_order == 2) %>% mutate(type = "Original <20"),
  adjusted_young %>% filter(birth_order == 2) %>% mutate(type = "Adjusted <20")
)

ggplot(comparison, aes(x = age_at_birth, fill = type)) +
  geom_histogram(binwidth = 1, position = "dodge") + 
  theme_minimal() +
  labs(title = "Effect of Adjustment on Age at Second Birth")

#### AFTER ####
# 1. Calculate the total count of children for each woman
final_dataset <- final_dataset %>%
  group_by(woman_id) %>%
  mutate(nChildrenAdj = n()) %>%
  ungroup() %>%
  # Position nChildren just after woman_id (2nd column)
  relocate(nChildrenAdj, .after = woman_id)

# 2. Pivot to wide format while keeping nChildren
fert_data_mod <- final_dataset %>%
  # We keep nChildren and woman_id, but remove current_age and age_at_birth for the pivot
  dplyr::select(woman_id, nChildrenAdj, birth_order, dob_cmc, sex) %>%  
  pivot_wider(
    id_cols = c(woman_id, nChildrenAdj), # Keep nChildren as an ID column so it isn't pivoted
    names_from = birth_order,
    values_from = c(dob_cmc,sex),
    names_glue = "{.value}{birth_order}"
  )

# 3. Join the pivoted data back to the original dataset
# remove the original dob_cmc columns before the join to avoid duplication
wfs <- wfs %>%
  select(-matches("(sex|dob_cmc)\\d+")) %>%  # Remove original dob_cmc columns
  left_join(fert_data_mod, by="woman_id")  # Join the pivoted data back to the original dataset

wfs$nChildrenAdj[is.na(wfs$nChildrenAdj)] <- 0

wfs <- relocate(wfs,nChildrenAdj,.after=nBioKids)
wfs <- relocate(wfs,indiv_age_survey,.after=yBirth)

wfs <- wfs %>%
  select(where(~ !all(is.na(.))))

#### optional: remove "sex" variables ####
if (FALSE) {
  wfs <- select(wfs,-matches("(sex)\\d+"))
}

#### optional: substitute in MEXICO_ENADID
if (FALSE) {
  MEXICO_ENADID<-subset(MEXICO_ENADID,survey!="WFS")
  MEXICO_ENADID <- wfs %>%
    #select(any_of(names(MEXICO_ENADID))) %>%  # Select only columns present in MEXICO_ENADID
    bind_rows(MEXICO_ENADID, .)
}