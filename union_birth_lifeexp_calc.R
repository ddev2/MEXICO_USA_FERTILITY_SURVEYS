setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("union_birth_life_expectancy.R")

loadENADID_data()
pooled <- dplyr::bind_rows(MEXICO_ENADID, NSFG_ENADID)
pooled <- filterDateQuality(pooled, dropAllBad=TRUE)          # drop all bad dates

res14 <- state_life_expectancy(pooled, groupVars = c("country"))
res7  <- state_life_expectancy_7state(res14)   # cohab vs married kept
res5  <- state_life_expectancy_5state(res14)   # original headline states
print(res5)

# ---- Part B: all women, by birth cohort, age-specific counts ----
by_age    <- state_counts_by_age(pooled, groupVars = "country", cohortWidth = 5)
by_age_5  <- state_counts_by_age_5state(by_age)

dd_expected <- by_age_5 |>
  group_by(group, cohort, state) |>
  summarise(expected_years = sum(prop_weighted, na.rm = TRUE) / 12, .groups = "drop")


dd_expected <- subset(dd_expected,cohort %in% c("1945-1949","1950-1954","1955-1959","1960-1964","1965-1969","1970-1974","1975-1979"))

FIVE_STATE_LABELS <- c("out_union", "union1", "sep1", "union2plus", "sep2plus")
STATE_LABELS_PRETTY <- c(
  out_union  = "Out of union",
  union1     = "First union",
  sep1       = "Separated (first union)",
  union2plus = "Second+ union",
  sep2plus   = "Separated (second+ union)"
)

dd_expected$state <- factor(dd_expected$state, levels = FIVE_STATE_LABELS)

pExp <- ggplot(dd_expected, aes(x = cohort, y = expected_years, fill = state)) +
  geom_col(position = position_stack(reverse = TRUE), width = 1) +
  facet_wrap(~ group) +
  scale_fill_brewer(palette = "Set2",
                    labels = STATE_LABELS_PRETTY[levels(dd_expected$state)]) +
  labs(x = "Birth cohort", y = "Expected years (ages 15-40)", fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
