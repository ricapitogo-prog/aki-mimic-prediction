# ============================================================================
# export_app_data.R
# Run from aki-ards-prediction/ root (same place as generate_site_assets.R).
# Exports a lightweight JSON file for the interactive web app.
# ============================================================================

library(tidyverse)
library(jsonlite)

OUT <- "docs/assets"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# ── Load data ───────────────────────────────────────────────────────────────
cohort      <- readRDS("final_cohort_imputed_baseline.rds")
aki_staging <- readRDS("aki_staging_at_intubation_imputed.rds")
feat        <- readRDS("feature_matrix_preimputation.rds")

aki_cohort <- cohort %>%
  left_join(
    aki_staging %>% select(stay_id, aki_stage, baseline_creat,
                           max_creat_at_intubation),
    by = "stay_id"
  ) %>%
  left_join(
    feat %>% select(stay_id, ends_with("_mean")),
    by = "stay_id"
  ) %>%
  mutate(
    race = case_when(
      grepl("WHITE",    toupper(race)) ~ "White",
      grepl("BLACK",    toupper(race)) ~ "Black",
      grepl("HISPANIC", toupper(race)) ~ "Hispanic",
      grepl("ASIAN",    toupper(race)) ~ "Asian",
      TRUE                             ~ "Other/Unknown"
    ),
    aki_label = case_when(
      is.na(aki_stage) | aki_stage == 0 ~ "No AKI",
      aki_stage == 1 ~ "Stage 1",
      aki_stage == 2 ~ "Stage 2",
      aki_stage == 3 ~ "Stage 3",
      TRUE ~ "No AKI"
    ),
    gender = ifelse(gender == "F", "Female", "Male")
  )

# ── Select columns for the app (keep it small) ─────────────────────────────
app_cols <- c(
  "stay_id", "subject_id", "age", "gender", "race", "aki_label",
  "admission_type", "first_careunit",
  # Key labs
  "creatinine_mean", "albumin_mean", "lactate_mean", "sodium_mean",
  "potassium_mean", "bicarbonate_mean", "glucose_mean", "hemoglobin_mean",
  "platelet_mean", "wbc_mean", "bilirubin_mean", "calcium_mean",
  # Key vitals
  "heart_rate_mean", "sbp_noninvasive_mean", "map_noninvasive_mean",
  "resp_rate_obs_mean", "temp_f_mean", "spo2_mean",
  # Severity
  "pf_ratio", "bmi",
  "sofa_resp", "sofa_coag", "sofa_liver", "sofa_cardio", "sofa_neuro"
)

# Only keep columns that exist
app_cols <- intersect(app_cols, names(aki_cohort))
app_data <- aki_cohort %>% select(any_of(app_cols))

# ── Round numerics to save space ────────────────────────────────────────────
app_data <- app_data %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

# ── Compute summary statistics for the app ──────────────────────────────────
# Pre-aggregated data so the app doesn't need to crunch 10k rows in JS

# 1. AKI distribution
aki_dist <- app_data %>%
  count(aki_label, name = "n") %>%
  mutate(pct = round(n / sum(n) * 100, 1))

# 2. Demographics by AKI
demo_by_aki <- list(
  age = app_data %>%
    group_by(aki_label) %>%
    summarise(
      median = median(age, na.rm = TRUE),
      q25 = quantile(age, 0.25, na.rm = TRUE),
      q75 = quantile(age, 0.75, na.rm = TRUE),
      mean = round(mean(age, na.rm = TRUE), 1),
      .groups = "drop"
    ),
  gender = app_data %>%
    count(aki_label, gender) %>%
    group_by(aki_label) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    ungroup(),
  race = app_data %>%
    count(aki_label, race) %>%
    group_by(aki_label) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    ungroup()
)

# 3. Lab summaries by AKI stage
lab_cols <- names(app_data)[grepl("_mean$", names(app_data)) &
                             !grepl("heart_rate|sbp|map|resp|temp|spo2", names(app_data))]
lab_summary <- app_data %>%
  select(aki_label, all_of(lab_cols)) %>%
  pivot_longer(-aki_label, names_to = "lab", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(lab = str_to_title(str_remove(lab, "_mean"))) %>%
  group_by(aki_label, lab) %>%
  summarise(
    median = round(median(value), 2),
    q25 = round(quantile(value, 0.25), 2),
    q75 = round(quantile(value, 0.75), 2),
    n = n(),
    .groups = "drop"
  )

# 4. Vital summaries by AKI stage
vital_cols <- intersect(
  c("heart_rate_mean", "sbp_noninvasive_mean", "map_noninvasive_mean",
    "resp_rate_obs_mean", "temp_f_mean", "spo2_mean"),
  names(app_data)
)
vital_labels <- c(
  heart_rate_mean = "Heart Rate", sbp_noninvasive_mean = "Systolic BP",
  map_noninvasive_mean = "MAP", resp_rate_obs_mean = "Resp Rate",
  temp_f_mean = "Temperature", spo2_mean = "SpO2"
)
vital_summary <- app_data %>%
  select(aki_label, all_of(vital_cols)) %>%
  pivot_longer(-aki_label, names_to = "vital", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(vital = recode(vital, !!!vital_labels)) %>%
  group_by(aki_label, vital) %>%
  summarise(
    median = round(median(value), 2),
    q25 = round(quantile(value, 0.25), 2),
    q75 = round(quantile(value, 0.75), 2),
    n = n(),
    .groups = "drop"
  )

# 5. Overall stats
overall <- list(
  n = nrow(app_data),
  n_unique = n_distinct(app_data$subject_id),
  median_age = median(app_data$age, na.rm = TRUE),
  pct_female = round(mean(app_data$gender == "Female", na.rm = TRUE) * 100, 1),
  pct_aki = round(mean(app_data$aki_label != "No AKI", na.rm = TRUE) * 100, 1)
)

# ── Bundle everything ───────────────────────────────────────────────────────
export <- list(
  overall = overall,
  aki_distribution = aki_dist,
  demographics = demo_by_aki,
  labs = lab_summary,
  vitals = vital_summary,
  # Include individual-level data for scatter/histogram (sampled if too large)
  patients = app_data %>%
          select(any_of(c("age", "gender", "race", "aki_label",
                          "creatinine_mean", "lactate_mean", "albumin_mean",
                          "heart_rate_mean", "sbp_noninvasive_mean", "spo2_mean",
                          "pf_ratio", "bmi")))
)

# ── Write JSON ──────────────────────────────────────────────────────────────
json_out <- toJSON(export, pretty = FALSE, auto_unbox = TRUE, na = "null")
writeLines(json_out, file.path(OUT, "app_data.json"))

cat(sprintf("Exported app data: %s KB\n",
            round(file.size(file.path(OUT, "app_data.json")) / 1024)))
cat("File: docs/assets/app_data.json\n")
