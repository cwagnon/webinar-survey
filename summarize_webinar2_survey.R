library(tidyverse)
library(janitor)
library(stringr)

# =========================================================
# Webinar 2 survey summary
# =========================================================
# This script:
#   1) reads the survey csv
#   2) cleans column names and whitespace
#   3) creates bar charts for binary / two-level responses
#   4) exports tables for participant names and open-ended responses
#   5) writes a brief response summary table
#
# Update the file path below if needed.
# =========================================================

input_file <- "raw-data/RGSMOT_Responses.csv"
out_dir <- "survey_summary_outputs"
data <- read_csv("raw-data/RGSMOT_Responses.csv")

dir.create(out_dir, showWarnings = FALSE)

# ---------- read + clean ----------
survey <- read_csv(
  input_file,
  locale = locale(encoding = "ISO-8859-1"),
  show_col_types = FALSE
) |>
  clean_names() |>
  mutate(across(everything(), ~ ifelse(is.character(.x), str_squish(.x), .x))) |>
  mutate(across(everything(), ~ na_if(.x, ""))) |>
  mutate(across(everything(), ~ na_if(.x, "nan")))

# optional: inspect names
print(names(survey))

# ---------- participant table ----------
participants <- survey |>
  transmute(
    first_name = str_trim(first_name),
    last_name  = str_trim(last_name),
    full_name  = str_trim(str_c(first_name, last_name, sep = " "))
  )

write_csv(participants, file.path(out_dir, "participant_names.csv"))

# ---------- identify open-ended columns ----------
# edit this vector if you want to add/remove text-response fields
open_ended_cols <- c(
  "reasoning",
  "concerns",
  "tmdl_feedback"
)

# export each open-ended question as its own table with names attached
for (col in open_ended_cols) {
  if (col %in% names(survey)) {
    tmp <- survey |>
      transmute(
        respondent = str_trim(str_c(first_name, last_name, sep = " ")),
        response = .data[[col]]
      ) |>
      filter(!is.na(response))

    write_csv(tmp, file.path(out_dir, paste0(col, "_table.csv")))
  }
}

# one combined long table of all open-ended responses
open_ended_long <- survey |>
  mutate(respondent = str_trim(str_c(first_name, last_name, sep = " "))) |>
  select(respondent, all_of(open_ended_cols)) |>
  pivot_longer(
    cols = all_of(open_ended_cols),
    names_to = "question",
    values_to = "response"
  ) |>
  filter(!is.na(response))

write_csv(open_ended_long, file.path(out_dir, "open_ended_responses_all.csv"))

# ---------- bar charts for binary / two-level questions ----------
# exclude participant and open-ended columns
exclude_cols <- c("first_name", "last_name", open_ended_cols)

candidate_cols <- setdiff(names(survey), exclude_cols)

# keep only columns with exactly 2 non-missing unique values
binary_cols <- candidate_cols[sapply(candidate_cols, function(x) {
  vals <- unique(na.omit(survey[[x]]))
  length(vals) == 2
})]

print(binary_cols)

# nicer labels for plots
pretty_labels <- c(
  combined_objective = "Should ecological function and cultural flows be combined?",
  tmdl_proxy = "Is TMDL a reasonable proxy?",
  tmdl_duplicative = "Are TMDL and river drying duplicative?",
  tmdl_drying = "Which metric should be retained?"
)

for (col in binary_cols) {
  plot_df <- survey |>
    count(response = .data[[col]], name = "n") |>
    mutate(percent = 100 * n / sum(n),
           label = paste0(n, " (", round(percent, 1), "%)"))

  p <- ggplot(plot_df, aes(x = response, y = n)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = label), vjust = -0.4, size = 4) +
    labs(
      title = ifelse(col %in% names(pretty_labels), pretty_labels[[col]], col),
      x = NULL,
      y = "Number of responses"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 15, hjust = 1)
    ) +
    expand_limits(y = max(plot_df$n) * 1.15)

  ggsave(
    filename = file.path(out_dir, paste0(col, "_bar_chart.png")),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}

# ---------- summary table for all closed-ended questions ----------
closed_question_summary <- survey |>
  select(all_of(candidate_cols)) |>
  pivot_longer(
    cols = everything(),
    names_to = "question",
    values_to = "response"
  ) |>
  filter(!is.na(response)) |>
  count(question, response, sort = FALSE) |>
  group_by(question) |>
  mutate(percent = round(100 * n / sum(n), 1)) |>
  ungroup()

write_csv(closed_question_summary, file.path(out_dir, "closed_question_summary.csv"))

# ---------- optional quick print to console ----------
cat("\nParticipant count:", nrow(participants), "\n")
cat("Outputs written to:", out_dir, "\n")

print(closed_question_summary)
