# ============================================================
# Webinar 2 Survey Summary Script (Report-Ready)
# ------------------------------------------------------------
# Purpose:
#   - Read and clean the Webinar 2 survey CSV
#   - Create polished bar charts for binary/two-option questions
#   - Force zero-count categories to appear when desired
#   - Export participant name tables
#   - Export open-ended response tables
#   - Export summary tables for closed-ended questions
#   - Create R objects that can be reused in an R Markdown report
#
# Input:
#   RGSMOT_Responses.csv
#
# Outputs:
#   survey_summary_outputs/
#     plots/
#     tables/
#     participant_names.csv
#     participant_names_unique.csv
#     question_summary_table.csv
#     question_summary_table_long.csv
#     response_rate_by_question.csv
#     codebook.csv
# ============================================================

# ---------------------------
# Packages
# ---------------------------
required_packages <- c(
  "tidyverse",
  "janitor",
  "stringr",
  "forcats",
  "scales",
  "glue"
)

installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed) install.packages(pkg)
}

library(tidyverse)
library(janitor)
library(stringr)
library(forcats)
library(scales)
library(glue)
library(gt)

# ---------------------------
# User settings
# ---------------------------
input_file <- "RGSMOT_Responses.csv"
out_dir <- "survey_summary_outputs"
plot_dir <- file.path(out_dir, "plots")
table_dir <- file.path(out_dir, "tables")

# Named vector of nicer question labels using the cleaned column names
question_labels <- c(
  combined_objective = "Decision 1: Should ecological function and cultural flows be combined?",
  assign_drying = "Decision 2: If kept separate, which objective should include river drying?",
  tmdl_proxy = "Decision 3: Is TMDL a reasonable proxy for cultural flows?",
  tmdl_duplicative = "Decision 4: Are TMDL and river drying duplicative?",
  tmdl_drying = "Decision 5: If one should be retained, which should remain?",
  reasoning = "Open-ended: Reasoning",
  concerns = "Open-ended: Concerns or reservations",
  tmdl_feedback = "Open-ended: TMDL feedback"
)

# Columns to treat as participant information
participant_cols <- c("first_name", "last_name")

# Open-ended question columns in this survey
open_ended_cols <- c("reasoning", "concerns", "tmdl_feedback")

# Manual ordering for response options
manual_order <- list(
  combined_objective = c("Keep objectives separate", "Combine into a single objective"),
  assign_drying = c("Ecological Function", "Cultural Flows"),
  tmdl_proxy = c("Yes", "No"),
  tmdl_duplicative = c("Yes - duplicative", "No - meaningfully different"),
  tmdl_drying = c("Retain Minimize River Drying", "Retain minimize TMDL")
)

# ---------------------------
# Helper functions
# ---------------------------
clean_text_value <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x[x %in% c("", "NA", "N/A", "na", "n/a", "NaN", "nan", "NULL", "null")] <- NA_character_
  x
}

make_safe_filename <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("(^_+|_+$)", "")
}

get_label <- function(var_name) {
  if (var_name %in% names(question_labels)) {
    question_labels[[var_name]]
  } else {
    str_replace_all(str_to_title(var_name), "_", " ")
  }
}

order_responses <- function(data, var_name) {
  if (var_name %in% names(manual_order)) {
    preferred <- manual_order[[var_name]]
    present <- unique(as.character(data$response))
    ordered_levels <- c(preferred[preferred %in% present], setdiff(sort(present), preferred))
    data |>
      mutate(response = factor(as.character(response), levels = ordered_levels))
  } else {
    data |>
      mutate(response = fct_infreq(as.factor(response)))
  }
}

complete_response_options <- function(data, var_name) {
  if (var_name %in% names(manual_order)) {
    data <- tibble(response = manual_order[[var_name]]) |>
      left_join(data, by = "response") |>
      mutate(n = replace_na(n, 0L))
  }
  data
}

# ---------------------------
# Create output folders
# ---------------------------
dir.create(out_dir, showWarnings = FALSE)
dir.create(plot_dir, showWarnings = FALSE)
dir.create(table_dir, showWarnings = FALSE)

# ---------------------------
# Read and clean data
# ---------------------------
survey_raw <- read_csv(
  input_file,
  locale = locale(encoding = "ISO-8859-1"),
  show_col_types = FALSE,
  trim_ws = FALSE
)

survey <- survey_raw |>
  clean_names() |>
  rename_with(~ str_replace_all(.x, "_+$", "")) |>
  mutate(across(everything(), clean_text_value))

n_participants <- nrow(survey)

message(glue("Read {n_participants} survey responses."))
message(glue("Cleaned columns: {toString(names(survey))}"))

# ---------------------------
# Build a codebook
# ---------------------------
codebook <- tibble(
  variable = names(survey),
  question_label = map_chr(names(survey), get_label),
  data_type = map_chr(names(survey), ~ class(survey[[.x]])[1]),
  non_missing_n = map_int(names(survey), ~ sum(!is.na(survey[[.x]]))),
  unique_non_missing_values = map_chr(
    names(survey),
    ~ paste(unique(na.omit(survey[[.x]])), collapse = " | ")
  )
)
write_csv(codebook, file.path(out_dir, "codebook.csv"))

# ---------------------------
# Participant tables
# ---------------------------
participants <- survey |>
  transmute(
    first_name = first_name,
    last_name = last_name,
    full_name = str_squish(str_c(first_name, last_name, sep = " "))
  )

participants_unique <- participants |>
  distinct() |>
  arrange(last_name, first_name)

write_csv(participants, file.path(out_dir, "participant_names.csv"))
write_csv(participants_unique, file.path(out_dir, "participant_names_unique.csv"))

# ---------------------------
# Open-ended response tables
# ---------------------------
open_ended_cols_present <- intersect(open_ended_cols, names(survey))

if (length(open_ended_cols_present) > 0) {
  open_ended_long <- survey |>
    mutate(
      response_id = row_number(),
      full_name = str_squish(str_c(first_name, last_name, sep = " "))
    ) |>
    select(response_id, full_name, all_of(open_ended_cols_present)) |>
    pivot_longer(
      cols = all_of(open_ended_cols_present),
      names_to = "question",
      values_to = "response"
    ) |>
    mutate(question_label = map_chr(question, get_label)) |>
    filter(!is.na(response)) |>
    select(response_id, full_name, question, question_label, response)

  write_csv(open_ended_long, file.path(table_dir, "open_ended_responses_all.csv"))

  for (col in open_ended_cols_present) {
    tab <- survey |>
      mutate(
        response_id = row_number(),
        full_name = str_squish(str_c(first_name, last_name, sep = " "))
      ) |>
      select(response_id, full_name, response = all_of(col)) |>
      filter(!is.na(response))

    write_csv(tab, file.path(table_dir, paste0(make_safe_filename(col), "_responses.csv")))
  }
} else {
  open_ended_long <- tibble()
}

# ---------------------------
# Identify closed-ended survey columns
# ---------------------------
closed_ended_cols <- setdiff(names(survey), c(participant_cols, open_ended_cols_present))

# Response rate table
response_rate <- tibble(
  variable = closed_ended_cols,
  question_label = map_chr(closed_ended_cols, get_label),
  responses_non_missing = map_int(closed_ended_cols, ~ sum(!is.na(survey[[.x]]))),
  total_participants = n_participants,
  response_rate = responses_non_missing / total_participants
)
write_csv(response_rate, file.path(out_dir, "response_rate_by_question.csv"))

# ---------------------------
# Summary tables for closed-ended questions
# ---------------------------
summary_long <- map_dfr(closed_ended_cols, function(col) {
  dat <- survey |>
    count(response = .data[[col]], name = "n") |>
    filter(!is.na(response)) |>
    mutate(response = as.character(response))
  
  dat <- complete_response_options(dat, col)
  
  if (nrow(dat) == 0) return(NULL)
  
  dat <- order_responses(dat, col)
  total_n <- sum(dat$n)
  
  dat |>
    mutate(
      variable = col,
      question_label = get_label(col),
      percent = if (total_n > 0) n / total_n else 0,
      total_responses = total_n,
      response = as.character(response)
    ) |>
    select(variable, question_label, response, n, percent, total_responses)
})

summary_wide <- summary_long |>
  mutate(percent_label = percent(percent, accuracy = 0.1)) |>
  mutate(n_percent = glue("{n} ({percent_label})")) |>
  select(question_label, response, n_percent) |>
  pivot_wider(names_from = response, values_from = n_percent)

write_csv(summary_long, file.path(out_dir, "question_summary_table_long.csv"))
write_csv(summary_wide, file.path(out_dir, "question_summary_table.csv"))

for (col in closed_ended_cols) {
  tab <- summary_long |>
    filter(variable == col) |>
    mutate(percent = percent(percent, accuracy = 0.1))

  if (nrow(tab) > 0) {
    write_csv(tab, file.path(table_dir, paste0(make_safe_filename(col), "_summary.csv")))
  }
}

# ---------------------------
# Plot two-option questions
# ---------------------------
plot_candidates <- closed_ended_cols[sapply(closed_ended_cols, function(col) {
  if (col %in% names(manual_order)) {
    length(manual_order[[col]]) == 2
  } else {
    vals <- unique(na.omit(survey[[col]]))
    length(vals) == 2
  }
})]

plot_data_list <- list()
plot_list <- list()

if (length(plot_candidates) > 0) {
  for (col in plot_candidates) {
    plot_dat <- survey |>
      count(response = .data[[col]], name = "n") |>
      filter(!is.na(response)) |>
      mutate(response = as.character(response))

    plot_dat <- complete_response_options(plot_dat, col)

    if (nrow(plot_dat) == 0) next

    plot_dat <- order_responses(plot_dat, col)
    total_n <- sum(plot_dat$n)
    if (total_n == 0) next

    plot_dat <- plot_dat |>
      mutate(
        prop = n / total_n,
        label = glue("{n} ({percent(prop, accuracy = 1)})")
      )

    plot_data_list[[col]] <- plot_dat

    p <- ggplot(plot_dat, aes(x = response, y = n)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = label, y = n + 0.05), size = 4.2) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.12)),
        breaks = pretty_breaks()
      ) +
      labs(
        title = get_label(col),
        subtitle = glue("n = {total_n} respondents"),
        x = NULL,
        y = "Number of responses",
        caption = "Percentages are calculated from non-missing responses for each question."
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10),
        axis.text.x = element_text(angle = 20, hjust = 1),
        panel.grid.minor = element_blank()
      )

    plot_list[[col]] <- p

    ggsave(
      filename = file.path(plot_dir, paste0(make_safe_filename(col), "_bar_chart.png")),
      plot = p,
      width = 8,
      height = 5,
      dpi = 300
    )
  }
}

# ---------------------------
# Optional: write a simple text summary
# ---------------------------
summary_lines <- c(
  "Webinar 2 Survey Summary",
  "========================",
  glue("Total responses: {n_participants}"),
  "",
  "Closed-ended questions summarized in:",
  "- question_summary_table.csv",
  "- question_summary_table_long.csv",
  "",
  "Participant tables:",
  "- participant_names.csv",
  "- participant_names_unique.csv",
  "",
  "Open-ended response tables are saved in the tables/ folder.",
  "Binary/two-option plots are saved in the plots/ folder."
)
writeLines(summary_lines, file.path(out_dir, "README_summary.txt"))

message("Done. Outputs saved to: ", out_dir)
