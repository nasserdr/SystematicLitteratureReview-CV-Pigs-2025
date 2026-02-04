#Distribution of biomarkers, applications and subapplications
#####
split_re <- "\\s*(?:,|;|/|\\+|&|\\band\\b)\\s*"
df_sel <- df_full_screening %>%
  select(
    Biomarker,
    Application,
    Subapplication
  ) %>%
  mutate(across(everything(), ~ {
    x <- as.character(.)
    x <- str_squish(x)
    x[x %in% c("", "NA", "N/A", "-")] <- NA_character_
    x
  }))
make_long <- function(df, col) {
  df %>%
    select({{ col }}) %>%
    filter(!is.na({{ col }})) %>%
    separate_rows({{ col }}, sep = split_re) %>%
    mutate({{ col }} := str_to_title(str_squish({{ col }}))) %>%
    filter({{ col }} != "")
}
bio_long <- make_long(df_sel, Biomarker)
app_long <- make_long(df_sel, Application)
sub_long <- make_long(df_sel, Subapplication)
count_and_plot <- function(data, col, title) {
  counts <- data %>%
    count({{ col }}, name = "n") %>%
    arrange(desc(n)) %>%
    mutate({{ col }} := fct_reorder({{ col }}, n))
  ggplot(counts, aes(x = n, y = {{ col }})) +
    geom_col(fill = "#4E79A7", width = 0.7) +
    geom_text(aes(label = n), hjust = -0.1, size = 3.5) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = "Number of Studies", y = NULL, title = title) +
    theme_classic(base_size = 13) +
    theme(
      axis.text.x  = element_text(size = 14),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      legend.text  = element_text(size = 14),
      
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
      plot.margin = margin(10, 20, 10, 10)
    )
}
p_bio <- count_and_plot(bio_long, Biomarker, "Biomarkers")
p_app <- count_and_plot(app_long, Application, "Applications")
p_sub <- count_and_plot(sub_long, Subapplication, "Subapplications")
combined_plot <- p_bio + p_app + p_sub + plot_layout(ncol = 3)
print(combined_plot)
ggsave(file.path(im_spare, "biomarker_application_subapplication_histograms.png"),
       plot = combined_plot, width = 15, height = 8, dpi = 300)


#Heatmap of biomarkers vs applications
#####
col_biomarker    <- "Biomarker"
col_application  <- "Application"
col_subapp <- "Subapplication"

df_sel <- df_full_screening %>%
  select(
    Biomarker    = all_of(col_biomarker),
    Application  = all_of(col_application),
    Subapp       = all_of(col_subapp)
  ) %>%
  mutate(across(everything(), ~ {
    x <- as.character(.)
    x <- str_squish(x)
    x[x %in% c("", "NA", "N/A", "-")] <- NA_character_
    x
  }))
split_re <- "\\s*(?:,|;|/|\\+|&|\\band\\b)\\s*"
df_ids <- df_sel %>% mutate(.row_id = row_number())
bio_pairs <- df_ids %>%
  select(.row_id, Biomarker) %>%
  filter(!is.na(Biomarker)) %>%
  separate_rows(Biomarker, sep = split_re) %>%
  mutate(Biomarker = str_to_title(str_squish(Biomarker))) %>%
  filter(Biomarker != "")
app_pairs <- df_ids %>%
  select(.row_id, Application) %>%
  filter(!is.na(Application)) %>%
  separate_rows(Application, sep = split_re) %>%
  mutate(Application = str_to_title(str_squish(Application))) %>%
  filter(Application != "")
bio_app_counts <- bio_pairs %>%
  inner_join(app_pairs, by = ".row_id") %>%
  count(Biomarker, Application, name = "n")
bio_order <- bio_app_counts %>% group_by(Biomarker) %>% summarise(t = sum(n), .groups="drop") %>%
  arrange(desc(t)) %>% pull(Biomarker)
app_order <- bio_app_counts %>% group_by(Application) %>% summarise(t = sum(n), .groups="drop") %>%
  arrange(desc(t)) %>% pull(Application)

p_heat <- ggplot(bio_app_counts,
                 aes(x = factor(Application, levels = app_order),
                     y = factor(Biomarker,  levels = bio_order),
                     fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "white", size = 4) +   
  labs(x = "Application", y = "Biomarker", fill = "Count") +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 35, hjust = 1,size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 14),
    
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
  )
print(p_heat)
ggsave(file.path(im_path,"biomarker_vs_application_heatmap.png"),
       plot = p_heat, width = 11, height = 8, dpi = 300)

#Heatmap of biomarkers vs applications vs Subspplications, Alluvial

# Helper to standardize strings
clean_str <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\\s+", " ") %>%
    str_squish() %>%
    na_if("") %>%
    tidyr::replace_na("Unknown")
}

# Pattern for "(same as application)" (any case/spacing/parentheses)
same_as_app_rx <- regex("^\\s*\\(?\\s*same\\s+as\\s+application\\s*\\)?\\s*$", ignore_case = TRUE)

# Clean/select three columns and normalize Subapplication
dat <- df_full_screening %>%
  transmute(
    Biomarker      = clean_str(.data$Biomarker),
    Application    = clean_str(.data$Application),
    Subapplication = clean_str(.data$Subapplication)
  ) %>%
  mutate(
    Subapplication = dplyr::case_when(
      is.na(Subapplication) ~ Application,
      Subapplication == "Unknown" ~ Application,
      str_detect(Subapplication, same_as_app_rx) ~ Application,
      TRUE ~ Subapplication
    )
  )

# Aggregate counts for the 3-level paths
path_counts <- dat %>%
  filter( Biomarker != 'Unknown') %>%
  count(Biomarker, Application, Subapplication, name = "n")

# --- Your plotting code (unchanged except it now has path_counts) ---

total_n <- sum(path_counts$n)

bg_df <- tibble(
  xmin = c(0.6, 1.6, 2.6),
  xmax = c(1.4, 2.4, 3.4),
  ymin = 0,
  ymax = total_n,
  axis = c("Biomarker", "Application", "Subapplication"),
  fill = c("#f8fafc", "#f1f5f9", "#f8fafc")
)

p_alluvial <- ggplot(
  path_counts,
  aes(axis1 = Biomarker, axis2 = Application, axis3 = Subapplication, y = n)
) +
  # background bands per axis
  geom_rect(
    data = bg_df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = bg_df$fill, color = NA, alpha = 1
  ) +
  scale_x_discrete(limits = c("Biomarker", "Application", "Subapplication")) +
  # flows & strata
  geom_alluvium(aes(fill = Biomarker), alpha = 0.7, knot.pos = 0.4) +
  geom_stratum(width = 0.15, fill = "grey90", color = "grey40") +
  # labels INSIDE each stratum box
  geom_fit_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    width = 0.15,                # match geom_stratum width
    min.size = 1,                # lower if many small boxes
    grow = TRUE, reflow = TRUE,  # shrink/wrap to fit
    contrast = FALSE             # keep black text on light boxes
  ) +
  labs(
    y = "Count", x = NULL
  ) +
  expand_limits(y = total_n * 1.08) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # remove grids (both x & y)
    panel.grid.minor = element_blank()
  )

p_alluvial

ggsave(file.path(im_spare, "alluvial_biomarker_application_subapplication.png"),
       p_alluvial, width = 50, height = 50, dpi = 400, limitsize = FALSE)
ggsave(file.path(im_spare,"alluvial_biomarker_application_subapplication.pdf"),
       p_alluvial, width = 50, height = 50, limitsize = FALSE)

