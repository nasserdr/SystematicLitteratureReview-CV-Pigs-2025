library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)
library(patchwork)
split_re <- "\\s*(?:,|;|/|\\+|&|\\band\\b)\\s*"

clean_str <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\\s+", " ") %>%
    str_squish() %>%
    na_if("") %>%
    tidyr::replace_na("Unknown")
}

make_orders <- function(df, row_var, col_var, n_var = "n") {
  row_order <- df %>%
    group_by(.data[[row_var]]) %>%
    summarise(t = sum(.data[[n_var]]), .groups = "drop") %>%
    arrange(desc(t)) %>%
    pull(.data[[row_var]])
  
  col_order <- df %>%
    group_by(.data[[col_var]]) %>%
    summarise(t = sum(.data[[n_var]]), .groups = "drop") %>%
    arrange(desc(t)) %>%
    pull(.data[[col_var]])
  
  list(row_order = row_order, col_order = col_order)
}

# Behavior applications / methods

df_long <- df_full_screening %>%
  select(Behavior, Methods) %>%
  mutate(Behavior = str_split(Behavior, ",\\s*")) %>%  # split by commas
  unnest(Behavior) %>%                                 # expand into multiple rows
  mutate(Behavior = str_trim(Behavior))                # trim spaces

df_final <- df_long %>%
  left_join(df_lookup, by = "Behavior") %>%
  rename(Category = Category) %>%  # or Behavior category if you prefer
  rename(Methods_old = Methods) %>%   # temporarily rename
  mutate(Type = if_else(Methods_old == "No", "Application", "Method")) %>%
  select(-Methods_old)     %>%           # remove old column
  filter(!is.na(Behavior), !is.na(Category))

df_counts <- df_final %>%
  count(Category, Type, Behavior, name = "n")

df_plot <- df_counts %>%
  # total per behavior (for ordering)
  group_by(Behavior, Category) %>%
  mutate(total_n = sum(n)) %>%
  ungroup() %>%
  # order behaviors by Category, then by total_n
  arrange(Category, desc(total_n)) %>%
  mutate(
    Behavior = factor(Behavior, levels = unique(Behavior)),
    n_signed = if_else(Type == "Method", -n, n)  # Method left, Application right
  )

# symmetric axis limit
max_n <- max(df_plot$n)

# --- 2. Plot ---

p <- ggplot(df_plot, aes(x = Behavior, y = n_signed, fill = Category)) +
  # thick vertical line at zero (horizontal before flip)
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    limits = c(-10, max_n),
    labels = function(x) abs(x)  # show positive numbers on both sides
  ) +
  labs(
    x = NULL,
    y = "Number of occurrences\n(Methods left, Applications right)",
    fill = "Category",
  ) +
  theme_minimal(base_size = 13) +
  theme_classic(base_size = 13) +                                         # consistent theme
  theme(
    axis.text.x  = element_text(hjust = 1,size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 40, 10, 10)                                  # extra margin for labels
  )

print(p)
ggsave(file.path(im_path, "behavior_full_figure.png"), p, width = 15, height = 10, dpi = 300)

# Heatmap Biomarker Vs Application
df_ids <- df_full_screening %>%
  transmute(
    .row_id = row_number(),
    Biomarker   = clean_str(Biomarker),
    Application = clean_str(Application)
  )

bio_pairs <- df_ids %>%
  filter(!is.na(Biomarker), Biomarker != "Unknown") %>%
  separate_rows(Biomarker, sep = split_re) %>%
  mutate(Biomarker = str_to_title(str_squish(Biomarker))) %>%
  filter(Biomarker != "") %>%
  select(.row_id, Biomarker)   # << IMPORTANT

app_pairs <- df_ids %>%
  filter(!is.na(Application), Application != "Unknown") %>%
  separate_rows(Application, sep = split_re) %>%
  mutate(Application = str_to_title(str_squish(Application))) %>%
  filter(Application != "") %>%
  select(.row_id, Application) # << IMPORTANT

bio_app_counts <- bio_pairs %>%
  inner_join(app_pairs, by = ".row_id", relationship = "many-to-many") %>% # expected
  count(Biomarker, Application, name = "n")

ord <- make_orders(bio_app_counts, row_var = "Biomarker", col_var = "Application")

p_bio_app <- plot_heatmap(
  bio_app_counts,
  row_var = "Biomarker",
  col_var = "Application",
  row_order = ord$row_order,
  col_order = ord$col_order,
  xlab = "Application",
  ylab = "Biomarker",
  out_path = file.path(im_path, "heatmap_biomarker_vs_application.png"),
  width = 11, height = 8, x_angle = 35, text_size = 4
)


# Heatmaps of Behavior vs Category, Biomarker, Application
plot_heatmap <- function(df_counts, row_var, col_var, row_order, col_order,
                         xlab, ylab, out_path = NULL,
                         width = 12, height = 8, dpi = 300,
                         x_angle = 35, text_size = 4) {
  
  p <- ggplot(df_counts,
              aes(x = factor(.data[[col_var]], levels = col_order),
                  y = factor(.data[[row_var]], levels = row_order),
                  fill = n)) +
    geom_tile(color = "white") +
    geom_text(aes(label = n), color = "white", size = text_size) +
    labs(x = xlab, y = ylab, fill = "Count") +
    theme_classic(base_size = 14) +
    theme(
      axis.text.x  = element_text(angle = x_angle, hjust = 1,size = 14),
      axis.text.y  = element_text(size = 14),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      legend.text  = element_text(size = 14),
      
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold"),
    )
  
  print(p)
  
  if (!is.null(out_path)) {
    ggsave(out_path, plot = p, width = width, height = height, dpi = dpi, limitsize = FALSE)
  }
  
  invisible(p)
}


behav_cat_counts <- df_full_screening %>%
  mutate(Behavior = str_squish(str_to_title(Behavior))) %>%
  filter(!is.na(Behavior), Behavior != "") %>%
  separate_rows(Behavior, sep = split_re) %>%
  mutate(Behavior = str_squish(Behavior)) %>%
  filter(Behavior != "") %>%
  left_join(
    df_lookup %>%
      mutate(
        Behavior = str_squish(str_to_title(Behavior)),
        Category = str_squish(str_to_title(Category))
      ),
    by = "Behavior"
  ) %>%
  filter(!is.na(Category), Category != "") %>%
  count(Category, Behavior, name = "n")

ord <- make_orders(behav_cat_counts, row_var = "Behavior", col_var = "Category")

p_behav_cat <- plot_heatmap(
  behav_cat_counts,
  row_var = "Behavior",
  col_var = "Category",
  row_order = ord$row_order,
  col_order = ord$col_order,
  xlab = "Category",
  ylab = "Behavior",
  out_path = file.path(im_path, "heatmap_behavior_vs_category.png"),
  width = 10, height = 10, x_angle = 35, text_size = 3.5
)


behav_biom_counts <- df_full_screening %>%
  mutate(
    Behavior  = str_squish(str_to_title(Behavior)),
    Biomarker = clean_str(Biomarker)
  ) %>%
  filter(!is.na(Behavior), Behavior != "", !is.na(Biomarker), Biomarker != "") %>%
  separate_rows(Behavior,  sep = split_re) %>%
  separate_rows(Biomarker, sep = split_re) %>%
  mutate(
    Behavior  = str_squish(Behavior),
    Biomarker = str_to_title(str_squish(Biomarker))
  ) %>%
  filter(Behavior != "", Biomarker != "") %>%
  count(Behavior, Biomarker, name = "n")

ord <- make_orders(behav_biom_counts, row_var = "Behavior", col_var = "Biomarker")

p_behav_biom <- plot_heatmap(
  behav_biom_counts,
  row_var = "Behavior",
  col_var = "Biomarker",
  row_order = ord$row_order,
  col_order = ord$col_order,
  xlab = "Biomarker",
  ylab = "Behavior",
  out_path = file.path(im_path, "heatmap_behavior_vs_biomarker.png"),
  width = 18, height = 14, x_angle = 35, text_size = 3.5
)

behav_app_counts <- df_full_screening %>%
  mutate(
    Behavior    = str_squish(str_to_title(Behavior)),
    Application = clean_str(Application)
  ) %>%
  filter(!is.na(Behavior), Behavior != "", !is.na(Application), Application != "") %>%
  separate_rows(Behavior,    sep = split_re) %>%
  separate_rows(Application, sep = split_re) %>%
  mutate(
    Behavior    = str_squish(Behavior),
    Application = str_to_title(str_squish(Application))
  ) %>%
  filter(Behavior != "", Application != "") %>%
  count(Behavior, Application, name = "n")

ord <- make_orders(behav_app_counts, row_var = "Behavior", col_var = "Application")

p_behav_app <- plot_heatmap(
  behav_app_counts,
  row_var = "Behavior",
  col_var = "Application",
  row_order = ord$row_order,
  col_order = ord$col_order,
  xlab = "Application",
  ylab = "Behavior",
  out_path = file.path(im_path, "heatmap_behavior_vs_application.png"),
  width = 18, height = 14, x_angle = 35, text_size = 3.5
)


cat_app_counts <- df_full_screening %>%
  mutate(
    Behavior    = str_squish(str_to_title(Behavior)),
    Application = clean_str(Application)
  ) %>%
  filter(!is.na(Behavior), Behavior != "", !is.na(Application), Application != "") %>%
  separate_rows(Behavior,    sep = split_re) %>%
  separate_rows(Application, sep = split_re) %>%
  mutate(
    Behavior    = str_squish(Behavior),
    Application = str_to_title(str_squish(Application))
  ) %>%
  filter(Behavior != "", Application != "") %>%
  left_join(
    df_lookup %>%
      mutate(
        Behavior = str_squish(str_to_title(Behavior)),
        Category = str_squish(str_to_title(Category))
      ),
    by = "Behavior"
  ) %>%
  filter(!is.na(Category), Category != "") %>%
  count(Category, Application, name = "n")

ord <- make_orders(cat_app_counts, row_var = "Category", col_var = "Application")

p_cat_app <- plot_heatmap(
  cat_app_counts,
  row_var = "Category",
  col_var = "Application",
  row_order = ord$row_order,
  col_order = ord$col_order,
  xlab = "Application",
  ylab = "Category",
  out_path = file.path(im_path, "heatmap_category_vs_application.png"),
  width = 18, height = 10, x_angle = 35, text_size = 4
)

# Merge all the 4 heatmaps together
combined_heatmaps <-
  (p_behav_cat | p_behav_biom) /
  (p_behav_app | p_cat_app)
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(
  filename = file.path(im_path, "heatmaps_all_combined.png"),
  plot = combined_heatmaps,
  width = 22, height = 18, dpi = 300, limitsize = FALSE
)
