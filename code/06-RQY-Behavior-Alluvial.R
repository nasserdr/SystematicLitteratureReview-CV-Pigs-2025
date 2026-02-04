# Behavior vs applications/methods paper
# 
# clean_str <- function(x) {
#   x %>%
#     as.character() %>%
#     str_replace_all("\\s+", " ") %>%
#     str_squish() %>%
#     na_if("") %>%
#     tidyr::replace_na("Unknown")
# }
# 
# # --- Parse behaviors & classify study type from Methods ---
# behav_long <- df_full_screening %>%
#   mutate(
#     Behavior      = str_squish(str_to_title(Behavior)),
#     Methods_clean = clean_str(Methods),
#     # classify: anything not "No" (case-insensitive) is a Method paper
#     StudyType = if_else(
#       is.na(Methods) | str_to_lower(Methods_clean) == "no",
#       "Application study",
#       "Method paper"
#     )
#   ) %>%
#   filter(!is.na(Behavior), Behavior != "") %>%
#   separate_rows(Behavior, sep = "\\s*(?:,|;|/|\\+|&|\\band\\b)\\s*") %>%
#   mutate(Behavior = str_squish(Behavior)) %>%
#   filter(Behavior != "")
# 
# # counts per behavior & study type
# behav_counts_type <- behav_long %>%
#   count(Behavior, StudyType, name = "n")
# 
# # totals to order and to label
# total_by_behavior <- behav_counts_type %>%
#   group_by(Behavior) %>%
#   summarise(total = sum(n), .groups = "drop")
# 
# # join totals & reorder behaviors by total desc
# behav_counts_plot <- behav_counts_type %>%
#   left_join(total_by_behavior, by = "Behavior") %>%
#   mutate(Behavior = fct_reorder(Behavior, total, .desc = TRUE))
# 
# # palette: application (blue) vs method (red)
# pal <- c("Application study" = "#4E79A7", "Method paper" = "#E15759")
# 
# # horizontal stacked bar chart
# p <- ggplot(behav_counts_plot, aes(y = Behavior, x = n, fill = StudyType)) +
#   geom_col(alpha = 0.95) +
#   # total labels at bar ends
#   geom_text(
#     data = distinct(behav_counts_plot, Behavior, total),
#     aes(x = total, y = Behavior, label = total),
#     inherit.aes = FALSE,
#     vjust = -1, size = 3.8, fontface = "bold"
#   ) +
#   coord_flip() + 
#   scale_fill_manual(values = pal, name = NULL) +
#   scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
#   labs(
#     x = "Number of Studies",
#     y = "Behavior"
#   ) +
#   theme_classic(base_size = 14) +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold"),
#     axis.title = element_text(face = "bold"),
#     legend.position = "bottom",
#     axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
#   )
# 
# print(p)
# 
# ggsave(
#   filename = file.path(im_path, "behavior_distribution_by_studytype_horizontal.pdf"),
#   plot = p,
#   width = 30, height = 15, dpi = 300
# )

# Behavior Vs Category of Behavior Vs applications / methods

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
    y = "Number of occurrences\n(Method left, Application right)",
    fill = "Category",
    title = "How often each behavior was studied\nin Method vs Application papers"
  ) +
  theme_minimal(base_size = 13) +
theme_classic(base_size = 13) +                                         # consistent theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 40, 10, 10)                                  # extra margin for labels
  )

print(p)
ggsave(file.path(im_path, "behavior_full_figure.png"), p, width = 10, height = 10, dpi = 300)


# ===========================================================
# ALLUVIAL: Behavior → Biomarker
# ===========================================================
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggalluvial)
library(ggfittext)
library(tibble)

# ---- Parameters ----
BASELINE     <- 10     # increases stratum heights
SCALE_FACTOR <- 1
STRATUM_W    <- 0.22   # stratum rectangle width
TEXT_W       <- 1   # text box width (match STRATUM_W)
TEXT_SIZE    <- 6
TEXT_MIN     <- 3

# ---- Build counts ----
behav_biom_counts2 <- df_full_screening %>%
  mutate(
    Behavior  = str_squish(str_to_title(Behavior)),
    Biomarker = clean_str(Biomarker)
  ) %>%
  filter(!is.na(Behavior), Behavior != "", !is.na(Biomarker), Biomarker != "") %>%
  separate_rows(Behavior, sep = "\\s*(?:,|;|/|\\+|&|\\band\\b)\\s*") %>%
  mutate(
    Behavior = str_squish(Behavior)
  ) %>%
  filter(Behavior != "") %>%
  count(Behavior, Biomarker, name = "n") %>%
  mutate(
    weight = (n + BASELINE) * SCALE_FACTOR,
    # multi-line labels (word-wrapping)
    Behavior_label  = str_wrap(Behavior,  width = 14),
    Biomarker_label = str_wrap(Biomarker, width = 14)
  )

total_n_bb <- sum(behav_biom_counts2$weight)

# ---- Background bands (recomputed AFTER total_n_bb is known) ----
bg_df_bb <- tibble(
  xmin = c(0.6, 1.6), xmax = c(1.4, 2.4),
  ymin = 0, ymax = total_n_bb,
  fill = c("grey98", "grey95")
)

# ---- Plot ----
p_alluvial_behav_biom <- ggplot(
  behav_biom_counts2,
  aes(axis1 = Behavior_label, axis2 = Biomarker_label, y = weight)
) +
  geom_rect(
    data = bg_df_bb,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = bg_df_bb$fill, color = NA
  ) +
  scale_x_discrete(limits = c("Behavior", "Biomarker")) +
  geom_alluvium(aes(fill = Behavior), alpha = 0.7, knot.pos = 0.4) +
  geom_stratum(width = STRATUM_W, fill = "grey90", color = "grey40") +
  geom_fit_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    width = TEXT_W,
    size = TEXT_SIZE,
    min.size = TEXT_MIN,
    grow = FALSE,
    reflow = TRUE,
    lineheight = 0.95
  ) +
  annotate(
    "text",
    x = c(1, 2), y = total_n_bb * 1.03,
    label = c("Behavior", "Biomarker"),
    fontface = "bold", size = 6
  ) +
  labs(y = "Count", x = NULL) +
  expand_limits(y = total_n_bb * 1.10) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p_alluvial_behav_biom

ggsave(
  file.path(im_spare, "alluvial_behavior_to_biomarker.tiff"),
  p_alluvial_behav_biom,
  width = 30, height = 30, dpi = 300, limitsize = FALSE
)



# ===========================================================
# ALLUVIAL: Behavior → Application
# ===========================================================

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggalluvial)
library(ggfittext)
library(tibble)

# ---- Parameters ----
BASELINE     <- 10
SCALE_FACTOR <- 1
STRATUM_W    <- 0.22
TEXT_W       <- 0.22
TEXT_SIZE    <- 15
TEXT_MIN     <- 2
WRAP_WIDTH   <- 7

# ---- Build counts ----
behav_app_counts2 <- df_full_screening %>%
  mutate(
    Behavior    = str_squish(str_to_title(Behavior)),
    Application = clean_str(Application)
  ) %>%
  filter(!is.na(Behavior), Behavior != "",
         !is.na(Application), Application != "") %>%
  separate_rows(Behavior, sep = "\\s*(?:,|;|/|\\+|&|\\band\\b)\\s*") %>%
  mutate(Behavior = str_squish(Behavior)) %>%
  filter(Behavior != "") %>%
  count(Behavior, Application, name = "n") %>%
  mutate(
    weight = (n + BASELINE) * SCALE_FACTOR,
    Behavior_label    = str_wrap(Behavior,    width = WRAP_WIDTH),
    Application_label = str_wrap(Application, width = WRAP_WIDTH)
  )

total_n_ba <- sum(behav_app_counts2$weight)

# ---- Background bands (recomputed AFTER total is known) ----
bg_df_ba <- tibble(
  xmin = c(0.6, 1.6), xmax = c(1.4, 2.4),
  ymin = 0, ymax = total_n_ba,
  fill = c("grey98", "grey95")
)

# ---- Plot ----
p_alluvial_behav_app <- ggplot(
  behav_app_counts2,
  aes(axis1 = Behavior_label, axis2 = Application_label, y = weight)
) +
  geom_rect(
    data = bg_df_ba,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = bg_df_ba$fill, color = NA
  ) +
  scale_x_discrete(limits = c("Behavior", "Application")) +
  geom_alluvium(aes(fill = Behavior), alpha = 0.7, knot.pos = 0.4) +
  geom_stratum(width = STRATUM_W, fill = "grey90", color = "grey40") +
  geom_fit_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    width = TEXT_W,
    size = TEXT_SIZE,
    min.size = TEXT_MIN,
    grow = FALSE,
    reflow = TRUE,
    lineheight = 0.95
  ) +
  annotate(
    "text",
    x = c(1, 2), y = total_n_ba * 1.03,
    label = c("Behavior", "Application"),
    fontface = "bold", size = 6
  ) +
  labs(y = "Count", x = NULL) +
  expand_limits(y = total_n_ba * 1.10) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p_alluvial_behav_app

ggsave(
  file.path(im_path, "alluvial_behavior_to_application.tiff"),
  p_alluvial_behav_app,
  width = 20, height = 14, dpi = 300, limitsize = FALSE
)

# ===========================================================
# ALLUVIAL: Behavior Category → Application
# ===========================================================

cat_app_counts <- df_full_screening %>%
  mutate(
    Behavior    = str_squish(str_to_title(Behavior)),
    Application = clean_str(Application)
  ) %>%
  filter(!is.na(Behavior), Behavior != "") %>%
  separate_rows(Behavior, sep = "\\s*(?:,|;|/|\\+|&|\\band\\b)\\s*") %>%
  mutate(Behavior = str_squish(Behavior)) %>%
  # ↓ join to get Category from Behavior
  left_join(df_lookup %>% 
              mutate(
                Behavior = str_squish(str_to_title(Behavior)),
                Category = str_squish(str_to_title(Category))
              ),
            by = "Behavior") %>%
  filter(!is.na(Category), Category != "") %>%
  count(Category, Application, name = "n")

# Optional: collapse rare categories or applications here if needed.
# cat_app_counts <- cat_app_counts %>% mutate(Category = fct_lump_min(Category, min = 3))

# Weight to avoid zero-height flows (keep your original 'baseline' value)
cat_app_counts <- cat_app_counts %>% mutate(weight = n + baseline)
total_n_ca <- sum(cat_app_counts$weight)

# Background shading (same style as your Behavior→Application plot)
bg_df_ca <- tibble(
  xmin = c(0.6, 1.6), xmax = c(1.4, 2.4),
  ymin = 0, ymax = total_n_ca,
  fill = c("#f8fafc", "#f1f5f9")
)

# Build the alluvial: Category → Application
p_alluvial_cat_app <- ggplot(
  cat_app_counts,
  aes(axis1 = Category, axis2 = Application, y = weight)
) +
  # background stripes
  geom_rect(
    data = bg_df_ca,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = bg_df_ca$fill, color = NA
  ) +
  # axis labels
  scale_x_discrete(limits = c("Category", "Application")) +
  # flows; fill by Category (legend hidden, like your original)
  geom_alluvium(aes(fill = Category), alpha = 0.7, knot.pos = 0.4) +
  # nodes
  geom_stratum(width = 0.18, fill = "grey90", color = "grey40") +
  # node text
  geom_fit_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    width = 0.18, min.size = 2, grow = TRUE, reflow = TRUE
  ) +
  # titles
  annotate("text",
           x = c(1, 2), y = total_n_ca * 1.03,
           label = c("Category", "Application"),
           fontface = "bold", size = 4) +
  labs(y = "Count", x = NULL) +
  expand_limits(y = total_n_ca * 1.08) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Render
p_alluvial_cat_app

# Save (keeping your sizes)
ggsave(file.path(im_spare,"alluvial_category_to_application.pdf"),
       p_alluvial_cat_app, width = 20, height = 14, dpi = 300, limitsize = FALSE)

