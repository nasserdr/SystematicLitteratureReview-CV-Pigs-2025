
#List of paper per year (total, applications, contribution to methods)
#####
df_prepped <- df_full_screening %>%
  mutate(
    Year = as.integer(Year),
    PaperType = case_when(
      is.na(`Methods`) ~ "Application",
      str_to_lower(str_trim(`Methods`)) == "no" ~ "Application",
      TRUE ~ "Method"
    )
  ) %>%
  filter(!is.na(Year))
counts_by_type <- df_prepped %>%
  count(Year, PaperType, name = "n")
totals_by_year <- counts_by_type %>%
  group_by(Year) %>%
  summarise(total = sum(n), .groups = "drop")
p <- ggplot() +
  # Stacked bars
  geom_col(
    data = counts_by_type,
    aes(x = Year, y = n, fill = PaperType),
    width = 0.8
  ) +
  # Total line on top
  geom_line(
    data = totals_by_year,
    aes(x = Year, y = total, group = 1),
    linewidth = 1.1,
    color = "black"
  ) +
  geom_point(
    data = totals_by_year,
    aes(x = Year, y = total),
    size = 2
  ) +
  scale_x_continuous(breaks = sort(unique(df_prepped$Year))) +
  labs(
    x = "Year",
    y = "Number of Papers",
    fill = "Paper Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x  = element_text(hjust = 1,size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 14),
    
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

print(p)
ggsave(
  filename = file.path(im_path,"methods_vs_applications_per_year.png"),
  plot = p,
  width = 9,   # inches
  height = 6,  # inches
  dpi = 300
)

#Annotation software used
#####
split_re <- "\\s*(?:,|;|/|\\+|&|\\band\\b)\\s*"

df_sel <- df_full_screening %>%
  select(Annotation_Software) %>%
  mutate(Annotation_Software = str_remove_all(Annotation_Software, "\\s*\\([^)]+\\)")) %>% 
  mutate(across(
    everything(),
    ~ {
      y <- as.character(.)
      y <- str_squish(y)
      y[y %in% c("", "NA", "N/A", "-")] <- NA_character_
      y
    }
  ))

anno_long <- df_sel %>%
  filter(!is.na(Annotation_Software)) %>%
  transmute(Annotation_Software = str_to_title(Annotation_Software)) %>%
  separate_rows(Annotation_Software, sep = split_re) %>%
  mutate(Annotation_Software = str_squish(Annotation_Software)) %>%
  filter(Annotation_Software != "")

anno_counts <- anno_long %>%
  count(Annotation_Software, name = "n") %>%
  arrange(desc(n)) %>%
  mutate(
    perc = round(100 * n / sum(n), 1),                      # ⬅️ add percentages
    Annotation_Software = fct_reorder(Annotation_Software, n)
  )

p_bar <- ggplot(anno_counts, aes(x = Annotation_Software, y = n)) +
  geom_col(fill = "#4E79A7", width = 0.75) +                 # ⬅️ consistent color
  geom_text(aes(label = paste0(n, " (", perc, "%)")),
            hjust = -0.1, size = 4) +                        # ⬅️ counts + %
  coord_flip(clip = "off") +                                 # allow labels to overflow
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +# extra space right
  labs(
    x = "Annotation Software",
    y = "Number of Studies"
  ) +
  theme_classic(base_size = 14) +                            # match style
  theme(
    axis.text.x  = element_text(hjust = 1,size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 14),
    
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 40, 10, 10)                     # avoid label clipping
  )

print(p_bar)

ggsave(file.path(im_path,"annotation_software_hist.png"),
       plot = p_bar, width = 9, height = 6, dpi = 300)

#Distribution of the number of labeled images 
#####
col_name <- "Number Labeled of Images (Training, Testing and Validation)"
df_counts <- df_full_screening %>%
  select(val = all_of(col_name)) %>%
  mutate(val = as.character(val)) %>%
  mutate(val = na_if(str_squish(val), "")) %>%
  mutate(val = ifelse(val %in% c("NA","N/A","-"), NA, val)) %>%
  mutate(n_labeled = parse_number(val)) %>%
  select(n_labeled) %>%
  filter(!is.na(n_labeled), n_labeled > 0) %>%
  filter(n_labeled < 10000)

# ---- 2) Histogram (linear scale) ----
p_linear <- ggplot(df_counts, aes(x = n_labeled)) +
  geom_boxplot( color = "black") +
  labs(
    x = "Number of Labeled Images",
    y = NULL,
  ) +
  coord_flip() +
  theme_classic(base_size = 14) +
  theme(
    axis.text.y  = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),

    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

print(p_linear)
ggsave(file.path(im_path,"num_labeled_images_hist_linear.png"),
       plot = p_linear, width = 8, height = 5.5, dpi = 300)

#Distribution of the image sizes (just like the bouding boxes ones overlay) - How to tell size of images and number of images at once?
#####
df_sz <- df_full_screening %>%
  select(`Image Size`) %>%
  mutate(`Image Size` = as.character(`Image Size`))
sizes_parsed <- df_sz %>%
  filter(!is.na(`Image Size`), `Image Size` != "") %>%
  mutate(
    raw = str_replace_all(`Image Size`, ",", ""),      # remove thousand separators if any
    raw = str_squish(raw)
  ) %>%
  separate_wider_delim(
    raw,
    delim = regex("\\s*[x×]\\s*"),  # split on 'x' or '×'
    names = c("w_raw", "h_raw"),
    too_few = "align_start",
    cols_remove = FALSE
  ) %>%
  mutate(
    w = suppressWarnings(as.integer(str_extract(w_raw, "\\d+"))),
    h = suppressWarnings(as.integer(str_extract(h_raw, "\\d+")))
  ) %>%
  filter(!is.na(w), !is.na(h), w > 0, h > 0) %>%
  mutate(
    width  = pmax(w, h),   # unify rotation: larger side = width
    height = pmin(w, h),
    res_label = paste0(width, "×", height)
  )
res_counts <- sizes_parsed %>%
  count(width, height, res_label, name = "n") %>%
  arrange(desc(n))
top3_labels <- res_counts$res_label[1:min(3, nrow(res_counts))]
rects <- res_counts %>%
  mutate(
    xmin = 0, xmax = width,    # anchor at origin
    ymin = 0, ymax = height,
    color_group = ifelse(res_label %in% top3_labels, res_label, "Other")
  ) %>%
  arrange(width * height)      # draw smaller first
max_w <- max(rects$width)
max_h <- max(rects$height)
p_rects_origin <- ggplot(rects) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        linewidth = n, color = color_group),
    fill = NA, lineend = "round"
  ) +
  scale_color_manual(
    values = c(setNames(c("#E15759", "#4E79A7", "#59A14F"), top3_labels), Other = "grey60"),
    guide = "none"
  ) +
  scale_linewidth_continuous(range = c(0.25, 3.0), guide = "none") +
  coord_fixed(xlim = c(0, max_w * 1.02), ylim = c(0, max_h * 1.02), expand = FALSE) +
  scale_x_continuous(breaks = scales::pretty_breaks(6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(6)) +
  labs(x = "Width (px)", y = "Height (px)") +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x  = element_text(hjust = 1,size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 14),
    
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(12, 12, 12, 12)
  )

print(p_rects_origin)
ggsave(file.path(im_path,"image_resolutions_rectangles_centered.png"),
       plot = p_rects_origin, width = 7.5, height = 7.5, dpi = 300)

#Availibility of codes and datasets publicly
#####
ds_candidates   <- c("Dataset_Availibility", "Dataset_Availability")
code_candidates <- c("Code_Availibility_Link",    "Code_Availibility_Link")
df_sel <- df_full_screening %>%
  select(any_of(c(ds_candidates, code_candidates))) %>%
  mutate(across(everything(), ~ {
    y <- as.character(.)
    y <- str_squish(y)
    y[y %in% c("", "NA", "N/A", "-")] <- NA_character_
    y
  }))
coalesce_candidates <- function(data, candidates) {
  present <- candidates[candidates %in% names(data)]
  if (length(present) == 0) {
    return(rep(NA_character_, nrow(data)))
  }
  Reduce(dplyr::coalesce, lapply(present, function(nm) data[[nm]]))
}
ds_raw   <- coalesce_candidates(df_sel, ds_candidates)
code_raw <- coalesce_candidates(df_sel, code_candidates)
ds_avail   <- !is.na(ds_raw)
code_avail <- !is.na(code_raw)
both_avail <- ds_avail & code_avail
flag_df <- tibble(
  Dataset = ds_avail,
  Code    = code_avail,
  Both    = both_avail
)
summary_df <- flag_df %>%
  pivot_longer(everything(), names_to = "Type", values_to = "Reported") %>%
  mutate(Status = if_else(Reported, "Reported", "Missing")) %>%
  count(Type, Status, name = "n") %>%
  group_by(Type) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup()
p <- ggplot(summary_df, aes(x = Type, y = n, fill = Status)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_fill(vjust = 0.5), size = 4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = NULL, y = "Share of Papers", fill = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x  = element_text(hjust = 1,size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 14),
    
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )
print(p)
ggsave(file.path(im_path,"dataset_code_both_availability_share.png"),
       plot = p, width = 8, height = 5.5, dpi = 300)

