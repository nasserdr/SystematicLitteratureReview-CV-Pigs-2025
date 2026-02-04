
################################################################################
#Performance metrics boxplots
metric_cols <- c(
  "Accuracy","Precision","Recall","Specificity","Sensitivity","AUC","MAE","DPPI",
  "MSE","RMSE","PCK","ROC","Corr_Coeff","F-measure","F1-score","MAP","MAP50",
  "MAp50;95","FLOP","GFLOP","BFLOPS","MAR","OKS","FPS","HOTA","OTA","OTP","MOTP",
  "ORP","TRP","FAR","MIOU","MOTA","AP50","AP75","AP50-95","TIOU","IOU","R2",
  "ID switch","MMA","PCA","TAR","FPR","MAD","IMA","MAPE","MT","ML","MPCK","IDF1"
)
alias_map <- c(
  "F1-score"    = "F1-score",
  "F-measure"   = "F1-score",    # treat as same as F1 if you want; comment this to keep separate
  "AP50-95"     = "mAP50-95",
  "MAp50;95"    = "mAP50-95",
  "Corr_Coeff"  = "Correlation",
  "ID switch"   = "ID_switch"
)
present_metrics <- intersect(metric_cols, names(df_full_screening))
df_metrics <- df_full_screening %>%
  select(any_of(present_metrics))
metrics_long <- df_metrics %>%
  pivot_longer(
    cols = everything(),
    names_to = "Metric",
    values_to = "raw"
  ) %>%
  mutate(Metric = ifelse(Metric %in% names(alias_map), alias_map[Metric], Metric)) %>%
  mutate(raw = as.character(raw)) %>%
  mutate(raw = na_if(str_squish(raw), "")) %>%
  mutate(value = suppressWarnings(parse_number(raw))) %>%
  filter(!is.na(value))  # keep only valid numbers
metric_order <- metrics_long %>%
  group_by(Metric) %>%
  summarise(
    range_val = max(value, na.rm = TRUE) - min(value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(range_val), desc(n)) %>%
  pull(Metric)
metrics_long <- metrics_long %>%
  mutate(Metric = factor(Metric, levels = metric_order))
p <- ggplot(metrics_long, aes(x = value, y = Metric)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.6) +
  labs(
    x = "Metric Value",
    y = "Metric Name"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold")
  )
h_inches <- 4 + 0.28 * length(unique(metrics_long$Metric))
p
ggsave(file.path(im_path,"performance_metrics_boxplots_sorted_by_range.png"),
       plot = p, width = 12, height = h_inches, dpi = 300)


################################################################################
alias_map <- c(
  "F1-score"    = "F1-score",
  "F-measure"   = "F1-score",
  "AP50-95"     = "mAP50-95",
  "MAp50;95"    = "mAP50-95",
  "Corr_Coeff"  = "Correlation",
  "ID switch"   = "ID_switch"
)

present_metrics <- intersect(metric_cols, names(df_full_screening))

df_metrics <- df_full_screening %>%
  select(any_of(present_metrics))

metric_usage <- df_metrics %>%
  summarise(across(everything(), ~ sum(!is.na(.) & str_squish(as.character(.)) != ""))) %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Count") %>%
  mutate(Metric = ifelse(Metric %in% names(alias_map), alias_map[Metric], Metric)) %>%
  group_by(Metric) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  arrange(desc(Count)) %>%
  mutate(perc = round(100 * Count / sum(Count), 1))   # add percentage


p_usage <- ggplot(metric_usage, aes(x = fct_reorder(Metric, Count), y = Count)) +
  geom_col(fill = "#4E79A7", width = 0.75) +                       # same color
  geom_text(aes(label = paste0(Count)),
            hjust = -0.1, size = 4) +                              # counts + %
  coord_flip(clip = "off") +                                       # let labels overflow
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +      # room on right for labels
  labs(
    x = "Metric",
    y = "Number of Studies"
  ) +
  theme_classic(base_size = 13) +                                  # match earlier plots
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.margin = margin(10, 40, 10, 10)                           # avoid clipping on save
  )

print(p_usage)
ggsave(file.path(im_path,"metric_usage_barplot.png"),
       plot = p_usage, width = 10, height = 8, dpi = 300)

