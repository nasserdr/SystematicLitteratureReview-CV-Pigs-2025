################################################################################
#Distribution of CV tasks
df <- df_full_screening %>%
  select(CV_Task) %>%
  mutate(CV_Task = str_to_title(str_squish(CV_Task))) %>%
  filter(!is.na(CV_Task), CV_Task != "") %>%
  mutate(CV_Task = str_remove_all(CV_Task, "\\s*\\([^)]+\\)")) %>% 
  separate_rows(CV_Task, sep = "\\s*(?:,|;|/|&|\\band\\b)\\s*") %>%
  mutate(CV_Task = str_squish(CV_Task)) %>%
  filter(CV_Task != "")

tasks_count <- df %>%
  count(CV_Task, name = "n") %>%
  mutate(perc = round(100 * n / sum(n), 1),
         CV_Task = fct_reorder(CV_Task, n))

p <- ggplot(tasks_count, aes(x = CV_Task, y = n)) +
  geom_col(fill = "#4E79A7") +                # single color
  geom_text(aes(label = paste0(n, " (", perc, "%)")),
            hjust = -0.1, size = 4) +                      # push labels to the right of bars
  coord_flip(clip = "off") +                               # allow labels beyond plot panel
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +  # extra room on right
  labs(x = "Computer Vision Task", y = "Number of Studies") +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 30, 10, 10)                   # more right margin
  )

print(p)
ggsave(file.path(im_path, "cv_tasks_distribution.tiff"),
       plot = p, width = 10, height = 6, dpi = 300)

################################################################################
#Distribution of Family of models used
models_long <- df_full_screening %>%
  select(Family) %>%
  mutate(Family = as.character(Family)) %>%
  filter(!is.na(Family) & str_squish(Family) != "") %>%
  separate_rows(Family, sep = ",") %>%
  mutate(Family = str_squish(Family)) %>%
  mutate(Family = str_to_title(Family)) %>%
  group_by(Family) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count)) %>%
  mutate(perc = round(100 * Count / sum(Count), 1))   # ⬅️ add percentage column

models_ge2 <- models_long %>% 
  filter(Count >= 2) %>% 
  filter(Family != 'Other')

if (nrow(models_ge2) == 0) {
  stop("No model families appear at least twice. Relax the threshold or check the 'Family' column.")
}

p_models <- ggplot(models_ge2, aes(x = fct_reorder(Family, Count), y = Count)) +
  geom_col(fill = "#4E79A7") +
  geom_text(aes(label = paste0(Count, " (", perc, "%)")),
            hjust = -0.1, size = 4) +      # ⬅️ add labels outside bars
  coord_flip(clip = "off") +               # allow labels beyond the panel
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +  # extra space for labels
  labs(
    x = "Model Family",
    y = "Number of Studies"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.margin = margin(10, 30, 10, 10)  # room for right-side labels
  )

print(p_models)

ggsave(file.path(im_path,"models_family_distribution.tiff"), 
       plot = p_models, width = 10, height = 8, dpi = 300)


################################################################################
#Evolution of CNN/transformers with time
df_flags <- df_full_screening %>%
  select(Year, CNN_Network_Name, Transformer_Name) %>%
  mutate(
    Year = as.integer(Year),
    CNN_Network_Name = ifelse(is.na(CNN_Network_Name), "", CNN_Network_Name),
    Transformer_Name = ifelse(is.na(Transformer_Name), "", Transformer_Name)
  ) %>%
  mutate(CNN_Network_Name = str_squish(CNN_Network_Name),
         Transformer_Name = str_squish(Transformer_Name)) %>%
  mutate(
    has_cnn = CNN_Network_Name != "" & str_detect(CNN_Network_Name, "\\S"),
    has_trans = Transformer_Name != "" & str_detect(Transformer_Name, "\\S")
  ) %>%
  filter(!is.na(Year))
per_year_counts <- df_flags %>%
  group_by(Year) %>%
  summarise(
    CNN_uses  = sum(has_cnn, na.rm = TRUE),
    Trans_uses = sum(has_trans, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(CNN_uses, Trans_uses),
               names_to = "Family", values_to = "count") %>%
  mutate(
    Family = recode(Family,
                    CNN_uses = "CNN",
                    Trans_uses = "Transformer")
  )
p <- ggplot(per_year_counts, aes(x = factor(Year), y = count, fill = Family)) +
  geom_col(position = "fill", width = 0.75) +      # relative percentages per year
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Year",
    y = "Share of Usage",
    fill = "Model Family"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    axis.title   = element_text(face = "bold"),
    legend.position = "top"
  )
print(p)
ggsave(file.path(im_path,"cnn_vs_transformer_share_by_year.tiff"),
       plot = p, width = 9, height = 6.5, dpi = 300)


# CV tasks Vs Downsteam models
df_filtered <- df_full_screening %>%
  filter(Downstream_Model != "" & !is.na(Downstream_Model)) %>% 
  select(CV_Task, Downstream_Model) %>% 
  separate_rows(CV_Task, sep = ",") %>%
  # Remove leading/trailing spaces that may appear after splitting
  mutate(CV_Task = trimws(CV_Task)) %>%
  group_by(CV_Task, Downstream_Model) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  arrange(desc(Frequency))

plot_df <- df_filtered %>%
  group_by(CV_Task, Downstream_Model) %>%
  mutate(Downstream_Model = str_remove_all(Downstream_Model, "\\s*\\([^)]+\\)")) %>% 
  mutate(CV_Task = str_remove_all(CV_Task, "\\s*\\([^)]+\\)")) %>% 
  summarise(Frequency = sum(Frequency, na.rm = TRUE), .groups = "drop")

# 3) Order downstream models by total frequency (so most used appear first on X-axis)
plot_df <- plot_df %>%
  group_by(Downstream_Model) %>%
  mutate(TotalPerModel = sum(Frequency)) %>%
  ungroup() %>%
  mutate(Downstream_Model = fct_reorder(Downstream_Model, TotalPerModel, .desc = TRUE))

# 4) Stacked bar plot (Downstream Models on x-axis, CV_Tasks in legend)
p <- ggplot(plot_df, aes(x = Downstream_Model, y = Frequency, fill = CV_Task)) +
  geom_col(position = "stack", width = 0.7) +
  scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
  labs(
    x = "Downstream Model",
    y = "Count",
    fill = "CV Task"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  ) + theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 20))


print(p)
ggsave(file.path(im_path,"downstream_cvtask_stacked_bar.tiff"), p, width = 15, height = 6.5, dpi = 300)
