
################################################################################
#Distribution of Challenges
challenges <- c(
  "Occlusion", "Overlapping", "Behavior Complexity", "Lighting",
  "Camera Position", "Image Quality", "Posture Recognition",
  "Hardware Limitation", "Pen Environment"
)

# Count non-empty entries for each challenge
challenge_counts <- df_full_screening %>%
  summarise(across(all_of(challenges), ~ sum(!is.na(.x) & .x != ""))) %>%
  pivot_longer(cols = everything(), names_to = "Challenge", values_to = "Count") %>%
  arrange(desc(Count)) %>%
  mutate(perc = round(100 * Count / sum(Count), 1))   # ⬅️ add percentage

# Plot
p_challenges <- ggplot(challenge_counts, aes(x = reorder(Challenge, Count), y = Count)) +
  geom_col(fill = "#4E79A7", width = 0.75) +                               # consistent color
  geom_text(aes(label = paste0(Count, " (", perc, "%)")),
            hjust = -0.1, size = 4) +                                     # show counts + %
  coord_flip(clip = "off") +                                              # allow labels outside
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +             # room on right
  labs(
    x = "Challenge Type",
    y = "Number of Studies Mentioning Challenge"
  ) +
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

print(p_challenges)

ggsave(file.path(im_path,"challenges_distribution.png"),
       plot = p_challenges, width = 10, height = 7, dpi = 300)

