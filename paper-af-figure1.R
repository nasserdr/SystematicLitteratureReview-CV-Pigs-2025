dir <- "~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/01_Projekte/2025-Vladimir_PigsBehavior/2_Goals/3_AF_Comparison/"
file_path <- file.path(dir, "citations.xlsx")
df <- read_excel(path = file_path)



data <- data.frame(
  Year = c("Y0", "Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7"),
  DLC_Mathis_2018 = c(6, 71, 199, 400, 513, 529, 589, 428),
  DLC_Nath_2019   = c(68, 140, 149, 164, 168, 129, NA, NA),
  DLC_Lauer_2022  = c(19, 69, 93, 71, NA, NA, NA, NA),
  SLEAP_Pereira_2022 = c(31, 84, 105, 108, NA, NA, NA, NA)
)

# Reshape into long format for ggplot2
data_long <- pivot_longer(
  data,
  cols = -Year,
  names_to = "Framework",
  values_to = "Citations"
)

# Optional: clean up names for the legend
data_long$Framework <- factor(data_long$Framework,
                              levels = c("DLC_Lauer_2022", "DLC_Mathis_2018", "DLC_Nath_2019", "SLEAP_Pereira_2022"),
                              labels = c("DLC. Lauer et al. 2022",
                                         "DLC. Mathis et al. 2018",
                                         "DLC. Nath et al. 2019",
                                         "SLEAP. Pereira et al. 2022")
)

# Plot
ggplot(data_long, aes(x = Year, y = Citations, color = Framework, group = Framework)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Year after publication",
    y = "Number of citations"
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = c(
    "DLC. Lauer et al. 2022" = "red",
    "DLC. Mathis et al. 2018" = "green",
    "DLC. Nath et al. 2019" = "blue",
    "SLEAP. Pereira et al. 2022" = "purple"
  ))

ggsave(filename = file.path(dir, "citation_comparison_plot.png"), width = 10, height = 6, dpi =300)
