
#Distribution of the critical review scores
#####
df_critical <- df_critical %>%
  mutate(Inclusion = ifelse(`Total Score` >= 8, "Included (≥8)", "Excluded (<8)"))
p <- ggplot(df_critical, aes(x = `Total Score`, fill = Inclusion)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.9) +
  geom_vline(xintercept = 7.5, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    x = "Total Score",
    y = "Number of Studies",
    fill = NULL       # or use fill = "" to hide the legend title
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )
print(p)
ggsave(file.path(im_path, "critical_appraisal_distribution.png"), 
       plot = p, width = 8, height = 6, dpi = 300)


#Journal distribution
df_j <- df_full_screening %>%
  select(Journal) %>%
  mutate(
    Journal = as.character(Journal),
    Journal = str_squish(Journal),
    Journal = na_if(Journal, ""),
    Journal = ifelse(Journal %in% c("NA","N/A","-"), NA, Journal)
  ) %>%
  filter(!is.na(Journal))

journal_counts <- df_j %>%
  count(Journal, name = "n") %>%
  arrange(desc(n)) %>%
  mutate(pct = n / sum(n))   # % of ALL journals (kept after filtering below)

journal_counts_filtered <- journal_counts %>%
  filter(n > 3)

p <- ggplot(journal_counts_filtered, aes(x = fct_reorder(Journal, n), y = n)) +
  geom_col(fill = "#4E79A7", width = 0.75) +                          # same color
  geom_text(aes(label = paste0(n, " (", percent(pct, accuracy = 0.1), ")")),
            hjust = -0.1, size = 4) +                                 # labels to the right
  coord_flip(clip = "off") +                                          # let labels overflow
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +         # more room on right
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35)) +
  labs(x = "Journals (appearing more than 3 times)", y = "Number of Papers") +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 45, 10, 10)                              # bigger right margin
  )

print(p)
ggsave(file.path(im_path,"journal_distribution_gt3.png"),
       plot = p, width = 10, height = 7, dpi = 300)

# TopK by methods and applications:
top_k <- 3       # <<— number of journals to keep per facet
keep_ties <- TRUE # set FALSE to cut strictly at top_k even if there are ties

# 1) Prepare + classify papers ----------------------------------------------
df_j2 <- df_full_screening %>%
  select(Journal, Methods) %>%
  mutate(
    Journal = as.character(Journal),
    Journal = str_squish(Journal),
    Journal = na_if(Journal, ""),
    Journal = ifelse(Journal %in% c("NA", "N/A", "-"), NA, Journal),
    paper_type = if_else(
      is.na(Methods) | tolower(str_squish(Methods)) == "no",
      "Application",
      "Method"
    )
  ) %>%
  filter(!is.na(Journal))

# 2) Count within facet + keep top_k per facet ------------------------------
journal_counts_by_type <- df_j2 %>%
  count(paper_type, Journal, name = "n") %>%
  group_by(paper_type) %>%
  mutate(pct = n / sum(n)) %>%
  {
    if (keep_ties) {
      # Keep all journals whose n is >= the nth largest within each facet
      thr <- summarise(., thr = sort(unique(n), decreasing = TRUE)[pmin(top_k, length(unique(n)))])
      left_join(., thr, by = "paper_type") %>%
        filter(n >= thr) %>%
        select(-thr)
    } else {
      slice_max(., order_by = n, n = top_k, with_ties = FALSE)
    }
  } %>%
  ungroup() %>%
  mutate(Journal_re = tidytext::reorder_within(Journal, n, paper_type))


# 3) Plot --------------------------------------------------------------
p_topk <- ggplot(journal_counts_by_type, aes(x = Journal, y = n)) +
  geom_col(fill = "#4E79A7", width = 0.75) +
  geom_text(aes(label = paste0(n, " (", percent(pct, accuracy = 0.1), ")")),
            hjust = -0.1, size = 4) +
  coord_flip(clip = "off") +
  facet_wrap(~ paper_type, nrow = 1, scales = "free_y") +
  tidytext::scale_x_reordered(labels = function(x) str_wrap(x, width = 35)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    x = "Journal Name",
    y = "Number of Papers"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 45, 10, 10)
  )

print(p_topk)

ggsave(file.path(im_path, paste0("journal_distribution_top", top_k, "_by_type.png")),
       plot = p_topk, width = 12, height = 7, dpi = 300)

#Top 25 authors by paper appearances
df_auth <- df_full_screening %>%
  select(Author) %>%
  mutate(Author = as.character(Author))

authors_long <- df_auth %>%
  filter(!is.na(Author), Author != "") %>%
  mutate(Author = str_squish(Author)) %>%
  separate_rows(Author, sep = "\\s*(?:\\band\\b|&|;)\\s+") %>%  # split on 'and', '&', ';'
  mutate(
    Author = str_squish(Author),
    Author = str_replace_all(Author, "\\s+", " "),
    Author = str_remove(Author, "\\s*\\.$")
  ) %>%
  filter(!str_detect(Author, regex("^et\\.?\\s*al\\.?$", ignore_case = TRUE))) %>%
  filter(Author != "")

author_counts <- authors_long %>%
  count(Author, name = "n") %>%
  arrange(desc(n)) %>%
  mutate(perc = round(100 * n / sum(n), 1))   # ⬅️ Add percentage column

topN <- 25
author_counts_top <- author_counts %>%
  slice_max(n, n = topN, with_ties = TRUE) %>%
  mutate(Author = fct_reorder(Author, n))

p <- ggplot(author_counts_top, aes(x = Author, y = n)) +
  geom_col(fill = "#4E79A7", width = 0.75) +                # ⬅️ same blue color
  geom_text(aes(label = paste0(n, " (", perc, "%)")), 
            hjust = -0.1, size = 4) +                       # ⬅️ show counts + %
  coord_flip(clip = "off") +                                # allow labels beyond bars
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + # space for labels
  labs(
    x = "Author",
    y = "Number of Papers"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 30, 10, 10)                    # extra right margin
  )

print(p)

ggsave(file.path(im_path,"author_appearances_top25.png"),
       plot = p, width = 9, height = 6, dpi = 300)


# Distribution of animal categories
df <- df_full_screening %>%
  rename(`Animal Category` = `Anmial Category`) %>%
  mutate(`Animal Category` = str_to_title(trimws(`Animal Category`))) %>%
  separate_rows(`Animal Category`,
                sep = "\\s*(?:,|;|/|&|\\band\\b)\\s*") %>%
  mutate(`Animal Category` = str_squish(`Animal Category`)) %>%
  filter(`Animal Category` != "")
animal_counts <- df %>%
  count(`Animal Category`) %>%
  mutate(perc = round(100 * n / sum(n), 1))
p <- ggplot(animal_counts,
            aes(x = reorder(`Animal Category`, -n), y = n, fill = `Animal Category`)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(n, " (", perc, "%)")),
            vjust = -0.4, size = 4) +
  labs(x = "Animal Category",
       y = "Number of Studies") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")
print(p)
ggsave(file.path(im_path, "animal_distribution.png"), 
       plot = p, width = 8, height = 6, dpi = 300)

