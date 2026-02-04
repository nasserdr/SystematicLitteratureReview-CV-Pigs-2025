library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(forcats)
library(tibble)
library(networkD3)
library(ggalluvial)
library(scales)
library(readr)
library(ggrepel)
library(ggfittext)
library(patchwork)
library(ggh4x)
library(tidytext)
library(cowplot)

#Load dataset
dir <- "~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/01_Projekte/2025-Vladimir_PigsBehavior/2_Goals/2_ReviewPaper/"
file_path <- file.path(dir, "SLR-V8.xlsx")
# df_all_papers <- read_excel(path = file_path, sheet = 'All papers')
# df_metadata <- read_excel(path = file_path, sheet = 'Metadata-Abstract Screening')
# df_full_text_e <- read_excel(path = file_path, sheet = 'Full Text Exclusion')
df_critical <- read_excel(path = file_path, sheet = 'Critical Appraisal')
df_full_screening <- read_excel(path = file_path, sheet = 'Full text screening')
df_lookup <- read_excel(path = file_path, sheet = 'Behavior Explanation')
im_path = file.path(dir, "figures")
dir.create(im_path, showWarnings = FALSE)


message("=== Starting Analysis Pipeline ===")

# Source all components
source("Intro.R")                    # Introduction and setup
source("RQ1.R")                      # Research Question 1
source("RQ2.R")                      # Research Question 2  
source("RQ3.R")                      # Research Question 3
source("RQ4.R")                      # Research Question 4
source("RQY-Behavior.R")             # Behavior analysis
source("RQZ-FurtherQuestions.R")     # Additional questions
source("others.R")                   # Utility functions

message("=== Analysis Pipeline Complete ===")
