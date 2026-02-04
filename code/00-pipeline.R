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
dir <- getwd()
file_path <- file.path(dir, "data/SLR-V8.xlsx")
# df_all_papers <- read_excel(path = file_path, sheet = 'All papers')
# df_metadata <- read_excel(path = file_path, sheet = 'Metadata-Abstract Screening')
# df_full_text_e <- read_excel(path = file_path, sheet = 'Full Text Exclusion')
df_critical <- read_excel(path = file_path, sheet = 'Critical Appraisal')
df_full_screening <- read_excel(path = file_path, sheet = 'Full text screening')
df_lookup <- read_excel(path = file_path, sheet = 'Behavior Explanation')
im_path = file.path(dir, "manuscript_tex/figures_produced_by_code")
dir.create(im_path, showWarnings = FALSE)

im_spare = file.path(dir, "manuscript_tex/figures_spare")
dir.create(im_spare, showWarnings = FALSE)


message("=== Starting Analysis Pipeline ===")

# Source all components
source(file.path(dir, "code/01-Intro.R"))                    # Introduction and setup
source(file.path(dir, "code/02-RQ1.R"))                      # Research Question 1
source(file.path(dir, "code/03-RQ2.R"))                      # Research Question 2  
source(file.path(dir, "code/04-RQ3.R"))                      # Research Question 3
source(file.path(dir, "code/05-RQ4.R"))                      # Research Question 4
source(file.path(dir, "code/06-RQY-Behvior-Heatmaps.R"))             # Behavior analysis
source(file.path(dir, "code/07-RQZ-FurtherQuestions.R"))     # Additional questions

message("=== Image Production Completed ===")
