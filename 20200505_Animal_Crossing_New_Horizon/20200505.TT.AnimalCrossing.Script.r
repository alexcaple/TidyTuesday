### TidyTuesday Challenge: Animal Crossing New Horizons
# Date: 5/5/2020
# Topic: ACNH
# Source: VilligarDB & Metacritic
# Location: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-05-05/readme.md

# Library Load ------------------------------------------------------------
# Base
library(readxl)
library(writexl)
library(tidyr)
library(dplyr)

# Specific to Project
library(tidytuesdayR) # Latest Version; says it needs updates!
library(lubridate)

# Graphical
library(ggplot2)
library(scales)
library(Cairo)
library(extrafont)
font_import(pattern = "calibri", prompt = FALSE)

# Here --------------------------------------------------------------------
library(here)

# Source ------------------------------------------------------------------
# None at this time.

# Functions ---------------------------------------------------------------
# None at this time.

# Data Load ---------------------------------------------------------------
ac_critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
ac_user <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
ac_items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
ac_vil <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

# Descriptive Review ------------------------------------------------------
head(ac_critic)
dim(ac_critic) # 107 reviews.

head(ac_items)
View(ac_items)

# Analysis ----------------------------------------------------------------


# Plot Analysis -----------------------------------------------------------



# Save Out Plot -----------------------------------------------------------

#Cairo(2700, 2100, file=here("20200505_Animal_Crossing_New_Horizon","Image",""), type="png", bg="white", dpi=300)

#dev.off()


