### TidyTuesday Challenge: 
# Date: 5/3/2020
# Topic: Broadway Weekly Grosses
# Source: Alex Cookson & Playbill
# Location: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-28/readme.md

# Library Load ------------------------------------------------------------
# Base
library(readxl)
library(writexl)
library(tidyr)
library(dplyr)
library(lubridate)

# Specific to Project
library(tidytuesdayR) # Latest Version; says it needs updates!

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
bg_gross <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
bg_show <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
bg_cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
bg_pre85 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')

# Descriptive Review ------------------------------------------------------
# CPI Review
head(bg_cpi)

bg_cpi %>% 
  mutate(Dupes = duplicated(year_month)) %>% 
  group_by(Dupes) %>% 
  summarize(Dupe.Check = n()) # No Duplicate Values

bg_cpi %>% 
  mutate(Miss = is.na(cpi)) %>% 
  group_by(Miss) %>% 
  summarize(`Missing Check` = n()) # None Missing

ggplot(data=bg_cpi, aes(x=cpi, y=year_month)) + geom_line() # No spikes; appears clean

# Weekly Gross
head(bg_gross)
str(bg_gross)

# week_ending
# show
# 


# Start date for shows starting earlier than 85; not used given no gross.
head(bg_pre85)

# Explanation of each show; not used since we don't want to explore text this week.
head(bg_show)

#  * * Clean Weekly Gross Data --------------------------------------------

bg_gross_clean <- bg_gross %>% 

# Analysis ----------------------------------------------------------------


# Plot Analysis -----------------------------------------------------------



# Save Out Plot -----------------------------------------------------------

#Cairo(2700, 2100, file=here("20200503_Broadway_Grosses","Image",""), type="png", bg="white", dpi=300)

#dev.off()


