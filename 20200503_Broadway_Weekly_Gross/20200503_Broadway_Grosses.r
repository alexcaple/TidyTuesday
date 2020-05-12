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
library(png)
library(reshape2)

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
# * Broadway Gross Data From Tidy Tuesday ---------------------------------
bg_gross <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
bg_show <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
bg_cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
bg_pre85 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')

# * American Airlines Theatre Seating Chart Load + Transform ----------------

download.file("https://seatplan.com/uploads/seatmaps/american-airlines-theatre-seating-plan-new-york.png?1556796643", here("20200503_Broadway_Weekly_Gross", "American_Airlines_Theatre_Seating_Chart.png"), mode = 'wb')
aa_seats <- readPNG(here("20200503_Broadway_Weekly_Gross", "American_Airlines_Theatre_Seating_Chart.png"))
grid::grid.raster(aa_seats)

# To create the graph, as I know no other way, I'm doing manually for now - other conversion packages seem like cheats for the purpose of TidyTuesday
#  * * Orchestra Section ----------------------------------------------------
# * AA 12
aa_stage_list <- list(aa = data.frame(y=rep(0, 12), x=c(14.5:25.5)))
# * A 3, 13
aa_stage_list$a <- data.frame(y=rep(-1, 19), x=c(10:12,14:26,28:30))
# * B 4, 14
aa_stage_list$b <- data.frame(y=rep(-2, 22), x=c(8.5:11.5, 13.5:26.5, 28.5:31.5))
# * C 5, 13
aa_stage_list$c <- data.frame(y=rep(-3, 23), x=c(8:12, 14:26, 28:32))
# * D 6, 14
aa_stage_list$d <- data.frame(y=rep(-4, 26), x=c(6.5:11.5, 13.5:26.5, 28.5:33.5))
# * E 7, 15
aa_stage_list$e <- data.frame(y=rep(-5, 29), x=c(4:11, 13:27, 29:34))
# * F 8, 14
aa_stage_list$f <- data.frame(y=rep(-6, 30), x=c(4.5:11.5, 13.5:26.5, 28.5:35.5))
# * G 9, 15
aa_stage_list$g <- data.frame(y=rep(-7, 33), x=c(3:11, 13:27, 29:37)) 
# * H 11, 16 
aa_stage_list$h <- data.frame(y=rep(-8, 38), x=c(1:11, 13:27, 29:40))
# * j 12, 15
aa_stage_list$j <- data.frame(y=rep(-9, 39), x=c(0.5:11.5, 13.5:26.5, 28.5:40.5)) 
# * K 12, 16
aa_stage_list$k <- data.frame(y=rep(-10, 40), x=c(0:11, 13:27, 29:41))
# * L 12, 15
aa_stage_list$l <- data.frame(y=rep(-11, 39), x=c(0.5:11.5, 13.5:26.5, 28.5:40.5))
# * M 8, 14
aa_stage_list$m <- data.frame(y=rep(-12, 30), x=c(4:11, 13:26, 28:35))
# * N 8, 13
aa_stage_list$n <- data.frame(y=rep(-13, 29), x=c(4.5:11.5, 13.5:25.5, 28.5:35.5))# Here
# * O 8, 14
aa_stage_list$o <- data.frame(y=rep(-14, 30), x=c(3:11, 13:26, 28:34))
# * P 7, 6
aa_stage_list$p <- data.frame(y=rep(-15, 20), x=c(5.5:11.15, 14.5, 16.5, 18.5, 20.5, 22.5, 24.5, 28.5:35.5))

# * * Mezzanine Section ------------------------------------------------ 
# Rows = 6
# * Q, S, U 41
aa_stage_list$q <- data.frame(y=rep(-19, 41), x=c(1:41))
aa_stage_list$s <- data.frame(y=rep(-21, 41), x=c(1:41))
aa_stage_list$u <- data.frame(y=rep(-23, 41), x=c(1:41 ))
# * R, T 40
aa_stage_list$r <- data.frame(y=rep(-20, 40), x=c(1.5:40.5))
aa_stage_list$t <- data.frame(y=rep(-22, 40), x=c(1.5:40.5))
# * V = 1-11, 17-40
aa_stage_list$v <- data.frame(y=rep(-24, 34), x=c(1.5:11.5, 17.5:39.5))
# * W = 20
aa_stage_list$w <- data.frame(y=rep(-25, 20), x=c(18:37))

# Combine List into dataframe
aa_stage <- bind_rows(aa_stage_list)

aa_stage <- aa_stage %>% 
  arrange(-y) %>% 
  mutate(UID = 1:length(y))

# * * * Basic American Airlines Plot --------------------------------------
ggplot(aa_stage, aes(x=x, y=y)) + 
  geom_point(color="black", fill="grey50", shape=21, size=10) +
  annotate("text", 
           x = 19, 
           y = 1.5, 
           label = 'bold("Stage")',
           hjust = 0, # LEFT ALIGN
           parse = TRUE,
           family="Calibri",
           size=10) +
  annotate("text", 
           x = 18, 
           y = -17, 
           label = 'bold("Mezzanine")',
           hjust = 0, # LEFT ALIGN
           parse = TRUE,
           family="Calibri",
           size=10) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.grid.major.x=element_blank(), 
        panel.grid.major.y=element_blank(),
        panel.background = element_rect(fill ="white",colour = NA))

# Descriptive Data Review ------------------------------------------------------
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

# How many seats in each theatre?
bg_gross %>% 
  select(theatre, seats_in_theatre) %>% 
  distinct(theatre, .keep_all = TRUE) %>% 
  arrange(-seats_in_theatre) # Gershwin Theatre is largest!

bg_gross %>% 
  group_by(theatre, show) %>% 
  distinct(theatre, show) %>% 
  mutate(Count = 1) %>% 
  ungroup(.) %>% 
  group_by(theatre) %>% 
  summarize(Total = sum(Count)) %>% 
  arrange(-Total) # American Airlines Theatre had the most shows

bg_gross %>% 
  filter(theatre %in% c("Gershwin Theatre", "American Airlines Theatre"),
         performances>6) %>%  # Some shows are missing data, which since I don't know this data set well enough I'll remove for now; should research if we have more time later.
  group_by(theatre) %>% 
  mutate(Cap = (seats_sold/performances)/seats_in_theatre,
         Min = min(Cap),
         Median = median(Cap),
         Avg = mean(Cap),
         Max = max(Cap)) %>% 
  select(theatre, performances, seats_sold, seats_in_theatre, Cap, Min, Median, Avg, Max)

bg_gross %>% 
  filter(theatre %in% c("Gershwin Theatre", "American Airlines Theatre"),
         performances>6) %>% 
  group_by(theatre) %>% 
  mutate(Cap = (seats_sold/performances)/seats_in_theatre) %>% 
  filter(Cap>1) %>% 
  select(theatre, performances, seats_sold, seats_in_theatre, Cap) # Interesting; either American Airlines misreported seats sold, or sold additional tickets night of to fill these seats. Would need to know more!

bg_gross %>% 
  filter(theatre %in% c("American Airlines Theatre")) %>% 
  select(seats_in_theatre) %>% 
  summary()

bg_gross %>% 
  filter(theatre %in% c("Gershwin Theatre")) %>% 
  select(seats_in_theatre) %>% 
  summary() # Missing Data in one year on number of seats in theatre.
  
  
# Start date for shows starting earlier than 85; not used given no gross.
head(bg_pre85)

# Explanation of each show; not used since we don't want to explore text this week.
head(bg_show)

# Clean Weekly Gross Data -------------------------------------------------

aa_gross_subset <- bg_gross %>% 
  filter(theatre %in% c("American Airlines Theatre")) %>% 
  select(week_ending, theatre, show, performances, seats_sold, seats_in_theatre)

# Performance Check for 0s 
summary(aa_gross_subset$performances)
table(aa_gross_subset$performances)

aa_gross_subset %>% 
  group_by(show) %>% 
  mutate(Avg.Performance = ceiling(performances)) %>% 
  distinct(show, .keep_all = TRUE) %>% 
  select(show, Avg.Performance)

# Shucks, all data on the # of performances for some shows is missing...

aa_gross_subset %>% 
  group_by(show) %>% 
  filter(performances>0) %>% 
  mutate(Avg.Seats.Sold = mean(seats_sold),
         Avg.P = mean(performances))%>% 
  distinct(show, .keep_all = TRUE) %>% 
  select(show, Avg.Seats.Sold, Avg.P) %>% 
  as.data.frame() # So close to 8 across the board we're going to assume all missing performances is 8 shows a week.

# Seats Sold Check for 0s
summary(aa_gross_subset$seats_sold) # No 0s, no missing

# Seat Capacity Check
summary(aa_gross_subset$seats_in_theatre) # No 0s, no missing

# Clean Performances
aa_gross_subset <- aa_gross_subset %>% 
  mutate(performances = ifelse(performances == 0, 8, as.numeric(performances)))

# Plot Creation -----------------------------------------------------------

dim(aa_stage)
head(aa_stage)

plot_aa_full <- aa_gross_subset %>% 
         mutate(Utilization = ifelse(ceiling((seats_sold/performances)/seats_in_theatre)>1, 716, round(((seats_sold/performances)/seats_in_theatre)*714, 0))) %>% 
  group_by(show) %>% 
  summarize(Utilization = round(mean(Utilization), 0)) %>% 
  mutate(UID = 716) %>% 
  uncount(UID) %>% # Increases the number of rows to match the Utilization values
  group_by(show) %>% 
  mutate(seats = 1:length(show),
         Connect.Utilization = 1:length(show),
         Connect.Utilization = paste0(show, Connect.Utilization))

plot_aa_utilization <- aa_gross_subset %>% 
  mutate(Utilization = ifelse(ceiling((seats_sold/performances)/seats_in_theatre)>1, 716, round(((seats_sold/performances)/seats_in_theatre)*714, 0))) %>% 
  group_by(show) %>% 
  summarize(Utilization = round(mean(Utilization), 0)) %>% 
  select(Utilization, show) %>% 
  uncount(Utilization) %>%
  group_by(show) %>% 
  mutate(Connect.Utilization = 1:length(show),
         Connect.Utilization = paste0(show, Connect.Utilization),
         Plot.Color = 1) %>% 
  ungroup(.) %>% 
  select(Connect.Utilization, Plot.Color)

plot_aa_full <- merge(plot_aa_full,
                      plot_aa_utilization,
                      by="Connect.Utilization",
                      all.x=TRUE)

plot_aa_full <- plot_aa_full %>% 
  select(show, Utilization, Plot.Color, seats) %>% 
  replace(., is.na(.), 0) %>% 
  arrange(-Utilization, show, seats)

plot_factor_order <- unique(plot_aa_full$show) 

plot_aa_full <- plot_aa_full %>% 
  mutate(show = factor(show, levels = plot_factor_order))

plot_stage_final <- merge(plot_aa_full,
                          aa_stage,
                          by.x="seats",
                          by.y="UID",
                          all.x=TRUE)


#  * American Airlines Stage Plot -----------------------------------------

final_plot <- ggplot(plot_stage_final, aes(x=x, y=y)) + 
  geom_point(aes(fill=as.factor(Plot.Color)), color="grey50", shape=21, size=1.25) +
  scale_fill_manual(values=c("black", "#0da5ff"))+
  labs(title="The Pajama Game Stole The Show at the American Airlines Theater",
       subtitle="On average, The Pajama Game was nearly sold out every night at the theatre, followed closely by The Price and The Women.\nShows below are ordered by average attendance from highest average seat utilization to lowest, at As Long as We Both Shall Laugh.",
       caption="Data source: Playbill, via Alex Cookson in the #TidyTuesday Challenge on Twitter.")+
  annotate("text", 
           x = 20, 
           y = 3, 
           label = 'bold("Stage")',
           hjust = .5, # LEFT ALIGN
           parse = TRUE,
           family="Calibri",
           size=2) +
  annotate("text", 
           x = 20, 
           y = -16.5, 
           label = 'bold("Mezzanine")',
           hjust = .5, # LEFT ALIGN
           parse = TRUE,
           family="Calibri",
           size=2) +
  theme(plot.title=element_text(face="bold", family="calibri", size=36, hjust=0.0),
        plot.caption = element_text(hjust = 1, family="calibiri", face = "italic"),
        legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.grid.major.x=element_blank(), 
        panel.grid.major.y=element_blank(),
        panel.background = element_rect(fill ="#f0f0f0",colour = NA),
        strip.text=element_text(size= 10, family = "calibri", colour = "#333333")) +
  facet_wrap(~show)


# Save Out Plot -----------------------------------------------------------

#Cairo(4500, 4000, file=here("20200503_Broadway_Weekly_Gross","Image","20200509_American_Airlines_Theatre_Plot.png"), type="png", bg="white", dpi=300)
final_plot
#dev.off()

