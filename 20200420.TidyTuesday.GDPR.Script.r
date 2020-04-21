### First Tidy tuesday! 
# Date: 4/20/2020
# Topic: European "General Data Protection Regulation"
# Likely need to read more!
# SOurce: Thomas Mock
# Location: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-21/readme.md

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
gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')


# Descriptive Review ------------------------------------------------------
dim(gdpr_violations)
str(gdpr_violations)
head(gdpr_violations)


# Which country saw the most enforecements?
gdpr_violations %>% 
  group_by(name) %>% 
  summarize(`Country Where Violation Enforced` = n()) %>% 
  arrange(-`Country Where Violation Enforced`)

# * * Follow-up: why does Spain have so many? Twice as many as anyone else? -------

# What was the largest fine? Median? Average? Min?
class(gdpr_violations$price)

gdpr_violations %>% 
  mutate(Max=max(price),
         Median=median(price),
         Average=mean(price),
         Min=min(price))%>% 
  select(Max, Median, Average, Min) %>% 
  slice(., 1)

# * * Follow-up: why is the minimum price 0 dollars? -------
# Not Interesting - fines yet to be distributed, or looked like weaker cases. TWO Massive ones hidden there tho; could be worth coming back to.
gdpr_violations %>% 
  filter(price<5) %>% 
  View()

gdpr_violations %>% 
  filter(price<5) %>% 
  select(controller, summary)
# Save to CSV to read; long chains.

# Which Authority enacted the most violation?
gdpr_violations %>% 
  group_by(authority) %>% 
  summarize(`Authority Enacting Violation` = n()) %>% 
  arrange(-`Authority Enacting Violation`)
# * * Follow-up: why isn't Germany's DPA mentioned 3rd from the top if they are the third highest in the country list? -------

# When did the most violations occur?
class(gdpr_violations$date)
head(gdpr_violations$date)

gdpr_violations %>% 
  mutate(Date = as.Date(date, "%m/%d/%Y"),
         Year = year(Date)) %>% 
  group_by(Year) %>% 
  summarize(`Year Count` = n())

# any spikes during the time of the year?
gdpr_violations %>% 
  mutate(Date = as.Date(date, "%m/%d/%Y"),
         Year = year(Date),
         Month = month(Date),
         Day = day(Date)) %>% 
  filter(Year == 2019) %>% 
  group_by(Month) %>% 
  summarize(Count = n()) %>% 
  ggplot()+
  geom_point(aes(x=Month,
                y=Count),
             alpha=.5)

# * * Follow-up: why did so many happen in October 2019? -----

# Who was the largest violator?
gdpr_violations %>% 
  group_by(controller) %>% 
  summarize(`Violator of Data` = n()) %>% 
  arrange(-`Violator of Data`) %>% 
  View()

# Damn, Vodaphone really got fined! 

# Big Questions ---------------------------------------------------------------
# How much did Vodaphone & Google Actually get Fined?

gdpr_violations %>% 
  mutate(controller_first.word =  gsub(" .*", "", controller)) %>% 
  group_by(controller_first.word) %>% 
  summarize(`Violator of Data` = n()) %>% 
  arrange(-`Violator of Data`) %>% 
  View()

gdpr_violations %>% 
  mutate(controller_first.word =  gsub(" .*", "", controller)) %>% 
  filter(controller_first.word %in% c("Vodafone",
                                      "Google")) %>% 
  group_by(controller_first.word) %>% 
  summarize(`Total Euro Fine` = sum(price))

# * * So Vodaphone could be interesting! ----

# What agencies acted where?

gdpr_violations %>% 
  group_by(authority, name) %>% 
  summarize(interaction_Au.Co = n()) %>% 
  View()
# Interesting; Germany has ther own field offices by city ...

# * * What happened in Spain? Why so many? ---------

gdpr_violations %>% 
  filter(name == "Spain") %>% 
  group_by(type) %>% 
  summarize(Type = n())

gdpr_violations %>% 
  filter(name == "Spain") %>% 
  mutate(Date = as.Date(date, "%m/%d/%Y"),
         Year = year(Date),
         Month = month(Date),
         Day = day(Date)) %>% 
  group_by(Date) %>% 
  summarize(Type = n()) %>% 
  View()

# * * What happened in Spain in October? -------
# Lets go with this one; seems the most interesting.

gdpr_violations %>% 
  filter(name == "Spain") %>% 
  mutate(Date = as.Date(date, "%m/%d/%Y"),
         Year = year(Date),
         Month = month(Date),
         Day = day(Date)) %>% 
  group_by(Date, controller) %>% 
  summarize(Type = n()) %>% 
  spread(controller, Type) %>% 
  View()

# Spain is the tidy Tuesday Topic of Choice! --------------------------
#  * * Create Spain Subset & Clean ------------------------------------

gdpr_violations %>% 
  filter(name == "Spain") %>% 
  select(controller) %>% 
  distinct(controller) %>% 
  arrange(controller) %>% 
  View()
  
class(gdpr_violations$controller)

gdpr_spain <- gdpr_violations %>% 
  filter(name == "Spain") %>% 
  mutate(Date = as.Date(date, "%m/%d/%Y"),
         Year = year(Date),
         Month = month(Date),
         Week = cut(Date, "week"),
         Day = wday(Date, label = TRUE, abbr = FALSE),
         # Start controller cleaning! - given we don't know this data well, we do risk some innaccuracy
         # Is EDP a duplicate? Same day, different categories.
         # Woah ! there own DPA got fined? 
         clean_controller = ifelse(controller == "Private individual", "Private individual 1",
                                   ifelse(controller == "Private person", "Private individual 2",
                                          ifelse(controller == "Vodafone Espana", "Vodafone España",
                                                 ifelse(controller == "Vodafone España, S.A.U.", "Vodafone España",
                                                        ifelse(controller == "Vodafone ONO", "Vodafone España", as.character(controller))))))) %>%
  # Note that we're removing the 5 instances that are ongoing, need to note in the graph.
  filter(Date != as.Date("1970-01-01"))

# When was Spain's DPA fined???

gdpr_spain %>% 
  filter(clean_controller == "Spanish Data Protection Authority (AEPD)")
# Someone used CCT unlawfully!

# * * Graph Violation counts by Week Day -------

gdpr_spain %>% 
  group_by(Day) %>% 
  summarize(`Violation Count` = n()) %>% 
  ggplot(., aes(x=Day, y=`Violation Count`)) + 
  geom_bar(stat="identity")

# Potentially interesting! Mostly Monday/Tuesday
# Total fines on a Day; Want to do Average and Mean too!
# Check for Outliers beyond the 

gdpr_spain %>% 
  mutate(`Maximum Fine` = max(price),
         `Median Fine` = median(price),
         `Average Fine` = round(mean(price), 0),
         `Minimum Fine` = min(price),
         `1st Quartile` = quantile(price)[2],
         `3rd Quartile` = quantile(price)[4],
         IQR = 1.5*(`3rd Quartile` -`1st Quartile`),
         UR = `3rd Quartile` + IQR,
         LR = `1st Quartile` - IQR,
         Outliers = ifelse(price <=LR | price>=UR, 1, 0)) %>% 
  filter(Outliers == 1)

# Of course the LaLiga is the outlier.
# Recalculate without LaLiga, add back in.
gdpr_spain_outlier <-gdpr_spain %>% 
                      filter(controller != "Professional Football League (LaLiga)") %>%
                      group_by(Day) %>% 
                      mutate(`Maximum Fine` = max(price),
                             `Median Fine` = median(price),
                             `Average Fine` = round(mean(price), 0),
                             `Minimum Fine` = min(price),
                             `1st Quartile` = quantile(price)[2],
                             `3rd Quartile` = quantile(price)[4])

gdpr_spain_outlier <- bind_rows(gdpr_spain_outlier %>%
  select(Day, `Minimum Fine`, `1st Quartile`, `Median Fine`, `3rd Quartile`,`Maximum Fine`) %>% 
  pivot_longer(-Day,
               names_to = "Type",
               values_to ="Fine") %>% 
    mutate(TF = paste0(Type, Fine)) %>% 
    distinct(TF, .keep_all = TRUE) %>% 
    mutate(TF = NULL),
  gdpr_spain %>% 
    filter(controller == "Professional Football League (LaLiga)") %>% 
    select(Day, price) %>% 
    mutate(Type = c("Outlier")) %>% 
    rename(Fine = price))

table(gdpr_spain_outlier$Type)

gdpr_spain_outlier <- gdpr_spain_outlier %>% 
  mutate(Type = factor(Type, levels = c("Minimum Fine",
                                        "1st Quartile",
                                        "Median Fine",
                                        "3rd Quartile",
                                        "Maximum Fine",
                                        "Outlier")),
         Fine = Fine/1000)

# Box Plot Basic Fancy - - - 
gdpr_spain_plot <- ggplot() + 
  geom_line(data = gdpr_spain_outlier %>% 
              filter(Type %in% c("1st Quartile", "3rd Quartile")), 
            aes(x=Fine, 
                y=reorder(Day, desc(Day))),
            size=4,
            color = "#9197ae") +
    geom_point(data = gdpr_spain_outlier,
               aes(x=Fine, 
                   y=reorder(Day, desc(Day)),
                   fill = Type),
               shape=21,
               size=4)+
  scale_fill_manual(values = c("#D3D4D9", # Min
                               "#4B88A2", #1st
                               "#C60B1E", #Median
                               "#DE7C5A", #3rd
                               "#570000", # MAx
                               "#FFCBDD")) + # Outlier
  scale_x_continuous(labels=dollar_format(prefix="???"),
                     breaks = seq(from = 0, to = 250, by = 25))+
  labs(title="In Spain I Want to Be Fined on a Tuesday",
       subtitle="Spain's General Data Protection Regulation (GDPR) fines are summarized by weekday below\nfrom the minimum fine to the maximum fine accrued. The grey-blue line represents the inter-quartile-range.",
       x="Fine (Thousands)",
       y="Day of Week",
       caption="Five cases were removed due to unresolved dates.\nData source: Privacy Affaris, via Bob Rudis and Roel Hogervorst in the #TidyTuesday Challenge on Twitter.",
       fill = "Legend")+
  theme(plot.title=element_text(face="bold", size=14, hjust=0.0),
        axis.title.x = element_text(face="bold", size=11), 
        axis.title.y = element_text(face="bold", size=11), 
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        panel.grid.minor=element_blank(), 
        panel.grid.major.y= ggplot2::element_line(color="#cbcbcb"), 
        panel.grid.major.x = ggplot2::element_line(color="#cbcbcb"),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  annotate("rect", 
           xmin = 110, 
           xmax = 220, 
           ymin = "Wednesday", 
           ymax = "Monday",
           color="#F79D5c",
           fill="#F79D5c",
           alpha = .2)+
  annotate("text", 
           x=115, 
           y="Tuesday", 
           label=c("Fines on Tuesdays skewed\nto under ???25K, with the 3rd Quartile\nreaching just ???45K"), 
           hjust = 0)+
  geom_curve(aes(x = 110, 
                 y="Tuesday", 
                 xend=50, 
                 yend = "Tuesday"), 
             curvature = -0.4, 
             arrow = arrow(),
             color="#F79D5c",
             size=1)


# Save Out Plot -----------------------------------------------------------

Cairo(2700, 2100, file=here("20200420.GDPR", "Output","20200421 Spain GDPR Fine by Weekday.png"), type="png", bg="white", dpi=300)
gdpr_spain_plot 
dev.off()
  
  
