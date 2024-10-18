library(tidyverse)

# Load UDP Data into R
udp2024 <- read.csv("data/2022/UDP_ALL_LOGBOOK_as_of_20231204.csv")


#' First, I visually checked the data to see which columns might have had some 
#' important information missing. Stored the rows where these entries occurred in a 
#' vector.
errors_SDATE <- c( 39192,  39557,  54433,  81612, 112891, 
                   175067, 203959, 263486, 271508, 283514, 
                   337927, 337950, 367018)


# Going to breakdown the SDATE into YEAR/MONTH/DAY to help with filtering easier
data <- udp2024 %>%
  mutate(SYEAR  = substr(SDATE, 1, 4),
         SMONTH = substr(SDATE, 5, 6),
         SDAY   = substr(SDATE, 7, 8))


# Only interested in YEAR here, but kept MONTH and DAY in case 
data %>% 
  select(SDATE, SYEAR, SMONTH, SDAY) %>% 
  head()


year_tab <- data %>%
  filter(SYEAR %in% 1900:2024) %>%  # Some entries had issues with SYEAR, filtering to years that make sense
  select(SYEAR) %>%
  group_by(SYEAR) %>%
  summarize(N_ENTRIES = n()) %>%    # n() gives the count for the total entries in each group (SYEAR)
  arrange(desc(SYEAR)) %>%
  rename(YEAR = SYEAR)


kbl(year_tab) %>%
  kable_styling(fixed_thead = TRUE) %>%
  scroll_box(height = "450px")


# I'm storing the years we are interested in here (past 5 years; will use for tables)
years = c("2018", "2019", "2020", "2021", "2022")


# Logbook entries are assigned a region based on the LON & LAT where they occurred
#' This is something that could be put into a function at some point, but for now it's fine
#' to leave as is since we're only assigning regions once
dat <- data
dat$REGION = NA

dat$REGION[which(dat$LATDEG >=  0 & dat$LATDEG <  5 & dat$LONDEG >= 20 & dat$LONDEG <  53)] <- "TUS"
dat$REGION[which(dat$LATDEG >=  5 & dat$LATDEG < 13 & dat$LONDEG >= 20 & dat$LONDEG <  60)] <- "TUN"
dat$REGION[which(dat$LATDEG >=  0 & dat$LATDEG < 22 & dat$LONDEG >= 60 & dat$LONDEG <  87)] <- "CAR"
dat$REGION[which(dat$LATDEG >= 13 & dat$LATDEG < 35 & dat$LONDEG >= 20 & dat$LONDEG <  60)] <- "NCA"
dat$REGION[which(dat$LATDEG >= 18 & dat$LATDEG < 31 & dat$LONDEG >= 87 & dat$LONDEG < 100)] <- "GOM"
dat$REGION[which(dat$LATDEG >= 22 & dat$LATDEG < 30 & dat$LONDEG >= 71 & dat$LONDEG <  82)] <- "FEC"
dat$REGION[which(dat$LATDEG >= 22 & dat$LATDEG < 31 & dat$LONDEG >= 82 & dat$LONDEG <  87)] <- "GOM"
dat$REGION[which(dat$LATDEG >= 22 & dat$LATDEG < 35 & dat$LONDEG >= 60 & dat$LONDEG <  71)] <- "SAR"
dat$REGION[which(dat$LATDEG >= 30 & dat$LATDEG < 35 & dat$LONDEG >= 71 & dat$LONDEG <  82)] <- "SAB"
dat$REGION[which(dat$LATDEG >= 35 & dat$LATDEG < 43 & dat$LONDEG >= 71 & dat$LONDEG <  80)] <- "MAB"
dat$REGION[which(dat$LATDEG >= 35 & dat$LATDEG < 45 & dat$LONDEG >= 60 & dat$LONDEG <  71)] <- "NEC"
dat$REGION[which(dat$LATDEG >= 35 & dat$LATDEG < 50 & dat$LONDEG >= 60 & dat$LONDEG <  65)] <- "NEC"
dat$REGION[which(dat$LATDEG >= 35 & dat$LATDEG < 55 & dat$LONDEG >= 20 & dat$LONDEG <  60)] <- "NED"
data$REGION = dat$REGION
rm(dat)


# Example: How many shortfin mako (SMA) and swordfish (SWO) were discarded 
# dead (D) and alive (A) between 2018 and 2022?
# You can also perform operations on the newly created columns (sum both dead and alive)
years
table(data$SYEAR) %>% data.frame()

data %>%
  filter(SYEAR %in% years) %>%
  filter(PLL == "Y") %>%
  group_by(SYEAR) %>%
  summarise(SMA_dead = sum(SMAA, na.rm = T),
            SMA_live = sum(SMAD, na.rm = T),
            SMA_totl = SMA_dead + SMA_live,  
            SWO_dead = sum(SWOA, na.rm = T),
            SWO_live = sum(SWOD, na.rm = T),
            SWO_totl = SWO_dead + SWO_live)
