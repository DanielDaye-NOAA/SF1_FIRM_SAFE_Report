# Chapter 6 SAFE Report Updates:
require(ggtext)
require(readxl)
require(terra)
require(tidyterra)
require(tidyverse)

source("./functions/TransposeTable.R")

# Load Data Files ----
path = paste0("./data/",report_year-1,"/processed_datasets/SAFE_",report_year-1,"_data_")
## NEFOP ----
NEFOP_HMS <- read_xlsx(paste0(path,"NEFOP_HMS.xlsx"))
NEFOP_sdog_trips <- read_xlsx(paste0(path,"NEFOP_smoothdog_trips.xlsx"))
NEFOP_sdog_length <- read_xlsx(paste0(path,"NEFOP_smoothdog_length.xlsx"))
## UDP ----
UDP_logbook <- read_xlsx(paste0(path,"UDP_logbook.xlsx"), guess_max = 1e7)

gc()

report_years = (report_year-5):(report_year-1)


# Table 6.11 [UDP] ----
# SWO, BFT, YFDT, BET, BAYS landings and discards in PLL
tab_6.11 <- UDP_logbook %>%
  filter(YEAR %in% report_years & PLL == "Y") %>%
  group_by(YEAR) %>%
  summarize(NUM_HOOK = round(sum(HOOKS, na.rm = T)/1000,digits = 2),
            SWO_kept = sum(SWOK, na.rm = T),
            SWO_disc = sum(c(SWOD, SWOA), na.rm = T),
            BFT_kept = sum(BFTA, na.rm = T),
            BFT_disc = sum(c(BFTD, BFTA), na.rm = T),
            YFT_kept = sum(YFTK, na.rm = T),
            YFT_disc = sum(c(YFTD, YFTA), na.rm = T),
            BET_kept = sum(BETK, na.rm = T),
            BET_disc = sum(c(BETD, BETA), na.rm = T),
            BAYS_kept = sum(c(BETK, ALBK, YFTK, SKJK), na.rm = T),
            BAYS_disc = sum(c(BETD, ALBD, YFTD, SKJD,
                              BETA, ALBA, YFTA, SKJA), na.rm = T)) %>%
  t() %>% data.frame()

# I'm going to transpose here to add in the means and diff from 1997-1999
# (I didn't include those years here, I assume they shouldn't change so am just 
# adding them in manually)
names(tab_6.11) <- paste0("YEAR_", tab_6.11[1,])
tab_6.11 <- tab_6.11[-1,]

# Second pipe to get the table in the same format as what's currently in the SAFE
# Report. It gets a bit confusing to follow but I used select to arrange the data
# properly before transposing it back into the correct orientation
tab_6.11 <- tab_6.11 %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(`1997_1999` = c(8533.10, 69131, 21519, 238, 877, 72342, 2489, 21308, 1133, 101477, 4224),
         `2001_2003` = c(7364.10, 50838, 13240, 212, 607, 55166, 1827, 13524, 395, 76116, 3069),
         `2018_2022` = round((YEAR_2018+YEAR_2019+YEAR_2020+YEAR_2021+YEAR_2022)/5,2),
         PCT_A = round((`2001_2003` - `1997_1999`)*100/`1997_1999`, 1),
         PCT_B = round((`2018_2022` - `1997_1999`)*100/`1997_1999`, 1)) %>%
  select(`1997_1999`, `2001_2003`, 
         YEAR_2018, YEAR_2019, YEAR_2020, YEAR_2021, YEAR_2022,
         `2018_2022`, PCT_A, PCT_B) %>% 
  mutate_if(is.numeric, as.character) %>% 
  t() %>% 
  data.frame()
tab_6.11


# Table 6.12 [UDP]* ----
# SHARK, DOL, WAH, BILLFISH, TURTLE landings and discards in Atlantic PLL with % Change
# This will need to be updated to work with different report_years
tab_6.12 <- UDP_logbook %>%
  filter(YEAR %in% report_years,
         PLL == "Y") %>%
  group_by(YEAR) %>%
  summarise(PSHK_kept = sum(c(BSHK, XTHK, OCSK, PORK, SMAK), na.rm = T),  # Pelagic Sharks kept
            PSHK_disc = sum(c(BSHD, BSHA, XTHD, XTHA, OCSD, OCSA,         # Pelagic Sharks disc
                              PORD, PORA, SMAD, SMAA), na.rm = T),
            LSHK_kept = sum(c(SBKK, SPLK, SHHK, FALK,                          # Large coastal kept
                              SSPK, TIGK, GHHK, SBUK), na.rm = T),       
            LSHK_disc = sum(c(SBKD, SBKA, SPLD, SPLA, SHHD, SHHA, FALD, FALA,  # Large coastal disc
                              SSPD, SSPA, TIGD, TIGA, GHHD, GHHA, SBUD, SBUA), na.rm = T),
            DOLP_kept = sum(DOLK, na.rm = T),
            DOLP_disc = sum(c(DOLD, DOLA), na.rm = T),
            WAHO_kept = sum(WAHK, na.rm = T),
            WAHO_disc = sum(c(WAHD, WAHA), na.rm = T),
            BLUM_disc = sum(c(BUMD, BUMA), na.rm = T),
            WHTE_disc = sum(c(WHMD, WHMA), na.rm = T),
            SAIL_disc = sum(c(SAID, SAIA), na.rm = T),
            SPEA_disc = sum(c(SPXD, SPXA), na.rm = T),
            TURT_ints = sum(c(TLB, TTL, TTG, KRT, THB, TTX), na.rm = T)) %>%
  TransposeTable() %>%
  mutate_if(is.character, as.numeric) %>% 
  mutate(y9799 = c(3898, 52093, 8860, 6308, 39711, 608, 5172, 175, 1621, 1973, 1342, 213, 596),
         ref_A = c(3237, 23017, 5306, 4581, 29361, 322, 3776, 74, 815, 1045, 341, 139, 429),
         ref_B = (`2018`+`2019`+`2020`+`2021`+`2022`)/5,
         dif_a = round((ref_A-y9799)*100/y9799, digits = 1),
         dif_b = round((ref_B-y9799)*100/y9799, digits = 1)) %>%
  select(y9799,ref_A,`2018`,`2019`,`2020`,`2021`,`2022`,ref_B,dif_a, dif_b) %>%
  TransposeTable(colToRowName = F)
tab_6.12


# Table 6.13 [UDP]* ----
# Distribution of HOOKS set by REGION 2018-2022, 
# Include % Change since 1997-1999 in ATL PLL
# Needs updating to work with other report_years, streamline code
tab_6.13 <- UDP_logbook %>%
  filter(YEAR %in% report_years & PLL == "Y") %>%
  group_by(REGION, YEAR) %>%
  summarize(TOT_HOOKS = sum(HOOKS, na.rm = T)) %>%
  spread(REGION, TOT_HOOKS) %>%
  replace(is.na(.), 0) %>%
  mutate(TUN_TUS = TUN + TUS,
         TOTAL = CAR + GOM + FEC + SAB + MAB + NEC + NED + SAR + NCA + TUN_TUS) %>%
  select(YEAR, CAR, GOM, FEC, SAB, MAB, NEC, NED, SAR, NCA, TUN_TUS, TOTAL) %>%
  column_to_rownames("YEAR") %>%  
  t() %>% data.frame()

# Still not a perfect solution as the column names have an X at the start, will remove
names(tab_6.13) = gsub("X", "YEAR_", names(tab_6.13))

# Manually adding in the data for '97-'99 and '01-'03 again
tab_6.13 <- tab_6.13 %>%
  mutate(YEAR_97_99 = c(328110, 3346298, 722580, 813111, 1267409, 901593, 511431, 14312, 191478, 436826, 8533148),
         YEAR_01_03 = c(175195, 3682536, 488838, 569965, 944929, 624497, 452430, 76130, 22070, 127497, 7364086),
         YEAR_18_22 = round((YEAR_2018 + YEAR_2019 + YEAR_2020 + YEAR_2021 + YEAR_2022)/5, 0),
         PCT_A = round((YEAR_01_03-YEAR_97_99)*100/YEAR_97_99, 1),
         PCT_B = round((YEAR_18_22-YEAR_97_99)*100/YEAR_97_99, 1)) %>%
  select(YEAR_97_99, YEAR_01_03, 
         YEAR_2018, YEAR_2019, YEAR_2020, YEAR_2021, YEAR_2022, 
         YEAR_18_22, PCT_A, PCT_B) %>%
  mutate_if(is.numeric, as.character) %>% 
  t() %>% 
  data.frame() 
tab_6.13


# Table 6.14 [UDP] ----
# BFT, SWO, Pelagic & Large Coastal Sharks, BILLFISH, TURTLES KEPT/DISC in MAB & NEC
tab_6.14 <- UDP_logbook %>%
  filter(REGION %in% c("MAB", "NEC"),
         PLL == "Y", 
         YEAR %in% report_years) %>%
  group_by(YEAR) %>%
  summarise(TOT_HOOK = sum(HOOKS, na.rm = T),
            THO_HOOK = round(TOT_HOOK/1000, digits = 6),
            BFT_K = sum(BFTK, na.rm = T),
            BFT_D = sum(c(BFTD, BFTA), na.rm = T),
            SWO_K = sum(SWOK, na.rm = T),
            SWO_D = sum(c(SWOD, SWOA), na.rm = T),
            PSHK_kept = sum(c(BSHK, XTHK, OCSK, PORK, SMAK), na.rm = T),  # Pelagic Sharks kept
            PSHK_disc = sum(c(BSHD, BSHA, XTHD, XTHA, OCSD, OCSA,         # Pelagic Sharks disc
                              PORD, PORA, SMAD, SMAA), na.rm = T),
            LSHK_kept = sum(c(SBKK, SPLK, SHHK, FALK,                          # Large coastal kept
                              SSPK, TIGK, GHHK, SBUK), na.rm = T),       
            LSHK_disc = sum(c(SBKD, SBKA, SPLD, SPLA, SHHD, SHHA, FALD, FALA,  # Large coastal disc
                              SSPD, SSPA, TIGD, TIGA, GHHD, GHHA, SBUD, SBUA), na.rm = T),
            BFSH_D = sum(c(BUMD, BUMA, WHMD, WHMA, SAID, SAIA, SPXD, SPXA), na.rm = T),
            TURT_I = sum(c(TLB, TTL, TTG, KRT, THB, TTX), na.rm = T))
tab_6.14


# Table 6.15 [UDP] ----
#' BFT, SWO, Pelagic & Large Coastal Sharks, BILLFISH, TURTLES KEPT/DISC in 
#' all regions except NEC and MAB
tab_6.15 <- UDP_logbook %>%
  filter(!REGION %in% c("MAB", "NEC"),
         PLL == "Y", 
         YEAR %in% report_years) %>%  #dim() # 9,148 x 323 
  group_by(YEAR) %>%
  summarise(TOT_HOOK = sum(HOOKS, na.rm = T),
            THOUSAND_HOOK = round(TOT_HOOK/1000, digits = 6),
            BFT_K = sum(BFTK, na.rm = T),
            BFT_D = sum(c(BFTD, BFTA), na.rm = T),
            SWO_K = sum(SWOK, na.rm = T),
            SWO_D = sum(c(SWOD, SWOA), na.rm = T),
            PSHK_kept = sum(c(BSHK, XTHK, OCSK, PORK, SMAK), na.rm = T),  # Pelagic Sharks kept
            PSHK_disc = sum(c(BSHD, BSHA, XTHD, XTHA, OCSD, OCSA,         # Pelagic Sharks disc
                              PORD, PORA, SMAD, SMAA), na.rm = T),
            LSHK_kept = sum(c(SBKK, SPLK, SHHK, FALK,                          # Large coastal kept
                              SSPK, TIGK, GHHK, SBUK), na.rm = T),       
            LSHK_disc = sum(c(SBKD, SBKA, SPLD, SPLA, SHHD, SHHA, FALD, FALA,  # Large coastal disc
                              SSPD, SSPA, TIGD, TIGA, GHHD, GHHA, SBUD, SBUA), na.rm = T),
            BFSH_D = sum(c(BUMD, BUMA, WHMD, WHMA, SAID, SAIA, SPXD, SPXA), na.rm = T),
            TURT_I = sum(c(TLB, TTL, TTG, KRT, THB, TTX), na.rm = T))
tab_6.15


# Table 6.25 [NEFOP] ----
# Observed Protected Species Interactions in NE and Mid ATL Gillnet Fishery
tab_6.25 <- NEFOP_sdog_trips %>%
  group_by(COMNAME, DISPDESC) %>%
  summarise(NUM_INT = n()) %>%
  spread(DISPDESC, NUM_INT) %>%
  filter(!(grepl("SHARK|DOGFISH|FLOUNDER|SKATE|DEBRIS|MACKEREL", COMNAME)),
         !(grepl("RAY|EGGS|BASS|SEA ROBIN|CRAB|BLUEFISH|JELLYFISH", COMNAME)),
         !(grepl("BONITO|COBIA|LOBSTER|MENHADEN|SCUP|MONKFISH", COMNAME)),
         !(grepl("STARGAZER|STARFISH|WEAKFISH|SHELL|TAUTOG|TUNA", COMNAME)))
tab_6.25


# Table 6.28 [NEFOP] ----
# Total Otter TRAWL shark catches from non-smooth dogfish targeted sets
tab_6.28 <- NEFOP_HMS %>%
  filter(HMSGEARCAT == "TRAWL" & SMOOTHDOGTRIP == FALSE) %>%
  filter(grepl("SHARK", COMNAME)) %>%
  group_by(COMNAME, STATUS, DISPDESC) %>%
  summarize(TOT = n()) %>%
  pivot_wider(names_from = c(STATUS, DISPDESC), values_from = TOT, values_fill = 0) %>%
  transmute(TOT = ALIVE_DISC + DEAD_DISC + NA_DISC + DEAD_KEPT,
            ALIVE_DISC, pALIVE_DISC = round(ALIVE_DISC*100/TOT, 1),
            DEAD_DISC, pDEAD_DISC = round(DEAD_DISC*100/TOT, 1),
            NA_DISC, pNA_DISC = round(NA_DISC*100/TOT, 1),
            DEAD_KEPT) %>%
  arrange(desc(TOT))
tab_6.28
paste0(sum(tab_6.28$TOT)," sharks caught from all non-smoothdog targeted otter trawls.")


# Table 6.30 [NEFOP] ----
# Prohibited Shark HMS spp Caught/Disc on BLL trips targeting FINFISH

tab_6.30 <- NEFOP_HMS %>% 
  filter(HMSGEARCAT == "BOTTOM LONGLINE") %>% # select(TARG1_COMMONNAME) %>% table()
  filter(grepl("TILEFISH|HADDOCK", TARG1_COMMONNAME)) %>% # select(HULLNUM1, VESSELNAME, TARG1_COMMONNAME)
  group_by(COMNAME, DISPDESC) %>%
  summarise(TOT = n()) %>%
  spread(DISPDESC, TOT) %>%
  transmute(TOT = sum(DISC, KEPT, na.rm = T), KEPT, DISC) %>%
  arrange(desc(TOT))
tab_6.30


# Table 6.31 [NEFOP] ----
# Sharks caught / discarded on trips using gillnet gear types targeting mixed TELEOSTS
# TELEOSTS = removing SHARK, DOGFISH, SKATE from TARG1
tab_6.31 <- NEFOP_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT),
         !grepl("SHARK|DOGFISH|SKATE", TARG1_COMMONNAME),
         !grepl("ANGEL|BASKING|DUSKY|NIGHT|GREENLAND|SAND TIGER|SHARK, WHITE", COMNAME),
         !grepl("TUNA", COMNAME)) %>% 
  group_by(COMNAME, DISPDESC) %>%
  summarise(count = n()) %>%
  spread(DISPDESC, count) %>%
  arrange(desc(DISC)) %>% 
  replace(is.na(.), 0) %>%
  mutate(TOTAL = DISC + KEPT,
         PCT.DISC = round(DISC*100/TOTAL, digits = 1)) %>%
  select(TOTAL, PCT.DISC) 
tab_6.31
paste0(sum(tab_6.31$TOTAL)," sharks caught/discarded across all GILLNET trips targeting mixed TELEOSTS")

# Text 6.5.1 [NEFOP] ----
# Mixed-species Otter Trawl TRIP and SET counts
# THIS IS NOW CORRECT AND IN-LINE WITH HEATHER'S MATH
text_6.5.1 <- NEFOP_HMS %>%
  filter(HMSGEARCAT == "TRAWL") %>% #select(GEARNM) %>% table()# all TRAWL in this data are OTTER TRAWL  #select(TARG1_COMMONNAME) %>% table()
  filter(!grepl("ANGEL|BASKING|DUSKY|NIGHT|GREENLAND|SAND TIGER|SHARK, WHITE", COMNAME)) %>% #select(COMNAME) %>% table()
  group_by(HULLNUM1, TRIPID) %>%
  summarize(VESNAME = unique(VESSELNAME),
            N_OBS = n(),
            N_SET = length(unique(paste(TRIPID, HAULNUM))),
            TARG = paste(unique(TARG1_COMMONNAME), collapse = ";"))

tab_6.5.1 <- data.frame(NUM_VESS = length(unique(text_6.5.1$HULLNUM1)),
                        NUM_TRIPS = length(unique(text_6.5.1$TRIPID)),
                        NUM_SETS = sum(text_6.5.1$N_SET))
tab_6.5.1


# Text 6.5.4.1 [NEFOP]* ----
# Add info for what exactly is being pulled in each of these (probably top species in X)
# first paragraph desc
NEFOP_sdog_trips %>%
  filter(grepl("GILLNET", HMSGEARCAT),
         DISPDESC == "KEPT") %>%
  group_by(COMNAME) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head()

# second paragraph desc
NEFOP_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT)) %>% 
  select(TARG1_COMMONNAME) %>% 
  table() %>% data.frame() %>% 
  arrange(desc(Freq)) %>% 
  head()

# drift-sink is NOT sink - is drift
gill <- NEFOP_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT),
         !grepl("SHARK|DOGFISH|SKATE", TARG1_COMMONNAME)) %>% #select(TARG1_COMMONNAME) %>% table()
  group_by(HULLNUM1) %>%
  summarise(NTRIP = length(unique(TRIPID)),
            NSETS = length(unique(paste(TRIPID, HAULNUM)))) %>% 
  arrange(desc(NTRIP))

data.frame(N_VESS = nrow(gill),
           N_TRIP = sum(gill$NTRIP),
           N_SETS = sum(gill$NSETS))

NEFOP_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT),
         !grepl("SHARK|DOGFISH|SKATE", TARG1_COMMONNAME)) %>%
  group_by(COMNAME) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  head()

drift <- NEFOP_HMS %>%
  filter(grepl("DRIFT", HMSGEARCAT),
         !grepl("SHARK|DOGFISH|SKATE", TARG1_COMMONNAME)) %>% #select(GEARNM) %>% table()
  group_by(HULLNUM1) %>%
  summarise(NTRIP = length(unique(TRIPID)),
            NSETS = length(unique(paste(TRIPID, HAULNUM))))

data.frame(N_VESS = nrow(drift),
           N_TRIP = sum(drift$NTRIP),
           N_SETS = sum(drift$NSETS))

NEFOP_HMS %>%
  filter(grepl("DRIFT", HMSGEARCAT),
         !grepl("SHARK|DOGFISH|SKATE", TARG1_COMMONNAME)) %>%
  group_by(COMNAME) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  head()

# GOOD  
sink <- NEFOP_HMS %>%
  filter(grepl("SINK", HMSGEARCAT),
         !grepl("DRIFT-SINK", HMSGEARCAT),
         !grepl("SHARK|DOGFISH|SKATE", TARG1_COMMONNAME),
         !grepl("ANGEL|BASKING|DUSKY|NIGHT|GREENLAND|SAND TIGER|SHARK, WHITE", COMNAME)) %>%  # select(GEARNM) %>% table()
  group_by(HULLNUM1) %>%
  summarise(NTRIP = length(unique(TRIPID)),
            NSETS = length(unique(paste(TRIPID, HAULNUM))))

data.frame(N_VESS = nrow(sink),
           N_TRIP = sum(sink$NTRIP),
           N_SETS = sum(sink$NSETS))

NEFOP_HMS %>%
  filter(grepl("SINK", HMSGEARCAT),
         !grepl("DRIFT-SINK", HMSGEARCAT),
         !grepl("SHARK|DOGFISH|SKATE", TARG1_COMMONNAME),
         !grepl("ANGEL|BASKING|DUSKY|NIGHT|GREENLAND|SAND TIGER|SHARK, WHITE", COMNAME)) %>%
  group_by(COMNAME) %>%
  summarize(n = n()) %>%
  arrange(desc(n))


# Misc ----
#NUM_SETS on NUM_TRIPS targeting sharks but not SMOOTHDOG
NEFOP_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT),
         grepl("SHARK|DOGFISH", TARG1_COMMONNAME),
         TARG1_COMMONNAME != "DOGFISH, SMOOTH") %>%
  group_by(VESSELNAME) %>%
  summarise(TOT_ROWS = n(),
            NUM_TRIP = length(unique(TRIPID)),
            NUM_SETS = length(unique(paste0(TRIPID,HAULNUM))),
            TRP_TARG = paste(unique(TARG1_COMMONNAME), collapse =":"),
            TRP_CTCH = paste(unique(COMNAME), collapse =":")) %>%
  arrange(VESSELNAME)  # NUM_ROWS = NUM_VESS

# Which targeted species have the most bycatch?
# This is a big table, probably want to save/export - gives a view of all HMS
# and from which target species they were caught as bycatch
bycatch_speccount <- NEFOP_HMS %>%
  group_by(TARG1_COMMONNAME, COMNAME) %>%
  summarise(NUM_HMSCATCH = n()) %>%
  spread(COMNAME, NUM_HMSCATCH) %>%
  replace(is.na(.), 0)

bycatch_haulcount <- NEFOP_HMS %>%
  group_by(TARG1_COMMONNAME) %>%
  summarise(TOT.HMS = n(),
            TRIPS = length(unique(TRIPID)),
            HAULS = length(unique(paste0(TRIPID,HAULNUM))),
            VSSLS = length(unique(VESSELNAME)))

NEFOP_HMS_TARG_2023 <- bycatch_haulcount %>%
  left_join(bycatch_speccount, by = "TARG1_COMMONNAME") %>%
  arrange(desc(TOT.HMS)) %>% filter(!grepl("SHARK", TARG1_COMMONNAME))

NEFOP_HMS_TARG_2023 %>%
  rename(TRIP_TARG = TARG1_COMMONNAME)


# Figure 6.1* ----
# Might want to hard-code the export dimensions for the figure
# Load state and world shapefiles from "maps" package
state <- map_data("state")
world <- map_data("world")
# Load shapefiles for closed areas (saved locally, here)
CB  <- vect("C:/Users/daniel.daye/Documents/DDaye/local_GIS_shapefiles/CBump_a.shp")
EF  <- vect("C:/Users/daniel.daye/Documents/DDaye/local_GIS_shapefiles/EFC_a.shp")
DS  <- vect("C:/Users/daniel.daye/Documents/DDaye/local_GIS_shapefiles/DeSoto_a.shp")
eez <- vect("C:/Users/daniel.daye/Documents/DDaye/local_GIS_shapefiles/US Fed Waters.shp")

#' Here I'm using custom axis breaks to mark specific longitudes and latitudes;
#' formatting like this allows me to add the deg-min-sec using unicode
xlabs <- paste0(c("90","80","70","60"),"\u00B0","0'",'0"W')
ylabs <- paste0(c("20","30","40"),"\u00B0","0'",'0"N')

# Storing as an r object so that I can save as a file via ggsave
fig_6.1 <- ggplot() +
  geom_sf(data = eez, col = "black", fill = NA) +
  annotation_map(world, fill = "gray60", col = "black", lwd = 0.25) +
  annotation_map(state, fill = "gray90", col = "black", lwd = 0.25) +
  # Text boxes
  geom_textbox(aes(x = c(-90, -75, -71.5, -61),
                   y = c( 25,  26,  32.0,  31),
                   label = c("**DeSoto Canyon**    \nYear-round",
                             "**East Florida Coast Closed Area**      \nYear-round",
                             "**Charleston Bump**       \nFeb 1 - April 30",
                             "**Northeast Distant Restricted Fishing Area**     \nYear-round with exceptions"),
                   box.colour = NA),
               width = unit(1.5, "inch"),
               col = "black", fill = "white") +
  geom_sf(data = CB, col = "black", fill = "#f68584") +
  geom_sf(data = EF, col = "black", fill = "#f68584") +
  geom_sf(data = DS, col = "black", fill = "#f68584") +
  geom_rect(aes(xmin = -60, xmax = -20,
                ymin =  35, ymax =  55),
            col = "black", fill = "#80b4fc", lwd = 0.25) +
  # Legend
  geom_rect(aes(xmin = -98, xmax = -85,
                ymin =  35, ymax =  41.5),
            col = "black", fill = "white", lwd = 0.25) +
  geom_rect(aes(xmin = -97.7, xmax = -96.2,
                ymin = c(35.8  , 37.5), 
                ymax = c(37.3, 39)),
            col = "black", lwd = 0.25,
            fill = c("#f68584", "#80b4fc")) +
  geom_text(aes(x = -97.7, y = 40, label = "Legend"),
            size = 8, hjust = 0, vjust = 0) +
  geom_text(aes(x = c(-96, -96, -96), 
                y = c(38.4, 36.7, 35.9),
                label = c("Pelagic Longline Closures", 
                          "Pelagic Longline",
                          "Gear Restricted Areas"),
                hjust = 0, vjust = 0)) +
  # Arrows
  annotate(geom = "segment", 
           x = c(-88.5, -88.5, -75.5, -75.5, -61.0),
           y = c( 26.0,  26.0,  27.5,  32.0,  33.5),
           xend = c(-85.8, -87.5, -79.0, -76.8, -58),
           yend = c( 26.5,  28.2,  30.0,  32.0,  36),
           color = "black", linewidth = 0.5, arrow = arrow(angle = 15,
                                                           length = unit(0.1, "inches"))) +
  # Formatting
  coord_sf(xlim = c(-97, -58), ylim = c(20, 41)) +
  scale_x_continuous(breaks = seq(-90, -60, by = 10),
                     labels = xlabs) +
  scale_y_continuous(breaks = seq(20, 40, by = 10),
                     labels = ylabs) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(plot.margin = margin(.5,0,0,0, unit = "cm"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10, family = "sans"))
fig_6.1
ggsave(paste0("./data/",report_year-1,"/figures/fig_6-1.png"), fig_6.1,
       width = 9, height = 6)


# Figure 6.3* ----
# Update to ggplot syntax
lims <- data.frame(x = c(-100, -20),
                   y = c(   0,  55))
basemap <- map_data("world")

{par(oma = c(1, 1, 1, 1), mar = c(1, 1, 1, 1))
  plot(NULL, xlim = lims$x, ylim = lims$y, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")
  maps::map("world", fill = FALSE, add = TRUE, interior = FALSE, lwd = 2,
            xlim = lims$x, ylim = lims$y, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")
  axis(side = 1, at = seq(-95, -20, by = 5), 
       lwd = 2)
  axis(side = 2, at = seq(5, 55, by = 5), 
       las = 1, lwd = 2)
  axis(side = 3, at = c(-82, -78, -71, -65, -60), 
       labels = c(-82, -78, -71, -65, -60), 
       lwd = 2)
  axis(side = 4, at = c(5, 13, 35), las = 1, lwd = 2)
  text(-99, 53.5-(0:11*1.9), adj = 0,
       labels = c("Caribbean (CAR)",
                  "Gulf of Mexico (GOM)",
                  "Florida East Coast (FEC)",
                  "South Atlantic Bight (SAB)",
                  "Mid Atlantic Bight (MAB)",
                  "Northeast Coastal (NEC)",
                  "Northeast Distant (NED)",
                  "Sargasso (SAR)",
                  "North Central Atlantic (NCA)",
                  "Tuna North (TUN)",
                  "Tuna South (TUS)"),
       cex = 1)
  text(c(73, 65.5, 40,   75.5, 90, 75.5, 65.5, 40, 75, 40, 40)*-1,
       c(38,   40, 40,   32.5, 25,   28,   28, 16, 15,  7,  2),
       adj = 0.5,
       labels = c("MAB\n(92)", "NEC\n(92)", "NED\n(94)", "SAB\n(92)",
                  "GOM\n(91)", "FEC\n(92)", "SAR\n(93)", "NCA\n(93)",
                  "CAR (93)", "TUN (93)", "TUS (96)"))
  segments(x0 = c(65, 71, 78, 82, 82, 87, 60, 52)*-1,
           x1 = c(60, 65, 71, 20, 71, 60, 20, 20)*-1,
           y0 = c(50, 45, 43, 35, 30, 22, 13, 5),
           y1 = c(50, 45, 43, 35, 30, 22, 13, 5),
           lwd = 2)
  segments(x0 = c(82, 78, 71, 65, 60)*-1,
           x1 = c(82, 78, 71, 65, 60)*-1,
           y0 = c(35, 43, 45, 50, 55),
           y1 = c(22, 35, 22, 45, 8),
           lwd = 2)
  box(which = "plot", lwd = 2)}