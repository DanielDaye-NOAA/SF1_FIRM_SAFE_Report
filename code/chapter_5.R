# Chapter 5 SAFE Report Updates
require(readxl)
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


# Table 5.20 [UDP-C] ----
# Average Number of HOOKS per PLL set (last 5 years)
# Need to check if any of the categories need to be made confidential:
conf_5.20 <- UDP_logbook %>%
  filter(YEAR %in% report_years, PLL == "Y") %>%
  select(ID, YEAR, TSWO, TBET, TYFT, TMIX, TSRK, TDOL, TOTH, HOOKS) %>%
  pivot_longer(cols = TSWO:TOTH) %>%
  filter(value == "Y") %>%
  group_by(YEAR, name) %>%
  summarize(numves = length(unique(ID))) %>%
  spread(name, numves) %>%
  TransposeTable() %>% 
  mutate_all(function(x) as.numeric(x))
conf_5.20
which_conf <- conf_5.20 < 3

tab_5.20 <- UDP_logbook %>%
  filter(YEAR %in% report_years, PLL == "Y") %>% 
  select(YEAR, TSWO, TBET, TYFT, TMIX, TSRK, TDOL, TOTH, HOOKS) %>%
  pivot_longer(cols = TSWO:TOTH) %>%
  filter(value == "Y") %>%
  group_by(YEAR, name) %>%
  summarize(HOOKAVG = mean(HOOKS, na.rm = T)) %>%
  spread(name, HOOKAVG) %>%
  select(YEAR, TSWO, TBET, TYFT, TMIX, TSRK, TDOL, TOTH) %>%
  TransposeTable() %>%
  mutate_all(as.numeric) %>% 
  mutate_all(round)
tab_5.20[which_conf] = "-"
tab_5.20


# Table 5.22 [UDP] ----
#' Reported catch and hook numbers  for suite of species in ATL PLL: swordfish (SWO),
#' blue marlin (BUM), white marlin (WHM), sailfish (SAI), spearfish (SPX), bluefin tuna (BFT),
#' BAYS tuna (BET, ALB, YFT, SKJ), Pelagic Sharks (BSH, XTH, OCS, POR, SMA), Large coastal 
#' sharks (SBK, SPL, SHH, FAL, SSP, TIG, GHH, SBU), dolphinfish (DOL), wahoo (WAH), and
#' turtle interactions (TLB, TTL, TTG, KRT, THB, TTX)
tab_5.22 <- UDP_logbook %>%
  filter(YEAR %in% report_years, PLL == "Y") %>%
  group_by(YEAR) %>%
  summarize(SWO_kept = sum(SWOK, na.rm = T),
            SWO_disc = sum(c(SWOD, SWOA), na.rm = T),
            BUM_disc = sum(c(BUMD, BUMA), na.rm = T),
            WHM_disc = sum(c(WHMD, WHMA), na.rm = T),
            SAI_disc = sum(c(SAID, SAIA), na.rm = T),
            SPX_disc = sum(c(SPXD, SPXA), na.rm = T),
            BFT_kept = sum(BFTK, na.rm = T),
            BFT_disc = sum(c(BFTD, BFTA), na.rm = T),
            BAYS_kept = sum(c(BETK, ALBK, YFTK, SKJK), na.rm = T),
            BAYS_disc = sum(c(BETD, ALBD, YFTD, SKJD,
                              BETA, ALBA, YFTA, SKJA), na.rm = T),
            PSHK_kept = sum(c(BSHK, XTHK, OCSK, PORK, SMAK), na.rm = T),  # Pelagic Sharks
            PSHK_disc = sum(c(BSHD, BSHA, XTHD, XTHA, OCSD, OCSA, 
                              PORD, PORA, SMAD, SMAA), na.rm = T),
            LSHK_kept = sum(c(SBKK, SPLK, SHHK, FALK, 
                              SSPK, TIGK, GHHK, SBUK), na.rm = T),  # Large Coastal Sharks
            LSHK_disc = sum(c(SBKD, SBKA, SPLD, SPLA, SHHD, SHHA, FALD, FALA,
                              SSPD, SSPA, TIGD, TIGA, GHHD, GHHA, SBUD, SBUA), na.rm = T),
            DOL_kept = sum(DOLK, na.rm = T),
            DOL_disc = sum(c(DOLD, DOLA), na.rm = T),
            WAH_kept = sum(WAHK, na.rm = T),
            WAH_disc = sum(c(WAHD, WAHA), na.rm = T),
            SEA_TURT = sum(c(TLB, TTL, TTG, KRT, THB, TTX), na.rm = T), # Involved (total), Injured, and Dead - just use total
            HOOKS_x1000 = round(sum(HOOKS, na.rm = T)/1000, 1)) %>% 
  TransposeTable()
tab_5.22


# Table 5.28 ----
# Reported buoy gear effort [UDP]

tab_5.28 <- UDP_logbook %>%
  filter(YEAR %in% report_years, BUOY == "Y") %>%
  group_by(YEAR) %>%
  summarise(NTRIP = length(unique(TRIPN)),
            NVESS = length(unique(ID)),
            AVG_BUOY = round(sum(BUOY_GEARS_DEPLOYED, na.rm = T)/NTRIP, 3),
            T_HOOKS = sum(BUOY_HOOKS_FISHED, na.rm = T),
            AVG_HOOK_BUOY = round(T_HOOKS/(AVG_BUOY*NTRIP), 3)) %>%
  transmute(YEAR = YEAR, NVESS, NTRIP, AVG_BUOY, T_HOOKS, AVG_HOOK_BUOY) %>%
  TransposeTable()
tab_5.28

```

### Table 5.31
Reported buoy gear landings by weight (lb dw), 2018-2022
```{r tab5.31}
data %>%
  filter(YEAR %in% years & BUOY =="Y") %>%
  group_by(YEAR) %>%
  summarise(LBSWO = sum(SWOLB, na.rm = T),
            LBDOL = sum(DOLPHIN_POUNDS, na.rm = T),
            LBOIL = sum(ESCOLAR_POUNDS, na.rm = T),
            LBWAH = sum(WAHOO_POUNDS, na.rm = T),
            LBBET = sum(BETLB, na.rm = T),
            LBKMC = sum(KING_MACKEREL_POUNDS, na.rm = T),
            LBYFT = sum(YFTLB, na.rm = T),
            LBBON = sum(BONLB, na.rm = T),
            LBBLK = sum(BLKLB, na.rm = T)) %>%
  TransposeTable()
```

### Table 5.45
Gillnet gear effort in US NE and MAR, targeting smooth dogfish, 2022
Source File: 2022_smooth_dogfish_trips.xlsx
```{r tab5.45}
smd_ggear_effort <- safe_sDog_trips %>%
  group_by(TRIPID) %>%
  summarize(NUM_ROWS = n(),
            NUM_SETS = length(unique(HAULNUM)),
            UID_SETS = paste(unique(HAULNUM), collapse =":"))
kbl(smd_ggear_effort) %>%
  kable_styling() %>%
  scroll_box(height = "400px")

data.frame(NUM_TRIPS = nrow(smd_ggear_effort),  #  34 Trips
           NUM_SETS = sum(smd_ggear_effort$NUM_SETS))  # 110 Sets
```

### Table 5.46
Catch and landings of smooth dogfish using gillnet gear
Source File: 2022_smooth_dogfish_trips.xlsx
```{r tab5.46}
smd_catch_landings <- safe_sDog_trips %>%
  group_by(DISPDESC) %>%
  filter(COMNAME == "DOGFISH, SMOOTH") %>%
  summarize(tot = sum(HAILWT))

smd_catch_landings  # 73,091 KEPT; 144 DISC

data.frame(Tot.Caught = sum(smd_catch_landings$tot),
           Num.Kept = smd_catch_landings$tot[2],
           Pct.Kept = round(smd_catch_landings$tot[2]*100/sum(smd_catch_landings$tot),1),
           Num.Disc = smd_catch_landings$tot[1],
           Pct.Disc = round(smd_catch_landings$tot[1]*100/sum(smd_catch_landings$tot),1))
```

### Table 5.48
Atl. HMS caught/kept on NEFOP BLL trips targeting FINFISH (Tilefish+)
```{r tab5.48, message=F}
safe_HMS %>%
  filter(HMSGEARCAT == "BOTTOM LONGLINE",
         grepl("TILEFISH|HADDOCK", TARG1_COMMONNAME)) %>%
  group_by(COMNAME, DISPDESC) %>%
  summarise(NUM = n()) %>%
  spread(DISPDESC, NUM) %>%
  replace(is.na(.), 0) %>%
  transmute(TOT_CAUGHT = DISC + KEPT, 
            KEPT, KEPT.PCT = round(KEPT*100/TOT_CAUGHT, digits = 1),
            DISC, DISC.PCT = round(DISC*100/TOT_CAUGHT, digits = 1)) %>%
  arrange(desc(TOT_CAUGHT))
```

### Table 5.49
Non-target SHARK caught/kept on NEFOP gillnet trips targeting TELEOSTS
```{r tab5.49, message=F}
tab <- safe_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT),
         grepl("SHARK|TUNA", COMNAME),
         !grepl("ATLANTIC SHARPNOSE|DOGFISH|SKATE", TARG1_COMMONNAME)) %>%# select(TARG1_COMMONNAME) %>% table()
  group_by(COMNAME, DISPDESC) %>%
  summarise(NUM = n()) %>%
  spread(DISPDESC, NUM) %>%
  replace(is.na(.), 0) %>%
  transmute(TOT = KEPT + DISC,
            KEPT, KEPT.PCT = round(KEPT*100/TOT, digits = 1),
            DISC, DISC.PCT = round(DISC*100/TOT, digits = 1)) %>%
  arrange(desc(TOT)) %>% print(n = 50)
kbl(tab) %>%
  kable_styling() %>%
  scroll_box(height = "450px")
```



```
## Text
### 5.3.6.2
Which species dominated the NEFOP smooth dogfish gillnet fishery
```{r text01, message=F}
tab <- safe_sDog_trips %>%
  group_by(COMNAME, DISPDESC) %>%
  summarise(TOT_WGT = sum(HAILWT, na.rm = T)) %>%
  spread(DISPDESC, TOT_WGT) %>%
  replace(is.na(.), 0) %>%
  mutate(TOT_HAILWGT = DISC + KEPT + `<NA>`)%>%
  transmute(TOT_HAILWGT, KEPT, DISC) %>%
  arrange(desc(TOT_HAILWGT))
kbl(tab) %>%
  kable_styling() %>%
  scroll_box(height = "450px")
```

Total VESSELS / SETS / TRIPS targeting smooth dogfish
```{r text02}
tab <- safe_sDog_trips %>% 
  group_by(HULLNUM1) %>%
  summarise(VESNAME = paste0(unique(VESSELNAME),sep="",collapse = ":"),
            N_TRIP = length(unique(TRIPID)),
            N_SETS = length(unique(paste(TRIPID, HAULNUM)))) %>%
  arrange(desc(HULLNUM1))
kbl(tab) %>%
  kable_styling() %>%
  scroll_box(fixed_thead = TRUE)
```

Number of Trips where Smooth dogfish were recorded caught
```{r text03}
sDog_tripCatches <- safe_sDog_trips %>% 
  filter(COMNAME == "DOGFISH, SMOOTH") %>%
  group_by(TRIPID) %>%
  summarize(NSETS = length(unique(HAULNUM)),
            UID_SETS = paste(unique(HAULNUM), collapse =":"),
            UID_CATCH = paste(unique(COMNAME), collapse = ":")) %>%
  arrange(TRIPID)

kbl(sDog_tripCatches) %>%
  kable_styling() %>%
  scroll_box(height = "400px", fixed_thead = TRUE)

sum(sDog_tripCatches$NSETS)
```

### 5.4.1
Total SETS and TRIPS targeting TILEFISH in BLL
```{r text04}
safe_HMS %>%
  filter(HMSGEARCAT == "BOTTOM LONGLINE",
         grepl("TILEFISH|HADDOCK", TARG1_COMMONNAME)) %>%
  group_by(TRIPID) %>%
  summarize(NUM_ROWS = n(),
            VESS = unique(VESSELNAME),
            NUM_SETS = length(unique(HAULNUM)),
            UID_SETS = paste(unique(HAULNUM), collapse =":"))
safe_HMS %>%
  filter(HMSGEARCAT == "BOTTOM LONGLINE",
         grepl("TILEFISH, GOLDEN", TARG1_COMMONNAME)) %>%
  group_by(TRIPID) %>%
  summarize(NUM_ROWS = n(),
            VESS = unique(VESSELNAME),
            NUM_SETS = length(unique(HAULNUM)),
            UID_SETS = paste(unique(HAULNUM), collapse =":"))
```

### 5.4.2
NUM TRIPS, SETS, VESSELS observed interacting with HMS in gillnet fishery
```{r text05}
safe_HMS %>% filter(grepl("GILLNET", HMSGEARCAT)) %>% nrow()
safe_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT)) %>%
  select(HULLNUM1, VESSELNAME, TRIPID, HAULNUM, TARG1_COMMONNAME, COMNAME) %>%
  kbl %>%
  kable_styling %>%
  scroll_box(height = "450px")
```

#### 5.4.2.1 
NUM SETS and VESSELS  interacting with HMS on gillnets
```{r text06, message=F}
# Discrepancy between me and heather regarding removing prohibited species from the data if 
# occurring in the "COMMONNAME" column. since COMNAME corresponds to the name of the bycatch
tab <- safe_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT),
         #grepl("SHARK|TUNA", COMNAME),
         !grepl("ATLANTIC SHARPNOSE|DOGFISH|SKATE", TARG1_COMMONNAME)) %>%  # select(TARG1_COMMONNAME) %>% table()
  group_by(HULLNUM1) %>%
  mutate(TRIPHAUL = paste(TRIPID, HAULNUM)) %>%
  summarise(n.trip = length(unique(TRIPID)),
            n.haul = length(unique(HAULNUM)),
            n.sets = length(unique(TRIPHAUL)))

tab %>% kbl() %>%
  kable_styling() %>%
  scroll_box(fixed_thead = TRUE, height = "450px")

data.frame(N_VESS = length(unique(tab$HULLNUM1)),
           N_TRIP = sum(tab$n.trip),
           N_SETS = sum(tab$n.sets))

safe_HMS %>%
  filter(grepl("GILLNET", HMSGEARCAT),
         #grepl("SHARK|TUNA", COMNAME),
         !grepl("ATLANTIC SHARPNOSE|DOGFISH|SKATE", TARG1_COMMONNAME)) %>%  # select(TARG1_COMMONNAME) %>% table()
  group_by(COMNAME) %>%
  summarise(num = n()) %>%
  arrange(desc(num))

# Drift Gillnet sets
drift <- safe_HMS %>%
  filter(grepl("DRIFT GILLNET", HMSGEARCAT),
         grepl("SHARK|TUNA", COMNAME),
         !grepl("ATLANTIC SHARPNOSE|DOGFISH|SKATE", TARG1_COMMONNAME)) %>% # select(HMSGEARCAT) %>% table()
  group_by(HULLNUM1, VESSELNAME) %>%
  mutate(TRIPHAUL = paste(TRIPID, HAULNUM)) %>%
  summarise(n.trip = length(unique(TRIPID)),
            n.haul = length(unique(HAULNUM)),
            n.sets = length(unique(TRIPHAUL)))
drift %>% print(n = nrow(tab))
data.frame(N_VESS = length(unique(drift$HULLNUM1)),
           N_SETS = sum(drift$n.sets),
           N_TRIP = sum(drift$n.trip))

# Sink Gillnet sets
sink <- safe_HMS %>%
  filter(grepl("SINK GILLNET", HMSGEARCAT),
         grepl("SHARK|TUNA", COMNAME),
         !grepl("ATLANTIC SHARPNOSE|DOGFISH|SKATE", TARG1_COMMONNAME)) %>% # select(HMSGEARCAT) %>% table()
  group_by(HULLNUM1, VESSELNAME) %>%
  mutate(TRIPHAUL = paste(TRIPID, HAULNUM)) %>%
  summarise(n.trip = length(unique(TRIPID)),
            n.haul = length(unique(HAULNUM)),
            n.sets = length(unique(TRIPHAUL)))
sink %>% print(n = nrow(tab)) 
data.frame(N_VESS = length(unique(sink$HULLNUM1)),
           N_SETS = sum(sink$n.sets),
           N_TRIP = sum(sink$n.trip))
```

### 5.4.3
HMS encounters and retention in TRAWLS
```{r text07}
safe_HMS %>%
  filter(grepl("TRAWL", HMSGEARCAT)) %>% #select(TARG1_COMMONNAME) %>%table()
  group_by(TARG1_COMMONNAME) %>%
  summarize(n = n()) %>% arrange(desc(n)) %>%
  kbl() %>%
  kable_styling() %>% 
  scroll_box(height = "450px")

safe_HMS %>%
  filter(grepl("TRAWL", HMSGEARCAT)) %>% #select(TARG1_COMMONNAME) %>%table()
  group_by(COMNAME) %>%
  summarize(n = n()) %>% arrange(desc(n)) %>%
  kbl() %>%
  kable_styling() %>% 
  scroll_box(height = "450px")

safe_HMS %>%
  filter(grepl("TRAWL", HMSGEARCAT),
         grepl("SWO|TUNA|SHARK", COMNAME),
         DISPDESC == "KEPT") %>% #select(TARG1_COMMONNAME) %>%table()
  group_by(TARG1_COMMONNAME) %>% #View()
  summarize(num = n(),
            spec = paste0(unique(COMNAME), collapse = ":")) %>% arrange(desc(num)) %>%
  kbl() %>%
  kable_styling()