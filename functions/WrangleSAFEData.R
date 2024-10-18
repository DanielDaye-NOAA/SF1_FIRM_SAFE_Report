WrangleSAFEData <- function(report_year) {
  require(readxl)
  require(tidyverse)
  require(writexl)
  
  # NEFOP Data ----
  #' NEFOP data is used to update a number of tables relating to HMS catch across
  #' multiple fishery types (handled previously by Heather & Delisse)
  message("Loading NEFOP data files...")
  
  # Load data sources
  NEFOP_HMS <- read_excel(paste0("./data/",report_year-1,"/NEFOP_",report_year-1,"_HMS.xlsx"))
  NEFOP_sDog_length <- read_excel(paste0("./data/",report_year-1,"/NEFOP_",report_year-1,"_smooth_dogfish_lengths.xlsx"))
  NEFOP_sDog_trips <- read_excel(paste0("./data/",report_year-1,"/NEFOP_",report_year-1,"_smooth_dogfish_trips.xlsx"))

  #' I created a separate excel file "SAFE_HMS_lookupTables.xlsx", which contains
  #' lookup tables for program codes, gear names, fish disposition, species codes,
  #' and status - pulled directly from the NEFOP manual
  message("Loading HMS Lookup Tables...")
  
  HMS_lookup_prog <- read_excel("./data/SAFE_HMS_lookupTables.xlsx", sheet = "program_codes") %>%
    mutate(PROGRAM = str_pad(Program, 3, pad = "0"))
  HMS_lookup_gear <- read_excel("./data/SAFE_HMS_lookupTables.xlsx", sheet = "gearname")
  HMS_lookup_disp <- read_excel("./data/SAFE_HMS_lookupTables.xlsx", sheet = "fishdisp")
  HMS_lookup_spec <- read_excel("./data/SAFE_HMS_lookupTables.xlsx", sheet = "species_codes", trim_ws = FALSE) %>%
    select(Species_Code, Common_Name, Scientific_Name, Log) %>%
    mutate(Species_Code = str_pad(Species_Code, 4, pad = "0"))
  HMS_lookup_stat <- read_excel("./data/SAFE_HMS_lookupTables.xlsx", sheet = "status")
  
  # Add lookup table columns to NEFOP HMS data
  message("Matching lookup tables to NEFOP HMS Data...")
  ind.prog <- match(NEFOP_HMS$PROGRAM,   HMS_lookup_prog$PROGRAM)
  ind.gear <- match(NEFOP_HMS$GEARNM,    HMS_lookup_gear$GEARNM)
  ind.targ <- match(NEFOP_HMS$TARGSPEC1, HMS_lookup_spec$Species_Code)
  ind.stat <- match(NEFOP_HMS$STATEND,   HMS_lookup_stat$STATEND)
  
  NEFOP_HMS$PROGNAME   <- HMS_lookup_prog$Program_Name[ind.prog]
  NEFOP_HMS$HMSGEARCAT <- HMS_lookup_gear$HMS_Gear_Cat[ind.gear]
  NEFOP_HMS$DISPDESC   <- ifelse(substr(NEFOP_HMS$FISHDISP, 1, 1) == 1, "KEPT", "DISC")
  NEFOP_HMS$TARG1_COMMONNAME <- HMS_lookup_spec$Common_Name[ind.targ]
  NEFOP_HMS$STATUS     <- HMS_lookup_stat$STATUS[ind.stat]
  
  rm(ind.prog, ind.gear, ind.targ, ind.stat)
  
  # Doing the same for the smooth dog trips
  message("Matching lookup tables to NEFOP Smooth dogfish Data...")
  
  ind.prog <- match(NEFOP_sDog_trips$PROGRAM,   HMS_lookup_prog$PROGRAM)
  ind.gear <- match(NEFOP_sDog_trips$GEARNM,    HMS_lookup_gear$GEARNM)
  ind.targ <- match(NEFOP_sDog_trips$T_TARGET1, HMS_lookup_spec$Species_Code)
  
  NEFOP_sDog_trips$PROGNAME   <- HMS_lookup_prog$Program_Name[ind.prog]
  NEFOP_sDog_trips$HMSGEARCAT <- HMS_lookup_gear$HMS_Gear_Cat[ind.gear]
  NEFOP_sDog_trips$DISPDESC   <- ifelse(substr(NEFOP_sDog_trips$FISHDISP, 1, 1) == 1, "KEPT", "DISC")
  NEFOP_sDog_trips$TARG1_COMMONNAME <- HMS_lookup_spec$Common_Name[ind.targ]
  
  rm(ind.prog, ind.gear, ind.targ)
  
  # Trip is coded as "Smoothdog Trip" if target columns contain code for species
  smoothDogCodes <- c(3511, 3518, 3512)
  smoothDogTrip <- vector()
  
  targCols <- which(grepl("TARGSPEC", colnames(NEFOP_HMS)))
  
  # Loop through these columns, check for smoothDogCodes, if found, will sum > 0
  for (i in 1:nrow(NEFOP_HMS)) {
    if(sum(smoothDogCodes %in% NEFOP_HMS[i, targCols]) > 0) {
      smoothDogTrip[i] = TRUE
    } else {
      smoothDogTrip[i] = FALSE
    }
  }
  NEFOP_HMS$SMOOTHDOGTRIP <- smoothDogTrip
  
  
  # NEFOP QAQC ----
  message("Check NEFOP samples for issues:")
  message("NEFOP HMS:")
  NEFOP_HMS %>%
    select(PROGRAM, PROGNAME, GEARNM, HMSGEARCAT, FISHDISP, 
           DISPDESC, TARGSPEC1, TARG1_COMMONNAME, STATEND, STATUS) %>%
    head() %>% 
    print()
  
  message("NEFOP Target Species:")
  NEFOP_HMS %>%
    select(TARGSPEC1, TARGSPEC2, TARGSPEC3, TARGSPEC4, TARGSPEC5, SMOOTHDOGTRIP) %>%
    head() %>% 
    print()
  
  message("Count of observations for each NEFOP Program:")
  table(NEFOP_HMS$PROGNAME) %>% 
    data.frame() %>% 
    arrange(desc(Freq)) %>%
    rename(Program_Name = Var1) %>% 
    print()

  message("Count of observations for each NEFOP Gear Category:")
  table(NEFOP_HMS$HMSGEARCAT) %>% 
    data.frame() %>% 
    arrange(desc(Freq)) %>%
    rename(Gear_Category = Var1) %>%
    print()
  
  message("NEFOP Fish Disposition Counts:")
  table(NEFOP_HMS$DISPDESC) %>% 
    data.frame() %>%
    rename(Disposition = Var1) %>%
    print()

  message("NEFOP Top Trip Targets:")
  table(NEFOP_HMS$TARG1_COMMONNAME) %>% 
    data.frame() %>% 
    arrange(desc(Freq)) %>%
    rename(Trip_target_1 = Var1) %>% 
    head(n=10) %>% print()
  
  message("NEFOP Catch Status:")
  table(NEFOP_HMS$STATUS) %>% 
    data.frame() %>%
    rename(Status = Var1) %>%
    print()

  
  na_df <- data.frame(prog = sum(is.na(NEFOP_HMS$PROGNAME)),
                      gear = sum(is.na(NEFOP_HMS$HMSGEARCAT)),
                      disp = sum(is.na(NEFOP_HMS$FISHDISP)),
                      targ = sum(is.na(NEFOP_HMS$TARG1_COMMONNAME)),
                      stat = sum(is.na(NEFOP_HMS$STATUS)))
  if (sum(na_df) == 0) {
    message("No missing data (NA) from NEFOP_HMS")
  } else {
    message("NEFOP_HMS contains missing (NA) values")
  }
  rm(na_df)
  
  
  # NEFOP Cleanup ----
  rm(i, smoothDogCodes, smoothDogTrip, targCols)

  # UDP Data ----
  #' UDP data is used to update catch information in PLL and other fisheries;
  #' originally handled by Tobey, who still pulls the logbook file

  # Load UDP Data into R
  message("Loading UDP Logbook Data...")
  logbook_file <- list.files(paste0('./data/',report_year-1))[grep("LOGBOOK",list.files(paste0('./data/',report_year-1)))]
  udp <- read.csv(paste0("./data/",report_year-1,"/",logbook_file))
  
  # Going to breakdown the SDATE into YEAR/MONTH/DAY to help with filtering easier
  UDP_logbook <- udp %>%
    mutate(YEAR  = substr(SDATE, 1, 4),
           MONTH = substr(SDATE, 5, 6),
           DAY   = substr(SDATE, 7, 8)) %>%
    filter(YEAR %in% (report_year-10):report_year)
  
  # Only need YEAR but will keep MONTH and DAY
  message("Check that SDATE has been properly broken into YEAR/MONTH/DAY")
  UDP_logbook %>% 
    select(SDATE, YEAR, MONTH, DAY) %>% 
    head() %>%
    print()
  
  message("Logbook entries for past 5 years:")
  UDP_logbook %>%
    filter(YEAR %in% 1900:(report_year-1)) %>%
    group_by(YEAR) %>%
    summarize(N_ENTRIES = n()) %>%
    arrange(desc(YEAR)) %>%
    head()

  message("Assigning Logbook Regions...")
  #' Region is assigned based on the LON & LAT where they occurred; not the most
  #' elegant solution but this works fine since it's run during initial setup
  
  # Temp dat
  dat <- UDP_logbook %>%
    mutate(REGION = NA)
  # Setting Region from LON/LAT
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
  # Connect back to data and remove temp df
  UDP_logbook$REGION = dat$REGION
  
  rm(dat, logbook_file, HMS_lookup_disp, HMS_lookup_gear, HMS_lookup_prog,
     HMS_lookup_spec, HMS_lookup_stat, udp)
  
  # How many entries do we have for each REGION from 2018-2022?
  UDP_logbook %>% 
    filter(YEAR %in% (report_year-5):(report_year-1)) %>% 
    group_by(REGION) %>%
    summarise(NUM_REC = n()) %>%
    arrange(desc(NUM_REC)) %>%
    print()
  
  if(!file.exists(paste0("./data/",report_year-1,"/processed_datasets/"))) {
    message("Directory for processed datasets does not exist.")
    cat("creating './data/",report_year-1,"/processed_datasets/' folder\n",sep="")
    dir.create(paste0("./data/",report_year-1,"/processed_datasets/"))
  } else {
    message("Processed datasets directory located")
  }
  
  message("Saving Datasets...")
  # NEFOP
  path = paste0("./data/",report_year-1,"/processed_datasets/")
  write_xlsx(NEFOP_HMS, format_headers = FALSE,
             path = paste0(path,"SAFE_",report_year-1,"_data_NEFOP_HMS.xlsx"))
  write_xlsx(NEFOP_sDog_length, format_headers = FALSE,
             path = paste0(path,"SAFE_",report_year-1,"_data_NEFOP_smoothdog_length.xlsx"))
  write_xlsx(NEFOP_sDog_trips, format_headers = FALSE,
             path = paste0(path,"SAFE_",report_year-1,"_data_NEFOP_smoothdog_trips.xlsx"))
  # UDP Logbook
  write_xlsx(UDP_logbook, format_headers = FALSE,
             path = paste0(path,"SAFE_",report_year-1,"_data_UDP_logbook.xlsx"))
  
  "Data Wrangled!"
}