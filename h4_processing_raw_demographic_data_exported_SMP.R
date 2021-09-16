#' =================================================================================================================
#' ~~ Code Purpose: Process seabird demographic data for each SPA of interest extracted from the Seabird Monitoring 
#'                  Program (SMP) database 
#'          
#' ~ Author: BARC
#' 
#' ~ Created: 19/03/2021
#' 
#' =================================================================================================================


#' --------------------------------
#  ====        Preamble        ====
#' --------------------------------

library(tidyverse)
library(lubridate)
library(magrittr)

options(dplyr.width = Inf)



#' --------------------------------
#  ====    Coquet Island       ====
#' --------------------------------

#' Note 1: replacing initially considered the Northumberland Marine pSPA. 
#' Note 2: Coquet Island data contained in initially gathered data

# --- Colony Counts  -----#

cc_coquet <- read_csv("data/Northumberland_colony counts_SMP database_data_export.csv") %>%
  rename_with(str_replace, pattern = " ", replacement = "_") %>%
  arrange(Start_date) %>%
  filter(Count > 0,
         Master_site == "Coquet Island SPA") %>%
  mutate(Year = year(dmy(Start_date))) ; cc_coquet


#' find out if there are more than one entry in one year, per site
cc_coquet %>% group_by(Year, Site) %>% summarise(n = n()) %>% filter(n>1)


# graphical check to see if there is anything out of order
cc_coquet %>%
  ggplot() +
  geom_col(aes(Year, Count)) +
  facet_wrap(~Site, nrow = 3)

# COMMENT: Colony counts increase over time - due to better surveying or increasing population??

# summary check
cc_coquet %>%
  group_by(Site) %>%
  summarise(min(Count), mean(Count), sd(Count),max(Count))


# Annual counts in SPA
counts_coquet <- cc_coquet %>%
  group_by(Species, Year, Unit) %>%
  summarise(Count = sum(Count)) %>%
  mutate(spa = "Coquet Island", .after = "Species") %>%
  mutate(source = "smp_db")




# --- Breeding Success -----#

bs_coquet <- read_csv("data/Northumberland_breeding_success_SMP database_data_export.csv") %>%
  rename_with(str_replace, pattern = " ", replacement = "_") %>%
  arrange(Year, Site) %>%
  filter(Master_site == "Coquet Island SPA") %>%
  mutate(fledgedPerPair = Fledged_count/Count) #' productivity (No. fledged/pair)


#' quick check summary
bs_coquet %>%
  group_by(Site) %>%
  summarise(startYear = min(Year), endYear = max(Year), mean_FPP = mean(fledgedPerPair))

#' More than one entry per year?
bs_coquet %>%
  group_by(Year, Site) %>%
  summarise(n=n()) %>% arrange(desc(n))

#' # Add colony counts data
#' bs_coquet %<>% 
#'   left_join(., select(cc_coquet, Year, Count), by = c("Species", "Year")) %>%
#'   rename(bs_count = Count.x, cc_count=(Count.y))
#' 
#' table(bs_coquet$Year)
#' 
#' #' Up till 2006, colony counts and number of nests surveyed for breeding success are equal
#' bs_coquet %>%
#'   select(Year,bs_count, cc_count) %>%
#'   pivot_longer(-Year, names_to = "count_type", values_to = "Count") %>%
#'   ggplot(aes(x = Year)) +
#'   geom_line(aes(y = Count, col = count_type))


#' Data available restricted to one site, one entry per year, 
#' so total mean productivity given by simple arithmetic mean over all FPP values
#' SD of productivity follows directly
prod_coquet <- bs_coquet %>% 
  group_by(Species, Master_site) %>%
  summarise(prod_mean = mean(fledgedPerPair), prod_sd = sd(fledgedPerPair),
            startYear = min(Year), endYear = max(Year), n = n())%>%
  mutate(source = "smp_db") %>%
  rename(spa = Master_site)

prod_coquet


#' ------------------------------------------
#  ====  St Abb’s Head to Fast Castle   ====
#' ------------------------------------------


# --- Colony Counts -----#

cc_st_abbs <- read_csv("data/St Abb’s Head to Fast Castle_colony counts_SMP database_data_export.csv") %>%
  rename_with(str_replace, pattern = " ", replacement = "_") %>%
  arrange(Start_date) %>%
  filter(Count > 0) %>%
  mutate(Year = year(dmy(Start_date))) ; cc_st_abbs


#' find out if there are more than one entry in one year, per site
cc_st_abbs %>% group_by(Year, Site) %>% summarise(n = n()) %>% filter(n>1)

cc_st_abbs %>% filter(Site == "St Abb's Head NNR") %>% filter(Year == 2006)
cc_st_abbs %>% filter(Site == "St Abb's Head NNR") %>% filter(Year == 2010)
cc_st_abbs %>% filter(Site == "Broadhaven to Moorburn Point") %>% filter(Year == 2016)

#' Not sure how to combine partial multiple counts in a given year. Are they counts from different cliffs?
#' Maybe multiple counting events of the same colony/cliff?
#'  
#' Summing them over, i.e. assuming counts made at different sections/cliffs of the site
cc_st_abbs %<>% 
  arrange(Year) %>%
  group_by(Species, County, Master_site, Site, Year, Unit) %>%
  summarise(Count = sum(Count), Accuracy = paste(Accuracy, collapse = "-"), .groups = "drop")

# graphical check to see if there is anything out of order
cc_st_abbs %>%
  ggplot() +
  geom_col(aes(Year, Count)) +
  facet_wrap(~Site, nrow = 3)

# summary check
cc_st_abbs %>%
  group_by(Site) %>%
  summarise(min(Count), mean(Count), sd(Count),max(Count))


#' Official counts for kittiwakes in St Abb's Head NNR reported in SMP Report 1986–2018 (Table 1):
#' (https://jncc.gov.uk/our-work/black-legged-kittiwake-rissa-tridactyla/)
#' Reports 3,244 kittiwakes in 2018 (Table 1)
cc_st_abbs %>% filter(Year == 2018) # So, summing over counts in each year appears to be the correct approach
#' Reports 11,077 in 2000
cc_st_abbs %>% filter(Year == 2000)
cc_st_abbs %>% filter(Year == 2017) 


# Annual counts in SPA
counts_st_abbs <- cc_st_abbs %>%
  group_by(Species, Master_site,Year, Unit) %>%
  summarise(Count = sum(Count)) %>%
  rename(spa = Master_site) %>%
  mutate(source = "smp_db")




# --- Breeding Success  -----#

bs_st_abbs <- read_csv("data/St Abb’s Head to Fast Castle_breeding_success_SMP database_data_export.csv") %>%
  rename_with(str_replace, pattern = " ", replacement = "_") %>%
  arrange(Year, Site) %>%
  mutate(fledgedPerPair = Fledged_count/Count) #' productivity (No. fledged/pair)


#' Quick check summary
bs_st_abbs %>%
  group_by(Master_site, Site) %>%
  summarise(startYear = min(Year), endYear = max(Year), mean_FPP = mean(fledgedPerPair))

#' More than one entry per year?
bs_st_abbs %>%
  group_by(Year, Site) %>%
  summarise(n=n()) %>% arrange(desc(n))

bs_st_abbs %>%
  ggplot(aes(x=Year, y = fledgedPerPair)) +
  geom_line()



#' Data available restricted to one site, one entry per year, 
#' so total mean productivity given by simple arithmetic mean over all FPP values
#' SD of productivity follows directly
prod_st_abbs <- bs_st_abbs %>% 
  group_by(Species, Master_site) %>%
  #filter(Year >= 2010) %>%
  summarise(prod_mean = mean(fledgedPerPair), prod_sd = sd(fledgedPerPair),
           startYear = min(Year), endYear = max(Year), n = n())%>%
  mutate(source = "smp_db") %>%
  rename(spa = Master_site)

prod_st_abbs



#' ---------------------------------------
#  ====         Farne Islands          ====
#' ---------------------------------------

# --- Colony Counts  -----#

cc_farne_islands <- read_csv("data/Farne Islands SPA_colony counts_SMP database_data_export.csv") %>%
  rename_with(str_replace, pattern = " ", replacement = "_") %>%
  arrange(Start_date) %>%
  filter(Count > 0) %>%
  mutate(Year = year(dmy(Start_date))) ; cc_farne_islands

#' Summing multiple entries in a year, as seen in previous section
cc_farne_islands %<>% 
  arrange(Year) %>%
  group_by(Species, County, Master_site, Site, Year, Unit) %>%
  summarise(Count = sum(Count), Accuracy = paste(Accuracy, collapse = "-"), .groups = "drop")

# graphical check to see if there is anything out of order
cc_farne_islands %>%
  ggplot() +
  geom_col(aes(Year, Count)) +
  facet_wrap(~Site, nrow = 3)

# summary check
cc_farne_islands %>%
  group_by(Site) %>%
  summarise(min(Count), mean(Count), sd(Count),max(Count))



# Annual counts in SPA
counts_farne_islands <- cc_farne_islands %>%
  group_by(Species, Master_site, Year , Unit) %>%
  summarise(Count = sum(Count)) %>%
  rename(spa = Master_site) %>%
  mutate(source = "smp_db")

# Checking against SMP report on Kittiwakes (see link above)
# Reports 5,096 in 1998
counts_farne_islands %>% filter(Year == 1998)  # 5009 - just below reported. Maybe there is other site to include?
# reports 5,327 in 2017 (same figure as Angus)
counts_farne_islands %>% filter(Year == 2017)  # 4753 - well below reported...!!!




# --- Breeding Success  -----#

bs_farne_islands <- read_csv("data/Farne Islands SPA_breeding_success_SMP database_data_export.csv") %>%
  rename_with(str_replace, pattern = " ", replacement = "_") %>%
  arrange(Year, Site) %>%
  mutate(fledgedPerPair = Fledged_count/Count) #' productivity (No. fledged/pair)

#' Quick check summary
bs_farne_islands %>%
  group_by(Master_site, Site) %>%
  summarise(startYear = min(Year), endYear = max(Year), mean_FPP = mean(fledgedPerPair))

#' More than one entry per year?
bs_farne_islands %>%
  group_by(Year, Site) %>%
  summarise(n=n()) %>% arrange(desc(n))

bs_farne_islands %>%
  ggplot(aes(x=Year, y = fledgedPerPair)) +
  geom_line()

#' Data available restricted to one site, one entry per year, 
#' so total mean productivity given by simple arithmetic mean over all FPP values
#' SD of productivity follows directly
prod_farne_islands <- bs_farne_islands %>% 
  group_by(Species, Master_site) %>%
  summarise(prod_mean = mean(fledgedPerPair), prod_sd = sd(fledgedPerPair),
            startYear = min(Year), endYear = max(Year), n = n())%>%
  mutate(source = "smp_db") %>%
  rename(spa = Master_site)

prod_farne_islands


#' ---------------------------------------
#  ====         Forth Islands         ====
#' ---------------------------------------

# --- Colony Counts  -----#

cc_forth_islands <- read_csv("data/Forth Islands SPA_colony counts_SMP database_data_export.csv") %>%
  rename_with(str_replace, pattern = " ", replacement = "_") %>%
  arrange(Start_date) %>%
  filter(Count > 0) %>%
  mutate(Year = year(dmy(Start_date))) ; cc_forth_islands

#' Summing multiple entries in a year, as seen in previous section
cc_forth_islands %<>% 
  arrange(Year) %>%
  group_by(Species, County, Master_site, Site, Year, Unit) %>%
  summarise(Count = sum(Count), Accuracy = paste(Accuracy, collapse = "-"), .groups = "drop")

# graphical check to see if there is anything out of order
cc_forth_islands %>%
  ggplot() +
  geom_col(aes(Year, Count)) +
  facet_wrap(~Site, nrow = 3)

# summary check
cc_forth_islands %>%
  group_by(Site) %>%
  summarise(min(Count), mean(Count), sd(Count),max(Count), n = n())

#' There is an entry with AOS (Apparently Occupied Site). 
#' For counting purposes, assuming it has the same value as AON, so replacing AOS with AON
table(cc_forth_islands$Unit)
cc_forth_islands %<>%
  mutate(Unit = str_replace_all(Unit, "AOS", "AON"))



# Annual counts in SPA
counts_forth_islands <- cc_forth_islands %>%
  group_by(Species, Master_site, Year, Unit) %>%
  summarise(Count = sum(Count)) %>%
  rename(spa = Master_site) %>%
  mutate(source = "smp_db")

# There are 
table(counts_forth_islands$Unit)


# Checking against SMP report on Kittiwakes (see link above)
counts_forth_islands %>% filter(Year == 2018)     # reports 3,514 in 2018 bang on!
counts_forth_islands %>% filter(Year == 2000)     # reports 6,632 in 2000 bang on!




# ---  Breeding Success  -----#

#' NOTE: breeding data not available from the SMP database. 
#' However CEH publishes breeding season data for the Isle of May
#' (https://www.ceh.ac.uk/isle-may-breeding-season-summaries)

bs_forth_islands <- read_csv("data/Kittiwake_IsleOfMay_productivities from CEH.csv") %>%
  mutate(Species = "Kittiwake", Master_site = "Forth Islands SPA", Site = "Isle of May", .before = "Year")
  
bs_forth_islands %>%
  ggplot(aes(x=Year, y = Productivity)) +
  geom_line()

prod_forth_islands <- bs_forth_islands %>% 
  group_by(Species, Master_site) %>%
  filter(Year >= 2010) %>%
  summarise(prod_mean = mean(Productivity), prod_sd = sd(Productivity),
            startYear = min(Year), endYear = max(Year), n = n())%>%
  mutate(source = "ceh") %>%
  rename(spa = Master_site)

prod_forth_islands



#' ---------------------------------------------
#  ====   Buchan Ness to Collieston Coast  ====
#' ---------------------------------------------

# --- Colony Counts  -----#

cc_buchan_collie <- read_csv("data/Buchan Ness to Collieston Coast SPA_colony counts_SMP database_data_export.csv") %>%
  rename_with(str_replace, pattern = " ", replacement = "_") %>%
  filter(Count > 0) %>%
  mutate(Year = year(dmy(Start_date))) %>%
  arrange(Year) ; cc_buchan_collie

#' Summing multiple entries in a year
cc_buchan_collie %<>% 
  group_by(Species, County, Master_site, Site, Year, Unit) %>%
  summarise(Count = sum(Count), Accuracy = paste(Accuracy, collapse = "-"), .groups = "drop")

# graphical check to see if there is anything out of order
cc_buchan_collie %>%
  ggplot() +
  geom_col(aes(Year, Count)) +
  facet_wrap(~Site, nrow = 3)

# summary check
cc_buchan_collie %>%
  group_by(Site) %>%
  summarise(min(Count), mean(Count), sd(Count),max(Count))

# Annual counts in SPA
counts_buchan_collie <- cc_buchan_collie %>%
  group_by(Species, Master_site, Year, Unit) %>%
  summarise(Count = sum(Count)) %>%
  rename(spa = Master_site) %>%
  mutate(source = "smp_db")


# Checking against SMP report on Kittiwakes (see link above)
# reports 12,542 in 2007
counts_buchan_collie %>% filter(Year == 2007)  # bang on!
# reports 14,091 in 2001
counts_buchan_collie %>% filter(Year == 2001)  # bang on!






# --- Breeding Success  -----#

bs_buchan_collie <- read_csv("data/Buchan Ness to Collieston Coast SPA_breeding_success_SMP database_data_export.csv") %>%
  rename_with(str_replace, pattern = " ", replacement = "_") %>%
  arrange(Year, Site) %>%
  mutate(fledgedPerPair = Fledged_count/Count) #' productivity (No. fledged/pair)

#' Quick check summary
bs_buchan_collie %>%
  group_by(Master_site, Site) %>%
  summarise(startYear = min(Year), endYear = max(Year), mean_FPP = mean(fledgedPerPair), n())

#' More than one entry per year?
bs_buchan_collie %>%
  group_by(Year, Site) %>%
  summarise(n=n()) %>% arrange(desc(n))

# Multiple entries in for different plots in a site, for a subset of the years
bs_buchan_collie %>% filter(Year == 2009)
bs_buchan_collie %>% filter(Year == 2010)
bs_buchan_collie %>% filter(Year == 1996)

#' No data on relative size of plots, so using arithmetic mean
annualProd_buchan_collie <- bs_buchan_collie %>% 
  group_by(Species, Master_site, Site, Year) %>%
  summarise(annualAvgProd =  mean(fledgedPerPair))

annualProd_buchan_collie %>%
  ggplot(aes(x=Year, y = annualAvgProd)) +
  geom_line()


# Total mean and sd productivity
prod_buchan_collie <- annualProd_buchan_collie %>%
  group_by(Species, Master_site) %>%
  summarise(prod_mean = mean(annualAvgProd), prod_sd = sd(annualAvgProd),
            startYear = min(Year), endYear = max(Year), n = n())%>%
  mutate(source = "smp_db")  %>%
  rename(spa = Master_site)

prod_buchan_collie



#' ---------------------------------------------
#  ===              Fowlsheugh               ===
#' ---------------------------------------------

# --- Colony Counts  -----#

cc_fowlsheugh <- read_csv("data/Fowlsheugh SPA_colony counts_SMP database_data_export.csv") %>%
  rename_with(str_replace, pattern = " ", replacement = "_") %>%
  filter(Count > 0) %>%
  mutate(Year = year(dmy(Start_date))) %>%
  arrange(Year) ; cc_fowlsheugh

#' Summing multiple entries in a year
cc_fowlsheugh %<>% 
  group_by(Species, County, Master_site, Site, Year, Unit) %>%
  summarise(Count = sum(Count), Accuracy = paste(Accuracy, collapse = "-"), .groups = "drop")

# graphical check to see if there is anything out of order
cc_fowlsheugh %>%
  ggplot() +
  geom_col(aes(Year, Count)) +
  facet_wrap(~Site, nrow = 3)

# summary check
cc_fowlsheugh %>%
  group_by(Site) %>%
  summarise(min(Count), mean(Count), sd(Count),max(Count))

# Annual counts in SPA
counts_fowlsheugh <- cc_fowlsheugh %>%
  group_by(Species, Master_site, Year, Unit) %>%
  summarise(Count = sum(Count)) %>%
  rename(spa = Master_site) %>%
  mutate(source = "smp_db")

# Checking against SMP report on Kittiwakes (see link above) 
# reports 14,039 in 2018
counts_fowlsheugh %>% filter(Year == 2018)  # bang on!
# reports 18,800 in 1999
counts_fowlsheugh %>% filter(Year == 1999)  # way off! Looks like an error in report due to failing to sum over sites




# --- Breeding Success  -----#

bs_fowlsheugh <- read_csv("data/Fowlsheugh SPA_breeding_success_SMP database_data_export.csv") %>%
  rename_with(str_replace, pattern = " ", replacement = "_") %>%
  arrange(Year, Site) %>%
  mutate(fledgedPerPair = Fledged_count/Count) #' productivity (No. fledged/pair)

#' Quick check summary
bs_fowlsheugh %>%
  group_by(Master_site, Site) %>%
  summarise(startYear = min(Year), endYear = max(Year), mean_FPP = mean(fledgedPerPair), n())

#' More than one entry per year?
bs_fowlsheugh %>%
  group_by(Year, Site) %>%
  summarise(n=n()) %>% arrange(desc(n))

# Multiple entries in for different plots in a site, for a subset of the years
bs_fowlsheugh %>% filter(Year == 2009)

#' No data on relative size of plots, so using arithmetic mean
annualProd_fowlsheugh <- bs_fowlsheugh %>% 
  group_by(Species, Master_site, Site, Year) %>%
  summarise(annualAvgProd =  mean(fledgedPerPair))


annualProd_fowlsheugh %>%
  ggplot(aes(x=Year, y = annualAvgProd)) +
  geom_line()


# Total mean and sd productivity
prod_fowlsheugh <- annualProd_fowlsheugh %>%
  group_by(Species, Master_site) %>%
  summarise(prod_mean = mean(annualAvgProd), prod_sd = sd(annualAvgProd),
            startYear = min(Year), endYear = max(Year), n = n())%>%
  mutate(source = "smp_db") %>%
  rename(spa = Master_site)

prod_fowlsheugh



#' ---------------------------------------------
#  ===    Troup, Pennan and Lion’s Heads     ===
#' ---------------------------------------------

# --- Colony Counts  -----#

cc_troup_pen_lion <- read_csv("data/Troup, Pennan and Lion's Heads SPA_colony counts_SMP database_data_export.csv") %>%
  rename_with(str_replace, pattern = " ", replacement = "_") %>%
  filter(Count > 0) %>%
  mutate(Year = year(dmy(Start_date))) %>%
  arrange(Year) ; cc_troup_pen_lion

#' Summing multiple entries in a year
cc_troup_pen_lion %<>% 
  group_by(Species, County, Master_site, Site, Year, Unit) %>%
  summarise(Count = sum(Count), Accuracy = paste(Accuracy, collapse = "-"), .groups = "drop")

# graphical check to see if there is anything out of order
cc_troup_pen_lion %>%
  ggplot() +
  geom_col(aes(Year, Count)) +
  facet_wrap(~Site, nrow = 3)

# summary check
cc_troup_pen_lion %>%
  group_by(Site) %>%
  summarise(min(Count), mean(Count), sd(Count),max(Count))


# Annual counts in SPA
counts_troup_pen_lion <- cc_troup_pen_lion %>%
  group_by(Species, Master_site, Year, Unit) %>%
  summarise(Count = sum(Count)) %>%
  rename(spa = Master_site) %>%
  mutate(source = "smp_db")


# Checking against SMP report on Kittiwakes (see link above) 
# reports 18,482 in 2001
counts_troup_pen_lion %>% filter(Year == 2001)  # somewhat off!
# reports 10,503 in 2007
counts_troup_pen_lion %>% filter(Year == 2007)  # way off! Report agrees with Angus... value here identical to record in database


cc_troup_pen_lion %>% filter(Year == 2017)




# --- Breeding Success  -----#

bs_troup_pen_lion <- read_csv("data/Troup, Pennan and Lion's Heads SPA_breeding_success_SMP database_data_export.csv") %>%
  rename_with(str_replace, pattern = " ", replacement = "_") %>%
  arrange(Year, Site) %>%
  mutate(fledgedPerPair = Fledged_count/Count) #' productivity (No. fledged/pair)

#' Quick check summary
bs_troup_pen_lion %>%
  group_by(Master_site, Site) %>%
  summarise(startYear = min(Year), endYear = max(Year), mean_FPP = mean(fledgedPerPair), n())

#' More than one entry per year?
bs_troup_pen_lion %>%
  group_by(Year, Site) %>%
  summarise(n=n()) %>% arrange(desc(n))

#' Multiple entries due to likely record duplication - carry on with taking the annual mean, 
#' as duplicates don't affect the mean
bs_troup_pen_lion %>% filter(Year == 2018)
bs_troup_pen_lion %>% filter(Year == 2016)


#' No data on relative size of plots, so using arithmetic mean
annualProd_troup_pen_lion <- bs_troup_pen_lion %>% 
  group_by(Species, Master_site, Site, Year) %>%
  summarise(annualAvgProd =  mean(fledgedPerPair))


annualProd_troup_pen_lion %>%
  ggplot(aes(x=Year, y = annualAvgProd)) +
  geom_line()


# Total mean and sd productivity
prod_troup_pen_lion <- annualProd_troup_pen_lion %>%
  group_by(Species, Master_site) %>%
  summarise(prod_mean = mean(annualAvgProd), prod_sd = sd(annualAvgProd),
            startYear = min(Year), endYear = max(Year), n = n()) %>%
  mutate(source = "smp_db") %>%
  rename(spa = Master_site)



prod_troup_pen_lion



#' -----------------------------------------------------
#  ===    Flamborough Head and Bempton Cliffs        ===
#' -----------------------------------------------------

#' --- Colony Counts  -----#

cc_flamb_bemp_cliffs <- read_csv("data/Flamborough Head and Bempton Cliffs_colony counts_SMP database_data_export.csv") %>%
  rename_with(str_replace, pattern = " ", replacement = "_") %>%
  filter(Count > 0) %>%
  mutate(Year = year(dmy(Start_date))) %>%
  arrange(Year) ; cc_flamb_bemp_cliffs


#' Summing multiple entries in a year
cc_flamb_bemp_cliffs %<>% 
  group_by(Species, County, Master_site, Site, Year, Unit) %>%
  summarise(Count = sum(Count), Accuracy = paste(Accuracy, collapse = "-"), .groups = "drop")


# graphical check to see if there is anything out of order
cc_flamb_bemp_cliffs %>%
  ggplot() +
  geom_col(aes(Year, Count)) +
  facet_wrap(~Site, nrow = 3)

# summary check
cc_flamb_bemp_cliffs %>%
  group_by(Site) %>%
  summarise(min(Count), mean(Count), sd(Count),max(Count))


# Annual counts in SPA
counts_flamb_bemp_cliffs <- cc_flamb_bemp_cliffs %>%
  group_by(Species, Site, Year, Unit) %>%
  summarise(Count = sum(Count)) %>%
  rename(spa = Site) %>%
  mutate(source = "smp_db")

# Checking against SMP report on Kittiwakes (see link above)
# reports 45,504 in 2017
counts_flamb_bemp_cliffs %>% filter(Year == 2017)  # bang on!
# reports 42,582 in 2000
counts_flamb_bemp_cliffs %>% filter(Year == 2000)  # bang on!




# --- Breeding Success  -----#

bs_flamb_bemp_cliffs <- read_csv("data/Flamborough Head and Bempton Cliffs_breeding_success_SMP database_data_export.csv") %>%
  rename_with(str_replace, pattern = " ", replacement = "_") %>%
  arrange(Year, Site) %>%
  mutate(fledgedPerPair = Fledged_count/Count) #' productivity (No. fledged/pair)

#' Quick check summary
bs_flamb_bemp_cliffs %>%
  group_by(Master_site, Site) %>%
  summarise(startYear = min(Year), endYear = max(Year), mean_FPP = mean(fledgedPerPair), n())

#' More than one entry per year?
bs_flamb_bemp_cliffs %>%
  group_by(Year, Site) %>%
  summarise(n=n()) %>% arrange(desc(n))

#' Multiple entries per year due to records from subsite plots, for a subset of years
bs_flamb_bemp_cliffs %>% filter(Year == 2010)
bs_flamb_bemp_cliffs %>% filter(Year == 2011)


#' No data on relative size of plots, so using arithmetic mean to get mean annual productivities
annualProd_flamb_bemp_cliffs <- bs_flamb_bemp_cliffs %>% 
  group_by(Species, Master_site, Site, Year) %>%
  summarise(annualAvgProd =  mean(fledgedPerPair))

annualProd_flamb_bemp_cliffs %>%
  ggplot(aes(x=Year, y = annualAvgProd)) +
  geom_line()




prod_flamb_bemp_cliffs <- annualProd_flamb_bemp_cliffs %>%
  filter(Year >= 2010) %>%
  group_by(Species, Site) %>%
  summarise(prod_mean = mean(annualAvgProd), prod_sd = sd(annualAvgProd),
            startYear = min(Year), endYear = max(Year), n = n()) %>%
  mutate(source = "smp_db") %>%
  rename(spa = Site)
  

prod_flamb_bemp_cliffs






#' -----------------------------------------------------------
#  ===    Gather and write-out productivity parameters     ===
#' -----------------------------------------------------------

SPAs_prod_pars <- bind_rows(
  prod_flamb_bemp_cliffs,
  prod_coquet,
  prod_st_abbs,
  prod_farne_islands,
  prod_forth_islands,
  prod_buchan_collie,
  prod_fowlsheugh,
  prod_troup_pen_lion
)

write_csv(SPAs_prod_pars, "data/SPAs_prod_pars.csv")



#' -------------------------------------------
#  ===    Gather colony counts data        ===
#' -------------------------------------------

spa_annual_counts <- bind_rows(
  counts_flamb_bemp_cliffs,
  counts_coquet,
  counts_st_abbs,
  counts_farne_islands,
  counts_forth_islands,
  counts_buchan_collie,
  counts_fowlsheugh,
  counts_troup_pen_lion
  )

# rearrange to wider format to get spas as columns for annual counts across rows (AON)
spa_annual_counts %<>%
  pivot_wider(names_from = spa, values_from = Count ) %>%
  arrange(Year)


write_csv(spa_annual_counts, "data/SPAs_annual_counts.csv")










#' #' ---------------------------------------------------------------------------------------
#' #  ===    Quick look at regional reconstructured survivals  from Horswill et al 2021   ===
#' #' --------------------------------------------------------------------------------------
#' 
#' reconst_demRates <- readxl::read_xlsx("../../data/kitti_demog_rates_Horswill_etal.xlsx") %>%
#'   mutate(population_main = str_extract(string = Population, pattern =".+(?=\\s:)|(.+$)"))
#' 
#' unique(reconst_demRates$population_main)
#' 
#' reconst_adultSurvivals <- reconst_demRates %>%
#'   filter(Demographic_rate == "Adult survival",
#'          population_main %in% c("Flamborough Head and Bempton Cliffs", "Fowlsheugh RSPB", "Farne Islands", 
#'                                 "Troup, Pennan and Lion's Heads" , "St Abb's Head NNR")) %>%
#'   group_by(Demographic_rate, population_main) %>%
#'   summarise(mean_reconst = mean(Reconst_median), sd_reconst = sd(Reconst_median), n = n())
#'   
#' reconst_adultSurvivals
#' 


