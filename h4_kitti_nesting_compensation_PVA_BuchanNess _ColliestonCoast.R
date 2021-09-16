#' =============================================================================================================
#' ~~ Purpose: NEPVA model for Kittiwakes in Buchan Ness to Collieston Coast
#'          
#' ~ Author: BARC
#' 
#' ~ Created: 14/05/2021
#' 
#' =============================================================================================================

rm(list=objects())

#' --------------------------------
#  ====        Preamble        ====
#' --------------------------------

library(tidyverse)
library(lubridate)
library(magrittr)

options(dplyr.width = Inf)

nepva_path <- "../Seabird_PVA_Tool/R/Rpackage/"

ff <- list.files(nepva_path, pattern="functions", full.names = TRUE) ;

for(k in 1:length(ff)){ source(ff[k])} ## Automated Version 4.8

library(popbio)

modeoptions <- read.csv(file.path(nepva_path, "ModeOptions.csv")) ## Added Version 2.1

spa_name <- "Buchan Ness to Collieston Coast SPA"


#' --------------------------------
#  ====    Upload data         ====
#' --------------------------------

annual_prod <- read_csv("data/SPAs_prod_pars.csv") %>%
  filter(spa == spa_name); annual_prod



#' -----------------------------------------------------------
#  ====   NEPVA inputs: Model structure and dimensions    ====
#' -----------------------------------------------------------

## Density independence
m_dd <- "nodd"

## No demographic stochasticity
m_ds <- FALSE

## Environmental stochasticity: beta for survival rates and productivity rates
m_es <- "betagamma"

# productivity rates constrained to <= maximum brood size (i.e. max number of eggs/clutch)
m_pmax <- TRUE

# Number of subpopulations (i.e. colonies)
ncolonies <- 1



#' -----------------------------------------------------------
#  ====   NEPVA inputs: baseline demographic parameters  ====
#' -----------------------------------------------------------

# maximum brood size
maxbrood <- 2   # from Snow and Perrins (1998)

# Age of first breeding
breedAge <- 4
ageCls = seq(0, breedAge)



# Initial Population Size, given in terms of AON (i.e. breeding pairs)
N_0_fmt <- "breeding.pairs"
N_0_aon <- 11295


# starting projection year - needs to be at least a year before start of impacts
N_0_year <- 2019


# Immature survival rates to be specified for each age from 0 to age of first breeding
immat_split_surv <- TRUE

# Baseline immature survivals
bsln_immat_surv <- data.frame(Mean = c(0.790, 0.854, 0.854, 0.854), SD = c(0, 0.051, 0.051, 0.051)); bsln_immat_surv


# Baseline Adult Survival
bsln_adult_surv <- data.frame(Mean = 0.854, SD = 0.051); bsln_adult_surv

# Baseline Productivity
bsln_prod <- data.frame(Mean = annual_prod$prod_mean, SD = annual_prod$prod_sd); bsln_prod



#' ----------------------------------------------------
#  ====   NEPVA inputs: Impact Scenarios        ====
#' ----------------------------------------------------

# The PVA function requires at least one impact scenario to be specified.
# So, for the purpose of this analysis, where impacts are not required, we set up a 0-impact scenario

# Number of impacts
nimpacts <- 1

# Baseline scenario to be included
bsln_scen <- TRUE


# impacts expressed as change in annual demographic rates (i.e. rate under baseline - rate under impact)
imp_rel <- TRUE

# impacts not applied separately to each colony
imp_pop_split <- FALSE

# impacts on immatures suffer same level of impacts as adults
imp_immat_split <- FALSE

# SDs for impacts not available
imp_sds <- FALSE

# impact scenario names
imp_names <- c("impact 1")

# Impacts on productivity
imp_prod_mean <- c(0)

# Impacts on survival
imp_survadult_mean <- c(0)

# impact starting year
imp_start <- N_0_year + 1

# impact final year
imp_end <- N_0_year + 35



#' ----------------------------------------------------
#  ====   NEPVA inputs: Simulation setup      ====
#' ----------------------------------------------------

# Number of simulations
nsim <- 5000

# Years of burn-in
nburnin <- 0

# Random Seed
seed_value <- 109


# Number of year to project the population forward - 35 years
projection_length <- 35

# Final year of projection 
endYear <- N_0_year + projection_length

# start year in outputs - same as the starting simulation year
out_startYear <- N_0_year

# vector of simulated years
sim_years <- N_0_year:endYear


#' ----------------------------------------------------------------------------------------
#  ====   PVA using the " nepva.simplescenarios" - the same used in the shiny version     ====
#' ----------------------------------------------------------------------------------------

pva_output <- nepva.simplescenarios(
  model.envstoch = m_es,
  model.demostoch = m_ds,
  model.dd = m_dd,
  model.prodmax = m_pmax,
  mbs = maxbrood,
  afb = breedAge,
  npop = ncolonies,
  nscen = nimpacts,
  sim.n = nsim,
  nburn = nburnin,
  sim.seed = seed_value,
  demobase.specify.as.params = FALSE,
  demobase.splitimmat = immat_split_surv,
  demobase.prod = bsln_prod,
  demobase.survadult = bsln_adult_surv,
  demobase.survimmat = bsln_immat_surv, 
  inipop.inputformat = N_0_fmt,
  inipop.years = N_0_year, 
  inipop.vals = N_0_aon,
  impacts.relative = imp_rel,
  impacts.splitpops = imp_pop_split,
  impacts.splitimmat = imp_immat_split,
  impacts.provideses = imp_sds,
  impacts.year.start = imp_start,
  impacts.year.end = imp_end,
  impacts.scennames = imp_names,
  impacts.prod.mean = imp_prod_mean,
  impacts.prod.se = NULL,
  impacts.survadult.mean = imp_survadult_mean,
  impacts.survadult.se = NULL,
  impacts.survimmat.mean = NULL,
  impacts.survimmat.se = NULL,
  output.agetype = "age.separated",
  output.year.end = endYear,
  output.year.start = out_startYear,
  silent = FALSE, 
  output.raw = TRUE,
  changetablenames = TRUE)



# shape of raw output dataset: (nscen, npop, nyears, nsims, nages)
#
# Extract draws for chicks for baseline scenario (shape = (nyears, nsim)) and calculate potential number
# of migrants under different levels of philopatry rates

philopatry_rate <- c(0.75, 0.5, 0.2)

migrants_wide <- list()

for(i in 1:length(philopatry_rate)){
  migrants_wide[[i]] <- as_tibble(pva_output$raw$nbyage[1, 1, , , 1] * (1 - philopatry_rate[i]),
                                  .name_repair = "unique") %>%
    mutate(spa = spa_name,
           fledged_year = sim_years,
           recruited_year = fledged_year + breedAge,
           phylopatry_rate = philopatry_rate[i],
           .before = 1) %>%
    rename_with(.cols = `...1`:last_col(), .fn = ~str_replace(.x, "...", replacement = "sim_"))
}


migrants_wide <- bind_rows(migrants_wide) %>%
  arrange(fledged_year, recruited_year, desc(phylopatry_rate))


# Convert to long, which is needed for plotting
migrants_long <- migrants_wide %>%
  pivot_longer(
    cols = starts_with("sim"),
    names_to = "sim",
    names_prefix = "sim_",
    values_to = "n_migrants"
  ) %>%
  mutate(sim = as.numeric(sim)); migrants_long


migrants_long %>%
  ggplot(aes(x = recruited_year, y = n_migrants, group = factor(sim))) +
  geom_line(aes(colour = factor(phylopatry_rate))) +
  facet_grid(~phylopatry_rate)



# write-out data (wide format)
write_rds(migrants_wide, path = "outputs/nesting/pva_BuchanNess_ColliestonCoast_migrants.rds", 
          compress = "gz")




