#' =============================================================================================================
#' ~~ Purpose: explore sandeel fishing compensation on Kittiwakes from Flamborough Head and Bempton Cliffs
#' 
#' ~~ Overview: 
#'     - Main goal is to assess indirect impacts of reduction in sandeel fishing mortality. Increasing sandeel 
#'     abundance would raise food availability for kittiwakes, which would potentially increase in kittiwake's productivity.
#'     
#'     - Based on sandeel stock assessment and a relationship with kittiwake productivity from from Carrol et al. 2017, 
#'     we derived scenarios of expected productivity (given by breeding success, theta) under a combination of sandeel 
#'     recruitment levels and fishing mortality levels (2 levels of mortality under each of 3 recruitment levels). 
#'     
#'     - So, here we use PVA models to project kittiwake populations forward in time under each scenario, essentially 
#'     comparing projections at two levels of fishing mortality (total and partial) within each level of recruitment. 
#'     The output of interest is the distribution of additional birds over time due to a reduction in fishing mortality
#'     
#'     - Here we run 3 PVAs, one for each recruitment level, and simulations are matched paired between baseline 
#'     and impacts. Total fishing mortality under each recruitment constitutes the baseline scenario
#'                  
#'          
#' ~ Author: BARC
#' 
#' ~ Created: 18/05/2021
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


rm(list=objects())

nepva_path <- "../Seabird_PVA_Tool/R/Rpackage/"

ff <- list.files(nepva_path, pattern="functions", full.names = TRUE) ;

for(k in 1:length(ff)){ source(ff[k])} ## Automated Version 4.8

library(popbio)

modeoptions <- read.csv(file.path(nepva_path, "ModeOptions.csv")) ## Added Version 2.1

spa_name <- "Flamborough Head and Bempton Cliffs"


#' --------------------------------
#  ====    Upload data         ====
#' --------------------------------

annual_prod <- read_csv("../../data/SPAs_prod_pars.csv") %>%
  filter(spa == spa_name); annual_prod


load("../../data/Bruno_eval.Rdata")
theta_given_F_draws <- data.bruno

colMeans(theta_given_F_draws)



#' ------------------------------------------------
#  ====   Productivity data pre-processing     ====
#' ------------------------------------------------

#' Theta defined as fledging probability per egg.
#' Expected number of fledged per nest obtained based on maximum eggs per nest/pair per breeding season (maximum brood size). 
#' E[chicks] = n_eggs x theta = n_nests x maxBrood x theta <=> 
#' <=> E[chicks/n_nests] = theta x maxBrood

maxBrood <- 2

# Calculate draws of fledged-per-nest under each scenario
FPN_given_F_draws <- theta_given_F_draws * maxBrood

# Derive PVA baseline productivity parameters, for each level of recruitment
bsln_prod_given_F_sumStats <- FPN_given_F_draws %>%
  select(contains("Ftot")) %>%
  pivot_longer(cols = everything(), names_to = "scenario_name", values_to = "fpn_draws") %>%
  group_by(scenario_name) %>%
  summarise(mean_fpn = mean(fpn_draws), sd_fpn = sd(fpn_draws), median_fpn = median(fpn_draws), 
            lowBound_fpn = quantile(fpn_draws, prob=0.025), uppBound_fpn = quantile(fpn_draws, prob=0.975))


# Derive PVA impact productivity parameters, i.e. total_F - partial_F, for each level of recruitment
impact_prod_given_F_sumStats <- FPN_given_F_draws %>%
  mutate(HighRec_Fpar_impact = HighRec_Ftot - HighRec_Fpar, 
         MedRec_Fpar_impact = MedRec_Ftot - MedRec_Fpar, 
         LowRec_Fpar_impact = lowRec_Ftot - lowRec_Fpar) %>%
  select(contains("impact")) %>%
  pivot_longer(cols = everything(), names_to = "scenario_name", values_to = "delta_fpn_draws") %>%
  group_by(scenario_name) %>%
  summarise(mean_delta_fpn = mean(delta_fpn_draws), 
            sd_delta_fpn = sd(delta_fpn_draws), 
            median_delta_fpn = median(delta_fpn_draws), 
            lowBound_delta_fpn = quantile(delta_fpn_draws, prob=0.025), 
            uppBound_delta_fpn = quantile(delta_fpn_draws, prob=0.975))



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
maxbrood <- maxBrood   # from Snow and Perrins (1998)

# Age of first breeding
breedAge <- 4
ageCls = seq(0, breedAge)


# Initial Population Size, given in terms of AON (i.e. breeding pairs)
N_0_fmt <- "breeding.pairs"
N_0_aon <- 45504


# starting projection year - needs to be at least a year before start of impacts
N_0_year <- 2017


# Immature survival rates to be specified for each age from 0 to age of first breeding
immat_split_surv <- TRUE

# Baseline immature survivals
bsln_immat_surv <- data.frame(Mean = c(0.790, 0.854, 0.854, 0.854), SD = c(0, 0.051, 0.051, 0.051)); bsln_immat_surv


# Baseline Adult Survival
bsln_adult_surv <- data.frame(Mean = 0.854, SD = 0.051); bsln_adult_surv


# Baseline Productivity, for each recruitment level (per list element)
bsln_prod <- bsln_prod_given_F_sumStats %>%
  split(.$scenario_name) %>%
  map(., function(x){
    x %>% 
      select(mean_fpn, sd_fpn) %>%
      rename(Mean = mean_fpn, SD = sd_fpn)
  })





#' ----------------------------------------------------
#  ====   NEPVA inputs: Impact Scenarios        ====
#' ----------------------------------------------------

# Number of impacts, per recruitment level
nimpacts <- 1

# Baseline scenario to be included
bsln_scen <- TRUE


# impacts expressed as change in annual demographic rates (i.e. rate under baseline - rate under impact)
imp_rel <- TRUE

# impacts not applied separately to each colony
imp_pop_split <- FALSE

# impacts on immatures suffer same level of impacts as adults
imp_immat_split <- FALSE

# SDs for impacts available
imp_sds <- TRUE

# impact scenario names
imp_names <- impact_prod_given_F_sumStats %>%
  split(.$scenario_name) %>%
  map(~as.character(select(., scenario_name)))


# Impacts on productivity
imp_prod_mean <- impact_prod_given_F_sumStats %>%
  split(.$scenario_name) %>%
  map(~(select(., mean_delta_fpn)))


imp_prod_sd <- impact_prod_given_F_sumStats %>%
  split(.$scenario_name) %>%
  map(~(select(., sd_delta_fpn)))


# Impacts on survival
imp_survadult_mean <- c(0)

imp_survadult_sd <- c(0)

# impact starting year
imp_start <- N_0_year + 1

# impact final year
imp_end <- N_0_year + 20




#' ----------------------------------------------------
#  ====   NEPVA inputs: Simulation setup      ====
#' ----------------------------------------------------

# Number of simulations
nsim <- 1000 # 5000

# Years of burn-in
nburnin <- 0

# Random Seed
seed_value <- 10009


# Number of year to project the population forward - 20 years
projection_length <- 20

# Final year of projection 
endYear <- N_0_year + projection_length

# start year in outputs - same as the starting simulation year
out_startYear <- N_0_year

# vector of simulated years
sim_years <- N_0_year:endYear


#' ----------------------------------------------------------------------------------------
#  ====   PVA using the " nepva.simplescenarios" - the same used in the shiny version     ====
#' ----------------------------------------------------------------------------------------

#' PVA for low recruitment scenario, with total F as baseline and partial F as impact
lowRec_pva_output <- nepva.simplescenarios(
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
  demobase.prod = bsln_prod$lowRec_Ftot,
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
  impacts.scennames = imp_names$LowRec_Fpar_impact,
  impacts.prod.mean = as.numeric(imp_prod_mean$LowRec_Fpar_impact),
  impacts.prod.se = as.numeric(imp_prod_sd$LowRec_Fpar_impact),
  impacts.survadult.mean = imp_survadult_mean,
  impacts.survadult.se = imp_survadult_sd,
  output.agetype = "age.separated",
  output.year.end = endYear,
  output.year.start = out_startYear,
  silent = FALSE, 
  output.raw = TRUE,
  changetablenames = TRUE)



#' PVA for medium recruitment scenario, with total F as baseline and partial F as impact
medRec_pva_output <- nepva.simplescenarios(
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
  demobase.prod = bsln_prod$MedRec_Ftot,
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
  impacts.scennames = imp_names$MedRec_Fpar_impact,
  impacts.prod.mean = as.numeric(imp_prod_mean$MedRec_Fpar_impact),
  impacts.prod.se = as.numeric(imp_prod_sd$MedRec_Fpar_impact),
  impacts.survadult.mean = imp_survadult_mean,
  impacts.survadult.se = imp_survadult_sd,
  output.agetype = "age.separated",
  output.year.end = endYear,
  output.year.start = out_startYear,
  silent = FALSE, 
  output.raw = TRUE,
  changetablenames = TRUE)


#' PVA for high recruitment scenario, with total F as baseline and partial F as impact
highRec_pva_output <- nepva.simplescenarios(
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
  demobase.prod = bsln_prod$HighRec_Ftot,
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
  impacts.scennames = imp_names$HighRec_Fpar_impact,
  impacts.prod.mean = as.numeric(imp_prod_mean$HighRec_Fpar_impact),
  impacts.prod.se = as.numeric(imp_prod_sd$HighRec_Fpar_impact),
  impacts.survadult.mean = imp_survadult_mean,
  impacts.survadult.se = imp_survadult_sd,
  output.agetype = "age.separated",
  output.year.end = endYear,
  output.year.start = out_startYear,
  silent = FALSE, 
  output.raw = TRUE,
  changetablenames = TRUE)



lowRec_addBirds_ya

lowRec_pva_output$tab


#' ------------------------------------------------------------------------------------------
#  ====   Gather results from PVAs and post-process (inc. additional birds calculation)  ====
#' ------------------------------------------------------------------------------------------

# gather outputs from each pva into a list object
pva_outputs <- list(lowRec_pva = lowRec_pva_output$raw, 
                    midRec_pva = medRec_pva_output$raw, 
                    highRec_pva = highRec_pva_output$raw)


# reshape outputs and compute changes from baseline
pva_outputs_addBirds_long <- pva_outputs %>%
  map(., function(x){
    
    # shape of raw nbyage dataset: (nscen, npop, nyears, nsims, nages)
    # transpose for post-processing convenience
    nbyage <- aperm(x$nbyage, perm = c(1, 2, 3, 5, 4))  # (nscen, npop, nyears, nages, nsims)
    
    ageCls <- 0:x$afb
    years <- x$years
    sim_n <- x$sim.n
    
    #browser()
    
    out <- list()
    k <- 1
    for(y in 1:length(years)){
      for(a in 1:length(ageCls)){
        out[[k]] <- as_tibble_col(nbyage[1, 1, y, a, ], column_name = "bsln_totF") %>%
          mutate(imp_parF = nbyage[2, 1, y, a, ],
                 n_change_from_bsln = imp_parF - bsln_totF) %>%    # compute change from baseline
          mutate(year = sim_years[y],
                 age_class = ageCls[a],
                 sim = 1:sim_n,
                 .before = 1)
        k <- k + 1
      }
    }
    
    out <- bind_rows(out)
    
    return(out)
  })


str(pva_outputs_addBirds_long)



# Some sense-check plots
pva_outputs_addBirds_long %>%
  bind_rows(.id = "recruit_scen") %>%
  filter(age_class == 4) %>%
  group_by(recruit_scen, year) %>%
  summarise(lowBound = quantile(n_change_from_bsln, probs = c(0.025)), 
            med = quantile(n_change_from_bsln, probs = c(0.5)),
            uppBound = quantile(n_change_from_bsln, probs = c(0.975))) %>%
  ggplot(aes(x = year, y = med)) +
  geom_line(aes(x = year, y = med, col = recruit_scen), size = 1) +
  geom_ribbon(aes(ymin = lowBound, ymax = uppBound, col = recruit_scen, fill =recruit_scen), alpha = 0.3,
              linetype = "dashed") +
  labs(y = "Change from baseline (adults)")


pva_outputs_addBirds_long %>%
  bind_rows(.id = "recruit_scen") %>%
  filter(age_class == 4) %>%
  select(-n_change_from_bsln) %>%
  pivot_longer(bsln_totF:imp_parF, names_to = "scenario") %>%
  group_by(recruit_scen, year, scenario) %>%
  summarise(lowBound = quantile(value, probs = c(0.025)), 
            med = quantile(value, probs = c(0.5)),
            uppBound = quantile(value, probs = c(0.975))) %>%
  ggplot(aes(x = year, y = med)) +
  geom_line(aes(x = year, y = med, col = scenario), size = 1) +
  geom_ribbon(aes(ymin = lowBound, ymax = uppBound,  col = scenario, fill = scenario), alpha = 0.3, linetype = "dashed")+
  facet_wrap(~recruit_scen, ncol = 3) +
  theme(legend.position="bottom") +
  labs(y = "Adults")



pva_outputs_addBirds_long %>%
  bind_rows(.id = "recruit_scen") %>%
  filter(age_class == 4) %>%
  select(-n_change_from_bsln) %>%
  pivot_longer(bsln_totF:imp_parF, names_to = "scenario") %>%
  group_by(recruit_scen, year, scenario) %>%
  summarise(med = quantile(value, probs = c(0.5))) %>%
  filter(year %in% c(2027, 2037)) %>%
  pivot_wider(names_from = scenario, values_from = med) %>%
  mutate(ptcg_change = (imp_parF-bsln_totF)/bsln_totF)


ktable

#' -------------------------------
#  ====   Write out data      ====
#' -------------------------------

write_rds(pva_outputs_addBirds_long, path = "../../outputs/fishing/pva_Flamb_Head_Bempt_Cliffs_outputs_scen_ya.rds", 
          compress = "gz")


