# Calculate cost and effect outputs

library(tidyverse)
library(netz)
library(treasure)
library(here)
library(data.table)

# Malaria model was simulated using malariasimulation (https://github.com/mrc-ide/malariasimulation)

# Functions --------------------------------------------------------------------

# Functions for DALY calculations
# mortality
mortality_rate <- function(x,
                           scaler = 0.215)           # severe case to death scaler 
{ 
  x %>%
    dplyr::mutate(mortality_rate = scaler * .data$sev) %>% 
    dplyr::mutate(deaths = .data$mortality_rate * .data$n)  # deaths
}

# case and death uncertainty
outcome_uncertainty <- function(x,
                                cases_cv = 0.227,   # case uncertainty SD scaler
                                deaths_cv = 0.265){ # death uncertainty SD scaler
  x %>%
    dplyr::mutate(cases_lower = round(pmax(0, stats::qnorm(0.025, .data$cases, .data$cases * cases_cv))),
                  cases_upper = round(stats::qnorm(0.975, .data$cases, .data$cases * cases_cv)),
                  deaths_lower = round(pmax(0, stats::qnorm(0.025, .data$deaths, .data$deaths * deaths_cv))),
                  deaths_upper = round(stats::qnorm(0.975, .data$deaths, .data$deaths * deaths_cv)))
}

# DALY components
daly_components <- function(x,
                            lifespan = 64.49,                   # average life expectancy
                            episode_length = 0.01375,        # average length of clinical episode
                            severe_episode_length = 0.04795, # average length of severe episode
                            mild_dw = 0.006, # disability weight mild malaria
                            moderate_dw = 0.051, # disability moderate severe malaria
                            severe_dw = 0.133) { # disability weight severe malaria)
  x %>%
    dplyr::mutate(yll = .data$deaths * (lifespan - ((.data$age_lower + .data$age_upper) / 2)),
                  yll_lower = .data$deaths_lower * (lifespan - ((.data$age_lower + .data$age_upper) / 2)),
                  yll_upper = .data$deaths_upper * (lifespan - ((.data$age_lower + .data$age_upper) / 2)),
                  
                  yll = ifelse(yll < 0, 0, yll),                    # should be no negative yll from older age groups
                  yll_lower = ifelse(yll_lower < 0, 0, yll_lower),  # should be no negative yll from older age groups
                  yll_upper =  ifelse(yll_upper < 0, 0, yll_upper), # should be no negative yll from older age groups
                  
                  yld = dplyr::case_when(.data$age_upper < 5 ~ .data$cases * episode_length * moderate_dw + 
                                           .data$severe_cases * severe_episode_length * severe_dw,
                                         .data$age_upper >= 5 ~ .data$cases * episode_length * mild_dw + 
                                           .data$severe_cases * severe_episode_length * severe_dw),
                  yld_lower = dplyr::case_when(.data$age_upper < 5 ~ .data$cases_lower * episode_length * moderate_dw + 
                                                 .data$severe_cases * severe_episode_length * severe_dw,
                                               .data$age_upper >= 5 ~ .data$cases_lower * episode_length * mild_dw + 
                                                 .data$severe_cases * severe_episode_length * severe_dw),
                  yld_upper = dplyr::case_when(.data$age_upper < 5 ~ .data$cases_upper * episode_length * moderate_dw + 
                                                 .data$severe_cases * severe_episode_length * severe_dw,
                                               .data$age_upper >= 5 ~ .data$cases_upper * episode_length * mild_dw + 
                                                 .data$severe_cases * severe_episode_length * severe_dw)) %>%
    dplyr::mutate(daly = yll + yld,
                  daly_upper = yll_lower + yld_lower,
                  daly_lower = yll_upper + yld_upper)
  
}

# Functions to calculate costs and effects
calculate_costs_effects <- function(path, sim_years = 3, filename) {
  
  dalyoutput <- readRDS(path)
  
  # Calculate DALYs 
  # DALYs = Years of life lost (YLL) + Years of life with disease (YLD)
  # YLL = Deaths * remaining years of life
  # YLD = cases and severe cases * disability weighting  * episode_length
  
  dalyoutput <- dalyoutput %>%
    mutate(treatment_scaler = 0.578*treatment + (1-treatment),
           inc_severe = treatment_scaler * inc_severe,
           severe_cases = treatment_scaler * severe_cases,
           sev = treatment_scaler * sev)
  
  # run functions
  dalyoutput <- mortality_rate(dalyoutput)
  dalyoutput <- outcome_uncertainty(dalyoutput)
  dalyoutput <- daly_components(dalyoutput)
  
  # consolidate across ages
  dalyoutput <- dalyoutput %>%
    select(-inc, -sev, -mortality_rate) %>% # get rid of rate vars
    group_by(file) %>%                      # group to condense to one record per run
    
    # create vars for childhood cases for PCV comparison
    mutate(n_0_1825 = ifelse(age %in% c('0-91.25', '91.25-1825'), n, 0),
           n_91.25_1825 = ifelse(age=='91.25-1825', n, 0), # SMC denominator
           u5_cases = ifelse(age %in% c('0-91.25', '91.25-1825'), cases, 0),
           u5_severe = ifelse(age %in% c('0-91.25', '91.25-1825'), severe_cases, 0),
           u5_dalys = ifelse(age %in% c('0-91.25', '91.25-1825'), daly, 0)) %>%
    
    mutate_at(vars(n, n_0_1825, n_91.25_1825, u5_cases, u5_severe, u5_dalys, 
                   inc_clinical:severe_cases, deaths:daly_lower), sum, na.rm=T) %>%  # condense outputs over all ages in population
    select(-age, -age_upper, -age_lower) %>%
    distinct()
  
  # costing data
  costs <- read.csv(here("data/costing_data_2023_usd.csv"))
  
  PYRcost <- costs$cost[costs$item=="PYRcost"]  
  PBOcost <- costs$cost[costs$item=="PBOcost"]  
  Pyrrolecost <- costs$cost[costs$item=="Pyrollecost"]           
 
  IRScost <- costs$cost[costs$item=="IRScost"]
  
  SMCcost <- costs$cost[costs$item=="SMCcost"]
  
  vacc_cost_per_dose <- costs$cost[costs$item=="vacc_cost_per_dose"] 
  vacc_delivery_cost <- costs$cost[costs$item=="vacc_delivery_cost"] 
  
  RDT <- costs$cost[costs$item=="RDTcost"]           
  AL_adult <- costs$cost[costs$item=="ALcost"] * 24  
  AL_child <- costs$cost[costs$item=="ALcost"] * 12  
  
  outpatient <- costs$cost[costs$item=="outpatient_cost"]  
  inpatient <- costs$cost[costs$item=="inpatient_cost"]    
  
  # clinical: RDT cost + Drug course cost + facility cost (outpatient), removing out of pocket 
  TREATcost_adult <- (RDT + AL_adult + outpatient) * 0.77
  TREATcost_child <- (RDT + AL_child + outpatient) * 0.77
  
  # severe: RDT cost + Drug course cost + facility cost (inpatient), removing out of pocket 
  SEVcost_adult <- (RDT + AL_adult + inpatient) * 0.77
  SEVcost_child <- (RDT + AL_child + inpatient) * 0.77
  
  # population
  population <- dalyoutput$population[[1]]
  
  # Prepare to add costs to dataset
  dalyoutput_cost <- dalyoutput %>%
    
    mutate(ITNuse2 = round(ifelse(ITNboost==1, min(ITNuse + 0.2,0.8), ITNuse),1), # account for booster coverage
           ITNcost = case_when(ITN=='pyr' ~ PYRcost,        # account for ITN type-specific cost
                               ITN=='pbo' ~ PBOcost,
                               ITN=='pyrrole' ~ Pyrrolecost)) %>%
    ungroup()
  
  # count the number of interventions administered and the frequency of ITN dist
  dalyoutput_cost$bednet_distribution_frequency <- as.numeric(lapply(lapply(
    dalyoutput_cost$bednet_timesteps,diff), unique))
  dalyoutput_cost$count_bednet_distributions <- sapply((
    lapply(dalyoutput_cost$bednet_timesteps, 
           function(x) x[x>0 & x<=sim_years*365])),
    function(x) length(x))
  dalyoutput_cost$count_smc_doses <- sapply((lapply(dalyoutput_cost$smc_timesteps, 
                                                    function(x) x[x>0 & x<=sim_years*365])),
                                            function(x) length(x))
  dalyoutput_cost$count_irs_distributions <- ifelse(dalyoutput_cost$IRS == 0.8, 
                                                    sim_years, 0)
  # Spraying campaigns happen once a year
  
  # find average population usage at equilibrium to match ITNuse2
  find_average_usage <- function(ITNuse2) {
    
    find_average_usage <- population_usage(distribution = rep(ITNuse2,11),
                                           distribution_timesteps = (seq(0,30,3)*365)+1,
                                           timesteps = 33*365, 
                                           half_life = 3*365) # assumed in the simulations
    
    #plot(x=1:(33*365), y = find_average_usage, type = "l")
    mean(find_average_usage[(15*365):length(find_average_usage)])
    
  }
  
  dalyoutput_cost <- dalyoutput_cost %>%
    mutate(ITNuse3 = case_when(ITNuse2 == 0.2 ~ round(find_average_usage(0.2),2),
                               ITNuse2 == 0.4 ~ round(find_average_usage(0.4),2),
                               ITNuse2 == 0.6 ~ round(find_average_usage(0.6),2),
                               ITNuse2 == 0.8 ~ round(find_average_usage(0.8),2)))
  
  # read in netz package data to find the annual nets to distribute to give the 
  # simulated usage
  summary(get_usage_rate_data()$usage_rate)
  # median use rate = 0.84
  
  # get nets to be distributed for each ITN usage
  ndist <- function(x) {
    output <- dalyoutput_cost %>% select(ITNuse2, ITNuse3) %>%
      distinct() %>%
      rename(distribution = ITNuse2, target_use = ITNuse3) %>%
      mutate(half_life = median(get_halflife_data()$half_life),
             usage_rate = x) %>%
      mutate(distribution_freq = round(unique(dalyoutput_cost$bednet_distribution_frequency)[
        !(is.na(unique(dalyoutput_cost$bednet_distribution_frequency)))],0)) %>%
      mutate(access = usage_to_access(target_use, usage_rate),
             npc = access_to_crop(access, type = "loess"),
             annual_percapita_nets_distributed = crop_to_distribution(npc, distribution_freq = distribution_freq,
                                                                      net_loss_function = net_loss_map,
                                                                      half_life = half_life)) %>%
      select(distribution, target_use, annual_percapita_nets_distributed)
    
  }
  
  # assume median observed use rate and median bednet half life (across Africa)
  nets_distributed <- ndist(0.84)
  nets_distributed <- nets_distributed %>%
    add_row(distribution = 0, target_use = 0, annual_percapita_nets_distributed = 0) %>% # set 0 usage to 0 nets distributed
    arrange(target_use)
  
  # save output
  saveRDS(nets_distributed, here("output_files/nets_distributed.rds"))
  
  # create cost variables
  # 77% of treatment costs are from the public sector (DHS, SSA)
  dalyoutput_cost <- dalyoutput_cost %>%
    left_join(nets_distributed,
              by=c('ITNuse3' = 'target_use')) %>%
    mutate(annual_percapita_nets_distributed = ifelse(ITNuse3==0, 0,
                                                      annual_percapita_nets_distributed),
           cost_ITN = population * annual_percapita_nets_distributed * sim_years * ITNcost,
           cost_ITN_linear = population * ITNuse2 * count_bednet_distributions * ITNcost,
           # non-severe treatment
           cost_clinical = ((cases - severe_cases - u5_cases) * treatment * TREATcost_adult) + # adult
             ((u5_cases - u5_severe) * treatment * TREATcost_child), # child
           # severe treatment
           cost_severe = (severe_cases * treatment * SEVcost_adult) + # adult
             (u5_severe * treatment * SEVcost_child), # child
           # SMC
           cost_SMC = n_91.25_1825 * SMC * SMCcost * count_smc_doses,
           # RTSS
           cost_vax = (dose1 + dose2 + dose3 + dose4) * (vacc_cost_per_dose + vacc_delivery_cost),
           cost_IRS = count_irs_distributions * IRS * population * IRScost,
           
           # TOTAL
           cost_total = cost_ITN + cost_clinical + cost_severe + cost_SMC + cost_vax + 
             cost_IRS,
           
           # cost just among children
           cost_total_u5 =
             # cost ITNs
             n_0_1825 * annual_percapita_nets_distributed * sim_years * ITNcost +
             # cost clinical
             ((u5_cases-u5_severe) * treatment * TREATcost_child) +
             # cost severe
             (u5_severe * treatment * SEVcost_child) +
             # cost SMC and cost VAX are all among children
             cost_SMC + cost_vax + 
             # cost IRS
             n_0_1825 * IRS * count_irs_distributions * IRScost)
  
  saveRDS(dalyoutput_cost, filename)
  
  return(dalyoutput_cost)
  
}

# Calculate DALYs and costs for all sets of simulations ------------------------

# Set 1, output over 3 years:
dalyoutput_cost1 <- calculate_costs_effects(path = here("output_files/output_long_set1_full_3y.rds"),
                        sim_years = 3,
                        filename = here("output_files/dalyoutput_cost_set1_3y.rds"))

# Set 2, output over 3 years:
dalyoutput_cost2 <- calculate_costs_effects(path = here("output_files/output_long_set2_full_3y.rds"),
                                            sim_years = 3,
                                            filename = here("output_files/dalyoutput_cost_set2_3y.rds"))


# Set 3, output over 3 years:
dalyoutput_cost3 <- calculate_costs_effects(path = here("output_files/output_long_set3_full_3y.rds"),
                                            sim_years = 3,
                                            filename = here("output_files/dalyoutput_cost_set3_3y.rds"))


