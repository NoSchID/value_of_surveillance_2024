# Assign scenarios and calculate DALYS averted/incremental costs

library(tidyverse)
library(here)

# Assign scenarios and calculate NMB for a single threshold
assign_scenarios <- function(sets, CE_threshold = 250) {
  
  if(sets == "no_irs") {
    
    dalyoutput_cost <- rbind(readRDS(here("output_files/dalyoutput_cost_set1_3y.rds")),
                             readRDS(here("output_files/dalyoutput_cost_set2_3y.rds")))
    
  } else if (sets == "with_irs") {
    
    dalyoutput_cost <- rbind(readRDS(here("output_files/dalyoutput_cost_set1_3y.rds")),
                             readRDS(here("output_files/dalyoutput_cost_set2_3y.rds")),
                             readRDS(here("output_files/dalyoutput_cost_set3_3y.rds")))
    
  }
  
  output <- dalyoutput_cost %>%
    mutate(ID = paste(pfpr, seasonality,  ITNuse, Q0, phi_indoors, resistance, estimate, 
                      sep="_")) # create unique identifier
  
  if (length(output$file) != length(unique(output$file))) {
    stop()
  }
  
  # Find baseline settings
  none <- output %>%
    filter(ITNboost==0 & ITN=='pyr' & RTSS=='none' & treatment==0.45 & # filter out interventions
             (SMC==0 | (seasonality=='highly seasonal')) & IRS == 0) %>%
    rename(daly_baseline = daly,
           cases_baseline = cases,
           severe_baseline = severe_cases,
           deaths_baseline = deaths,
           u5_dalys_baseline = u5_dalys,
           u5_cases_baseline = u5_cases,
           u5_severe_baseline = u5_severe,
           n_baseline = n,
           u5_n_baseline = n_0_1825,
           cost_total_baseline = cost_total,
           cost_total_u5_baseline = cost_total_u5,
           cost_ITN_baseline = cost_ITN
    ) %>%
    select(file, ID, daly_baseline, cases_baseline, severe_baseline, deaths_baseline,
           u5_dalys_baseline, u5_cases_baseline, u5_severe_baseline,
           n_baseline, u5_n_baseline,
           cost_total_baseline,  
           cost_total_u5_baseline, cost_ITN_baseline)
  
  base_IDs <- none$file

  if (length(none$file)!=length(unique(none$file))) {
    stop()
  }
 
  scenarios <- output %>% 
    filter(!(file %in% base_IDs)) %>%
    left_join(none %>% select(-file), by=c('ID')) %>%
    mutate(dalys_averted = daly_baseline - daly,
           incremental_cost = cost_total - cost_total_baseline,
           net_health_benefit = dalys_averted - incremental_cost/CE_threshold,
           net_monetary_benefit = net_health_benefit * CE_threshold,
           CE = (cost_total - cost_total_baseline) / (daly_baseline - daly),
           cases_averted = cases_baseline-cases,
           severe_cases_averted = severe_baseline-severe_cases,
           u5_cases_averted = u5_cases_baseline-u5_cases,
           u5_severe_cases_averted = u5_severe_baseline-u5_severe) %>% # 
    mutate(intervention = case_when(                        # treatment is the common element to all non VC packages
      ITN == "pyr" & ITNboost == 0 & treatment==0.45 & IRS==0 ~ "No intervention",
      ITN == "pyr" & ITNboost == 0 & treatment==0.45 & IRS==0.8 ~ "IRS",
      ITN == "pyr" & ITNboost == 0 & treatment==0.60 & IRS==0 ~ "Non-VC interventions",
      ITN == "pyr" & ITNboost == 0 & treatment==0.60 & IRS==0.8 ~ "Non-VC interventions & IRS",
      ITN == "pyr" & ITNboost == 1 & treatment==0.45 & IRS==0 ~ "Increase ITNs by 20%", 
      ITN == "pyr" & ITNboost == 1 & treatment==0.60 & IRS==0 ~ "Increase ITNs by 20% & non-VC interventions", 
      ITN == "pbo" & ITNboost == 0 & treatment==0.45 & IRS==0 ~ "Switch to PBO", 
      ITN == "pbo" & ITNboost == 0 & treatment==0.45 & IRS==0.8 ~ "Switch to PBO & IRS", 
      ITN == "pbo" & ITNboost == 0 & treatment==0.60 & IRS==0 ~ "Switch to PBO & non-VC interventions", 
      ITN == "pbo" & ITNboost == 0 & treatment==0.60 & IRS==0.8 ~ "Switch to PBO & non-VC interventions & IRS", 
      ITN == "pbo" & ITNboost == 1 & treatment==0.45 & IRS==0 ~ "Switch to PBO & increase by 20%",
      ITN == "pbo" & ITNboost == 1 & treatment==0.60 & IRS==0 ~ "Switch to PBO & increase by 20% & non-VC interventions",
      ITN == "pyrrole" & ITNboost == 0 & treatment==0.45 & IRS==0 ~ "Switch to pyrrole",
      ITN == "pyrrole" & ITNboost == 0 & treatment==0.45 & IRS==0.8 ~ "Switch to pyrrole & IRS", 
      ITN == "pyrrole" & ITNboost == 0 & treatment==0.60 & IRS==0 ~ "Switch to pyrrole & non-VC interventions", 
      ITN == "pyrrole" & ITNboost == 0 & treatment==0.60 & IRS==0.8 ~ "Switch to pyrrole & non-VC interventions & IRS",
      ITN == "pyrrole" & ITNboost == 1 & treatment==0.45 & IRS==0 ~ "Switch to pyrrole & increase by 20%",
      ITN == "pyrrole" & ITNboost == 1 & treatment==0.60 & IRS==0 ~ "Switch to pyrrole & increase by 20% & non-VC interventions"),
      intervention = factor(intervention, levels =
                              c("Increase ITNs by 20%", "Switch to PBO", "Switch to pyrrole",
                                "Non-VC interventions", "IRS",
                                "Increase ITNs by 20% & non-VC interventions",
                                "Non-VC interventions & IRS",
                                "Switch to PBO & IRS", "Switch to pyrrole & IRS", 
                                "Switch to PBO & non-VC interventions", "Switch to pyrrole & non-VC interventions",
                                "Switch to PBO & non-VC interventions & IRS",
                                "Switch to pyrrole & non-VC interventions & IRS",
                                "Switch to PBO & increase by 20%",
                                "Switch to pyrrole & increase by 20%",
                                "Switch to PBO & increase by 20% & non-VC interventions",
                                "Switch to pyrrole & increase by 20% & non-VC interventions"))) %>%
    mutate(rank=as.numeric(as.factor(intervention)),
           estimate = factor(estimate, levels = c("X1", "X2", "X3", "X4", "X5",
                                                  "X6", "X7", "X8", "X9", "X10"))) 
  
  scenarios <- scenarios %>%
    mutate(name = gsub("[.]","", paste0("pfpr", pfpr, "_", seasonality, "_Q0",
                                        Q0, "_phi_indoors", phi_indoors,
                                        "_ITN", ITNuse, "_", ITN,"_boost", ITNboost, "_res",
                                        resistance, "_est", estimate, "_treat", treatment,
                                        "_SMC", SMC, "_RTSS_", RTSS, "_IRS", IRS)),
           baseline_name = paste(pfpr, seasonality, ITNuse,  sep="_"))  
  
  scenarios$baseline_name <- factor(scenarios$baseline_name, 
                                    levels = c("0.05_perennial_0.2",
                                               "0.05_perennial_0.4",
                                               "0.05_perennial_0.6",
                                               "0.1_perennial_0.2",
                                               "0.1_perennial_0.4",
                                               "0.1_perennial_0.6",
                                               "0.2_perennial_0.2",
                                               "0.2_perennial_0.4",
                                               "0.2_perennial_0.6",
                                               "0.4_perennial_0.2",
                                               "0.4_perennial_0.4",
                                               "0.4_perennial_0.6",
                                               "0.05_seasonal_0.2",
                                               "0.05_seasonal_0.4",
                                               "0.05_seasonal_0.6",
                                               "0.1_seasonal_0.2",
                                               "0.1_seasonal_0.4",
                                               "0.1_seasonal_0.6",
                                               "0.2_seasonal_0.2",
                                               "0.2_seasonal_0.4",
                                               "0.2_seasonal_0.6",
                                               "0.4_seasonal_0.2",
                                               "0.4_seasonal_0.4",
                                               "0.4_seasonal_0.6",
                                               "0.05_highly seasonal_0.2",
                                               "0.05_highly seasonal_0.4",
                                               "0.05_highly seasonal_0.6",
                                               "0.1_highly seasonal_0.2",
                                               "0.1_highly seasonal_0.4",
                                               "0.1_highly seasonal_0.6",
                                               "0.2_highly seasonal_0.2",
                                               "0.2_highly seasonal_0.4",
                                               "0.2_highly seasonal_0.6",
                                               "0.4_highly seasonal_0.2",
                                               "0.4_highly seasonal_0.4",
                                               "0.4_highly seasonal_0.6"
                                    ))
  
  
  if(sets == "no_irs") {
    
    saveRDS(scenarios, paste0("output_files/scenarios_set1to2_3y_CE", CE_threshold, ".rds"))
    
  } else if (sets == "with_irs") {
    
    saveRDS(scenarios, paste0("output_files/scenarios_set1to3_3y_CE", CE_threshold, ".rds"))
    
  }
  
  return(scenarios)
  
  
}

scenarios1 <- assign_scenarios(sets = "no_irs")
scenarios2 <- assign_scenarios(sets = "with_irs")

