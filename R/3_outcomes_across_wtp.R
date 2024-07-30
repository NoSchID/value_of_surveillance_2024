# Calculate NMB,EVPI and EVPPI across cost-effectiveness thresholds

# Setup
library(tidyverse)
library(here)
library(dampack)
library(ggplot2)
library(voi)

# Calculate NMB at these thresholds
threshold <- c(75, c(100, 200, 250, seq(300,1000,100), 1500, 3000))
#threshold <- c(75, seq(80,3000,10))

# Select one of these:
scenarios <- readRDS(here("output_files/scenarios_set1to2_3y_CE250.rds"))
#scenarios <- readRDS(here("output_files/scenarios_set1to3_3y_CE250.rds"))
sim_years <- 3

# Load scenarios and assign WTP thresholds -------------------------------------

# Select key quantities
scenarios <- ungroup(scenarios) %>%
  select(intervention,  ID, file:fifth, dalys_averted, incremental_cost, net_health_benefit,
         net_monetary_benefit, CE, daly_baseline, cost_total_baseline, cases_averted,
         name, baseline_name) %>%
  mutate(resistance = as.factor(resistance))

baseline_selection <- filter(scenarios, ITNuse != 0) %>%
  select(baseline_name) %>% distinct() %>% pull(baseline_name) %>% as.character()

# Calculate mean NMBs ---------------------------------------------------

scenarios_vary_wtp <- list()

for(i in 1:length(threshold)) {
  scenarios_vary_wtp[[i]] <- scenarios %>%
    mutate(wtp = threshold[i],
           net_monetary_benefit = 
             (dalys_averted - incremental_cost/threshold[i]) * threshold[i])
  
}

# Net monetary benefit and net loss/EVPI  for different interventions across WTPs 
# Average by baseline name, WTP and intervention
names(scenarios_vary_wtp) <- threshold

scenarios_vary_wtp2 <- 
  cbind(sapply(scenarios_vary_wtp, "[[", "baseline_name") %>%
          as.data.frame() %>%
          pivot_longer(cols = everything(), names_to = "wtp", values_to = "baseline_name"),
        sapply(scenarios_vary_wtp, "[[", "file") %>%
          as.data.frame() %>%
          pivot_longer(cols = everything(), names_to = "wtp", values_to = "file") %>%
          select(file),
        sapply(scenarios_vary_wtp, "[[", "ID") %>%
          as.data.frame() %>%
          pivot_longer(cols = everything(), names_to = "wtp", values_to = "ID") %>%
          select(ID),
        sapply(scenarios_vary_wtp, "[[", "intervention") %>%
          as.data.frame() %>%
          pivot_longer(cols = everything(), names_to = "wtp", values_to = "intervention") %>%
          select(intervention),
        sapply(scenarios_vary_wtp, "[[", "net_monetary_benefit") %>%
          as.data.frame() %>%
          pivot_longer(cols = everything(), names_to = "wtp", values_to = "net_monetary_benefit") %>%
          select(net_monetary_benefit)) %>%
  group_by(wtp, baseline_name, intervention) %>%
  summarise(net_monetary_benefit = mean(net_monetary_benefit)) %>%
  mutate(wtp = as.numeric(wtp))

scenarios_vary_wtp2 <- scenarios_vary_wtp2 %>%
  mutate(baseline_setting2 = baseline_name) %>%
  separate(baseline_setting2, into = c("prev", "seas", "itn"), sep = "_") %>%
  mutate(seas=stringr::str_to_title(seas),
         prev = paste0(as.numeric(prev) * 100, "%"),
         prev = ifelse(prev == "5%", "  5%", prev),
         itn = paste0(as.numeric(itn) * 100, "%"),
         label = paste0(prev, "  ", itn))

# Save outputs
#saveRDS(scenarios_vary_wtp2, "output_files/nmb_over_wtp.RDS")
#saveRDS(scenarios_vary_wtp2, "output_files/nmb_over_wtp_irs.RDS")

# Calculate EVPPI using Gaussian process model -------------------------

calculate_evppi_gp <- function(setting, threshold) {
  
  nmb <- list()
  parameters <- list()
  evppi1 <- list()
  models1 <- list()
  evppi2 <- list()
  models2 <- list()
  
  for(i in 1:length(threshold)) {
    nmb[[i]] <- scenarios %>%
      filter(baseline_name == setting) %>%
      mutate(wtp = threshold[i],
             net_monetary_benefit = 
               (dalys_averted - incremental_cost/threshold[i]) * threshold[i]) %>%
      select(resistance, estimate, Q0, phi_indoors, intervention, net_monetary_benefit) %>%
      mutate(X1 = ifelse(estimate == "X1", 1, 0),
             X2 = ifelse(estimate == "X2", 1, 0),
             X3 = ifelse(estimate == "X3", 1, 0),
             X4 = ifelse(estimate == "X4", 1, 0),
             X5 = ifelse(estimate == "X5", 1, 0),
             X6 = ifelse(estimate == "X6", 1, 0),
             X7 = ifelse(estimate == "X7", 1, 0),
             X8 = ifelse(estimate == "X8", 1, 0),
             X9 = ifelse(estimate == "X9", 1, 0)) %>%
      select(-estimate) %>%
      pivot_wider(id_cols =c("resistance", "Q0", "phi_indoors", "X1", "X2", "X3", 
                             "X4", "X5", "X6", "X7", "X8", "X9"), 
                  names_from = intervention, values_from = net_monetary_benefit) 
    
    parameters[[i]] <- nmb[[i]] %>% select(resistance, Q0, phi_indoors, X1, X2, X3, X4, X5, X6, X7, X8, X9) %>%
      as.data.frame()
    parameters[[i]][,"resistance"] <- as.numeric(as.character(parameters[[i]][,"resistance"]))
    nmb[[i]] <- nmb[[i]] %>% select(-resistance, -Q0, -phi_indoors, -X1, -X2, -X3, -X4, -X5, -X6, 
                          -X7, -X8, -X9)%>%
      as.matrix()
    
    nmb[[i]] <- cbind(base=0, nmb[[i]])
    
    evppi1[[i]] <- voi::evppi(outputs=nmb[[i]], inputs = parameters[[i]], pars = c("resistance", "X1", "X2", "X3", 
                                                                "X4", "X5", "X6", "X7", "X8", "X9"), 
                          method = "gp", 
                          verbose = TRUE, check =TRUE)

    
    evppi2[[i]] <- voi::evppi(outputs=nmb[[i]], inputs = parameters[[i]], pars = c("Q0", "phi_indoors"), 
                         method = "gp", verbose = TRUE, check =TRUE)
    
    
  }  
  
  names(evppi1) <- threshold
  names(evppi2) <- threshold
  
  return(list(evppi1=evppi1, evppi2=evppi2))
  
}

unique(scenarios$baseline_name)

# These take long to run:
perennial_5_20 <- calculate_evppi_gp(setting = "0.05_perennial_0.2", threshold = c(75,250,500,1000,3000))
saveRDS(perennial_5_20, "output_files/evppi/perennial_5_20.RDS")
perennial_5_40 <- calculate_evppi_gp(setting = "0.05_perennial_0.4", threshold = c(75,250,500,1000,3000))
saveRDS(perennial_5_40, "output_files/evppi/perennial_5_40.RDS")
perennial_5_60 <- calculate_evppi_gp(setting = "0.05_perennial_0.6", threshold = c(75,250,500,1000,3000))
saveRDS(perennial_5_60, "output_files/evppi/perennial_5_60.RDS")
seasonal_5_20 <- calculate_evppi_gp(setting = "0.05_seasonal_0.2", threshold = c(75,250,500,1000,3000))
saveRDS(seasonal_5_20, "output_files/evppi/seasonal_5_20.RDS")
seasonal_5_40 <- calculate_evppi_gp(setting = "0.05_seasonal_0.4", threshold = c(75,250,500,1000,3000))
saveRDS(seasonal_5_40, "output_files/evppi/seasonal_5_40.RDS")
seasonal_5_60 <- calculate_evppi_gp(setting = "0.05_seasonal_0.6", threshold = c(75,250,500,1000,3000))
saveRDS(seasonal_5_60, "output_files/evppi/seasonal_5_60.RDS")
highly_seasonal_5_20 <- calculate_evppi_gp(setting = "0.05_highly seasonal_0.2", threshold = c(75,250,500,1000,3000))
saveRDS(highly_seasonal_5_20, "output_files/evppi/highly_seasonal_5_20.RDS")
highly_seasonal_5_40 <- calculate_evppi_gp(setting = "0.05_highly seasonal_0.4", threshold = c(75,250,500,1000,3000))
saveRDS(highly_seasonal_5_40, "output_files/evppi/highly_seasonal_5_40.RDS")
highly_seasonal_5_60 <- calculate_evppi_gp(setting = "0.05_highly seasonal_0.6", threshold = c(75,250,500,1000,3000))
saveRDS(highly_seasonal_5_60, "output_files/evppi/highly_seasonal_5_60.RDS")
perennial_10_20 <- calculate_evppi_gp(setting = "0.1_perennial_0.2", threshold = c(75,250,500,1000,3000))
saveRDS(perennial_10_20, "output_files/evppi/perennial_10_20.RDS")
perennial_10_40 <- calculate_evppi_gp(setting = "0.1_perennial_0.4", threshold = c(75,250,500,1000,3000))
saveRDS(perennial_10_40, "output_files/evppi/perennial_10_40.RDS")
perennial_10_60 <- calculate_evppi_gp(setting = "0.1_perennial_0.6", threshold = c(75,250,500,1000,3000))
saveRDS(perennial_10_60, "output_files/evppi/perennial_10_60.RDS")
seasonal_10_20 <- calculate_evppi_gp(setting = "0.1_seasonal_0.2", threshold = c(75,250,500,1000,3000))
saveRDS(seasonal_10_20, "output_files/evppi/seasonal_10_20.RDS")
seasonal_10_40 <- calculate_evppi_gp(setting = "0.1_seasonal_0.4", threshold = c(75,250,500,1000,3000))
saveRDS(seasonal_10_40, "output_files/evppi/seasonal_10_40.RDS")
seasonal_10_60 <- calculate_evppi_gp(setting = "0.1_seasonal_0.6", threshold = c(75,250,500,1000,3000))
saveRDS(seasonal_10_60, "output_files/evppi/seasonal_10_60.RDS")
highly_seasonal_10_20 <- calculate_evppi_gp(setting = "0.1_highly seasonal_0.2", threshold = c(75,250,500,1000,3000))
saveRDS(highly_seasonal_10_20, "output_files/evppi/highly_seasonal_10_20.RDS")
highly_seasonal_10_40 <- calculate_evppi_gp(setting = "0.1_highly seasonal_0.4", threshold = c(75,250,500,1000,3000))
saveRDS(highly_seasonal_10_40, "output_files/evppi/highly_seasonal_10_40.RDS")
highly_seasonal_10_60 <- calculate_evppi_gp(setting = "0.1_highly seasonal_0.6", threshold = c(75,250,500,1000,3000))
saveRDS(highly_seasonal_10_60, "output_files/evppi/highly_seasonal_10_60.RDS")
perennial_20_20 <- calculate_evppi_gp(setting = "0.2_perennial_0.2", threshold = c(75,250,500,1000,3000))
saveRDS(perennial_20_20, "output_files/evppi/perennial_20_20.RDS")
perennial_20_40 <- calculate_evppi_gp(setting = "0.2_perennial_0.4", threshold = c(75,250,500,1000,3000))
saveRDS(perennial_20_40, "output_files/evppi/perennial_20_40.RDS")
perennial_20_60 <- calculate_evppi_gp(setting = "0.2_perennial_0.6", threshold = c(75,250,500,1000,3000))
saveRDS(perennial_20_60, "output_files/evppi/perennial_20_60.RDS")
seasonal_20_20 <- calculate_evppi_gp(setting = "0.2_seasonal_0.2", threshold = c(75,250,500,1000,3000))
saveRDS(seasonal_20_20, "output_files/evppi/seasonal_20_20.RDS")
seasonal_20_40 <- calculate_evppi_gp(setting = "0.2_seasonal_0.4", threshold = c(75,250,500,1000,3000))
saveRDS(seasonal_20_40, "output_files/evppi/seasonal_20_40.RDS")
seasonal_20_60 <- calculate_evppi_gp(setting = "0.2_seasonal_0.6", threshold = c(75,250,500,1000,3000))
saveRDS(seasonal_20_60, "output_files/evppi/seasonal_20_60.RDS")
highly_seasonal_20_20 <- calculate_evppi_gp(setting = "0.2_highly seasonal_0.2", threshold = c(75,250,500,1000,3000))
saveRDS(highly_seasonal_20_20, "output_files/evppi/highly_seasonal_20_20.RDS")
highly_seasonal_20_40 <- calculate_evppi_gp(setting = "0.2_highly seasonal_0.4", threshold = c(75,250,500,1000,3000))
saveRDS(highly_seasonal_20_40, "output_files/evppi/highly_seasonal_20_40.RDS")
highly_seasonal_20_60 <- calculate_evppi_gp(setting = "0.2_highly seasonal_0.6", threshold = c(75,250,500,1000,3000))
saveRDS(highly_seasonal_20_60, "output_files/evppi/highly_seasonal_20_60.RDS")
perennial_40_20 <- calculate_evppi_gp(setting = "0.4_perennial_0.2", threshold = c(75,250,500,1000,3000))
saveRDS(perennial_40_20, "output_files/evppi/perennial_40_20.RDS")
perennial_40_40 <- calculate_evppi_gp(setting = "0.4_perennial_0.4", threshold = c(75,250,500,1000,3000))
saveRDS(perennial_40_40, "output_files/evppi/perennial_40_40.RDS")
perennial_40_60 <- calculate_evppi_gp(setting = "0.4_perennial_0.6", threshold = c(75,250,500,1000,3000))
saveRDS(perennial_40_60, "output_files/evppi/perennial_40_60.RDS")
seasonal_40_20 <- calculate_evppi_gp(setting = "0.4_seasonal_0.2", threshold = c(75,250,500,1000,3000))
saveRDS(seasonal_40_20, "output_files/evppi/seasonal_40_20.RDS")
seasonal_40_40 <- calculate_evppi_gp(setting = "0.4_seasonal_0.4", threshold = c(75,250,500,1000,3000))
saveRDS(seasonal_40_40, "output_files/evppi/seasonal_40_40.RDS")
seasonal_40_60 <- calculate_evppi_gp(setting = "0.4_seasonal_0.6", threshold = c(75,250,500,1000,3000))
saveRDS(seasonal_40_60, "output_files/evppi/seasonal_40_60.RDS")
highly_seasonal_40_20 <- calculate_evppi_gp(setting = "0.4_highly seasonal_0.2", threshold = c(75,250,500,1000,3000))
saveRDS(highly_seasonal_40_20, "output_files/evppi/highly_seasonal_40_20.RDS")
highly_seasonal_40_40 <- calculate_evppi_gp(setting = "0.4_highly seasonal_0.4", threshold = c(75,250,500,1000,3000))
saveRDS(highly_seasonal_40_40, "output_files/evppi/highly_seasonal_40_40.RDS")
highly_seasonal_40_60 <- calculate_evppi_gp(setting = "0.4_highly seasonal_0.6", threshold = c(75,250,500,1000,3000))
saveRDS(highly_seasonal_40_60, "output_files/evppi/highly_seasonal_40_60.RDS")

# Process EVPPI runs

# Set 1 to 2
files <- list.files(path = 'output_files/evppi/set1to3/', 
                    full.names = TRUE)
dat_list <- lapply(files, function (x) readRDS(x))
names(dat_list) <- gsub(".RDS", "", str_extract(files, "([^/]+$)"))
dat <- data.table::rbindlist(dat_list, fill = TRUE, idcol="file")
dat$evppi1 <- sapply(dat$evppi1, "[", "evppi")
dat$evppi2 <- sapply(dat$evppi2, "[", "evppi")
dat$wtp <- rep(c(75,250,500,1000,3000), times = 36)

dat <- dat %>%
  mutate(prev_itn = gsub(".RDS", "", str_extract(file,"(\\d).*")),
         seas = gsub("irs_", "", file),
         seas = str_extract(seas, "[^_]+"),
         seas= ifelse(seas == "highly", "highly seasonal", seas)) %>%
  separate(prev_itn, c("prev", "itn"), "_") %>%
  mutate(prev = as.numeric(prev)/100, 
         prev = ifelse(prev == 0.05, round(prev, 2), round(prev,1)),
         itn = round(as.numeric(itn)/100,1),
         setting = paste0(prev, "_", seas, "_", itn)) %>%
  rename(resistance_estimate = evppi1,
         Q0_phi_indoors = evppi2) %>%
  select(wtp, setting, resistance_estimate, Q0_phi_indoors) %>%
  pivot_longer(cols = c("resistance_estimate", "Q0_phi_indoors"),
              names_to = "parameter", values_to = "evppi") %>%
  mutate(evppi=as.numeric(evppi),
         evppi_per_person = evppi/200000) %>%
  select(wtp, setting, parameter, evppi, evppi_per_person)

#saveRDS(dat, "output_files/evppi_over_wtp_gp.RDS")
#saveRDS(dat, "output_files/evppi_over_wtp_gp_irs.RDS")

# Calculate EVPI  --------------------------------------------------------------
# Net loss across WTPs (lower frontier corresponds to EVPI)

net_loss_by_wtp_df <- data.frame(wtp = NA, baseline_setting = NA, ITNuse = NA, 
                                 intervention = NA,
                                 mean_nmb_loss=NA)

evpi_by_wtp_df <- data.frame(wtp = NA, baseline_setting = NA, 
                             highest_mean_nmb = NA,
                             max_mean_nmb = NA,   # Expected value under uncertainty
                             ev_certainty = NA,   # Expected value with certainty = mean of highest NMBs in each iteration
                             ITNuse = NA, evpi=NA,
                             count_alternative_strategies = NA)

for(i in 1:length(threshold)) {
  for (j in 1:length(baseline_selection)) {  #
    # Need dataframe of net benefit with columns being scenarios and rows being varied parameter
    nmb <- scenarios_vary_wtp[[i]] %>%
      filter(baseline_name == baseline_selection[j]) %>%
      select(resistance, Q0, phi_indoors, estimate, intervention, net_monetary_benefit) %>%
      pivot_wider(id_cols=c("resistance", "Q0","phi_indoors", "estimate"),
                  names_from = "intervention",
                  values_from = "net_monetary_benefit") %>%
      arrange(resistance, Q0, phi_indoors, estimate) %>%
      select(-resistance, -Q0,-phi_indoors, -estimate) %>%
      as.matrix()
    
    ev_certainty <- mean(apply(nmb, 1, max))
    highest_mean_nmb <- colnames(nmb)[which(apply(nmb, 2, mean)==max(apply(nmb, 2, mean)))]
    max_mean_nmb <- max(apply(nmb, 2, mean))
    max_str_rowwise <- max.col(nmb)     # find maximum NMB for each parmset (strategy with highest NMB in each)
    max_strategies <- colnames(nmb)[max_str_rowwise]
    alternative_strategies <- length(unique(colnames(nmb)[max_str_rowwise][
      colnames(nmb)[max_str_rowwise] != highest_mean_nmb]))
    
    nmb_loss <-  nmb[cbind(1:nrow(nmb), max_str_rowwise)] - nmb
    
    mean_nmb_loss <- apply(nmb_loss, 2, mean)  # mean for each intervention
    
    evpi <- min(mean_nmb_loss)
    
    net_loss_by_wtp_df <- rbind(net_loss_by_wtp_df, 
                                data.frame(wtp = threshold[i],
                                           baseline_setting = baseline_selection[j], 
                                           ITNuse = 
                                             unique(scenarios_vary_wtp[[i]]$ITNuse[
                                               scenarios_vary_wtp[[i]]$baseline_name == baseline_selection[j]]),
                                           intervention = rownames(as.data.frame(mean_nmb_loss)),
                                           mean_nmb_loss = as.data.frame(mean_nmb_loss),
                                           row.names=NULL))
    
    evpi_by_wtp_df <- rbind(evpi_by_wtp_df, 
                            data.frame(wtp = threshold[i],
                                       baseline_setting = baseline_selection[j], 
                                       highest_mean_nmb = highest_mean_nmb,
                                       max_mean_nmb = max_mean_nmb,
                                       ev_certainty = ev_certainty,
                                       ITNuse = 
                                         unique(scenarios_vary_wtp[[i]]$ITNuse[
                                           scenarios_vary_wtp[[i]]$baseline_name == baseline_selection[j]]),
                                       evpi = evpi,
                                       count_alternative_strategies = alternative_strategies,
                                       row.names=NULL))
    
  }
}

net_loss_by_wtp_df <- drop_na(net_loss_by_wtp_df) %>%
  group_by(wtp, intervention) %>%
  summarise(net_loss = median(mean_nmb_loss))

evpi_by_wtp_df <- drop_na(evpi_by_wtp_df) 
evpi_by_wtp_df <- evpi_by_wtp_df %>%
  mutate(evpi_per_person = evpi/200000,
         prop_of_expected_nmb = evpi/max_mean_nmb)

# Save outputs
#saveRDS(evpi_by_wtp_df, "output_files/evpi_over_wtp.RDS")
#saveRDS(evpi_by_wtp_df, "output_files/evpi_over_wtp_irs.RDS")

# Exploratory plots ------------------------------------------------------------------------

# NMB in each setting 
ggplot(scenarios_vary_wtp2) +
  geom_line(aes(x = wtp, y = net_monetary_benefit/1000000, col = intervention), size=1.3) +
  scale_color_brewer(palette = "Paired") +
  facet_wrap(~baseline_name, scales = "free_y") +
  theme_classic()

# EVPI plots

ggplot(net_loss_by_wtp_df) +
  geom_line(aes(x=wtp,y = net_loss/200000, col = intervention)) 

ggplot(evpi_by_wtp_df) +
  geom_line(aes(x=wtp,y = evpi/200000)) +
  geom_vline(xintercept=250) +
  geom_vline(xintercept=500) +
  geom_vline(xintercept=1000) +
  theme_bw() +
  facet_wrap(~baseline_setting, ncol = 9, scales = "free_y")

