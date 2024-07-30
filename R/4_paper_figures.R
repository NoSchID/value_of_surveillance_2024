# Figures for paper

# Setup
library(tidyverse)
library(here)
library(dampack)
library(grid)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(MetBrewer) 
library(patchwork)

### Load data --------------------------------------------------------------------

# No IRS:
scenarios <- readRDS(here("output_files/scenarios_set1to2_3y_CE250.rds"))
sim_years <- 3
nmb_df <- readRDS(here("output_files/nmb_over_wtp.RDS"))       # this dataset already contains the mean NMB in each setting

evpi_df <- readRDS(here("output_files/evpi_over_wtp.RDS"))
evpi_df <- evpi_df %>%
  mutate(evpi = ev_certainty - max_mean_nmb) %>%
  mutate(evpi_per_person = evpi/200000,
         prop_of_expected_nmb = evpi/max_mean_nmb)
evppi_df <- readRDS(here("output_files/evppi_over_wtp_gp.RDS"))

# With IRS:
scenarios_irs <- readRDS(here("output_files/scenarios_set1to3_3y_CE250.rds"))
nmb_df_irs <- readRDS(here("output_files/nmb_over_wtp_irs.RDS"))       
evpi_df_irs <- readRDS(here("output_files/evpi_over_wtp_irs.RDS"))
evpi_df_irs <- evpi_df_irs %>%
  mutate(evpi = ev_certainty - max_mean_nmb) %>%
  mutate(evpi_per_person = evpi/200000,
         prop_of_expected_nmb = evpi/max_mean_nmb)
evppi_df_irs <- readRDS(here("output_files/evppi_over_wtp_gp_irs.RDS"))

nmb_df <- nmb_df %>%
  mutate(intervention = factor(intervention, levels =
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
                          "Switch to pyrrole & increase by 20% & non-VC interventions")))


nmb_df_irs <- nmb_df_irs %>%
  mutate(intervention = factor(intervention, levels =
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
                                   "Switch to pyrrole & increase by 20% & non-VC interventions")))

colours <- c(as.character(met.brewer("Redon",11, "discrete")),
             brewer.pal(n=6,"Set3"))

assign_colours <-  c("Switch to PBO" = colours[2],
                     "Switch to PBO & non-VC interventions" = colours[6],
                     "Switch to PBO & increase by 20%" = colours[8],
                     "Switch to PBO & increase by 20% & non-VC interventions" =
                       colours[10],
                     "Non-VC interventions" = colours[4],
                     "Increase ITNs by 20%" = colours[1],
                     "Increase ITNs by 20% & non-VC interventions" =
                       colours[5],
                     "Switch to pyrrole" = colours[3],
                     "Switch to pyrrole & non-VC interventions"= colours[7],
                     "Switch to pyrrole & increase by 20%" = colours[9],
                     "Switch to pyrrole & increase by 20% & non-VC interventions"= 
                       colours[11],
                     "IRS" = colours[12],
                     "Non-VC interventions & IRS"= colours[13],
                     "Switch to PBO & IRS" = colours[14],
                     "Switch to pyrrole & IRS" = colours[15],
                     "Switch to PBO & non-VC interventions & IRS" = colours[16],
                     "Switch to pyrrole & non-VC interventions & IRS"= colours[17])

# Change levels of intervention
change_intervention_levels <- function(x) {
  factor(x, levels = 
           c("Switch to PBO", "Switch to pyrrole",
             "Increase ITNs by 20%", "Non-VC interventions", 
             "Switch to PBO & increase by 20%",                           
             "Switch to pyrrole & increase by 20%",
             "Switch to PBO & non-VC interventions",                      
             "Switch to pyrrole & non-VC interventions",
             "Increase ITNs by 20% & non-VC interventions",
             "Switch to PBO & increase by 20% & non-VC interventions",
             "Switch to pyrrole & increase by 20% & non-VC interventions",
             "IRS",
             "Switch to PBO & IRS","Switch to pyrrole & IRS",  "Non-VC interventions & IRS",
             "Switch to PBO & non-VC interventions & IRS",
             "Switch to pyrrole & non-VC interventions & IRS"))
}

scenarios$intervention <- change_intervention_levels(scenarios$intervention)
scenarios_irs$intervention <- change_intervention_levels(scenarios_irs$intervention)

# Alternative intervention names
intervention_names2 <- data.frame(intervention = levels(scenarios$intervention),
                                  intervention2 = c("ITN switch to pyr-PBO",
                                                    "ITN switch to pyr-pyrr",
                                                    "ITN scale-up",
                                                    "Treatment and prevention",
                                                    "ITN switch to pyr-PBO + scale-up",
                                                    "ITN switch to pyr-pyrr + scale-up",
                                                    "ITN switch to pyr-PBO + treatment and prevention",
                                                    "ITN switch to pyr-pyrr + treatment and prevention",
                                                    "ITN scale-up + treatment and prevention",
                                                    "ITN switch to pyr-PBO + scale-up +\ntreatment and prevention",
                                                    "ITN switch to pyr-pyrr + scale-up +\ntreatment and prevention",
                                                    "IRS",
                                                    "ITN switch to pyr-PBO + IRS",
                                                    "ITN switch to pyr-pyrr + IRS",
                                                    "Treatment and prevention + IRS",
                                                    "ITN switch to pyr-PBO + treatment and prevention + IRS",
                                                    "ITN switch to pyr-pyrr + treatment and prevention + IRS")) %>%
  mutate(intervention2 = factor(intervention2, levels = c(
    "ITN switch to pyr-PBO",
    "ITN switch to pyr-pyrr",
    "ITN scale-up",
    "Treatment and prevention",
    "ITN switch to pyr-PBO + scale-up",
    "ITN switch to pyr-pyrr + scale-up",
    "ITN switch to pyr-PBO + treatment and prevention",
    "ITN switch to pyr-pyrr + treatment and prevention",
    "ITN scale-up + treatment and prevention",
    "ITN switch to pyr-PBO + scale-up +\ntreatment and prevention",
    "ITN switch to pyr-pyrr + scale-up +\ntreatment and prevention",
    "IRS",
    "ITN switch to pyr-PBO + IRS",
    "ITN switch to pyr-pyrr + IRS",
    "Treatment and prevention + IRS",
    "ITN switch to pyr-PBO + treatment and prevention + IRS",
    "ITN switch to pyr-pyrr + treatment and prevention + IRS"
  )))

assign_colours2 <-  c("1. ITN switch to pyr-PBO" = colours[1],
                      "2. ITN switch to pyr-pyrr" = colours[2],
                      "3. ITN scale-up" = colours[3],
                      "4. Treatment and prevention" = colours[4],
                      "5. ITN switch to pyr-PBO + scale-up" = colours[5],
                      "6. ITN switch to pyr-pyrr + scale-up" = colours[6],
                      "7. ITN switch to pyr-PBO + treatment and prevention" = colours[7],
                      "8. ITN switch to pyr-pyrr + treatment and prevention"= colours[8],
                      "9. ITN scale-up + treatment and prevention" =
                        colours[9],
                      "10. ITN switch to pyr-PBO + scale-up +\ntreatment and prevention" =
                        colours[10],
                      "11. ITN switch to pyr-pyrr + scale-up +\ntreatment and prevention"= 
                        colours[11])

assign_colours3 <-  c("1. ITN switch to pyr-PBO" = colours[1],
                      "2. ITN switch to pyr-pyrr" = colours[2],
                      "3. ITN scale-up" = colours[3],
                      "4. Treatment and prevention" = colours[4],
                      "5. ITN switch to pyr-PBO + scale-up" = colours[5],
                      "6. ITN switch to pyr-pyrr + scale-up" = colours[6],
                      "7. ITN switch to pyr-PBO + treatment and prevention" = colours[7],
                      "8. ITN switch to pyr-pyrr + treatment and prevention"= colours[8],
                      "9. ITN scale-up + treatment and prevention" =
                        colours[9],
                      "10. ITN switch to pyr-PBO + scale-up +\ntreatment and prevention" =
                        colours[10],
                      "11. ITN switch to pyr-pyrr + scale-up +\ntreatment and prevention"= 
                        colours[11],
                      "12. IRS" = colours[12],
                      "13. ITN switch to pyr-PBO + IRS" = colours[13],
                      "14. ITN switch to pyr-pyrr + IRS" = colours[14],
                      "15. Treatment and prevention + IRS" = colours[15],
                      "16. ITN switch to pyr-PBO + treatment and prevention + IRS" = colours[16],
                      "17. ITN switch to pyr-pyrr + treatment and prevention + IRS" = colours[17])

# Prepare NMB datasets for WTPs of $75, $250 and $1000
scenarios_250_1000 <- scenarios %>%
  select(-net_monetary_benefit) %>%
  mutate(net_monetary_benefit_250 = (dalys_averted - incremental_cost/250)*250,
         net_monetary_benefit_75 = (dalys_averted - incremental_cost/75)*75,
         net_monetary_benefit_1000 = (dalys_averted - incremental_cost/1000)*1000) %>%
  pivot_longer(cols=c("net_monetary_benefit_75","net_monetary_benefit_250", "net_monetary_benefit_1000"),
               names_to = "wtp",
               values_to = "net_monetary_benefit") %>%
  mutate(wtp = as.numeric(gsub("net_monetary_benefit_","",wtp))) %>%
  left_join(intervention_names2, by = "intervention") %>%
  left_join(data.frame(intervention = sort(unique(scenarios$intervention)),
                       intervention_number = 1:11), by = "intervention") %>%
  mutate(intervention_label = paste0(intervention_number, ". ", intervention2),
         seasonality = stringr::str_to_sentence(seasonality),
         seasonality = factor(seasonality, levels = c(
           "Perennial", "Seasonal", "Highly seasonal")))

scenarios_250_1000_irs <- scenarios_irs %>%
  select(-net_monetary_benefit) %>%
  mutate(net_monetary_benefit_250 = (dalys_averted - incremental_cost/250)*250,
         net_monetary_benefit_75 = (dalys_averted - incremental_cost/75)*75,
         net_monetary_benefit_1000 = (dalys_averted - incremental_cost/1000)*1000) %>%
  pivot_longer(cols=c("net_monetary_benefit_75","net_monetary_benefit_250", "net_monetary_benefit_1000"),
               names_to = "wtp",
               values_to = "net_monetary_benefit") %>%
  mutate(wtp = as.numeric(gsub("net_monetary_benefit_","",wtp))) %>%
  left_join(intervention_names2, by = "intervention") %>%
  left_join(data.frame(intervention = sort(unique(scenarios_irs$intervention)),
                       intervention_number = 1:17), by = "intervention") %>%
  mutate(intervention_label = paste0(intervention_number, ". ", intervention2),
         seasonality = stringr::str_to_sentence(seasonality),
         seasonality = factor(seasonality, levels = c(
           "Perennial", "Seasonal", "Highly seasonal")))

## Net monetary benefit plots --------------------------------------------------

# Bar chart of bars are the overall median and Cri the overall variation,
# with points showing the median for each setting
nmb_plot_250 <- ggplot() +
  geom_col(data=(scenarios_250_1000 %>% filter(wtp %in% c(250)) %>%
                   group_by(seasonality,intervention_number, intervention_label) %>%
                   summarise(net_monetary_benefit = median(net_monetary_benefit))),
           aes(x=as.factor(intervention_number), y = net_monetary_benefit/200000, 
               fill = reorder(intervention_label, intervention_number),
               col = reorder(intervention_label, intervention_number)),
           alpha = 0.7) +
  geom_errorbar(data=(scenarios_250_1000 %>% filter(wtp %in% c(250)) %>%
                        group_by(seasonality,intervention_number, intervention_label) %>%
                        summarise(net_monetary_benefit_lo = quantile(net_monetary_benefit, 0.025),
                                  net_monetary_benefit_hi = quantile(net_monetary_benefit, 0.975))),
                aes(x=intervention_number, ymin = net_monetary_benefit_lo/200000, ymax=net_monetary_benefit_hi/200000,
                    col = reorder(intervention_label, intervention_number)),
                width = 0.5) +
  geom_jitter(data= (scenarios_250_1000 %>% filter(wtp %in% c(250)) %>%
                       group_by(seasonality,intervention_number, intervention_label, baseline_name) %>%
                       summarise(net_monetary_benefit = median(net_monetary_benefit))),
              aes(x=intervention_number, y = net_monetary_benefit/200000, 
                  col = reorder(intervention_label, intervention_number)), width = 0.25) +
  geom_blank(data= (scenarios_250_1000 %>% filter(wtp %in% c(250)) %>%
                      group_by(seasonality,intervention_number, intervention_label) %>%
                      summarise(net_monetary_benefit_lo = quantile(net_monetary_benefit, 0.025),
                                net_monetary_benefit_hi = quantile(net_monetary_benefit, 0.975))),
             aes(x=intervention_number, y = -net_monetary_benefit_hi/200000/6)) +
  facet_wrap(~seasonality,nrow=3, scales= "free_y", strip.position = "left") +
  scale_colour_manual(values = assign_colours2) +
  scale_fill_manual(values = assign_colours2)+  
  theme_classic() +
  labs(title = "Threshold = US$250 per DALY averted", x = "Intervention package",
       fill = "Intervention package",
       col = "Intervention package",
       y = "Net monetary benefit (US$ per person at risk)", tag = "") +
  geom_hline(yintercept=0) +
  guides(colour="none") +
  theme(axis.title=element_text(size = 14),
        legend.title=element_text(size = 14), 
        legend.text=element_text(size = 11), 
        axis.text = element_text(size = 12), 
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title.y = element_blank(),
        strip.placement = "outside",
        panel.border = element_rect(colour = "black", fill=NA),
        title = element_text(size = 11),
        plot.tag = element_text(size = 16))  

nmb_plot_75 <- ggplot() +
  geom_col(data=(scenarios_250_1000 %>% filter(wtp %in% c(75)) %>%
                   group_by(seasonality,intervention_number, intervention_label) %>%
                   summarise(net_monetary_benefit = median(net_monetary_benefit))),
           aes(x=as.factor(intervention_number), y = net_monetary_benefit/200000, 
               fill = reorder(intervention_label, intervention_number),
               col = reorder(intervention_label, intervention_number)),
           alpha = 0.7) +
  geom_errorbar(data=(scenarios_250_1000 %>% filter(wtp %in% c(75)) %>%
                        group_by(seasonality,intervention_number, intervention_label) %>%
                        summarise(net_monetary_benefit_lo = quantile(net_monetary_benefit, 0.025),
                                  net_monetary_benefit_hi = quantile(net_monetary_benefit, 0.975))),
                aes(x=intervention_number, ymin = net_monetary_benefit_lo/200000, ymax=net_monetary_benefit_hi/200000,
                    col = reorder(intervention_label, intervention_number)),
                width = 0.5) +
  geom_jitter(data= (scenarios_250_1000 %>% filter(wtp %in% c(75)) %>%
                       group_by(seasonality,intervention_number, intervention_label, baseline_name) %>%
                       summarise(net_monetary_benefit = median(net_monetary_benefit))),
              aes(x=intervention_number, y = net_monetary_benefit/200000, 
                  col = reorder(intervention_label, intervention_number)), width = 0.25) +
  geom_blank(data= (scenarios_250_1000 %>% filter(wtp %in% c(75)) %>%
                      group_by(seasonality,intervention_number, intervention_label) %>%
                      summarise(net_monetary_benefit_lo = quantile(net_monetary_benefit, 0.025),
                                net_monetary_benefit_hi = quantile(net_monetary_benefit, 0.975))),
             aes(x=intervention_number, y = -net_monetary_benefit_hi/200000/6)) +
  facet_wrap(~seasonality,nrow=3, scales= "free_y", strip.position = "left") +
  scale_colour_manual(values = assign_colours2) +
  scale_fill_manual(values = assign_colours2)+  
  theme_classic() +
  labs(title = "Threshold = US$75 per DALY averted", x = "Intervention package",
       fill = "Intervention package",
       col = "Intervention package", tag = "B",
       y = "Net monetary benefit (US$ per person at risk)") +
  geom_hline(yintercept=0) +
  guides(colour="none") +
  theme(axis.title=element_text(size = 14),
        legend.title=element_text(size = 14), 
        legend.text=element_text(size = 11), 
        axis.text = element_text(size = 12), 
        strip.text=element_text(size = 11), 
        strip.placement = "outside",
        panel.border = element_rect(colour = "black", fill=NA),
        title = element_text(size = 11),
        plot.tag = element_text(size = 16))  

nmb_plot_1000 <- ggplot() +
  geom_col(data=(scenarios_250_1000 %>% filter(wtp %in% c(1000)) %>%
                   group_by(seasonality,intervention_number, intervention_label) %>%
                   summarise(net_monetary_benefit = median(net_monetary_benefit))),
           aes(x=as.factor(intervention_number), y = net_monetary_benefit/200000, 
               fill = reorder(intervention_label, intervention_number),
               col = reorder(intervention_label, intervention_number)),
           alpha = 0.7) +
  geom_errorbar(data=(scenarios_250_1000 %>% filter(wtp %in% c(1000)) %>%
                        group_by(seasonality,intervention_number, intervention_label) %>%
                        summarise(net_monetary_benefit_lo = quantile(net_monetary_benefit, 0.025),
                                  net_monetary_benefit_hi = quantile(net_monetary_benefit, 0.975))),
                aes(x=intervention_number, ymin = net_monetary_benefit_lo/200000, ymax=net_monetary_benefit_hi/200000,
                    col = reorder(intervention_label, intervention_number)),
                width = 0.5) +
  geom_jitter(data= (scenarios_250_1000 %>% filter(wtp %in% c(1000)) %>%
                       group_by(seasonality,intervention_number, intervention_label, baseline_name) %>%
                       summarise(net_monetary_benefit = median(net_monetary_benefit))),
              aes(x=intervention_number, y = net_monetary_benefit/200000, 
                  col = reorder(intervention_label, intervention_number)), width = 0.25) +
  geom_blank(data= (scenarios_250_1000 %>% filter(wtp %in% c(1000)) %>%
                      group_by(seasonality,intervention_number, intervention_label) %>%
                      summarise(net_monetary_benefit_lo = quantile(net_monetary_benefit, 0.025),
                                net_monetary_benefit_hi = quantile(net_monetary_benefit, 0.975))),
             aes(x=intervention_number, y = -net_monetary_benefit_hi/200000/6)) +
  facet_wrap(~seasonality,nrow=3, scales= "free_y", strip.position = "left") +
  scale_colour_manual(values = assign_colours2) +
  scale_fill_manual(values = assign_colours2)+  
  theme_classic() +
  labs(title = "Threshold = US$1000 per DALY averted", x = "Intervention package", 
       fill = "Intervention package",
       col = "Intervention package", tag = "",
       y = "Net monetary benefit (US$ per person at risk)") +
  geom_hline(yintercept=0) +
  guides(colour="none") +
  theme(axis.title=element_text(size = 14),
        legend.title=element_text(size = 14), 
        legend.text=element_text(size = 11), 
        axis.text = element_text(size = 12), 
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title.y = element_blank(),
        strip.placement = "outside",
        panel.border = element_rect(colour = "black", fill=NA),
        title = element_text(size = 11),
        plot.tag = element_text(size = 16)
        )

# DALYS averted
dalys_plot <- ggplot() +
  geom_col(data=(scenarios_250_1000 %>% filter(wtp %in% c(1000)) %>%
                   group_by(seasonality,intervention_number, intervention_label) %>%
                   summarise(dalys_averted = median(dalys_averted))),
           aes(x=as.factor(intervention_number), y = dalys_averted*10000/200000, 
               fill = reorder(intervention_label, intervention_number),
               col = reorder(intervention_label, intervention_number)),
           alpha = 0.7) +
    geom_errorbar(data=(scenarios_250_1000 %>% filter(wtp %in% c(1000)) %>%
                          group_by(seasonality,intervention_number, intervention_label) %>%
                          summarise(dalys_averted_lo = quantile(dalys_averted, 0.025),
                                    dalys_averted_hi = quantile(dalys_averted, 0.975))),
                  aes(x=intervention_number, ymin = dalys_averted_lo*10000/200000, 
                      ymax=dalys_averted_hi*10000/200000,
                      col = reorder(intervention_label, intervention_number)),
                  width = 0.5) +
  geom_jitter(data= (scenarios_250_1000 %>% filter(wtp %in% c(1000)) %>%
                           group_by(seasonality,intervention_number, intervention_label, baseline_name) %>%
                           summarise(dalys_averted = median(dalys_averted))),
                  aes(x=intervention_number, y = dalys_averted*10000/200000, 
                      col = reorder(intervention_label, intervention_number)), width = 0.25) +
  geom_blank(data=(scenarios_250_1000 %>% filter(wtp %in% c(1000)) %>%
                            group_by(seasonality,intervention_number, intervention_label) %>%
                            summarise(dalys_averted_lo = quantile(dalys_averted, 0.025),
                                      dalys_averted_hi = quantile(dalys_averted, 0.975))),
                    aes(x=intervention_number, y = -dalys_averted_hi*10000/200000/6)) +
  facet_wrap(~seasonality,nrow=3, scales= "free_y", strip.position = "left") +
  scale_colour_manual(values = assign_colours2) +
  scale_fill_manual(values = assign_colours2) +    
  theme_classic() +
  labs(title = "", x = "Intervention package", fill = "Intervention package",
       col = "Intervention package",
       y = "DALYs averted (per 10,000 persons at risk)", tag = "A") +
  geom_hline(yintercept=0) +
  guides(colour="none") +
  theme(axis.title=element_text(size = 14),
        legend.title=element_text(size = 16), 
        legend.text=element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text=element_text(size = 11), 
        strip.placement = "outside",
        panel.border = element_rect(colour = "black", fill=NA),
        title = element_text(size = 11),
        plot.tag = element_text(size = 16)
  ) 

leg_nmb <- get_legend(dalys_plot)
leg_nmb <- as_ggplot(leg_nmb)

ggpubr::ggarrange(ggpubr::ggarrange(dalys_plot, leg_nmb, ncol = 2, nrow = 1, 
                                    legend = "none"),
                  ggpubr::ggarrange(nmb_plot_75, nmb_plot_250, nmb_plot_1000, ncol = 3, nrow = 1, 
                                    legend = "none", widths = c(1.15,1,1)),
                  nrow = 2, ncol = 1)
#ggsave("plots/fig1.png", width = 30, height = 25, units="cm")

# Same barchart with IRS

nmb_plot_75_irs <- ggplot() +
  geom_col(data=(scenarios_250_1000_irs %>% filter(wtp %in% c(75)) %>%
                   group_by(seasonality,intervention_number, intervention_label) %>%
                   summarise(net_monetary_benefit = median(net_monetary_benefit))),
           aes(x=as.factor(intervention_number), y = net_monetary_benefit/200000, 
               fill = reorder(intervention_label, intervention_number),
               col = reorder(intervention_label, intervention_number)),
           alpha = 0.7) +
  geom_errorbar(data=(scenarios_250_1000_irs %>% filter(wtp %in% c(75)) %>%
                        group_by(seasonality,intervention_number, intervention_label) %>%
                        summarise(net_monetary_benefit_lo = quantile(net_monetary_benefit, 0.025),
                                  net_monetary_benefit_hi = quantile(net_monetary_benefit, 0.975))),
                aes(x=intervention_number, ymin = net_monetary_benefit_lo/200000, ymax=net_monetary_benefit_hi/200000,
                    col = reorder(intervention_label, intervention_number)),
                width = 0.5) +
  geom_jitter(data= (scenarios_250_1000_irs %>% filter(wtp %in% c(75)) %>%
                       group_by(seasonality,intervention_number, intervention_label, baseline_name) %>%
                       summarise(net_monetary_benefit = median(net_monetary_benefit))),
              aes(x=intervention_number, y = net_monetary_benefit/200000, 
                  col = reorder(intervention_label, intervention_number)), width = 0.25) +
  geom_blank(data= (scenarios_250_1000_irs %>% filter(wtp %in% c(75)) %>%
                      group_by(seasonality,intervention_number, intervention_label) %>%
                      summarise(net_monetary_benefit_lo = quantile(net_monetary_benefit, 0.025),
                                net_monetary_benefit_hi = quantile(net_monetary_benefit, 0.975))),
             aes(x=intervention_number, y = -net_monetary_benefit_hi/200000/5)) +
  facet_wrap(~seasonality,nrow=3, scales= "free_y", strip.position = "left") +
  scale_fill_manual(values = assign_colours3) +
  scale_colour_manual(values = assign_colours3) + 
  theme_classic() +
  labs(title = "Threshold = $75 per DALY averted", x = "Intervention package",
       fill = "Intervention package",
       col = "Intervention package", tag = "B",
       y = "Net monetary benefit (US$ per person at risk)") +
  geom_hline(yintercept=0) +
  guides(colour="none") +
  theme(axis.title=element_text(size = 14),
        legend.title=element_text(size = 14), 
        legend.text=element_text(size = 11), 
        axis.text = element_text(size = 10), 
        strip.text=element_text(size = 11), 
        strip.placement = "outside",
        panel.border = element_rect(colour = "black", fill=NA),
        title = element_text(size = 11),
        plot.tag = element_text(size = 16)) 

nmb_plot_250_irs <- ggplot() +
  geom_col(data=(scenarios_250_1000_irs %>% filter(wtp %in% c(250)) %>%
                   group_by(seasonality,intervention_number, intervention_label) %>%
                   summarise(net_monetary_benefit = median(net_monetary_benefit))),
           aes(x=as.factor(intervention_number), y = net_monetary_benefit/200000, 
               fill = reorder(intervention_label, intervention_number),
               col = reorder(intervention_label, intervention_number)),
           alpha = 0.7) +
  geom_errorbar(data=(scenarios_250_1000_irs %>% filter(wtp %in% c(250)) %>%
                        group_by(seasonality,intervention_number, intervention_label) %>%
                        summarise(net_monetary_benefit_lo = quantile(net_monetary_benefit, 0.025),
                                  net_monetary_benefit_hi = quantile(net_monetary_benefit, 0.975))),
                aes(x=intervention_number, ymin = net_monetary_benefit_lo/200000, ymax=net_monetary_benefit_hi/200000,
                    col = reorder(intervention_label, intervention_number)),
                width = 0.5) +
  geom_jitter(data= (scenarios_250_1000_irs %>% filter(wtp %in% c(250)) %>%
                       group_by(seasonality,intervention_number, intervention_label, baseline_name) %>%
                       summarise(net_monetary_benefit = median(net_monetary_benefit))),
              aes(x=intervention_number, y = net_monetary_benefit/200000, 
                  col = reorder(intervention_label, intervention_number)), width = 0.25) +
  geom_blank(data= (scenarios_250_1000_irs %>% filter(wtp %in% c(250)) %>%
                      group_by(seasonality,intervention_number, intervention_label) %>%
                      summarise(net_monetary_benefit_lo = quantile(net_monetary_benefit, 0.025),
                                net_monetary_benefit_hi = quantile(net_monetary_benefit, 0.975))),
             aes(x=intervention_number, y = -net_monetary_benefit_hi/200000/5)) +
  facet_wrap(~seasonality,nrow=3, scales= "free_y", strip.position = "left") +
  scale_fill_manual(values = assign_colours3) +
  scale_colour_manual(values = assign_colours3) + 
  theme_classic() +
  labs(title = "Threshold = $250 per DALY averted", x = "Intervention package",
       fill = "Intervention package",
       col = "Intervention package", tag = "",
       y = "Net monetary benefit (US$ per person at risk)") +
  geom_hline(yintercept=0) +
  guides(colour="none") +
  theme(axis.title=element_text(size = 14),
        legend.title=element_text(size = 14), 
        legend.text=element_text(size = 11), 
        axis.text = element_text(size = 10), 
        strip.text=element_text(size = 11), 
        strip.placement = "outside",
        panel.border = element_rect(colour = "black", fill=NA),
        title = element_text(size = 11),
        plot.tag = element_text(size = 16)) 

nmb_plot_1000_irs <- ggplot() +
  geom_col(data=(scenarios_250_1000_irs %>% filter(wtp %in% c(1000)) %>%
                   group_by(seasonality,intervention_number, intervention_label) %>%
                   summarise(net_monetary_benefit = median(net_monetary_benefit))),
           aes(x=as.factor(intervention_number), y = net_monetary_benefit/200000, 
               fill = reorder(intervention_label, intervention_number),
               col = reorder(intervention_label, intervention_number)),
           alpha = 0.7) +
  geom_errorbar(data=(scenarios_250_1000_irs %>% filter(wtp %in% c(1000)) %>%
                        group_by(seasonality,intervention_number, intervention_label) %>%
                        summarise(net_monetary_benefit_lo = quantile(net_monetary_benefit, 0.025),
                                  net_monetary_benefit_hi = quantile(net_monetary_benefit, 0.975))),
                aes(x=intervention_number, ymin = net_monetary_benefit_lo/200000, ymax=net_monetary_benefit_hi/200000,
                    col = reorder(intervention_label, intervention_number)),
                width = 0.5) +
  geom_jitter(data= (scenarios_250_1000_irs %>% filter(wtp %in% c(1000)) %>%
                       group_by(seasonality,intervention_number, intervention_label, baseline_name) %>%
                       summarise(net_monetary_benefit = median(net_monetary_benefit))),
              aes(x=intervention_number, y = net_monetary_benefit/200000, 
                  col = reorder(intervention_label, intervention_number)), width = 0.25) +
  geom_blank(data= (scenarios_250_1000_irs %>% filter(wtp %in% c(1000)) %>%
                      group_by(seasonality,intervention_number, intervention_label) %>%
                      summarise(net_monetary_benefit_lo = quantile(net_monetary_benefit, 0.025),
                                net_monetary_benefit_hi = quantile(net_monetary_benefit, 0.975))),
             aes(x=intervention_number, y = -net_monetary_benefit_hi/200000/5)) +
  facet_wrap(~seasonality,nrow=3, scales= "free_y", strip.position = "left") +
  #facet_wrap(seasonality~with_irs,nrow=3, scales= "free", strip.position = "left") +
  scale_fill_manual(values = assign_colours3) +
  scale_colour_manual(values = assign_colours3) + 
  theme_classic() +
  labs(title = "Threshold = $1000 per DALY averted", x = "Intervention package",
       fill = "Intervention package",
       col = "Intervention package", 
       y = "Net monetary benefit (US$ per person at risk)", tag = "") +
  geom_hline(yintercept=0) +
  #theme(axis.text.x=element_blank(), strip.placement = "outside",
  #      panel.border = element_rect(colour = "black", fill=NA),
  #      title = element_text(size = 9))  # axis.text.x=element_text(angle=90, hjust=1)
  guides(colour="none") +
  theme(axis.title=element_text(size = 14),
        legend.title=element_text(size = 14), 
        legend.text=element_text(size = 11), 
        axis.text = element_text(size = 10), 
        strip.text=element_text(size = 11), 
        strip.placement = "outside",
        panel.border = element_rect(colour = "black", fill=NA),
        title = element_text(size = 11),
        plot.tag = element_text(size = 16)) 

# DALYS averted
dalys_plot_irs <- ggplot() +
  geom_col(data=(scenarios_250_1000_irs %>% filter(wtp %in% c(250)) %>%
                   group_by(seasonality,intervention_number, intervention_label) %>%
                   summarise(dalys_averted = median(dalys_averted))),
           aes(x=as.factor(intervention_number), y =  dalys_averted*10000/200000, 
               fill = reorder(intervention_label, intervention_number),
               col = reorder(intervention_label, intervention_number)),
           alpha = 0.7) +
  geom_errorbar(data=(scenarios_250_1000_irs %>% filter(wtp %in% c(250)) %>%
                        group_by(seasonality,intervention_number, intervention_label) %>%
                        summarise(dalys_averted_lo = quantile(dalys_averted, 0.025),
                                  dalys_averted_hi = quantile(dalys_averted, 0.975))),
                aes(x=intervention_number,  ymin = dalys_averted_lo*10000/200000, 
                    ymax=dalys_averted_hi*10000/200000,
                    col = reorder(intervention_label, intervention_number)),
                width = 0.5) +
  geom_jitter(data= (scenarios_250_1000_irs %>% filter(wtp %in% c(250)) %>%
                       group_by(seasonality,intervention_number, intervention_label, baseline_name) %>%
                       summarise(dalys_averted = median(dalys_averted))),
              aes(x=intervention_number, y = dalys_averted*10000/200000, 
                  col = reorder(intervention_label, intervention_number)), width = 0.25) +
  geom_blank(data= (scenarios_250_1000_irs %>% filter(wtp %in% c(250)) %>%
                      group_by(seasonality,intervention_number, intervention_label) %>%
                      summarise(dalys_averted_lo = quantile(dalys_averted, 0.025),
                                dalys_averted_hi = quantile(dalys_averted, 0.975))),
             aes(x=intervention_number, y = -dalys_averted_hi*10000/200000/5)) +
  facet_wrap(~seasonality,nrow=3, scales= "free_y", strip.position = "left") +
  scale_fill_manual(values = assign_colours3) +
  scale_colour_manual(values = assign_colours3) + 
  theme_classic() +
  labs(x = "Intervention package",
       fill = "Intervention package",
       col = "Intervention package", tag = "A",
       y = "DALYs averted (per 10,000 persons at risk)") +
  geom_hline(yintercept=0) +
  guides(colour="none") +
  theme(axis.title=element_text(size = 14),
        legend.title=element_text(size = 16), 
        legend.text=element_text(size = 14), 
        axis.text = element_text(size = 10), 
        strip.text=element_text(size = 11), 
        strip.placement = "outside",
        panel.border = element_rect(colour = "black", fill=NA),
        title = element_text(size = 11),
        plot.tag = element_text(size = 16)) 

leg_nmb_irs <- get_legend(dalys_plot_irs)
leg_nmb_irs <- as_ggplot(leg_nmb_irs)

ggpubr::ggarrange(ggpubr::ggarrange(dalys_plot_irs, leg_nmb_irs, ncol = 2, nrow = 1, 
                                    legend = "none"),
                  ggpubr::ggarrange(nmb_plot_75_irs, nmb_plot_250_irs, nmb_plot_1000_irs, ncol = 3, nrow = 1, 
                                    legend = "none", widths = c(1,1,1)),
                  nrow = 2, ncol = 1)
#ggsave("plots/fig1_irs.png", width = 33, height = 27, units="cm")

## EVPI summary table -----------------------------------------------------

evpi_summary <- filter(evpi_df, wtp %in% c(75, 250, 500, 1000, 3000)) %>%
  group_by(wtp) %>%
  summarise(ev_certainty_per_person_median = round(median(ev_certainty/200000),2),
            ev_certainty_per_person_IQR = paste0(round(quantile(ev_certainty/200000, 0.25),2),
                                                 "-", round(quantile(ev_certainty/200000, 0.75),2)),
            ev_certainty_per_person_range = paste0(round(min(ev_certainty/200000),2), 
                                           "-", round(max(ev_certainty/200000),2)),
            ev_uncertainty_per_person_median = round(median(max_mean_nmb/200000),2),
            ev_uncertainty_per_person_IQR = paste0(round(quantile(max_mean_nmb/200000, 0.25),2),
                                                 "-", round(quantile(max_mean_nmb/200000, 0.75),2)),
            ev_uncertainty_per_person_range = paste0(round(min(max_mean_nmb/200000),2), 
                                                   "-", round(max(max_mean_nmb/200000),2)),
            evpi_per_person_median = round(median(evpi_per_person),2),
            evpi_per_person_IQR = paste0(round(quantile(evpi_per_person, 0.25),2),
                                         "-", round(quantile(evpi_per_person, 0.75),2)),
            evpi_per_person_range = paste0(round(min(evpi_per_person),2), 
                                           "-", round(max(evpi_per_person),2)),
            evpi_percent_of_expected_nmb_median = round(median(prop_of_expected_nmb*100),2),
            evpi_percent_of_expected_nmb_IQR = paste0(round(quantile(prop_of_expected_nmb*100, 0.25),2),
                                                      "-", round(quantile(prop_of_expected_nmb*100, 0.75),2)),
            evpi_percent_of_expected_nmb_range = paste0(round(min(prop_of_expected_nmb*100),2), 
                                                        "-", round(max(prop_of_expected_nmb*100),2))  )

#write.csv(evpi_summary, "output_files/evpi_summary.csv")

# With IRS

evpi_summary_irs <- filter(evpi_df_irs, wtp %in% c(75, 250, 500, 1000, 3000)) %>%
  group_by(wtp) %>%
  summarise(ev_certainty_per_person_median = round(median(ev_certainty/200000),2),
            ev_certainty_per_person_IQR = paste0(round(quantile(ev_certainty/200000, 0.25),2),
                                                 "-", round(quantile(ev_certainty/200000, 0.75),2)),
            ev_certainty_per_person_range = paste0(round(min(ev_certainty/200000),2), 
                                                   "-", round(max(ev_certainty/200000),2)),
            ev_uncertainty_per_person_median = round(median(max_mean_nmb/200000),2),
            ev_uncertainty_per_person_IQR = paste0(round(quantile(max_mean_nmb/200000, 0.25),2),
                                                   "-", round(quantile(max_mean_nmb/200000, 0.75),2)),
            ev_uncertainty_per_person_range = paste0(round(min(max_mean_nmb/200000),2), 
                                                     "-", round(max(max_mean_nmb/200000),2)),
            evpi_per_person_median = round(median(evpi_per_person),2),
            evpi_per_person_IQR = paste0(round(quantile(evpi_per_person, 0.25),2),
                                         "-", round(quantile(evpi_per_person, 0.75),2)),
            evpi_per_person_range = paste0(round(min(evpi_per_person),2), 
                                           "-", round(max(evpi_per_person),2)),
            evpi_percent_of_expected_nmb_median = round(median(prop_of_expected_nmb*100),2),
            evpi_percent_of_expected_nmb_IQR = paste0(round(quantile(prop_of_expected_nmb*100, 0.25),2),
                                                      "-", round(quantile(prop_of_expected_nmb*100, 0.75),2)),
            evpi_percent_of_expected_nmb_range = paste0(round(min(prop_of_expected_nmb*100),2), 
                                                        "-", round(max(prop_of_expected_nmb*100),2))
            
  )
#write.csv(evpi_summary_irs, "output_files/evpi_summary_irs.csv")

## EVPI by setting plot ------------------------------------------------------

evpi_ranks <- evpi_df %>%
  group_by(wtp) %>%
  mutate(rank = rank(-evpi)) %>%
  arrange(wtp, rank) 

evpi_ranks$baseline_setting <- factor(evpi_ranks$baseline_setting, 
                                      levels = c("0.05_perennial_0.2", "0.05_perennial_0.4",
                                                 "0.05_perennial_0.6","0.1_perennial_0.2",
                                                 "0.1_perennial_0.4","0.1_perennial_0.6",
                                                 "0.2_perennial_0.2","0.2_perennial_0.4",
                                                 "0.2_perennial_0.6","0.4_perennial_0.2",
                                                 "0.4_perennial_0.4","0.4_perennial_0.6",
                                                 "0.05_seasonal_0.2","0.05_seasonal_0.4",
                                                 "0.05_seasonal_0.6","0.1_seasonal_0.2",
                                                 "0.1_seasonal_0.4","0.1_seasonal_0.6",
                                                 "0.2_seasonal_0.2","0.2_seasonal_0.4",
                                                 "0.2_seasonal_0.6","0.4_seasonal_0.2",
                                                 "0.4_seasonal_0.4","0.4_seasonal_0.6",
                                                 "0.05_highly seasonal_0.2","0.05_highly seasonal_0.4",
                                                 "0.05_highly seasonal_0.6","0.1_highly seasonal_0.2",
                                                 "0.1_highly seasonal_0.4","0.1_highly seasonal_0.6",
                                                 "0.2_highly seasonal_0.2","0.2_highly seasonal_0.4",
                                                 "0.2_highly seasonal_0.6","0.4_highly seasonal_0.2",
                                                 "0.4_highly seasonal_0.4","0.4_highly seasonal_0.6"
                                      ))
evpi_ranks <- evpi_ranks %>%
  mutate(baseline_setting2 = baseline_setting) %>%
  separate(baseline_setting2, into = c("prev", "seas", "itn"), sep = "_") %>%
  mutate(seas=stringr::str_to_title(seas),
         prev = paste0(as.numeric(prev) * 100, "%"),
         prev = ifelse(prev == "5%", "  5%", prev),
         itn = paste0(as.numeric(itn) * 100, "%"),
         label = paste0(prev, "  ", itn))

evpi_ranks_irs <- evpi_df_irs %>%
  mutate(baseline_setting2 = baseline_setting) %>%
  separate(baseline_setting2, into = c("prev", "seas", "itn"), sep = "_") %>%
  mutate(seas=stringr::str_to_title(seas),
         prev = paste0(as.numeric(prev) * 100, "%"),
         prev = ifelse(prev == "5%", "  5%", prev),
         itn = paste0(as.numeric(itn) * 100, "%"),
         label = paste0(prev, "  ", itn))

evpi_ranks_irs$baseline_setting <- factor(evpi_ranks_irs$baseline_setting, 
                                             levels = c("0.05_perennial_0.2", "0.05_perennial_0.4",
                                                        "0.05_perennial_0.6","0.1_perennial_0.2",
                                                        "0.1_perennial_0.4","0.1_perennial_0.6",
                                                        "0.2_perennial_0.2","0.2_perennial_0.4",
                                                        "0.2_perennial_0.6","0.4_perennial_0.2",
                                                        "0.4_perennial_0.4","0.4_perennial_0.6",
                                                        "0.05_seasonal_0.2","0.05_seasonal_0.4",
                                                        "0.05_seasonal_0.6","0.1_seasonal_0.2",
                                                        "0.1_seasonal_0.4","0.1_seasonal_0.6",
                                                        "0.2_seasonal_0.2","0.2_seasonal_0.4",
                                                        "0.2_seasonal_0.6","0.4_seasonal_0.2",
                                                        "0.4_seasonal_0.4","0.4_seasonal_0.6",
                                                        "0.05_highly seasonal_0.2","0.05_highly seasonal_0.4",
                                                        "0.05_highly seasonal_0.6","0.1_highly seasonal_0.2",
                                                        "0.1_highly seasonal_0.4","0.1_highly seasonal_0.6",
                                                        "0.2_highly seasonal_0.2","0.2_highly seasonal_0.4",
                                                        "0.2_highly seasonal_0.6","0.4_highly seasonal_0.2",
                                                        "0.4_highly seasonal_0.4","0.4_highly seasonal_0.6"
                                             ))
evpi_ranks_irs <- evpi_ranks_irs %>%
  mutate(baseline_setting2 = baseline_setting) %>%
  separate(baseline_setting2, into = c("prev", "seas", "itn"), sep = "_") %>%
  mutate(seas=stringr::str_to_title(seas),
         prev = paste0(as.numeric(prev) * 100, "%"),
         prev = ifelse(prev == "5%", "  5%", prev),
         itn = paste0(as.numeric(itn) * 100, "%"),
         label = paste0(prev, "  ", itn))

evpi_ranks_2dim <- evpi_ranks %>%
  filter(wtp %in% c(75, 250,500,1000,3000)) %>%
  group_by(wtp, prev, seas) %>%
  summarise(evpi_per_person = median(evpi_per_person)) %>%  # average EVPI by ITN use
  group_by(wtp) %>%
  mutate(rank = rank(-evpi_per_person)) %>%
  group_by(prev, seas) %>%
  summarise(rank = mean(rank),
            evpi_per_person = median(evpi_per_person)) %>%
  mutate(seas = factor(seas, levels = c("Perennial", "Seasonal", "Highly Seasonal"))) 

evpi_ranks_2dim_b <- evpi_ranks %>%
  filter(wtp %in% c(75, 250,500,1000,3000)) %>%
  group_by(wtp, prev, seas) %>%
  summarise(evpi_per_person = median(evpi_per_person)) %>%  # average EVPI by ITN use
  mutate(seas = factor(seas, levels = c("Perennial", "Seasonal", "Highly Seasonal"))) 


# CET 75:
evppi_a_75 <- ggplot(filter(evpi_ranks_2dim_b, wtp == 75), 
                      aes(prev,reorder(seas, desc(seas)))) + 
  geom_tile(colour= "grey70", fill= "white", size = 0.1) + 
  geom_point(aes(col = evpi_per_person), 
             shape = 16, size = 30) + 
  scale_colour_gradientn(colours= brewer.pal(7,"YlOrRd"), 
                         breaks = c(max(evpi_ranks_2dim_b$evpi_per_person[evpi_ranks_2dim_b$wtp == 75]),
                                    min(evpi_ranks_2dim_b$evpi_per_person[evpi_ranks_2dim_b$wtp == 75])),
                         labels = c("Highest","Lowest")) +
  coord_cartesian(expand = FALSE) +
  labs(col = "Value of\ninformation\n", tag = "A") +   # , tag = "A"
  theme_bw() +
  labs(title = "Threshold = US$75 per DALY averted", y = "Seasonality profile",
       x = expression(paste(italic("P. falciparum"), " parasite prevalence"))) +
  guides(size = "none") +
  scale_y_discrete(labels = c("Highly Seasonal" = "Highly seasonal")) +
  theme(axis.ticks=element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.title = element_text(color="black", size=16,hjust = 0.5),
        axis.text.x=element_text(angle=0, size = 14,hjust = 0.5, vjust = 1),
        axis.text.y=element_text(size = 14),
        axis.title.y=element_text(size = 16),
        axis.title.x=element_blank(),
        strip.text=element_text(size = 14),
        legend.title=element_text(size = 16, hjust=0.5),
        legend.text=element_text(size = 14, hjust=0.5),
        plot.tag=element_text(size = 20),
        strip.placement = "outside", 
        plot.margin = unit(c(5.5, 15, 10, 5.5), 
                           "pt"))

# CET 250:
evppi_a_250 <- ggplot(filter(evpi_ranks_2dim_b, wtp == 250), 
       aes(prev,reorder(seas, desc(seas)))) + 
  geom_tile(colour= "grey70", fill= "white", size = 0.1) + 
  geom_point(aes(col = evpi_per_person), 
             shape = 16, size = 30) + 
  scale_colour_gradientn(colours= brewer.pal(7,"YlOrRd"), 
                         breaks = c(max(evpi_ranks_2dim_b$evpi_per_person[evpi_ranks_2dim_b$wtp == 250]),
                                    min(evpi_ranks_2dim_b$evpi_per_person[evpi_ranks_2dim_b$wtp == 250])),
                         labels = c("Highest","Lowest")) +
  coord_cartesian(expand = FALSE) +
  labs(col = "Value of\ninformation\n", tag = "A") +   # , tag = "A"
  theme_bw() +
  labs(title = "Threshold = US$250 per DALY averted", y = "Seasonality profile",
       x = expression(paste(italic("P. falciparum"), " parasite prevalence"))) +
  guides(size = "none") +
  scale_y_discrete(labels = c("Highly Seasonal" = "Highly seasonal")) +
  theme(axis.ticks=element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.title = element_text(color="black", size=16,hjust = 0.5),
        axis.text.x=element_text(angle=0, size = 14,hjust = 0.5, vjust = 1),
        axis.text.y=element_text(size = 14),
        axis.title.y=element_text(size = 16),
        axis.title.x=element_blank(),
        strip.text=element_text(size = 14),
        legend.title=element_text(size = 16, hjust=0.5),
        legend.text=element_text(size = 14, hjust=0.5),
        plot.tag=element_text(size = 20, colour = "white"),
        strip.placement = "outside", 
        plot.margin = unit(c(5.5, 15, 10, 5.5), 
                           "pt"))

# CET 1000:
evppi_a_1000 <- ggplot(filter(evpi_ranks_2dim_b, wtp == 1000), 
                      aes(prev,reorder(seas, desc(seas)))) + 
  geom_tile(colour= "grey70", fill= "white", size = 0.1) + 
  geom_point(aes(col = evpi_per_person), 
             shape = 16, size = 30) + 
  scale_colour_gradientn(colours= brewer.pal(7,"YlOrRd"), 
                         breaks = c(max(evpi_ranks_2dim_b$evpi_per_person[evpi_ranks_2dim_b$wtp ==1000]),
                                    min(evpi_ranks_2dim_b$evpi_per_person[evpi_ranks_2dim_b$wtp ==1000])),
                         labels = c("Highest","Lowest")) +
  coord_cartesian(expand = FALSE) +
  labs(col = "Value of\ninformation\n", tag = "A") +   # , tag = "A"
  theme_bw() +
  labs(title = "Threshold = US$1000 per DALY averted", y = "Seasonality profile",
       x = expression(paste(italic("P. falciparum"), " parasite prevalence"))) +
  guides(size = "none") +
  scale_y_discrete(labels = c("Highly Seasonal" = "Highly seasonal")) +
  theme(axis.ticks=element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.title = element_text(color="black", size=16,hjust = 0.5),
        axis.text.x=element_text(angle=0, size = 14,hjust = 0.5, vjust = 1),
        axis.text.y=element_text(size = 14),
        axis.title=element_text(size = 16),
        strip.text=element_text(size = 14),
        legend.title=element_text(size = 16, hjust=0.5),
        legend.text=element_text(size = 14, hjust=0.5),
        plot.tag=element_text(size = 20, colour = "white"),
        strip.placement = "outside", 
        plot.margin = unit(c(5.5, 15, 10, 5.5), 
                           "pt"))

# With IRS
evpi_ranks_2dim_irs <- evpi_ranks_irs %>%
  filter(wtp %in% c(75, 250,500,1000,3000)) %>%
  group_by(wtp, prev, seas) %>%
  summarise(evpi_per_person = median(evpi_per_person)) %>%  # average EVPI by ITN use
  group_by(wtp) %>%
  mutate(rank = rank(-evpi_per_person)) %>%
  group_by(prev, seas) %>%
  summarise(rank = mean(rank),
            evpi_per_person = mean(evpi_per_person)) %>%
  mutate(seas = factor(seas, levels = c("Perennial", "Seasonal", "Highly Seasonal"))) 

evpi_ranks_2dim_irs_b <- evpi_ranks_irs %>%
  filter(wtp %in% c(75, 250,500,1000,3000)) %>%
  group_by(wtp, prev, seas) %>%
  summarise(evpi_per_person = median(evpi_per_person)) %>% # average EVPI by ITN use
mutate(seas = factor(seas, levels = c("Perennial", "Seasonal", "Highly Seasonal"))) 

# CET 75:
evppi_c_75 <- ggplot(filter(evpi_ranks_2dim_irs_b, wtp == 75), 
                      aes(prev,reorder(seas, desc(seas)))) + 
  geom_tile(colour= "grey70", fill= "white", size = 0.1) + 
  geom_point(aes(col = evpi_per_person), 
             shape = 16, size = 30) + 
  scale_colour_gradientn(colours= brewer.pal(7,"YlOrRd"), 
                         breaks = c(max(evpi_ranks_2dim_irs_b$evpi_per_person[evpi_ranks_2dim_irs_b$wtp == 75]),
                                    min(evpi_ranks_2dim_irs_b$evpi_per_person[evpi_ranks_2dim_irs_b$wtp == 75])),
                         labels = c("Highest","Lowest")) +
  coord_cartesian(expand = FALSE) +
  labs(col = "Value of\ninformation\n", tag = "B") +   # , tag = "A"
  theme_bw() +
  labs(title = "Threshold = US$75 per DALY averted", y = "Seasonality profile",
       x = expression(paste(italic("P. falciparum"), " parasite prevalence"))) +
  guides(size = "none") +
  scale_y_discrete(labels = c("Highly Seasonal" = "Highly seasonal")) +
  theme(axis.ticks=element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.title = element_text(color="black", size=16,hjust = 0.5),
        axis.text.x=element_text(angle=0, size = 14,hjust = 0.5, vjust = 1),
        axis.text.y=element_text(size = 14),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        strip.text=element_text(size = 14),
        legend.title=element_text(size = 16, hjust=0.5),
        legend.text=element_text(size = 14, hjust=0.5),
        plot.tag=element_text(size = 20),
        strip.placement = "outside", 
        plot.margin = unit(c(5.5, 15, 10, 5.5), 
                           "pt"))

# CET 250:
evppi_c_250 <- ggplot(filter(evpi_ranks_2dim_irs_b, wtp == 250), 
                      aes(prev,reorder(seas, desc(seas)))) + 
  geom_tile(colour= "grey70", fill= "white", size = 0.1) + 
  geom_point(aes(col = evpi_per_person), 
             shape = 16, size = 30) + 
  scale_colour_gradientn(colours= brewer.pal(7,"YlOrRd"), 
                         breaks = c(max(evpi_ranks_2dim_irs_b$evpi_per_person[evpi_ranks_2dim_irs_b$wtp == 250]),
                                    min(evpi_ranks_2dim_irs_b$evpi_per_person[evpi_ranks_2dim_irs_b$wtp == 250])),
                         labels = c("Highest","Lowest")) +
  coord_cartesian(expand = FALSE) +
  labs(col = "Value of\ninformation\n", tag = "B") +   # , tag = "A"
  theme_bw() +
  labs(title = "Threshold = US$250 per DALY averted", y = "Seasonality profile",
       x = expression(paste(italic("P. falciparum"), " parasite prevalence"))) +
  guides(size = "none") +
  scale_y_discrete(labels = c("Highly Seasonal" = "Highly seasonal")) +
  theme(axis.ticks=element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.title = element_text(color="black", size=16,hjust = 0.5),
        axis.text.x=element_text(angle=0, size = 14,hjust = 0.5, vjust = 1),
        axis.text.y=element_text(size = 14),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        strip.text=element_text(size = 14),
        legend.title=element_text(size = 16, hjust=0.5),
        legend.text=element_text(size = 14, hjust=0.5),
        plot.tag=element_text(size = 20, colour = "white"),
        strip.placement = "outside", 
        plot.margin = unit(c(5.5, 15, 10, 5.5), 
                           "pt"))

# CET 1000:
evppi_c_1000 <- ggplot(filter(evpi_ranks_2dim_irs_b, wtp == 1000), 
                       aes(prev,reorder(seas, desc(seas)))) + 
  geom_tile(colour= "grey70", fill= "white", size = 0.1) + 
  geom_point(aes(col = evpi_per_person), 
             shape = 16, size = 30) + 
  scale_colour_gradientn(colours= brewer.pal(7,"YlOrRd"), 
                         breaks = c(max(evpi_ranks_2dim_irs_b$evpi_per_person[evpi_ranks_2dim_irs_b$wtp ==1000]),
                                    min(evpi_ranks_2dim_irs_b$evpi_per_person[evpi_ranks_2dim_irs_b$wtp ==1000])),
                         labels = c("Highest","Lowest")) +
  coord_cartesian(expand = FALSE) +
  labs(col = "Value of\ninformation\n", tag = "B") +   # , tag = "A"
  theme_bw() +
  labs(title = "Threshold = US$1000 per DALY averted", y = "Seasonality profile",
       x = expression(paste(italic("P. falciparum"), " parasite prevalence"))) +
  guides(size = "none") +
  scale_y_discrete(labels = c("Highly Seasonal" = "Highly seasonal")) +
  theme(axis.ticks=element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.title = element_text(color="black", size=16,hjust = 0.5),
        axis.text.x=element_text(angle=0, size = 14,hjust = 0.5, vjust = 1),
        axis.text.y=element_text(size = 14),
        axis.title.x=element_text(size = 16),
        axis.title.y=element_blank(),
        strip.text=element_text(size = 14),
        legend.title=element_text(size = 16, hjust=0.5),
        legend.text=element_text(size = 14, hjust=0.5),
        plot.tag=element_text(size = 20, colour = "white"),
        strip.placement = "outside", 
        plot.margin = unit(c(5.5, 15, 10, 5.5), 
                           "pt"))

# EVPI by setting: combine with and without IRS
fig2 <- ggarrange(evppi_a_75, evppi_c_75, evppi_a_250, evppi_c_250, evppi_a_1000, evppi_c_1000, nrow = 3, ncol = 2, common.legend= TRUE, legend = "right")
#ggsave("plots/fig2.png", fig2, width = 33, height = 31, units = "cm")

## EVPPI plot ------------------------------------------------------------------

parameter_labels <- c(
  "resistance" = "Resistance",
  "Q0" = "Anthropophagic\nvector behaviour",
  "phi_indoors" = "Indoor biting\nvector behaviour",
  "estimate" = "ITN entomological efficacy",
  "estimate_Q0_phi_indoors" = "ITN effect +\nvector behaviour",
  "Q0_phi_indoors" = "Vector behaviour", 
  "resistance_estimate" = "ITN effectiveness",
  "resistance_Q0_phi_indoors" = "Resistance +\nvector behaviour",
  "evpi" = "All"
)

evppi_plot_df <- rbind(evppi_df %>% mutate(parameter_number = 2),
                       select(evpi_df, wtp, baseline_setting, evpi, evpi_per_person) %>% 
                         mutate(parameter = "evpi", parameter_number = 3) %>% 
                         rename(setting = baseline_setting, evppi=evpi, 
                                evppi_per_person = evpi_per_person) %>% 
                         select(wtp, setting, evppi, parameter, parameter_number,evppi_per_person)) %>%
  filter(wtp %in% c(75, 250,500,1000)) %>%
  mutate(evppi_group = case_when(evppi_per_person == 0 ~ "0",
                                 evppi_per_person > 0 & evppi_per_person <= 0.05 ~ "<0.01 - 0.05",
                                 evppi_per_person > 0.05 & evppi_per_person <= 0.1 ~ "0.05 - 0.1",
                                 evppi_per_person > 0.1 & evppi_per_person <= 0.5 ~ "0.1 - 0.5",
                                 evppi_per_person > 0.5 & evppi_per_person <= 1 ~ "0.5 - 1.0",
                                 evppi_per_person > 1 ~ "1.0 - 1.4"),
         evppi_group = factor(evppi_group, levels = c("0", "<0.01 - 0.05", "0.05 - 0.1", "0.1 - 0.5",
                                                      "0.5 - 1.0", "1.0 - 1.4")),
         parameter = factor(parameter, levels = c("resistance", "estimate", "Q0", "phi_indoors",
                                                  "Q0_phi_indoors", "resistance_Q0_phi_indoors",
                                                  "resistance_estimate",
                                                  "estimate_Q0_phi_indoors", "evpi")),
         wtp_label = paste0("US$", as.character(wtp)),
         wtp_label = factor(wtp_label, levels = c("US$75", "US$250", "US$500", "US$1000"))) %>%
  group_by(wtp_label, parameter, parameter_number, evppi_group) %>% tally() %>%
  mutate(prop = n/36)

# Panel A (without IRS)
evppi_plot <- ggplot(filter(evppi_plot_df, parameter %in% c("resistance", "resistance_estimate",
                                                            "Q0_phi_indoors", "evpi",
                                                            "resistance_Q0_phi_indoors"))) +
  geom_col(aes(x=parameter, y = prop*100, fill = reorder(evppi_group, desc(evppi_group))), 
           position = "stack") +
  facet_wrap(~wtp_label, ncol = 1, nrow = 5, strip.position = "right") +
  labs(x= "Group of parameters", fill = "Value of information\n($US per\nperson at risk)", 
       tag = "A"
  ) +
  scale_fill_manual(values=c("0" = met.brewer("Morgenstern")[5],
                             "<0.01 - 0.05" = met.brewer("Morgenstern")[3], 
                             "0.05 - 0.1" = met.brewer("Morgenstern")[1], 
                             "0.1 - 0.5" = met.brewer("Morgenstern")[7],
                             "0.5 - 1.0" = met.brewer("Morgenstern")[8],
                             "1.0 - 1.4" = "#73463D"), 
                    guide=guide_legend(reverse=T)) +
  scale_x_discrete(labels = parameter_labels) +
  scale_y_continuous(name = "Percent of settings", 
                     sec.axis = sec_axis(~., breaks=NULL,
                                         name = "Cost-effectiveness threshold")) +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        axis.text.x=element_text(angle=45, size = 14,hjust = 1, vjust = 1),
        axis.text.y=element_text(size = 14),
        axis.title=element_text(size = 16),
        strip.text=element_text(size = 14),
        legend.title=element_text(size = 14, hjust=0.5),
        legend.text=element_text(size = 13, hjust=0.5),
        plot.tag=element_text(size = 20))


# With IRS

evppi_plot_df_irs <- rbind(evppi_df_irs %>% mutate(parameter_number=2),
                           select(evpi_df_irs, wtp, baseline_setting, evpi, evpi_per_person) %>% 
                             mutate(parameter = "evpi", parameter_number = 3) %>% 
                             rename(setting = baseline_setting, evppi=evpi, 
                                    evppi_per_person = evpi_per_person) %>% 
                             select(wtp, setting, evppi, parameter, parameter_number,evppi_per_person)) %>%
  filter(wtp %in% c(75, 250,500,1000)) %>%
  mutate(evppi_group = case_when(evppi_per_person == 0 ~ "0",
                                 evppi_per_person > 0 & evppi_per_person <= 0.05 ~ "<0.01 - 0.05",
                                 evppi_per_person > 0.05 & evppi_per_person <= 0.1 ~ "0.05 - 0.1",
                                 evppi_per_person > 0.1 & evppi_per_person <= 0.5 ~ "0.1 - 0.5",
                                 evppi_per_person > 0.5 & evppi_per_person <= 1 ~ "0.5 - 1.0",
                                 evppi_per_person > 1 ~ "1.0 - 1.4"),
         evppi_group = factor(evppi_group, levels = c("0", "<0.01 - 0.05", "0.05 - 0.1", "0.1 - 0.5",
                                                      "0.5 - 1.0", "1.0 - 1.4")),
         parameter = factor(parameter, levels = c("resistance", "estimate", "Q0", "phi_indoors",
                                                  "Q0_phi_indoors", "resistance_Q0_phi_indoors",
                                                  "resistance_estimate",
                                                  "estimate_Q0_phi_indoors", "evpi")),
         wtp_label = paste0("US$", as.character(wtp)),
         wtp_label = factor(wtp_label, levels = c("US$75", "US$250", "US$500", "US$1000"))) %>%
  group_by(wtp_label, parameter, parameter_number, evppi_group) %>% tally() %>%
  mutate(prop = n/36)

evppi_plot_irs <- ggplot(filter(evppi_plot_df_irs, parameter %in% c("resistance", "resistance_estimate",
                                                            "Q0_phi_indoors", "evpi",
                                                            "resistance_Q0_phi_indoors"))) +
  geom_col(aes(x=parameter, y = prop*100, fill = reorder(evppi_group, desc(evppi_group))), 
           position = "stack") +
  facet_wrap(~wtp_label, ncol = 1, nrow = 5, strip.position = "right") +
  labs(x= "Group of parameters", fill = "Value of information\n($US per\nperson at risk)", 
       tag = "B"
  ) +
  scale_fill_manual(values=c("0" = met.brewer("Morgenstern")[5],
                             "<0.01 - 0.05" = met.brewer("Morgenstern")[3], 
                             "0.05 - 0.1" = met.brewer("Morgenstern")[1], 
                             "0.1 - 0.5" = met.brewer("Morgenstern")[7],
                             "0.5 - 1.0" = met.brewer("Morgenstern")[8],
                             "1.0 - 1.4" = "#73463D"), 
                    guide=guide_legend(reverse=T)) +
  scale_x_discrete(labels = parameter_labels) +
  scale_y_continuous(name = "Percent of settings", 
                     sec.axis = sec_axis(~., breaks=NULL,
                                         name = "Cost-effectiveness threshold")) +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        axis.text.x=element_text(angle=45, size = 14,hjust = 1, vjust = 1),
        axis.text.y=element_text(size = 14),
        axis.title=element_text(size = 16),
        strip.text=element_text(size = 14),
        legend.title=element_text(size = 14, hjust=0.5),
        legend.text=element_text(size = 13, hjust=0.5),
        plot.tag=element_text(size = 20))

fig3 <- ggarrange(evppi_plot, evppi_plot_irs, nrow = 1, ncol = 2, common.legend= TRUE, legend = "right")
#ggsave("plots/fig3.png", fig3, width = 30, height = 15, units = "cm")
