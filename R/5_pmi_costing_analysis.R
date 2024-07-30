# Costing analysis of PMI budget for entomological monitoring and ITN durability monitoring
# 2021-2023

library(here)
library(tidyverse)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(patchwork)

# Load extracted costing data
data <- read.csv("./data/pmi_costing_analysis.csv")

# Load population at risk data
library(foresite)
extract_par_2023 <- function(iso) {
  
  sum(iso$population$par_pf[iso$population$year==2023])
  
}

pars <- 0
for (i in 1:length(data$iso)) {
  pars[i] <- extract_par_2023(iso = eval(parse(text=data$iso[i])))
}

data$par <- pars

data <- data %>%
  mutate(prop_total_budget = entomological_monitoring_budget/total_malaria_budget,
         funding_per_par = entomological_monitoring_budget/par)

quantile(data$prop_total_budget)
quantile(data$funding_per_par)

# MAP OF FUNDING PER POPULATION AT RISK ----------------------------------------

afr_countries <- c("Angola","Burundi", "Benin", "Burkina Faso", "Botswana",                            
                   "Central African Republic","Ivory Coast", "Cameroon",                           
                   "Democratic Republic of the Congo", "Republic of Congo",                  
                   "Comoros", "Cape Verde","Djibouti","Algeria", "Egypt",                              
                   "Eritrea","Ethiopia","Reunion", "Mayotte", "Gabon",                              
                   "Ghana", "Guinea", "Gambia", "Guinea-Bissau","Equatorial Guinea",                  
                   "Kenya","Liberia","Libya",  "Lesotho", "Morocco",  "Madagascar",                          
                   "Mali", "Mozambique", "Mauritania", "Mauritius",                           
                   "Malawi", "Namibia", "Niger", "Nigeria", "Rwanda",                              
                   "Western Sahara","Sudan","South Sudan","Senegal", "Sierra Leone",                         
                   "Somalia", "Sao Tome and Principe","Eswatini", "Seychelles",                                    
                   "Chad", "Togo","Tunisia","Tanzania", "Uganda", "South Africa" ,                      
                   "Zambia", "Zimbabwe")

africa <- map_data("world", region = afr_countries)
unique(data$country)
africa[africa$region=="Democratic Republic of the Congo",]$region <- "Democratic Republic of Congo"
africa[africa$region=="Ivory Coast",]$region <- "Cote d'Ivoire"

# Make funding categories based on EVPI at $250 threshold
quantile(data$funding_per_par)
data <- data %>%
  mutate(funding_per_par_cat = case_when(funding_per_par <0.05 ~ "0.02-0.05",
                                         funding_per_par >= 0.05 & 
                                           funding_per_par < 0.08 ~ "0.05-0.08",
                                         funding_per_par >= 0.08 & 
                                           funding_per_par < 0.24 ~ "0.08-0.24",
                                         funding_per_par >=0.24 ~ "0.24-0.59")) %>%
  mutate(funding_per_par_cat = case_when(funding_per_par <0.05 ~ "0.02-0.05",
                                         funding_per_par >= 0.05 & 
                                           funding_per_par < 0.07 ~ "0.05-0.07",
                                         funding_per_par >= 0.07 & 
                                           funding_per_par < 0.13 ~ "0.07-0.13",
                                         funding_per_par >=0.13 ~ "0.13-0.59"))

africa <- left_join(africa, data, by = c("region" = "country")) 
africa$funding_per_par_cat[is.na(africa$funding_per_par_cat)] <- "No funding information"
africa$funding_per_par_cat <- factor(africa$funding_per_par_cat,
                                     levels =c("0.02-0.05", "0.05-0.07",
                                               "0.07-0.13", "0.13-0.59", "No funding information"))


afr_map <- ggplot(africa) + 
  geom_polygon(aes(x = long, y = lat, group = group, 
                   fill = funding_per_par_cat), color = "black") + 
  scale_fill_manual(values = c(brewer.pal(4, "YlOrRd"), "white")) +
  labs(fill = "Entomological surveillance\nfunding (US$ per person at risk)", tag = "A") +
  ylim(-35, max(africa$lat)) +
  coord_fixed(1.1)+
  theme(axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        plot.tag = element_text(size = 20),
        plot.margin = unit(c(0,0,0,0), "mm"))

## Compare EVPI with PMI funding -----------------------------------------------

evpi_df <- readRDS(here("output_files/evpi_over_wtp.RDS")) %>%
  mutate(evpi = ev_certainty - max_mean_nmb) %>%
  mutate(evpi_per_person = evpi/200000,
         prop_of_expected_nmb = evpi/max_mean_nmb)

evpi_summary <- filter(evpi_df, wtp %in% c(75, 250, 500, 1000)) 

# Distribution of funding per person at risk
round(quantile(data$funding_per_par),2)

pmi1 <- ggplot(evpi_summary) +
  geom_hline(yintercept=0, lty = "dashed", col = "grey70") +
  geom_boxplot(aes(x=as.factor(wtp), y=evpi_per_person), fill = "#edd7d9", col = "#b1615c", width = 0.5) +
  geom_hline(yintercept=0, lty = "dashed", col = "grey70") +
  scale_x_discrete(limits=rev) +
  labs(x="Cost-effectiveness\nthreshold ($US)", y = "Modelled value of information\n(US$ per person at risk)") +
  ylim(0,4) +
  coord_flip() +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        plot.tag = element_text(size = 20))

pmi2 <- ggplot(data) +
  geom_boxplot(aes(x="PMI", y=funding_per_par), fill = "#c9c9dd", col = "#5a5a83", width = 0.5) +
  geom_hline(yintercept=0, lty = "dashed", col = "grey70") +
  scale_x_discrete(limits=rev) +
  labs(x="", y = "Entomological surveillance funding\n(US$ per person at risk)", tag = "B") +
  coord_flip() +
  ylim(0,4) +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        plot.tag = element_text(size = 20))

afr_map + ((pmi2 / pmi1) + plot_layout(heights = c(1,3))) + plot_layout(ncol = 2)
#ggsave("plots/fig4.png", width = 30, height = 15, units = "cm")

