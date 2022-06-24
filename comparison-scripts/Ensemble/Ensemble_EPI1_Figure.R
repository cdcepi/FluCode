library(ggplot2)
library(dplyr)
library(viridis)
library(khroma)
library(ggsci)
library(ggpubr)
library(reshape)
library(gridExtra)
library(egg)
library(plyr)
library(tidyr)
library(scales)
library(lubridate)
library(data.table)
library(magrittr)
library(grid)
library(doBy)

#Environment -------------------------------------------------------------------------------------

setwd("C:/Users/ppf6/Desktop/GitHub/FluCode/comparison-scripts")
path <- "../model-forecasts/"

ens.levels <- c("ENS", "UVA", "UTA", "NEU3", "IMP", "COL2", "COL")
scenarios = c("HP01_", "HP02_", "HP03_", "HP04_", "HP05_", "HP06_", "HP07_", "HP08_", "HP09_", "HP10_", "HP11_", "HP12_")

# Figures -------------------------------------------------------------

dat <- read.csv(paste0(path, "MeanBased-ensemble/PAN1_symillness_ensemble.csv"))
dat$Date <- as.Date(dat$Date)
dat$team <- factor(dat$team, ens.levels)

ens <- dat %>% 
  filter(team == "ENS") %>%
  select(Date, paste0(scenarios,"mean")) %>%
  pivot_longer(cols = paste0(scenarios,"mean"), names_to = "scenario", values_to = "sm.mean") %>%
  mutate(scenario = gsub("_mean", "", scenario))

png("supp_fig4a.png",width = 1100, height=750, res= 220)
ggplot(data = ens, aes(x = Date, group=scenario)) +
  geom_line(aes(y = sm.mean, color=scenario),lwd = 1.5) +
  ylab("% Incident Symptomatic Illness") +
  scale_fill_viridis(discrete=TRUE)+
  scale_color_viridis(discrete=TRUE)+
  ggtitle("a.")+
  theme_light()+
  scale_x_date(labels = date_format("%b"))
dev.off()



