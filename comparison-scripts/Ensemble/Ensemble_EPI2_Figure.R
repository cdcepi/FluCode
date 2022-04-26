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

scenarios = c("HP13_", "HP14_", "HP15_", "HP16_", "HP17_", "HP18_", "HP19_", "HP20_", "HP21_", "HP22_", "HP23_", "HP24_")
ens.levels <- c("ENS", "UVA", "UTA", "NEU3", "IMP", "COL2", "COL")

# Figures -------------------------------------------------------------

dat <- read.csv(paste0(path, "MeanBased-ensemble/PAN2_symillness_ensemble.csv"))
dat$Date <- as.Date(dat$Date)
dat$team <- factor(dat$team, ens.levels)

ens <- dat %>% 
  filter(team == "ENS") %>%
  select(Date, paste0(scenarios,"mean")) %>%
  pivot_longer(cols = paste0(scenarios,"mean"), names_to = "scenario", values_to = "sm.mean") %>%
  mutate(scenario = gsub("_mean", "", scenario))

png("supp_fig4b.png",width = 850, height=500)
ggplot(data = ens, aes(x = Date, group=scenario)) +
  geom_line(aes(y = sm.mean, color=scenario),lwd = 1.5) +
  ylab("% Incident Symptomatic Illness") +
  scale_fill_viridis(discrete=TRUE)+
  scale_color_viridis(discrete=TRUE)+
  ggtitle("")+
  theme_light()+
  scale_x_date(labels = date_format("%b"))
dev.off()

