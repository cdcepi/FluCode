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

setwd("C:/Users/ppf6/Desktop/GitHub/FluCode/comparison-scripts")
path <- "../model-forecasts/"
ens.levels <- c("ENS", "UVA", "UTA", "NEU", "IMP", "COL2", "COL")

#Figure 7 - age-specific AR---------------------------------------

s <- read.csv("Visualization/SympIllnesses_EPI1_HP01.csv")
d <- read.csv("Visualization/Deaths_EPI1_HP01.csv")

s <- s %>% group_by("agegroup")
s$agegroup <- factor(x = s$agegroup, levels = c("Overall", "0-4 years", "5-17 years", "18-49 years", "50-64 years", "65+ years"))

s <- s[order(s$team),]
s <- s[3:7]

ens <- s %>% dplyr::group_by(agegroup) %>% dplyr::summarise(cml = mean(cml),
                                                            lower = mean(lower),
                                                            upper = mean(upper))
e <- data.frame(team="ENS",
                agegroup=ens$agegroup,
                cml = ens$cml,
                lower = ens$lower,
                upper = ens$upper)
s <- rbind(s, e)
s$team <- factor(x = s$team, levels = ens.levels)

s$cml <- s$cml * 100
s$lower <- s$lower * 100
s$upper <- s$upper * 100

pal <- pal_nejm()
p1 <- ggplot(s, aes(x = team, y = cml, color=team)) +
  facet_wrap( ~ agegroup, scales = "free_y", nrow=1)+
  expand_limits(y = 0)+
  geom_point( position=position_dodge2(0.9)) +
  geom_errorbar(aes(x = team, ymax = upper, ymin = lower), 
                position=position_dodge2(0.9))+
  scale_color_manual(values = c("black", pal(6)))+
  xlab("")+
  ylab("Percent Symptomatic")+ theme(panel.grid.major.x = element_blank())+
  # ggtitle("a. Age-specific symptomatic percentages in EPI1 no intervention scenario (HP01)")+
  ggtitle("a.")+
  theme(
    axis.text.x = element_blank(),
    # axis.text.y = element_text(size=13),
    axis.ticks.x = element_blank())#,
    # strip.text = element_text(size = 20))


d <- d %>% group_by("agegroup")
d$agegroup <- factor(x = d$agegroup, levels = c("Overall", "0-4 years", "5-17 years", "18-49 years", "50-64 years", "65+ years"))

d <- d[order(d$team),]
d <- d[3:7]

ens <- d %>%  dplyr::group_by(agegroup) %>% dplyr::summarise(cml = mean(cml),
                                                             lower = mean(lower),
                                                             upper = mean(upper))
e <- data.frame(team="ENS",
                agegroup=ens$agegroup,
                cml = ens$cml,
                lower = ens$lower,
                upper = ens$upper)
d <- rbind(d, e)
d$team <- factor(x = d$team, levels = ens.levels)

pal <- pal_nejm()
p2 <- ggplot(d, aes(x = team, y = cml, color=team)) + 
  facet_wrap( ~ agegroup, scales = "free_y", nrow=1)+
  expand_limits(y = 0)+
  geom_point(position=position_dodge2(0.9)) +
  geom_errorbar(aes(x = team, ymax = upper, ymin = lower),
                position=position_dodge2(0.9))+
  scale_color_manual(values = c("black", pal(6)))+
  xlab("")+
  ylab("Death Rate per 100,000")+ theme(panel.grid.major.x = element_blank())+
  # ggtitle("b. Age-specific death rates per 100,000 in EPI1 no intervention scenario (HP01)")+
  ggtitle("b.")+
  theme(
    axis.text.x = element_blank(),
    # axis.text.y = element_text(size=13),
    axis.ticks.x = element_blank())#,
    # strip.text = element_text(size = 20))


png("fig2_EPI1.png",res=220, 1600, 1000)
ggarrange(p1, p2, nrow=2, ncol=1)
dev.off()


#Figure 6 - unmitigated epi curve---------------------------------------

long_ens <- read.csv(paste0(path, "MeanBased-Ensemble/PAN1_symillness_ensemble.csv"))
long_ens <- long_ens %>% mutate(team = as.character(team)) %>% 
  mutate(team = case_when(team == "NEU3" ~ "NEU",
                          team != "NEU3" ~ team))
long_ens$Date <- as.Date(long_ens$Date)
long_ens$team <- factor(long_ens$team, ens.levels)
pal_black <- c("black",pal(6))

png("fig1_EPI1.png",res=220, 1600, 1000)
ggplot(data = long_ens, aes(x = Date, y = HP01_mean, group = team)) +
  geom_line(aes(color = team), lwd = 1.5) +
  geom_ribbon(aes(ymin = HP01_perc2p5, ymax = HP01_perc97p5, fill = team), alpha = 0.15,
              show.legend = FALSE)+
  ylab("% Incident Symptomatic Illness") +
  scale_color_manual(values = pal_black)+
  scale_fill_manual(values = pal_black)+
  ggtitle("a.")+
  theme_light()+
  scale_x_date(labels = date_format("%b"))

dev.off()

#Figure 7 --------------------------------------------------------------

age_ill <- read.csv(paste0(path, "MeanBased-Ensemble/PAN1_symillness_base_age.csv"))
age_ill$Agegroup <- gsub("yr", "years", age_ill$Agegroup)
age_ill$team <- factor(x = age_ill$team, levels = ens.levels[-1])
age_ill$Agegroup <- factor(x = age_ill$Agegroup, levels = c("0-4 years", "5-17 years", "18-49 years", "50-64 years", "65+ years"))

age_dead <- read.csv(paste0(path, "MeanBased-Ensemble/PAN1_death_base_age.csv"))
age_dead$Agegroup <- gsub("yr", "years", age_dead$Agegroup)
age_dead$team <- factor(x = age_dead$team, levels = ens.levels[-1])
age_dead$Agegroup <- factor(x = age_dead$Agegroup, levels = c("0-4 years", "5-17 years", "18-49 years", "50-64 years", "65+ years"))

fig7a <- ggplot(age_ill, aes(fill=Agegroup, y=Cml, x=team)) + 
  geom_bar(position="fill", stat="identity") +
  ylab("Percent")+
  ggtitle("a.")+
  scale_fill_d3(name = "Age Group")

fig7b <- ggplot(age_dead, aes(fill=Agegroup, y=Cml, x=team)) + 
  geom_bar(position="fill", stat="identity") +
  ylab("Percent")+
  ggtitle("b.")+
  scale_fill_d3(name = "Age Group")

png("fig7_EPI1.png",width = 900, height=1000, res = 220)
ggarrange(fig7a, fig7b, nrow=2, ncol=1)
dev.off()

#Figure 10 - rank order figures---------------------------------------

plot_data <- read.csv(paste0(path, "MeanBased-Ensemble/PAN1_symillness_metrics.csv")) %>%
  mutate(Team = ifelse(Team == "NEU3", "NEU", as.character(Team)))
plot_data$Team <- factor(plot_data$Team, ens.levels)

p6 <- plot_data %>% filter(Output == "Averted Number") %>% mutate(x = 1)%>% dplyr::rename(Averted = Reduction) %>%
  ggplot(., aes( x = x, y = Averted, fill = Rank)) +
  geom_col() +
  facet_grid( Team ~  Scenario, switch = "y") +
  coord_flip()+
  scale_fill_gradientn(colors = wes_palette("Zissou1", 5)[c(1,3,5)],
                       breaks = c(1:11),
                       labels = c(1:11),
                       guide = "legend") +
  # geom_text(aes(label=paste(format(round(Averted / 1e6, 1), trim = TRUE))), 
  #           size=5, hjust=1.0) +
  geom_hline(yintercept = 0, color = "black")+
  theme_minimal() + 
  labs(y = 'Illnesses averted (millions)', x = '', fill = 'Rank') +
  theme(#axis.title = element_text(size=12), strip.text = element_text(size=10),
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(hjust = .5),
        axis.text.y = element_blank(), #element_text(size=10),
        strip.text.y.left = element_text(angle = 360),
        axis.line.x = element_line(colour = "black"),
        legend.position = 'bottom'
        )+
  scale_y_continuous(labels = scales::label_number(scale = 1 / 1e6))+
  ggtitle("a.")

p3 <- plot_data %>% filter(Output == "Averted Percent") %>% ggplot(.) +
  geom_raster(aes(y = Team, x = Scenario, fill = Rank)) +
  scale_fill_gradientn(colors = wes_palette("Zissou1", 5)[c(1,3,5)],
                       breaks = c(1:11),
                       labels = c(1:11),
                       guide = "legend") +  
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 0), "%"))) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Rank') +
  theme(#axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(hjust = .5),
        #axis.text.y = element_text(size=10), 
        legend.position = 'bottom'
        )+
  ggtitle("b.")+ 
  scale_y_discrete(limits = rev(levels(plot_data$Team)))

p4 <- plot_data %>% filter(Output == "Magnitude Reduction") %>% ggplot(.) +
  geom_raster(aes(y = Team, x = Scenario, fill = Rank)) +
  scale_fill_gradientn(colors = wes_palette("Zissou1", 5)[c(1,3,5)],
                       breaks = c(1:11),
                       labels = c(1:11),
                       guide = "legend") +
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 0), "%"))) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Rank') +
  theme(#axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text( hjust = .5),
        legend.position = 'bottom'
        )+
  ggtitle("a.")+ 
  scale_y_discrete(limits = rev(levels(plot_data$Team)))


p5 <- plot_data %>% filter(Output == "Peak Delay") %>% dplyr::rename(Delay = Reduction)%>% ggplot(.) +
  geom_raster(aes(y = Team, x = Scenario, fill = Delay)) +
  scale_fill_gradientn(colors = wes_palette("Zissou1", 5)[c(5,3,1)]) +
  geom_text(aes(y = Team, x = Scenario, label = Delay)) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Delay, Weeks') +
  theme(#axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text( hjust = .5),
        legend.position = 'bottom',
        legend.key.width = unit(1.5, "cm")
        )+
  ggtitle("b.")+ 
  scale_y_discrete(limits = rev(levels(plot_data$Team))[-7])
 

png("fig3_EPI1.png",res=220, 2500, 1600)
ggarrange(p6, p3, nrow=2, ncol=1)#,common.legend = TRUE, legend="bottom")
dev.off()

png("fig3_cd_EPI1.png",res=220, 2500, 1600)
ggarrange(p4, p5, nrow=2, ncol=1)#,common.legend = TRUE, legend="bottom")
dev.off()

# Supp figure hospitalizations - rank order figures ----------------------------

plot_data <- read.csv(paste0(path, "MeanBased-Ensemble/PAN1_hospitalization_metrics.csv")) %>%
  mutate(Team = ifelse(Team == "NEU3", "NEU", as.character(Team)))
plot_data$Team <- factor(plot_data$Team, ens.levels)

h6 <- plot_data %>% filter(Output == "Averted Number") %>% mutate(x = 1)%>% dplyr::rename(Averted = Reduction) %>%
  ggplot(., aes( x = x, y = Averted, fill = Rank)) +
  geom_col() +
  facet_grid( Team ~  Scenario, switch = "y") +
  coord_flip()+
  scale_fill_gradientn(colors = wes_palette("Zissou1", 5)[c(1,3,5)],
                       breaks = c(1:11),
                       labels = c(1:11),
                       guide = "legend") +
  geom_hline(yintercept = 0, color = "black")+
  theme_minimal() + 
  labs(y = 'Hospitalizations averted (millions)', x = '', fill = 'Rank') +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(hjust = .5),
        axis.text.y = element_blank(),
        strip.text.y.left = element_text(angle = 360),
        axis.line.x = element_line(colour = "black"),
        legend.position = 'bottom'
        )+
  scale_y_continuous(labels = scales::label_number(scale = 1 / 1e6))+
  ggtitle("a.")

h3 <-  plot_data %>% filter(Output == "Averted Percent") %>% ggplot(.) +
  geom_raster(aes(y = Team, x = Scenario, fill = Rank)) +
  scale_fill_gradientn(colors = wes_palette("Zissou1", 5)[c(1,3,5)],
                       breaks = c(1:11),
                       labels = c(1:11),
                       guide = "legend") +
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 0), "%"))) +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Rank') +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(hjust = .5),
        legend.position = 'bottom'
        )+
  ggtitle("b.")+ 
  scale_y_discrete(limits = rev(levels(plot_data$Team)))

h4 <- plot_data %>% filter(Output == "Magnitude Reduction") %>% ggplot(.) +
  geom_raster(aes(y = Team, x = Scenario, fill = Rank)) +
  scale_fill_gradientn(colors = wes_palette("Zissou1", 5)[c(1,3,5)],
                       breaks = c(1:11),
                       labels = c(1:11),
                       guide = "legend") +
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 0), "%"))) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Rank') +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(hjust = .5),
        legend.position = 'bottom'
        )+
  ggtitle("c.")+ 
  scale_y_discrete(limits = rev(levels(plot_data$Team)))

h5 <- plot_data %>% filter(Output == "Peak Delay") %>% dplyr::rename(Delay = Reduction)%>% ggplot(.) +
  geom_raster(aes(y = Team, x = Scenario, fill = Delay)) +
  scale_fill_gradientn(colors = wes_palette("Zissou1", 5)[c(5,3,1)]) +
  geom_text(aes(y = Team, x = Scenario, label = Delay)) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Delay, Weeks') +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(hjust = .5),
        legend.position = 'bottom',
        legend.key.width = unit(1.5, "cm")
        )+
  ggtitle("d.")+ 
  scale_y_discrete(limits = rev(levels(plot_data$Team))[-7])


png("fig3h_EPI1.png",width = 2500, height=3200, res = 220)
ggarrange(h6, h3, h4, h5, nrow=4, ncol=1)
dev.off()

# Supp figure deaths - rank order figures ----------------------------

scaleFUNd <- function(x) sprintf("%.0f", x / 1e6)

plot_data <- read.csv(paste0(path, "MeanBased-Ensemble/PAN1_death_metrics.csv")) %>%
  mutate(Team = ifelse(Team == "NEU3", "NEU", as.character(Team)))
plot_data$Team <- factor(plot_data$Team, ens.levels)

d6 <- plot_data %>% filter(Output == "Averted Number") %>% mutate(x = 1)%>% dplyr::rename(Averted = Reduction) %>%
  ggplot(., aes( x = x, y = Averted, fill = Rank)) +
  geom_col() +
  facet_grid( Team ~  Scenario, switch = "y") +
  coord_flip()+
  scale_fill_gradientn(colors = wes_palette("Zissou1", 5)[c(1,3,5)],
                       breaks = c(1:11),
                       labels = c(1:11),
                       guide = "legend") +
  geom_hline(yintercept = 0, color = "black")+
  theme_minimal() + 
  labs(y = 'Deaths averted (millions)', x = '', fill = 'Rank') +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(hjust = .5),
        axis.text.y = element_blank(), 
        strip.text.y.left = element_text(angle = 360),
        axis.line.x = element_line(colour = "black"),
        legend.position = 'bottom'
        )+
  scale_y_continuous(labels = scaleFUNd)+
  ggtitle("a.")


d3 <- plot_data %>% filter(Output == "Averted Percent") %>% ggplot(.) +
  geom_raster(aes(y = Team, x = Scenario, fill = Rank)) +
  scale_fill_gradientn(colors = wes_palette("Zissou1", 5)[c(1,3,5)],
                       breaks = c(1:11),
                       labels = c(1:11),
                       guide = "legend") +
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 0), "%"))) +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Rank') +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(hjust = .5),
        legend.position = 'bottom'
        )+
  ggtitle("b.")+ 
  scale_y_discrete(limits = rev(levels(plot_data$Team)))

d4 <- plot_data %>% filter(Output == "Magnitude Reduction") %>% ggplot(.) +
  geom_raster(aes(y = Team, x = Scenario, fill = Rank)) +
  scale_fill_gradientn(colors = wes_palette("Zissou1", 5)[c(1,3,5)],
                       breaks = c(1:11),
                       labels = c(1:11),
                       guide = "legend") +
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 0), "%"))) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Rank') +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(hjust = .5),
        legend.position = 'bottom'
        )+
  ggtitle("c.")+ 
  scale_y_discrete(limits = rev(levels(plot_data$Team)))

d5 <- plot_data %>% filter(Output == "Peak Delay") %>% dplyr::rename(Delay = Reduction)%>% ggplot(.) +
  geom_raster(aes(y = Team, x = Scenario, fill = Delay)) +
  scale_fill_gradientn(colors = wes_palette("Zissou1", 5)[c(5,3,1)]) +
  geom_text(aes(y = Team, x = Scenario, label = Delay)) +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Delay, Weeks') +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(hjust = .5),
        legend.position = 'bottom',
        legend.key.width = unit(1.5, "cm")
        )+
  ggtitle("d.")+ 
  scale_y_discrete(limits = rev(levels(plot_data$Team))[-7])


png("fig3d_EPI1.png",width = 2500, height=3200, res = 220)
ggarrange(d6, d3, d4, d5, nrow=4, ncol=1)
dev.off()
