library(ggplot2)
library(plyr)
library(dplyr)
library(viridis)
library(khroma)
library(ggsci)
library(ggpubr)
library(reshape)
library(gridExtra)
library(egg)
library(tidyr)
library(scales)
library(lubridate)
library(data.table)
library(magrittr)
library(grid)
library(doBy)

setwd("C:/Users/ppf6/Desktop/GitHub/FluCode/comparison-scripts")

#Figure 2 - age-stratified ---------------------------------------


s <- read.csv("SympIllnesses_2009_RL01.csv")
d <- read.csv("Deaths_2009_RL01.csv")

s <- s %>% group_by("agegroup")
s <- s[which(!(s$team %in% c("NEU", "NEU2"))),]
s$agegroup <- factor(x = s$agegroup, levels = c("0-4 years", "5-17 years", "18-49 years", "50-64 years", "65+ years"))

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
s$team <- factor(x = s$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))

pal <- pal_nejm()
p1 <- ggplot(s, aes(x = team, y = cml, color=team)) +
  facet_wrap( ~ agegroup, scales = "free_y", nrow=1)+
  geom_point(size = 3.5, position=position_dodge2(0.9)) +
  geom_errorbar(aes(x = team, ymax = upper, ymin = lower),
                position=position_dodge2(0.9), size=1)+
  scale_color_manual(values = c(rev(pal(6)), "black"))+
  xlab("")+
  ylab("Percent Symptomatic")+ theme(panel.grid.major.x = element_blank())+
  ggtitle("a. Age-specific symptomatic percentages in 2009 no intervention scenario (RL01)")+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())


d <- d %>% group_by("agegroup")
d <- d[which(!(d$team %in% c("NEU", "NEU2"))),]
d$agegroup <- factor(x = d$agegroup, levels = c("0-4 years", "5-17 years", "18-49 years", "50-64 years", "65+ years"))

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
d$team <- factor(x = d$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))

pal <- pal_nejm()
p2 <- ggplot(d, aes(x = team, y = cml, color=team)) + 
  facet_wrap( ~ agegroup, scales = "free_y", nrow=1)+
  geom_point(size = 3.5, position=position_dodge2(0.9)) +
  geom_errorbar(aes(x = team, ymax = upper, ymin = lower), size=1,
                position=position_dodge2(0.9))+
  scale_color_manual(values = c(rev(pal(6)), "black"))+
  xlab("")+
  ylab("Death Rate per 100,000")+ theme(panel.grid.major.x = element_blank())+
  ggtitle("b. Age-specific death rates per 100,000 in 2009 no intervention scenario (RL01)")+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())


png("fig2_2009.png",width = 950, height=800)
ggarrange(p1, p2, nrow=2, ncol=1)
dev.off()


#Figure 1 - unmitigated epi curve---------------------------------------

path <- "../model-forecasts/"

team.folder <- c("Columbia/COL/RL/", "Northeastern/RL3/",  "UTAustin/", "UVirginia/", "Imperial/", "Columbia/COL2/RL/")
team.abbrev <- c("COL", "NEU", "UTA", "UVA", "IMP", "COL2")
file.types <-c("SymIllness", "Hosp", "Deaths", "AntiviralTX")
scn = "RL01_"
read_overall_mean <- function(scn, outcome){
  
  cols <- c("sm.mean", "sm.perc2p5", "sm.perc97p5")
  
  s <- read.csv(paste0(path,team.folder[1],scn,file.types[outcome],"_",team.abbrev[1],".csv"))
  symillness <- data.frame(c(s[which(s$Agegroup == "Overall" &
                                       s$Bin %in% cols),],
                             `team` = team.abbrev[1]))
  
  for(i in 2:6){
    
    if(i == 4){
      scn = gsub("RL", "RL_", scn)
    }
    if(i == 5){
      scn = gsub("_", "", scn)
    }
    if(i == 6){
      scn = paste0(scn, "_")
    }
    
    f =  paste0(path,team.folder[i],scn,file.types[outcome],"_",team.abbrev[i],".csv")
    if(i == 5){
      f =  paste0(path,team.folder[i],scn,file.types[outcome],team.abbrev[i],".csv")
      if(outcome == 3){
        f =  paste0(path,team.folder[i],scn,"Death",team.abbrev[i],".csv")
      }
    }
    
    if(file.exists(f)){
      s <- read.csv(f)
      if(ncol(s) > 58){
        s <- s[-1]
      }
      if(i == 2){
        team.abbrev[i] = "NEU3"
      }
      symillness <- rbind.fill(symillness, 
                               data.frame(c(s[which(s$Agegroup == "Overall" & 
                                                      s$Bin %in% cols),],`team` = team.abbrev[i])))
    }
    
  }
  symillness$Peak.Magnitude[which(is.na(symillness$Peak.Magnitude))] <-
    symillness$PeakMagnitude[which(is.na(symillness$Peak.Magnitude))]
  
  weeks <- paste0("Week",1:52)
  symillness$Cml[which(is.na(symillness$Cml))] <- symillness$cml[which(is.na(symillness$Cml))]
  cml <<- symillness[c("Cml", "team", "Bin", "Peak.Magnitude")]
  
  long <- gather(symillness, week, illnesses, weeks, factor_key=TRUE)
  long$week <- as.integer(gsub("Week", "", long$week))
  
  cols <- c("Bin", "team", "week", "illnesses")
  long <- long[cols]
  long_wide <- spread(long, Bin, illnesses)
  
  return(long_wide)
}

get_peaks <- function(dat){
  teams <- unique(dat$team)
  peak.weeks = c()
  for(g in teams){
    sub <- dat[which(dat$team == g),]
    peak.weeks <- c(peak.weeks, sub$week[which.max(sub$sm.mean)])
  }
  pw <- data.frame(teams = teams,
                   peakWeek = peak.weeks)
  return(pw)
}

long_RL01 <- read_overall_mean(scn = "RL01_", 1)
long_RL01$team <- factor(x = long_RL01$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA"))

long_RL01 <- long_RL01[order(long_RL01$team),]
long_RL01$Date <- as.Date("2020-06-24") + (long_RL01$week*7)

long <- long_RL01

wide <- long[-6] %>% 
  gather(ens, ill, -(team:week)) %>%
  spread(key=team, value=ill)

ens <- c()

for(i in 1:nrow(wide)){
  ens <- c(ens, mean(as.double(wide[i,3:8])))
}

wide$ill <- ens

wide$Date <- as.Date("2020-06-24") + (wide$week*7)
wide$ens <- factor(x = wide$ens, levels = c("sm.mean", "sm.median","sm.perc2p5", "sm.perc97p5"))

ensemble <- data.frame(team="ENS",
                       week = 1:52,
                       sm.mean = wide$ill[which(wide$ens == "sm.mean")],
                       sm.perc2p5 = wide$ill[which(wide$ens == "sm.perc2p5")],
                       sm.perc97p5 = wide$ill[which(wide$ens == "sm.perc97p5")],
                       Date = unique(wide$Date))

long_ens <- rbind(long_RL01, ensemble)

pal_black <- c(rev(pal(6)), "black")

long_ens$team
png("fig1_2009.png",width = 850, height=500)
ggplot(data = long_ens, aes(x = Date, y = sm.mean, group = team)) +
  geom_line(aes(color = team), lwd = 1.5) +
  geom_ribbon(aes(ymin = sm.perc2p5, ymax = sm.perc97p5, fill = team), 
              alpha = 0.15,
              show.legend = FALSE)+
  ylab("% Incident Symptomatic Illness") +
  scale_color_manual(values = pal_black)+
  scale_fill_manual(values = pal_black)+
  ggtitle("2009, No Intervention: Symptomatic Illness Epidemic Curves")+
  theme_light()+
  scale_x_date(labels = date_format("%b"))

dev.off()

#Figure 3 - rank order figures---------------------------------------

cml <- cml[which(cml$Bin == "sm.mean"),]
total_cml <- cml[-3:-4]

cml_sum <- cml$Cml

read_overall_mean(scn = "RL02_", 1)
total_cml$Cml2 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum <- data.frame(RL01 = cml_sum,
                      RL02 = cml$Cml[which(cml$Bin == "sm.mean")])

read_overall_mean(scn = "RL03_", 1)
total_cml$Cml3 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL03 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "RL04_", 1)
total_cml$Cml4 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL04 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "RL05_", 1)
total_cml$Cml5 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL05 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "RL06_", 1)
total_cml$Cml6 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL06 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "RL07_", 1)
total_cml$Cml7 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL07 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "RL08_", 1)
total_cml$Cml8 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL08 <- cml$Cml[which(cml$Bin == "sm.mean")]

total_cml$team <- factor(x = total_cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))

cml_sum <- rbind(cml_sum, colMeans(cml_sum))
total_cml <- rbind(total_cml,
                   c(as.numeric(cml_sum[7,1]),
                     "ENS", 
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL02[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL03[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL04[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL05[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL06[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL07[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL08[7]) / cml_sum$RL01[7])
                     )
                   )

rl01 <- total_cml[1:2]
rl01$Cml <- as.numeric(rl01$Cml)
total_cml <- total_cml[-1]
total_cml <-  cbind(total_cml[1], lapply(total_cml[-1], as.numeric))

averted <- cbind(total_cml[1], total_cml[-1] * (rl01$Cml/100) * 306.8e6)
colnames(averted) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
a <- melt(averted, id.vars = c("team"), variable.name = "Averted")
names(a) <- c("Team", 'Scenario', "Averted")

rank <- as.data.frame(cbind(averted$team,
                            rbind(rank(-averted[1,2:8]),
                                  rank(-averted[2,2:8]),
                                  rank(-averted[3,2:8]),
                                  rank(-averted[4,2:8]),
                                  rank(-averted[5,2:8]),
                                  rank(-averted[6,2:8]),
                                  rank(-averted[7,2:8]))
                            )
                      )
names(rank)[1] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Averted")
a$Rank <- r$value

# a$scn <- ifelse(a$Scenario == "RL01", "Base",
#                 ifelse(a$Scenario == "RL02", "VX", 
#                        ifelse(a$Scenario == "RL03", "AV",
#                               ifelse(a$Scenario == "RL04", "VX; AV",
#                                      ifelse(a$Scenario == "RL05", "SC trigger",
#                                             ifelse(a$Scenario == "RL06", "SC fall",
#                                                    ifelse(a$Scenario == "RL07", 
#                                                           "VX; AV; SC trigger",
#                                                           "VX; AV; SC fall")))))))
# a$Scenario <- factor(a$scn, levels = unique(a$scn))
p6 <- ggplot(a) +
  geom_raster(aes(y = Team, x = Scenario, fill = Averted)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 15e6, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label=scales::comma(Averted)), size=3) +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Averted') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom',
        legend.key.width = unit(1,"cm"))+
  ggtitle("a. Averted illnesses")

rank <- as.data.frame(cbind(total_cml$team,
                            rbind(rank(-total_cml[1,2:8]),
                            rank(-total_cml[2,2:8]),
                            rank(-total_cml[3,2:8]),
                            rank(-total_cml[4,2:8]),
                            rank(-total_cml[5,2:8]),
                            rank(-total_cml[6,2:8]),
                            rank(-total_cml[7,2:8]))
                            )
                      )
names(rank)[1] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Reduction")

colnames(total_cml) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
t <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
colnames(t) <- c("Team", "Scenario", "Reduction")
t$Reduction <- t$Reduction*100
t$Rank <- r$value

# t$scn <- ifelse(t$Scenario == "RL01", "Base",
#                 ifelse(t$Scenario == "RL02", "VX", 
#                        ifelse(t$Scenario == "RL03", "AV",
#                               ifelse(t$Scenario == "RL04", "VX; AV",
#                                      ifelse(t$Scenario == "RL05", "SC trigger",
#                                             ifelse(t$Scenario == "RL06", "SC fall",
#                                                    ifelse(t$Scenario == "RL07", 
#                                                           "VX; AV; SC trigger",
#                                                           "VX; AV; SC fall")))))))
# t$Scenario <- factor(t$scn, levels = unique(t$scn))
p3 <- ggplot(t) +
  geom_raster(aes(y = Team, x = Scenario, fill = Reduction)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 32, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 1), "% (", Rank, ")")), size=3) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Reduction, %') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom')+
  ggtitle("b. Percent change (rank) in illness burden")


r <- read_overall_mean(scn = "RL01_", 1)
peak.weeks <- get_peaks(r)
peak.weeks$Scenario <- "RL01"
cml <- cml[which(cml$Bin == "sm.mean"),]
total_cml <- cml[-1]
total_cml <- total_cml[-2]

r <- read_overall_mean(scn = "RL02_", 1)
p <- get_peaks(r)
p$Scenario <- "RL02"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml2 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL03_", 1)
p <- get_peaks(r)
p$Scenario <- "RL03"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml3 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL04_", 1)
p <- get_peaks(r)
p$Scenario <- "RL04"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml4 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL05_", 1)
p <- get_peaks(r)
p$Scenario <- "RL05"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml5 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL06_", 1)
p <- get_peaks(r)
p$Scenario <- "RL06"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml6 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL07_", 1)
p <- get_peaks(r)
p$Scenario <- "RL07"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml7 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL08_", 1)
p <- get_peaks(r)
p$Scenario <- "RL08"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml8 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 

team <- as.character(total_cml$team)

total_cml <- cbind(factor(x = c(team), 
                          levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS")),
                   as.data.frame(lapply(total_cml[-(1:2)], as.numeric)))
names(total_cml)[1] <- "team"

total_cml <- rbind(total_cml, c("ENS", colMeans(total_cml[-1])))

total_cml <- cbind( total_cml$team,
                    as.data.frame(lapply(total_cml[-1], as.numeric)))

rank <- as.data.frame(rbind(rank(-total_cml[1,2:8]),
                                  rank(-total_cml[2,2:8]),
                                  rank(-total_cml[3,2:8]),
                                  rank(-total_cml[4,2:8]),
                                  rank(-total_cml[5,2:8]),
                                  rank(-total_cml[6,2:8]),
                                  rank(-total_cml[7,2:8]))
                      )

rank$team <- factor(x = c("COL", "NEU3", "UTA",  
                          "UVA",  "IMP",  "COL2", "ENS"), 
                    levels = c("COL", "COL2", "IMP", "NEU3", 
                               "UTA", "UVA", "ENS"))

names(rank)[8] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Reduction")

colnames(total_cml) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
t <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
colnames(t) <- c("Team", "Scenario", "Reduction")
t$Reduction <- t$Reduction*100
t$Rank <- r$value

# t$scn <- ifelse(t$Scenario == "RL01", "Base",
#                 ifelse(t$Scenario == "RL02", "VX", 
#                        ifelse(t$Scenario == "RL03", "AV",
#                               ifelse(t$Scenario == "RL04", "VX; AV",
#                                      ifelse(t$Scenario == "RL05", "SC trigger",
#                                             ifelse(t$Scenario == "RL06", "SC fall",
#                                                    ifelse(t$Scenario == "RL07", 
#                                                           "VX; AV; SC trigger",
#                                                           "VX; AV; SC fall")))))))
# t$Scenario <- factor(t$scn, levels = unique(t$scn))

p4 <- ggplot(t) +
  geom_raster(aes(y = Team, x = Scenario, fill = Reduction)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 37, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 1), "% (", Rank, ")")), size=3) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Reduction, %') +
  theme(axis.title = element_text(size=12), 
        strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), 
        legend.position = 'bottom')+
  ggtitle("c. Percent change (rank) in peak illnesses")

pw <- spread(peak.weeks, Scenario, peakWeek)

pw$Delay2 <- pw$RL02 - pw$RL01
pw$Delay3 <- pw$RL03 - pw$RL01
pw$Delay4 <- pw$RL04 - pw$RL01
pw$Delay5 <- pw$RL05 - pw$RL01
pw$Delay6 <- pw$RL06 - pw$RL01
pw$Delay7 <- pw$RL07 - pw$RL01
pw$Delay8 <- pw$RL08 - pw$RL01

pw <- pw[-2:-9]
colnames(pw) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
p <- melt(pw, id.vars = c("team"), variable.name = "Delay")
colnames(p) <- c("Team", "Scenario", "Delay")

p$Team <- factor(x = p$Team,
                 levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA")
                 )

# p$scn <- ifelse(p$Scenario == "RL01", "Base",
#                 ifelse(p$Scenario == "RL02", "VX", 
#                        ifelse(p$Scenario == "RL03", "AV",
#                               ifelse(p$Scenario == "RL04", "VX; AV",
#                                      ifelse(p$Scenario == "RL05", "SC trigger",
#                                             ifelse(p$Scenario == "RL06", "SC fall",
#                                                    ifelse(p$Scenario == "RL07", 
#                                                           "VX; AV; SC trigger",
#                                                           "VX; AV; SC fall")))))))
# p$Scenario <- factor(p$scn, levels = unique(p$scn))

p5 <- ggplot(p) +
  geom_raster(aes(y = Team, x = Scenario, fill = Delay)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 6, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label = Delay), size=4) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Delay, Weeks') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom')+
  ggtitle("d. Delay in peak illnesses")


png("fig3_2009.png",width = 800, height=1000)
ggarrange(p6, p3, p4, p5, nrow=4, ncol=1)#,common.legend = TRUE, legend="bottom")
dev.off()


#Supp figure hospitalizations - rank order figures----------------------------

long_RL01 <- read_overall_mean(scn = "RL01_", 2)
long_RL01$team <- factor(x = long_RL01$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA"))

long_RL01 <- long_RL01[order(long_RL01$team),]
long_RL01$Date <- as.Date("2020-06-24") + (long_RL01$week*7)

cml <- cml[which(cml$Bin == "sm.mean"),]
total_cml <- cml[-3:-4]

cml_sum <- cml$Cml

read_overall_mean(scn = "RL02_", 2)
total_cml$Cml2 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum <- data.frame(RL01 = cml_sum,
                      RL02 = cml$Cml[which(cml$Bin == "sm.mean")])

read_overall_mean(scn = "RL03_", 2)
total_cml$Cml3 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL03 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "RL04_", 2)
total_cml$Cml4 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL04 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "RL05_", 2)
total_cml$Cml5 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL05 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "RL06_", 2)
total_cml$Cml6 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL06 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "RL07_", 2)
total_cml$Cml7 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL07 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "RL08_", 2)
total_cml$Cml8 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL08 <- cml$Cml[which(cml$Bin == "sm.mean")]

total_cml$team <- factor(x = total_cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))

cml_sum <- rbind(cml_sum, colMeans(cml_sum))
total_cml <- rbind(total_cml,
                   c(as.numeric(cml_sum[7,1]),
                     "ENS", 
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL02[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL03[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL04[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL05[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL06[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL07[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL08[7]) / cml_sum$RL01[7])
                   )
)

rl01 <- total_cml[1:2]
rl01$Cml <- as.numeric(rl01$Cml)
total_cml <- total_cml[-1]
total_cml <-  cbind(total_cml[1], lapply(total_cml[-1], as.numeric))

averted <- cbind(total_cml[1], total_cml[-1] * (rl01$Cml * 100000))
colnames(averted) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
a <- melt(averted, id.vars = c("team"), variable.name = "Averted")
names(a) <- c("Team", 'Scenario', "Averted")

rank <- as.data.frame(cbind(averted$team,
                            rbind(rank(-averted[1,2:8]),
                                  rank(-averted[2,2:8]),
                                  rank(-averted[3,2:8]),
                                  rank(-averted[4,2:8]),
                                  rank(-averted[5,2:8]),
                                  rank(-averted[6,2:8]),
                                  rank(-averted[7,2:8]))
)
)
names(rank)[1] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Averted")
a$Rank <- r$value

# a$scn <- ifelse(a$Scenario == "RL01", "Base",
#                 ifelse(a$Scenario == "RL02", "VX", 
#                        ifelse(a$Scenario == "RL03", "AV",
#                               ifelse(a$Scenario == "RL04", "VX; AV",
#                                      ifelse(a$Scenario == "RL05", "SC trigger",
#                                             ifelse(a$Scenario == "RL06", "SC fall",
#                                                    ifelse(a$Scenario == "RL07", 
#                                                           "VX; AV; SC trigger",
#                                                           "VX; AV; SC fall")))))))
# a$Scenario <- factor(a$scn, levels = unique(a$scn))

h6 <- ggplot(a) +
  geom_raster(aes(y = Team, x = Scenario, fill = Averted)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 8e6, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label=scales::comma(Averted)), size=3) +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Averted') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom',
        legend.key.width = unit(1,"cm"))+
  ggtitle("a. Averted hospitalizations")

rank <- as.data.frame(cbind(total_cml$team,
                            rbind(rank(-total_cml[1,2:8]),
                                  rank(-total_cml[2,2:8]),
                                  rank(-total_cml[3,2:8]),
                                  rank(-total_cml[4,2:8]),
                                  rank(-total_cml[5,2:8]),
                                  rank(-total_cml[6,2:8]),
                                  rank(-total_cml[7,2:8]))
)
)
names(rank)[1] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Reduction")

colnames(total_cml) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
t <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
colnames(t) <- c("Team", "Scenario", "Reduction")
t$Reduction <- t$Reduction*100
t$Rank <- r$value

# t$scn <- ifelse(t$Scenario == "RL01", "Base",
#                 ifelse(t$Scenario == "RL02", "VX", 
#                        ifelse(t$Scenario == "RL03", "AV",
#                               ifelse(t$Scenario == "RL04", "VX; AV",
#                                      ifelse(t$Scenario == "RL05", "SC trigger",
#                                             ifelse(t$Scenario == "RL06", "SC fall",
#                                                    ifelse(t$Scenario == "RL07", 
#                                                           "VX; AV; SC trigger",
#                                                           "VX; AV; SC fall")))))))
# t$Scenario <- factor(t$scn, levels = unique(t$scn))


h3 <- ggplot(t) +
  geom_raster(aes(y = Team, x = Scenario, fill = Reduction)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 33, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 1), "% (", Rank, ")")), size=3) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Reduction, %') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom')+
  ggtitle("b. Percent change (rank) in hospitalization burden")


r <- read_overall_mean(scn = "RL01_", 2)
peak.weeks <- get_peaks(r)
peak.weeks$Scenario <- "RL01"
cml <- cml[which(cml$Bin == "sm.mean"),]
total_cml <- cml[-1]
total_cml <- total_cml[-2]

r <- read_overall_mean(scn = "RL02_", 2)
p <- get_peaks(r)
p$Scenario <- "RL02"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml2 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL03_", 2)
p <- get_peaks(r)
p$Scenario <- "RL03"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml3 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL04_", 2)
p <- get_peaks(r)
p$Scenario <- "RL04"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml4 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL05_", 2)
p <- get_peaks(r)
p$Scenario <- "RL05"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml5 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL06_", 2)
p <- get_peaks(r)
p$Scenario <- "RL06"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml6 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL07_", 2)
p <- get_peaks(r)
p$Scenario <- "RL07"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml7 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL08_", 2)
p <- get_peaks(r)
p$Scenario <- "RL08"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml8 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 

team <- as.character(total_cml$team)

total_cml <- cbind(factor(x = c(team), 
                          levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS")),
                   as.data.frame(lapply(total_cml[-(1:2)], as.numeric)))
names(total_cml)[1] <- "team"

total_cml <- rbind(total_cml, c("ENS", colMeans(total_cml[-1])))

total_cml <- cbind( total_cml$team,
                    as.data.frame(lapply(total_cml[-1], as.numeric)))

rank <- as.data.frame(rbind(rank(-total_cml[1,2:8]),
                            rank(-total_cml[2,2:8]),
                            rank(-total_cml[3,2:8]),
                            rank(-total_cml[4,2:8]),
                            rank(-total_cml[5,2:8]),
                            rank(-total_cml[6,2:8]),
                            rank(-total_cml[7,2:8]))
)

rank$team <- factor(x = c("COL", "NEU3", "UTA",  
                          "UVA",  "IMP",  "COL2", "ENS"), 
                    levels = c("COL", "COL2", "IMP", "NEU3", 
                               "UTA", "UVA", "ENS"))

names(rank)[8] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Reduction")

colnames(total_cml) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
t <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
colnames(t) <- c("Team", "Scenario", "Reduction")
t$Reduction <- t$Reduction*100
t$Rank <- r$value

# t$scn <- ifelse(t$Scenario == "RL01", "Base",
#                 ifelse(t$Scenario == "RL02", "VX", 
#                        ifelse(t$Scenario == "RL03", "AV",
#                               ifelse(t$Scenario == "RL04", "VX; AV",
#                                      ifelse(t$Scenario == "RL05", "SC trigger",
#                                             ifelse(t$Scenario == "RL06", "SC fall",
#                                                    ifelse(t$Scenario == "RL07", 
#                                                           "VX; AV; SC trigger",
#                                                           "VX; AV; SC fall")))))))
# t$Scenario <- factor(t$scn, levels = unique(t$scn))

h4 <- ggplot(t) +
  geom_raster(aes(y = Team, x = Scenario, fill = Reduction)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 40, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 1), "% (", Rank, ")")), size=3) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Reduction, %') +
  theme(axis.title = element_text(size=12), 
        strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), 
        legend.position = 'bottom')+
  ggtitle("c. Percent change (rank) in peak hospitalizations")

pw <- spread(peak.weeks, Scenario, peakWeek)

pw$Delay2 <- pw$RL02 - pw$RL01
pw$Delay3 <- pw$RL03 - pw$RL01
pw$Delay4 <- pw$RL04 - pw$RL01
pw$Delay5 <- pw$RL05 - pw$RL01
pw$Delay6 <- pw$RL06 - pw$RL01
pw$Delay7 <- pw$RL07 - pw$RL01
pw$Delay8 <- pw$RL08 - pw$RL01

pw <- pw[-2:-9]
colnames(pw) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
p <- melt(pw, id.vars = c("team"), variable.name = "Delay")
colnames(p) <- c("Team", "Scenario", "Delay")

p$Team <- factor(x = p$Team,
                 levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA")
)

# p$scn <- ifelse(p$Scenario == "RL01", "Base",
#                 ifelse(p$Scenario == "RL02", "VX", 
#                        ifelse(p$Scenario == "RL03", "AV",
#                               ifelse(p$Scenario == "RL04", "VX; AV",
#                                      ifelse(p$Scenario == "RL05", "SC trigger",
#                                             ifelse(p$Scenario == "RL06", "SC fall",
#                                                    ifelse(p$Scenario == "RL07", 
#                                                           "VX; AV; SC trigger",
#                                                           "VX; AV; SC fall")))))))
# p$Scenario <- factor(p$scn, levels = unique(p$scn))

h5 <- ggplot(p) +
  geom_raster(aes(y = Team, x = Scenario, fill = Delay)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 5, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label = Delay), size=4) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Delay, Weeks') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom')+
  ggtitle("d. Delay in peak hospitalizations")


png("fig3h_2009.png",width = 800, height=1000)
ggarrange(h6, h3, h4, h5, nrow=4, ncol=1)#,common.legend = TRUE, legend="bottom")
dev.off()

#Supp figure deaths - rank order figures----------------------------

long_RL01 <- read_overall_mean(scn = "RL01_", 3)
long_RL01$team <- factor(x = long_RL01$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA"))

long_RL01 <- long_RL01[order(long_RL01$team),]
long_RL01$Date <- as.Date("2020-06-24") + (long_RL01$week*7)

cml <- cml[which(cml$Bin == "sm.mean"),]
total_cml <- cml[-3:-4]

cml_sum <- cml$Cml

read_overall_mean(scn = "RL02_", 3)
total_cml$Cml2 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum <- data.frame(RL01 = cml_sum,
                      RL02 = cml$Cml[which(cml$Bin == "sm.mean")])

read_overall_mean(scn = "RL03_", 3)
total_cml$Cml3 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL03 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "RL04_", 3)
total_cml$Cml4 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL04 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "RL05_", 3)
total_cml$Cml5 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL05 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "RL06_", 3)
total_cml$Cml6 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL06 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "RL07_", 3)
total_cml$Cml7 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL07 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "RL08_", 3)
total_cml$Cml8 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL08 <- cml$Cml[which(cml$Bin == "sm.mean")]

total_cml$team <- factor(x = total_cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))

cml_sum <- rbind(cml_sum, colMeans(cml_sum))
total_cml <- rbind(total_cml,
                   c(as.numeric(cml_sum[7,1]),
                     "ENS", 
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL02[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL03[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL04[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL05[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL06[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL07[7]) / cml_sum$RL01[7]),
                     as.numeric((cml_sum$RL01[7] - cml_sum$RL08[7]) / cml_sum$RL01[7])
                   )
)

rl01 <- total_cml[1:2]
rl01$Cml <- as.numeric(rl01$Cml)
total_cml <- total_cml[-1]
total_cml <-  cbind(total_cml[1], lapply(total_cml[-1], as.numeric))

averted <- cbind(total_cml[1], total_cml[-1] * (rl01$Cml * 100000))
colnames(averted) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
a <- melt(averted, id.vars = c("team"), variable.name = "Averted")
names(a) <- c("Team", 'Scenario', "Averted")

rank <- as.data.frame(cbind(averted$team,
                            rbind(rank(-averted[1,2:8]),
                                  rank(-averted[2,2:8]),
                                  rank(-averted[3,2:8]),
                                  rank(-averted[4,2:8]),
                                  rank(-averted[5,2:8]),
                                  rank(-averted[6,2:8]),
                                  rank(-averted[7,2:8]))
)
)
names(rank)[1] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Averted")
a$Rank <- r$value

# a$scn <- ifelse(a$Scenario == "RL01", "Base",
#                 ifelse(a$Scenario == "RL02", "VX", 
#                        ifelse(a$Scenario == "RL03", "AV",
#                               ifelse(a$Scenario == "RL04", "VX; AV",
#                                      ifelse(a$Scenario == "RL05", "SC trigger",
#                                             ifelse(a$Scenario == "RL06", "SC fall",
#                                                    ifelse(a$Scenario == "RL07", 
#                                                           "VX; AV; SC trigger",
#                                                           "VX; AV; SC fall")))))))
# a$Scenario <- factor(a$scn, levels = unique(a$scn))

d6 <- ggplot(a) +
  geom_raster(aes(y = Team, x = Scenario, fill = Averted)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 550e3, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label=scales::comma(Averted)), size=3) +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Averted') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom',
        legend.key.width = unit(1,"cm"))+
  ggtitle("a. Averted deaths")

rank <- as.data.frame(cbind(total_cml$team,
                            rbind(rank(-total_cml[1,2:8]),
                                  rank(-total_cml[2,2:8]),
                                  rank(-total_cml[3,2:8]),
                                  rank(-total_cml[4,2:8]),
                                  rank(-total_cml[5,2:8]),
                                  rank(-total_cml[6,2:8]),
                                  rank(-total_cml[7,2:8]))
)
)
names(rank)[1] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Reduction")

colnames(total_cml) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
t <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
colnames(t) <- c("Team", "Scenario", "Reduction")
t$Reduction <- t$Reduction*100
t$Rank <- r$value

# t$scn <- ifelse(t$Scenario == "RL01", "Base",
#                 ifelse(t$Scenario == "RL02", "VX", 
#                        ifelse(t$Scenario == "RL03", "AV",
#                               ifelse(t$Scenario == "RL04", "VX; AV",
#                                      ifelse(t$Scenario == "RL05", "SC trigger",
#                                             ifelse(t$Scenario == "RL06", "SC fall",
#                                                    ifelse(t$Scenario == "RL07", 
#                                                           "VX; AV; SC trigger",
#                                                           "VX; AV; SC fall")))))))
# t$Scenario <- factor(t$scn, levels = unique(t$scn))

d3 <- ggplot(t) +
  geom_raster(aes(y = Team, x = Scenario, fill = Reduction)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 33, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 1), "% (", Rank, ")")), size=3) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Reduction, %') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom')+
  ggtitle("b. Percent change (rank) in death burden")


r <- read_overall_mean(scn = "RL01_", 3)
peak.weeks <- get_peaks(r)
peak.weeks$Scenario <- "RL01"
cml <- cml[which(cml$Bin == "sm.mean"),]
total_cml <- cml[-1]
total_cml <- total_cml[-2]

r <- read_overall_mean(scn = "RL02_", 3)
p <- get_peaks(r)
p$Scenario <- "RL02"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml2 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL03_", 3)
p <- get_peaks(r)
p$Scenario <- "RL03"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml3 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL04_", 3)
p <- get_peaks(r)
p$Scenario <- "RL04"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml4 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL05_", 3)
p <- get_peaks(r)
p$Scenario <- "RL05"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml5 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL06_", 3)
p <- get_peaks(r)
p$Scenario <- "RL06"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml6 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL07_", 3)
p <- get_peaks(r)
p$Scenario <- "RL07"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml7 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "RL08_", 3)
p <- get_peaks(r)
p$Scenario <- "RL08"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml8 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 

team <- as.character(total_cml$team)

total_cml <- cbind(factor(x = c(team), 
                          levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS")),
                   as.data.frame(lapply(total_cml[-(1:2)], as.numeric)))
names(total_cml)[1] <- "team"

total_cml <- rbind(total_cml, c("ENS", colMeans(total_cml[-1])))

total_cml <- cbind( total_cml$team,
                    as.data.frame(lapply(total_cml[-1], as.numeric)))

rank <- as.data.frame(rbind(rank(-total_cml[1,2:8]),
                            rank(-total_cml[2,2:8]),
                            rank(-total_cml[3,2:8]),
                            rank(-total_cml[4,2:8]),
                            rank(-total_cml[5,2:8]),
                            rank(-total_cml[6,2:8]),
                            rank(-total_cml[7,2:8]))
)

rank$team <- factor(x = c("COL", "NEU3", "UTA",  
                          "UVA",  "IMP",  "COL2", "ENS"), 
                    levels = c("COL", "COL2", "IMP", "NEU3", 
                               "UTA", "UVA", "ENS"))

names(rank)[8] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Reduction")

colnames(total_cml) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
t <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
colnames(t) <- c("Team", "Scenario", "Reduction")
t$Reduction <- t$Reduction*100
t$Rank <- r$value

# t$scn <- ifelse(t$Scenario == "RL01", "Base",
#                 ifelse(t$Scenario == "RL02", "VX", 
#                        ifelse(t$Scenario == "RL03", "AV",
#                               ifelse(t$Scenario == "RL04", "VX; AV",
#                                      ifelse(t$Scenario == "RL05", "SC trigger",
#                                             ifelse(t$Scenario == "RL06", "SC fall",
#                                                    ifelse(t$Scenario == "RL07", 
#                                                           "VX; AV; SC trigger",
#                                                           "VX; AV; SC fall")))))))
# t$Scenario <- factor(t$scn, levels = unique(t$scn))

d4 <- ggplot(t) +
  geom_raster(aes(y = Team, x = Scenario, fill = Reduction)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 40, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 1), "% (", Rank, ")")), size=3) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Reduction, %') +
  theme(axis.title = element_text(size=12), 
        strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), 
        legend.position = 'bottom')+
  ggtitle("c. Percent change (rank) in peak deaths")

pw <- spread(peak.weeks, Scenario, peakWeek)

pw$Delay2 <- pw$RL02 - pw$RL01
pw$Delay3 <- pw$RL03 - pw$RL01
pw$Delay4 <- pw$RL04 - pw$RL01
pw$Delay5 <- pw$RL05 - pw$RL01
pw$Delay6 <- pw$RL06 - pw$RL01
pw$Delay7 <- pw$RL07 - pw$RL01
pw$Delay8 <- pw$RL08 - pw$RL01

pw <- pw[-2:-9]
colnames(pw) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
p <- melt(pw, id.vars = c("team"), variable.name = "Delay")
colnames(p) <- c("Team", "Scenario", "Delay")

p$Team <- factor(x = p$Team,
                 levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA")
)

# p$scn <- ifelse(p$Scenario == "RL01", "Base",
#                 ifelse(p$Scenario == "RL02", "VX", 
#                        ifelse(p$Scenario == "RL03", "AV",
#                               ifelse(p$Scenario == "RL04", "VX; AV",
#                                      ifelse(p$Scenario == "RL05", "SC trigger",
#                                             ifelse(p$Scenario == "RL06", "SC fall",
#                                                    ifelse(p$Scenario == "RL07", 
#                                                           "VX; AV; SC trigger",
#                                                           "VX; AV; SC fall")))))))
# p$Scenario <- factor(p$scn, levels = unique(p$scn))

d5 <- ggplot(p) +
  geom_raster(aes(y = Team, x = Scenario, fill = Delay)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 5, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label = Delay), size=4) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Delay, Weeks') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom')+
  ggtitle("d. Delay in peak deaths")


png("fig3d_2009.png",width = 800, height=1000)
ggarrange(d6, d3, d4, d5, nrow=4, ncol=1)#,common.legend = TRUE, legend="bottom")
dev.off()

