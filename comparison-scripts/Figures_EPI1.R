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

#Figure 7 - age-specific AR---------------------------------------


s <- read.csv("SympIllnesses_EPI1_HP01.csv")
d <- read.csv("Deaths_EPI1_HP01.csv")

s <- s %>% group_by("agegroup")
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
s$team <- factor(x = s$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))

pal <- pal_nejm()
p1 <- ggplot(s, aes(x = team, y = cml, color=team)) +
  facet_wrap( ~ agegroup, scales = "free_y", nrow=1)+
  geom_point(size = 3.5, position=position_dodge2(0.9)) +
  geom_errorbar(aes(x = team, ymax = upper, ymin = lower), size=1,
                position=position_dodge2(0.9))+
  scale_color_manual(values = c(rev(pal(6)), "black"))+
  xlab("")+
  ylab("Percent Symptomatic")+ theme(panel.grid.major.x = element_blank())+
  ggtitle("a. Age-specific symptomatic percentages in EPI1 no intervention scenario (HP01)")+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())


d <- d %>% group_by("agegroup")
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
d$team <- factor(x = d$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))

pal <- pal_nejm()
p2 <- ggplot(d, aes(x = team, y = cml, color=team)) + 
  facet_wrap( ~ agegroup, scales = "free_y", nrow=1)+
  geom_point(size = 3.5, position=position_dodge2(0.9)) +
  geom_errorbar(aes(x = team, ymax = upper, ymin = lower), size=1,
                position=position_dodge2(0.9))+
  scale_color_manual(values = c(rev(pal(6)), "black"))+
  xlab("")+
  ylab("Death Rate per 100,000")+ theme(panel.grid.major.x = element_blank())+
  ggtitle("b. Age-specific death rates per 100,000 in EPI1 no intervention scenario (HP01)")+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())


png("fig2_EPI1.png",width = 950, height=800)
ggarrange(p1, p2, nrow=2, ncol=1)
dev.off()


#Figure 6 - unmitigated epi curve---------------------------------------


path <- "../model-forecasts/"


team.folder <- c("Columbia/COL/EPI1/", "Northeastern/EPI1/",  "UTAustin/", "UVirginia/", "Imperial/", "Columbia/COL2/EPI1/")
team.abbrev <- c("COL", "NEU", "UTA", "UVA", "IMP", "COL2")
file.types <-c("SymIllness", "Hosp", "Deaths", "AntiviralTX")
scn = "HP01_"
read_overall_mean <- function(scn, outcome){
  
  cols <- c("sm.mean", "sm.median", "sm.perc2p5", "sm.perc97p5")
  
  s <- read.csv(paste0(path,team.folder[1],scn,file.types[outcome],"_",team.abbrev[1],".csv"))
  symillness <- data.frame(c(s[which(s$Agegroup == "Overall" &
                                       s$Bin %in% cols),],
                             `team` = team.abbrev[1]))
  
  for(i in 2:6){
    
    if(grepl("RL", scn, fixed=TRUE)){
      if(i == 4){
        scn = gsub("RL", "RL_", scn)
      }
      if(i == 5){
        scn = gsub("_", "", scn)
      }
      if(i == 6){
        scn = paste0(scn, "_")
      }
    }
    else{
      if(i == 4){
        scn = gsub("HP", "HP_", scn)
      }
      if(i == 5){
        scn = gsub("_", "", scn)
      }
      if(i == 6){
        scn = paste0(scn, "_")
      }
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
      if(i == 2 & grepl("RL", scn, fixed=TRUE)){
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

long_HP01 <- read_overall_mean(scn = "HP01_", 1)
long_HP01$team <- factor(x = long_HP01$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA"))

long_HP01 <- long_HP01[order(long_HP01$team),]
long_HP01$Date <- as.Date("2020-06-24") + (long_HP01$week*7)

long <- long_HP01

wide <- long[-7] %>% 
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

long_ens <- rbind(long_HP01[-4], ensemble)

pal_black <- c(rev(pal(6)), "black")


png("fig1_EPI1.png",width = 850, height=500)
ggplot(data = long_ens, aes(x = Date, y = sm.mean, group = team)) +
  geom_line(aes(color = team), lwd = 1.5) +
  geom_ribbon(aes(ymin = sm.perc2p5, ymax = sm.perc97p5, fill = team), alpha = 0.15,
              show.legend = FALSE)+
  ylab("% Incident Symptomatic Illness") +
  scale_color_manual(values = pal_black)+
  scale_fill_manual(values = pal_black)+
  # ggtitle("Figure 6. No intervention scenario symptomatic illness epidemic curves")+
  theme_light()+
  scale_x_date(labels = date_format("%b"))

dev.off()

#Figure 10 - rank order figures---------------------------------------

cml <- cml[which(cml$Bin == "sm.mean"),]
total_cml <- cml[-3:-4]
cml_sum <- cml$Cml

read_overall_mean(scn = "HP02_", 1)
total_cml$Cml2 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum <- data.frame(HP01 = cml_sum,
                      HP02 = cml$Cml[which(cml$Bin == "sm.mean")])

read_overall_mean(scn = "HP03_", 1)
total_cml$Cml3 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP03 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP04_", 1)
total_cml$Cml4 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP04 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP05_", 1)
total_cml$Cml5 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP05 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP06_", 1)
total_cml$Cml6 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP06 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP07_", 1)
total_cml$Cml7 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP07 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP08_", 1)
total_cml$Cml8 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP08 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP09_", 1)
total_cml$Cml9 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP09 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP10_", 1)
total_cml$Cml10 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP10 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP11_", 1)
total_cml$Cml11 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP11 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP12_", 1)
total_cml$Cml12 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP12 <- cml$Cml[which(cml$Bin == "sm.mean")]

total_cml$team <- factor(x = total_cml$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))

cml_sum <- rbind(cml_sum, colMeans(cml_sum))
total_cml <- rbind(total_cml,
                   c(as.numeric(cml_sum[7,1]),
                     "ENS", 
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP02[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP03[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP04[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP05[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP06[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP07[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP08[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP09[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP10[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP11[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP12[7]) / cml_sum$HP01[7])
                   )
)
hp01 <- total_cml[1:2]
hp01$Cml <- as.numeric(hp01$Cml)
total_cml <- total_cml[-1]
total_cml <-  cbind(total_cml[1], lapply(total_cml[-1], as.numeric))

averted <- cbind(total_cml[1], total_cml[-1] * (hp01$Cml/100) * 328.2e6)
colnames(averted) <- c("team", "HP02", "HP03", "HP04", "HP05", "HP06", "HP07", "HP08", "HP09", "HP10", "HP11", "HP12")
a <- melt(averted, id.vars = c("team"), variable.name = "Averted")
names(a) <- c("Team", 'Scenario', "Averted")

rank <- as.data.frame(cbind(averted$team,
                            rbind(rank(-averted[1,2:12]),
                                  rank(-averted[2,2:12]),
                                  rank(-averted[3,2:12]),
                                  rank(-averted[4,2:12]),
                                  rank(-averted[5,2:12]),
                                  rank(-averted[6,2:12]),
                                  rank(-averted[7,2:12]))
)
)
names(rank)[1] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Averted")
a$Rank <- r$value

p6 <- ggplot(a) +
  geom_raster(aes(y = Team, x = Scenario, fill = Averted)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 56e6, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label=scales::comma(Averted)), size=3) +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Averted') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom',
        legend.key.width = unit(1.5,"cm"))+
  ggtitle("a. Averted illnesses")

rank <- as.data.frame(cbind(total_cml$team,
                            rbind(rank(-total_cml[1,2:12]),
                                  rank(-total_cml[2,2:12]),
                                  rank(-total_cml[3,2:12]),
                                  rank(-total_cml[4,2:12]),
                                  rank(-total_cml[5,2:12]),
                                  rank(-total_cml[6,2:12]),
                                  rank(-total_cml[7,2:12]))
                            )
                      )
names(rank)[1] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Reduction")

colnames(total_cml) <- c("team", "HP02", "HP03", "HP04", "HP05", "HP06", "HP07", "HP08", "HP09", "HP10", "HP11", "HP12")
t <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
colnames(t) <- c("Team", "Scenario", "Reduction")
t$Reduction <- t$Reduction*100
t$Rank <- r$value

p3 <- ggplot(t) +
  geom_raster(aes(y = Team, x = Scenario, fill = Reduction)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 47, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 1), "% (", Rank, ")")), size=3) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Reduction, %') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom')+
  ggtitle("b. Percent change (rank) in illness burden")


r <- read_overall_mean(scn = "HP01_", 1)
peak.weeks <- get_peaks(r)
peak.weeks$Scenario <- "HP01"
cml <- cml[which(cml$Bin == "sm.mean"),]
total_cml <- cml[-1]
total_cml <- total_cml[-2]

r <- read_overall_mean(scn = "HP02_", 1)
p <- get_peaks(r)
p$Scenario <- "HP02"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml2 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP03_", 1)
p <- get_peaks(r)
p$Scenario <- "HP03"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml3 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP04_", 1)
p <- get_peaks(r)
p$Scenario <- "HP04"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml4 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP05_", 1)
p <- get_peaks(r)
p$Scenario <- "HP05"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml5 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP06_", 1)
p <- get_peaks(r)
p$Scenario <- "HP06"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml6 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP07_", 1)
p <- get_peaks(r)
p$Scenario <- "HP07"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml7 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP08_", 1)
p <- get_peaks(r)
p$Scenario <- "HP08"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml8 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP09_", 1)
p <- get_peaks(r)
p$Scenario <- "HP09"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml9 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP10_", 1)
p <- get_peaks(r)
p$Scenario <- "HP10"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml10 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP11_", 1)
p <- get_peaks(r)
p$Scenario <- "HP11"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml11 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP12_", 1)
p <- get_peaks(r)
p$Scenario <- "HP12"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml12 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 

team <- as.character(total_cml$team)

total_cml <- cbind(factor(x = c(team), 
                          levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS")),
                   as.data.frame(lapply(total_cml[-(1:2)], as.numeric)))
names(total_cml)[1] <- "team"

total_cml <- rbind(total_cml, c("ENS", colMeans(total_cml[-1])))

total_cml <- cbind( total_cml$team,
                    as.data.frame(lapply(total_cml[-1], as.numeric)))

rank <- as.data.frame(cbind(total_cml$team,
                            rbind(rank(-total_cml[1,2:12]),
                                  rank(-total_cml[2,2:12]),
                                  rank(-total_cml[3,2:12]),
                                  rank(-total_cml[4,2:12]),
                                  rank(-total_cml[5,2:12]),
                                  rank(-total_cml[6,2:12]),
                                  rank(-total_cml[7,2:12]))
                            )
                      )
rank$team <- factor(x = c("COL", "NEU", "UTA",  
                          "UVA",  "IMP",  "COL2", "ENS"), 
                    levels = c("COL", "COL2", "IMP", "NEU", 
                               "UTA", "UVA", "ENS"))

r <- melt(rank, id.vars = c("team"), variable.name = "Reduction")

colnames(total_cml) <- c("team", "HP02", "HP03", "HP04", "HP05", "HP06", "HP07", "HP08", "HP09", "HP10", "HP11", "HP12")
t <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
colnames(t) <- c("Team", "Scenario", "Reduction")
t$Reduction <- t$Reduction*100
t$Rank <- r$value

p4 <- ggplot(t) +
  geom_raster(aes(y = Team, x = Scenario, fill = Reduction)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 47, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 1), "% (", Rank, ")")), size=3) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Reduction, %') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom')+
  ggtitle("c. Percent change (rank) in peak illnesses")

pw <- spread(peak.weeks, Scenario, peakWeek)

pw$Delay2 <- pw$HP02 - pw$HP01
pw$Delay3 <- pw$HP03 - pw$HP01
pw$Delay4 <- pw$HP04 - pw$HP01
pw$Delay5 <- pw$HP05 - pw$HP01
pw$Delay6 <- pw$HP06 - pw$HP01
pw$Delay7 <- pw$HP07 - pw$HP01
pw$Delay8 <- pw$HP08 - pw$HP01
pw$Delay9 <- pw$HP09 - pw$HP01
pw$Delay10 <- pw$HP10 - pw$HP01
pw$Delay11 <- pw$HP11 - pw$HP01
pw$Delay12 <- pw$HP12 - pw$HP01

pw <- pw[-2:-13]
colnames(pw) <- c("team", "HP02", "HP03", "HP04", "HP05", "HP06", "HP07", "HP08", "HP09", "HP10", "HP11", "HP12")
p <- melt(pw, id.vars = c("team"), variable.name = "Delay")
colnames(p) <- c("Team", "Scenario", "Delay")

p$Team <- factor(x = p$Team,
                 levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA")
)

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


png("fig3_EPI1.png",width = 800, height=1000)
ggarrange(p6, p3, p4, p5, nrow=4, ncol=1)#,common.legend = TRUE, legend="bottom")
dev.off()

# Supp figure hospitalizations - rank order figures ----------------------------

hp01 <- read_overall_mean(scn = "HP01_", 2)

cml <- cml[which(cml$Bin == "sm.mean"),]
total_cml <- cml[-3:-4]
cml_sum <- cml$Cml

read_overall_mean(scn = "HP02_", 2)
total_cml$Cml2 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum <- data.frame(HP01 = cml_sum,
                      HP02 = cml$Cml[which(cml$Bin == "sm.mean")])

read_overall_mean(scn = "HP03_", 2)
total_cml$Cml3 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP03 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP04_", 2)
total_cml$Cml4 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP04 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP05_", 2)
total_cml$Cml5 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP05 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP06_", 2)
total_cml$Cml6 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP06 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP07_", 2)
total_cml$Cml7 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP07 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP08_", 2)
total_cml$Cml8 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP08 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP09_", 2)
total_cml$Cml9 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP09 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP10_", 2)
total_cml$Cml10 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP10 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP11_", 2)
total_cml$Cml11 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP11 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP12_", 2)
total_cml$Cml12 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP12 <- cml$Cml[which(cml$Bin == "sm.mean")]

total_cml$team <- factor(x = total_cml$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))

cml_sum <- rbind(cml_sum, colMeans(cml_sum))
total_cml <- rbind(total_cml,
                   c(as.numeric(cml_sum[7,1]),
                     "ENS", 
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP02[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP03[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP04[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP05[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP06[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP07[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP08[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP09[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP10[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP11[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP12[7]) / cml_sum$HP01[7])
                   )
)
hp01 <- total_cml[1:2]
hp01$Cml <- as.numeric(hp01$Cml)
total_cml <- total_cml[-1]
total_cml <-  cbind(total_cml[1], lapply(total_cml[-1], as.numeric))

averted <- cbind(total_cml[1], total_cml[-1] * (hp01$Cml*100000))
colnames(averted) <- c("team", "HP02", "HP03", "HP04", "HP05", "HP06", "HP07", "HP08", "HP09", "HP10", "HP11", "HP12")
a <- melt(averted, id.vars = c("team"), variable.name = "Averted")
names(a) <- c("Team", 'Scenario', "Averted")

rank <- as.data.frame(cbind(averted$team,
                            rbind(rank(-averted[1,2:12]),
                                  rank(-averted[2,2:12]),
                                  rank(-averted[3,2:12]),
                                  rank(-averted[4,2:12]),
                                  rank(-averted[5,2:12]),
                                  rank(-averted[6,2:12]),
                                  rank(-averted[7,2:12]))
)
)
names(rank)[1] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Averted")
a$Rank <- r$value

h6 <- ggplot(a) +
  geom_raster(aes(y = Team, x = Scenario, fill = Averted)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 19e6, high = '#2CA02CFF', na.value = 'lightgrey') +
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
                            rbind(rank(-total_cml[1,2:12]),
                                  rank(-total_cml[2,2:12]),
                                  rank(-total_cml[3,2:12]),
                                  rank(-total_cml[4,2:12]),
                                  rank(-total_cml[5,2:12]),
                                  rank(-total_cml[6,2:12]),
                                  rank(-total_cml[7,2:12]))
)
)
names(rank)[1] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Reduction")

colnames(total_cml) <- c("team", "HP02", "HP03", "HP04", "HP05", "HP06", "HP07", "HP08", "HP09", "HP10", "HP11", "HP12")
t <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
colnames(t) <- c("Team", "Scenario", "Reduction")
t$Reduction <- t$Reduction*100
t$Rank <- r$value

h3 <- ggplot(t) +
  geom_raster(aes(y = Team, x = Scenario, fill = Reduction)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 47, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 1), "% (", Rank, ")")), size=3) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Reduction, %') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom')+
  ggtitle("b. Percent change (rank) in hospitalization burden")


r <- read_overall_mean(scn = "HP01_", 2)
peak.weeks <- get_peaks(r)
peak.weeks$Scenario <- "HP01"
cml <- cml[which(cml$Bin == "sm.mean"),]
total_cml <- cml[-1]
total_cml <- total_cml[-2]

r <- read_overall_mean(scn = "HP02_", 2)
p <- get_peaks(r)
p$Scenario <- "HP02"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml2 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP03_", 2)
p <- get_peaks(r)
p$Scenario <- "HP03"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml3 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP04_", 2)
p <- get_peaks(r)
p$Scenario <- "HP04"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml4 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP05_", 2)
p <- get_peaks(r)
p$Scenario <- "HP05"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml5 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP06_", 2)
p <- get_peaks(r)
p$Scenario <- "HP06"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml6 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP07_", 2)
p <- get_peaks(r)
p$Scenario <- "HP07"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml7 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP08_", 2)
p <- get_peaks(r)
p$Scenario <- "HP08"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml8 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP09_", 2)
p <- get_peaks(r)
p$Scenario <- "HP09"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml9 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP10_", 2)
p <- get_peaks(r)
p$Scenario <- "HP10"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml10 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP11_", 2)
p <- get_peaks(r)
p$Scenario <- "HP11"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml11 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP12_", 2)
p <- get_peaks(r)
p$Scenario <- "HP12"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml12 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 

team <- as.character(total_cml$team)

total_cml <- cbind(factor(x = c(team), 
                          levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS")),
                   as.data.frame(lapply(total_cml[-(1:2)], as.numeric)))
names(total_cml)[1] <- "team"

total_cml <- rbind(total_cml, c("ENS", colMeans(total_cml[-1])))

total_cml <- cbind( total_cml$team,
                    as.data.frame(lapply(total_cml[-1], as.numeric)))

rank <- as.data.frame(cbind(total_cml$team,
                            rbind(rank(-total_cml[1,2:12]),
                                  rank(-total_cml[2,2:12]),
                                  rank(-total_cml[3,2:12]),
                                  rank(-total_cml[4,2:12]),
                                  rank(-total_cml[5,2:12]),
                                  rank(-total_cml[6,2:12]),
                                  rank(-total_cml[7,2:12]))
)
)
rank$team <- factor(x = c("COL", "NEU", "UTA",  
                          "UVA",  "IMP",  "COL2", "ENS"), 
                    levels = c("COL", "COL2", "IMP", "NEU", 
                               "UTA", "UVA", "ENS"))

r <- melt(rank, id.vars = c("team"), variable.name = "Reduction")

colnames(total_cml) <- c("team", "HP02", "HP03", "HP04", "HP05", "HP06", "HP07", "HP08", "HP09", "HP10", "HP11", "HP12")
t <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
colnames(t) <- c("Team", "Scenario", "Reduction")
t$Reduction <- t$Reduction*100
t$Rank <- r$value

h4 <- ggplot(t) +
  geom_raster(aes(y = Team, x = Scenario, fill = Reduction)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 47, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 1), "% (", Rank, ")")), size=3) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Reduction, %') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom')+
  ggtitle("c. Percent change (rank) in peak hospitalizations")

pw <- spread(peak.weeks, Scenario, peakWeek)

pw$Delay2 <- pw$HP02 - pw$HP01
pw$Delay3 <- pw$HP03 - pw$HP01
pw$Delay4 <- pw$HP04 - pw$HP01
pw$Delay5 <- pw$HP05 - pw$HP01
pw$Delay6 <- pw$HP06 - pw$HP01
pw$Delay7 <- pw$HP07 - pw$HP01
pw$Delay8 <- pw$HP08 - pw$HP01
pw$Delay9 <- pw$HP09 - pw$HP01
pw$Delay10 <- pw$HP10 - pw$HP01
pw$Delay11 <- pw$HP11 - pw$HP01
pw$Delay12 <- pw$HP12 - pw$HP01

pw <- pw[-2:-13]
colnames(pw) <- c("team", "HP02", "HP03", "HP04", "HP05", "HP06", "HP07", "HP08", "HP09", "HP10", "HP11", "HP12")
p <- melt(pw, id.vars = c("team"), variable.name = "Delay")
colnames(p) <- c("Team", "Scenario", "Delay")

p$Team <- factor(x = p$Team,
                 levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA")
)

h5 <- ggplot(p) +
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
  ggtitle("d. Delay in peak hospitalizations")


png("fig3h_EPI1.png",width = 800, height=1000)
ggarrange(h6, h3, h4, h5, nrow=4, ncol=1)#,common.legend = TRUE, legend="bottom")
dev.off()

# Supp figure deaths - rank order figures ----------------------------

hp01 <- read_overall_mean(scn = "HP01_", 3)

cml <- cml[which(cml$Bin == "sm.mean"),]
total_cml <- cml[-3:-4]
cml_sum <- cml$Cml

read_overall_mean(scn = "HP02_", 3)
total_cml$Cml2 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum <- data.frame(HP01 = cml_sum,
                      HP02 = cml$Cml[which(cml$Bin == "sm.mean")])

read_overall_mean(scn = "HP03_", 3)
total_cml$Cml3 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP03 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP04_", 3)
total_cml$Cml4 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP04 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP05_", 3)
total_cml$Cml5 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP05 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP06_", 3)
total_cml$Cml6 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP06 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP07_", 3)
total_cml$Cml7 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP07 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP08_", 3)
total_cml$Cml8 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP08 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP09_", 3)
total_cml$Cml9 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP09 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP10_", 3)
total_cml$Cml10 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP10 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP11_", 3)
total_cml$Cml11 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP11 <- cml$Cml[which(cml$Bin == "sm.mean")]

read_overall_mean(scn = "HP12_", 3)
total_cml$Cml12 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP12 <- cml$Cml[which(cml$Bin == "sm.mean")]

total_cml$team <- factor(x = total_cml$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))

cml_sum <- rbind(cml_sum, colMeans(cml_sum))
total_cml <- rbind(total_cml,
                   c(as.numeric(cml_sum[7,1]),
                     "ENS", 
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP02[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP03[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP04[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP05[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP06[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP07[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP08[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP09[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP10[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP11[7]) / cml_sum$HP01[7]),
                     as.numeric((cml_sum$HP01[7] - cml_sum$HP12[7]) / cml_sum$HP01[7])
                   )
)
hp01 <- total_cml[1:2]
hp01$Cml <- as.numeric(hp01$Cml)
total_cml <- total_cml[-1]
total_cml <-  cbind(total_cml[1], lapply(total_cml[-1], as.numeric))

averted <- cbind(total_cml[1], total_cml[-1] * (hp01$Cml*100000))
colnames(averted) <- c("team", "HP02", "HP03", "HP04", "HP05", "HP06", "HP07", "HP08", "HP09", "HP10", "HP11", "HP12")
a <- melt(averted, id.vars = c("team"), variable.name = "Averted")
names(a) <- c("Team", 'Scenario', "Averted")

rank <- as.data.frame(cbind(averted$team,
                            rbind(rank(-averted[1,2:12]),
                                  rank(-averted[2,2:12]),
                                  rank(-averted[3,2:12]),
                                  rank(-averted[4,2:12]),
                                  rank(-averted[5,2:12]),
                                  rank(-averted[6,2:12]),
                                  rank(-averted[7,2:12]))
)
)
names(rank)[1] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Averted")
a$Rank <- r$value

d6 <- ggplot(a) +
  geom_raster(aes(y = Team, x = Scenario, fill = Averted)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 2e6, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label=scales::comma(round(Averted, 0))), size=3) +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Averted') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom',
        legend.key.width = unit(1,"cm"))+
  ggtitle("a. Averted deaths")

rank <- as.data.frame(cbind(total_cml$team,
                            rbind(rank(-total_cml[1,2:12]),
                                  rank(-total_cml[2,2:12]),
                                  rank(-total_cml[3,2:12]),
                                  rank(-total_cml[4,2:12]),
                                  rank(-total_cml[5,2:12]),
                                  rank(-total_cml[6,2:12]),
                                  rank(-total_cml[7,2:12]))
)
)
names(rank)[1] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Reduction")

colnames(total_cml) <- c("team", "HP02", "HP03", "HP04", "HP05", "HP06", "HP07", "HP08", "HP09", "HP10", "HP11", "HP12")
t <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
colnames(t) <- c("Team", "Scenario", "Reduction")
t$Reduction <- t$Reduction*100
t$Rank <- r$value

d3 <- ggplot(t) +
  geom_raster(aes(y = Team, x = Scenario, fill = Reduction)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 47, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 1), "% (", Rank, ")")), size=3) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Reduction, %') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom')+
  ggtitle("b. Percent change (rank) in death burden")


r <- read_overall_mean(scn = "HP01_", 3)
peak.weeks <- get_peaks(r)
peak.weeks$Scenario <- "HP01"
cml <- cml[which(cml$Bin == "sm.mean"),]
total_cml <- cml[-1]
total_cml <- total_cml[-2]

r <- read_overall_mean(scn = "HP02_", 3)
p <- get_peaks(r)
p$Scenario <- "HP02"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml2 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP03_", 3)
p <- get_peaks(r)
p$Scenario <- "HP03"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml3 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP04_", 3)
p <- get_peaks(r)
p$Scenario <- "HP04"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml4 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP05_", 3)
p <- get_peaks(r)
p$Scenario <- "HP05"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml5 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP06_", 3)
p <- get_peaks(r)
p$Scenario <- "HP06"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml6 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP07_", 3)
p <- get_peaks(r)
p$Scenario <- "HP07"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml7 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP08_", 3)
p <- get_peaks(r)
p$Scenario <- "HP08"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml8 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP09_", 3)
p <- get_peaks(r)
p$Scenario <- "HP09"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml9 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP10_", 3)
p <- get_peaks(r)
p$Scenario <- "HP10"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml10 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP11_", 3)
p <- get_peaks(r)
p$Scenario <- "HP11"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml11 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
r <- read_overall_mean(scn = "HP12_", 3)
p <- get_peaks(r)
p$Scenario <- "HP12"
peak.weeks <- rbind(peak.weeks, p)
total_cml$Cml12 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 

team <- as.character(total_cml$team)

total_cml <- cbind(factor(x = c(team), 
                          levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS")),
                   as.data.frame(lapply(total_cml[-(1:2)], as.numeric)))
names(total_cml)[1] <- "team"

total_cml <- rbind(total_cml, c("ENS", colMeans(total_cml[-1])))

total_cml <- cbind( total_cml$team,
                    as.data.frame(lapply(total_cml[-1], as.numeric)))

rank <- as.data.frame(cbind(total_cml$team,
                            rbind(rank(-total_cml[1,2:12]),
                                  rank(-total_cml[2,2:12]),
                                  rank(-total_cml[3,2:12]),
                                  rank(-total_cml[4,2:12]),
                                  rank(-total_cml[5,2:12]),
                                  rank(-total_cml[6,2:12]),
                                  rank(-total_cml[7,2:12]))
)
)
rank$team <- factor(x = c("COL", "NEU", "UTA",  
                          "UVA",  "IMP",  "COL2", "ENS"), 
                    levels = c("COL", "COL2", "IMP", "NEU", 
                               "UTA", "UVA", "ENS"))

r <- melt(rank, id.vars = c("team"), variable.name = "Reduction")

colnames(total_cml) <- c("team", "HP02", "HP03", "HP04", "HP05", "HP06", "HP07", "HP08", "HP09", "HP10", "HP11", "HP12")
t <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
colnames(t) <- c("Team", "Scenario", "Reduction")
t$Reduction <- t$Reduction*100
t$Rank <- r$value

d4 <- ggplot(t) +
  geom_raster(aes(y = Team, x = Scenario, fill = Reduction)) +
  scale_fill_gradient2(low = 'white', mid = 'deepskyblue2', midpoint = 47, high = '#2CA02CFF', na.value = 'lightgrey') +
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 1), "% (", Rank, ")")), size=3) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Reduction, %') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom')+
  ggtitle("c. Percent change (rank) in peak deaths")

pw <- spread(peak.weeks, Scenario, peakWeek)

pw$Delay2 <- pw$HP02 - pw$HP01
pw$Delay3 <- pw$HP03 - pw$HP01
pw$Delay4 <- pw$HP04 - pw$HP01
pw$Delay5 <- pw$HP05 - pw$HP01
pw$Delay6 <- pw$HP06 - pw$HP01
pw$Delay7 <- pw$HP07 - pw$HP01
pw$Delay8 <- pw$HP08 - pw$HP01
pw$Delay9 <- pw$HP09 - pw$HP01
pw$Delay10 <- pw$HP10 - pw$HP01
pw$Delay11 <- pw$HP11 - pw$HP01
pw$Delay12 <- pw$HP12 - pw$HP01

pw <- pw[-2:-13]
colnames(pw) <- c("team", "HP02", "HP03", "HP04", "HP05", "HP06", "HP07", "HP08", "HP09", "HP10", "HP11", "HP12")
p <- melt(pw, id.vars = c("team"), variable.name = "Delay")
colnames(p) <- c("Team", "Scenario", "Delay")

p$Team <- factor(x = p$Team,
                 levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA")
)

d5 <- ggplot(p) +
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
  ggtitle("d. Delay in peak deaths")


png("fig3d_EPI1.png",width = 800, height=1000)
ggarrange(d6, d3, d4, d5, nrow=4, ncol=1)#,common.legend = TRUE, legend="bottom")
dev.off()
