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

#Functions---------------------------------------

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


#Figure 3 - rank order figures---------------------------------------

long_RL01 <- read_overall_mean(scn = "RL01_", 1)
total_cml <- cml[which(cml$Bin == "sm.mean"),][-3:-4]
cml_2p5 <- cml[which(cml$Bin == "sm.perc2p5"),][-3:-4]
cml_97p5 <- cml[which(cml$Bin == "sm.perc97p5"),][-3:-4]
cml_sum <- total_cml$Cml

read_overall_mean(scn = "RL02_", 1)
total_cml$Cml2 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum <- data.frame(team = cml$team[which(cml$Bin == "sm.mean")],
                      RL01 = cml_sum,
                      RL02 = cml$Cml[which(cml$Bin == "sm.mean")])
cml_2p5 <- data.frame(team = cml_2p5$team,
                      RL01 = cml_2p5$Cml,
                      RL02 = cml$Cml[which(cml$Bin == "sm.perc2p5")])
cml_97p5 <- data.frame(team = cml_97p5$team,
                       RL01 = cml_97p5$Cml,
                       RL02 = cml$Cml[which(cml$Bin == "sm.perc97p5")])

read_overall_mean(scn = "RL03_", 1)
total_cml$Cml3 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL03 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL03 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL03 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "RL04_", 1)
total_cml$Cml4 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL04 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL04 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL04 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "RL05_", 1)
total_cml$Cml5 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL05 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL05 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL05 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "RL06_", 1)
total_cml$Cml6 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL06 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL06 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL06 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "RL07_", 1)
total_cml$Cml7 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL07 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL07 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL07 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "RL08_", 1)
total_cml$Cml8 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL08 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL08 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL08 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

total_cml$team <- factor(x = total_cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml_sum$team <- factor(x = cml_sum$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml_2p5$team <- factor(x = cml_2p5$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml_97p5$team <- factor(x = cml_97p5$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))

cml_sum <- rbind(cml_sum, c("ENS", as.numeric(colMeans(cml_sum[2:9]))))
cml_2p5 <- rbind(cml_2p5, c("ENS", as.numeric(colMeans(cml_2p5[2:9]))))
cml_97p5 <- rbind(cml_97p5, c("ENS", as.numeric(colMeans(cml_97p5[2:9]))))

total_cml <- rbind(total_cml,
                   c(as.numeric(cml_sum[7,2]),
                     "ENS", 
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL02[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL03[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL04[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL05[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL06[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL07[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL08[7])) / as.numeric(cml_sum$RL01[7])
                     )
                   )


rl01 <- total_cml[1:2]
rl01$Cml <- as.numeric(rl01$Cml)
total_cml <- total_cml[-1]
total_cml <-  cbind(total_cml[1], lapply(total_cml[-1], as.numeric))
cml_2p5 <-  cbind(cml_2p5[1], lapply(cml_2p5[-1], as.numeric))
cml_97p5 <-  cbind(cml_97p5[1], lapply(cml_97p5[-1], as.numeric))

total_2p5 <- data.frame(team = cml_2p5$team,
                        Cml2 = (cml_2p5$RL01 - cml_2p5$RL02) / cml_2p5$RL01,
                        Cml3 = (cml_2p5$RL01 - cml_2p5$RL03) / cml_2p5$RL01,
                        Cml4 = (cml_2p5$RL01 - cml_2p5$RL04) / cml_2p5$RL01,
                        Cml5 = (cml_2p5$RL01 - cml_2p5$RL05) / cml_2p5$RL01,
                        Cml6 = (cml_2p5$RL01 - cml_2p5$RL06) / cml_2p5$RL01,
                        Cml7 = (cml_2p5$RL01 - cml_2p5$RL07) / cml_2p5$RL01,
                        Cml8 = (cml_2p5$RL01 - cml_2p5$RL08) / cml_2p5$RL01)
total_97p5 <- data.frame(team = cml_97p5$team,
                        Cml2 = (cml_97p5$RL01 - cml_97p5$RL02) / cml_97p5$RL01,
                        Cml3 = (cml_97p5$RL01 - cml_97p5$RL03) / cml_97p5$RL01,
                        Cml4 = (cml_97p5$RL01 - cml_97p5$RL04) / cml_97p5$RL01,
                        Cml5 = (cml_97p5$RL01 - cml_97p5$RL05) / cml_97p5$RL01,
                        Cml6 = (cml_97p5$RL01 - cml_97p5$RL06) / cml_97p5$RL01,
                        Cml7 = (cml_97p5$RL01 - cml_97p5$RL07) / cml_97p5$RL01,
                        Cml8 = (cml_97p5$RL01 - cml_97p5$RL08) / cml_97p5$RL01)

averted <- cbind(total_cml[1], total_cml[-1] * (rl01$Cml/100) * 306.8e6)
averted_2p5 <- cbind(total_2p5[1], total_2p5[-1] * (rl01$Cml/100) * 306.8e6)
averted_97p5 <- cbind(total_97p5[1], total_97p5[-1] * (rl01$Cml/100) * 306.8e6)

colnames(averted) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
a <- melt(averted, id.vars = c("team"), variable.name = "Averted")
names(a) <- c("Team", 'Scenario', "Averted")

a_2p5 <- melt(averted_2p5, id.vars = c("team"), variable.name = "Averted_Low")
names(a_2p5) <- c("Team", 'Scenario', "Averted_Low")

a_97p5 <- melt(averted_97p5, id.vars = c("team"), variable.name = "Averted_Upper")
names(a_97p5) <- c("Team", 'Scenario', "Averted_Upper")

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
a$Averted_Low <- a_2p5$Averted_Low
a$Averted_Upper <- a_97p5$Averted_Upper

name <- cml_sum$team
cml_sum <- as.data.frame(t(as.matrix(cml_sum[2:9])))
colnames(cml_sum) <- name

name <- cml_2p5$team
cml_2p5 <- as.data.frame(t(as.matrix(cml_2p5[2:9])))
colnames(cml_2p5) <- name

name <- cml_97p5$team
cml_97p5 <- as.data.frame(t(as.matrix(cml_97p5[2:9])))
colnames(cml_97p5) <- name

for_csv <- data.frame(COL = paste0(round(as.numeric(as.character(cml_sum$COL)), 1), "% (",
                                   round(as.numeric(as.character(cml_2p5$COL)), 1), "%, ",
                                   round(as.numeric(as.character(cml_97p5$COL)), 1), "%)"),
                      COL2 = paste0(round(as.numeric(as.character(cml_sum$COL2)), 1), "% (",
                                    round(as.numeric(as.character(cml_2p5$COL2)), 1), "%, ",
                                    round(as.numeric(as.character(cml_97p5$COL2)), 1), "%)"),
                      IMP = paste0(round(as.numeric(as.character(cml_sum$IMP)), 1), "% (",
                                   round(as.numeric(as.character(cml_2p5$IMP)), 1), "%, ",
                                   round(as.numeric(as.character(cml_97p5$IMP)), 1), "%)"),
                      NEU3 = paste0(round(as.numeric(as.character(cml_sum$NEU3)), 1), "% (",
                                    round(as.numeric(as.character(cml_2p5$NEU3)), 1), "%, ",
                                    round(as.numeric(as.character(cml_97p5$NEU3)), 1), "%)"),
                      UTA = paste0(round(as.numeric(as.character(cml_sum$UTA)), 1), "% (",
                                   round(as.numeric(as.character(cml_2p5$UTA)), 1), "%, ",
                                   round(as.numeric(as.character(cml_97p5$UTA)), 1), "%)"),
                      UVA = paste0(round(as.numeric(as.character(cml_sum$UVA)), 1), "% (",
                                   round(as.numeric(as.character(cml_2p5$UVA)), 1), "%, ",
                                   round(as.numeric(as.character(cml_97p5$UVA)), 1), "%)"),
                      ENS = paste0(round(as.numeric(as.character(cml_sum$ENS)), 1), "% (",
                                   round(as.numeric(as.character(cml_2p5$ENS)), 1), "%, ",
                                   round(as.numeric(as.character(cml_97p5$ENS)), 1), "%)"))

write.csv(for_csv, "2009_illness_table.csv")

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
  geom_text(aes(y = Team, x = Scenario, label=paste0(round(Averted/1e6, 1), "M (",
                                                     round((Averted_Upper/1e6), 1), "M, ",
                                                     round((Averted_Low/1e6), 1), "M)")), size=3) +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Averted') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom',
        legend.key.width = unit(1,"cm"),
        legend.text = )+
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

t_2p5 <- melt(total_2p5, id.vars = c("team"), variable.name = "Reduction_2p5")
colnames(t_2p5) <- c("Team", "Scenario", "Reduction_2p5")

t_97p5 <- melt(total_97p5, id.vars = c("team"), variable.name = "Reduction_97p5")
colnames(t_97p5) <- c("Team", "Scenario", "Reduction_97p5")

t$Reduction_Low <- t_97p5$Reduction_97p5 * 100
t$Reduction_Up <- t_2p5$Reduction_2p5 *100

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
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 1), "% (",
                                                       round(Reduction_Low, 1), "%, ",
                                                       round(Reduction_Up, 1), "%) [",
                                                       Rank, "]")), size=3) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Reduction, %') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom')+
  ggtitle("b. Percent change [rank] in illness burden")


r <- read_overall_mean(scn = "RL01_", 1)
peak.weeks <- get_peaks(r)
peak.weeks$Scenario <- "RL01"
# cml <- cml[which(cml$Bin == "sm.mean"),]
total_cml <- cml[-1]
peak_mag <- total_cml
peak_mag$team <- factor(x = peak_mag$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
peak_mag <- rbind(peak_mag,
                  c("ENS", "sm.mean", mean(peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")])),
                  c("ENS", "sm.perc2p5", mean(peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")])),
                  c("ENS", "sm.perc97p5", mean(peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")])))
peak_mag$Peak.Magnitude <- as.numeric(peak_mag$Peak.Magnitude)

total_cml <- peak_mag[which(peak_mag$Bin == "sm.mean"),]

r <- read_overall_mean(scn = "RL02_", 1)
p <- get_peaks(r)
p$Scenario <- "RL02"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml2 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml2 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml2 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL03_", 1)
p <- get_peaks(r)
p$Scenario <- "RL03"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml3 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml3 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml3 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL04_", 1)
p <- get_peaks(r)
p$Scenario <- "RL04"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml4 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml4 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml4 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL05_", 1)
p <- get_peaks(r)
p$Scenario <- "RL05"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml5 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml5 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml5 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL06_", 1)
p <- get_peaks(r)
p$Scenario <- "RL06"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml6 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml6 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml6 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL07_", 1)
p <- get_peaks(r)
p$Scenario <- "RL07"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml7 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml7 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml7 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL08_", 1)
p <- get_peaks(r)
p$Scenario <- "RL08"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml8 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml8 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml8 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

team <- as.character(total_cml$team)

total_cml <- cbind(factor(x = c(team), 
                          levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS")),
                   as.data.frame(lapply(total_cml[-(1:2)], as.numeric)))
names(total_cml)[1] <- "team"

# total_cml <- rbind(total_cml, c("ENS", colMeans(total_cml[-1])))

total_cml <- cbind( total_cml$team,
                    as.data.frame(lapply(total_cml[-1], as.numeric)))

rank <- as.data.frame(rbind(rank(-total_cml[1,3:9]),
                                  rank(-total_cml[2,3:9]),
                                  rank(-total_cml[3,3:9]),
                                  rank(-total_cml[4,3:9]),
                                  rank(-total_cml[5,3:9]),
                                  rank(-total_cml[6,3:9]),
                                  rank(-total_cml[7,3:9]))
                      )

rank$team <- factor(x = total_cml$`total_cml$team`, 
                    levels = c("COL", "COL2", "IMP", "NEU3", 
                               "UTA", "UVA", "ENS"))

names(rank)[8] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Reduction")

total_cml <- total_cml[-2]
colnames(total_cml) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
t <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
colnames(t) <- c("Team", "Scenario", "Reduction")
t$Reduction <- t$Reduction*100
t$Rank <- r$value

t_2p5 <- melt(total_2p5, id.vars = c("team"), variable.name = "Reduction")
colnames(t_2p5) <- c("Team", "Scenario", "Reduction")
t_97p5 <- melt(total_97p5, id.vars = c("team"), variable.name = "Reduction")
colnames(t_97p5) <- c("Team", "Scenario", "Reduction")
t$Reduction_Up <- t_2p5$Reduction *100
t$Reduction_Low <- t_97p5$Reduction *100

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
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 1), "% (",
                                                       round(Reduction_Low, 1), "%, ",
                                                       round(Reduction_Up, 1), "%) [", 
                                                       Rank, "]")), size=3) +
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
  ggtitle("c. Percent change [rank] in peak illnesses")

p4
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


png("fig3_ci_2009.png",width = 1000, height=1000)
ggarrange(p6, p3, p4, p5, nrow=4, ncol=1)#,common.legend = TRUE, legend="bottom")
dev.off()


#Supp figure hospitalizations - rank order figures----------------------------

long_RL01 <- read_overall_mean(scn = "RL01_", 2)
total_cml <- cml[which(cml$Bin == "sm.mean"),][-3:-4]
cml_2p5 <- cml[which(cml$Bin == "sm.perc2p5"),][-3:-4]
cml_97p5 <- cml[which(cml$Bin == "sm.perc97p5"),][-3:-4]
cml_sum <- total_cml$Cml

read_overall_mean(scn = "RL02_", 2)
total_cml$Cml2 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum <- data.frame(team = cml$team[which(cml$Bin == "sm.mean")],
                      RL01 = cml_sum,
                      RL02 = cml$Cml[which(cml$Bin == "sm.mean")])
cml_2p5 <- data.frame(team = cml_2p5$team,
                      RL01 = cml_2p5$Cml,
                      RL02 = cml$Cml[which(cml$Bin == "sm.perc2p5")])
cml_97p5 <- data.frame(team = cml_97p5$team,
                       RL01 = cml_97p5$Cml,
                       RL02 = cml$Cml[which(cml$Bin == "sm.perc97p5")])

read_overall_mean(scn = "RL03_", 2)
total_cml$Cml3 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL03 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL03 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL03 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "RL04_", 2)
total_cml$Cml4 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL04 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL04 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL04 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "RL05_", 2)
total_cml$Cml5 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL05 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL05 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL05 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "RL06_", 2)
total_cml$Cml6 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL06 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL06 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL06 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "RL07_", 2)
total_cml$Cml7 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL07 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL07 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL07 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "RL08_", 2)
total_cml$Cml8 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL08 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL08 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL08 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

total_cml$team <- factor(x = total_cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml_sum$team <- factor(x = cml_sum$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml_2p5$team <- factor(x = cml_2p5$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml_97p5$team <- factor(x = cml_97p5$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))

cml_sum <- rbind(cml_sum, c("ENS", as.numeric(colMeans(cml_sum[2:9]))))
cml_2p5 <- rbind(cml_2p5, c("ENS", as.numeric(colMeans(cml_2p5[2:9]))))
cml_97p5 <- rbind(cml_97p5, c("ENS", as.numeric(colMeans(cml_97p5[2:9]))))

total_cml <- rbind(total_cml,
                   c(as.numeric(cml_sum[7,2]),
                     "ENS", 
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL02[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL03[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL04[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL05[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL06[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL07[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL08[7])) / as.numeric(cml_sum$RL01[7])
                   )
)


rl01 <- total_cml[1:2]
rl01$Cml <- as.numeric(rl01$Cml)
total_cml <- total_cml[-1]
total_cml <-  cbind(total_cml[1], lapply(total_cml[-1], as.numeric))
cml_2p5 <-  cbind(cml_2p5[1], lapply(cml_2p5[-1], as.numeric))
cml_97p5 <-  cbind(cml_97p5[1], lapply(cml_97p5[-1], as.numeric))

total_2p5 <- data.frame(team = cml_2p5$team,
                        Cml2 = (cml_2p5$RL01 - cml_2p5$RL02) / cml_2p5$RL01,
                        Cml3 = (cml_2p5$RL01 - cml_2p5$RL03) / cml_2p5$RL01,
                        Cml4 = (cml_2p5$RL01 - cml_2p5$RL04) / cml_2p5$RL01,
                        Cml5 = (cml_2p5$RL01 - cml_2p5$RL05) / cml_2p5$RL01,
                        Cml6 = (cml_2p5$RL01 - cml_2p5$RL06) / cml_2p5$RL01,
                        Cml7 = (cml_2p5$RL01 - cml_2p5$RL07) / cml_2p5$RL01,
                        Cml8 = (cml_2p5$RL01 - cml_2p5$RL08) / cml_2p5$RL01)
total_97p5 <- data.frame(team = cml_97p5$team,
                         Cml2 = (cml_97p5$RL01 - cml_97p5$RL02) / cml_97p5$RL01,
                         Cml3 = (cml_97p5$RL01 - cml_97p5$RL03) / cml_97p5$RL01,
                         Cml4 = (cml_97p5$RL01 - cml_97p5$RL04) / cml_97p5$RL01,
                         Cml5 = (cml_97p5$RL01 - cml_97p5$RL05) / cml_97p5$RL01,
                         Cml6 = (cml_97p5$RL01 - cml_97p5$RL06) / cml_97p5$RL01,
                         Cml7 = (cml_97p5$RL01 - cml_97p5$RL07) / cml_97p5$RL01,
                         Cml8 = (cml_97p5$RL01 - cml_97p5$RL08) / cml_97p5$RL01)

averted <- cbind(total_cml[1], total_cml[-1] * (rl01$Cml * 100000))
averted_2p5 <- cbind(total_2p5[1], total_2p5[-1] * (rl01$Cml * 100000))
averted_97p5 <- cbind(total_97p5[1], total_97p5[-1] * (rl01$Cml * 100000))

colnames(averted) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
a <- melt(averted, id.vars = c("team"), variable.name = "Averted")
names(a) <- c("Team", 'Scenario', "Averted")

a_2p5 <- melt(averted_2p5, id.vars = c("team"), variable.name = "Averted_Low")
names(a_2p5) <- c("Team", 'Scenario', "Averted_Low")

a_97p5 <- melt(averted_97p5, id.vars = c("team"), variable.name = "Averted_Upper")
names(a_97p5) <- c("Team", 'Scenario', "Averted_Upper")

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
a$Averted_Low <- a_2p5$Averted_Low
a$Averted_Upper <- a_97p5$Averted_Upper

name <- cml_sum$team
cml_sum <- as.data.frame(t(as.matrix(cml_sum[2:9])))
colnames(cml_sum) <- name

name <- cml_2p5$team
cml_2p5 <- as.data.frame(t(as.matrix(cml_2p5[2:9])))
colnames(cml_2p5) <- name

name <- cml_97p5$team
cml_97p5 <- as.data.frame(t(as.matrix(cml_97p5[2:9])))
colnames(cml_97p5) <- name

for_csv <- data.frame(COL = paste0(round(as.numeric(as.character(cml_sum$COL)), 0), " (",
                                   round(as.numeric(as.character(cml_2p5$COL)), 0), ", ",
                                   round(as.numeric(as.character(cml_97p5$COL)), 0), ")"),
                      COL2 = paste0(round(as.numeric(as.character(cml_sum$COL2)), 0), " (",
                                    round(as.numeric(as.character(cml_2p5$COL2)), 0), ", ",
                                    round(as.numeric(as.character(cml_97p5$COL2)), 0), ")"),
                      IMP = paste0(round(as.numeric(as.character(cml_sum$IMP)), 0), " (",
                                   round(as.numeric(as.character(cml_2p5$IMP)), 0), ", ",
                                   round(as.numeric(as.character(cml_97p5$IMP)), 0), ")"),
                      NEU3 = paste0(round(as.numeric(as.character(cml_sum$NEU3)), 0), " (",
                                    round(as.numeric(as.character(cml_2p5$NEU3)), 0), ", ",
                                    round(as.numeric(as.character(cml_97p5$NEU3)), 0), ")"),
                      UTA = paste0(round(as.numeric(as.character(cml_sum$UTA)), 0), " (",
                                   round(as.numeric(as.character(cml_2p5$UTA)), 0), ", ",
                                   round(as.numeric(as.character(cml_97p5$UTA)), 0), ")"),
                      UVA = paste0(round(as.numeric(as.character(cml_sum$UVA)), 0), " (",
                                   round(as.numeric(as.character(cml_2p5$UVA)), 0), ", ",
                                   round(as.numeric(as.character(cml_97p5$UVA)), 0), ")"),
                      ENS = paste0(round(as.numeric(as.character(cml_sum$ENS)), 0), " (",
                                   round(as.numeric(as.character(cml_2p5$ENS)), 0), ", ",
                                   round(as.numeric(as.character(cml_97p5$ENS)), 0), ")"))

write.csv(for_csv, "2009_hospitalization_table.csv")

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
  geom_text(aes(y = Team, x = Scenario, label=paste0(round(Averted/1e3), "K (",
                                                     round((Averted_Upper/1e3)), "K, ",
                                                     round((Averted_Low/1e3)), "K)")), size=3) +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Averted') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom',
        legend.key.width = unit(1.2,"cm"))+
  ggtitle("a. Averted hospitalizations")


h6

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

t_2p5 <- melt(total_2p5, id.vars = c("team"), variable.name = "Reduction_2p5")
colnames(t_2p5) <- c("Team", "Scenario", "Reduction_2p5")

t_97p5 <- melt(total_97p5, id.vars = c("team"), variable.name = "Reduction_97p5")
colnames(t_97p5) <- c("Team", "Scenario", "Reduction_97p5")

t$Reduction_Low <- t_97p5$Reduction_97p5 * 100
t$Reduction_Up <- t_2p5$Reduction_2p5 *100

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
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 1), "% (",
                                                       round(Reduction_Low, 1), "%, ",
                                                       round(Reduction_Up, 1), "%) [",
                                                       Rank, "]")), size=3) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Reduction, %') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom')+
  ggtitle("b. Percent change [rank] in hospitalization burden")

h3

r <- read_overall_mean(scn = "RL01_", 2)
peak.weeks <- get_peaks(r)
peak.weeks$Scenario <- "RL01"
# cml <- cml[which(cml$Bin == "sm.mean"),]
total_cml <- cml[-1]
peak_mag <- total_cml
peak_mag$team <- factor(x = peak_mag$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
peak_mag <- rbind(peak_mag,
                  c("ENS", "sm.mean", mean(peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")])),
                  c("ENS", "sm.perc2p5", mean(peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")])),
                  c("ENS", "sm.perc97p5", mean(peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")])))
peak_mag$Peak.Magnitude <- as.numeric(peak_mag$Peak.Magnitude)

total_cml <- peak_mag[which(peak_mag$Bin == "sm.mean"),]

r <- read_overall_mean(scn = "RL02_", 2)
p <- get_peaks(r)
p$Scenario <- "RL02"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml2 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml2 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml2 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL03_", 2)
p <- get_peaks(r)
p$Scenario <- "RL03"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml3 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml3 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml3 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL04_", 2)
p <- get_peaks(r)
p$Scenario <- "RL04"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml4 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml4 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml4 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL05_", 2)
p <- get_peaks(r)
p$Scenario <- "RL05"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml5 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml5 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml5 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL06_", 2)
p <- get_peaks(r)
p$Scenario <- "RL06"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml6 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml6 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml6 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL07_", 2)
p <- get_peaks(r)
p$Scenario <- "RL07"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml7 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml7 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml7 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL08_", 2)
p <- get_peaks(r)
p$Scenario <- "RL08"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml8 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml8 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml8 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

team <- as.character(total_cml$team)

total_cml <- cbind(factor(x = c(team), 
                          levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS")),
                   as.data.frame(lapply(total_cml[-(1:2)], as.numeric)))
names(total_cml)[1] <- "team"

# total_cml <- rbind(total_cml, c("ENS", colMeans(total_cml[-1])))

total_cml <- cbind( total_cml$team,
                    as.data.frame(lapply(total_cml[-1], as.numeric)))

rank <- as.data.frame(rbind(rank(-total_cml[1,3:9]),
                            rank(-total_cml[2,3:9]),
                            rank(-total_cml[3,3:9]),
                            rank(-total_cml[4,3:9]),
                            rank(-total_cml[5,3:9]),
                            rank(-total_cml[6,3:9]),
                            rank(-total_cml[7,3:9]))
)

rank$team <- factor(x = total_cml$`total_cml$team`, 
                    levels = c("COL", "COL2", "IMP", "NEU3", 
                               "UTA", "UVA", "ENS"))

names(rank)[8] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Reduction")

total_cml <- total_cml[-2]
colnames(total_cml) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
t <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
colnames(t) <- c("Team", "Scenario", "Reduction")
t$Reduction <- t$Reduction*100
t$Rank <- r$value

t_2p5 <- melt(total_2p5, id.vars = c("team"), variable.name = "Reduction")
colnames(t_2p5) <- c("Team", "Scenario", "Reduction")
t_97p5 <- melt(total_97p5, id.vars = c("team"), variable.name = "Reduction")
colnames(t_97p5) <- c("Team", "Scenario", "Reduction")
t$Reduction_Up <- t_2p5$Reduction *100
t$Reduction_Low <- t_97p5$Reduction *100

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
  geom_text(aes(y = Team, x = Scenario, label =  paste0(round(Reduction, 1), "% (",
                                                        round(Reduction_Low, 1), "%, ",
                                                        round(Reduction_Up, 1), "%) [", 
                                                        Rank, "]")), size=3) +
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
  ggtitle("c. Percent change [rank] in peak hospitalizations")

h4

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


png("fig3h_ci_2009.png",width = 1000, height=1000)
ggarrange(h6, h3, h4, h5, nrow=4, ncol=1)#,common.legend = TRUE, legend="bottom")
dev.off()

#Supp figure deaths - rank order figures----------------------------

long_RL01 <- read_overall_mean(scn = "RL01_", 3)
total_cml <- cml[which(cml$Bin == "sm.mean"),][-3:-4]
cml_2p5 <- cml[which(cml$Bin == "sm.perc2p5"),][-3:-4]
cml_97p5 <- cml[which(cml$Bin == "sm.perc97p5"),][-3:-4]
cml_sum <- total_cml$Cml

read_overall_mean(scn = "RL02_", 3)
total_cml$Cml2 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum <- data.frame(team = cml$team[which(cml$Bin == "sm.mean")],
                      RL01 = cml_sum,
                      RL02 = cml$Cml[which(cml$Bin == "sm.mean")])
cml_2p5 <- data.frame(team = cml_2p5$team,
                      RL01 = cml_2p5$Cml,
                      RL02 = cml$Cml[which(cml$Bin == "sm.perc2p5")])
cml_97p5 <- data.frame(team = cml_97p5$team,
                       RL01 = cml_97p5$Cml,
                       RL02 = cml$Cml[which(cml$Bin == "sm.perc97p5")])

read_overall_mean(scn = "RL03_", 3)
total_cml$Cml3 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL03 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL03 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL03 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "RL04_", 3)
total_cml$Cml4 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL04 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL04 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL04 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "RL05_", 3)
total_cml$Cml5 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL05 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL05 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL05 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "RL06_", 3)
total_cml$Cml6 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL06 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL06 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL06 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "RL07_", 3)
total_cml$Cml7 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL07 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL07 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL07 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "RL08_", 3)
total_cml$Cml8 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$RL08 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$RL08 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$RL08 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

total_cml$team <- factor(x = total_cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml_sum$team <- factor(x = cml_sum$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml_2p5$team <- factor(x = cml_2p5$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml_97p5$team <- factor(x = cml_97p5$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))

cml_sum <- rbind(cml_sum, c("ENS", as.numeric(colMeans(cml_sum[2:9]))))
cml_2p5 <- rbind(cml_2p5, c("ENS", as.numeric(colMeans(cml_2p5[2:9]))))
cml_97p5 <- rbind(cml_97p5, c("ENS", as.numeric(colMeans(cml_97p5[2:9]))))

total_cml <- rbind(total_cml,
                   c(as.numeric(cml_sum[7,2]),
                     "ENS", 
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL02[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL03[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL04[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL05[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL06[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL07[7])) / as.numeric(cml_sum$RL01[7]),
                     (as.numeric(cml_sum$RL01[7]) - as.numeric(cml_sum$RL08[7])) / as.numeric(cml_sum$RL01[7])
                   )
)


rl01 <- total_cml[1:2]
rl01$Cml <- as.numeric(rl01$Cml)
total_cml <- total_cml[-1]
total_cml <-  cbind(total_cml[1], lapply(total_cml[-1], as.numeric))
cml_2p5 <-  cbind(cml_2p5[1], lapply(cml_2p5[-1], as.numeric))
cml_97p5 <-  cbind(cml_97p5[1], lapply(cml_97p5[-1], as.numeric))

total_2p5 <- data.frame(team = cml_2p5$team,
                        Cml2 = (cml_2p5$RL01 - cml_2p5$RL02) / cml_2p5$RL01,
                        Cml3 = (cml_2p5$RL01 - cml_2p5$RL03) / cml_2p5$RL01,
                        Cml4 = (cml_2p5$RL01 - cml_2p5$RL04) / cml_2p5$RL01,
                        Cml5 = (cml_2p5$RL01 - cml_2p5$RL05) / cml_2p5$RL01,
                        Cml6 = (cml_2p5$RL01 - cml_2p5$RL06) / cml_2p5$RL01,
                        Cml7 = (cml_2p5$RL01 - cml_2p5$RL07) / cml_2p5$RL01,
                        Cml8 = (cml_2p5$RL01 - cml_2p5$RL08) / cml_2p5$RL01)
total_97p5 <- data.frame(team = cml_97p5$team,
                         Cml2 = (cml_97p5$RL01 - cml_97p5$RL02) / cml_97p5$RL01,
                         Cml3 = (cml_97p5$RL01 - cml_97p5$RL03) / cml_97p5$RL01,
                         Cml4 = (cml_97p5$RL01 - cml_97p5$RL04) / cml_97p5$RL01,
                         Cml5 = (cml_97p5$RL01 - cml_97p5$RL05) / cml_97p5$RL01,
                         Cml6 = (cml_97p5$RL01 - cml_97p5$RL06) / cml_97p5$RL01,
                         Cml7 = (cml_97p5$RL01 - cml_97p5$RL07) / cml_97p5$RL01,
                         Cml8 = (cml_97p5$RL01 - cml_97p5$RL08) / cml_97p5$RL01)

averted <- cbind(total_cml[1], total_cml[-1] * (rl01$Cml * 100000))
averted_2p5 <- cbind(total_2p5[1], total_2p5[-1] * (rl01$Cml * 100000))
averted_97p5 <- cbind(total_97p5[1], total_97p5[-1] * (rl01$Cml * 100000))

colnames(averted) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
a <- melt(averted, id.vars = c("team"), variable.name = "Averted")
names(a) <- c("Team", 'Scenario', "Averted")

a_2p5 <- melt(averted_2p5, id.vars = c("team"), variable.name = "Averted_Low")
names(a_2p5) <- c("Team", 'Scenario', "Averted_Low")

a_97p5 <- melt(averted_97p5, id.vars = c("team"), variable.name = "Averted_Upper")
names(a_97p5) <- c("Team", 'Scenario', "Averted_Upper")

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
a$Averted_Low <- a_2p5$Averted_Low
a$Averted_Upper <- a_97p5$Averted_Upper

name <- cml_sum$team
cml_sum <- as.data.frame(t(as.matrix(cml_sum[2:9])))
colnames(cml_sum) <- name

name <- cml_2p5$team
cml_2p5 <- as.data.frame(t(as.matrix(cml_2p5[2:9])))
colnames(cml_2p5) <- name

name <- cml_97p5$team
cml_97p5 <- as.data.frame(t(as.matrix(cml_97p5[2:9])))
colnames(cml_97p5) <- name

for_csv <- data.frame(COL = paste0(round(as.numeric(as.character(cml_sum$COL)), 0), " (",
                                   round(as.numeric(as.character(cml_2p5$COL)), 0), ", ",
                                   round(as.numeric(as.character(cml_97p5$COL)), 0), ")"),
                      COL2 = paste0(round(as.numeric(as.character(cml_sum$COL2)), 0), " (",
                                    round(as.numeric(as.character(cml_2p5$COL2)), 0), ", ",
                                    round(as.numeric(as.character(cml_97p5$COL2)), 0), ")"),
                      IMP = paste0(round(as.numeric(as.character(cml_sum$IMP)), 0), " (",
                                   round(as.numeric(as.character(cml_2p5$IMP)), 0), ", ",
                                   round(as.numeric(as.character(cml_97p5$IMP)), 0), ")"),
                      NEU3 = paste0(round(as.numeric(as.character(cml_sum$NEU3)), 0), " (",
                                    round(as.numeric(as.character(cml_2p5$NEU3)), 0), ", ",
                                    round(as.numeric(as.character(cml_97p5$NEU3)), 0), ")"),
                      UTA = paste0(round(as.numeric(as.character(cml_sum$UTA)), 0), " (",
                                   round(as.numeric(as.character(cml_2p5$UTA)), 0), ", ",
                                   round(as.numeric(as.character(cml_97p5$UTA)), 0), ")"),
                      UVA = paste0(round(as.numeric(as.character(cml_sum$UVA)), 0), " (",
                                   round(as.numeric(as.character(cml_2p5$UVA)), 0), ", ",
                                   round(as.numeric(as.character(cml_97p5$UVA)), 0), ")"),
                      ENS = paste0(round(as.numeric(as.character(cml_sum$ENS)), 0), " (",
                                   round(as.numeric(as.character(cml_2p5$ENS)), 0), ", ",
                                   round(as.numeric(as.character(cml_97p5$ENS)), 0), ")"))

write.csv(for_csv, "2009_death_table.csv")

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
  geom_text(aes(y = Team, x = Scenario, label=paste0(round(Averted/1e3, 0), "K (",
                                                     round((Averted_Upper/1e3), 0), "K, ",
                                                     round((Averted_Low/1e3), 0), "K)")), size=3) +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Averted') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom',
        legend.key.width = unit(1,"cm"))+
  ggtitle("a. Averted deaths")

d6

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

t_2p5 <- melt(total_2p5, id.vars = c("team"), variable.name = "Reduction_2p5")
colnames(t_2p5) <- c("Team", "Scenario", "Reduction_2p5")

t_97p5 <- melt(total_97p5, id.vars = c("team"), variable.name = "Reduction_97p5")
colnames(t_97p5) <- c("Team", "Scenario", "Reduction_97p5")

t$Reduction_Low <- t_97p5$Reduction_97p5 * 100
t$Reduction_Up <- t_2p5$Reduction_2p5 *100

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
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 1), "% (",
                                                       round(Reduction_Low, 1), "%, ",
                                                       round(Reduction_Up, 1), "%) [",
                                                       Rank, "]")), size=3) +
  # facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
  theme_minimal() + 
  labs(y = 'Model', x = 'Scenario', fill = 'Reduction, %') +
  theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=10, hjust = .5),
        axis.text.y = element_text(size=10), legend.position = 'bottom')+
  ggtitle("b. Percent change [rank] in death burden")

d3

r <- read_overall_mean(scn = "RL01_", 3)
peak.weeks <- get_peaks(r)
peak.weeks$Scenario <- "RL01"
# cml <- cml[which(cml$Bin == "sm.mean"),]
total_cml <- cml[-1]
peak_mag <- total_cml
peak_mag$team <- factor(x = peak_mag$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
peak_mag <- rbind(peak_mag,
                  c("ENS", "sm.mean", mean(peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")])),
                  c("ENS", "sm.perc2p5", mean(peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")])),
                  c("ENS", "sm.perc97p5", mean(peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")])))
peak_mag$Peak.Magnitude <- as.numeric(peak_mag$Peak.Magnitude)

total_cml <- peak_mag[which(peak_mag$Bin == "sm.mean"),]

r <- read_overall_mean(scn = "RL02_", 3)
p <- get_peaks(r)
p$Scenario <- "RL02"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml2 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml2 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml2 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL03_", 3)
p <- get_peaks(r)
p$Scenario <- "RL03"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml3 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml3 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml3 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL04_", 3)
p <- get_peaks(r)
p$Scenario <- "RL04"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml4 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml4 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml4 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL05_", 3)
p <- get_peaks(r)
p$Scenario <- "RL05"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml5 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml5 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml5 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL06_", 3)
p <- get_peaks(r)
p$Scenario <- "RL06"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml6 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml6 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml6 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL07_", 3)
p <- get_peaks(r)
p$Scenario <- "RL07"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml7 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml7 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml7 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

r <- read_overall_mean(scn = "RL08_", 3)
p <- get_peaks(r)
p$Scenario <- "RL08"
peak.weeks <- rbind(peak.weeks, p)
cml$team <- factor(x = cml$team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))
cml <- rbind(cml,
             c(0, "ENS", "sm.mean", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.mean")])),
             c(0, "ENS", "sm.perc2p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")])),
             c(0, "ENS", "sm.perc97p5", mean(cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")])))
cml$Peak.Magnitude <- as.numeric(cml$Peak.Magnitude)

total_cml$Cml8 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.mean")] 
total_2p5$Cml8 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")] - 
                     cml$Peak.Magnitude[which(cml$Bin == "sm.perc2p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc2p5")]
total_97p5$Cml8 <- (peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")] - 
                      cml$Peak.Magnitude[which(cml$Bin == "sm.perc97p5")]) / 
  peak_mag$Peak.Magnitude[which(peak_mag$Bin == "sm.perc97p5")]

team <- as.character(total_cml$team)

total_cml <- cbind(factor(x = c(team), 
                          levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS")),
                   as.data.frame(lapply(total_cml[-(1:2)], as.numeric)))
names(total_cml)[1] <- "team"

# total_cml <- rbind(total_cml, c("ENS", colMeans(total_cml[-1])))

total_cml <- cbind( total_cml$team,
                    as.data.frame(lapply(total_cml[-1], as.numeric)))

rank <- as.data.frame(rbind(rank(-total_cml[1,3:9]),
                            rank(-total_cml[2,3:9]),
                            rank(-total_cml[3,3:9]),
                            rank(-total_cml[4,3:9]),
                            rank(-total_cml[5,3:9]),
                            rank(-total_cml[6,3:9]),
                            rank(-total_cml[7,3:9]))
)

rank$team <- factor(x = total_cml$`total_cml$team`, 
                    levels = c("COL", "COL2", "IMP", "NEU3", 
                               "UTA", "UVA", "ENS"))

names(rank)[8] <- "team"
r <- melt(rank, id.vars = c("team"), variable.name = "Reduction")

total_cml <- total_cml[-2]
colnames(total_cml) <- c("team", "RL02", "RL03", "RL04", "RL05", "RL06", "RL07", "RL08")
t <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
colnames(t) <- c("Team", "Scenario", "Reduction")
t$Reduction <- t$Reduction*100
t$Rank <- r$value

t_2p5 <- melt(total_2p5, id.vars = c("team"), variable.name = "Reduction")
colnames(t_2p5) <- c("Team", "Scenario", "Reduction")
t_97p5 <- melt(total_97p5, id.vars = c("team"), variable.name = "Reduction")
colnames(t_97p5) <- c("Team", "Scenario", "Reduction")
t$Reduction_Up <- t_2p5$Reduction *100
t$Reduction_Low <- t_97p5$Reduction *100


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
  geom_text(aes(y = Team, x = Scenario, label = paste0(round(Reduction, 1), "% (",
                                                       round(Reduction_Low, 1), "%, ",
                                                       round(Reduction_Up, 1), "%) [", 
                                                       Rank, "]")), size=3) +
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
  ggtitle("c. Percent change [rank] in peak deaths")

d4 

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


png("fig3d_ci_2009.png",width = 1000, height=1000)
ggarrange(d6, d3, d4, d5, nrow=4, ncol=1)#,common.legend = TRUE, legend="bottom")
dev.off()

