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

#Functions -------------------------------------------------------------------------------------

setwd("C:/Users/ppf6/Desktop/GitHub/FluCode/comparison-scripts")

path <- "../model-forecasts/"


team.folder <- c("Columbia/COL/EPI1/", "Northeastern/EPI1/",  "UTAustin/", "UVirginia/", "Imperial/", "Columbia/COL2/EPI1/")
team.abbrev <- c("COL", "NEU", "UTA", "UVA", "IMP", "COL2")
file.types <-c("SymIllness", "Hosp", "Deaths", "AntiviralTX")
scn = "HP01_"
read_overall_mean <- function(scn){
  
  cols <- c("sm.mean", "sm.median", "sm.perc2p5", "sm.perc97p5")
  
  s <- read.csv(paste0(path,team.folder[1],scn,file.types[1],"_",team.abbrev[1],".csv"))
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
    
    f =  paste0(path,team.folder[i],scn,file.types[1],"_",team.abbrev[i],".csv")
    if(i == 5){
      f =  paste0(path,team.folder[i],scn,file.types[1],team.abbrev[i],".csv")
    }
    
    if(file.exists(f)){
      s <- read.csv(f)
      if(i == 4){
        s <- s[which(s$Location == "US National"),]
      }
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

#HP01 ------------------------------------------------------------------
long <- read_overall_mean(scn = "HP01_")
wide <- long %>% 
  gather(ens, ill, -(team:week)) %>%
  spread(key=team, value=ill)

ens <- c()

for(i in 1:nrow(wide)){
  ens <- c(ens, mean(as.double(wide[i,3:8])))
}

wide$ill <- ens

wide$Date <- as.Date("2020-06-24") + (wide$week*7)
wide$ens <- factor(x = wide$ens, levels = c("sm.mean", "sm.median","sm.perc2p5", "sm.perc97p5"))
wide$type <- as.character(ifelse(wide$ens == "sm.median", 2, 1))

wide$scenario = rep("HP01", nrow(wide))
full <- wide[-(3:8)]

# HP02 ----------------------------------------------------------------
long <- read_overall_mean(scn = "HP02_")
wide <- long %>% 
  gather(ens, ill, -(team:week)) %>%
  spread(key=team, value=ill)

ens <- c()

for(i in 1:nrow(wide)){
  ens <- c(ens, mean(as.double(wide[i,3:8])))
}

wide$ill <- ens

wide$Date <- as.Date("2020-06-24") + (wide$week*7)
wide$ens <- factor(x = wide$ens, levels = c("sm.mean", "sm.median","sm.perc2p5", "sm.perc97p5"))
wide$type <- as.character(ifelse(wide$ens == "sm.median", 2, 1))

wide$scenario = rep("HP02", nrow(wide))
full <- rbind(full, wide[-(3:8)])

# HP03 ----------------------------------------------------------------
long <- read_overall_mean(scn = "HP03_")
wide <- long %>% 
  gather(ens, ill, -(team:week)) %>%
  spread(key=team, value=ill)

ens <- c()

for(i in 1:nrow(wide)){
  ens <- c(ens, mean(as.double(wide[i,3:8])))
}

wide$ill <- ens

wide$Date <- as.Date("2020-06-24") + (wide$week*7)
wide$ens <- factor(x = wide$ens, levels = c("sm.mean", "sm.median","sm.perc2p5", "sm.perc97p5"))
wide$type <- as.character(ifelse(wide$ens == "sm.median", 2, 1))

wide$scenario = rep("HP03", nrow(wide))
full <- rbind(full, wide[-(3:8)])

# HP04 ----------------------------------------------------------------
long <- read_overall_mean(scn = "HP04_")
wide <- long %>% 
  gather(ens, ill, -(team:week)) %>%
  spread(key=team, value=ill)

ens <- c()

for(i in 1:nrow(wide)){
  ens <- c(ens, mean(as.double(wide[i,3:8])))
}

wide$ill <- ens

wide$Date <- as.Date("2020-06-24") + (wide$week*7)
wide$ens <- factor(x = wide$ens, levels = c("sm.mean", "sm.median","sm.perc2p5", "sm.perc97p5"))
wide$type <- as.character(ifelse(wide$ens == "sm.median", 2, 1))

wide$scenario = rep("HP04", nrow(wide))
full <- rbind(full, wide[-(3:8)])

# HP05 ----------------------------------------------------------------
long <- read_overall_mean(scn = "HP05_")
wide <- long %>% 
  gather(ens, ill, -(team:week)) %>%
  spread(key=team, value=ill)

ens <- c()

for(i in 1:nrow(wide)){
  ens <- c(ens, mean(as.double(wide[i,3:8])))
}

wide$ill <- ens

wide$Date <- as.Date("2020-06-24") + (wide$week*7)
wide$ens <- factor(x = wide$ens, levels = c("sm.mean", "sm.median","sm.perc2p5", "sm.perc97p5"))
wide$type <- as.character(ifelse(wide$ens == "sm.median", 2, 1))

wide$scenario = rep("HP05", nrow(wide))
full <- rbind(full, wide[-(3:8)])

# HP06 ----------------------------------------------------------------
long <- read_overall_mean(scn = "HP06_")
wide <- long %>% 
  gather(ens, ill, -(team:week)) %>%
  spread(key=team, value=ill)

ens <- c()

for(i in 1:nrow(wide)){
  ens <- c(ens, mean(as.double(wide[i,3:8])))
}

wide$ill <- ens

wide$Date <- as.Date("2020-06-24") + (wide$week*7)
wide$ens <- factor(x = wide$ens, levels = c("sm.mean", "sm.median","sm.perc2p5", "sm.perc97p5"))
wide$type <- as.character(ifelse(wide$ens == "sm.median", 2, 1))

wide$scenario = rep("HP06", nrow(wide))
full <- rbind(full, wide[-(3:8)])


# HP07 ----------------------------------------------------------------
long <- read_overall_mean(scn = "HP07_")
wide <- long %>% 
  gather(ens, ill, -(team:week)) %>%
  spread(key=team, value=ill)

ens <- c()

for(i in 1:nrow(wide)){
  ens <- c(ens, mean(as.double(wide[i,3:8])))
}

wide$ill <- ens

wide$Date <- as.Date("2020-06-24") + (wide$week*7)
wide$ens <- factor(x = wide$ens, levels = c("sm.mean", "sm.median","sm.perc2p5", "sm.perc97p5"))
wide$type <- as.character(ifelse(wide$ens == "sm.median", 2, 1))

wide$scenario = rep("HP07", nrow(wide))
full <- rbind(full, wide[-(3:8)])


# HP08 ----------------------------------------------------------------
long <- read_overall_mean(scn = "HP08_")
wide <- long %>% 
  gather(ens, ill, -(team:week)) %>%
  spread(key=team, value=ill)

ens <- c()

for(i in 1:nrow(wide)){
  ens <- c(ens, mean(as.double(wide[i,3:8])))
}

wide$ill <- ens

wide$Date <- as.Date("2020-06-24") + (wide$week*7)
wide$ens <- factor(x = wide$ens, levels = c("sm.mean", "sm.median","sm.perc2p5", "sm.perc97p5"))
wide$type <- as.character(ifelse(wide$ens == "sm.median", 2, 1))

wide$scenario = rep("HP08", nrow(wide))
full <- rbind(full, wide[-(3:8)])

# HP09 ----------------------------------------------------------------
long <- read_overall_mean(scn = "HP09_")
wide <- long %>% 
  gather(ens, ill, -(team:week)) %>%
  spread(key=team, value=ill)

ens <- c()

for(i in 1:nrow(wide)){
  ens <- c(ens, mean(as.double(wide[i,3:8])))
}

wide$ill <- ens

wide$Date <- as.Date("2020-06-24") + (wide$week*7)
wide$ens <- factor(x = wide$ens, levels = c("sm.mean", "sm.median","sm.perc2p5", "sm.perc97p5"))
wide$type <- as.character(ifelse(wide$ens == "sm.median", 2, 1))

wide$scenario = rep("HP09", nrow(wide))
full <- rbind(full, wide[-(3:8)])

# HP10 ----------------------------------------------------------------
long <- read_overall_mean(scn = "HP10_")
wide <- long %>% 
  gather(ens, ill, -(team:week)) %>%
  spread(key=team, value=ill)

ens <- c()

for(i in 1:nrow(wide)){
  ens <- c(ens, mean(as.double(wide[i,3:8])))
}

wide$ill <- ens

wide$Date <- as.Date("2020-06-24") + (wide$week*7)
wide$ens <- factor(x = wide$ens, levels = c("sm.mean", "sm.median","sm.perc2p5", "sm.perc97p5"))
wide$type <- as.character(ifelse(wide$ens == "sm.median", 2, 1))

wide$scenario = rep("HP10", nrow(wide))
full <- rbind(full, wide[-(3:8)])

# HP11 ----------------------------------------------------------------
long <- read_overall_mean(scn = "HP11_")
wide <- long %>% 
  gather(ens, ill, -(team:week)) %>%
  spread(key=team, value=ill)

ens <- c()

for(i in 1:nrow(wide)){
  ens <- c(ens, mean(as.double(wide[i,3:8])))
}

wide$ill <- ens

wide$Date <- as.Date("2020-06-24") + (wide$week*7)
wide$ens <- factor(x = wide$ens, levels = c("sm.mean", "sm.median","sm.perc2p5", "sm.perc97p5"))
wide$type <- as.character(ifelse(wide$ens == "sm.median", 2, 1))

wide$scenario = rep("HP11", nrow(wide))
full <- rbind(full, wide[-(3:8)])

# HP12 ----------------------------------------------------------------
long <- read_overall_mean(scn = "HP12_")
wide <- long %>% 
  gather(ens, ill, -(team:week)) %>%
  spread(key=team, value=ill)

ens <- c()

for(i in 1:nrow(wide)){
  ens <- c(ens, mean(as.double(wide[i,3:8])))
}

wide$ill <- ens

wide$Date <- as.Date("2020-06-24") + (wide$week*7)
wide$ens <- factor(x = wide$ens, levels = c("sm.mean", "sm.median","sm.perc2p5", "sm.perc97p5"))
wide$type <- as.character(ifelse(wide$ens == "sm.median", 2, 1))

wide$scenario = rep("HP12", nrow(wide))
full <- rbind(full, wide[-(3:8)])

# Figures -------------------------------------------------------------

f <- full[-5] %>% spread(key = ens, value=ill)

# f$scn <- ifelse(f$scenario == "HP01", "Base",
#                 ifelse(f$scenario == "HP02", "VX earlier", 
#                        ifelse(f$scenario == "HP03", "VX later",
#                               ifelse(f$scenario == "HP04", "SC trigger & early reopen",
#                                      ifelse(f$scenario == "HP05", "SC 10-28 & early reopen",
#                                             ifelse(f$scenario == "HP06", "SC trigger & late reopen",
#                                                    ifelse(f$scenario == "HP07", "SC trigger & ",
#                                                           iflese(f$scenario == "HP08", "AV",
#                                                                  ifelse(f$scenario == "HP09", "")))))))))
# f$scenario <- factor(f$scn, levels = unique(f$scn))

f$Date <- f$Date + 62

pal <- rainbow(12)
png("supp_fig4a.png",width = 850, height=500)
ggplot(data = f, aes(x = Date, group=scenario)) +
  geom_line(aes(y = sm.median, color=scenario),lwd = 1.5) +
  geom_ribbon(aes(ymin = sm.perc2p5, ymax = sm.perc97p5, fill=scenario), 
              alpha = 0.0, show.legend = FALSE)+
  ylab("% Incident Symptomatic Illness") +
  scale_fill_viridis(discrete=TRUE)+
  scale_color_viridis(discrete=TRUE)+
  # scale_fill_manual(values=pal) +
  # scale_color_manual(values=pal)+
  ggtitle("EPI1 Scenario - Ensemble for all Interventions")+
  theme_light()+
  scale_x_date(labels = date_format("%b"))
dev.off()


