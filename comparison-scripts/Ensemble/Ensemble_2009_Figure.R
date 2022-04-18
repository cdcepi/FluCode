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


team.folder <- c("Columbia/COL/RL/", "Northeastern/RL3/",  "UTAustin/", "UVirginia/", "Imperial/", "Columbia/COL2/RL/")
team.abbrev <- c("COL", "NEU", "UTA", "UVA", "IMP", "COL2")
file.types <-c("SymIllness", "Hosp", "Deaths", "AntiviralTX")
scn = "RL01_"
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

#RL01 ------------------------------------------------------------------
long <- read_overall_mean(scn = "RL01_")
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

wide$scenario = rep("RL01", nrow(wide))
full <- wide[-(3:8)]

# RL02 ----------------------------------------------------------------
long <- read_overall_mean(scn = "RL02_")
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

wide$scenario = rep("RL02", nrow(wide))
full <- rbind(full, wide[-(3:8)])

# RL03 ----------------------------------------------------------------
long <- read_overall_mean(scn = "RL03_")
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

wide$scenario = rep("RL03", nrow(wide))
full <- rbind(full, wide[-(3:8)])

# RL04 ----------------------------------------------------------------
long <- read_overall_mean(scn = "RL04_")
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

wide$scenario = rep("RL04", nrow(wide))
full <- rbind(full, wide[-(3:8)])

# RL05 ----------------------------------------------------------------
long <- read_overall_mean(scn = "RL05_")
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

wide$scenario = rep("RL05", nrow(wide))
full <- rbind(full, wide[-(3:8)])

# RL06 ----------------------------------------------------------------
long <- read_overall_mean(scn = "RL06_")
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

wide$scenario = rep("RL06", nrow(wide))
full <- rbind(full, wide[-(3:8)])


# RL07 ----------------------------------------------------------------
long <- read_overall_mean(scn = "RL07_")
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

wide$scenario = rep("RL07", nrow(wide))
full <- rbind(full, wide[-(3:8)])


# RL08 ----------------------------------------------------------------
long <- read_overall_mean(scn = "RL08_")
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

wide$scenario = rep("RL08", nrow(wide))
full <- rbind(full, wide[-(3:8)])

# Figures -------------------------------------------------------------

f <- full[-5] %>% spread(key = ens, value=ill)

f_sub <- f[which(f$scenario %in% c("RL05", "RL06", "RL08")),]

# f$scn <- ifelse(f$scenario == "RL01", "Base",
#                 ifelse(f$scenario == "RL02", "VX", 
#                        ifelse(f$scenario == "RL03", "AV",
#                               ifelse(f$scenario == "RL04", "VX; AV",
#                                      ifelse(f$scenario == "RL05", "SC trigger",
#                                             ifelse(f$scenario == "RL06", "SC fall",
#                                                    ifelse(f$scenario == "RL07", 
#                                                           "VX, AV, SC trigger",
#                                                           "VX, AV, SC fall")))))))
# f$scenario <- factor(f$scn, levels = unique(f$scn))

f$scenario <- gsub("RL0", "2009-", f$scenario)
png("fig4.png",width = 850, height=500)
ggplot(data = f, aes(x = Date, group=scenario)) +
  geom_line(aes(y = sm.median, color=scenario),lwd = 1.5) +
  # geom_ribbon(aes(ymin = sm.perc2p5, ymax = sm.perc97p5, fill=scenario), 
  #             alpha = 0.15, show.legend = FALSE)+
  ylab("% Incident Symptomatic Illness") +
  scale_fill_viridis(discrete=TRUE)+
  scale_color_viridis(discrete=TRUE)+
  ggtitle("")+
  theme_light()+
  scale_x_date(labels = date_format("%b"))
  # theme(panel.background = element_rect(fill = 'black'),
  #       legend.key = element_rect(color = "gray", fill = "black"))
dev.off()

print(f[which(f$sm.mean > f$sm.perc97p5),])

