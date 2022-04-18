library(ggplot2)
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
library(Hmisc)
library(dplyr)

#2009 -------------------------------------------------------------------------------------

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
pal <- pal_jco()#rainbow(6)#colour("smooth rainbow")
val_pal <- c(pal(4))

ggplot(data = wide, aes(x = Date, y = ill, group=ens)) +
  geom_line(aes(color=ens, lty=type),lwd = 1.5) +
  ylab("% Incident Symptomatic Illness") +
  scale_fill_manual(values = val_pal)+
  ggtitle("2009 Base Scenario - Quantile Ensemble")

# Mean probability distribution---------------------------------------------------------------


setwd("C:/Users/ppf6/Desktop/GitHub/FluCode/comparison-scripts")

path <- "../model-forecasts/"

team.folder <- c("Columbia/COL/RL/", "Northeastern/RL3/",  "UTAustin/", "UVirginia/", "Imperial/", 
                 "Columbia/COL2/RL/")
team.abbrev <- c("COL", "NEU", "UTA", "UVA", "IMP", "COL2")
file.types <-c("SymIllness", "Hosp", "Deaths", "AntiviralTX")
scn = "RL01_"
read_probs <- function(scn){
  
  s <- read.csv(paste0(path,team.folder[1],scn,file.types[1],"_",team.abbrev[1],".csv"))
  cols <- s$Bin[1:200]
  cols <-  factor(x = cols, levels = cols)
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
      if(ncol(s) > 58){
        s <- s[-1]
      }
      if(i == 2 & grepl("RL", scn, fixed=TRUE)){
        team.abbrev[i] = "NEU3"
      }
      
      h <- s[1:200,]
      
      symillness <- rbind.fill(symillness, 
                               data.frame(c(h,`team` = team.abbrev[i])))
    }
    
  }
  # symillness$Peak.Magnitude[which(is.na(symillness$Peak.Magnitude))] <-
  # symillness$PeakMagnitude[which(is.na(symillness$Peak.Magnitude))]
  
  # weeks <- paste0("Week",1:52)
  # symillness$Cml[which(is.na(symillness$Cml))] <- symillness$cml[which(is.na(symillness$Cml))]
  # cml <<- symillness[c("Cml", "team", "Bin", "Peak.Magnitude")]
  
  symillness <- symillness[-1:-4]
  symillness <- symillness[-2]
  symillness <- symillness[-55:-57]
  
  return(symillness)
}

l <- read_probs("RL01_")

ensemble <- data.frame(Bin = l$Bin[1:200])
team.abbrev <- c("COL", "NEU3", "UTA", "UVA", "IMP", "COL2")

for(w in 1:52){
  d <- l[which(l$team == team.abbrev[1]),]
  col = paste0("Week",w)
  weekW <- data.frame(Bin = d$Bin,
                      COL = d[,col])
  
  for(t in team.abbrev[2:6]){
    d <- l[which(l$team == t),]
    weekW[,t] <- d[,col]
  }
  
  e <- c()
  ##Making the ensemble
  for(r in 1:200){
    e <- c(e, mean(as.double(weekW[r,2:7])))
  }
  ensemble[,col] <- e
}

e <- ensemble %>% gather(key = Week, value=Probability, Week1:Week52)
e$Week <- as.integer(gsub("Week", "", e$Week))

b <- gsub("-.*","",e$Bin)
e$Bin <- as.double(gsub("\\[", "", b))
detach(package:plyr)
# mean <- weighted.mean(x = e$Bin[which(e$Week == 1)], w = e$Probability[which(e$Week == 1)])
lam <- e %>%
  group_by(Week) %>% 
  summarise(bins.mean = weighted.mean(Bin, Probability),
            bins.perc2p5 = wtd.quantile(x = Bin, probs = c(0.025), weights = Probability, normwt = TRUE),
            bins.perc97p5 = wtd.quantile(x = Bin, probs = c(0.975), weights = Probability, normwt = TRUE))

long <- lam %>% gather(key = "ens", value = "ill", bins.mean:bins.perc97p5)
long$type <- as.character(ifelse(long$ens == "bins.mean",3, 1))
names(long)[1] <- "week"

both <- rbind(wide[,c(1,2,9,11)],
              long)
# long <- long[which(long$team %in% c("ENS_MEAN", "ENS_MEDN")),]
pal <- pal_nejm()#rainbow(6)#colour("smooth rainbow")
val_pal <- c(pal(8)[2:4], rev(pal(8))[1:4] )
ggplot() + 
  scale_fill_gradientn(values=c(1, 0.0001, 0), colours=c("black", "black", "white"))+
  geom_raster(data=e, aes(x=Week, y = Bin, fill=Probability))+
  # ylim(0,19)+
  ggtitle("2009 - All Ensembles") +
  geom_line(data = wide, aes(x = week, y = ill, group=ens, color=ens,lty=type)
            ,lwd = 1.2) +
  geom_line(data = long, aes(x = week, y = ill, group=ens, color=ens,lty=type)
            ,lwd = 1.2) +
  scale_color_manual(values = val_pal)+
  ylab("% Incident Symptomatic Illness") 

#EPI1 ---------------------------------------
library(plyr)
team.folder <- c("Columbia/COL/EPI1/", "Northeastern/EPI1/",  "UTAustin/", "UVirginia/", "Imperial/", "Columbia/COL2/EPI1/")
team.abbrev <- c("COL", "NEU", "UTA", "UVA", "IMP", "COL2")
file.types <-c("SymIllness", "Hosp", "Deaths", "AntiviralTX")
scn = "HP01_"

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
pal <- pal_jco()#rainbow(6)#colour("smooth rainbow")
val_pal <- c(pal(4))

ggplot(data = wide, aes(x = Date, y = ill, group=ens)) +
  geom_line(aes(color=ens, lty=type),lwd = 1.5) +
  ylab("% Incident Symptomatic Illness") +
  scale_fill_manual(values = val_pal)+
  ggtitle("EPI1 Base Scenario - Quantile Ensemble")


#EPI 1 Probability ------------------------------------------------
l <- read_probs("HP01_")

ensemble <- data.frame(Bin = l$Bin[1:200])
team.abbrev <- c("COL", "NEU", "UTA", "UVA", "IMP", "COL2")

for(w in 1:52){
  d <- l[which(l$team == team.abbrev[1]),]
  col = paste0("Week",w)
  weekW <- data.frame(Bin = d$Bin,
                      COL = d[,col])
  
  for(t in team.abbrev[2:6]){
    d <- l[which(l$team == t),]
    weekW[,t] <- d[,col]
  }
  
  e <- c()
  ##Making the ensemble
  for(r in 1:200){
    e <- c(e, mean(as.double(weekW[r,2:7])))
  }
  ensemble[,col] <- e
}

e <- ensemble %>% gather(key = Week, value=Probability, Week1:Week52)
e$Week <- as.integer(gsub("Week", "", e$Week))

b <- gsub("-.*","",e$Bin)
e$Bin <- as.double(gsub("\\[", "", b))

# long <- long[which(long$team %in% c("ENS_MEAN", "ENS_MEDN")),]
detach(package:plyr)
lam <- e %>%
  group_by(Week) %>% 
  summarise(bins.mean = weighted.mean(Bin, Probability),
            bins.perc2p5 = wtd.quantile(x = Bin, probs = c(0.025), weights = Probability, normwt = TRUE),
            bins.perc97p5 = wtd.quantile(x = Bin, probs = c(0.975), weights = Probability, normwt = TRUE))

long <- lam %>% gather(key = "ens", value = "ill", bins.mean:bins.perc97p5)
long$type <- as.character(ifelse(long$ens == "bins.mean",3, 1))
names(long)[1] <- "week"

pal <- pal_nejm()#rainbow(6)#colour("smooth rainbow")
val_pal <- c(pal(8)[2:4], rev(pal(8))[1:4] )

ggplot() + 
  # scale_fill_gradient2(low = 'white', mid = 'black', midpoint = 0.5, high = "black", na.value = 'lightgrey')+
  scale_fill_gradientn(values=c(1, 0.01, 0), colours=c("black", "black", "white"))+
  geom_raster(data=e, aes(x=Week, y = Bin, fill=Probability))+
  # ylim(0,7.5)+
  ggtitle("EPI 1 - All Ensembles") +
  geom_line(data = wide, aes(x = week, y = ill, group=ens, color=ens,lty=type)
            ,lwd = 1.2) +
  geom_line(data = long, aes(x = week, y = ill, group=ens, color=ens,lty=type)
            ,lwd = 1.2) +
  scale_color_manual(values = val_pal)+
  ylab("% Incident Symptomatic Illness") 

# #EPI2 ---------------------------------------

library(plyr)
team.folder <- c("Columbia/COL/EPI2/", "Northeastern/EPI2/",  "UTAustin/", "UVirginia/", "Imperial/", "Columbia/COL2/EPI2/")
team.abbrev <- c("COL", "NEU", "UTA", "UVA", "IMP", "COL2")
file.types <-c("SymIllness", "Hosp", "Deaths", "AntiviralTX")
scn = "HP13_"

long <- read_overall_mean(scn = "HP13_")
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
pal <- pal_jco()#rainbow(6)#colour("smooth rainbow")
val_pal <- c(pal(4))

ggplot(data = wide, aes(x = Date, y = ill, group=ens)) +
  geom_line(aes(color=ens, lty=type),lwd = 1.5) +
  ylab("% Incident Symptomatic Illness") +
  scale_fill_manual(values = val_pal)+
  ggtitle("EPI 2 Base Scenario - Quantile Ensemble")

# EPI2 - Probability---------------------------------------------------
l <- read_probs("HP13_")

ensemble <- data.frame(Bin = l$Bin[1:200])
team.abbrev <- c("COL", "NEU", "UTA", "UVA", "IMP", "COL2")

for(w in 1:52){
  d <- l[which(l$team == team.abbrev[1]),]
  col = paste0("Week",w)
  weekW <- data.frame(Bin = d$Bin,
                      COL = d[,col])
  
  for(t in team.abbrev[2:6]){
    d <- l[which(l$team == t),]
    weekW[,t] <- d[,col]
  }
  
  e <- c()
  ##Making the ensemble
  for(r in 1:200){
    e <- c(e, mean(as.double(weekW[r,2:7])))
  }
  ensemble[,col] <- e
}

e <- ensemble %>% gather(key = Week, value=Probability, Week1:Week52)
e$Week <- as.integer(gsub("Week", "", e$Week))

b <- gsub("-.*","",e$Bin)
e$Bin <- as.double(gsub("\\[", "", b))

# long <- long[which(long$team %in% c("ENS_MEAN", "ENS_MEDN")),]
detach(package:plyr)
lam <- e %>%
  group_by(Week) %>% 
  summarise(bins.mean = weighted.mean(Bin, Probability),
            bins.perc2p5 = wtd.quantile(x = Bin, probs = c(0.025), weights = Probability, normwt = TRUE),
            bins.perc97p5 = wtd.quantile(x = Bin, probs = c(0.975), weights = Probability, normwt = TRUE))

long <- lam %>% gather(key = "ens", value = "ill", bins.mean:bins.perc97p5)
long$type <- as.character(ifelse(long$ens == "bins.mean",3, 1))
names(long)[1] <- "week"

pal <- pal_nejm()#rainbow(6)#colour("smooth rainbow")
val_pal <- c(pal(8)[2:4], rev(pal(8))[1:4] )

ggplot() + 
  scale_fill_gradientn(values=c(1, 0.001, 0), colours=c("black", "black", "white"))+
  geom_raster(data=e, aes(x=Week, y = Bin, fill=Probability))+
  # ylim(0,10)+
  ggtitle("EPI 2 - All Ensembles") +
  geom_line(data = wide, aes(x = week, y = ill, group=ens, color=ens,lty=type)
            ,lwd = 1.2) +
  geom_line(data = long, aes(x = week, y = ill, group=ens, color=ens,lty=type)
            ,lwd = 1.2) +
  ylab("% Incident Symptomatic Illness") +
  scale_color_manual(values = val_pal)





  


