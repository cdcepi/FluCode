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

#Functions---------------------------------------


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


#Figure 10 - rank order figures---------------------------------------

long_HP01 <- read_overall_mean(scn = "HP01_", 1)
total_cml <- cml[which(cml$Bin == "sm.mean"),][-3:-4]
cml_2p5 <- cml[which(cml$Bin == "sm.perc2p5"),][-3:-4]
cml_97p5 <- cml[which(cml$Bin == "sm.perc97p5"),][-3:-4]
cml_sum <- total_cml$Cml

read_overall_mean(scn = "HP02_", 1)
total_cml$Cml2 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum <- data.frame(team = cml$team[which(cml$Bin == "sm.mean")],
                      HP01 = cml_sum,
                      HP02 = cml$Cml[which(cml$Bin == "sm.mean")])
cml_2p5 <- data.frame(team = cml_2p5$team,
                      HP01 = cml_2p5$Cml,
                      HP02 = cml$Cml[which(cml$Bin == "sm.perc2p5")])
cml_97p5 <- data.frame(team = cml_97p5$team,
                       HP01 = cml_97p5$Cml,
                       HP02 = cml$Cml[which(cml$Bin == "sm.perc97p5")])

read_overall_mean(scn = "HP03_", 1)
total_cml$Cml3 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP03 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP03 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP03 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP04_", 1)
total_cml$Cml4 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP04 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP04 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP04 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP05_", 1)
total_cml$Cml5 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP05 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP05 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP05 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP06_", 1)
total_cml$Cml6 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP06 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP06 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP06 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP07_", 1)
total_cml$Cml7 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP07 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP07 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP07 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP08_", 1)
total_cml$Cml8 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP08 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP08 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP08 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP09_", 1)
total_cml$Cml9 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP09 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP09 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP09 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP10_", 1)
total_cml$Cml10 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP10 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP10 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP10 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP11_", 1)
total_cml$Cml11 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP11 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP11 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP11 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP12_", 1)
total_cml$Cml12 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP12 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP12 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP12 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

total_cml$team <- factor(x = total_cml$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_sum$team <- factor(x = cml_sum$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_2p5$team <- factor(x = cml_2p5$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_97p5$team <- factor(x = cml_97p5$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))

cml_sum <- rbind(cml_sum, c("ENS", as.numeric(colMeans(cml_sum[2:13]))))
cml_2p5 <- rbind(cml_2p5, c("ENS", as.numeric(colMeans(cml_2p5[2:13]))))
cml_97p5 <- rbind(cml_97p5, c("ENS", as.numeric(colMeans(cml_97p5[2:13]))))

hp01 <- total_cml[1:2]
hp01$Cml <- as.numeric(hp01$Cml)
total_cml <- total_cml[-1]
total_cml <-  cbind(total_cml[1], lapply(total_cml[-1], as.numeric))
cml_2p5 <-  cbind(cml_2p5[1], lapply(cml_2p5[-1], as.numeric))
cml_97p5 <-  cbind(cml_97p5[1], lapply(cml_97p5[-1], as.numeric))


for_csv <- data.frame(team = cml_sum$team,
                      HP01 = paste0(round(as.numeric(as.character(cml_sum$HP01)), 1), "% (",
                                   round(as.numeric(as.character(cml_2p5$HP01)), 1), "%, ",
                                   round(as.numeric(as.character(cml_97p5$HP01)), 1), "%)"),
                      HP02 = paste0(round(as.numeric(as.character(cml_sum$HP02)), 1), "% (",
                                    round(as.numeric(as.character(cml_2p5$HP02)), 1), "%, ",
                                    round(as.numeric(as.character(cml_97p5$HP02)), 1), "%)"),
                      HP03 = paste0(round(as.numeric(as.character(cml_sum$HP03)), 1), "% (",
                                   round(as.numeric(as.character(cml_2p5$HP03)), 1), "%, ",
                                   round(as.numeric(as.character(cml_97p5$HP03)), 1), "%)"),
                      HP04 = paste0(round(as.numeric(as.character(cml_sum$HP04)), 1), "% (",
                                    round(as.numeric(as.character(cml_2p5$HP04)), 1), "%, ",
                                    round(as.numeric(as.character(cml_97p5$HP04)), 1), "%)"),
                      HP05 = paste0(round(as.numeric(as.character(cml_sum$HP05)), 1), "% (",
                                   round(as.numeric(as.character(cml_2p5$HP05)), 1), "%, ",
                                   round(as.numeric(as.character(cml_97p5$HP05)), 1), "%)"),
                      HP06 = paste0(round(as.numeric(as.character(cml_sum$HP06)), 1), "% (",
                                   round(as.numeric(as.character(cml_2p5$HP06)), 1), "%, ",
                                   round(as.numeric(as.character(cml_97p5$HP06)), 1), "%)"),
                      HP07 = paste0(round(as.numeric(as.character(cml_sum$HP07)), 1), "% (",
                                   round(as.numeric(as.character(cml_2p5$HP07)), 1), "%, ",
                                   round(as.numeric(as.character(cml_97p5$HP07)), 1), "%)"),
                      HP08 = paste0(round(as.numeric(as.character(cml_sum$HP08)), 1), "% (",
                                    round(as.numeric(as.character(cml_2p5$HP08)), 1), "%, ",
                                    round(as.numeric(as.character(cml_97p5$HP08)), 1), "%)"),
                      HP09 = paste0(round(as.numeric(as.character(cml_sum$HP09)), 1), "% (",
                                    round(as.numeric(as.character(cml_2p5$HP09)), 1), "%, ",
                                    round(as.numeric(as.character(cml_97p5$HP09)), 1), "%)"),
                      HP10 = paste0(round(as.numeric(as.character(cml_sum$HP10)), 1), "% (",
                                    round(as.numeric(as.character(cml_2p5$HP10)), 1), "%, ",
                                    round(as.numeric(as.character(cml_97p5$HP10)), 1), "%)"),
                      HP11 = paste0(round(as.numeric(as.character(cml_sum$HP11)), 1), "% (",
                                    round(as.numeric(as.character(cml_2p5$HP11)), 1), "%, ",
                                    round(as.numeric(as.character(cml_97p5$HP11)), 1), "%)"),
                      HP12 = paste0(round(as.numeric(as.character(cml_sum$HP12)), 1), "% (",
                                    round(as.numeric(as.character(cml_2p5$HP12)), 1), "%, ",
                                    round(as.numeric(as.character(cml_97p5$HP12)), 1), "%)"))

write.csv(for_csv, "epi1_illness_table.csv")

# Supp figure hospitalizations - rank order figures ----------------------------

hp01 <- read_overall_mean(scn = "HP01_", 2)
total_cml <- cml[which(cml$Bin == "sm.mean"),][-3:-4]
cml_2p5 <- cml[which(cml$Bin == "sm.perc2p5"),][-3:-4]
cml_97p5 <- cml[which(cml$Bin == "sm.perc97p5"),][-3:-4]
cml_sum <- total_cml$Cml

read_overall_mean(scn = "HP02_", 2)
total_cml$Cml2 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum <- data.frame(team = cml$team[which(cml$Bin == "sm.mean")],
                      HP01 = cml_sum,
                      HP02 = cml$Cml[which(cml$Bin == "sm.mean")])
cml_2p5 <- data.frame(team = cml_2p5$team,
                      HP01 = cml_2p5$Cml,
                      HP02 = cml$Cml[which(cml$Bin == "sm.perc2p5")])
cml_97p5 <- data.frame(team = cml_97p5$team,
                       HP01 = cml_97p5$Cml,
                       HP02 = cml$Cml[which(cml$Bin == "sm.perc97p5")])

read_overall_mean(scn = "HP03_", 2)
total_cml$Cml3 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP03 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP03 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP03 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP04_", 2)
total_cml$Cml4 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP04 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP04 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP04 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP05_", 2)
total_cml$Cml5 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP05 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP05 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP05 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP06_", 2)
total_cml$Cml6 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP06 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP06 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP06 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP07_", 2)
total_cml$Cml7 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP07 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP07 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP07 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP08_", 2)
total_cml$Cml8 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP08 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP08 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP08 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP09_", 2)
total_cml$Cml9 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP09 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP09 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP09 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP10_", 2)
total_cml$Cml10 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP10 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP10 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP10 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP11_", 2)
total_cml$Cml11 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP11 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP11 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP11 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP12_", 2)
total_cml$Cml12 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP12 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP12 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP12 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

total_cml$team <- factor(x = total_cml$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_sum$team <- factor(x = cml_sum$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_2p5$team <- factor(x = cml_2p5$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_97p5$team <- factor(x = cml_97p5$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))

cml_sum <- rbind(cml_sum, c("ENS", as.numeric(colMeans(cml_sum[2:13]))))
cml_2p5 <- rbind(cml_2p5, c("ENS", as.numeric(colMeans(cml_2p5[2:13]))))
cml_97p5 <- rbind(cml_97p5, c("ENS", as.numeric(colMeans(cml_97p5[2:13]))))

hp01 <- total_cml[1:2]
hp01$Cml <- as.numeric(hp01$Cml)
total_cml <- total_cml[-1]
total_cml <-  cbind(total_cml[1], lapply(total_cml[-1], as.numeric))
cml_2p5 <-  cbind(cml_2p5[1], lapply(cml_2p5[-1], as.numeric))
cml_97p5 <-  cbind(cml_97p5[1], lapply(cml_97p5[-1], as.numeric))

for_csv <- data.frame(team = cml_sum$team,
                      HP01 = paste0(round(as.numeric(as.character(cml_sum$HP01))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP01))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP01))), ")"),
                      HP02 = paste0(round(as.numeric(as.character(cml_sum$HP02))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP02))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP02))), ")"),
                      HP03 = paste0(round(as.numeric(as.character(cml_sum$HP03))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP03))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP03))), ")"),
                      HP04 = paste0(round(as.numeric(as.character(cml_sum$HP04))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP04))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP04))), ")"),
                      HP05 = paste0(round(as.numeric(as.character(cml_sum$HP05))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP05))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP05))), ")"),
                      HP06 = paste0(round(as.numeric(as.character(cml_sum$HP06))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP06))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP06))), ")"),
                      HP07 = paste0(round(as.numeric(as.character(cml_sum$HP07))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP07))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP07))), ")"),
                      HP08 = paste0(round(as.numeric(as.character(cml_sum$HP08))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP08))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP08))), ")"),
                      HP09 = paste0(round(as.numeric(as.character(cml_sum$HP09))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP09))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP09))), ")"),
                      HP10 = paste0(round(as.numeric(as.character(cml_sum$HP10))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP10))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP10))), ")"),
                      HP11 = paste0(round(as.numeric(as.character(cml_sum$HP11))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP11))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP11))), ")"),
                      HP12 = paste0(round(as.numeric(as.character(cml_sum$HP12))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP12))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP12))), ")"))

write.csv(for_csv, "epi1_hospitalization_table.csv")

# Supp figure deaths - rank order figures ----------------------------


hp01 <- read_overall_mean(scn = "HP01_", 3)
total_cml <- cml[which(cml$Bin == "sm.mean"),][-3:-4]
cml_2p5 <- cml[which(cml$Bin == "sm.perc2p5"),][-3:-4]
cml_97p5 <- cml[which(cml$Bin == "sm.perc97p5"),][-3:-4]
cml_sum <- total_cml$Cml

read_overall_mean(scn = "HP02_", 3)
total_cml$Cml2 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum <- data.frame(team = cml$team[which(cml$Bin == "sm.mean")],
                      HP01 = cml_sum,
                      HP02 = cml$Cml[which(cml$Bin == "sm.mean")])
cml_2p5 <- data.frame(team = cml_2p5$team,
                      HP01 = cml_2p5$Cml,
                      HP02 = cml$Cml[which(cml$Bin == "sm.perc2p5")])
cml_97p5 <- data.frame(team = cml_97p5$team,
                       HP01 = cml_97p5$Cml,
                       HP02 = cml$Cml[which(cml$Bin == "sm.perc97p5")])

read_overall_mean(scn = "HP03_", 3)
total_cml$Cml3 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP03 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP03 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP03 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP04_", 3)
total_cml$Cml4 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP04 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP04 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP04 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP05_", 3)
total_cml$Cml5 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP05 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP05 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP05 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP06_", 3)
total_cml$Cml6 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP06 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP06 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP06 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP07_", 3)
total_cml$Cml7 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP07 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP07 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP07 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP08_", 3)
total_cml$Cml8 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP08 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP08 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP08 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP09_", 3)
total_cml$Cml9 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP09 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP09 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP09 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP10_", 3)
total_cml$Cml10 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP10 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP10 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP10 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP11_", 3)
total_cml$Cml11 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP11 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP11 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP11 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP12_", 3)
total_cml$Cml12 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP12 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP12 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP12 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

total_cml$team <- factor(x = total_cml$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_sum$team <- factor(x = cml_sum$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_2p5$team <- factor(x = cml_2p5$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_97p5$team <- factor(x = cml_97p5$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))

cml_sum <- rbind(cml_sum, c("ENS", as.numeric(colMeans(cml_sum[2:13]))))
cml_2p5 <- rbind(cml_2p5, c("ENS", as.numeric(colMeans(cml_2p5[2:13]))))
cml_97p5 <- rbind(cml_97p5, c("ENS", as.numeric(colMeans(cml_97p5[2:13]))))

hp01 <- total_cml[1:2]
hp01$Cml <- as.numeric(hp01$Cml)
total_cml <- total_cml[-1]
total_cml <-  cbind(total_cml[1], lapply(total_cml[-1], as.numeric))
cml_2p5 <-  cbind(cml_2p5[1], lapply(cml_2p5[-1], as.numeric))
cml_97p5 <-  cbind(cml_97p5[1], lapply(cml_97p5[-1], as.numeric))

for_csv <- data.frame(team = cml_sum$team,
                      HP01 = paste0(round(as.numeric(as.character(cml_sum$HP01))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP01))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP01))), ")"),
                      HP02 = paste0(round(as.numeric(as.character(cml_sum$HP02))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP02))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP02))), ")"),
                      HP03 = paste0(round(as.numeric(as.character(cml_sum$HP03))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP03))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP03))), ")"),
                      HP04 = paste0(round(as.numeric(as.character(cml_sum$HP04))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP04))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP04))), ")"),
                      HP05 = paste0(round(as.numeric(as.character(cml_sum$HP05))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP05))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP05))), ")"),
                      HP06 = paste0(round(as.numeric(as.character(cml_sum$HP06))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP06))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP06))), ")"),
                      HP07 = paste0(round(as.numeric(as.character(cml_sum$HP07))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP07))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP07))), ")"),
                      HP08 = paste0(round(as.numeric(as.character(cml_sum$HP08))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP08))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP08))), ")"),
                      HP09 = paste0(round(as.numeric(as.character(cml_sum$HP09))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP09))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP09))), ")"),
                      HP10 = paste0(round(as.numeric(as.character(cml_sum$HP10))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP10))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP10))), ")"),
                      HP11 = paste0(round(as.numeric(as.character(cml_sum$HP11))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP11))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP11))), ")"),
                      HP12 = paste0(round(as.numeric(as.character(cml_sum$HP12))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP12))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP12))), ")"))

write.csv(for_csv, "epi1_death_table.csv")