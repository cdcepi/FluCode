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


team.folder <- c("Columbia/COL/EPI2/", "Northeastern/EPI2/",  "UTAustin/", "UVirginia/", "Imperial/", "Columbia/COL2/EPI2/")
team.abbrev <- c("COL", "NEU", "UTA", "UVA", "IMP", "COL2")
file.types <-c("SymIllness", "Hosp", "Deaths", "AntiviralTX")
scn = "HP13_"
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

long_HP13 <- read_overall_mean(scn = "HP13_", 1)
total_cml <- cml[which(cml$Bin == "sm.mean"),][-3:-4]
cml_2p5 <- cml[which(cml$Bin == "sm.perc2p5"),][-3:-4]
cml_97p5 <- cml[which(cml$Bin == "sm.perc97p5"),][-3:-4]
cml_sum <- total_cml$Cml

read_overall_mean(scn = "HP14_", 1)
total_cml$Cml2 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum <- data.frame(team = cml$team[which(cml$Bin == "sm.mean")],
                      HP13 = cml_sum,
                      HP14 = cml$Cml[which(cml$Bin == "sm.mean")])
cml_2p5 <- data.frame(team = cml_2p5$team,
                      HP13 = cml_2p5$Cml,
                      HP14 = cml$Cml[which(cml$Bin == "sm.perc2p5")])
cml_97p5 <- data.frame(team = cml_97p5$team,
                       HP13 = cml_97p5$Cml,
                       HP14 = cml$Cml[which(cml$Bin == "sm.perc97p5")])

read_overall_mean(scn = "HP15_", 1)
total_cml$Cml3 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP15 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP15 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP15 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP16_", 1)
total_cml$Cml4 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP16 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP16 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP16 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP17_", 1)
total_cml$Cml5 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP17 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP17 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP17 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP18_", 1)
total_cml$Cml6 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP18 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP18 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP18 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP19_", 1)
total_cml$Cml7 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP19 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP19 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP19 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP20_", 1)
total_cml$Cml8 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP20 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP20 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP20 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP21_", 1)
total_cml$Cml9 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP21 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP21 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP21 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP22_", 1)
total_cml$Cml10 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP22 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP22 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP22 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP23_", 1)
total_cml$Cml11 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP23 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP23 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP23 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP24_", 1)
total_cml$Cml12 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP24 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP24 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP24 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

total_cml$team <- factor(x = total_cml$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_sum$team <- factor(x = cml_sum$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_2p5$team <- factor(x = cml_2p5$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_97p5$team <- factor(x = cml_97p5$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))

cml_sum <- rbind(cml_sum, c("ENS", as.numeric(colMeans(cml_sum[2:13]))))
cml_2p5 <- rbind(cml_2p5, c("ENS", as.numeric(colMeans(cml_2p5[2:13]))))
cml_97p5 <- rbind(cml_97p5, c("ENS", as.numeric(colMeans(cml_97p5[2:13]))))

hp13 <- total_cml[1:2]
hp13$Cml <- as.numeric(hp13$Cml)
total_cml <- total_cml[-1]
total_cml <-  cbind(total_cml[1], lapply(total_cml[-1], as.numeric))
cml_2p5 <-  cbind(cml_2p5[1], lapply(cml_2p5[-1], as.numeric))
cml_97p5 <-  cbind(cml_97p5[1], lapply(cml_97p5[-1], as.numeric))


for_csv <- data.frame(team = cml_sum$team,
                      HP13 = paste0(round(as.numeric(as.character(cml_sum$HP13)), 1), "% (",
                                   round(as.numeric(as.character(cml_2p5$HP13)), 1), "%, ",
                                   round(as.numeric(as.character(cml_97p5$HP13)), 1), "%)"),
                      HP14 = paste0(round(as.numeric(as.character(cml_sum$HP14)), 1), "% (",
                                    round(as.numeric(as.character(cml_2p5$HP14)), 1), "%, ",
                                    round(as.numeric(as.character(cml_97p5$HP14)), 1), "%)"),
                      HP15 = paste0(round(as.numeric(as.character(cml_sum$HP15)), 1), "% (",
                                   round(as.numeric(as.character(cml_2p5$HP15)), 1), "%, ",
                                   round(as.numeric(as.character(cml_97p5$HP15)), 1), "%)"),
                      HP16 = paste0(round(as.numeric(as.character(cml_sum$HP16)), 1), "% (",
                                    round(as.numeric(as.character(cml_2p5$HP16)), 1), "%, ",
                                    round(as.numeric(as.character(cml_97p5$HP16)), 1), "%)"),
                      HP17 = paste0(round(as.numeric(as.character(cml_sum$HP17)), 1), "% (",
                                   round(as.numeric(as.character(cml_2p5$HP17)), 1), "%, ",
                                   round(as.numeric(as.character(cml_97p5$HP17)), 1), "%)"),
                      HP18 = paste0(round(as.numeric(as.character(cml_sum$HP18)), 1), "% (",
                                   round(as.numeric(as.character(cml_2p5$HP18)), 1), "%, ",
                                   round(as.numeric(as.character(cml_97p5$HP18)), 1), "%)"),
                      HP19 = paste0(round(as.numeric(as.character(cml_sum$HP19)), 1), "% (",
                                   round(as.numeric(as.character(cml_2p5$HP19)), 1), "%, ",
                                   round(as.numeric(as.character(cml_97p5$HP19)), 1), "%)"),
                      HP20 = paste0(round(as.numeric(as.character(cml_sum$HP20)), 1), "% (",
                                    round(as.numeric(as.character(cml_2p5$HP20)), 1), "%, ",
                                    round(as.numeric(as.character(cml_97p5$HP20)), 1), "%)"),
                      HP21 = paste0(round(as.numeric(as.character(cml_sum$HP21)), 1), "% (",
                                    round(as.numeric(as.character(cml_2p5$HP21)), 1), "%, ",
                                    round(as.numeric(as.character(cml_97p5$HP21)), 1), "%)"),
                      HP22 = paste0(round(as.numeric(as.character(cml_sum$HP22)), 1), "% (",
                                    round(as.numeric(as.character(cml_2p5$HP22)), 1), "%, ",
                                    round(as.numeric(as.character(cml_97p5$HP22)), 1), "%)"),
                      HP23 = paste0(round(as.numeric(as.character(cml_sum$HP23)), 1), "% (",
                                    round(as.numeric(as.character(cml_2p5$HP23)), 1), "%, ",
                                    round(as.numeric(as.character(cml_97p5$HP23)), 1), "%)"),
                      HP24 = paste0(round(as.numeric(as.character(cml_sum$HP24)), 1), "% (",
                                    round(as.numeric(as.character(cml_2p5$HP24)), 1), "%, ",
                                    round(as.numeric(as.character(cml_97p5$HP24)), 1), "%)"))

write.csv(for_csv, "epi2_illness_table.csv")

# Supp figure hospitalizations - rank order figures ----------------------------

hp13 <- read_overall_mean(scn = "HP13_", 2)
total_cml <- cml[which(cml$Bin == "sm.mean"),][-3:-4]
cml_2p5 <- cml[which(cml$Bin == "sm.perc2p5"),][-3:-4]
cml_97p5 <- cml[which(cml$Bin == "sm.perc97p5"),][-3:-4]
cml_sum <- total_cml$Cml

read_overall_mean(scn = "HP14_", 2)
total_cml$Cml2 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum <- data.frame(team = cml$team[which(cml$Bin == "sm.mean")],
                      HP13 = cml_sum,
                      HP14 = cml$Cml[which(cml$Bin == "sm.mean")])
cml_2p5 <- data.frame(team = cml_2p5$team,
                      HP13 = cml_2p5$Cml,
                      HP14 = cml$Cml[which(cml$Bin == "sm.perc2p5")])
cml_97p5 <- data.frame(team = cml_97p5$team,
                       HP13 = cml_97p5$Cml,
                       HP14 = cml$Cml[which(cml$Bin == "sm.perc97p5")])

read_overall_mean(scn = "HP15_", 2)
total_cml$Cml3 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP15 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP15 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP15 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP16_", 2)
total_cml$Cml4 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP16 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP16 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP16 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP17_", 2)
total_cml$Cml5 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP17 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP17 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP17 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP18_", 2)
total_cml$Cml6 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP18 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP18 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP18 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP19_", 2)
total_cml$Cml7 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP19 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP19 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP19 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP20_", 2)
total_cml$Cml8 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP20 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP20 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP20 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP21_", 2)
total_cml$Cml9 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP21 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP21 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP21 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP22_", 2)
total_cml$Cml10 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP22 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP22 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP22 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP23_", 2)
total_cml$Cml11 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP23 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP23 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP23 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP24_", 2)
total_cml$Cml12 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP24 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP24 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP24 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

total_cml$team <- factor(x = total_cml$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_sum$team <- factor(x = cml_sum$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_2p5$team <- factor(x = cml_2p5$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_97p5$team <- factor(x = cml_97p5$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))

cml_sum <- rbind(cml_sum, c("ENS", as.numeric(colMeans(cml_sum[2:13]))))
cml_2p5 <- rbind(cml_2p5, c("ENS", as.numeric(colMeans(cml_2p5[2:13]))))
cml_97p5 <- rbind(cml_97p5, c("ENS", as.numeric(colMeans(cml_97p5[2:13]))))

hp13 <- total_cml[1:2]
hp13$Cml <- as.numeric(hp13$Cml)
total_cml <- total_cml[-1]
total_cml <-  cbind(total_cml[1], lapply(total_cml[-1], as.numeric))
cml_2p5 <-  cbind(cml_2p5[1], lapply(cml_2p5[-1], as.numeric))
cml_97p5 <-  cbind(cml_97p5[1], lapply(cml_97p5[-1], as.numeric))

for_csv <- data.frame(team = cml_sum$team,
                      HP13 = paste0(round(as.numeric(as.character(cml_sum$HP13))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP13))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP13))), ")"),
                      HP14 = paste0(round(as.numeric(as.character(cml_sum$HP14))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP14))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP14))), ")"),
                      HP15 = paste0(round(as.numeric(as.character(cml_sum$HP15))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP15))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP15))), ")"),
                      HP16 = paste0(round(as.numeric(as.character(cml_sum$HP16))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP16))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP16))), ")"),
                      HP17 = paste0(round(as.numeric(as.character(cml_sum$HP17))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP17))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP17))), ")"),
                      HP18 = paste0(round(as.numeric(as.character(cml_sum$HP18))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP18))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP18))), ")"),
                      HP19 = paste0(round(as.numeric(as.character(cml_sum$HP19))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP19))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP19))), ")"),
                      HP20 = paste0(round(as.numeric(as.character(cml_sum$HP20))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP20))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP20))), ")"),
                      HP21 = paste0(round(as.numeric(as.character(cml_sum$HP21))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP21))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP21))), ")"),
                      HP22 = paste0(round(as.numeric(as.character(cml_sum$HP22))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP22))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP22))), ")"),
                      HP23 = paste0(round(as.numeric(as.character(cml_sum$HP23))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP23))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP23))), ")"),
                      HP24 = paste0(round(as.numeric(as.character(cml_sum$HP24))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP24))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP24))), ")"))

write.csv(for_csv, "epi2_hospitalization_table.csv")

# Supp figure deaths - rank order figures ----------------------------


hp13 <- read_overall_mean(scn = "HP13_", 3)
total_cml <- cml[which(cml$Bin == "sm.mean"),][-3:-4]
cml_2p5 <- cml[which(cml$Bin == "sm.perc2p5"),][-3:-4]
cml_97p5 <- cml[which(cml$Bin == "sm.perc97p5"),][-3:-4]
cml_sum <- total_cml$Cml

read_overall_mean(scn = "HP14_", 3)
total_cml$Cml2 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum <- data.frame(team = cml$team[which(cml$Bin == "sm.mean")],
                      HP13 = cml_sum,
                      HP14 = cml$Cml[which(cml$Bin == "sm.mean")])
cml_2p5 <- data.frame(team = cml_2p5$team,
                      HP13 = cml_2p5$Cml,
                      HP14 = cml$Cml[which(cml$Bin == "sm.perc2p5")])
cml_97p5 <- data.frame(team = cml_97p5$team,
                       HP13 = cml_97p5$Cml,
                       HP14 = cml$Cml[which(cml$Bin == "sm.perc97p5")])

read_overall_mean(scn = "HP15_", 3)
total_cml$Cml3 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP15 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP15 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP15 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP16_", 3)
total_cml$Cml4 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP16 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP16 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP16 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP17_", 3)
total_cml$Cml5 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP17 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP17 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP17 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP18_", 3)
total_cml$Cml6 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP18 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP18 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP18 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP19_", 3)
total_cml$Cml7 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP19 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP19 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP19 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP20_", 3)
total_cml$Cml8 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP20 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP20 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP20 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP21_", 3)
total_cml$Cml9 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP21 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP21 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP21 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP22_", 3)
total_cml$Cml10 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP22 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP22 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP22 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP23_", 3)
total_cml$Cml11 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP23 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP23 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP23 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

read_overall_mean(scn = "HP24_", 3)
total_cml$Cml12 <- ( total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
cml_sum$HP24 <- cml$Cml[which(cml$Bin == "sm.mean")]
cml_2p5$HP24 <- cml$Cml[which(cml$Bin == "sm.perc2p5")]
cml_97p5$HP24 <- cml$Cml[which(cml$Bin == "sm.perc97p5")]

total_cml$team <- factor(x = total_cml$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_sum$team <- factor(x = cml_sum$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_2p5$team <- factor(x = cml_2p5$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))
cml_97p5$team <- factor(x = cml_97p5$team, levels = c("COL", "COL2", "IMP", "NEU", "UTA", "UVA", "ENS"))

cml_sum <- rbind(cml_sum, c("ENS", as.numeric(colMeans(cml_sum[2:13]))))
cml_2p5 <- rbind(cml_2p5, c("ENS", as.numeric(colMeans(cml_2p5[2:13]))))
cml_97p5 <- rbind(cml_97p5, c("ENS", as.numeric(colMeans(cml_97p5[2:13]))))

hp13 <- total_cml[1:2]
hp13$Cml <- as.numeric(hp13$Cml)
total_cml <- total_cml[-1]
total_cml <-  cbind(total_cml[1], lapply(total_cml[-1], as.numeric))
cml_2p5 <-  cbind(cml_2p5[1], lapply(cml_2p5[-1], as.numeric))
cml_97p5 <-  cbind(cml_97p5[1], lapply(cml_97p5[-1], as.numeric))

for_csv <- data.frame(team = cml_sum$team,
                      HP13 = paste0(round(as.numeric(as.character(cml_sum$HP13))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP13))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP13))), ")"),
                      HP14 = paste0(round(as.numeric(as.character(cml_sum$HP14))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP14))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP14))), ")"),
                      HP15 = paste0(round(as.numeric(as.character(cml_sum$HP15))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP15))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP15))), ")"),
                      HP16 = paste0(round(as.numeric(as.character(cml_sum$HP16))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP16))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP16))), ")"),
                      HP17 = paste0(round(as.numeric(as.character(cml_sum$HP17))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP17))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP17))), ")"),
                      HP18 = paste0(round(as.numeric(as.character(cml_sum$HP18))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP18))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP18))), ")"),
                      HP19 = paste0(round(as.numeric(as.character(cml_sum$HP19))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP19))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP19))), ")"),
                      HP20 = paste0(round(as.numeric(as.character(cml_sum$HP20))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP20))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP20))), ")"),
                      HP21 = paste0(round(as.numeric(as.character(cml_sum$HP21))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP21))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP21))), ")"),
                      HP22 = paste0(round(as.numeric(as.character(cml_sum$HP22))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP22))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP22))), ")"),
                      HP23 = paste0(round(as.numeric(as.character(cml_sum$HP23))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP23))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP23))), ")"),
                      HP24 = paste0(round(as.numeric(as.character(cml_sum$HP24))), " (",
                                    round(as.numeric(as.character(cml_2p5$HP24))), ", ",
                                    round(as.numeric(as.character(cml_97p5$HP24))), ")"))

write.csv(for_csv, "epi2_death_table.csv")

