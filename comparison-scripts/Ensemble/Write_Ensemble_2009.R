library(ggplot2)
library(plyr)
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
library(wesanderson)
library(rio)
library(dplyr)

#Functions ---------------------------------------------------------------------------------------------
read_overall_mean <- function(scn, outcome){
  
  cols <- c("sm.mean", "sm.perc2p5", "sm.perc97p5")
  
  s <- read.csv(paste0(path,team.folder[1],scn,outcome,"_",team.abbrev[1],".csv"))
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
    
    f =  paste0(path,team.folder[i],scn,outcome,"_",team.abbrev[i],".csv")
    if(i == 5){
      f =  paste0(path,team.folder[i],scn,outcome,team.abbrev[i],".csv")
      if(outcome == "Deaths"){
        f =  paste0(path,team.folder[i],scn,"Death",team.abbrev[i],".csv")
      }
    }
    
    if(file.exists(f)){
      s <- read.csv(f)
      # if(ncol(s) > 58){
      #   s <- s[-1]
      # }
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
  cml <- symillness[c("Cml", "team", "Bin", "Peak.Magnitude")]
  
  long <- gather(symillness, week, illnesses, weeks, factor_key=TRUE)
  long$week <- as.integer(gsub("Week", "", long$week))
  
  cols <- c("Bin", "team", "week", "illnesses")
  long <- long[cols]
  long_wide <- spread(long, Bin, illnesses)
  
  return(list(long_wide, cml))
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

calculate_metrics<- function(scenarios, outcome, team.levels, team.abbrev, team.folder, ens.levels, find, replace ){
  
  # Base Trajectory --------------------------------------------------------------------------------------------
  
  mean_run <- read_overall_mean(scn = scenarios[1], outcome)
  
  long_RL01 <- mean_run[[1]]
  long_RL01$team <- factor(x = long_RL01$team, levels = team.levels)
  
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
  long_ens$Date <- as.Date(long_ens$Date)
  long_ens$team <- factor(long_ens$team, levels = ens.levels)
  
  # Save summary measure ------------------------------------------------------------------------------
  if(outcome == "SymIllness"){
    percent_sign = "%"
    sig_fig = 1
  }else{
    percent_sign = ""
    sig_fig = 0
  }
  
  table <- rbind(mean_run[[2]] %>% group_by(Bin) %>% summarise(Cml = mean(Cml)) %>% mutate(team ="ENS") %>% select(Cml, team, Bin),
                 mean_run[[2]] %>% select(Cml, team, Bin)) %>%
    mutate(team = factor(team, levels = ens.levels)) %>%
    arrange(team, Bin) %>%
    pivot_wider(names_from = Bin, values_from = Cml) %>%
    mutate(formatted = paste0(round(as.numeric(as.character(sm.mean)), sig_fig), percent_sign, " (",
                              round(as.numeric(as.character(sm.perc2p5)), sig_fig), percent_sign,", ",
                              round(as.numeric(as.character(sm.perc97p5)), sig_fig), percent_sign, ")")) %>%
    select(team, formatted)
  
  names(table)[2] <- outcome
  
  # Averted Total -------------------------------------------------------------------------------------
  
  cml <- mean_run[[2]] %>% filter(Bin == "sm.mean")
  total_cml <- cml[-3:-4]
  
  cml_sum <- cml$Cml
  
  cml <- read_overall_mean(scn = scenarios[2], outcome)[[2]]
  total_cml$Cml2 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum <- data.frame(RL01 = cml_sum,
                        RL02 = cml$Cml[which(cml$Bin == "sm.mean")])
  
  cml <- read_overall_mean(scn = scenarios[3], outcome)[[2]]
  total_cml$Cml3 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum$RL03 <- cml$Cml[which(cml$Bin == "sm.mean")]
  
  cml <- read_overall_mean(scn = scenarios[4], outcome)[[2]]
  total_cml$Cml4 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum$RL04 <- cml$Cml[which(cml$Bin == "sm.mean")]
  
  cml <- read_overall_mean(scn = scenarios[5], outcome)[[2]]
  total_cml$Cml5 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum$RL05 <- cml$Cml[which(cml$Bin == "sm.mean")]
  
  cml <- read_overall_mean(scn = scenarios[6], outcome)[[2]]
  total_cml$Cml6 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum$RL06 <- cml$Cml[which(cml$Bin == "sm.mean")]
  
  cml <- read_overall_mean(scn = scenarios[7], outcome)[[2]]
  total_cml$Cml7 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum$RL07 <- cml$Cml[which(cml$Bin == "sm.mean")]
  
  cml <- read_overall_mean(scn = scenarios[8], outcome)[[2]]
  total_cml$Cml8 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum$RL08 <- cml$Cml[which(cml$Bin == "sm.mean")]
  
  total_cml$team <- factor(x = total_cml$team, levels = ens.levels)
  
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
  
  if(outcome == "SymIllness"){
    averted <- cbind(total_cml[1], total_cml[-1] * (rl01$Cml/100) * 306.8e6)
  }else{
    averted <- cbind(total_cml[1], total_cml[-1] * (rl01$Cml*100000))
  }
  
  colnames(averted) <- c("team", gsub("_", "", scenarios[-1]))
  a <- melt(averted, id.vars = c("team"), variable.name = "Averted")
  names(a) <- c("Team", 'Scenario', "Averted")
  
  # Percent Averted --------------------------------------------------------------------------------
  
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
  
  a$Scenario <- gsub(find, replace, a$Scenario)
  
  a$x <- 1
  a$Team <- factor(x = a$Team, levels = ens.levels)
  
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
  
  colnames(total_cml) <- c("team", gsub("_", "", scenarios[-1]))
  t <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
  colnames(t) <- c("Team", "Scenario", "Reduction")
  t$Reduction <- t$Reduction*100
  t$Rank <- r$value
  t$Scenario <- gsub(find, replace, t$Scenario)
  
  # Peak Magnitude -----------------------------------------------------------------------------------------
  r <- read_overall_mean(scn = scenarios[1], outcome)
  peak.weeks <- get_peaks(r[[1]])
  peak.weeks$Scenario <- gsub("_", "", scenarios[1])
  cml <- r[[2]] %>% filter(Bin == "sm.mean")
  total_cml <- cml[-1]
  total_cml <- total_cml[-2]
  
  r <- read_overall_mean(scn = scenarios[2], outcome)
  p <- get_peaks(r[[1]])
  p$Scenario <- gsub("_", "", scenarios[2])
  peak.weeks <- rbind(peak.weeks, p)
  cml <- r[[2]] %>% filter(Bin == "sm.mean")
  total_cml$Cml2 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
  
  r <- read_overall_mean(scn = scenarios[3], outcome)
  p <- get_peaks(r[[1]])
  p$Scenario <- gsub("_", "", scenarios[3])
  peak.weeks <- rbind(peak.weeks, p)
  cml <- r[[2]] %>% filter(Bin == "sm.mean")
  total_cml$Cml3 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
  
  r <- read_overall_mean(scn = scenarios[4], outcome)
  p <- get_peaks(r[[1]])
  p$Scenario <- gsub("_", "", scenarios[4])
  peak.weeks <- rbind(peak.weeks, p)
  cml <- r[[2]] %>% filter(Bin == "sm.mean")
  total_cml$Cml4 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
  
  r <- read_overall_mean(scn = scenarios[5], outcome)
  p <- get_peaks(r[[1]])
  p$Scenario <- gsub("_", "", scenarios[5])
  peak.weeks <- rbind(peak.weeks, p)
  cml <- r[[2]] %>% filter(Bin == "sm.mean")
  total_cml$Cml5 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
  
  r <- read_overall_mean(scn = scenarios[6], outcome)
  p <- get_peaks(r[[1]])
  p$Scenario <- gsub("_", "", scenarios[6])
  peak.weeks <- rbind(peak.weeks, p)
  cml <- r[[2]] %>% filter(Bin == "sm.mean")
  total_cml$Cml6 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
  
  r <- read_overall_mean(scn = scenarios[7], outcome)
  p <- get_peaks(r[[1]])
  p$Scenario <- gsub("_", "", scenarios[7])
  peak.weeks <- rbind(peak.weeks, p)
  cml <- r[[2]] %>% filter(Bin == "sm.mean")
  total_cml$Cml7 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
  
  r <- read_overall_mean(scn = scenarios[8], outcome)
  p <- get_peaks(r[[1]])
  p$Scenario <- gsub("_", "", scenarios[8])
  peak.weeks <- rbind(peak.weeks, p)
  cml <- r[[2]] %>% filter(Bin == "sm.mean")
  total_cml$Cml8 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
  
  team <- as.character(total_cml$team)
  
  total_cml <- cbind(factor(x = c(team), 
                            levels = ens.levels),
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
  
  rank$team <- factor(x = ens.levels, 
                      levels = ens.levels)
  
  names(rank)[8] <- "team"
  r <- melt(rank, id.vars = c("team"), variable.name = "Reduction")
  
  colnames(total_cml) <- c("team", gsub("_", "", scenarios[-1]))
  m <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
  colnames(m) <- c("Team", "Scenario", "Reduction")
  m$Reduction <- m$Reduction*100
  m$Rank <- r$value
  
  m$Scenario <- gsub(find, replace, m$Scenario)
  
  # Peak Week -----------------------------------------------------------------------------------
  
  pw <- spread(peak.weeks, Scenario, peakWeek)
  
  pw$Delay2 <- pw$RL02 - pw$RL01
  pw$Delay3 <- pw$RL03 - pw$RL01
  pw$Delay4 <- pw$RL04 - pw$RL01
  pw$Delay5 <- pw$RL05 - pw$RL01
  pw$Delay6 <- pw$RL06 - pw$RL01
  pw$Delay7 <- pw$RL07 - pw$RL01
  pw$Delay8 <- pw$RL08 - pw$RL01
  
  pw <- pw[-2:-9]
  colnames(pw) <- c("team", gsub("_", "", scenarios[-1]))
  p <- melt(pw, id.vars = c("team"), variable.name = "Delay")
  colnames(p) <- c("Team", "Scenario", "Delay")
  
  p$Team <- factor(x = p$Team,
                   levels = team.levels
                   )
  
  p$Scenario <- gsub(find, replace, p$Scenario)
  
  # Writing all data -----------------------------------------------------------------------------
  
  a <- a %>% rename(Reduction = Averted) %>% select(!x) %>% mutate(Output = "Averted Number")
  t <- t %>% mutate(Output = "Averted Percent")
  m <- m %>% mutate(Output = "Magnitude Reduction")
  p <- p %>% rename(Reduction = Delay) %>% 
    mutate(Rank = 0, Output = "Peak Delay")
  
  return(list(long_ens, rbind(a,t,m,p), table))
}

# Inputs -----------------------------------------------------------------------------------------

setwd("C:/Users/ppf6/Desktop/GitHub/FluCode/comparison-scripts/Ensemble/")

path <- "../../model-forecasts/"

team.folder <- c("Columbia/COL/RL/", "Northeastern/RL3/",  "UTAustin/", "UVirginia/", "Imperial/", "Columbia/COL2/RL/")
team.abbrev <- c("COL", "NEU", "UTA", "UVA", "IMP", "COL2")
team.levels <- c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA")
ens.levels <- c("ENS", "UVA", "UTA", "NEU3", "IMP", "COL2", "COL")
file.types <-c("SymIllness", "Hosp", "Deaths", "AntiviralTX")

scenarios = c("RL01_", "RL02_", "RL03_", "RL04_", "RL05_", "RL06_", "RL07_", "RL08_")

# Symptomatic Illnesses --------------------------------------------------------------------------

output <- calculate_metrics(scenarios, outcome = file.types[1], team.levels, team.abbrev, team.folder, ens.levels, find = "RL0", replace = "2009-")

write.csv(output[[1]], paste0(path, "MeanBased-Ensemble/2009_symillness_RL01_ensemble.csv"))

write.csv(output[[2]], paste0(path,"/MeanBased-Ensemble/2009_symillness_metrics.csv"))

write.csv(output[[3]], paste0(path,"/MeanBased-Ensemble/2009_symillness_table.csv"))

# Hospitalizations --------------------------------------------------------------------------

output <- calculate_metrics(scenarios, outcome = file.types[2], team.levels, team.abbrev, team.folder, ens.levels, find = "RL0", replace = "2009-")

write.csv(output[[1]], paste0(path, "MeanBased-Ensemble/2009_hospitalization_RL01_ensemble.csv"))

write.csv(output[[2]], paste0(path,"/MeanBased-Ensemble/2009_hospitalization_metrics.csv"))

write.csv(output[[3]], paste0(path,"/MeanBased-Ensemble/2009_hospitalization_table.csv"))

# Deaths --------------------------------------------------------------------------

output <- calculate_metrics(scenarios, outcome = file.types[3], team.levels, team.abbrev, team.folder, ens.levels, find = "RL0", replace = "2009-")

write.csv(output[[1]], paste0(path, "MeanBased-Ensemble/2009_death_RL01_ensemble.csv"))

write.csv(output[[2]], paste0(path,"/MeanBased-Ensemble/2009_death_metrics.csv"))

write.csv(output[[3]], paste0(path,"/MeanBased-Ensemble/2009_death_table.csv"))
