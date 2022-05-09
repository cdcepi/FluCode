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
                                       s$Bin %in% cols &
                                       s$Location == "US National"),],
                             `team` = team.abbrev[1]))
  age <- s %>% 
    filter(Agegroup != "Overall", Bin_cml == "sm.mean", Location == "US National") %>%
    select(Agegroup, Bin_cml, Cml) %>%
    mutate(team = team.abbrev[1])
  
  for(i in 2:6){
    
    if(grepl("HP", scn, fixed=TRUE)){
      if(i == 4){
        scn = gsub("HP", "HP_", scn)
      }
      if(i == 5){
        scn = gsub("_", "", scn)
      }
      if(i == 6){
        scn = paste0(scn, "_")
      }
    }else{
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
    
    f =  paste0(path,team.folder[i],scn,outcome,"_",team.abbrev[i],".csv")
    if(i == 5){
      f =  paste0(path,team.folder[i],scn,outcome,team.abbrev[i],".csv")
      if(outcome == "Deaths"){
        f =  paste0(path,team.folder[i],scn,"Death",team.abbrev[i],".csv")
      }
    }
    
    if(file.exists(f)){
      s <- read.csv(f)
      
      age <- rbind(age, s %>% 
                     filter(Agegroup != "Overall", Bin_cml == "sm.mean", Location == "US National") %>%
                     select(Agegroup, Bin_cml, Cml) %>%
                     mutate(team = team.abbrev[i]))
      
      if(i == 2 & grepl("HP", scn, fixed=TRUE)){
        team.abbrev[i] = "NEU3"
      }
      symillness <- rbind.fill(symillness, 
                               data.frame(c(s[which(s$Agegroup == "Overall" & 
                                                      s$Bin %in% cols &
                                                      s$Location == "US National"),],`team` = team.abbrev[i])))
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
  
  return(list(long_wide, cml, age))
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

get_ensemble <- function(scn, long, team.levels, ens.levels){
  
  long$team <- factor(x = long$team, levels = team.levels)
  
  long <- long[order(long$team),]
  long$Date <- as.Date("2020-06-24") + (long$week*7)
  
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
  long_ens <- rbind(long, ensemble)
  long_ens$Date <- as.Date(long_ens$Date)
  long_ens$team <- factor(long_ens$team, levels = ens.levels)
  
  names(long_ens) <- gsub("sm.", scn, names(long_ens))
  
  return(long_ens)
}

calculate_metrics<- function(scenarios, outcome, team.levels, team.abbrev, team.folder, ens.levels, find, replace ){
  
  # Base Trajectory --------------------------------------------------------------------------------------------
  
  mean_run <- read_overall_mean(scn = scenarios[1], outcome)
  age <- mean_run[[3]]
  long_ens <- get_ensemble(scn = scenarios[1], long = mean_run[[1]], team.levels, ens.levels)
  
  # Summary Measure ------------------------------------------------------------------------------
  if(outcome == "SymIllness"){
    percent_sign = "%"
    sig_fig = 0
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
  
  mean_run <- read_overall_mean(scn = scenarios[2], outcome)
  long_ens <- left_join(long_ens, get_ensemble(scn = scenarios[2], long = mean_run[[1]], team.levels, ens.levels), by = c("team", "week", "Date"))
  cml <- mean_run[[2]]
  total_cml$Cml2 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum <- data.frame(HP13 = cml_sum,
                        HP14 = cml$Cml[which(cml$Bin == "sm.mean")])
  
  mean_run <- read_overall_mean(scn = scenarios[3], outcome)
  long_ens <- left_join(long_ens, get_ensemble(scn = scenarios[3], long = mean_run[[1]], team.levels, ens.levels), by = c("team", "week", "Date"))
  cml <- mean_run[[2]]
  total_cml$Cml3 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum$HP15 <- cml$Cml[which(cml$Bin == "sm.mean")]
  
  mean_run <- read_overall_mean(scn = scenarios[4], outcome)
  long_ens <- left_join(long_ens, get_ensemble(scn = scenarios[4], long = mean_run[[1]], team.levels, ens.levels), by = c("team", "week", "Date"))
  cml <- mean_run[[2]]
  total_cml$Cml4 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum$HP16 <- cml$Cml[which(cml$Bin == "sm.mean")]
  
  mean_run <- read_overall_mean(scn = scenarios[5], outcome)
  long_ens <- left_join(long_ens, get_ensemble(scn = scenarios[5], long = mean_run[[1]], team.levels, ens.levels), by = c("team", "week", "Date"))
  cml <- mean_run[[2]]
  total_cml$Cml5 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum$HP17 <- cml$Cml[which(cml$Bin == "sm.mean")]
  
  mean_run <- read_overall_mean(scn = scenarios[6], outcome)
  long_ens <- left_join(long_ens, get_ensemble(scn = scenarios[6], long = mean_run[[1]], team.levels, ens.levels), by = c("team", "week", "Date"))
  cml <- mean_run[[2]]
  total_cml$Cml6 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum$HP18 <- cml$Cml[which(cml$Bin == "sm.mean")]
  
  mean_run <- read_overall_mean(scn = scenarios[7], outcome)
  long_ens <- left_join(long_ens, get_ensemble(scn = scenarios[7], long = mean_run[[1]], team.levels, ens.levels), by = c("team", "week", "Date"))
  cml <- mean_run[[2]]  
  total_cml$Cml7 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum$HP19 <- cml$Cml[which(cml$Bin == "sm.mean")]
  
  mean_run <- read_overall_mean(scn = scenarios[8], outcome)
  long_ens <- left_join(long_ens, get_ensemble(scn = scenarios[8], long = mean_run[[1]], team.levels, ens.levels), by = c("team", "week", "Date"))
  cml <- mean_run[[2]]
  total_cml$Cml8 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum$HP20 <- cml$Cml[which(cml$Bin == "sm.mean")]
  
  mean_run <- read_overall_mean(scn = scenarios[9], outcome)
  long_ens <- left_join(long_ens, get_ensemble(scn = scenarios[9], long = mean_run[[1]], team.levels, ens.levels), by = c("team", "week", "Date"))
  cml <- mean_run[[2]]
  total_cml$Cml9 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum$HP21 <- cml$Cml[which(cml$Bin == "sm.mean")]
  
  mean_run <- read_overall_mean(scn = scenarios[10], outcome)
  long_ens <- left_join(long_ens, get_ensemble(scn = scenarios[10], long = mean_run[[1]], team.levels, ens.levels), by = c("team", "week", "Date"))
  cml <- mean_run[[2]]
  total_cml$Cml10 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum$HP22 <- cml$Cml[which(cml$Bin == "sm.mean")]
  
  mean_run <- read_overall_mean(scn = scenarios[11], outcome)
  long_ens <- left_join(long_ens, get_ensemble(scn = scenarios[11], long = mean_run[[1]], team.levels, ens.levels), by = c("team", "week", "Date"))
  cml <- mean_run[[2]]
  total_cml$Cml11 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum$HP23 <- cml$Cml[which(cml$Bin == "sm.mean")]
  
  mean_run <- read_overall_mean(scn = scenarios[12], outcome)
  long_ens <- left_join(long_ens, get_ensemble(scn = scenarios[12], long = mean_run[[1]], team.levels, ens.levels), by = c("team", "week", "Date"))
  cml <- mean_run[[2]]
  total_cml$Cml12 <- (total_cml$Cml - cml$Cml[which(cml$Bin == "sm.mean")]) / total_cml$Cml 
  cml_sum$HP24 <- cml$Cml[which(cml$Bin == "sm.mean")]
  
  total_cml$team <- factor(x = total_cml$team, levels = ens.levels)
  
  cml_sum <- rbind(cml_sum, colMeans(cml_sum))
  total_cml <- rbind(total_cml,
                     c(as.numeric(cml_sum[7,1]),
                       "ENS", 
                       as.numeric((cml_sum$HP13[7] - cml_sum$HP14[7]) / cml_sum$HP13[7]),
                       as.numeric((cml_sum$HP13[7] - cml_sum$HP15[7]) / cml_sum$HP13[7]),
                       as.numeric((cml_sum$HP13[7] - cml_sum$HP16[7]) / cml_sum$HP13[7]),
                       as.numeric((cml_sum$HP13[7] - cml_sum$HP17[7]) / cml_sum$HP13[7]),
                       as.numeric((cml_sum$HP13[7] - cml_sum$HP18[7]) / cml_sum$HP13[7]),
                       as.numeric((cml_sum$HP13[7] - cml_sum$HP19[7]) / cml_sum$HP13[7]),
                       as.numeric((cml_sum$HP13[7] - cml_sum$HP20[7]) / cml_sum$HP13[7]),
                       as.numeric((cml_sum$HP13[7] - cml_sum$HP21[7]) / cml_sum$HP13[7]),
                       as.numeric((cml_sum$HP13[7] - cml_sum$HP22[7]) / cml_sum$HP13[7]),
                       as.numeric((cml_sum$HP13[7] - cml_sum$HP23[7]) / cml_sum$HP13[7]),
                       as.numeric((cml_sum$HP13[7] - cml_sum$HP24[7]) / cml_sum$HP13[7])
                     )
  )
  
  HP13 <- total_cml[1:2]
  HP13$Cml <- as.numeric(HP13$Cml)
  total_cml <- total_cml[-1]
  total_cml <-  cbind(total_cml[1], lapply(total_cml[-1], as.numeric))
  
  if(outcome == "SymIllness"){
    averted <- cbind(total_cml[1], total_cml[-1] * (HP13$Cml/100) * 306.8e6)
  }else{
    averted <- cbind(total_cml[1], total_cml[-1] * (HP13$Cml*100000))
  }
  
  colnames(averted) <- c("team", gsub("_", "", scenarios[-1]))
  a <- melt(averted, id.vars = c("team"), variable.name = "Averted")
  names(a) <- c("Team", 'Scenario', "Averted")
  
  # Percent Averted --------------------------------------------------------------------------------
  
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
  
  a$Scenario <- gsub(find, replace, a$Scenario)
  
  a$x <- 1
  a$Team <- factor(x = a$Team, levels = ens.levels)
  
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
  
  r <- read_overall_mean(scn = scenarios[9], outcome)
  p <- get_peaks(r[[1]])
  p$Scenario <- gsub("_", "", scenarios[9])
  peak.weeks <- rbind(peak.weeks, p)
  cml <- r[[2]] %>% filter(Bin == "sm.mean")
  total_cml$Cml9 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
  
  r <- read_overall_mean(scn = scenarios[10], outcome)
  p <- get_peaks(r[[1]])
  p$Scenario <- gsub("_", "", scenarios[10])
  peak.weeks <- rbind(peak.weeks, p)
  cml <- r[[2]] %>% filter(Bin == "sm.mean")
  total_cml$Cml10 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
  
  r <- read_overall_mean(scn = scenarios[11], outcome)
  p <- get_peaks(r[[1]])
  p$Scenario <- gsub("_", "", scenarios[11])
  peak.weeks <- rbind(peak.weeks, p)
  cml <- r[[2]] %>% filter(Bin == "sm.mean")
  total_cml$Cml11 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
  
  r <- read_overall_mean(scn = scenarios[12], outcome)
  p <- get_peaks(r[[1]])
  p$Scenario <- gsub("_", "", scenarios[12])
  peak.weeks <- rbind(peak.weeks, p)
  cml <- r[[2]] %>% filter(Bin == "sm.mean")
  total_cml$Cml12 <- (total_cml$Peak.Magnitude - cml$Peak.Magnitude[which(cml$Bin == "sm.mean")]) / total_cml$Peak.Magnitude 
  
  team <- as.character(total_cml$team)
  
  total_cml <- cbind(factor(x = c(team), 
                            levels = ens.levels),
                     as.data.frame(lapply(total_cml[-(1:2)], as.numeric)))
  names(total_cml)[1] <- "team"
  
  total_cml <- rbind(total_cml, c("ENS", colMeans(total_cml[-1])))
  
  total_cml <- cbind( total_cml$team,
                      as.data.frame(lapply(total_cml[-1], as.numeric)))
  
  rank <- as.data.frame(rbind(rank(-total_cml[1,2:12]),
                              rank(-total_cml[2,2:12]),
                              rank(-total_cml[3,2:12]),
                              rank(-total_cml[4,2:12]),
                              rank(-total_cml[5,2:12]),
                              rank(-total_cml[6,2:12]),
                              rank(-total_cml[7,2:12]))
  )
  
  rank$team <- factor(x = ens.levels, 
                      levels = ens.levels)
  
  names(rank)[12] <- "team"
  r <- melt(rank, id.vars = c("team"), variable.name = "Reduction")
  
  colnames(total_cml) <- c("team", gsub("_", "", scenarios[-1]))
  m <- melt(total_cml, id.vars = c("team"), variable.name = "Reduction")
  colnames(m) <- c("Team", "Scenario", "Reduction")
  m$Reduction <- m$Reduction*100
  m$Rank <- r$value
  
  m$Scenario <- gsub(find, replace, m$Scenario)
  
  # Peak Week -----------------------------------------------------------------------------------
  
  pw <- spread(peak.weeks, Scenario, peakWeek)
  
  pw$Delay2 <- pw$HP14 - pw$HP13
  pw$Delay3 <- pw$HP15 - pw$HP13
  pw$Delay4 <- pw$HP16 - pw$HP13
  pw$Delay5 <- pw$HP17 - pw$HP13
  pw$Delay6 <- pw$HP18 - pw$HP13
  pw$Delay7 <- pw$HP19 - pw$HP13
  pw$Delay8 <- pw$HP20 - pw$HP13
  pw$Delay9 <- pw$HP21 - pw$HP13
  pw$Delay10 <- pw$HP22 - pw$HP13
  pw$Delay11 <- pw$HP23 - pw$HP13
  pw$Delay12 <- pw$HP24 - pw$HP13
  
  pw <- pw[-2:-13]
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
  
  return(list(long_ens, rbind(a,t,m,p), table, age))
}

# Inputs -----------------------------------------------------------------------------------------

setwd("C:/Users/ppf6/Desktop/GitHub/FluCode/comparison-scripts/Ensemble/")

path <- "../../model-forecasts/"

team.folder <- c("Columbia/COL/EPI2/", "Northeastern/EPI2/",  "UTAustin/", "UVirginia/", "Imperial/", "Columbia/COL2/EPI2/")
team.abbrev <- c("COL", "NEU", "UTA", "UVA", "IMP", "COL2")
team.levels <- c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA")
ens.levels <- c("ENS", "UVA", "UTA", "NEU3", "IMP", "COL2", "COL")
file.types <-c("SymIllness", "Hosp", "Deaths", "AntiviralTX")

scenarios = c("HP13_", "HP14_", "HP15_", "HP16_", "HP17_", "HP18_", "HP19_", "HP20_", "HP21_", "HP22_", "HP23_", "HP24_")

# Symptomatic Illnesses --------------------------------------------------------------------------

output <- calculate_metrics(scenarios, outcome = file.types[1], team.levels, team.abbrev, team.folder, ens.levels, find = "HP", replace = "HP")

write.csv(output[[1]], paste0(path, "MeanBased-Ensemble/PAN2_symillness_ensemble.csv"))

write.csv(output[[2]], paste0(path,"/MeanBased-Ensemble/PAN2_symillness_metrics.csv"))

write.csv(output[[3]], paste0(path,"/MeanBased-Ensemble/PAN2_symillness_table.csv"))

write.csv(output[[4]], paste0(path,"/MeanBased-Ensemble/PAN2_symillness_base_age.csv"))

# Hospitalizations --------------------------------------------------------------------------

output <- calculate_metrics(scenarios, outcome = file.types[2], team.levels, team.abbrev, team.folder, ens.levels, find = "HP", replace = "HP")

# write.csv(output[[1]], paste0(path, "MeanBased-Ensemble/PAN2_hospitalization_ensemble.csv"))

write.csv(output[[2]], paste0(path,"/MeanBased-Ensemble/PAN2_hospitalization_metrics.csv"))

write.csv(output[[3]], paste0(path,"/MeanBased-Ensemble/PAN2_hospitalization_table.csv"))

# Deaths --------------------------------------------------------------------------

output <- calculate_metrics(scenarios, outcome = file.types[3], team.levels, team.abbrev, team.folder, ens.levels, find = "HP", replace = "HP")

# write.csv(output[[1]], paste0(path, "MeanBased-Ensemble/PAN2_death_ensemble.csv"))

write.csv(output[[2]], paste0(path,"/MeanBased-Ensemble/PAN2_death_metrics.csv"))

write.csv(output[[3]], paste0(path,"/MeanBased-Ensemble/PAN2_death_table.csv"))

write.csv(output[[4]], paste0(path,"/MeanBased-Ensemble/PAN2_death_base_age.csv"))


