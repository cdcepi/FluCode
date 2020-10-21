library(shiny)
library(ggplot2)
library(data.table)
library(magrittr)
library(grid)
library(doBy)
library(dplyr)

cleanFilePath <- function(x){
  x <- strsplit(x, '/') %>% unlist
  
  x[length(x)]
}

getScenarioType <- function(scn){
  ifelse(scn %in% paste0('HP', formatC(1:12, width=2, flag = '0')), 'EPI1',
         ifelse(scn %in% paste0('HP', 13:24), 'EPI2', 'RL'))
}

getMaxBin <- function(type, is.cml = F){
  limit = switch(type,
         ill = 20,
         hosp = 60,
         mort = 6,
         meds = 40)
  
  if(is.cml)
    limit = limit*2
  
  limit
}

getType <- function(x){
  if(grepl('SymIllness', x)) 'ill'
  else if(grepl('Hosp', x)) 'hosp'
  else if(grepl('Death', x)) 'mort'
  else if(grepl('Antiviral', x)) 'meds'
  else NULL
}

getIncPlots <- function(d, pTitle=''){
  if(is.null(d) | nrow(d) == 0)
    return(ggplot(d) + geom_blank() + theme_bw())
  
  d <- d[grepl('sm.', Bin) & Bin != 'sm.peak']
  
  d <- subset(d, select = c('Version', 'Scenario', 'Bin', paste0('Week', 1:52)))
  setnames(d, c('Version', 'Scenario', 'Bin'), c('model', 'scenario', 'measure'))
  
  d <- melt(d, id.vars = c('model', 'scenario', 'measure'), variable.name = 'week')
  d$week <- gsub('Week', '', d$week) %>% as.integer()
  d$value = round(d$value, 4)
  d <- dcast(d, model+scenario+week~measure, value.var = 'value')
  
  p <- ggplot(d) +
    geom_line(aes(x = week, y = sm.median, color =model)) +
    geom_ribbon(aes(x = week, ymin = pmax(0, sm.perc25), ymax = sm.perc75, fill = model), alpha=.1) +
    scale_color_manual(values = reqColors) +
    scale_fill_manual(values = reqColors) +
    scale_x_continuous(breaks = seq(1, 52, 6), labels = subWeeks[seq(1, 52, 6)]) +
    facet_wrap(~scenario, ncol=8, scales = 'free_y') + 
    theme_bw() + labs(y = 'Estimate', x = 'Week', title = pTitle) +
    theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
          panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(size=8, vjust = .5, hjust = 1, angle=90),
          axis.text.y = element_text(size=8), legend.position = 'bottom')
  
  return(p)
}

# getIncPlotsDiff <- function(d, pTitle=''){
#   if(is.null(d) | nrow(d) == 0)
#     return(ggplot(d) + geom_blank() + theme_bw())
#   
#   d <- subset(d, select = c('Version', 'Scenario', paste0('Week', 1:52)))
#   setnames(d, c('Version', 'Scenario'), c('model', 'scenario'))
#   
#   d <- melt(d, id.vars = c('model', 'scenario'), variable.name = 'week')
#   d$week <- gsub('Week', '', d$week) %>% as.integer()
#   d$value = round(d$value, 4)
#   
#   p <- ggplot(d) +
#     geom_line(aes(x = week, y = value, color =model)) +
#     scale_color_manual(values = reqColors) +
#     scale_x_continuous(breaks = seq(1, 52, 6), labels = subWeeks[seq(1, 52, 6)]) +
#     facet_wrap(~scenario, ncol=8, scales = 'free_y') + 
#     theme_bw() + labs(y = 'Estimate', x = 'Week', title = pTitle) +
#     theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
#           panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
#           axis.text.x = element_text(size=8, vjust = .5, hjust = 1, angle=90),
#           axis.text.y = element_text(size=8), legend.position = 'bottom')
#   
#   return(p)
# }

getCmlPlots  <- function(d, max.limit = 0, pTitle =''){
  if(is.null(d) | nrow(d) == 0)
    return(ggplot(d) + geom_blank() + theme_bw())
  
  d <- d[, j = list(model = Version, scenario = Scenario, Bin = Bin_cml, value = round(Cml, 4))]
  
  d <- d[grepl('sm.', Bin) & Bin != 'sm.peak']
  
  d$measure = d$Bin
  
  
  d <- dcast(d, model+scenario~measure, value.var = 'value')
  
  p <- ggplot(d) +
    # geom_point(aes(y = model, x = sm.mean, color = model)) +
    geom_point(aes(y = model, x = sm.median, color =model), shape=17, size=2) +
    geom_errorbarh(aes(y = model, xmin = pmax(0, sm.perc25), xmax = sm.perc75, color = model), height=0.1) +
    # geom_vline(aes(xintercept = max.limit), linetype = 'dashed') + 
    scale_color_manual(values = reqColors) +
    facet_wrap(~scenario, ncol=8, scales= 'free_x') + guides(color=F) +
    theme_bw() + labs(x = 'Cumulative estimate', y = 'Model', title = pTitle) +
    theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
          panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(size=8, vjust = .5, hjust = 1),
          axis.text.y = element_text(size=10), legend.position = 'bottom')
  
  return(p)
}

getPlotsDiff  <- function(d, pTitle =''){
  if(is.null(d) | nrow(d[!is.na(value)]) == 0)
    return(ggplot(d) + geom_blank() + theme_bw())
  
  p <- ggplot(d) +
    geom_bar(aes(x = model, y = value, fill = model), stat = 'identity', color = 'black', width=.5, alpha=.2) +
    geom_text(aes(x = model, y = 0, label = round(value)), color = 'black', size=3, vjust = -.5) +
    geom_hline(yintercept = 0) +
    scale_fill_manual(values = reqColors) +
    facet_wrap(~scenario, ncol=8) + guides(fill=F) +
    theme_bw() + labs(y = 'Estimate, Diff (%) from baseline', x = 'Model', title = pTitle) +
    theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
          panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(size=8, hjust = .5),
          axis.text.y = element_text(size=8), legend.position = 'bottom')
  
  return(p)
}


getRankPlots <- function(d, pTitle =''){
  if(is.null(d) | nrow(d[!is.na(value)]) == 0)
    return(ggplot(d) + geom_blank() + theme_bw())
  
  p <-   ggplot(d) +
    geom_raster(aes(y = Version, x = Scenario, fill = value)) +
    scale_fill_gradient2(low = 'darkgreen', mid = 'white', midpoint = -50, high = 'lightblue', na.value = 'lightgrey') +
    geom_text(aes(y = Version, x = Scenario, label = value.rank), size=4) +
    facet_wrap(Scenario.type~measure, ncol = 2, scales = 'free_x') +
    theme_minimal() + 
    labs(y = 'Model', x = 'Scenario', fill = 'Reduction, %', title = pTitle) +
    theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
          panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(size=10, hjust = .5),
          axis.text.y = element_text(size=10), legend.position = 'bottom')
  
  return(p)
}

getPIPlots <- function(d, max.limit = 0, week = 'Peak.Magnitude', pTitle=''){
  if(is.null(d) | nrow(d) == 0)
    return(ggplot(d) + geom_blank() + theme_bw())
  
  setnames(d, week, 'value')
  
  d <- d[, j = list(model = Version, scenario = Scenario, Bin, value = round(value, 4))]
  
  d <- d[grepl('sm.', Bin) & Bin != 'sm.peak']
  
  d$measure = d$Bin
  
  
  d <- dcast(d, model+scenario~measure, value.var = 'value')
  
  p <- ggplot(d) +
    # geom_point(aes(y = model, x = sm.mean, color = model)) +
    geom_point(aes(y = model, x = sm.median, color =model), shape=17, size=2) +
    geom_errorbarh(aes(y = model, xmin = pmax(0, sm.perc25), xmax = sm.perc75, color = model), height=0.1) +
    # geom_vline(aes(xintercept = max.limit), linetype = 'dashed') + 
    scale_color_manual(values = reqColors) +
    facet_wrap(~scenario, ncol=8, scales= 'free_x') + guides(color=F) +
    theme_bw() + labs(x = paste0(week, ' estimate'), y = 'Model', title = pTitle) +
    theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
          panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(size=8, vjust = .5, hjust = 1),
          axis.text.y = element_text(size=10), legend.position = 'bottom')
  
  return(p)
}

getPeakPlots  <- function(d, pTitle=''){
  if(is.null(d) | nrow(d) == 0)
    return(ggplot(d) + geom_blank() + theme_bw())


  d <- melt(d[grepl('peak', tolower(Bin)) | grepl('peak', tolower(Bin_cml))], id.vars = c('Agegroup', 'Scenario', 'Version'),
            measure.vars = paste0('Week', 1:52),
            variable.name = 'week', value.name = 'value')

  d$week <- factor(d$week, levels = paste0('Week', 1:52), labels = subWeeks)

  d[value < 5e-5]$value <- NA
  d$value = round(d$value, 4)

  p <- ggplot(d) +
    geom_tile(aes(x = as.integer(week), y = factor(Version), fill = log(value)), alpha=.5, na.rm = T) +
    scale_fill_gradient(low = 'yellow', high = 'red', na.value = 'white') +
    facet_wrap(~Scenario, ncol = 8) +
    scale_x_continuous(breaks = seq(1, 52, 6), labels = subWeeks[seq(1, 52, 6)]) +
    theme_bw() + labs(x = 'Week', y = 'Model', fill = 'P, log', title = pTitle) +
    theme(axis.title = element_text(size=12), strip.text = element_text(size=10),
          panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(size=8, hjust = 1, vjust = .5, angle=90),
          axis.text.y = element_text(size=10), legend.position = 'bottom')

  return(p)
}

#############

baseDir <- '../model-forecasts/'
d <- list(ill = NULL, hosp = NULL, mort = NULL, meds = NULL)
d.diff <- list(ill = NULL, hosp = NULL, mort = NULL, meds = NULL)
d.diff.rank <- list(ill = NULL, hosp = NULL, mort = NULL, meds = NULL)

# weeks of projections
subWeeks <- paste0('W', formatC(1:52, flag = '0', width=2))

reqTypeMap <- data.table(type = c('ill', 'hosp', 'mort', 'meds'), 
                         name = c('Symp. Illness', 'Hospitalizations', 'Deaths', 'Antiviral RX'))

reqColors = c('orange', 'red', 'blue', 'darkgreen', 'darkgrey', 'purple')

# ----

# Define server logic 
shinyServer(function(input, output, session) {
  for(f in list.files(baseDir, '.csv', recursive = T)){
    if(grepl('Northeastern/RL/', f) | grepl('Northeastern/RL2/', f))
      next
    
    print(f)
    
    type <- getType(f)
    
    if(is.null(type))
      next
    
    dSub <- read.csv(paste0(baseDir, f), header = T) %>% data.table
    if('PeakMagnitude' %in% names(dSub))
      setnames(dSub, 'PeakMagnitude', 'Peak.Magnitude')
    
    if('Week53' %in% names(dSub))#UVA
      dSub$Week53 = NULL
    
    if('Week54' %in% names(dSub))#UVA
      dSub$Week54 = NULL
    
    f <- cleanFilePath(f)
    file.name <- strsplit(f, '_') %>% unlist
    
    if(length(file.name) == 3){# convention
      dSub$Scenario = file.name[1]
      dSub$Version <- gsub('.csv', '', file.name[3])
    } else if(length(file.name) == 4){ #UVA uses underscore in scenario-id
      dSub$Scenario = paste0(file.name[1], file.name[2])
      dSub$Version <- gsub('.csv', '', file.name[4])
    } else if(length(file.name) == 1){ # Imperial missing underscores
      file.name = gsub('.csv', '', file.name)
      
      dSub$Scenario = substr(file.name, 1, 4)
      dSub$Version <- substr(file.name, nchar(file.name)-2, nchar(file.name))
    }
    
    dSub$Type = type
    
    d[[type]] <<- rbind(d[[type]], dSub[grepl('sm.', Bin) | grepl('sm.', Bin_cml) | grepl('Peak', Bin_cml)])
    
  }
  
  # clean age groups
  d <<- lapply(d, function(x){
    x$Agegroup = gsub('years', '', x$Agegroup) %>% trimws
    x$Agegroup = gsub('yr', '', x$Agegroup) %>% trimws
    x$Agegroup = factor(x$Agegroup, levels = c('Overall', '0-4', '5-17', '18-49', '50-64', '65+'))
    
    x$Location = NULL
    
    x
  })
  
  # Calculate difference from baseline
  # For each model, under each scenario, we calculate the impact from interventions rel to baseline (RL01, HP01, hp13)
  # impact is difference as a percent of baseline; -ve indictes decrease from baseline
  d.diff <<- lapply(d, function(x){
    x =  subset(x[Bin == 'sm.mean'], select = -c(Type, Bin, Bin_cml))%>%
      melt(id.vars = c('Agegroup', 'Scenario', 'Version'), variable.name = 'Measure', value.name = 'Estimate')
    
    x$Estimate = round(x$Estimate, 2)
    
    refs <- x[Scenario %in% c('HP01', 'HP13', 'RL01')]
    x$Scenario.ref = ifelse(x$Scenario %in% paste0('HP', formatC(1:12, width=2, flag = '0')), 'HP01',
                            ifelse(x$Scenario %in% paste0('HP', formatC(13:24, width=2, flag = '0')), 'HP13', 'RL01'))
    
    x <- merge(x, refs, by.x = c('Agegroup', 'Scenario.ref', 'Version', 'Measure'), 
               by.y = c('Agegroup', 'Scenario', 'Version', 'Measure'),
               suffixes = c('', '.ref'), all.x = T)
    x$Estimate = ifelse(x$Estimate.ref == 0 | is.na(x$Estimate.ref), NA,  
                        round((x$Estimate - x$Estimate.ref)*100/x$Estimate.ref, 2)) # *100/x$Estimate.ref
    
    x$Scenario.ref = x$Estimate.ref = NULL
    
    dcast(x, Agegroup+Scenario+Version~Measure, value.var = 'Estimate')
  })
  
  

  
  d.diff.rank <<- lapply(d.diff, function(x){
    splitBy(~Agegroup+Scenario.type+Version, x[, j = list(Agegroup, Version, Scenario, 
                                                          Scenario.type = getScenarioType(Scenario), Cml, Peak.Magnitude)]) %>%
      lapply(function(grp){
        grp[Scenario %in% c('RL01', 'HP01', 'HP13')]$Cml = NA
        grp[Scenario %in% c('RL01', 'HP01', 'HP13')]$Peak.Magnitude = NA
        
        grp$Cml.rank = frankv(grp$Cml, ties.method = 'min', na.last = 'keep')
        grp$Peak.Magnitude.rank = frankv(grp$Peak.Magnitude, ties.method = 'min', na.last = 'keep')
        
        grp
      }) %>%
      rbindlist()
  })
  

  
  #tab1
  output$cmlPlot <- renderPlot({
    reqGrp <- input$grp
    reqType <- reqTypeMap[name == input$outcome]$type
    
    reqGrp <- gsub('years', '', reqGrp) %>% trimws
    
    if(any(sapply(d, function(x) !is.null(x)))){
      dSub <- d[[reqType]]
      dSub <- dSub[Agegroup == reqGrp]#  & Version %in% reqVersions
      
      max.limit = getMaxBin(reqType, is.cml = T)
      
      if(nrow(dSub) > 0)
        getCmlPlots(dSub, max.limit, pTitle=paste0(input$outcome, ' - ', reqGrp))
    }
  })
  
  
  #tab2
  output$peakIntPlot <- renderPlot({
    reqGrp <- input$grp
    reqType <- reqTypeMap[name == input$outcome]$type
    
    reqGrp <- gsub('years', '', reqGrp) %>% trimws
    
    if(any(sapply(d, function(x) !is.null(x)))){
      dSub <- d[[reqType]]
      dSub <- dSub[Agegroup == reqGrp]#  & Version %in% reqVersions
      
      max.limit = getMaxBin(reqType, is.cml = F)
      
      if(nrow(dSub) > 0)
        getPIPlots(dSub, max.limit, week = 'Peak.Magnitude', pTitle=paste0(input$outcome, ' - ', reqGrp))
    }
  })
  
  #tab3
  output$peakPlot <- renderPlot({
    reqGrp <- input$grp
    reqType <- reqTypeMap[name == input$outcome]$type

    reqGrp <- gsub('years', '', reqGrp) %>% trimws

    if(any(sapply(d, function(x) !is.null(x)))){
      dSub <- d[[reqType]]
      dSub <- dSub[Agegroup == reqGrp]  # & Version %in% reqVersions

      if(nrow(dSub) > 0)
        getPeakPlots(dSub, pTitle=paste0(input$outcome, ' - ', reqGrp))
    }
  })
  
  #tab4
  output$trajPlot <- renderPlot({
    reqGrp <- input$grp
    reqType <- reqTypeMap[name == input$outcome]$type
    
    reqGrp <- gsub('years', '', reqGrp) %>% trimws
    
    if(any(sapply(d, function(x) !is.null(x)))){
      dSub <- d[[reqType]]
      dSub <- dSub[Agegroup == reqGrp]
      
      if(nrow(dSub) > 0)
        getIncPlots(dSub, pTitle=paste0(input$outcome, ' - ', reqGrp))
    }
  })
  
  
  
  #tab5
  output$smTable <- renderDataTable({
    reqGrp <- input$grp
    reqType <- reqTypeMap[name == input$outcome]$type
    
    reqGrp <- gsub('years', '', reqGrp) %>% trimws
    
    if(any(sapply(d, function(x) !is.null(x)))){
      dSub <- d[[reqType]]
      dSub <- dSub[Agegroup == reqGrp] 
      
      if(nrow(dSub) > 0){
        dSub =  subset(dSub[grepl('sm.', Bin) & Bin != 'sm.peak'], select = -c(Type, Agegroup, Bin_cml))%>%
          melt(id.vars = c('Scenario', 'Version', 'Bin'), variable.name = 'Measure', value.name = 'Estimate')
        
        dSub$Estimate = round(dSub$Estimate, 2)
        dSub$Bin = gsub('sm.', '', dSub$Bin)
        
        dSub
        
      }
    }
  },
  rownames = F, filter = 'top', options = list(pageLength = 100))
  
  
  
  ######### Diff plots
  #tab6
  output$cmlPlotDiff <- renderPlot({
    reqGrp <- input$grp
    reqType <- reqTypeMap[name == input$outcome]$type
    
    reqGrp <- gsub('years', '', reqGrp) %>% trimws
    
    if(any(sapply(d.diff, function(x) !is.null(x)))){
      dSub <- d.diff[[reqType]]
      dSub <- dSub[Agegroup == reqGrp, j = list(model = Version, scenario = Scenario, value = round(Cml, 4))]
      
      if(nrow(dSub) > 0)
        getPlotsDiff(dSub, pTitle=paste0(input$outcome, ' - ', reqGrp))
    }
  })
  
  
  #tab7
  output$peakIntPlotDiff <- renderPlot({
    reqGrp <- input$grp
    reqType <- reqTypeMap[name == input$outcome]$type
    
    reqGrp <- gsub('years', '', reqGrp) %>% trimws
    
    if(any(sapply(d.diff, function(x) !is.null(x)))){
      dSub <- d.diff[[reqType]]
      dSub <- dSub[Agegroup == reqGrp, j = list(model = Version, scenario = Scenario, value = round(Peak.Magnitude, 4))]
      
      if(nrow(dSub) > 0)
        getPlotsDiff(dSub, pTitle=paste0(input$outcome, ' - ', reqGrp))
    }
  })
  
  
  # #tab8
  # output$trajPlotDiff <- renderPlot({
  #   reqGrp <- input$grp
  #   reqType <- reqTypeMap[name == input$outcome]$type
  #   
  #   reqGrp <- gsub('years', '', reqGrp) %>% trimws
  #   
  #   if(any(sapply(d.diff, function(x) !is.null(x)))){
  #     dSub <- d.diff[[reqType]]
  #     dSub <- dSub[Agegroup == reqGrp]
  #     
  #     if(nrow(dSub) > 0)
  #       getIncPlotsDiff(dSub, pTitle=paste0(input$outcome, ' - ', reqGrp))
  #   }
  # })
  
  #tab9
  output$smTableDiff <- renderDataTable({
    reqGrp <- input$grp
    reqType <- reqTypeMap[name == input$outcome]$type
    
    reqGrp <- gsub('years', '', reqGrp) %>% trimws
    
    if(any(sapply(d.diff, function(x) !is.null(x)))){
      dSub <- d.diff[[reqType]]
      dSub <- dSub[Agegroup == reqGrp] 
      
      if(nrow(dSub) > 0){
        dSub =  subset(dSub, select = -c(Agegroup))%>%
          melt(id.vars = c('Scenario', 'Version'), variable.name = 'Measure', value.name = 'Estimate')
        
        dSub$Estimate = round(dSub$Estimate, 2)
        
        dSub
      }
    }
  },
  rownames = F, filter = 'top', options = list(pageLength = 100))
  
  
  #tab10
  output$rankPlot <- renderPlot({
    reqGrp <- input$grp
    reqType <- reqTypeMap[name == input$outcome]$type
    
    reqGrp <- gsub('years', '', reqGrp) %>% trimws
    
    if(any(sapply(d.diff.rank, function(x) !is.null(x)))){
      dSub <- d.diff.rank[[reqType]]
      
      dSub.1 = reshape2::melt(subset(dSub, Agegroup == reqGrp, select = -Agegroup), 
                              measure.vars = c('Cml', 'Peak.Magnitude'), 
                              id.vars = c('Version', 'Scenario.type', 'Scenario'), variable.name = 'measure')
      dSub.2 = reshape2::melt(subset(dSub, Agegroup == reqGrp, select = -Agegroup),
                              measure.vars = c('Cml.rank', 'Peak.Magnitude.rank'),
                              id.vars = c('Version', 'Scenario.type', 'Scenario'), variable.name = 'measure')
      dSub.2$measure = gsub('.rank', '', dSub.2$measure)
      
      dSub <- merge(dSub.1, dSub.2, by  = c('Version', 'Scenario.type', 'Scenario', 'measure'), suffixes = c('', '.rank'))
      
      
      if(nrow(dSub) > 0)
        getRankPlots(dSub, pTitle=paste0(input$outcome, ' - ', reqGrp))
    }
  })
  
  # tab11
  output$scnLegend <- renderImage({
    filename <- normalizePath(file.path('./Legend.png'))
    list(src = filename)
  }, deleteFile = F)
})


