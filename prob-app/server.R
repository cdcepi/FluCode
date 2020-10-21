library(shiny)
library(ggplot2)
library(data.table)
library(magrittr)
library(grid)
library(doBy)
library(dplyr)

getBins <- function(lwr = 0, upr, inc){
  bins <- seq(lwr, upr, by = inc) %>% round(2)
  bins <- data.frame(lwr = bins, upr = bins+inc)
  
  
  bins$label = paste0('[', bins$lwr, '-', bins$upr, ')')
  
  bins$upr = ifelse(bins$upr > upr+1e-4, Inf, bins$upr)
  bins$label = ifelse(is.infinite(bins$upr), paste0('[', bins$lwr, '-', bins$upr, ']'), bins$label)
  bins
  
}

cleanBins <- function(x){
  x <- gsub('\\.0-', '-', x)
  x <- gsub('\\.0)', ')', x)
  gsub('Inf)', 'Inf]', x)
}


cleanFilePath <- function(x){
  x <- strsplit(x, '/') %>% unlist
  
  x[length(x)]
}

getType <- function(x){
  if(grepl('SymIllness', x)) 'ill'
  else if(grepl('Hosp', x)) 'hosp'
  else if(grepl('Death', x)) 'mort'
  else if(grepl('Antiviral', x)) 'meds'
  else NULL
}



getCmlPlots  <- function(d, type, scn, pTitle =''){
  if(is.null(d) | nrow(d) == 0)
    return(ggplot(d) + geom_blank() + theme_bw())
  
  reqBins <- switch(type,
                    ill = getBins(0, 39.8, .2),
                    hosp = getBins(0, 119.8, .2),
                    mort = getBins(0, 11.98, .02),
                    meds = getBins(0, 7.98, .02)
  )
  
  if(scn == 'HP'){
    reqBins <- switch(type,
                      ill = getBins(0, 39.8, .2),
                      hosp = getBins(0, 599, 1),
                      mort = getBins(0, 59.9, .1),
                      meds = getBins(0, 7.98, .02)
    )
  }
  
  reqCol <- 4
  if(scn == 'HP')
    reqCol = 6
  
  req.sig = 1
  if(scn == 'HP' & type == 'hosp'){
    req.sig = 0
  } else if(type == 'meds' | (type == 'mort' & scn == 'RL')){
    req.sig = 2
  }
  
  d.pt <- d[Bin_cml == 'sm.mean', j = list(model = Version, scenario = Scenario, Bin = Bin_cml, 
                                         value = round(Cml, req.sig))]
  d.pt <- merge(as.data.frame(d.pt), reqBins, all.x = T) %>% data.table()
  d.pt <- d.pt[value >= lwr & value < upr-1e-4, j = list(model, scenario, value, Bin = label)]
  d.pt$Bin = factor(d.pt$Bin, levels = reqBins$label)
  
  
  d <- d[!grepl('sm.', Bin_cml), j = list(model = Version, scenario = Scenario, Bin = Bin_cml, value = round(Cml, 4))]
  
  d$Bin = factor(d$Bin, levels = reqBins$label)
  
  super <- merge(unique(d$model), unique(d$scenario))
  setnames(super, c('model', 'scenario'))
  
  super = merge(super, data.frame(Bin = reqBins$label))
  
  d <- merge(d, super, by = c('model', 'scenario', 'Bin'), all = T)
  
  d[is.na(value)]$value = 5e-5
  d$value <- pmax(pmin(d$value, .999), 5e-5) %>% log 
  
  p <- ggplot(d) +
    geom_tile(aes(y = model, x = as.integer(Bin), fill = value)) +
    scale_fill_gradient2(low = 'white', midpoint = -5, mid = 'yellow', high = 'red') +
    scale_x_continuous(breaks = seq(1, nrow(reqBins), nrow(reqBins)/10), 
                       labels = reqBins$label[seq(1, nrow(reqBins), nrow(reqBins)/10)]) +
    geom_point(data = d.pt, aes(x = as.integer(Bin), y = model), color = 'darkblue', size=.9) +
    facet_wrap(~scenario, ncol=reqCol) + 
    theme_bw() + labs(x = 'Cumulative estimate', y = 'Model', title = pTitle) +
    theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
          panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(angle = 90, size=10, vjust=.5, hjust = 1), axis.ticks.x = element_blank(), 
          axis.text.y = element_text(size=10), legend.position = 'bottom')
  
  return(p)
}



getPIPlots <- function(d, type, scn, pTitle=''){
  if(is.null(d) | nrow(d) == 0)
    return(ggplot(d) + geom_blank() + theme_bw())
  
  reqBins <- switch(type,
                    ill = getBins(0, 19.9, .1),
                    hosp = getBins(0, 59.9, .1),
                    mort = getBins(0, 5.99, .01),
                    meds = getBins(0, 3.99, .01)
  )
  
  if(scn == 'HP'){
    reqBins <- switch(type,
                      ill = getBins(0, 19.9, .1),
                      hosp = getBins(0, 149.75, .25),
                      mort = getBins(0, 14.975, .025),
                      meds = getBins(0, 3.99, .01)
    )
  }
  
  reqCol <- 4
  if(scn == 'HP')
    reqCol = 6
  
  req.sig = 1
  if(scn == 'HP' & type == 'mort'){
    req.sig = 3
  } else if(type == 'meds' | (type == 'hosp' & scn == 'HP') | (type == 'mort' & scn == 'RL')){
    req.sig = 2
  }
  
  d.pt <- d[Bin_cml == 'sm.mean', j = list(model = Version, scenario = Scenario, Bin, value = round(Peak.Magnitude, req.sig))]
  d.pt <- merge(as.data.frame(d.pt), reqBins, all.x = T) %>% data.table()
  d.pt <- d.pt[value >= lwr & value < upr-1e-4, j = list(model, scenario, value, Bin = label)]
  d.pt$Bin = factor(d.pt$Bin, levels = reqBins$label)
  
  
  d <- d[!grepl('sm.', Bin), j = list(model = Version, scenario = Scenario, Bin, value = round(Peak.Magnitude, 4))]
  
  d$Bin = factor(d$Bin, levels = reqBins$label)  

  super <- merge(unique(d$model), unique(d$scenario))
  setnames(super, c('model', 'scenario'))
  
  super = merge(super, data.frame(Bin = reqBins$label))
  
  d <- merge(d, super, by = c('model', 'scenario', 'Bin'), all = T)
  
  d[is.na(value)]$value = 5e-5
  d$value <- pmax(pmin(d$value, .999), 5e-5) %>% log 
  
  
  p <- ggplot(d) +
    geom_tile(aes(y = model, x = as.integer(Bin), fill = value)) +
    scale_fill_gradient2(low = 'white', midpoint = -5, mid = 'yellow', high = 'red') +
    scale_x_continuous(breaks = seq(1, nrow(reqBins), nrow(reqBins)/10), 
                       labels = reqBins$label[seq(1, nrow(reqBins), nrow(reqBins)/10)]) +
    geom_point(data = d.pt, aes(x = as.integer(Bin), y = model), color = 'darkblue', size=.9) +
    facet_wrap(~scenario, ncol=reqCol) + 
    theme_bw() + labs(x = 'Peak magnitude estimate', y = 'Model', title = pTitle) +
    theme(axis.title = element_text(size=12), strip.text = element_text(size=10), 
          panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(angle = 90, size=10, vjust=.5, hjust = 1), axis.ticks.x = element_blank(), 
          axis.text.y = element_text(size=10), legend.position = 'bottom')
  
  return(p)
}


#############

baseDir <- '../model-forecasts/'
d <- list(ill = NULL, hosp = NULL, mort = NULL, meds = NULL)

# weeks of projections
subWeeks <- paste0('W', formatC(1:52, flag = '0', width=2))

reqTypeMap <- data.table(type = c('ill', 'hosp', 'mort', 'meds'), 
                         name = c('Symp. Illness', 'Hospitalizations', 'Deaths', 'Antiviral RX'))


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
    
    if('Week53' %in% names(dSub))
      dSub$Week53 = NULL
    
    if('Week54' %in% names(dSub))
      dSub$Week54 = NULL
    
    f <- cleanFilePath(f)
    file.name <- strsplit(f, '_') %>% unlist
    
    dSub <- dSub[Cml > 0 | Peak.Magnitude > 0, 
                 j = list(Agegroup, Bin_cml = cleanBins(as.character(Bin_cml)), Cml = round(Cml, 4), 
                          Bin = cleanBins(as.character(Bin)), Peak.Magnitude = round(Peak.Magnitude, 4))]
    
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
    
    d[[type]] <<- rbind(d[[type]], dSub)
    
  }
  
  d <<- lapply(d, function(x){
    x$Agegroup = gsub('years', '', x$Agegroup) %>% trimws
    x$Agegroup = gsub('yr', '', x$Agegroup) %>% trimws
    x$Agegroup = factor(x$Agegroup, levels = c('Overall', '0-4', '5-17', '18-49', '50-64', '65+'))
    
    x
  })
  

  #tab1
  output$cmlPlot <- renderPlot({
    reqGrp <- input$grp
    reqScn <- input$scn
    reqType <- reqTypeMap[name == input$outcome]$type
    
    reqGrp <- gsub('years', '', reqGrp) %>% trimws
    
    if(any(sapply(d, function(x) !is.null(x)))){
      dSub <- d[[reqType]]
      dSub <- dSub[Agegroup == reqGrp & grepl(reqScn, Scenario)]#  & Version %in% reqVersions
      
      if(nrow(dSub) > 0)
        getCmlPlots(dSub, reqType, reqScn, pTitle=paste0(input$outcome, ' - ', reqGrp))
    }
  })
  
  
  #tab2
  output$peakIntPlot <- renderPlot({
    reqGrp <- input$grp
    reqScn <- input$scn
    reqType <- reqTypeMap[name == input$outcome]$type
    
    reqGrp <- gsub('years', '', reqGrp) %>% trimws
    
    if(any(sapply(d, function(x) !is.null(x)))){
      dSub <- d[[reqType]]
      dSub <- dSub[Agegroup == reqGrp & grepl(reqScn, Scenario)]#  & Version %in% reqVersions
      
      if(nrow(dSub) > 0)
        getPIPlots(dSub, reqType, reqScn, pTitle=paste0(input$outcome, ' - ', reqGrp))
    }
  })
  

  

  # tab3
  output$scnLegend <- renderImage({
    filename <- normalizePath(file.path('./Legend.png'))
    list(src = filename)
  }, deleteFile = F)
})

