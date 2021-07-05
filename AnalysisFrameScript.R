BiocManager::install("devtools", force = T)
library(devtools)
source_url("https://github.com/ltoker/GeneralRscripts/blob/main/generalFunc.R?raw=T")

ResultsPath = "Results"
if(!ResultsPath %in% list.dirs(full.names = F)){
  dir.create(ResultsPath)
}

ResultsPath = paste0(ResultsPath, "/")


#Preparing the data for analysis
source("ProjectScripts/PrepareData.R")

#Calculations and plotting of descriptive epidemiology - incidence, prevalence and mortality
source("ProjectScripts/EpiPlotting.R")

#Survival analysis - Kaplan-Meier
source("ProjectScripts/SurvivalAnalyses.R")

saveRDS(session_info(), "SessionInf.Rds")
