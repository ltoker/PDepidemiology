#source("ProjectScripts/PrepareData.R")
#install_github("https://github.com/eliocamp/ggnewscale")

library("ggnewscale")
packageF("grid")
packageF("epitools")
packageF("ggfortify")
packageF("ggpubr")
packageF("survminer")
packageF("survival")
source_url("https://github.com/kassambara/survminer/blob/master/R/ggcoxzph.R?raw=T")


StatBankDataSurvival <- StatBankDataPerYear %>% filter(Year > 2004, Year <= 2017, Age > 29)

#Create pseudo observational data for general population for Kaplan-Meier analyses
CreateSurvData <- function(sex, BulkData = StatBankDataSurvival){
  SubDataList <- list(BulkData1 <- BulkData %>% filter(Year > 2004, Year <= 2015),
                      BulkData2 <- BulkData %>% filter(Year > 2005, Year <= 2016),
                      BulkData3 <- BulkData %>% filter(Year > 2006, Year <= 2017))
  
  # for(year in unique(SubDataList$BulkData3$Year)[-11]){
  #   NumberTot <- BulkData %>% filter(Year == year) %>% .$Number
  #   DeathTot <- BulkData %>% filter(Year == year) %>% .$Death
  #   NumberSub <-SubDataList$BulkData2  
  # }
  
  YearStart = min(BulkData$Year)
  YearEnd = max(BulkData$Year)
  AgeBaseNum <- BulkData %>% filter(Year == YearStart, Sex == sex) %>%
    select(Age,Number, Death)
  
  SurvivalDF <- sapply(as.character(AgeBaseNum$Age), function(age){
    age = as.numeric(age)
    Data <- data.frame(AgeStart = rep(age, AgeBaseNum %>% filter(Age == age) %>% .$Number),
                       YearDeath = NA)
    YearRun = YearStart
    while(YearRun < YearEnd){
      Death <- BulkData %>% filter(Year == YearRun, Age == age, Sex == sex) %>% .$Death
      if(sum(is.na(Data$YearDeath)) == 0 | length(Death) == 0){
        break()
      }
      if(Death == 0){
        YearRun = YearRun +1
        age = age +1
      } else {
        if(Death > nrow(Data[is.na(Data$YearDeath),])){
          Data[is.na(Data$YearDeath),]$YearDeath <- YearRun
        } else {
          Data[is.na(Data$YearDeath),]$YearDeath[1:Death] <- YearRun
        }
        YearRun = YearRun +1
        age = age +1
      }
    }
    Data
  }, simplify = F) %>% rbindlist() %>% data.frame()
  
  SurvivalDF %<>% mutate(Sex = sex,
                         Status = 1)
  
  SurvivalDF$Status[is.na(SurvivalDF$YearDeath)] <- 0 
  SurvivalDF %<>% mutate(YearStart = YearStart,
                         MonthToDeath = 12*(YearDeath - YearStart),
                         ObsTime = MonthToDeath)
  SurvivalDF %<>% mutate(ObsTime = MonthToDeath)
  SurvivalDF$ObsTime[is.na(SurvivalDF$YearDeath)] <- 12*(YearEnd - YearStart)  
  
  #Add random months
  for(i in unique(SurvivalDF$ObsTime)){
    temp <- which(SurvivalDF$ObsTime == i)
    SurvivalDF$ObsTime[temp] <- round(i+runif(length(temp), 1, 12))
  }
  
  return(SurvivalDF)
}

MaleSurvivalDF <- CreateSurvData(sex = "M")
FemaleSurvivalDF <- CreateSurvData(sex = "F")

GeneralSurvival <- rbind(MaleSurvivalDF, FemaleSurvivalDF)
GeneralSurvival <- merge(GeneralSurvival, AgeGroups, by.x = "AgeStart", by.y = "Age")

#GeneralSurvival$ObsTime[GeneralSurvival$Status == 0] <- 132

# #Create a pseudo 11 year follow up for the registry data
# 
# SubPDsurvival <- IndividualMortality %>% filter(year(DateFirstDrug) > 2004, year(DateFirstDrug) < 2007)
# SubPDsurvival %<>% mutate(AgeStart = AgeOfOnsetNew,
#                           ObsTime = MonthToEnd) 
# 
# SubPDsurvival$Status[SubPDsurvival$MonthToEnd > 132] <- 0 #Individual who died after more than 11 years should be considered allive
# SubPDsurvival$ObsTime[SubPDsurvival$MonthToEnd > 132] <- 132  #Max observation time (in months) is 11 years

IndividualMortality %<>% mutate(AgeStart = AgeOfOnsetNew,
                               ObsTime = MonthToEnd)

SurvivalCombined <- rbind(GeneralSurvival %>% select(AgeStart, AgeGroup5, AgeGroup5b, Sex, ObsTime, Status) %>% mutate(Population = "Global"),
                          IndividualMortality %>% filter(year(DateFirstDrug) > 2004) %>% select(AgeStart, AgeGroup5, AgeGroup5b, Sex, ObsTime, Status) %>% mutate(Population = "PD")) %>%
  mutate(AgeGroup5 = paste0("Age_", AgeGroup5),
         AgeGroup5b = paste0("Age_", AgeGroup5b)) %>%
  filter(AgeGroup5b != "Age_100 or older")


SurvivalCombined$ObsTime[SurvivalCombined$ObsTime >= 156] <- 156
SurvivalCombined$Status[SurvivalCombined$ObsTime >= 156] <- 0

SurvivalCombined$SurvObj <- with(SurvivalCombined, Surv(ObsTime, event = Status))

SurvivalCombined$Sex <- sapply(SurvivalCombined$Sex, function(x){
  if(grepl("^F", as.character(x))){
    "F"
  } else if(grepl("^M", as.character(x))){
    "M"
  }
})

KMbySexandPop <- sapply(unique(SurvivalCombined$AgeGroup5b), function(group){
  data = SurvivalCombined %>% filter(AgeGroup5b == group)
  survfit(SurvObj ~ Sex + Population, data = data, conf.type = "log-log")
},simplify = F)

PlotKMforAgeGroupPopultion <- sapply(names(KMbySexandPop), function(group){
  Surv <- KMbySexandPop[[group]] %>% summary %>% .$surv
  Ymin = signif(min(Surv, digits = 1))
  if(Ymin > 0.5){
    Ylim  = c(Ymin - 0.1,1)
    temp <- ggsurvplot(KMbySexandPop[[group]], data = SurvivalCombined %>% filter(AgeGroup5b == group),
                       linetype = rep(c("dashed", "solid"),2),
                       palette = c(rep(MoviePalettes$BugsLife[4], 2),
                                   rep(MoviePalettes$BugsLife[2], 2)), censor = F, censor.size = 3,
                       censor.shape = 124, pval = F,conf.int = F, legend.labs = c("All_F", "PD_F", "All_M", "PD_M"),
                       legend.title = "Group")
    temp$plot <- temp$plot + coord_cartesian(ylim=Ylim)
    temp
  } else {
    ggsurvplot(KMbySexandPop[[group]], data = SurvivalCombined %>% filter(AgeGroup5b == group),
               linetype = rep(c("dashed", "solid"),2),
               palette = c(rep(MoviePalettes$BugsLife[4], 2),
                           rep(MoviePalettes$BugsLife[2], 2)), surv.median.line = "hv", censor = F, censor.size = 3,
               censor.shape = 124,pval = F,conf.int = F, legend = "none")
  }
},simplify = F)


KaplanMeierPlot <- ggarrange(plotlist = sapply(names(PlotKMforAgeGroupPopultion), function(group){
  PlotKMforAgeGroupPopultion[[group]]$plot +
    labs(title = group)
}, simplify = F))

ggsave("Results/KaplanMeierPlotAll_new.pdf", plot = KaplanMeierPlot,
       device = "pdf", width = 15,
       height = 10, dpi = 300, useDingbats = F)


#Get 50% survival table

AllGroups = SurvivalCombined %>% mutate(Group = paste(Population, Sex, sep = "_")) %>%
  .$Group %>% unique 

SurvTable <- sapply(names(PlotKMforAgeGroupPopultion), function(Age){
  data = PlotKMforAgeGroupPopultion[[Age]]$data.survplot %>%
    mutate(Group = paste(Population, Sex, sep = "_"))
  DF <- data %>% filter(surv <= 0.5, ) %>% group_by(Group) %>%
    summarise(Surv_0.5 = min(time)) %>% data.frame() %>%
    mutate(Age = Age)

  Missing = AllGroups[!AllGroups %in% DF$Group]
  if(length(Missing) > 0){
    if(length(Missing) == 4){
      DF = data.frame(Group = Missing,
                      Surv_0.5 = "> 156",
                      Age = Age)
    } else {
      MissingDF = data.frame(Group = Missing,
                             Surv_0.5 = "> 156",
                             Age = Age)
      DF = rbind(DF, MissingDF)
    }
    
  }
  return(DF)
}, simplify = F) %>% rbindlist() %>% data.frame()

SurvTable %>% pivot_wider(names_from = Age,
                          names_sep = ".", values_from = c(Surv_0.5)) %>%
  data.frame() %>%
  write.table(paste0(ResultsPath, "SurvivalTable.tsv"), sep = "\t", row.names = F, col.names = T)

#Compare five year PD survival between cased with incidence 2005-2007 and cases with incidence
#2010-2012
SubPDsurvivalEarly <- IndividualMortality %>% filter(year(DateFirstDrug) >= 2005,
                                                     year(DateFirstDrug) <= 2007)

SubPDsurvivalLate <- IndividualMortality %>% filter(year(DateFirstDrug) >= 2010,
                                                     year(DateFirstDrug) <= 2012)

SurvivalYear <- rbind(SubPDsurvivalEarly %>% select(AgeStart, AgeGroup5b,
                                                    Sex, ObsTime, Status) %>%
                        mutate(TimePeriod = "Early"),
                      SubPDsurvivalLate %>% select(AgeStart, AgeGroup5b,
                                                   Sex, ObsTime, Status) %>%
                        mutate(TimePeriod = "Late")) %>%
  mutate(AgeGroup5b = paste0("Age_", AgeGroup5b)) %>%
  filter(AgeGroup5b != "Age_100 or older")

SurvivalYear$Status[SurvivalYear$ObsTime > 60] <- 0 #Individual who died after more than 5 years should be considered allive
SurvivalYear$ObsTime[SurvivalYear$ObsTime > 60] <- 60  #Max observation time (in months) is 5 years


SurvivalYear$SurvObj <- with(SurvivalYear, Surv(ObsTime, event = Status))

#Get Kaplan-Meier plots comparing early and late years

KMplotBySex <- function(sex, Colors = c(MoviePalettes$MoonRiseKingdomColors[4],
                                        MoviePalettes$MoonRiseKingdomColors[7])){
  KMbyYear <- sapply(unique(SurvivalYear$AgeGroup5b), function(group){
    data = SurvivalYear %>% filter(AgeGroup5b == group, Sex == sex)
    survfit(SurvObj ~ TimePeriod, data = data, conf.type = "log-log")
  },simplify = F)
  
  PlotKMforAgeGroupYears <- sapply(names(KMbyYear), function(group){
    Surv <- KMbyYear[[group]] %>% summary %>% .$surv
    Ymin = signif(min(Surv, digits = 1))
    if(Ymin > 0.5){
      Ylim  = c(Ymin - 0.1,1)
      temp <- ggsurvplot(KMbyYear[[group]], data = SurvivalYear %>%
                           filter(AgeGroup5b == group, Sex == sex),
                         palette = Colors,
                         censor = T, censor.size = 3,
                         pval = T,
                         censor.shape = 124, conf.int = F,
                         legend.labs = c("2005-2007", "2010-2012"),
                         legend.title = "")
      temp$plot <- temp$plot + coord_cartesian(ylim=Ylim)
      temp
    } else {
      ggsurvplot(KMbyYear[[group]], data = SurvivalYear %>%
                   filter(AgeGroup5b == group, Sex == sex),
                 palette = c(MoviePalettes$MoonRiseKingdomColors[4],
                             MoviePalettes$MoonRiseKingdomColors[7]),
                 surv.median.line = "hv", censor = T, censor.size = 3,
                 inetype = "strata",risk.table = "abs_pct", pval = T,
                 censor.shape = 124,conf.int = F, legend = "none")
    }
  },simplify = F)
  
  
  KaplanMeierYearPlot <- ggarrange(plotlist = sapply(names(PlotKMforAgeGroupYears), function(group){
    PlotKMforAgeGroupYears[[group]]$plot +
      labs(title = group)
  }, simplify = F))
  return(list(KMdata = KMbyYear, Plot = KaplanMeierYearPlot))
}


KMbyYear_F <- KMplotBySex(sex = "Female")
KMbyYear_M <- KMplotBySex(sex = "Male")

ggsave("Results/KaplanMeierPlotYearsF.pdf", plot = KMbyYear_F$Plot,
       device = "pdf", width = 15,
       height = 15, dpi = 300, useDingbats = F)

ggsave("Results/KaplanMeierPlotYearsM.pdf", plot = KMbyYear_M$Plot,
       device = "pdf", width = 15,
       height = 15, dpi = 300, useDingbats = F)
