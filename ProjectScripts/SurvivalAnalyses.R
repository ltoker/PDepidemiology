source("ProjectScripts/PrepareData.R")
install_github("https://github.com/eliocamp/ggnewscale")
library("ggnewscale")
packageF("grid")
packageF("epitools")
packageF("ggfortify")
packageF("coxphw")
packageF("ggpubr")
packageF("survminer")
packageF("survival")
source_url("https://github.com/kassambara/survminer/blob/master/R/ggcoxzph.R?raw=T")


StatBankDataSurvival <- StatBankDataPerYear %>% filter(Year > 2004, Year <= 2017, Age > 30)

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
GeneralSurvival <- merge(GeneralSurvival, AgeGroups5, by.x = "AgeStart", by.y = "Age")

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

SurvivalCombined <- rbind(GeneralSurvival %>% select(AgeStart, AgeGroup, AgeGroup2, Sex, ObsTime, Status) %>% mutate(Population = "Global"),
                          IndividualMortality %>% select(AgeStart, AgeGroup, AgeGroup2, Sex, ObsTime, Status) %>% mutate(Population = "PD")) %>%
  mutate(AgeGroup = paste0("Age_", AgeGroup),
         AgeGroup2 = paste0("Age_", AgeGroup2)) %>%
  filter(AgeGroup2 != "Age_100 or older")



SurvivalCombined$SurvObj <- with(SurvivalCombined, Surv(ObsTime, event = Status))

KMbySexandPop <- sapply(unique(SurvivalCombined$AgeGroup2), function(group){
  data = SurvivalCombined %>% filter(AgeGroup2 == group)
  survfit(SurvObj ~ Sex + Population, data = data, conf.type = "log-log")
},simplify = F)

PlotKMforAgeGroupPopultion <- sapply(names(KMbySexandPop), function(group){
  Surv <- KMbySexandPop[[group]] %>% summary %>% .$surv
  Ymin = signif(min(Surv, digits = 1))
  if(Ymin > 0.5){
    Ylim  = c(Ymin - 0.1,1)
    temp <- ggsurvplot(KMbySexandPop[[group]], data = SurvivalCombined %>% filter(AgeGroup2 == group),
                       palette = c("chocolate", "brown4", "darkolivegreen", "darkcyan"), censor = T, censor.size = 3,
                       censor.shape = 124, pval = F,conf.int = F, legend.labs = c("All_F", "PD_F", "All_M", "PD_M"),
                       legend.title = "Group")
    temp$plot <- temp$plot + coord_cartesian(ylim=Ylim)
    temp
  } else {
    ggsurvplot(KMbySexandPop[[group]], data = SurvivalCombined %>% filter(AgeGroup == group),
               palette = c("chocolate", "brown4", "darkolivegreen", "darkcyan"), surv.median.line = "hv", censor = T, censor.size = 3,
               censor.shape = 124,pval = F,conf.int = F, legend = "none")
  }
},simplify = F)


KaplanMeierPlot <- ggarrange(plotlist = sapply(names(PlotKMforAgeGroupPopultion), function(group){
  PlotKMforAgeGroupPopultion[[group]]$plot +
    labs(title = group)
}, simplify = F))

ggsave("Results/KaplanMeierPlotAll.pdf", plot = KaplanMeierPlot,
       device = "pdf", width = 15,
       height = 15, dpi = 300, useDingbats = F)


#Compare five year PD survival between cased with incidence 2005-2007 and cases with incidence
#2010-2012
SubPDsurvivalEarly <- IndividualMortality %>% filter(year(DateFirstDrug) >= 2005,
                                                     year(DateFirstDrug) <= 2007)

SubPDsurvivalLate <- IndividualMortality %>% filter(year(DateFirstDrug) >= 2010,
                                                     year(DateFirstDrug) <= 2012)

SurvivalYear <- rbind(SubPDsurvivalEarly %>% select(AgeStart, AgeGroup2,
                                                    Sex, ObsTime, Status) %>%
                        mutate(TimePeriod = "Early"),
                      SubPDsurvivalLate %>% select(AgeStart, AgeGroup2,
                                                   Sex, ObsTime, Status) %>%
                        mutate(TimePeriod = "Late")) %>%
  mutate(AgeGroup2 = paste0("Age_", AgeGroup2)) %>%
  filter(AgeGroup2 != "Age_100 or older")

SurvivalYear$Status[SurvivalYear$ObsTime > 60] <- 0 #Individual who died after more than 5 years should be considered allive
SurvivalYear$ObsTime[SurvivalYear$ObsTime > 60] <- 60  #Max observation time (in months) is 5 years

SurvivalYear %<>% filter(!AgeGroup2 %in% c("Age_30-59",  "Age_60-64",
                                          "Age_65-69", "Age_70-74"))


SurvivalYear$SurvObj <- with(SurvivalYear, Surv(ObsTime, event = Status))

#Calculate separately for males and females
KMbyYear_F <- sapply(unique(SurvivalYear$AgeGroup2), function(group){
  data = SurvivalYear %>% filter(AgeGroup2 == group, Sex == "Female")
  survfit(SurvObj ~ TimePeriod, data = data, conf.type = "log-log")
},simplify = F)

PlotKMforAgeGroupYears_F <- sapply(names(KMbyYear_F), function(group){
  Surv <- KMbyYear_F[[group]] %>% summary %>% .$surv
  Ymin = signif(min(Surv, digits = 1))
  if(Ymin > 0.5){
    Ylim  = c(Ymin - 0.1,1)
    temp <- ggsurvplot(KMbyYear_F[[group]], data = SurvivalYear %>%
                         filter(AgeGroup2 == group, Sex == "Female"),
                       palette = c("chocolate", "brown4", "darkolivegreen", "darkcyan"),
                       censor = T, censor.size = 3,linetype = "strata", risk.table = "abs_pct",
                       pval = T,
                       censor.shape = 124, conf.int = F,
                       legend.labs = c("2004-2007", "2010-2012"),
                       legend.title = "Period")
    temp$plot <- temp$plot + coord_cartesian(ylim=Ylim)
    temp
  } else {
    ggsurvplot(KMbyYear_F[[group]], data = SurvivalYear %>%
                 filter(AgeGroup2 == group, Sex == "Female"),
               palette = c("chocolate", "brown4", "darkolivegreen", "darkcyan"),
               surv.median.line = "hv", censor = T, censor.size = 3,
               inetype = "strata",risk.table = "abs_pct", pval = T,
               censor.shape = 124,conf.int = F, legend = "none")
  }
},simplify = F)


KaplanMeierYearPlot <- ggarrange(plotlist = sapply(names(PlotKMforAgeGroupYears_F), function(group){
  PlotKMforAgeGroupYears_F[[group]]$plot +
    labs(title = group)
}, simplify = F))

ggsave("Results/KaplanMeierPlotYears.pdf", plot = KaplanMeierPlot,
       device = "pdf", width = 15,
       height = 15, dpi = 300, useDingbats = F)
