BiocManager::install("devtools")
library(devtools)
source_url("https://github.com/ltoker/GeneralRscripts/blob/main/generalFunc.R?raw=T")


SetAgeGroup <- function(AgeStart = 30, AgeEnd = 99, Gap = 5, Agevec){
  DF <- data.frame(Age = unique(Agevec))
  Groups <- vector(length = length(AgeStart:AgeEnd)/Gap)
  xMin = AgeStart
  i = 1
  while(xMin < AgeEnd){
    Groups[i] <- paste0(xMin, "-", xMin+Gap-1)
    xMin = xMin + Gap
    i = i+1
  }
  GroupDF <- sapply(Groups, function(x){
    xMin = strsplit(x, "-")[[1]][1] %>% as.numeric()
    xMax = strsplit(x, "-")[[1]][2] %>% as.numeric()
    data.frame(Age = c(xMin:xMax), AgeGroup = x)
  }, simplify = F) %>% rbindlist
  
  DF <- merge(DF, GroupDF, by = "Age", all.x = T)
  DF %<>% mutate(AgeGroup = as.character(AgeGroup))
  DF$AgeGroup[DF$Age < AgeStart] <- paste0("Younger than ", AgeStart)
  DF$AgeGroup[DF$Age > AgeEnd | DF$Age == "105 or older"] <- paste0(AgeEnd + 1, " or older")
  DF$AgeGroup <- factor(DF$AgeGroup, levels = unique(DF$AgeGroup))
  names(DF)[names(DF == "AgeGroup")] <- paste("AgeGroup", Gap)
  return(DF)
}


########### Get general population demographics data from https://www.ssb.no/en/statbank ################
StatBankDataPerYear <- read.table("data/PersonerByOneAgeYearUpdated.txt", header = T, sep = "\t", comment.char = "#") %>% select(-Region)

AgeGroups <- SetAgeGroup(AgeStart = 30, AgeEnd = 99, Gap = 5, Agevec = StatBankDataPerYear$Age)
AgeGroups %<>% mutate(AgeGroup5b = as.character(AgeGroup5))
AgeGroups$AgeGroup5b[AgeGroups$Age < 60 & AgeGroups$Age > 29 ] <- "30-59"
AgeGroups$AgeGroup5b <- factor(AgeGroups5$AgeGroup2, levels = unique(AgeGroups$AgeGroup2))
AgeGroups$AgeGroup10 <- SetAgeGroup(AgeStart = 30, AgeEnd = 99, Gap = 10, Agevec = StatBankDataPerYear$Age)
AgeGroups$AgeGroup10[AgeGroups$Age < 60 & AgeGroups$Age > 29 ] <- "30-59"

StatBankDataPerYear %<>% gather(key = "Year", value = "Number", -Sex, -Age)
StatBankDataPerYear$Year <- sapply(StatBankDataPerYear$Year, function(x) gsub("X", "", x)) %>% as.numeric()
StatBankDataPerYear <- merge(StatBankDataPerYear, AgeGroups, by = "Age", all.x = T) %>% arrange(Sex, Age, Year)
StatBankDataPerYear$Sex <- sapply(StatBankDataPerYear$Sex, function(x){
  x <- gsub("Females", "F", x)
  gsub("Males", "M", x)
})

StatBankDataPerYear %<>% mutate(YearAgeSex = paste(Year, Age, Sex, sep = "_"),
                                YearAgeGroupSex5 = paste(Year, AgeGroup5, Sex, sep = "_"),
                                YearAgeGroupSex5b = paste(Year, AgeGroup5b, Sex, sep = "_"),
                                YearAgeGroupSex10 = paste(Year, AgeGroup10, Sex, sep = "_"))

StatBankDataPerYear_death <- read.table("data/DeathByOneYearUpdated.txt", header = T, sep = "\t", comment.char = "#")
StatBankDataPerYear_death %<>% gather(key = "Year", value = "Death", -Sex, -Age)
StatBankDataPerYear_death$Year <- sapply(StatBankDataPerYear_death$Year, function(x) gsub("X", "", x)) %>% as.numeric()
StatBankDataPerYear_death <- merge(StatBankDataPerYear_death, AgeGroups, by = "Age", all.x = T) %>% arrange(Sex, Age, Year)
StatBankDataPerYear_death$Sex <- sapply(StatBankDataPerYear_death$Sex, function(x){
  x <- gsub("Females", "F", x)
  gsub("Males", "M", x)
})

StatBankDataPerYear_death %<>% mutate(YearAgeSex = paste(Year, Age, Sex, sep = "_"))

StatBankDataPerYear <- merge(StatBankDataPerYear,
                             StatBankDataPerYear_death %>% select(Death, YearAgeSex),
                             by = "YearAgeSex", all.x = T, all.y = T, sort = F)
StatBankDataPerYear %<>% mutate(Mortality = 100000*Death/Number) %>% filter(Year < 2020) %>% droplevels()


#########################################################################################################

#Get PD data
IndividualMortality <- read.table("data/IndividualMortalityUpdated.txt", header = T, sep = "\t")
IndividualMortality$Sex <- sapply(IndividualMortality$PasientKjonn, function(x){
  if(x == 1){
    "Male"
  } else {
    "Female"
  }
}) %>% unlist



IndividualMortality$DateFirstDrug <- as.Date(IndividualMortality$DateFirstDrug, format =  "%d-%B-%y")
IndividualMortality$DateLastDrug <- as.Date(IndividualMortality$DateLastDrug, format =  "%d-%B-%y")
IndividualMortality %<>% mutate(DeathDate = paste0("28-", PasientDodsMnd, "-", PasientDodsAr),
                                BirthDate = paste0("01-01-", PasientFodtAr))

IndividualMortality$DeathDate[grepl("NA", IndividualMortality$DeathDate)] <- NA
IndividualMortality$DeathDate <- as.Date(IndividualMortality$DeathDate, format =  "%d-%m-%Y")
IndividualMortality$BirthDate <- as.Date(IndividualMortality$BirthDate, format =  "%d-%m-%Y")

IndividualMortality %<>% mutate(AgeOfOnsetNew = year(DateFirstDrug) - year(BirthDate))



IndividualMortality$Status <- 0
IndividualMortality[!is.na(IndividualMortality$DeathDate),]$Status <- 1
IndividualMortality %<>% mutate(EndObsDate = DeathDate)
IndividualMortality$EndObsDate[is.na(IndividualMortality$DeathDate)] <- as.Date("2018-01-01", format = "%Y-%m-%d")

IndividualMortality$MonthToEnd <- apply(IndividualMortality %>%
                                          select(EndObsDate, DateFirstDrug),1,
                                        function(Subj){
                                          12*(year(Subj[1])-year(Subj[2])) + month(Subj[1])-month(Subj[2])
                                        })

IndividualMortality %<>% mutate(DeathYearDummy = year(DeathDate))
IndividualMortality$DeathYearDummy[is.na(IndividualMortality$DeathYearDummy)] <- as.Date("01-01-0001", format("%d-%m-%Y"))



IndividualMortality$AgeAtdeathNew <-  apply(IndividualMortality %>% select(BirthDate, DeathDate), 1, function(x) {
  temp <- difftime(x[2], x[1], units = "days") %>% as.double()
  floor(temp/365)
})


IndividualMortality <- merge(IndividualMortality, AgeGroups, by.x = "AgeOfOnsetNew", by.y = "Age", all.x = T) 

IndividualMortality$YearsOnPrescription <- c(difftime(IndividualMortality$DateLastDrug,
                                                      IndividualMortality$DateFirstDrug, units = "days")  %>% as.double)/360

IndividualMortality$YearsOffPrescription <- c(difftime(IndividualMortality$EndObsDate,
                                                     IndividualMortality$DateLastDrug, units = "days")  %>% as.double)/360



IndividualMortalityAll <- IndividualMortality

ggplot(IndividualMortality %>% filter(!is.na(AgeGroup5)), aes(YearsOnPrescription, YearsOffPrescription)) +
  theme_bw() +
  geom_point(size = 0.3) +
  facet_wrap(~AgeGroup)

ggplot(IndividualMortality %>% filter(!is.na(AgeGroup5)), aes(YearsOnPrescription, YearsOffPrescription)) +
  theme_bw() +
  geom_point(size = 0.5)

#Filter patients with treated for more than one year and untreated for more than two years
# with age of onset < 75, and subjects younger than 30 
IndividualMortality %<>% filter(MonthToEnd > 0,
                                !(YearsOnPrescription < 1 & YearsOffPrescription > 2 & AgeOfOnsetNew < 80),
                                AgeGroup5 != "Younger than 30")



DeathPD <- IndividualMortality %>% filter(year(DateFirstDrug) < 2017) %>%
  group_by(AgeAtdeath, PasientDodsAr, Sex) %>% summarise(DeathPD = n(), .groups = 'drop') %>%
  data.frame() %>% mutate(YearAgeSex = paste(PasientDodsAr, AgeAtdeath, Sex, sep = "_"))

IncidencePD <- sapply(unique(year(IndividualMortality  %>%
                                    filter(year(DateFirstDrug) > 2004,
                                           year(DateFirstDrug) < 2017) %>%
                                    .$DateFirstDrug)) %>% sort, function(Year){
                                      data <- IndividualMortalityAll %>%
                                        filter(year(IndividualMortalityAll$DateFirstDrug) == Year)
                                      data %<>% mutate(Year = Year) 
                                      group_by(data, AgeOfOnsetNew, Sex) %>% summarise(PDnew = n(),
                                                                                       Year = unique(Year),
                                                                                       .groups = 'drop') %>%
                                        mutate(YearAgeSex = paste(Year, AgeOfOnsetNew, Sex, sep = "_"))
                                    }, simplify = F) %>% rbindlist() %>% data.frame()

PrevalencePD <- sapply(c(2004:2016), function(Year){
  data <- IndividualMortality %>% filter(year(DateFirstDrug) <= Year, 
                                            PasientDodsAr >= Year)
  data %<>% mutate(Year = Year,
                   Age = Year - PasientFodtAr)
  group_by(data, Age, Sex) %>% summarise(PrevalenceRaw = n(), Year = unique(Year), .groups = 'drop') %>%
    mutate(YearAgeSex = paste(Year, Age, Sex, sep = "_"))
}, simplify = F) %>% rbindlist() %>% data.frame() %>% filter(Year > 2004)



SummarizedPD <- merge(IncidencePD  %>% select(-Sex, -AgeOfOnsetNew, -Year),
                      PrevalencePD %>% select(-Sex, -Age, -Year), by = "YearAgeSex", all.x = T, all.y = T)
SummarizedPD <- merge(SummarizedPD,  DeathPD %>%
                        select(-Sex, -AgeAtdeath, -PasientDodsAr), by = "YearAgeSex", all.x = T, all.y = T)


SummarizedPD$YearAgeSex <- sapply(SummarizedPD$YearAgeSex, function(x){
  gsub("emale|ale", "", x)
})

CombinedData <- merge(StatBankDataPerYear, SummarizedPD,
                      by = "YearAgeSex", all.x = T) %>%
  filter(Age > 29) 

#Filling in 0 for the data points for which no records were received
CombinedData$PDnew[CombinedData$Year > 2004 &
                     CombinedData$Year < 2017 &
                     is.na(CombinedData$PDnew)] <- 0
CombinedData$PrevalenceRaw[CombinedData$Year > 2004 &
                             CombinedData$Year < 2017 &
                             is.na(CombinedData$PrevalenceRaw)] <- 0
CombinedData$DeathPD[CombinedData$Year > 2004 &
                             CombinedData$Year < 2017 &
                             is.na(CombinedData$DeathPD)] <- 0



CombinedData %<>%  mutate(Mortality = 100000*Death/Number,
                          Incidence = 100000*PDnew/Number,
                          Incidence2 = 100000*PDnew/(Number - PrevalenceRaw + PDnew),
                          Prevalence = 100000*PrevalenceRaw/Number,
                          MortalityPD = 100000*DeathPD/PrevalenceRaw,
                          BirthDate = Year - Age)


CombinedData$BirthEvent <- sapply(CombinedData$BirthDate, function(x){
  if(x >= 1914 & x <= 1918){
    "WWI"
  } else if(x >= 1939 & x <= 1945){
    "WWII"
  } else {
    "None"
  }
})

#Plot Incidence per Age year
ggplot(CombinedData %>% filter(Year < 2017, Year > 2004, !is.na(Incidence), Incidence > 0), aes(Age, Incidence)) +
  theme_bw() +
  theme(panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x ="", y = "Incidence (per 100,000)", title = "PD - definite") +
  scale_fill_manual(values = c("brown", "darkgreen")) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year))) +
  geom_smooth() +
  facet_wrap(~Sex)

#Plot Prevalence per Age year
ggplot(CombinedData %>% filter(Year < 2017, Year > 2004, !is.na(Prevalence), Prevalence >= 0), aes(Age, Prevalence)) +
  theme_bw() +
  theme(panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x ="", y = "Prevalence (per 100,000)", title = "PD - definite") +
  scale_fill_manual(values = c("brown", "darkgreen")) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year))) +
  geom_smooth() +
  facet_wrap(~Sex)


#Plot PD mortality per Age year
ggplot(CombinedData %>% filter(Year < 2017, Year > 2004, !is.na(MortalityPD), DeathPD > 5, Age > 45), aes(Age, MortalityPD)) +
  theme_bw() +
  theme(panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x ="", y = "MortalityPD (per 100,000)", title = "PD - definite") +
  scale_fill_manual(values = c("brown", "darkgreen")) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year))) +
  geom_smooth() +
  facet_wrap(~Sex)



#Group by age groups
CombinedDataFiveYears <- CombinedData %>%
  group_by(YearAgeGroupSex5) %>% summarise(AgeGroup = unique(AgeGroup5),
                                           Sex = unique(Sex),
                                           Year = unique(Year),
                                           Number = sum(Number),
                                           Death = sum(Death, na.rm = T),
                                           PrevalenceRaw = sum(PrevalenceRaw, na.rm = T),
                                           PDnew = sum(PDnew, na.rm = T),
                                           DeathPD = sum(DeathPD, na.rm = T),
                                           Incidence = mean(Incidence),
                                           Prevalence = mean(Prevalence),
                                           Mortality = mean(Mortality),
                                           MortalityPD = mean(MortalityPD, na.rm = T), .groups = "drop") %>%
                                        
  data.frame() %>% 
  mutate(Incidence2 = (10^5)*PDnew/Number,
         Prevalence2 = (10^5)*PrevalenceRaw/Number,
         Mortality2 = (10^5)*Death/Number,
         MortalityPD2 = (10^5)*DeathPD/PrevalenceRaw) %>%
  arrange(Sex, Year, AgeGroup5) %>%
  select(Sex, AgeGroup5, Year, Number, Death, PDnew, PrevalenceRaw,
         DeathPD, Mortality, Mortality2, Incidence, Incidence2,
         Prevalence,  Prevalence2, MortalityPD,MortalityPD2,  YearAgeGroupSex) %>%
  mutate(DeltaPD = PDnew - DeathPD)
                                          
                                                           


CombinedDataFiveYears2 <- CombinedData %>%
  group_by(YearAgeGroupSex5b) %>% summarise(AgeGroup = unique(AgeGroup5b),
                                            Sex = unique(Sex),
                                            Year = unique(Year),
                                            Number = sum(Number),
                                            Death = sum(Death, na.rm = T),
                                            PrevalenceRaw = sum(PrevalenceRaw, na.rm = T),
                                            PDnew = sum(PDnew, na.rm = T),
                                            DeathPD = sum(DeathPD, na.rm = T),
                                            Incidence = mean(Incidence),
                                            Prevalence = mean(Prevalence),
                                            Mortality = mean(Mortality),
                                            MortalityPD = mean(MortalityPD, na.rm = T), .groups = "drop") %>%
  data.frame() %>% 
  mutate(Incidence2 = (10^5)*PDnew/Number,
         Prevalence2 = (10^5)*PrevalenceRaw/Number,
         Mortality2 = (10^5)*Death/Number,
         MortalityPD2 = (10^5)*DeathPD/PrevalenceRaw) %>%
  arrange(Sex, Year, AgeGroup) %>% 
  select(Sex, AgeGroup, Year, Number, Death, PDnew, PrevalenceRaw,
         DeathPD, Mortality, Mortality2, Incidence, Incidence2,
         Prevalence,  Prevalence2, MortalityPD,MortalityPD2,  YearAgeGroupSex2) %>%
  mutate(DeltaPD = PDnew - DeathPD)


CombinedDataFiveYears10 <- CombinedData %>%
  group_by(YearAgeGroupSex10) %>% summarise(AgeGroup = unique(AgeGroup10),
                                            Sex = unique(Sex),
                                            Year = unique(Year),
                                            Number = sum(Number),
                                            Death = sum(Death, na.rm = T),
                                            PrevalenceRaw = sum(PrevalenceRaw, na.rm = T),
                                            PDnew = sum(PDnew, na.rm = T),
                                            DeathPD = sum(DeathPD, na.rm = T),
                                            Incidence = mean(Incidence),
                                            Prevalence = mean(Prevalence),
                                            Mortality = mean(Mortality),
                                            MortalityPD = mean(MortalityPD, na.rm = T), .groups = "drop") %>%
  data.frame() %>% 
  mutate(Incidence2 = (10^5)*PDnew/Number,
         Prevalence2 = (10^5)*PrevalenceRaw/Number,
         Mortality2 = (10^5)*Death/Number,
         MortalityPD2 = (10^5)*DeathPD/PrevalenceRaw) %>%
  arrange(Sex, Year, AgeGroup) %>% 
  select(Sex, AgeGroup, Year, Number, Death, PDnew, PrevalenceRaw,
         DeathPD, Mortality, Mortality2, Incidence, Incidence2,
         Prevalence,  Prevalence2, MortalityPD,MortalityPD2,  YearAgeGroupSex2) %>%
  mutate(DeltaPD = PDnew - DeathPD)
