#devtools::install_git("https://github.com/JanMarvin/readspss.git")

source("generalFunc.R")
packageF("devtools")
install_git("https://github.com/eliocamp/ggnewscale")
library("ggnewscale")
packageF("tidyr")

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
  DF$AgeGroup[DF$Age > AgeEnd] <- paste0(AgeEnd + 1, " or older")
  DF$AgeGroup <- factor(DF$AgeGroup, levels = unique(DF$AgeGroup))
  return(DF)
}



#https://www.ssb.no/en/statbank
StatBankDataPerYear <- read.table("data/PersonerByOneAgeYear.txt", header = T, sep = "\t")

AgeGroups5 <- SetAgeGroup(AgeStart = 30, AgeEnd = 99, Gap = 5, Agevec = StatBankDataPerYear$Age)
AgeGroups2 <- SetAgeGroup(AgeStart = 30, AgeEnd = 99, Gap = 2, Agevec = StatBankDataPerYear$Age)

StatBankDataPerYear %<>% gather(key = "Year", value = "Number", -Sex, -Age)
StatBankDataPerYear$Year <- sapply(StatBankDataPerYear$Year, function(x) gsub("X", "", x)) %>% as.numeric()
StatBankDataPerYear <- merge(StatBankDataPerYear, AgeGroups5, by = "Age", all.x = T) %>% arrange(Sex, Age, Year)
StatBankDataPerYear %<>% mutate(YearAgeSex = paste(Year, Age, Sex, sep = "_"),
                                YearAgeGroupSex = paste(Year, AgeGroup, Sex, sep = "_"))

StatBankDataPerYear_death <- read.table("data/DeathByOneYear.txt", header = T, sep = "\t")
StatBankDataPerYear_death %<>% gather(key = "Year", value = "Death", -Sex, -Age)
StatBankDataPerYear_death$Year <- sapply(StatBankDataPerYear_death$Year, function(x) gsub("X", "", x)) %>% as.numeric()
StatBankDataPerYear_death <- merge(StatBankDataPerYear_death, AgeGroups5, by = "Age", all.x = T) %>% arrange(Sex, Age, Year)
StatBankDataPerYear_death %<>% mutate(YearAgeSex = paste(Year, Age, Sex, sep = "_"))

StatBankDataPerYear <- merge(StatBankDataPerYear,
                             StatBankDataPerYear_death %>% select(Death, YearAgeSex),
                             by = "YearAgeSex", all.x = T, all.y = T, sort = F)
StatBankDataPerYear %<>% mutate(Mortality = 100000*Death/Number) %>% filter(Year < 2020) %>% droplevels()

StatBankDataPerYear$YearAgeSex <- sapply(StatBankDataPerYear$YearAgeSex, function(x){
  x <- gsub("Females", "F", x)
  gsub("Males", "M", x)
})

StatBankDataPerYear$YearAgeGroupSex <- sapply(StatBankDataPerYear$YearAgeGroupSex, function(x){
  x <- gsub("Females", "F", x)
  gsub("Males", "M", x)
})


StatBankDataPerYear %<>% filter(Age != "Younger than 30") %>% droplevels()


#Get PD data

PDdataAll <- read.table("data/PDdata_Allupdated.txt", header = T, sep = "\t")

#PD prevalance based on Norwegian drug registry
#PDprevalencePerYear <- read.table("PDprevalenceOneAgeYear.txt", header = T, sep = "\t") %>% filter(Prescription == 3)
PDprevalencePerYear <- PDdataAll %>% filter(Measure == "Prevalence")

PDprevalencePerYear <- PDprevalencePerYear  %>% gather(key = "Age", value = "Prevalence", -Sex, -Year, -Measure) 

PDprevalencePerYear$Age <- sapply(as.character(PDprevalencePerYear$Age), function(x){
  x <- gsub("X", "", x)
  as.numeric(x)
})


PDprevalencePerYear %<>% mutate(YearAgeSex = paste(Year, Age, Sex, sep = "_")) 
PDprevalencePerYear <- merge(PDprevalencePerYear, AgeGroups5, by = "Age", all.x = T) %>% arrange(Sex, Age, Year)
PDprevalencePerYear %<>% mutate(YearAgeSex = paste(Year, Age, Sex, sep = "_"),
                                YearAgeGroupSex = paste(Year, AgeGroup, Sex, sep = "_"))



#PD incidence data based on Norwegian drug registry
#PDincidenceOneYear <- read.table("PDincidenceOneYear.txt", header = T, sep = "\t") %>% filter(Prescription == 3)
#PDincidenceOneYear <- PDincidenceOneYear %>% gather(key = "Age", value = "PDnew", -Prescription, -Sex, -Year) 

PDincidenceOneYear <- PDdataAll %>% filter(Measure == "Incidence")

PDincidenceOneYear <- PDincidenceOneYear %>% gather(key = "Age", value = "PDnew", -Sex, -Year, -Measure) 


PDincidenceOneYear$Age <- sapply(as.character(PDincidenceOneYear$Age), function(x){
  x <- gsub("X", "", x)
  as.numeric(x)
})


PDincidenceOneYear %<>% mutate(YearAgeSex = paste(Year, Age, Sex, sep = "_")) 
PDincidenceOneYear <- merge(PDincidenceOneYear, AgeGroups5, by = "Age", all.x = T) %>% arrange(Sex, Age, Year)
PDincidenceOneYear %<>% mutate(YearAgeSex = paste(Year, Age, Sex, sep = "_"),
                               YearAgeGroupSex = paste(Year, AgeGroup, Sex, sep = "_"))


#PD mortality rate based on Norwegian drug registry
#PDmortalityOneYear <- read.table("PDmortalityOneAgeYear.txt", header = T, sep = "\t", comment.char = "#")  %>% filter(Prescription == 3)
#PDmortalityOneYear <- PDmortalityOneYear %>% gather(key = "Age", value = "DeathPD", -Prescription, -Sex, -Year) 

PDmortalityOneYear <- PDdataAll %>% filter(Measure == "DeathPD")
PDmortalityOneYear <- PDmortalityOneYear %>% gather(key = "Age", value = "DeathPD", -Sex, -Year, -Measure) 

PDmortalityOneYear$Age <- sapply(as.character(PDmortalityOneYear$Age), function(x){
  x <- gsub("X", "", x)
  as.numeric(x)
})


PDmortalityOneYear %<>% mutate(YearAgeSex = paste(Year, Age, Sex, sep = "_"))
PDmortalityOneYear <- merge(PDmortalityOneYear, AgeGroups5, by = "Age", all.x = T) %>% arrange(Sex, Age, Year)
PDmortalityOneYear %<>% mutate(YearAgeSex = paste(Year, Age, Sex, sep = "_"),
                               YearAgeGroupSex = paste(Year, AgeGroup, Sex, sep = "_"))



#Recalculate prevalence from incidence and PD mortality
YearStart = 2006
YearEnd = 2016
AgeRange = c(36:105)


PDprevalencePerYearRecal <- sapply(c("F", "M"), function(sex){
  year = YearStart
  
  PrevalencePerYear <- as.list(c(YearStart:YearEnd))
  names(PrevalencePerYear) <- c(YearStart:YearEnd)
  
  
  BaselinePrevalence <- PDprevalencePerYear %>% filter(Year == YearStart-1, Sex == sex)
  BaselinePrevalenceAge <- PDprevalencePerYear %>% filter(Age == AgeRange[1] -1, Sex == sex)
  while(year <= YearEnd){
    BaselinePrevalence <- sapply(AgeRange, function(age){
      if(age == 36){
        PrevalenceOld = BaselinePrevalenceAge %>% filter(Year == year-1, Age == age-1) %>% .$Prevalence
      } else {
        PrevalenceOld = BaselinePrevalence %>% filter(Year == year-1, Age == age-1) %>% .$Prevalence
      }
      Death = PDmortalityOneYear %>% filter(Year == year-1, Age == age-1, Sex == sex) %>% .$DeathPD
      Incidence = PDincidenceOneYear %>% filter(Year == year, Age == age, Sex == sex) %>% .$PDnew
      data.frame(Age = age, Year = year, Sex = sex, Prevalence = PrevalenceOld-Death+Incidence, YearAgeSex = paste(year, age, sex, sep = "_"))
    }, simplify = F) %>% rbindlist
    print(year)
    PrevalencePerYear[[which(names(PrevalencePerYear) == year)]] <- BaselinePrevalence
    year = year +1
  }
  
  PrevalencePerYear <- rbindlist(PrevalencePerYear)
}, simplify = F) %>% rbindlist

#Fix negative Prevalence.While this is important for the recursive model, since it allows correction
#for the small descripancies in years, negative value is equivalent to zero

PDprevalencePerYearRecal$Prevalence <- sapply(PDprevalencePerYearRecal$Prevalence, function(x){
  if(x < 0){
    0
  } else {
    x
  }
})

PDprevalencePerYear <- merge(PDprevalencePerYear, PDprevalencePerYearRecal %>% select(YearAgeSex, Prevalence), by = "YearAgeSex", all.x = T, suffixes = c("Raw", "Raw_Calc"))

#Combine all data
CombinedData <- merge(StatBankDataPerYear, PDprevalencePerYear %>% select(YearAgeSex, PrevalenceRaw, PrevalenceRaw_Calc), by = "YearAgeSex", all.x = T)
CombinedData <- merge(CombinedData, PDincidenceOneYear %>% select(YearAgeSex, PDnew), by = "YearAgeSex", all.x = T)
CombinedData <- merge(CombinedData, PDmortalityOneYear %>% select(YearAgeSex, DeathPD), by = "YearAgeSex", all.x = T)

#Fix issues when prevalence is 0 and cases when  mortality > prevalence, since this are  the cases with very small prevalence, in which the numbers are not precise due to the age assignment 
CombinedData[which(CombinedData$DeathPD > CombinedData$PrevalenceRaw),]$DeathPD <- CombinedData[which(CombinedData$DeathPD > CombinedData$PrevalenceRaw),]$PrevalenceRaw

CombinedData %<>% mutate(Mortality = 100000*Death/Number,
                         Incidence = 100000*PDnew/Number,
                         Incidence2 = 100000*PDnew/(Number - PrevalenceRaw + PDnew),
                         Prevalence = 100000*PrevalenceRaw/Number,
                         Prevalence_Calc = 100000*PrevalenceRaw_Calc/Number,
                         MortalityPD = 100000*DeathPD/PrevalenceRaw,
                         MortalityPD_Calc = 100000*DeathPD/PrevalenceRaw_Calc)


#Plot Incidence per Age year
ggplot(CombinedData %>% filter(Year != 2017, Year > 2004, !is.na(Incidence), Incidence > 0), aes(Age, Incidence)) +
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
ggplot(CombinedData %>% filter(Year != 2017, Year > 2004, !is.na(Prevalence), Prevalence >= 0), aes(Age, Prevalence)) +
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


#Plot Calculated Prevalence per Age year
ggplot(CombinedData %>% filter(Year != 2017, Year > 2006, !is.na(Prevalence_Calc), Prevalence_Calc >= 0), aes(Age, Prevalence_Calc)) +
  theme_bw() +
  theme(panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x ="", y = "Prevalence_Calc (per 100,000)", title = "PD - definite") +
  scale_fill_manual(values = c("brown", "darkgreen")) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year))) +
  geom_smooth() +
  facet_wrap(~Sex)


#Plot PD mortality per Age year
ggplot(CombinedData %>% filter(Year != 2017, Year > 2006, !is.na(MortalityPD), MortalityPD, Age > 45), aes(Age, MortalityPD)) +
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

#Plot PD mortality per Age year based on calculated prevalence
ggplot(CombinedData %>% filter(Year != 2017, Year > 2006, !is.na(MortalityPD_Calc), MortalityPD_Calc, Age > 45), aes(Age, MortalityPD_Calc)) +
  theme_bw() +
  theme(panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x ="", y = "MortalityPD_Calc (per 100,000)", title = "PD - definite") +
  scale_fill_manual(values = c("brown", "darkgreen")) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year))) +
  geom_smooth() +
  facet_wrap(~Sex)


#Group by age groups
CombinedDataFiveYears <- CombinedData %>%
  group_by(YearAgeGroupSex) %>% summarise(AgeGroup = unique(AgeGroup),
                                          Sex = unique(Sex),
                                          Year = unique(Year),
                                          Number = sum(Number),
                                          Death = sum(Death, na.rm = T) - 0.00005*sum(is.na(Death)),
                                          PrevalenceRaw = sum(PrevalenceRaw, na.rm = T) - 0.00005*sum(is.na(PrevalenceRaw)),
                                          PrevalenceRaw_Calc = sum(PrevalenceRaw_Calc, na.rm = T) - 0.00005*sum(is.na(PrevalenceRaw_Calc)),
                                          PDnew = sum(PDnew, na.rm = T) - 0.00005*sum(is.na(PDnew)),
                                          DeathPD = sum(DeathPD, na.rm = T) - 0.00005*sum(is.na(DeathPD)),
                                          Incidence = mean(Incidence, na.rm = T),
                                          Incidence2 = mean(Incidence2, na.rm = T),
                                          Prevalence = mean(Prevalence, na.rm = T),
                                          Prevalence_Calc = mean(Prevalence_Calc, na.rm = T),
                                          Mortality = mean(Mortality, na.rm = T),
                                          MortalityPD = mean(MortalityPD, na.rm = T),
                                          MortalityPD_Calc = mean(MortalityPD_Calc, na.rm = T)) %>%
  data.frame() %>% arrange(Sex, Year, AgeGroup) %>% select(Sex, AgeGroup, Year, Number, Death, PDnew, PrevalenceRaw, PrevalenceRaw_Calc,
                                                           DeathPD, Mortality, Incidence, Incidence2, Prevalence, Prevalence_Calc,
                                                           MortalityPD, MortalityPD_Calc,  YearAgeGroupSex)


CombinedDataFiveYears$Death[CombinedDataFiveYears$Death < -0.000049] <- NA
CombinedDataFiveYears$PrevalenceRaw[CombinedDataFiveYears$PrevalenceRaw < -0.000049] <- NA
CombinedDataFiveYears$PrevalenceRaw_Calc[CombinedDataFiveYears$PrevalenceRaw_Calc < -0.000049] <- NA
CombinedDataFiveYears$PDnew[CombinedDataFiveYears$PDnew < -0.000049] <- NA
CombinedDataFiveYears$DeathPD[CombinedDataFiveYears$DeathPD < -0.000049] <- NA


#Get long format for calculating 
temp <- CombinedDataFiveYears %>% filter(Year > 2004, Year < 2017, DeathPD >= 3) %>% select(Year, Sex, AgeGroup, Mortality, MortalityPD)
tempLong <- gather(temp, key = "Group", value = "Mortality", Mortality, MortalityPD)
tempLong$Group <- sapply(tempLong$Group, function(x) {
  if(x == "Mortality"){
    "All"
  }
  else if(x == "MortalityPD"){
    "PD"
  }
})


lm(log(Mortality)~Year+Group*Sex + as.numeric(AgeGroup), data = tempLong %>% filter(!is.infinite(Mortality))) %>% summary()
lm(log(Mortality)~Year+Sex + as.numeric(AgeGroup), data = tempLong %>% filter(Group == "All")) %>% summary()
lm(log(Mortality)~Year+Sex + as.numeric(AgeGroup), data = tempLong %>% filter(Group == "PD")) %>% summary()



# CombinedDataFiveYears %<>% mutate(Mortality = 100000*Death/Number,
#                                   Incidence = 100000*PDnew/Number,, 
#                                   Incidence2 = 100000*PDnew/(Number - PrevalenceRaw + PDnew),
#                                   Prevalence = 100000*PrevalenceRaw/Number,
#                                   Prevalence_Calc = 100000*PrevalenceRaw_Calc/Number,
#                                   MortalityPD = 100000*DeathPD/PrevalenceRaw,
#                                   MortalityPD_Calc = 100000*DeathPD/PrevalenceRaw_Calc)



RectDF <- data.frame(xMin = seq(from = 0.5, to = 14.5, by = 2),
                     xMax = seq(from = 1.5, to = 15.5, by = 2),
                     yMin = -Inf, yMax = Inf,
                     Color = "grey")




# Plot General population sizes across the years in different population groups
ggplot(CombinedDataFiveYears %>%
         filter(AgeGroup %in% c("40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79")),
       aes(Year, Number, color = AgeGroup)) +
  theme_classic() +
  geom_point(size = 3) +
  geom_line(size = 1.5, alpha = 0.5) +
  facet_wrap(~Sex, ncol = 1, scales = "free_y")

ggplot(CombinedDataFiveYears %>%
         filter(AgeGroup %in% c("60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99"), Year > 2006, Year < 2017),
       aes(Year, Mortality, color = AgeGroup)) +
  theme_classic() +
  geom_point(size = 3) +
  geom_line(size = 1.5, alpha = 0.5) +
  facet_wrap(~Sex, ncol = 1, scales = "free_y")


#Plot PD incidence
ggplot(CombinedDataFiveYears %>% filter(!Year %in% c(2004, 2017), !is.na(Incidence)), aes(AgeGroup, Incidence)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x ="", y = "Incidence (per 100,000)", title = "PD - definite") +
  new_scale("fill") +
  scale_fill_manual(values = c("brown", "darkgreen")) +
  geom_rect(data = RectDF,
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  geom_boxplot(outlier.shape = NA, aes(fill = Sex)) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year), group = Sex),
             position = position_dodge2(width = 0.5), show.legend = T) 

#Plot PD incidence corrected
ggplot(CombinedDataFiveYears %>% filter(!Year %in% c(2004, 2017), !is.na(Incidence2)), aes(AgeGroup, Incidence2)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x ="", y = "Corrected Incidence (per 100,000)", title = "PD - definite") +
  new_scale("fill") +
  scale_fill_manual(values = c("brown", "darkgreen")) +
  geom_rect(data = RectDF,
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  geom_boxplot(outlier.shape = NA, aes(fill = Sex)) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year), group = Sex),
             position = position_dodge2(width = 0.5)) 


#Plot PD Prevalence
ggplot(CombinedDataFiveYears %>% filter(!Year %in% c(2004, 2017), !is.na(Prevalence)), aes(AgeGroup, Prevalence)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x ="", y = "Prevalence (per 100,000)", title = "PD - definite") +
  new_scale("fill") +
  scale_fill_manual(values = c("brown", "darkgreen")) +
  geom_rect(data = RectDF,
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  geom_boxplot(outlier.shape = NA, aes(fill = Sex)) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year), group = Sex),
             position = position_dodge2(width = 0.5)) 


#Plot PD Prevalence Calculated
ggplot(CombinedDataFiveYears %>% filter(!Year %in% c(2004, 2017), !is.na(Prevalence_Calc)), aes(AgeGroup, Prevalence_Calc)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x ="", y = "Prevalence_Calculated (per 100,000)", title = "PD - definite") +
  new_scale("fill") +
  scale_fill_manual(values = c("brown", "darkgreen")) +
  geom_rect(data = RectDF,
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  geom_boxplot(outlier.shape = NA, aes(fill = Sex)) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year), group = Sex),
             position = position_dodge2(width = 0.5)) 

#Plot Population size
ggplot(CombinedDataFiveYears %>% filter(Year != 2017, !is.na(PrevalenceRaw)), aes(AgeGroup, Number)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed") ) +
  labs(x ="Age group", y = "Population size", title = "General population") +
  new_scale("fill") +
  scale_fill_manual(values = c("brown", "darkgreen")) +
  geom_rect(data = RectDF,
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  geom_boxplot(outlier.shape = NA, aes(fill = Sex)) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year), group = Sex),
             position = position_dodge2(width = 0.5)) 


#Plot PD Mortality
ggplot(CombinedDataFiveYears %>% filter(!Year %in% c(2004, 2017), !is.na(MortalityPD), DeathPD > 3), aes(AgeGroup, log(MortalityPD))) +
  theme_bw() +
  labs(x ="Age group", y = "Mortality (per 100,000)", title = "PD - definite") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed") ) +
  new_scale("fill") +
  scale_fill_manual(values = c("brown", "darkgreen")) +
  geom_rect(data = RectDF[1:5,],
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  geom_boxplot(outlier.shape = NA, aes(fill = Sex)) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year), group = Sex),
             position = position_dodge2(width = 0.5)) 


#Plot PD Mortality using calculated prevalence
ggplot(CombinedDataFiveYears %>% filter(!Year %in% c(2004, 2017), !is.na(MortalityPD_Calc), MortalityPD_Calc <= 10^5,  DeathPD > 3), aes(AgeGroup, log(MortalityPD_Calc))) +
  theme_bw() +
  labs(x ="Age group", y = "Mortality based on calculated Prevalence (per 100,000)", title = "PD - definite") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed") ) +
  new_scale("fill") +
  scale_fill_manual(values = c("brown", "darkgreen")) +
  geom_rect(data = RectDF[1:5,],
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  geom_boxplot(outlier.shape = NA, aes(fill = Sex)) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year), group = Sex),
             position = position_dodge2(width = 0.5))


#Plot general population Mortality
ggplot(CombinedDataFiveYears %>% filter(Year < 2017, Year > 2005, AgeGroup != ("Younger than 30"), DeathPD >5), aes(AgeGroup, log(Mortality))) +
  theme_bw() +
  labs(x ="Age group", y = "log(Mortality (per 100,000))", title = "General population + PD") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_line(colour = "grey50", linetype = "dashed"),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed") ) +
  new_scale("fill") +
  scale_fill_manual(values = c("brown", "darkgreen")) +
  geom_rect(data = RectDF[1:5,],
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  geom_boxplot(outlier.shape = NA, aes(fill = Sex)) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year), group = Sex),
             position = position_dodge2(width = 0.5)) +
  geom_boxplot(data = CombinedDataFiveYears %>% filter(!Year %in% c(2004, 2017), !is.na(MortalityPD), MortalityPD <= 10^5,  DeathPD > 5), aes(AgeGroup, log(MortalityPD), fill = Sex)) +
  geom_point(data = CombinedDataFiveYears %>% filter(!Year %in% c(2004, 2017), !is.na(MortalityPD), MortalityPD <= 10^5,  DeathPD > 5), aes(AgeGroup, log(MortalityPD), color = as.character(Year), group = Sex),
             position = position_dodge2(width = 0.5))


#Plot PD mortality relative to general mortality
ggplot(CombinedDataFiveYears %>% filter(Year < 2017, Year > 2004, DeathPD > 5), aes(AgeGroup, MortalityPD/Mortality)) +
  theme_bw() +
  labs(x ="Age group", y = "Mortality ratio", title = "PD mortality relative to population mortality") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_line(colour = "grey50", linetype = "dashed"),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed") ) +
  new_scale("fill") +
  scale_fill_manual(values = c("brown", "darkgreen")) +
  geom_rect(data = RectDF[1:5,],
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  geom_boxplot(outlier.shape = NA, aes(fill = Sex)) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year), group = Sex),
             position = position_dodge2(width = 0.5)) 


#Look at the ratio M/F ratios
CombinedDataFiveYearWide <- pivot_wider(CombinedDataFiveYears %>% droplevels() %>%
                                  select(Year, AgeGroup, Sex,
                                         Number, Death, Mortality, 
                                         PDnew, Incidence,
                                         PrevalenceRaw, Prevalence,
                                         DeathPD, MortalityPD),
                                names_from = Sex,
                                values_from = c(Number, Death, Mortality,PDnew, Incidence,
                                                PrevalenceRaw, Prevalence,
                                                DeathPD,MortalityPD)) %>% data.frame() %>%
  mutate(MortalityRatio = Mortality_Males/Mortality_Females,
         IncidenceRatio = Incidence_Males/Incidence_Females,
         PrevalenceRatio = Prevalence_Males/Prevalence_Females,
         MortalityPDRatio = MortalityPD_Males/MortalityPD_Females,
         AgeRangeNumeric = as.numeric(AgeGroup))


#M/F Mortality ratio plots

# Mortality ratio M/F plot PD
ggplot(CombinedDataFiveYearWide %>% filter(!is.na(Prevalence_Males), DeathPD_Males > 5, DeathPD_Females > 5), aes(AgeGroup, MortalityPDRatio)) +
  labs(x ="Age group", y = "M/F Mortality ratio", title = "Mortality M/F ratio, Parkinson's disease") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_line(colour = "grey50", linetype = "dashed"),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed") ) +
  geom_boxplot(outlier.shape = NA, fill = "grey", alpha = 0.4) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1])) +
  geom_jitter(width = 0.2) +
  #geom_boxplot(aes(AgeGroup, MortalityPDRatio), fill = "red", alpha = 0.5) +
  geom_hline(yintercept = 1.5, color = "red")

# Mortality ratio M/F plot General population
ggplot(CombinedDataFiveYearWide %>% filter(!is.na(Prevalence_Males), Death_Males > 5, Death_Females > 5), aes(AgeGroup, MortalityRatio)) +
  labs(x ="Age group", y = "M/F Mortality ratio", title = "Mortality M/F ratio, Whole population") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_line(colour = "grey50", linetype = "dashed"),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed") ) +
  geom_boxplot(outlier.shape = NA, fill = "grey", alpha = 0.4) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1])) +
  geom_jitter(width = 0.2) +
  #geom_boxplot(data = CombinedData5YearWide %>% filter(!is.na(Prevalence_Males), Death_Males > 5, Death_Females > 5), aes(AgeGroup, MortalityPDRatio),inherit.aes = F,  fill = "red", alpha = 0.5) +
  geom_hline(yintercept = 1.5, color = "red")

# Mortality ratio M/F plot General population - points (long Format again)
CombinedDataFiveYearWide_Long <- gather(CombinedDataFiveYearWide, key = "Group",
                                        value = "MortalityRatio", matches("Mortality(PD)?Ratio"))
CombinedDataFiveYearWide_Long$Group <- sapply(CombinedDataFiveYearWide_Long$Group, function(x){
  if(x == "MortalityRatio"){
    "All"
  } else if(x == "MortalityPDRatio"){
    "PD"
  }
})


ggplot(CombinedDataFiveYearWide_Long %>%
         filter(!is.na(Prevalence_Males), Death_Males > 5, DeathPD_Females > 5, AgeGroup != "55-59"), aes(AgeGroup, MortalityRatio, color = Group)) +
  theme_bw() +
  labs(x ="Age group", y = "M/F Mortality ratio", title = "Mortality M/F ratio") +
  theme(panel.grid = element_blank()) +
  #geom_point(position =position_dodge(width = 0.3), aes(group = Group), size = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_se", position =position_dodge(width = 0.3), size = 0.5) +
  scale_color_manual(values = c("darkgrey", "coral4")) +
  geom_hline(yintercept = 1.5, color = "red", linetype = "dashed")

# Mortality ratio, ratio plot
ggplot(CombinedDataFiveYearWide %>% filter(!is.na(Prevalence_Males), DeathPD_Males > 5, DeathPD_Females > 5, !is.infinite(MortalityPDRatio)), aes(AgeGroup, MortalityPDRatio/MortalityRatio)) +
  labs(x ="Age group", y = "PD/All Mortality ratio", title = "") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_line(colour = "grey50", linetype = "dashed"),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed") ) +
  geom_boxplot(outlier.shape = NA, fill = "grey", alpha = 0.4) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1])) +
  geom_jitter(width = 0.2) +
  #geom_boxplot(aes(AgeGroup, MortalityPDRatio), fill = "red", alpha = 0.5) +
  geom_hline(yintercept = 1, color = "red")


#Incidence ratio plot
ggplot(CombinedDataFiveYearWide %>% filter(PDnew_Females > 10, PDnew_Males > 10), aes(AgeGroup, IncidenceRatio)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Male/Female incidence ratio", title = "") + 
  geom_boxplot(outlier.shape = NA, fill = "grey") +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year)),
             position = position_dodge2(width = 0.5)) 


packageF("ggpubr")
ggarrange(IncidencePlot, IncidenceRatioPlot, ncol = 1)


ggplot(CombinedData5YearWide %>% filter(PDnew_Females > 10, PDnew_Males > 10), aes(as.character(Year), IncidenceRatio)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x ="Age group", y = "Incidence ratio", title = "PD - definite") +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1])) +
  geom_point(aes(color = AgeGroup))

ggplot(CombinedData5YearWide %>% filter(PrevalenceRaw_Females > 10, PrevalenceRaw_Males > 10),
       aes(AgeGroup, PrevalenceRatio)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x ="Age group", y = "Prevalence ratio", title = "PD - definite") +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1])) +
  geom_point(aes(color = as.character(Year)))

ggplot(CombinedData5YearWide %>% filter(PDnew_Females > 10, PDnew_Males > 10),
       aes(PrevalenceRatio, IncidenceRatio)) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1])) +
  geom_point(aes(color = as.character(Year))) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~AgeGroup)
  

lm(IncidenceRatio~AgeRangeNumeric + Year,
   data = CombinedData5YearWide %>% filter(PDnew_Females > 10, PDnew_Males > 10)) %>% summary() 

lm(PrevalenceRatio~AgeRangeNumeric + Year,
   data = CombinedData5YearWide %>% filter(PDnew_Females > 10, PDnew_Males > 10)) %>% summary() 


temp <- CombinedData5YearWide %>% filter(AgeGroup == "85-89")
wilcox.test(temp$MortalityRatio, temp$MortalityPDRatio)


PDincidence2 <- read.table("PDincidenceUncertain.txt", header = T, sep = "\t")
PDincidenceLong2 <- PDincidence2 %>% filter(!Year %in% c(2004, 2017)) %>%
  gather(key = "AgeGroup", value = "PDnew", -Prescription, -Sex, -Year) 

PDincidenceLong2$AgeGroup <- sapply(as.character(PDincidenceLong2$AgeGroup), function(x){
  x <- gsub("X", "", x)
  x <- gsub("^\\.", ">", x)
  gsub("\\.", "-", x)
})

PDincidenceLong2 %<>% mutate(YearAgeSex = paste(Year, AgeGroup, Sex, sep = "_"))


CombinedData2 <- merge(StatBankData, PDincidenceLong2 %>%
                        filter(Prescription == 2) %>%
                        select(PDnew, YearAgeSex), by = "YearAgeSex")

CombinedData2 %<>% mutate(Incidence = 100000*PDnew/Number)

ggplot(CombinedData2 %>% filter(Year > 2007), aes(age, Incidence)) +
  labs(x ="Age group", y = "Incidence (per 100,000)", title = "PD - Uncertain") +
  geom_boxplot(outlier.shape = NA, aes(fill = sex)) +
  scale_fill_manual(values = c("brown", "darkgreen")) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1])) +
  geom_point(aes(color = as.character(Year), group = sex),
             position = position_dodge2(width = 0.5))

#Prevalence


#ParkWest data
temp <- foreign::read.spss("../ParkVest_V02-V21_020817_extr.sav", to.data.frame = F)
VarLabels <- data.frame(Var = names(attr(temp, "variable.labels")),
                        Meaning = attr(temp, "variable.labels"))

CodeList <- attr(temp, "label.table")
CodeListMixed <- sapply(names(CodeList), function(x){
  ans <- tolower(names(CodeList[[x]]))
  if(sum(ans %in% c("yes", "no") > 0)){
    "Yes"
  } else {
    "No"
  }
})


dataPVall <- read.sav("../ParkVest_V02-V21_020817_extr.sav")


#remove columns which don't vary between individuals
UniqueVal <- apply(dataPVall, 2, function(x) length(unique(x)))
dataPVall <- dataPVall[, UniqueVal > 1]

#Harmonize the fields
dataPVall %<>% mutate_if(is.character, list(~na_if(.,"")))

dataPV <- data.frame(SubjectID = dataPVall$BL_CASE)

for(colName in names(dataPVall)[-1]){
  Val <- dataPVall[[colName]]
  if(is.character(Val)){
    Val <- toupper(Val)
    Val <- sapply(Val, function(x){
      if(is.na(x)){
        x
      } else if(x == "nei"){
        "no"
      } else if(x == "ja"){
        "yes"
      } else {
        x
      }
    })
  } else if (is.factor(Val)){
    levVal <- tolower(levels(Val))
    levVal <- sapply(levVal, function(x){
      if(is.na(x)){
        x
      } else if(x == "nei"){
        "no"
      } else if(x == "ja"){
        "yes"
      } else {
        x
      }
    })
    levels(Val) <- levVal
  }
  dataPV[[colName]] <- Val
}

dataPV %<>% select(-matches("KFP|flønes|MRI"))

#Change the number NAs to 0 when applicable
CoffeCol <- names(dataPV %>% select(matches("COF")))
CoffVisit <- sapply(CoffeCol, function(x){
  strsplit(x, "_")[[1]][[1]]
}) %>% unique



dataPV[1:10, 1:10]



RepeatedMeasures <- sapply(names(dataPVall), function(x) gsub("^(BL_)|(V..?_)", "", x, ignore.case = F))

DataLong <- pivot_longer(dataPVall %>% select(-matches("KFP|flønes|MRI|")),
                         matches("^BL|^V", ignore.case = F),
                         names_to = c("Visit", ".value"),
                         names_pattern = "(.*)_(.*)")