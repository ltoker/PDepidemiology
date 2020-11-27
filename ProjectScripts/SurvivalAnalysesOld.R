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


IndividualMortality <- IndividualMortality %>%
  filter(DeathYearDummy <= 2017, year(DateFirstDrug) > 2004) %>% droplevels()





# #Deal with global mortality as time-dependent covariate
# IndividualMortality2 <- sapply(IndividualMortality$PasientIndex, function(ID){
#   data <- IndividualMortality%>% filter(PasientIndex == ID) %>% mutate(YearOfOnset = year(firstDateParkmedisin))
#   DF <- data.frame(PasientIndex = ID,
#                    RelAge = data$AgeOfOnset:data$AgeAtdeath,
#                    RelYear = data$YearOfOnset:c(data$YearOfOnset + data$AgeAtdeath-data$AgeOfOnset),
#                    tStart = c(0, seq(12-month(data$firstDateParkmedisin), by = 12,
#                                      length.out = c(data$AgeAtdeath-data$AgeOfOnset))),
#                    tEnd = c(seq(12-month(data$firstDateParkmedisin), by = 12,
#                             length.out = c(data$AgeAtdeath-data$AgeOfOnset)), data$MonthToEnd))
#   if(DF$tEnd[1] == 0){
#     DF$tEnd[1] <- 1
#     DF$tEnd[2] <- 1
#   }
#   temp <- merge(data, DF, by = "PasientIndex") %>% mutate(YearAgeSex = paste(RelYear, RelAge, Sex, sep = "_"))
#   temp$Status[1:nrow(temp)-1] <- 0
#   temp
# }, simplify = F) %>% rbindlist() %>% data.frame()

#IndividualMortality2$GlobalMortality <- StatBankDataPerYear$Mortality[match(IndividualMortality2$YearAgeSex,
#                                                                           StatBankDataPerYear$YearAgeSex)]
#IndividualMortality2 %<>% mutate(Delta = tEnd - tStart)

IndividualMortality %<>%  mutate(YearAgeSex = paste(year(DateFirstDrug), AgeOfOnsetNew, Sex, sep = "_"))
IndividualMortality$GlobalMortalityOnset <- StatBankDataPerYear$Mortality[match(IndividualMortality$YearAgeSex,
                                                                            StatBankDataPerYear$YearAgeSex)] 


IndividualMortality$SurvObj <- with(IndividualMortality, Surv(MonthToEnd, event = Status))
IndividualMortality %<>% mutate(AgeGroup = paste0("Age_", AgeGroup))


#Plot Kaplan-Meir
km.by.Sex <- survfit(Surv(MonthToEnd, event = Status) ~ Sex + strata(AgeGroup), data = IndividualMortality, conf.type = "log-log")

plot(km.by.Sex, mark.time=FALSE, lty=1:2,xlab="Years post diagnosis", ylab="Survival")

KMageOnset <- sapply(unique(IndividualMortality$AgeGroup), function(group){
  data = IndividualMortality %>% filter(AgeGroup == group)
  km.by.sex <- survfit(Surv(MonthToEnd, event = Status) ~ Sex, data = data, conf.type = "log-log")
},simplify = F)


## Show object
PlotKMforAgeGroup <- sapply(names(KMageOnset), function(group){
  autoplot(KMageOnset[[group]]) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(x="Months post diagnosis", y = "Survival", title  = group) +
    scale_fill_manual(values = c("darkorange4", "darkgreen"))
},simplify = F)

ggarrange(plotlist = PlotKMforAgeGroup[-c(1:4)])


## Look at the age of onset
ggplot(IndividualMortality, aes(Sex, AgeOfOnsetNew)) + geom_boxplot() +
  facet_wrap(~AgeGroup, scales = "free")



IndividualMortality2$SurvObj <- with(IndividualMortality2, Surv(tStart, tEnd, Status))

IndividualMortality2 %<>% mutate(Delata = tEnd - tStart)

res.cox1 <- coxph(SurvObj ~ Sex + GlobalMortalityOnset:MonthToEnd,
                  data = IndividualMortality %>% filter(AgeGroup == "Age_65-69"))

res.zph1 <- cox.zph(res.cox1)
ggcoxzph(res.zph1, ggtheme = theme_classic())


plot.survfit(res.cox1)

res.cox2 <- coxph(SurvObj ~ Sex + GlobalMortality:Delta, id = PasientIndex,
                  data = IndividualMortality2 %>% droplevels)

res.cox3 <- coxph(SurvObj ~ Sex + AgeOfOnset:Delta + GlobalMortality:Delta, id = PasientIndex,
                  data = IndividualMortality2 %>% droplevels)


res.zph2 <- cox.zph(res.cox3)

ggcoxzph(cox.zph(res.cox1), ggtheme = theme_classic())


