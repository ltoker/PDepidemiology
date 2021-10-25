#Generating data
Years = c(1994:2004)
Ages = c(30:89)

Years2 <- c(2005:2016)

#Seeding the initial values

#Pre-observation period
ToyExample <- data.frame(Year = rep(Years, length(Ages)),
                         Age = sapply(Ages, function(x){
                           rep(x, length(Years)) 
                         }) %>% as.numeric(),
                         Population = 100000,
                         PDnew = sapply(round(exp(seq(0.4, 5.5, length.out = length(Ages)))), function(x){
                           rep(x, length(Years))
                         }) %>% as.numeric())
ToyExample %<>% mutate(PrevalenceRaw = PDnew*3.5)

ToyExample$PDnew2 <- sapply(unique(ToyExample$Age), function(age){
  round((ToyExample %>% filter(Age == age) %>% .$PDnew)*(1.07)^(1:length(Years)))
}) %>% as.numeric()


#Observation period
ToyExample2 <- data.frame(Year = rep(Years2, length(Ages)),
                          Age = sapply(Ages, function(x){
                            rep(x, length(Years2)) 
                          }) %>% as.numeric(),
                          Population = 100000,
                          PDnew = NA,
                          PrevalenceRaw = NA)

ToyExample2$PDnew2 <- sapply(unique(Ages), function(age){
  rep((ToyExample %>% filter(Age == age, Year == Years[length(Years)])) %>% .$PDnew2, length(Years2))
}) %>% as.numeric()

#Matching the observed decrease in incidence among individuals < 60 in the real data
ToyExample2$PDnew2 <- sapply(unique(ToyExample2$Age), function(age){
  if(age < 60){
    round((ToyExample2 %>% filter(Age == age) %>%
             .$PDnew2)*(0.92)^(1:length(Years2)))
  } else {
    ToyExample2 %>% filter(Age == age) %>% .$PDnew2
  }
}) %>% as.numeric()

#For simplicity, the prevalence of the youngest age group is kept constant
ToyExample2$PrevalenceRaw[ToyExample2$Age == 30] <- ToyExample %>%
  filter(Age == 30, Year == 2004) %>% .$PrevalenceRaw

ToyExample <- rbind(ToyExample, ToyExample2) %>% arrange(Age, Year)

ToyExample %<>% mutate(PrevalenceRaw2 = PrevalenceRaw)

AgeMortality = data.frame(Age = Ages,
                          Mortality = exp(seq(4, 10, length.out = length(Ages))))

ToyExample <- merge(ToyExample, AgeMortality, by = "Age", sort = F)
ToyExample$DeathPD <- NA

#Propagating the prevalence
for(year in unique(ToyExample$Year)[-length(c(Years, Years2))]){
  for(age in unique(ToyExample$Age)[-length(Ages)]){
    Death = ToyExample %>% filter(Year == year, Age == age) %>% mutate(DeathPD = PrevalenceRaw2*(Mortality/100000)) %>% .$DeathPD %>% round()
    NewPD = ToyExample %>% filter(Year == year +1, Age == age+1) %>% .$PDnew2
    OldPD  = ToyExample %>% filter(Year == year, Age == age) %>% .$PrevalenceRaw2
    temp <- OldPD + NewPD - Death
    ToyExample$PrevalenceRaw2[ToyExample$Age == age + 1 & ToyExample$Year == year + 1] <- temp
    #ToyExample$DeathPD[ToyExample$Age == age & ToyExample$Year == year] <- Death
  }
}

ToyExample %<>% mutate(DeathPD = round(PrevalenceRaw2*(Mortality/100000)))
DF <- SetAgeGroup(AgeStart = 30, Gap = 5, AgeEnd = 89, Agevec = Ages)
ToyExample <- merge(ToyExample, DF, by = "Age", sort = F)
ToyExample %<>% mutate(AgeGroup5 = paste0("AgeGroup ", AgeGroup5))


#Plotting
RectDFyears2 <- data.frame(xMin = c(-Inf, 2016),
                           xMax = c(2004, Inf),
                           yMin = -Inf, yMax = Inf,
                           Color = "grey")


ToyExampleAgeGroup5 <- ToyExample %>% group_by(AgeGroup5, Year) %>%
  summarise(Incidence = mean(PDnew2, na.rm = T),
            Prevalence = mean(PrevalenceRaw2, na.rm = T),
            MortalityPD = mean((10^5)*DeathPD/PrevalenceRaw2, na.rm = T)) %>% data.frame() %>%
  mutate(Type = "Toy example")

ToyExampleAgeGroup5$AgeGroup5 <- sapply(ToyExampleAgeGroup5$AgeGroup5, function(x){
  gsub("AgeGroup ", "", x)
})


RealDataAgeGroup5 <- CombinedData %>% filter(Age > 29, Age < 90,
                                             Year > 2004, Year < 2017) %>% group_by(AgeGroup5, Year) %>%
  summarise(Incidence = mean(Incidence , na.rm = T),
            Prevalence = mean(Prevalence, na.rm = T),
            MortalityPD = mean(MortalityPD, na.rm = T)) %>% data.frame() %>%
  mutate(Type = "RealData")


PlotData <- rbind(ToyExampleAgeGroup5, RealDataAgeGroup5)

PrevalencePlotCombined <- ggplot(PlotData, aes(Year, Prevalence, color = Type)) +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Prevalence (per 100K)", x = "") + 
  geom_rect(data = RectDFyears[1, ],
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  scale_color_manual(values = c("grey40", MoviePalettes$MadMaxDesert[7]), name = "") +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 12) +
  facet_wrap(~AgeGroup5, scales = "free_y", ncol = 4)

IncidencePlotCombined <- ggplot(PlotData, aes(Year, Incidence, color = Type)) +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Incidence (per 100K)", x = "") + 
  geom_rect(data = RectDFyears[1, ],
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  scale_color_manual(values = c("grey40", MoviePalettes$MadMaxDesert[7]), name = "") +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 12) +
  facet_wrap(~AgeGroup5, scales = "free_y", ncol = 4)

MortalityPlotCombined <- ggplot(PlotData, aes(Year, MortalityPD, color = Type)) +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Mortality (per 100K)", x = "") + 
  geom_rect(data = RectDFyears[1, ],
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  scale_color_manual(values = c("grey40", MoviePalettes$MadMaxDesert[7]), name = "") +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 12) +
  facet_wrap(~AgeGroup5, scales = "free_y", ncol = 4)

ggarrange(IncidencePlotCombined,
          MortalityPlotCombined,
          PrevalencePlotCombined,
          nrow = 3, common.legend = T)
ggsave("Results/ToyExample.pdf", device = "pdf", width = 12,
       height = 10, dpi = 300, useDingbats = F)
