#Generating data

Years = c(1994:2004)
Ages = c(60:89)

Years2 <- c(2005:2016)

ToyExample <- data.frame(Year = rep(Years, length(Ages)),
                         Age = sapply(Ages, function(x){
                           rep(x, length(Years)) 
                         }) %>% as.numeric(),
                         Population = 100000,
                         PDnew = sapply(round(exp(seq(4, 5.5, length.out = length(Ages)))), function(x){
                           rep(x, length(Years))
                         }) %>% as.numeric())
ToyExample %<>% mutate(PrevalenceRaw = PDnew*5)

ToyExample$PDnew2 <- sapply(unique(ToyExample$Age), function(age){
  (ToyExample %>% filter(Age == age) %>% .$PDnew) + runif(n = length(Years), 0, 11) %>% sort %>% round()
}) %>% as.numeric()


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

ToyExample <- rbind(ToyExample, ToyExample2) %>% arrange(Age, Year)

ToyExample %<>% mutate(PrevalenceRaw2 = PrevalenceRaw)

AgeMortality = data.frame(Age = Ages,
                          Mortality = exp(seq(8, 9.5, length.out = length(Ages))))



ToyExample <- merge(ToyExample, AgeMortality, by = "Age", sort = F)
ToyExample$DeathPD <- NA

for(year in unique(ToyExample$Year)[-length(c(Years, Years2))]){
  for(age in unique(ToyExample$Age)[-length(Ages)]){
    Death = ToyExample %>% filter(Year == year, Age == age) %>% mutate(DeathPD = PrevalenceRaw2*(Mortality/100000)) %>% .$DeathPD %>% round()
    NewPD = ToyExample %>% filter(Year == year +1, Age == age+1) %>% .$PDnew2
    OldPD  = ToyExample %>% filter(Year == year, Age == age) %>% .$PrevalenceRaw2
    temp <- OldPD + NewPD - Death
    ToyExample$PrevalenceRaw2[ToyExample$Age == age + 1 & ToyExample$Year == year + 1] <- temp
    ToyExample$DeathPD[ToyExample$Age == age & ToyExample$Year == year] <- Death
  }
}

DF <- SetAgeGroup(AgeStart = 60, Gap = 5, AgeEnd = 89, Agevec = Ages)
ToyExample <- merge(ToyExample, DF, by = "Age", sort = F)
ToyExample %<>% mutate(AgeGroup5 = paste0("AgeGroup ", AgeGroup5))


#Plotting
RectDFyears2 <- data.frame(xMin = c(-Inf, 2016),
                           xMax = c(2004, Inf),
                           yMin = -Inf, yMax = Inf,
                           Color = "grey")

ToyIncidencePlot <- ggplot(ToyExample %>% filter(Age > 74), aes(Year, PDnew2, color = factor(Age))) +
  theme_classic(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Incidence (per 100K)", x = "") + 
  geom_rect(data = RectDFyears[1, ],
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  guides(color = guide_legend(title = "Age")) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 12) +
  facet_wrap(~AgeGroup5, scales = "free_y", nrow = 1)

ToyPrevalencePlot <- ggplot(ToyExample %>% filter(Age > 74), aes(Year, PrevalenceRaw2, color = factor(Age))) +
  theme_classic(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Prevalence (per 100K)", x = "Year") + 
  geom_rect(data = RectDFyears[1, ],
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  guides(color = guide_legend(title = "Age")) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 12) +
  facet_wrap(~AgeGroup5, scales = "free_y", nrow = 1)

ToyMortalityPlot <- ggplot(ToyExample %>% filter(Age > 74), aes(Year, (10^5)*DeathPD/PrevalenceRaw2, color = factor(Age))) +
  theme_classic(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Mortality (per 100K)", x = "Year") + 
  geom_rect(data = RectDFyears[1, ],
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  guides(color = guide_legend(title = "Age")) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 12) +
  facet_wrap(~AgeGroup5, scales = "free_y", nrow = 1)

ggarrange(ToyIncidencePlot, ToyMortalityPlot,  ToyPrevalencePlot, ncol = 1, heights = c(0.7, 0.7, 1), common.legend = T, legend = "right")
ggsave("Results/ToyExample.pdf", device = "pdf", width = 12, height = 10, dpi = 300, useDingbats = F)
