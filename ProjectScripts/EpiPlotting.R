#source("ProjectScripts/PrepareData.R")
install_github("https://github.com/eliocamp/ggnewscale")
library("ggnewscale")
packageF("grid")
packageF("epitools")
packageF("jtools")
packageF("forestplot")
#packageF("gt")


PlotEachYear <- function(measure, title, LegendPos = "none"){
  Plot <- ggplot(CombinedData %>% filter(Year < 2017, Year > 2004), aes_string("Age", measure)) +
    theme_bw() +
    theme(panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
          panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = LegendPos) +
    labs(x ="", y = paste0(measure, " (per 100,000)"), title = title,
         subtitle = "(not binned by age groups)") +
    scale_x_continuous(n.breaks = 8) +
    scale_fill_manual(values = c("brown", "darkgreen")) +
    scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                  RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
    geom_point(aes(color = as.character(Year))) +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), color = "black") +
    facet_wrap(~Sex)
  ChangeFacetLabels(Plot, FillCol = c(MoviePalettes$BugsLife[4],
                                                      MoviePalettes$BugsLife[2]))
}

#Plot Incidence, prevalence and PD mortality not binned by age groups
IncidenceEachAgePlot <- PlotEachYear("Incidence", "PD incidence")

PrevalenceEachAgePlot <- PlotEachYear("Prevalence", "PD prevalence")

MortalityPDEachAgePlot <- PlotEachYear("MortalityPD", "PD mortality")

temp <- ggplot(CombinedData %>% filter(Year < 2017, Year > 2004), aes(Age, Incidence)) +
  theme_bw() +
  geom_point(aes(color = as.character(Year))) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year")
    

ggarrange(ggarrange(IncidenceEachAgePlot, PrevalenceEachAgePlot, MortalityPDEachAgePlot, ncol = 1),
          legend.grob = get_legend(temp), legend = "right")

ggsave(paste0(ResultsPath, "DescriptiveEpiUnbinned.pdf"), device = "pdf", width = 6, height = 9, dpi = 300, useDingbats = F)

#### Plot General population sizes across the years in different population groups

RectDFyears <- data.frame(xMin = c(-Inf, 2016),
                          xMax = c(2004, Inf),
                          yMin = -Inf, yMax = Inf,
                          Color = "grey")


RectDF <- data.frame(xMin = seq(from = 0.5, to = 14.5, by = 2),
                     xMax = seq(from = 1.5, to = 15.5, by = 2),
                     yMin = -Inf, yMax = Inf,
                     Color = "grey")

PopulationSizePlot <- ggplot(CombinedDataFiveYears %>%
                               filter(AgeGroup %in% c("40-44", "45-49", "50-54", "55-59", "60-64",
                                                      "65-69", "70-74", "75-79", "80-84", "85-89")),
                             aes(Year, Number/10000, color = AgeGroup)) +
  theme_classic() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.box.spacing = unit(0.05, "line"), legend.spacing.x = unit(0.2, 'mm')) +
  guides(color = guide_legend(title.position = "top")) +
  labs(title = "General population, population size by age group", y = "Population size (in 10K)", x = "") +
  geom_point(size = 2) +
  scale_color_manual(values = MoviePalettes$MoonRiseKingdomColors) +
  scale_x_continuous(n.breaks = 17) +
  geom_line(size = 1.5, alpha = 0.7) +
  geom_rect(data = RectDFyears,
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  facet_wrap(~Sex, ncol = 1, scales = "free_y")

PopulationSizePlot <- ChangeFacetLabels(PopulationSizePlot, FillCol = c(MoviePalettes$BugsLife[4],
                                                                        MoviePalettes$BugsLife[2]))

RelData <- sapply(unique(CombinedDataFiveYears$AgeGroup), function(ageGroup){
  data <- CombinedDataFiveYears %>% filter(AgeGroup == ageGroup,
                                           Year > 2003, Year < 2017,
                                           !AgeGroup %in%  c("Younger than 30", "100 or older")) %>%
    select(AgeGroup, Sex, Year, Mortality, PDnew, PrevalenceRaw, DeathPD, Incidence, Prevalence, MortalityPD, YearAgeGroupSex5)
  dataNorm <- sapply(unique(data$Sex), function(sex){
    dataSex = data %>% filter(Sex == sex) %>% mutate(RelMortality = rescale(Mortality, c(0,1)),
                                                     RelIncidence = rescale(Incidence, c(0,1)),
                                                     RelPrevalence = rescale(Prevalence, c(0,1)),
                                                     RelMortalityPD = rescale(MortalityPD, c(0,1)))
  }, simplify = F) %>% rbindlist() %>% data.frame()
}, simplify = F) %>% rbindlist() %>% data.frame() %>% droplevels()

LogMortalityPlot <- ggplot(CombinedDataFiveYears %>%
                             filter(Year < 2017, Year > 2003,
                                    !AgeGroup %in% c("Younger than 30", "100 or older")),
                           aes(AgeGroup, log(Mortality), color = Sex)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = c(0.1,0.8)) +
  scale_color_manual(values = c(MoviePalettes$BugsLife[4],
                                MoviePalettes$BugsLife[2]), name = "") +
  labs(x ="", y = "log(Mortality (per 100,000))", title = "General population, mortality by age group") +
  stat_summary(fun.data = "mean_sdl",  position =position_dodge(width = 0.3), size = 0.2)

RelMortalityPlot <- ggplot(RelData, aes(AgeGroup, as.character(Year))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.background = element_blank(), legend.box.spacing = unit(0.1, "line"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.key.size = unit(1.2, "line"), legend.spacing.y = unit(0.5, 'mm')) +
  labs(y = "", x = "", title = "General population, yearly mortality change") +
  geom_tile(aes(fill = RelMortality)) +
  
  scale_fill_gradient2(low = MoviePalettes$MoonRiseKingdomColors[10],
                       mid = MoviePalettes$BugsLife[10],
                       high = MoviePalettes$MadMaxDesert[5], midpoint = 0.5, name = "Fold change mortality") +
  
  facet_wrap(~Sex, nrow = 1, scales = "free_x")



RelMortalityPlot <- ChangeFacetLabels(RelMortalityPlot, FillCol = c(MoviePalettes$BugsLife[4],
                                                                    MoviePalettes$BugsLife[2]))

ggarrange(PopulationSizePlot,
          ggarrange(LogMortalityPlot, RelMortalityPlot,
                    nrow = 2), ncol =2)

ggsave("Results/PopulationDemographics.pdf", device = "pdf", width = 10, height = 6, dpi = 300, useDingbats = F)

#Plot Population size
PopoultayionByAgeGroupPlot <- ggplot(CombinedDataFiveYears %>%
                                       filter(Year < 2017, Year > 2003, AgeGroup != "100 or older"),
                                     aes(AgeGroup, Number)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        legend.box = "horizontal", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.box.spacing = unit(0.05, "line"), legend.spacing.x = unit(0.2, 'mm')) +
  guides(color = guide_legend(title.position = "top")) +
  labs(x ="", y = "Population size", title = "General population") +
  new_scale("fill") +
  scale_fill_manual(values = c(MoviePalettes$BugsLife[4],
                               MoviePalettes$BugsLife[2])) +
  geom_rect(data = RectDF[-8,],
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  geom_boxplot(outlier.shape = NA, aes(fill = Sex)) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year), group = Sex),
             position = position_dodge2(width = 0.5)) 

ggsave("Results/PopoultayionByAgeGroupPlot.pdf", plot = PopoultayionByAgeGroupPlot, device = "pdf", width = 10, height = 6, dpi = 300, useDingbats = F)

PDincidencePlot <- ggplot(CombinedDataFiveYears %>%
                            filter(Year >  2004, Year < 2017, PDnew > 2,
                                   !AgeGroup %in% c("Younger than 30", "100 or older")),
                          aes(AgeGroup, Incidence)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background  = element_rect(fill = 0),
        legend.key = element_rect(fill = 0)) +
  labs(x ="", y = "Incidence (per 100,000)", title = "PD incidence (> 2 new cases)") +
  new_scale("fill") +
  scale_fill_manual(values = c(MoviePalettes$BugsLife[4],
                               MoviePalettes$BugsLife[2]), name = "") +
  geom_rect(data = RectDF[1:7, ],
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  geom_boxplot(outlier.shape = NA, aes(fill = Sex)) #+


RelIncidencePlot <- ggplot(RelData %>% filter(Year > 2004), aes(AgeGroup, as.character(Year))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.background = element_rect(colour = NA), legend.box.spacing = unit(0.1, "line"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.key.size = unit(1, "line"), legend.spacing.y = unit(0.5, 'mm')) +
  labs(y = "", x = "", title = "Age group incidence rescaled to [0-1]") +
  geom_tile(aes(fill = RelIncidence)) +
  scale_fill_gradient2(low = MoviePalettes$MoonRiseKingdomColors[10],
                       mid = MoviePalettes$BugsLife[10],
                       high = MoviePalettes$MadMaxDesert[5], midpoint = 0.5,
                       name = "Rescaled incidence", na.value = NA) +
  
  facet_wrap(~Sex, nrow = 1, scales = "free_x")

RelIncidencePlot <- ChangeFacetLabels(RelIncidencePlot, FillCol = c(MoviePalettes$BugsLife[4],
                                                                    MoviePalettes$BugsLife[2]))
ggarrange(PDincidencePlot, RelIncidencePlot, ncol = 2, widths = c(1.5,2))
ggsave("Results/PDincidence.pdf", device = "pdf", width = 10, height = 4, dpi = 300, useDingbats = F)



#Plot PD Prevalence
PDprevalencePlot <- ggplot(CombinedDataFiveYears %>% 
                             filter(Year > 2004, Year <2017, PrevalenceRaw > 2,
                                    !AgeGroup %in% c("Younger than 30", "100 or older")),
                           aes(AgeGroup, Prevalence)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background  = element_rect(fill = 0),
        legend.key = element_rect(fill = 0)) +
  labs(x ="", y = "Prevalence (per 100,000)", title = "PD prevalence (raw prevalance > 2)") +
  new_scale("fill") +
  scale_fill_manual(values = c(MoviePalettes$BugsLife[4],
                               MoviePalettes$BugsLife[2]), name = "") +
  geom_rect(data = RectDF[1:7, ],
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  geom_boxplot(outlier.shape = NA, aes(fill = Sex))


RelPrevalencePlot <- ggplot(RelData %>% filter(Year > 2004), aes(AgeGroup, as.character(Year))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.background = element_rect(colour = NA), legend.box.spacing = unit(0.1, "line"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.key.size = unit(1, "line"), legend.spacing.y = unit(0.5, 'mm')) +
  labs(y = "", x = "", title = "PD prevalence scaled by year") +
  geom_tile(aes(fill = RelPrevalence)) +
  scale_fill_gradient2(low = MoviePalettes$MoonRiseKingdomColors[10],
                       mid = MoviePalettes$BugsLife[10],
                       high = MoviePalettes$MadMaxDesert[5], midpoint = 0.5,
                       name = "Relative prevalence", na.value = NA) +
  
  facet_wrap(~Sex, nrow = 1, scales = "free_x")

RelPrevalencePlot <- ChangeFacetLabels(RelPrevalencePlot, FillCol = c(MoviePalettes$BugsLife[4],
                                                                      MoviePalettes$BugsLife[2]))
ggarrange(PDprevalencePlot, RelPrevalencePlot, ncol = 2, widths = c(1.5,2))
ggsave("Results/PDprevalence.pdf", device = "pdf", width = 10, height = 4, dpi = 300, useDingbats = F)


ggarrange(PDincidencePlot, PDprevalencePlot)
ggsave("Results/PDinc_prev.pdf", device = "pdf", width = 10, height = 4, dpi = 300, useDingbats = F)



#Plot PD Mortality
MortalityPDPlot <- ggplot(CombinedDataFiveYears5b %>%
                            filter(Year > 2004, Year < 2017,
                                   !is.na(MortalityPD),
                                   !AgeGroup %in% c("Younger than 30", "100 or older")),
                          aes(AgeGroup, log(MortalityPD), color = Sex)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = c(0.1,0.8)) +
  coord_cartesian(ylim = c(4, 12)) +
  scale_color_manual(values = c(MoviePalettes$BugsLife[4],
                                MoviePalettes$BugsLife[2]), name = "") +
  labs(x ="", y = "log(Mortality (per 100,000))", title = "PD mortality") +
  stat_summary(fun.data = "mean_sdl",  position =position_dodge(width = 0.3), size = 0.2)

LogMortalityPlot2 <- ggplot(CombinedDataFiveYears5b %>%
                             filter(Year < 2017, Year > 2004,
                                    !AgeGroup %in% c("Younger than 30", "100 or older")),
                           aes(AgeGroup, log(Mortality), color = Sex)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = c(0.1,0.8)) +
  coord_cartesian(ylim = c(4, 12)) +
  scale_color_manual(values = c(MoviePalettes$BugsLife[4],
                                MoviePalettes$BugsLife[2]), name = "") +
  labs(x ="", y = "log(Mortality (per 100,000))", title = "General population mortality") +
  stat_summary(fun.data = "mean_sdl",  position =position_dodge(width = 0.3), size = 0.2)


RelMortalityPDPlot <- ggplot(RelData %>% filter(!AgeGroup %in% c("30-34", "35-39",
                                                                 "40-44", "45-49"), Year >2004),
                             aes(AgeGroup, as.character(Year))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.background = element_rect(colour = NA), legend.box.spacing = unit(0.1, "line"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.key.size = unit(1, "line"), legend.spacing.y = unit(0.5, 'mm')) +
  labs(y = "", x = "", title = "Yearly PD mortality change") +
  geom_tile(aes(fill = RelMortalityPD)) +
  scale_fill_gradient2(low = MoviePalettes$MoonRiseKingdomColors[10],
                       mid = MoviePalettes$BugsLife[10],
                       high = MoviePalettes$MadMaxDesert[5], midpoint = 0.5,
                       name = "Relative mortality", na.value = NA) +
  
  facet_wrap(~Sex, nrow = 1, scales = "free_x")

RelMortalityPDPlot <- ChangeFacetLabels(RelMortalityPDPlot, FillCol = c(MoviePalettes$BugsLife[4],
                                                                        MoviePalettes$BugsLife[2]))
ggarrange(MortalityPDPlot, RelMortalityPDPlot, ncol = 2, widths = c(1.5,2))
ggsave("Results/PDmortality.pdf", device = "pdf", width = 10, height = 4, dpi = 300, useDingbats = F)



## Ploting the yearly changes in measures
PlotMeasureChangeSub <- function(data = CombinedDataFiveYears5b, Agegroups,
                              YearMin = 2005, YearMax = 2016, measure, colors,
                              alpha = 1, linesize = 1, pointsize = 1){
  Plot <- ggplot(data %>%
                   filter(AgeGroup %in% Agegroups,
                          Year >= YearMin, Year <= YearMax),
                 aes_string("Year",measure, color = "AgeGroup")) +
    theme_classic() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.box.spacing = unit(0.05, "line"), legend.spacing.x = unit(0.2, 'mm')) +
    guides(color = guide_legend(title.position = "top")) +
    labs(title = "", y = paste0(measure, " (per 100,000)", x = "")) +
    geom_point(size = pointsize) +
    scale_color_manual(values = colors) +
    scale_x_continuous(n.breaks = 17) +
    geom_line(size = linesize, alpha = alpha) +
    facet_wrap(~Sex, ncol = 1, scales = "free_y")
  
  ChangeFacetLabels(Plot, FillCol = c(MoviePalettes$BugsLife[4],
                                      MoviePalettes$BugsLife[2]))
}


PlotMeasureChangeWrap <- function(data = CombinedDataFiveYears5b, measure,
                                  AgeGroups1 = c("30-59", "60-64", "65-69", "70-74"),
                                  AgeGroups2 = c("75-79", "80-84", "85-89", "90-94"),
                                  colors1 = MoviePalettes$BlueIsTheWarmestColor[c(10, 4, 6, 7)],
                                  colors2 = MoviePalettes$BlueIsTheWarmestColor[c(10, 4, 6, 7)]){
  
  Plot1 <- PlotMeasureChangeSub(data = CombinedDataFiveYears5b,
                                Agegroups =  AgeGroups1,
                                measure = measure,
                                colors = colors1)
  
  Plot2 <- PlotMeasureChangeSub(data = CombinedDataFiveYears5b,
                                Agegroups =  AgeGroups2,
                                measure = measure,
                                colors = colors2)
  ggarrange(Plot1, Plot2, ncol = 2)                             
  
}



IncidenceChangePlot <- PlotMeasureChangeWrap(data = CombinedDataFiveYears5b, measure = "Incidence")

PrevaenceChangePlot <- PlotMeasureChangeWrap(data = CombinedDataFiveYears5b, measure = "Prevalence")
                                        
MortalityChangePlot <- PlotMeasureChangeWrap(data = CombinedDataFiveYears5b, measure = "MortalityPD")


#Plot PD mortality relative to general mortality
RelativeMortalityPlot <- ggplot(CombinedDataFiveYears5b %>% filter(Year < 2017, Year > 2004,
                                                                  DeathPD > 2,
                                                                  AgeGroup != "100 or older"),
                                aes(AgeGroup, MortalityPD/Mortality)) +
  theme_bw() +
  labs(x ="Age group", y = "Mortality ratio ( PD / general population)",
       title = "PD mortality relative to population mortality") +
  
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_line(colour = "grey50", linetype = "dashed"),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.8,0.8),
        legend.background  = element_rect(fill = 0, color = 0),
        legend.key = element_rect(fill = 0, color = 0)) +
  new_scale("fill") +
  scale_fill_manual(values =c(MoviePalettes$BugsLife[4],
                              MoviePalettes$BugsLife[2]), name = "") +
  geom_rect(data = RectDF[1:5,],
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  geom_boxplot(outlier.shape = NA, aes(fill = Sex)) +
  coord_cartesian(ylim = c(0, 7)) +
  geom_hline(yintercept = 1, color = "red")


ggsave("Results/RelativeMortalityPlot.pdf", plot = RelativeMortalityPlot,  device = "pdf", width = 6, height = 4, dpi = 300, useDingbats = F)


#Plot incidence ratio separately in each age
ggplot(CombinedDataWide %>%
         filter(PDnew_F > 2, PDnew_M > 2, Year > 2004, Year < 2017),
       aes(Age, IncidenceRatio)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed")) +
  labs(x = "Age (years)", y = "Male/Female incidence ratio", title = "PDnew_F > 2 & PDnew_M > 2") + 
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year))) +
  geom_smooth(method = "lm",color = "black")

ggsave("Results/IncidenceRatioPerage.pdf", device = "pdf", width = 8, height = 8, dpi = 300, useDingbats = F)


#linear regression for incidence ratio age dependency
lm(IncidenceRatio~Age + c(Year-2015), data = CombinedDataWide %>%
     filter(PDnew_F > 2, PDnew_M > 2, Year > 2004, Year < 2017)) %>% summary


#Negative binomial regression to check for whether the effect of sex
IncideceRatioStat2 <- MASS::glm.nb(PDnew ~ c(Age-30)+Sex + Year + offset(log(Number)),data = CombinedData %>% 
                           filter(Year > 2004, Year < 2017, AgeGroup5b != "100 or older"))

IncideceRatioStat2b <- MASS::glm.nb(PDnew ~ c(Age-30)*Sex + Year + offset(log(Number)),data = CombinedData %>% 
                                      filter(Year > 2004, Year < 2017, AgeGroup5b != "100 or older"))

#check for whether adding interaction term is appropriate
anova(IncideceRatioStat2, IncideceRatioStat2b)


#Calculate odds ratio for mortality M vs F in each age in PD vs General
temp2 <- CombinedDataFiveYears5b %>% filter(Year > 2004, Year < 2017,
                                           DeathPD > 2, !is.na(AgeGroup), AgeGroup != "100 or older") %>%
  select(Year, Sex, AgeGroup, Number, Death, PrevalenceRaw, DeathPD) %>% droplevels()

OddsList <- sapply(levels(temp2$AgeGroup) , function(agegroup){
  data = temp2 %>% filter(AgeGroup == agegroup)
  YearData <- sapply(unique(data$Year), function(year){
    dataYear = data %>% filter(Year == year) %>% select(-Year) %>%
      mutate(AliveAll = Number - Death, AlivePD = PrevalenceRaw - DeathPD)
    
    sapply(dataYear$Sex, function(sex){
      dataSex <- matrix(dataYear %>% filter(Sex ==sex) %>%
                          select(AliveAll, AlivePD, Death, DeathPD) %>% unlist, byrow = F, nrow = 2)
      
      dimnames(dataSex) <- list(Group = c("All", "PD"),
                                Outcome = c("Alive", "Dead"))
      Output = oddsratio(dataSex)
      data.frame(Year = year,
                 Sex = sex,
                 AgeGroup = agegroup,
                 OddRatioEst = round(Output$measure[2,1], digits = 2),
                 OddLow = round(Output$measure[2,2], digits = 2),
                 OddsHigh = round(Output$measure[2,3], digits = 2),
                 OddsRatio = paste0(round(Output$measure[2,1], digits = 2),
                                    " (", round(Output$measure[2,2], digits = 2), "-",
                                    round(Output$measure[2,3], digits = 2),")"),
                 ChiPvalue = signif(Output$p.value[2,3], digits = 1))
    }, simplify = F) %>% rbindlist() %>% data.frame()
  }, simplify = F) 
  names(YearData) <- unique(data$Year)
  YearData %>% rbindlist %>% data.frame()
}, simplify = F) %>% rbindlist() %>% data.frame()

OddsList$Sex <- factor(OddsList$Sex, levels = c("F", "M"))

OddsRatioPlot <- ggplot(OddsList,
                        aes(AgeGroup, OddRatioEst)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_line(colour = "grey50", linetype = "dashed"),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        #axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.8,0.8),
        legend.background  = element_rect(fill = 0, color = 0),
        legend.key = element_rect(fill = 0, color = 0)) +
  labs(x = "", y = "OddsRatio (Chi-square)") +
  new_scale("fill") +
  scale_fill_manual(values =c(MoviePalettes$BugsLife[4],
                              MoviePalettes$BugsLife[2]), name = "") +
  geom_rect(data = RectDF[1:5,],
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
            alpha=0.4, inherit.aes = FALSE) +
  geom_boxplot(outlier.shape = NA, aes(fill = Sex)) +
  coord_cartesian(ylim = c(0, 7)) +
  geom_hline(yintercept = 1, color = "red")


pd <- position_dodge(0.5)
ggplot(OddsList,
       aes(AgeGroup, OddRatioEst)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") +
  labs(x = "", y = "OddsRatio (Chi-square)") +
  new_scale("fill") +
  scale_color_manual(values =c(MoviePalettes$BugsLife[4],
                              MoviePalettes$BugsLife[2]), name = "") +
  geom_hline(yintercept = 1, color = "red", lty = "dashed") +
  # geom_rect(data = RectDF[1:5,],
  #           aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), fill="grey",
  #           alpha=0.4, inherit.aes = FALSE) +
  geom_point(aes(color = Sex, group = Sex),
                 position = pd, show.legend = T) +
  geom_errorbar(aes(ymin = OddLow, ymax = OddsHigh, color = Sex, group = Sex),
                position = pd, show.legend = T) +
  coord_cartesian(ylim = c(0, 10)) +
  facet_wrap(~Year)
ggsave(paste0(ResultsPath, "OddsRatiosByYear.pdf"), device = "pdf", width = 10, height = 8, dpi = 300, useDingbats = F)



ggarrange(ggarrange(MortalityPDPlot, LogMortalityPlot2, labels = c("A", "B")),
          OddsRatioPlot, nrow = 2, labels = c("", "C"), heights = c(1.5,1))

ggsave("Results/MortalityRatioCombined.pdf",  device = "pdf", width = 6, height = 6, dpi = 300, useDingbats = F)

#Create Forest plots
PDdata <- CombinedData %>% filter(AgeGroup5 != "100 or older",
                                  Year > 2004, Year < 2017)

GetModSumm <- function(Mod, exp = T, digits = 2){
  Coef <- Mod %>% summary %>% coef() %>% .[-1,] %>% data.frame()
  Covar =  rownames(Coef)
  names(Coef) <- c("Est", "SE", "zVal", "pVal")
  Coef$pVal <- sapply(Coef$pVal, function(x){
    if(x < 0.01){
      paste0(formatC(x, digits = digits, format = "e"), GetSigChar(x))
    } else {
      round(x, digits = 2)
    }
  })
  
  Covar = data.frame(Covar = Covar)
  DF <- cbind(Covar, Coef)
  
  DF$Model <- if(unique(Mod$family$family) == "poisson"){
    "Poisson"
  } else {
    "NegBin"
  }
  
  Confint <- confint(Mod) %>% .[-1,] %>% data.frame()
  names(Confint) <- c("low", "high")
  
  DF <- cbind(DF, Confint)
  if(exp){
    
    DF %<>% mutate("exp(Est)" = exp(Est),
                   "exp(low)" = exp(low),
                   "exp(high)" = exp(high))
    
    DF$Est <- sapply(DF$Est, function(x){
      if(x < 0.001){
        formatC(x, digits = 2, format = "e")
      } else {
        signif(x, digits = 2)
      }
    })
    
    DF$SE <- sapply(DF$SE, function(x){
      if(x < 0.001){
        formatC(x, digits = 2, format = "e")
      } else {
        signif(x, digits = 2)
      }
    })
  }
  return(DF)
}


GetPoissoStat <- function(Data = PDdata, ageGroups = unique(PDdata$AgeGroup5b), AgeGroupCol = "AgeGroup5b",
                          measure, Offset = "Number", rawMeasure){
  StatsList <- sapply(ageGroups, function(ageGroup){
    ObsYears = unique(Data$Year)
    subData <- Data[Data[[AgeGroupCol]] == ageGroup,]
    Base = subData %>% filter(Year %in% c(ObsYears[1]:ObsYears[3])) %>% .[[measure]] %>% mean
    Final = subData %>% filter(Year %in% c(2014:2016)) %>% .[[measure]] %>% mean
    Mean = mean(subData[[measure]])
    SD = sd(subData[[measure]])
    Model = formula(paste0(rawMeasure, "~Year + Sex + offset(log(", Offset,"))"))
    
    temp <- glm(Model, family = "poisson", data = subData)
    
    DispPoiss = formatC(temp$deviance/temp$df.residual, digits = 2, format = "e")
    
    CoefYearPoiss = summary(temp)$coef[2,1]
    pValuYearePoiss =summary(temp)$coef[2,4]
    CoefSexPoiss = summary(temp)$coef[3,1]
    pValuSexPoiss = summary(temp)$coef[3,4]
    DF <- data.frame(AgeGroup = ageGroup,
                     CaseNumberCor = cor.test(subData[[rawMeasure]], subData[["Number"]])$estimate,
                     CoefYearPoiss = formatC(CoefYearPoiss, digits = 2, format = "e"),
                     logCoefYearPoiss = formatC(exp(CoefYearPoiss), digits = 2, format = "e"),
                     pValueYearPoiss = formatC(pValuYearePoiss, digits = 2, format = "e"),
                     CoefSexPoiss = formatC(CoefSexPoiss, digits = 2, format = "e"),
                     logCoefSexPoiss = signif(exp(CoefSexPoiss), digits = 2),
                     pValueSexPoiss = formatC(pValuSexPoiss, digits = 2, format = "e"),
                     DispPoiss = DispPoiss,
                     CoefYearNB = NA,
                     logCoefYearNB = NA,
                     pValueYearNB = NA,
                     CoefSexNB = NA,
                     logCoefSexNB = NA,
                     pValueSexNB = NA,
                     Change = paste0(round(100*(Final/Base-1), digits = 0), "%"),
                     YearlyCases = round(mean(subData$PDnew), digits = 1),
                     CV = paste0(round(100*SD/Mean, digits = 1), "%"))
    DFsum <- GetModSumm(temp, exp = T, digits = 2)
    
    temp2 = NA
    if(DispPoiss > 1.5){
      temp2 <- MASS::glm.nb(formula = Model, data = subData)
      if("th.warn" %in% names(temp2)){
        if(temp2$th.warn == "iteration limit reached"){
          DF %<>% mutate(CoefYearNB = "Didn't converge")
        } else {
          DF %<>% mutate(CoefSexNB = "Non-convergence issue")
        }
        temp2 <- NA
      } else {
        DF %<>% mutate(CoefYearNB = summary(temp2)$coef[2,1],
                       logCoefYearNB = formatC(exp(CoefYearNB), digits = 2, format = "e"),
                       pValueYearNB = formatC(summary(temp2)$coef[2,4], digits = 2, format = "e"),
                       CoefSexNB = summary(temp2)$coef[3,1],
                       logCoefSexNB = formatC(exp(CoefSexNB), digits = 2, format = "e"),
                       pValueSexNB = formatC(summary(temp2)$coef[3,4], digits = 2, format = "e"),
                       CoefYearNB = formatC(CoefYearNB, digits = 2, format = "e"),
                       CoefSexNB = formatC(CoefSexNB, digits = 2, format = "e"))
        
        DFsum <- GetModSumm(temp2, exp = T, digits = 2)
      }
    }
    
    FinalDF <- DFsum %>% mutate(AgeGroup = ageGroup) %>%
      select(ncol(.), c(1:ncol(.)-1))

    FinalDF %<>% mutate(DispPoisson = DF$DispPoiss,
                        Change = DF$Change,
                        CV = DF$CV,
                        YearlyCases = DF$YearlyCases,
                        Est_CI = paste0(Est, " (", formatC(low, digits = 2, format = "e"),
                                        ",", formatC(high, digits = 2, format = "e"), ")"))
    list(poissonMod = temp, nbMod = temp2, statsDF_all = DF, FinalStat_DF = FinalDF)
  }, simplify = F)
  
  
  
  StatsAlldata <- lapply(StatsList, function(ageGroup){
    ageGroup$statsDF_all
  }) %>% rbindlist() %>% data.frame
  
  
  StatsFinal <- lapply(StatsList, function(ageGroup){
    ageGroup$FinalStat_DF
  }) %>% rbindlist() %>% data.frame %>%
    arrange(Covar)
  return(list(StatsList = StatsList, StatsAlldata = StatsAlldata, StatsFinal = StatsFinal))
  
}

GetForestplotTable <- function(Data, covar){
  CovarData <- Data %>% filter(Covar == covar)
  rownames(CovarData) <- paste0("Age_", CovarData$AgeGroup)
  
  Table <- cbind(c("AgeGroup", as.character(CovarData$AgeGroup)),
                 c("pValue", CovarData$pVal),
                 c("Estimate (SE)", paste0(CovarData$Est, " (",
                                           CovarData$SE, ")")))
  return(list(Data = CovarData, Table = Table))
}



#Change in Incidence across the years
InsidenceStats <- GetPoissoStat(Data = PDdata, measure = "Incidence", rawMeasure = "PDnew")
IncidenceSexForest <- GetForestplotTable(InsidenceStats$StatsFinal, "SexM")
IncidenceYearForest <- GetForestplotTable(InsidenceStats$StatsFinal, "Year")

write.table(InsidenceStats$StatsFinal, file = paste0(ResultsPath, "InsidenceStat.tsv"), sep = "\t", row.names = F, col.names = T)

#Change in Prevalence across the years
PrevalenceStats <- GetPoissoStat(measure = "Prevalence", rawMeasure = "PrevalenceRaw")
PrevalenceSexForest <- GetForestplotTable(PrevalenceStats$StatsFinal, "SexM")
PrevalenceYearForest <- GetForestplotTable(PrevalenceStats$StatsFinal, "Year")

write.table(PrevalenceStats$StatsFinal, file = paste0(ResultsPath, "PrevalenceStat.tsv"), sep = "\t", row.names = F, col.names = T)

#Change in Mortality of PD across the years
MortalityPDStats <- GetPoissoStat(PDdata %>% filter(PrevalenceRaw > 5),
                                  measure = "MortalityPD", rawMeasure = "DeathPD", Offset = "PrevalenceRaw")
MortalityPDSexForest <- GetForestplotTable(MortalityPDStats$StatsFinal, "SexM")
MortalityPDYearForest <- GetForestplotTable(MortalityPDStats$StatsFinal, "Year")


pdf(paste0(ResultsPath, "ForestPlots.pdf"),  width = 6, height = 4, useDingbats = F)                
forestplot(title = "Incidence, Sex",
           IncidenceSexForest$Table,
           mean = c(NA, IncidenceSexForest$Data$exp.Est.),
           lower = c(NA, IncidenceSexForest$Data$exp.low.),
           upper = c(NA, IncidenceSexForest$Data$exp.high.),
           is.summary = c(TRUE, rep(FALSE, 9)), 
           clip = c(1,2.4), zero = 1.5, boxsize = 0.2, 
           col = fpColors(box=c("royalblue"),line="darkblue", summary="royalblue", hrz_lines = "#444444"),
           txt_gp = fpTxtGp(cex=1, title = gpar(cex = 1.5), xlab = gpar(cex=1), ticks = gpar(cex=0.75)),
           xlab = "M/F ratio")


forestplot(title = "Incidence (2005-2016)",
           IncidenceYearForest$Table, new_page = T,
           mean = c(NA, IncidenceYearForest$Data$exp.Est.),
           lower = c(NA, IncidenceYearForest$Data$exp.low.),
           upper = c(NA, IncidenceYearForest$Data$exp.high.),
           is.summary = c(TRUE, rep(FALSE, 9)), 
           clip = c(0.9,1.1), zero = 1, boxsize = 0.2, xticks = c(seq(0.9, 1.1, 0.02)),
           col = fpColors(box="royalblue",line="darkblue", summary="royalblue", hrz_lines = "#444444"),
           txt_gp = fpTxtGp(cex=1, title = gpar(cex = 1.5), xlab = gpar(cex=1), ticks = gpar(cex=0.75)),
           xlab = "Yearly change")



forestplot(title = "Prevalence, Sex",
           PrevalenceSexForest$Table, new_page = TRUE,
           mean = c(NA, PrevalenceSexForest$Data$exp.Est.),
           lower = c(NA, PrevalenceSexForest$Data$exp.low.),
           upper = c(NA, PrevalenceSexForest$Data$exp.high.),
           is.summary = c(TRUE, rep(FALSE, 9)), 
           clip = c(1,2), zero = 1.5, boxsize = 0.2, xticks = (seq(1,2, 0.1)),
           col = fpColors(box=c("royalblue"),line="darkblue", summary="royalblue", hrz_lines = "#444444"),
           txt_gp = fpTxtGp(cex=1, title = gpar(cex = 1.5), xlab = gpar(cex=1), ticks = gpar(cex=0.75)),
           xlab = "M/F ratio")



forestplot(title = "Prevalence (2005-2016)",
           PrevalenceYearForest$Table, new_page = TRUE,
           mean = c(NA, PrevalenceYearForest$Data$exp.Est.),
           lower = c(NA, PrevalenceYearForest$Data$exp.low.),
           upper = c(NA, PrevalenceYearForest$Data$exp.high.),
           is.summary = c(TRUE, rep(FALSE, 9)), 
           clip = c(0.9,1.1), zero = 1, boxsize = 0.2, xticks = seq(0.9, 1.1, 0.02),
           col = fpColors(box="royalblue",line="darkblue", summary="royalblue", hrz_lines = "#444444"),
           txt_gp = fpTxtGp(cex=1, title = gpar(cex = 1.5), xlab = gpar(cex=1), ticks = gpar(cex=0.75)),
           xlab = "Yearly change")

forestplot(title = "MortalityPD, Sex",
           MortalityPDSexForest$Table,
           mean = c(NA, MortalityPDSexForest$Data$exp.Est.),
           lower = c(NA, MortalityPDSexForest$Data$exp.low.),
           upper = c(NA, MortalityPDSexForest$Data$exp.high.),
           is.summary = c(TRUE, rep(FALSE, 9)), 
           clip = c(0,2.5), zero = 1, boxsize = 0.2, xticks = c(seq(0, 2.5, 0.5)),
           col = fpColors(box=c("royalblue"),line="darkblue", summary="royalblue", hrz_lines = "#444444"),
           txt_gp = fpTxtGp(cex=1, title = gpar(cex = 1.5), xlab = gpar(cex=1), ticks = gpar(cex=0.75)),
           xlab = "M/F ratio")

forestplot(title = "MortalityPD, Year",
           MortalityPDYearForest$Table,
           mean = c(NA, MortalityPDYearForest$Data$exp.Est.),
           lower = c(NA, MortalityPDYearForest$Data$exp.low.),
           upper = c(NA, MortalityPDYearForest$Data$exp.high.),
           is.summary = c(TRUE, rep(FALSE, 9)), 
           clip = c(0.9,1.1), zero = 1, boxsize = 0.2, xticks = c(seq(0.9, 1.1, 0.02)),
           col = fpColors(box=c("royalblue"),line="darkblue", summary="royalblue", hrz_lines = "#444444"),
           txt_gp = fpTxtGp(cex=1, title = gpar(cex = 1.5), xlab = gpar(cex=1), ticks = gpar(cex=0.75)),
           xlab = "Yearly change")
dev.off()

