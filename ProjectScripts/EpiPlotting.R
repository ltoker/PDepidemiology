source("ProjectScripts/PrepareData.R")
install_github("https://github.com/eliocamp/ggnewscale")
library("ggnewscale")
packageF("grid")
packageF("epitools")
packageF("gt")


#### Plot General population sizes across the years in different population groups

RectDFyears <- data.frame(xMin = c(-Inf, 2016),
                          xMax = c(2005, Inf),
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
                                           Year > 2004, Year < 2017,
                                           !AgeGroup %in%  c("Younger than 30", "100 or older")) %>%
    select(AgeGroup, Sex, Year, Mortality, PDnew, PrevalenceRaw, DeathPD, Incidence, Prevalence, MortalityPD, YearAgeGroupSex)
  dataNorm <- sapply(unique(data$Sex), function(sex){
    dataSex = data %>% filter(Sex == sex) %>% mutate(RelMortality = rescale(Mortality, c(0,1)),
                                                     RelIncidence = rescale(Incidence, c(0,1)),
                                                     RelPrevalence = rescale(Prevalence, c(0,1)),
                                                     RelMortalityPD = rescale(MortalityPD, c(0,1)))
  }, simplify = F) %>% rbindlist() %>% data.frame()
}, simplify = F) %>% rbindlist() %>% data.frame() %>% droplevels()

LogMortalityPlot <- ggplot(CombinedDataFiveYears %>%
                             filter(Year < 2017, Year > 2005,
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
                                       filter(Year < 2017, Year > 2004, AgeGroup != "100 or older"),
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


RelIncidencePlot <- ggplot(RelData, aes(AgeGroup, as.character(Year))) +
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



#Plotting incidence change over years
IncidenceChangePlot1 <- ggplot(CombinedDataFiveYears2 %>%
                                 filter(AgeGroup %in% c("30-59", "60-64", "65-69", "70-74"),
                                        Year > 2004, Year < 2017),
                               aes(Year,Incidence, color = AgeGroup)) +
  theme_classic() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.box.spacing = unit(0.05, "line"), legend.spacing.x = unit(0.2, 'mm')) +
  guides(color = guide_legend(title.position = "top")) +
  labs(title = "", y = "Incidence (per 100,000)", x = "") +
  geom_point(size = 1) +
  scale_color_manual(values = MoviePalettes$MoonRiseKingdomColors) +
  scale_x_continuous(n.breaks = 17) +
  geom_line(size = 1.2, alpha = 0.7) +
  facet_wrap(~Sex, ncol = 1, scales = "free_y")

IncidenceChangePlot1 <- ChangeFacetLabels(IncidenceChangePlot1, FillCol = c(MoviePalettes$BugsLife[4],
                                                                            MoviePalettes$BugsLife[2]))

IncidenceChangePlot2 <- ggplot(CombinedDataFiveYears2 %>%
                                 filter(AgeGroup %in% c("75-79", "80-84", "85-89", "90-94"),
                                        Year > 2004, Year < 2017),
                               aes(Year,Incidence, color = AgeGroup)) +
  theme_classic() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.box.spacing = unit(0.05, "line"), legend.spacing.x = unit(0.2, 'mm')) +
  guides(color = guide_legend(title.position = "top")) +
  labs(title = "", y = "", x = "") +
  geom_point(size = 1) +
  scale_color_manual(values = MoviePalettes$MoonRiseKingdomColors) +
  scale_x_continuous(n.breaks = 17) +
  geom_line(size = 1.2, alpha = 0.7) +
  facet_wrap(~Sex, ncol = 1, scales = "free_y")

IncidenceChangePlot2 <- ChangeFacetLabels(IncidenceChangePlot2, FillCol = c(MoviePalettes$BugsLife[4],
                                                                            MoviePalettes$BugsLife[2]))
ggarrange(IncidenceChangePlot1, IncidenceChangePlot2, ncol = 2)                                                                       

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


RelPrevalencePlot <- ggplot(RelData, aes(AgeGroup, as.character(Year))) +
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


PrevalenceChangePlot1 <- ggplot(CombinedDataFiveYears %>%
                                  filter(AgeGroup %in% c("35-39", "40-44", "45-49", 
                                                         "50-54", "55-59", "60-64"),
                                         Year > 2004, Year < 2017),
                                aes(Year,Prevalence, color = AgeGroup)) +
  theme_classic() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.box.spacing = unit(0.05, "line"), legend.spacing.x = unit(0.2, 'mm')) +
  guides(color = guide_legend(title.position = "top")) +
  labs(title = "", y = "Prevalence (per 100,000)", x = "") +
  geom_point(size = 1) +
  scale_color_manual(values = MoviePalettes$MoonRiseKingdomColors) +
  scale_x_continuous(n.breaks = 17) +
  geom_line(size = 1.2, alpha = 0.7) +
  facet_wrap(~Sex, ncol = 1, scales = "free_y")

PrevalenceChangePlot1 <- ChangeFacetLabels(PrevalenceChangePlot1, FillCol = c(MoviePalettes$BugsLife[4],
                                                                              MoviePalettes$BugsLife[2]))

PrevalenceChangePlot2 <- ggplot(CombinedDataFiveYears %>%
                                  filter(AgeGroup %in% c("65-69", "70-74", "75-79",
                                                         "80-84", "85-89", "90-94"),
                                         Year > 2004, Year < 2017),
                                aes(Year,Prevalence, color = AgeGroup)) +
  theme_classic() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.box.spacing = unit(0.05, "line"), legend.spacing.x = unit(0.2, 'mm')) +
  guides(color = guide_legend(title.position = "top")) +
  labs(title = "", y = "", x = "") +
  geom_point(size = 1) +
  scale_color_manual(values = MoviePalettes$MoonRiseKingdomColors) +
  scale_x_continuous(n.breaks = 17) +
  geom_line(size = 1.2, alpha = 0.7) +
  facet_wrap(~Sex, ncol = 1, scales = "free_y")

PrevalenceChangePlot2 <- ChangeFacetLabels(PrevalenceChangePlot2, FillCol = c(MoviePalettes$BugsLife[4],
                                                                              MoviePalettes$BugsLife[2]))
ggarrange(PrevalenceChangePlot1, PrevalenceChangePlot2, ncol = 2)       



#Plot PD Mortality
MortalityPDPlot <- ggplot(CombinedDataFiveYears %>%
                            filter(Year > 2004, Year < 2017,
                                   !is.na(MortalityPD), DeathPD > 2,
                                   PrevalenceRaw > 5,
                                   !AgeGroup %in% c("Younger than 30", "100 or older")),
                          aes(AgeGroup, log(MortalityPD))) +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.15,0.8),
        legend.background  = element_rect(fill = 0, color = 0),
        legend.key = element_rect(fill = 0, color = 0)) +
  labs(x ="", y = "log (Mortality/ per 100K)", title = "PD mortality\n(Death > 2, raw prevalence > 5)") +
  new_scale("fill") +
  scale_fill_manual(values = c(MoviePalettes$BugsLife[4],
                               MoviePalettes$BugsLife[2]), name = "") +
  geom_rect(data = RectDF[1:5, ],
            aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax),
            fill = "grey",
            alpha=0.4, inherit.aes = FALSE) +
  geom_boxplot(outlier.shape = NA, aes(fill = Sex))


RelMortalityPDPlot <- ggplot(RelData %>% filter(!AgeGroup %in% c("30-34", "35-39",
                                                                 "40-44", "45-49")),
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
ggsave("Results/PDmortality.png", device = "png", width = 10, height = 4, dpi = 300)



MortalityPDChangePlot1 <- ggplot(CombinedDataFiveYears %>%
                                   filter(AgeGroup %in% c("35-39", "40-44", "45-49", 
                                                          "50-54", "55-59", "60-64"),
                                          Year > 2004, Year < 2017),
                                 aes(Year,MortalityPD, color = AgeGroup)) +
  theme_classic() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.box.spacing = unit(0.05, "line"), legend.spacing.x = unit(0.2, 'mm')) +
  guides(color = guide_legend(title.position = "top")) +
  labs(title = "", y = "MortalityPD (per 100,000)", x = "") +
  geom_point(size = 1) +
  scale_color_manual(values = MoviePalettes$MoonRiseKingdomColors) +
  scale_x_continuous(n.breaks = 17) +
  geom_line(size = 1.2, alpha = 0.7) +
  facet_wrap(~Sex, ncol = 1, scales = "free_y")

MortalityPDChangePlot1 <- ChangeFacetLabels(MortalityPDChangePlot1, FillCol = c(MoviePalettes$BugsLife[4],
                                                                                MoviePalettes$BugsLife[2]))

MortalityPDChangePlot2 <- ggplot(CombinedDataFiveYears %>%
                                   filter(AgeGroup %in% c("65-69", "70-74", "75-79",
                                                          "80-84", "85-89", "90-94"),
                                          Year > 2004, Year < 2017),
                                 aes(Year,MortalityPD, color = AgeGroup)) +
  theme_classic() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.box.spacing = unit(0.05, "line"), legend.spacing.x = unit(0.2, 'mm')) +
  guides(color = guide_legend(title.position = "top")) +
  labs(title = "", y = "", x = "") +
  geom_point(size = 1) +
  scale_color_manual(values = MoviePalettes$MoonRiseKingdomColors) +
  scale_x_continuous(n.breaks = 17) +
  geom_line(size = 1.2, alpha = 0.7) +
  facet_wrap(~Sex, ncol = 1, scales = "free_y")

MortalityPDChangePlot2 <- ChangeFacetLabels(MortalityPDChangePlot2, FillCol = c(MoviePalettes$BugsLife[4],
                                                                                MoviePalettes$BugsLife[2]))
ggarrange(MortalityPDChangePlot1, MortalityPDChangePlot2, ncol = 2)   



#Plot PD mortality relative to general mortality
RelativeMortalityPlot <- ggplot(CombinedDataFiveYears2 %>% filter(Year < 2017, Year > 2004,
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

ggsave("Results/RelativeMortalityPlot", plot = RelativeMortalityPlot,  device = "pdf", width = 6, height = 4, dpi = 300, useDingbats = F)
ggsave("Results/PRelativeMortalityPlot.png", plot = RelativeMortalityPlot, device = "png", width = 6, height = 4, dpi = 300)



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
  mutate(MortalityRatio = Mortality_M/Mortality_F,
         IncidenceRatio = Incidence_M/Incidence_F,
         PrevalenceRatio = Prevalence_M/Prevalence_F,
         MortalityPDRatio = MortalityPD_M/MortalityPD_F,
         AgeRangeNumeric = as.numeric(AgeGroup))


CombinedDataWide <- pivot_wider(CombinedData %>% droplevels() %>%
                                  select(Year, Age, Sex,
                                         Number, Death, Mortality, 
                                         PDnew, Incidence,
                                         PrevalenceRaw, Prevalence,
                                         DeathPD, MortalityPD),
                                names_from = Sex,
                                values_from = c(Number, Death, Mortality,PDnew, Incidence,
                                                PrevalenceRaw, Prevalence,
                                                DeathPD,MortalityPD)) %>% data.frame() %>%
  mutate(MortalityRatio = Mortality_M/Mortality_F,
         IncidenceRatio = Incidence_M/Incidence_F,
         PrevalenceRatio = Prevalence_M/Prevalence_F,
         MortalityPDRatio = MortalityPD_M/MortalityPD_F)

#M/F Mortality ratio plots
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
         filter(!is.na(Prevalence_M), Death_M > 5, DeathPD_F > 5, AgeGroup != "55-59"),
       aes(AgeGroup, MortalityRatio, color = Group)) +
  theme_bw() +
  labs(x ="Age group", y = "M/F Mortality ratio", title = "Mortality M/F ratio") +
  theme(panel.grid = element_blank()) +
  #geom_point(position =position_dodge(width = 0.3), aes(group = Group), size = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_se", position =position_dodge(width = 0.3), size = 0.5) +
  scale_color_manual(values = c("darkgrey", "coral4")) +
  geom_hline(yintercept = 1.5, color = "red", linetype = "dashed")




#Incidence ratio plot
StatIncidenceRatio <- lm(IncidenceRatio~AgeRangeNumeric + Year, data = CombinedDataFiveYearWide %>%
                           filter(PDnew_F > 5, PDnew_M > 5, Year > 2004, Year < 2017)) %>% summary

StatIncidenceRatioDF <- data.frame(x = 4, y = 3.5,
                                   label = paste0("Coef = ", round(StatIncidenceRatio$coefficients[2,1], digits = 3),
                                                  ", p = ",
                                                  signif(StatIncidenceRatio$coefficients[2,4], digits = 2)))

IncidenceRatioPlot <- ggplot(CombinedDataFiveYearWide %>%
                               filter(PDnew_F > 5, PDnew_M > 5, Year > 2004, Year < 2017),
                             aes(AgeGroup, IncidenceRatio)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Male/Female incidence ratio", title = "PDnew_F > 5 & PDnew_M > 5") + 
  geom_boxplot(outlier.shape = NA, fill = MoviePalettes$AmericanBeauty[3]) +
  # scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
  #                               RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  # geom_point(aes(color = as.character(Year)),
  #            position = position_dodge2(width = 0.5))
  geom_jitter(width = 0.2, height = 0, size = 1, color = MoviePalettes$AmericanBeauty[1]) +
  geom_hline(yintercept = 1.5, color = "red") +
  geom_text(data = StatIncidenceRatioDF, aes(x, y, label = label))

ggsave("Results/IncidenceRatioPlot.pdf", plot = IncidenceRatioPlot,  device = "pdf", width = 5, height = 5, dpi = 300, useDingbats = F)
ggsave("Results/IncidenceRatioPlot.png", device = "png", width = 5, height = 5, dpi = 300)


ggplot(CombinedDataWide %>%
         filter(PDnew_F > 3, PDnew_M > 3, Year > 2004, Year < 2017),
       aes(Age, IncidenceRatio)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Male/Female incidence ratio", title = "") + 
  #geom_boxplot(outlier.shape = NA, fill = "grey") +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
                                RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  geom_point(aes(color = as.character(Year))) +
  geom_smooth(method = "lm", formula = )

lm(IncidenceRatio~Age + Year, data = CombinedDataWide %>%
     filter(PDnew_F > 3, PDnew_M > 3, Year > 2004, Year < 2017)) %>% summary

glm(IncidenceRatio~Age + Year, family = "poisson", data = CombinedDataWide %>% 
     filter(PDnew_F > 3, PDnew_M > 3, Year > 2004, Year < 2017)) %>% summary

glm(IncidenceRatio~as.numeric(AgeGroup) + Year, family = "poisson", data = CombinedDataFiveYearWide %>% 
      filter(PDnew_F > 3, PDnew_M > 3, Year > 2004, Year < 2017)) %>% summary

#Incidence ratio plot
StatPrevalenceRatio <- lm(PrevalenceRatio~AgeRangeNumeric + Year, data = CombinedDataFiveYearWide %>%
                            filter(PrevalenceRaw_F > 5, PrevalenceRaw_M > 5, Year > 2004, Year < 2017)) %>% summary

StatPrevalenceRatioDF <- data.frame(x = 5, y = 3.5,
                                    label = paste0("Coef = ", round(StatPrevalenceRatio$coefficients[2,1], digits = 3),
                                                   ", p = ",
                                                   signif(StatPrevalenceRatio$coefficients[2,4], digits = 2)))

PrevalenceRatioPlot <- ggplot(CombinedDataFiveYearWide %>%
                                filter(PrevalenceRaw_F > 5, PrevalenceRaw_M > 5, Year > 2004, Year < 2017),
                              aes(AgeGroup, PrevalenceRatio)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50", linetype = "dashed"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Male/Female prevalence ratio", title = "PrevalenceRaw_F > 5 & PrevalenceRaw_M > 5") + 
  geom_boxplot(outlier.shape = NA, fill = MoviePalettes$AmericanBeauty[3]) +
  # scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(7, name = "Purples")[-1]),
  #                               RColorBrewer::brewer.pal(9, name = "Oranges")[-1]), name = "Year") +
  # geom_point(aes(color = as.character(Year)),
  #            position = position_dodge2(width = 0.5))
  geom_jitter(width = 0.2, height = 0, size = 1, color = MoviePalettes$AmericanBeauty[1]) +
  geom_hline(yintercept = 1.5, color = "red") +
  geom_text(data = StatPrevalenceRatioDF, aes(x, y, label = label))

ggsave("Results/PrevalenceRatioPlot.pdf", plot = PrevalenceRatioPlot, device = "pdf", width = 5, height = 5, dpi = 300, useDingbats = F)
ggsave("Results/PrevalenceRatioPlot.png", plot = PrevalenceRatioPlot, device = "png", width = 5, height = 5, dpi = 300)


#Calculate odds ratio for mortality M vs F in each age in PD vs General
temp2 <- CombinedDataFiveYears2 %>% filter(Year > 2004, Year < 2017,
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
                 OddsRatio = paste0(round(Output$measure[2,1], digits = 2), " (", round(Output$measure[2,2], digits = 2), "-",  round(Output$measure[2,3], digits = 2),")"),
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
        axis.text.x = element_text(angle = 45, hjust = 1),
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

ggsave("Results/OddsRatioPlot2.pdf", plot = OddsRatioPlot,  device = "pdf", width = 6, height = 4, dpi = 300, useDingbats = F)
ggsave("Results/OddsRatioPlot2.png", plot = OddsRatioPlot, device = "png", width = 6, height = 4, dpi = 300)
