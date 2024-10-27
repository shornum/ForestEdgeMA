# Code for t-Test and Figures for ENVS 397 Capstone

# Load Packages
library(ggplot2)
library(ggrepel)
library(ggsignif)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(stats)
library(metafor)
library(ggstatsplot)
library(rstantools)
library(rlang)
library(grid)
library(meta)

# Load in data
carbon <- read.csv("ForestEdgeData/ForestDataCarbon.csv")
structure <- read.csv("ForestEdgeData/ForestDataStructure.csv")

# Make data look a little better
colnames(carbon) <- c("Paper", "Authors", "Year", "Country", "Latitude",
                      "Longitude", "Ecosystem", "Type", "SoilCarbon",
                      "TotalCarbon", "AGB", "BGB", "TotalBiomass")
colnames(structure) <- c("Paper", "Authors", "Year", "Country", "Latitude",
                         "Longitude", "Ecosystem", "Type", "Mortality",
                         "Shannon")

# Create a few sub-tables
carbonEdge <- carbon %>% filter(Type == "Edge")
carbonIn <- carbon %>% filter(Type == "Interior")

structureEdge <- structure %>% filter(Type == "Edge")
structureIn <- structure %>% filter(Type == "Interior")

CTemp <- carbon %>% filter(Ecosystem == "Temperate")
CTrop <- carbon %>% filter(Ecosystem == "Tropical")
CSub <- carbon %>% filter(Ecosystem == "Subtropical")
CBor <- carbon %>% filter(Ecosystem == "Boreal")

STemp <- structure %>% filter(Ecosystem == "Temperate")
STrop <- structure %>% filter(Ecosystem == "Tropical")
SSub <- structure %>% filter(Ecosystem == "Subtropical")
SBor <- structure %>% filter(Ecosystem == "Boreal")

carbon$EcoType <- paste(carbon$Ecosystem, carbon$Type, sep = " ")

# Create Figures
SC <- carbon %>% filter(!is.na(SoilCarbon))
TC <- carbon %>% filter(!is.na(TotalCarbon))
Mort <- structure %>% filter(!is.na(Mortality))
Div <- structure %>% filter(!is.na(Shannon))
AB <- carbon %>% filter(!is.na(AGB))
BB <- carbon %>% filter(!is.na(BGB))

## Soil carbon
figSC <- ggplot(SC, aes(Ecosystem, SoilCarbon, 
                        group = interaction(Ecosystem, Type))) +
  geom_point(aes(color = Type, fill = after_scale(alpha(colour, 0.5))),
             position = position_jitterdodge(dodge.width = 0.9, 0.1),
             size = 3, shape = 21) +
  geom_violin(fill = NA, color = "black", width = 0.6, linewidth = 0.4,
              position = position_dodge(0.9)) +
  geom_point(stat = "summary", size = 3, color = "#8a0f00",
             position = position_dodge(0.9), fun = mean) +
  geom_label_repel(stat = "summary", fun = mean, size = 2.75,
                   aes(label = paste0("hat(mu)*scriptstyle(mean)==", 
                                      round(after_stat(y), 2))),
                   parse = T, 
                   position = position_dodge(preserve = "total", width = 1)) +
  geom_signif(y_position = 75, xmin = 2.8, xmax = 3.2,
              annotation = "*", tip_length = 0) +
  scale_color_brewer(palette = "Accent", direction = -1) +
  theme_minimal(base_size = 12) +
  theme(axis.title = element_text(face = 2),
        legend.position = "bottom",
        axis.text.y.right = element_blank())

figSC <- figSC + labs(x = "Ecosystem", y = "Soil Carbon Content (Mg/ha)") +
  theme(
    text = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  )

figSC <- figSC + annotation_custom(
  textGrob(
    label=paste0("(n = ",10,")"), gp=gpar(fontsize = 9, col = "#656565")
  ),
  xmin="Boreal", xmax = "Boreal", ymin = 10, ymax = 12
) + theme(axis.title.x = element_text(margin=margin(20,0,0,0))) + 
  ylim(20,95) + coord_cartesian(clip = "off") +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",10,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Subtropical", xmax = "Subtropical", ymin = 10, ymax = 12
  ) +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",18,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Temperate", xmax = "Temperate", ymin = 10, ymax = 12
  ) +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",10,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Tropical", xmax = "Tropical", ymin = 10, ymax = 12
  )

figSC <- figSC +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(color = "grey50"),
    panel.grid = element_line(color = "grey"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed")
  )

figSC <- figSC + labs(color = "Forest Status:") + 
  theme(legend.title = element_text(face = "bold")) 

figSC 

### Save Plot
ggsave("SoilCarbon.jpg", plot = last_plot(), width=27.78, height=15.63, 
       units= "cm", dpi=300, bg = "white")

## Mortality
figMort <- ggplot(Mort, aes(Ecosystem, Mortality, 
                            group = interaction(Ecosystem, Type))) +
  geom_point(aes(color = Type, fill = after_scale(alpha(colour, 0.5))),
             position = position_jitterdodge(dodge.width = 0.9, 0.1),
             size = 3, shape = 21) +
  geom_violin(fill = NA, color = "black", width = 0.6, linewidth = 0.4,
              position = position_dodge(0.9)) +
  geom_point(stat = "summary", size = 3, color = "#8a0f00",
             position = position_dodge(0.9), fun = mean) +
  geom_label_repel(stat = "summary", fun = mean, size = 2.75,
                   aes(label = paste0("hat(mu)*scriptstyle(mean)==", 
                                      round(after_stat(y), 2))),
                   parse = T, 
                   position = position_dodge(preserve = "total", width = 1)) +
  geom_signif(y_position = 37, xmin = 2.8, xmax = 3.2,
              annotation = "*", tip_length = 0) +
  scale_color_brewer(palette = "Accent", direction = -1) +
  theme_minimal(base_size = 12) +
  theme(axis.title = element_text(face = 2),
        legend.position = "bottom",
        axis.text.y.right = element_blank())

figMort <- figMort + labs(x = "Ecosystem", y = "Annual Tree Mortality (%)") +
  theme(
    text = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  )

figMort <- figMort + annotation_custom(
  textGrob(
    label=paste0("(n = ",15,")"), gp=gpar(fontsize = 9, col = "#656565")
  ),
  xmin="Boreal", xmax = "Boreal", ymin = -7, ymax = -5
) + theme(axis.title.x = element_text(margin=margin(20,0,0,0))) + 
  ylim(0,50) + coord_cartesian(clip = "off") +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",16,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Subtropical", xmax = "Subtropical", ymin = -7, ymax = -5
  ) +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",14,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Temperate", xmax = "Temperate", ymin = -7, ymax = -5
  ) +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",15,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Tropical", xmax = "Tropical", ymin = -7, ymax = -5
  )

figMort <- figMort +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(color = "grey50"),
    panel.grid = element_line(color = "grey"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed")
  )

figMort <- figMort + labs(color = "Forest Status:") + 
  theme(legend.title = element_text(face = "bold")) 

figMort

### Save Plot
ggsave("Mortality.jpg", plot = last_plot(), width=27.78, height=15.63, 
       units= "cm", dpi=300, bg = "white")

## Diversity
figDiv <- ggplot(Div, aes(Ecosystem, Shannon, 
                          group = interaction(Ecosystem, Type))) +
  geom_point(aes(color = Type, fill = after_scale(alpha(colour, 0.5))),
             position = position_jitterdodge(dodge.width = 0.9, 0.1),
             size = 3, shape = 21) +
  geom_violin(fill = NA, color = "black", width = 0.6, linewidth = 0.4,
              position = position_dodge(0.9)) +
  geom_point(stat = "summary", size = 3, color = "#8a0f00",
             position = position_dodge(0.9), fun = mean) +
  geom_label_repel(stat = "summary", fun = mean, size = 2.75,
                   aes(label = paste0("hat(mu)*scriptstyle(mean)==", 
                                      round(after_stat(y), 2))),
                   parse = T, 
                   position = position_dodge(preserve = "total", width = 1)) +
  scale_color_brewer(palette = "Accent", direction = -1) +
  theme_minimal(base_size = 12) +
  theme(axis.title = element_text(face = 2),
        legend.position = "bottom",
        axis.text.y.right = element_blank())

figDiv <- figDiv + labs(x = "Ecosystem", y = "Shannon Diversity Index") +
  theme(
    text = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  )

figDiv <- figDiv + annotation_custom(
  textGrob(
    label=paste0("(n = ",9,")"), gp=gpar(fontsize = 9, col = "#656565")
  ),
  xmin="Boreal", xmax = "Boreal", ymin = -1, ymax = 0
) + theme(axis.title.x = element_text(margin=margin(20,0,0,0))) + 
  ylim(0,4) + coord_cartesian(clip = "off") +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",9,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Subtropical", xmax = "Subtropical", ymin = -1, ymax = 0
  ) +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",8,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Temperate", xmax = "Temperate", ymin = -1, ymax = 0
  ) +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",9,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Tropical", xmax = "Tropical", ymin = -1, ymax = 0
  )

figDiv <- figDiv +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(color = "grey50"),
    panel.grid = element_line(color = "grey"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed")
  )

figDiv <- figDiv + labs(color = "Forest Status:") + 
  theme(legend.title = element_text(face = "bold")) 

figDiv

### Save Plot
ggsave("Diversity.jpg", plot = last_plot(), width=27.78, height=15.63, 
       units= "cm", dpi=300, bg = "white")

## AGB
figAB <- ggplot(AB, aes(Ecosystem, AGB, group = interaction(Ecosystem, Type))) +
  geom_point(aes(color = Type, fill = after_scale(alpha(colour, 0.5))),
             position = position_jitterdodge(dodge.width = 0.9, 0.1),
             size = 3, shape = 21) +
  geom_violin(fill = NA, color = "black", width = 0.6, linewidth = 0.4,
              position = position_dodge(0.9)) +
  geom_point(stat = "summary", size = 3, color = "#8a0f00",
             position = position_dodge(0.9), fun = mean) +
  geom_label_repel(stat = "summary", fun = mean, size = 2.75,
                   aes(label = paste0("hat(mu)*scriptstyle(mean)==", 
                                      round(after_stat(y), 2))),
                   parse = T, 
                   position = position_dodge(preserve = "total", width = 1),
                   max.overlaps = 11) +
  scale_color_brewer(palette = "Accent", direction = -1) +
  theme_minimal(base_size = 12) +
  theme(axis.title = element_text(face = 2),
        legend.position = "bottom",
        axis.text.y.right = element_blank()) +
  geom_signif(y_position = 230, xmin = 2.8, xmax = 3.2,
              annotation = "*", tip_length = 0) +
  geom_signif(y_position = 1100, xmin = 0.8, xmax = 1.2,
              annotation = "*", tip_length = 0) 

figAB <- figAB + labs(x = "Ecosystem", y = "Aboveground Biomass (Mg/ha)") +
  theme(
    text = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  )

figAB <- figAB + annotation_custom(
  textGrob(
    label=paste0("(n = ",6,")"), gp=gpar(fontsize = 9, col = "#656565")
  ),
  xmin="Boreal", xmax = "Boreal", ymin = -150, ymax = -148
) + theme(axis.title.x = element_text(margin=margin(20,0,0,0))) + 
  ylim(0,1200) + coord_cartesian(clip = "off") +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",8,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Subtropical", xmax = "Subtropical",  ymin = -150, ymax = -148
  ) +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",6,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Temperate", xmax = "Temperate",  ymin = -150, ymax = -148
  ) +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",8,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Tropical", xmax = "Tropical",  ymin = -150, ymax = -148
  )

figAB <- figAB +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(color = "grey50"),
    panel.grid = element_line(color = "grey"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed")
  )

figAB <- figAB + labs(color = "Forest Status:") + 
  theme(legend.title = element_text(face = "bold")) 

figAB

### Save Plot
ggsave("AGBiomass.jpg", plot = last_plot(), width=29, height=15.63, 
       units= "cm", dpi=300, bg = "white")

## BGB
figBB <- ggplot(BB, aes(Ecosystem, BGB, group = interaction(Ecosystem, Type))) +
  geom_point(aes(color = Type, fill = after_scale(alpha(colour, 0.5))),
             position = position_jitterdodge(dodge.width = 0.9, 0.1),
             size = 3, shape = 21) +
  geom_violin(fill = NA, color = "black", width = 0.6, linewidth = 0.4,
              position = position_dodge(0.9)) +
  geom_point(stat = "summary", size = 3, color = "#8a0f00",
             position = position_dodge(0.9), fun = mean) +
  geom_label_repel(stat = "summary", fun = mean, size = 2.75,
                   aes(label = paste0("hat(mu)*scriptstyle(mean)==", 
                                      round(after_stat(y), 2))),
                   parse = T, 
                   position = position_dodge(preserve = "total", width = 1),
                   max.overlaps = 11) +
  scale_color_brewer(palette = "Accent", direction = -1) +
  theme_minimal(base_size = 12) +
  theme(axis.title = element_text(face = 2),
        legend.position = "bottom",
        axis.text.y.right = element_blank()) +
  geom_signif(y_position = 120, xmin = 3.8, xmax = 4.2,
              annotation = "*", tip_length = 0) +
  geom_signif(y_position = 230, xmin = 0.8, xmax = 1.2,
              annotation = "*", tip_length = 0) 

figBB <- figBB + labs(x = "Ecosystem", y = "Belowground Biomass (Mg/ha)") +
  theme(
    text = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  )

figBB <- figBB + annotation_custom(
  textGrob(
    label=paste0("(n = ",6,")"), gp=gpar(fontsize = 9, col = "#656565")
  ),
  xmin="Boreal", xmax = "Boreal", ymin = -35, ymax = -33
) + theme(axis.title.x = element_text(margin=margin(20,0,0,0))) + 
  ylim(0,250) + coord_cartesian(clip = "off") +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",8,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Subtropical", xmax = "Subtropical",  ymin = -35, ymax = -33
  ) +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",6,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Temperate", xmax = "Temperate",  ymin = -35, ymax = -33
  ) +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",8,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Tropical", xmax = "Tropical",  ymin = -35, ymax = -33
  )

figBB <- figBB +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(color = "grey50"),
    panel.grid = element_line(color = "grey"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed")
  )

figBB <- figBB + labs(color = "Forest Status:") + 
  theme(legend.title = element_text(face = "bold")) 

figBB

### Save Plot
ggsave("BGBiomass.jpg", plot = last_plot(), width=29, height=16, 
       units= "cm", dpi=300, bg = "white")

## Total Carbon
figTC <- ggplot(TC, aes(Ecosystem, TotalCarbon, 
                        group = interaction(Ecosystem, Type))) +
  geom_point(aes(color = Type, fill = after_scale(alpha(colour, 0.5))),
             position = position_jitterdodge(dodge.width = 0.9, 0.1),
             size = 3, shape = 21) +
  geom_violin(fill = NA, color = "black", width = 0.6, linewidth = 0.4,
              position = position_dodge(0.9)) +
  geom_point(stat = "summary", size = 3, color = "#8a0f00",
             position = position_dodge(0.9), fun = mean) +
  geom_label_repel(stat = "summary", fun = mean, size = 2.75,
                   aes(label = paste0("hat(mu)*scriptstyle(mean)==", 
                                      round(after_stat(y), 2))),
                   parse = T, 
                   position = position_dodge(preserve = "total", width = 1),
                   max.overlaps = 11) +
  scale_color_brewer(palette = "Accent", direction = -1) +
  theme_minimal(base_size = 12) +
  theme(axis.title = element_text(face = 2),
        legend.position = "bottom",
        axis.text.y.right = element_blank()) +
  geom_signif(y_position = 275, xmin = 3.8, xmax = 4.2,
              annotation = "*", tip_length = 0) +
  geom_signif(y_position = 125, xmin = 2.8, xmax = 3.2,
              annotation = "*", tip_length = 0) +
  geom_signif(y_position = 645, xmin = 0.8, xmax = 1.2,
              annotation = "*", tip_length = 0) 

figTC <- figTC + labs(x = "Ecosystem", y = "Total Carbon Content (Mg/ha)") +
  theme(
    text = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  )

figTC <- figTC + annotation_custom(
  textGrob(
    label=paste0("(n = ",6,")"), gp=gpar(fontsize = 9, col = "#656565")
  ),
  xmin="Boreal", xmax = "Boreal", ymin = -82, ymax = -80
) + theme(axis.title.x = element_text(margin=margin(20,0,0,0))) + 
  ylim(0,675) + coord_cartesian(clip = "off") +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",8,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Subtropical", xmax = "Subtropical",  ymin = -82, ymax = -80
  ) +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",6,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Temperate", xmax = "Temperate",  ymin = -82, ymax = -80
  ) +
  annotation_custom(
    textGrob(
      label=paste0("(n = ",8,")"), gp=gpar(fontsize = 9, col = "#656565")
    ),
    xmin="Tropical", xmax = "Tropical",  ymin = -82, ymax = -80
  )

figTC <- figTC +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(color = "grey50"),
    panel.grid = element_line(color = "grey"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed")
  )

figTC <- figTC + labs(color = "Forest Status:") + 
  theme(legend.title = element_text(face = "bold")) 

figTC

### Save Plot
ggsave("TotalCarbon.jpg", plot = last_plot(), width=29, height=15.63, 
       units= "cm", dpi=300, bg = "white")

# t-Test
## Soil Carbon
carbon %>% group_by(Type) %>% get_summary_stats(SoilCarbon, type = "mean_sd")

### Overall
SCSig <- carbon %>% t_test(SoilCarbon ~ Type, detailed = T) %>% 
  add_significance()

### Boreal
SCSigBor <- CBor %>% t_test(SoilCarbon ~ Type, detailed = T) %>% 
  add_significance()

### Temperate
SCSigTemp <- CTemp %>% t_test(SoilCarbon ~ Type, detailed = T) %>% 
  add_significance()

### Tropical
SCSigTrop <- CTrop %>% t_test(SoilCarbon ~ Type, detailed = T) %>% 
  add_significance()

### Subtropical
SCSigSub <- CSub %>% t_test(SoilCarbon ~ Type, detailed = T) %>% 
  add_significance()

## Total Carbon
carbon %>% group_by(Type) %>% get_summary_stats(TotalCarbon, type = "mean_sd")

### Overall
TCSig <- carbon %>% t_test(TotalCarbon ~ Type, detailed = T) %>% 
  add_significance()

### Boreal
TCSigBor <- CBor %>% t_test(TotalCarbon ~ Type, detailed = T) %>% 
  add_significance()

### Temperate
TCSigTemp <- CTemp %>% t_test(TotalCarbon ~ Type, detailed = T) %>% 
  add_significance()

### Tropical
TCSigTrop <- CTrop %>% t_test(TotalCarbon ~ Type, detailed = T) %>% 
  add_significance()

### Subtropical
TCSigSub <- CSub %>% t_test(TotalCarbon ~ Type, detailed = T) %>% 
  add_significance()

## Aboveground Biomass
carbon %>% group_by(Type) %>% get_summary_stats(AGB, type = "mean_sd")

### Overall
AGBSig <- carbon %>% t_test(AGB ~ Type, detailed = T) %>% 
  add_significance()

### Boreal
AGBSigBor <- CBor %>% t_test(AGB ~ Type, detailed = T) %>% 
  add_significance()

### Temperate
AGBSigTemp <- CTemp %>% t_test(AGB ~ Type, detailed = T) %>% 
  add_significance()

### Tropical
AGBSigTrop <- CTrop %>% t_test(AGB ~ Type, detailed = T) %>% 
  add_significance()

### Subtropical
AGBSigSub <- CSub %>% t_test(AGB ~ Type, detailed = T) %>% 
  add_significance()

## Belowground Biomass
carbon %>% group_by(Type) %>% get_summary_stats(BGB, type = "mean_sd")

### Overall
BGBSig <- carbon %>% t_test(BGB ~ Type, detailed = T) %>% 
  add_significance()

### Boreal
BGBSigBor <- CBor %>% t_test(BGB ~ Type, detailed = T) %>% 
  add_significance()

### Temperate
BGBSigTemp <- CTemp %>% t_test(BGB ~ Type, detailed = T) %>% 
  add_significance()

### Tropical
BGBSigTrop <- CTrop %>% t_test(BGB ~ Type, detailed = T) %>% 
  add_significance()

### Subtropical
BGBSigSub <- CSub %>% t_test(BGB ~ Type, detailed = T) %>% 
  add_significance()

## Mortality
structure %>% group_by(Type) %>% get_summary_stats(Mortality, type = "mean_sd")

### Overall
MortSig <- structure %>% t_test(Mortality ~ Type, detailed = T) %>% 
  add_significance()

### Boreal
MortSigBor <- SBor %>% t_test(Mortality ~ Type, detailed = T) %>% 
  add_significance()

### Temperate
MortSigTemp <- STemp %>% t_test(Mortality ~ Type, detailed = T) %>% 
  add_significance()

### Tropical
MortSigTrop <- STrop %>% t_test(Mortality ~ Type, detailed = T) %>% 
  add_significance()

### Subtropical
MortSigSub <- SSub %>% t_test(Mortality ~ Type, detailed = T) %>% 
  add_significance()

## Diversity
structure %>% group_by(Type) %>% get_summary_stats(Shannon, type = "mean_sd")

### Overall
DivSig <- structure %>% t_test(Shannon ~ Type, detailed = T) %>% 
  add_significance()

### Boreal
DivSigBor <- SBor %>% t_test(Shannon ~ Type, detailed = T) %>% 
  add_significance()

### Temperate
DivSigTemp <- STemp %>% t_test(Shannon ~ Type, detailed = T) %>% 
  add_significance()

### Tropical
DivSigTrop <- STrop %>% t_test(Shannon ~ Type, detailed = T) %>% 
  add_significance()

### Subtropical
DivSigSub <- SSub %>% t_test(Shannon ~ Type, detailed = T) %>% 
  add_significance()
