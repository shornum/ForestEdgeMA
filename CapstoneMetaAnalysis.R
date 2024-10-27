# Code for Meta-Analysis ENVS 397 Capstone

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

# Load in data and clean
SC_MA <- read.csv("MAData/MADataSC.csv")
TC_MA <- read.csv("MAData/MADataTC.csv")
Mort_MA <- read.csv("MAData/MADataMort.csv")
Mort_MA$Author <- iconv(Mort_MA$Author, from="UTF-8", to="LATIN1")
Div_MA <- read.csv("MAData/MADataDiv.csv")
Div_MA$Author <- iconv(Div_MA$Author, from="UTF-8", to="LATIN1")
AB_MA <- read.csv("MAData/MADataAGB.csv")
BB_MA <- read.csv("MAData/MADataBGB.csv")

# Run the Meta-Analysis
## Soil Carbon
mgen_SC <- metagen(TE = TE,
                   seTE = seTE,
                   studlab = Author,
                   data = SC_MA,
                   sm = "SMD",
                   fixed = F,
                   random = T,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "Soil Carbon Data",
                   subgroup = Ecosystem)

## Total Carbon
mgen_TC <- metagen(TE = TE,
                   seTE = seTE,
                   studlab = Author,
                   data = TC_MA,
                   sm = "SMD",
                   fixed = F,
                   random = T,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "Total Carbon Data",
                   subgroup = Ecosystem)

## Mortality
mgen_mort <- metagen(TE = TE,
                     seTE = seTE,
                     studlab = Author,
                     data = Mort_MA,
                     sm = "SMD",
                     fixed = F,
                     random = T,
                     method.tau = "REML",
                     method.random.ci = "HK",
                     title = "Mortality Data",
                     subgroup = Ecosystem)

## Diversity
mgen_div <- metagen(TE = TE,
                    seTE = seTE,
                    studlab = Author,
                    data = Div_MA,
                    sm = "SMD",
                    fixed = F,
                    random = T,
                    method.tau = "REML",
                    method.random.ci = "HK",
                    title = "Diversity Data",
                    subgroup = Ecosystem)

## Aboveground Biomass
mgen_AB <- metagen(TE = TE,
                   seTE = seTE,
                   studlab = Author,
                   data = AB_MA,
                   sm = "SMD",
                   fixed = F,
                   random = T,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "Aboveground Biomass Data",
                   subgroup = Ecosystem)

## Belowground Biomass
mgen_BB <- metagen(TE = TE,
                   seTE = seTE,
                   studlab = Author,
                   data = BB_MA,
                   sm = "SMD",
                   fixed = F,
                   random = T,
                   method.tau = "REML",
                   method.random.ci = "HK",
                   title = "Belowground Biomass Data",
                   subgroup = Ecosystem)

# View Summary of Results
summary(mgen_SC)
summary(mgen_TC)
summary(mgen_mort)
summary(mgen_div)
summary(mgen_AB)
summary(mgen_BB)

# Create Forest Plots and Save as PNG
## Soil Carbon
png(file = "SCForest.png", width = 27, height = 30, res = 300, units = "cm")
meta::forest(mgen_SC,
             sortvar = TE,
             prediction = F,
             print.tau2 = F,
             leftlabs = c("Author", "g", "SE"),
             hetstat = F,
             test.subgroup = F)
dev.off()

## Total Carbon
png(file = "TCForest.png", width = 27, height = 30, res = 300, units = "cm")
meta::forest(mgen_TC,
             sortvar = TE,
             prediction = F,
             print.tau2 = F,
             leftlabs = c("Author", "g", "SE"),
             hetstat = F,
             test.subgroup = F)
dev.off()

## Mortality
png(file = "MortForest.png", width = 27, height = 30, res = 300, units = "cm")
meta::forest(mgen_mort,
             sortvar = TE,
             prediction = F,
             print.tau2 = F,
             leftlabs = c("Author", "g", "SE"),
             hetstat = F,
             test.subgroup = F)
dev.off()

## Diversity
png(file = "DivForest.png", width = 27, height = 35, res = 300, units = "cm")
meta::forest(mgen_div,
             sortvar = TE,
             prediction = F,
             print.tau2 = F,
             leftlabs = c("Author", "g", "SE"),
             hetstat = F,
             test.subgroup = F)
dev.off()

## Aboveground Biomass
png(file = "AGBForest.png", width = 27, height = 30, res = 300, units = "cm")
meta::forest(mgen_AB,
             sortvar = TE,
             prediction = F,
             print.tau2 = F,
             leftlabs = c("Author", "g", "SE"),
             hetstat = F,
             test.subgroup = F)
dev.off()

## Belowground Biomass
png(file = "BGBForest.png", width = 27, height = 30, res = 300, units = "cm")
meta::forest(mgen_BB,
             sortvar = TE,
             prediction = F,
             print.tau2 = F,
             leftlabs = c("Author", "g", "SE"),
             hetstat = F,
             test.subgroup = F)
dev.off()
