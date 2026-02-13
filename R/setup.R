#### Load packages and functions -----------------------------------------------

rm(list = ls())

## packages
library(tidyverse)
library(cmdstanr)
library(bayesplot)
library(plotly)
library(gridExtra)

## functions
source("./R/utils.R")

#### variables -----------------------------------------------------------------
## model name to load stan model:
modnL    <- c("zinb_Dmat_oH", "zinb_Dmat_oH", "pois_Dmat_oH_NSpec")

RUN      <- T # if TRUE model is run, if FALSE model is read in
PREDICT  <- T # if TRUE predictions are run, if FALSE predictions are read in

LANG     <- "EN" # Language in German "GER" or English "EN"

## radius of landscape shares in m
radL     <- c(500, 750, 1000)

## minimum trapping duration per site in h (exclude sites with less effort)
DUR      <- 30

## number of simulations extracted for plotting confidence
SIM      <- 100

## species groups used for modelling: c("Migration", "RedList_DE", )
# groupL   <- c("Migration", "Habitat", "Diet", "Habitat.Density", "Primary.Lifestyle", "RedList_DE")
groupL   <- c("Migration", "Habitat", "Habitat.Density", "Primary.Lifestyle", "RedList_DE")

## plot settings
bs       <- 14 # base size
theme_set(theme_light(base_size = bs))

## Species list for single species model (ordered by abundance)
SpecL <- c("Blaumeise", "Zilpzalp", "Kohlmeise", "Teichrohrsänger", 
                "Rotkehlchen", "Buchfink", "Fitis", "Feldsperling", "Mönchsgrasmücke", 
                "Goldammer", "Rohrammer", "Sumpfrohrsänger", "Heckenbraunelle")

## create folder for saved files
dir.create("./graphs", showWarnings = FALSE, recursive = TRUE)
dir.create("./graphs/prefig", showWarnings = FALSE, recursive = TRUE)
dir.create("./output", showWarnings = FALSE, recursive = TRUE)
dir.create("./saved models", showWarnings = FALSE, recursive = TRUE)

#### load data -----------------------------------------------------------------
## data for individual density per species and species group
dat       <- read.csv("./data/dat_mod_decade.csv",  fileEncoding = "latin1")
## data for species diversity
dat.s     <- read.csv("./data/dat_mod2_decade.csv", fileEncoding = "latin1")
## data with net specifications
dat.n     <- read.csv("./data/dat_net_decade.csv", fileEncoding = "latin1")
## site parameters for filtering DUR
dat.site  <- read.csv("./data/dat_site.csv",        fileEncoding = "latin1")
## species groups
dat.spec  <- read.csv("./data/spec_info.csv",       fileEncoding = "latin1") 
dat.spec2 <- read.csv("./data/AVONET1.csv",         fileEncoding = "latin1") 

dat.spec <- left_join(dat.spec, select(dat.spec2, -c("Migration", "Habitat")), by = c("species_sci" = "Species1"))

dat.spec <- dat.spec[, c("species", "species_sci", "species_eng", groupL)]

## filter data
dat   <- dat[dat$site_short %in% dat.site$site_short[dat.site$dur_h >= DUR],]
dat.s <- dat.s[dat.s$site_short %in% dat.site$site_short[dat.site$dur_h >= DUR],]

## merge with dat.n
dat <- left_join(dat, dat.n, by = c("site_short", "netNr", "julian_day"))
dat.s <- left_join(dat.s, dat.n, by = c("site_short", "netNr", "julian_day"))

## add scientific and english species names
dat <- left_join(dat, dat.spec, by = "species")

## change habitat open to farmland
dat$Habitat[dat$Habitat == "open"] <- "farmland"

## identify species without a Red List category
dat$RedList_DE[is.na(dat$RedList_DE)] <- "*"

## get trait list
xx <- unique(dat[, c("species_sci", "species", groupL)])

#dat$Diet[dat$Diet == "herbivore"] <- "granivorous"
#dat$Diet[dat$Diet == "carnivorous"] <- "invertebrates"

table(dat$Migration)
table(dat$Habitat)
#table(dat$Diet)
table(dat$Habitat.Density)
table(dat$Primary.Lifestyle)
table(dat$RedList_DE)

## define whether year should be part of the model (<- dat$year) or not (<- 1)
dat$year2 <- 1 
dat.s$year2 <- 1 

## check whether some species need to be removed (because no individuals were trapped)
dat <- dat %>% group_by(species) %>% mutate(Ind_tot = sum(NInd_pd)) %>% ungroup()
dat <- dat[dat$Ind_tot > 0,]
dat <- as.data.frame(dat)

## save data
dat.spec <- unique(dat[,colnames(dat.spec)])

write.csv(dat,   "./output/data.csv", row.names = F, fileEncoding = "latin1")
write.csv(dat.s, "./output/data_spec.csv", row.names = F, fileEncoding = "latin1")
write.csv(dat.spec, "./output/data_Traits.csv", row.names = F, fileEncoding = "latin1")

group <- groupL[1]

## get min and max values for paper methods
dat.site <- dat.site[dat.site$site_short %in% dat$site_short, ]
min(dat.site$dur_h)
max(dat.site$dur_h)
min(dat.site$Lnet_m)
max(dat.site$Lnet_m)
min(dat.site$Ndays)
max(dat.site$Ndays)
min(dat.site$area_ha[dat.site$scheme == "voluntary"])
max(dat.site$area_ha[dat.site$scheme == "voluntary"])
nrow(dat.site[dat.site$scheme == "voluntary",])
min(dat.site$area_ha[dat.site$scheme == "professional"])
max(dat.site$area_ha[dat.site$scheme == "professional"])
nrow(dat.site[dat.site$scheme == "professional",])
sum(dat.site$area_ha)


#### m1) Model individual-density per species ----------------------------------
# note: due to the large dataset, each model runs up to one or two hours. 
# The species diversity model m3) is faster
type <- "m1"
modn <- modnL[1]

for(RAD in radL){
  
  rmarkdown::render(input = "./R/net-based.Rmd", 
                    output_file = paste0("../output/Output_", type, "_", modn, "_", RAD, "m.html"))
  
}

#### m2) Model individual-density per species group ----------------------------
type1 <- "m2"
modn  <- modnL[2]

## loop through different species groups
for(group in groupL){
  type <- paste0(type1, "_", group)
  for(RAD in radL){
    
    rmarkdown::render(input = "./R/net-based.Rmd", 
                      output_file = paste0("../output/Output_", type, "_", modn, "_", RAD, "m.html"))
    
  }
}

#### m3) Model species diversity -----------------------------------------------
type <- "m3"
modn  <- modnL[3]

for(RAD in radL){
  
  rmarkdown::render(input = "./R/net-based_NSpec.Rmd", 
                    output_file = paste0("../output/Output_", type, "_", modn, "_", RAD, "m.html"))
  
}

