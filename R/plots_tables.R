#### plots and tables for MaisLE_net -------------------------------------------
#------------------------------------------------------------------------------#

rm(list = ls())
gc()

#### 0) load packages and data -------------------------------------------------
#------------------------------------------------------------------------------#

## packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggdist) # stat_slab
library(ggspatial) # annotation_scale
library(sf)
library(patchwork)

## variables
nsamp <- 1000
bs <- 14 # base size for plots

mod.list <- c("species", "Migration", "Habitat", "Habitat.Density", 
              "Primary.Lifestyle", "RedList_DE", "Diversity")

## data
ce.500 <- list()
ce.750 <- list()
ce.1000 <- list()

for(i in mod.list){
  if(i == "species") {
    ce.500[[i]] <- read.csv("./output/Coefficient_estimates_m1_zinb_Dmat_oH_500m.csv", 
                          encoding = "latin1")
    ce.750[[i]] <- read.csv("./output/Coefficient_estimates_m1_zinb_Dmat_oH_750m.csv", 
                            encoding = "latin1")
    ce.1000[[i]] <- read.csv("./output/Coefficient_estimates_m1_zinb_Dmat_oH_1000m.csv", 
                            encoding = "latin1")
  }
  
  if(i == "Diversity") {
    ce.500[[i]] <- read.csv("./output/Coefficient_estimates_m3_pois_Dmat_oH_NSpec_500m.csv", 
                                 encoding = "latin1")
    ce.750[[i]] <- read.csv("./output/Coefficient_estimates_m3_pois_Dmat_oH_NSpec_750m.csv", 
                            encoding = "latin1")
    ce.1000[[i]] <- read.csv("./output/Coefficient_estimates_m3_pois_Dmat_oH_NSpec_1000m.csv", 
                            encoding = "latin1")
  }
 
  if(!i %in% c("Diversity", "species")) {
    ce.500[[i]] <- read.csv(paste0("./output/Coefficient_estimates_m2_", i, "_zinb_Dmat_oH_500m.csv"), 
                                 encoding = "latin1")
    ce.750[[i]] <- read.csv(paste0("./output/Coefficient_estimates_m2_", i, "_zinb_Dmat_oH_750m.csv"), 
                            encoding = "latin1")
    ce.1000[[i]] <- read.csv(paste0("./output/Coefficient_estimates_m2_", i, "_zinb_Dmat_oH_1000m.csv"), 
                            encoding = "latin1")
  }
}

dat.mod <- read.csv("./output/data.csv", fileEncoding = "latin1")
dat.mod2 <- read.csv("./output/data_spec.csv", fileEncoding = "latin1")
dat.art <- read.csv("./output/data_art.csv", fileEncoding = "latin1")
shp.Ger    <- read_sf("./data/shp/Germany_FS.shp")

pp.mig <- read.csv("./output/Predictions_m2_Migration_zinb_Dmat_oH_500m.csv", encoding = "latin1")
ff.mig <- read.csv("./output/Predictions_sim_m2_Migration_zinb_Dmat_oH_500m.csv", encoding = "latin1")
pp.sD <- read.csv("./output/Predictions_m3_pois_Dmat_oH_NSpec_500m.csv", encoding = "latin1")
ff.sD <- read.csv("./output/Predictions_sim_m3_pois_Dmat_oH_NSpec_500m.csv", encoding = "latin1")


## species list
Spec.order <- c("Blaumeise", "Zilpzalp", "Kohlmeise", "Teichrohrsänger", "Rotkehlchen", 
                "Buchfink", "Fitis", "Feldsperling", "Mönchsgrasmücke", "Goldammer", 
                "Rohrammer", "Sumpfrohrsänger", "Heckenbraunelle")

Spec.list <- sort(Spec.order)

## create folder for saved files (not publication ready)
if(!dir.exists("./graphs/prefig")) dir.create("./graphs/prefig")

#### plot map ------------------------------------------------------------------
## reduce data to site_short
dat.mod <- dat.mod[dat.mod$netNr != 100,]
tmp <- dat.mod %>%
  group_by(site_short, year) %>%
  summarize(lon = mean(lon),
            lat = mean(lat),
            .groups = "drop")
shp <- st_as_sf(tmp, coords = c("lon", "lat"))
st_crs(shp) <- 4326

col.df <- data.frame(year = c(2016:2024),
                     site.size = c(0.4, 1, 1.6, 2.2, 2.8, 3.4, 4.0, 4.5, 5.2),
                     x = 16, 
                     y = rev(c(47.5, 48, 48.5, 49, 49.5, 50, 50.5, 51, 51.5)))
col.df$site.size <- col.df$site.size*2.5

shp <- left_join(shp, col.df, by = "year")

shp <- shp %>% 
  arrange(desc(year))

## number of sites per year
nSites <- st_drop_geometry(shp) %>% group_by(year) %>%
  summarize(nSites = n(), .groups = "drop")

## merge with legend year
col.df <- left_join(col.df, nSites, by = "year")
col.df$year.site <- paste0(col.df$year, " (", col.df$nSites, ")")

g <- ggplot() +
  geom_sf(data = shp.Ger, fill = "white", color = "#888888", lwd = 1) +
  #geom_sf(data = shp, color = shp$site.col, size = shp$site.size) +
  geom_sf(data = shp, aes(fill = as.character(year)), size = 3*shp$site.size, color = "#888888", pch = 21) +
  
  geom_sf_text(data = shp[shp$site_short == "BW-Z3-22",],
               aes(label = "Rottenburg"), size = 14,
               nudge_y = 0.3, nudge_x = 0.5) +
  
  geom_sf_text(data = shp[shp$site_short == "BB-C2-23",],
               aes(label = "Seelow"), size = 14,
               nudge_y = 0.3, nudge_x = 0.6) +
  geom_sf_text(data = shp[shp$site_short == "NI-N2-24",],
               aes(label = "Cloppenburg"), size = 14,
               nudge_y = -0.3, nudge_x = 0) +
  
  
  ## legend
  geom_point(aes(x = 19, y = 50), pch = 1, color = "white") + # only dummy point to increase margin
  
  geom_point(data = col.df, 
             aes(x = x, y = y, fill = as.character(year)),
             size = 3*col.df$site.size, pch = 21, color = "grey55") + 
  geom_text(data = col.df, aes(x = x + 0.6, y = y, label = year.site), 
            vjust = 0.5, hjust = 0, size = 14) +
  geom_text(aes(x = unique(col.df$x)-0.1, y = 52), label = "year (# sites)", 
            vjust = 0.5, hjust = 0, size = 16) + # legend title
  
  scale_fill_viridis_d(end = 0.9, name = "year", direction = 1, option = "inferno") +
  scale_color_viridis_d(end = 0.9, name = "year", direction = 1, option = "inferno") +
  
  ## Maßstab
  annotation_scale(height = unit(1, "cm"), text_cex = 3, width_hint = 0.4, 
                   style = "ticks", line_width = 4, text_pad = unit(0.5, "cm"),
                   pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),) +
  
  theme_void(base_size = bs) +
  theme(legend.position = "none")

png("./graphs/Map.png", height = 1600, width = 2000)
g
dev.off()

#### plot effort ---------------------------------------------------------------
## reduce data to site_short
tmp <- dat.mod[dat.mod$netNr != 100 & dat.mod$species == "Blaumeise",]
## merge with legend year
tmp <- left_join(tmp, nSites, by = "year")
tmp$year.site <- paste0(tmp$year, " (", tmp$nSites, ")")

g <- ggplot(tmp) +
  
  geom_col(aes(y = effort/1000, x = julian_day, fill = year.site), 
           # fill = "#365c8d", 
           width = 0.9) +
  
  ## layout
  # ylab("Fangaufwand [1000 hm²]") +
  # xlab("Dekade im Jahr (10-Tages-Intervall)") +
  ylab("effort [1000 hm²]") +
  xlab("decade of the year (10-day interval)") +
  
  scale_fill_viridis_d(end = 0.9, name = "year (# sites)", direction = 1, option = "inferno") +
  
  theme_light(base_size = 4*bs)  +
  theme(legend.position = "right")

png("./graphs/Effort.png", height = 1200, width = 2000)
g
dev.off()

#### plot phenology
# plot predictions per predictor
plot3 <- function(preds = pp, sims = ff, x = NULL, y = "fit", 
                  xlab = NULL, ylab = dens, basesize = bs, color = "species", c.label = spec,
                  plot.sim = TRUE, nsim = gsim){
  preds$x     <- preds[,x]
  preds$y     <- preds[,y]
  preds$color <- preds[,color]
  
  # base plot 
  g <- ggplot(data = preds, aes(x = x, y = y)) +
    theme_light(base_size = basesize)
  
  # add random draws for confidence lines (if plot.sim = TRUE)
  if(plot.sim) {
    for(i in 1:nsim) {
      preds$lsim <- sims[,i]
      g <- g + geom_line(data = preds, aes(x = x, y = lsim, color = color), alpha = 0.05, lwd = 1)
    }
  }
  # add predictions
  g <- g +
    
    geom_line(data = preds, aes(x = x, y = y, color = color), lwd = 3) +
    
    scale_color_viridis_d(c.label, direction = -1, end = 0.9) +
    scale_fill_viridis_d(c.label, direction = -1, end = 0.9)
  
  g <- g +
    xlab(xlab) +
    ylab(ylab) +
    ylim(0, NA)
  
  if(spec == "species diversity") g <- g + theme(legend.position = "none")
  if(spec == "migration") g <- g + theme(legend.position = c(0.3, 0.78), 
                                         legend.box.background = element_rect(colour = "grey30"))
  
  return(g)
  
}

gsim <- 100
spec <- "migration"
g1 <- plot3(preds = pp.mig[pp.mig$pred == "jday",], sims = ff.mig[ff.mig$pred == "jday",], 
      xlab = "decade of the year [10-day interval]", x = "julian_day",
      ylab = "density [ind. per 1000 hm²]", basesize = bs*3)
spec <- "species diversity"
g2 <- plot3(preds = pp.sD[pp.sD$pred == "jday",], sims = ff.sD[ff.sD$pred == "jday",], 
      xlab = "decade of the year [10-day interval]", x = "julian_day",
      ylab = "species diversity [number of species]", basesize = bs*3)

Pheno <- g1 + g2 + plot_layout(nrow = 1)
png(paste0("./graphs/Results_Pheno.png"), height = 1200, width = 2000)
Pheno
dev.off()


#### plot coefficient estimate per radius --------------------------------------
## prepare data
df <- data.frame()
for(m in mod.list){
  
  if(m == "species") list2 <- Spec.list
  if(m == "Diversity") list2 <- "Diversity"
  if(!m %in% c("Diversity", "species")) list2 <- sort(unique(dat.mod[,m]))
  
  
  for(i in 1:length(list2)){
    
    tmp <- data.frame(y2016 = ce.500[[m]][,paste0("alpha.", i, ".1.")],
                      dist = ce.500[[m]][,paste0("beta.", i, ".1.")],
                      maize = ce.500[[m]][,paste0("beta.", i, ".2.")],
                      woodS = ce.500[[m]][,paste0("beta.", i, ".3.")],
                      weed = ce.500[[m]][,paste0("beta.", i, ".4.")],
                      jday1 = ce.500[[m]][,paste0("gamma.", i, ".1.")],
                      jday2 = ce.500[[m]][,paste0("gamma.", i, ".2.")],
                      # zi = ce.500[,paste0("theta.", i, ".")],
                      chain = ce.500[[m]]$.chain,
                      species = list2[i],
                      model = m,
                      radius = 0.5)
    
    df <- rbind(df, tmp)
    
    tmp <- data.frame(y2016 = ce.750[[m]][,paste0("alpha.", i, ".1.")],
                      dist = ce.750[[m]][,paste0("beta.", i, ".1.")],
                      maize = ce.750[[m]][,paste0("beta.", i, ".2.")],
                      woodS = ce.750[[m]][,paste0("beta.", i, ".3.")],
                      weed = ce.750[[m]][,paste0("beta.", i, ".4.")],
                      jday1 = ce.750[[m]][,paste0("gamma.", i, ".1.")],
                      jday2 = ce.750[[m]][,paste0("gamma.", i, ".2.")],
                      # zi = ce.750[,paste0("theta.", i, ".")],
                      chain = ce.750[[m]]$.chain,
                      species = list2[i],
                      model = m,
                      radius = 0.75)
    
    df <- rbind(df, tmp)
    
    tmp <- data.frame(y2016 = ce.1000[[m]][,paste0("alpha.", i, ".1.")],
                      dist = ce.1000[[m]][,paste0("beta.", i, ".1.")],
                      maize = ce.1000[[m]][,paste0("beta.", i, ".2.")],
                      woodS = ce.1000[[m]][,paste0("beta.", i, ".3.")],
                      weed = ce.1000[[m]][,paste0("beta.", i, ".4.")],
                      jday1 = ce.1000[[m]][,paste0("gamma.", i, ".1.")],
                      jday2 = ce.1000[[m]][,paste0("gamma.", i, ".2.")],
                      # zi = ce.1000[,paste0("theta.", i, ".")],
                      chain = ce.1000[[m]]$.chain,
                      species = list2[i],
                      model = m,
                      radius = 1)
    
    df <- rbind(df, tmp)
  }
}

df$radius <- as.factor(df$radius)

## get BayesP
cols <- c("y2016", 
          # "y2017", "y2018", "y2019", "y2020", "y2021", "y2022", "y2023", "y2024",
          "dist", "maize", "woodS", "weed", "jday1", "jday2")
df.sum <- df %>% group_by(species, radius, model) %>%
  summarize(across(all_of(cols),
    ~ (sum(.x > 0) + 0.5 * sum(.x == 0)) / n()), .groups = "drop")

# long format
df.sum <- pivot_longer(df.sum, names_to = "coef", values_to = "BayesP", cols = all_of(cols))
df.sum$robust <- "no"
df.sum$robust[df.sum$BayesP <= 0.025 | df.sum$BayesP >= 0.975] <- "yes"

## plot function
plot.ce <- function(df, ce, ce.name, facet, df.s, pos, neg, min){
  if(is.null(facet)) facet <- TRUE
  df$ce <- df[,ce]
  df.s <- df.s[df.s$model %in% unique(df$model) &df.s$coef == ce,]
  df.s$star <- ifelse(df.s$BayesP > 0.975 | df.s$BayesP < 0.025, "*", NA)
  df.s$star2 <- ifelse(df.s$BayesP > 0.95 | df.s$BayesP < 0.05, "*", NA)
  g <- list()
  
  g[[1]] <- ggplot(df) +
    geom_vline(xintercept = 0, lty = "dashed", lwd = 3) +
    stat_slab(aes(x = ce, y = species, fill = radius, color = radius),
              alpha = 0.7, height = 2 #, slab_color = "grey30"
    ) +

    geom_text(aes(y = species, x = ifelse(BayesP > 0.95, pos-0.06, neg-0.06), 
                  label = star2, color = radius), alpha = 0.8, size = 35,
              position = position_nudge(y = 0.3),
              data = df.s[df.s$radius == "0.5",], fontface = "plain") +
    geom_text(aes(y = species, x = ifelse(BayesP > 0.95, pos, neg), 
                  label = star2, color = radius), alpha = 0.8, size = 35,
              position = position_nudge(y = 0.3),
              data = df.s[df.s$radius == "0.75",], fontface = "plain") +
    geom_text(aes(y = species, x = ifelse(BayesP > 0.95, pos+0.06, neg+0.06), 
                  label = star2, color = radius), alpha = 0.8, size = 35,
              position = position_nudge(y = 0.3),
              data = df.s[df.s$radius == "1",], fontface = "plain") +

    geom_text(aes(y = species, x = ifelse(BayesP > 0.975, pos-0.06, neg-0.06), 
                  label = star, color = radius), alpha = 1, size = 40,
              position = position_nudge(y = 0.3),
              data = df.s[df.s$radius == "0.5",], fontface = "bold") +
    geom_text(aes(y = species, x = ifelse(BayesP > 0.975, pos, neg), 
                  label = star, color = radius), alpha = 1, size = 40,
              position = position_nudge(y = 0.3),
              data = df.s[df.s$radius == "0.75",], fontface = "bold") +
    geom_text(aes(y = species, x = ifelse(BayesP > 0.975, pos+0.06, neg+0.06), 
                  label = star, color = radius), alpha = 1, size = 40,
              position = position_nudge(y = 0.3),
              data = df.s[df.s$radius == "1",], fontface = "bold") +
    # geom_text(aes(y = species, x = neg, color = radius, group = radius),
    #           alpha = 1, label = "*", size = 10,
    #           position = position_dodge(width = 0.8, orientation = "x"),
    #           data = df.s[df.s$BayesP < 0.025,]) +
    scale_fill_viridis_d("landscape radius [km]", begin = 0.1, end = 0.9, direction = -1) +
    scale_color_viridis_d("landscape radius [km]", begin = 0.1, end = 0.9, direction = -1) +
    labs(x = paste0("coefficient estimate '", ce.name, "'"), y = "species") +
    # xlim(quantile(unlist(df[,cols]), probs = 0.025), quantile(unlist(df[,cols]), probs = 0.975)) +
    xlim(min, min+2.2) + # -2.5, 0.75
    # scale_y_reverse(limits = rev(levels(df$species))) +
    theme_minimal(base_size = 8*bs) +
    # theme(legend.position = "none",
    #       axis.title = element_blank())
    theme(legend.position = "none")
  
  if(facet){ 
    g[[1]] <- g[[1]] + 
      facet_wrap(~model, ncol = 1, scales = "free_y", strip.position = "right")
  }
  
  if(facet & ce != "woodS"){ 
    g[[1]] <- g[[1]] + 
      theme(strip.background = element_blank(), strip.text = element_blank())
  }
  
  if(ce != "weed"){
    g[[1]] <- g[[1]] + 
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank())
    
  }
  
  ## plot legend
  g[[2]] <- ggplot() + 
    geom_ribbon(aes(x = c(-30:0)/10, ymin = c(-30:0)/10, ymax = c(-30:0)/-10), fill = "orange", alpha = 0.5) +
    geom_ribbon(aes(x = c(30:0)/10, ymin = c(30:0)/10, ymax = c(30:0)/-10), fill = "#669933", alpha = 0.5) +
    xlim(min, min+2.2) + # -2.5, 0.75
    scale_y_continuous(breaks = c(-3, 0, 3), labels = c(" ", "Mönchsgrasmücke", " ")) +
    ylab("species") +
    theme_classic(base_size = 8*bs)  + 
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.line.x.bottom = element_line(color = NA),
          axis.line.y.left = element_line(color = NA))
    
  if(ce != "weed"){
    g[[2]] <- g[[2]] + 
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank())
    
  }
  
  return(g)
  
}

## rename species to appear as last species
df$species[df$species == "Diversity"] <- "AAA_Diversity"
df.sum$species[df.sum$species == "Diversity"] <- "AAA_Diversity"

## reverse levels
df$species <- factor(df$species, levels = rev(sort(unique(df$species))))
df.sum$species <- factor(df.sum$species, levels = rev(sort(unique(df.sum$species))))

## exclude some trait groups
# df <- df[df$model != "Diet",]
# df.sum <- df.sum[df.sum$model != "Diet",]



g1 <- plot.ce(df = df[df$model %in% c("Diversity", "species"),], df.s = df.sum,
                    ce = "weed", ce.name = "Verunkrautung",
                    pos = 0.6, neg = -0.4, min = -1.5, facet = FALSE)

g2 <- plot.ce(df = df[df$model %in% c("Diversity", "species"),], df.s = df.sum,
                    ce = "dist", ce.name = "Abstand zur Feldgrenze",
                    pos = 0.5, neg = -1, min = -1.3, facet = FALSE)


g3 <- plot.ce(df = df[df$model %in% c("Diversity", "species"),], df.s = df.sum,
                    ce = "maize", ce.name = "Maisanteil im Umfeld",
                    pos = 0.45, neg = -1.5, min = -1.7, facet = FALSE)

g4 <- plot.ce(df = df[df$model %in% c("Diversity", "species"),], df.s = df.sum,
                    ce = "woodS", ce.name = "Gehölzanteil im Umfeld",
                    pos = 1, neg = -0.75, min = -1, facet = FALSE)


SpecDiv <- g1[[1]] + g2[[1]] + g3[[1]] + g4[[1]] + plot_layout(nrow = 1)
png(paste0("./graphs/prefig/Radius_SpecDiv.png"), height = 3200, width = 6000)
SpecDiv
dev.off()

SpecDiv <- g1[[2]] + g2[[2]] + g3[[2]] + g4[[2]] + plot_layout(nrow = 1)
png(paste0("./graphs/prefig/Radius_SpecDiv_leg.png"), height = 500, width = 6000)
SpecDiv
dev.off()

g1 <- plot.ce(df = df[!df$model %in% c("Diversity", "species"),], df.s = df.sum,
              ce = "weed", ce.name = "Verunkrautung", facet = TRUE,
              pos = 0.5, neg = -1.25, min = -1.5)

g2 <- plot.ce(df = df[!df$model %in% c("Diversity", "species"),], df.s = df.sum,
                      ce = "dist", ce.name = "Abstand zur Feldgrenze", facet = TRUE,
                      pos = 0.75, neg = -1, min = -1.2)


g3 <- plot.ce(df = df[!df$model %in% c("Diversity", "species"),], df.s = df.sum,
              ce = "maize", ce.name = "Maisanteil im Umfeld", facet = TRUE,
              pos = 0.5, neg = -1.25, min = -1.5)

g4 <- plot.ce(df = df[!df$model %in% c("Diversity", "species"),], df.s = df.sum,
              ce = "woodS", ce.name = "Gehölzanteil im Umfeld", facet = TRUE,
              pos = 0.75, neg = -1, min = -1.2)


Trait <- g1[[1]] + g2[[1]] + g3[[1]] + g4[[1]] + plot_layout(nrow = 1)
png(paste0("./graphs/prefig/Radius_Trait.png"), height = 3200, width = 6000)
Trait
dev.off()

Trait <- g1[[2]] + g2[[2]] + g3[[2]] + g4[[2]] + plot_layout(nrow = 1)
png(paste0("./graphs/prefig/Radius_Trait_leg.png"), height = 500, width = 6000)
Trait
dev.off()


plot.ce(df = df, ce = "jday1", ce.name = "julian day (linear)")

plot.ce(df = df, ce = "jday2", ce.name = "julian day (quadratic)")

plot.ce(df = df, ce = "y2016", ce.name = "year 2016")

# plot.ce(df = df, ce = "y2017", ce.name = "year 2017")
# plot.ce(df = df, ce = "y2018", ce.name = "year 2018")
# plot.ce(df = df, ce = "y2019", ce.name = "year 2019")
# plot.ce(df = df, ce = "y2020", ce.name = "year 2020")
# plot.ce(df = df, ce = "y2021", ce.name = "year 2021")
# plot.ce(df = df, ce = "y2022", ce.name = "year 2022")
# plot.ce(df = df, ce = "y2023", ce.name = "year 2023")
# plot.ce(df = df, ce = "y2024", ce.name = "year 2024")
# plot.ce(df = df, ce = "zi", ce.name = "zero inflation")

## plot BayesP
ggplot(df.sum[df.sum$coef %in% c("dist", "maize", "woodS", "weed", "jday1", "jday2"),]) + 
  geom_point(aes(x = BayesP, y = species, color = radius, pch = robust), 
             size = 3, alpha = 0.7, stroke = 1.5) +
  facet_wrap(~coef) +
  scale_color_viridis_d("landscape radius [km]", begin = 0.1, end = 0.9, direction = -1) +
  scale_shape_manual(values = c("no" = 1, "yes" = 16)) +
  labs(x = "Bayes p-value", y = "species") +
  theme_minimal(base_size = 20) +
  theme(legend.position = "top")

# ggplot(df.sum[df.sum$coef %in% c("y2016", "y2017", "y2018", "y2019", "y2020", "y2021", "y2022", "y2023", "y2024"),]) + 
#   geom_point(aes(x = BayesP, y = species, color = radius, pch = robust), 
#              size = 3, alpha = 0.7, stroke = 1.5) +
#   facet_wrap(~coef) +
#   scale_color_viridis_d("landscape radius [km]", begin = 0.1, end = 0.9, direction = -1) +
#   scale_shape_manual(values = c("no" = 1, "yes" = 16)) +
#   labs(x = "Bayes p-value", y = "species") +
#   theme_minimal(base_size = 20) +
#   theme(legend.position = "top")

#### plot predictors per model -------------------------------------------------
ggplot(dat.mod[dat.mod$species == "Blaumeise",]) +
  geom_histogram(aes(x = distance_m), alpha = 0.5, binwidth = 10) +
  labs(x = "distance to edge [m]") +
  theme_minimal(base_size = 3*bs) +
  theme(legend.position = "top")
#ggsave("./graphs/prefig/hist_dist.png", height = 15, width = 20)
ggplot(dat.mod[dat.mod$species == "Blaumeise",]) +
  geom_histogram(aes(x = distance_m), alpha = 0.5, binwidth = 0.1) +
  labs(x = "distance to edge [m]") +
  scale_x_log10() +
  theme_minimal(base_size = 3*bs) +
  theme(legend.position = "top")
#ggsave("./graphs/prefig/hist_dist_log.png", height = 15, width = 20)

ggplot(dat.mod[dat.mod$species == "Blaumeise",]) +
  geom_histogram(aes(x = prop_weed/100), alpha = 0.5, binwidth = 0.05) +
  labs(x = "weed infestation") +
  theme_minimal(base_size = 3*bs) +
  theme(legend.position = "top")
#ggsave("./graphs/prefig/hist_weed.png", height = 15, width = 20)
ggplot(dat.mod[dat.mod$species == "Blaumeise",]) +
  geom_histogram(aes(x = (prop_weed/100) + 0.005), alpha = 0.5, binwidth = 0.2) +
  labs(x = "weed infestation") +
  scale_x_log10() +
  theme_minimal(base_size = 3*bs) +
  theme(legend.position = "top")
#ggsave("./graphs/prefig/hist_weed_log.png", height = 15, width = 20)

ggplot(dat.mod[dat.mod$species == "Blaumeise",]) +
  geom_histogram(aes(x = decade), alpha = 0.5, binwidth = 1) +
  labs(x = "decade") +
  theme_minimal(base_size = 3*bs) +
  theme(legend.position = "top")
#ggsave("./graphs/prefig/hist_decade.png", height = 15, width = 20)

ggplot(dat.mod[dat.mod$species == "Blaumeise",]) +
  geom_histogram(aes(x = woodS_1000m/100, fill = "1", color = "1"), alpha = 0.5, binwidth = 0.05) +
  geom_histogram(aes(x = woodS_750m/100,  fill = "0.75", color = "0.75"), alpha = 0.5, binwidth = 0.05) +
  geom_histogram(aes(x = woodS_500m/100,  fill = "0.5", color = "0.5"), alpha = 0.5, binwidth = 0.05) +
  scale_fill_viridis_d("landscape radius [km]", begin = 0.1, end = 0.9, direction = -1) +
  scale_color_viridis_d("landscape radius [km]", begin = 0.1, end = 0.9, direction = -1) +
  labs(x = "proportion of woods and forests") +
  theme_minimal(base_size = 3*bs) +
  theme(legend.position = "top")
#ggsave("./graphs/prefig/hist_woodS.png", height = 15, width = 20)
ggplot(dat.mod[dat.mod$species == "Blaumeise",]) +
  geom_histogram(aes(x = woodS_1000m/100, fill = "1", color = "1"), alpha = 0.5, binwidth = 0.1) +
  geom_histogram(aes(x = woodS_750m/100,  fill = "0.75", color = "0.75"), alpha = 0.5, binwidth = 0.1) +
  geom_histogram(aes(x = woodS_500m/100,  fill = "0.5", color = "0.5"), alpha = 0.5, binwidth = 0.1) +
  scale_fill_viridis_d("landscape radius [km]", begin = 0.1, end = 0.9, direction = -1) +
  scale_color_viridis_d("landscape radius [km]", begin = 0.1, end = 0.9, direction = -1) +
  labs(x = "proportion of woods and forests") +
  scale_x_log10() +
  theme_minimal(base_size = 3*bs) +
  theme(legend.position = "top")
#ggsave("./graphs/prefig/hist_woodS_log.png", height = 15, width = 20)

ggplot(dat.mod[dat.mod$species == "Blaumeise",]) +
  geom_histogram(aes(x = maize_1000m/100, fill = "1", color = "1"), alpha = 0.5, binwidth = 0.05) +
  geom_histogram(aes(x = maize_750m/100,  fill = "0.75", color = "0.75"), alpha = 0.5, binwidth = 0.05) +
  geom_histogram(aes(x = maize_500m/100,  fill = "0.5", color = "0.5"), alpha = 0.5, binwidth = 0.05) +
  scale_fill_viridis_d("landscape radius [km]", begin = 0.1, end = 0.9, direction = -1) +
  scale_color_viridis_d("landscape radius [km]", begin = 0.1, end = 0.9, direction = -1) +
  labs(x = "proportion of maize") +
  theme_minimal(base_size = 3*bs) +
  theme(legend.position = "top")
#ggsave("./graphs/prefig/hist_maize.png", height = 15, width = 20)
ggplot(dat.mod[dat.mod$species == "Blaumeise",]) +
  geom_histogram(aes(x = maize_1000m/100, fill = "1", color = "1"), alpha = 0.5, binwidth = 0.1) +
  geom_histogram(aes(x = maize_750m/100,  fill = "0.75", color = "0.75"), alpha = 0.5, binwidth = 0.1) +
  geom_histogram(aes(x = maize_500m/100,  fill = "0.5", color = "0.5"), alpha = 0.5, binwidth = 0.1) +
  scale_fill_viridis_d("landscape radius [km]", begin = 0.1, end = 0.9, direction = -1) +
  scale_color_viridis_d("landscape radius [km]", begin = 0.1, end = 0.9, direction = -1) +
  labs(x = "proportion of maize") +
  scale_x_log10() +
  theme_minimal(base_size = 3*bs) +
  theme(legend.position = "top")
#ggsave("./graphs/prefig/hist_maize_log.png", height = 15, width = 20)


#### plot arthropods -----------------------------------------------------------

plot.art <- function(data, y, title){
  data$y <- data[, y]
  
  g <- ggplot(data) +
    geom_point(aes(x = julian_day, y = y, color = as.factor(year), group = as.factor(year)),
               alpha = 0.5, size = 7, pch = 1, stroke = 4,
               position = position_dodge(width = 0.2)) +
    scale_color_viridis_d("Jahr", begin = 0.1, end = 0.8, direction = -1) +
    xlab("Dekade") +
    ylab("Arthropodenmasse [g]") +
    ggtitle(title) +
    theme_minimal(base_size = 3*bs)  
  
  return(g)
    
  
}

plot.art(data = dat.art[dat.art$species == "Blaumeise",], 
         y = "mass_g_Diptera_0.3mm", title = "Diptera 0-3mm")
ggsave("./graphs/prefig/Art_Dipt1.png", height = 15, width = 20)

plot.art(data = dat.art[dat.art$species == "Blaumeise",], 
         y = "mass_g_Diptera_3.10mm", title = "Diptera 3-10mm")
ggsave("./graphs/prefig/Art_Dipt2.png", height = 15, width = 20)

plot.art(data = dat.art[dat.art$species == "Blaumeise",], 
         y = "mass_g_Diptera_.10mm", title = "Diptera >10mm")
ggsave("./graphs/prefig/Art_Dipt3.png", height = 15, width = 20)

plot.art(data = dat.art[dat.art$species == "Blaumeise",], 
         y = "mass_g_Hemiptera_0.3mm", title = "Hemiptera 0-3mm")
ggsave("./graphs/prefig/Art_Hemi1.png", height = 15, width = 20)

plot.art(data = dat.art[dat.art$species == "Blaumeise",], 
         y = "mass_g_Hemiptera_3.10mm", title = "Hemiptera 3-10mm")
ggsave("./graphs/prefig/Art_Hemi2.png", height = 15, width = 20)

plot.art(data = dat.art[dat.art$species == "Blaumeise",], 
         y = "mass_g_Hemiptera_.10mm", title = "Hemiptera >10mm")
ggsave("./graphs/prefig/Art_Hemi3.png", height = 15, width = 20)

plot.art(data = dat.art[dat.art$species == "Blaumeise",], 
         y = "mass_g_Arachnea_0.3mm", title = "Arachnea 0-3mm")
ggsave("./graphs/prefig/Art_Arach1.png", height = 15, width = 20)

plot.art(data = dat.art[dat.art$species == "Blaumeise",], 
         y = "mass_g_Arachnea_3.10mm", title = "Arachnea 3-10mm")
ggsave("./graphs/prefig/Art_Arach2.png", height = 15, width = 20)

plot.art(data = dat.art[dat.art$species == "Blaumeise",], 
         y = "sum_0_3", title = "alle 0-3mm")
ggsave("./graphs/prefig/Art_All1.png", height = 15, width = 20)

plot.art(data = dat.art[dat.art$species == "Blaumeise",], 
         y = "sum_3_10", title = "alle 3-10mm")
ggsave("./graphs/prefig/Art_All2.png", height = 15, width = 20)

plot.art(data = dat.art[dat.art$species == "Blaumeise",], 
         y = "sum_10", title = "alle >10mm")
ggsave("./graphs/prefig/Art_All3.png", height = 15, width = 20)

ggplot(dat.art[dat.art$species == "Blaumeise",]) +
  geom_boxplot(aes(y = sum_0_3, x = site_short)) +
  facet_wrap(~year, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(dat.art[dat.art$species == "Blaumeise",]) +
  geom_boxplot(aes(y = sum_3_10, x = site_short)) +
  facet_wrap(~year, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(dat.art[dat.art$species == "Blaumeise",]) +
  geom_boxplot(aes(y = sum_10, x = site_short)) +
  facet_wrap(~year, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(dat.art[dat.art$species == "Blaumeise",]) +
  geom_point(aes(x = sum_0_3, y = sum_3_10), alpha = 0.5)
ggplot(dat.art[dat.art$species == "Blaumeise",]) +
  geom_point(aes(x = sum_0_3, y = sum_10), alpha = 0.5)
ggplot(dat.art[dat.art$species == "Blaumeise",]) +
  geom_point(aes(x = sum_3_10, y = sum_10), alpha = 0.5)

