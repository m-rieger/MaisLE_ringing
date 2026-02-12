# Data preparation -------------------------------------------------------------

# sequence generation between min and max
seq_range <- function(x, length.out = 100) {
  seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = length.out)
}

# Diagnostics ------------------------------------------------------------------

# get prop. of zeros in data
prop_zero   <- function(z) sum(z == 0) / length(z)

# center and scale data
std <- function(x, ref) (x - mean(ref)) / sd(ref)

# rescale data
rescale <- function(var, var_c) var_c*sd(var) + mean(var)

# extract posterior predictions
get_sims <- function(d = NULL, fit = fitmat, margin = 2, sims = 50) {
  if (margin == 2) {
    simm <- c(fit[, round(runif(sims, 1, ncol(fit)))])
  } else {
    simm <- c(t(fit[round(runif(sims, 1, ncol(fit))), ]))
  }
  sim <- data.frame(
    x = rep(d, sims),
    sim = simm,
    draw = rep(1:sims, each = nrow(newdat))
  )
  return(sim)
}

# extract draws
get_draws <- function(var = NULL, d = modsims) {
  class(d) <- "data.frame"
  dd <- as.matrix(d[, startsWith(colnames(d), var)])
  return(dd)
}

# MCMC diagnostics
diagMCMC <- function(drws) {
  require(gridExtra)
  require(bayesplot)

  for (i in 1:(ncol(drws) - 3)) {
    d <- drws[, c(i, (ncol(drws) - 2):ncol(drws))]

    g1 <- mcmc_trace(d) + ylab(NULL) + theme(legend.position = "none")
    g2 <- mcmc_acf_bar(d) + ylab(NULL)
    g3 <- mcmc_rank_overlay(d) + theme(legend.position = "none")
    g4 <- mcmc_dens_overlay(d) + theme(legend.position = "none")

    grid.arrange(g1, g2, g3, g4)
  }
}

# Plots ------------------------------------------------------------------------

# plot response per predictor
plot1 <- function(x = NULL, y = "dens_1000hm2", color = "year", c.label = year,
                  data = dat.mod, xlab = NULL, ylab = dens, wrap = "species", basesize = bs){
  
  data$x     <- data[,x]
  data$y     <- data[,y]
  data$color <- as.factor(data[,color])
  data$wrap  <- data[,wrap]

  g <- ggplot(data) +
    # plot points
    geom_point(aes(x = x, y = y, color = color), pch = 1, size = 2) +
    # define color range of color-variable
    scale_color_viridis_d(c.label, direction = -1, end = 0.9, option = "inferno", alpha = 0.5) +
    # wrap per variable
    facet_wrap(~wrap, scales = "free") +
    # axis labels
    xlab(xlab) +
    ylab(ylab) +
    theme_light(base_size = basesize)
  
  return(g)
}

# plot nSpec response per predictor
plot1s <- function(x = NULL, y = "nSpec", color = "year", c.label = year,
                  data = dat.mod, xlab = NULL, ylab = nS, basesize = bs){
  
  data$x     <- data[,x]
  data$y     <- data[,y]
  data$color <- as.factor(data[,color])

  g <- ggplot(data) +
    # plot points
    geom_point(aes(x = x, y = y, color = color), pch = 1, size = 2) +
    # define color range of color-variable
    scale_color_viridis_d(c.label, direction = -1, end = 0.9, option = "inferno", alpha = 0.5) +
    # axis labels
    xlab(xlab) +
    ylab(ylab) +
    theme_light(base_size = basesize)
  
  return(g)
}

# plot predictor per predictor
plot2 <- function(x = NULL, y = NULL, color = "year", c.label = year,
                  data = dat.mod[dat.mod$species == Spec.list[1],], 
                  xlab = NULL, ylab = NULL, basesize = bs){
  
  data$x     <- data[,x]
  data$y     <- data[,y]
  data$color <- as.factor(data[,color])
  
  g <- ggplot(data) +
    # plot points (jittered)
    geom_point(aes(x = x, y = y, color = color), pch = 1, size = 2, position = "jitter") +
    # define color range of color-variable
    scale_color_viridis_d(c.label, direction = -1, end = 0.9, option = "inferno", alpha = 0.5) +
    # axis labels
    xlab(xlab) +
    ylab(ylab) +
    theme_light(base_size = basesize)
  
  return(g)
}

# plot nSpec predictor per predictor
plot2s <- function(x = NULL, y = NULL, color = "year", c.label = year,
                  data = dat.mod, 
                  xlab = NULL, ylab = NULL, basesize = bs){
  
  data$x     <- data[,x]
  data$y     <- data[,y]
  data$color <- as.factor(data[,color])
  
  g <- ggplot(data) +
    # plot points (jittered)
    geom_point(aes(x = x, y = y, color = color), pch = 1, size = 2, position = "jitter") +
    # define color range of color-variable
    scale_color_viridis_d(c.label, direction = -1, end = 0.9, option = "inferno", alpha = 0.5) +
    # axis labels
    xlab(xlab) +
    ylab(ylab) +
    theme_light(base_size = basesize)
  
  return(g)
}

# plot predictions per predictor
plot3 <- function(preds = pp, sims = ff, x = NULL, y = "fit", 
                  xlab = NULL, ylab = dens, basesize = bs, color = "species", c.label = spec,
                  plot.wrap = TRUE, wrap = "species",
                  plot.sim = TRUE, nsim = gsim, # number of random draws for confidence lines
                  plot.raw = TRUE, data = dat.mod, yraw = "dens_1000hm2"){
  data$x      <- data[,x]
  preds$x     <- preds[,x]
  data$yraw   <- data[,yraw]
  preds$y     <- preds[,y]
  data$color  <- data[,color]
  preds$color <- preds[,color]
  data$wrap   <- data[,wrap]
  preds$wrap  <- preds[,wrap]
  
 
  # base plot 
  g <- ggplot(data = data, aes(x = x, y = yraw)) +
    theme_light(base_size = basesize)
  
  # add raw data in grey (if raw = TRUE)
  if(plot.raw) {
    g <- g +
      geom_point(data = data, aes(x = x, y = yraw),
                 color = "grey40", alpha = 0.3, pch = 1)    
  }

  # add random draws for confidence lines (if CIline = TRUE)
  if(plot.sim) {
    for(i in 1:length(nsim)) {
      preds$lsim <- sims[,i]
      g <- g + geom_line(data = preds, aes(x = x, y = lsim, color = wrap), alpha = 0.1)
    }
  }
  # add predictions
  g <- g +
    
    geom_line(data = preds, aes(x = x, y = y, color = wrap), lwd = 1.2) +
    
    scale_color_viridis_d(c.label, direction = -1, end = 0.9) +
    scale_fill_viridis_d(c.label, direction = -1, end = 0.9)
  
  # wrap (if plot.wrap = TRUE)
  if(plot.wrap) {
    g <- g + facet_wrap(~wrap, scales = "free_y")
  }
    
  g <- g +
    xlab(xlab) +
    ylab(ylab) +
    ylim(0, NA)
  
  return(g)
  
}

# plot predictions per predictor
plot3s <- function(preds = pp, sims = ff, x = NULL, y = "fit", 
                  xlab = NULL, ylab = nS, basesize = bs, 
                  plot.sim = TRUE, nsim = gsim, # number of random draws for confidence lines
                  plot.raw = TRUE, data = dat.mod, yraw = "nSpec"){
  data$x      <- data[,x]
  preds$x     <- preds[,x]
  data$yraw   <- data[,yraw]
  preds$y     <- preds[,y]

  
  # base plot 
  g <- ggplot(data = data, aes(x = x, y = yraw)) +
    theme_light(base_size = basesize)
  
  # add raw data in grey (if raw = TRUE)
  if(plot.raw) {
    g <- g +
      geom_point(data = data, aes(x = x, y = yraw),
                 color = "grey40", alpha = 0.3, pch = 1)    
  }
  
  # add random draws for confidence lines (if CIline = TRUE)
  if(plot.sim) {
    for(i in 1:length(nsim)) {
      preds$lsim <- sims[,i]
      g <- g + geom_line(data = preds, aes(x = x, y = lsim), alpha = 0.1)
    }
  }
  # add predictions
  g <- g +
    
    geom_line(data = preds, aes(x = x, y = y), lwd = 1.2) +
    
    xlab(xlab) +
    ylab(ylab) +
    ylim(0, NA)
  
  return(g)
  
}
