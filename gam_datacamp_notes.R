#### intro ####
# ------------------------------------------------------------------------------
library(mgcv)
library(tidyverse)
data("mpg", package = "gamair")
# simple model with smooths on three continuous predictors
mod_city <- gam(city.mpg ~ s(weight) + s(length) + s(price),
                data = mpg, method = "REML")
plot(mod_city, pages = 1)

# add three categoricals
mod_city2 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + 
                     fuel + drive + style,
                 data = mpg, method = "REML")
plot(mod_city2, pages = 1, all.terms = TRUE)

# add category level smooths for different drive types
summary(mpg$drive)
mod_city3 <- gam(city.mpg ~ s(weight, 
                              # including this inside the s() term adds the 
                              # effect there directly
                              by = drive) + s(length, by = drive) + 
                     s(price, by = drive) + drive, 
                 data = mpg, method = "REML")

# Plot the model
plot(mod_city3, pages = 1)
# ------------------------------------------------------------------------------

#### interpreting, visualising GAMs #### 
# ------------------------------------------------------------------------------
# use output from summary() to interpret GAM
# Fit the model
mod_city4 <- 
    gam(city.mpg ~ s(weight) + s(length) + s(price) + s(rpm) + s(width),
        family = gaussian, 
        data = mpg, method = "REML")
# View the summary
summary(mod_city4)
# edf is effective degrees of freedom, essentially is the degree of the 
# polynomial that was fit
# ------------------------------------------------------------------------------
# plotting important for checking model fit. plot.gam() has many options
plot(mod_city4, pages = 1) # show partial effects plots for all predictors on 
                           # one page
plot(mod_city4, select = c(1, 1), pages = 1) # select specific effects
plot(mod_city2, all.terms = TRUE, pages = 1) # all.terms adds categoricals
plot(mod_city4, pages = 1, rug = TRUE) # add the predictor values at bottom, 
                                       # TRUE by default
plot(mod_city4, pages = 1, residuals = TRUE) # add the partial residuals
plot(mod_city4, pages = 1, residuals = TRUE, rug = FALSE) # residuals, no rug
plot(mod_city4, pages = 1, residuals = TRUE, rug = FALSE, 
     pch = 1, cex = 1) # change plotting character and size
plot(mod_city4, shade = TRUE, pages = 1) # use shade for standard error
plot(mod_city4, shade = TRUE, pages = 1, shade.col = "lightblue")
# add uncertainty about the mean to the confidence intervals
plot(mod_city4, pages = 1, seWithMean = TRUE)
plot(mod_city4, pages = 1, seWithMean = TRUE, shift = coef(mod_city4)[1])
# ------------------------------------------------------------------------------
# use gam.check() to diagnose problems with models
# set up the data used on DataCamp
d <- read_csv(
    "./data/dat.csv", 
    col_names = c("y", "x0", "x1", "x2", "x3", "f", "f0", "f1", "f2", "f3") 
    
) 

names(d) <- c("y", "x0", "x1", "x2", "x3", "f", "f0", "f1", "f2", "f3")

mod <- gam(y ~ s(x0, k = 3) + s(x1, k = 3) + s(x2, k = 3) + s(x3, k = 3),
           data = d, method = "REML")
par(mfrow = c(2, 2))
gam.check(mod)
# check for "full convergence"
# Q-Q plot should be straight
# histogram of residuals roughly normal
# response vs. fitted clustered well around x = y line
# smooths with low p-value likely to be a problem
# ------------------------------------------------------------------------------
# concurvity a higher-order analogue of correlation
# concurvity() allows for checking global and pairwise concurvity in the GAM
mod <- gam(hw.mpg ~ s(length) + s(width) + s(height) + s(weight), 
           data = mpg, method = "REML")
concurvity(mod, full = TRUE)  
# default for full is TRUE, will give the worst concurvity value for each smooth
concurvity(mod, full = FALSE) 
# this gives the pairwise concurvities, not an intuitive name for the option!
# ------------------------------------------------------------------------------

#### Interactions ####

# interaction terms -------------------------------------------------------
# can add interactions by putting multiple variables inside s()
# examples in the course use meuse data, recreated locally as no longer 
# available in sp package
meuse <- read_csv("./data/meuse.csv", 
                  col_names = c("x", "y", "cadmium", "copper", "lead",
                                "zinc", "elev", "dist", "om", "ffreq", 
                                "soil", "lime", "landuse", "dist.m")) %>% 
    mutate(ffreq = factor(ffreq)) %>% 
    mutate(soil = factor(soil)) %>% 
    mutate(lime = factor(lime)) %>% 
    mutate(landuse = factor(landuse, 
                            levels = c("Aa", "Ab", "Ag", "Ah", "Am", "B", "Bw", 
                                       "DEN", "Fh", "Fw", "Ga", "SPO", "STA", 
                                       "Tv", "W"))) %>% 
    mutate_if(is.integer, as.double)

# can then build a model of cadmium as a function of x and y coords

mod2d <- gam(cadmium ~ s(x, y), data = meuse, method = "REML")
summary(mod2d)
# NB: edf much higher, takes many more basis functions to build a 2d surface

# can still add smooths for single variables
mod2da <- gam(cadmium ~ s(x, y) + s(elev) + s(dist), 
              data = meuse, method = "REML")
summary(mod2da)


# plotting interactions ---------------------------------------------------
par(mfrow = c(1, 1))
# basic method gives a contour plot
plot(mod2d)

# can get a 3d plot
plot(mod2d, scheme = 1)

# heatmap
plot(mod2d, scheme = 2)

# finer controls via vis.gam()
vis.gam(
    x = mod2d, # the GAM
    view = c("x", "y"), # variables to view jointly
    plot.type = "persp",  # this sort of perspective plot can be for any two 
                          # variables, not just interactions
    se = 2 # add this many standard errors to the plot
)

vis.gam(
    x = mod2d, 
    view = c("x", "y"), 
    plot.type = "contour", 
    too.far = 0.05 # how far outside the data should the plot extrapolate?
                   # scaled 0 to 1
)

vis.gam(
    x = mod2d, 
    view = c("x", "y"), 
    plot.type = "persp",
    theta = 90, # use theta, phi to rotate
    phi = 15, 
    r = 2 # set zoom: low value gives more perspective but distorted
)

vis.gam(
    x = mod2d, 
    view = c("x", "y"), 
    plot.type = "contour", 
    color = "cm", #set main colour palette
    contour.col = "navy" # colour of contour lines
)


# continuous-categorical interactions -------------------------------------

# first create a GAM with separate smooths for each factor level
mod_sep <- gam(copper ~ s(dist, by = landuse) + landuse, 
               data = meuse, 
               method = "REML")

# then use a factor smooth
mod_fs <- gam(copper ~ s(dist, 
                         landuse, # the factor still goes inside s()
                         bs = "fs"), # this argument uses a factor smooth 
              data = meuse, 
              method = "REML")

# Examine the summaries
summary(mod_sep)
summary(mod_fs)

# factor smooths not good for distinguishing between different levels: use them 
# to control for factors that are not the main variable of interest

# now plot both
vis.gam(mod_sep, view = c("dist", "landuse"), plot.type = "persp")
vis.gam(mod_fs, view = c("dist", "landuse"), plot.type = "persp")
# fs model good with categories with few observations, fewer wild swings


# tensor smooths ----------------------------------------------------------

# for variables on different scales having a single sp for s() not sufficient
# a tensor smooth, y = te(x1, x2), has two smoothing parameters
# create these with te()

# can separate interactions with ti(). For example: s(x1) + s(x2) + ti(x1, x2)
# each of those three would have its own smoothing parameters

#### classification GAMs ####

# add family = binomial to gam() to model binary outcome
# outputs will be log-odds, use plogis() to convert to probability
# can use trans = plogis in plot to convert to probability scale
# seWithMean = TRUE will add intercept uncertainty to the plot


# prediction --------------------------------------------------------------

# use predict() as normal to predict using a GAM
# set type = "link" to return values on link scale, "response" on probability 
# scale (if binomial)

# se.fit = TRUE will return a list with fitted values and standard errors
# NB. only use this for binary outcomes with type = "link"

# type = "terms" gives a matrix with contribution of each smooth to each 
# prediction


# other sources -----------------------------------------------------------
# mgcv::gam() works well with broom and caret
?smooth.terms
?family.mgcv
?gam.selection
?gam.models
