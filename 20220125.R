# Milica -- Blue Carbon 
# 2022 January
# code copied from uni_glm.R with some local revisions.

library(tidyverse)
library(emmeans) # To compare the means (i.e., pairwise comparisons)
library(statmod) # For qresiduals()
library(broom)

# Packages for figure building
library(ggpubr)
library(patchwork)
library(lemon)
library(magick) # Save to pdf then to png
library(showtext) # I want to use Noto Sans

# Use a google font because its pretty
if(!any(str_detect(font_families(), "notosans"))) {
  font_add_google("Noto Sans","notosans")
}
# Set the default theme of the ggplots
theme_pubr(base_size = 10, base_family = "notosans") |> theme_set()
showtext_auto()
Sys.setlocale("LC_TIME", "en_US.UTF-8") # This is to set the server time locate to en_US.UTF-8


# Declare functions
 
zvalue = function(x) {
  # Calculate z-scores (i.e., center and divide by the standard deviation)
  scale(x, center = TRUE, scale = TRUE)[, 1]
  }

# Load data and examine
data_folder = rprojroot::find_rstudio_root_file("Data")
fname = dir(data_folder, full = TRUE, pattern = "carbon*.*csv")
uni = read_csv(fname) |> print()

uni = uni |> distinct() # There are many many duplicated lines!

# Rescale pop, coast_dist, river_dist  by 100

uni = uni |> mutate(across(c(pop, coast_dist, river_dist), \(x) {x / 100}))

# What am I looking at?
ggplot(uni)+
  geom_point(aes(x=landuse, y=Corg_Total, color=Country),
             position = position_dodge(width = 0.3))

ggplot(uni)+
  geom_histogram(aes(x = Corg_Total)) +
  facet_grid(cols = vars(landuse),
             rows = vars(Country))

# Simple GLM ###################################################################
GLM1 = glm(Corg_Total ~., data = uni, family = Gamma(link = "log"))
glance(GLM1)
tidy(GLM1)

uni2 = uni2 |>
  mutate(qres = qresiduals(GLM1),
         pred = predict.glm(GLM1, type = "link"))

# Not a good sign, since the randomized quantile residuals are not 
# uniformly distributed around 0
ggplot(uni2) + 
  geom_point(aes(x = pred, y = qres)) +
  geom_hline(yintercept = 0, linetype = "dashed")


xlab = "Value of variable"
uni |> 
  pivot_longer(cols = c(bathymetry, coast_dist, river_dist, pop)) |> print() |> 
  ggplot() +
  geom_point(aes(x = value, y = Corg_Total, color = landuse),
             alpha = 0.2) +
  scale_x_continuous(xlab) +
  facet_wrap(vars(name), scales = "free", ncol = 4)

uni |> 
  pivot_longer(cols = c(bathymetry, coast_dist, river_dist, pop)) |> print() |> 
  ggplot() +
  geom_point(aes(x = Latitude, y = Longitude, size =),
             alpha = 0.2) +
  scale_x_continuous(xlab) +
  facet_wrap(vars(name, landuse), scales = "free", ncol = 4)
################################################################################
################################################################################
################################################################################
library(lme4)
# The variables must be on the same scale, otherwise the correlation of fixed effects might not be estimated well
# Rescale the numeric variables by calculating their z-scores.

# Crossed random effects
# rem1 = lmer(Corg_Total ~ coast_dist + river_dist + pop + bathymetry  + landuse + (1|Latitude) + (1|Longitude),
#            data=uni |> mutate(across(c(coast_dist, river_dist, pop, bathymetry), zvalue)), 
#            REML = T)
# ml1 = lmer(Corg_Total ~ coast_dist + river_dist + pop + bathymetry  + landuse + (1|Latitude) + (1|Longitude),
#            data=uni |> mutate(across(c(coast_dist, river_dist, pop, bathymetry), zvalue)), 
#            REML = F)
xtabs(~ Latitude + Longitude, data = uni) |> as_tibble()

# Nested random effects
rem1 = lmer(Corg_Total ~ coast_dist + river_dist + pop + bathymetry  + landuse + (1|Latitude/Longitude),
           data=uni |> mutate(across(c(coast_dist, river_dist, pop, bathymetry), zvalue)),
           REML = T)
ml1 = lmer(Corg_Total ~ coast_dist + river_dist + pop + bathymetry  + landuse + (1|Latitude/Longitude),
           data=uni |> mutate(across(c(coast_dist, river_dist, pop, bathymetry), zvalue)),
           REML = F)
summary(rem1)
coef(rem1)

p2 = ranef(rem1, condVar = TRUE) |> as_tibble() |> 
  ggplot() + 
  geom_qq(aes(sample = condval)) + 
  geom_qq_line(aes(sample = condval)) + 
  coord_flip() +
  facet_wrap(vars(grpvar), scales = "free", as.table = T)

p1 = lattice::qqmath(ranef(rem1, condVar = TRUE))
p1 = ggpubr::ggarrange(plotlist = p1)

p1/p2

lme4::ranef(rem1) |> as_tibble()

# predict() includes the effects of the random effects.
uni = uni |> 
  mutate(pred = predict(rem1),
         resid = rstudent(rem1))

ggplot(uni) + 
  geom_point(aes(x = pred, y = resid)) +
  geom_hline(yintercept = 0, linetype = "dashed")


uni |> pivot_longer(cols = c(pop, coast_dist, river_dist, bathymetry)) |> 
  ggplot() + 
  geom_point(aes(x = value, y = pred)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(vars(name), scales = "free")

pr1 = profile(ml1)
lattice::xyplot(pr1, absVal = F)
confint(pr1)
lattice::splom(pr1)

################################################################################


