#### Figure 1. Macroalgal carbon assimilation ####
#### Figure 1a. Aquaculture production ####
species <- read.csv("~/PATH/Species.csv")
species$Production <- species$Production/1000 # convert kt to Mt
species$Product <- factor(species$Product, as.character(unique(species$Product)))


require(ggplot2)
mytheme <- theme(panel.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 plot.margin = unit(c(.8, .5, .2, .5),"cm"),
                 axis.line = element_line(),
                 axis.title = element_text(size = 15, hjust = 0),
                 axis.text = element_text(size = 12, colour = "black"),
                 axis.ticks.length = unit(.25, "cm"),
                 axis.ticks = element_line(colour = "black"),
                 legend.key = element_blank(),
                 legend.key.size = unit(.3, "cm"),
                 legend.key.height = unit(.45, "cm"),
                 legend.spacing.x = unit(.1, "cm"),
                 legend.spacing.y = unit(.05, "cm"),
                 legend.background = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.text.align = 0,
                 legend.title.align = 0,
                 legend.title = element_blank())

sp <- ggplot(species, aes(Year, Production, colour = Product)) +
  geom_smooth(method = "loess", span = 0.8, se = F) +
  scale_colour_manual(values = c("#50590d", "#ea949d", "#d52f25", 
                                 "#a06c7e","#81a512", "#bd9b61")) +
  labs(y = "Aquaculture production (Mt)", 
       x = expression("Carbon assimilation (kg C m"^-2*" yr"^-1*")")) +
  scale_y_continuous(breaks = seq(0, 15, by = 5)) +
  scale_x_continuous(breaks = seq(2000, 2018, by = 6)) +
  coord_cartesian(ylim = c(0, 15), expand = F) +
  mytheme +
  theme(legend.position = c(0.3, 0.88),
        axis.title.x = element_text(colour = NA),
        plot.margin = unit(c(.2, .5, .2, .2),"cm"))
sp
# dimensions: 5 x 3 in

#### Figure 1b1. Aquaculture distribution map ####
countries <- read.csv("~/PATH/Countries.csv")
countries$Production <- countries$Production/1000000 # convert t to Mt
mapdf <- map_data(map = "world")
colnames(mapdf)[5] <- "Country"
require(dplyr)
mapdf <- left_join(mapdf, countries, by = "Country")
mapdf[is.na(mapdf$Production),]$Production <- 0.0001
colnames(mapdf)[5] <- "id"

assimilation <- read.csv("~/PATH/Assimilation.csv")
assimilation <- assimilation[complete.cases(assimilation$Latitude_decimal_degrees),]
assimilation <- assimilation[assimilation$Avg_NPP_kg_C_m2_y > 0,]

map <- ggplot() +
  geom_map(data = mapdf, map = mapdf, colour = "#ffffff", size = 0.1,
           aes(fill = log10(Production), map_id = id)) +
  geom_point(data = assimilation, aes(Longitude_decimal_degrees, 
                                      Latitude_decimal_degrees),
             shape = 4, size = 1, colour = "#2E4A5B", alpha = 0.5) +
  scale_fill_gradient(low = "#DBE6ED",
                      high = "#2E4A5B", 
                      limits = c(-4, 2),
                      labels = c(expression("<10"^-4), expression("10"^-2), 
                                 expression("10"^0), expression("10"^2)),
                      breaks = seq(-4, 2, by = 2),
                      guide = guide_colourbar(raster = T, ticks = T, direction = "horizontal",
                                              title = expression("Aquaculture production (Mt yr"^-1*")"), title.position = "top",
                                              barwidth = 12, barheight = 0.5)) +
  scale_y_continuous(breaks = seq(-80, 80, by = 40), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(-90, 150, by = 30), expand = c(0, 0)) +
  xlab(expression("Carbon assimilation (kg C m"^-2*" yr"^-1*")")) +
  # coord_map(xlim = c(-95, 195), ylim = c(-70, 85),
  #           projection = "gall", parameters = 0) +
  coord_fixed(xlim = c(-90, 150), ylim = c(-80, 80)) +
  mytheme +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(colour = NA),
        axis.ticks.x = element_line(colour = NA),
        axis.text.x = element_text(colour = NA),
        axis.line = element_blank(),
        legend.title = element_text(size = 12),
        panel.border = element_rect(fill = NA, size = 1),
        legend.position = c(0.36, 0.17),
        plot.margin = unit(c(.2, 0, .2, .2),"cm"),)

map # print (save as 5 x 7 in)

#### Figure 1b2. Macroalgal production by latitude ####
require(mgcv)
m1 <- gam(Avg_NPP_kg_C_m2_y ~ s(Latitude_decimal_degrees),
          data = assimilation)
plot(resid(m1) ~ Latitude_decimal_degrees, data = assimilation) 
abline(0,0) # heterogenous
hist(resid(m1)) # severely right-skewed

m2 <- gam(Avg_NPP_kg_C_m2_y ~ s(Latitude_decimal_degrees),
          family = Gamma(link = "log"),
          data = assimilation)
plot(resid(m2) ~ Latitude_decimal_degrees, data = assimilation) 
abline(0,0) # more homogenous
hist(resid(m2)) # normal

summary(m2)
# Family: Gamma 
# Link function: log 
# 
# Formula:
#   Avg_NPP_kg_C_m2_y ~ s(Latitude_decimal_degrees)
# 
# Parametric coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.47166    0.04671  -31.51   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#                               edf Ref.df     F p-value    
# s(Latitude_decimal_degrees) 8.512  8.929 49.08  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =   0.16   Deviance explained = 18.2%
# GCV = 3.0338  Scale est. = 2.3389    n = 1072

new <- data.frame(Latitude_decimal_degrees = seq(-62.24252, 77.78333, by = 0.001))
inv <- family(m2)$linkinv
predicted <- predict(m2, type = "link", se.fit = TRUE, newdata = new)
new$fit <- inv(predicted$fit)
new$upper <- inv(predicted$fit + predicted$se.fit * qnorm(0.975))
new$lower <- inv(predicted$fit - predicted$se.fit * qnorm(0.975))

assimilation <- assimilation %>%
  mutate(Highlights = if_else(Vegetation_category == "Floating Sargassum", "Sargassum", 
                              if_else(Species == "Saccharina japonica" | 
                                        Species == "Saccharina angustata" |
                                        Species == "Saccharina latissima" |
                                        Species == "Saccharina longicruris" |
                                        Species == "Laminaria digitata", "Kombu", "Other")))

assimilation$Highlights <- factor(assimilation$Highlights, levels = c("Sargassum", "Kombu", "Other"))

npp <- ggplot() +
  geom_point(data = assimilation, 
             aes(Latitude_decimal_degrees, Avg_NPP_kg_C_m2_y,
                 colour = Highlights, alpha = Highlights),
             shape = 16, size = 3) +
  scale_colour_manual(values = c("#F1C700", "#50590d", "#2E4A5B"),
                      labels = c(expression(italic("Sargassum natans")*" and "*italic("fluitans")),
                                 expression("Kombu "*italic("sensu lato")),
                                 "")) +
  scale_alpha_manual(values = c(1, 1, 0.1),
                     labels = c(expression(italic("Sargassum natans")*" and "*italic("fluitans")),
                                expression("Kombu "*italic("sensu lato")),
                                "")) +
  geom_line(data = new, aes(Latitude_decimal_degrees, fit), 
            colour = "#2E4A5B", size = 1) +
  geom_ribbon(data = new, aes(Latitude_decimal_degrees, ymin = lower, ymax = upper),
              alpha = 0.5, fill = "#2E4A5B") +
  annotate("text", x = c(60, 40), y = c(2, 3), # 2.248453193, 3.218643000, 3.531000000 kg C m-2 yr-1
           parse = T, size = 4.2,
           label = c('italic("Laminaria digitata")',
                     'italic("Saccharina longicruris")')) + # these annotations will be positioned
  ylab(expression("Carbon assimilation (kg C m"^-2*" yr"^-1*")")) +
  scale_y_continuous(breaks = seq(0, 4, by = 1)) +
  coord_flip(ylim = c(0, 4), xlim = c(-80, 80), expand = F, clip = "off") +
  mytheme +
  theme(axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(0.37, 0.035),
        plot.margin = unit(c(.2, .5, .2, -.4),"cm"),)

npp # 5 x 3.5 in

#### Figure 1c1. Macroalgal production ####
assimilationb <- assimilation[assimilation$Phyla == "Brown" & 
                              assimilation$Vegetation_category != "Floating Sargassum",]
mean(assimilation[assimilation$Vegetation_category == "Floating Sargassum",]$Avg_NPP_kg_C_m2_y)
# mean NPP for Sargassum natans and fluitans = 0.000204473 kg C m^-2 yr^-1 (n = 5 studies)

require(ggridges)
nppd <- ggplot() +
  geom_density_ridges(data = assimilationb, aes(Avg_NPP_kg_C_m2_y, 0.55),
                      position = "raincloud", jittered_points = T,
                      bandwidth = 0.05, calc_ecdf = T, quantile_lines = T,
                      colour = "#2E4A5B", fill = "#2E4A5B",
                      alpha = 0.5, point_alpha = 0.1, point_shape = 16, point_size = 3,
                      scale = 0.15) +
  geom_crossbar(data = assimilation, aes(x = mean(Avg_NPP_kg_C_m2_y), xmin = -1000, xmax = -1000, y = 0.3),
                width = 0.5, size = 0.2) +
  # geom_text(data = assimilationb, aes(Avg_NPP_kg_C_m2_y, 0, label = Species),
  #           position = position_jitter()) +
  annotate("text", x = c(1, 3, 5), y = 0.05, # 4.29, 4.855, 5.47 kg C m^-2 yr^-1
           parse = T, size = 4.2,
           label = c('italic("Laminaria hyperborea")',
                     'italic("Durvillaea antarctica")',
                     'italic("Ecklonia radiata")')) + # these annotations will be positioned
  geom_vline(aes(xintercept = 0.000204473,
                 colour = "Sargassum natans and fluitans")) +
  scale_colour_manual(values = "#F1C700",
                      labels = expression(italic("Sargassum natans")*" and "*italic("fluitans")*" (n = 5)")) +
  xlab(expression("Carbon assimilation (kg C m"^-2*" yr"^-1*")")) +
  coord_cartesian(xlim = c(0, 6), expand = F, clip = "off") +
  mytheme +
  theme(legend.position = c(1.03, 0.95),
        legend.justification = 1,
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

nppd

#### Figure 1c2. Macroalgal total chlorophyll ####
chlorophyll <- read.csv("~/PATH/Chlorophyll.csv")
chlorophyll$Chl <- with(chlorophyll, Chl.a + Chl.c)
chlorophyll <- chlorophyll[-c(352, 367, 414, 437, 455, 467, 511),] # remove extreme outliers

(485.2 + 39.36)/1000
# mean total chlorophyll for Sargassum natans and fluitans = 0.52456 mg g^-1 (n = 288 samples)
# DOI: 10.1029/2018GL078858

chl <- ggplot() +
  geom_density_ridges(data = chlorophyll, aes(Chl, 0.55),
                      position = "raincloud", jittered_points = T,
                      bandwidth = 0.1, calc_ecdf = T, quantile_lines = T,
                      colour = "#2E4A5B", fill = "#2E4A5B",
                      alpha = 0.5, point_alpha = 0.1, point_shape = 16, point_size = 3,
                      scale = 0.61) +
  geom_crossbar(data = chlorophyll, aes(x = mean(Chl), xmin = -1000, xmax = -1000, y = 0.3),
                width = 0.5, size = 0.2) +
  # geom_text(data = assimilationb, aes(Avg_NPP_kg_C_m2_y, 0, label = Species),
  #           position = position_jitter()) +
  annotate("text", x = c(0, 2, 4, 6), y = 0.05, # 5.744593, 5.857696, 6.689625, 6.762581 mg g^-1
           parse = T, size = 4.2,
           label = c('italic("Macrocystis pyrifera")',
                     'italic("Marginariella urvilliana")',
                     'italic("Spatoglossum chapmanii")',
                     'italic("Dictyota kunthii")')) + # these annotations will be positioned
  geom_vline(aes(xintercept = 0.52456,
                 colour = "Sargassum natans and fluitans")) +
  scale_colour_manual(values = "#F1C700",
                      labels = expression(italic("Sargassum natans")*" and "*italic("fluitans")*" (n = 288)")) +
  xlab(expression("Chlorophyll "*italic("a")*" + "*italic("c")*" (mg g"^-1*")")) +
  scale_x_continuous(breaks = seq(0, 7, by = 1)) +
  coord_cartesian(xlim = c(0, 7), expand = F, clip = "off") +
  mytheme +
  theme(legend.position = c(1.03, 0.95),
        legend.justification = 1,
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

chl

#### Figure 1d1. Calcifying epifaunal cover ####
calcification <- read.csv("~/PATH/Calcification.csv")
calcification.nosarg <- calcification[-c(1:36),] # remove Sargassum fluitans and natans

mean(calcification[1:36,]$Cover) # = 41.59289%
length(calcification[1:36,]$Cover) # = 36
# mean hydroid cover for Sargassum natans and fluitans = 41.59289% (n = 36 samples)
# DOI: 10.7717/peerj.9795

cal <- ggplot() +
  geom_density_ridges(data = calcification.nosarg, aes(Cover, 0.55),
                      position = "raincloud", jittered_points = T,
                      bandwidth = 1, calc_ecdf = T, quantile_lines = T,
                      colour = "#2E4A5B", fill = "#2E4A5B",
                      alpha = 0.5, point_alpha = 0.1, point_shape = 16, point_size = 3,
                      scale = 2.617) +
  geom_crossbar(data = calcification.nosarg, aes(x = mean(Cover), xmin = -1000, xmax = -1000, y = 0.3),
                width = 0.5, size = 0.2) +
  # geom_text(data = calcification, aes(Mean, 0, label = Plant),
  #           position = position_jitter()) +
  annotate("text", x = c(60, 90), y = 0.05, # 82.49709000%, 99.90469000%
           parse = T, size = 4.2,
           label = c('italic("Saccharina longicruris")',
                     'italic("Saccharina latissima")')) + # these annotations will be positioned
  geom_vline(aes(xintercept = 41.59289,
                 colour = "Sargassum natans and fluitans")) +
  scale_colour_manual(values = "#F1C700",
                      labels = expression(italic("Sargassum natans")*" and "*italic("fluitans")*" (n = 36)")) +
  xlab("Calcified surface (%)") +
  scale_x_continuous(breaks = seq(0, 100, by = 20)) +
  coord_cartesian(xlim = c(0, 100), expand = F, clip = "off") +
  mytheme +
  theme(legend.position = c(1.03, 0.95),
        legend.justification = 1,
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

cal

#### Figure 1d2. Rafting and calcification ####
rafting <- read.csv("~/PATH/Rafting.csv")

rafting$Season <- factor(rafting$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))

m3 <- nls(Cover ~ SSlogis(Day, A, μ, invk), data = rafting)
summary(m3)

plot(resid(m3) ~ rafting$Day) # fairly balanced
hist(resid(m3)) # fairly normal
# however, I want to simplify/constrain the model to give me a sigmoid between 0 and 100%

m4 <- nls(Cover ~ 100/(1 + exp(-k*(Day - μ))), 
          start = list(k = 1/3.2216, # input parameters from m4
                       μ = 21.2810),
          data = rafting)
summary(m4)

plot(resid(m4) ~ rafting$Day) # somewhat unbalanced
hist(resid(m4)) # less normal than m4
# try fix it with generalised nonlinear least squares

require(nlme)
m5 <- gnls(Cover ~ 100/(1 + exp(-k*(Day - μ))),
           start = list(k = 0.182928, 
                        μ = 24.305264),
           weights = varFixed(~Day),
           data = rafting)

plot(resid(m5, type = "normalized") ~ rafting$Day) # more balanced
hist(resid(m5, type = "normalized")) # more normal
# m5 is chosen as the optimal model

summary(m5)
# Generalized nonlinear least squares fit
# Model: Cover ~ 100/(1 + exp(-k * (Day - μ))) 
# Data: rafting 
# AIC      BIC    logLik
# 3641.082 3653.363 -1817.541
# 
# Variance function:
#   Structure: fixed weights
# Formula: ~Day 
# 
# Coefficients:
#       Value Std.Error  t-value p-value
# k  0.214523 0.0105365 20.36001       0
# μ 23.945599 0.2625619 91.19983       0
# 
# Correlation: 
#   k     
# μ -0.397
# 
# Standardized residuals:
#   Min          Q1         Med          Q3         Max 
# -3.97203770 -0.55343000 -0.22988019  0.09575731  3.94359204 
# 
# Residual standard error: 3.530643 
# Degrees of freedom: 443 total; 441 residual

# generate a new prediction dataframe
new <- data.frame(Day = seq(7, 53, by = 0.5))
# get model to predict values given those artificial data
new$fit <- predict(m5, newdata = new)

# bootstrap confidence interval
bootfun <- function(newdata) {
  start <- coef(m5)
  boot <- rafting[sample(nrow(rafting), size = nrow(rafting), replace = TRUE),]
  bootfit <- try(update(m6,
                        start = start,
                        data = boot),
                 silent = TRUE)
  if (inherits(bootfit, "try-error")) return(rep(NA, nrow(newdata)))
  predict(bootfit, newdata)
}

bmat <- replicate(1000, bootfun(new))
new$lwr <- apply(bmat, 1, quantile, 0.025, na.rm = TRUE)
new$upr <- apply(bmat, 1, quantile, 0.975, na.rm = TRUE)

raf <- ggplot() +
  geom_point(data = rafting, aes(Day, Cover, shape = Season, size = Season), 
             alpha = 0.2, colour = "#2E4A5B") +
  geom_line(data = new, aes(Day, fit), colour = "#2E4A5B", size = 1) +
  geom_ribbon(data = new, aes(Day, ymin = lwr, ymax = upr), 
              fill = "#2E4A5B", alpha = 0.5) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  scale_size_manual(values = c(3, 3, 3, 4)) +
  annotate("text", x = 2, y = 95, hjust = 0,
           parse = T, size = 4.2,
           label = 'italic("Macrocystis pyrifera")') +
  geom_text(aes(60, 20), label = expression("y = "*frac(100, 1 + italic(e)^-0.21(x - 23.95))),
            size = 4.2, hjust = 1, check_overlap = T) +
  labs(y = "Calcified surface (%)", x = "Time spent rafting (d)") +
  scale_x_continuous(breaks = seq(0, 60, by = 15)) +
  scale_y_continuous(breaks = seq(0, 100, by = 50)) +
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 100), expand = F, clip = "off") +
  mytheme +
  theme(legend.position = c(0.103, 0.7))
raf

#### Figure 1. Combined figure ####
require(cowplot)
top <- plot_grid(sp, map, npp, nrow = 1, labels = c("a", "b", ""), label_size = 20,
                 rel_widths = c(0.4285714, 1, 0.5), hjust = 0)
bottom <- plot_grid(nppd, chl, cal, raf, nrow = 2, labels = c("c", "", "d", ""), 
                    label_size = 20, hjust = 0)
Fig1 <- plot_grid(top, bottom, ncol = 1, labels = "", rel_heights = c(1, 1.1))
Fig1 # 10.5 x 13.5 in


#### No Figure. Other factors explaining variation in epifaunal calcification ####
# Artificial vs natural substrata
artificial <- read.csv("~/PATH/Artificial.csv")
require(betareg) # used to model the beta distribution
# the beta distribution only allows values between 0 and 1
# hence Cover needs to be divided by 100 and
# all 0s need to be changed to the smallest and all 1s to the largest possible value
c <- artificial$Cover/100 # proportional cover
c[c == 0] <- 0.001
c[c == 1] <- 0.999
sub <- factor(artificial$Substratum)

m6 <- betareg(c ~ sub,
              link = "logit")
plot(resid(m6) ~ fitted(m6)) 
# acceptable heterogeneity (there is no way of modelling heterogeneity in betareg)
hist(resid(m6)) # balanced

require(car)
Anova(m6, type = 2)
# Response: c
#     Df  Chisq Pr(>Chisq)    
# sub  1 49.114  2.414e-12 ***

require(psych)
describeBy(c, sub, mat = T)
# absolute Δ mean = 23.82%
# absolute Δ median = 20.3%
# relative Δ mean = 244.27%
# relative Δ median = 1020%

# Aquaculture vs natural
c <- calcification$Cover/100 # proportional cover
c[c == 0] <- 0.001
c[c == 1] <- 0.999
cul <- factor(calcification$Farmed)

m7 <- betareg(c ~ cul,
              link = "logit")
plot(resid(m7) ~ fitted(m7)) 
# acceptable heterogeneity (there is no way of modelling heterogeneity in betareg)
hist(resid(m7)) # normal

Anova(m7, type = 2)
# Response: c
#     Df  Chisq Pr(>Chisq)
# cul  1 1.6416     0.2001

describeBy(c, cul, mat = T)
# absolute Δ mean = 2.09%
# absolute Δ median = 3.4%
# relative Δ mean = 14.39%
# relative Δ median = 1552%

# Molar ratios of CO2 to CaCO3
require(seacarb)

# Bach et al. subtropical
psi(flag = 15, var1 = 2350e-6, var2 = 2047.5e-6, 
    S = 35, T = 25, P = 0, Pt = 0, Sit = 0, pHscale = "T", 
    kf = "pf", k1k2 = "l", ks = "d")
# 0.6300337 CO2 per CaCO3

# Sargasso Sea (10.1016/j.marchem.2007.05.003)
psi(flag = 15, var1 = 2420e-6, var2 = 2080e-6, 
    S = 36.6, T = 28, P = 0, Pt = 0, Sit = 0, pHscale = "T", 
    kf = "pf", k1k2 = "l", ks = "d")
# 0.6035531 CO2 per CaCO3







#### Figure 2. Algal carbon fate ####
#### Figure 2a. Carbon remineralisation, export and accumulation ####
fate <- read.csv("~/PATH/Fate.csv")

fate$Ecosystem <- factor(fate$Ecosystem, 
                         levels = c("Phytoplankton", "Macroalgal forests", "Seagrass meadows", 
                                    "Mangrove forests", "Marshes", "Grasslands", "Terrestrial forests"))

m8 <- lm(Carbon ~ Ecosystem, data = fate[fate$Fate == "Accumulated",])
boxplot(resid(m8) ~ Ecosystem, data = fate[fate$Fate == "Accumulated",]) # heterogenous
qqnorm(resid(m8))
qqline(resid(m8)) # right-skewed

m9 <- glm(Carbon ~ Ecosystem, family = Gamma(link = "log"),
          data = fate[fate$Fate == "Accumulated",])
boxplot(resid(m9) ~ Ecosystem, data = fate[fate$Fate == "Accumulated",]) # homogenous
qqnorm(resid(m9))
qqline(resid(m9)) # normal

Anova(m9)
# Response: Carbon
#           LR Chisq Df Pr(>Chisq)    
# Ecosystem   82.433  6  1.122e-15 ***
  
require(emmeans)
emmeans(m9, pairwise ~ "Ecosystem", adjust = "none")$contrasts
# contrast                                 estimate    SE  df z.ratio p.value
# Phytoplankton - Macroalgal forests         -0.769 0.471 Inf  -1.631  0.1030 NS
# Phytoplankton - Seagrass meadows           -2.658 0.598 Inf  -4.445  <.0001 ***
# Phytoplankton - Mangrove forests           -2.778 0.576 Inf  -4.822  <.0001 ***
# Phytoplankton - Marshes                    -4.024 0.459 Inf  -8.766  <.0001 ***
# Phytoplankton - Grasslands                 -2.496 0.459 Inf  -5.437  <.0001 ***
# Phytoplankton - Terrestrial forests        -3.527 0.448 Inf  -7.864  <.0001 ***
# Macroalgal forests - Seagrass meadows      -1.889 0.615 Inf  -3.069  0.0021 **
# Macroalgal forests - Mangrove forests      -2.009 0.594 Inf  -3.381  0.0007 ***
# Macroalgal forests - Marshes               -3.255 0.482 Inf  -6.759  <.0001 ***
# Macroalgal forests - Grasslands            -1.727 0.482 Inf  -3.586  0.0003 ***
# Macroalgal forests - Terrestrial forests   -2.758 0.471 Inf  -5.849  <.0001 ***
# Seagrass meadows - Mangrove forests        -0.120 0.699 Inf  -0.172  0.8636 NS
# Seagrass meadows - Marshes                 -1.366 0.606 Inf  -2.254  0.0242 *
# Seagrass meadows - Grasslands               0.162 0.606 Inf   0.268  0.7890 NS
# Seagrass meadows - Terrestrial forests     -0.869 0.598 Inf  -1.453  0.1462 NS
# Mangrove forests - Marshes                 -1.246 0.584 Inf  -2.132  0.0330 *
# Mangrove forests - Grasslands               0.282 0.584 Inf   0.483  0.6291 NS
# Mangrove forests - Terrestrial forests     -0.749 0.576 Inf  -1.300  0.1936 NS
# Marshes - Grasslands                        1.528 0.469 Inf   3.256  0.0011 **
# Marshes - Terrestrial forests               0.497 0.459 Inf   1.083  0.2789 NS
# Grasslands - Terrestrial forests           -1.031 0.459 Inf  -2.246  0.0247 *

m10 <- lm(Carbon ~ Ecosystem, data = fate[fate$Fate == "Remineralised",])
boxplot(resid(m10) ~ Ecosystem, data = fate[fate$Fate == "Remineralised",]) # heterogenous
qqnorm(resid(m10))
qqline(resid(m10)) # right-skewed

m11 <- glm(Carbon ~ Ecosystem, family = Gamma(link = "log"),
           data = fate[fate$Fate == "Remineralised",])
boxplot(resid(m11) ~ Ecosystem, data = fate[fate$Fate == "Remineralised",]) # homogenous
qqnorm(resid(m11))
qqline(resid(m11)) # normal

Anova(m11)
# Response: Carbon
#           LR Chisq Df Pr(>Chisq)    
# Ecosystem   126.16  6  < 2.2e-16 ***
  
emmeans(m11, pairwise ~ "Ecosystem", adjust = "none")$contrasts
# contrast                                 estimate    SE  df z.ratio p.value
# Phytoplankton - Macroalgal forests        -1.3866 0.217 Inf  -6.396  <.0001 ***
# Phytoplankton - Seagrass meadows          -0.1292 0.209 Inf  -0.617  0.5370 NS
# Phytoplankton - Mangrove forests           0.4716 0.335 Inf   1.407  0.1594 NS
# Phytoplankton - Marshes                   -0.4154 0.279 Inf  -1.489  0.1365 NS
# Phytoplankton - Grasslands                 0.5563 0.236 Inf   2.353  0.0186 *
# Phytoplankton - Terrestrial forests        0.6352 0.211 Inf   3.016  0.0026 **
# Macroalgal forests - Seagrass meadows      1.2573 0.216 Inf   5.816  <.0001 ***
# Macroalgal forests - Mangrove forests      1.8582 0.339 Inf   5.474  <.0001 ***
# Macroalgal forests - Marshes               0.9712 0.284 Inf   3.418  0.0006 ***
# Macroalgal forests - Grasslands            1.9428 0.242 Inf   8.012  <.0001 ***
# Macroalgal forests - Terrestrial forests   2.0218 0.217 Inf   9.301  <.0001 ***
# Seagrass meadows - Mangrove forests        0.6009 0.335 Inf   1.795  0.0727 NS
# Seagrass meadows - Marshes                -0.2861 0.278 Inf  -1.027  0.3042 NS
# Seagrass meadows - Grasslands              0.6855 0.236 Inf   2.906  0.0037 **
# Seagrass meadows - Terrestrial forests     0.7645 0.210 Inf   3.640  0.0003 ***
# Mangrove forests - Marshes                -0.8870 0.382 Inf  -2.321  0.0203 *
# Mangrove forests - Grasslands              0.0846 0.352 Inf   0.240  0.8101 NS
# Mangrove forests - Terrestrial forests     0.1636 0.336 Inf   0.488  0.6258 NS
# Marshes - Grasslands                       0.9716 0.299 Inf   3.246  0.0012 **
# Marshes - Terrestrial forests              1.0506 0.279 Inf   3.760  0.0002 ***
# Grasslands - Terrestrial forests           0.0790 0.237 Inf   0.333  0.7390 NS

m12 <- lm(Carbon ~ Ecosystem, data = fate[fate$Fate == "Exported",])
boxplot(resid(m12) ~ Ecosystem, data = fate[fate$Fate == "Exported",]) # heterogenous
qqnorm(resid(m12))
qqline(resid(m12)) # right-skewed

m13 <- glm(Carbon ~ Ecosystem, family = Gamma(link = "log"),
           data = fate[fate$Fate == "Exported",])
boxplot(resid(m13) ~ Ecosystem, data = fate[fate$Fate == "Exported",]) # homogenous
qqnorm(resid(m13))
qqline(resid(m13)) # normal

Anova(m13)
# Response: Carbon
#           LR Chisq Df Pr(>Chisq)    
# Ecosystem   38.037  6  1.105e-06 ***
  
emmeans(m13, pairwise ~ "Ecosystem", adjust = "none")$contrasts
# contrast                                 estimate    SE  df z.ratio p.value
# Phytoplankton - Macroalgal forests        -1.7839 0.482 Inf  -3.704  0.0002 ***
# Phytoplankton - Seagrass meadows          -0.9503 0.542 Inf  -1.752  0.0798 NS
# Phytoplankton - Mangrove forests          -1.6697 0.611 Inf  -2.735  0.0062 **
# Phytoplankton - Marshes                   -1.5775 0.521 Inf  -3.026  0.0025 **
# Phytoplankton - Grasslands                 1.9833 0.772 Inf   2.568  0.0102 *
# Phytoplankton - Terrestrial forests        1.8186 0.727 Inf   2.503  0.0123 *
# Macroalgal forests - Seagrass meadows      0.8336 0.392 Inf   2.128  0.0333 *
# Macroalgal forests - Mangrove forests      0.1143 0.482 Inf   0.237  0.8124 NS
# Macroalgal forests - Marshes               0.2065 0.362 Inf   0.570  0.5683 NS
# Macroalgal forests - Grasslands            3.7672 0.675 Inf   5.581  <.0001 ***
# Macroalgal forests - Terrestrial forests   3.6026 0.622 Inf   5.789  <.0001 ***
# Seagrass meadows - Mangrove forests       -0.7193 0.542 Inf  -1.326  0.1848 NS
# Seagrass meadows - Marshes                -0.6272 0.440 Inf  -1.426  0.1538 NS
# Seagrass meadows - Grasslands              2.9336 0.720 Inf   4.076  <.0001 ***
# Seagrass meadows - Terrestrial forests     2.7689 0.671 Inf   4.129  <.0001 ***
# Mangrove forests - Marshes                 0.0922 0.521 Inf   0.177  0.8596 NS
# Mangrove forests - Grasslands              3.6529 0.772 Inf   4.730  <.0001 ***
# Mangrove forests - Terrestrial forests     3.4883 0.727 Inf   4.800  <.0001 ***
# Marshes - Grasslands                       3.5607 0.704 Inf   5.059  <.0001 ***
# Marshes - Terrestrial forests              3.3961 0.654 Inf   5.196  <.0001 ***
# Grasslands - Terrestrial forests          -0.1647 0.867 Inf  -0.190  0.8494 NS

stats <- with(fate, describeBy(Carbon, list(Fate, Ecosystem), mat = TRUE))

stats$group2 <- factor(stats$group2, 
                       levels = c("Phytoplankton", "Macroalgal forests", "Seagrass meadows", 
                                  "Mangrove forests", "Marshes", "Grasslands", "Terrestrial forests"))

mytheme <- theme(panel.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 plot.margin = unit(c(.2, .5, .2, .2),"cm"),
                 axis.line = element_line(),
                 axis.title = element_text(size = 15, hjust = 0),
                 axis.text = element_text(size = 12, colour = "black"),
                 axis.ticks.length = unit(.25, "cm"),
                 axis.ticks = element_line(colour = "black"),
                 axis.line.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 legend.key = element_blank(),
                 legend.key.size = unit(.3, "cm"),
                 legend.key.height = unit(.45, "cm"),
                 legend.spacing.x = unit(.1, "cm"),
                 legend.spacing.y = unit(.05, "cm"),
                 legend.background = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.text.align = 0,
                 legend.title.align = 0,
                 legend.title = element_blank())

CA <- ggplot() +
  geom_density_ridges(fate[fate$Fate == "Accumulated",], mapping = aes(Carbon/1000, Ecosystem, colour = Ecosystem),
                      scale = 0.5, bandwidth = 0.03, fill = NA,
                      point_alpha = 0.5, point_shape = 16, position = "raincloud", point_size = 2,
                      calc_ecdf = TRUE, quantile_lines = TRUE, quantiles = 0.5,
                      jittered_points = TRUE) +
  scale_colour_manual(values = c("#81a512", "#bd9b61", rep("#2E4A5B", 5))) +
  geom_crossbar(stats[stats$group1 == "Accumulated",], mapping = aes(x = mean/1000, xmin = -1000, xmax = -1000, y = 0.75:6.75),
                width = 0.5, size = 0.2) +
  annotate("text", label = paste(stats[stats$group1 == "Accumulated",]$group2, expression(" (n = "), 
                                 stats[stats$group1 == "Accumulated",]$n, expression(")"), sep = ""), 
           x = 2, y = 1.32:7.32, hjust = 1, size = 4.2) +
  coord_cartesian(xlim = c(0, 2), clip = "off") +
  labs(x = expression("Assimilated carbon (kg C m"^-2*" yr"^-1*")"), y = "Accumulated fraction") +
  scale_x_continuous(position = "top", expand = c(0, 0)) +
  scale_y_discrete(labels = c("d", "d", "ab", "ab", "c", "a", "bc")) +
  mytheme +
  theme(legend.position = "none",
        axis.title.y = element_text(margin = margin(0, 5, 0, 0)),
        axis.text.y = element_text(margin = margin(0, 20, 0, 0)))
CA

CR <- ggplot() +
  geom_density_ridges(fate[fate$Fate == "Remineralised",], mapping = aes(Carbon/1000, Ecosystem, colour = Ecosystem),
                      scale = 0.5, bandwidth = 0.03, fill = NA,
                      point_alpha = 0.5, point_shape = 16, position = "raincloud", point_size = 2,
                      calc_ecdf = TRUE, quantile_lines = TRUE, quantiles = 0.5,
                      jittered_points = TRUE) +
  scale_colour_manual(values = c("#81a512", "#bd9b61", rep("#2E4A5B", 5))) +
  geom_crossbar(stats[stats$group1 == "Remineralised",], mapping = aes(x = mean/1000, xmin = -1000, xmax = -1000, y = 0.75:6.75),
                width = 0.5, size = 0.2) +
  annotate("text", label = paste(stats[stats$group1 == "Remineralised",]$group2, expression(" (n = "), 
                                 stats[stats$group1 == "Remineralised",]$n, expression(")"), sep = ""), 
           x = 2, y = 1.32:7.32, hjust = 1, size = 4.2) +
  coord_cartesian(xlim = c(0, 2), clip = "off") +
  labs(x = expression("Assimilated carbon (kg C m"^-2*" yr"^-1*")"), y = "Remineralised fraction") +
  scale_x_continuous(position = "top", expand = c(0, 0)) +
  scale_y_discrete(labels = c("bc", "d", "bc", "ab", "c", "a", "a")) +
  mytheme +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(0, 5, 0, 0)),
        axis.text.y = element_text(margin = margin(0, 20, 0, 0)),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
CR

CE <- ggplot() +
  geom_density_ridges(fate[fate$Fate == "Exported",], mapping = aes(Carbon/1000, Ecosystem, colour = Ecosystem),
                      scale = 0.5, bandwidth = 0.03, fill = NA,
                      point_alpha = 0.5, point_shape = 16, position = "raincloud", point_size = 2,
                      calc_ecdf = TRUE, quantile_lines = TRUE, quantiles = 0.5,
                      jittered_points = TRUE) +
  scale_colour_manual(values = c("#81a512", "#bd9b61", rep("#2E4A5B", 5))) +
  geom_crossbar(stats[stats$group1 == "Exported",], mapping = aes(x = mean/1000, xmin = -1000, xmax = -1000, y = 0.75:6.75),
                width = 0.5, size = 0.2) +
  annotate("text", label = paste(stats[stats$group1 == "Exported",]$group2, expression(" (n = "), 
                                 stats[stats$group1 == "Exported",]$n, expression(")"), sep = ""), 
           x = 2, y = 1.32:7.32, hjust = 1, size = 4.2) +
  coord_cartesian(xlim = c(0, 2), clip = "off") +
  labs(x = expression("Assimilated carbon (kg C m"^-2*" yr"^-1*")"), y = "Exported fraction") +
  scale_x_continuous(position = "top", expand = c(0, 0)) +
  scale_y_discrete(labels = c("b", "d", "bc", "cd", "cd", "a", "a")) +
  mytheme +
  theme(legend.position = "none",
        axis.title.y = element_text(margin = margin(0, 5, 0, 0)),
        axis.text.y = element_text(margin = margin(0, 20, 0, 0)),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())
CE



#### Figure 2b1. Remineralisation of exported carbon ####
decomp <- read.csv("~/PATH/Decomposition.csv")
decomp$Plant <- factor(decomp$Plant, levels = c("marine phytoplankton", "marine macroalgae"))

m14 <- lm(k ~ Plant, data = decomp)
boxplot(resid(m14) ~ Plant, data = decomp) # heterogenous
qqnorm(resid(m14))
qqline(resid(m14)) # right-skewed

m15 <- glm(k+0.0032 ~ Plant, data = decomp, family = Gamma(link = "log"))
boxplot(resid(m15) ~ Plant, data = decomp) # homogenous
qqnorm(resid(m15))
qqline(resid(m15)) # more normal

Anova(m15, type = 2)
# Response: k + 0.0032
#       LR Chisq Df Pr(>Chisq)    
# Plant   35.766  1  2.225e-09 ***

stats2 <- with(decomp, describeBy(k, Plant, mat = T))

kp <- ggplot() +
  geom_density_ridges(data = decomp, aes(k, Plant, colour = Plant, fill = Plant),
                      position = "raincloud", jittered_points = T,
                      bandwidth = 0.015, calc_ecdf = T, quantile_lines = T,
                      alpha = 0.5, point_alpha = 0.5, point_shape = 16, point_size = 2,
                      scale = 0.5) +
  geom_crossbar(stats2, mapping = aes(x = mean, xmin = -1000, xmax = -1000, y = 0.75:1.75),
                width = 0.5, size = 0.2) +
  scale_colour_manual(values = c("#81a512", "#bd9b61"),
                      labels = c("Phytoplankton (n = 91)", "Macroalgae (n = 133)")) +
  scale_fill_manual(values = c("#81a512", "#bd9b61"),
                    labels = c("Phytoplankton (n = 91)", "Macroalgae (n = 133)")) +
  # geom_text(data = decomp, aes(k, Plant, label = Species),
  #           position = position_jitter()) +
  annotate("text", x = 1, y = 0.5,
           parse = T, size = 4.2,
           label = 'italic("Thalassiosira angstii ")*italic("Phaeodactylum tricornutum ")*italic("Skeletonema costatum ")*
                    italic("Ecklonia maxima ")*italic("Codium fragile ")*italic("Agardhiella subulata")') + # these annotations will be positioned
  xlab(expression("Remineralisation of exported carbon ("*italic(k)*" d"^-1*")")) +
  coord_cartesian(xlim = c(0, 2), expand = F, clip = "off") +
  mytheme +
  theme(legend.position = c(1.02, 0.95),
        legend.justification = 1,
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

kp


#### Figure 2b2. Refractory dissolved organic carbon ####
# the beta distribution only allows values between 0 and 1
# hence Cover needs to be divided by 100 and
# all 0s need to be changed to the smallest and all 1s to the largest possible value
rdoc <- decomp[complete.cases(decomp$rDOC),]$rDOC/100 # proportion
rdoc[rdoc == 0] <- 0.001
plant <- factor(decomp[complete.cases(decomp$rDOC),]$Plant)

m16 <- betareg(rdoc ~ plant,
               link = "logit")
plot(resid(m16) ~ fitted(m16)) 
# acceptable heterogeneity (there is no way of modelling heterogeneity in betareg)
hist(resid(m16)) # fairly normal

Anova(m16, type = 2)
# Response: rdoc
#       Df  Chisq Pr(>Chisq)
# plant  1 0.8336     0.3612

stats3 <- with(decomp, describeBy(rDOC, Plant, quant = c(0.25, 0.75), mat = T))

decomp <- decomp %>% 
  mutate(Period = case_when(
  d > 20 ~ "> 20 d", d < 20 ~ "< 20 d"
))


DOCp <- ggplot() +
  geom_jitter(data = decomp[complete.cases(decomp$rDOC),], aes(rDOC, Plant, colour = Plant, shape = Period),
              alpha = 0.5, size = 2, height = 0.15) +
  geom_boxplot(data = decomp[complete.cases(decomp$rDOC),], aes(rDOC, Plant, colour = Plant),
               fill = NA, fatten = 1, width = 0.3) +
  geom_crossbar(stats3, mapping = aes(x = mean, xmin = -1000, xmax = -1000, y = group1),
                width = 0.3, size = 0.2) +
  scale_colour_manual(values = c("#81a512", "#bd9b61"),
                      labels = c("Phytoplankton (n = 14)  ", "Macroalgae (n = 12)")) +
  scale_fill_manual(values = c("#81a512", "#bd9b61"),
                    labels = c("Phytoplankton (n = 14)  ", "Macroalgae (n = 12)")) +
  scale_shape_manual(values = c(16, 17),
                     labels = c("< 20 d  ", "> 20 d")) +
  # geom_text(data = decomp[complete.cases(decomp$rDOC),], aes(rDOC, Plant, label = Species),
  #           position = position_jitter()) +
  annotate("text", x = c(0, 3.510906, 65.721620, 72.726010, 
                         10.338040, 10.414700, 78, 85.7), 
                   y = c(1, 1.2, 1.4, 1.6, 2, 2.2, 2.4, 2.6),
           parse = T, size = 4.2,
           label = c('italic("Scrippsiella acuminata")',
                     'italic("Thalassiosira angstii")',
                     'italic("Cylindrotheca fusiformis")',
                     'community',
                     'italic("Laminaria pallida")',
                     'italic("Ulva lactuca")',
                     'italic("Sargassum horneri")',
                     'italic("Ecklonia cava")')) + # these annotations will be positioned
  xlab('"Refractory" dissolved organic carbon (%)') +
  coord_cartesian(xlim = c(0, 100), clip = "off") +
  scale_x_continuous(expand = c(0, 0)) +
  mytheme +
  theme(legend.position = "top",
        legend.justification = "right",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0, .5, .2, .2),"cm"))

DOCp

#### Figure 2. Combined figure ####
Fig2 <- plot_grid(CA, CR, CE, kp, DOCp, ncol = 1, rel_heights = c(1, 0.9, 0.9, 0.9, 0.8),
                  labels = c("a", "", "", "b", ""), label_size = 20, hjust = 0, align = "v")
Fig2 # dimensions: 12 x 6 in

#### No Figure. Faunal diets ####
diet <- read.csv("~/PATH/Diet.csv")
diet$Plant <- factor(diet$Plant, levels = c("Phytoplankton", "Macroalgae"))

diet$Average[3]-diet$Average[4]
# absolute Δ mean = 3.25%

(diet$Average[3]-diet$Average[4])/diet$Average[4]
# relative Δ mean = 12.75%

# dp <- ggplot(data = diet) +
#   geom_pointrange(aes(Q0.5*100, Plant, xmin = Q0.25*100, xmax = Q0.75*100,
#                       colour = Plant), shape = 16, size = 0.6) +
#   geom_point(aes(Mean*100, Plant, colour = Plant), shape = 4, size = 3) +
#   geom_rug(aes(Average*100, colour = Plant), length = unit(0.25, "cm")) +
#   scale_colour_manual(values = c("#81a512", "#bd9b61"),
#                       labels = c("Phytoplankton", "Macroalgae")) +
#   geom_text(aes(0, 1.5, label = Fauna), hjust = 0, size = 4.2) +
#   facet_grid(Fauna ~ .) +
#   xlab("Contribution to diet of benthic fauna (%)") +
#   coord_cartesian(xlim = c(0, 60)) +
#   scale_x_continuous(breaks = seq(0, 60, by = 20), expand = c(0, 0)) +
#   mytheme +
#   theme(legend.position = c(0.9, 0.9),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.line.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         strip.text = element_blank(),
#         strip.background = element_blank(),
#         plot.margin = unit(c(.2, .5, .2, .2),"cm"))
# 
# dp

#### Clean up ####
detach(package:dplyr)
detach(package:mgcv)
detach(package:betareg)
detach(package:car)
detach(package:psych)
detach(package:nlme)
detach(package:emmeans)
detach(package:ggridges)
detach(package:cowplot)
detach(package:ggplot2)
detach(package:seacarb)
rm(list = ls())
graphics.off()
cat("\014")
