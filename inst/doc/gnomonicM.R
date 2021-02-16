## ---- echo = FALSE, message = FALSE-------------------------------------------
library(kableExtra)
library(gnomonicM)

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("gnomonicM")

## ---- eval=FALSE--------------------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("ejosymart/gnomonicM")

## ---- eval=FALSE--------------------------------------------------------------
#  library("gnomonicM")

## ----echo=TRUE----------------------------------------------------------------
model_hf <- gnomonic(nInterval   = 7, 
                     eggDuration = 2, 
                     longevity   = 365, 
                     fecundity   = 200000, 
                     a_init      = 2)

## ----echo=TRUE----------------------------------------------------------------
model_lf <- gnomonic(nInterval   = 7, 
                     eggDuration = 2, 
                     longevity   = 365, 
                     fecundity   = 135, 
                     a_init      = 2)

## ----echo=TRUE, results = 'hide'----------------------------------------------
modelAddInfo <- gnomonic(nInterval   = 7, 
                         eggDuration = 2, 
                         addInfo     = c(4, NA, NA, 40, NA, NA),
                         longevity   = 365, 
                         fecundity   = 200000, 
                         a_init      = 2)

## ----echo = TRUE--------------------------------------------------------------
#Species with high fecundity.
print(model_hf)

plot(model_hf)

#Species with low fecundity.
print(model_lf)

plot(model_lf, xlab = "My X label", ylab = "My Y label", cex = 3, bg = "blue")

## ---- echo=FALSE--------------------------------------------------------------
data_hf <- data.frame(Gnomonic_interval = 1:7, 
                    interval_duration_day = c(2.00, 2.76, 6.58, 15.67, 37.33, 88.91, 211.75),
                    M_year = c(300.16, 217.25, 91.27, 38.30, 16.08, 6.75, 2.84),
                    No_Surv = c(38614, 7455, 1439, 278, 54, 10, 2))
data_hf %>%
  kbl(caption = "Results provided by Caddy (1996) in the estimation of M based on high fecundity species (MLF = 200,000 eggs, egg stage duration = 2 days, and longevity = 365 days)", booktabs = T) %>% 
  kable_styling(full_width = F, position = "center", latex_options = c("striped", "hold_position"))

## ---- echo=FALSE--------------------------------------------------------------
data_lf <- data.frame(Gnomonic_interval = 1:7, 
                    interval_duration_day = c(2.00, 2.76, 6.58, 15.67, 37.33, 88.91, 211.75),
                    M_year = c(109.82, 79.48, 33.37, 14.01, 5.88, 2.47, 1.04),
                    No_Surv = c(74, 41, 22, 12, 7, 4, 2))
data_lf %>%
  kbl(caption = "Results provided by Caddy (1996) in the estimation of M based on low fecundity species (MLF = 135, egg stage duration = 2 days, and longevity = 365 days)", booktabs = T) %>% 
  kable_styling(full_width = F, position = "center", latex_options = c("striped", "hold_position"))

## ----echo=TRUE----------------------------------------------------------------
model_cm_hf <- gnomonicStochastic(nInterval     = 8, 
                                  eggDuration   = 2.33,
                                  longevity     = 2920,
                                  distr         = "uniform", 
                                  min_fecundity = 11805, 
                                  max_fecundity = 144543, 
                                  niter         = 1000, 
                                  a_init        = 2)


model_cm_lf <- gnomonicStochastic(nInterval     = 8, 
                                  eggDuration   = 2.33,
                                  longevity     = 2920,
                                  distr         = "uniform", 
                                  min_fecundity = 7603, 
                                  max_fecundity = 53921, 
                                  niter         = 1000, 
                                  a_init        = 2)

## ----echo = TRUE, results = 'hide'--------------------------------------------
#The results are not shown here. Please run it in your console.
print(model_cm_hf)

print(model_cm_lf)

## ----echo = TRUE--------------------------------------------------------------
par(mar = c(6,6,6,6))
plot(model_cm_hf, main = "M for chub mackerel, MLF = [11 805 - 144 543]", dayUnits = FALSE)

plot(model_cm_lf, main = "M for chub mackerel, MLF = [7 603 - 53 921]", dayUnits = FALSE)

## ----echo=TRUE----------------------------------------------------------------
modelUniformAddInfo <- gnomonicStochastic(nInterval     = 7, 
                                          eggDuration   = 2,
                                          addInfo       = c(4, NA, NA, 40, NA, NA),
                                          longevity     = 365,
                                          distr         = "uniform", 
                                          min_fecundity = 100000, 
                                          max_fecundity = 300000, 
                                          niter         = 1000, 
                                          a_init        = 2)


modelNormal <- gnomonicStochastic(nInterval     = 7, 
                                  eggDuration   = 2,
                                  longevity     = 365,
                                  distr         = "normal", 
                                  fecundity     = 200000, 
                                  sd_fecundity  = 50000, 
                                  niter         = 1000, 
                                  a_init        = 2)


modelTriangle <- gnomonicStochastic(nInterval     = 7, 
                                    eggDuration   = 2,
                                    longevity     = 365,
                                    distr         = "triangle", 
                                    fecundity     = 200000,
                                    min_fecundity = 100000,
                                    max_fecundity = 300000,
                                    niter         = 1000, 
                                    a_init        = 2)

## ----echo = TRUE--------------------------------------------------------------
plot(modelUniformAddInfo, main = "Uniform distribution in MLF, with additional information")
plot(modelNormal, main = "Normal distribution in MLF")
plot(modelTriangle, main = "Triangular distribution in MLF")

## ----echo=TRUE----------------------------------------------------------------
Farfantopenaeus <-  gnomonic(nInterval   = 7,
                             eggDuration = 1.5, 
                             longevity   = 480,
                             fecundity   = 500000,
                             a_init      = 1)


Vannamei <- gnomonic(nInterval   = 7,
                     eggDuration = 0.54, 
                     longevity   = 365,
                     fecundity   = 265000,   
                     a_init      = 3)


Sardinops <- gnomonicStochastic(nInterval     = 10,
                                eggDuration   = 2.5, 
                                longevity     = 2555,
                                min_fecundity = 646763,
                                max_fecundity = 1090678,
                                niter         = 1000, 
                                a_init        = 2)


Epinephelus <- gnomonicStochastic(nInterval     = 11,
                                  eggDuration   = 2, 
                                  longevity     = 7300,
                                  min_fecundity = 102000,
                                  max_fecundity = 573500,
                                  niter         = 1000, 
                                  a_init        = 2)


Dosidicus <- gnomonicStochastic(nInterval     = 5,
                                eggDuration   = 6, 
                                longevity     = 438,
                                min_fecundity = 813000,
                                max_fecundity = 25887000,
                                niter         = 1000, 
                                a_init        = 2)



Isostichopus <- gnomonicStochastic(nInterval     = 6,
                                   eggDuration   = 2,
                                   longevity     = 3650,
                                   min_fecundity = 13500,
                                   max_fecundity = 5062490, 
                                   niter         = 1000,
                                   a_init        = 2)


## ----echo = TRUE--------------------------------------------------------------
par(mar=c(5.1, 4.1, 6, 2.1))
plot(Farfantopenaeus, main = "M for Farfantopenaeus duorarum", dayUnits = FALSE)

plot(Vannamei, main = "M for Penaeus vannamei", col = "darkred", dayUnits = FALSE)

plot(Sardinops, main = "M for Sardinops caeruleus", col = "blue")

plot(Epinephelus, main = "M for Epinephelus morio", col = "darkgreen", dayUnits = FALSE)

plot(Dosidicus, main = "M for Dodisicus gigas", col = "purple", dayUnits = FALSE)

plot(Isostichopus, main = "M for Isostichopus badionotus", col = "skyblue", dayUnits = FALSE)

