#########################################################################
#########################################################################
######### Black River Popenaias popeii Salinity Tolerance Code ##########
#########################################################################
# Code for Rangaswami et al. (2025): Using Salinity Tolerance to Inform Conservation of an Endangered Unionid Mussel, Popenaias popeii (Texas Hornshell), in a Salinized River
# Published in Aquatic Conservation: Marine and Freshwater Ecosystems
# Creator: Xenia L. Rangaswami
# Property of Charles R. Randklev Laboratory (Mussel Conservation Lab), Texas A&M Natural Resources Institute
# This code was used to determine the lethal concentrations (LC) of salinity (ppt / PSU) for 12h glochidia, 24h glochidia, & 96h juveniles based on data from laboratory experiments.
#########################################################################
#########################################################################


# 12h Glochidia -----------------------------------------------------------
########################################
##############12h Glochidia#############
# Tested: 06/02/2022
# Collected: 06/01/2022
########################################

######################
####Load packages#####
######################

rm(list=ls(all=TRUE))

# packages
library(multcomp)
library(drc)
library(dplyr)
library(lmtest) 
library(Rcmdr)

setwd("") # enter your working directory here

####fixing estfun error to calculate robust standard errors
registerS3method("estfun", "drc", drc:::estfun.drc)
registerS3method("bread", "drc", drc:::bread.drc)

############### Import data

sal12 <- 
  readXL("Popenaias popeii Black Salinity Data Clean",
         rownames=FALSE, header=TRUE, na="", sheet="gloch_12h_data", 
         stringsAsFactors=TRUE)

attach(sal12)

####################################
###four-parameter log-logistic model
####################################

###based on proportion dead
np1drc4 <- drm(Proportion_dead~conc, fct = LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50"))) 

modelFit(np1drc4)
summary(np1drc4)
plot(np1drc4, type="all")

###robust CI
ED(np1drc4, c(5, 9, 46), interval="delta", vcov=sandwich)#since data doesn't go to 0 or 100 need to calculate 5, 10 and 50% percentiles based on range of data


######################################
###three-parameter log-logistic model
######################################

###based on proportion dead
np1drc3 <- drm(Proportion_dead~conc, fct = LL.3(names = c("Slope","Upper Limit", "ED50"))) 

summary(np1drc3)
plot(np1drc3, type="all")

###robust CI
ED(np1drc3, c(5, 9, 46), interval="delta", vcov=sandwich)#since data doesn't go to 0 or 100 need to calculate 5, 10 and 50% percentiles based on range of data


#########################################
###two-parameter log-logistic model
#########################################

###based on proportion dead
np1drc2 <- drm(Proportion_dead~conc, fct = LL.2(names = c("Slope","ED50")))

summary(np1drc2)
plot(np1drc2, type="all")

###robust CI
ED(np1drc2, c(5, 9, 46), interval="delta", vcov=sandwich)#since data doesn't go to 0 or 100 need to calculate 5, 10 and 50% percentiles based on range of data


####################################
###four-parameter Weibull type 1
####################################

###based on proportion dead
np1drcW41 <- drm(Proportion_dead~conc, fct = W1.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50"))) 

modelFit(np1drcW41)
summary(np1drcW41)
plot(np1drcW41, type="all")

###robust CI
ED(np1drcW41, c(5, 9, 46), interval="delta", vcov=sandwich)#since data doesn't go to 0 or 100 need to calculate 5, 10 and 50% percentiles based on range of data


####################################
###four-parameter Weibull type 2
####################################

###based on proportion dead
np1drcW42 <- drm(Proportion_dead~conc, fct = W2.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50"))) 

modelFit(np1drcW42)
summary(np1drcW42)
plot(np1drcW42, type="all")

###robust CI
ED(np1drcW42, c(5, 9, 46), interval="delta", vcov=sandwich)#since data doesn't go to 0 or 100 need to calculate 5, 10 and 50% percentiles based on range of data


####################################
###three-parameter Weibull type 1
####################################

###based on proportion dead
np1drcW31 <- drm(Proportion_dead~conc, fct = W1.3(names = c("Slope", "Lower Limit", "ED50"))) 

modelFit(np1drcW31)
summary(np1drcW31)
plot(np1drcW31, type="all")

###robust CI
ED(np1drcW31, c(5, 9, 46), interval="delta", vcov=sandwich)#since data doesn't go to 0 or 100 need to calculate 5, 10 and 50% percentiles based on range of data


####################################
###three-parameter Weibull type 2
####################################

###based on proportion dead
np1drcW32 <- drm(Proportion_dead~conc, fct = W2.3(names = c("Slope", "Lower Limit", "ED50"))) 

modelFit(np1drcW32)
summary(np1drcW32)
plot(np1drcW32, type="all")

###robust CI
ED(np1drcW32, c(5, 9, 46), interval="delta", vcov=sandwich)#since data doesn't go to 0 or 100 need to calculate 5, 10 and 50% percentiles based on range of data


###########################################
###########################################
#Choosing a model
###########################################
###########################################

# Results Proportion_dead

plot(np1drc4, xlab="concentration", ylab="% viability", type="all", lty=1, lwd=2)
plot(np1drc3, add=TRUE, col="orange", lty=2, lwd=2)
plot(np1drc2, add=TRUE, col="yellow", lty=2, lwd=2) 
plot(np1drcW41, add=TRUE, col="pink", lty=2, lwd=2) 
plot(np1drcW42, add=TRUE, col="blue", lty=2, lwd=2) 
plot(np1drcW32, add=TRUE, col="red", lty=2, lwd=2) 
plot(np1drcW31, add=TRUE, col="green", lty=2, lwd=2) 

mselect(np1drc4, fctList = list(LL.2(), LL.3(), W2.3(), W2.4(), W1.3(), W1.4()))

####results_proportional survival 
# logLik        IC  Lack of fit    Res var
# LL.2 16.79881 -27.59762 1.831398e-04 0.01018714
# W1.3 17.53630 -27.07261 1.351536e-04 0.01001137
# LL.3 16.80480 -25.60961 8.362998e-05 0.01085906
# W1.4 17.53908 -25.07817 5.052584e-05 0.01072316
# W2.3 16.28905 -24.57811 5.959326e-05 0.01149952
# LL.4 17.09988 -24.19976 3.770087e-05 0.01125943
# W2.4 16.30285 -22.60569 2.216080e-05 0.01230204

## Selected LL.2 based on model fit and CI comparison
# Estimated effective doses
# Estimate Std. Error   Lower   Upper
# e:1:5   1.29103    0.11769 1.04153 1.54053
# e:1:9   1.55517    0.11772 1.30562 1.80472
# e:1:46  2.93584    0.14490 2.62868 3.24301



# 24h Glochidia -----------------------------------------------------------
########################################
##############24h Glochidia#############
# Tested: 06/02/2022
# Collected: 06/01/2022
########################################
######################
#Load packages########
######################

rm(list=ls(all=TRUE))

# packages
library(multcomp)
library(drc)
library(dplyr)
library(lmtest) 
library(Rcmdr)

setwd("") # enter your working directory here

####fixing estfun error to calculate robust standard errors
registerS3method("estfun", "drc", drc:::estfun.drc)
registerS3method("bread", "drc", drc:::bread.drc)

############### Import data

sal24 <- 
  readXL("Popenaias popeii Black Salinity Data Clean.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="gloch_12h_data", 
         stringsAsFactors=TRUE)

attach(sal24)

####################################
###four-parameter log-logistic model
####################################

###based on proportion dead
np1drc4 <- drm(Proportion_dead~conc, fct = LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50"))) 

modelFit(np1drc4)
summary(np1drc4)
plot(np1drc4, type="all")

###robust CI
ED(np1drc4, c(5, 10, 48), interval="delta", vcov=sandwich)#since data doesn't go to 0 or 100 need to calculate 5, 10 and 50% percentiles based on range of data


######################################
###three-parameter log-logistic model
######################################

###based on proportion dead
np1drc3 <- drm(Proportion_dead~conc, fct = LL.3(names = c("Slope","Upper Limit", "ED50"))) 

summary(np1drc3)
plot(np1drc3, type="all")

###robust CI
ED(np1drc3, c(5, 10, 48), interval="delta", vcov=sandwich)#since data doesn't go to 0 or 100 need to calculate 5, 10 and 50% percentiles based on range of data


#########################################
###two-parameter log-logistic model
#########################################

###based on proportion dead
np1drc2 <- drm(Proportion_dead~conc, fct = LL.2(names = c("Slope","ED50")))

summary(np1drc2)
plot(np1drc2, type="all")

###robust CI
ED(np1drc2, c(5, 10, 48), interval="delta", vcov=sandwich)#since data doesn't go to 0 or 100 need to calculate 5, 10 and 50% percentiles based on range of data


####################################
###four-parameter Weibull type 1
####################################

###based on proportion dead
np1drcW41 <- drm(Proportion_dead~conc, fct = W1.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50"))) 

modelFit(np1drcW41)
summary(np1drcW41)
plot(np1drcW41, type="all")

###robust CI
ED(np1drcW41, c(5, 10, 48), interval="delta", vcov=sandwich)#since data doesn't go to 0 or 100 need to calculate 5, 10 and 50% percentiles based on range of data


####################################
###four-parameter Weibull type 2
####################################

###based on proportion dead
np1drcW42 <- drm(Proportion_dead~conc, fct = W2.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50"))) 

modelFit(np1drcW42)
summary(np1drcW42)
plot(np1drcW42, type="all")

###robust CI
ED(np1drcW42, c(5, 10, 48), interval="delta", vcov=sandwich)#since data doesn't go to 0 or 100 need to calculate 5, 10 and 50% percentiles based on range of data


####################################
###three-parameter Weibull type 1
####################################

###based on proportion dead
np1drcW31 <- drm(Proportion_dead~conc, fct = W1.3(names = c("Slope", "Lower Limit", "ED50"))) 

modelFit(np1drcW31)
summary(np1drcW31)
plot(np1drcW31, type="all")

###robust CI
ED(np1drcW31, c(5, 10, 48), interval="delta", vcov=sandwich)#since data doesn't go to 0 or 100 need to calculate 5, 10 and 50% percentiles based on range of data


####################################
###three-parameter Weibull type 2
####################################

###based on proportion dead
np1drcW32 <- drm(Proportion_dead~conc, fct = W2.3(names = c("Slope", "Lower Limit", "ED50"))) 

modelFit(np1drcW32)
summary(np1drcW32)
plot(np1drcW32, type="all")

###robust CI
ED(np1drcW32, c(5, 10, 48), interval="delta", vcov=sandwich)#since data doesn't go to 0 or 100 need to calculate 5, 10 and 50% percentiles based on range of data


###########################################
###########################################
#Choosing a model
###########################################
###########################################

# Results Proportion_dead

plot(np1drc4, xlab="concentration", ylab="% viability", type="all", lty=1, lwd=2)
plot(np1drc3, add=TRUE, col="orange", lty=2, lwd=2)
plot(np1drc2, add=TRUE, col="yellow", lty=2, lwd=2) 
plot(np1drcW41, add=TRUE, col="pink", lty=2, lwd=2) 
plot(np1drcW42, add=TRUE, col="blue", lty=2, lwd=2) 
plot(np1drcW32, add=TRUE, col="red", lty=2, lwd=2) 
plot(np1drcW31, add=TRUE, col="green", lty=2, lwd=2) 

mselect(np1drc4, fctList = list(LL.2(), LL.3(), W2.3(), W2.4(), W1.3(), W1.4()))

####results_proportional survival 
# logLik        IC Lack of fit     Res var
# W1.3 31.17665 -54.35329  0.26478050 0.002199275
# LL.3 30.67215 -53.34431  0.19781857 0.002326076
# W1.4 31.17624 -52.35248  0.14805441 0.002356473
# LL.4 30.67974 -51.35948  0.10633340 0.002490125
# W2.3 28.38113 -48.76226  0.04955175 0.003000385
# LL.2 27.05620 -48.11239  0.04025359 0.003258989
# W2.4 28.39022 -46.78044  0.02310926 0.003211454

# Picked 3 parameter Weibull type 1 based on model fit and CI comparison

# Estimated effective doses
# Estimate Std. Error    Lower    Upper
# e:1:5  1.777399   0.025766 1.722481 1.832318
# e:1:10 1.891185   0.020299 1.847919 1.934452
# e:1:48 2.476386   0.055021 2.359111 2.593661


# 96h Juveniles -----------------------------------------------------------
########################################
##############96h Juveniles#############
# Tested: 06/23/2022
# Counted: 06/27/2022
# Collected: 06/01/2022
########################################

######################
#Load packages########
######################

rm(list=ls(all=TRUE))

# packages
library(multcomp)
library(drc)
library(dplyr)
library(lmtest) 
library(Rcmdr)

setwd("") # enter your working directory here

####fixing estfun error to calculate robust standard errors
registerS3method("estfun", "drc", drc:::estfun.drc)
registerS3method("bread", "drc", drc:::bread.drc)

############### Import data

sal96 <- 
  readXL("Popenaias popeii Black Salinity Data Clean.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="juv_96h_data", 
         stringsAsFactors=TRUE)

attach(sal96)

####################################
###four-parameter log-logistic model
####################################

###based on proportion dead
np1drc4 <- drm(Proportion_dead~conc, fct = LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50"))) 

modelFit(np1drc4)
summary(np1drc4)
plot(np1drc4, type="all")

###robust CI
ED(np1drc4, c(5, 10, 50), interval="delta", vcov=sandwich)#specify percentiles


######################################
###three-parameter log-logistic model
######################################

###based on proportion dead
np1drc3 <- drm(Proportion_dead~conc, fct = LL.3(names = c("Slope","Upper Limit", "ED50"))) 

summary(np1drc3)
plot(np1drc3, type="all")

###robust CI
ED(np1drc3, c(5, 10, 50), interval="delta", vcov=sandwich)#specify percentiles


#########################################
###two-parameter log-logistic model
#########################################

###based on proportion dead
np1drc2 <- drm(Proportion_dead~conc, fct = LL.2(names = c("Slope","ED50")))

summary(np1drc2)
plot(np1drc2, type="all")

###robust CI
ED(np1drc2, c(5, 10, 50), interval="delta", vcov=sandwich)#specify percentiles

####################################
###four-parameter Weibull type 1
####################################

###based on proportion dead
np1drcW41 <- drm(Proportion_dead~conc, fct = W1.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50"))) 

modelFit(np1drcW41)
summary(np1drcW41)
plot(np1drcW41, type="all")

###robust CI
ED(np1drcW41, c(5, 10, 50), interval="delta", vcov=sandwich)#specify percentiles

####################################
###four-parameter Weibull type 2
####################################

###based on proportion dead
np1drcW42 <- drm(Proportion_dead~conc, fct = W2.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50"))) 

modelFit(np1drcW42)
summary(np1drcW42)
plot(np1drcW42, type="all")

###robust CI
ED(np1drcW42, c(5, 10, 50), interval="delta", vcov=sandwich)#specify percentiles

####################################
###three-parameter Weibull type 1
####################################

###based on proportion dead
np1drcW31 <- drm(Proportion_dead~conc, fct = W1.3(names = c("Slope", "Lower Limit", "ED50"))) 

modelFit(np1drcW31)
summary(np1drcW31)
plot(np1drcW31, type="all")

###robust CI
ED(np1drcW31, c(5, 10, 50), interval="delta", vcov=sandwich)#specify percentiles

####################################
###three-parameter Weibull type 2
####################################

###based on proportion dead
np1drcW32 <- drm(Proportion_dead~conc, fct = W2.3(names = c("Slope", "Lower Limit", "ED50"))) 

modelFit(np1drcW32)
summary(np1drcW32)
plot(np1drcW32, type="all")

###robust CI
ED(np1drcW32, c(5, 10, 50), interval="delta", vcov=sandwich)#specify percentiles

###########################################
###########################################
#Choosing a model
###########################################
###########################################

# Results Proportion_dead

plot(np1drc4, xlab="concentration", ylab="% viability", type="all", lty=1, lwd=2)
plot(np1drc3, add=TRUE, col="orange", lty=2, lwd=2)
plot(np1drc2, add=TRUE, col="yellow", lty=2, lwd=2) 
plot(np1drcW41, add=TRUE, col="pink", lty=2, lwd=2) 
plot(np1drcW42, add=TRUE, col="blue", lty=2, lwd=2) 
plot(np1drcW32, add=TRUE, col="red", lty=2, lwd=2) 
plot(np1drcW31, add=TRUE, col="green", lty=2, lwd=2) 

mselect(np1drc4, fctList = list(LL.2(), LL.3(), W2.3(), W2.4(), W1.3(), W1.4()))

####results_proportion dead
# logLik        IC Lack of fit     Res var
# W2.3 40.99547 -73.99095 0.004872242 0.003161224
# LL.2 39.65131 -73.30262 0.003749446 0.003352496
# LL.4 41.48382 -72.96764 0.003638791 0.003181476
# W1.4 41.06094 -72.12188 0.002817582 0.003282711
# W2.4 40.88997 -71.77993 0.002539872 0.003324550
# LL.3 39.65904 -71.31808 0.002206401 0.003490184
# W1.3 39.49690 -70.99381 0.002002312 0.003532355

# Picked two-parameter log-logistic model (LL.2) based on combination of CIs and model comparison
# although W2.3 had slightly better IC and Res var, LL.2 had better Lack of fit and tighter CIs, so LL.2 was selected

# Estimated effective doses
# Estimate Std. Error    Lower    Upper
# e:1:5  5.678947   0.098969 5.475116 5.882777
# e:1:10 5.827110   0.077420 5.667661 5.986559
# e:1:50 6.285569   0.061521 6.158864 6.412274

