rm(list=ls(all=TRUE))

# packages
library(multcomp)
library(drc)
library(dplyr)
library(lmtest) 

setwd("~/Dropbox/Mussel Lab/Thesis Project/R Code/Water Files")

####fixing estfun error to calculate robust standard errors
registerS3method("estfun", "drc", drc:::estfun.drc)
registerS3method("bread", "drc", drc:::bread.drc)

##### Comparing LC05s and LC50s - Ratio Method #####

# STEP 0: read in data
est <- read.csv("popenaias_salinity_estimates.csv",header=T)

# STEP 2: create separate dfs for LC and SE estimate values
# LC05
l05 <- est$est[est$thresh=="LC05"]

# LC50
l50 <- est$est[est$thresh=="LC50"]
l50 <- l50[-1] # remove 12-h

# SE for LC05
se05 <- est$SE[est$thresh=="LC05"]

# SE for LC50
se50 <- est$SE[est$thresh=="LC50"]
se50 <- se50[-1] # remove 12-h

# STEP 3: generate matrices for all combinations of LTs and SEs
LC05.combos <- combn(l05,2)
se05.combos <- combn(se05,2)

LC50.combos <- combn(l50,2)
se50.combos <- combn(se50,2)


# STEP 4: loop to compare each unique combo of LC05s and LC50s
# LC05
bonf <- 1 - (0.05/3) # making 3 comparisons
ratio05 <- NULL
for (i in 1:(length(LC05.combos)/2)) {
  LC05 <- t(LC05.combos[,i])
  SE05 <- t(se05.combos[,i])
  m05 <- comped(LC05.combos[,i], se05.combos[,i], log=FALSE, 
                level=bonf, operator = "/")
  ratio05 <- rbind(ratio05, data.frame(LC05,SE05,m05))
}
loop_names <- cbind("LC05 1", "LC05 2", "SE 1", "SE 2", "Estimate", 
                    "Std..Error", "Lower", "Upper")
colnames(ratio05) <- loop_names

# LC50
bonf <- 1 - (0.05/15) # making 15 comparisons
ratio50 <- NULL
for (i in 1:(length(LC50.combos)/2)) {
  LC50 <- t(LC50.combos[,i])
  SE50 <- t(se50.combos[,i])
  m50 <- comped(LC50.combos[,i], se50.combos[,i], log=FALSE, 
                level=bonf, operator = "/")
  ratio50 <- rbind(ratio50, data.frame(LC50,SE50,m50))
}
loop_names <- cbind("LC50 1", "LC50 2", "SE 1", "SE 2", "Estimate", 
                    "Std..Error", "Lower", "Upper")
colnames(ratio50) <- loop_names


# STEP 5: write out csv files
write.csv(x = ratio05, file = "Popenaias_CI_Ratios_LC05.csv")
write.csv(x = ratio50, file = "Popenaias_CI_Ratios_LC50.csv")

#if 1 is not bound within CIs then LC50 or LC05s are not the same


