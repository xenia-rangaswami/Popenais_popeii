# Xenia Rangaswami #
# Created: 04/17/2023 #
# Updated:  04/19/2023#

rm(list=ls(all=TRUE))
setwd("~/Dropbox/Mussel Lab/Thesis Project/R Code/Water Files/TCEQ CSVs/Output")
# "C:/Users/xenia.rangaswami/Dropbox/Mussel Lab/Thesis Project/R Code/Water Files/TCEQ CSVs/Output"

# load packages
library(ggplot2)
library(lubridate)
library(tidyverse)
library(plotrix)

########## Salinity Exceedances ##########
########## Read in files
# create list of dataframes
filenames <- list.files(pattern="sal_chlor_",full.names=F)
c <- lapply(filenames,function(i){
  read.csv(i, header=T,stringsAsFactors=F)
})

# renaming dfs within list
filenames <- gsub(".csv","",filenames) # changing the end of each df name
filenames <- gsub("sal_chlor","sal",filenames)

# label each dataframe within list with TCEQ segment name
names(c) <- paste0(filenames)

# keep only date and salinity_ppt columns
c <- lapply(c, "[", c(3,8))

# format date & add year
c <- lapply(c, function(x) {x$date <- as.Date(x$date,format="%Y-%m-%d");return(x)})
c <- lapply(c, function(x) {x$year <- year(x$date);return(x)})
c <- lapply(c, function(x) {x$year <- ifelse(x$year > 2022, x$year-100,x$year);return(x)}) # correct for years prior to 1969

# omit NAs
c <- lapply(c,na.omit)

########## Create separate dataframes for each threshold
# create empty list
l <- list()

# create list of dataframe names
# LC50s and GAM breakpoints only (not including LC05 in this analysis)
dfs <- list('gl24','ju96','lr96','lc96','lr10','lc10','lj.GAM','as.GAM') # glochidia 24-h LC50, juvenile 96-h LC50, adult Laredo 96-h LC50, adult Lower Canyond 96-h LC50, adult Laredo 10-day LC50, adult Lower Canyons 10-day LC50, GAM live juvenile, GAM attachment success

# for loop to create item in list for each df
for(i in seq_along(dfs)){
  l[[i]] <- c
}

# re-name dfs within list
names(l) <- c(dfs[1:length(dfs)])

# add thresholds into each df
thr <- c(2.48,6.29,5.72,5.38,4.78,4.52,4,3)

for(i in 1:length(thr)){
  for(i in 1:length(l)) {
    for(j in 1:length(c)) {
      c[[j]]$thresh <- thr[[i]]
      l[[i]] <- c
    }
  }
  
}

########## Calculate exceedances for each threshold
## use nested lapply
l <- lapply(l,function(x){
  lapply(x,function(y){
    y$exc <- ifelse(y$salinity_ppt > y$thresh, 1,0);return(y)
  })
})

e <- as.data.frame(lapply(l,function(x) {lapply(lapply(x,'[[','exc'),sum)}))
f <- as.data.frame(lapply(l,function(x) {lapply(lapply(x,'[[','exc'),length)}))
g <- e/f

write.csv(g,"Salinity Exceedances Popenaias.csv")

########## Calculate date range; min, mean, and max salinity
# start year
for (i in c){
  print(min(i$year))
}

# end year
for (i in c){
  print(max(i$year))
}

# mean salinity
for (i in c){
  print(mean(i$salinity_ppt))
}

# SE salinity
se <- function(i) sd(i)/sqrt(length(i))
for (i in c){
  print(se(i$salinity_ppt))
}

# min salinity
for (i in c){
  print(min(i$salinity_ppt))
}

# year of min salinity
for (i in c){
  print(i$year[i$salinity_ppt==min(i$salinity_ppt)])
}

# max salinity
for (i in c){
  print(max(i$salinity_ppt))
}

# year of max salinity
for (i in c){
  print(i$year[i$salinity_ppt==max(i$salinity_ppt)])
}

# number of records
for (i in c){
  print(nrow(i))
}
