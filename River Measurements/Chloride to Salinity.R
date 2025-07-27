####### Author: Xenia Rangaswami
####### Created: 03/21/2023
####### Updated: 

rm(list=ls())
setwd('~/Dropbox/Mussel Lab/Thesis Project/R Code/Water Files/TCEQ CSVs')

################ read in files
# create list of dataframes
filenames <- list.files(pattern="chloride_",full.names=F)
c <- lapply(filenames,function(i){
  read.csv(i, header=T,stringsAsFactors=F)
})

# renaming dfs within list
filenames <- gsub(".csv","",filenames) # changing the end of each df name
filenames <- gsub("chloride","sal_chlor",filenames)

# label each dataframe within list with TCEQ segment name
names(c) <- paste0(filenames)

# keep only necessary columns: station ID, start date, parameter code, value
c <- lapply(c, "[", c(2,4,11,12))

# re-name Value column
c <- lapply(c,function(x){names(x)[4] <- "chloride_mg.l";return(x)})

# format date
c <- lapply(c,function(x){names(x)[2] <- "date";return(x)})
c <- lapply(c,function(x){x$date <- as.Date(x$date,format="%m/%d/%y");return(x)})

################ converting chloride (mg/l) to:
# a: chloride (ppt)
c <- lapply(c,function(x) {x$chloride_ppt <- as.numeric(x$chloride_mg.l)/1000;return(x)})

# b: salinity (mg/l)
c <- lapply(c,function(x) {x$salinity_mg.l <- as.numeric(x$chloride_mg.l)*0.0018066*1000;return(x)})

# c: salinity (ppt)
c <- lapply(c,function(x) {x$salinity_ppt <- as.numeric(x$salinity_mg.l)/1000;return(x)})

################ delete NAs
c <- lapply(c,na.omit)

################ write out csv files
setwd('~/Dropbox/Mussel Lab/Thesis Project/R Code/Water Files/TCEQ CSVs/Output')
write <- sapply(names(c), 
                function (x) write.csv(c[[x]], file=paste(x, "csv", sep=".")))

############### calculations for each segment
sapply(c,function(x){summary(x$salinity_ppt)})
# 04/21/2023
# sal_chlor_2302 sal_chlor_2303 sal_chlor_2304 sal_chlor_2305 sal_chlor_2306 sal_chlor_2307
# Min.         0.0289056      0.0858135     0.00027099      0.0054198     0.00523914      0.0018066
# 1st Qu.      0.2348580      0.1777243     0.17162700      0.0252924     0.14814120      0.3450606
# Median       0.2836362      0.2104689     0.20233920      0.0686508     0.28273290      0.5726922
# Mean         0.3176911      0.2091236     0.20317085      0.1663771     0.39364199      0.7422600
# 3rd Qu.      0.3432540      0.2402778     0.23485800      0.2041458     0.57449880      1.0631841
# Max.        37.5772800      0.4046784     0.83284260      2.4027780     4.04678400      6.4314960
# sal_chlor_2309 sal_chlor_2310 sal_chlor_2311
# Min.        0.01625940      0.9737574      0.0036132
# 1st Qu.     0.02529240      1.8878970      5.0133150
# Median      0.02709900      2.5292400      7.2805980
# Mean        0.02970263      2.7203355      7.6523901
# 3rd Qu.     0.02989923      3.1976820      9.5749800
# Max.        0.18969300      8.9968680     29.8089000
