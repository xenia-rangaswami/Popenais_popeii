## Author: Xenia Rangaswami ##
## Created: 04/25/2023 ##
## Updated: ##

rm(list=ls(all=TRUE))

setwd("~/Dropbox/Mussel Lab/Thesis Project/R Code/Water Files")

## load packages
library(dataRetrieval)
library(lubridate)

######### LOGGER
## read in data
log <- read.csv("davis_salinity.csv",header=T)
log$date <- as.Date(log$date,format="%Y-%m-%d")

## keep only date and salinity columns
log <- log[,c(1,4)]

## add year and month-year
log$year <- year(log$date)
log$month.yr <- format(as.Date(log$date),"%m-%Y")

######### USGS GAGE
## import USGS gauge data from Black River at Harkey Crossing NR Malaga, NM - 08405400
cds <- c("00001","00002","00003") #min, mean, max
har <- readNWISdv("08405400","00480","2019-06-29","2023-04-25")
har <- har[,c(3,4)]
names(har) <- c("date","salinity_ppt")
threshold <- c(2.48,6.29,5.72,5.38,4.78,4.52,4,3) #'gl24','ju96','lr96','lc96','lr10','lc10','lj.GAM','as.GAM'

## add in year and month-year columns for plotting
har$year <- year(har$date)
har$month.yr <- format(as.Date(har$date),"%m-%Y")

## calculate date range; min, mean, and max salinity
# start year
min(log$year) # [1] 2020
min(har$year) # [1] 2019

# end year
max(log$year) # [1] 2021
max(har$year) # [1] 2023
# mean salinity
mean(log$salinity_ppt) # [1] 0.7788159
mean(har$salinity_ppt) # [1] 0.7870209

# SE salinity
se <- function(x) sd(x)/sqrt(length(x))
se(log$salinity_ppt) # [1] 0.0005890708
se(har$salinity_ppt) # [1] 0.001404271

# max salinity
max(log$salinity_ppt) # [1] 0.9663
max(har$salinity_ppt) # [1] 1

# year of max salinity
log$year[log$salinity_ppt == max(log$salinity_ppt)] # [1] 2021
har$year[har$salinity_ppt == max(har$salinity_ppt)] # [1] 2021

# min salinity
min(log$salinity_ppt) # [1] 0.62265
min(har$salinity_ppt) # [1] 0.3

# year of min salinity
log$year[log$salinity_ppt == min(log$salinity_ppt)] # [1] 2021
har$year[har$salinity_ppt == min(har$salinity_ppt)] # [1] 2021


# number of recorDs
nrow(log) # [1] 16152
nrow(har) # [1] 1148


############ CREATE PLOT ############ 
# thresholds
threshold <- c(2.48,6.29,5.72,5.38,4.78,4.52,4,3) #'gl24','ju96','lr96','lc96','lr10','lc10','lj.GAM','as.GAM'

# create variable for theme
y <-  theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black",linewidth=0.3),
            axis.ticks.length=unit(0.75, "line"),
            axis.text.x=element_text(size=15,colour="black",angle=0), axis.title.x=element_text(size=20,colour="black",margin=margin(t=15,r=0,b=10,l=0)),
            axis.text.y=element_text(size=15,colour="black"),
            axis.title.y = element_text(size=20,margin = margin(t = 0, r = 15, b = 0, l = 10)),
            axis.title.y.right = element_text(angle=90,size=20,margin=margin(t = 0, r = 10, b = 0, l = 15)), 
            legend.key.width=unit(2.75,"line"),
            legend.text=element_text(size=15),
            legend.title=element_text(size=15),
            legend.position="none")

# function for y-axis max
a <- ifelse(max(har$salinity_ppt) < threshold[[2]],7,max(har$salinity_ppt))

# plot
p2<-har%>%
  ggplot(aes(y=salinity_ppt, x = year))+
  coord_cartesian(ylim=c(0,a),xlim=c(2018,2023.5)) +
  scale_y_continuous(limits=c(0,a),breaks=seq(0,a,2)) +
  scale_x_continuous(breaks=seq(2018,2023,by=1)) +
  geom_boxplot(aes(y=salinity_ppt,group=year))+
  geom_hline(aes(yintercept = threshold[1],linetype = "24-h",color="BR glochidia"),linewidth=1) +
  geom_hline(aes(yintercept = threshold[2],linetype ="96-h",color="BR juvenile"),linewidth=1) +
  geom_hline(aes(yintercept = threshold[3],linetype ="96-h",color="LR adult"),linewidth=1) +
  geom_hline(aes(yintercept = threshold[4],linetype ="96-h",color="LC adult"),linewidth=1) +
  geom_hline(aes(yintercept = threshold[5],linetype ="10-d",color="LR adult"),linewidth=1) +
  geom_hline(aes(yintercept = threshold[6],linetype ="10-d",color="LC adult"),linewidth=1) +
  geom_hline(aes(yintercept = threshold[7],linetype ="GAM",color="BR live juveniles"),linewidth=1) +
  geom_hline(aes(yintercept = threshold[8],linetype ="GAM",color="BR attachment success"),linewidth=1) +
  scale_linetype_manual(breaks=c("24-h","96-h","10-d","GAM"),values=c("longdash","21","twodash","dashed"),name="Threshold") +
  scale_color_manual(breaks=c("BR glochidia","BR juvenile","LR adult","LC adult","BR live juveniles","BR attachment success"),values=c("red4","red2","blue2","blue4","cyan2","cyan4"),name="Population/stage") +
  labs(y = "Salinity (ppt)", x = "Year") +
  
  theme_bw() +
  y

p2
setwd("~/Desktop")
ggsave("Harkey Salinity.tiff",p2,height=5,width=5,dpi=300)

######## Plot using logger data
# plot
p2<-log%>%
  ggplot(aes(y=salinity_ppt, x = year))+
  coord_cartesian(ylim=c(0,a)) +
  scale_y_continuous(limits=c(0,a),breaks=seq(0,a,2)) +
  scale_x_continuous(breaks=seq(2018,2023,by=1)) +
  geom_boxplot(aes(y=salinity_ppt,group=year))+
  geom_hline(aes(yintercept = threshold[1],linetype = "24-h",color="BR glochidia"),linewidth=1) +
  geom_hline(aes(yintercept = threshold[2],linetype ="96-h",color="BR juvenile"),linewidth=1) +
  geom_hline(aes(yintercept = threshold[3],linetype ="96-h",color="LR adult"),linewidth=1) +
  geom_hline(aes(yintercept = threshold[4],linetype ="96-h",color="LC adult"),linewidth=1) +
  geom_hline(aes(yintercept = threshold[5],linetype ="10-d",color="LR adult"),linewidth=1) +
  geom_hline(aes(yintercept = threshold[6],linetype ="10-d",color="LC adult"),linewidth=1) +
  geom_hline(aes(yintercept = threshold[7],linetype ="GAM",color="BR live juveniles"),linewidth=1) +
  geom_hline(aes(yintercept = threshold[8],linetype ="GAM",color="BR attachment success"),linewidth=1) +
  scale_linetype_manual(breaks=c("24-h","96-h","10-d","GAM"),values=c("longdash","21","twodash","dashed"),name="Threshold") +
  scale_color_manual(breaks=c("BR glochidia","BR juvenile","LR adult","LC adult","BR live juveniles","BR attachment success"),values=c("red4","red2","blue2","blue4","cyan2","cyan4"),name="Population/stage") +
  labs(y = "Salinity (ppt)", x = "Year") +
  
  theme_bw() +
  y

p2
setwd("~/Desktop")
ggsave("Davis Salinity.tiff",p2,height=5,width=5,dpi=300)

