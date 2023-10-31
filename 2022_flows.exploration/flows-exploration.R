#####################################################################

# This is the script for the 2023 data exploration for time series
# relationships between abiotic parameters and fish passage

# USGS field station from data retrieval package in R:
# https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html

library(lubridate) # for date formatting--time series
library(dataRetrieval)
# unpacking for use--pulling data from specific stations

# Rediversion canal site:
# https://waterdata.usgs.gov/nwis/uv?site_no=02171645

siteNumber <- "02171645"
parameterCd <- c("00010","00060","00300")  # Temperature, discharge, and DO
statCd <- c("00001","00002","00003")  # Maximum, Minimum, and Mean measurements
startDate <- "1999-01-29"
endDate <- "2023-05-09"

tempdisdo <- readNWISdv(siteNumber, parameterCd, 
                        startDate, endDate, statCd=statCd)  

SRRC <- tempdisdo[c(-1,-2,-5,-7,-9,-11,-13,-15,-17,-19,-21)]
# getting rid of the unnecessary columns (approved/pending, site # repeats, agency property..)


library(dplyr)

head(SRRC)
# SO (in order), my columns NOW are: 
# Date, Temp (max, min, mean), Discharge (max, min, mean), and DO (max, min, mean)


renamed.SRRC <- rename(SRRC, "Water.temp.max" = "X_00010_00001", 
                     "Water.temp.min" = "X_00010_00002",
                     "Water.temp.mean" = "X_00010_00003",
                     "Discharge.max" = "X_00060_00001",
                     "Discharge.min" = "X_00060_00002",
                     "Discharge.mean" = "X_00060_00003",
                     "DO.max" = "X_00300_00001",
                     "DO.min" = "X_00300_00002",
                     "DO.mean" = "X_00300_00003")

# Used dplyr to rename these, as appropriate

# Creating the Month column to filter out months with no operations..
is.Date(renamed.SRRC$Date)
# CHECK

renamed.SRRC$Month <- format(as.Date(renamed.SRRC$Date), "%m")

extracted.SRRC<-filter(renamed.SRRC, Month<=("05"))
# Filtered off-season months out of this data pull.

# sending this out so I can match up with my FL attraction flows and counts:

write.csv(extracted.SRRC,"C:/Users/Kyle/Desktop/flows-exploration-main/2022_flows.exploration/Daily.abiotics.csv", row.names = TRUE)

# I took these values, found the overlapping period with our operational
# data ("Daily.flows.vs.counts.xlsx"), and input the three measures of each
# parameter (temp, discharge, DO) for each day in the entire series. This file
# was reformatted as the "full.parameters" file.

# Full.parameters <- read.csv("C:/Users/Kyle/Desktop/flows-exploration-main/2022_flows.exploration/Full.parameters.csv")
# AT HOME; just need to update the attn. flows if we ever want to model with that

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### NOW FOR GRAPHICS ###

Full.parameters<-Full_parameters

head(Full.parameters)
summary(Full.parameters)
# several things here:
# Date is reading as character...and we have a million NAs for temps/DOs/discharges
# Mostly this seems due to how USGS manages the data (e.g., no max/min discharge 
# in recent years, and temp/DO weren't recorded early in this series)

#-----------------------------------------skip--------------------------------
# ALL this is just a reminder that if you have commas, R will read as character
# and that creates a lot of these-type issues:
#Full.parameters$BLH_passed<-gsub(",","",Full.parameters$BLH_passed)
# fixed herring
#Full.parameters$AMS_passed<-gsub(",","",Full.parameters$AMS_passed)
# fixed shad
#Full.parameters$Discharge_mean<-gsub(",","",Full.parameters$Discharge_mean)
# fixed average discharge
#------------------------------------------------------------------------------

# NOW, to change the date formatting..

is.Date(Full.parameters$Date)
# FALSE
Full.parameters$Date<-as.Date(Full.parameters$Date, "%m/%d/%Y" )
is.Date(Full.parameters$Date)
# TRUE

Full.parameters$Month_day <- format(as.Date(Full.parameters$Date), "%m/%d")
# Created the Month+Day column that I can use (hopefully)
# to stack multi-years

is.Date(Full.parameters$Month_day)
# FALSE, naturally...

Full.parameters$Month_day<-as.Date(Full.parameters$Month_day, "%m/%d" )
is.Date(Full.parameters$Month_day)
# TRUE...sweet

# PUlling out "YEAR" as a factor for graphics separation.

Full.parameters$YEAR <- format(as.Date(Full.parameters$Date), "%Y")
Full.parameters$YEAR <- as.factor(Full.parameters$YEAR)
is.factor(Full.parameters$YEAR)
# CHECK.

summary(Full.parameters)


subset<-Full.parameters[c(1,2,3,4,8,11,14,15,16)]
# pulling a subset of data that may be of interest, and trying that..

summary(subset)
# Great: all look numeric (NOW) AND...Dates are reading appropriately

## NOW, from the DFP table, it looks like 2009-2011 were really decent passage years
## for herring and shad. I'll filter for just these years, then try to depict them
## as the "stacked years" graphic.

# Creating some datasets for different graphic purposes:
# Decent for multi-year graphic, individual years for DO, TEMP, Discharge plotted  

library(dplyr)

Decent<-filter(subset,subset$Date >= "2009-02-09" & subset$Date <= "2011-04-29")

# Interestingly, there was no temp or DO measurements prior to 2007

# I have mean discharge and passage counts for all years in this range,
# but I can't plot in the same way as 2007-2022 (with temp...)

ninetynine<-filter(subset, YEAR=="1999")
twothousand<-filter(subset, YEAR=="2000")
one<-filter(subset,YEAR=="2001")
two<-filter(subset, YEAR=="2002")
three<-filter(subset, YEAR=="2003")
four<-filter(subset, YEAR=="2004")
five<-filter(subset, YEAR=="2005")
six<-filter(subset, YEAR=="2006")

# NOW for all those with full abiotic measurements :

seven<-filter(subset, YEAR=="2007")
eight<-filter(subset, YEAR=="2008")
o.nine<-filter(subset, YEAR=="2009")
ten<-filter(subset, YEAR=="2010")
eleven<-filter(subset, YEAR=="2011")
twelve<-filter(subset, YEAR=="2012")
thirteen<-filter(subset, YEAR=="2013")
fourteen<-filter(subset, YEAR=="2014")
fifteen<-filter(subset, YEAR=="2015")
sixteen<-filter(subset, YEAR=="2016")
seventeen<-filter(subset, YEAR=="2017")
eighteen<-filter(subset, YEAR=="2018")
nineteen<-filter(subset, YEAR=="2019")
twenty<-filter(subset, YEAR=="2020")
twentyone<-filter(subset, YEAR=="2021")
twentytwo<-filter(subset, YEAR=="2022")
twentythree<-filter(subset, YEAR=="2023")

library("viridis")
# so, supposedly Rcolorbrewer cannot visualize all 15 years; I'm going to try
# a scale from the viridis package instead:

library(scales)
# for the comma-formatting of y-axis

library(tidyverse)
library(ggplot2)

# This basically recreates the stacked-years passage graphic:

# This "seems" to be okay...full-graphic...a little busy..

Paired.discharge <- ggplot(Decent, aes(x=Month_day)) +
  geom_line(aes(y=BLH_passed * 1 / 4 , color=YEAR), size=1.25) +
  geom_line(aes(y=Discharge_mean, color=YEAR), size=1.75,linetype=3,alpha=1) +
  scale_color_viridis(discrete=TRUE, option="E", alpha=1) +
  labs(color='') +
  scale_x_date(date_breaks="week", date_labels="%m/%d",
              limits=as.Date(c('2022-02-07','2022-04-30')), 
              expand=c(0,0)) +
  scale_y_continuous(name=expression("Mean Daily Discharge(CFS)"),
                     sec.axis = sec_axis(~.* 4 / 1, name="Blueback Herring Passed (#)",
                                         breaks=seq(0,100000,20000),label=comma),
                    breaks = seq(0,25000,5000), label=comma, expand=c(0.01,0.01), limits=c(0,25000)) + 

theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size=10, family = "serif")) +
  theme(legend.position = "top", legend.key.width=unit(1.5,"cm")) +
  guides(col=guide_legend(nrow=1))
  theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())
  
Herring.pass.with.discharge <- Paired.discharge + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
    theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
    theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
    theme(axis.text.x= element_text(family = "serif", color="black", angle=0)) +
    theme(axis.text.y= element_text(family = "serif", color="black")) +
    theme(axis.title.x= element_blank())
  
Herring.pass.with.discharge 




### 2008 ###

Eight <- ggplot(eight, aes(x=Month_day)) +
  geom_line(aes(y=BLH_passed * 1 / 4 , color="Herring count"), size=1.25) +
  geom_line(aes(y=AMS_passed * 1 / 4 , color="Shad count"), size=1.25) +
  geom_line(aes(y=Discharge_mean, color="Discharge"), size=1.25,linetype=3,alpha=1) +
  labs(color='') +
  scale_x_date(date_breaks="week", date_labels="%m/%d",
               limits=as.Date(c('2023-02-01','2023-05-01')), 
               expand=c(0,0)) +
  scale_y_continuous(name=expression("Mean Daily Discharge (CFS), Water Temperature (C) x 10^3"),
                     sec.axis = sec_axis(~.* 4 / 1, name="Diadromous Fish Passed (#)",
                                         breaks=seq(0,100000,20000),label=comma),
                     breaks = seq(0,25000,5000), label=comma, expand=c(0.01,0.01), limits=c(0,25000)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size=10, family = "serif")) +
  theme(legend.position = "top", legend.key.width=unit(1.5,"cm")) +
  guides(col=guide_legend(nrow=1))
  theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())

eight.pass.w.discharge <- Eight + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black")) +
  theme(axis.title.x= element_blank()) +
  annotate("text", x=as.Date("02/07", "%m/%d"),y=22000, label="2008",
           family="serif", size=6)


G8 <- eight.pass.w.discharge + 
  geom_line(aes(y=Water.temp_mean * 1000, color="Temperature"), size=1.25) +
  scale_color_manual(values=c("Herring count"="dodgerblue",
                              "Shad count"="chocolate",
                              "Discharge"="black",
                              "Temperature"="black")) 

G8

# The 2008 season was a severe drought, such that we could not operate for most days in the season--
# see Al's Counts vs Flows excel file for more details.


### 2009 ### 

Nine <- ggplot(o.nine, aes(x=Month_day)) +
  geom_line(aes(y=BLH_passed * 1 / 4 , color="Herring count"), size=1.25) +
  geom_line(aes(y=AMS_passed * 1 / 4 , color="Shad count"), size=1.25) +
  geom_line(aes(y=Discharge_mean, color="Discharge"), size=1.25,linetype=3,alpha=1) +
  labs(color='') +
  scale_x_date(date_breaks="week", date_labels="%m/%d",
               limits=as.Date(c('2023-02-01','2023-05-01')), 
               expand=c(0,0)) +
  scale_y_continuous(name=expression("Mean Daily Discharge (CFS), Water Temperature (C) x 10^3"),
                     sec.axis = sec_axis(~.* 4 / 1, name="Fish Passed (#)",
                                         breaks=seq(0,100000,20000),label=comma),
                     breaks = seq(0,25000,5000), label=comma, expand=c(0.01,0.01), limits=c(0,25000)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size=10, family = "serif")) +
  theme(legend.position = "top", legend.key.width=unit(1.5,"cm")) +
  guides(col=guide_legend(nrow=1))
  theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())

Nine.pass.w.discharge <- Nine + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black")) +
  theme(axis.title.x= element_blank()) +
  annotate("text", x=as.Date("02/07", "%m/%d"),y=22000, label="2009",
           family="serif", size=6)


G9 <- Nine.pass.w.discharge + 
  # no points!: geom_point(aes(y=Water.temp_mean * 1000, color="Temperature"), shape="diamond",size=2,alpha=1) +
  geom_line(aes(y=Water.temp_mean * 1000, color="Temperature"), size=1.25) +
  scale_color_manual(values=c("Herring count"="dodgerblue",
                              "Shad count"="chocolate",
                              "Discharge"="black",
                              "Temperature"="black"))
  

# FULL bars
G9.recent <- G9 + 
  geom_rect(data=o.nine,aes(xmin=(as.Date("02/20", "%m/%d")), xmax=(as.Date("02/23", "%m/%d")),
                            fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE) +
  geom_rect(data=o.nine,aes(xmin=(as.Date("03/10", "%m/%d")), xmax=(as.Date("03/11", "%m/%d")),
                            fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE)

G9.recent

# dot-dash lines:
# G9.new <- G9 + geom_vline(xintercept=(as.Date("02/20","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("02/23","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("03/10","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("03/11","%m/%d")), color="red", linetype="dotdash") 

# G9.new


### 2010 ### 

Ten <- ggplot(ten, aes(x=Month_day)) +
  geom_line(aes(y=BLH_passed * 1 / 4 , color="Herring count"), size=1.25) +
  geom_line(aes(y=AMS_passed * 1 / 4 , color="Shad count"), size=1.25) +
  geom_line(aes(y=Discharge_mean, color="Discharge"), size=1.25,linetype=3,alpha=1) +
  labs(color='') +
  scale_x_date(date_breaks="week", date_labels="%m/%d",
               limits=as.Date(c('2023-02-01','2023-05-01')), 
               expand=c(0,0)) +
  scale_y_continuous(name=expression("Mean Daily Discharge (CFS), Water Temperature (C) x 10^3"),
                     sec.axis = sec_axis(~.* 4 / 1, name="Fish Passed (#)",
                                         breaks=seq(0,100000,20000),label=comma),
                     breaks = seq(0,25000,5000), label=comma, expand=c(0.01,0.01), limits=c(0,25000)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size=10, family = "serif")) +
  theme(legend.position = "top", legend.key.width=unit(1.5,"cm")) +
  guides(col=guide_legend(nrow=1))
  theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())

Ten.pass.w.discharge <- Ten + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black")) +
  theme(axis.title.x= element_blank()) +
  annotate("text", x=as.Date("02/07", "%m/%d"),y=22000, label="2010",
           family="serif", size=6)


G10 <- Ten.pass.w.discharge + 
  geom_line(aes(y=Water.temp_mean * 1000, color="Temperature"), size=1.25) +
  scale_color_manual(values=c("Herring count"="dodgerblue",
                              "Shad count"="chocolate",
                              "Discharge"="black",
                              "Temperature"="black"))

G10

# 2010 did not have more than one day inoperable, here and there throughout season...not worth graphing.
# However, I'm not sure why the season started so late...


### 2011 ### 

Eleven <- ggplot(eleven, aes(x=Month_day)) +
  geom_line(aes(y=BLH_passed * 1 / 4 , color="Herring count"), size=1.25) +
  geom_line(aes(y=AMS_passed * 1 / 4 , color="Shad count"), size=1.25) +
  geom_line(aes(y=Discharge_mean, color="Discharge"), size=1.25,linetype=3,alpha=1) +
  labs(color='') +
  scale_x_date(date_breaks="week", date_labels="%m/%d",
               limits=as.Date(c('2023-02-01','2023-05-01')), 
               expand=c(0,0)) +
  scale_y_continuous(name=expression("Mean Daily Discharge (CFS), Water Temperature (C) x 10^3"),
                     sec.axis = sec_axis(~.* 4 / 1, name="Fish Passed (#)",
                                         breaks=seq(0,100000,20000),label=comma),
                     breaks = seq(0,25000,5000), label=comma, expand=c(0.01,0.01), limits=c(0,25000)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size=10, family = "serif")) +
  theme(legend.position = "top", legend.key.width=unit(1.5,"cm")) +
  guides(col=guide_legend(nrow=1))
  theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())

Eleven.pass.w.discharge <- Eleven + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black")) +
  theme(axis.title.x= element_blank()) +
  annotate("text", x=as.Date("02/07", "%m/%d"),y=22000, label="2011",
           family="serif", size=6)


G11 <- Eleven.pass.w.discharge +
  geom_line(aes(y=Water.temp_mean * 1000, color="Temperature"), size=1.25) +
  scale_color_manual(values=c("Herring count"="dodgerblue",
                              "Shad count"="chocolate",
                              "Discharge"="black",
                              "Temperature"="black"))
  
  
G11.new <- G11 + 
  geom_rect(data=eleven,aes(xmin=(as.Date("02/19", "%m/%d")), xmax=(as.Date("02/21", "%m/%d")),
                            fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE) 


# G11.new <- G11 + geom_vline(xintercept=(as.Date("02/19","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("02/21","%m/%d")), color="red", linetype="dotdash")

G11.new


# SO, IF we want to stack these figures:

# I would need to get rid of the 2010 and 2011 legends, probably get rid of
# x-axis for 2009 and 2010, and perhaps only keep the y-axis labels for 2010:
# theoretically allowing each to sit somewhat centered between the three graphs.

G9.up <- G9.new + theme(axis.text.x= element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.y.right = element_blank())

G10.up <- G10 + theme(legend.position = "none") +
  theme(axis.text.x = element_blank())

G11.up <- G11.new + theme(legend.position = "none") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.y.right = element_blank())


library(cowplot)
# For "stacked" graphics...

Nine.to.eleven<-plot_grid(G9.up,G10.up,G11.up,ncol=1, align="v",axis='tblr',
          labels=c('2009', '2010', '2011'), label_size = 18, vjust=c(4.7,2,2),hjust=-1.5)



library('Cairo')

ggsave(Nine.to.eleven, filename = 'Herring+Shad.dis.temp.9to11.png', dpi = 300, type = 'cairo',
       width = 8.5, height = 11, units = 'in')

# INTERESTINGLY: this got rid of my NA's (missing values)
# which made this figure look better than the annual report stacked
# herring passage graphic. Adopt this for the next DFP!


### 2012 ###

Twelve <- ggplot(twelve, aes(x=Month_day)) +
  geom_line(aes(y=BLH_passed * 1 / 4 , color="Herring count"), size=1.25) +
  geom_line(aes(y=AMS_passed * 1 / 4 , color="Shad count"), size=1.25) +
  geom_line(aes(y=Discharge_mean, color="Discharge"), size=1.25,linetype=3,alpha=1) +
  labs(color='') +
  scale_x_date(date_breaks="week", date_labels="%m/%d",
               limits=as.Date(c('2023-02-01','2023-05-01')), 
               expand=c(0,0)) +
  scale_y_continuous(name=expression("Mean Daily Discharge (CFS), Water Temperature (C) x 10^3"),
                     sec.axis = sec_axis(~.* 4 / 1, name="Diadromous Fish Passed (#)",
                                         breaks=seq(0,100000,20000),label=comma),
                     breaks = seq(0,25000,5000), label=comma, expand=c(0.01,0.01), limits=c(0,25000)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size=10, family = "serif")) +
  theme(legend.position = "top", legend.key.width=unit(1.5,"cm")) +
  guides(col=guide_legend(nrow=1))
  theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())
  

twelve.pass.w.discharge <- Twelve + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black")) +
  theme(axis.title.x= element_blank()) +
  annotate("text", x=as.Date("02/07", "%m/%d"),y=22000, label="2012",
           family="serif", size=6) 

G12 <- twelve.pass.w.discharge + 
  geom_line(aes(y=Water.temp_mean * 1000, color="Temperature"), size=1.25) +
  scale_color_manual(values=c("Herring count"="dodgerblue",
                              "Shad count"="chocolate",
                              "Discharge"="black",
                              "Temperature"="black"))

G12

G12.new <- G12 + 
  geom_rect(data=twelve,aes(xmin=(as.Date("04/05", "%m/%d")), xmax=(as.Date("04/25", "%m/%d")),
                            fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE) 
G12.new



### 2013 ###

Thirteen <- ggplot(thirteen, aes(x=Month_day)) +
  geom_line(aes(y=BLH_passed * 1 / 4 , color="Herring count"), size=1.25) +
  geom_line(aes(y=AMS_passed * 1 / 4 , color="Shad count"), size=1.25) +
  geom_line(aes(y=Discharge_mean, color="Discharge"), size=1.25,linetype=3,alpha=1) +
  labs(color='') +
  scale_x_date(date_breaks="week", date_labels="%m/%d",
               limits=as.Date(c('2023-02-01','2023-05-01')), 
               expand=c(0,0)) +
  scale_y_continuous(name=expression("Mean Daily Discharge (CFS), Water Temperature (C) x 10^3"),
                     sec.axis = sec_axis(~.* 4 / 1, name="Diadromous Fish Passed (#)",
                                         breaks=seq(0,100000,20000),label=comma),
                     breaks = seq(0,25000,5000), label=comma, expand=c(0.01,0.01), limits=c(0,25000)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size=10, family = "serif")) +
  theme(legend.position = "top", legend.key.width=unit(1.5,"cm")) +
  guides(col=guide_legend(nrow=1))
  theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())

thirteen.pass.w.discharge <- Thirteen + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black")) +
  theme(axis.title.x= element_blank()) +
  annotate("text", x=as.Date("03/03", "%m/%d"),y=22000, label="2013",
           family="serif", size=6) 


G13 <- thirteen.pass.w.discharge + 
  geom_line(aes(y=Water.temp_mean * 1000, color="Temperature"), size=1.25) +
  scale_color_manual(values=c("Herring count"="dodgerblue",
                              "Shad count"="chocolate",
                              "Discharge"="black",
                              "Temperature"="black"))

G13

G13.new <- G13 + 
  geom_rect(data=thirteen,aes(xmin=(as.Date("02/08", "%m/%d")), 
                              xmax=(as.Date("02/27", "%m/%d")),
                              fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE) +
  geom_rect(data=thirteen,aes(xmin=(as.Date("03/07", "%m/%d")), 
                              xmax=(as.Date("04/01", "%m/%d")),
                              fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE) +
  geom_rect(data=thirteen,aes(xmin=(as.Date("04/27", "%m/%d")), 
                              xmax=(as.Date("05/15", "%m/%d")),
                              fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE)

  
G13.new  
  
  
#  geom_vline(xintercept=(as.Date("02/08","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("02/27","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("03/07","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("04/01","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("04/27","%m/%d")), color="red", linetype="dotdash") 



### 2014 ###

Fourteen <- ggplot(fourteen, aes(x=Month_day)) +
  geom_line(aes(y=BLH_passed * 1 / 4 , color="Herring count"), size=1.25) +
  geom_line(aes(y=AMS_passed * 1 / 4 , color="Shad count"), size=1.25) +
  geom_line(aes(y=Discharge_mean, color="Discharge"), size=1.25,linetype=3,alpha=1) +
  labs(color='') +
  scale_x_date(date_breaks="week", date_labels="%m/%d",
               limits=as.Date(c('2023-02-01','2023-05-01')), 
               expand=c(0,0)) +
  scale_y_continuous(name=expression("Mean Daily Discharge (CFS), Water Temperature (C) x 10^3"),
                     sec.axis = sec_axis(~.* 4 / 1, name="Diadromous Fish Passed (#)",
                                         breaks=seq(0,100000,20000),label=comma),
                     breaks = seq(0,25000,5000), label=comma, expand=c(0.01,0.01), limits=c(0,25000)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size=10, family = "serif")) +
  theme(legend.position = "top", legend.key.width=unit(1.5,"cm")) +
  guides(col=guide_legend(nrow=1))
  theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())

fourteen.pass.w.discharge <- Fourteen + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black")) +
  theme(axis.title.x= element_blank()) +
  annotate("text", x=as.Date("03/29", "%m/%d"),y=24000, label="2014",
           family="serif", size=6) 


G14 <- fourteen.pass.w.discharge + 
  geom_line(aes(y=Water.temp_mean * 1000, color="Temperature"), size=1.25) +
  scale_color_manual(values=c("Herring count"="dodgerblue",
                              "Shad count"="chocolate",
                              "Discharge"="black",
                              "Temperature"="black"))

G14

G14.new <- G14 + 
  geom_rect(data=fourteen,aes(xmin=(as.Date("02/01", "%m/%d")), 
                              xmax=(as.Date("03/24", "%m/%d")),
                              fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE)
G14.new


### 2015 ###

Fifteen <- ggplot(fifteen, aes(x=Month_day)) +
  geom_line(aes(y=BLH_passed * 1 / 4 , color="Herring count"), size=1.25) +
  geom_line(aes(y=AMS_passed * 1 / 4 , color="Shad count"), size=1.25) +
  geom_line(aes(y=Discharge_mean, color="Discharge"), size=1.25,linetype=3,alpha=1) +
  labs(color='') +
  scale_x_date(date_breaks="week", date_labels="%m/%d",
               limits=as.Date(c('2023-02-01','2023-05-01')), 
               expand=c(0,0)) +
  scale_y_continuous(name=expression("Mean Daily Discharge (CFS), Water Temperature (C) x 10^3"),
                     sec.axis = sec_axis(~.* 4 / 1, name="Diadromous Fish Passed (#)",
                                         breaks=seq(0,100000,20000),label=comma),
                     breaks = seq(0,25000,5000), label=comma, expand=c(0.01,0.01), limits=c(0,25000)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size=10, family = "serif")) +
  theme(legend.position = "top", legend.key.width=unit(1.5,"cm")) +
  guides(col=guide_legend(nrow=1))
  theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())
  

fifteen.pass.w.discharge <- Fifteen + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black")) +
  theme(axis.title.x= element_blank()) +
  annotate("text", x=as.Date("02/07", "%m/%d"),y=22000, label="2015",
             family="serif", size=6)  


G15 <- fifteen.pass.w.discharge + 
  geom_line(aes(y=Water.temp_mean * 1000, color="Temperature"), size=1.25) +
  scale_color_manual(values=c("Herring count"="dodgerblue",
                              "Shad count"="chocolate",
                              "Discharge"="black",
                              "Temperature"="black"))
  
  
G15

G15.new <- G15 + 
  geom_rect(data=fifteen,aes(xmin=(as.Date("03/14", "%m/%d")), 
                              xmax=(as.Date("03/16", "%m/%d")),
                              fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE) +
  geom_rect(data=fifteen,aes(xmin=(as.Date("04/29", "%m/%d")), 
                              xmax=(as.Date("05/15", "%m/%d")),
                              fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE)
  
#  geom_vline(xintercept=(as.Date("03/14","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("03/16","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("04/29","%m/%d")), color="red", linetype="dotdash")

G15.new


### 2016 ###

Sixteen <- ggplot(sixteen, aes(x=Month_day)) +
  geom_line(aes(y=BLH_passed * 1 / 4 , color="Herring count"), size=1.25) +
  geom_line(aes(y=AMS_passed * 1 / 4 , color="Shad count"), size=1.25) +
  geom_line(aes(y=Discharge_mean, color="Discharge"), size=1.25,linetype=3,alpha=1) +
  labs(color='') +
  scale_x_date(date_breaks="week", date_labels="%m/%d",
               limits=as.Date(c('2023-02-01','2023-05-01')), 
               expand=c(0,0)) +
  scale_y_continuous(name=expression("Mean Daily Discharge (CFS), Water Temperature (C) x 10^3"),
                     sec.axis = sec_axis(~.* 4 / 1, name="Diadromous Fish Passed (#)",
                                         breaks=seq(0,100000,20000),label=comma),
                     breaks = seq(0,25000,5000), label=comma, expand=c(0.01,0.01), limits=c(0,25000)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size=10, family = "serif")) +
  theme(legend.position = "top", legend.key.width=unit(1.5,"cm")) +
  guides(col=guide_legend(nrow=1))
  theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())

sixteen.pass.w.discharge <- Sixteen + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black")) +
  theme(axis.title.x= element_blank()) +
  annotate("text", x=as.Date("02/07", "%m/%d"),y=22000, label="2016",
           family="serif", size=6)


G16 <- sixteen.pass.w.discharge + 
  geom_line(aes(y=Water.temp_mean * 1000, color="Temperature"), size=1.25) +
  scale_color_manual(values=c("Herring count"="dodgerblue",
                              "Shad count"="chocolate",
                              "Discharge"="black",
                              "Temperature"="black"))

G16

G16.new <- G16 +
  geom_rect(data=sixteen,aes(xmin=(as.Date("03/01", "%m/%d")), 
                             xmax=(as.Date("03/02", "%m/%d")),
                             fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE) +
  geom_rect(data=sixteen,aes(xmin=(as.Date("03/11", "%m/%d")), 
                              xmax=(as.Date("04/06", "%m/%d")),
                              fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE) +
  geom_rect(data=sixteen,aes(xmin=(as.Date("04/16", "%m/%d")), 
                             xmax=(as.Date("04/17", "%m/%d")),
                             fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE)

G16.new

# Adding in vertical lines to show those periods of inoperation...

# G16.new <- G16 + geom_vline(xintercept=(as.Date("03/01","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("03/02","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("03/11","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("04/06","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("04/16","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("04/17","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("05/01","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("05/08","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("05/14","%m/%d")), color="red", linetype="dotdash") 


### 2017 ###

Seventeen <- ggplot(seventeen, aes(x=Month_day)) +
  geom_line(aes(y=BLH_passed * 1 / 4 , color="Herring count"), size=1.25) +
  geom_line(aes(y=AMS_passed * 1 / 4 , color="Shad count"), size=1.25) +
  geom_line(aes(y=Discharge_mean, color="Discharge"), size=1.25,linetype=3,alpha=1) +
  labs(color='') +
  scale_x_date(date_breaks="week", date_labels="%m/%d",
               limits=as.Date(c('2023-02-01','2023-05-01')), 
               expand=c(0,0)) +
  scale_y_continuous(name=expression("Mean Daily Discharge (CFS), Water Temperature (C) x 10^3"),
                     sec.axis = sec_axis(~.* 4 / 1, name="Diadromous Fish Passed (#)",
                                         breaks=seq(0,100000,20000),label=comma),
                     breaks = seq(0,25000,5000), label=comma, expand=c(0.01,0.01), limits=c(0,25000)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size=10, family = "serif")) +
  theme(legend.position = "top", legend.key.width=unit(1.5,"cm")) +
  guides(col=guide_legend(nrow=1))
  theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())

seventeen.pass.w.discharge <- Seventeen + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black")) +
  theme(axis.title.x= element_blank()) +
  annotate("text", x=as.Date("02/07", "%m/%d"),y=22000, label="2017",
           family="serif", size=6)


G17 <- seventeen.pass.w.discharge + 
  geom_line(aes(y=Water.temp_mean * 1000, color="Temperature"), size=1.25) +
  scale_color_manual(values=c("Herring count"="dodgerblue",
                              "Shad count"="chocolate",
                              "Discharge"="black",
                              "Temperature"="black"))

G17

# Adding in vertical lines to show those periods of inoperation...

G17.new <- G17 + 
  geom_rect(data=seventeen,aes(xmin=(as.Date("02/18", "%m/%d")), 
                             xmax=(as.Date("02/20", "%m/%d")),
                             fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE) +
  geom_rect(data=seventeen,aes(xmin=(as.Date("02/23", "%m/%d")), 
                             xmax=(as.Date("02/28", "%m/%d")),
                             fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE)
  
  
#  geom_vline(xintercept=(as.Date("02/18","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("02/20","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("02/23","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("02/28","%m/%d")), color="red", linetype="dotdash") 

G17.new

### 2018 ###

Eighteen <- ggplot(eighteen, aes(x=Month_day)) +
  geom_line(aes(y=BLH_passed * 1 / 4 , color="Herring count"), size=1.25) +
  geom_line(aes(y=AMS_passed * 1 / 4 , color="Shad count"), size=1.25) +
  geom_line(aes(y=Discharge_mean, color="Discharge"), size=1.25,linetype=3,alpha=1) +
  labs(color='') +
  scale_x_date(date_breaks="week", date_labels="%m/%d",
               limits=as.Date(c('2023-02-01','2023-05-01')), 
               expand=c(0,0)) +
  scale_y_continuous(name=expression("Mean Daily Discharge (CFS), Water Temperature (C) x 10^3"),
                     sec.axis = sec_axis(~.* 4 / 1, name="Diadromous Fish Passed (#)",
                                         breaks=seq(0,100000,20000),label=comma),
                     breaks = seq(0,25000,5000), label=comma, expand=c(0.01,0.01), limits=c(0,25000)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size=10, family = "serif")) +
  theme(legend.position = "top", legend.key.width=unit(1.5,"cm")) +
  guides(col=guide_legend(nrow=1))
  theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())

eighteen.pass.w.discharge <- Eighteen + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black")) +
  theme(axis.title.x= element_blank()) +
  annotate("text", x=as.Date("02/07", "%m/%d"),y=22000, label="2018",
           family="serif", size=6)


G18 <- eighteen.pass.w.discharge + 
  geom_line(aes(y=Water.temp_mean * 1000, color="Temperature"), size=1.25) +
  scale_color_manual(values=c("Herring count"="dodgerblue",
                              "Shad count"="chocolate",
                              "Discharge"="black",
                              "Temperature"="black"))

G18

# Adding in vertical lines to show those periods of inoperation...

G18.new <- G18 + 
  geom_rect(data=eighteen,aes(xmin=(as.Date("03/19", "%m/%d")), 
                               xmax=(as.Date("04/08", "%m/%d")),
                               fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE)
  
  
#  geom_vline(xintercept=(as.Date("03/19","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("04/08","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("05/06","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("05/15","%m/%d")), color="red", linetype="dotdash")

G18.new

### 2019 ###

Nineteen <- ggplot(nineteen, aes(x=Month_day)) +
  geom_line(aes(y=BLH_passed * 1 / 4 , color="Herring count"), size=1.25) +
  geom_line(aes(y=AMS_passed * 1 / 4 , color="Shad count"), size=1.25) +
  geom_line(aes(y=Discharge_mean, color="Discharge"), size=1.25,linetype=3,alpha=1) +
  labs(color='') +
  scale_x_date(date_breaks="week", date_labels="%m/%d",
               limits=as.Date(c('2023-02-01','2023-05-01')), 
               expand=c(0,0)) +
  scale_y_continuous(name=expression("Mean Daily Discharge (CFS), Water Temperature (C) x 10^3"),
                     sec.axis = sec_axis(~.* 4 / 1, name="Diadromous Fish Passed (#)",
                                         breaks=seq(0,100000,20000),label=comma),
                     breaks = seq(0,25000,5000), label=comma, expand=c(0.01,0.01), limits=c(0,25000)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size=10, family = "serif")) +
  theme(legend.position = "top", legend.key.width=unit(1.5,"cm")) +
  guides(col=guide_legend(nrow=1))
  theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())

nineteen.pass.w.discharge <- Nineteen + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black")) +
  theme(axis.title.x= element_blank()) +
  annotate("text", x=as.Date("02/10", "%m/%d"),y=22000, label="2019",
           family="serif", size=6)


G19 <- nineteen.pass.w.discharge + 
  geom_line(aes(y=Water.temp_mean * 1000, color="Temperature"), size=1.25) +
  scale_color_manual(values=c("Herring count"="dodgerblue",
                              "Shad count"="chocolate",
                              "Discharge"="black",
                              "Temperature"="black"))

G19

# Adding in vertical lines to show those periods of inoperation...

G19.new <- G19 + 
  geom_rect(data=nineteen,aes(xmin=(as.Date("03/30", "%m/%d")), 
                              xmax=(as.Date("04/13", "%m/%d")),
                              fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE)
  
#  geom_vline(xintercept=(as.Date("03/30","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("04/13","%m/%d")), color="red", linetype="dotdash") 

G19.new


### 2020 ### 

Twenty <- ggplot(twenty, aes(x=Month_day)) +
  geom_line(aes(y=BLH_passed * 1 / 4 , color="Herring count"), size=1.25) +
  geom_line(aes(y=AMS_passed * 1 / 4 , color="Shad count"), size=1.25) +
  geom_line(aes(y=Discharge_mean, color="Discharge"), size=1.25,linetype=3,alpha=1) +
  labs(color='') +
  scale_x_date(date_breaks="week", date_labels="%m/%d",
               limits=as.Date(c('2023-02-01','2023-05-01')), 
               expand=c(0,0)) +
  scale_y_continuous(name=expression("Mean Daily Discharge (CFS), Water Temperature (C) x 10^3"),
                     sec.axis = sec_axis(~.* 4 / 1, name="Diadromous Fish Passed (#)",
                                         breaks=seq(0,100000,20000),label=comma),
                     breaks = seq(0,25000,5000), label=comma, expand=c(0.01,0.01), limits=c(0,25000)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size=10, family = "serif")) +
  theme(legend.position = "top", legend.key.width=unit(1.5,"cm")) +
  guides(col=guide_legend(nrow=1))
theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())

twenty.pass.w.discharge <- Twenty + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black")) +
  theme(axis.title.x= element_blank()) +
  annotate("text", x=as.Date("03/01", "%m/%d"),y=24000, label="2020",
           family="serif", size=6)



G20 <- twenty.pass.w.discharge + 
  geom_line(aes(y=Water.temp_mean * 1000, color="Temperature"), size=1.25) +
  scale_color_manual(values=c("Herring count"="dodgerblue",
                              "Shad count"="chocolate",
                              "Discharge"="black",
                              "Temperature"="black"))

G20

# Adding in vertical lines to show those periods of inoperation...

G20.new <- G20 + 
  geom_rect(data=twenty,aes(xmin=(as.Date("02/10", "%m/%d")), 
                              xmax=(as.Date("02/21", "%m/%d")),
                              fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE) +
  geom_rect(data=twenty,aes(xmin=(as.Date("03/20", "%m/%d")), 
                            xmax=(as.Date("03/24", "%m/%d")),
                            fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE)
    
#  geom_vline(xintercept=(as.Date("02/10","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("02/21","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("03/20","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("03/24","%m/%d")), color="red", linetype="dotdash") 

G20.new

### 2021 ### 

Twentyone <- ggplot(twentyone, aes(x=Month_day)) +
  geom_line(aes(y=BLH_passed * 1 / 4 , color="Herring count"), size=1.25) +
  geom_line(aes(y=AMS_passed * 1 / 4 , color="Shad count"), size=1.25) +
  geom_line(aes(y=Discharge_mean, color="Discharge"), size=1.25,linetype=3,alpha=1) +
  labs(color='') +
  scale_x_date(date_breaks="week", date_labels="%m/%d",
               limits=as.Date(c('2023-02-01','2023-05-01')), 
               expand=c(0,0)) +
  scale_y_continuous(name=expression("Mean Daily Discharge (CFS), Water Temperature (C) x 10^3"),
                     sec.axis = sec_axis(~.* 4 / 1, name="Diadromous Fish Passed (#)",
                                         breaks=seq(0,100000,20000),label=comma),
                     breaks = seq(0,25000,5000), label=comma, expand=c(0.01,0.01), limits=c(0,25000)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size=10, family = "serif")) +
  theme(legend.position = "top", legend.key.width=unit(1.5,"cm")) +
  guides(col=guide_legend(nrow=1))
  theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())

twentyone.pass.w.discharge <- Twentyone + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black")) +
  theme(axis.title.x= element_blank()) +
  annotate("text", x=as.Date("02/07", "%m/%d"),y=22000, label="2021",
           family="serif", size=6)
  


G21 <- twentyone.pass.w.discharge + 
  geom_line(aes(y=Water.temp_mean * 1000, color="Temperature"), size=1.25) +
  scale_color_manual(values=c("Herring count"="dodgerblue",
                              "Shad count"="chocolate",
                              "Discharge"="black",
                              "Temperature"="black"))

G21

# Adding in vertical lines to show those periods of inoperation...

G21.new <- G21 + 
  geom_rect(data=twentyone,aes(xmin=(as.Date("03/30", "%m/%d")), 
                            xmax=(as.Date("04/29", "%m/%d")),
                            fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE)
  
#  geom_vline(xintercept=(as.Date("03/30","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("04/29","%m/%d")), color="red", linetype="dotdash") +
#  geom_vline(xintercept=(as.Date("05/15","%m/%d")), color="red", linetype="dotdash") 

G21.new


### 2022 ### 

Twentytwo <- ggplot(twentytwo, aes(x=Month_day)) +
  geom_line(aes(y=BLH_passed * 1 / 4 , color="Herring count"), size=1.25) +
  geom_line(aes(y=AMS_passed * 1 / 4 , color="Shad count"), size=1.25) +
  geom_line(aes(y=Discharge_mean, color="Discharge"), size=1.25,linetype=3,alpha=1) +
  labs(color='') +
  scale_x_date(date_breaks="week", date_labels="%m/%d",
               limits=as.Date(c('2023-02-01','2023-05-01')), 
               expand=c(0,0)) +
  scale_y_continuous(name=expression("Mean Daily Discharge (CFS), Water Temperature (C) x 10^3"),
                     sec.axis = sec_axis(~.* 4 / 1, name="Diadromous Fish Passed (#)",
                                         breaks=seq(0,100000,20000),label=comma),
                     breaks = seq(0,25000,5000), label=comma, expand=c(0.01,0.01), limits=c(0,25000)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size=10, family = "serif")) +
  theme(legend.position = "top", legend.key.width=unit(1.5,"cm")) +
  guides(col=guide_legend(nrow=1))
  theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())

twentytwo.pass.w.discharge <- Twentytwo + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black")) +
  theme(axis.title.x= element_blank()) +
  annotate("text", x=as.Date("02/07", "%m/%d"),y=22000, label="2022",
           family="serif", size=6)


G22 <- twentytwo.pass.w.discharge + 
  geom_line(aes(y=Water.temp_mean * 1000, color="Temperature"), size=1.25) +
  scale_color_manual(values=c("Herring count"="dodgerblue",
                              "Shad count"="chocolate",
                              "Discharge"="black",
                              "Temperature"="black"))

G22

########## There were no days of inoperation during the 2022 season ########################

# 2023 #

Twentythree <- ggplot(twentythree, aes(x=Month_day)) +
  geom_line(aes(y=BLH_passed * 1 / 2.1 , color="Herring count"), size=1.25) +
  geom_line(aes(y=AMS_passed * 1 / 2.1 , color="Shad count"), size=1.25) +
  geom_line(aes(y=Discharge_mean, color="Discharge"), size=1.25,linetype=3,alpha=1) +
  labs(color='') +
  scale_x_date(date_breaks="week", date_labels="%m/%d",
               limits=as.Date(c('2023-02-01','2023-05-01')), 
               expand=c(0,0)) +
  scale_y_continuous(name=expression("Mean Daily Discharge (CFS), Water Temperature (C) x 10^3"),
                     sec.axis = sec_axis(~.* 2.1 / 1, name="Diadromous Fish Passed (#)",
                                         breaks=seq(0,54600,10920),label=comma),
                     breaks = seq(0,26000,5200), label=comma, expand=c(0.01,0.01), limits=c(0,26000)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size=10, family = "serif")) +
  theme(legend.position = "top", legend.key.width=unit(1.5,"cm")) +
  guides(col=guide_legend(nrow=1)) +
theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())

twentythree.pass.w.discharge <- Twentythree + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black")) +
  theme(axis.title.x= element_blank()) +
  annotate("text", x=as.Date("02/16", "%m/%d"),y=24000, label="2023",
           family="serif", size=6)


G23 <- twentythree.pass.w.discharge + 
  geom_line(aes(y=Water.temp_mean * 1000, color="Temperature"), size=1.25) +
  scale_color_manual(values=c("Herring count"="dodgerblue",
                              "Shad count"="chocolate",
                              "Discharge"="black",
                              "Temperature"="black"))

G23

# Adding in vertical lines to show those periods of inoperationz: 2/7, 3/8, 4/15-4/16

G23.new <- G23 + 
  geom_rect(data=twentythree,aes(xmin=(as.Date("02/07", "%m/%d")), 
                               xmax=(as.Date("02/08", "%m/%d")),
                               fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE) +
  geom_rect(data=twentythree,aes(xmin=(as.Date("03/08", "%m/%d")), 
                                 xmax=(as.Date("03/09", "%m/%d")),
                                 fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE) +
  geom_rect(data=twentythree,aes(xmin=(as.Date("04/15", "%m/%d")), 
                                 xmax=(as.Date("04/17", "%m/%d")),
                                 fill="darkred"),
            ymin=-1, ymax=27000,color="darkred",alpha=0.75, show.legend = FALSE)

G23.new  





# I found a little operation shutdown data for 09 and 11, 2010 went well, and 2008 was a significant
# drought year that didn't allow for many operational days..

G8
G9.recent
G10
G11.new

# These have more significant operational shutdowns included:
G12.new
G13.new
G14.new
G15.new
G16.new
G17.new
G18.new
G19.new
G20.new
G21.new
G22


# I think I will try to save as individual PDF files, and then combine
# with my Adobe Pro (office)....have to figure out how to do that with
# the cairo rendering though.

library('Cairo')


# trying to figure out the pdf output of cairo
# ggsave(G18.new, filename = '2018.pdf', dpi = 300, type = 'pdf',
#       width = 8.5, height = 11, units = 'in')


ggsave(G8, filename = '2008.png', dpi = 300, type = 'cairo',
       width = 8, height = 7, units = 'in')

ggsave(G9.recent, filename = '2009.png', dpi = 300, type = 'cairo',
       width = 8, height = 7, units = 'in')

ggsave(G10, filename = '2010.png', dpi = 300, type = 'cairo',
       width = 8, height = 7, units = 'in')

ggsave(G11.new, filename = '2011.png', dpi = 300, type = 'cairo',
       width = 8, height = 7, units = 'in')

ggsave(G12.new, filename = '2012.png', dpi = 300, type = 'cairo',
       width = 8, height = 7, units = 'in')

ggsave(G13.new, filename = '2013.png', dpi = 300, type = 'cairo',
       width = 8, height = 7, units = 'in')

ggsave(G14.new, filename = '2014.png', dpi = 300, type = 'cairo',
       width = 8, height = 7, units = 'in')

ggsave(G15.new, filename = '2015.png', dpi = 300, type = 'cairo',
       width = 8, height = 7, units = 'in')

ggsave(G16.new, filename = '2016.png', dpi = 300, type = 'cairo',
       width = 8, height = 7, units = 'in')

ggsave(G17.new, filename = '2017.png', dpi = 300, type = 'cairo',
       width = 8, height = 7, units = 'in')

ggsave(G18.new, filename = '2018.png', dpi = 300, type = 'cairo',
       width = 8, height = 7, units = 'in')

ggsave(G19.new, filename = '2019.png', dpi = 300, type = 'cairo',
       width = 8, height = 7, units = 'in')

ggsave(G20.new, filename = '2020.png', dpi = 300, type = 'cairo',
       width = 8, height = 7, units = 'in')

ggsave(G21.new, filename = '2021.png', dpi = 300, type = 'cairo',
       width = 8, height = 7, units = 'in')

ggsave(G22, filename = '2022.png', dpi = 300, type = 'cairo',
       width = 8, height = 7, units = 'in')

ggsave(G23.new, filename = '2023.png', dpi = 300, type = 'cairo',
       width = 8, height = 7, units = 'in')
