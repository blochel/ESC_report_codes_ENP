
# Annual Report for NPS (Cape) --------------------------------------------



library("ggplot2")
library("plyr")
library("dplyr")
library("data.table")
library("scales")
library("ggrepel")
library("Hmisc")
library("ggpmisc")
library("hablar")
library("ggpubr")
library("Rmisc")
library('readxl')


#NPS ANNUAL REPORT

remove_HY <- '2024-25'
this_report <- "2023-24"   

#data download
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, guess_max=9999))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


SD_day<- read_excel("/Hydrology/Quality Checked Data_all years/Hydro Files/SD_HYDRO.xlsx",
                    guess_max=9999) %>%  
  filter(HY != remove_HY)

SD_h<- read_excel_allsheets("/Hydrology/Quality Checked Data_all years/SD hourlies.xlsx") %>% 
  rbindlist(fill=TRUE) %>% 
  as.data.frame() %>% 
  select("Date    Time","HY" , "YEAR" ,"MONTH" ,"DAY" ,
         "Water Level (cm)","Salinity (psu)","Water Temp (°C)" )%>% 
  dplyr::rename(Date_Time = `Date    Time`, 
                Depth = `Water Level (cm)`,
                temp = `Water Temp (°C)`,
                Salinity = `Salinity (psu)`) %>% 
  filter(HY != remove_HY)



LI_day<- read_excel("/Hydrology/Quality Checked Data_all years/Hydro Files/LI_HYDRO.xlsx",
                    guess_max=9999) %>%  
  filter(HY != remove_HY)

LI_h<- read_excel_allsheets("/Hydrology/Quality Checked Data_all years/LI hourlies.xlsx") %>% 
  rbindlist(fill=TRUE) %>% 
  as.data.frame() %>% 
  select("Date    Time","HY" , "YEAR" ,"MONTH" ,"DAY" ,'Rain (in.)',
         "Water Level (cm)","Salinity (psu)","Water Temp (°C)" )%>% 
  dplyr::rename(Date_Time = `Date    Time`, 
                Depth = `Water Level (cm)`,
                temp = `Water Temp (°C)`,
                Salinity = `Salinity (psu)`) %>% 
  filter(HY != remove_HY)




BL_day<- read_excel("/Hydrology/Quality Checked Data_all years/Hydro Files/BL_HYDRO.xlsx",
                    guess_max=9999) %>%  
  filter(HY != remove_HY)

BL_h<- read_excel_allsheets("/Hydrology/Quality Checked Data_all years/BL hourlies.xlsx") %>% 
  rbindlist(fill=TRUE) %>% 
  as.data.frame() %>% 
  select("Date    Time","HY" , "YEAR" ,"MONTH" ,"DAY" ,
         "Water Level (cm)","Salinity (psu)","Water Temp (°C)" )%>% 
  dplyr::rename(Date_Time = `Date    Time`, 
                Depth = `Water Level (cm)`,
                temp = `Water Temp (°C)`,
                Salinity = `Salinity (psu)`) %>% 
  filter(HY != remove_HY)


  

SLR<- read_excel("/Hydrology/SLR/KWMSL_1913-ALLdata.xlsm", 
                 sheet = 'KWWL_1913-ALLdata')

#choose what hydro year it is - change green text to the HY of interest. 
                                                             #change this nr to current HY! (YYYY-YY)



#THIS YEARS ANNUAL MEAN WITH POR####
#BL####
BL_day.now<- which(BL_day$HY == this_report)
BL_hour.now<- which(BL_h$HY == this_report)
#create this HY and POR data frames
BLday.POR<- BL_day[-c(BL_day.now),]
BLday.now<- BL_day[c(BL_day.now),]
BLhour.POR<- BL_h[-c(BL_hour.now),]
BLhour.now<- BL_h[c(BL_hour.now),]

#calcualting long term averages BL####
#BL depth
BLhour.POR$MonthDay <- paste( BLhour.POR$MONTH, BLhour.POR$DAY, sep="-" )
BLhour.now$MonthDay <- paste( BLhour.now$MONTH, BLhour.now$DAY, sep="-" )
depthBL.POR.df<- ddply(BLhour.POR, .(MonthDay), summarise, DepthPOR = mean(Depth, na.rm=TRUE))
#depthBL.POR.df<-depthBL.POR.df[-c(which(depthBL.POR.df$MonthDay == "NA-NA")),]
#depthBL.POR.df<-depthBL.POR.df[-c(which(depthBL.POR.df$MonthDay == "#VALUE!-#VALUE!")),]
#depthBL.POR.df<-depthBL.POR.df[-c(which(depthBL.POR.df$MonthDay == "-")),]
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 on non-leap years
#depthBL.POR.df<- depthBL.POR.df[depthBL.POR.df$MonthDay!="2-29", ]   ## REMOVE this line on leap years


xdf<-data.frame(matrix( data = 1:nrow(depthBL.POR.df), ncol = 2, nrow = nrow(depthBL.POR.df)))
ydf<- unique(BLhour.now$MonthDay)
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
depthBL.POR.df<- merge(xydf,depthBL.POR.df, by = "MonthDay")
depthBL.POR<- depthBL.POR.df[order(depthBL.POR.df$ind),]
#BL sal
salBL.POR.df<- ddply(BLhour.POR, .(MonthDay), summarise, SalPOR = mean(as.numeric(Salinity), na.rm=TRUE))
#salBL.POR.df<-salBL.POR.df[-c(which(salBL.POR.df$MonthDay == "NA-NA")),]
#salBL.POR.df<-salBL.POR.df[-c(which(salBL.POR.df$MonthDay == "#VALUE!-#VALUE!")),]
#salBL.POR.df<-salBL.POR.df[-c(which(salBL.POR.df$MonthDay == "-")),]
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 on non-leap years
#salBL.POR.df<- salBL.POR.df[salBL.POR.df$MonthDay!="2-29", ]   ## REMOVE this line on leap years
xdf<-data.frame(matrix( data = 1:nrow(salBL.POR.df), ncol = 2, nrow = nrow(salBL.POR.df)))
ydf<- unique(BLhour.now$MonthDay)
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
salBL.POR.df<- merge(xydf,salBL.POR.df, by = "MonthDay")
salBL.POR<- salBL.POR.df[order(salBL.POR.df$ind),]


#
#SD####
SD_day.now<- which(SD_day$HY == this_report)
SD_hour.now<- which(SD_h$HY == this_report)
#create this HY and POR data frames
SDday.POR<- SD_day[-c(SD_day.now),]
SDday.now<- SD_day[c(SD_day.now),]
SDhour.POR<- SD_h[-c(SD_hour.now),]
SDhour.now<- SD_h[c(SD_hour.now),]

#calcualting long term averages SD####
#SD depth
SDhour.POR$MonthDay <- paste( SDhour.POR$MONTH, SDhour.POR$DAY, sep="-" )
SDhour.now$MonthDay <- paste( SDhour.now$MONTH, SDhour.now$DAY, sep="-" )
depthSD.POR.df<- ddply(SDhour.POR, .(MonthDay), summarise, DepthPOR = mean(Depth, na.rm=TRUE))
#depthSD.POR.df<-depthSD.POR.df[-c(which(depthSD.POR.df$MonthDay == "NA-NA")),]
#depthSD.POR.df<-depthSD.POR.df[-c(which(depthSD.POR.df$MonthDay == "#VALUE!-#VALUE!")),]
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 on non-leap years
#depthSD.POR.df<- depthSD.POR.df[depthSD.POR.df$MonthDay!="2-29", ]   ## REMOVE this line on leap years

xdf<-data.frame(matrix( data = 1:nrow(depthSD.POR.df), ncol = 2, nrow = nrow(depthSD.POR.df)))
ydf<- unique(SDhour.now$MonthDay)
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
depthSD.POR.df<- merge(xydf,depthSD.POR.df, by = "MonthDay")
depthSD.POR<- depthSD.POR.df[order(depthSD.POR.df$ind),]
#SD sal
salSD.POR.df<- ddply(SDhour.POR, .(MonthDay), summarise, SalPOR = mean(as.numeric(Salinity), na.rm=TRUE))
#salSD.POR.df<-salSD.POR.df[-c(which(salSD.POR.df$MonthDay == "NA-NA")),]
#salSD.POR.df<-salSD.POR.df[-c(which(salSD.POR.df$MonthDay == "#VALUE!-#VALUE!")),]
#create a data frame so dates care sorted after HY structure         ## REVOVE 2-29 on non-leap years
#salSD.POR.df<- salSD.POR.df[salSD.POR.df$MonthDay!="2-29", ]   ## REMOVE this line on leap years
xdf<-data.frame(matrix( data = 1:nrow(salSD.POR.df), ncol = 2, nrow = nrow(salSD.POR.df)))
ydf<- unique(SDhour.now$MonthDay)
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
salSD.POR.df<- merge(xydf,salSD.POR.df, by = "MonthDay")
salSD.POR<- salSD.POR.df[order(salSD.POR.df$ind),]

#
#LI#### 
LI_day.now<- which(LI_day$HY == this_report)
LI_hour.now<- which(LI_h$HY == this_report)
#create this HY and POR data frames
LIday.POR<- LI_day[-c(LI_day.now),]
LIday.now<- LI_day[c(LI_day.now),]
LIhour.POR<- LI_h[-c(LI_hour.now),]
LIhour.now<- LI_h[c(LI_hour.now),]

#calcualting long term averages LI ####
#LI depth
LIhour.POR$MonthDay <- paste( LIhour.POR$MONTH, LIhour.POR$DAY, sep="-" )
LIhour.now$MonthDay <- paste( LIhour.now$MONTH, LIhour.now$DAY, sep="-" )
depthLI.POR.df<- ddply(LIhour.POR, .(MonthDay), summarise, DepthPOR = mean(Depth, na.rm=TRUE))
#depthLI.POR.df<-depthLI.POR.df[-c(which(depthLI.POR.df$MonthDay == "-")),]
#depthLI.POR.df<-depthLI.POR.df[-c(which(depthLI.POR.df$MonthDay == "#VALUE!-#VALUE!")),]
#create a data frame so dates care sorted after HY structure          ## REVOVE 2-29 on non-leap years
#depthLI.POR.df<- depthLI.POR.df[depthLI.POR.df$MonthDay!="2-29", ]   ## REMOVE this line on leap years
xdf<-data.frame(matrix( data = 1:nrow(depthLI.POR.df), ncol = 2, nrow = nrow(depthLI.POR.df)))
ydf<- unique(LIhour.now$MonthDay)
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
depthLI.POR.df<- merge(xydf,depthLI.POR.df, by = "MonthDay")
depthLI.POR<- depthLI.POR.df[order(depthLI.POR.df$ind),]
#LI sal
salLI.POR.df<- ddply(LIhour.POR, .(MonthDay), summarise, SalPOR = mean(as.numeric(Salinity), na.rm=TRUE))
#salLI.POR.df<-salLI.POR.df[-c(which(salLI.POR.df$MonthDay == "-")),]
#salLI.POR.df<-salLI.POR.df[-c(which(salLI.POR.df$MonthDay == "#VALUE!-#VALUE!")),]
#create a data frame so dates care sorted after HY structure         ### REVOVE 2-29 on non-leap years
#salLI.POR.df<- salLI.POR.df[salLI.POR.df$MonthDay!="2-29", ]   ## REMOVE this line on leap years
xdf<-data.frame(matrix( data = 1:nrow(salLI.POR.df), ncol = 2, nrow = nrow(salLI.POR.df)))
ydf<- unique(LIhour.now$MonthDay)
xydf<- cbind(xdf,ydf )
xydf.col.name<- c("ind", "extra", "MonthDay")
colnames(xydf)<- xydf.col.name
#merge the two data sets and order them after the HY structure, (note FEB 29 (leap years) are not in order, last; but small nr)
salLI.POR.df<- merge(xydf,salLI.POR.df, by = "MonthDay")
salLI.POR<- salLI.POR.df[order(salLI.POR.df$ind),]



#create data set for grapth ####
#LI


daily.LI.data<-cbind(LIday.now, depthLI.POR)                                      
daily.LI.data<-cbind(daily.LI.data, salLI.POR)                                 
drops <- c("MonthDay","extra", "ind")
daily.LI.data<-daily.LI.data[ , !(names(daily.LI.data) %in% drops)]
#SD
daily.SD.data <-cbind(SDday.now, depthSD.POR)                                      
daily.SD.data<-cbind(daily.SD.data, salSD.POR)                                 
drops <- c("MonthDay","extra", "ind")
daily.SD.data<-daily.SD.data[ , !(names(daily.SD.data) %in% drops)]
#BL

daily.BL.data <-cbind(BLday.now, depthBL.POR)                                      
daily.BL.data<-cbind(daily.BL.data, salBL.POR)                                 
drops <- c("MonthDay","extra", "ind")
daily.BL.data<-daily.BL.data[ , !(names(daily.BL.data) %in% drops)]


LI.DF1<-daily.LI.data %>% select(YEAR, MONTH, DAY, LABEL, Season, HY, Depth, Salinity)
LI.DF1$what<-sprintf("NOW", 1:nrow(LI.DF1))
LI.DF2<-daily.LI.data %>% select(YEAR, MONTH, DAY, LABEL, Season, HY, DepthPOR, SalPOR)
LI.DF2$what<- sprintf("POR", 1:nrow(LI.DF2)) 
names(LI.DF2)[names(LI.DF2) == "DepthPOR"] <- "Depth"
names(LI.DF2)[names(LI.DF2) == "SalPOR"] <- "Salinity"
daily.LI.DF<- rbind(LI.DF1, LI.DF2)

BL.DF1<-daily.BL.data %>% select(YEAR, MONTH, DAY, LABEL, Season, HY, Depth, Salinity)
BL.DF1$what<-sprintf("NOW", 1:nrow(BL.DF1))
BL.DF2<-daily.BL.data %>% select(YEAR, MONTH, DAY, LABEL, Season, HY, DepthPOR, SalPOR)
BL.DF2$what<- sprintf("POR", 1:nrow(BL.DF2)) 
names(BL.DF2)[names(BL.DF2) == "DepthPOR"] <- "Depth"
names(BL.DF2)[names(BL.DF2) == "SalPOR"] <- "Salinity"
daily.BL.DF<- rbind(BL.DF1, BL.DF2)

SD.DF1<-daily.SD.data %>% select(YEAR, MONTH, DAY, LABEL, Season, HY, Depth, Salinity)
SD.DF1$what<-sprintf("NOW", 1:nrow(SD.DF1))
SD.DF2<-daily.SD.data %>% select(YEAR, MONTH, DAY, LABEL, Season, HY, DepthPOR, SalPOR)
SD.DF2$what<- sprintf("POR", 1:nrow(SD.DF2)) 
names(SD.DF2)[names(SD.DF2) == "DepthPOR"] <- "Depth"
names(SD.DF2)[names(SD.DF2) == "SalPOR"] <- "Salinity"
daily.SD.DF<- rbind(SD.DF1, SD.DF2)
#check this as POR is on everything... 


#daily plots  ####

d.plot.LI.DAILY<- ggplot(data = daily.LI.DF, aes(x=as.Date(LABEL), y=Depth, colour=what ))+
  geom_line()+
  scale_color_manual(values=c("blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("") + ylab("Water Level (cm)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(-5,85, 10))+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


d.plot.BL.DAILY<- ggplot(data = daily.BL.DF, aes(x=as.Date(LABEL), y=Depth, colour=what ))+
  geom_line()+
  scale_color_manual(values=c("blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("") + ylab("Water Level (cm)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(-5,85, 10))+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


d.plot.SD.DAILY<- ggplot(data = daily.SD.DF, aes(x=as.Date(LABEL), y=Depth, colour=what ))+
  geom_line()+
  scale_color_manual(values=c("blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("") + ylab("Water Level (cm)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(-5,85, 10))+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

s.plot.LI.DAILY <- ggplot(data = daily.LI.DF, aes(x=as.Date(LABEL), y=Salinity, colour=what ))+
  geom_line()+
  scale_color_manual(values=c("blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.LI.data$Depth)/5)*5, max(daily.LI.data$Depth), 5))+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

s.plot.BL.DAILY <- ggplot(data = daily.BL.DF, aes(x=as.Date(LABEL), y=Salinity, colour=what ))+
  geom_line()+
  scale_color_manual(values=c("blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.LI.data$Depth)/5)*5, max(daily.LI.data$Depth), 5))+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

s.plot.SD.DAILY <- ggplot(data = daily.SD.DF, aes(x=as.Date(LABEL), y=Salinity, colour=what ))+
  geom_line()+
  scale_color_manual(values=c("blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
  theme_bw() +
  xlab("") + ylab("Salinity (psu)") + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
  scale_y_continuous(breaks=seq(round(min(daily.SD.DF$Salinity)/5)*5, max(daily.SD.DF$Salinity), 5))+
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

daily.LI.DF %>% 
  filter(what == 'NOW') %>% 
  #filter(Season == '1') %>%  
  pull(Salinity) %>% mean() - 
daily.LI.DF %>% 
  filter(what == 'POR') %>% 
  #filter(Season == '1') %>% 
  pull(Salinity) %>% mean()


daily.SD.DF %>% filter(what == 'NOW') %>% 
  filter(Season == '1') %>%  
  pull(Salinity) %>% mean() - 
daily.SD.DF %>% filter(what == 'POR') %>% 
  filter(Season == '1') %>% 
  pull(Salinity) %>% mean()




#LONG TERM ANNUAL MEAN WITH SE
#BL####
BL.depth.all.POR<- ddply(BL_day, .(HY), dplyr::summarise, DepthPOR.all = mean(Depth, na.rm=TRUE))
BL.sal.all.POR<- ddply(BL_day, .(HY), summarise, salPOR.all = mean(as.numeric(Salinity), na.rm=TRUE))

BL.depth.all.POR1<- mutate(BL_day, Season= recode(Season, "2"="Wet Season", "1"="Dry Season"))
BL.depth.dry <- which(BL.depth.all.POR1$Season == "Dry Season")
BL.depth.dry <- BL.depth.all.POR1[c(BL.depth.dry),]
BL.depth.wet <- which(BL.depth.all.POR1$Season == "Wet Season")
BL.depth.wet <- BL.depth.all.POR1[c(BL.depth.wet),]

BL.wet.HY<- ddply(BL.depth.wet, .(HY), dplyr::summarise, depth = mean(Depth, na.rm=TRUE), sd.depth = sd(Depth, na.rm=TRUE),
                  sal = mean(as.numeric(Salinity), na.rm=TRUE), sd.sal = sd(as.numeric(Salinity), na.rm=TRUE), n = n())
BL.dry.HY<- ddply(BL.depth.dry, .(HY), dplyr::summarise, depth = mean(Depth, na.rm=TRUE), sd.depth = sd(Depth, na.rm=TRUE),
                  sal = mean(Salinity, na.rm=TRUE), sd.sal = sd(Salinity, na.rm=TRUE), n = n())

#BL sal
BL.s.sd<- ddply(BL_day, .(HY), summarise, sal.SD = sd(Salinity, na.rm=TRUE))
BL.s.sd[is.na(BL.s.sd)] <- 0
BL_day$n.sal<- as.numeric(!is.na(BL_day$Salinity))
BL.s.n<- as.data.frame(BL_day %>% group_by(HY, n.sal) %>% tally() )
#NEED TO FIND THE DUPLICATED HY AND REMOVE IT BY HAND, CANT FIGURE THE CODE OUT NOW
BL.s.n<- BL.s.n[-c(7),]
BL.sal.all.POR$SE.sal<-BL.s.sd$sal.SD/sqrt(BL.s.n$n)
#BL depth
BL.d.sd<- ddply(BL_day, .(HY), summarise, depth.SD = sd(Depth, na.rm=TRUE))
BL.d.sd[is.na(BL.d.sd)] <- 0
BL_day$n.depth<- as.numeric(!is.na(BL_day$Depth))
BL.d.n<- as.data.frame(BL_day %>% group_by(HY, n.sal) %>% tally() )
#NEED TO FIND THE DUPLICATED HY AND REMOVE IT BY HAND, CANT FIGURE THE CODE OUT NOW
BL.d.n<- BL.d.n[-c(7),]
BL.depth.all.POR$SE.depth<-BL.d.sd$depth.SD/sqrt(BL.d.n$n)




#SD####
SD.depth.all.POR<- ddply(SD_day, .(HY), summarise, DepthPOR.all = mean(Depth, na.rm=TRUE))
SD.sal.all.POR<- ddply(SD_day, .(HY), summarise, salPOR.all = mean(Salinity, na.rm=TRUE))

SD.depth.all.POR1<- mutate(SD_day, Season= recode(Season, "2"="Wet Season", "1"="Dry Season"))
SD.depth.dry <- which(SD.depth.all.POR1$Season == "Dry Season")
SD.depth.dry <- SD.depth.all.POR1[c(SD.depth.dry),]
SD.depth.wet <- which(SD.depth.all.POR1$Season == "Wet Season")
SD.depth.wet <- SD.depth.all.POR1[c(SD.depth.wet),]
SD.wet.HY<- ddply(SD.depth.wet, .(HY), summarise, depth = mean(Depth, na.rm=TRUE), sd.depth = sd(Depth, na.rm=TRUE),
                  sal = mean(Salinity, na.rm=TRUE), sd.sal = sd(Salinity, na.rm=TRUE), n = n())
SD.dry.HY<- ddply(SD.depth.dry, .(HY), summarise, depth = mean(Depth, na.rm=TRUE), sd.depth = sd(Depth, na.rm=TRUE),
                  sal = mean(Salinity, na.rm=TRUE), sd.sal = sd(Salinity, na.rm=TRUE), n = n())

#SD sal
SD.s.sd<- ddply(SD_day, .(HY), summarise, sal.SD = sd(Salinity, na.rm=TRUE))
SD.s.sd[is.na(SD.s.sd)] <- 0
SD_day$n.sal<- as.numeric(!is.na(SD_day$Salinity))
SD.s.n<- as.data.frame(SD_day %>% group_by(HY, n.sal) %>% tally() )
#NEED TO FIND THE DUPLICATED HY AND REMOVE IT BY HAND, CANT FIGURE THE CODE OUT NOW
#SD.s.n<- SD.s.n[-c(7),]
SD.sal.all.POR$SE.sal<-SD.s.sd$sal.SD/sqrt(SD.s.n$n)
#SD depth
SD.d.sd<- ddply(SD_day, .(HY), summarise, depth.SD = sd(Depth, na.rm=TRUE))
SD.d.sd[is.na(SD.d.sd)] <- 0
SD_day$n.depth<- as.numeric(!is.na(SD_day$Depth))
SD.d.n<- as.data.frame(SD_day %>% group_by(HY, n.sal) %>% tally() )
#NEED TO FIND THE DUPLICATED HY AND REMOVE IT BY HAND, CANT FIGURE THE CODE OUT NOW
#SD.d.n<- SD.d.n[-c(7),]
SD.depth.all.POR$SE.depth<-SD.d.sd$depth.SD/sqrt(SD.d.n$n)


#LI####
LI.depth.all.POR<- ddply(LI_day, .(HY), summarise, DepthPOR.all = mean(Depth, na.rm=TRUE))
LI.sal.all.POR<- ddply(LI_day, .(HY), summarise, salPOR.all = mean(Salinity, na.rm=TRUE))

LI.depth.all.POR1<- mutate(LI_day, Season= recode(Season, "2"="Wet Season", "1"="Dry Season"))
LI.depth.dry <- which(LI.depth.all.POR1$Season == "Dry Season")
LI.depth.dry <- LI.depth.all.POR1[c(LI.depth.dry),]
LI.depth.wet <- which(LI.depth.all.POR1$Season == "Wet Season")
LI.depth.wet <- LI.depth.all.POR1[c(LI.depth.wet),]
LI.wet.HY<- ddply(LI.depth.wet, .(HY), summarise, depth = mean(Depth, na.rm=TRUE), sd.depth = sd(Depth, na.rm=TRUE),
                  sal = mean(Salinity, na.rm=TRUE), sd.sal = sd(Salinity, na.rm=TRUE), n = n())
LI.dry.HY<- ddply(LI.depth.dry, .(HY), summarise, depth = mean(Depth, na.rm=TRUE), sd.depth = sd(Depth, na.rm=TRUE),
                  sal = mean(Salinity, na.rm=TRUE), sd.sal = sd(Salinity, na.rm=TRUE), n = n())

#LI sal
LI.s.sd<- ddply(LI_day, .(HY), summarise, sal.SD = sd(Salinity, na.rm=TRUE))
LI.s.sd[is.na(LI.s.sd)] <- 0
LI_day$n.sal<- as.numeric(!is.na(LI_day$Salinity))
LI.s.n<- as.data.frame(LI_day %>% group_by(HY, n.sal) %>% tally() )
#NEED TO FIND THE DUPLICATED HY AND REMOVE IT BY HAND, CANT FIGURE THE CODE OUT NOW
LI.s.n<- LI.s.n[-c(1),]
LI.sal.all.POR$SE.sal<-LI.s.sd$sal.SD/sqrt(LI.s.n$n)
#LI depth
LI.d.sd<- ddply(LI_day, .(HY), summarise, depth.SD = sd(Depth, na.rm=TRUE))
LI.d.sd[is.na(LI.d.sd)] <- 0
LI_day$n.depth<- as.numeric(!is.na(LI_day$Depth))
LI.d.n<- as.data.frame(LI_day %>% group_by(HY, n.sal) %>% tally() )
#NEED TO FIND THE DUPLICATED HY AND REMOVE IT BY HAND, CANT FIGURE THE CODE OUT NOW
LI.d.n<- LI.d.n[-c(1),]
LI.depth.all.POR$SE.depth<-LI.d.sd$depth.SD/sqrt(LI.d.n$n)





#PLOTS  ANNUAL MEAN WITH SE (NEEDS season...)####
BL.depth.all.POR$site<-sprintf("BL", 1:nrow(BL.depth.all.POR))
BL.sal.all.POR$site<-sprintf("BL", 1:nrow(BL.sal.all.POR))
SD.depth.all.POR$site<-sprintf("SD", 1:nrow(SD.depth.all.POR))
SD.sal.all.POR$site<-sprintf("SD", 1:nrow(SD.sal.all.POR))
LI.depth.all.POR$site<-sprintf("LI", 1:nrow(LI.depth.all.POR))
LI.sal.all.POR$site<-sprintf("LI", 1:nrow(LI.sal.all.POR))
depth.por.grapth<- rbind(BL.depth.all.POR, SD.depth.all.POR, LI.depth.all.POR)
sal.por.grapth<- rbind(BL.sal.all.POR, SD.sal.all.POR, LI.sal.all.POR)

TO_JERRY2<- write.csv(sal.por.grapth, "salinity.csv")

BL.wet.HY$site<-sprintf("BL", 1:nrow(BL.wet.HY))
SD.wet.HY$site<-sprintf("SD", 1:nrow(SD.wet.HY))
LI.wet.HY$site<-sprintf("LI", 1:nrow(LI.wet.HY))
BL.dry.HY$site<-sprintf("BL", 1:nrow(BL.dry.HY))
SD.dry.HY$site<-sprintf("SD", 1:nrow(SD.dry.HY))
LI.dry.HY$site<-sprintf("LI", 1:nrow(LI.dry.HY))


BL.wet.HY$SE.depth<-BL.wet.HY$sd.depth/sqrt(BL.wet.HY$n)
LI.wet.HY$SE.depth<-LI.wet.HY$sd.depth/sqrt(LI.wet.HY$n)
SD.wet.HY$SE.depth<-SD.wet.HY$sd.depth/sqrt(SD.wet.HY$n)
BL.wet.HY$SE.sal<-BL.wet.HY$sd.sal/sqrt(BL.wet.HY$n)
LI.wet.HY$SE.sal<-LI.wet.HY$sd.sal/sqrt(LI.wet.HY$n)
SD.wet.HY$SE.sal<-SD.wet.HY$sd.sal/sqrt(SD.wet.HY$n)

BL.dry.HY$SE.depth<-BL.dry.HY$sd.depth/sqrt(BL.dry.HY$n)
LI.dry.HY$SE.depth<-LI.dry.HY$sd.depth/sqrt(LI.dry.HY$n)
SD.dry.HY$SE.depth<-SD.dry.HY$sd.depth/sqrt(SD.dry.HY$n)
BL.dry.HY$SE.sal<-BL.dry.HY$sd.sal/sqrt(BL.dry.HY$n)
LI.dry.HY$SE.sal<-LI.dry.HY$sd.sal/sqrt(LI.dry.HY$n)
SD.dry.HY$SE.sal<-SD.dry.HY$sd.sal/sqrt(SD.dry.HY$n)


wet.HY.por.grapth<- rbind(BL.wet.HY, SD.wet.HY, LI.wet.HY) 
dry.HY.por.grapth<- rbind(BL.dry.HY, SD.dry.HY, LI.dry.HY)


depth.por.grapth1<-depth.por.grapth[!(depth.por.grapth$HY=="1989-90"),] 
depth.por.grapth1<-depth.por.grapth1[!(depth.por.grapth1$HY=="1990-91"),] 
depth.por.grapth1<-depth.por.grapth1[!(depth.por.grapth1$HY=="1991-92"),] 
depth.por.grapth1<-depth.por.grapth1[!(depth.por.grapth1$HY=="1992-93"),] 
depth.por.grapth1<-depth.por.grapth1[!(depth.por.grapth1$HY=="1993-94"),] 
depth.por.grapth1<-depth.por.grapth1[!(depth.por.grapth1$HY=="1994-95"),] 
depth.por.grapth1<-depth.por.grapth1[!(depth.por.grapth1$HY=="1995-96"),] 
depth.por.grapth1<-depth.por.grapth1[!(depth.por.grapth1$HY=="1996-97"),] 
depth.por.grapth1<-depth.por.grapth1[!(depth.por.grapth1$HY=="1997-98"),] 
depth.por.grapth1<-depth.por.grapth1[!(depth.por.grapth1$HY=="1998-99"),] 
depth.por.grapth1<-depth.por.grapth1[!(depth.por.grapth1$HY=="1999-00"),] 
depth.por.grapth1<-depth.por.grapth1[!(depth.por.grapth1$HY=="2000-01"),] 
depth.por.grapth1<-depth.por.grapth1[!(depth.por.grapth1$HY=="2001-02"),]
depth.por.grapth1<-depth.por.grapth1[!(depth.por.grapth1$HY=="2002-03"),] 
depth.por.grapth1<-depth.por.grapth1[!(depth.por.grapth1$HY=="2003-04"),] 
depth.por.grapth1<-depth.por.grapth1[!(depth.por.grapth1$HY=="2004-05"),] 

wet.HY.por.grapth1<-wet.HY.por.grapth[!(wet.HY.por.grapth$HY=="1989-90"),] 
wet.HY.por.grapth1<-wet.HY.por.grapth1[!(wet.HY.por.grapth1$HY=="1990-91"),] 
wet.HY.por.grapth1<-wet.HY.por.grapth1[!(wet.HY.por.grapth1$HY=="1991-92"),] 
wet.HY.por.grapth1<-wet.HY.por.grapth1[!(wet.HY.por.grapth1$HY=="1992-93"),] 
wet.HY.por.grapth1<-wet.HY.por.grapth1[!(wet.HY.por.grapth1$HY=="1993-94"),] 
wet.HY.por.grapth1<-wet.HY.por.grapth1[!(wet.HY.por.grapth1$HY=="1994-95"),] 
wet.HY.por.grapth1<-wet.HY.por.grapth1[!(wet.HY.por.grapth1$HY=="1995-96"),] 
wet.HY.por.grapth1<-wet.HY.por.grapth1[!(wet.HY.por.grapth1$HY=="1996-97"),] 
wet.HY.por.grapth1<-wet.HY.por.grapth1[!(wet.HY.por.grapth1$HY=="1997-98"),] 
wet.HY.por.grapth1<-wet.HY.por.grapth1[!(wet.HY.por.grapth1$HY=="1998-99"),] 
wet.HY.por.grapth1<-wet.HY.por.grapth1[!(wet.HY.por.grapth1$HY=="1999-00"),] 
wet.HY.por.grapth1<-wet.HY.por.grapth1[!(wet.HY.por.grapth1$HY=="2000-01"),] 
wet.HY.por.grapth1<-wet.HY.por.grapth1[!(wet.HY.por.grapth1$HY=="2001-02"),]
wet.HY.por.grapth1<-wet.HY.por.grapth1[!(wet.HY.por.grapth1$HY=="2002-03"),] 
wet.HY.por.grapth1<-wet.HY.por.grapth1[!(wet.HY.por.grapth1$HY=="2003-04"),] 
wet.HY.por.grapth1<-wet.HY.por.grapth1[!(wet.HY.por.grapth1$HY=="2004-05"),] 

dry.HY.por.grapth1<-dry.HY.por.grapth[!(dry.HY.por.grapth$HY=="1989-90"),] 
dry.HY.por.grapth1<-dry.HY.por.grapth1[!(dry.HY.por.grapth1$HY=="1990-91"),] 
dry.HY.por.grapth1<-dry.HY.por.grapth1[!(dry.HY.por.grapth1$HY=="1991-92"),] 
dry.HY.por.grapth1<-dry.HY.por.grapth1[!(dry.HY.por.grapth1$HY=="1992-93"),] 
dry.HY.por.grapth1<-dry.HY.por.grapth1[!(dry.HY.por.grapth1$HY=="1993-94"),] 
dry.HY.por.grapth1<-dry.HY.por.grapth1[!(dry.HY.por.grapth1$HY=="1994-95"),] 
dry.HY.por.grapth1<-dry.HY.por.grapth1[!(dry.HY.por.grapth1$HY=="1995-96"),] 
dry.HY.por.grapth1<-dry.HY.por.grapth1[!(dry.HY.por.grapth1$HY=="1996-97"),] 
dry.HY.por.grapth1<-dry.HY.por.grapth1[!(dry.HY.por.grapth1$HY=="1997-98"),] 
dry.HY.por.grapth1<-dry.HY.por.grapth1[!(dry.HY.por.grapth1$HY=="1998-99"),] 
dry.HY.por.grapth1<-dry.HY.por.grapth1[!(dry.HY.por.grapth1$HY=="1999-00"),] 
dry.HY.por.grapth1<-dry.HY.por.grapth1[!(dry.HY.por.grapth1$HY=="2000-01"),] 
dry.HY.por.grapth1<-dry.HY.por.grapth1[!(dry.HY.por.grapth1$HY=="2001-02"),]
dry.HY.por.grapth1<-dry.HY.por.grapth1[!(dry.HY.por.grapth1$HY=="2002-03"),] 
dry.HY.por.grapth1<-dry.HY.por.grapth1[!(dry.HY.por.grapth1$HY=="2003-04"),] 
dry.HY.por.grapth1<-dry.HY.por.grapth1[!(dry.HY.por.grapth1$HY=="2004-05"),]

#this next year- http://www.sthda.com/english/wiki/one-way-anova-test-in-r

ann.depth.grapth.w.SE<-  ggplot(depth.por.grapth1 %>% 
                                  tidyr::drop_na(DepthPOR.all),
                                aes(x=HY, y=DepthPOR.all, group=site, color=site)) + 
  geom_line() +
  geom_point(shape = 21, fill = "white")+   
  geom_errorbar(aes(ymin=DepthPOR.all-SE.depth, ymax=DepthPOR.all+SE.depth), width=.2,
                position=position_dodge(0.00))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  scale_y_continuous(breaks=seq(floor(min(depth.por.grapth1$DepthPOR.all, na.rm=T)/5)*5, 
                                ceiling(max(depth.por.grapth1$DepthPOR.all, na.rm=T)), 5))+
  xlab("") + ylab("Water Level (cm)") 

dry.HY.depth.grapth.w.SE<-  ggplot(dry.HY.por.grapth1, aes(x=HY, y=depth, group=site, color=site)) + 
  geom_line() +
  geom_point(shape = 21, fill = "white")+   
  geom_errorbar(aes(ymin=depth-SE.depth, ymax=depth+SE.depth), width=.2,
                position=position_dodge(0.00))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  scale_y_continuous(breaks=seq(floor(min(dry.HY.por.grapth1$depth, na.rm=T)/5)*5, 
                                ceiling(max(dry.HY.por.grapth1$depth, na.rm=T)/5)*5, 5))+
  xlab("") + ylab("Water Level (cm)") 

wet.HY.depth.grapth.w.SE<-  ggplot(wet.HY.por.grapth1, aes(x=HY, y=depth, group=site, color=site)) + 
  geom_line() +
  geom_point(shape = 21, fill = "white")+   
  geom_errorbar(aes(ymin=depth-SE.depth, ymax=depth+SE.depth), width=.2,
                position=position_dodge(0.00))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  scale_y_continuous(breaks=seq(floor(min(wet.HY.por.grapth1$depth, na.rm=T)/5)*5, 
                                ceiling(max(wet.HY.por.grapth1$depth, na.rm=T)/5)*5, 5))+
  xlab("") + ylab("Water Level (cm)")


#options(max.print=10000000)

ANOVA.FIG2<- summary(aov(DepthPOR.all~HY, data = depth.por.grapth1))
TUKEY.FIG2<- TukeyHSD(aov(DepthPOR.all~HY, data = depth.por.grapth1))

now.mean<-mean(BLday.now$Depth)
por.mean<-mean(BLday.POR$Depth, na.rm = T)
now.mean-por.mean

now.mean<-mean(BLday.now %>% 
                 filter(Season == 2) %>% 
                 pull(Salinity))
por.mean<-mean(BLday.POR %>% 
                 filter(HY != '1989-90') %>% 
                 filter(Season == 2) %>% 
                 pull(Salinity), na.rm = T)
now.mean-por.mean

min(SDday.now$YEAR)-min(SDday.POR$YEAR)


summary(aov(Depth~HY*site, data = rbind(BLday.POR %>% 
                                          mutate(site = "BL") %>% 
                                            select(c(site, LABEL, HY, Depth, Salinity)), 
                                          SDday.POR %>% 
                                          mutate(site = "SD") %>% 
                                            select(c(site, LABEL, HY, Depth, Salinity)), 
                                          LIday.POR %>% 
                                          mutate(site = "LI") %>% 
                                            select(c(site, LABEL, HY, Depth, Salinity))) ))



sal.por.grapth1<-sal.por.grapth[!(sal.por.grapth$HY=="1989-90"),] 
sal.por.grapth1<-sal.por.grapth1[!(sal.por.grapth1$HY=="1990-91"),] 
sal.por.grapth1<-sal.por.grapth1[!(sal.por.grapth1$HY=="1991-92"),] 
sal.por.grapth1<-sal.por.grapth1[!(sal.por.grapth1$HY=="1992-93"),] 
sal.por.grapth1<-sal.por.grapth1[!(sal.por.grapth1$HY=="1993-94"),] 
sal.por.grapth1<-sal.por.grapth1[!(sal.por.grapth1$HY=="1994-95"),] 
sal.por.grapth1<-sal.por.grapth1[!(sal.por.grapth1$HY=="1995-96"),] 
sal.por.grapth1<-sal.por.grapth1[!(sal.por.grapth1$HY=="1996-97"),] 
sal.por.grapth1<-sal.por.grapth1[!(sal.por.grapth1$HY=="1997-98"),] 
sal.por.grapth1<-sal.por.grapth1[!(sal.por.grapth1$HY=="1998-99"),] 
sal.por.grapth1<-sal.por.grapth1[!(sal.por.grapth1$HY=="1999-00"),] 
sal.por.grapth1<-sal.por.grapth1[!(sal.por.grapth1$HY=="2000-01"),] 
sal.por.grapth1<-sal.por.grapth1[!(sal.por.grapth1$HY=="2001-02"),]
sal.por.grapth1<-sal.por.grapth1[!(sal.por.grapth1$HY=="2002-03"),] 
sal.por.grapth1<-sal.por.grapth1[!(sal.por.grapth1$HY=="2003-04"),] 
sal.por.grapth1<-sal.por.grapth1[!(sal.por.grapth1$HY=="2004-05"),] 

ann.sal.grapth.w.SE<-  ggplot(sal.por.grapth1 %>% 
                                filter(HY != ""), aes(x=HY, y=salPOR.all, group=site, color=site)) + 
  geom_line() +
  geom_point(shape = 21, fill = "white")+   
  geom_errorbar(aes(ymin=salPOR.all-SE.sal, ymax=salPOR.all+SE.sal), width=.2,
                position=position_dodge(0.00))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  scale_y_continuous(breaks=seq(floor(min(sal.por.grapth1$salPOR.all, na.rm=T)/5)*5, 
                                ceiling(max(sal.por.grapth1$salPOR.all, na.rm=T)), 5))+
  xlab("") + ylab("Salinity (psu)")                     

dry.HY.sal.grapth.w.SE<-  ggplot(dry.HY.por.grapth1, aes(x=HY, y=sal, group=site, color=site)) + 
  geom_line() +
  geom_point(shape = 21, fill = "white")+   
  geom_errorbar(aes(ymin=sal-SE.sal, ymax=sal+SE.sal), width=.2,
                position=position_dodge(0.00))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  scale_y_continuous(breaks=seq(floor(min(dry.HY.por.grapth1$sal, na.rm=T)/5)*5, 
                                ceiling(max(dry.HY.por.grapth1$sal, na.rm=T)/5)*5, 5))+
  xlab("") + ylab("Salinity (psu)") 

wet.HY.sal.grapth.w.SE<-  ggplot(wet.HY.por.grapth1, aes(x=HY, y=sal, group=site, color=site)) + 
  geom_line() +
  geom_point(shape = 21, fill = "white")+   
  geom_errorbar(aes(ymin=sal-SE.sal, ymax=sal+SE.sal), width=.2,
                position=position_dodge(0.00))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  scale_y_continuous(breaks=seq(floor(min(wet.HY.por.grapth1$sal, na.rm=T)/5)*5, 
                                ceiling(max(wet.HY.por.grapth1$sal, na.rm=T)/5)*5, 5))+
  xlab("") + ylab("Salinity (psu)") 


ANOVA.FIG10<- summary(aov(Salinity~HY, data = SD_day))
TUKEY.FIG10<- TukeyHSD(aov(Salinity~HY, data = SD_day))

now.mean<-mean(LIday.now[LIday.now$Season == "1",]$Salinity)
por.mean<-mean(LIday.POR[LIday.POR$Season == "1",]$Salinity, na.rm = T)
now.mean<-mean(LIday.now$Salinity)
por.mean<-mean(LIday.POR$Salinity, na.rm = T)
now.mean-por.mean

LIday.now %>% 
  filter( Salinity < 40) %>% pull(MONTH) %>% unique()


min(as.numeric(LIday.now$YEAR))-min(as.numeric(LIday.POR$YEAR), na.rm = TRUE)

### RAIN - Annual, wet/dry season - Exceedance curves  (NEEDS WORK) #### 

#LIday.POR
#LIday.now

# this years seasonal rainfall
rain.now.dry <- which(LIday.now$Season == "1")
rain.now.dry <- LIday.now[c(rain.now.dry),]
rain.now.wet <- which(LIday.now$Season == "2")
rain.now.wet <- LIday.now[c(rain.now.wet),]

#sum this year and season rainfall
this_year_ann<- sum(LIday.now$`Rain (in.)`)
this_year_wet<- sum(rain.now.wet$`Rain (in.)`)
this_year_dry<- sum(rain.now.dry$`Rain (in.)`)


LI_day$Season<- as.character(LI_day$Season)
LI.rain<- mutate(LI_day, Season= recode(Season, "2"="Wet Season", "1"="Dry Season"))


#mean all other years and seasons rainfall
ann.ex<- ddply(LI.rain, .(HY), summarise, total_rain = sum(`Rain (in.)` ,na.rm=TRUE)) %>% 
  filter(HY != '')
season.ex<- ddply(LI.rain, .(HY, Season), summarise, total_rain = sum(`Rain (in.)` ,na.rm=TRUE))%>% 
  filter(Season != '<NA>')
dry.ex <- which(season.ex$Season == "Dry Season")
dry.ex <- season.ex[c(dry.ex),]
wet.ex <- which(season.ex$Season == "Wet Season")
wet.ex <- season.ex[c(wet.ex),]

# % for x-axis bargraph
y.ex<-seq(from = 0, to = 1, by = 1/(length(ann.ex$HY)-1))


#create an ind by sorting rain; largest to smallest for wet, dry and annual rainfall 
wet.ind<-order(-wet.ex$total_rain)
dry.ind<-order(-dry.ex$total_rain)
ann.ind<-order(-ann.ex$total_rain)

#create df for ggplot
Wet.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex),
                  What=rep("Wet Season", length(ann.ex$HY)),
                  HY=as.factor(wet.ex$HY[wet.ind]), 
                  Rain=wet.ex$total_rain[wet.ind] )
Dry.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex),What=rep("Dry Season", length(ann.ex$HY)),
                  HY=as.factor(dry.ex$HY[dry.ind]), 
                  Rain=dry.ex$total_rain[dry.ind] )
Ann.df<-bind_cols(Percent=label_percent(accuracy=1)(y.ex),What=rep("Annual", length(ann.ex$HY)),
                  HY=as.factor(ann.ex$HY[ann.ind]), 
                  Rain=ann.ex$total_rain[ann.ind] )
#bind all the individual df together, and change to factors
exceedance_df<- data.frame(bind_rows(Ann.df, Wet.df, Dry.df) )
exceedance_df$Percent<- factor(exceedance_df$Percent, levels=label_percent(accuracy=1)(y.ex))
exceedance_df$What<- as.factor(exceedance_df$What)



#highlight this years rain as points

this_year_df <- as.data.frame(exceedance_df[exceedance_df$HY == this_report, ])



#creating the exceedance graph
rain.exceedance <- ggplot(exceedance_df, aes(x=Percent, y= Rain)) + geom_line(aes(group=What, colour= What))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        legend.title = element_blank())+ 
  geom_point(shape = 21,aes(color=What))+
  geom_point(data=this_year_df, aes(group=What, colour= What))+
  theme_bw() +
  geom_label_repel(data=this_year_df, aes(label=HY, group=What, colour= What),  show.legend = FALSE,
                   box.padding   = 1, point.padding = 0.5, segment.color = 'grey50')+  #change box.padding and point- 
  xlab("Percent Time Equaled or Exceeded") + ylab("Rainfall (in.)")+  #padding if boxes are in the wrong place
  scale_y_continuous(breaks=seq(round(min(exceedance_df$Rain)/5)*5, ceiling(max(exceedance_df$Rain)), 5))+
  theme(legend.title=element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


# rain all locations ------------------------------------------------------


LI_this_year_ann<- LIday.now %>% 
  select(LABEL, Season, `Rain (in.)`) %>% 
  mutate(site = 'LI') %>% filter(`Rain (in.)` == max(`Rain (in.)`))

SDday.now %>% 
  select(LABEL, Season, `Rain (in.)`) %>% 
  mutate(site = 'SD')

BLday.now %>% 
  select(LABEL, Season, `Rain (in.)`) %>% 
  mutate(site = 'BL')




### Annual Min daily mean ####

BL.min.depth.df <- ddply(BL_h , .(HY, MONTH, DAY), summarise, Depth.min.BL = min(Depth, na.rm=TRUE))
SD.min.depth.df <- ddply(SD_h, .(HY, MONTH, DAY), summarise, Depth.min.SD = min(Depth, na.rm=TRUE))
LI.min.depth.df <- ddply(LI_h, .(HY, MONTH, DAY), summarise, Depth.min.LI = min(Depth, na.rm=TRUE))

BL.max.depth.df <- ddply(BL_h , .(HY, MONTH, DAY), summarise, Depth.max.BL = max(Depth, na.rm=TRUE))
SD.max.depth.df <- ddply(SD_h, .(HY, MONTH, DAY), summarise, Depth.max.SD = max(Depth, na.rm=TRUE))
LI.max.depth.df <- ddply(LI_h, .(HY, MONTH, DAY), summarise, Depth.max.LI = max(Depth, na.rm=TRUE))

BL.sd.depth.df <- ddply(BL_h , .(HY, MONTH, DAY), summarise, Depth.sd.BL = sd(Depth, na.rm=TRUE))
SD.sd.depth.df <- ddply(SD_h, .(HY, MONTH, DAY), summarise, Depth.sd.SD = sd(Depth, na.rm=TRUE))
LI.sd.depth.df <- ddply(LI_h, .(HY, MONTH, DAY), summarise, Depth.sd.LI = sd(Depth, na.rm=TRUE))

BL.depth.df<- BL.min.depth.df
BL.depth.df$Depth.max.BL<- BL.max.depth.df$Depth.max.BL
BL.depth.df$BL.amp.depth<- BL.max.depth.df$Depth.max.BL - BL.min.depth.df$Depth.min.BL
BL.depth.df$BL.sd<- BL.sd.depth.df$Depth.sd.BL

SD.depth.df<- SD.min.depth.df
SD.depth.df$Depth.max.SD<- SD.max.depth.df$Depth.max.SD
SD.depth.df$SD.amp.depth<- SD.max.depth.df$Depth.max.SD - SD.min.depth.df$Depth.min.SD
SD.depth.df$SD.sd<- SD.sd.depth.df$Depth.sd.SD

LI.depth.df<- LI.min.depth.df
LI.depth.df$Depth.max.LI<- LI.max.depth.df$Depth.max.LI
LI.depth.df$LI.amp.depth<- LI.max.depth.df$Depth.max.LI - LI.min.depth.df$Depth.min.LI
LI.depth.df$LI.sd<- LI.sd.depth.df$Depth.sd.LI


BL.sd.sal.df <- ddply(BL_h , .(HY, MONTH, DAY), summarise, Sal.sd.BL = sd(Salinity, na.rm=TRUE), 
                      n = sum(is.na(Salinity))) %>% 
  filter(!is.na(Sal.sd.BL))

SD.sd.sal.df <- ddply(SD_h, .(HY, MONTH, DAY), summarise, Sal.sd.SD = sd(as.numeric(Salinity), na.rm=TRUE), 
                      n = sum(is.na(Salinity))) %>% 
  filter(!is.na(Sal.sd.SD)) 

LI.sd.sal.df <- ddply(LI_h, .(HY, MONTH, DAY), summarise, Sal.sd.LI = sd(as.numeric(Salinity), na.rm=TRUE), 
                      n = sum(is.na(Salinity))) %>% 
  filter(n != 24) %>% 
  filter(!is.na(Sal.sd.LI))

#test to get min working?
#seems like up untill 2016-17 they did annual daily minimum mean waterlevel. after it changed to annual minimum
# daily mean waterlevel... Going back to the old way. This is the case for LI atleast. 
#test.LI <- ddply(LI_day , .(HY, MONTH, DAY), summarise, Depth.min.LI = min(Depth, na.rm=TRUE))
#invisible(lapply(names(test.LI),function(.name) set(test.LI, which(is.infinite(test.LI[[.name]])), j = .name,value =NA)))
#test<- data.table(test.LI)
#ddply(test, .(HY), summarise, LI.min = min(Depth.min.LI, na.rm=TRUE))
#THIS IS WHATS NEEDED!
#THIS<-ddply(LI_day, .(HY), summarise, LI.min = min(Depth, na.rm=TRUE))
#ggplot(THIS, aes(x=HY, y=LI.min)) + 
#  geom_bar(position="dodge", stat="identity")+
# theme(axis.text.x = element_text(angle = 90))+
# xlab("") + ylab("Annual Minimum Daily Mean Water Level (cm)") 


#LI data comparison to code generated numbers. Petes(?) calculations are below...
#HY LI.min
#1  2005-06 -13.38
#2  2006-07 -12.50
#3  2007-08 -16.10
#4  2008-09 -15.63
#5  2009-10  -7.94
#6  2010-11  -5.66
#7  2011-12  -2.30
#8  2012-13   0.45
#9  2013-14   2.67
#10 2014-15   0.70
#11 2015-16   5.02
#12 2016-17  -5.48
#13 2017-18  -0.67
#14 2018-19   3.28
#15 2019-20  -3.52

#-13.38	-12.50	-15.50	-15.63	-7.94	-5.66	-2.30	0.45	2.67	0.70	5.02	-5.48



#change inf to NA
DT.BL<- data.table(BL.depth.df)
invisible(lapply(names(DT.BL),function(.name) set(DT.BL, which(is.infinite(DT.BL[[.name]])), j = .name,value =NA)))
DT.SD<- data.table(SD.depth.df)
invisible(lapply(names(DT.SD),function(.name) set(DT.SD, which(is.infinite(DT.SD[[.name]])), j = .name,value =NA)))
DT.LI<- data.table(LI.depth.df)
invisible(lapply(names(DT.LI),function(.name) set(DT.LI, which(is.infinite(DT.LI[[.name]])), j = .name,value =NA)))

#mean after hy
BL.hy.df<- ddply(DT.BL, .(HY), summarise, BL.min = mean(Depth.min.BL, na.rm=TRUE), 
                 BL.amp = mean(BL.amp.depth, na.rm=TRUE), sd.BL = mean(BL.sd, na.rm=TRUE))
SD.hy.df<- ddply(DT.SD, .(HY), summarise, SD.min = mean(Depth.min.SD, na.rm=TRUE), 
                 SD.amp = mean(SD.amp.depth, na.rm=TRUE), sd.SD = mean(SD.sd, na.rm=TRUE))
LI.hy.df<- ddply(DT.LI, .(HY), summarise, LI.min = mean(Depth.min.LI, na.rm=TRUE), 
                 LI.amp = mean(LI.amp.depth, na.rm=TRUE), sd.LI = mean(LI.sd, na.rm=TRUE))


BL.hy.sal.df<- ddply(BL.sd.sal.df, .(HY), summarise, sd.sal = mean(Sal.sd.BL, na.rm=TRUE))


SD.hy.sal.df<- ddply(rbind(SD.sd.sal.df %>% 
                             filter(HY != '2021-22'), 
                           SD.sd.sal.df %>% 
                             dplyr::filter(HY == '2021-22') %>% 
                             dplyr::filter(Sal.sd.SD < 2)),#added this filter for days probe was in halocline at SD
                     .(HY), summarise, sd.sal = mean(Sal.sd.SD, na.rm=TRUE))

LI.hy.sal.df<- ddply(LI.sd.sal.df, .(HY), summarise, sd.sal = mean(Sal.sd.LI, na.rm=TRUE))


SD_day

BLxx<-BL.hy.sal.df
BLxx$site<-'BL'
SDxx<-SD.hy.sal.df
SDxx$site<-'SD'
LIxx<-LI.hy.sal.df
LIxx$site<-'LI'
stdev.salinity<-rbind(BLxx, SDxx,LIxx)
stdev.salinity<-stdev.salinity[!(stdev.salinity$HY=="2000-01"),]
stdev.salinity<-stdev.salinity[!(stdev.salinity$HY=="2001-02"),]
stdev.salinity<-stdev.salinity[!(stdev.salinity$HY=="2002-03"),] 
stdev.salinity<-stdev.salinity[!(stdev.salinity$HY=="2003-04"),] 
stdev.salinity<-stdev.salinity[!(stdev.salinity$HY=="2004-05"),] 
stdev.salinity<-stdev.salinity[!(stdev.salinity$HY=="2005-06"),] 

stdev.salinity <- stdev.salinity %>% 
  tidyr::drop_na(sd.sal)


#TO_JERRY_st.dev<-rbind(BLxx, SDxx,LIxx)
#write.csv(TO_JERRY_st.dev, "std.dev_data.csv")


#the hys that are of intrest 
BLx<- BL.hy.df[6:nrow(BL.hy.df),]
BLx<-rename(BLx, c("min"="BL.min", "amp"="BL.amp", "sd"="sd.BL" ))
BLx$site<-sprintf("BL", 1:nrow(BLx))
SDx<-SD.hy.df[3:nrow(SD.hy.df),]
SDx$site<-sprintf("SD", 1:nrow(SDx))
SDx<-rename(SDx, c("min"="SD.min", "amp"="SD.amp", "sd"="sd.SD" ))
LIx<-LI.hy.df
LIx<-rename(LIx, c("min"="LI.min", "amp"="LI.amp", "sd"="sd.LI" ))
LIx$site<-sprintf("LI", 1:nrow(LIx))

min.amp.sd<-rbind(BLx, SDx,LIx)

#MIN, St.DEV, AMPLITUDE GRAPHS####

#code for dam building... 
#https://stackoverflow.com/questions/32543176/highlight-areas-within-certain-x-range-in-ggplot2


St.dev.of.daily.D.mean<- ggplot(min.amp.sd, aes(fill=site, x=HY, y=sd)) + 
  geom_bar(position="dodge", stat="identity")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  scale_y_continuous(breaks=seq(round(min(min.amp.sd$sd)/5)*5, max(min.amp.sd$sd), 1))+
  xlab("") + ylab("St.Dev. of Daily Mean\nWater Level (cm)")    

St.dev.of.daily.S.mean<- ggplot(stdev.salinity, aes(fill=site, x=HY, y=sd.sal)) + 
  #annotate("rect", xmin = as.numeric(stdev.salinity$HY[[5]])-0.5, xmax = "2011-12", 
  #                                   ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5)+
  geom_bar(width= 0.5, position=position_dodge(0.7), size= 5, stat="identity")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("") + ylab("St.Dev. of Daily Mean\nSalinity (psu)") 


stdev_for_anova <- min.amp.sd %>% 
  filter(HY != '2006-07' & 
           HY != '2005-06'&
           HY != '2007-08'& 
           HY != '2008-09'& 
           HY != '2009-10'& 
           HY != '2010-11')

day_amp <- min.amp.sd %>% 
  filter(HY != '2006-07' & 
           HY != '2005-06'&
           HY != '2007-08'& 
           HY != '2008-09'& 
           HY != '2009-10'& 
           HY != '2010-11')

PRE_amp <- min.amp.sd %>% 
  filter(HY == '2006-07' | 
           HY == '2005-06'|
           HY == '2007-08'| 
           HY == '2008-09'| 
           HY == '2009-10'| 
           HY == '2010-11')

summary(aov(PRE_amp$sd ~ PRE_amp$site))
TukeyHSD(aov(PRE_amp$sd ~ PRE_amp$site))

summary(aov(PRE_amp$amp ~ PRE_amp$site))
TukeyHSD(aov(PRE_amp$amp ~ PRE_amp$site))

summary(aov(day_amp$sd ~ day_amp$site))
TukeyHSD(aov(day_amp$sd ~ day_amp$site))

summary(aov(day_amp$amp ~ day_amp$site))
TukeyHSD(aov(day_amp$amp ~ day_amp$site))



BL.SD.AOV <- BL.sd.sal.df[-c(1:3),]%>% 
  filter(HY != '2006-07' & 
           HY != '2005-06'&
           HY != '2007-08'& 
           HY != '2008-09'& 
           HY != '2009-10'& 
           HY != '2010-11')
names(BL.SD.AOV)[names(BL.SD.AOV) == "Sal.sd.BL"] <- "sdev"
BL.SD.AOV$site <- sprintf("BL", 1:nrow(BL.SD.AOV))
SD.SD.AOV <- SD.sd.sal.df[-1,]%>% 
  filter(HY != '2006-07' & 
           HY != '2005-06'&
           HY != '2007-08'& 
           HY != '2008-09'& 
           HY != '2009-10'& 
           HY != '2010-11')
names(SD.SD.AOV)[names(SD.SD.AOV) == "Sal.sd.SD"] <- "sdev"
SD.SD.AOV$site <- sprintf("SD", 1:nrow(SD.SD.AOV))
LI.SD.AOV <- LI.sd.sal.df[-c(1:2),]%>% 
  filter(HY != '2006-07' & 
           HY != '2005-06'&
           HY != '2007-08'& 
           HY != '2008-09'& 
           HY != '2009-10'& 
           HY != '2010-11')
names(LI.SD.AOV)[names(LI.SD.AOV) == "Sal.sd.LI"] <- "sdev"
LI.SD.AOV$site <- sprintf("LI", 1:nrow(LI.SD.AOV))
sal.sdev.aov<- rbind(BL.SD.AOV,SD.SD.AOV, LI.SD.AOV)

anova(aov(BL.SD.AOV$sdev ~ BL.SD.AOV$HY ))
sd.res<- TukeyHSD(aov(BL.SD.AOV$sdev ~ BL.SD.AOV$HY ))

anova(aov(SD.SD.AOV$sdev ~ SD.SD.AOV$HY ))
sd.res<- TukeyHSD(aov(SD.SD.AOV$sdev ~ SD.SD.AOV$HY ))

anova(aov(sal.sdev.aov$sdev ~ sal.sdev.aov$HY*sal.sdev.aov$site ))
sd.res<- TukeyHSD(aov(sal.sdev.aov$sdev ~ sal.sdev.aov$HY*sal.sdev.aov$site ))

stdev.salinity

#options(max.print=10000000)


# how to get rectangles correct
#https://stackoverflow.com/questions/49719326/in-ggplot-how-to-manage-width-of-rectangle-when-scale-is-discrete




Daily.Amplitude<- ggplot(min.amp.sd, aes(x=HY, y=amp, group=site, color=site)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=amp-(sd/sqrt(365)), ymax=amp+(sd/sqrt(365))), width=.2,
                position=position_dodge(0.05))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  scale_y_continuous(breaks=seq(round(min(min.amp.sd$amp)/5)*5, ceiling(max(min.amp.sd$amp)+1), 2))+
  xlab("") + ylab("Daily Tidal Amplitude (cm)")  

#geom_rect(mapping = aes(xmin="2010-11" , xmax="2010-11", ymin=-Inf, ymax=Inf),color="grey",
#          size=5, alpha = 0.5)+

min.amp.sd1 <- min.amp.sd[!(min.amp.sd$HY=="2005-06"),] 
min.amp.sd1 <- min.amp.sd1[!(min.amp.sd1$HY=="2006-07"),]
min.amp.sd1 <- min.amp.sd1[!(min.amp.sd1$HY=="2007-08"),] 
min.amp.sd1 <- min.amp.sd1[!(min.amp.sd1$HY=="2008-09"),]
min.amp.sd1 <- min.amp.sd1[!(min.amp.sd1$HY=="2009-10"),] 
min.amp.sd1 <- min.amp.sd1[!(min.amp.sd1$HY=="2010-11"),]



ANOVA.FIG8<- summary(aov(sd~site+HY, data = min.amp.sd1))
TUKEY.FIG8<- TukeyHSD(aov(sd~site+HY, data = min.amp.sd1))

LI.analysis<- min.amp.sd[(min.amp.sd1$site=="LI"),]
ANOVA.FIG8<- summary(aov(sd~HY, data = LI.analysis))
TUKEY.FIG8<- TukeyHSD(aov(sd~site+HY, data = min.amp.sd1))

BL.analysis<- min.amp.sd[(min.amp.sd1$site=="BL"),]
ANOVA.FIG8<- summary(aov(sd~HY, data = BL.analysis))
TUKEY.FIG8<- TukeyHSD(aov(sd~site+HY, data = min.amp.sd1))

SD.analysis<- min.amp.sd[(min.amp.sd$site=="SD"),]
ANOVA.FIG8<- summary(aov(sd~HY, data = SD.analysis))
TUKEY.FIG8<- TukeyHSD(aov(sd~site+HY, data = min.amp.sd1))


#DT.LI




LI_day.min1<-ddply(LI_h, .(HY,MONTH,DAY), summarise, LI.depth.min = mean(Depth, na.rm=TRUE))
LI_day.min2<-ddply(LI_day.min1, .(HY), summarise, depth = min(LI.depth.min, na.rm=TRUE))
LI_day.min2<- LI_day.min2[-c(1:2),]

SD_day.min1<-ddply(SD_h, .(HY,MONTH,DAY), summarise, SD.depth.min = mean(Depth, na.rm=TRUE))
SD_day.min2<-ddply(SD_day.min1, .(HY), summarise, depth = min(SD.depth.min, na.rm=TRUE))
SD_day.min2<- SD_day.min2[-c(1),]

BL_day.min1<-ddply(BL_h, .(HY,MONTH,DAY), summarise, BL.depth.min = mean(Depth, na.rm=TRUE))
BL_day.min2<-ddply(BL_day.min1, .(HY), summarise, depth = min(BL.depth.min, na.rm=TRUE))
BL_day.min2<- BL_day.min2[-c(1:2),]

BL.hy.min<-ddply(BL_day, .(HY), summarise, BL.min = min(Depth, na.rm=TRUE))
SD.hy.min<-ddply(SD_day, .(HY), summarise, SD.min = min(Depth, na.rm=TRUE))
LI.hy.min<-ddply(LI_day, .(HY), summarise, LI.min = min(Depth, na.rm=TRUE))

BL.min.water.level<-ggplot(BL.hy.min, aes(x=HY, y=BL.min)) + 
  geom_bar(position="dodge", stat="identity")+
  theme_bw() +                  
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("") + ylab("Water Level (cm)")+
  scale_y_continuous(limits = c(-35, 5))+
  geom_smooth(aes(x = as.numeric(factor(HY)), y = BL.min), method = 'lm', se=F, colour="red")+
  scale_x_discrete(breaks = BL.hy.min$HY[seq(1, length(BL.hy.min$HY), by = 2)])

SD.min.water.level<-ggplot(SD.hy.min, aes(x=HY, y=SD.min)) + 
  geom_bar(position="dodge", stat="identity")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("") + ylab("Annual Daily Minimum Mean\nWater Level (cm)")+
  scale_y_continuous(limits = c(-35, 5))+
  geom_smooth(aes(x = as.numeric(factor(HY)), y = SD.min), method = 'lm', se=F, colour="red")  

LI.min.water.level<-ggplot(LI.hy.min %>% 
                             filter(HY != ''), aes(x=HY, y=LI.min)) + 
  geom_bar(position="dodge", stat="identity")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("") + ylab("Annual Daily Minimum Mean\nWater Level (cm)")+
  scale_y_continuous(limits = c(-35, 5))+
  geom_smooth(aes(x = as.numeric(factor(HY)), y = LI.min), method = 'lm', se=F, colour="red")  




#BL.min.water.level2<-ggplot(BL_day.min2, aes(x=HY, y=depth)) + 
#  geom_bar(position="dodge", stat="identity")+
#  theme_bw() +
#  theme(axis.text.x = element_text(angle = 90))+
#  xlab("") + ylab("Annual Daily Minimum Mean\nWater Level (cm)")+
# geom_smooth(aes(x = as.numeric(factor(HY)), y = depth), method = 'lm', se=F, colour="red") 

#SD.min.water.level2<-ggplot(SD_day.min2, aes(x=HY, y=depth)) + 
#  geom_bar(position="dodge", stat="identity")+
#  theme_bw() +
#  theme(axis.text.x = element_text(angle = 90))+
#  xlab("") + ylab("Annual Daily Minimum Mean\nWater Level (cm)")+
# geom_smooth(aes(x = as.numeric(factor(HY)), y = depth), method = 'lm', se=F, colour="red") 

#LI.min.water.level2<-ggplot(LI_day.min2, aes(x=HY, y=depth)) + 
#                    geom_bar(position="dodge", stat="identity")+
#                    theme_bw() +
#                    theme(axis.text.x = element_text(angle = 90))+
#                  xlab("") + ylab("Annual Daily Minimum Mean\nWater Level (cm)")+
#                   geom_smooth(aes(x = as.numeric(factor(HY)), y = depth), method = 'lm', se=F, colour="red") 


#summary(lm(LI_day$Depth ~ LI_day$HY))
summary(lm(LI_day.min1$LI.depth.min ~ LI_day.min1$HY))
#summary(lm(LI.hy.min$BL.min ~ LI.hy.min$HY))

#summary(lm(SD_day$Depth ~ SD_day$HY))
summary(lm(SD_day.min1$SD.depth.min ~ SD_day.min1$HY))
#summary(lm(SD.hy.min$SD.min ~ SD.hy.min$HY))

#summary(lm(BL_day$Depth ~ BL_day$HY))
summary(lm(BL_day.min1$BL.depth.min ~ BL_day.min1$HY))
#summary(lm(BL.hy.min$BL.min ~ BL.hy.min$HY))

#http://www.sthda.com/english/articles/40-regression-analysis/167-simple-linear-regression-in-r/


BL.cor.min<- BL.hy.min
BL.cor.min$year<- c(1:nrow(BL.hy.min))
SD.cor.min<- SD.hy.min
SD.cor.min$year<- c(1:nrow(SD.hy.min))
LI.cor.min<- LI.hy.min %>% 
  filter(HY != '')
LI.cor.min$year<- c(1:nrow(LI.hy.min %>% 
                             filter(HY != '')))
LI.cor.min$year<- as.numeric(LI.cor.min$year)


COR_TEST_BL_min<- cor.test(BL.cor.min$year, BL.cor.min$BL.min)
R2.BL.min<-cor(BL.cor.min$BL.min, BL.cor.min$year)^2
COR_TEST_SD_min<- cor.test(SD.cor.min$year, SD.cor.min$SD.min)
R2.SD.min<-cor(SD.cor.min$SD.min, SD.cor.min$year)^2
COR_TEST_LI_min<- cor.test(LI.cor.min$year, LI.cor.min$LI.min)
R2.LI.min<-cor(LI.cor.min$LI.min, LI.cor.min$year)^2

#wrong... 
#min.water.level<- ggplot(min.amp.sd, aes(fill=site, x=HY, y=min)) + 
#                 geom_bar(position="dodge", stat="identity")+
#                  theme(axis.text.x = element_text(angle = 90))+
#                  xlab("") + ylab("Annual Minimum Daily Mean Water Level (cm)") 
# geom_errorbar(stat="summary", 
#               fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))}, 
#               fun.ymax=function(x) {mean(x)+sd(x)/sqrt(length(x))})






#ANNUAL MEAN (NEED SE) SAL AT BL####


# Calculate  SD           geom_errorbar(aes(ymin=0, ymax=amp+(sd/sqrt(365))), width=.2,
#                        position=position_dodge(0.05))+


BL_day1<-BL_day[!(BL_day$HY=="1989-90"),] 
BL_day1<-BL_day1[!(BL_day1$HY=="1990-91"),] 
BL_day1<-BL_day1[!(BL_day1$HY=="1991-92"),] 
BL_day1<-BL_day1[!(BL_day1$HY=="1992-93"),] 
BL_day1<-BL_day1[!(BL_day1$HY=="1993-94"),] 
BL_day1<-BL_day1[!(BL_day1$HY=="1994-95"),] 

BL.sal.graph <- summarySE(BL_day1, measurevar="Salinity", groupvars="HY", na.rm = T)
COR_TEST_BL_SAL<- cor.test( as.numeric(factor(BL.sal.graph$HY)), BL.sal.graph$Salinity)
R2_BL_SAL<- cor(as.numeric(factor(BL.sal.graph$HY)) ,BL.sal.graph$Salinity)^2


anova(aov(data = BL_day1, 
    Salinity ~ HY))
TukeyHSD(aov(data = BL_day1, 
             Salinity ~ HY)) 


BL.sal.ann<- ggplot(BL.sal.graph, aes(x=HY, y=Salinity)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin= Salinity, ymax=Salinity+sd), width=.2,
                position=position_dodge(0.00))+
  theme_bw() +           
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("") + ylab("Salinity (psu)")+
  geom_smooth(aes(x = as.numeric(factor(HY)), y = Salinity), method = 'lm', se=F, colour="red")



SD_day1<-SD_day[!(SD_day$HY=="2003-04"),] 
SD_day1<-SD_day1[!(SD_day1$HY=="2004-05"),] 
SD_day1<-SD_day1[!(SD_day1$HY=="2005-06"),] 
SD_day1<-SD_day1[!(SD_day1$HY=="2006-07"),] 
SD_day1<-SD_day1[!(SD_day1$HY=="2007-08"),] 
SD_day1<-SD_day1[!(SD_day1$HY=="2008-09"),]

SD.sal.graph <- summarySE(SD_day1, measurevar="Salinity", groupvars="HY", na.rm = T)
COR_TEST_SD_SAL<- cor.test( as.numeric(factor(SD.sal.graph$HY)), SD.sal.graph$Salinity)
R2_SD_SAL<- cor(as.numeric(factor(SD.sal.graph$HY)) ,SD.sal.graph$Salinity)^2


SD.sal.ann<- ggplot(SD.sal.graph, aes(x=HY, y=Salinity)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin= Salinity, ymax=Salinity+sd), width=.2,
                position=position_dodge(0.00))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("") + ylab("Salinity (psu)")+
  geom_smooth(aes(x = as.numeric(factor(HY)), y = Salinity), method = 'lm', se=F, colour="red") 

LI.sal.graph <- summarySE(LI_day, measurevar="Salinity", groupvars="HY", na.rm = T) %>% 
  filter(N != 0)
COR_TEST_LI_SAL<- cor.test( as.numeric(factor(LI.sal.graph$HY)), LI.sal.graph$Salinity)
R2_LI_SAL<- cor(as.numeric(factor(LI.sal.graph$HY)) ,LI.sal.graph$Salinity)^2

LI.sal.ann<- ggplot(LI.sal.graph, aes(x=HY, y=Salinity)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin= Salinity, ymax=Salinity+sd), width=.2,
                position=position_dodge(0.00))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("") + ylab("Salinity (psu)")+
  geom_smooth(aes(x = as.numeric(factor(HY)), y = Salinity), method = 'lm', se=F, colour="red") 


#VIOLIN GRAPHS  ####

violin.BL<- ggplot(BL_day, aes(x=HY,  y=Depth)) + 
  geom_violin(colour="blue", alpha=0.5) +
  geom_point(stat="summary", fun="mean") + 
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  labs(x="", y="Water Level (cm)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "right",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  geom_smooth(aes(x = as.numeric(factor(HY)), y = Depth), method = 'lm', colour="red")

corr.line.bl<- summary(lm(BL_day$Depth~ as.numeric(factor(BL_day$HY))))

BL_day<- mutate(BL_day, Season= recode(Season, "2"="Wet Season", "1"="Dry Season"))
BL_day.dry <- which(BL_day$Season == "Dry Season")
BL_day.dry <- BL_day[c(BL_day.dry),]
BL_day.wet <- which(BL_day$Season == "Wet Season")
BL_day.wet <- BL_day[c(BL_day.wet),]

violin.BL.dry<- ggplot(BL_day.dry, aes(x=HY,  y=Depth)) + 
  geom_violin(colour="blue", alpha=0.5) +
  geom_point(stat="summary", fun="mean") + 
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  labs(x="", y="Water Level (cm)") +
  scale_x_discrete("", breaks = unique(BL_day.wet$HY)[seq(1, length(unique(BL_day.wet$HY)), by = 2)])+
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "right",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  geom_smooth(aes(x = as.numeric(factor(HY)), y = Depth), method = 'lm', colour="red")

corr.line.bl.dry<-summary(lm(BL_day.dry$Depth~ as.numeric(factor(BL_day.dry$HY))))


violin.BL.wet<- ggplot(BL_day.wet, aes(x=HY,  y=Depth)) + 
  geom_violin(colour="blue", alpha=0.5) +
  geom_point(stat="summary", fun="mean") + 
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  labs(x="", y="Water Level (cm)") +
  scale_x_discrete("", breaks = unique(BL_day.wet$HY)[seq(1, length(unique(BL_day.wet$HY)), by = 2)])+
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "right",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  geom_smooth(aes(x = as.numeric(factor(HY)), y = Depth), method = 'lm', colour="red")

corr.line.bl.wet<-summary(lm(BL_day.wet$Depth~ as.numeric(factor(BL_day.wet$HY))))


BL_all_HY<- ddply(BL_day, .(HY), summarise, SD.depth.min = mean(Depth, na.rm=TRUE))


violin.SD<- ggplot(SD_day, aes(x=HY,  y=Depth)) + 
  geom_violin(colour="blue", alpha=0.5) +
  geom_point(stat="summary", fun="mean") + 
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  labs(x="", y="Water Level (cm)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "right",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  geom_smooth(aes(x = as.numeric(factor(HY)), y = Depth), method = 'lm', colour="red")

SD_day<- mutate(SD_day, Season= recode(Season, "2"="Wet Season", "1"="Dry Season"))
SD_day.dry <- which(SD_day$Season == "Dry Season")
SD_day.dry <- SD_day[c(SD_day.dry),]
SD_day.wet <- which(SD_day$Season == "Wet Season")
SD_day.wet <- SD_day[c(SD_day.wet),]

violin.SD.dry<- ggplot(SD_day.dry, aes(x=HY,  y=Depth)) + 
  geom_violin(colour="blue", alpha=0.5) +
  geom_point(stat="summary", fun="mean") + 
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  labs(x="", y="Water Level (cm)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "right",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  geom_smooth(aes(x = as.numeric(factor(HY)), y = Depth), method = 'lm', colour="red")

violin.SD.wet<- ggplot(SD_day.wet, aes(x=HY,  y=Depth)) + 
  geom_violin(colour="blue", alpha=0.5) +
  geom_point(stat="summary", fun="mean") + 
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  labs(x="", y="Water Level (cm)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "right")+
  geom_smooth(aes(x = as.numeric(factor(HY)), y = Depth), method = 'lm', colour="red")

my.formula <- y~x 
r2.depth <-summary(lm(LI_day$Depth ~ LI_day$HY))$r.squared
#R2<- "R^2"
corr.line.SD.ann<-summary(lm(SD_day$Depth~ as.numeric(factor(SD_day$HY))))
corr.line.SD.wet<-summary(lm(SD_day.wet$Depth~ as.numeric(factor(SD_day.wet$HY))))
corr.line.SD.dry<-summary(lm(SD_day.dry$Depth~ as.numeric(factor(SD_day.dry$HY))))



violin.LI<- ggplot(LI_day %>% 
                     filter(HY != ''), aes(x=HY,  y=Depth)) + 
  geom_violin(colour="blue", alpha=0.5) +
  geom_point(stat="summary", fun="mean") + 
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  labs(x="", y="Water Level (cm)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "right",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  geom_smooth(aes(x = as.numeric(factor(HY)), y = Depth), method = 'lm', colour="red")

LI_day<- mutate(LI_day, Season= recode(Season, "2"="Wet Season", "1"="Dry Season"))
LI_day.dry <- which(LI_day$Season == "Dry Season")
LI_day.dry <- LI_day[c(LI_day.dry),]
LI_day.wet <- which(LI_day$Season == "Wet Season")
LI_day.wet <- LI_day[c(LI_day.wet),]

violin.LI.dry<- ggplot(LI_day.dry, aes(x=HY,  y=Depth)) + 
  geom_violin(colour="blue", alpha=0.5) +
  geom_point(stat="summary", fun="mean") + 
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  labs(x="", y="Water Level (cm)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "right",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  geom_smooth(aes(x = as.numeric(factor(HY)), y = Depth), method = 'lm', colour="red")

violin.LI.wet<- ggplot(LI_day.wet, aes(x=HY,  y=Depth)) + 
  geom_violin(colour="blue", alpha=0.5) +
  geom_point(stat="summary", fun="mean") + 
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  labs(x="", y="Water Level (cm)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "right",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  geom_smooth(aes(x = as.numeric(factor(HY)), y = Depth), method = 'lm', colour="red")  

corr.line.LI.ann<-summary(lm(LI_day$Depth~ as.numeric(factor(LI_day$HY))))
corr.line.LI.wet<-summary(lm(LI_day.wet$Depth~ as.numeric(factor(LI_day.wet$HY))))
corr.line.LI.dry<-summary(lm(LI_day.dry$Depth~ as.numeric(factor(LI_day.dry$HY))))

#this for R2 values
cor(as.numeric(factor(BL_day$HY)),BL_day$Depth)^ 2
#anova 
anova(lm(as.numeric(factor(BL_day$HY))~BL_day$Depth))


#Sea Level Rise ####


SLR1<- ddply(SLR, .(hydroyear), summarise, MLLW = mean(MSL, na.rm=TRUE))
SLR2<- SLR1[87:nrow(SLR1),]

SLR_MLLW1 <- ggplot(SLR1, aes(x=hydroyear, y=MLLW)) + 
  geom_rect(xmin="2000-01", xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5,fill="grey90")+
  geom_vline(xintercept = "2000-01")+
  geom_bar(position="dodge", stat="identity")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size=12, vjust=0.5), 
        panel.grid.major = element_line(colour = "grey80"),
        axis.title.y = element_text( size=12,face="bold"),
        axis.text.y = element_text(face="bold", size=12)) +
  xlab("") + scale_y_continuous(name="MSL Relative to MLLW in Meters", limits=c(-0.2, 0.2),n.breaks = 10 )+
  geom_smooth(aes(x = as.numeric(factor(hydroyear)), y = MLLW), method = 'lm', se=F, colour="red")+ 
  scale_x_discrete(breaks = c("1913-14", "1917-18", "1921-22","1925-26", "1929-30", "1933-34", "1937-38", "1941-42",
                              "1945-46", "1949-50", "1953-54", "1957-58", "1961-62", "1965-66", "1969-70", "1973-74", 
                              "1977-78", "1981-82","1985-86", "1989-90", "1993-94", "1997-98", "2001-02", "2005-06", 
                              "2009-10", "2013-14", "2017-18", '2023-24'))


corr.line.SLR_MLLW1<-summary(lm(SLR1$MLLW~ as.numeric(factor(SLR1$hydroyear))))
#adding formula as text
#https://stackoverflow.com/questions/44647613/receive-the-equation-of-the-stat-smooth-in-ggplot2-r-mtcars-example


SLR_MLLW2 <-  ggplot(SLR2, aes(x=hydroyear, y=MLLW)) + 
  geom_bar(position="dodge", stat="identity")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size=20,face="bold", vjust=0.5), 
        panel.grid.major = element_line(colour = "grey80"),
        axis.title.y = element_text( size=15,face="bold"),
        axis.text.y = element_text(face="bold", size=12))+
  xlab("") + scale_y_continuous(name="MSL Relative to\nMLLW in Meters", limits=c(0, 0.2),n.breaks = 5 )+
  geom_smooth(aes(x = as.numeric(factor(hydroyear)), y = MLLW), method = "glm", method.args=list(family="binomial"),
              se =F, colour="red")+
  scale_x_discrete(breaks = c("2000-01", "2004-05", "2008-09", "2012-13", 
                              "2016-17",  "2020-21",  '2023-24'))


corr.line.SLR_MLLW2<-summary(glm(SLR2$MLLW~ as.numeric(factor(SLR2$hydroyear))))
R2.SLR_MLLW2<- with(corr.line.SLR_MLLW2, 1 - deviance/null.deviance)
# srl extra ---------------------------------------------------------------

extra_SLR <- BL.hy.min %>% 
  left_join(
    SLR1 %>% 
      rename(min_KW = MLLW, 
             HY = hydroyear) %>% 
      mutate(min_KW = min_KW * 100),
    by = join_by(HY)) %>% 
  left_join(
    SD.hy.min, by = join_by(HY))%>% 
  left_join(
    LI.hy.min, by = join_by(HY))


summary(lm(extra_SLR$BL.min ~ extra_SLR$min_KW))
summary(lm(extra_SLR$SD.min ~ extra_SLR$min_KW))
summary(lm(extra_SLR$LI.min ~ extra_SLR$min_KW))



#all grapths ####

#code for dam building... 
#https://stackoverflow.com/questions/32543176/highlight-areas-within-certain-x-range-in-ggplot2

#daily plots

d.plot.LI.DAILY
d.plot.BL.DAILY
d.plot.SD.DAILY
ggarrange(d.plot.BL.DAILY,d.plot.SD.DAILY,d.plot.LI.DAILY, ncol = 1, nrow = 3, 
          labels = c("   BL ", "   SD", "   LI"), 
          common.legend = TRUE, vjust= -0.5 )     #fig 3 FAB2
s.plot.LI.DAILY
s.plot.BL.DAILY
s.plot.SD.DAILY
ggarrange(s.plot.BL.DAILY,s.plot.SD.DAILY,s.plot.LI.DAILY, ncol = 1, nrow = 3, labels = c("BL ", "SD", "LI"), 
          common.legend = TRUE, vjust= -0.5 ) #fig 11 FAB10
#PLOTS  ANNUAL & seasonal MEAN WITH SE
ann.depth.grapth.w.SE                       #fig 2 FAB1
ann.sal.grapth.w.SE                         #fig 10 FAB9

dry.HY.depth.grapth.w.SE                 
wet.HY.depth.grapth.w.SE                   
ggarrange(wet.HY.depth.grapth.w.SE,dry.HY.depth.grapth.w.SE,  ncol = 2, nrow = 1, labels=c("Wet Season", "Dry Season"),
          common.legend = TRUE )           #fig5 FAB4
dry.HY.sal.grapth.w.SE
wet.HY.sal.grapth.w.SE
ggarrange(wet.HY.sal.grapth.w.SE,dry.HY.sal.grapth.w.SE,  ncol = 2, nrow = 1, labels=c("Wet Season", "Dry Season"),
          common.legend = TRUE )           #fig5 FAB13

# RAIN - Annual, wet/dry season - Exceedance curves
rain.exceedance                            #fig 9 FAB8
#St.DEV, AMPLITUDE GRAPHS
St.dev.of.daily.D.mean                     #
St.dev.of.daily.S.mean                     #fig 15  FAB12
Daily.Amplitude                            #
ggarrange(Daily.Amplitude, St.dev.of.daily.D.mean, ncol=1, nrow=2)  #fig 8 FAB7

#Min water levels
BL.min.water.level
SD.min.water.level2<-  SD.min.water.level + theme(axis.title.y = element_blank())
LI.min.water.level2<-  LI.min.water.level + theme(axis.title.y = element_blank())
ggarrange(BL.min.water.level, SD.min.water.level2, LI.min.water.level2, ncol=3, nrow=1, 
          labels=c("        BL", "   SD", "    LI"))#fig 7 FAB6

ggarrange(
ggarrange(BL.min.water.level, SD.min.water.level2, LI.min.water.level2, ncol=3, nrow=1, 
          labels=c("       BL", "    SD", "     LI")),
ggarrange(
  ggplot(extra_SLR, aes(y=BL.min, x=min_KW))+
    geom_point()+
    geom_smooth(method=lm, se=FALSE)+ 
    #ggtitle("Plot of length \n by dose") +
    ylab("BL daily min water level (cm)") + xlab("Key West Harbor Mean Lower Low Water (cm)")+
    theme(plot.margin = margin(5.5, 5.5, 100, 5.5)), 
  ggplot(extra_SLR, aes(y=SD.min, x=min_KW))+
    geom_point()+
    geom_smooth(method=lm, se=FALSE)+ 
    #ggtitle("Plot of length \n by dose") +
    ylab("SD daily min water level (cm)") + xlab("Key West Harbor Mean Lower Low Water (cm)")+
    theme(plot.margin = margin(5.5, 5.5, 100, 5.5)),
  ggplot(extra_SLR, aes(y=LI.min, x=min_KW))+
    geom_point()+
    geom_smooth(method=lm, se=FALSE)+ 
    #ggtitle("Plot of length \n by dose") +
    ylab("LI daily min water level (cm)") + xlab("Key West Harbor Mean Lower Low Water (cm)")+
    theme(plot.margin = margin(5.5, 5.5, 100, 5.5)), 
  ncol=3, nrow=1
), ncol = 1 )


#   vjust= 2, hjust = -2)  NEED TO FIX LABELS!


#annual mean (se) salinity at BL
BL.sal.ann                                   #fig 12 FAB11
SD.sal.ann                                   #apendix
LI.sal.ann                                   #apendix
ggarrange(BL.sal.ann, SD.sal.ann, LI.sal.ann, ncol=3, nrow=1, 
          labels=c("      BL", "      SD", "       LI"))
#SLR
SLR_MLLW1                                    #fig 6 FAB5
SLR_MLLW2                                    #fig 6 FAB5
#extra graphs
violin.BL                                  
violin.BL.dry                               
violin.BL.wet                               
ggarrange(violin.BL, ggarrange(violin.BL.wet,violin.BL.dry, ncol=2, labels=c("  Wet Season", "  Dry Season"), vjust= 2, hjust = -0.5),
          nrow=2,  labels = "Annual", vjust= 2, hjust = -1)#fig 4 FAB3
violin.SD
violin.SD.dry
violin.SD.wet
violin.LI
violin.LI.dry
violin.LI.wet
ggarrange(violin.SD, ggarrange(violin.SD.wet,violin.SD.dry, ncol=2, labels=c("Wet Season", "Dry Season"), vjust= 2, hjust = -0.5),
          nrow=2, labels = "Annual", vjust= 2, hjust = -1)#apendix
ggarrange(violin.LI, ggarrange(violin.LI.wet,violin.LI.dry, ncol=2, labels=c("Wet Season", "Dry Season"), vjust= 2, hjust = -0.5),
          nrow=2, labels = "Annual", vjust= 2, hjust = -1)#apendix

#data numbers ####

#rain
this_year_ann
this_year_wet
this_year_dry


#annual mean POR
daily.SD.DF$HY == this_report
SD_POR<- daily.SD.DF[daily.SD.DF$what == "POR",]
annual_mean_POR_SD<- mean(daily.SD.DF[daily.SD.DF$what == "POR",]$Depth)
SD_POR[SD_POR$Season == "2",]

mean(daily.BL.data[daily.BL.data$Season == "2",]$Depth ) -mean(daily.BL.data[daily.BL.data$Season == "2",]$DepthPOR)
mean(daily.BL.data$Depth ) -mean(daily.BL.data$DepthPOR)

















# inflow to the cape  -----------------------------------------------------

#install.packages("dataRetrieval")

# https://waterdata.usgs.gov/blog/dataretrieval/
# https://maps.waterdata.usgs.gov/mapper/index.html
library('dataRetrieval')



#parameter codes
# 00060 - Discharge
# 00065 - Gage height [MAIN NAVD88]
# 72137 - Discharge,tide fltrd
# 72255 - Mean water velocity [XVelocity]
# 00010 - Temperature, water
# 00095 - Specific cond at 25C
# 00480 - Salinity
# 63680 - Turbidity, Form Neph
# 00400 - pH
# parameterCd = "code number"






siteNo <- "05427948"
pCode <- "00060"
start.date <- "1989-06-01"
end.date <- "2024-05-31"

#EAST SIDE CREEK
EastSideCreek <- readNWISuv(siteNumbers = 250802081035500,
                       parameterCd = "all",
                       startDate = start.date,
                       endDate = end.date) %>% 
  renameNWISColumns() %>% 
  mutate(site = 'EastSideCreek',
         temp = Wtemp_Inst,
         flow = Flow_Inst, 
         waterlevel = GH_Inst,
         sal = X_00480_Inst,
         turbid = Turb_Inst,
         discarge_flow = X_72137_Inst) %>% 
  select(dateTime, site, temp, flow, waterlevel, sal, turbid, discarge_flow)


#RAULERSON BROTHERS
RaulersonBrothers <- readNWISuv(siteNumbers = 251115081075800,
                            parameterCd = "all",
                            startDate = start.date,
                            endDate = end.date) %>% 
  renameNWISColumns() %>% 
  mutate(site = 'RaulersonBrothers',
         flow = Flow_Inst, 
         waterlevel = GH_Inst,
         discarge_flow = X_72137_Inst, 
         temp = '',
         sal = '',
         turbid = '') %>% 
  select(dateTime, site, temp, flow, waterlevel, sal, turbid, discarge_flow)


ESC_USGS_depth <- 
  left_join(
    left_join(
      EastSideCreek %>%
        mutate(date_col = as.Date(dateTime)) %>%
        group_by(date_col) %>%
        summarise(es_waterlevel_sum = sum(waterlevel),
                  es_waterlevel_mean = mean(waterlevel, na.rm = T),
                  es_waterlevel_max = max(waterlevel, na.rm = T),
                  es_waterlevel_min = min(waterlevel, na.rm = T),
                  es_waterlevel_sd = sd(waterlevel, na.rm = T),
                  es_flow_sum = sum(flow),
                  es_flow_mean = mean(flow, na.rm = T),
                  es_flow_max = max(flow, na.rm = T),
                  es_flow_min = min(flow, na.rm = T),
                  es_flow_sd = sd(flow, na.rm = T)),
      RaulersonBrothers %>%
        mutate(date_col = as.Date(dateTime)) %>%
        group_by(date_col) %>%
        summarise(rb_waterlevel_sum = sum(waterlevel),
                  rb_waterlevel_mean = mean(waterlevel, na.rm = T),
                  rb_waterlevel_max = max(waterlevel, na.rm = T),
                  rb_waterlevel_min = min(waterlevel, na.rm = T),
                  rb_waterlevel_sd = sd(waterlevel, na.rm = T),
                  rb_flow_sum = sum(flow),
                  rb_flow_mean = mean(flow, na.rm = T),
                  rb_flow_max = max(flow, na.rm = T),
                  rb_flow_min = min(flow, na.rm = T),
                  rb_flow_sd = sd(flow, na.rm = T)),
      by = 'date_col'),
    left_join(
      LI_day %>% filter(YEAR >= 2009) %>% 
        rename(LI_depth = Depth, 
               LI_sal = Salinity) %>% 
        select(LABEL, LI_depth, LI_sal),
      BL_day %>% filter(YEAR >= 2009) %>% 
        rename(BL_depth = Depth, 
               BL_sal = Salinity) %>% 
        select(LABEL, BL_depth, BL_sal),
      by = 'LABEL') %>% 
      left_join( SD_day %>% filter(YEAR >= 2009) %>% 
                   rename(SD_depth = Depth, 
                          SD_sal = Salinity) %>% 
                   select(LABEL, SD_depth, SD_sal),
                 by = 'LABEL') %>% 
      rename(date_col = LABEL), 
    by = 'date_col')
  

ESC_USGS_depth


ggarrange(
ggplot(ESC_USGS_depth, aes(y=LI_sal, x=rb_waterlevel_sd))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE),

ggplot(ESC_USGS_depth, aes(y=SD_sal, x=rb_waterlevel_sd))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE),

ggplot(ESC_USGS_depth, aes(y=BL_sal, x=rb_waterlevel_sd))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE),

ggplot(ESC_USGS_depth, aes(y=LI_sal, x=es_waterlevel_sd))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE),

ggplot(ESC_USGS_depth, aes(y=SD_sal, x=es_waterlevel_sd))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE),

ggplot(ESC_USGS_depth, aes(y=BL_sal, x=es_waterlevel_sd))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE))

ggarrange(
  ggplot(ESC_USGS_depth, aes(y=LI_sal, x=rb_flow_sd))+
    geom_point()+
    geom_smooth(method=lm, se=FALSE),
  
  ggplot(ESC_USGS_depth, aes(y=SD_sal, x=rb_flow_sd))+
    geom_point()+
    geom_smooth(method=lm, se=FALSE),
  
  ggplot(ESC_USGS_depth, aes(y=BL_sal, x=rb_flow_sd))+
    geom_point()+
    geom_smooth(method=lm, se=FALSE),
  
  ggplot(ESC_USGS_depth, aes(y=LI_sal, x=es_flow_sd))+
    geom_point()+
    geom_smooth(method=lm, se=FALSE),
  
  ggplot(ESC_USGS_depth, aes(y=SD_sal, x=es_flow_sd))+
    geom_point()+
    geom_smooth(method=lm, se=FALSE),
  
  ggplot(ESC_USGS_depth, aes(y=BL_sal, x=es_flow_sd))+
    geom_point()+
    geom_smooth(method=lm, se=FALSE))


y <- ggarrange(
  ggplot(ESC_USGS_depth, aes(y=LI_depth, x=rb_waterlevel_max))+
    geom_point()+
    geom_smooth(method=lm, se=FALSE)+ 
    #ggtitle("Plot of length \n by dose") +
    ylab("LI depth (cm)") + xlab("RaulersonBrothers\n max depth (ft)"),
  
  ggplot(ESC_USGS_depth, aes(y=SD_depth, x=rb_waterlevel_max))+
    geom_point()+
    geom_smooth(method=lm, se=FALSE)+ 
    #ggtitle("Plot of length \n by dose") +
    ylab("SD depth (cm)") + xlab("RaulersonBrothers\n max depth (ft)"),
  
  ggplot(ESC_USGS_depth, aes(y=BL_depth, x=rb_waterlevel_max))+
    geom_point()+
    geom_smooth(method=lm, se=FALSE)+ 
    #ggtitle("Plot of length \n by dose") +
    ylab("BL depth (cm)") + xlab("RaulersonBrothers\n max depth (ft)"),
  
  ggplot(ESC_USGS_depth, aes(y=LI_depth, x=es_waterlevel_max))+
    geom_point()+
    geom_smooth(method=lm, se=FALSE)+ 
    #ggtitle("Plot of length \n by dose") +
    ylab("LI depth (cm)") + xlab("EastSideCreek\n max depth (ft)"),
  
  ggplot(ESC_USGS_depth, aes(y=SD_depth, x=es_waterlevel_max))+
    geom_point()+
    geom_smooth(method=lm, se=FALSE)+ 
    #ggtitle("Plot of length \n by dose") +
    ylab("SD depth (cm)") + xlab("EastSideCreek\n max depth (ft)"),
  
  ggplot(ESC_USGS_depth, aes(y=BL_depth, x=es_waterlevel_max))+
    geom_point()+
    geom_smooth(method=lm, se=FALSE)+ 
    #ggtitle("Plot of length \n by dose") +
    ylab("BL depth (cm)") + xlab("EastSideCreek\n max depth (ft)"))


x <- ggarrange(
  ggplot(ESC_USGS_depth, aes(x=as.Date(date_col), y=rb_waterlevel_sd))+
    geom_line()+ 
    geom_smooth()+
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
    xlab("") + ylab("RaulersonBrothers\n St.Dev depth (ft)"),
  ggplot(ESC_USGS_depth, aes(x=as.Date(date_col), y=es_waterlevel_sd))+
    geom_line()+ 
    geom_smooth()+
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
    xlab("") + ylab("EastSideCreek\n St.Dev depth (ft)"),
  ncol = 1)


ggarrange(x,y, ncol = 1)


capeflow <- rbind(ddply(EastSideCreek, 
                        .(date_d = as.Date(EastSideCreek$dateTime,format='%Y-%m-%d %H:%M:%S')),
                        summarise, 
                        site = site,
                        temp = mean(temp, na.rm=TRUE),
                        flow = mean(flow, na.rm=TRUE),
                        waterlevel = mean(waterlevel, na.rm=TRUE),
                        waterlevel_sd = sd(waterlevel, na.rm=TRUE),
                        sal = mean(sal, na.rm=TRUE),
                        turbid = mean(turbid, na.rm=TRUE),
                        discarge_flow = mean(discarge_flow, na.rm=TRUE)),
                  ddply(RaulersonBrothers, 
                        .(date_d = as.Date(RaulersonBrothers$dateTime,format='%Y-%m-%d %H:%M:%S')),
                        summarise, 
                        site = site,
                        temp = mean(temp, na.rm=TRUE),
                        flow = mean(flow, na.rm=TRUE),
                        waterlevel = mean(waterlevel, na.rm=TRUE),
                        waterlevel_sd = sd(waterlevel, na.rm=TRUE),
                        sal = mean(sal, na.rm=TRUE),
                        turbid = mean(turbid, na.rm=TRUE),
                        discarge_flow = mean(discarge_flow, na.rm=TRUE))) 





ggplot(capeflow, aes(x=as.Date(date_d), y=waterlevel, color=site))+
  geom_line(aes(linetype=site))+ 
  facet_grid(site ~ .) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")

ggplot(capeflow, aes(x=as.Date(date_d), y=flow, color=site))+
  geom_line(aes(linetype=site))+ 
  facet_grid(site ~ .) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")

ggplot(capeflow, aes(x=as.Date(date_d), y=discarge_flow, color=site))+
  geom_line(aes(linetype=site)) + 
  facet_grid(site ~ .) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")






ESC_sites <- rbind(
LI_day %>% filter(YEAR >= 2009) %>% mutate(site = 'LI') %>% 
  select(LABEL, site, Depth, Salinity),
BL_day %>% filter(YEAR >= 2009) %>% mutate(site = 'BL')%>% 
  select(LABEL, site, Depth, Salinity),
SD_day %>% filter(YEAR >= 2009) %>% mutate(site = 'SD')%>% 
  select(LABEL, site, Depth, Salinity))



capeflow %>% 
select(date_d, waterlevel, waterlevel_sd, site) %>% 
  tidyr::pivot_wider(names_from = site, 
                     names_glue = "{site}_{.value}",
              values_from = c(waterlevel,waterlevel_sd))

ESC_sites



ggplot(ESC_sites, aes(x=as.Date(LABEL), y=Depth, color=site))+
  geom_line(aes(linetype=site)) + 
  facet_grid(site ~ .) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")

ggplot(ESC_sites, aes(x=as.Date(LABEL), y=Salinity, color=site))+
  geom_line(aes(linetype=site)) + 
  facet_grid(site ~ .) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")




ggplot(capeflow %>% 
         filter(year(date_d) >= 2021), aes(x=as.Date(date_d), y=waterlevel, color=site))+
  geom_line(aes(linetype=site))+ 
  facet_grid(site ~ .) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")

ggplot(capeflow%>% 
         filter(year(date_d) >= 2021), aes(x=as.Date(date_d), y=flow, color=site))+
  geom_line(aes(linetype=site))+ 
  #facet_grid(site ~ .) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")

ggplot(capeflow%>% 
         filter(year(date_d) >= 2021), aes(x=as.Date(date_d), y=discarge_flow, color=site))+
  geom_line() + 
  #facet_grid(site ~ .) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")

ggplot(ESC_sites%>% 
        filter(year(LABEL) >= 2021), aes(x=as.Date(LABEL), y=Depth, color=site))+
  geom_line(aes(linetype=site)) + 
  facet_grid(site ~ .) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")

ggplot(ESC_sites%>% 
         filter(year(LABEL) >= 2021), aes(x=as.Date(LABEL), y=Salinity, color=site))+
  geom_line(aes(linetype=site)) + 
  facet_grid(site ~ .) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")


df1 <- capeflow %>% 
  mutate(month = month(date_d),
         year = year(date_d)) %>% 
  #filter(year >= 2000) %>% 
  group_by(year, month) %>% 
  dplyr::select(year, month, site, discarge_flow) %>% 
  tidyr::pivot_wider(names_from = site,
                     values_from = discarge_flow,
                     values_fn = ~ max(.x, na.rm = TRUE)) %>% 
  tidyr::drop_na()%>% 
  filter_all(all_vars(is.finite(.))) #%>% 
  #filter(EastSideCreek < 400) %>% 
  #filter(EastSideCreek > 0)

cor.test(df1$EastSideCreek, df1$RaulersonBrothers)

mylims <- range(with(df1, c(EastSideCreek, RaulersonBrothers)))

df1 %>% 
  ggplot(aes(EastSideCreek, RaulersonBrothers)) +
  geom_point() +
  geom_smooth(method = lm, formula=y ~ poly(x, 2, raw=TRUE),se = TRUE, color = 'red') +
  geom_smooth(method = lm, se = TRUE, color = 'green') + 
  coord_fixed()
  
ggarrange(
capeflow %>% 
  mutate(month = month(date_d),
         year = year(date_d)) %>% 
  filter(year > 2012) %>% 
  group_by(year, month) %>% 
  dplyr::select(year, month, site, flow) %>% 
  tidyr::pivot_wider(names_from = site,
                     values_from = flow,
                     values_fn = ~ min(.x, na.rm = TRUE)) %>% 
  #filter(EastSideCreek > 0) %>% 
  tidyr::drop_na()%>% 
  filter_all(all_vars(is.finite(.)))%>% 
  ggplot(aes(RaulersonBrothers,EastSideCreek)) +
  geom_point() +
  geom_smooth(method = lm, formula=y ~ poly(x, 2, raw=TRUE),se = TRUE, color = 'red') +
  geom_smooth(method = lm, se = TRUE, color = 'green') +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = y ~ poly(x, 2, raw=TRUE), color = 'darkgreen', label.y = -900)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = y ~ x, color = 'red', label.y = -930)+  
  #coord_fixed()+
  ggtitle('Min Monthly Flow'), 
capeflow %>% 
  mutate(month = month(date_d),
         year = year(date_d)) %>% 
  filter(year > 2012) %>% 
  group_by(year, month) %>% 
  dplyr::select(year, month, site, flow) %>% 
  tidyr::pivot_wider(names_from = site,
                     values_from = flow,
                     values_fn = ~ mean(.x, na.rm = TRUE)) %>% 
  filter(EastSideCreek > 0) %>% 
  tidyr::drop_na()%>% 
  filter_all(all_vars(is.finite(.)))%>% 
  ggplot(aes(RaulersonBrothers,EastSideCreek)) +
  geom_point() +
  geom_smooth(method = lm, formula=y ~ poly(x, 2, raw=TRUE),se = TRUE, color = 'red') +
  geom_smooth(method = lm, se = TRUE, color = 'green') +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = y ~ poly(x, 2, raw=TRUE), color = 'darkgreen', label.y = 550)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = y ~ x, color = 'red', label.y = 530)+ 
  #coord_fixed()+
  ggtitle('Mean Monthly Flow'),
capeflow %>% 
  mutate(month = month(date_d),
         year = year(date_d)) %>% 
  filter(year > 2012) %>% 
  group_by(year, month) %>% 
  dplyr::select(year, month, site, flow) %>% 
  tidyr::pivot_wider(names_from = site,
                     values_from = flow,
                     values_fn = ~ max(.x, na.rm = TRUE)) %>% 
  filter(EastSideCreek > 0) %>% 
  tidyr::drop_na()%>% 
  filter_all(all_vars(is.finite(.)))%>% 
  ggplot(aes(RaulersonBrothers,EastSideCreek)) +
  geom_point() +
  geom_smooth(method = lm, formula=y ~ poly(x, 2, raw=TRUE),se = TRUE, color = 'red') +
  geom_smooth(method = lm, se = TRUE, color = 'green') +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = y ~ poly(x, 2, raw=TRUE), color = 'darkgreen')+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = y ~ x, color = 'red', label.y = 1700)+
  #coord_fixed()+
  ggtitle('Max Monthly Flow'), 
ncol = 3 )



#annual 
ggarrange(
  capeflow %>% 
    mutate(month = month(date_d),
           year = year(date_d)) %>% 
    filter(year > 2010) %>% 
    group_by(year) %>% 
    dplyr::select(year, site, flow) %>% 
    tidyr::pivot_wider(names_from = site,
                       values_from = flow,
                       values_fn = ~ min(.x, na.rm = TRUE)) %>% 
    #filter(EastSideCreek > 0) %>% 
    tidyr::drop_na()%>% 
    filter_all(all_vars(is.finite(.)))%>% 
    ggplot(aes(RaulersonBrothers,EastSideCreek)) +
    geom_point() +
    geom_smooth(method = lm, formula=y ~ poly(x, 2, raw=TRUE),se = TRUE, color = 'red') +
    geom_smooth(method = lm, se = TRUE, color = 'green') + 
    #coord_fixed()+
    ggtitle('Min annual Flow'), 
  capeflow %>% 
    mutate(month = month(date_d),
           year = year(date_d)) %>% 
    #filter(year <= 2015) %>% 
    group_by(year) %>% 
    dplyr::select(year,  site, flow) %>% 
    tidyr::pivot_wider(names_from = site,
                       values_from = flow,
                       values_fn = ~ mean(.x, na.rm = TRUE)) %>% 
    filter(EastSideCreek > 0) %>% 
    tidyr::drop_na()%>% 
    filter_all(all_vars(is.finite(.)))%>% 
    ggplot(aes(RaulersonBrothers,EastSideCreek)) +
    geom_point() +
    geom_smooth(method = lm, formula=y ~ poly(x, 2, raw=TRUE),se = TRUE, color = 'red') +
    geom_smooth(method = lm, se = TRUE, color = 'green') + 
    #coord_fixed()+
    ggtitle('Mean Annual Flow'),
  capeflow %>% 
    mutate(month = month(date_d),
           year = year(date_d)) %>% 
    #filter(year <= 2015) %>% 
    group_by(year) %>% 
    dplyr::select(year, site, flow) %>% 
    tidyr::pivot_wider(names_from = site,
                       values_from = flow,
                       values_fn = ~ max(.x, na.rm = TRUE)) %>% 
    filter(EastSideCreek > 0) %>% 
    tidyr::drop_na()%>% 
    filter_all(all_vars(is.finite(.)))%>% 
    ggplot(aes(RaulersonBrothers,EastSideCreek)) +
    geom_point() +
    geom_smooth(method = lm, formula=y ~ poly(x, 2, raw=TRUE),se = TRUE, color = 'red') +
    geom_smooth(method = lm, se = TRUE, color = 'green') + 
    #coord_fixed()+
    ggtitle('Max Annual Flow'), 
  ncol = 3 )

# salinty flow ------------------------------------------------------------
site_esc <- 'SD'

left_join(
  ESC_sites %>% 
  filter(site == site_esc) %>% 
  mutate(year = year(LABEL),
         month = month(LABEL),
         date_d = LABEL) %>% 
  #dplyr::select(-c(Depth, LABEL)) %>% 
  filter(year > 2012) %>% 
  #group_by(year, month)%>% 
    dplyr::select(date_d, site, Salinity)  %>% 
  tidyr::pivot_wider(names_from = site,
                     values_from = Salinity,
                     values_fn = ~ max(.x, na.rm = TRUE)),
capeflow %>% 
  mutate(month = month(date_d),
         year = year(date_d)) %>% 
  filter(year > 2012) %>% 
  #group_by(year, month) %>% 
  dplyr::select(date_d,  site, flow) %>% 
  tidyr::pivot_wider(names_from = site,
                     values_from = flow,
                     values_fn = ~ min(.x, na.rm = TRUE)) %>% 
  #filter(EastSideCreek > 0) %>% 
  tidyr::drop_na()%>% 
  filter_all(all_vars(is.finite(.))),
by = c('date_d')
)%>% 
  tidyr::drop_na()%>% 
  filter_all(all_vars(is.finite(.)))%>% 
  ggplot(aes(RaulersonBrothers,SD)) +
  geom_point() +
  geom_smooth(method = lm, formula=y ~ poly(x, 2, raw=TRUE),se = TRUE, color = 'red') +
  geom_smooth(method = lm, se = TRUE, color = 'green') + 
  #coord_fixed()+
  ggtitle('Min Monthly Flow')


ggarrange(
  capeflow %>% 
    mutate(month = month(date_d),
           year = year(date_d)) %>% 
    filter(year > 2012) %>% 
    group_by(year, month) %>% 
    dplyr::select(year, month, site, flow) %>% 
    tidyr::pivot_wider(names_from = site,
                       values_from = flow,
                       values_fn = ~ min(.x, na.rm = TRUE)) %>% 
    #filter(EastSideCreek > 0) %>% 
    tidyr::drop_na()%>% 
    filter_all(all_vars(is.finite(.)))%>% 
    ggplot(aes(RaulersonBrothers,EastSideCreek)) +
    geom_point() +
    geom_smooth(method = lm, formula=y ~ poly(x, 2, raw=TRUE),se = TRUE, color = 'red') +
    geom_smooth(method = lm, se = TRUE, color = 'green') + 
    #coord_fixed()+
    ggtitle('Min Monthly Flow'), 
  capeflow %>% 
    mutate(month = month(date_d),
           year = year(date_d)) %>% 
    filter(year > 2012) %>% 
    group_by(year, month) %>% 
    dplyr::select(year, month, site, flow) %>% 
    tidyr::pivot_wider(names_from = site,
                       values_from = flow,
                       values_fn = ~ mean(.x, na.rm = TRUE)) %>% 
    filter(EastSideCreek > 0) %>% 
    tidyr::drop_na()%>% 
    filter_all(all_vars(is.finite(.)))%>% 
    ggplot(aes(RaulersonBrothers,EastSideCreek)) +
    geom_point() +
    geom_smooth(method = lm, formula=y ~ poly(x, 2, raw=TRUE),se = TRUE, color = 'red') +
    geom_smooth(method = lm, se = TRUE, color = 'green') + 
    #coord_fixed()+
    ggtitle('Mean Monthly Flow'),
  capeflow %>% 
    mutate(month = month(date_d),
           year = year(date_d)) %>% 
    filter(year > 2012) %>% 
    group_by(year, month) %>% 
    dplyr::select(year, month, site, flow) %>% 
    tidyr::pivot_wider(names_from = site,
                       values_from = flow,
                       values_fn = ~ max(.x, na.rm = TRUE)) %>% 
    filter(EastSideCreek > 0) %>% 
    tidyr::drop_na()%>% 
    filter_all(all_vars(is.finite(.)))%>% 
    ggplot(aes(RaulersonBrothers,EastSideCreek)) +
    geom_point() +
    geom_smooth(method = lm, formula=y ~ poly(x, 2, raw=TRUE),se = TRUE, color = 'red') +
    geom_smooth(method = lm, se = TRUE, color = 'green') + 
    #coord_fixed()+
    ggtitle('Max Monthly Flow'), 
  ncol = 3 )

#jerry extra ####


#asumtion is that SUMMARY NUMBERS IS THE SAME AS AVAILABLE DENSITY.... 

#BL.fish<- read.csv("BL_Available_Fish_from_jerry.csv", header= T)
#BL.fish$sampledate<- as.Date(BL.fish$sampledate, "%m/%d/%Y") 
#LI.fish<- read.csv("LI_Available_Fish_from_jerry.csv", header = T)
#LI.fish$sampledate<- as.Date(LI.fish$sampledate, "%m/%d/%Y") 
#all.BLfish<-BL.fish
#all.LIfish<-LI.fish

#BL.fish<-BL.fish[(BL.fish$hydro_yr=="2019"),] 
#BL.fish.df<- ddply(BL.fish, .(hydro_yr, sampledate, Month), summarise, avail_dens = mean(Avail.Dens, na.rm=TRUE))
#BL.fish.df$LABEL <- BL.fish.df$sampledate
#LI.fish<-LI.fish[(LI.fish$hydro_yr=="2019"),] 
#LI.fish.df<- ddply(LI.fish, .(hydro_yr, sampledate, Month), summarise, avail_dens = mean(Avail.Desn, na.rm=TRUE))
#LI.fish.df$LABEL <- LI.fish.df$sampledate#



#this is for excel data for Jerry
#all.BLfish.df<- ddply(all.BLfish, .(hydro_yr, sampledate, Month), summarise, avail_dens = mean(Avail.Dens, na.rm=TRUE),
#                      depth.site = mean(dep.site., na.rm=TRUE), sal.ppt = mean(sal.ppt, na.rm=TRUE))
#all.BLfish.df$LABEL <- all.BLfish.df$sampledate
#all.LIfish.df<- ddply(all.LIfish, .(hydro_yr, sampledate, Month), summarise, avail_dens = mean(Avail.Desn, na.rm=TRUE))
#all.LIfish.df$LABEL <- all.LIfish.df$sampledate
#table.BL.DF.fish1<- merge(x = BL_day, y = all.BLfish.df, by = "LABEL", all.x = TRUE)
#table.LI.DF.fish1<- merge(x = LI_day, y = all.LIfish.df, by = "LABEL", all.x = TRUE)

#df <- sapply(table.BL.DF.fish1, as.character)
#Month avail_dens depth.site sal.ppt
#hydro_yr sampledate
#table.BL.DF.fish1$sampledate[is.na(table.BL.DF.fish1$sampledate)] <- " "
#create csv (maybe remove NAs)
#write.csv(table.BL.DF.fish1, "BL_hydro_fish_JERRY.csv")
#write.csv(table.LI.DF.fish1, "LI_hydro_fish_JERRY.csv")


#daily.LI.DF.fish<- merge(x = daily.LI.DF, y = LI.fish.df, by = "LABEL", all.x = TRUE)
#daily.LI.DF.fish1<- daily.LI.DF.fish[(daily.LI.DF.fish$what=="NOW"),] 
#daily.BL.DF.fish<- merge(x = daily.BL.DF, y = BL.fish.df, by = "LABEL", all.x = TRUE)
#daily.BL.DF.fish1<- daily.BL.DF.fish[(daily.BL.DF.fish$what=="NOW"),] 


#H.plot.LI.DAILY<- ggplot(data = daily.LI.DF.fish1, aes(x=LABEL, y=Depth, label = Depth))+
#                  geom_line()+
#  scale_color_manual(values=c("blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
#  geom_point(aes(x=sampledate, y=Depth), colour= "red")+
#  geom_text(aes(x=sampledate, y=Depth), angle = 90, vjust = 0.5, nudge_y = 15, colour= "red")+
#  theme_bw() +
#  xlab("LI") + ylab("Water Level (cm)") + 
#  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
#  scale_y_continuous(breaks=seq(round(min(daily.LI.data$Depth)/5)*5, max(daily.LI.data$Depth), 5))+
#  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

#F.plot.LI.DAILY<- ggplot(data = daily.LI.DF.fish1, aes(x=LABEL))+
#  geom_col(aes(y=avail_dens), size =1, color= "darkblue", fill = "white")
#  geom_text(aes(x=sampledate, y=Depth), angle = 90, vjust = 0.5, nudge_y = 15, colour= "red")+
#  theme_bw() +
#  xlab("LI") + ylab("Water Level (cm)") + 
#  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
#  scale_y_continuous(breaks=seq(round(min(daily.LI.data$Depth)/5)*5, max(daily.LI.data$Depth), 5))+
#  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

#  ggplot(data = daily.LI.DF.fish1, aes(x=LABEL, label = Depth)) + 
#    geom_col(aes(y=avail_dens), size =0.5, color= "red", fill = "white") +
#    geom_line(aes(y=Depth, label = Depth), color="blue") +
#    geom_point(aes(x=sampledate, y=Depth), colour= "red")+
#    #geom_text(data = daily.BL.DF.fish1, aes(label= Depth, x=sampledate, y=Depth), angle = 90, 
#    #          vjust = 0.5, nudge_y = 0, colour= "blue")+
#geom_text(data = daily.BL.DF.fish1, aes(label= avail_dens, x=sampledate, y=Depth), angle = 90, 
#          vjust = 1.5, nudge_y = 0, colour= "red")+
#    theme_bw() +
#    xlab("LI") + ylab("Water Level (cm)") + 
#    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
#    scale_y_continuous(breaks=seq(round(min(daily.LI.data$Depth)/5)*5, max(daily.LI.data$Depth), 5),
#                       sec.axis = sec_axis(~.*1, name = "Available Fish Density"))+
#    theme(axis.text.x = element_text(angle=90), legend.title = element_blank())



#H.plot.BL.DAILY<- ggplot(data = daily.BL.DF.fish1, aes(x=LABEL, y=Depth, label = Depth))+
#                  geom_line()+
#  scale_color_manual(values=c("blue", "red"),labels=c(this_report,"Long Term\nAverage"))+
#  geom_point(aes(x=sampledate, y=Depth), colour= "red")+
#  geom_text(aes(x=sampledate, y=Depth), angle = 90, vjust = 0.5, nudge_y = 15, colour= "red")+
#  theme_bw() +
#  xlab("BL") + ylab("Water Level (cm)") + 
#  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
#  scale_y_continuous(breaks=seq(round(min(daily.BL.data$Depth)/5)*5, max(daily.BL.data$Depth), 5))+
#  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())






#F.plot.BL.DAILY<- ggplot(data = daily.BL.DF.fish1, aes(x=LABEL))+
#  geom_col(aes(y=avail_dens), size =1, color= "darkblue", fill = "white")
#geom_text(aes(x=sampledate, y=Depth), angle = 90, vjust = 0.5, nudge_y = 15, colour= "red")+
#  theme_bw() +
#  xlab("BL") + ylab("Water Level (cm)") + 
#  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
#  scale_y_continuous(breaks=seq(round(min(daily.BL.data$Depth)/5)*5, max(daily.BL.data$Depth), 5))+
#  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())

#ggplot(data = daily.BL.DF.fish1, aes(x=LABEL)) + 
#  geom_col(aes(y=avail_dens), size =0.5, color= "red", fill = "white") +
#  geom_line(aes(y=Depth), color="blue") +
#  geom_point(aes(x=sampledate, y=Depth), colour= "red")+
#geom_text(data = daily.BL.DF.fish1, aes(label= Depth, x=sampledate, y=Depth), angle = 90, 
#          vjust = 0.5, nudge_y = 20, colour= "blue")+
#geom_text(data = daily.BL.DF.fish1, aes(label= avail_dens, x=sampledate, y=Depth), angle = 90, 
#          vjust = 1.5, nudge_y = 20, colour= "red")+
#  theme_bw() +
#  xlab("BL") + ylab("Water Level (cm)") + 
#  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %Y"))+
#  scale_y_continuous(breaks=seq(round(min(daily.BL.data$Depth)/5)*5, max(daily.BL.data$Depth), 5),
#                     sec.axis = sec_axis(~.*1, name = "Available Fish Density"))+
#  theme(axis.text.x = element_text(angle=90), legend.title = element_blank())


#ggplot( data = daily.BL.DF.fish1, aes(y=avail_dens))+
#  geom_bar()


#https://biostats.w.uib.no/overlaying-a-line-plot-and-a-column-plot/
#ggplot(df) + 
#  geom_col(aes(x = ID, y = series1), size = 1, color = "darkblue", fill = "white") +
#  geom_line(aes(x = ID, y = 3*series2), size = 1.5, color="red", group = 1) + 
#  scale_y_continuous(sec.axis = sec_axis(~./3, name = "series2"))


# extra for jerry ####

#BL_day.min1<-ddply(BL_h, .(HY,MONTH,DAY), summarise, BL.depth.min = mean(Depth, na.rm=TRUE))
#BL_day.min2<-ddply(BL_day.min1, .(HY), summarise, depth = min(BL.depth.min, na.rm=TRUE))

#BL_h$Time <- format(BL_h$Date_Time,"%H:%M:%S")
#BL_min_day<-ddply(BL_h, .(HY,MONTH,DAY), summarise, BL.depth.min = min(Depth, na.rm=TRUE))
#BL_max_day<-ddply(BL_h, .(HY,MONTH,DAY), summarise, BL.depth.max = max(Depth, na.rm=TRUE))
#only_8_bl<- BL_h[c(which(BL_h$Time == "08:00:00")),]
#names(only_8_bl)[names(only_8_bl) == 'Depth'] <- 'depth_8am'
#names(only_8_bl)[names(only_8_bl) == 'Salinity'] <- 'salinity_8am'
#names(only_8_bl)[names(only_8_bl) == 'Temp'] <- 'temp_8am'

#BL_day$new_date<- with(BL_day, paste0(HY, MONTH, DAY))
#BL_min_day$new_date<- with(BL_min_day, paste0(HY, MONTH, DAY))
#BL_min_day<-BL_min_day[-c(which(BL_min_day$HY == "")),]
#BL_min_day<-BL_min_day[-c(which(BL_min_day$HY == "#VALUE!")),]
#BL_max_day$new_date<- with(BL_max_day, paste0(HY, MONTH, DAY)) 
#BL_max_day<-BL_max_day[-c(which(BL_max_day$HY == "")),]
#BL_max_day<-BL_max_day[-c(which(BL_max_day$HY == "#VALUE!")),]
#only_8_bl$new_date<- with(only_8_bl, paste0(HY, MONTH, DAY)) 

#BL_min_max_8am<- merge(BL_day,BL_min_day, by = "new_date", all.x = TRUE)
##BL_min_max_8am<- merge(BL_min_max_8am,BL_max_day, by = "new_date", all.x = TRUE)
#BL_min_max_8am<- merge(BL_min_max_8am,only_8_bl, by = "new_date", all.x = TRUE)

#write.csv(BL_min_max_8am, "test_jerry_data.csv")

#LI_h$Time <- format(LI_h$Date_Time,"%H:%M:%S")
#LI_min_day<-ddply(LI_h, .(HY,MONTH,DAY), summarise, LI.depth.min = min(Depth, na.rm=TRUE))
#LI_max_day<-ddply(LI_h, .(HY,MONTH,DAY), summarise, LI.depth.max = max(Depth, na.rm=TRUE))
#only_8_LI<- LI_h[c(which(LI_h$Time == "08:00:00")),]
#names(only_8_LI)[names(only_8_LI) == 'Depth'] <- 'depth_8am'
#names(only_8_LI)[names(only_8_LI) == 'Salinity'] <- 'salinity_8am'
#names(only_8_LI)[names(only_8_LI) == 'Temp'] <- 'temp_8am'

#LI_day$new_date<- with(LI_day, paste0(HY, MONTH, DAY))
#LI_min_day$new_date<- with(LI_min_day, paste0(HY, MONTH, DAY))
#LI_min_day<-LI_min_day[-c(which(LI_min_day$HY == "")),]
#LI_min_day<-LI_min_day[-c(which(LI_min_day$HY == "#VALUE!")),]
#LI_max_day$new_date<- with(LI_max_day, paste0(HY, MONTH, DAY)) 
#LI_max_day<-LI_max_day[-c(which(LI_max_day$HY == "")),]
#LI_max_day<-LI_max_day[-c(which(LI_max_day$HY == "#VALUE!")),]
#only_8_LI$new_date<- with(only_8_LI, paste0(HY, MONTH, DAY)) 

#LI_min_max_8am<- merge(LI_day,LI_min_day, by = "new_date", all.x = TRUE)
#LI_min_max_8am<- merge(LI_min_max_8am,LI_max_day, by = "new_date", all.x = TRUE)
#LI_min_max_8am<- merge(LI_min_max_8am,only_8_LI, by = "new_date", all.x = TRUE)

#LI.fish<- read.csv("LI_Available_Fish_from_jerry.csv", header = T)
#LI.fish$sampledate<- as.Date(LI.fish$sampledate, "%m/%d/%Y") 
#LI.fish$LABEL<-LI.fish$sampledate

#daily.LI.DF.fish<- merge(x = LI_min_max_8am, y = LI.fish, by = "LABEL", all.x = TRUE)
#write.csv(daily.LI.DF.fish, "LI_hydroCOMB_fish_jerry_data.csv")


