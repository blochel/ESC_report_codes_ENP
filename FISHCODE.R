#sample dates table, % catch phylogenic table, % catch salinity class table, % catch salinty class bargraph, % catch most abundant species (mosquito fish, sailfin molly,
# sheepshead minnow) line graph, % catch individual salinty classes 4 bar graphs, fish denisty BL and LI, fish biomass BL and LI, fish denisty BL and LI PRE 05-06 to 09-10,
#fish biomass BL and LI PRE 05-06 to 09-10



#####

# for next year -----------------------------------------------------------

##include available fish


# -------------------------------------------------------------------------







library("plyr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("grid")
library("gridExtra")
library("data.table")
library("kableExtra")
library("xtable")
library("ggpubr")
library('readxl')

this_hy <- '2023-24'
#download Fish data from access before running code. Save csv files in P:\Databases\tidy\Reports\NPS\fish_data. 
#make sure files are named HY_variable.csv; example 2021-22_catch.csv. 
#Always keep 'species list w salinity classes' and 'species list w order fmily' in the folder. 
#see how to download data from access in code below




# percent catch. 
# export/analyze data > % catch > hydrologic years, select all years > 
# all months > Bear Lake and Lake Ingraham > deep/creek, shallow/flats > 
# select all, sci. name, combine size categories > 
# year x site, no stratification, numbers, per net (9M^2) 
fish_catch<-read.csv(
  paste('/Databases/tidy/Reports/NPS/fish_data/',this_hy,'_catch.csv', sep = ''), 
  header=T, check.names=FALSE)[-1] %>% 
  filter(hydro_yr >= 2005) %>% 
  mutate(hydro_yr = paste(hydro_yr,sprintf('%02d', (hydro_yr+1) %% 100 ), sep = '-')) 



#density and biomass
# export/analyze data > summarized/multivariate > hydrologic years, select all years > 
# all months > Bear Lake and Lake Ingraham > deep/creek, shallow/flats >
#select all, sci. name, combine size categories, use size constraint <= 13.5 > 
# year x month x site, stratified replicates, numbers, biomass, per M^2
#here you get both density (numbers) and biomass (separated in sheets), 
#save density as xxxx-xx_density_weighted.csv, save biomass as xxxx-xx_biomass_weighted.csv


fish_density_biomass_weighted<-  read.csv(
  paste('/Databases/tidy/Reports/NPS/fish_data/',this_hy,'_density_Weighted.csv', sep = ''), 
  header=T, check.names=FALSE) %>% 
  rename(Total.Density = TOTAL) %>% 
  dplyr::select(year:Total.Density) %>% 
  cbind(
    read.csv(
      paste('/Databases/tidy/Reports/NPS/fish_data/',this_hy,'_biomass_Weighted.csv', sep = ''), 
      header=T, check.names=FALSE)%>% 
      rename(Total.Biomass = TOTAL) %>% 
      dplyr::select(Total.Biomass)
  )%>% 
  filter(hydro_yr >= 2005) %>% 
  mutate(hydro_yr = paste(hydro_yr,sprintf('%02d', (hydro_yr+1) %% 100 ), sep = '-'),
         Season = if_else(month >= 6 & month <= 11, 'Wet Season', 'Dry Season')) 

#total numbers of fish salinity class &total numbers of fish species
# export/analyze data > summarized/multivariate > hydrologic years, select all years > 
# all months > Bear Lake and Lake Ingraham > deep/creek, shallow/flats >
# select all, select all sal class (manual, there is no button for this...), combine size categories > 
# year x site, no stratification, numbers, per net(9M^2), efficiency: raw
#save Group_Numbers as BL_LI_total_Fish_sal.csv, save Summary_Numbers as BL_LI_total_Fish_species.csv

total_fish_sal<- read.csv(
  "/Databases/tidy/Reports/NPS/fish_data/BL_LI_total_Fish_sal.csv", header = T) %>% 
  filter(hydro_yr >= 2005)

total_fish_species<- read.csv(
  "/Databases/tidy/Reports/NPS/fish_data/BL_LI_total_Fish_species.csv", header= T) %>% 
  filter(hydro_yr >= 2005)


#these files are always the same 
fish_family<- read.csv(
  "/Databases/tidy/Reports/NPS/fish_data/species list w order fmily.csv", 
  header=T)
fish_salinity<- read.csv(
  "/Databases/tidy/Reports/NPS/fish_data/species list w salinity classes.csv", 
  header=T)


#using catch csv. no need to download from Access 
BL_3_fish<- read.csv(
  paste('/Databases/tidy/Reports/NPS/fish_data/',this_hy,'_catch.csv', sep = ''), 
  header=T, check.names=FALSE)[-1] %>% 
  filter(TSC_site_code == 'BL') %>% 
  mutate(hydro_yr = paste(hydro_yr,sprintf('%02d', (hydro_yr+1) %% 100 ), sep = '-')) 




fish_df<- left_join(fish_family, fish_salinity, "SPP...")




#make sure structure is followed 
fish_density_biomass_weighted$sampledate<- as.Date(fish_density_biomass_weighted$sampledate, "%m/%d/%Y") 
sample_month<- c("Jun", "Sep", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")
sample.month.x<- c("Jun", "Sep", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")
sample.months<- as.data.frame(sample_month, sample.month.x)
fish_density_biomass_weighted$sample_month<- factor(fish_density_biomass_weighted$sample_month, levels = sample.month.x)
#add other missing sites when working with older data (7p, TP)
fish_density_biomass_weighted$TSC_site_code<- factor(fish_density_biomass_weighted$TSC_site_code, 
                                                     levels = c("LI","BL","TR","EC","WJ","JB","SB","HC","MB","BS","CS"))  

#sites
biomassdensity_BL<- fish_density_biomass_weighted[fish_density_biomass_weighted$TSC_site_code == "BL", ]
biomassdensity_LI<- fish_density_biomass_weighted[fish_density_biomass_weighted$TSC_site_code == "LI", ]




#weighted annual biomass/m2 from BL and LI sample sites from 2005-06 ####


BL_biomass<- ddply(biomassdensity_BL, .(hydro_yr), summarise, Biomass = mean(Total.Biomass, na.rm=T), 
                   SE = sd(Total.Biomass, na.rm=T)/sqrt(length(Total.Biomass)))
BL_biomass$site<- "BL"
LI_biomass<- ddply(biomassdensity_LI, .(hydro_yr), summarise, Biomass = mean(Total.Biomass, na.rm=T), 
                   SE = sd(Total.Biomass, na.rm=T)/sqrt(length(Total.Biomass)))
LI_biomass$site<-"LI"

biomass_for_plot<- rbind(BL_biomass, LI_biomass)

anova(aov(biomassdensity_BL$Total.Biomass~biomassdensity_BL$hydro_yr))
TukeyHSD(aov(biomassdensity_BL$Total.Biomass~as.factor(biomassdensity_BL$hydro_yr)))
anova(aov(biomassdensity_LI$Total.Biomass~biomassdensity_LI$hydro_yr))
TukeyHSD(aov(biomassdensity_LI$Total.Biomass~as.factor(biomassdensity_LI$hydro_yr)))

  
Biomass_bar_plot<-  ggplot(biomass_for_plot, aes(x=as.factor(hydro_yr), y=Biomass, fill=site)) + 
                    geom_bar(stat="identity",color="black", position=position_dodge())+
                    geom_errorbar(aes(ymin=Biomass, ymax=Biomass+SE), width=.2,position=position_dodge(.9))+
                    theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
                    scale_y_continuous(breaks=seq(0,round(max(biomass_for_plot$SE)+max(biomass_for_plot$Biomass)),2))+
                    labs(x = "", y = expression ("Fish Biomass " (~g/m^2)))
  


#weighted annual density/m2 from BL and LI sample sites from 2005-06 ####

BL_density<- ddply(biomassdensity_BL, .(hydro_yr), summarise, Density = mean(Total.Density, na.rm=T), 
                   SE = sd(Total.Density, na.rm=T)/sqrt(length(Total.Density)))
BL_density$site<- "BL"
LI_density<- ddply(biomassdensity_LI, .(hydro_yr), summarise, Density = mean(Total.Density, na.rm=T), 
                   SE = sd(Total.Density, na.rm=T)/sqrt(length(Total.Density)))
LI_density$site<-"LI"

density_for_plot<- rbind(BL_density, LI_density)




Density_bar_plot<-ggplot(density_for_plot, aes(x=as.factor(hydro_yr), y=Density, fill=site)) + 
                  geom_bar(stat="identity",color="black", position=position_dodge())+
                  geom_errorbar(aes(ymin=Density, ymax=Density+SE), width=.2,position=position_dodge(.9))+
                  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
                  scale_y_continuous(breaks=seq(0,round(max(density_for_plot$SE)+max(density_for_plot$Density)),5))+
                  labs(x = "", y = expression ("Fish Density" (~fish/m^2) )) 

#Biomass
anova(aov(biomassdensity_BL$Total.Biomass~biomassdensity_BL$hydro_yr))
TukeyHSD(aov(biomassdensity_BL$Total.Biomass~as.factor(biomassdensity_BL$hydro_yr)))
anova(aov(biomassdensity_LI$Total.Biomass~biomassdensity_LI$hydro_yr))
TukeyHSD(aov(biomassdensity_LI$Total.Biomass~as.factor(biomassdensity_LI$hydro_yr)))
#Density
anova(aov(biomassdensity_BL$Total.Density~biomassdensity_BL$hydro_yr))
TukeyHSD(aov(biomassdensity_BL$Total.Density~as.factor(biomassdensity_BL$hydro_yr)))
anova(aov(biomassdensity_LI$Total.Density~biomassdensity_LI$hydro_yr))
TukeyHSD(aov(biomassdensity_LI$Total.Density~as.factor(biomassdensity_LI$hydro_yr)))

density_for_plot %>% filter(site == 'BL') %>% filter(hydro_yr != this_hy) %>% pull(Density) %>% mean()
density_for_plot %>% filter(site == 'BL') %>% filter(hydro_yr == this_hy) %>% pull(Density) %>% mean()
density_for_plot %>% filter(site == 'LI') %>% filter(hydro_yr != this_hy) %>% pull(Density) %>% mean()
density_for_plot %>% filter(site == 'LI') %>% filter(hydro_yr == this_hy) %>% pull(Density) %>% mean() 

biomass_for_plot %>% filter(site == 'BL') %>% filter(hydro_yr != this_hy) %>% pull(Biomass) %>% mean()
biomass_for_plot %>% filter(site == 'BL') %>% filter(hydro_yr == this_hy) %>% pull(Biomass) %>% mean()
biomass_for_plot %>% filter(site == 'LI') %>% filter(hydro_yr != this_hy) %>% pull(Biomass) %>% mean()
biomass_for_plot %>% filter(site == 'LI') %>% filter(hydro_yr == this_hy) %>% pull(Biomass) %>% mean()
biomass_for_plot %>% filter(hydro_yr == this_hy) 


# biomass / density with rain ---------------------------------------------


LI_day_rain<- read_excel("/Hydrology/Quality Checked Data_all years/Hydro Files/LI_HYDRO.xlsx",
                         guess_max=9999) %>%  
  filter(HY != '2023-24') %>% 
  rename(rain = `Rain (in.)` ) %>% 
  dplyr::select(LABEL, Season, HY, rain)



density_for_plot

#left_join(
#biomassdensity_BL %>% 
#  select(sampledate, hydro_yr, Season, Total.Density, Total.Biomass) %>% 
#  rename(Total.Density_BL = Total.Density,
#         Total.Biomass_BL = Total.Biomass),
#biomassdensity_LI%>% 
#  select(sampledate, hydro_yr, Season, Total.Density, Total.Biomass)%>% 
#  rename(Total.Density_LI = Total.Density,
#         Total.Biomass_LI = Total.Biomass),
#by = c('sampledate', 'hydro_yr', 'Season'))


rain_biomass_density <- 
  left_join(
    left_join(
      LI_day_rain %>% 
        mutate(Season = if_else(month(LABEL) >= 6 & month(LABEL) <= 11, 'Wet Season', 'Dry Season')),
      biomassdensity_BL %>% 
        rename(LABEL = sampledate) %>% 
        dplyr::select(LABEL, hydro_yr, Season, Total.Density, Total.Biomass) %>% 
        rename(Total.Density_BL = Total.Density,
               Total.Biomass_BL = Total.Biomass)),
    
    biomassdensity_LI%>% 
      rename(LABEL = sampledate) %>% 
      dplyr::select(LABEL, hydro_yr, Season, Total.Density, Total.Biomass)%>% 
      rename(Total.Density_LI = Total.Density,
             Total.Biomass_LI = Total.Biomass)) 
  



rain_biomass_density


ggplot(rain_biomass_density %>% 
         filter(Season == 'Wet Season'), aes(y=log(Total.Biomass_BL), x=log(rain)))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

rain_fish <- 
  rain_biomass_density %>% 
  group_by(hydro_yr, Season) %>% 
  summarise(sum_dens_BL = sum(Total.Density_BL, na.rm=T),
            sum_dens_LI = sum(Total.Density_LI, na.rm=T),
            mean_dens_BL = mean(Total.Density_BL, na.rm=T),
            mean_dens_LI = mean(Total.Density_LI, na.rm=T),
            sum_bio_BL = sum(Total.Biomass_BL, na.rm=T),
            sum_bio_LI = sum(Total.Biomass_LI, na.rm=T),
            mean_bio_BL = mean(Total.Biomass_BL, na.rm=T),
            mean_bio_LI = mean(Total.Biomass_LI, na.rm=T),
            sum_rain = sum(rain, na.rm = T),
            mean_rain = mean(rain, na.rm = T),
            max_rain = max(rain, na.rm = T)) %>% 
  filter(hydro_yr != "")

ggplot( rain_fish %>% 
          filter(Season == 'Dry Season'), aes(y=sum_bio_LI, x=max_rain))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

#maybe this to create table below bargraphs

#https://learnr.wordpress.com/2009/04/29/ggplot2-labelling-data-series-and-adding-a-data-table/
#data_table <- ggplot( biomass_for_plot, aes(x=as.factor(hydro_yr), y=Biomass,
#label = format(site, nsmall = 1), colour = Biomass)) +
#  geom_text(size = 3.5) + 
#  theme_bw()  + 
#  scale_y_discrete(formatter = abbreviate,limits = c("BL", "LI"))+
#  opts(panel.grid.major = none, legend.position = "none",
#       panel.border = none, axis.text.x = none,
#       axis.ticks = none) + opts(plot.margin = unit(c(-0.5,
#                                                      1, 0, 0.5), "lines")) + xlab(NULL) + ylab(NULL)







#density and biomass pre/post damn, 2005-06 - 2009-2010 vs 2011-12 - 2018-19####
# this section is weighted 

x1<-fish_density_biomass_weighted[fish_density_biomass_weighted$hydro_yr == "2005-06", ]
x2<-fish_density_biomass_weighted[fish_density_biomass_weighted$hydro_yr == "2006-07", ]
x3<-fish_density_biomass_weighted[fish_density_biomass_weighted$hydro_yr == "2007-08", ]
x4<-fish_density_biomass_weighted[fish_density_biomass_weighted$hydro_yr == "2008-09", ]
x5<-fish_density_biomass_weighted[fish_density_biomass_weighted$hydro_yr == "2009-10", ]
pre_dam<- rbind(x1,x2,x3,x4,x5)
pre_dam$period<- "PRE"

y1<-fish_density_biomass_weighted[fish_density_biomass_weighted$hydro_yr != "2005-06", ]
y2<-y1[y1$hydro_yr != "2006-07", ]
y3<-y2[y2$hydro_yr != "2007-08", ]
y4<-y3[y3$hydro_yr != "2008-09", ]
post_dam<-y4[y4$hydro_yr != "2009-10", ]
post_dam$period<- "POST"


post_pre_df<- rbind(pre_dam, post_dam)


#sites
post_pre_df_BL<- post_pre_df[post_pre_df$TSC_site_code == "BL", ]
post_pre_df_LI<- post_pre_df[post_pre_df$TSC_site_code == "LI", ]


#density
BL_density_post_pre<- ddply(post_pre_df_BL, .(period), summarise, Density = mean(Total.Density, na.rm=T), SE = sd(Total.Density, na.rm=T)/sqrt(length(Total.Density)))
BL_density_post_pre$site<- "BL"
LI_density_post_pre<- ddply(post_pre_df_LI, .(period), summarise, Density = mean(Total.Density, na.rm=T), SE = sd(Total.Density, na.rm=T)/sqrt(length(Total.Density)))
LI_density_post_pre$site<-"LI"
density_for_post_pre_plot<- rbind(BL_density_post_pre, LI_density_post_pre)
density_for_post_pre_plot$period<-  ordered(density_for_post_pre_plot$period, levels = c("PRE", "POST"))

Density_bar_post_pre_plot<-ggplot(density_for_post_pre_plot, aes(x=as.factor(site), y=Density, fill=period)) + 
  geom_bar(stat="identity",color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=Density, ymax=Density+SE), width=.2,position=position_dodge(.9))+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(x = "", y = expression ("Fish Density" (~fish/m^2) ))+
  theme(legend.title = element_blank())

#biomass
BL_biomass_post_pre<- ddply(post_pre_df_BL, .(period), summarise, biomass = mean(Total.Biomass, na.rm=T), SE = sd(Total.Biomass, na.rm=T)/sqrt(length(Total.Biomass)))
BL_biomass_post_pre$site<- "BL"
LI_biomass_post_pre<- ddply(post_pre_df_LI, .(period), summarise, biomass = mean(Total.Biomass, na.rm=T), SE = sd(Total.Biomass, na.rm=T)/sqrt(length(Total.Biomass)))
LI_biomass_post_pre$site<-"LI"
biomass_for_post_pre_plot<- rbind(BL_biomass_post_pre, LI_biomass_post_pre)
biomass_for_post_pre_plot$period<-  ordered(biomass_for_post_pre_plot$period, levels = c("PRE", "POST"))

Biomass_bar_post_pre_plot<-  ggplot(biomass_for_post_pre_plot, aes(x=as.factor(site), y=biomass, fill=period)) + 
  geom_bar(stat="identity",color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=biomass, ymax=biomass+SE), width=.2,position=position_dodge(.9))+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(x = "", y = expression ("Fish Biomass " (~g/m^2)))+
  theme(legend.title = element_blank())

anova(aov(post_pre_df_BL$Total.Density~post_pre_df_BL$period))
TukeyHSD(aov(biomass_for_post_pre_plot$biomass~as.character(biomass_for_post_pre_plot$period)))
anova(aov(post_pre_df_BL$Total.Biomass~post_pre_df_BL$period))
TukeyHSD(aov(biomass_for_post_pre_plot$biomass~biomass_for_post_pre_plot$period))

anova(aov(post_pre_df_LI$Total.Density~post_pre_df_LI$period))
TukeyHSD(aov(biomass_for_post_pre_plot$biomass~as.character(biomass_for_post_pre_plot$period)))
anova(aov(post_pre_df_LI$Total.Biomass~post_pre_df_LI$period))
TukeyHSD(aov(biomass_for_post_pre_plot$biomass~biomass_for_post_pre_plot$period))

#catch ####


sci_name.genus_species<-as.data.frame(c(names(fish_catch[7:length(fish_catch)])))
original<-as.data.frame(c(names(fish_catch[7:length(fish_catch)])))
fish_names<-as.data.frame(cbind(sci_name.genus_species,original))
fish_names<-setNames(fish_names, c("sci_name.x","original"))
#all species_genus good. 
#change "unknown" to "unknown larvae n/a "

#fish_df = "Sphoeroides parvus"
#fish_names ="Sphoeroides  parvus"
fish_df$sci_name.x[fish_df$sci_name.x == "Sphoeroides parvus"] <- "Sphoeroides  parvus"


#remove spaces after names
fish_df$sci_name.x<- trimws(fish_df$sci_name.x) 

fish_this_data<-left_join(fish_names, fish_df, "sci_name.x")
#remove goliath grouper 

#fish_this_data<-fish_this_data[fish_this_data$sci_name.y != "Epinephelus itajara", ]



fish_this_data<- fish_this_data[!grepl("BIG", fish_this_data$code.x),]
#fish_this_data<- fish_this_data[complete.cases(fish_this_data), ]
fish_this_data<-left_join(fish_names, fish_this_data, "sci_name.x")
fish_this_data[is.na(fish_this_data)] <- "unknown"

#create new df with sal class instead of name
salclass_fish<- fish_catch
colnames(salclass_fish)[7:length(salclass_fish)] <- c(fish_this_data$sal_class.x)
colnames(salclass_fish)[7:length(salclass_fish)] <- c(fish_this_data$sal_class.x)



salclass_fish<- as.data.frame(salclass_fish)
names(salclass_fish)[names(salclass_fish) == 'euryhaline'] <- 'euhaline'





df_salclass_fish<- salclass_fish %>% 
                   pivot_longer(!c("hydro_yr" ,"Site_ID" ,
                                   "TSC_site_code", "# of nets", 
                                   "hydro_yr", "site_ID", "TOTAL"), 
                                names_to = "salinity", values_to = "catch", 
                   names_repair = "unique")



df_salclass_fish$salinity <- factor(df_salclass_fish$salinity, levels = c("freshwater", "oligohaline", "mesohaline","polyhaline", "euhaline", "unknown"))



#sites
df_salclass_fish_BL<- df_salclass_fish[df_salclass_fish$TSC_site_code == "BL", ]
df_salclass_fish_LI<- df_salclass_fish[df_salclass_fish$TSC_site_code == "LI", ]


#graphs all salinity classes 

BL_salclass_bar_plot<-ggplot(df_salclass_fish_BL, aes(x=as.factor(hydro_yr), y=catch, fill=salinity)) + 
  geom_bar(stat="identity")+
  #geom_errorbar(aes(ymin=Density, ymax=Density+SE), width=.2,position=position_dodge(.9))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(x = "", y = expression ("% Catch" )) 
  

  
LI_salclass_bar_plot<-ggplot(df_salclass_fish_LI, aes(x=as.factor(hydro_yr), y=catch, fill=salinity)) + 
  geom_bar(stat="identity")+
  #geom_errorbar(aes(ymin=Density, ymax=Density+SE), width=.2,position=position_dodge(.9))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(x = "", y = expression ("% Catch" )) 




#catch graphs individual salinity classes onlt LI

LI_oligohaline<- df_salclass_fish_LI[df_salclass_fish_LI$salinity == "oligohaline", ]
LI_mesohaline<- df_salclass_fish_LI[df_salclass_fish_LI$salinity == "mesohaline", ]
LI_polyhaline<- df_salclass_fish_LI[df_salclass_fish_LI$salinity == "polyhaline", ]
LI_euhaline<- df_salclass_fish_LI[df_salclass_fish_LI$salinity == "euhaline", ]


LI_oligohaline_bar_plot<-ggplot(LI_oligohaline, aes(x=as.factor(hydro_yr), y=catch)) + 
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x = "", y = expression ("% Catch" ))+
  ylim(0, 100)+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

LI_mesohaline_bar_plot<-ggplot(LI_mesohaline, aes(x=as.factor(hydro_yr), y=catch)) + 
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x = "", y = expression ("% Catch" ))+
  ylim(0, 100)+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

LI_polyhaline_bar_plot<-ggplot(LI_polyhaline, aes(x=as.factor(hydro_yr), y=catch)) + 
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x = "", y = expression ("% Catch" ))+
  ylim(0, 100)+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

LI_euhaline_bar_plot<-ggplot(LI_euhaline, aes(x=as.factor(hydro_yr), y=catch)) + 
  geom_bar(stat="identity")+
  theme_bw()+
  labs(x = "", y = expression ("% Catch" ))+
  ylim(0, 100)+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


# catch Gambusia affinis, poe-lat, cyp-var ####

#need data going back to 1990-91

#three_fish<- fish_catch[,-1]
#names(three_fish)[names(three_fish) == "Gambusia affinis"] <- "Gambusia_affinis"
#names(three_fish)[names(three_fish) == "Poecilia latipinna"] <- "Poecilia_latipinna"
#names(three_fish)[names(three_fish) == "Cyprinodon variegatus"] <- "Cyprinodon_variegatus"

#three_fish<- ddply(three_fish, .(hydro_yr), summarise, Gambusia_affinis = mean(Gambusia_affinis, na.rm=T), 
#             Poecilia_latipinna = mean(Poecilia_latipinna, na.rm=T),Cyprinodon_variegatus = mean(Cyprinodon_variegatus, na.rm=T))


three_fish<- ddply(BL_3_fish %>% 
                     rename(HY=hydro_yr), .(HY), summarise, Gambusia_affinis = mean(`Gambusia affinis` , na.rm=T), 
                      Cyprinodon_variegatus = mean(`Cyprinodon variegatus`, na.rm=T),
                   Poecilia_latipinna = mean(`Poecilia latipinna`, na.rm=T))

GAMBUSIA<-three_fish[,1:2]
GAMBUSIA$species<- "Gambusia affinis"
colnames(GAMBUSIA)[2] <- "catch"
POECILIA<-three_fish[,c(1,3)]
POECILIA$species<- "Poecilia latipinna"
colnames(POECILIA)[2] <- "catch"
CYPRINODON<-three_fish[,c(1,4)]
CYPRINODON$species<- "Cyprinodon variegatus"
colnames(CYPRINODON)[2] <- "catch"

three_fish<- rbind(GAMBUSIA, POECILIA, CYPRINODON)


three_fish_line_graph<- ggplot(three_fish, aes(x=as.factor(HY), y=catch, group=species)) + 
                        geom_line(aes(color=species))+
                        geom_point(aes(color=species))+
                        theme_bw()+
                        scale_y_continuous(breaks=seq(0,round(max(three_fish$catch)+max(three_fish$catch)),10))+
                        labs(x = "", y = expression ("% Catch" )) +
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),legend.position = "right")

BL3FISH_PERCENTAGE_OF_CATCH<- ddply(three_fish, .(HY, species), summarise, prercentage = sum(catch , na.rm=T))


  







#tables #### #sample dates, NOT SAMPLED MISSING #####    #NEED TO ADD MANUALLY IF WE DIDNT SAMPLE! #####
BL.sample.date<- ddply(biomassdensity_BL, .(hydro_yr, sample_month), summarise, date = mean(sampledate))
LI.sample.date<- ddply(biomassdensity_LI, .(hydro_yr, sample_month), summarise, date = mean(sampledate))



#dates where no samples where collected 
biomassdensity_BL %>% 
  filter(hydro_yr == this_hy) %>% 
  filter(sample_month %in% 
setdiff(
  biomassdensity_BL %>% 
    filter(hydro_yr == this_hy) %>% 
    pull(sample_month) %>% unique(),
  biomassdensity_BL %>% 
  filter(hydro_yr == this_hy) %>% 
  drop_na(Total.Density) %>% 
  pull(sample_month) %>% unique() )) %>% 
  pull(sampledate) %>% unique()

biomassdensity_LI %>% 
  filter(hydro_yr == this_hy) %>% 
  filter(sample_month %in% 
           setdiff(
             biomassdensity_LI %>% 
               filter(hydro_yr == this_hy) %>% 
               pull(sample_month) %>% unique(),
             biomassdensity_LI %>% 
               filter(hydro_yr == this_hy) %>% 
               drop_na(Total.Density) %>% 
               pull(sample_month) %>% unique() )) %>% 
  pull(sampledate) %>% unique()

#BL
BL.sample.date<-left_join(sample.months, BL.sample.date, "sample_month")
BL.sample.date$date<-as.Date(BL.sample.date$date, format = "%Y-%m-%d")
BL.sample.date$date<-format.Date(BL.sample.date$date, "%d-%b-%y")
#need to add manually if we did not sample!
BL.sample.date$date[BL.sample.date$date == "21-Oct-17"] <- "NSHI"
BL.sample.date$date[BL.sample.date$date == "27-Dec-13"] <- "NS"
BL.sample.date$date[BL.sample.date$date == "23-Jan-14"] <- "NS"
BL.sample.date$date[BL.sample.date$date == "17-Feb-11"] <- "Dry"
BL.sample.date$date[BL.sample.date$date == "03-May-11"] <- "Dry"
BL.sample.date$date[BL.sample.date$date == "25-Apr-13"] <- "Dry"
BL.sample.date$date[BL.sample.date$date == "16-Mar-22"] <- "Dry"
BL.sample.date$date[BL.sample.date$date == "09-Jun-22"] <- "Flooded"
BL.sample.date$date[BL.sample.date$date == "21-Feb-23"] <- "Dry"
BL.sample.date$date[BL.sample.date$date == "29-Jan-24"] <- "Dry"
BL.sample.date$date[BL.sample.date$date == "28-Apr-24"] <- "Dry"


# dry?
# 7/1/2021


#LI
LI.sample.date<-left_join(sample.months, LI.sample.date, "sample_month")
LI.sample.date$date<-as.Date(LI.sample.date$date, format = "%Y-%m-%d")
LI.sample.date$date<-format.Date(LI.sample.date$date, "%d-%b-%y")
#need to add manually if we did not sample!
LI.sample.date$date[LI.sample.date$date == "14-Jul-05"] <- ""
LI.sample.date$date[LI.sample.date$date == "05-Aug-05"] <- ""
LI.sample.date$date[LI.sample.date$date == "14-Oct-05"] <- ""
LI.sample.date$date[LI.sample.date$date == "24-Nov-08"] <- "Dry"
LI.sample.date$date[LI.sample.date$date == "22-Jan-09"] <- "Dry"
LI.sample.date$date[LI.sample.date$date == "28-Jan-10"] <- "Dry"
LI.sample.date$date[LI.sample.date$date == "23-Mar-21"] <- "COVID"

BL_sampleDates<- pivot_wider(BL.sample.date, names_from = sample_month, values_from = date) 
names(BL_sampleDates)[names(BL_sampleDates) == "hydro_yr"] <- "Hydro Year"
#BL_sampleDates %>% format_cells(1, nrow(BL_sampleDates), "bold")


LI_sampleDates<- pivot_wider(LI.sample.date, names_from = sample_month, values_from = date) 
names(LI_sampleDates)[names(LI_sampleDates) == "hydro_yr"] <- "Hydro Year"

#BL_sampleDates<- as.data.frame(BL_sampleDates)
#format_cells <- function(df, rows ,cols, value = c("italics", "bold", "strikethrough")){
  
  # select the correct markup
  # one * for italics, two ** for bold
#  map <- setNames(c("*", "**", "~~"), c("italics", "bold", "strikethrough"))
#  markup <- map[value]  
#  for (r in rows){
#    for(c in cols){
#      
#      # Make sure values are not factors
#      df[[c]] <- as.character( df[[c]])
#      
#      # Update formatting
#      df[r, c] <- paste0(markup, df[r, c], markup)
#    }
#  }
#  
#  return(df)
#}
#BL_sampleDates<- format_cells(BL_sampleDates, 1:nrow(BL_sampleDates), 1, value = "bold")

#tables
table_sample_date_BL<- knitr::kable(BL_sampleDates, format="html", escape=FALSE, align=rep('c', length(BL_sampleDates[,1])), 
                       caption = "Bear Lake Sample Dates") %>%
                       kable_classic(lightable_options = "striped", full_width = F, html_font = "Cambria", font_size=12) 

table_sample_date_LI<- knitr::kable(LI_sampleDates, format="html", escape=FALSE, align=rep('c', length(BL_sampleDates[,1])), 
                       caption = "Lake Ingraham Sample Dates") %>%
                       kable_classic(lightable_options = "striped", full_width = F, html_font = "Cambria", font_size=12)
    

#salinity species table #####



total_fish_sal
total_fish_sal[total_fish_sal$TSC_site_code == "BL",]

#ddply(    , .(hydro_yr), summarise, prercentage = sum(TOTAL , na.rm=T))

sal_fish_LI<- df_salclass_fish_LI[,-1] 
names(sal_fish_LI)[names(sal_fish_LI) == "hydro_yr"] <- "Hydro Year"

LI_fish_sal_table<- sal_fish_LI %>% 
  dplyr::group_by(`Hydro Year`,salinity) %>%  
  dplyr::reframe(catch = sum(catch, na.rm = TRUE)) %>% 
  pivot_wider( names_from = "Hydro Year", values_from = catch)
LI_fish_sal_table<- as.data.frame(LI_fish_sal_table)
#LI_fish_sal_table<- LI_fish_sal_table[,-(1:4)]

LI_fish_sal_table$salinity<-factor(LI_fish_sal_table$salinity, levels = c("freshwater", "oligohaline", "mesohaline","polyhaline", "euhaline", "unknown"))

LI_fish_sal_table<-LI_fish_sal_table[order(LI_fish_sal_table$salinity ),]
LI_fish_sal_table$'POR %' <-rowMeans(LI_fish_sal_table[,c(2:ncol(LI_fish_sal_table))], na.rm = TRUE)
LI_fish_sal_table[LI_fish_sal_table==0] <- ""
LI_fish_sal_table$'POR %'<- format(round(as.numeric(LI_fish_sal_table$'POR %', 2)), nsmall = 2)
LI_fish_sal_table$'POR %'<-as.character(LI_fish_sal_table$'POR %')
LI_fish_sal_table$'POR %'[LI_fish_sal_table$'POR %'==" 0.00"] <- "<0.01"
row.names(LI_fish_sal_table) <- NULL


table_sal_fish_LI<- knitr::kable(LI_fish_sal_table, format="html", escape=FALSE, align=rep('c', length(LI_fish_sal_table[,1])), 
                                    caption = "Lake Ingraham") %>%row_spec(0,bold=TRUE) %>%
                                    kable_classic(lightable_options = "striped", full_width = F, html_font = "Cambria", font_size=12)



sal_fish_BL<- df_salclass_fish_BL[,-1]
names(sal_fish_BL)[names(sal_fish_BL) == "hydro_yr"] <- "Hydro Year"

BL_fish_sal_table<- sal_fish_BL %>% 
  dplyr::group_by(`Hydro Year`,salinity) %>%  
  dplyr::reframe(catch = sum(catch, na.rm = TRUE)) %>% 
  pivot_wider( names_from = "Hydro Year", values_from = catch)
BL_fish_sal_table<- as.data.frame(BL_fish_sal_table)
#BL_fish_sal_table<- BL_fish_sal_table[,-(1:4)]

BL_fish_sal_table$salinity<-factor(BL_fish_sal_table$salinity, levels = c("freshwater", "oligohaline", "mesohaline","polyhaline", "euhaline", "unknown"))
BL_fish_sal_table<-BL_fish_sal_table[order(BL_fish_sal_table$salinity ),]
BL_fish_sal_table$'POR %' <-rowMeans(BL_fish_sal_table[,c(2:ncol(BL_fish_sal_table))], na.rm = TRUE)
BL_fish_sal_table[BL_fish_sal_table==0] <- ""
BL_fish_sal_table[BL_fish_sal_table == 100.01] <- "100"
BL_fish_sal_table$'POR %'<- format(round(BL_fish_sal_table$'POR %', 2), nsmall = 2)
BL_fish_sal_table$'POR %'<-as.character(BL_fish_sal_table$'POR %')
BL_fish_sal_table$'POR %'[BL_fish_sal_table$'POR %'==" 0.00"] <- "<0.01"
row.names(BL_fish_sal_table) <- NULL

table_sal_fish_BL<- knitr::kable(BL_fish_sal_table, format="html", escape=FALSE, align=rep('c', length(BL_fish_sal_table[,1])), 
                                 caption = "Bear Lake") %>% row_spec(0,bold=TRUE) %>%
                                 kable_classic(lightable_options = "striped", full_width = F, html_font = "Cambria", font_size=12)








#phylogenetic fish table ####



phylogeny<- as.data.frame(cbind(fish_this_data$order, fish_this_data$fmily, fish_this_data$sci_name.x, fish_this_data$common.name.x))
colnames(phylogeny) <- c("Order", "Family", "Genus Species", "Common Name")

fish_catch_BL<- fish_catch[fish_catch$TSC_site_code == "BL", ]
fish_catch_LI<- fish_catch[fish_catch$TSC_site_code == "LI", ]

df_species_fish_BL<- fish_catch_BL %>% 
                     pivot_longer(!c("hydro_yr" ,"Site_ID" ,"TSC_site_code", "# of nets", "hydro_yr", "site_ID", "TOTAL"), names_to = "Genus Species", values_to = "catch", 
                     names_repair = "unique")

df_species_fish_LI<- fish_catch_LI %>% 
                     pivot_longer(!c("hydro_yr" ,"Site_ID" ,"TSC_site_code", "# of nets", "hydro_yr", "site_ID", "TOTAL"), names_to = "Genus Species", values_to = "catch", 
                     names_repair = "unique")

#have "Eucinostomus spp.", "Floridichthys carpio", "unknown larvae n/a", "Cichlasoma urophthalmus", 

format_cells <- function(df, rows ,cols, value = c("italics", "bold", "strikethrough")){
  
  # select the correct markup
  # one * for italics, two ** for bold
  map <- setNames(c("<i>", "**", "~~"), c("italics", "bold", "strikethrough"))
  markup <- map[value]  
  
  for (r in rows){
    for(c in cols){
      
      # Make sure values are not factors
      df[[c]] <- as.character( df[[c]])
      
      # Update formatting
      df[r, c] <- paste0(markup, df[r, c], markup)
    }
  }
  
  return(df)
}

BL_phylo<- df_species_fish_BL %>% 
  dplyr::group_by(hydro_yr,`Genus Species`) %>%  
  dplyr::reframe(catch = sum(catch, na.rm = TRUE)) %>% 
  pivot_wider( names_from = hydro_yr, values_from = catch)
#BL_phylo<- as.data.frame(BL_phylo[,-(1:5)])
LI_phylo<- df_species_fish_LI %>% 
  dplyr::group_by(hydro_yr,`Genus Species`) %>%  
  dplyr::reframe(catch = sum(catch, na.rm = TRUE)) %>% 
  pivot_wider( names_from = hydro_yr, values_from = catch)
#LI_phylo<- as.data.frame(LI_phylo[,-(1:5)])

BL_phylo_df<- left_join(phylogeny, BL_phylo, "Genus Species")
LI_phylo_df<- left_join(phylogeny, LI_phylo, "Genus Species")

#add row of zeros
#BL_phylo_df<- rbind(BL_phylo_df, as.vector(matrix(0,nrow=ncol(BL_phylo_df))))
#add total numbers 
#BL_phylo_df<- rbind(BL_phylo_df,c(0,0,0, "Total # fish", total_fish_species[total_fish_species$TSC_site_code == "BL",]$TOTAL ,
#  sum(total_fish_species[total_fish_species$TSC_site_code == "BL",]$TOTAL)))

#LI_phylo = "Sphyraena barracuda"
#LI_phylo = "Brevoortia spp." 
#phylogeny = "Sphyraena barracuda" 
#phylogeny = "Brevoortia spp." 
#LI_phylo_df = "Brevoortia spp." 
#LI_phylo_df = "Sphyraena barracuda"

BL_phylo_df<- BL_phylo_df[BL_phylo_df$`Genus Species`!="unknown",]
BL_phylo_df$TOTAL <-rowMeans(BL_phylo_df[,c(5:ncol(BL_phylo_df))], na.rm = TRUE)

BL_phylo_df$`Genus Species` [BL_phylo_df$`Genus Species` == "unknown larvae n/a"] <- ""
BL_phylo_df$`Common Name` [BL_phylo_df$`Common Name` == "larvae"] <- "Juvenile Fish Larvae"
BL_phylo_df<-BL_phylo_df[BL_phylo_df$TOTAL!=0,]
BL_phylo_df<- as.data.frame(BL_phylo_df, row.names=NULL)
BL_phylo_df$Order<-toupper(BL_phylo_df$Order) 
BL_phylo_df<- BL_phylo_df[order(BL_phylo_df$`Genus Species` ),]
BL_phylo_df<- BL_phylo_df[order(BL_phylo_df$Family ),]
BL_phylo_df<- BL_phylo_df[order(BL_phylo_df$Order ),]
BL_phylo_df$Family<- trimws(BL_phylo_df$Family) 
BL_phylo_df$Order<- trimws(BL_phylo_df$Order) 
BL_phylo_df$Order[duplicated(BL_phylo_df$Order)] <- ""
BL_phylo_df$Family[duplicated(BL_phylo_df$Family)] <- ""
BL_phylo_df<- format_cells(BL_phylo_df,c(2:nrow(BL_phylo_df)) ,3, value ="italics")
BL_phylo_df$TOTAL<- format(round(BL_phylo_df$TOTAL, 2), nsmall = 2)
BL_phylo_df[BL_phylo_df==0] <- ""
BL_phylo_df$TOTAL<-as.character(BL_phylo_df$TOTAL)
BL_phylo_df$TOTAL[BL_phylo_df$TOTAL==" 0.00"] <- "<0.01"
row.names(BL_phylo_df) <- NULL


LI_phylo_df<- LI_phylo_df[LI_phylo_df$`Genus Species`!="unknown",]
LI_phylo_df<-LI_phylo_df[LI_phylo_df$`Genus Species`!= "Epinephelus itajara", ]
LI_phylo_df$TOTAL <-rowMeans(LI_phylo_df[,c(5:ncol(LI_phylo_df))], na.rm = TRUE)

LI_phylo_df$`Genus Species` [LI_phylo_df$`Genus Species` == "unknown larvae n/a"] <- ""
LI_phylo_df$`Common Name` [LI_phylo_df$`Common Name` == "larvae"] <- "Juvenile Fish Larvae"
LI_phylo_df<-LI_phylo_df[LI_phylo_df$TOTAL!=0,]
LI_phylo_df$Order<-toupper(LI_phylo_df$Order) 
LI_phylo_df<- LI_phylo_df[order(LI_phylo_df$`Genus Species` ),]
LI_phylo_df<- LI_phylo_df[order(LI_phylo_df$Family ),]
LI_phylo_df<- LI_phylo_df[order(LI_phylo_df$Order ),]
LI_phylo_df$Family<- trimws(LI_phylo_df$Family) 
LI_phylo_df$Order<- trimws(LI_phylo_df$Order) 
LI_phylo_df$Order[duplicated(LI_phylo_df$Order)] <- ""
LI_phylo_df$Family[duplicated(LI_phylo_df$Family)] <- ""
LI_phylo_df$TOTAL<- format(round(LI_phylo_df$TOTAL, 2), nsmall = 2)
LI_phylo_df[LI_phylo_df==0] <- ""
LI_phylo_df$TOTAL<-as.character(LI_phylo_df$TOTAL)
LI_phylo_df$TOTAL[LI_phylo_df$TOTAL==" 0.00"] <- "<0.01"
LI_phylo_df<- format_cells(LI_phylo_df,c(2:nrow(LI_phylo_df)) ,3, value ="italics")

row.names(LI_phylo_df) <- NULL


#nr table

#add row of zeros
#BL_phylo_df<- rbind(BL_phylo_df, as.vector(matrix(0,nrow=ncol(BL_phylo_df))))
#add total numbers 
#BL_phylo_df<- rbind(BL_phylo_df,c(0,0,0, "Total # fish", total_fish_species[total_fish_species$TSC_site_code == "BL",]$TOTAL ,
#                                  sum(total_fish_species[total_fish_species$TSC_site_code == "BL",]$TOTAL)))

#LI_phylo_df_new<- rbind(LI_phylo_df, as.vector(matrix(0,nrow=ncol(LI_phylo_df))))

table_phylo_fish_BL<- knitr::kable(BL_phylo_df, format="html", escape=FALSE, align=rep('c', length(BL_phylo_df[,1])), 
                                 caption = "Bear Lake") %>%  row_spec(0,bold=TRUE) %>%
                                 kable_classic(lightable_options = "striped", full_width = F, html_font = "Cambria", font_size=12) 

table_phylo_fish_LI<- knitr::kable(LI_phylo_df, format="html", escape=FALSE, align=rep('c', length(LI_phylo_df[,1])), 
                                   caption = "Lake Ingraham") %>% row_spec(0,bold=TRUE) %>%
                                   kable_classic(lightable_options = "striped", full_width = F, html_font = "Cambria", font_size=12)




#adding total count and total species
LI_phylo_df_new<- rbind(LI_phylo_df,c(0,0,0, "<b>Total<b>"))
LI_phylo_df_new<- rbind(LI_phylo_df_new,c(0,0,0, "<b>Number of Fish<b>", total_fish_species[total_fish_species$TSC_site_code == "LI",]$TOTAL ,
                                          sum(total_fish_species[total_fish_species$TSC_site_code == "LI",]$TOTAL)))
species_data_to_countLI<- LI_phylo_df[,5:c(ncol(LI_phylo_df)-1)]

LI_phylo_col <- vector()
for(i in 1:ncol(species_data_to_countLI)){
  LI_phylo_col[i] <- length(species_data_to_countLI[species_data_to_countLI[,i] >0 ,i]) 
}

LI_phylo_df_new<- rbind(LI_phylo_df_new,c(0,0,0, "<b>Number of Species<b>", LI_phylo_col))

LI_phylo_df_new2<- LI_phylo_df_new[c(nrow(LI_phylo_df_new)-1,nrow(LI_phylo_df_new)),]
LI_phylo_df_new2[LI_phylo_df_new2==0] <- ""
row.names(LI_phylo_df_new2) <- NULL

LI_phylo_df_new3<- rbind(LI_phylo_df, LI_phylo_df_new2)


BL_phylo_df_new<- rbind(BL_phylo_df,c(0,0,0, "<b>Total<b>"))
BL_phylo_df_new<- rbind(BL_phylo_df_new,c(0,0,0, "<b>Number of Fish<b>", total_fish_species[total_fish_species$TSC_site_code == "BL",]$TOTAL ,
                                          sum(total_fish_species[total_fish_species$TSC_site_code == "BL",]$TOTAL)))
species_data_to_countBL<- BL_phylo_df[,5:c(ncol(BL_phylo_df)-1)]

BL_phylo_col <- vector()
for(i in 1:ncol(species_data_to_countBL)){
  BL_phylo_col[i] <- length(species_data_to_countBL[species_data_to_countBL[,i] >0 ,i]) 
}

BL_phylo_df_new<- rbind(BL_phylo_df_new,c(0,0,0, "<b>Number of Species<b>", BL_phylo_col))

BL_phylo_df_new2<- BL_phylo_df_new[c(nrow(BL_phylo_df_new)-1,nrow(BL_phylo_df_new)),]
BL_phylo_df_new2[BL_phylo_df_new2==0] <- ""
row.names(BL_phylo_df_new2) <- NULL

BL_phylo_df_new3<- rbind(BL_phylo_df, BL_phylo_df_new2)






table_phylo_fish_BL2<-  knitr::kable(BL_phylo_df_new3, format="html", escape=FALSE, align=rep('c', length(BL_phylo_df[,1])),
                        caption = "Bear Lake") %>%  
                        row_spec(0,bold=TRUE) %>%
                        pack_rows("Counts", nrow(BL_phylo_df_new3)-1,nrow(BL_phylo_df_new3)) %>%
                        kable_classic(lightable_options = "striped", full_width = F, html_font = "Cambria", font_size=12) 



table_phylo_fish_LI2<-  knitr::kable(LI_phylo_df_new3, format="html", escape=FALSE, align=rep('c', length(LI_phylo_df[,1])),
                        caption = "Lake Ingraham") %>%  
                        row_spec(0,bold=TRUE) %>%
                        pack_rows("Counts", nrow(LI_phylo_df_new3)-1,nrow(LI_phylo_df_new3)) %>%
                        kable_classic(lightable_options = "striped", full_width = F, html_font = "Cambria", font_size=12) 




#adding total fish and classes sal tables
BL_fish_sal_table_new<- sal_fish_BL %>% 
  dplyr::group_by(`Hydro Year`,salinity) %>%  
  dplyr::reframe(catch = sum(catch, na.rm = TRUE)) %>% 
  pivot_wider( names_from = `Hydro Year`, values_from = catch)


BL_fish_sal_table_new<- as.data.frame(BL_fish_sal_table_new)

BL_fish_sal_table_new$salinity<- factor(BL_fish_sal_table_new$salinity, levels = c("freshwater", "oligohaline", "mesohaline","polyhaline", "euhaline", 
                                                                                  "unknown", "<b>Number of Fish<b>", "<b>Number of Salinity Classes<b>",
                                                                                  "<b>Number of Species<b>"))
BL_fish_sal_table_new %>% rbind(c("<b>Number of Fish<b>", total_fish_species[total_fish_species$TSC_site_code == "BL",]$TOTAL))
BL_fish_sal_table_new[nrow(BL_fish_sal_table_new)+1,]<- c("<b>Number of Fish<b>", total_fish_species[total_fish_species$TSC_site_code == "BL",]$TOTAL)

bl_salclass_data_to_count<- BL_fish_sal_table_new[1:5,2:c(ncol(BL_fish_sal_table_new))]
BL_species_col <- vector()
for(i in 1:ncol(bl_salclass_data_to_count)){
  BL_species_col[i] <- length(bl_salclass_data_to_count[bl_salclass_data_to_count[,i] >0 ,i]) 
}

BL_fish_sal_table_new[nrow(BL_fish_sal_table_new)+1,]<- c("<b>Number of Salinity Classes<b>", BL_species_col)
BL_fish_sal_table_new<- rbind(BL_fish_sal_table_new,c("<b>Number of Species<b>", BL_phylo_col))


BL_fish_sal_table_new<-BL_fish_sal_table_new[order(BL_fish_sal_table_new$salinity ),]
#BL_fish_sal_table_new$'POR %' <-rowMeans(BL_fish_sal_table_new[,c(2:ncol(BL_fish_sal_table_new))], na.rm = TRUE)
BL_fish_sal_table_new[BL_fish_sal_table_new==0] <- ""
BL_fish_sal_table_new[BL_fish_sal_table_new == 100.01] <- "100"
#BL_fish_sal_table_new$'POR %'<- format(round(BL_fish_sal_table_new$'POR %', 2), nsmall = 2)
#BL_fish_sal_table_new$'POR %'<-as.character(BL_fish_sal_table_new$'POR %')
#BL_fish_sal_table_new$'POR %'[BL_fish_sal_table_new$'POR %'==" 0.00"] <- "<0.01"
row.names(BL_fish_sal_table_new) <- NULL


LI_fish_sal_table_new<- sal_fish_LI %>% 
  dplyr::group_by(`Hydro Year`,salinity) %>%  
  dplyr::reframe(catch = sum(catch, na.rm = TRUE)) %>% 
  pivot_wider( names_from = `Hydro Year`, values_from = catch)
LI_fish_sal_table_new<- as.data.frame(LI_fish_sal_table_new)
#LI_fish_sal_table_new<- LI_fish_sal_table_new[,-(1:4)]

#LI_fish_sal_table_new$salinity<-factor(LI_fish_sal_table_new$salinity, levels = c("freshwater", "oligohaline", "mesohaline","polyhaline", "euhaline", 
#                                                                                  "unknown", "<b>Number of Fish<b>", "<b>Number of Salinity Classes<b>",
#                                                                                  "<b>Number of Species<b>"))

LI_fish_sal_table_new<-LI_fish_sal_table_new[order(LI_fish_sal_table_new$salinity),]
LI_fish_sal_table_new <- LI_fish_sal_table_new %>% mutate(salinity = as.character(salinity)) %>% 
  rbind(c("<b>Number of Fish<b>", total_fish_species[total_fish_species$TSC_site_code == "LI",]$TOTAL))


LI_salclass_data_to_count<- LI_fish_sal_table_new[1:5,2:c(ncol(LI_fish_sal_table_new))]
LI_species_col <- vector()
for(i in 1:ncol(LI_salclass_data_to_count)){
  LI_species_col[i] <- length(LI_salclass_data_to_count[LI_salclass_data_to_count[,i] >0 ,i]) 
}

LI_fish_sal_table_new[nrow(LI_fish_sal_table_new)+1,]<- c("<b>Number of Salinity Classes<b>", LI_species_col)
LI_fish_sal_table_new<- rbind(LI_fish_sal_table_new,c("<b>Number of Species<b>", LI_phylo_col))


#LI_fish_sal_table_new<-LI_fish_sal_table_new[order(LI_fish_sal_table_new$salinity ),]
#LI_fish_sal_table_new$'POR %' <-rowMeans(LI_fish_sal_table_new[,c(2:ncol(LI_fish_sal_table_new))], na.rm = TRUE)
LI_fish_sal_table_new[LI_fish_sal_table_new==0] <- ""
LI_fish_sal_table_new[LI_fish_sal_table_new == 100.01] <- "100"
#LI_fish_sal_table_new$'POR %'<- format(round(LI_fish_sal_table_new$'POR %', 2), nsmall = 2)
#LI_fish_sal_table_new$'POR %'<-as.character(LI_fish_sal_table_new$'POR %')
#LI_fish_sal_table_new$'POR %'[LI_fish_sal_table_new$'POR %'==" 0.00"] <- "<0.01"
row.names(LI_fish_sal_table_new) <- NULL




table_sal_fish_BL2<-  knitr::kable(BL_fish_sal_table_new, format="html", escape=FALSE, align=rep('c', length(BL_fish_sal_table_new[,1])),
                                   caption = "Bear Lake") %>%  
                                   row_spec(0,bold=TRUE) %>%
                                   pack_rows("Counts", nrow(BL_fish_sal_table_new)-2,nrow(BL_fish_sal_table_new)) %>%
                                   kable_classic(lightable_options = "striped", full_width = F, html_font = "Cambria", font_size=12) 


table_sal_fish_LI2<-  knitr::kable(LI_fish_sal_table_new, format="html", escape=FALSE, align=rep('c', length(LI_fish_sal_table_new[,1])),
                                   caption = "Lake Ingraham") %>%  
                                   row_spec(0,bold=TRUE) %>%
                                   pack_rows("Counts", nrow(LI_fish_sal_table_new)-2,nrow(LI_fish_sal_table_new)) %>%
                                   kable_classic(lightable_options = "striped", full_width = F, html_font = "Cambria", font_size=12) 




#figures and tables! ####
#figures
BL_salclass_bar_plot
three_fish_line_graph
LI_salclass_bar_plot

ggarrange(LI_oligohaline_bar_plot, LI_mesohaline_bar_plot, LI_polyhaline_bar_plot,LI_euhaline_bar_plot,
          labels=c("       A", "       B", "       C", "       D"), nrow= 2, ncol=2, vjust= 2, hjust = -0.5, common.legend = TRUE)

Density_bar_plot
Biomass_bar_plot

ggarrange(Density_bar_post_pre_plot, Biomass_bar_post_pre_plot,
          labels=c("        A", "       B"), nrow= 2, vjust= 2, hjust = -0.5, common.legend = TRUE)




#tables
table_sample_date_BL
table_sample_date_LI

table_phylo_fish_BL2
table_sal_fish_BL2

table_phylo_fish_LI2
table_sal_fish_LI2











#link fish with fish family and salinity through fish$species, fish_family$SPP.., fish_salinity$SPP..

#fish$SPP...<- as.integer(fish_percent$species)

#fish_df<- left_join(fish, fish_family, "SPP...")
#fish_df<- left_join(fish_df, fish_salinity, "SPP...")

#fish_df$date<- as.Date(fish_df$date, "%m/%d/%Y") 
#month<- c("Jun", "Sep", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")
#sample.month.x<- c("Jun", "Sep", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")
#sample.months<- as.data.frame(month, sample.month.x)

#fish_df$month<- factor(fish_df$month, levels = sample.month.x)
#factor(fish_df$site.code, levels = c("LI","BL","TR","EC","WJ","JB","SB","HC","MB","BS","CS"))  #add other missing sites when working with older data (7p, TP)

#BL.sample.date<- ddply(fish_df[fish_df$site.code == "BL", ], .(hydro.yr, month), summarise, date = mean(date))
#LI.sample.date<- ddply(fish_df[fish_df$site.code == "LI", ], .(hydro.yr, month), summarise, date = mean(date))


#figure out how to include DRY (non sampled months )
#BL.sample.date<-left_join(sample.months, BL.sample.date, "month")
#LI.sample.date<-left_join(sample.months, LI.sample.date, "month")
#pivot_wider(BL.sample.date, names_from = month, values_from = date) 
#pivot_wider(LI.sample.date, names_from = month, values_from = date) 
#check this more once i have all the data....





#BL.fish<- fish_df[c(which(fish_df$site.code == "BL")),]
#LI.fish<- fish_df[c(which(fish_df$site.code == "LI")),]




