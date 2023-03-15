#Set work directory
setwd("C:/Users/ajitn/Documents/R")

#Load packages
library(tidyverse)
library(ggplot2)

#This dataset shows how much of energy consumption comes from renewables

#import data
RENEWABLES = read.csv("renewable energy ec.csv", header = TRUE, stringsAsFactors=FALSE)

#plot data
#add line segments for targets
ggplot(RENEWABLES,aes(x=year,y=renewables))+
  geom_line()+
  theme_minimal()+
  scale_y_continuous(limits=c(0,50),breaks=c(0,10,20,30,40,50))+
  scale_x_continuous(limits=c(2010,2035),breaks=c(2010,2020,2030))+
  annotate("segment", x = 2021, xend = 2030, y = 21.7, yend = 45, linetype = "dashed", color = "red")+
  annotate("segment", x = 2021, xend = 2030, y = 21.7, yend = 40, linetype = "dashed", color = "blue")+
  labs(title="The EU keeps raising its renewable targets", x = NULL, y=NULL, subtitle = "Share of renewable energy in final consumption (%)",caption="Source: European Commission") +
  annotate("text",x=2033,y=40,label="2021 plan")+
  annotate("text",x=2033,y=45,label="2022 plan")
ggsave(file="renewables.svg")



#This dataset shows how much wind and solar is needed to stay on a 1.5 C pathway

#import data
SOLAR_WIND<-read.csv("renewables-needed-2.csv",header=TRUE,stringsAsFactors = FALSE, na.strings = c("", "NA"))

#go from wide to long
a<-SOLAR_WIND %>% 
  pivot_longer(cols = c("wind","needed.wind","solar","needed.solar","total","needed.total","expected.wind","expected.solar"),names_to="energy") %>% 
  rename(capacity=value)

#partial datasets
b<-a %>% 
  filter(energy=="expected.wind" | energy=="wind")
c<-a %>% 
  filter(energy=="expected.solar" | energy=="solar")

#plot wind
ggplot(NULL,aes(x=year,y=capacity,fill=energy))+
  geom_bar(data=b,stat="identity",position="dodge")+
  theme_minimal()+
  scale_x_continuous(limits=c(2021,2030),breaks=c(2021,2025,2030))+
  scale_y_continuous(limits=c(0,600),breaks=c(0,200,400,600))+
  labs(title="The EU is building enough solar panels but not enough wind turbines", x = NULL, y=NULL, subtitle = "Solar and wind capacity (GW) projected by industry versus the amount needed to meet the 1.5 C target",caption="Source: EMBER, SolarPowerEurope, WindEurope, IEA")
ggsave(file="wind.svg")

#plot solar
ggplot(NULL,aes(x=year,y=capacity,fill=energy))+
  geom_bar(data=c,stat="identity",position="dodge")+
  theme_minimal()+
  scale_x_continuous(limits=c(2021,2030),breaks=c(2021,2025,2030))+
  scale_y_continuous(limits=c(0,600),breaks=c(0,200,400,600))+
  labs(title="The EU is building enough solar panels but not enough wind turbines", x = NULL, y=NULL, subtitle = "Solar and wind capacity (GW) projected by industry versus the amount needed to meet the 1.5 C target",caption="Source: EMBER, SolarPowerEurope, WindEurope, IEA")
ggsave(file="solar.svg")

#This dataset shows how many heat pumps are being built and how many would be needed to reach the EU's targets

#import data
HEATPUMPS<-read.csv("heat pumps.csv",header=TRUE,stringsAsFactors = FALSE, na.strings = c("", "NA"))

#go from wide to long
d<-HEATPUMPS %>% 
  pivot_longer(cols = c("historic","projected","required"),names_to="type") %>% 
  rename(number=value)

#plot
ggplot(d,aes(x=year,y=number,color=type))+
  geom_line()+
  theme_minimal()+
  scale_x_continuous(limits=c(2010,2030),breaks=c(2010,2020,2030))+
  scale_y_continuous(limits=c(0,65),breaks=c(0,20,40,60))+
  labs(title="The EU is installing too few heat pumps", x = NULL, y=NULL, subtitle = "Millions of heat pumps",caption="Source: Regulatory Assistance Project")
ggsave(file="heatpumps.svg")

#This dataset shows how far EU emissions have to fall

#import data
EU_EMISSIONS<-read.csv("eu-emissions.csv",header=TRUE,stringsAsFactors = FALSE, na.strings = c("", "NA"),check.names=FALSE)

#go from wide to long
e<-EU_EMISSIONS %>% 
  pivot_longer(cols =!type,names_to="year") %>% 
  rename(emissions=value) %>% 
  filter(type=="total emissions" | type=="additional measures") %>% 
  mutate(year = as.numeric(year),
         emissions = emissions/1000)

#plot
ggplot(e,aes(x=year,y=emissions,color=type))+
  geom_line()+
  theme_minimal()+
  scale_x_continuous(limits=c(1990,2030),breaks=c(1990,2000,2010,2020,2030))+
  scale_y_continuous(limits=c(0,5),breaks=c(0,1,2,3,4,5))+
  annotate("segment", x = 2021, xend = 2030, y =3.312, yend = 2.109, linetype = "dashed", color = "red")+
  annotate("segment", x = 2021, xend = 2030, y =3.312, yend = 1.862, linetype = "dashed", color = "green")+
  labs(title="The EU is cutting greenhouse gas pollution too slowly to hit its targets", x = NULL, y=NULL, subtitle = "Billions of tons of CO2 equivalent (Gt CO2e)",caption="Source: European Environment Agency")+
  theme(legend.position="none")+
  annotate("text",x=2021,y=2.1,label="-57% reduction target")+
  annotate("segment", x = 2027, xend = 2030, y = 2.1,yend=2.1, color = "grey")
ggsave(file="emissions.svg")

#This dataset shows how much cleaners cars must get if the EU passes its target to phase out combustion engine cars

#import data
CAR_EMISSIONS<-read.csv("car-emissions.csv",header=TRUE,stringsAsFactors = FALSE, na.strings = c("", "NA"),check.names=FALSE) %>% 
  rename(emissions=actual)

#plot
ggplot(CAR_EMISSIONS,aes(x=year,y=emissions))+
  geom_line(color="black")+
  theme_minimal()+
  scale_x_continuous(limits=c(2015,2035),breaks=c(2015,2020,2025,2030,2035))+
  scale_y_continuous(limits=c(0,150),breaks=c(0,50,100,150))+
  annotate("segment", x = 2021, xend = 2030, y =114.7, yend = 59.4, linetype = "dashed", color = "red")+
  annotate("segment", x = 2030, xend = 2035, y =59.4, yend = 0, linetype = "dashed", color = "red")+
  labs(title="The EU wants to stop selling dirty cars by 2035", x = NULL, y=NULL, subtitle = "Emissions from new car sales (gCO2/km)",caption="Source: European Environment Agency")+
  theme(legend.position="none")
ggsave(file="cars.svg")

#This dataset shows how agriculture emissions have changed and where they are projected to go

#import data
FARMS<-read.csv("eu-agriculture.csv",header=TRUE,stringsAsFactors = FALSE, na.strings = c("", "NA"),check.names=FALSE)

#go from wide to long
g<-FARMS %>% 
  select(1:5) %>% 
  pivot_longer(cols=!year,names_to="type")
h<-FARMS %>% 
  select(1,6,7) %>% 
  pivot_longer(cols=!year,names_to="type")

#plot
ggplot(NULL)+
  geom_bar(data=g,stat="identity",aes(x=year,y=value,fill=type))+
  geom_line(data=h,aes(x=year,y=value,color=type))+
  theme_minimal()+
  scale_x_continuous(limits=c(2005,2030),breaks=c(2005,2010,2015,2020,2025,2030))+
  labs(title="European farms are barely getting cleaner", x = NULL, y=NULL, subtitle = "Million tons of CO2 equivalent (MtCO2e) from agriculture ",caption="Source: European Environment Agency")+
  theme(legend.position="top",legend.title=element_blank())
ggsave(file="farms.svg")

#This dataset shows how registrations of new cars by type

#import data
EVS<-read.csv("new-electric-vehicles-in-eu-2.csv",header=TRUE,stringsAsFactors = FALSE, na.strings = c("", "NA"))
EVS$type <- factor(EVS$type, levels = c("combustion", "plug", "bev"))

#plot
ggplot(NULL)+
  geom_bar(data=EVS,stat="identity",aes(x=year,y=number,fill=type))+
  theme_minimal()+
  scale_x_continuous()+
  labs(title="Electric cars sales are rising fast but the share is still small", x = NULL, y=NULL, subtitle = "Number of new electric and hybrid cars sold",caption="Source: European Environment Agency")+
  theme(legend.position="top",legend.title=element_blank())
ggsave(file="electrics.svg")