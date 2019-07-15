#Problem Set 2
#Name: Salil Shahane (salils@uchicago.edu)

#Loading tidyverse and lubridate packages
library(tidyverse)
library(lubridate)

#Section 1

#Loading CSV file
ohare <- read_csv("ohare_airport_temps.csv")
ohare_temp <- as.Date(ohare$timestamp)
#separating the year, month, week, day from the date
ohare1 <- ohare%>%
  mutate(week=week((ohare$timestamp)))%>%
  mutate(year=year(ohare$timestamp))%>%
  mutate(month=month(ohare$timestamp))%>%
  mutate(day= day(ohare$timestamp))%>%
  mutate(hour= hour(ohare$timestamp))

  temperature_avg<-
    ohare1%>%
    group_by(year)%>%
    summarise(temps_avg= mean(temp_f))
  
  head(temperature_avg,n=4)
  
 #Q1
  #lowest temperatures
  arrange(temperature_avg,temps_avg)%>%
    head(2)
  
  #Q2
  #hottest temperature
  ohare1%>%
  group_by(hour)%>%
    summarise(avg_temp=mean(temp_f))%>%
    arrange(desc(avg_temp))%>%
    head(1)
  
  #Q3
  
  # temp<-ohare1%>%
  #   filter(hours>=3& hours<=15)%>%
  #   group_by(year) %>% 
  #   spread(year,value=temp_f)%>%
  #   mutate(temp_diff=abs(`15`-`3`))%>%
  #   arrange(desc(temp_diff))%>%
  #   head(1)
  # Didn't work! 
  
  #alternative solution
  
  ohare1 %>% 
    group_by(year, month, day) %>% 
    filter(hour==3| hour==15) %>% 
    summarise(temp_swing = max(temp_f) - min(temp_f)) %>% 
    arrange(desc(temp_swing))%>%
    head(1)
    
  #Q4
  #summarise the temperatures according to every week every year. Using lag function take the difference in two consecutive rows.Arrange in descending order.
  ohare1%>%
    group_by(week,year)%>%
    summarise(avg_tem=mean(temp_f))%>%
    group_by(week)%>%
    mutate(avg_tem_diff=avg_tem-lag(avg_tem))%>%
    mutate(avg_temp_diff = abs(avg_tem_diff))%>%
    arrange(-avg_temp_diff)%>%
    head(1)
  
    
 #plot1
    #summarising average temp according to year
    plot_data<-ohare1%>%
      group_by(year)%>%
      summarise(Temperature= mean(temp_f))
    
    library(ggplot2)
    ggplot(plot_data) +
      geom_path(aes(x=year, y= Temperature, group =1), colour= "maroon", size=1.5)+
      ggtitle("Average Temperature by Year in Chicago")+
      labs(caption = "Source: NOAA Weather Station, O'Hare Airport")+
      scale_x_continuous(limits = c(2000,2018))+
      scale_y_continuous(limits = c(47.5,55.0))
    
    
#section 2
    
#Q1
    library(jsonlite)
    homicide_data<- read_json("https://data.cityofchicago.org/resource/ijzp-q8t2.json?primary_type=HOMICIDE&$limit=9753", simplifyVector = TRUE)  
    
  homicide_data%>%
    group_by(year)%>%
    summarise(homicides = n())%>%
    arrange(-homicides)%>%
    head(1)
  
  #Q2
 homicide_data<-homicide_data%>%
   mutate(dates=ymd_hms(date))%>%
    mutate(hour=hour(dates))%>%mutate(week=week(dates))%>%mutate(year=year(dates))
 
 homicide_data%>%
    group_by(hour)%>%
    mutate(count=n())%>%
    summarise(avg_homicides=mean(count))%>%
   arrange(-avg_homicides)%>%
   head(1)
 
 #Q3
 
community_areas<- read_csv("CommAreas.csv")
homicide_data<-homicide_data%>%
  mutate(AREA_NUMBE=community_area)%>%
  mutate(AREA_NUMBE=as.numeric(AREA_NUMBE))

joined<-left_join(homicide_data,community_areas,by="AREA_NUMBE")

joined%>%
  group_by(community_area,COMMUNITY)%>%
  summarise(homicides = n())%>%
  arrange(homicides)%>%
  head(1)

#plot2
homicide_data<-homicide_data%>%
  group_by(week,year)%>%
  mutate(`Number of Homicides`=n())

library(RColorBrewer)

ggplot(homicide_data)+
  geom_tile(aes(x=week,y=year,fill=`Number of Homicides`),position="dodge")+
  ggtitle("Homicides Over Time in Chicago")+
  labs(caption="Source: City of Chicago Data Portal")+
  scale_x_continuous(limits=c(0,50))+
  scale_y_reverse(breaks=seq(2001,2017,2))

 

#Section 3

plot_combined <- homicide_data%>%
  left_join(ohare1,by=c("year","week","hour"))%>%
  group_by(year,week)%>%
  summarise(avg_temp=mean(temp_f),avg_homicides=mean(`Number of Homicides`))

ggplot(plot_combined,aes(x=week),fill="white")+
  geom_smooth(aes(y=avg_homicides),color="blue", fill="white",size=1.5)+
  geom_smooth(aes(y=avg_temp/5),color="red",fill="white",size=1.5)+
  scale_y_continuous(sec.axis = sec_axis(~.*5,name="Average Temp"))+
  ggtitle("Homicides vs Temperature in Chicago (2001-2019)")+
  labs(y="Average # Homicides", x= "Week")+
  theme_bw()
#The plot seems complex as it overlaps multiple information like average temperature and average number of homicides. Instead we could use facet wrap to split the graph in two.
  
  
  