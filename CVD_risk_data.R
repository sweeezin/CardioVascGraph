#Data structures and data wrangling#
#Jan 12, 2021

rm(list = ls(all = TRUE)) # Clear all memory
setwd("~/R/cvd") #set your working directory (my STEPS folder is inside Documents so I can use the ~ shorthand to get there)
#you can also do this by clicking the Session menu and go to Set Working Directory -> Choose directory
#and then just select the folder you want to work in

library(dplyr)
library(data.table)

#using data.table
data<-fread("cvd_risk_data.csv")

#using base R
data2<-read.csv("cvd_risk_data.csv")
#loads all strings as Factors unless you tell it otherwise

data2<-read.csv("cvd_risk_data.csv", stringsAsFactors = F)

#make new columns for starting age vs ending age of age groups
data2$startage<-as.numeric(substr(data2$age, 1, 2)) 
#as a number, take the first and the second character of the age column
data2$endage<-as.numeric(substr(data2$age,4,5))
#as a number, take the fourth and the fifth character of the age column


#how to do this using data.table
#the format is: data[row,column,by group]
#when you leave a spot blank, then it just means take all rows/columns/groups
data[ , startage:=as.numeric(substr(age,1,2)),]
#use the := instead of = when you are overriding data (or creating new data)

#################################################################
#filtering/grouping

#using dplyr, make a new data set that only has the all age category
all.age.data2<-data2%>%filter(all.age==1)
# use logical operators for comparison. So == means same as (don't use =). 
#Other logical operators are >= greater than or equal to, %in% means matching a list of objects
acountries<-c("Aleriga", "American Samoa", "Armenia", "Aruba")

#all ages for only countries that start with A
a.data2<-data2%>%filter(all.age==1 & Country%in%acountries)
# & means AND, | means OR

#do this with data.table    
all.age.data<-data[`all age`==1]
# use ` ` if there is a space in the column name
a.data<-data[`all age`==1 & Country%in%acountries]

#############################################################
#making age categories
#conditioning statement
data2$age.group[data2$all.age==1]<-"All ages"
data2$age.group[data2$endage<45]<-"Under 45"
data2$age.group[data2$startage>=45]<-"Age 45 and above"

any(is.na(data2$age.group))

############################################################
names(data2)[8]<-"Proportion healthy"

#graphs#
library(ggplot2)
library(plotly)

#ggplot(data, aes(x= , y= , color = ,))+geom_point()+facet_wrap(~age.group)
ggplot(data2, aes(x=year, y=`Proportion healthy`, color=age.group))+
  geom_point()+
  facet_wrap(~sex)

#plotly(data, X=~ , y=~, color=~)
plot_ly(data2, x=~year, y=~`Proportion healthy`,
       type="scatter",
       hoverinfo='text',
       text=paste(data2$Country, ",", data2$`Proportion healthy`, ",", 
                  data2$sex,",", data2$age.group))


#########################################################################
#first, make your Shiny file, and call it CVDdata (or else change the name below to whatever you choose)

#writing data
setwd("~/STEPS/CVDdata")
write.csv(data2, "ourdata.csv", row.names = F)

