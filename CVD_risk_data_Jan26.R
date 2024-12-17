#Troubleshooting and data wrangling#
#Jan 26, 2021

rm(list = ls(all = TRUE)) # Clear all memory
setwd("~/R/cvd") #set your working directory (my STEPS folder is inside Documents so I can use the ~ shorthand to get there)
#you can also do this by clicking the Session menu and go to Set Working Directory -> Choose directory
#and then just select the folder you want to work in

library(dplyr)
library(readxl)

#load data
data2<-read_xlsx("Copy of cvd_risk_data.xlsx", sheet= "Sheet1")
View(data2) #notice the difference in the column names**

regions<-read_xlsx("regions.xlsx", sheet = "Sheet1")

#make new columns for starting age vs ending age of age groups
data2$startage<-as.numeric(substr(data2$age, 1, 2)) 
#as a number, take the first and the second character of the age column
data2$endage<-as.numeric(substr(data2$age,4,5))
#as a number, take the fourth and the fifth character of the age column

###check for NAs###
any(is.na(data2$startage))
any(is.na(data2$endage))
##answer was TRUE, so there are some NA's. Let's find them.
which(is.na(data2$startage))
which(is.na(data2$endage))
##ok, so the NAs are in lines 856,857,858. Time to inspect.
##What do you see?

#age span column
data2$agespan<-data2$endage-data2$startage

#Identify max age span for each unique entry#
maxagespan<-data2%>%
  group_by(year, sex, Country, `file type`)%>%
  filter(agespan==max(agespan))

View(maxagespan)
#are all values of the "all age" column=1?
checkmaxage<-maxagespan%>%filter(`all age`==0)
#how many lines of data have an all age = 0 but the max age span?
View(checkmaxage)
#How would you resolve each of these inconsistencies?


#upload cleaned data
data2<-read_xlsx("cvd_risk_data", sheet= "Sheet1")
#in this data set, we now take the all age column as correct fo making age categories

#now that we've loaded in this new data, let's remake those start and end age columns 
data2$startage<-as.numeric(substr(data2$age, 1, 2)) 
data2$endage<-as.numeric(substr(data2$age,4,5))

#making age categories
#conditioning statement
data2$age.group[data2$`all age`==1]<-"All ages"
data2$age.group[data2$endage<45]<-"Under 45"
data2$age.group[data2$startage>=45]<-"Age 45 and above"

View(data2)

############################################################
#Renaming columns
names(data2)
names(data2)[8]<-"Proportion healthy"

#make sure this value is a number
data2$`Proportion healthy`<-as.numeric(data2$`Proportion healthy`)


############################################################
#merging data set 
View(regions)
#caution rewriting a dataframe with the same name
data_left<-left_join(data2, regions, by="Country")
data_right<-right_join(regions, data2, by="Country")

data_wrong<-left_join(regions, data2, by="Country")

#rename data_left to data2
data2<-data_left
View(data2)

#find errors in merge
which(is.na(data2$Region))
#Wow, lots of them. This is why using iso3 codes is much better than names
#Let's mannually fix a few
data2$Region[data2$Country=="Cape Verde"]<-"Western Sub-Saharan Africa"
which(is.na(data2$Region))
data2$Region[data2$Country=="Côte d'Ivoire"]<-"Western Sub-Saharan Africa"
which(is.na(data2$Region))
data2$Region[data2$Country=="São Tomé and Principe"]<-"Western Sub-Saharan Africa"
which(is.na(data2$Region))
data2$Region[data2$Country=="Zanzibar"]<-"Eastern Sub-Saharan Africa"
#Zanzibar is considered part of Tanzania, so let's just assign it the same region
which(is.na(data2$Region))
data2$Region[data2$Country=="Anguilla"]<-"Caribbean"
#some of these you may just have to Google
which(is.na(data2$Region))
data2$Region[data2$Country=="Aruba"]<-"Caribbean"
data2$Region[data2$Country=="Bermuda"]<-"Caribbean"
which(is.na(data2$Region))
data2$Region[data2$Country=="British Virgin Islands"]<-"Caribbean"

#Now you can try a few if you'd like! 

#But you can see, this is pretty tedious.
#Usually I will add these to my master conversion spreadsheet that includes
#all the different types of spellings and territories/island countries which are 
#not always included in every countrty list.


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

#filter by age group, region, and sex

plotdata<-data2%>%filter(age.group=="All ages" &
                           sex=="both" &
                           Region=="Western Sub-Saharan Africa")

plot_ly(plotdata, x=~year, y=~`Proportion healthy`,
        type="scatter",
        hoverinfo='text',
        text=paste(plotdata$Country, ",", plotdata$`Proportion healthy`, ",", 
                   plotdata$year))

#notice we have two values for Cote d'Ivoire for the same year
#big difference between fact sheet (age 15-64) and country report (age 25-64)

#########################################################################
#first, make your Shiny file, and call it CVDdata (or else change the name below to whatever you choose)

#writing data
setwd("~/R/cvd")
write.csv(data2, "ourdata.csv", row.names = F)

