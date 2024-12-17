# Load Libraries

library(shiny)
library(dplyr)
library(plotly)
library(data.table)

# Read Data

data<-read.csv("cvd_risk_data.csv", stringsAsFactors = F)


region.data <- read.csv("regions", stringsAsFactors = F)

region.merge<-right_join(regions, data, by="Country")
View(region.merge)

