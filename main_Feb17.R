
library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(data.table)

############code from sarah######################

#new package
library(readxl)

##download the new data files from google drive and add them to your working directory folder
data<-read_excel("Copy of cvd_risk_data_cleaned.xlsx", sheet = "Sheet1")
iso3codes<-read_excel("Copy of cvd_risk_Data_cleaned.xlsx", sheet = "Sheet2")

#merge data by Country name and put back into dataframe called "data"
data<-left_join(data, iso3codes, by="Country")

#now read in regions file
regions<-read_excel("regions.xlsx")

#merge data by iso3 code and put back into dataframe called "data"
data<-left_join(data, regions, by="iso3")

############## Fiona's original code #######################

#data<-read.csv("cvd_risk_data.csv", stringsAsFactors = F)

data$startage<-as.numeric(substr(data$age, 1, 2)) 
data$endage<-as.numeric(substr(data$age,4,5))

data$age.group[data$`all age`==1]<-"All ages"  #i changed this based on the names read in by read_excel vs. read.csv
data$age.group[data$endage<45]<-"Under 45"
data$age.group[data$startage>=45]<-"Age 45 and above"

#data2<-data2%>%rename("3+" = `Proportion of people with 3+ risk factors (%)`)
names(data)[8]<-"0"
names(data)[9]<-"1-2"
names(data)[10]<-"3+"



ui <- fluidPage(

    # Application title
    titlePanel("CVD Data"),

    sidebarLayout(
        sidebarPanel(
           h6("Welcome
                line 2"),
           h6("a"),
           selectInput("age", "Age group:", c("All ages", "Under 45", "Age 45 and above"), selected="All ages"),
            br(),
           selectInput("risk", "Risks:", c("0", "1-2", "3+" )),
           sliderInput("risk2", "test", min=0, max=3, value = 0)
           #selectInput("sex", "Gender:", c("Male", "Female", "Both", selected = "Both"))
            
        ),
  

        mainPanel(
           plotlyOutput("healthyplot")
           
        )
    )
)

#data$yaxis<-data[, paste(input$risk)]
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
   
    
    output$healthyplot <- renderPlotly({
        # data$yaxis<-data[, paste(input$risk)]
         data<-data%>%filter(age.group==input$age )

         #p1<-p1%>%layout(xaxis=list(title="Year"),              
         #yaxis=list(title=paste0("Proportion with ", input$risk, " risk factors (%)")))
         
         
         
         plot1 <- plot_ly(data, x=~year, y=~yaxis, 
               color=~ sex , 
               type="scatter",
               mode= 'markers',
               hoverinfo='text',
            text=paste(data$yaxis))
         plot1<-plot1%>%layout(title = "Plot Title",
                              xaxis = list(title = "Year"), 
                              yaxis = list(title = paste0("Porportion with ", input$risk, " risk factors (%)" ))
         )
    plot1
        
    })
}

shinyApp(ui = ui, server = server)

