
library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(readxl)


data<-read.csv("cvd_risk_data2.csv", stringsAsFactors = F)
regions<-read.csv("regions2.csv", stringsAsFactors = F)
region.choice<-unique(regions$Region)

names(data)[1]<-"Country"
data<-left_join(data, regions, by="Country")

data$startage<-as.numeric(substr(data$age, 1, 2)) 
data$endage<-as.numeric(substr(data$age,4,5))

data$age.group[data$all.age==1]<-"All ages"
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
           h2("Filters"),
          
           selectInput("age", "Age group:", c("All ages", "Under 45", "Age 45 and above"), selected="All ages"),
            br(),
           selectInput("risk", "Risks:", c("0", "1-2", "3+" )),
           sliderInput("risk2", "test", min=0, max=3, value = 0),
           selectInput("region", "Region: ", c("all regions", region.choice ))
        
           #selectInput("sex", "Gender:", c("Male", "Female", "Both", selected = "Both"))
            
        ),
  

        mainPanel(
           plotlyOutput("healthyplot"), br(), 
           plotlyOutput("plot2")
           
        )
    )
)

#data$yaxis<-data[, paste(input$risk)]
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
   
    
    output$healthyplot <- renderPlotly({
         data$yaxis<-data[, paste(input$risk)]
         data<-data%>%filter(age.group==input$age)
         
         if(input$region=="all regions"){
           data<-data
         }
         
         else{
           data<-data%>%filter(Region==input$region)
         }

         #p1<-p1%>%layout(xaxis=list(title="Year"),              
         #yaxis=list(title=paste0("Proportion with ", input$risk, " risk factors (%)")))
         
         
         
         plot1 <- plot_ly(data, x=~year, y=~yaxis, 
               color=~ sex , 
               type="scatter",
               mode= 'markers',
               hoverinfo='text',
            text=paste(data$Country, ",", data$yaxis, "%,", 
                       data$Region))
         
         
         
         plot1<-plot1%>%layout(title = "CVD risk data plot",
                              xaxis = list(title = "Year"), 
                              yaxis = list(title = paste0("Porportion with ", input$risk, " risk factors (%)" ))
         )
         
    plot1
        
    })
    output$plot2 <- renderPlotly({
      data$yaxis<-data[, paste(input$risk)]
      data<-data%>%filter(sex=="female" )
      
      #p1<-p1%>%layout(xaxis=list(title="Year"),              
      #yaxis=list(title=paste0("Proportion with ", input$risk, " risk factors (%)")))
      
      
      
      plot1 <- plot_ly(data, x=~year, y=~yaxis, 
                       color=~ age.group , 
                       type="scatter",
                       mode= 'markers',
                       hoverinfo='text',
                       text=paste(data$Country, ",", data$yaxis, "%,", 
                                  data$Region))
      
      
      
      plot1<-plot1%>%layout(title = "CVD risk data plot",
                            xaxis = list(title = "Year"), 
                            yaxis = list(title = paste0("Porportion with ", input$risk, " risk factors (%)" ))
      )
      
      plot1
      
    })
}

shinyApp(ui = ui, server = server)

