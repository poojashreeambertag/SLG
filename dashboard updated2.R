setwd('/home/poojashree/Downloads/')
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(tableHTML)
library(leaflet)
library(gridExtra)
library(DT)
library(proportion)

recommendation <- read.csv('/home/poojashree/Downloads/Prediction.csv')
Data_summary <- read.csv('/home/poojashree/Downloads/summary.csv')
summ <- data.frame(Rad =min(Data_summary$Radiation.gfs.) , Temp=min(Data_summary$Ambient_Temp))
summ$Energy <-min(recommendation$Forecasted_Generation.in.MW)
summ1 <- data.frame(Rad =round(max(Data_summary$Radiation.gfs.),2) , Temp =max(Data_summary$Ambient_Temp))
summ1$Energy <-max(recommendation$Forecasted_Generation.in.MW)
rownames(summ) = "Min"
rownames(summ1) = "Max"
summary_ <- rbind(summ,summ1)
summary_  <- c("#bgred {background-color: #FF0000;}",
                    "#bgblue {background-color: #0000FF;}")

#summary_ <- table(summary_)

################## row fill color#####################################
# summary_ %>% 
#   tableHTML(rownames = FALSE,
#             widths = paste(fixedWidth*proportion(summary_)) %>% 
#   add_css_row(rows = which(summary_$Irradiation == '25') + 1,
#               css = list(c("background-color"),
#                          c("orange"))) %>% 
#   add_css_row(rows = which(summary_$Irradiation != '25') + 1,
#               css = list(c("background-color"),
#                          c("blue"))))

###########################################################################


#Dashboard header carrying the title of the dashboard#######################
header <- dashboardHeader(title = "solar Prediction")
 
anchor <- tags$a(href='logo.png',
                 tags$img(src='http://www.ambertag.com/blog/wp-content/uploads/2018/05/amberTAG-logo1.png', height='40', width='40'),
                 'SPG')

header$children[[2]]$children <- tags$div(
  tags$head(tags$style(HTML(".name { background-color: white }"))),
  anchor,
  class = 'name')
#Sidebar content of the dashboard############################################3
sidebar <- dashboardSidebar(
   sidebarMenu(
     # menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
     # menuItem("Visit-us", icon = icon("send",lib='glyphicon'),
     #         href = "https://www.salesforce.com")
  )
 )

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
  
  
)

frow2 <- fluidRow(
  box(
    status = "primary"
    ,width = 8
    ,solidHeader = F
    ,collapsible = F
    ,height = "500"
    ,align="center"
    ,plotOutput("Forecasted_Generation.in.MWbyFrom_Time", height = "450",width = "650")
  ) 
 
    ,box(title="SUMMARY",
      solidHeader = F,
      collapsible = F,
      align="center",
      width=4,height = "180px",
      column( width = 2,
             dataTableOutput("summary"), height = "100"))
)
      


# combine the two fluid rows to make the body########
body <- dashboardBody(frow1,frow2)
#completing the ui part with dashboardPage###################
ui <- dashboardPage(header,sidebar, body, skin='red')
# create the server functions for the dashboard ################# 
server <- function(input, output) { 
 
 
  Forecasted_Generation <- paste(sum(recommendation$Forecasted_Generation.in.MW)," MW",sep = "")
  Prediction <- NULL
  summary.pred <- NULL
  Prediction <- paste(max(recommendation$Forecasted_Generation.in.MW)," MW",sep = "
                      ")
  Prediction <- paste(min(recommendation$Forecasted_Generation.in.MW),Prediction,sep = ",  ")
  
  output$value1 <- renderValueBox({
    
    valueBox(
      formatC(Forecasted_Generation, format="f", big.mark=',')
      ,(paste('Prediction for',Sys.Date()+1, sep = " "))
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")
    
  })
  # output$value2 <- renderValueBox({
  #   
  #   valueBox(
  #     formatC(Prediction, format="f", big.mark=',')
  #     ,('Min_Prediction 
  #       Max_prediction')
  #     ,icon = icon("menu-hamburger",lib='glyphicon')
  #     ,color = "yellow")
  #   
  # })
 
  #output$messageMenu <- renderMenu({
    #msgs <- apply(messageData, 1, function(row) {
      #messageItem(from = row[["from"]], message = row[["message"]])
    #})
    #dropdownMenu(type = "messages", .list = msgs)
  #})
  
  #creating the plotOutput content##################################
  #fill=factor(X))) 
  #recommendation <- recommendation[25:73,]
  output$Forecasted_Generation.in.MWbyFrom_Time <- renderPlot({
    ggplot(data = recommendation,
           aes(x=From_Time, y=Forecasted_Generation.in.MW,group=1,fill=as.factor("Forecasted_Generation")))+
      geom_bar(position = "dodge",stat = "identity",colour="black") + ylab("Forecasted_Power (in MW)") +
      xlab("Time") + theme(axis.text.x = element_text(angle = 90,hjust=1, vjust = 0, face="italic", colour="black")
                           ,plot.title = element_text(size=18, face="bold"),legend.position = c(0.8,0.8))+ggtitle("Forecasted_Generation")+labs(fill = "Forecasted_Generation")
    
  }) 
  output$summary <- renderDataTable({datatable(summary_,options = list(dom = 't',
      autoWidth = TRUE,
      columnDefs = list(list(width = "100px", targets = 1:3))
    ))
    }
  )

                                

  

#fill=as.factor("Forecasted_Generation.in.MW")       
# labs(fill = "Forecasted_Generation.in.MW")   
   #stat = "identity"
  #scale_x_continuous(breaks=seq(00:00, 01:00, 1))
  #legend.position="bottom" 
  #+ labs(fill = "Region")  
  #output$Available_Capacity.AvC.MWbyFrom_Time <- renderPlot({
  #ggplot(data = recommendation, 
  #aes(x=From_Time, y=Available_Capacity.AvC.MW, fill=factor(Region))) + 
  #geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in MW)") + 
  #xlab("Time") + theme(legend.position="bottom" 
  #,plot.title = element_text(size=15, face="bold")) + 
  #ggtitle("Available_Capacity") + labs(fill = "Region")
  #})
  
  
} 
 
shinyApp(ui, server) 
