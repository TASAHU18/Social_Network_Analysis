list.of.packages <- c("DT","shiny","shinythemes","visNetwork", "plotly", "networkD3")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(DT)
library(shiny)
library(shinythemes)
library(visNetwork)
library(plotly)
library(networkD3)


shinyUI(fluidPage(theme = shinytheme("darkly"),
       titlePanel("Social Network Analysis"),
        sidebarLayout(
          sidebarPanel(
            # fileinput() function is used to get the file upload control option
              fileInput("file","Upload the file"),
              tags$hr(),
              tags$p(h4("Developer: Takesh Sahu"))

          ),
          
          mainPanel(
            wellPanel(tabsetPanel(
              tabPanel("Dataset" , tableOutput("inputDataset"),style = "overflow-y:scroll; max-height: 500px !important"),
              tabPanel("Tall Table", dataTableOutput("meltOutput"),style = "overflow-y:scroll; max-height: 500px !important"),
              tabPanel("Call Summary", tableOutput("callSummary"),style = "overflow-y:scroll; max-height: 500px !important"),
              
                       tabPanel("Network Visuals" ,
                       tabsetPanel(
                         tabPanel("Simple Network",forceNetworkOutput("callNetworkSimple") ,style = "overflow-y:scroll; max-height: 700px !important"),
                         tabPanel("Outbound ",forceNetworkOutput("directedOutbound"),style = "overflow-y:scroll; max-height: 700px !important"),
                         tabPanel("Inbound ", forceNetworkOutput ("directedInbound"),style = "overflow-y:scroll; max-height: 700px !important"),
                         tabPanel("Degree Centrality ", forceNetworkOutput("degreeCentrality"),style = "overflow-y:scroll"),
                         tabPanel("Betweenness ",forceNetworkOutput("betweenness"),style = "overflow-y:scroll "),
                         
                         tabPanel("InOutDegrees ",
                                  fluidRow(
                                    column(6,wellPanel("InDegree", forceNetworkOutput("inDegree")), style = "border-right:thick double #fff"),
                                    column(6,wellPanel("OutDegree", forceNetworkOutput("outDegree")))
                                  ),style = "overflow-y:scroll" ),
                         tabPanel("SummaryTable",dataTableOutput("summaryTables"),style = "overflow-y:scroll")
                      
                       )
                    ),
             tabPanel("Observations",
               
               fluidRow(
                        
                      wellPanel( fluidRow( tableOutput("observations")), style="padding-bottom:0px"),
                      wellPanel( fluidRow(htmlOutput("observationsText")),style="padding-top:0px")
               )
            
             )

          ),style=" max-height: 800px !important")
          
        )
)))