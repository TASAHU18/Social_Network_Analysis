list.of.packages <- c("DT","shiny","reshape2","dplyr", "igraph", "networkD3")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(DT)
library(shiny)
library(reshape2)
library(dplyr)
library(networkD3)
library(igraph)


shinyServer(function(input,output){
  
  #Reactive load of data from the UI
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    dataset= read.csv(file=file1$datapath,fileEncoding="UTF-8-BOM",header = TRUE)
    
  })
  
  
  #inputDataset -- Variable containing actual dataset for display on the UI 
  output$inputDataset <- renderTable({
    inputDataSet= data();
    rownames(inputDataSet)= inputDataSet[,1]
    inputDataSet
  })
  
  #meltOutput -- Coverting input data to tall table using melt function of library(reshape2)
  output$meltOutput <-renderDataTable(style='bootstrap',{
    meltController(formatTable());
  })
  
  #Call details
  output$callSummary <-renderTable({
    tallTable= meltController(formatTable())
    totalCalls= sum(tallTable$times)
    
    aggregateMostCallsPlaced=aggregate(tallTable$times, by=list(var=tallTable$calling), FUN=sum)
    personMostCallsPlaced=(aggregateMostCallsPlaced[aggregateMostCallsPlaced$x==max(aggregateMostCallsPlaced$x),])$var
   
    aggregateMostCallsReceived = aggregate(tallTable$times, by=list(var=tallTable$called), FUN=sum)
    personMostCallsReceived=(aggregateMostCallsReceived[aggregateMostCallsReceived$x==max(aggregateMostCallsReceived$x),])$var
   
    data = c(totalCalls,  paste0(personMostCallsReceived,collapse = ", "), paste0(personMostCallsPlaced,collapse = ", ")) 
    headers =c("Total Calls","Persons who received the most calls","Person who placed the most calls")
    outputDF= as.data.frame(cbind(headers,data))
    colnames(outputDF)<- c("Summary Type", "Value")
    outputDF
    
  })
  
  
  output$callNetworkSimple <- renderForceNetwork({
    d3Tables=createTablesD3()
    d3Tables$personNode$size=30
    forceNetwork(Links = d3Tables$undirectedPhoneLinks, Nodes = d3Tables$personNode, Source = "src",
                 Target = "trgt", opacityNoHover = 1 ,NodeID = "name", fontSize=17,
                 Group = "group", opacity = 0.8, zoom=TRUE,linkDistance = 100, linkColour = "#FFF",
                 Nodesize="size", radiusCalculation = JS(" Math.sqrt(d.nodesize)+3"))
  })
  
  
  output$directedOutbound <- renderForceNetwork({
    d3Tables=createTablesD3()
    d3Tables$personNode$size=30
    forceNetwork(Links = d3Tables$undirectedPhoneLinks, Nodes = d3Tables$personNode, Source = "src",
                 Target = "trgt",opacityNoHover = 1, NodeID = "name",Value="value", fontSize=17, linkColour = "#FFF",
                 Group = "group", opacity = 0.8, zoom=TRUE,linkDistance = 100,arrows = T,
                 Nodesize="size", radiusCalculation = JS(" Math.sqrt(d.nodesize)+3"))
  })
  
  
  output$directedInbound <- renderForceNetwork({
    d3Tables=createTablesD3()
    d3Tables$personNode$size=30
    chart= forceNetwork(Links = d3Tables$undirectedPhoneLinks, Nodes = d3Tables$personNode, Source = "trgt",
                 Target = "src", opacityNoHover = 1, NodeID = "name",Value="value", fontSize=17, linkColour = "#FFF",
                 Group = "group",opacity = 0.8, zoom=TRUE,linkDistance = 100, arrows = T,
                 Nodesize="size", radiusCalculation = JS(" Math.sqrt(d.nodesize)+3"))
    
   
  })
  
  
  
  output$degreeCentrality <- renderForceNetwork({
    g=initGraph("undirected");
    set.seed(198)
    d3Tables=createTablesD3()
    d3Tables$personNode$size=centralization.degree(g)$res
    d3Tables$personNode$name = paste(d3Tables$personNode$name, d3Tables$personNode$size, sep=",")
    chart= forceNetwork(Links = d3Tables$undirectedPhoneLinks, Nodes = d3Tables$personNode, Source = "src",
                        Target = "trgt", NodeID = "name",Value="value", linkColour = "#FFF",
                        Group = "group",opacity = 0.8, zoom=TRUE,linkDistance = 200,
                        Nodesize="size",opacityNoHover = 1,fontSize=17 ,
                        radiusCalculation = JS(" Math.sqrt(d.nodesize)*3"),legend=F,arrows = F)
    
  })
  
  
  output$betweenness <- renderForceNetwork({
    g=initGraph("undirected");
    set.seed(198)
    d3Tables=createTablesD3()
    d3Tables$personNode$size=centralization.betweenness(g)$res
    d3Tables$personNode$name = paste(d3Tables$personNode$name, d3Tables$personNode$size, sep=",")
    chart= forceNetwork(Links = d3Tables$undirectedPhoneLinks, Nodes = d3Tables$personNode, Source = "src",
                        Target = "trgt", NodeID = "name",Value="value", linkColour = "#FFF",
                        Group = "group",opacity = 0.8, zoom=TRUE,linkDistance = 200,
                        Nodesize="size",opacityNoHover = 1,fontSize=17 ,
                        radiusCalculation = JS(" Math.sqrt(d.nodesize)*2"),legend=F,arrows = F)
    
    
  })
  
  
  output$inDegree <- renderForceNetwork({
    g=initGraph("directed");
    set.seed(198)
    d3Tables=createTablesD3()
    d3Tables$personNode$size=centralization.degree(g,mode = "in")$res
    d3Tables$personNode$name = paste(d3Tables$personNode$name, d3Tables$personNode$size, sep=",")
    chart= forceNetwork(Links = d3Tables$undirectedPhoneLinks, Nodes = d3Tables$personNode, Source = "src",
                        Target = "trgt", NodeID = "name",Value="value", linkColour = "#FFF",
                        Group = "group",opacity = 0.8, zoom=TRUE,linkDistance = 100,
                        Nodesize="size",opacityNoHover = 1,fontSize=17 ,
                        radiusCalculation = JS(" Math.sqrt(d.nodesize)*2.5"),legend=F,arrows = F)
    
  })
  
  
  output$outDegree <- renderForceNetwork({
    g=initGraph("directed");
    set.seed(198)
    d3Tables=createTablesD3()
    d3Tables$personNode$size=centralization.degree(g,mode="out")$res
    d3Tables$personNode$name = paste(d3Tables$personNode$name, d3Tables$personNode$size, sep=",")
    chart= forceNetwork(Links = d3Tables$undirectedPhoneLinks, Nodes = d3Tables$personNode, Source = "src",
                        Target = "trgt", NodeID = "name",Value="value", linkColour = "#FFF",
                        Group = "group",opacity = 0.8, zoom=TRUE,linkDistance = 100,
                        Nodesize="size",opacityNoHover = 1,fontSize=17 ,
                        radiusCalculation = JS(" Math.sqrt(d.nodesize)*2.5"),legend=F,arrows = F)
    
  })
  
  output$summaryTables <- renderDataTable(style='bootstrap',{
    dataSummary=createSummary()
    dataSummary
  })
  
  output$observations <- renderTable({
    
    dataSummary=createSummary()

    sortedDataByDC <- tail( dataSummary[order(dataSummary$DegreeCentrality),] )      
    sortedDataByDC[sortedDataByDC$DegreeCentrality>5, ]
    topPersonsDC <- paste(rev(sortedDataByDC$Names), sep = ",",collapse=", ")
     
    sortedDataByBW <- tail( dataSummary[order(dataSummary$Betweenness),]) 
    sortedDataByBW[sortedDataByBW$Betweenness>20, ]
    topPersonsBW <- paste(rev(sortedDataByBW$Names), sep = ",",collapse=", ")
    
    headers =c("Id","Observation")
    observation_1 =c("1",   paste(topPersonsBW, "have betweenness greater than 20", sep=" ")   )
    observation_2 =c("2", paste(topPersonsDC, "have the degree centrality greater than 5", sep=" ")   )
    outputDF= as.data.frame(rbind(observation_1,observation_2  ))
    names(outputDF) <- headers
    outputDF
    
    
    
  })
  
  output$observationsText <- renderUI({
    
    tryCatch( {
      dataSummary=createSummary()
      maxBW <-(dataSummary[dataSummary$Betweenness==max(dataSummary$Betweenness),])[1,]$Names
      str1 <- paste("A node with higher degree centrality is better connected to the network. This implies that, information about the other nodes (drug dealers in this case) can be retrieved easily from these nodes." )
      str2 <- paste("A node with higher betweenness would have more control over the network because more information will pass through that node. This indicates that person with higher betweenness are potentially prominant drug dealers and/or suppliers.")
      str3 <- paste (c("According to the given dataset, ", as.character(maxBW), "is the most prominant drug dealer and plays the most vital role in this network."), sep = " ", collapse = " ")
      HTML(paste(str1,str2,str3, sep = '<br/><br/>'))  
    },
    error=function(cond){
      str1 <- paste("Please load the file first")
      HTML(paste(str1, sep = '<br/>'))  
    }
      )

      
    
    
  })
  
  
  #### Functions ####
  #### --------- ####
  
  createSummary <- function()
  {
    g=initGraph("undirected")
    g1= initGraph("directed")
    set.seed(198)
    d3Tables=createTablesD3()
    columnDegCentral=centralization.degree(g)$res
    columnBetweenness=centralization.betweenness(g)$res
    columnOutDegree=centralization.degree(g1,mode="out")$res
    columnInDegree=centralization.degree(g1,mode="in")$res
    d3Tables$personNode$DegreeCentrality=columnDegCentral
    d3Tables$personNode$Betweenness=columnBetweenness
    d3Tables$personNode$OutDegree=columnOutDegree
    d3Tables$personNode$InDegree=columnInDegree
    d3Tables$personNode=d3Tables$personNode[,-2]
    names(d3Tables$personNode)[1]=c("Names")
    d3Tables$personNode
  }
  
  #function used to create graph from adjacency matrix, to find centralization parameters
  initGraph <- function(typeOfGraph){
    dCentralMatrix= formatTable()
    dCentralMatrix=as.matrix.data.frame(dCentralMatrix[,-1])
    graph.adjacency(dCentralMatrix,weighted=T, mode=typeOfGraph)
  }
  
  
  
  #Function to create tables in networkD3 format
  createTablesD3 <- function( ){
    persons=meltController(formatTable())
    personNode=  distinct_(persons[1])
    
    colnames(personNode) <- c("name")
    personNode$group <-  rownames(personNode)
    personNode$group <- as.integer(personNode$group)-1  
    
    undirectedPhoneLinks= persons[persons$times>0,]
    colnames(undirectedPhoneLinks) =c("source","target","value")
    
    personNodeSource=personNode
    colnames(personNodeSource)=c("source","group")
    personNodetarget=personNode
    colnames(personNodetarget)=c("target","group")
    undirectedPhoneLinks <- (merge(personNodeSource, undirectedPhoneLinks, by = 'source'))
    undirectedPhoneLinks=undirectedPhoneLinks[,-1]
    colnames(undirectedPhoneLinks)[1] = "src"
    undirectedPhoneLinks<- (merge(personNodetarget, undirectedPhoneLinks, by = 'target'))
    undirectedPhoneLinks=undirectedPhoneLinks[,-1]
    colnames(undirectedPhoneLinks)[1] <- "trgt"
    
    undirectedPhoneLinks =as.data.frame(sapply(undirectedPhoneLinks, function(x) as.integer(x)))
    list(personNode=personNode,undirectedPhoneLinks=undirectedPhoneLinks)
    
    
  }  
  
  
  
  #Function used to convert input dataset to tall table
  meltController <- function(dataset){
    datasetD=melt(dataset)
    colnames(datasetD)<- c('calling','called','times')
    datasetD
  }
  
  #Function used to preprocess the data
  formatTable <- function(){
    inputDataset=as.data.frame(data());
    rownames(inputDataset) = inputDataset[,1]
    inputDataset
  }
  
  
  
  
  
  }
)