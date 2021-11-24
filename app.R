library(leaflet)

library(tidyverse)

library(shiny)

library(lubridate)

library(plotly)

library(dplyr)

library(leaflet.extras)

library(shinythemes)

library(ajfhelpR)

# import data

#install.packages("remotes")

#remotes::install_github("Ajfrick/ajfhelpR")



alldata <-read_rds("data/cache_all_turntiles_data.rds")

alldata <- drop_na(alldata)

alldata$date = as.Date(alldata$date,"%m/%d/%Y")

alldata <-  mutate(alldata,sum = exits + entries)

alldata <-  alldata %>% group_by(stop_name,date) %>% dplyr::summarize(sum = sum(sum),exits= sum(exits), entries= sum(entries),gtfs_longitude = max(gtfs_longitude), gtfs_latitude = max(gtfs_latitude))



#weekly



alldata$week <- floor_date(alldata$date, "week")



weeklydata <-  alldata %>% group_by(week,stop_name) %>% dplyr::summarize(sum = sum(sum),exits= sum(exits), entries= sum(entries),gtfs_longitude = max(gtfs_longitude), gtfs_latitude = max(gtfs_latitude))



names(weeklydata)[names(weeklydata)=="week"] <- "date"



#monthly



alldata$month <- floor_date(alldata$date, "month")



monthdata <-  alldata %>% group_by(month,stop_name) %>% dplyr::summarize(sum = sum(sum),
                                                                         
                                                                         exits= sum(exits), entries= sum(entries),gtfs_longitude = max(gtfs_longitude), gtfs_latitude = max(gtfs_latitude))



names(monthdata)[names(monthdata)=="month"] <- "date"



#yearly



alldata$year <- floor_date(alldata$date, "year")



yeardata <-  alldata %>% group_by(year,stop_name) %>% dplyr::summarize(sum = sum(sum),
                                                                       
                                                                       exits= sum(exits), entries= sum(entries),gtfs_longitude = max(gtfs_longitude), gtfs_latitude = max(gtfs_latitude))

names(yeardata)[names(yeardata)=="year"] <- "date"





top20 <- yeardata[which(yeardata$date == "2019-01-01"),] %>% arrange(desc(sum))%>%slice(1:20)



top20 <- yeardata[which(yeardata$stop_name  %in% top20$stop_name),]







#UI------------------------------------------------------------------------------------------------------------

ui <- bootstrapPage(
  
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">MTA Turntiles tracker</a>'), id="nav",
             
             windowTitle = "COVID-19 tracker",
             
             
             
             tabPanel("MTA Turntile Data",
                      
                      sidebarPanel(
                        
                        
                        
                        uiOutput(outputId = "slider"),
                        
                        
                        
                        
                        
                        
                        
                        dateRangeInput("DateRange", strong("Date range"), start = "2019-01-01", end = "2021-9-9",
                                       
                                       min = "2019-01-01", max = "2021-9-9"),
                        
                        
                        
                        actionButton("refresh",label = "Range Refresh" )
                        
                        
                        
                      ),
                      
                      
                      
                      mainPanel(
                        
                        
                        
                        leafletOutput("mymap",width ="100%",height = 500),
                        
                        
                        
                        uiOutput(outputId = "station"),
                        
                        plotlyOutput("Barchart_Station",width ="100%",height = 500),
                        
                      )
                      
             ),
             
             tabPanel("Region plots",
                      
                      sidebarPanel(        
                        
                        selectInput("selectInt", label = "Select Interval",
                                    
                                    choices = list("Daily" = 1, "Weekly" = 2, "Monthly" = 3,"Annualy" = 4),
                                    
                                    selected = 2),
                        
                        actionButton("PlotRefresh",label = "Plot Data" )),
                      
                      mainPanel(
                        
                        plotlyOutput("MTAplot",width ="100%",height = 600)
                        
                        
                        
                      )
                      
             ),
             
             
             
             tabPanel("Top 20 Busiest Station In NYC",
                      
                      
                      
                      plotlyOutput("BarChart",width ="100%",height = 600)
                      
             ),
             
             
             
             tabPanel("Post_Covid_Recovery",
                      
                      mainPanel(
                        
                        
                        
                        leafletOutput("Recovermap",width ="100%",height = 500)
                        
                      )
                      
                      
                      
             )
             
             
             
  ))



server <- function(input, output) {
  
  
  
  #load default weekly data
  
  all_clean <- weeklydata
  
  # render ui for slider ----------------------------------------------------
  
  output$slider <- renderUI({
    
    sliderInput(inputId =  "slide",
                
                label = "Select a date range",
                
                min = as.Date("2019-01-01"),
                
                max = as.Date("2021-09-09"),
                
                value = c(as.Date("2019-01-01")),
                
                animate = FALSE)})
  
  
  
  # map part ----------------------------------------------------------------
  
  output$mymap  <- renderLeaflet({
    
    leaflet(all_clean[which(all_clean$date == min(all_clean$date)),]) %>%
      
      addTiles()%>% addCircleMarkers(all_clean$gtfs_longitude,
                                     
                                     all_clean$gtfs_latitude,
                                     
                                     radius =ifelse((5*all_clean$sum/median(all_clean$sum))>50,50,5*all_clean$sum/median(all_clean$sum)),
                                     
                                     layerId=all_clean$stop_name)
    
  })
  
  
  
  
  
  output$station <- renderUI({
    
    textInput("SelectedStation", "StationName", value = "1 Av")})
  
  
  
  output$MTAplot<-renderPlotly({
    
    plot_ly(all_clean[which(all_clean$stop_name == "1 Av"),], type = 'scatter', mode = 'markers') %>%
      
      add_trace(x = ~date, y = ~exits, name = 'Exits')%>%
      
      add_trace(x = ~date, y = ~entries, name  =  'Entries')%>%
      
      layout("1 Av",showlegend = T)  })
  
  
  
  
  
  output$Barchart_Station<-renderPlotly({
    
    dataslice = all_clean[which(all_clean$stop_name == "1 Av"),]
    
    close_date = date_near(weeklydata$date, as.Date("2019-01-01"), onlypre = T)
    
    dataslice = dataslice[which(dataslice$date == close_date),]
    
    x <- data.frame("Type" =c("Entries","Exit"), "Count" = c(dataslice$entries,dataslice$exits))
    
    plot_ly(x, labels = ~Type, values = ~Count, type = 'pie') %>%
      
      layout(title =  '1 AV:  Exits and Entries Count ')
    
  })
  
  
  
  output$Recovermap  <- renderLeaflet({
    
    leaflet(all_clean[which(all_clean$date == min(all_clean$date)),]) %>%
      
      addTiles()%>% addHeatmap(lng=all_clean$gtfs_longitude,lat=all_clean$gtfs_latitude,
                               
                               intensity=all_clean$sum/1000000,max=100,radius=20,blur=10)
    
  })
  
  
  
  
  
  output$BarChart<-renderPlotly({
    
    p <-ggplot(top20, aes((x= reorder(top20$stop_name,-top20$sum)),y= top20$sum,
                          
                          fill =factor (top20$date))) +  geom_bar(stat="identity",position="dodge")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    fig<- ggplotly(p)
    
    fig
    
    
    
  })
  
  
  
  
  
  observeEvent(input$slide,{
    
    
    
    
    
    close_date = date_near(weeklydata$date, as.Date(input$slide,"%m/%d/%Y"), onlypre = T)
    
    
    
    
    
    leafletProxy("mymap", data= all_clean[which(all_clean$date ==close_date),]) %>%
      
      addTiles() %>%
      
      clearMarkers() %>%
      
      addCircleMarkers(all_clean$gtfs_longitude,
                       
                       all_clean$gtfs_latitude,
                       
                       radius =ifelse((5*all_clean$sum/median(all_clean$sum))>50,50,5*all_clean$sum/median(all_clean$sum)),
                       
                       layerId=all_clean$stop_name)
    
    
    
    
    
    output$Barchart_Station<-renderPlotly({
      
      dataslice = all_clean[which(all_clean$stop_name == input$SelectedStation),]
      
      dataslice = dataslice[which(dataslice$date == close_date),]
      
      x <- data.frame("Type" =c("Entries","Exit"), "Count" = c(dataslice$entries,dataslice$exits))
      
      plot_ly(x, labels = ~Type, values = ~Count, type = 'pie') %>%
        
        layout(title =  paste(input$SelectedStation, 'Exits and Entries Count ', sep=":") )})
    
    
    
    
    
  })
  
  
  
  
  
  observeEvent(input$mymap_marker_click, {
    
    
    
    p <- input$mymap_marker_click
    
    output$MTAplot<-renderPlotly({
      
      plot_ly(all_clean[which(all_clean$stop_name %in% p$id),], type = 'scatter', mode = 'markers') %>%
        
        add_trace(x = ~date, y = ~exits, name = 'Exits')%>%
        
        add_trace(x = ~date, y = ~entries, name  =  'Entries')%>%
        
        layout(title = p$id,showlegend = T)
      
    })
    
    
    
    
    
    output$station <- renderUI({
      
      textInput("SelectedStation", "StationName", value = p$id)})
    
    
    
    
    
    output$Barchart_Station<-renderPlotly({
      
      dataslice = all_clean[which(all_clean$stop_name ==  p$id),]
      
      close_date = date_near(all_clean$date, as.Date(input$slide,"%m/%d/%Y"), onlypre = T)
      
      dataslice = dataslice[which(dataslice$date == close_date),]
      
      x <- data.frame("Type" =c("Entries","Exit"), "Count" = c(dataslice$entries,dataslice$exits))
      
      plot_ly(x, labels = ~Type, values = ~Count, type = 'pie') %>%
        
        layout(title =  paste(input$SelectedStation, 'Exits and Entries Count ', sep=":") )})
    
    
    
    
    
  })
  
  
  
  observeEvent(input$refresh, {
    
    output$slider <- renderUI({
      
      sliderInput(inputId =  "slide",
                  
                  label = "Select a date range",
                  
                  min = as.Date(input$DateRange[1]),
                  
                  max = as.Date(input$DateRange[2]),
                  
                  value = c(as.Date(input$DateRange[1])),
                  
                  animate =  FALSE)}) 
    
  })
  
  
  
  
  
  observeEvent(input$PlotRefresh, {
    
    
    
    if (input$selectInt ==1)
      
    {all_clean <- alldata}
    
    if (input$selectInt ==2)
      
    {all_clean <- weeklydata}
    
    if (input$selectInt ==3)
      
    {all_clean <- monthdata}
    
    if (input$selectInt ==4)
      
    {all_clean <- yeardata}
    
    
    
    output$MTAplot<-renderPlotly({
      
      dataslice = all_clean[which(all_clean$stop_name == input$SelectedStation),]
      
      plot_ly(dataslice, type = 'scatter', mode = 'markers') %>%
        
        add_trace(x = ~date, y = ~exits, name = 'Exits')%>%
        
        add_trace(x = ~date, y = ~entries, name  =  'Entries')%>%
        
        layout(title= input$SelectedStation, showlegend = T)  }) 
    
  })
  
  
  
  observeEvent(input$selectInt, {
    
    
    
  })
  
  
  
}

shinyApp(ui, server)