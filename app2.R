
library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)


library(tidyverse)
library(shiny)
library(shinyWidgets)
library(dslabs)
library(tidyverse)
library(plotly)


getwd()
setwd("/Users/songjh/Desktop/Dataset")

dataset <- readRDS("/Users/songjh/Desktop/Dataset/shiny/index_data.rds")
#chracter <- us_contagious_chracters
#chracter <- mutate(chracter, percapita = count/(population/100000)) %>% 
#  pivot_longer(cols = c(count, percapita), 
#               names_to = "data", values_to = "value")
korea <- shapefile('TL_SCCO_SIG.shp') %>% 
  spTransform(CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(korea, region = 'SIG_CD')
new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]
names(dataset) <- c('region','avg_officialrate','id','Amount','TradingPrice','Households','one-person','one-person-ratio','light','pop','elder','foreign','elder-ratio','foregin-ratio','subway','artculture','welfare','sport','boys','culture','GDP','GDP_ratio','GDP_per','park','hospital','drug','food','cctv','police','fire','safety','life','trans')




ui <- fluidPage(
  
  titlePanel("Characteristics of Seoul Data"),
  sidebarLayout(
    sidebarPanel(
      # inputs
      selectizeInput("charInput", "characteristic",
                     choices = colnames(dataset),  
                     selected="safety", multiple =FALSE),
      selectizeInput("nInput", "region to compare",
                     choices = dataset$region,  
                     selected="강남구", multiple =TRUE),
      br(),
      tableOutput("view2"),
    ),  
    
    mainPanel(
      plotOutput("datasetplot"),
      br(), 
      br(), 
      plotOutput("distplot"),
      h5('"강남구","강동구","강북구","강서구","관악구","광진구","구로구","금천구","노원구"  
                 ,"도봉구","동대문구" "동작구","마포구","서대문구" "서초구","성동구","성북구","송파구"  
                 ,"양천구","영등포구" "용산구","은평구","종로구","중구","중랑구"순으로 표기'),
      h4("Observations"),
      tableOutput("view")
    ) 
  )   
)   

server <- function(input, output) {
  
 
  d <- reactive({
    dataset %>% dplyr::select(region,input$charInput,id)
  })

  

  output$datasetplot <- renderPlot({
    xx <- merge(seoul_map,d(), by = "id")
    a <- noquote(input$charInput)
    ggplot() +
      geom_polygon(data = xx , 
                   aes(x = long, 
                       y = lat, 
                       group = group,
                       fill = safety), 
                   color = "white") 
  })
  
  

  output$distplot <- renderPlot({
    barplot(dataset[,input$charInput], 
            main=input$region,
            ylab=input$charInput,
            color=input$charInput)+
      geom_text(aes(label = input$charInput), vjust = 1.5, colour = "white") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
    
  })
  output$view2 <- renderTable({
    dataset %>% dplyr::select(region,input$charInput)
  })
  
  output$view <- renderTable({
    head(dataset %>% filter(region == input$nInput))
  })

  

  
}

shinyApp(ui=ui, server=server)

