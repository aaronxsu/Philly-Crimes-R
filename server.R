library(shiny)
library(leaflet)
library(rgdal)

crimeByYear <- read.csv("crimes_by_year.csv")

phillycrime <- readOGR("crimes_philly_tract.geojson", "OGRGeoJSON")

phillycrime$crimebucket2009 <- factor(
  cut(as.numeric(phillycrime$crime2009), c(-1, 300, 700, 1200, 1801, 99999999)),
  labels = c("Less than 300", "300 to 699", "700 to 1199", "1200 to 1800", "More than 1800")
)
phillycrime$crimebucket2010 <- factor(
  cut(as.numeric(phillycrime$crime2010), c(-1, 300, 700, 1200, 1801, 99999999)),
  labels = c("Less than 300", "300 to 699", "700 to 1199", "1200 to 1800", "More than 1800")
)
phillycrime$crimebucket2011 <- factor(
  cut(as.numeric(phillycrime$crime2011), c(-1, 300, 700, 1200, 1801, 99999999)),
  labels = c("Less than 300", "300 to 699", "700 to 1199", "1200 to 1800", "More than 1800")
)
phillycrime$crimebucket2012 <- factor(
  cut(as.numeric(phillycrime$crime2012), c(-1, 300, 700, 1200, 1801, 99999999)),
  labels = c("Less than 300", "300 to 699", "700 to 1199", "1200 to 1800", "More than 1800")
)
phillycrime$crimebucket2013 <- factor(
  cut(as.numeric(phillycrime$crime2013), c(-1, 300, 700, 1200, 1801, 99999999)),
  labels = c("Less than 300", "300 to 699", "700 to 1199", "1200 to 1800", "More than 1800")
)
phillycrime$crimebucket2014 <- factor(
  cut(as.numeric(phillycrime$crime2014), c(-1, 300, 700, 1200, 1801, 99999999)),
  labels = c("Less than 300", "300 to 699", "700 to 1199", "1200 to 1800", "More than 1800")
)
phillycrime$crimebucket2015 <- factor(
  cut(as.numeric(phillycrime$crime2015), c(-1, 300, 700, 1200, 1801, 99999999)),
  labels = c("Less than 300", "300 to 699", "700 to 1199", "1200 to 1800", "More than 1800")
)
phillycrime$crimebucket2016 <- factor(
  cut(as.numeric(phillycrime$crime2016), c(-1, 300, 700, 1200, 1801, 99999999)),
  labels = c("Less than 300", "300 to 699", "700 to 1199", "1200 to 1800", "More than 1800")
)

crimeBucketPal <- colorFactor(c("#3b2a3d", "#755a4a", "#8b844a",  "#e3c700", "#e2e900"), phillycrime$crimebucket2009)

whichYear <- function(year){
  return(phillycrime[[paste("crime", toString(year))]])
}

whichBucket <- function(year){
  #return(phillycrime[paste("crimebucket", toString(year))])
  ifelse(year == 2009, return(phillycrime$crimebucket2009),
  ifelse(year == 2010, return(phillycrime$crimebucket2010),
  ifelse(year == 2011, return(phillycrime$crimebucket2011),
  ifelse(year == 2012, return(phillycrime$crimebucket2012),
  ifelse(year == 2013, return(phillycrime$crimebucket2013),
  ifelse(year == 2014, return(phillycrime$crimebucket2014),
  ifelse(year == 2015, return(phillycrime$crimebucket2015), 
  return(phillycrime$crimebucket2016))))))))
}

function(input, output) {
#-------------------------
#
#   The Chart Page
#
#-------------------------
  model <- reactive({
    brushed_data <- brushedPoints(crimeByYear, input$brush1,
                                  xvar = "Year", yvar = "Crime")
    if(nrow(brushed_data) < 2){
      return(NULL)
    }
    lm(Crime ~ Year, data = brushed_data)
  })
  output$slopeOut <- renderText({
    if(is.null(model())){
      "No Model Found"
    } else {
      model()[[1]][2]
    }
  })
  output$intOut <- renderText({
    if(is.null(model())){
      "No Model Found"
    } else {
      model()[[1]][1]
    }
  })
  output$plot1 <- renderPlot({
    plot(crimeByYear$Year, crimeByYear$Crime, xlab = "Year",
         ylab = "Count of Crime", main = "Crimes in Philadelphia 2009 to 2016",
         cex = 1.5, pch = 16, bty = "n")
    if(!is.null(model())){
      abline(model(), col = "blue", lwd = 2)
    }
  })
#-------------------------
#
#   The Map Page
#
#-------------------------
  output$crimeMap <- renderLeaflet({
    
    leaflet(phillycrime) %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons( data = phillycrime,
                   fillColor = ~crimeBucketPal(whichBucket(input$slider1)),
                   weight = 0.8,
                   opacity = 0.9,
                   smoothFactor = 0.1,
                   color = ~crimeBucketPal(whichBucket(input$slider1)),
                   fillOpacity = 0.9,
                   label = ~paste0("Crimes: ", whichYear(input$slider1)),
                   highlight = highlightOptions(
                     fillColor = "orange",
                     fillOpacity = 1,
                     bringToFront = FALSE)) %>%
      addLegend(pal = crimeBucketPal, 
                values = ~whichBucket(input$slider1), 
                position = "bottomright", 
                title = paste("Crimes in ", toString(input$slider1)),
                opacity = 1)
    
  })
}