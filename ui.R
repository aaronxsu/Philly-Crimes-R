library(shiny)
library(shinythemes)
library(leaflet)

navbarPage("Crimes In Philly",
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("simplex"),
  tabPanel("About", includeMarkdown("about.md")),
  tabPanel("Graphs",fluidPage(
    titlePanel("Visualize The Trend of Crimes"),
    sidebarLayout(
      sidebarPanel(
        h3("Slope"),
        textOutput("slopeOut"),
        h3("Intercept"),
        textOutput("intOut")
      ),
      mainPanel(
        plotOutput("plot1", brush = brushOpts(
          id = "brush1"
        ))
      )
    )
  )),
  tabPanel("Maps",fluidPage(
    titlePanel("Crimes by Census Tract in Philly 2009 - 2016"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("slider1", "Choose a year", 2009, 2016, 2009)
      ),
      mainPanel(
        leafletOutput("crimeMap")
      )
    )
  )),
  selected = "About"
)