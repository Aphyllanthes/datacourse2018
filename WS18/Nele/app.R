#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)
library(leaflet)
library(dplyr)

indices <- read.table("Metadata_DataCourse_2017_2018 - hobo_indices.csv", 
                      sep=",", header=TRUE, stringsAsFactors=FALSE)
meta_hobo <- read.table("Metadata_DataCourse_2017_2018 - meta_hobo.csv", 
                        sep=",",  header=TRUE, stringsAsFactors=FALSE)
meta_hobo <- meta_hobo[-which(is.na(meta_hobo$latitude)==T),]
meta_hobo_all <- left_join(meta_hobo, indices[,5:11], by="hobo_id")

## leaflet
library(sp)

hobo_sp <- SpatialPointsDataFrame(meta_hobo_all[,c("longitude", "latitude")], meta_hobo_all[,c(1:6, 10:16)])

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
pal.mx <- colorNumeric(
  palette = topo.colors(10),
  domain = c(0, 10))
pal.rev <-  colorNumeric(
  palette = rev(topo.colors(10)),
  domain = c(0, 10))


ui <- navbarPage("Exercises Nele Stackelberg",
                 tabPanel("Map",
  leafletOutput("mymap"),
  p(),
  helpText("This is the temperature data measured by the hobo-sensors",
           "collected by students in the christmas-break 2017/2018."),
  titlePanel("Temperatures in Freiburg"),
  sidebarLayout(
    sidebarPanel(
      selectInput("value", label = "Index", choices = c("Tmax"="Tmx", "Tmean"="Tmean", "Tmin"="Tmn"), selected = "Tmean"),
      checkboxInput("label", label="Label")
    ),
    mainPanel( #h3(textOutput("caption")),
               p("The Temperatures were measured with the Hobo-sensor, which has an acuracy of 0.5 °K. The result shown here has differences in maximum, mean and minimum of about 1 K. The fact, that there is no clear patter visible in the map, implies, that there will be no significant effect of variables like the density of buildings or the elevation."), 
               plotOutput("mpgPlot"))
  )),
  tabPanel("SQL-excersise",
           includeMarkdown("Nele_sql_exercises.Rmd")
  ),
  tabPanel("SQL-advanced",
           includeMarkdown("Nele_sql_advanced.Rmd")
  )
)


server <- function(input, output, session) {
 
  output$Map <-  output$mymap <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap, options = tileOptions(opacity=0.6, noWrap = TRUE)) %>%
      fitBounds(7.65, 47.96, 7.97, 48.05) %>% 
      addLegend("bottomright", pal = pal.rev, values = 0:10,
                title = "Temperature",
                opacity = 0.6, 
                labFormat = labelFormat(suffix = " °C", 
                                        transform = function(x) sort(x, decreasing = TRUE))
      )
    
  })
  
  
  observe({
    
    punkte <- eventReactive(hobo_sp[,input$value], {
      coordinates(hobo_sp)
    }, ignoreNULL = FALSE)
    werte <- as.data.frame(hobo_sp)[,input$value]
    
    leafletProxy("mymap") %>% clearMarkers() %>% 
      addCircleMarkers(data = punkte(), 
                       radius=8,
                       fillOpacity = 0.9,
                       stroke = F,
                       color=rev(pal.mx(werte)),
                       label=if(input$label){as.character(werte)},
                       labelOptions = labelOptions(noHide = T, textsize = "10px"),
                       popup=hobo_sp$first_name) 
  })
  
}



# leaflet() %>%
#   addProviderTiles(providers$Stamen.TonerLite,
#                    options = providerTileOptions(noWrap = TRUE)
#   ) %>%
#   addMarkers(data = points())

# Run the application 
shinyApp(ui = ui, server = server)

