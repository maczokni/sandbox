# example URL: http://127.0.0.1:6848/?a=25&b=30

library(shiny) ; library(rgdal); library(dplyr) ; library(leaflet)

data <- read.csv("crime_data.csv", header = T, na.strings=c("",".","NA"))

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(id = "controls", class="panel panel-default",
                fixed = TRUE, draggable = FALSE, top = 20, left = "auto", right = 10, bottom = "auto", width = 200, height = "auto",
                div(class="outer",
                    tags$head(includeCSS('custom.css')),
                    br(),
                    actionButton("selectall", label="Select/Deselect all"),
                    br(),
                    br(),
                    div(uiOutput('category'), style = "color:#ffffff", align = "left"),
                    br(),
                    uiOutput(style = "color:red", align = "left", "frequency")))
)

server <- function(input, output, session) 
{
  
  output$category <- renderUI({
    checkboxGroupInput("category", label = NULL,
                choices = levels(droplevels(selected()$category)),
                selected = levels(droplevels(selected()$category)))
  }) 
  
  # select/deselect all button
  observe({
    if (input$selectall > 0) {
      if (input$selectall %% 2 == 0){
        updateCheckboxGroupInput(session=session, inputId="category",
                                 choices = levels(droplevels(selected()$category)),
                                 selected = levels(droplevels(selected()$category)))
      }
      else {
        updateCheckboxGroupInput(session=session, inputId="category",
                                 choices = levels(droplevels(selected()$category)),
                                 selected = c())
      }
    }
  })
  
  # query data through URL
    selected <- reactive({
      qq <- parseQueryString(session$clientData$url_search)
      qA <- qq$a    
      qB <- qq$b    
      qAB <- paste(qA,qB, sep=":")
      data[qA:qB, ]
    })
  
  # frequency of incidents displayed in browser window    
    dataInBounds <- reactive({
      df <- selected() %>% 
        filter(category %in% input$category)
      if (is.null(input$map_bounds))
        return(df[FALSE,])
      bounds <- input$map_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      subset(df,
             lat >= latRng[1] & lat <= latRng[2] &
               long >= lngRng[1] & long <= lngRng[2])
    })
    
    # print frequency of incidents displayed in browser window  
    output$frequency <- renderUI({
      df <- selected() %>% 
        filter(category %in% input$category)
      if (nrow(df) == 0){
        return(NULL)
      }else{
        paste("Incidents displayed: ", nrow(df))
      }
    })
   
    # map output 
    output$map <- renderLeaflet({
      df <- selected() %>% 
        filter(category %in% input$category)
      leaflet(df) %>%
        fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))

      popup <- paste0("<strong>Category: </strong>", df$category,
                      "<br><strong>Month: </strong>", df$date,
                      "<br><strong>Location: </strong>", df$location)
      
      factpal <- colorFactor("Dark2", df$category)
      
      map <- leafletProxy("map", data=df) #%>% 
      if(nrow(distinct(df, concat = paste(long, lat, sep="-"))) > 1) {
        map %>% 
          addProviderTiles("CartoDB.Positron") %>% 
          addCircleMarkers(stroke = TRUE, color = "white", weight = 5, fillColor = ~factpal(category), fillOpacity = 0.9, popup = popup)
      } else 
        if(nrow(distinct(df, concat = paste(long, lat, sep="-"))) == 1) {
          map %>% 
            addProviderTiles("CartoDB.Positron") %>% 
            setView(unique(df$long), unique(df$lat), zoom = 17) %>% 
            addCircleMarkers(stroke = TRUE, color = "white", weight = 5, fillColor = ~factpal(category), fillOpacity = 0.9, popup = popup)
        } else{
          map
        }
      
    })

}

shinyApp(ui, server)
