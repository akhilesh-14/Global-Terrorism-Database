library(shiny)
library(plotly)
library(dplyr)
library(shinythemes)
library(DT)

terr2 <- read.csv('C:/Users/akhil/Downloads/modified_globalterrorismdb_0718dist.csv')

region <- unique(terr2$region_txt)

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  titlePanel("Global Terrorism Database"),
  tags$head(
    tags$style(HTML("
    .shiny-text-output { color: 'orange'; }
  "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("w_countries", "Select Region:", choices = region),
      uiOutput("w_countries1"),
      sliderInput("select_years", "Select Year:",
                  min = 1970, max = 2017, value = c(2010, 2017))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
                 plotlyOutput("map_1")
        ),
        tabPanel("Charts",
                 plotlyOutput("bar_line_1"),
                 plotlyOutput("pie")
        ),
        tabPanel("Summary", DTOutput("summaryTable"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  #Selecting the countries from above input and subsetting the countries based on region.
  # whenever there is a change in input$w_countries it will automatically excecute the updateselectIput statement.
  observeEvent(input$w_countries, {
    #It will update the below select Input UI element based on user selection.
    updateSelectInput(session, "w_countries1", choices = unique(terr2$country_txt[terr2$region_txt == input$w_countries]))
  })
  
  # code for select Input drop down.
  output$w_countries1 <- renderUI({
    countries <- unique(terr2$country_txt[terr2$region_txt == input$w_countries])
    selectInput("w_countries1", "Select Country:", choices = countries)
  })
  
  output$map_1 <- renderPlotly({
    terr3 <- terr2 %>%
      group_by(region_txt, country_txt, provstate, city, iyear, latitude, longitude) %>%
      summarise(nkill = sum(nkill), nwound = sum(nwound), .groups = "drop") # Added .groups argument
    
    terr4 <- terr3 %>%
      filter(region_txt == input$w_countries & country_txt == input$w_countries1 & 
               iyear >= input$select_years[1] & iyear <= input$select_years[2])
    
    if (!is.null(input$w_countries1)) {
      zoom_lat <- terr4$latitude[1]
      zoom_lon <- terr4$longitude[1]
      zoom <- 3
    } else {
      zoom_lat <- 0
      zoom_lon <- 0
      zoom <- 1
    }
    
    p <- plot_ly(terr4, lon = ~longitude, lat = ~latitude, type = "scattermapbox", mode = "markers",
                 marker = list(size = terr4$nwound, color = terr4$nwound, colorscale = "hsv", showscale = FALSE, sizemode = "area"),
                 text = paste("<b>Region:</b> ", terr4$region_txt, "<br>",
                              "<b>Country:</b> ", terr4$country_txt, "<br>",
                              "<b>Province/State:</b> ", terr4$provstate, "<br>",
                              "<b>City:</b> ", terr4$city, "<br>",
                              "<b>Longitude:</b> ", terr4$longitude, "<br>",
                              "<b>Latitude:</b> ", terr4$latitude, "<br>"),
                 hoverinfo = "text")
    
    # Add layout
    p <- p %>% layout(
      mapbox = list(
        style = "open-street-map",
        zoom = zoom,
        center = list(lat = zoom_lat, lon = zoom_lon)
      ),
      autosize = TRUE
    )
    
    return(p)
  })
  
  
  output$bar_line_1 <- renderPlotly({
    terr5 <- terr2 %>%
      filter(region_txt == input$w_countries & country_txt == input$w_countries1 & iyear >= input$select_years[1] & iyear <= input$select_years[2]) %>%
      group_by(iyear) %>%
      summarise(nkill = sum(nkill), attcktyp1 = sum(attacktype1), nwound = sum(nwound))
    
    plot_ly() %>%
      add_trace(data = terr5, x = ~iyear, y = ~nkill, type = "scatter", mode = "lines+markers", name = "Death",
                line = list(shape = "spline", smoothing = 1.3, width = 3, color = '#FF00FF'),
                marker = list(size = 10, symbol = 'circle', color = 'white',
                              line = list(color = '#FF00FF', width = 2)
                ),
                hoverinfo = "text",
                text = paste("<b>Region:</b> ", input$w_countries, "<br>",
                             "<b>Country:</b> ", input$w_countries1, "<br>",
                             "<b>Year:</b> ", terr5$iyear, "<br>",
                             "<b>Death:</b> ", terr5$nkill, "<br>"
                )
      ) %>%
      add_trace(data = terr5, x = ~iyear, y = ~attcktyp1, type = "bar", name = "Attack",
                marker = list(color = 'orange'),
                hoverinfo = "text",
                text = paste("<b>Region:</b> ", input$w_countries, "<br>",
                             "<b>Country:</b> ", input$w_countries1, "<br>",
                             "<b>Year:</b> ", terr5$iyear, "<br>",
                             "<b>Attack:</b> ", terr5$attcktyp1, "<br>"
                )
      ) %>%
      add_trace(data = terr5, x = ~iyear, y = ~nwound, type = "bar", name = "Wounded",
                marker = list(color = '#9C0C38'),
                hoverinfo = "text",
                text = paste("<b>Region:</b> ", input$w_countries, "<br>",
                             "<b>Country:</b> ", input$w_countries1, "<br>",
                             "<b>Year:</b> ", terr5$iyear, "<br>",
                             "<b>Wounded:</b> ", terr5$nwound, "<br>"
                )
      ) %>%
      layout(barmode = "stack",
             plot_bgcolor = 'white',  # Adjusted background color
             paper_bgcolor = 'white', # Adjusted background color
             title = paste("Attack and Death: ", input$w_countries1, " - ", input$select_years[1], " to ", input$select_years[2]),
             xaxis = list(title = "Year", tick0 = 0, dtick = 1, color = 'black', showline = TRUE, showgrid = TRUE, showticklabels = TRUE,
                          linecolor = 'black', linewidth = 2, ticks = "outside",
                          tickfont = list(family = 'Arial', size = 12, color = 'black')),
             yaxis = list(title = "Attack and Death", color = 'black', showline = TRUE, showgrid = TRUE, showticklabels = TRUE,
                          linecolor = 'black', linewidth = 2, ticks = "outside",
                          tickfont = list(family = 'Arial', size = 12, color = 'black')),
             legend = list(orientation = "h", bgcolor = 'white', xanchor = "center", x = 0.5, y = -0.3),
             font = list(family = 'Arial', size = 12, color = 'black')
      )
  })
  
  output$pie <- renderPlotly({
    terr9 <- terr2 %>%
      filter(region_txt == input$w_countries & country_txt == input$w_countries1 & iyear >= input$select_years[1] & iyear <= input$select_years[2]) %>%
      summarise(death = sum(nkill), wound = sum(nwound), attack = sum(attacktype1))
    
    plot_ly(terr9, type = "pie", labels = c("Total Death", "Total Wounded", "Total Attack"), values = c(terr9$death, terr9$wound, terr9$attack), textposition = "inside",
            textinfo = "percent+label", insidetextfont = list(color = "black"),
            hoverinfo = "label+value+percent",
            marker = list(colors = c("#FF00FF", "#9C0C38", "orange"), line = list(color = "black", width = 2))) %>%
      layout(title = paste("Total Casualties: ", input$w_countries1, " - ", input$select_years[1], " to ", input$select_years[2]),
             showlegend = TRUE,
             legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.15),
             plot_bgcolor = 'white',  # Adjusted background color
             paper_bgcolor = 'white', # Adjusted background color
             font = list(family = "Arial", size = 12, color = "black"),
             margin = list(l = 100, r = 100, t = 100, b = 100, pad = 4))
  })
  
  
  # Create a summary table
  # output$summaryTable <- renderDT({
  #   datatable(terr2)
  #  })
  
  
  filteredData <- reactive({
    req(input$w_countries) # Ensure that input$w_countries is not NULL
    terr2 %>%
      filter(region_txt == input$w_countries,
             country_txt %in% input$w_countries1,
             iyear >= input$select_years[1],
             iyear <= input$select_years[2])
  })
  
  # Render the summary table
  output$summaryTable <- renderDataTable({
    req(filteredData()) # Ensure that filteredData is not NULL
    datatable(filteredData(), options = list(pageLength = 5))
  })
  
}

shinyApp(ui = ui, server = server)

