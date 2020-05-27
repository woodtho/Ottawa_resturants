
library(shiny)
library(tidyverse)
library(ggmap)
library(plotly)
library(shinydashboard)

plot_data <-  readxl::read_xlsx(path = "Ottawa_resturants/data/Ottawa resturants Plot Data.xlsx") %>% 
    rename_all(., ~str_remove(., " F"))
register_google(key = "AIzaSyA6OHhamkObM7HS7TFnbj2cMv16SQgfDHU")

p <- qmap(location = "Ottawa, Canada") 

status <- c("All", plot_data$Conclusion %>% unique() %>% sort())

dates <- plot_data$RY %>% unique() %>%  sort()

types <- c("All", 
           paste0({plot_data$types %>% na.omit()},
                  collapse = ", ") %>% 
               str_split(., pattern = ",",
                         simplify = T) %>% 
               str_to_title() %>% 
               str_squish() %>% 
               unique() %>% 
               sort())

categories <- c("All", 
                paste0({plot_data$categories_yelp %>% na.omit()},
                       collapse = ", ") %>%
                    str_split(., pattern = ",",
                              simplify = T) %>%
                    str_to_title() %>% 
                    str_squish() %>% 
                    unique() %>% 
                    sort())

ui <- dashboardPage(
    # Application title
    dashboardHeader(title = "Open Hours Ottawa"),
    
    # Sidebar with a slider input for number of bins 
    dashboardSidebar(sidebarMenu(
        selectInput("indicator", "Indicator: ",
            choices = c("Status Changes" = "Conclusion", "Hour Changes" = "Total")),
        selectInput("date", "Date: ", dates),
        selectInput("Conclusion", "Business Status: ", status, selected = "All"),
        selectInput("type", "Business Type: ", types, selected = "All"),
        selectInput("categories", "Business Categories: ", categories, selected = "All")
    )), 

        # Show a plot of the generated distribution
        dashboardBody(
            tags$head(
                tags$script(
                    '
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        '
                )
            ),
            
           plotlyOutput("plot")
        )
    )




# Define server logic required to draw a histogram
server <- function(input, output) {
    
       output$plot <- renderPlotly({
           req(input$categories)
           req(input$type)
           req(input$indicator)
           req(input$Conclusion)
           req(input$date)
           req(input$dimension)
           
           data <- plot_data %>%
               filter(stringr::str_detect(str_to_title(.$types), as.character(input$type)) |
                          input$type == 'All') %>%
               filter(stringr::str_detect(str_to_title(.$categories_yelp), as.character(input$categories)) |
                          as.character(input$categories) == 'All') %>%
               
               filter(stringr::str_detect(str_to_title(.$Conclusion), as.character(input$Conclusion)) |
                          as.character(input$Conclusion) == 'All') %>% 
               filter(as.character(RY) == input$date)
           
        p2 <- p +
            geom_point(
                aes_string(
                    x = "geometry_location_lng",
                    y = "geometry_location_lat",
                    fill = input$indicator,
                    text = "text"
                ),
                data = data,
                size = 2,
                colour = "black"
            ) +
            switch(input$indicator,
                   "Conclusion" = scale_fill_viridis_d(option = "plasma"),
                   "Total" = scale_fill_viridis_c(option = "plasma")
                   ) 
            
        
        # make plot interactive ---------------------------------------------------
        
        plotly::ggplotly(p2, tooltip = "text",
                         width = (0.85 * as.numeric(input$dimension[1])),
                         height = (.9 * as.numeric(input$dimension[2]))
                         ) %>%
            plotly::layout(legend = list(
                                        orientation = "h",
                                        x = 0.25, 
                                        y = 0 )) %>% 
            config(
                displaylogo  = FALSE,
                modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "toggleSpikelines", "autoScale2d"),
                toImageButtonOptions = list(
                    format = "png",
                    filename = "Open Hours Ottawa"
                )
                
            )
        
       
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
