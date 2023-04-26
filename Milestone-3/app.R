knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(dplyr)
library(lubridate)
library(shiny)
library(imputeTS)

airlines_delay = read.csv("https://uwmadison.box.com/shared/static/fqhiqrx2x4lf7n7dkwv1av86fhz76xhx")

library(ceramic)
library(raster)
library(sf)
library(terra)
library(tidyverse)
library(tmap)
library(shiny)
library(tmap)
library(tsibble)
library(feasts)
library(tsibbledata)
library(plotly)
library(bslib)

airline_map <- st_read("https://uwmadison.box.com/shared/static/xko8dxlpp6ex5nzttkgyt7nfakw13ghi")

us_map <- st_read("https://uwmadison.box.com/shared/static/1pno64szhaa3j2uhtlwp5ibfx6cqtcg8")


final_df_not_refactored <- airlines_delay

final_df_not_refactored <- final_df_not_refactored %>% 
    mutate(late_pct = (arr_del15 / arr_flights) * 100) %>% 
    dplyr::select(year, month,carrier, carrier_name,airport_name, arr_flights, arr_del15, arr_delay, late_pct)

avg_time_series <- final_df_not_refactored %>% 
    group_by(year, carrier_name) %>% 
    summarize(avg_late_pct = mean(late_pct))

tsib <- tsibble(avg_time_series,key = carrier_name, index = year)

tsib <- tsib %>%
    na_interpolation("linear", .cols = avg_late_pct)


# Find top 20 busiest airports in airlines_delay data frame, per outside sources
top_20_ap <- airlines_delay %>%
    filter(airport %in% c("ATL", "LAX", "ORD", "DFW", "DEN", "CLT", "PHX", "SFO", "IAH", "SEA", "MCO", "EWR", "LAS", "MIA", "MSP", "DTW", "BOS", "PHL", "LGA", "BWI")) %>% 
    group_by(airport) %>% 
    summarize()

# Filter airlines_delay data frame to include only top 10 busiest airports
airlines_delay <- airlines_delay %>% 
    filter(airport %in% top_20_ap$airport)

# Convert arr_delay from minutes to hours
airlines_delay <- airlines_delay %>% 
    mutate(arr_delay = arr_delay / 60,
           late_pct = (arr_del15 / arr_flights) * 100)


# Filter airline_mapping.geojson data frame to include only top 10 busiest airports
ap_20 <- airlines_delay %>% 
    group_by(airport) %>% 
    summarize()
airline_map <- airline_map %>% 
    filter(ita %in% ap_20$airport)

# Rename ita column to airport

airline_map <- as.data.frame(airline_map)

# Rename ita column to airport and select columns
airline_map <- airline_map %>% 
    rename(airport = ita) %>% 
    dplyr::select(airport, geometry)

# Perform a left join to merge final_df with airline_map on "airport" column
final_df <- left_join(airlines_delay, airline_map, by = "airport")

# Convert final_df back to sf_object
final_df <- st_as_sf(final_df)

final_df$month <- factor(month.name[as.numeric(final_df$month)], levels = month.name)


plot_us_map <- function(x) {
    tmap_mode("view")
    tm_shape(us_map) +
        tm_polygons() +
        tmap_options(check.and.fix = TRUE) +
        tm_shape(x) +
        tm_dots(col = "red",
                size = 0.3,
                legend.show = FALSE,
                id = "airport_name",
                popup.vars = c("arr_flights", "arr_del15" ,"late_pct", "arr_delay"),
                palette = "RdYlBu") +
        tm_layout(title = "Top 20 airports of the US", title.size = 20)
} 

my_theme <- function() {
    theme(legend.position = "top right",
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          title = element_text(size = 30, face = "bold", color = "white"), # Set title text color to white
          axis.text = element_text(size = 20, face = "bold", color = "white"), # Set axis text color to white
          axis.title = element_text(size = 25, face = "bold", color = "white"), # Set axis title text color to white
          axis.title.x = element_text(vjust = -0.5, color = "white"), # Set x-axis title text color to white
          axis.title.y = element_text(vjust = 0.5, angle = 90, color = "white"), # Set y-axis title text color to white
          plot.background = element_rect(fill = "#343a40"), # Set plot background color to match darkly theme
          panel.background = element_rect(fill = "#343a40"), # Set panel background color to match darkly theme
          legend.background = element_rect(fill = "#343a40"), # Set legend background color to match darkly theme
          legend.key = element_rect(fill = "#343a40", color = "#343a40"), # Set legend key background and border color to match darkly theme
          legend.text = element_text(color = "white"), # Set legend text color to white
          legend.title = element_text(color = "white")) # Set legend title text color to white
}


plot_time_series <- function(df) {
    ggplot(df) +
        geom_line(aes(year, avg_late_pct, color = carrier_name)) +
        xlab("Year") +
        ylab("Late Percentage %") +
        labs(title = "Average Late Percentage %", subtitle = "2004-2023") + # Set title and subtitle
        guides(color = guide_legend(override.aes = list(fill = NA, size = 2))) +
        my_theme()
    
}


descriptive_text = p("Immerse yourself in the world of aviation with our interactive visualization, which allows you to explore the top 20 airports from 2004 to 2023. Each dot on the plot represents an airport, and the plot dynamically updates based on your input. It's important to note that the data includes information up until 2023, the most recent year available. However, due to the presence of newer carriers and potential data gaps from the early 2010s, some information may be incomplete. Nevertheless, in order to be comprehensive, we have retained data for all airports. This app is designed to empower consumers in making informed decisions about airlines and airports for their travels. We hope this tool helps you in selecting the best airline for your needs.")


# Define UI
ui <- fluidPage(
    theme = bs_theme(bootswatch = "darkly",
                     base_font = font_google("PT Serif")),
    # Add tab panels
    titlePanel("US Flight Data from 2004 to 2023"),
    tabsetPanel(
        tabPanel("US Map",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("year", "Select Year", choices = unique(final_df$year)),
                         selectInput("month", "Select Month", choices = unique(final_df$month)),
                         selectInput("carrier_map", "Select Carrier", choices = unique(final_df$carrier_name)),
                         descriptive_text
                     ),
                     mainPanel(
                         tmapOutput("map", height = "900px", width = "1500px")
                     )
                 )
        ),
        tabPanel("Time Series",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("carrier_time_series", "Select Carrier", choices = unique(tsib$carrier_name), multiple = TRUE),
                         sliderInput("year_range", "Select Year Range", 
                                     min = min(tsib$year), 
                                     max = max(tsib$year), 
                                     value = c(min(tsib$year), max(tsib$year)),
                                     step = 1)
                     ),
                     mainPanel(
                         plotOutput("time_series_plot", height = "900px", width = "1500px")
                     )
                 )
        ),
        tabPanel("Boxplot",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("carrier", "Select Carrier(s):", 
                                     choices = unique(airlines_delay$carrier_name), 
                                     multiple = TRUE),
                         sliderInput("year_range", "Select Year Range:",
                                     min = min(airlines_delay$year),
                                     max = max(airlines_delay$year),
                                     value = c(min(airlines_delay$year), max(airlines_delay$year))),
                         sliderInput("month_range", "Select Month Range:",
                                     min = 1,
                                     max = 12,
                                     value = c(1, 12),
                                     step = 1)
                     ),
                     mainPanel(
                         plotOutput("boxplot", "800px", width = "1400px"),
                         tableOutput("summary_table")
                     )
                 )
        )
    ))

# Define Server
server <- function(input, output) {
    
    # Create reactive filtered data based on user input for map
    filtered_data_map <- reactive({
        final_df %>%
            filter(year == input$year,
                   month == input$month,
                   carrier_name == input$carrier_map)
    })
    
    # Create reactive filtered data based on user input for time series
    filtered_data_time_series <- reactive({
        tsib %>%
            filter(year >= input$year_range[1] & year <= input$year_range[2],
                   carrier_name %in% input$carrier_time_series)
    })
    
    # Create reactive filtered data based on user input for box plot
    filtered_data_boxplot <- reactive({
        airlines_delay %>%
            filter(carrier_name %in% input$carrier) %>%
            filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
            filter(month >= input$month_range[1] & month <= input$month_range[2])
    })
    
    # Create reactive filtered data based on user input for bar plot
    
    
    # Render the tmap map
    output$map <- renderTmap({
        plot_us_map(filtered_data_map())
    })
    
    # Render the time series plot
    output$time_series_plot <- renderPlot({
        plot_time_series(filtered_data_time_series())
    })
    
    # Render the box plot
    output$boxplot <- renderPlot({
        data <- filtered_data_boxplot()
        
        ggplot(data, aes(x = reorder(carrier_name, -arr_delay, mean), y = arr_delay)) +
            geom_boxplot() +
            labs(x = "Carrier Name", y = "Arrival Delay in Hours") +
            my_theme()
        
    })
    
    # Render the summary table for the box plot
    output$summary_table <- renderTable({
        data <- filtered_data_boxplot()
        aggregate(arr_delay ~ carrier_name, data, FUN = function(x) {
            paste0(formatC(mean(x), digits = 2, format = "f"))
        }) %>%
            arrange(desc(arr_delay)) %>%
            setNames(c("Carrier", "Arrival Delay"))
    }, rownames = FALSE)
    
    
}

# Run the Shiny app
shinyApp(ui, server)





