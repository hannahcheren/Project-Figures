---
title: "Stat 431 Project"
author: "Stanley Wu"
date: "2023-02-10"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(tidyverse)
library(dplyr)
library(lubridate)
library(shiny)
```

```{r}
airlines_delay = read.csv("https://uwmadison.box.com/shared/static/fqhiqrx2x4lf7n7dkwv1av86fhz76xhx")
```


```{r}
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
```

```{r}
library(bslib)
```


```{r}
airline_map <- st_read("https://uwmadison.box.com/shared/static/xko8dxlpp6ex5nzttkgyt7nfakw13ghi")
```

```{r}
us_map <- st_read("https://uwmadison.box.com/shared/static/1pno64szhaa3j2uhtlwp5ibfx6cqtcg8")
```

THIS TO CREATE TIME SERIES
```{r}

final_df_not_refactored <- airlines_delay

sample_not_refactored <- final_df_not_refactored %>% 
  mutate(late_pct = (arr_del15 / arr_flights) * 100) %>% 
  dplyr::select(year, month,carrier, carrier_name,airport_name, arr_flights, arr_del15, arr_delay, late_pct)

sample_not_refactored$date <- as.Date(paste(sample_not_refactored$year, sample_not_refactored$month, "1", sep = "-"))
```


```{r}
tsib <- as_tsibble(sample_not_refactored, key = c(carrier, airport_name), index = date)
```

THIS CODE CHUNK TO CREATE MAPPING OF US AND AIRPORTS
```{r}
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

```


TEST MAPPING OF US AND AIRPORTS
```{r}
tmap_mode("view")
tm_shape(us_map) +
  tm_polygons() +
  tmap_options(check.and.fix = TRUE) +
  
  # Plot airport dots
  tm_shape(final_df) +
  tm_dots(col = "late_pct",
          size = 0.1,
          alpha = 0.1,
          legend.show = FALSE,
          id = "airport_name",
          popup.vars = c( "year", "arr_flights", "month", "late_pct"),
          palette = "RdYlBu")
```

TEST TIME SERIES
```{r}
ggplot(tsib, aes(x = date, y = late_pct, color = carrier)) +
  geom_line() +
  labs(title = "Time Series of Arriving Flights",
       x = "Date", y = "Arriving Flights",
       color = "Carrier")
```




```{r}
theme_set(theme_bw())
plot_time_series <- function(df) {
  ggplot(df) +
    geom_line(aes(date, late_pct, color = carrier)) +
    labs(x = "Year", y = "Late Percentage %") +
    guides(color = guide_legend(override.aes = list(fill = NA, size = 2))) +
    theme(
      panel.background = element_rect(fill= "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_text(size = 20),
      legend.background = element_rect(fill = "transparent"),
      legend.key = element_rect(fill = "transparent")
    )
}
```


```{r}
descriptive_text = p("Immerse yourself in the world of aviation with our interactive visualization, which allows you to explore the top 20 airports from 2004 to 2023. Each dot on the plot represents an airport, and the plot dynamically updates based on your input. It's important to note that the data includes information up until 2023, the most recent year available. However, due to the presence of newer carriers and potential data gaps from the early 2010s, some information may be incomplete. Nevertheless, in order to be comprehensive, we have retained data for all airports. This app is designed to empower consumers in making informed decisions about airlines and airports for their travels. We hope this tool helps you in selecting the best airline for your needs.")
```


SHINY APP
```{r}
# Define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly",
                   base_font = font_google("PT Serif")),
  # Add tab panels
  titlePanel("Analyzing US Flight Data from 2004 to 2023"),
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
                 plotOutput("time_series_plot")
               )
             )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Create reactive filtered data based on user input for map
  filtered_data_map <- reactive({
    final_df %>%
      filter(year == input$year,
             month == input$month,
             carrier_name == input$carrier_map)
  })
  
  # Render the tmap map
  output$map <- renderTmap({
    tmap_mode("view")
    tm_shape(us_map) +
      tm_polygons() +
      tmap_options(check.and.fix = TRUE) +
      tm_shape(filtered_data_map()) +
      tm_dots(col = "red",
              size = 0.3,
              legend.show = FALSE,
              id = "airport_name",
              popup.vars = c("arr_flights", "late_pct", "arr_delay"),
              palette = "RdYlBu")
  })
  
  # Create reactive filtered data based on user input for time series
  filtered_data_time_series <- reactive({
    tsib %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2],
             carrier_name %in% input$carrier_time_series)
  })
  
  # Render the time series plot
  output$time_series_plot <- renderPlot({
    plot_time_series(filtered_data_time_series())
  })
}

# Run the Shiny app
shinyApp(ui, server)

```





