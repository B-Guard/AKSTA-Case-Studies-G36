library(jsonlite)
library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)
library(maps)
library(countrycode)
library(DT)
library(tidyjson)

json_data <- fromJSON(readLines("../data/data_cia.json"))
df <- as.data.frame(json_data)
df <- df %>% filter(!is.na(continent))

# Prepare the world map data
world_map <- map_data("world")
world_map$ISO3 <- countrycode::countrycode(
  sourcevar = world_map$region,
  origin = "country.name",
  destination = "iso3c", nomatch = NA
)

# Join world map with df
df$ISO3 <- countrycode::countrycode(df$country, "country.name", "iso3c")

### APP ###

# Define a mapping from variable names to user-friendly labels
friendly_labels <- list(
  median_age = "Median age",
  youth_unempl_rate = "Youth unemployment rate",
  net_migr_rate = "Net migration rate",
  pop_growth_rate = "Population growth rate",
  electricity_fossil_fuel = "Electricity (fossil fuel)",
  life_expectancy = "Life expectancy",
  population = "Population",
  area = "Area",
  country = "Country"
)


ui <- fluidPage(
  titlePanel(tags$strong("CIA World Factbook 2020"), "CIA World Factbook 2020"),
  helpText(
    "Welcome to my shiny app, which allows you",
    "to visualize variables from the CIA 2020 factbook",
    "on the world map, generate descriptive statistics",
    "and statistical graphics."
  ),
  tabsetPanel(
    tabPanel(
      "Univariate analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput("variable", "Select a variable:",
            choices = list(
              "Median age" = "median_age",
              "Youth unemployment rate" = "youth_unempl_rate",
              "Net migration rate" = "net_migr_rate",
              "Population growth rate" = "pop_growth_rate",
              "Electricity (fossil fuel)" = "electricity_fossil_fuel",
              "Life expectancy" = "life_expectancy"
            )
          ),
          actionButton("view_data", "View raw data"),
          dataTableOutput("raw_data"),
          radioButtons("outlier", "Outliers:",
            choices = list("Keep" = "keep", "Remove" = "remove"),
            selected = "keep"
          ),
        ),
        mainPanel(
          tabsetPanel(
            id = "univariate_tabs",
            tabPanel("Map", plotlyOutput("map_plot")),
            tabPanel("Boxplot (overall)", plotlyOutput("boxplot_overall")),
            tabPanel("Boxplot per continent", plotlyOutput("boxplot_continent"))
          )
        )
      )
    ),
    tabPanel(
      "Multivariate analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput("var_x", "Select variable 1:",
            choices = list(
              "Median age" = "median_age",
              "Youth unemployment rate" = "youth_unempl_rate",
              "Net migration rate" = "net_migr_rate",
              "Population growth rate" = "pop_growth_rate",
              "Electricity (fossil fuel)" = "electricity_fossil_fuel",
              "Life expectancy" = "life_expectancy"
            )
          ),
          selectInput("var_y", "Select variable 2:",
            choices = list(
              "Median age" = "median_age",
              "Youth unemployment rate" = "youth_unempl_rate",
              "Net migration rate" = "net_migr_rate",
              "Population growth rate" = "pop_growth_rate",
              "Electricity (fossil fuel)" = "electricity_fossil_fuel",
              "Life expectancy" = "life_expectancy"
            )
          ),
          selectInput("size_var", "Scale points by:",
            choices = list("Population" = "population", "Area" = "area")
          )
        ),
        mainPanel(
          plotlyOutput("scatter_plot")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  selected_data <- reactive({
    data <- df
    if (input$outlier == "remove") {
      data <- data %>% dplyr::filter(net_migr_rate > -31, net_migr_rate < 25)
    }
    data <- data %>% select(country, continent, !!input$variable, ISO3)
    data
  })

  observeEvent(input$variable, {
    print(input$variable)
  })

  observeEvent(input$var_x, {
    print(input$var_x)
  })

  output$raw_data <- renderDataTable(
    {
      req(input$view_data)
      data <- selected_data()
      # Drop ISO3 column
      data <- data %>% select(-ISO3)
      colnames(data) <- c("Country", "Continent", friendly_labels[[input$variable]])
      # Order by the selected variable
      data <- data[order(data[[friendly_labels[[input$variable]]]]), ]
      data
    },
    options = list(
      pageLength = 15, autoWidth = TRUE, lengthChange = FALSE
    )
  )

  output$map_plot <- renderPlotly({
    df <- selected_data()
    world_map <- left_join(world_map, df, by = c("ISO3" = "ISO3"))
    gg <- ggplot(world_map, aes(
      x = long, y = lat, group = group, fill = !!as.name(input$variable),
      text = paste("Country:", country, "<br>", friendly_labels[[input$variable]], get(input$variable))
    ), ) +
      geom_polygon(color = "white") +
      scale_fill_viridis_c() +
      theme_void() +
      labs(fill = friendly_labels[[input$variable]]) +
     ggplotly(gg, tooltip = "text")
  })

  output$boxplot_overall <- renderPlotly({
    df <- selected_data()
    gg <- ggplot(df, aes(y = !!as.name(input$variable))) +
      geom_boxplot() +
      labs(y = friendly_labels[[input$variable]], x = "Overall")
    ggplotly(gg)
  })

  output$boxplot_continent <- renderPlotly({
    df <- selected_data()
    gg <- ggplot(df, aes(x = continent, y = !!as.name(input$variable), fill = continent)) +
      geom_boxplot() +
      labs(y = friendly_labels[[input$variable]], x = "Continent")
    ggplotly(gg)
  })

  output$scatter_plot <- renderPlotly({
    gg <- ggplot(df, aes(
      x = !!as.name(input$var_x), y = !!as.name(input$var_y),
      color = continent, size = !!as.name(input$size_var),
      text = paste(
        friendly_labels[[input$var_x]], df[[input$var_x]],
        "<br>", friendly_labels[[input$var_y]], df[[input$var_y]],
        "<br>Country:", df$country
      )
    )) +
      geom_point(alpha = 0.6) +
      labs(x = friendly_labels[[input$var_x]], y = friendly_labels[[input$var_y]])
    ggplotly(gg, tooltip = "text")
  })
}

# https://medium.com/@derilraju/fix-multiple-unwanted-legends-in-ggplotly-in-r-af734c2fc427
# https://stackoverflow.com/questions/69484878/how-to-change-legend-position-in-ggplotly-in-r

shinyApp(ui = ui, server = server)
