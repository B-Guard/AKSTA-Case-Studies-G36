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
world_map <- left_join(world_map, df, by = c("ISO3" = "ISO3"))

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
          dataTableOutput("raw_data")
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
    df %>% select(country, continent, !!input$variable)
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
      selected_data()
    },
    options = list(
      pageLength = 15, autoWidth = TRUE,
      lengthMenu = list(c(15, -1), c("15", "All"))
    )
  )

  output$map_plot <- renderPlotly({
    gg <- ggplot(world_map, aes(x = long, y = lat, group = group, fill = !!as.name(input$variable))) +
      geom_polygon(color = "white") +
      scale_fill_viridis_c() +
      theme_void() +
      labs(fill = input$variable)
    ggplotly(gg, tooltip = c("region", input$variable))
  })

  output$boxplot_overall <- renderPlotly({
    gg <- ggplot(df, aes(y = !!as.name(input$variable))) +
      geom_boxplot() +
      labs(y = input$variable, x = "Overall")
    ggplotly(gg)
  })

  output$boxplot_continent <- renderPlotly({
    gg <- ggplot(df, aes(x = continent, y = !!as.name(input$variable), fill = continent)) +
      geom_boxplot() +
      labs(y = input$variable, x = "Continent")
    ggplotly(gg)
  })

  output$scatter_plot <- renderPlotly({
    gg <- ggplot(df, aes(
      x = !!as.name(input$var_x), y = !!as.name(input$var_y),
      color = continent, size = !!as.name(input$size_var)
    )) +
      geom_point() +
      labs(x = input$var_x, y = input$var_y)
    ggplotly(gg)
  })
}

shinyApp(ui = ui, server = server)
