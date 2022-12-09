#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# https://kaiandreic.shinyapps.io/a5_app/
rsconnect::setAccountInfo(name='kaiandreic',
                          token='BCB414A2FF7CAA890FD59358CD92F035',
                          secret='kLBXIoAqWcERaxQNeDMzJeHi4k7mYHoXegsT/Hoa')
df <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
library(shiny)
library(dplyr)
library(rsconnect)
library(ggplot2)
library(plotly)
library(rlang)
oil_co2_all <- df %>% 
  select(country, year, oil_co2)
oil_co2_2021 <- df %>% 
  select(country, year, oil_co2) %>% 
  filter(year == max(year))
oil_co2_2021<- oil_co2_2021[complete.cases(oil_co2_2021),]
oil_co2_vs_pop <- df %>% 
  select(country,year,oil_co2, population, oil_co2_per_capita)
highest_co2_pop <- oil_co2_vs_pop %>% 
  filter(population == max(population, na.rm = TRUE)) %>% 
  select(oil_co2)
high_oil <- oil_co2_2021 %>% 
  filter(country == "China") %>% 
  filter(oil_co2 == max(oil_co2, na.rm = TRUE))  %>% 
  select(oil_co2)
per_captia <- oil_co2_vs_pop %>% 
  filter(oil_co2_per_capita == max(oil_co2_per_capita, na.rm = TRUE)) %>% 
  select(oil_co2_per_capita)
?paste



  introduction_page <- tabPanel(
              "Introduction", 
              h1(strong("Introduction")),
              p(em("Info 201, A5, Kai Andreic")),
              h3("Introduction"),
              p("Throughout this project the main goal was to explore
                the amount of oil CO2 is being exposed and polluting
                the Earth. For example, I wanted to see which
                country had the most CO2 based off oil and it was
                China with", paste(high_oil),".", "Additionally, I wanted to look
                at how much oil is being used per capita and the most being used
                was", paste(per_captia),".", "Lastly, another thing I grabbed was from
                the highest amount at one time was oil CO2 being polluted which was
                ", paste(highest_co2_pop,"."))
        )
    
page2 <- tabPanel(
  "Oil CO2 per country", 
  h1("R Shiny Dynamically create Drop Down List"),
  selectInput(inputId = "sel_country",
              label = "Choose which Country",
              "Names",
              selected = "Andorra",
              multiple = TRUE),
 
  plotOutput("plot")
)
page3 <- tabPanel(
  "Oil CO2 per each Continent", 
  h1(strong("Oil CO2 per each Continent throughout the years")),
  p("Select a country and time range to view the population trends"),
  img(), 
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "country", 
        label = "Select a Country", 
        choices = c("World","Europe", "North America",
                    "South America","Antarctica", "Australia", "Asia", "Africa"),
        selected = "World",
        multiple = TRUE
      ),
      
      sliderInput("timeRange",
                  "Timeframe",
                  min = 1750,
                  max = 2021,
                  value = c(1750, 2021),
                  sep = "")
    ),
    
    mainPanel(
      plotlyOutput("oil_trends_plot")
)
)
)
page4 <- tabPanel(
  "Oil vs Population", 
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "country", 
        label = "Choose Which Continent", 
        choices = c("World","Europe", "North America",
                    "South America","Antarctica", "Australia", "Asia", "Africa"),
        selected = "World", 
        multiple = TRUE
      ),
      
      sliderInput("time_range",
                  "Timeframe",
                  min = 1750,
                  max = 2021,
                  value = c(1750, 2021),
                  sep = ""
      ),
      radioButtons("color_var",
                   "Color Encoding",
                   choiceValues = c("year"),
                   selected = "year",
                   choiceNames = c("year")
      )
    ),
    
    mainPanel(
      plotlyOutput("dt_plot")
    )
  )
  )
  



ui <- navbarPage(
  "A5",
  introduction_page,
  page2,
  page3,
  page4
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  data <- reactive({
    req(input$sel_country)
    oil_df <- oil_co2_2021 %>% filter(country %in% input$sel_country) %>% group_by(country) %>% summarise(oil_co2 = sum(oil_co2))
  })
  
  observe({
    updateSelectInput(session, "sel_country", choices = oil_co2_2021$country)
  })
  
  output$plot <- renderPlot({
    g <- ggplot(data(), aes( y = oil_co2, x = country, color = country))
    g + geom_bar(stat = "sum")
  })
  oil_trends <- reactive({
    oil_co2_all %>% 
      filter(country %in% input$country,
             year %in% c(input$timeRange[1]:input$timeRange[2]))
  })
  
  output$oil_trends_plot <- renderPlotly({ggplotly(ggplot(oil_trends(), aes(x=year, y=oil_co2, color = country)) + 
                                                     geom_line() + 
                                                     ggtitle("Population Trends") +
                                                     labs(y = "Population Size in Millions") 
  ) 
  })
  
  oil_vs_pop <- reactive({ 
    oil_co2_vs_pop %>% 
      filter(country %in% input$country,
             year %in% c(input$time_range[1]:input$time_range[2]))
  })
  output$dt_plot <- renderPlotly({ggplotly(ggplot(oil_vs_pop()) +
                                             geom_point(mapping = aes(
                                               x = oil_co2, 
                                               y = population, 
                                               color = .data[[input$color_var]], 
                                               text = paste("Country:", country, "\nOil CO2 Per Capita: ", oil_co2_per_capita,"")
                                             ), alpha = .5) +
                                             scale_color_gradient(low = "gray", high = "red") +
                                             facet_wrap(~country) + 
                                             labs(
                                               title = "Oil CO2 vs Population",
                                               x = "Oil CO2",
                                               y = "Population"
                                             ) 
  )
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
