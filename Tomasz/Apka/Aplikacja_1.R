library(shiny)
library('plotly')
library('data.table')
source('~/PDU/Pliki/Project/helper.R')
lotniska <- fread('~/PDU/Pliki/Project/dataverse_files/airports.csv')

#           ZBADAĆ DLACZEGO SĄ RÓŻNICE W PRZYLOTACH-ODLOTACH

ui <- fluidPage(
  titlePanel(h1("Liczba przylotów i odlotów z danych lotnisk, z danych stanów")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("stan", h3("Wybierz stan:"),
                  choices = list("Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ",
                                 "Arkansas" = "AR","California" = "CA","Colorado" = "CO",
                                 "Connecticut" = "CT","Delaware" = "DE","Florida" = "FL",
                                 "Georgia" = "GA","Hawaii" = "HI","Idaho" = "ID",
                                 "Illinois" = "IL","Indiana" = "IN","Iowa" = "IA",
                                 "Kansas" = "KS","Kentucky" = "KY","Louisiana" = "LA",
                                 "Maine" = "ME","Maryland" = "MD","Massachusetts" = "MA",
                                 "Michigan" = "MI","Minnesota" = "MN","Mississippi" = "MS",
                                 "Missouri" = "MO","Montana" = "MT","Nebraska" = "NE",
                                 "Nevada" = "NV","New Hampshire" = "NH","New Jersey" = "NJ",
                                 "New Mexico" = "NM","New York" = "NY","North Carolina" = "NC",
                                 "North Dakota" = "ND","Ohio" = "OH","Oklahoma" = "OK",
                                 "Oregon" = "OR","Pennsylvania" = "PA","Rhode Island" = "RI",
                                 "South Carolina" = "SC","South Dakota" = "SD",
                                 "Tennessee" = "TN","Texas" = "TX","Utah" = "UT",
                                 "Vermont" = "VT","Virginia" = "VA","Washington" = "WA",
                                 "West Virginia" = "WV","Wisconsin" = "WI","Wyoming" = "WY"),
                  selected = "AK"),
      
      selectInput("rok", h3("Wybierz rok:"),
                  choices = as.list(1987:2008),
                  selected = 2007),
      
      dateRangeInput("dates", h3("Wybierz przedział dat:"),
                     start = as.Date("2007-01-01"),
                     end = as.Date("2007-12-31"),
                     min = as.Date("2007-01-01"),
                     max = as.Date("2007-12-31")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Odloty", plotlyOutput("odloty")),
        tabPanel("Przyloty", plotlyOutput("przyloty")),
        tabPanel("Tabela", tableOutput("dataTable"))
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$rok, {
    updateDateRangeInput(inputId = "dates", 
                         start = as.Date(paste(input$rok, "-01-01", sep = '')),
                         end = as.Date(paste(input$rok, "-12-31", sep = '')),
                         min = as.Date(paste(input$rok, "-01-01", sep = '')),
                         max = as.Date(paste(input$rok, "-12-31", sep = '')))
  })
  
  huj <- reactive({
    rok <- input$rok
    przyloty_odloty(paste("~/PDU/Pliki/Project/dataverse_files/", rok, ".csv.bz2", sep = ''))
  })
  
  daty <- reactive({
    ramka(input$dates[1], input$dates[2], huj())
  })
  
  stany <- reactive({
    kasowanie(daty(), input$stan)
  })
  
  output$odloty <- renderPlotly({
    data <- stany()
    wykres <- plot_ly(data, 
            x = ~Liczba_odlotow, 
            y = ~airport,
            type = 'bar', 
            orientation = 'h')
    wykres <- layout(wykres, xaxis = list(title = "Liczba odlotów"),
                     yaxis = list(title = "Lotniska"))
  })
  
  output$przyloty <- renderPlotly({
    data <- stany()
    wykres <- plot_ly(data, 
            x = ~Liczba_przylotow, 
            y = ~airport,
            type = 'bar', 
            orientation = 'h')
    wykres <- layout(wykres, xaxis = list(title = "Liczba przylotów"),
                     yaxis = list(title = "Lotniska"))
  })
  
  output$dataTable <- renderTable({
    stany()
  })
}

shinyApp(ui, server)