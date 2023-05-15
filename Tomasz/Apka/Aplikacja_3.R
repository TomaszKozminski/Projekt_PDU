library(shiny)
library('data.table')
library('plotly')
polaczenia <- fread("~/PDU/Pliki/Project/Apka/Ap_3/polaczenia_rama.csv")

#        DODAĆ MAPĘ CIEPLNĄ DANYCH LINII LOTNICZYCH W DANYCH STANACH I ICH UDZIAŁ

ui <- fluidPage(
  titlePanel("Popularność połączeń i linii lotniczych"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Linia", h3("Wybierz linię lotniczą"),
                  choices = as.list(unique(polaczenia$Description))),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Popularność", plotlyOutput("wykres_kol"))
      )
    )
  )
)

server <- function(input, output) {
  wycinanie <- reactive({
    data <- polaczenia[Description == input$Linia,]
    data <- data[order(data$ilosc_lotow, decreasing = TRUE), ]
    data <- head(data, 10)
    data
  })
  
  output$wykres_kol <- renderPlotly({
    wykres <- plot_ly(data = wycinanie(),
                      labels = ~polaczenie,
                      values = ~ilosc_lotow,
                      type = "pie")
    wykres <- layout(wykres, title = "Popularność połączeń")
  })
}

shinyApp(ui, server)