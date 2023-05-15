library('ggplot2')
library('data.table')
library('plotly')
file_names <- list.files("~/PDU/Pliki/Project/Apka/Ap_2", pattern = ".csv", full.names = TRUE)
lista_anul <- lapply(file_names, fread)
anul_lacznie <- fread("~/PDU/Pliki/Project/Apka/Ap_2/anul/anul_lacznie.csv")
anul_data <- fread("~/PDU/Pliki/Project/Apka/Ap_2/anul/anul_data.csv")


ui <- fluidPage(
  titlePanel(h1("Opóźnienia lotów")),
  
  sidebarLayout(
    sidebarPanel(
      p(h5("Ta aplikacja pokazuje ile lotów zostało anulowanych przez dane linie lotnicze.")),
      textInput("rok", "Podaj rok z przedziału: 1987-2008:", 2008),
      br(),
      checkboxInput("ogolnie", "Pokaż wykres z wszystkich lat", value = FALSE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Udział", plotlyOutput("wykres_kol")),
        tabPanel("Wykres", plotOutput("wykres_lin"))
      )
    )
  )
)

server <- function(input, output){
  
  
  output$wykres_kol <- renderPlotly({
    if (input$ogolnie){ 
      plot_ly(
      data = anul_lacznie,
      labels = ~Description,
      values = ~Liczba_anul,
      type = 'pie'
    )}
    else {rok <- switch(input$rok,
                       "1987" = lista_anul[[1]],
                       "1988" = lista_anul[[2]],
                       "1989" = lista_anul[[3]],
                       "1990" = lista_anul[[4]],
                       "1991" = lista_anul[[5]],
                       "1992" = lista_anul[[6]],
                       "1993" = lista_anul[[7]],
                       "1994" = lista_anul[[8]],
                       "1995" = lista_anul[[9]],
                       "1996" = lista_anul[[10]],
                       "1997" = lista_anul[[11]],
                       "1998" = lista_anul[[12]],
                       "1999" = lista_anul[[13]],
                       "2000" = lista_anul[[14]],
                       "2001" = lista_anul[[15]],
                       "2002" = lista_anul[[16]],
                       "2003" = lista_anul[[17]],
                       "2004" = lista_anul[[18]],
                       "2005" = lista_anul[[19]],
                       "2006" = lista_anul[[20]],
                       "2007" = lista_anul[[21]],
                       "2008" = lista_anul[[22]])
    
    #             ŚREDNIA WAŻONA BO PRZECIEŻ NIEKTÓRE LINIE SĄ WAŻNIEJSZE
    
    plot_ly(
      data = rok,
      labels = ~Description,
      values = ~N_anul_lotow,
      type = 'pie'
    )}
  })
  #       ZMIENIĆ TEN WYKRES NA BARDZIEJ REAKTYWNY
  output$wykres_lin <- renderPlot({
    ggplot(anul_data) +
      geom_smooth(mapping = aes(x = data, y = N_anul_lotow, color = Description), se = FALSE)
  })
    
}

shinyApp(ui, server)