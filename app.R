library(tidyverse)
library(httr)
library(readxl)
library(shiny)
library(forcats)
require(DT)

url1 <- "https://github.com/pedrow28/sementes-presentes/raw/master/Informa%C3%A7%C3%B5es%202018.xlsx"

GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))

info_18 <- read_excel(tf, 1L)

colunas <- names(info_18)


# Define UI for application that draws a histogram
ui <- fluidPage(

   #Input
    sidebarPanel(
      
      #Selecionar SRE
        (selectInput(inputId = "sre",
                     label = "SRE",
                     choices = unique(info_18$sre),
                     selected = "ALMENARA")
         ),
        
        
      #Selecionar variável X  
         (selectInput(inputId = "x",
                     label = "Eixo X:",
                     choices = colunas,
                     selected = "valor_comprado")
         ),
      
      #Selecionar variável Y  
      (selectInput(inputId = "y",
                   label = "Eixo Y:",
                   choices = colunas,
                   selected = "valor_comprado")
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput(outputId = "plot", brush = "plot_brush"),
      #Data table 
         dataTableOutput(outputId = "table")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Criar Data Frame para o Grafico
  sre_selected <- reactive({
    req(input$sre)
    info_18 %>% filter(sre %in% input$sre)
  })
   
  #Criar Grafico
  output$plot <- renderPlot({
    ggplot(data = sre_selected(), aes_string(x = input$x, y = input$y)) +
      geom_point(aes(col = municipio)) +
      theme_classic() +
      theme(axis.text.x = element_blank())
  })
  
  # Create data table
  output$table <- renderDataTable({
    brushedPoints(sre_selected(), brush = input$plot_brush) %>% 
      select(municipio, escola, n_produtos, valor_comprado, percentual_termo)
  })


}

# Run the application 
shinyApp(ui = ui, server = server)

