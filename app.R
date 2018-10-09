
# INICIAL -----------------------------------------------------------------



library(tidyverse)
library(httr)
library(readxl)
library(shiny)
library(forcats)
require(DT)
library(rsconnect)



url1 <- "https://github.com/pedrow28/sementes-presentes/raw/master/Informa%C3%A7%C3%B5es%202018.xlsx"

GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))

info_18 <- read_excel(tf, 1L)

# info_18 <- read_excel("Informações 2018.xlsx")
# 
# colunas <- names(info_18)


# UI ----------------------------------------------------------------------


#Abrinda interface do usuario

ui <- fluidPage(
  
  #Titulo Aplicativo
  titlePanel("Dados Sementes Presentes 2018", windowTitle = "Dados SP 18"),
  
  
  #Layout
  sidebarLayout(
    
    #Painel lateral
    sidebarPanel(
      
      #Escolher SRE
      selectInput(inputId = "sre",
                  label = "SRE",
                  choices = unique(info_18$sre),
                  selected = "ALMENARA")
      
      
    ),#Fim sidebarPanel
    
    #Painel com info
    mainPanel(
      #Criar abas
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Dados Gerais",
                           h3("Dados"),
                           htmlOutput(outputId = "info"),
                           h3("Base de dados"),
                           dataTableOutput(outputId = "datatable")),
                  tabPanel(title = "Graficos",
                           plotOutput(outputId = "plot"))
                  
      )#Fim tabsetPanel
    )#Fim mainPanel
  )#Fim sidebarLayout
)#Fim fluidPage


# SERVER ------------------------------------------------------------------


#Criando servidor

server <- function(input, output) {
  
  #Data para ir para o painel(sre_selected)
  sre_selected <- reactive({
    req(input$sre)
    info_18 %>% filter(sre %in% input$sre) %>% 
      select(3, 2, 5:9)
  })#Fim sre_selected
  
  
  #Gerar data table
  output$datatable <- renderDataTable({
    sre_selected()})#Fim data table
  
  #Gerar plot1
  output$plot <- renderPlot({
    ggplot(data = sre_selected(), aes(x = reorder(escola, percentual_termo), y = percentual_termo, col = municipio)) +
      geom_point() +
      labs(title = "Percentual comprado com AGF por Escola",
           x = "Escolas da SRE",
           y = "Percentual comprado com a AGF") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1.5, by = 0.1)) +
      scale_color_discrete(name = "Municipios") +
      geom_hline(yintercept = 0.3, linetype = "dashed", color = "red") +
      theme_classic() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
    
  })#fim plot 1
  
  
  #Gerar informações gerais
  output$info <- renderUI({
    HTML(paste0("A Regional de ",
                tags$em(input$sre),
                " teve um total de ",
                tags$b(n_distinct(sre_selected()$escola)),
                " escolas, distribuidas em ",
                tags$b(n_distinct(sre_selected()$municipio)),
                " cidades diferentes, envolvidas no projeto, Adquiriu o total de ", 
                tags$b("R$ "),
                tags$b(round(sum(sre_selected()$valor_comprado, na.rm = TRUE), digits = 2)),
                " com produtos da agricultura familiar.", br(),
                br(),
                "Considerando que a soma de todos os termos das escolas da regional totalizam " ,
                tags$b("R$ "),
                tags$b(round(sum(sre_selected()$valor_do_termo, na.rm = TRUE), digits = 2)),
                ", as escolas participantes da SRE atingiram ",
                tags$b((round(sum(sre_selected()$valor_comprado) / sum(sre_selected()$valor_do_termo, na.rm = TRUE), digits = 4) * 100)),
                tags$b(" %"),
                " em compras com a Agricultura Familiar comparado ao total repassado por meio de recursos federais em 2018."))
  })#Fim info2
  
}#Fim func servidor


# APP ---------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
