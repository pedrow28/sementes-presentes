
library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(DT)
library(plotly)





# DADOS -------------------------------------------------------------------

#Importar remoto



#Dados no computador
dados <- read_excel("Data App 2 - SP.xlsx")


# UI ----------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("readable"),
  
  #Titulo app
  titlePanel("Dados Sementes Presentes 2018", windowTitle = "Dados SP 18"),
  
  #Layout
  sidebarLayout(
    
    #Painel lateral
    sidebarPanel(
      #Escolha de input
      checkboxGroupInput(inputId = "sre",
                  label = "Escolha SRE",
                  choices = unique(dados$SRE),
                  selected = "TODAS"),#fim input
      
    "Os dados abaixo controlam as informações disponibilizadas nas abas.", br(),
    br(),
      
      #dados para o grafico - Valores
      checkboxGroupInput(inputId = "dados_plot",
                         label = "Escolha os valores para visualizar",
                         choices = c("Valor dos editais",
                                     "Valor comprado",
                                     "Valor frustado"),
                         selected = "Valor dos editais"), #Fim dados grafico
    
      #dados para o grafico - Quantidades
      checkboxGroupInput(inputId = "dados_plot1",
                         label = "Escolha as informações sobre quantidade para visualizar",
                         choices = c("Quantidade prevista nos editais",
                                     "Quantidade comprada",
                                     "Quantidade frustrada"),
                         selected = "Quantidade prevista nos editais") #Fim dados grafico
      
      
    ), #Fim painel lateral
    
    #mainPanel
    mainPanel(
      #Abas
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Dados consolidados",
                           h3("Dados Gerais"),
                           
                           "O projeto Sementes Presentes em 2018 esteve presente em ", tags$b(n_distinct(dados$SRE)), " SREs distintas, sendo elas:", br(),
                           br(),
                                  tags$li("Almenara"),
                                  tags$li("Diamantina"),
                                  tags$li("Governador Valadares"),
                                  tags$li("Januária"),
                                  tags$li("Montes Claros"),
                           br(),

                           "Selecione a SRE no menu ao lado para informações específicas de cada Regional. As informações consolidadas são apresentados no texto abaixo,
                           Além disso, as abas acima contêm informações mais detalhadas para a SRE selecionada.", br(),
                           br(),
                           
                           "Importante ressaltar que as informações apresentadas neste painel dizem respeito aos dados enviados
                           para os Núcleos de Planejamento das SRE's pelas escolas no âmbito do Projeto Sementes Presentes.",
                           
                           h3(textOutput(outputId = "title_sre")),
                           htmlOutput(outputId = "text_sre")), #Fim painel 1
                  tabPanel(title = "Dados visuais",
                           h3(textOutput(outputId = "title_sre1")),
                           h4("Valores"),
                           "A linha pontilhada no gráfico representa 30% do total de recursos recebidos por repasse do governo federal.",
                           plotOutput(outputId = "plot1", height = 600, width = 600),
                           h4("Quantidades"),
                           plotOutput(outputId = "plot2", height = 900, width = 650)), #Fim painel 2
                  tabPanel(title = "Visualização das escolas",
                           h3(textOutput(outputId = "title_sre2")),
                           "O gráfico abaixo distribui as escolas pelo percentual comprado com AGF, passe o mouse pelos pontos
                           para maiores informações.", br(),
                           br(),
                           "A linha contínua marca aquelas escolas que não apresentaram nenhum valor de compra com a Agricultura Familiar,
                           os pontos acima da linha pontilhada indicam compras acima de 30% do valor total do termo.",
                           "Clicando e arrastando é possível ampliar a imagem do gráfico na área selecionada para melhor visualização,
                           para retornar ao tamanho original selecione 'Reset axes' no menu na parte superior do gráfico.",
                          plotlyOutput(outputId = "plot3")),
                  tabPanel(title = "Tabela",
                           "Utilize a tabela abaixo para pesquisar e obter maior detalhamento das informações. Selecione o número de linhas
                           a serem mostradas no item 'Show X entries'",
                           dataTableOutput(outputId = "dt"))
      )#Fim abas
    )#Fim Main panel
  )#Fim layout
)#Fim fluidPage

# SERVER ------------------------------------------------------------------


server <- function(input, output) {
  
  #Nome SRE para o UI

  
  nome_sre <- reactive({
    
    
    if (length(input$sre) == length(unique(dados$SRE))) {
      "TODAS"
    }
      else if (length(input$sre) > 2) {
      x <- paste(input$sre[-length(input$sre)], collapse = ', ')
      paste0(x, " e ", input$sre[length(input$sre)])
    } else if (length(input$sre) == 2) {
      x <- paste(input$sre[-length(input$sre)], collapse = ', ')
      paste0(x, " e ", input$sre[length(input$sre)])
    } else {
      input$sre
    }

  })
  
  output$title_sre <- renderText({nome_sre()})
  #Fim nome SRE
  
  #Nome SRE1 para o UI
  output$title_sre1 <- renderText({nome_sre()}) #Fim nome SRE1
  
  #Nome SRE2 para visualizacao
  output$title_sre2 <- renderText({nome_sre()})#Fim nome SRE2
  
  #Gerar BD reactive
  
  sre_selected <- reactive({
    req(input$sre)

      dados %>% filter(SRE %in% input$sre)

  }) #Fim reactive
  
  #Texto SRE Especifica
  output$text_sre <- renderUI({
    HTML(paste0("Total de ", tags$b(n_distinct(sre_selected()$Escola)),
               " escolas de ", tags$b(n_distinct(sre_selected()$Municipio)), " municípios distintos.", br(),
               br(),
               "Essas escolas compraram o total de ", tags$b("R$ "), tags$b(format((sum(sre_selected()$`Valor comprado`)), nsmall = 2, big.mark = ".", decimal.mark = ",")),
               " dos ", tags$b("R$ "), tags$b(format(sum(sre_selected()$`Valor dos editais`), nsmall = 2, big.mark = ".", decimal.mark = ",")),
               " inicialmente previstos nos editais.", br(),
               br(),
               "A quantidade total de alimentos demandada em edital foi de ",
               tags$b(format((sum(sre_selected()$`Quantidade prevista nos editais`)), nsmall = 2, big.mark = ".", decimal.mark = ",")), " KG.",
               " Sendo que desta demanda ",
               tags$b(format((sum(sre_selected()$`Quantidade comprada`)), nsmall = 2, big.mark = ".", decimal.mark = ",")),
               " foi atendida pelos fornecedores.", br(),
               br(),
               "Considerando que os repasses para as escolas da SRE, participantes do projeto, advindos do governo federal por meio do FNDE totalizam", tags$b(" R$ "),
               tags$b(format((sum(sre_selected()$`Valor do termo`)), nsmall = 2, big.mark = ".", decimal.mark = ",")), ", as compras realizadas representam ",
               tags$b((round((sum(sre_selected()$`Valor comprado`) / sum(sre_selected()$`Valor do termo`)), digits = 4)) * 100),
               tags$b(" %"),
               " em relação ao total de recursos recebidos."
               ))#Fim HTML
  })#Fim texto SRE
  
  #Grafico valores
  output$plot1 <- renderPlot({
    req(input$dados_plot)
  data1 <- sre_selected() %>% gather(info, valor, 5:11) %>% 
                     filter(info %in% input$dados_plot) %>%
                     group_by(SRE, info) %>% summarise(valor = sum(valor))
  
  linhas <- data <- sre_selected() %>% group_by(SRE) %>% 
                                       summarise(valor = sum(`Valor do termo`)) %>% 
                                       mutate(hlines = valor * 0.3)
  
                     ggplot(data1, (aes(y = valor, x = SRE, fill = info))) +
                       geom_col(position = position_dodge(width = 7), col = "black") +
                       geom_text(aes(label = paste0("RS ",format(valor, nsmall = 2, big.mark = ".", decimal.mark = ",")),
                                     vjust = -0.3), position = position_dodge(width = 10), fontface = "bold") +
                       geom_hline(data = linhas, aes(yintercept = hlines), linetype = "dashed", color = "black", alpha = 0.3) +
                       scale_y_continuous(label = scales::dollar) +
                       scale_y_log10() +
                       scale_fill_discrete(name = "Informações") +
                       facet_grid(SRE ~ ., switch = "y") +
                       theme_bw() +
                       theme(axis.title = element_blank(),
                             axis.text.x = element_blank(),
                             axis.text.y = element_blank(),
                             axis.ticks = element_blank(),
                             panel.grid.major = element_blank(),
                             strip.text.y = element_text(angle = 180))
    
    



    
  })#Fim plot1
  
  #Grafico Quantidades
  output$plot2 <- renderPlot({
    req(input$dados_plot1)
    sre_selected() %>% gather(info, valor, 5:11) %>% 
      filter(info %in% input$dados_plot1) %>%
      group_by(SRE, info) %>% summarise(valor = sum(valor)) %>% 
      ggplot(aes(y = valor, x = SRE, fill = info)) +
      geom_col(position = position_dodge(width = 7), col = "black") +
      geom_text(aes(label = (format(valor, nsmall = 2, big.mark = ".", decimal.mark = ",")),
                    vjust = -0.1), position = position_dodge(width = 10), fontface = "bold") +
      scale_fill_discrete(name = "Informações") +
      scale_y_continuous(limits = c(0, 600000)) +
      scale_y_log10() +
      facet_grid(SRE ~ ., switch = "y") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_blank(),
            panel.grid.major = element_blank(),
            strip.text.y = element_text(angle = 180),
            axis.ticks = element_blank())
    
    
    
    
    
    
  })#Fim plot1
  
  #Gerar tabela para aba

  
  output$dt <- renderDataTable({
    datatable(sre_selected() %>% select(SRE, Escola, Municipio, input$dados_plot, input$dados_plot1), options = list(pageLength = 5,
                                           lengthMenu = c(5, 10, 15, 20)))
  })
  
  #Fim tabela
  
  
  # Gerar plot 3
  

  output$plot3 <- renderPlotly({
    
    ggplotly({
     ggplot(sre_selected(), aes(x = Escola, y = `Percentual do termo comprado com AGF (%)`, col = SRE)) +
        geom_point(alpha = 0.5) +
        scale_y_continuous(label = scales::percent, breaks = seq(0, 2, by = 0.1)) +
        labs(x = "Escolas") +
        geom_hline(yintercept = 0.3, linetype = "dashed", color = "black", alpha = 0.7) +
        geom_hline(yintercept = 0, color = "black") +
        theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              panel.grid = element_blank())
       
    })

      
  })#Fim plot 3
  

} #Fim funcao server




# RUN APP -----------------------------------------------------------------



shinyApp(ui = ui, server = server)

