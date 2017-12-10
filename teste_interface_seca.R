# Teste Interface seca. 

library(shiny)
library(shinydashboard)
library(ggplot2)



# UI ----------------------------------------------------------------------

dados <- read.table('/home/walef/Dropbox/SECA Programação/tábuas_leitura_R.txt', h=T)

ui <- dashboardPage(
  dashboardHeader(title = "Seguros"),
  dashboardSidebar(
    
    
    selectInput("seg", "Selecione o seguro:",choices = c("Seguro Temporário" = 1 ,"Seguro Vitalício" = 2,"Anuidade" = 3),multiple = F),
    selectInput("tab", "Selecione a tábua de vida", choices = c("AT 49" = 1, "AT 83" = 2, "AT 20000" = 3)),
    selectInput("sex", "Sexo:",choices = c("Masculino" = 1 ,"Feminino" = 2), multiple = F),
    numericInput("ben", "Beneficio", min = 0, max = Inf, value = 1),
    numericInput("idade", "Idade", min = 0, max = (nrow(dados)-1), value = 0, step = 1),
    numericInput("n", "Período",min = 0, max = (nrow(dados)-1), value = 1, step = 1),
    numericInput("tx", "Taxa de juros", min = 0, max = 1, value = 0.06, step = 0.001 ),
    
    #Se a Seguro vitalício for selecionada...
    conditionalPanel(condition = "input.dist==1",numericInput("Período","Taxa de juros",value = 0),
                     numericInput("sd","Idade",value = 1,min = 0)),
    
    #Se a Uniforme for selecionada...
    conditionalPanel(condition = "input.dist==2",numericInput("a","a",value = 0),
                     numericInput("b","b",value = 1)),
    
    #Se a Exponencial for selecionada...
    conditionalPanel(condition = "input.dist==3",numericInput("lambda","Lambda",value = 0.5,min=0))
    
    
    
    
  ),dashboardBody(
    
    verbatimTextOutput("hist"))
)


# Funções -----------------------------------------------------------------
dados <- read.table('/home/walef/Dropbox/SECA Programação/tábuas_leitura_R.txt', h=T)
attach(dados)
Premio_A <- function( i, idade, n, b, qx){ # i = taxa de juros, n = tempo, b = valor do beneficio
  px <- 1-qx
  f.desconto <- 1/(i+1)
  v <- f.desconto^(1:n)
  qxx <- c(qx[(idade+1):(idade+n)])
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  Ax <- b* sum(v*pxx*qxx)
  Ax <- round(Ax, 2)
  return (Ax)
}

Premio_V <- function( i, idade, b, qx){ # i = taxa de juros, n = tempo, b = valor do beneficio
  n <- max(Idade)-25 
  px <- 1-qx
  f.desconto <- 1/(i+1)
  v <- f.desconto^(1:n)
  qxx <- c(qx[(idade+1):(idade+n)])
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  Ax <- b* sum(v*pxx*qxx)
  Ax <- round(Ax, 2)
  return (Ax)
}



# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  output$hist <- renderPrint({
    if((max(dados$Idade)-input$idade) >= input$n){
      if(input$tab==1){
        qx <- dados$AT_49_qx
      }
      if(input$tab==2){
        qx <- dados$AT_83_qx
      }
      if(input$tab==3){
        if(input$sex==1){
          qx <- dados$AT_2000B_M_qx
        }
        if(input$sex==2){
          qx <- dados$AT_2000B_F_qx
        }
      }
      if(input$seg==1){
        a <- Premio_A(input$tx, round(input$idade, 0), input$n, input$ben, qx)
      }
      if(input$seg==2){
        a <- Premio_V(input$tx, input$idade, input$ben, qx)}
      if(input$seg==3){
        a <- Premio_A(input$tx, input$idade, input$n, input$ben, qx)
      }
      cat('O valor do seu prêmio é:', a)
    }else{
      cat('O período temporário está errado')
    } 
  })
  
  
  # output$grafico <- renderPlot({
  #   if(input$seg==1){
  #     a <- Premio_A(input$tx, input$idade, input$n, input$ben)
  #   }
  #   if(input$seg==2){
  #     a <- Premio_A(input$tx, input$idade, input$n, input$ben)}
  #   if(input$seg==3){
  #     a <- Premio_A(input$tx, input$idade, input$n, input$ben)
  #   }
  #   a
  # })
  
  
}

shinyApp(ui, server)
