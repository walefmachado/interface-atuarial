# Teste Interface seca. 

library(shiny)
library(shinydashboard)
library(ggplot2)



# UI ----------------------------------------------------------------------

dados <- read.table('/home/walef/Dropbox/SECA Programação/tábuas_leitura_R.txt', h=T)

ui <- dashboardPage(
  dashboardHeader(title = "Seguros"),
  dashboardSidebar(
    
    
    selectInput("seg", "Selecione o seguro:",choices = c("Seguro Temporário" = 1 ,"Previdência" = 2,"Anuidade" = 3),multiple = F),
    selectInput("sex", "Sexo:",choices = c("Masculino" = 1 ,"Feminino" = 2), multiple = F),
    numericInput("ben", "Beneficio", min = 0, max = Inf, value = 1),
    numericInput("idade", "Idade", min = 0, max = (nrow(dados)-1), value = 0, step = 1),
    sliderInput("n", "Período",min = 0, max = (nrow(dados)-1), animate = F, value = 1, step = 1),
    sliderInput("tx", "Taxa de juros", min = 0, max = 1, value = 0.06, step = 0.001 ),
    
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

Premio_A <- function( i, idade, n, b, dados){ # i = taxa de juros, n = tempo, b = valor do beneficio
  dados <- read.table('/home/walef/Dropbox/SECA Programação/tábuas_leitura_R.txt', h=T)
  attach(dados)
  qx <- dados$AT_2000B_M_qx
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
      if(input$seg==1){
        a <- Premio_A(input$tx, round(input$idade, 0), input$n, input$ben, input$dados)
      }
      if(input$seg==2){
        a <- Premio_A(input$tx, input$idade, input$n, input$ben)}
      if(input$seg==3){
        a <- Premio_A(input$tx, input$idade, input$n, input$ben)
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
