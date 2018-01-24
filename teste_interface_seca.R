# Teste Interface seca. Teste
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)


# UI ----------------------------------------------------------------------

local <- paste0(getwd(), '/tábuas_leitura_R.txt')
dados <- read.table(local, h=T)
# dados <- read.table('/home/walef/Dropbox/SECA Programação/tábuas_leitura_R.txt', h=T)
ui <- dashboardPage(
  dashboardHeader(title = "Cálculo Atuarial"),
  dashboardSidebar(
    

    conditionalPanel(condition = "input.abaselecionada==1",
                     selectInput("seg", "Selecione o seguro:",choices = c("Seguro Temporário" = 1 ,"Seguro Vitalício" = 2) ,multiple = F),
                     conditionalPanel(condition = "input.seg != 2", numericInput("n", "Período", min = 0, max = (nrow(dados)-1), value = 1, step = 1))),

    conditionalPanel(condition = "input.abaselecionada==2",
                     selectInput("anu", "Selecione o Produto:",choices = c("Anuidade Temporária" = 1, "Anuidade Vitalícia"=2) ,multiple = F),
                     conditionalPanel(condition = "input.anu != 2", numericInput("n", "Período", min = 0, max = (nrow(dados)-1), value = 1, step = 1))),

    conditionalPanel(condition = "input.abaselecionada==3",
                     selectInput("dot", "Selecione o Produto:",choices = c("Dotal Puro" = 1, "Dotal Misto" = 2) ,multiple = F),
                     numericInput("n", "Período", min = 0, max = (nrow(dados)-1), value = 1, step = 1)),

    selectInput("tab", "Selecione a tábua de vida", choices = c("AT 49" = 1, "AT 83" = 2, "AT 2000" = 3)),
    # Se a tábua at2000 for selecionada então o individuo pode escolher o sexo do participante.
    
    conditionalPanel(condition = "input.tab == 3", 
                     selectInput("sex", "Sexo:",choices = c("Masculino" = 1 ,"Feminino" = 2), multiple = F)),
    
    numericInput("idade", "Idade", min = 0, max = (nrow(dados)-1), value = 0, step = 1),
    numericInput("ben", "Beneficio ($)", min = 0, max = Inf, value = 1),
    numericInput("tx", "Taxa de juros", min = 0, max = 1, value = 0.06, step = 0.001 ),
    conditionalPanel(condition = "input.abaselecionada==666", # \m/
                     checkboxInput(inputId = "fecha", label = "fecha"))
   
    #numericInput("fecha", "fecha", min = 0, max = 1, value = 0)
  ),
  dashboardBody(
    tabsetPanel(type = "tab",
                tabPanel("Seguro de Vida", icon=icon("user"),verbatimTextOutput("segs"),value = 1),
                #tabPanel("Seguro Vitalício",verbatimTextOutput("vit"),value = 2),
                tabPanel("Anuidade", icon=icon("cubes"),verbatimTextOutput("anuids"), value = 2),
                tabPanel("Seguro Dotal", icon=icon("user-o"),verbatimTextOutput("dots"),value = 3),
                #tabPanel("Dotal Misto",tableOutput("dotm"),value = 5)
                id = "abaselecionada"),
    #plotOutput("gratabua")
    plotlyOutput("plot"),
    verbatimTextOutput("event")
    )
)


# Funções -----------------------------------------------------------------
#dados <- read.table('/home/walef/Dropbox/SECA Programação/tábuas_leitura_R.txt', h=T)
#dados <- read.table('C:/Users/Yagho Note/interface-atuarial/tábuas_leitura_R.txt', h=T)

attach(dados)

SV_Temp <- function( i, idade, n, b, qx, f.desconto){ # i = taxa de juros, n = tempo, b = valor do beneficio
  px <- 1-qx
  if(missing(f.desconto)){
    f.desconto <- 1/(i+1)
  }
  v <- f.desconto^(1:n)
  qxx <- c(qx[(idade+1):(idade+n)])
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  Ax <-  b* sum(v*pxx*qxx)
  return (Ax)
}

SV_Vit <- function(i, idade, b, qx, f.desconto){ # i = taxa de juros, n = tempo, b = valor do beneficio
  n <- max(Idade)-idade 
  px <- 1-qx
  if(missing(f.desconto)){
    f.desconto <- 1/(i+1)
  }
  v <- f.desconto^(1:n)
  qxx <- c(qx[(idade+1):(idade+n)])
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  Ax <-  b* sum(v*pxx*qxx)
  Ax <- round(Ax, 2)
  return (Ax)
}

VAR <- function(i, idade, n, b, qx, se){ # i = taxa de juros, n = tempo, b = valor do beneficio
  be <- 1
  if(se==1){
    Ax <- SV_Temp(i, idade, n, be, qx)
    Ax2 <- SV_Temp(i, idade, n, be, qx, ((1/(i+1))^2))
  }
  if(se==2){
    Ax <- SV_Vit(i, idade, be, qx)
    Ax2 <- SV_Vit(i, idade, be, qx, ((1/(i+1))^2))
  }
  # if(se==3){
  #   Ax <- Anuid(i, idade, n , be, qx)
  #   Ax2 <- Anuid(i, idade, n , be, qx, ((1/(i+1))^2))
  # }
  Var <- (Ax2 - (Ax)^2)* b
  return (Var)
}

# Verificar o cáculo Pedro
Anuid <- function(i, idade, n , b, qx, f.desconto){ # i= taxa de juros, n= período, b = benefício
  px <- 1-qx
  if(missing(f.desconto)){
    f.desconto <- 1/(i+1)
  }
  v <- f.desconto^(1:n)
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  ax <- (b* sum(v*pxx))
  return(ax)
}

Dotal_Puro <- function(i, idade, n, b, qx){
  px <- 1-qx
  v <- 1/(i+1)
  Ax <-  b*(v^n)*cumprod(px[(idade+1):(idade+n)])[n]
  return(Ax)
}

Dotal <- function(i, idade, n, b, qx){
  Ax<- (Dotal_Puro(i, idade, n, b, qx)+SV_Temp(i, idade, n, b, qx))
  return(Ax)
}

tabSelect <- function(tab, sex){
  if(tab==1)
    return(dados$AT_49_qx)
  if(tab==2)
    return(dados$AT_83_qx)
  if(tab==3){
    if(sex==1)
      return(dados$AT_2000B_M_qx)
    if(sex==2)
      return(dados$AT_2000B_F_qx)
  }
}

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  observe({  #Função usa dados da URL pra mudar parametros dentro do código ex: 127.0.0.1:6510/?abaselecionada=1&seg=2
    query <- parseQueryString(session$clientData$url_search)
    
    for (i in 1:(length(reactiveValuesToList(input)))) {
      nameval = names(reactiveValuesToList(input)[i])
      valuetoupdate = query[[nameval]]
      
      if ((!is.null(query[[nameval]]))&(!input$fecha)) {
        if (nameval=="abaselecionada"){
          updateTabsetPanel(session, "abaselecionada",
                            selected = valuetoupdate)
        }
        if (is.na(as.numeric(valuetoupdate))) {
          updateTextInput(session, nameval, value = valuetoupdate)
        }
        else {
          updateTextInput(session, nameval, value = as.numeric(valuetoupdate))
        }
      }
      
    }
  })
  
  output$segs <- renderPrint({
    if((max(dados$Idade)-input$idade) >= input$n){
      qx<-tabSelect(input$tab, input$sex)
      if(input$seg==1){
        a <- round(SV_Temp(input$tx, round(input$idade, 0), input$n, input$ben, qx), 2)
        b <- round(VAR(input$tx, round(input$idade, 0), input$n, input$ben, qx, input$seg), 2)
      }
      if(input$seg==2){
        a <- round(SV_Vit(input$tx, input$idade, input$ben, qx), 2)
        b <- round(VAR(input$tx, round(input$idade, 0), input$n, input$ben, qx, input$seg), 2)  
      }
      cat('O prêmio puro único é:', a, '\nA variância do prêmio é:', b, '\nO desvio padrão é:', round(sqrt(b), 2))
    }else{
      cat('O período temporário está errado')
    }
  })
  
  output$anuids = renderPrint({
    if((max(dados$Idade)-input$idade) >= input$n){
      qx<-tabSelect(input$tab, input$sex)
      if(input$anu==1){
        a <- round(Anuid(input$tx, input$idade, input$n,  input$ben, qx), 2)
        # b <- round(VAR(input$tx, input$idade, input$n, input$ben, qx, input$anu), 2)
      }
      # if(input$anu==2){                                                                        
      #   a <- Dotal_Puro(input$tx, input$idade , input$n, input$ben, qx)
      # }
      # if(input$anu==3){
      #   a <- Dotal(input$tx, input$idade, input$n, input$ben, qx)
      # }
      cat('O prêmio puro único é:', a) 
    }else{
      cat('O período temporário está errado')
    }
  })
  
  output$dots = renderPrint({
    if((max(dados$Idade)-input$idade) >= input$n){
      qx<-tabSelect(input$tab, input$sex)
      if(input$dot==1){
        a <- Dotal_Puro(input$tx, input$idade , input$n, input$ben, qx)
      }
      if(input$dot==2){
        a <- Dotal(input$tx, input$idade, input$n, input$ben, qx)
      }
      cat('O prêmio puro único é:', a)
    }else{
      cat('O período temporário está errado')
    }
  })
  

  output$plot <- renderPlotly({
    ti <- "título"
    qx<-tabSelect(input$tab, input$sex)
    ggplot(dados, aes(Idade, (10000*cumprod(1 -qx)))) + geom_line(size = 1)  +
      scale_color_brewer(palette = "Dark2") + labs(title=ti, x='Anos', y='População')
  })

  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Passe o mouse sobre um ponto!" else d
  })

}

shinyApp(ui, server)
    

