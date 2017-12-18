# Teste Interface seca. Teste

library(shiny)
library(shinydashboard)
library(ggplot2)



# UI ----------------------------------------------------------------------

local <- paste0(getwd(), '/tábuas_leitura_R.txt')
dados <- read.table(local, h=T)
# dados <- read.table('/home/walef/Dropbox/SECA Programação/tábuas_leitura_R.txt', h=T)

ui <- dashboardPage(
  dashboardHeader(title = "Cálculo Atuarial"),
  dashboardSidebar(
  
    
    conditionalPanel(condition = "input.abaselecionada==1", 
                     selectInput("seg", "Selecione o seguro:",choices = c("Seguro Temporário" = 1 ,"Seguro Vitalício" = 2) ,multiple = F),
                     # Se a tábua at2000 for selecionada então o individuo pode escolher o sexo do participante.
                     selectInput("tab", "Selecione a tábua de vida", choices = c("AT 49" = 1, "AT 83" = 2, "AT 2000" = 3)),
                     conditionalPanel(condition = "input.tab == 3", selectInput("sex", "Sexo:",choices = c("Masculino" = 1 ,"Feminino" = 2), multiple = F)),
                     numericInput("idade", "Idade", min = 0, max = (nrow(dados)-1), value = 0, step = 1),
                     numericInput("ben", "Beneficio ($)", min = 0, max = Inf, value = 1),
                     conditionalPanel(condition = "input.seg != 2", numericInput("n", "Período", min = 0, max = (nrow(dados)-1), value = 1, step = 1)),
                     numericInput("tx", "Taxa de juros", min = 0, max = 1, value = 0.06, step = 0.001 )),
    conditionalPanel(condition = "input.abaselecionada==2", 
                     selectInput("anu", "Selecione o Produto:",choices = c("Anuidade" = 1, "Dotal Puro" = 2, "Dotal Misto" = 3) ,multiple = F),
                     # Se a tábua at2000 for selecionada então o individuo pode escolher o sexo do participante.
                     selectInput("tab", "Selecione a tábua de vida", choices = c("AT 49" = 1, "AT 83" = 2, "AT 2000" = 3)),
                     conditionalPanel(condition = "input.tab == 3", selectInput("sex", "Sexo:", choices = c("Masculino" = 1 ,"Feminino" = 2), multiple = F)),
                     numericInput("idade", "Idade", min = 0, max = (nrow(dados)-1), value = 0, step = 1),
                     numericInput("ben", "Beneficio ($)", min = 0, max = Inf, value = 1),
                     numericInput("tx", "Taxa de juros", min = 0, max = 1, value = 0.06, step = 0.001 ))
  ),
  dashboardBody(
    tabsetPanel(type = "tab",
                tabPanel("Seguros",column(width = 6,verbatimTextOutput("segs")),value = 1),
                #tabPanel("Seguro Vitalício",verbatimTextOutput("vit"),value = 2),
                tabPanel("Anuidade e Dotal",verbatimTextOutput("anuids"), value = 2),
                #tabPanel("Dotal Puro",verbatimTextOutput("dotp"),value = 4),
                #tabPanel("Dotal Misto",tableOutput("dotm"),value = 5)
                id = "abaselecionada"),
    plotOutput("gratabua")
  )
)

# Funções -----------------------------------------------------------------
#dados <- read.table('/home/walef/Dropbox/SECA Programação/tábuas_leitura_R.txt', h=T)
#dados <- read.table('C:/Users/Yagho Note/interface-atuarial/tábuas_leitura_R.txt', h=T)

attach(dados)
SV_Temp <- function( i, idade, n, b, qx){ # i = taxa de juros, n = tempo, b = valor do beneficio
  px <- 1-qx
  f.desconto <- 1/(i+1)
  v <- f.desconto^(1:n)
  qxx <- c(qx[(idade+1):(idade+n)])
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  Ax <-  b* sum(v*pxx*qxx)
  Ax <- round(Ax, 2)
  return (Ax)
}

SV_Vit <- function( i, idade, b, qx){ # i = taxa de juros, n = tempo, b = valor do beneficio
  n <- max(Idade)-idade 
  px <- 1-qx
  f.desconto <- 1/(i+1)
  v <- f.desconto^(1:n)
  qxx <- c(qx[(idade+1):(idade+n)])
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  Ax <-  b* sum(v*pxx*qxx)
  Ax <- round(Ax, 2)
  return (Ax)
}

# Verificar o cáculo Pedro
Anuid = function( i, idade, n , b, qx){ # i= taxa de juros, n= período, b = benefício

  px <- 1-qx
  f.desconto <- 1/(i+1)
  v <- f.desconto^(1:n)
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  ax <- round((b* sum(v*pxx)),2)
  return(ax)
}

Dotal_Puro<-function(i, idade, n, b, qx){
  px <- 1-qx
  v <- 1/(i+1)
  Ax <-  b*(v^n)*cumprod(px[(idade+1):(idade+n)])[n]
  return(Ax)
}
Dotal<-function(i, idade, n, b, qx){
  Ax<- (Dotal_Puro(i, idade, n, b, qx)+SV_Temp(i, idade, n, b, qx))
  return(Ax)
}




# Server ------------------------------------------------------------------

server <- function(input, output) {

  output$segs <- renderPrint({
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
        a <- SV_Temp(input$tx, round(input$idade, 0), input$n, input$ben, qx)
      }
      if(input$seg==2){
        a <- SV_Vit(input$tx, input$idade, input$ben, qx)
      }
      cat('O prêmio puro único é:', a)
    }else{
      cat('O período temporário está errado')
    }
  })
  
  output$anuids = renderPlot({
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
    if(input$anu==1){
      a <- Anuid(input$tx, input$idade, input$n,  input$ben, qx)
    }
    if(input$anu==2){
      a <- Dotal_Puro(input$tx, input$idade , input$n, input$ben, qx)
    }
    if(input$anu==3){
      a <- Dotal(input$tx, input$idade, input$n, input$ben, qx)
    }
    cat('O prêmio puro único é:', a)
  }else{
    cat('O período temporário está errado')
  }
})
  

  output$gratabua = renderPlot({
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
    }
    plot(10000*cumprod(1 -qx), type="l", color="green", width="20", xlab="Anos", ylab="População")
  })
}

shinyApp(ui, server)
