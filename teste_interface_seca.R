  # Teste Interface seca.
  # To do list:
  # Link para a página do outro grupo
  # Fórmulas em TEX
  # Saída em formato de Relatório
  # Comentar
  # Criar os arquivos de idiomas
  # Aplicar o Premio Nivelado
  # Alterar a forma de trabalhar com as tábuas dados$input$tab ou encontrar a tábua pelo nome.. OK


  library(shiny)
  library(shinydashboard)
  library(ggplot2)
  library(plotly)
  library(reshape2)
  
  
  # UI ----------------------------------------------------------------------
  
  local <- paste0(getwd(), '/tábuas_leitura_R.txt')
  dados <- read.table(local, h=T)
  notacao<<-"$$A_{x^{1}:\\bar{n}|}= \\displaystyle\\sum_{t=0}^{n-1}v^{t+1}  \\text{}_{t}p_{x}q_{x+t}$$"
  ui <- dashboardPage(
    dashboardHeader(title = "Halley"), #Cabeçalho
    dashboardSidebar( #Menu Lateral
      
      #Inputs condicionados às abas que se encontram no corpo do código
      conditionalPanel(condition = "input.abaselecionada==1",
                       selectInput("seg", "Selecione o seguro:",choices = c("Seguro Temporário" = 1 ,"Seguro Vitalício" = 2) ,multiple = F)),
      
      conditionalPanel(condition = "input.abaselecionada==2",
                       selectInput("anu", "Selecione o Produto:",choices = c("Anuidade Temporária" = 1, "Anuidade Vitalícia"=2) ,multiple = F)),
                                       
      conditionalPanel(condition = "input.abaselecionada==3",
                       selectInput("dot", "Selecione o Produto:",choices = c("Dotal Puro" = 1, "Dotal Misto" = 2) ,multiple = F)),
      
      conditionalPanel(condition = "(!((input.seg == 2 && input.abaselecionada== 1) || (input.abaselecionada==2 && input.anu == 2)))", numericInput("n", "Período", min = 0, max = (nrow(dados)-1), value = 1, step = 1)),
      checkboxInput(inputId = "diferido", label = "Diferido"),
      conditionalPanel(condition = "input.diferido",
                       numericInput("m", "Período de diferimento (m)", min = 0, max = (nrow(dados)-1), value = 1, step = 1)),
      #Inputs gerais, aparecem em todos os produtos
      selectInput("tab", "Selecione a tábua de vida", choices = c("AT 49" = "AT_49_qx", "AT 83" = "AT_83_qx", "AT 2000M" = "AT_2000B_M_qx", "AT 2000F" = "AT_2000B_F_qx")),
      
      # Se a tábua at2000 for selecionada então o individuo pode escolher o sexo do participante.
      # conditionalPanel(condition = "input.tab == 3", 
      #                  selectInput("sex", "Sexo:",choices = c("Masculino" = 1 ,"Feminino" = 2), multiple = F)),
      
      numericInput("idade", "Idade", min = 0, max = (nrow(dados)-1), value = 0, step = 1),
      numericInput("ben", "Beneficio ($)", min = 0, max = Inf, value = 1),
      numericInput("tx", "Taxa de juros", min = 0, max = 1, value = 0.06, step = 0.001 ),
      conditionalPanel(condition = "input.abaselecionada==666", # \m/
                       checkboxInput(inputId = "fecha", label = "fecha"))
      
      
    ),
    dashboardBody( #Corpo da página
      #Abas usadas para organizar a página por produtos e chamar a saída respectiva para o mesmo
      
      tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #0d0d0d;
                                font-family: "Times New Roman", Times, serif;
                                color: #039be5;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #FAFAFA;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #0d0d0d;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #212121;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #330033;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #00ff00;
                                color: #000000;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #FAFAFA;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #660066;
                                }
                                .content-wrapper{
                                background-color: #FAFAFA;
                                }
                                div#math{
                                font-size:24;
                                font-color: navy;
                                }
                                '))),



      
      
      
      tabsetPanel(type = "tab", 
                  tabPanel("Seguro de Vida", icon=icon("user"),verbatimTextOutput("segs"),value = 1),
                  tabPanel("Anuidade", icon=icon("cubes"),verbatimTextOutput("anuids"), value = 2),
                  tabPanel("Seguro Dotal", icon=icon("user-o"),verbatimTextOutput("dots"),value = 3),
                  
                  id = "abaselecionada"),
      uiOutput("math"),
      plotlyOutput("plot"), #Saída do gráfico definida pelo UI
      verbatimTextOutput("event") #Saída
    )
  )
  
  
  # Funões ------------------------------------------------------------------
  
  attach(dados)
  
  SV_Temp <- function( i, idade, n, b, qx, f.desconto){ # i = taxa de juros, n = tempo, b = valor do beneficio
    px <- 1-qx
    if(missing(f.desconto))
      f.desconto <- 1/(i+1)
    
    v <- f.desconto^(1:n)
    qxx <- c(qx[(idade+1):(idade+n)])
    pxx <- c(1, cumprod( px[(idade+1):(idade+n)]) )
    Ax <-  b* sum(v*pxx*qxx)
    return (Ax)
  }
  
  SV_Vit <- function(i, idade, b, qx, f.desconto){ # i = taxa de juros, n = tempo, b = valor do beneficio
    n <- max(Idade)-idade
    px <- 1-qx
    if(missing(f.desconto))
      f.desconto <- 1/(i+1)
    
    v <- f.desconto^(1:n)
    qxx <- c(qx[(idade+1):(idade+n)])
    pxx <- c(1, cumprod( px[(idade+1):(idade+n)]) )
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
  #Anuidade temporária
  #df: 0 para postecipado e 1 para antecipado
  Anuid <- function(i, idade, n , b, qx, df,f.desconto){ # i= taxa de juros, n= período, b = benefício
    px <- 1-qx
    if(missing(f.desconto))
      f.desconto <- 1/(i+1)
    
    v <- f.desconto^((1-df):(n-df))
    pxx <- c(1, cumprod( px[(idade+1):(idade+n)]) )
    ax <- (b* sum(v*pxx))
    return(ax)
  }
  
  
  
  #Anuidade vitalícia
  #df: 0 para postecipado e 1 para antecipado
  Anuidvit <- function(i, idade,b, qx, df, f.desconto){ # i= taxa de juros, n= período, b = benefício
    n <- max(Idade)-idade 
    px <- 1-qx
    if(missing(f.desconto))
      f.desconto <- 1/(i+1)
    
    v <- f.desconto^((1- df):(n-df))
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
  #Dotal Misto
  Dotal <- function(i, idade, n, b, qx){
    Ax<- (Dotal_Puro(i, idade, n, b, qx)+SV_Temp(i, idade, n, b, qx))
    return(Ax)
  }
  
  Diferido<- function(i, idade, qx, p, m){
    Dx<-p*Dotal_Puro(i, idade, m, 1, qx)
    return(Dx)
  }
  
  #Premio nivelado
  #df: 0 para postecipado e 1 para antecipado
  Premio_Niv <- function(i, idade, n, a, qx, df, fr){  #i=taxa, n=periodo de pagamento, usar um N especifico como input
    P<-((Anuid(i, idade, n , 1, qx, df))/a)*(fr^(-1))         #a é o Premio Puro Unico retornado de outro produto, qx=tabua
    return(P)                                  #fr é o fator de fracionamento do premio, input a ser criado****
  }
  

  #Função de seleção de tábua de vida, usa os inputs como parametros e retorna a tábua desejada
  tabSelect <- function(tab){ # tab=input$tab e sex=input$sex
    operador<-tab==colnames(dados[,])
    return(dados[,which(operador)])
    }
  
  
  
  # Cria colunas com a população de uma coorte hipotética para cada tábua de vida para utilização no gráfico
  dados$pop_at_49 <- (10000*cumprod(1 - dados$AT_49_qx))
  dados$pop_at_83 <- (10000*cumprod(1 - dados$AT_83_qx))
  dados$pop_at_2000_m <- (10000*cumprod(1 - dados$AT_2000B_M_qx))
  dados$pop_at_2000_f <- (10000*cumprod(1 - dados$AT_2000B_F_qx))
  
  dados_g <- data.frame(dados[,6:9])
  dados_g$Idade <- dados$Idade
  names(dados_g)
  dados_long <- melt(dados_g, id="Idade")
  colnames(dados_long)[colnames(dados_long)=="variable"] <- "Tábua"
  colnames(dados_long)[colnames(dados_long)=="value"] <- "População"
  
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
    
    #Saída caso a aba selecionada seja a de Seguros de Vida
    output$segs <- renderPrint({
      if((max(dados$Idade)-input$idade) >= input$n){
        qx<-tabSelect(input$tab)
        idade<-round(input$idade, 0)
        if (input$diferido)
          idade <- round(input$idade, 0)+input$m
        
        if(input$seg==1){
          a <- SV_Temp(input$tx, idade, input$n, input$ben, qx)
          b <- VAR(input$tx, round(input$idade, 0), input$n, input$ben, qx, input$seg)
        }
        if(input$seg==2){
          a <- SV_Vit(input$tx, idade, input$ben, qx)
          b <- VAR(input$tx, round(input$idade, 0), input$n, input$ben, qx, input$seg)
        }
        if (input$diferido)
          a<-Diferido(input$tx, input$idade, qx, a,input$m )
        cat('O prêmio puro único é:', a,
            '\nIdade: ', input$idade, 
            '\nPeríodo: ', input$n, 
            '\nBenefício: ', input$ben, 
            '\nTábua: ', input$tab, 
            '\nA variância do prêmio é:', b, 
            '\nO desvio padrão é:', round(sqrt(b), 2))
      }else{
        cat('O período temporário está errado')
      }
    })
    
    #Saída caso a aba selecionada seja a de Anuidades
    output$anuids = renderPrint({
      if((max(dados$Idade)-input$idade) >= input$n){
        qx<-tabSelect(input$tab)
        idade<-round(input$idade, 0)
        if (input$diferido)
          idade <- round(input$idade, 0)+input$m
        
        if(input$anu==1){
          a <- Anuid(input$tx, idade, input$n,  input$ben, qx, 0)
          # b <- round(VAR(input$tx, input$idade, input$n, input$ben, qx, input$anu), 2)
        }
  
        if(input$anu==2){
          a <- Anuidvit(input$tx, idade,  input$ben, qx, 0)
          
        }
        
        if (input$diferido)
          a<-Diferido(input$tx, input$idade, qx, a,input$m )
        
        cat('O prêmio puro único é:', a, 
            '\nTaxa de juros: ', input$tx, 
            '\nIdade: ', input$idade, 
            '\nBenefício', input$ben, 
            '\nTábua utilizada: ', input$tab ) 
      }else{
        cat('O período temporário está errado')
      }
    })
    
    #Saída caso a aba selecionada seja a dos Dotais
    output$dots = renderPrint({
      if((max(dados$Idade)-input$idade) >= input$n){
        qx<-tabSelect(input$tab)
        idade<-round(input$idade, 0)
        if (input$diferido)
          idade <- round(input$idade, 0)+input$m
        if(input$dot==1){
          a <- Dotal_Puro(input$tx, idade , input$n, input$ben, qx)
          nome<-"Dotal Puro"
        }
        if(input$dot==2){
          a <- Dotal(input$tx, idade, input$n, input$ben, qx)
          nome<-"Dotal Misto"
        }
        periodo<-input$n #Checar
        if (input$diferido)
          a<-Diferido(input$tx, input$idade, qx, a,input$m )
        cat('Produto:', nome, 
            '\nO prêmio puro único:', a, 
            '\nPeriodo(n):', periodo, 
            '\nTaxa de juros: ', input$tx, 
            '\nBenefício', input$ben, 
            '\nTábua: ', input$tab )
      }else{
        cat('O período temporário está errado')
      }
    })
    output$math <- renderUI(
      tags$a(href = "shiny.rstudio.com/tutorial", withMathJax(helpText(notacao)))  
      
    )
    
    #Saída de gráficos, no momento ainda não existe nenhuma condição para que apareça, apenas um modelo
    output$plot <- renderPlotly({
      ti <- "título"
      ggplot(data=dados_long,
             aes(x=Idade, y=População, colour= Tábua)) + geom_line() +
        scale_color_brewer(palette = "Dark2") + labs(title=ti, x='Anos', y='População')
    })
  
    output$event <- renderPrint({
      d <- event_data("plotly_hover")
      if (is.null(d)) "Passe o mouse sobre um ponto!" else d
    })
    
  
      
  }
  
  shinyApp(ui, server)
  
