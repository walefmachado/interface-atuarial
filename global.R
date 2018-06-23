# Verifica Bibliotecas
if (!(is.element("pacman", installed.packages()[,1]))) {
    install.packages("pacman")
}

pacman::p_load(plotly, reshape2, shiny, shinydashboard)
#packrat::.snapshotImpl(".", snapshot.sources = FALSE) # cria arquivo packrat com todas dependências 

# Bibliotecas
library(plotly)
library(reshape2)


# Tabuas de Vida
dados <- read.table('data/tabuas_de_vida.txt', h=T)
teste <- read.delim('data/teste.csv',
                    stringsAsFactors = FALSE, header=TRUE)
attach(dados)


# i = taxa de juros, n = tempo, b = valor do beneficio
SV_Temp <- function( i, idade, n, b, qx) {
  px <- 1-qx
  v <- 1/(i+1)^(1:n)
  vp2 <- (((1/(i+1))^2)^(1:n))
  qxx <- c(qx[(idade+1):(idade+n)])
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  Ax <-  b * sum(v*pxx*qxx)
  Ax2 <- b * sum(vp2*pxx*qxx)
  Var <- (Ax2 - (Ax)^2)* b
  list(Ax=Ax, Ax2=Ax2, Var=Var)     # mudar a notação Ax da saida? A saida agora é uma lista!!! 
}

# i = taxa de juros, n = tempo, b = valor do beneficio
SV_Vit <- function(i, idade, nv, b, qx){ #nv=nevermind, só serve para padronizar a chamada dos produtos
    n <- max(Idade)-idade
    px <- 1-qx
    v <- 1/(i+1)^(1:n)
    vp2 <- (((1/(i+1))^2)^(1:n)) 
    qxx <- c(qx[(idade+1):(idade+n)])
    pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
    Ax <-  b * sum(v*pxx*qxx)
    Ax2 <- b * sum(vp2*pxx*qxx)
    Var <- (Ax2 - (Ax)^2)* b
    list(Ax=Ax, Ax2=Ax2, Var=Var)     # mudar a notação Ax da saida
}

# Verificar o cáculo Pedro
# Anuidade temporária
# df: 0 para postecipado e 1 para antecipado
# i= taxa de juros, n= período, b = benefício
Anuid <- function(i, idade, n , b, qx, df){ #para calcular a variancia... v2<-(1-(1/(1+i))^(1:n))/(1-(1/(1+i)))
    px <- 1-qx
    if(missing(df))
        df<-1

    v <- (1/(1+i))^((1-df):(n-df))
    if(df==1)
        pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
    else
        pxx <- cumprod( px[(idade+1):(idade+n)])
    ax <- (b* sum(v*pxx))
    return(ax)
}

# Anuidade vitalícia
# df: 0 para postecipado e 1 para antecipado
# i= taxa de juros, n= período, b = benefício
Anuidvit <- function(i, idade, nv, b, qx, df){ #nv=nevermind só coloquei para padronizar a chamada de produtos
    n <- max(Idade)-idade
    px <- 1-qx
    if(missing(df))
      df<-1

    v <- (1/(1+i))^((1- df):(n-df))
    if(df==1)
        pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
    else
        pxx <- cumprod( px[(idade+1):(idade+n)])
    ax <- (b* sum(v*pxx))
    return(ax)
}

Dotal_Puro <- function(i, idade, n, b, qx){
    px <- 1-qx
    v <- 1/(i+1)
    vp2 <- ((1/(i+1))^2)
    Ax <-  b*(v^n)*cumprod(px[(idade+1):(idade+n)])[n]
    Ax2 <- b*(vp2^n)*cumprod(px[(idade+1):(idade+n)])[n]
    Var <- (Ax2 - (Ax)^2)* b
    list(Ax=Ax, Ax2=Ax2, Var=Var)
}

# Dotal Misto
Dotal <- function(i, idade, n, b, qx){
    Ax <- (((Dotal_Puro(i, idade, n, b, qx))$Ax)+(SV_Temp(i, idade, n, b, qx))$Ax)
    Ax2 <- (((Dotal_Puro(i, idade, n, b, qx))$Ax)+(SV_Temp(i, idade, n, b, qx))$Ax)
    Var <- (Ax2 - (Ax)^2)* b
    list(Ax=Ax, Ax2=Ax2, Var=Var)
}
#Diferido<- function(i, idade, qx, p, m){  também checar df para anuidades postecipadas
Diferido<- function(PROD=Anuid, i, idade, n, b, qx, m){
    Dx<-PROD(i, idade+m, n , b, qx)*(Dotal_Puro(i, idade, m, 1, qx))$Ax
    return(Dx)
}


#Premio nivelado
#df: 0 para postecipado e 1 para antecipado
Premio_Niv <- function(i, idade, n, a, qx, df, fr){  #i=taxa, n=periodo de pagamento, usar um N especifico como input  
    P<-(a/((Anuid(i, idade, n , 1, qx, df))))*(fr^(-1))         #a é o Premio Puro Unico retornado de outro produto (mudar notação), qx=tabua
    return(P)                                  #fr é o fator de fracionamento do premio, input a ser criado****
}

# Vida conjunta
vidaConjunta<-function(qx1, qx2, idade1, idade2){
  if (idade1>idade2){
    fim<-116-(idade1-idade2)
    return(1-((1-qx1[idade1:116])*(1-qx2[idade2:fim])))
  }
  else{
    fim<-116-(idade2-idade1)
    return(1-((1-qx1[idade1:fim])*(1-qx2[idade2:116])))
  }
  
}

#Ultimo Sobrevivente
LastSurvivor<-function(PROD=Anuid, i, idadex, idadez, n , b, qx, qz){ #Checar df será necessário para usar a função para anuidades postecipadas
  als<-PROD(i, idadex, n , b, qx)+PROD(i, idadez, n , b, qz)-PROD(i, 0, n, b, vidaConjunta(qx, qz, idadex, idadez))
  return(als)
}

#Função generalizada para calcular a variancia dos produtos, primeiro estudar o caso das anuidades antes de aplicar as alterações na interface
VAR<-function(PROD=Anuid, i, idade, n , b, qx){ #Checar df será necessário para usar a função para anuidades postecipadas
  i2<-((i+1)^2)-1
  als<-PROD(i2, idadex, n , b, qx)-(PROD(i, idade, n , b, qx)^2)
  return(als)
}

#Função de seleção de tábua de vida, usa os inputs como parametros e retorna a tábua desejada
tabSelect <- function(tab){ # tab=input$tab e sex=input$sex
    operador<-tab==colnames(dados[,])
    return(dados[,which(operador)])
}

# Cria colunas com a população de uma coorte hipotética para cada tábua de vida para utilização no gráfico
{
    dados[paste0(paste0("pop_",names(dados[,2:32])))] <- (10000*cumprod(1 - dados[,2:32]))
    dados_g <- data.frame(dados[,33:63])
    dados_g$Idade <- dados$Idade
    dados_long <- melt(dados_g, id="Idade")
    colnames(dados_long)[colnames(dados_long)=="variable"] <- "Tábua"
    colnames(dados_long)[colnames(dados_long)=="value"] <- "População"
}

# Cria um conjunto de dados com o prêmio por idade
p_gra <- function(i, idade, b, qx){
    p <- c()
    id <- c()
    for(j in (0:length(dados$Idade))){
        id[j] <- j
        p[j] <- (SV_Vit(i, j, 0, b, qx))$Ax
    }
    list(premio=p, idade=id)
}


# Cria duas listas com VPA e VP financeiro para comparativo
fa_gra <- function(i, idade, fim, b, qx){ 
  vpa <- c()
  vp <- c()
  n <- c()
  for(t in (1:fim)){
    n[t] <- t
    vp[t] <- b*(1/(1+i)^t)
    vpa[t] <- (Dotal_Puro(i, idade, t, b, qx))$Ax
  }
  list(tempo=n, financeiro=vp, atuarial=vpa)
}


