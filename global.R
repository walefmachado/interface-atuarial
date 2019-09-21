# Verifica Bibliotecas
if (!(is.element("pacman", installed.packages()[,1]))) {
    install.packages("pacman")
}

pacman::p_load(plotly, reshape2, shiny, shinydashboard)
#packrat::.snapshotImpl(".", snapshot.sources = FALSE) # cria arquivo packrat com todas dependências 


# Tabuas de Vida
dados <- read.table('data/tabuas_de_vida.txt', h=T)

attach(dados)


# Produtos ----------------------------------------------------------------

# i = taxa de juros, n = tempo, b = valor do beneficio, nvdf apenas para padronização
# seguro de vida temporario
SV_Temp <- function( i, idade, n, b, qx, nvdf=0) {
  px  <- 1-qx
  v   <- 1/(i+1)^(1:n)
  vp2 <- (((1/(i+1))^2)^(1:n))
  qxx <- c(qx[(idade+1):(idade+n)])
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  Ax  <-  b * sum(v*pxx*qxx)                # Primeiro momento multiplicado ao benefício 
  Ax2 <- b * sum(vp2*pxx*qxx)              # Segundo momento multiplicado ao benefício (usado para variância)
  Var <- (b*Ax2 - (Ax)^2)
  return(list(Ax=Ax, Ax2=Ax2, Var=Var))     # mudar a notação Ax da saida? A saida agora é uma lista!!! 
}

# i = taxa de juros, n = tempo, b = valor do beneficio, nvdf apenas para padronização
# Seguro de vida vitalício
SV_Vit <- function(i, idade, nv, b, qx, nvdf=0){ #nv=nevermind, só serve para padronizar a chamada dos produtos
  #n <- max(Idade)-idadeO
  n   <- min(which(qx==1))-idade
  px  <- 1-qx
  v   <- 1/(i+1)^(1:n)
  vp2 <- (((1/(i+1))^2)^(1:n)) 
  qxx <- c(qx[(idade+1):(idade+n)])
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  Ax  <- b * sum(v*pxx*qxx)
  Ax2 <- b * sum(vp2*pxx*qxx)
  Var <- (b*Ax2 - (Ax)^2)
  return(list(Ax=Ax, Ax2=Ax2, Var=Var))     # mudar a notação Ax da saida
} 

# Anuidade temporária
# df: 0 para postecipado e 1 para antecipado  < controlador >
# i= taxa de juros, n= período, b = benefício
Anuid <- function(i, idade, n , b, qx, df){ #para calcular a variancia... v2<-(1-(1/(1+i))^(1:n))/(1-(1/(1+i)))
  px <- 1-qx
  if(missing(df))
    df<-1
  else
    df<-if(df) 1 else 0
  
  v<-1/(i+1)
  # ((1-(v^(n+1)))/(1-v))-1
  
  vp <- (((1-(v^((1-df+1):(n-df+1))))/(1-v)))      # forma fechada das anuidades < série de termos>
  #vp2 <- ((((1-(v^((1-df+1):(n-df+1))))/(1-v))))^2
  
  if((n==1)&&(df==1)){
    Frac = sum(vp*pxx*qxx) - (1 - pxx[2] * vp)*(11/24) # prêmio fracionado mensal 
    return(list(Ax=1, Ax2= 1, Var=0, Frac = Frac))  #Corrigir variancia
  }
  if(df==1){
    vp2 <- vp^2
    qxx <- c(qx[(idade+1):(idade+n-1)], 1)
    pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]))
  }else{
    vp<-vp-1
    vp2<-vp^2
    pxx <- cumprod( px[(idade+1):(idade+n)])
    if (n==1){
      qxx<-1
    }else
      qxx <- c(qx[(idade+1):(idade+n-1)], 1)
  }
  ax <- (b* sum(vp*pxx*qxx))
  ax2 <- (b * sum(vp2*pxx*qxx))
  Var <- (b*ax2 - (ax)^2)
  Frac = sum(vp*pxx*qxx) - (1 - pxx[n] * vp[n])*(11/24) # prêmio fracionado mensal 
  return(list(Ax=ax, Ax2=ax2, Var=Var, Frac=Frac)) #Corrigir variancia
}


# Anuidade vitalícia 
Anuidvit <- function(i, idade, nv, b, qx, df){ #nv=nevermind só coloquei para padronizar a chamada de produtos
  #n <- max(Idade)-idade
  n<-min(which(qx==1))-idade
  px <- 1-qx
  if(missing(df))
    df<-1
  else
    df<-if(df) 1 else 0
  v<-1/(1+i)
  vp <- (((1-(v^((1-df+1):(n-df+1))))/(1-v)))
  # vp2 <- (((1-(v^((1-df+1):(n-df+1))))/(1-v)))^2
  #v2<-v^2
  #vp2 <- (((1-(v2^((1-df+1):(n-df+1))))/(1-v2)))
  if(df==1){
    vp2<-vp^2
    qxx <- c(qx[(idade+1):(idade+n)])
    pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]))
  }else{
    vp<-vp[-n]-1
    vp2<-vp^2
    pxx <- cumprod( px[(idade+1):(idade+n-1)])
    qxx <- c(qx[(idade+2):(idade+n)])
  }
  ax <- (b* sum(vp*pxx*qxx))
  ax2 <- (b * sum(vp2*pxx*qxx))
  Var <- (b*ax2 - (ax)^2)
  return(list(Ax=ax, Ax2=ax2, Var=Var)) #Corrigir variancia
}

#nvdf apenas para padronização
Dotal_Puro <- function(i, idade, n, b, qx, nvdf){
    px <- 1-qx
    v <- 1/(i+1)
    vp2 <- ((1/(i+1))^2)
    Ax <-  b*(v^n)*prod(px[(idade+1):(idade+n)])
    Ax2 <- b *(vp2^n)*prod(px[(idade+1):(idade+n)])
    Var <- (b*Ax2 - (Ax)^2)
    return(list(Ax=Ax, Ax2=Ax2, Var=Var))
}

# Dotal Misto , nvdf apenas para padronização
Dotal <- function(i, idade, n, b, qx, nvdf){
    Ax <- (((Dotal_Puro(i, idade, n, b, qx))$Ax)+(SV_Temp(i, idade, n, b, qx))$Ax)
    Ax2 <- (((Dotal_Puro(i, idade, n, b, qx))$Ax)+(SV_Temp(i, idade, n, b, qx))$Ax2)
    Var <- (b*Ax2 - (Ax)^2)
    return(list(Ax=Ax, Ax2=Ax2, Var=Var))
} 



# Funções para opções adicionais --------------------------------------



#Diferido<- function(i, idade, qx, p, m){  também checar df para anuidades postecipadas
Diferido<- function(PROD=Anuid, i, idade, n, b, qx, m, df){
    Dx<-(PROD(i, idade+m, n , b, qx, df))$Ax*(Dotal_Puro(i, idade, m, 1, qx, 0))$Ax
    return(list(Ax=Dx, Ax2=Dx, Var=0)) #Corrigir variancia
    #return(Dx)
}



#Premio nivelado
#df: 0 para postecipado e 1 para antecipado
Premio_Niv <- function(i, idade, n, a, qx, df, fr){  #i=taxa, n=periodo de pagamento, usar um N especifico como input  
    P<-(a/((Anuid(i, idade, n , 1, qx, df)$Ax)))*(fr^(-1))         #a é o Premio Puro Unico retornado de outro produto (mudar notação), qx=tabua
    return(P)                                  #fr é o fator de fracionamento do premio, input a ser criado****
}

# Anuid <- function(i, idade, n , b, qx, df)

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
LastSurv<-function(PROD=Anuid, i, idadex, idadez, n , b, qx, qz){ #Checar df será necessário para usar a função para anuidades postecipadas
  als<-PROD(i, idadex, n , b, qx, df)+PROD(i, idadez, n , b, qz, df)-PROD(i, 0, n, b, vidaConjunta(qx, qz, idadex, idadez), df)
  return(als)
}


# Informações extras ------------------------------------------------------



#Expectativa de vida
Exp_Vida <- function(idade, qx){ 
  n <- max(Idade)-idade
  px <- 1-qx
  t<- ((idade+1):max(Idade))
  qxx <- c(qx[(idade+1):(idade+n)])
  pxx <- c(1, cumprod( px[(idade+1):(idade+n-1)]) )
  E <-  sum(t*pxx*qxx)
  return(E-idade)
}

#Função generalizada para calcular a variancia dos produtos, primeiro estudar o caso das anuidades antes de aplicar as alterações na interface
VAR<-function(PROD=Anuid, i, idade, n , b, qx, df){ #Checar df será necessário para usar a função para anuidades postecipadas
  i2<-((i+1)^2)-1
  als<-abs((PROD(i2, idade, n , b, qx, df)$Ax-(PROD(i, idade, n , b, qx, df)$Ax^2)))
  return(als)
}


# Gráficos ----------------------------------------------------------------



# Cria colunas com a população de uma coorte hipotética para cada tábua de vida para utilização no gráfico
# {
#     dados[paste0(paste0("pop_",names(dados[,2:32])))] <- (10000*cumprod(1 - dados[,2:32]))
#     dados_g <- data.frame(dados[,33:63])
#     dados_g$Idade <- dados$Idade
#     dados_long <- melt(dados_g, id="Idade")
#     colnames(dados_long)[colnames(dados_long)=="variable"] <- "Tábua"
#     colnames(dados_long)[colnames(dados_long)=="value"] <- "População"
# }

# Cria um conjunto de dados com o prêmio por idade
p_gra <- function(i, idade, b, qx, FUN){
    p <- c()
    maxR<-c()
    minR<-c()
    id <- c()
    for(j in (0:min(which(qx==1)))){
        id[j+1] <- j
        aux<- (FUN(i, j, 0, b, qx, 0))
        p[j+1] <- aux$Ax
        maxR[j+1] <- p[j+1]+sqrt(aux$Var)
        minR[j+1] <- p[j+1]-sqrt(aux$Var)
    }
    return(list(premio=p, idade=id, maxR=maxR, minR=minR))
}


# Cria duas listas com VPA e VP financeiro para comparativo
fa_gra <- function(i, idade, fim, b, qx){ 
  vpa <- c()
  vp <- c()
  n <- c()
  for(t in (1:fim)){
    n[t] <- t
    vp[t] <- b*(1/(1+i)^t)
    vpa[t] <- (Dotal_Puro(i, idade, t, b, qx, 0))$Ax
  }
  return(list(tempo=n, financeiro=vp, atuarial=vpa))
}
#actuReport(Anuid, Anuidvit, input$anu, input$tab, input$tx, input$ben, input$idade, input$n, input$diferido, 
#input$m, input$premio, input$npremio)



# Relatório final e processamento -----------------------------------------



#actuReport(Anuid, Anuidvit, 1, 'AT.49_MALE', 0.06, 1, 0, 1, FALSE, 1, 1, 1, TRUE)
actuReport<-function(FUN1=SV_Temp, FUN2=SV_Vit, tipo=1, tab, tx, ben, idade, n=1, diferido, m=0, premio=1, npremio=1, df=T){
  if((max(dados$Idade)-idade) >= n){
    qx<-tabSelect(tab)
    ntotal<-n
    if (diferido){
      ntotal <- n+m
    }
    if(tipo==1){
      A <- FUN1
      cobertura<- paste('\nCobertura:', n)
    }
    if(tipo==2){
      A <- FUN2
      cobertura <- ""
    }
    if (diferido){ #(PROD=Anuid, i, idade, n, b, qx, m)
      a<-Diferido(A, tx, idade, n, ben, qx, m, df)
    }else{
      a<-A(tx, idade, n, ben, qx, df)  #preciso enviar input$df 
    }
    if (premio==1){
      saidapremio <- paste('Prêmio puro único imediato:', round(a$Ax, 4))
    }else if(premio==2){
      #1 indica ser antecipado, depois criar o input, o segundo ntotal é o fracionamento, depois criar o input
      aniv <- Premio_Niv(tx, idade, ntotal, a$Ax, qx, 1, ntotal)
      saidapremio <- paste('Prêmio nivelado:', round(aniv, 4), '\nNúmero de parcelas: ', ntotal)
    }else if(premio==3){
      #1 indica ser antecipado, depois criar o input, o segundo input$premio é o fracionamento, depois criar o input
      aniv <- Premio_Niv(tx, idade, npremio, a$Ax, qx, 1, npremio)
      saidapremio <- paste('Prêmio nivelado:', round(aniv, 4), '\nNúmero de parcelas: ', npremio)
    }else if (premio == 4){
      premio_frac <-  a$Ax / (12 * Anuid(tx, idade, n, ben, qx, T)$Frac)
      saidapremio <- paste('Prêmio puro periódico anual fracionado: ', round(premio_frac, 4))
      #saidapremio <- paste('Prêmio puro único imediato:', round(a$Ax, 4))
    }
    cat(saidapremio,
        '\nIdade: ', idade,
        cobertura,
        '\nTaxa de juros:', tx,
        '\nBenefício: ', ben,
        '\nTábua utilizada: ', tab,
        '\nIdade máxima da tábua:', min(which(qx==1)),
        '\nExpectativa de vida:', round(Exp_Vida(idade, qx), 0),
        '\nO desvio padrão do prêmio é:', round(sqrt(a$Var), 4))#,
        # '\nO desvio padrão é:', round(sqrt(a$Var), 2))
    
  }else{
    cat('O período temporário está errado')
  }
}


#Função de seleção de tábua de vida, usa os inputs como parametros e retorna a tábua desejada
tabSelect <- function(tab){ # tab=input$tab e sex=input$sex
  operador<-tab==colnames(dados[,])
  return(dados[,which(operador)])
}
