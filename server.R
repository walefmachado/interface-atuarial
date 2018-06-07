# assign(dados)

shinyServer(function(input, output, session) {
    
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
            ntotal<-input$n
            if (input$diferido){
                idade <- round(input$idade, 0)+input$m
                ntotal <- input$n+input$m
            }
            if(input$seg==1){
                a <- SV_Temp(input$tx, idade, input$n, input$ben, qx)
                cobertura<- paste('\nCobertura(n):', input$n)
            }
            if(input$seg==2){
                a <- SV_Vit(input$tx, idade, input$ben, qx)
                cobertura <- ""
            }
            if (input$diferido)
                a<-Diferido(input$tx, input$idade, qx, a, input$m)
            
            if (input$premio==1){
                saidapremio <- paste('O prêmio puro único é:', a$Ax)
            }
            else if(input$premio==2){
                #0 indica ser antecipado, depois criar o input, o segundo ntotal é o fracionamento, depois criar o input
                aniv <- Premio_Niv(input$tx, input$idade, ntotal, a$Ax, qx, 1, ntotal)
                saidapremio <- paste('Prêmio nivelado:', aniv, '\nNúmero de parcelas: ', ntotal)
            }
            else if(input$premio==3){
                #0 indica ser antecipado, depois criar o input, o segundo ntotal é o fracionamento, depois criar o input
                aniv <- Premio_Niv(input$tx, input$idade, input$npremio, a$Ax, qx, 1, input$npremio)
                saidapremio <- paste('Prêmio nivelado:', aniv, '\nNúmero de parcelas: ', input$npremio)
            }
            cat(saidapremio,
                '\nIdade: ', input$idade,
                cobertura,
                '\nBenefício: ', input$ben,
                '\nTábua: ', input$tab,
                '\nIdade máxima da tábua:', min(which(qx==1)),
                '\nA variância do prêmio é:', a$Var,
                '\nO desvio padrão é:', round(sqrt(a$Var), 2))
        }else{
            cat('O período temporário está errado')
        }
    })

    #Saída caso a aba selecionada seja a de Anuidades
    output$anuids = renderPrint({
        if((max(dados$Idade)-input$idade) >= input$n){
            qx <- tabSelect(input$tab)
            idade <- round(input$idade, 0)
            ntotal <- input$n
            if (input$diferido){
                idade <- round(input$idade, 0)+input$m
                ntotal <- input$n+input$m
            }
            if(input$anu==1){
                a <- Anuid(input$tx, idade, input$n,  input$ben, qx, 1)
                cobertura<- paste('\nCobertura(n):', input$n)
            }
            if(input$anu==2){
                a <- Anuidvit(input$tx, idade,  input$ben, qx, 1)
                cobertura<- ""
            }
            if (input$diferido){
                a <- Diferido(input$tx, input$idade, qx, a,input$m )
            }
            if (input$premio==1){
                saidapremio<-paste('O prêmio puro único é:', a)
            }
            else if(input$premio==2){
                aniv <- Premio_Niv(input$tx, input$idade, ntotal, a, qx, 1, ntotal)#0 indica ser antecipado, depois criar o input, o segundo ntotal é o fracionamento, depois criar o input
                saidapremio <- paste('Prêmio nivelado:', aniv, '\nNúmero de parcelas: ', ntotal)
            }
            else if(input$premio==3){
                aniv <- Premio_Niv(input$tx, input$idade, input$npremio, a, qx, 1, input$npremio)#0 indica ser antecipado, depois criar o input, o segundo ntotal é o fracionamento, depois criar o input
                saidapremio <- paste('Prêmio nivelado:', aniv, '\nNúmero de parcelas: ', input$npremio)
            }
            cat(saidapremio,
                '\nTaxa de juros: ', input$tx,
                '\nIdade: ', input$idade,
                cobertura,
                '\nBenefício', input$ben,
                '\nTábua utilizada: ', input$tab,
                '\nIdade máxima da tábua:', min(which(qx==1))
                )
        }else{
            cat('O período temporário está errado')
        }
    })

    #Saída caso a aba selecionada seja a dos Dotais
    output$dots = renderPrint({
        if((max(dados$Idade)-input$idade) >= input$n){
            qx<-tabSelect(input$tab)
            idade<-round(input$idade, 0)
            ntotal <- input$n
            if (input$diferido){
                idade <- round(input$idade, 0)+input$m
                ntotal <- input$n+input$m
            }
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
            if (input$premio==1){
                saidapremio<-paste('O prêmio puro único é:', a$Ax)
            }
            else if(input$premio==2){
                aniv<-Premio_Niv(input$tx, input$idade, ntotal, a, qx, 1, ntotal)#0 indica ser antecipado, depois criar o input, o segundo ntotal é o fracionamento, depois criar o input
                saidapremio<-paste('Prêmio nivelado:', aniv, '\nNúmero de parcelas: ', ntotal)
            }
            else if(input$premio==3){
                aniv<-Premio_Niv(input$tx, input$idade, input$npremio, a$Ax, qx, 1, input$npremio)#0 indica ser antecipado, depois criar o input, o segundo ntotal é o fracionamento, depois criar o input
                saidapremio<-paste('Prêmio nivelado:', aniv, '\nNúmero de parcelas: ', input$npremio)
            }
            cat(saidapremio,
                '\nO prêmio puro único:', a$Ax,
                '\nCobertura(n):', periodo,
                '\nTaxa de juros: ', input$tx,
                '\nBenefício', input$ben,
                '\nTábua: ', input$tab,
                '\nIdade máxima da tábua:', min(which(qx==1)),
                '\nA variância do prêmio é:', a$Var,
                '\nO desvio padrão é:', round(sqrt(a$Var), 2))
        }else{
            cat('O período temporário está errado')
        }
    })

    output$not_seg_temp <- renderUI({
        if (input$diferido)
            tags$a(href = "https://lcaunifal.github.io/portalhalley/",
                   withMathJax(helpText("$$\\text{}_{m|}{}A_{x^{1}:\\overline{n}\\mid}= \\displaystyle\\sum_{t=m}^{(m+n)-1}v^{t+1}\\text{   }_{t}p_{x}q_{x+t}$$")))
        else
            tags$a(href = "https://lcaunifal.github.io/portalhalley/",
                   withMathJax(helpText("$$A_{x^{1}:\\overline{n}\\mid}= \\displaystyle\\sum_{t=0}^{n-1}v^{t+1}\\text{   }_{t}p_{x}q_{x+t}$$")))

    })
    output$not_seg_vit <- renderUI({
        if (input$diferido)
            tags$a(href = "https://lcaunifal.github.io/portalhalley/",
                   withMathJax(helpText("$$\\text{}_{m|}{}A_{x}= \\displaystyle\\sum_{t=m}^{\\infty} bv^{t+1}\\text{   }_{t}p_{x}q_{x+t}$$")))
        else
            tags$a(href = "https://lcaunifal.github.io/portalhalley/",
                   withMathJax(helpText("$$A_{x}= \\displaystyle\\sum_{t=0}^{\\infty} bv^{t+1}\\text{   }_{t}p_{x}q_{x+t}$$")))
    })
    output$not_seg_dot_p <- renderUI({
        if (input$diferido)
            tags$a(href = "https://lcaunifal.github.io/portalhalley/",
                   withMathJax(helpText("$$\\text{}_{m|}{}A_{x:\\overline{n}\\mid^1}= v^{n+m}\\text{ }_{n+m}p_{x}$$")))
        else
            tags$a(href = "https://lcaunifal.github.io/portalhalley/",
                   withMathJax(helpText("$$A_{x:\\overline{n}\\mid^1}= b v^{n}\\text{ }_{n}p_{x}$$")))
    })
    output$not_seg_dot_m <- renderUI({
        if (input$diferido)
            tags$a(href = "https://lcaunifal.github.io/portalhalley/",
                   withMathJax(helpText("$$\\text{}_{m|}{}A_{x:\\overline{n}\\mid}= \\text{}_{m|}{}A_{x^{1}:\\overline{n|}} + \\text{}_{m|}{}A_{x:\\overline{n|}}")))
        else
            tags$a(href = "https://lcaunifal.github.io/portalhalley/",
                   withMathJax(helpText("$$A_{x:\\overline{n}\\mid}=A_{x^{1}:\\overline{n}} + A_{x:\\overline{n}^1} $$")))
    })
    output$anu_vit <- renderUI({
        if (input$diferido)
            tags$a(href = "https://lcaunifal.github.io/portalhalley/",
                   withMathJax(helpText("$$\\text{}_{m|}{}\\ddot{a}_{x}= v^m\\text{   }_{m}p_{x}\\displaystyle\\sum_{t=0}^{\\infty} \\frac{1-v^{t+1}}{1-v}\\text{   }_{t}p_{x+m}q_{x+t+m}$$")))
        else
            tags$a(href = "https://lcaunifal.github.io/portalhalley/",
                   withMathJax(helpText("$$\\ddot{a}_{x}= \\displaystyle\\sum_{t=0}^{\\infty} \\frac{1-v^{t+1}}{1-v}\\text{   }_{t}p_{x}q_{x+t}$$")))
    })
    output$anu_temp <- renderUI({
        if (input$diferido)
            tags$a(href = "https://lcaunifal.github.io/portalhalley/",
                   withMathJax(helpText("$$\\text{}_{m|}{}\\ddot{a}_{x:\\overline{n}\\mid}= v^m\\text{   }_{m}p_{x} \\displaystyle\\sum_{t=0}^{n-1} v^t \\text{   }_{t}p_{x+m}$$")))
        else
            tags$a(href = "https://lcaunifal.github.io/portalhalley/",
                   withMathJax(helpText("$$\\ddot{a}_{x:\\overline{n}\\mid}= \\displaystyle\\sum_{t=0}^{n-1} v^t \\text{   }_{t}p_{x}$$")))
    })

    # Saída de gráficos, no momento ainda não existe nenhuma condição para que apareça, apenas um modelo
    # output$plot <- renderPlotly({
    #   ti <- "título"
    #   ggplot(data=dados_long,
    #          aes(x=Idade, y=População, colour= Tábua)) + geom_line() + theme(legend.position = "none") +
    #     scale_color_brewer(palette = "Dark2") + labs(title=ti, x='Anos', y='População')
    # })

    output$plot2 <- renderPlot({
        qx <- tabSelect(input$tab)
        Idade <- input$idade
        p_gra0 <- as.data.frame(p_gra(input$tx, idade, input$ben, qx)) # input$tx, idade, input$ben, qx
        ggplot(p_gra0) +
            geom_point(aes(x = idade, y = premio, colour = idade == Idade, size = idade == Idade)) +
            scale_colour_manual(values = c("black", "red")) +
            scale_size_manual(values =c(1, 3)) +
            geom_ribbon(data=p_gra0,aes(idade, ymin=(premio+0.1),ymax=(premio-0.1)),alpha=0.3) +
            theme(legend.position = "none") +
            labs(title="Prêmio por idade", x='Idade', y='Prêmio')
    })

    # output$event <- renderPrint({
    #   d <- event_data("plotly_hover")
    #   if (is.null(d)) "Passe o mouse sobre um ponto!" else d
    # })

})
