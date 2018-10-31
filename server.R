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

  #Saída aba relatório caso a aba selecionada seja a de Seguros de Vida
  output$segs <- renderPrint({
    actuReport(SV_Temp, SV_Vit, input$seg, input$tab, input$tx, input$ben, input$idade, input$n, input$diferido,
               input$m, input$premio, input$npremio, 0)
  })

  #Saída aba relatório caso a aba selecionada seja a de Anuidades
  output$anuids = renderPrint({
    actuReport(Anuid, Anuidvit, input$anu, input$tab, input$tx, input$ben, input$idade, input$n, input$diferido,
               input$m, input$premio, input$npremio, input$df)
  })

  #Saída aba relatório caso a aba selecionada seja a dos Dotais
  output$dots = renderPrint({
    actuReport(Dotal_Puro, Dotal, input$dot, input$tab, input$tx, input$ben, input$idade, input$n, input$diferido,
               input$m, input$premio, input$npremio, 0)

  })


# Notações ----------------------------------------------------------------

  
  
  output$not_seg_temp <- renderUI({
      if (input$diferido)
          withMathJax(helpText("$$\\text{}_{m|}{}A_{x^{1}:\\overline{n}\\mid}= \\displaystyle\\sum_{t=m}^{(m+n)-1}v^{t+1}\\text{   }_{t}p_{x}q_{x+t}$$"))
      else
          withMathJax(helpText("$$A_{x^{1}:\\overline{n}\\mid}= \\displaystyle\\sum_{t=0}^{n-1}v^{t+1}\\text{   }_{t}p_{x}q_{x+t}$$"))

  })
  output$not_seg_vit <- renderUI({
      if (input$diferido)
          withMathJax(helpText("$$\\text{}_{m|}{}A_{x}= \\displaystyle\\sum_{t=m}^{\\infty} v^{t+1}\\text{   }_{t}p_{x}q_{x+t}$$"))
      else
          withMathJax(helpText("$$A_{x}= \\displaystyle\\sum_{t=0}^{\\infty} v^{t+1}\\text{   }_{t}p_{x}q_{x+t}$$"))
  })
  output$not_seg_dot_p <- renderUI({
      if (input$diferido)
          withMathJax(helpText("$$\\text{}_{m|}{}A_{x:\\overline{n}\\mid^1}= v^{n+m}\\text{ }_{n+m}p_{x}$$"))
      else
          withMathJax(helpText("$$A_{x:\\overline{n}\\mid^1}= v^{n}\\text{ }_{n}p_{x}$$"))
  })
  output$not_seg_dot_m <- renderUI({
      if (input$diferido)
          withMathJax(helpText("$$\\text{}_{m|}{}A_{x:\\overline{n}\\mid}= \\text{}_{m|}{}A_{x^{1}:\\overline{n|}} + \\text{}_{m|}{}A_{x:\\overline{n|}}"))
      else
          withMathJax(helpText("$$A_{x:\\overline{n}\\mid}=A_{x^{1}:\\overline{n}} + A_{x:\\overline{n}^1} $$"))
  })
  output$anu_vitA <- renderUI({
      if (input$diferido)
          withMathJax(helpText("$$\\text{}_{m|}{}\\ddot{a}_{x}= v^m\\text{   }_{m}p_{x}\\displaystyle\\sum_{t=0}^{\\infty} \\frac{1-v^{t+1}}{1-v}\\text{   }_{t}p_{x+m}q_{x+t+m}$$"))
      else
          withMathJax(helpText("$$\\ddot{a}_{x}= \\displaystyle\\sum_{t=0}^{\\infty} \\frac{1-v^{t+1}}{1-v}\\text{   }_{t}p_{x}q_{x+t}$$"))
  })
  output$anu_tempA <- renderUI({
      if (input$diferido)
          tags$a(href = "https://walefmachado.github.io/portal-halley/",
                 withMathJax(helpText("$$\\text{}_{m|}{}\\ddot{a}_{x:\\overline{n}\\mid}= v^m\\text{   }_{m}p_{x} \\displaystyle\\sum_{t=0}^{n-1} v^t \\text{   }_{t}p_{x+m}$$")))
      else
          tags$a(href = "https://walefmachado.github.io/portal-halley/",
                 withMathJax(helpText("$$\\ddot{a}_{x:\\overline{n}\\mid}= \\displaystyle\\sum_{t=0}^{n-1} v^t \\text{   }_{t}p_{x}$$")))
  })
  output$anu_vitP <- renderUI({
    if (input$diferido)
      tags$a(href = "https://walefmachado.github.io/portal-halley/",
             withMathJax(helpText("$$\\text{}_{m|}{}{a}_{x}= v^m\\text{   }_{m}p_{x}\\displaystyle\\sum_{t=0}^{\\infty} \\frac{1-v^{t+1}}{1-v}\\text{   }_{t}p_{x+m}q_{x+t+m}$$")))
    else
      tags$a(href = "https://walefmachado.github.io/portal-halley/",
             withMathJax(helpText("$${a}_{x}= \\displaystyle\\sum_{t=0}^{\\infty} \\frac{1-v^{t+1}}{1-v}\\text{   }_{t}p_{x}q_{x+t}$$")))
  })
  output$anu_tempP <- renderUI({
    if (input$diferido)
      tags$a(href = "https://walefmachado.github.io/portal-halley/",
             withMathJax(helpText("$$\\text{}_{m|}{}{a}_{x:\\overline{n}\\mid}= v^m\\text{   }_{m}p_{x} \\displaystyle\\sum_{t=0}^{n-1} v^t \\text{   }_{t}p_{x+m}$$")))
    else
      tags$a(href = "https://walefmachado.github.io/portal-halley/",
             withMathJax(helpText("$${a}_{x:\\overline{n}\\mid}= \\displaystyle\\sum_{t=0}^{n-1} v^t \\text{   }_{t}p_{x}$$")))
  })

  # Saída de gráficos, no momento ainda não existe nenhuma condição para que apareça, apenas um modelo
  # output$plot <- renderPlotly({
  #   ti <- "título"
  #   ggplot(data=dados_long,
  #          aes(x=Idade, y=População, colour= Tábua)) + geom_line() + theme(legend.position = "none") +
  #     scale_color_brewer(palette = "Dark2") + labs(title=ti, x='Anos', y='População')
  # })

# Gráficos ----------------------------------------------------------------


  output$plot2 <- renderPlotly({
      qx <- tabSelect(input$tab)
      Idade <- input$idade
      p_gra0 <- as.data.frame(p_gra(input$tx, idade, input$ben, qx, SV_Vit)) # input$tx, idade, input$ben, qx
      g<-ggplot(p_gra0) +
        #geom_point(aes(x = idade, y = premio, colour = idade == Idade, size = idade == Idade)) +
        geom_point(aes(x = idade, y = premio, colour = idade == input$idade, size = idade == input$idade)) +
        scale_colour_manual(values = c("black", "red")) +
        scale_size_manual(values =c(1, 3)) +
        geom_ribbon(data=p_gra0,aes(idade, ymin=(minR),ymax=(maxR)),alpha=0.3) +
        theme(legend.position = "none") +
        labs(title="", x='Idade', y='Prêmio')
      ggplotly(g)
  })
  
  output$plot3 <- renderPlotly({
    qx <- tabSelect(input$tab)
    fa_gra0 <- as.data.frame(fa_gra(input$tx, input$idade, input$n, input$ben, qx)) 
    fa_gra0 <- melt(fa_gra0, id='tempo')
    g<-ggplot(fa_gra0) +
      geom_line(aes(x = tempo, y = value, colour=variable)) +
      #geom_line(aes(x = tempo, y = atuarial)) +
      #geom_line(aes(x = tempo, y = atuarial), color="red") +
      scale_colour_manual(values = c("black", "red")) +
      theme(legend.title = element_blank()) +
      labs(title="", x='Tempo', y='$')
    ggplotly(g)
  })

  output$plot4 <- renderPlotly({ #Plot para anuidade
    qx <- tabSelect(input$tab)
    Idade <- input$idade
    p_gra0 <- as.data.frame(p_gra(input$tx, idade, input$ben, qx, Anuidvit)) # input$tx, idade, input$ben, qx
    g<-ggplot(p_gra0) +
      geom_point(aes(x = idade, y = premio, colour = idade == Idade, size = idade == Idade)) +
      scale_colour_manual(values = c("black", "red")) +
      scale_size_manual(values =c(1, 3)) +
      geom_ribbon(data=p_gra0,aes(idade, ymin=(minR),ymax=(maxR)),alpha=0.3) +
      theme(legend.position = "none") +
      labs(title="", x='Idade', y='Prêmio')
    ggplotly(g)
  })
  # output$event <- renderPrint({
  #   d <- event_data("plotly_hover")
  #   if (is.null(d)) "Passe o mouse sobre um ponto!" else d
  # })

})
