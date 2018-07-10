library(shiny)
library(shinydashboard)


# Teste Interface seca.
# To do list:
# Criar os arquivos de idiomas
# Conferir calculo dos produtos, checar se as posições do px e qx que estão sendo utilizadas são as corretas
# Conferir as notações dos dotais e das anuidades diferidas
# Colocar as notações estatísticas ex:  E[Zt]= Sum(Zt*P(T=t))
# Criar um botão de help, para tornar a ferramenta mais independente
# Separar os arquivos


dashboardPage(
    dashboardHeader(title = "Portal Halley"), #Cabeçalho
    dashboardSidebar( #Menu Lateral

        #Inputs condicionados às abas que se encontram no corpo do código
        conditionalPanel(condition = "input.abaselecionada==1",
                         selectInput("seg", "Selecione o seguro:",choices = c("Seguro Temporário" = 1 ,"Seguro Vitalício" = 2) ,multiple = F)),

        conditionalPanel(condition = "input.abaselecionada==2",
                         selectInput("anu", "Selecione o Produto:",choices = c("Anuidade Temporária" = 1, "Anuidade Vitalícia"=2) ,multiple = F)),

        conditionalPanel(condition = "input.abaselecionada==3",
                         selectInput("dot", "Selecione o Produto:",choices = c("Dotal Puro" = 1, "Dotal Misto" = 2) ,multiple = F)),

        conditionalPanel(condition = "(!((input.seg == 2 && input.abaselecionada== 1) || (input.abaselecionada==2 && input.anu == 2)))", numericInput("n", "Cobertura", min = 0, max = (nrow(dados)-1), value = 1, step = 1)),
        checkboxInput(inputId = "diferido", label = "Diferido"),
        conditionalPanel(condition = "input.diferido",
                         numericInput("m", "Período de diferimento (m)", min = 0, max = (nrow(dados)-1), value = 1, step = 1)),
        #Inputs gerais, aparecem em todos os produtos
        selectInput("tab", "Selecione a tábua de vida", choices = c("AT 49M" = "AT.49_MALE", "AT 49F" = "AT.49_FEMALE", "IBGE 2006" = "IBGE_2006",
                                                                    "IBGE 2007" = "IBGE_2007", "IBGE 2008" = "IBGE_2008", "IBGE 2009" = "IBGE_2009",
                                                                    "AT 83F" = "AT.83_FEMALE_IAM", "AT 83M" = "AT.83_MALE_IAM"  ,"AT 2000M" = "AT.2000_MALE",
                                                                    "AT 2000F" = "AT.2000_FEMALE")),
        # Se a tábua at2000 for selecionada então o individuo pode escolher o sexo do participante.
        # conditionalPanel(condition = "input.tab == 3",
        #                  selectInput("sex", "Sexo:",choices = c("Masculino" = 1 ,"Feminino" = 2), multiple = F)),

        numericInput("idade", "Idade", min = 0, max = (nrow(dados)-1), value = 0, step = 1),
        numericInput("ben", "Beneficio ($)", min = 0, max = Inf, value = 1),
        numericInput("tx", "Taxa de juros", min = 0, max = 1, value = 0.06, step = 0.001 ),
        # radioButtons(inputId = "premio", label = "Prêmio", choices= c("Puro Único"=1, "Nivelado pela duração do produto"=2, "Nivelado Personalizado"=3)),
        # conditionalPanel(condition = "input.premio==3",
        #                  numericInput("npremio", "Periodo de pagamento", min = 0, max = (nrow(dados)-1), value = 1, step = 1)),
        conditionalPanel(condition = "input.abaselecionada==666", # \m/
                         checkboxInput(inputId = "fecha", label = "fecha"))


    ),

    dashboardBody( #Corpo da página
        #Abas usadas para organizar a página por produtos e chamar a saída respectiva para o mesmo

        tags$head(tags$link(rel = "stylesheet",
                       type = "text/css",
                       href = "styles.css")),

        fluidRow(
          tabsetPanel(type = "tab",
                      tabPanel("Seguro de Vida", icon=icon("user"),
                               box(
                                   title = "Relatório", status = "primary", #solidHeader = TRUE,
                                   collapsible = TRUE,
                                   verbatimTextOutput("segs")),value = 1
                               ),
                      tabPanel("Anuidade", icon=icon("cubes"),
                               box(
                                   title = "Relatório", status = "primary", #solidHeader = TRUE,
                                   collapsible = TRUE,
                                   verbatimTextOutput("anuids")), value = 2
                               ),
                      tabPanel("Seguro Dotal", icon=icon("user-o"),
                               box(
                                   title = "Relatório", status = "primary", #solidHeader = TRUE,
                                   collapsible = TRUE,
                                   verbatimTextOutput("dots")), value = 3
                               ),
                      id = "abaselecionada"),
          box(
            
            title = "Fórmula de calculo", status = "primary", collapsible = TRUE,
            conditionalPanel(condition = "input.abaselecionada==1 && input.seg==1",
                             uiOutput("not_seg_temp")),
            conditionalPanel(condition = "input.abaselecionada==1 && input.seg==2",
                             uiOutput("not_seg_vit")),
            conditionalPanel(condition = "input.abaselecionada==2 && input.anu==1",
                             uiOutput("anu_temp")),
            conditionalPanel(condition = "input.abaselecionada==2 && input.anu==2",
                             uiOutput("anu_vit")),
            conditionalPanel(condition = "input.abaselecionada==3 && input.dot==1",
                             uiOutput("not_seg_dot_p")),
            conditionalPanel(condition = "input.abaselecionada==3 && input.dot==2",
                             uiOutput("not_seg_dot_m")),
            "x = Idade do segurado", br(),
            "n = Período", br(),
            "m = Período de diferimento"
          )
        ),

        fluidRow(
            box(
              radioButtons(inputId = "premio", label = "Prêmio", choices= c("Puro Único"=1, "Nivelado pela duração do produto"=2, "Nivelado Personalizado"=3)),
              conditionalPanel(condition = "input.premio==3",
                               numericInput("npremio", "Periodo de pagamento", min = 0, max = (nrow(dados)-1), value = 1, step = 1))
            ),
            # box(
            #   title = "Tábuas de Vida", status = "primary", #solidHeader = TRUE,
            #   collapsible = TRUE,
            #   plotlyOutput("plot"),
            #   verbatimTextOutput("event") #Saída
            #   #box(plotlyOutput("plot")),
            # ),
            conditionalPanel(condition = "input.abaselecionada==1",
                             box(
                               title = "Gráfico prêmio por idade", status = "primary", #solidHeader = TRUE,
                               collapsible = TRUE,
                               plotOutput("plot2")
                             )),
            conditionalPanel(condition = "input.abaselecionada==3",
                             box(
                               title = "VPA x VP", status = "primary", #solidHeader = TRUE,
                               collapsible = TRUE,
                               plotOutput("plot3")
                             ))
            
        )#,
        # box(
        #   radioButtons(inputId = "premio", label = "Prêmio", choices= c("Puro Único"=1, "Nivelado pela duração do produto"=2, "Nivelado Personalizado"=3)),
        #     conditionalPanel(condition = "input.premio==3",
        #                      numericInput("npremio", "Periodo de pagamento", min = 0, max = (nrow(dados)-1), value = 1, step = 1))
        # )
        
        #plotlyOutput("plot"), #Saída do gráfico definida pelo UI
    )
)
