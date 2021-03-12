library(shiny)
library(shinydashboard)
library(extRemes)
library(DT)
library(data.table)
library(plotly)
library(e1071)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Introdução", tabName = "intro", icon = icon("book")),
        menuItem("Entrada de dados", icon = icon("table"), tabName = "data"),
        menuItem("Análise exploratória", icon = icon("chart-bar"), tabName = "explore"),
        menuItem("Análise de Frequência", icon = icon("microscope"), tabName = "freq")
    )
)
 


body <- dashboardBody(
    tabItems(
        tabItem(tabName = "intro",
                h2("Introdução"),
                tabsetPanel(
                    tabPanel(
                        h3("Histórico"),br(),
                        
                        h3(strong("Sobre a producao e disponibilizacao do EstatCheias/SGB")),
                        p("O aplicativo EstatCheias/SGB foi produzido no intuito de auxiliar a análise de frequência de cheias, utilizando dados inseridos pelos/as usuarios/as."),
                        p("O aplicativo é útil para profissionais, estudantes e pesquisadores interessados em hidrologia com pouca experiência em programacao e/ou que desejem utilizar uma interface simples e amigavel para uma analise de frequencia de eventos extremos, com foco na analise de cheias"),
                        p("Para isso, foi produzida uma interface simples e intuitiva para que seja possivel fazer ajustes de distribuicoes de probabilidade utilizando-se metodos de inferencia estatistica suficientemente robustos para análises de frequência rápidas."),
                        p("O aplicativo foi produzido pelo Pesquisador em Geociências Marcus Suassuna Santos como parte de suas atividades na operacao de alertas no Sistema de Alerta de Eventos Criticos pelo Servico Geológico do Brasil (CPRM/SGB)."),
                        p("A elaboracao e disponibilizacao deste aplicativo está alinhada com a missao do Servico Geológico do Brasil em",
                          strong(" gerar e disseminar conhecimento geocientifico com excelência, contribuindo para melhoria da qualidade de vida e desenvolvimento sustentavel do Brasil.")
                        ),
                        p("Brasilia, março de 2020."),
                        br()
                    ),
                    tabPanel(
                        h3("O aplicativo"),br(),
                        
                        h3(strong("Detalhes de funcionamento do aplicativo EstatCheias/SGB")),
                        p("O EstatCheias/SGB foi produzido de modo a possibilitar uma interface fluida e intuitiva para a analise de frequencia de exentos extremos, com foco na analise de cheias. Contudo, é importante explicitar alguns elementos acerta de sua estrutura e funcionamento."),
                        p("Alem deste painel introdutório, o aplicativo é composto por outras três partes:"),
                        
                        h4(strong("Entrada de Dados")),
                        p("No segundo painel, o/a usuario/a podera carregar seus próprios dados hidrológicos, visualizar os dados em formato de tabela dinâmica e visualizar um sumário estatístico dessas informacões."),
                        p("Para correta leitura dos dados, é obrigatório que o dado de entrada seja um arquivo .csv, com ao menos uma coluna com nome \"Anos\" e outra \"Vazao\"."),
                        p("É preferivel que as colunas do arquivo de entrada sejam separadas por ponto-e-virgula e as casas decimais sejam virgula. Caso isso nao ocorra, o/a usuario/a tera algumas opcões de ajuste de formato de separacao de colunas e casas decimais no painel lateral."),
                        p("Para correta leitura dos dados, tambem é importante que não exista valores faltantes."),
                        p("O carregamento do arquivo por parte do/a usuario/a é feito navegando em seus arquivos e pressionando o botao \"Busque o arquivo\". "),
                        
                        h4(strong("Análise exploratória")),
                        p("Neste painel, o/a usuario/a podera visualizar alguns gráficos que ilustram o comportamento geral da amostra. São eles: a série histórica dos dados; o histograma de frequência em conjunto com a densidade empírica; um diagrama de caixas (ou Boxplot); e um gráfico do tipo violino."),
                        p("A proposta é a de que alguns aspectos tais tendência, mudanças de comportamento ao longo do histórico, assimetria e distribuição dos dados possam ser investigados."),
                        
                        h4(strong("Análise de Frequência")),
                        p("No quarto painel, o/a usuario/a podera otimizar os parametros calibrados por meio do algoritmo genetico Multi-Objective Particle Swarm Optimization (MOPSO) (Nascimento et al., 2009). 4 Funcões objetivo diferentes sao admitidas: o coeficiente de Nash-Sutcliffe, a curva de permanência de vazões, e a curva de permanência considerando apenas as vazões altas ou baixas. Neste painel, as condicões da bacia nao sao calibradas, sao apenas inseridos pelo/a usuario/a."),
                        p("Para os seis parametros do modelo, o/a usuario/a devera escolher uma faixa que acredite conter os melhores parametros do modelo. Sugere-se, que essa faixa contenha os valores estimados no painel anterior. O/A usuario/a deve inserir um limite inferior e superior para os parametros. O/A usuario/a tambem podera alterar os parametros do algoritmo MOPSO e ao final clicar em \"Rodar\"."),
                        
                        br(),br(),br(),br()
                        
                        
                    ),
                    tabPanel(
                        h3("Análise de frequência de valores extremos"),br(),
                        
                        h3(strong("Elementos de análise de frequência de eventos extremos")),
                        p("O EstatCheias/SGB implementa métodos de análise de frequência clássico para a análise de eventos extremos. Para essa finalidade faz uso do pacote \"extRemes\" (Gilleland e Katz, 2014). Para que se entenda os procedimentos de análise de frequência implementados neste aplicativo, esta aba explora brevemente alguns aspectos dos aspectos da análise de frequência de eventos extremos."),
                        
                        h4(strong("A distribuição Generalizada de Valores Extremos")),
                        withMathJax(),
                        p("Caso um conjunto de variáveis aleatórias Independetes e Identicamente Distribuídas (IID), \\(\\ X_1,...X_n \\) siga uma distribuição \\(\\ df \\) e caso se queira aproximar a distribuição \\(\\ df \\) para os valores máximos, \\(\\ M_n = max\\{X_1, ..., X_n\\} \\), a \\(\\ Prob\\{M_n < z\\} = F^n(z)\\) e \\(\\ F^n(z) \\rightarrow 0 \\) na medida em que \\(\\ n \\rightarrow \\infty  \\)."),
                        p("Analogamente ao Teorema do Limite Central, cujo embasamento teórico para médias faz com que na formulação acima a  as considerações acima dão suporte para que \\(\\ df \\) tenda para a distribuição normal, as considerações acima dão suporte para que, no caso de valores extremos, a \\(\\ df \\) tenda assintoticamente para alguns casos específicos."),
                        p("Duas condições específicas podem ser mencionadas: na primeira, as séries históricas são reduzidas a blocos (por exemplo o caso de valores máximos diários ou anuais); na segunda considera-se valores acima de determinados limiares (picos sobre limiares). Na primeira situação a distribuição Generalizada de Valores Extremos oferece suporte teórico para que se ajuste às séries em blocos, e para a segunda, a distribuição Generalizada de Pareto oferece suporte similar."),
                        withMathJax("Este aplicativo considera apenas o caso em que as séries são analisadas em blocos. Ou seja, dedica-se aos casos em que a amostra é descrita pela distribuição Generalizada de Valores Extremos, dada pela seguinte expressão:
                        $$G(z) = exp\\left[- \\left( 1 + \\xi \\left( \\frac{z- \\mu}{\\sigma} \\right)\\right)^{\\frac{-1}{\\xi}}_{+} \\right]$$"),
                        p("em que \\(\\ y+ = max\\{y,0\\} \\), \\(\\ \\sigma > 0  \\), \\(\\ -\\infty < \\mu \\) e \\(\\ \\xi < \\infty \\). Na equação acima, três casos são considerados a depender do sinal de \\(\\ \\xi \\): a distribuição Frechét, quando \\(\\ \\xi > 0 \\); a distribuição Weibull (limitada superiormente) quando \\(\\ \\xi < 0 \\); e a distribuição Gumbel, quando \\(\\ \\xi \\) tende a zero."),
                        withMathJax("Nest último caso, a expressão acima resulta em:
                        $$G(z) = exp\\left[- exp \\left(- \\left( \\frac{z- \\mu}{\\sigma} \\right)\\right) \\right]$$"),
                        p("A função de quantis da distribuição GEV (relação entre probabilidade - ou períoro de recorrência) e valores de cheias estimados pode ser derivada diretamente da expressão de função acumulada de probabilidades. Considerando \\(\\ y_p = -1/ln\\left( 1 - p \\right) \\), em que \\(\\ p \\) é a probabilidade de não-excedência de um determinado valor de cheia e, assim, o período de retorno é dado por \\(\\ 1/p \\), o quantil de cheia é dado pelas seguintes expressões:"),
                        withMathJax("$$ z_p = \\mu + \\frac{\\sigma }{\\xi} \\left[ y_{p}^{\\xi} - 1\\right], \\ \\xi \\neq 0 $$"), p(" ou "),
                        withMathJax("$$ z_p = \\mu + \\sigma ln\\left( y_{p} \\right), \\ \\xi = 0 $$"),
                        
                        h4(strong("Inferência estatística aplicada à distribuição GEV")),
                        
                        p("Existem algumas formas de se estimar parâmetros das funções de distribuição de probabilidades. Aqui neste aplicativo, o método da Máxima Verossimilihança (no caso das distribuições GEV e Gumbel) e dos Momentos-L (apenas GEV) são utilizados para essas estimativas."),
                        p("A estimativa de máxima verossimilhança é feita por meio da definição do conjunto de parâmetros que maximizam a função de verossimilhança (ou da log-verossimilhança). Para distribuição GEV, a função de Log-Verossimilhança é dada pela função abaixo:"),
                        
                        withMathJax("$$ l \\left(\\mu,\\sigma, \\xi ; z_{1}, \\dots , z_{m} \\right) =
                        - m ln \\sigma - \\left( 1 + 1 \\ / \\xi \\right) \\sum_{i = 1}^{m} ln\\left[1 + \\xi \\left(\\frac{z_1-\\mu}{\\sigma} \\right) \\right]_{+} -
                        \\sum_{i = 1}^{m} \\left[1 + \\xi \\left(\\frac{z_1-\\mu}{\\sigma} \\right)\\right]_{+}^{-1 \\ / \\xi}$$"),
                        
                        
                    )
                )
        ),
        
        tabItem(tabName = "data",
                h2("Entrada de dados"),
                
                fluidRow(
                    column(width = 3,
                           box(title = "Entrada de Dados",
                               width = NULL,
                               status = "primary",
                               solidHeader = TRUE,
                               
                               fileInput("file1", "Escolha arquivo csv",
                                         multiple = FALSE,
                                         buttonLabel = "Busque o arquivo...",
                                         placeholder = "Nenhum arquivo selecionado",
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")),
                               
                               tags$hr(),
                               
                               # Separador de colunas do arquivo de entrada
                               radioButtons("sep", "Separados de coluna",
                                            choices = c(Virgula = ",",
                                                        Ponto_e_Virgula = ";",
                                                        Tab = "\t"),
                                            selected = ";"),
                               
                               tags$hr(),
                               
                               # Separador de casas decimais do arquivo de entrada
                               radioButtons("dec", "Decimal",
                                            choices = c(Virgula = ",",
                                                        Ponto = "."),
                                            selected = ","),
                               tags$hr(),
                               
                               )),
                    column(width = 3,
                           box(title = "Tabela com os dados",
                               width = NULL,
                               status = "success",
                               solidHeader = TRUE,
                               DT::dataTableOutput("tabela"))
                           
                           ),
                    column(
                        width = 6,
                        
                        box(title = "Sumário estatístico", width = 10, solidHeader = TRUE, status = "warning",
                            
                            tableOutput("sumario")
                            
                            )
                        
                        )
                    )
                
                ),
        
        tabItem(tabName = "explore",
                h2("Análise exploratória"),
                
                fluidRow(
                    box(title = "Série Histórica", width = 12, solidHeader = TRUE, status = "primary",
                        plotlyOutput("timeSeries"))
                ),
                
                fluidRow(
                    box(title = "Histograma", width = 4, solidHeader = TRUE, status = "success",
                        sliderInput("bins",
                                    "Número de classes do histograma:",
                                    min = 1,
                                    max = 30,
                                    value = 8),
                        tags$hr(),
                        
                        plotlyOutput("distPlot")
                        
                    ),
                    box(title = "Boxplot", width = 4, solidHeader = TRUE, status = "success",
                        plotlyOutput("boxPlot")
                    ),
                    box(title = "Violino", width = 4, solidHeader = TRUE, status = "success",
                        plotlyOutput("violinPlot")
                    )
                    
                )
                
                ),
        
        tabItem(tabName = "freq",
                h2("Análise de frequência"),
                fluidRow(
                         box(title = "Opções de ajuste", width = 12, solidHeader = TRUE, status = "primary",
                             fluidRow(
                                 box(width = 4,
                                     helpText("Para a distribuição Gumbel, apenas o método da Máxima Verossimilhando é admitido para a estimação de parâmetros."),
                                     selectInput("dist", 
                                                 h3("Escolha a distribuição de probabilidades"),
                                                 choices = list("GEV" = "GEV", 
                                                                "Gumbel" = "Gumbel"),
                                                 selected = "GEV")   
                                 ),
                                 box(width = 4,
                                     
                                     conditionalPanel(
                                         h3("Escolha o método de estimação dos parâmetros"), 
                                         condition = "input.dist == 'GEV'",
                                         selectInput("metodo", "Metodo",
                                                     list("Máxima Verossimilhança" = "MLE",
                                                          "Momentos-L" = "Lmoments"))
                                         
                                     ),
                                     
                                     conditionalPanel(
                                         h3("Escolha o método de estimação dos parâmetros"), 
                                         condition = "input.dist == 'Gumbel'",
                                         selectInput("metodo", "Metodo",
                                                     list("Máxima Verossimilhança" = "MLE"))
                                     )
                                     
                                     )
                                 
                             )
                                
                         )
                ),
                fluidRow(
                    box(title = "Sumário do Ajuste", width = 4, solidHeader = TRUE, status = "success",
                               tableOutput("parCI"),
                        tags$hr(),
                        div("Atenção!", style = "color:red"),
                        br(),
                        textOutput("evalForma")
                        ),
                    box(title = "Vazões estimadas", width = 4, solidHeader = TRUE, status = "success",
                        tableOutput("returnLevels")
                        ),
                    box(title = "TR para diferentes vazões", width = 4, solidHeader = TRUE, status = "success",
                        
                        numericInput("q", "Vazão para cálculo do período de retorno:",
                                    value = 1),
                        
                        
                        tableOutput("returnPeriods")
                        )
                    ),
                fluidRow(
                    box(title = "Gráficos dos ajustes", width = 12, solidHeader = TRUE, status = "warning",
                        box(width = 6,
                            plotlyOutput("quantquant")   
                        ),
                        box(width = 6,
                            plotlyOutput("density")
                        ),
                        box(width = 12,
                            plotlyOutput("quantci"),
                            downloadButton("downloadData", 'Baixar Resultado'))
                        )
                )
                    
                    
                )
        )
    )


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = span(img(src = "logomarcacprmhorizontal_v2.jpg",
                                   height = 30, width = 30*5),"\t",
                               ".    EstatCheias/SGB"),
                  titleWidth = 450),
  sidebar,
  body
    
    )

server <- function(input, output) {
    
  # show intro modal
  observeEvent("", {
    showModal(modalDialog(size = "l",
                          includeHTML("intro_text.html"),
                          easyClose = TRUE,
                          footer = modalButton("Sair da introducao")
    ))
  })
  
  observeEvent(input$intro,{
    removeModal()
  })
    
    dataInput <- reactive({
      
      req(input$file1)
      
      tryCatch({
        
        df <- read.table(input$file1$datapath,
                         sep = input$sep,
                         dec = input$dec,
                         header = TRUE)
        
      },
      error = function(e) {
        stop(safeError(e))
      })
      
    })

    output$tabela <- DT::renderDataTable({
        dataInput()
    })
    
    output$sumario <- renderTable({
      
      tryCatch({
        
        data.frame("Variável" = c("Média", "Variância", "Desvio Padrão",
                                  "Coeficiente de variação",
                                  "Coeficiente de assimetria", "Curtose"),
                   "Valor" = c(paste(round(mean(dataInput()$Vazao), 1), "m³/s"),
                               paste(round(var(dataInput()$Vazao), 1), "(m³/s)²"),
                               paste(round(sd(dataInput()$Vazao), 1), "m³/s"),
                               paste(round(sd(dataInput()$Vazao)/mean(dataInput()$Vazao), 3)),
                               paste(round(skewness(dataInput()$Vazao), 3)),
                               paste(round(kurtosis(dataInput()$Vazao), 3))))
        
      },
      error = function(e) {
        paste("Erro na leitura dos dados.")
      })
      
      
    })
    
    output$media <- renderValueBox({
      valueBox(
            "Máxima média", paste0(round(mean(dataInput()$Vazao), 1), "m³/s"), icon = icon("list"),
            color = "purple"
        )
    })
    
    output$variancia <- renderValueBox({
      valueBox(
            "Variância", paste0(round(var(dataInput()$Vazao), 1), "(m³/s)²"), icon = icon("list"),
            color = "purple"
        )
    })
    
    output$desvpad <- renderValueBox({
      valueBox(
            "Desvio Padrão", paste0(round(sd(dataInput()$Vazao), 1), "m³/s"), icon = icon("list"),
            color = "purple"
        )
    })
    
    output$assimetria <- renderValueBox({
      valueBox(
            "Coeficiente de assimetria", paste0(round(skewness(dataInput()$Vazao), 3)), icon = icon("list"),
            color = "purple"
        )
    })
    
    output$curtose <- renderValueBox({
      valueBox(
            "Curtose", paste0(round(kurtosis(dataInput()$Vazao), 3)), icon = icon("list"),
            color = "purple"
        )
    })
    
    output$cv <- renderValueBox({
      valueBox(
            "Coeficiente de variação", paste0(round(sd(dataInput()$Vazao)/mean(dataInput()$Vazao), 3)), icon = icon("list"),
            color = "purple"
        )
    })
    
    output$timeSeries <- renderPlotly(
        plot1 <- plot_ly(
            x = dataInput()$Anos,
            y = dataInput()$Vazao,
            type = 'scatter',
            mode = 'lines',
            hoverinfo = 'text',
            text = ~paste('</br> Ano: ', dataInput()$Anos,
                          '</br> Vazão: ', round(dataInput()$Vazao, 1), "m³/s")) %>%
            layout(title = "Série de máximos anuais",
                   xaxis = list(title = "Anos"),
                   yaxis = list (title = "Vazão (m³/s)"))
        
    )
    
    output$distPlot <- renderPlotly({
        x <- dataInput()$Vazao
        fit <- density(dataInput()$Vazao)
        plot3 <- plot_ly(x = x, type = "histogram", nbinsx = input$bins, alpha = 0.7, name = "Histograma") %>%
            add_lines(fit$x, fit$y, yaxis = "y2", name = "Densidade", fill = "tozeroy") %>%
            layout(yaxis2 = list(overlaying = "y", side = "right"))
        
    })
    
    output$boxPlot <- renderPlotly({
        x    <- dataInput()$Vazao
        plot3 <- plot_ly(y = x, type = "box", boxpoints = "all", jitter = 0.3, pointpos = -1.8, name = "Vazão")
    })
    
    output$violinPlot <- renderPlotly({
        x    <- dataInput()$Vazao
        plot4 <- plot_ly(y = x, type = "violin", box = list(visible = TRUE), meanline = list(visible = T), name = "Vazão")
    })
    
    fit1 <- reactive({
        x <- dataInput()$Vazao
        fit1 <- fevd(x, units = "m³/s",
                     type = input$dist,
                     method = input$metodo)
    })
    
    output$sumGev <- renderTable({
        fit <- fit1()
        data.frame("Parametro" = names(distill(fit)),
                   "Valor" = as.numeric(distill(fit)))
    })
    
    output$plotFit1 <- renderPlot({
        fit <- fit1()
        plot(fit)
    })
    
    output$returnLevels <- renderTable({
        fit <- fit1()
        TR <- c(2, 25, 50, 100, 200)
        data.frame("TR" = TR,
                   "Estimativa" = return.level(fit, return.period = TR, do.ci = TRUE)[,2],
                   "Lim_Inf" = return.level(fit, return.period = TR, do.ci = TRUE)[,1],
                   "Lim_Sup" = return.level(fit, return.period = TR, do.ci = TRUE)[,3])
    })
    
    output$parCI <- renderTable({
        fit <- fit1()
        CI <- ci(fit, type = "parameter")
        
        if(input$dist == "GEV"){
            data.frame("Parâmetro" = c("Posição", "Escala", "Forma"),
                       "Estimativa" = CI[,2],
                       "Lim_Inf" = CI[,1],
                       "Lim_Sup" = CI[,3])
        } else {
            data.frame("Parâmetro" = c("Posição", "Escala"),
                       "Estimativa" = CI[,2],
                       "Lim_Inf" = CI[,1],
                       "Lim_Sup" = CI[,3])
        }
        
        
    })
    
    output$evalForma <- renderText({
        fit <- fit1()
        CI <- ci(fit, type = "parameter")
        
        if(input$dist == "GEV"){
            
            if(CI[3,2] > 0){
                if(sign(CI[3,1]) == sign(CI[3,3])){
                    paste("O sinal do parâmetro de forma é positivo e o intervalo de confiança não inclui o zero.
                      É provável que a distribuição Gumbel não se ajuste aos dados.
                      Sugere-se utilizar a distribuição GEV e não Gumbel.")
                } else {
                    paste("O sinal do parâmetro de forma é positivo e o intervalo de confiança inclui o zero.
                      É provável que a distribuição Gumbel se ajuste bem aos dados.
                      Sugere-se utilizar ou a distribuição GEV ou a distribuição de Gumbel.")
                }
            } else {
                
                if(sign(CI[3,1]) == sign(CI[3,3])){
                    paste("O sinal do parâmetro de forma é negativo e o intervalo de confiança não inclui o zero.
                      Há indícios de que a distribuição que melhor se ajusta aos dados seja limitada superiormente, o que é improvável que ocorra com as vazões.
                      Sugere-se verificar os dados e ver se não há problemas com eles, antes de utilizar o ajuste para qualquer finalidade.")
                } else {
                    paste("O sinal do parâmetro de forma é negativo e o intervalo de confiança inclui o zero.
                      É provável que a distribuição Gumbel se ajuste aos dados, porém há indícios de que a distribuição seja limitada superiormente.
                      Sugere-se utilizar a distribuição Gumbel, contudo, com cautela.")
                }
                
                
            }
            
        } else {
            paste("Distribuição Gumbel ajustada")
        }
        
        
    })
    
    output$returnPeriods <- renderTable({
        fit <- fit1()
        Quant <- c(input$q)
        data.frame("Quant" = Quant,
                   "ProbabilidadeSupera(%)" = pextRemes(fit, q = Quant, lower.tail = FALSE) * 100,
                   "TR" = 1/pextRemes(fit, q = Quant, lower.tail = FALSE))
    })
    
    output$quantquant <- renderPlotly({
        fit <- fit1()
        x <- dataInput()$Vazao
        N <- length(x)
        cunane <- (c(1:N)-0.4)/(N+0.2)
        teorico <- round(return.level(fit, return.period = 1/cunane))
        teorico <- teorico[order(teorico)]
        obs <- round(x[order(x)])

        plot5 <- plot_ly(
            x = teorico,
            y = teorico,
            name = "Terórico",
            type = 'scatter',
            mode = 'lines') %>%
            layout(title = "Quantis empíricos e estimados pela distribuição GEV ajustada",
                   xaxis = list(title = "Quantis do modelo GEV (m³/s)"),
                   yaxis = list (title = "Quantis observados (m³/s)")) %>%
            add_trace(x = teorico, y = obs, mode = "markers", name = "Observado")
        
        
    })
    
    Analise_Freq <- reactive({
      
      x <- dataInput()$Vazao
      fit <- fit1()
      N <- length(x)
      cunane <- (c(1:N)-0.4)/(N+0.2)
      plotprob <- c(1/cunane, 20, 50, 100, 200, 500, 1000)
      plotprob <- plotprob[order(plotprob)]
      teorico <- round(return.level(fit, return.period = plotprob, do.ci = TRUE))
      
      est <- teorico[,2]
      inf <- teorico[,1]
      sup <- teorico[,3]
      obs <- round(x[order(x, decreasing = TRUE)])
      
      data.frame("Tempo de retorno" = plotprob,
                 "Probabilidade" = 1/plotprob,
                 "Estimativa" = est,
                 "LimInf" = inf,
                 "LimSup" = sup)
      
    })
    
    
    output$downloadData <- downloadHandler(
      filename = function() paste0("Analise_Freq.csv"),
      content = function(file) {
        write.csv2(Analise_Freq(), file, row.names = FALSE)
      }
    )
    
    
    output$quantci <- renderPlotly({
        x <- dataInput()$Vazao
        fit <- fit1()
        N <- length(x)
        cunane <- (c(1:N)-0.4)/(N+0.2)
        plotprob <- c(1/cunane, 20, 50, 100, 200, 500, 1000)
        plotprob <- plotprob[order(plotprob)]
        teorico <- round(return.level(fit, return.period = plotprob, do.ci = TRUE))
        
        est <- teorico[,2]
        inf <- teorico[,1]
        sup <- teorico[,3]
        obs <- round(x[order(x, decreasing = TRUE)])
        
        plot_ly(
            x = plotprob,
            y = est,
            name = "Teórico",
            type = 'scatter',
            mode = 'lines') %>%
            add_trace(x = plotprob, y = inf, name = "Inferior", mode = "lines", colors = "grey",
                      line = list(color = "grey", width = 1, dash = "dash")) %>%
            add_trace(x = plotprob, y = sup, name = "Superior", mode = "lines", colors = "grey",
                      line = list(color = "grey", width = 1, dash = "dot")) %>%
            add_trace(x = 1/cunane, y = obs, name = "Observado", mode = "markers") %>%
            layout(title = "Estimativas de cheias para diferentes períodos de retorno com a distribuição GEV",
                   xaxis = list(title = "Período de retorno (anos)", type = "log"),
                   yaxis = list (title = "Vazão estimada (m³/s)"))
        
        
    })
    
    output$density <- renderPlotly({
        
        fit_d <- density(dataInput()$Vazao)
        fit <- fit1()
        
        if(input$dist == "GEV"){
            
            if(input$metodo == "MLE"){
                plot_ly(x = fit_d$x, y = fit_d$y, name = "Densidade empírica", fill = "tozeroy", mode = "lines", type = "scatter") %>%
                    add_trace(x = fit_d$x,
                              y = devd(fit_d$x, fit$results$par[1], fit$results$par[2], fit$results$par[3],
                                       type = "GEV"),
                              name = "Distribuição ajustada")
            } else {
                
                plot_ly(x = fit_d$x, y = fit_d$y, name = "Densidade empírica", fill = "tozeroy", mode = "lines", type = "scatter") %>%
                    add_trace(x = fit_d$x,
                              y = devd(fit_d$x, fit$results[1], fit$results[2], fit$results[3],
                                       type = "GEV"),
                              name = "Distribuição ajustada")
                
            }
            
            
        } else {
            plot_ly(x = fit_d$x, y = fit_d$y, name = "Densidade empírica", fill = "tozeroy", mode = "lines", type = "scatter") %>%
                add_trace(x = fit_d$x,
                          y = devd(fit_d$x, fit$results$par[1], fit$results$par[2],
                                   type = "GEV"),
                          name = "Distribuição ajustada")
        }
        
        
        
        
    })

}

shinyApp(ui = ui, server = server)