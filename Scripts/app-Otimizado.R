
# Execução de packages utilizados no script. Se eles ainda não estão instalados,
# recomendamos executar o arquivo "install_packages.R" disponibilizado no GitHub
# deste projeto.
library(shiny)
library(shinyEventLogger)

library(dplyr)
library(DiagrammeR)

library(bupaR)
library(processmapR)

library(bupaR)
library(daqapo)
library(datetime)
library(dplyr)
library(edeaR)
library(eventdataR)
library(heuristicsmineR)
library(jsonlite)
library(lubridate)
library(petrinetR)
library(pm4py)
library(processanimateR)
library(processcheckR)
library(processmapR)
library(processmonitR)
library(shiny)
library(shinycssloaders)
library(tidyr)
library(tidyverse)
library(xesreadR)
library(ggplot2)

library(DiagrammeR)

library(heuristicsmineR)
#library(petrinetR)


#Diretorio onde estão os arquivos-----------------------------------------------

diretorio <- 'D:/Inova/Dados-CNJ_disponibilizados/base/'

#-------------------------------------------------------------------------------

#setar diretorio
setwd(diretorio)

# 0.BASE DE DADOS

tribunal_orig <- NA

tribunal_orig <- read.csv2("./Arquivos/1/TRF2.csv", header=TRUE, sep=";")

# tribunal_int é uma variável intermediária dos dados do tribunal
tribunal_int <- NA

tribunal_int <- tribunal_orig %>%
  # mutate(dadosBasicos.assunto.codigoNacional2 = dadosBasicos.assunto.codigoNacional) %>%
  mutate(dadosBasicos.assunto.descricao2 = dadosBasicos.assunto.descricao) %>%
  # mutate(dadosBasicos.assunto.principal2 = dadosBasicos.assunto.principal) %>%
  # mutate(dadosBasicos.classeProcessual2 = dadosBasicos.classeProcessual) %>%
  # mutate(dadosBasicos.codigoLocalidade2 = dadosBasicos.codigoLocalidade) %>%
  # mutate(dadosBasicos.competencia2 = dadosBasicos.competencia) %>%
  mutate(dadosBasicos.dataAjuizamento2 = dadosBasicos.dataAjuizamento) %>%
  mutate(dadosBasicos.descricaoclasseProcessual2 = dadosBasicos.descricaoclasseProcessual) %>%
  # mutate(dadosBasicos.nivelSigilo2 = dadosBasicos.nivelSigilo) %>%
  mutate(dadosBasicos.numero2 = dadosBasicos.numero) %>%
  # mutate(dadosBasicos.orgaoJulgador.codigoMunicipioIBGE2 = dadosBasicos.orgaoJulgador.codigoMunicipioIBGE) %>%
  # mutate(dadosBasicos.orgaoJulgador.codigoOrgao2 = dadosBasicos.orgaoJulgador.codigoOrgao) %>%
  mutate(dadosBasicos.orgaoJulgador.instancia2 = dadosBasicos.orgaoJulgador.instancia) %>%
  mutate(dadosBasicos.orgaoJulgador.nomeOrgao2 = dadosBasicos.orgaoJulgador.nomeOrgao) %>%
  # mutate(dadosBasicos.siglaclasse2 = dadosBasicos.siglaclasse) %>%
  # mutate(dadosBasicos.valorCausa2 = dadosBasicos.valorCausa) %>%
  mutate(grau2 = grau) %>%
  # mutate(IBGE.Nome_Mesorregiao2 = IBGE.Nome_Mesorregiao) %>%
  # mutate(IBGE.Nome_Municipio2 = IBGE.Nome_Municipio) %>%
  # mutate(millisInsercao2 = millisInsercao) %>%
  mutate(movimento.dataHora2 = movimento.dataHora) %>%
  mutate(movimento.movimentoNacional.codigoNacional2 = movimento.movimentoNacional.codigoNacional) %>%
  mutate(movimento.movimentoNacional.descricao2 = movimento.movimentoNacional.descricao) %>%
  mutate(siglaTribunal2 = siglaTribunal) %>%
  mutate(cluster2=cluster)

tribunal_int[1:ncol(tribunal_orig)] <- NULL

trf2_final <- tribunal_int

# O código comentado abaixo é destinado à reunião de dados de outros tribunais
# Para a sua utilização basta replicar o código acima feito para o TRF2 para
# cada novo dataframe. Ao final serão todos reunidos na variável "pg" de processos
# gerais.

# trf11_final
# trf12_final
# trf13_final
# trf14_final
# trf15_final
# trf2_final
# trf31_final
# trf32_final
# trf33_final
# trf51_final
# trf52_final
# trf53_final
# trf54_final
# 
# pg <- NA
# 
# pg <- rbind(trf11_final,
#             trf12_final,
#             trf13_final,
#             trf14_final,
#             trf15_final,
#             trf2_final,
#             trf31_final,
#             trf32_final,
#             trf33_final,
#             trf51_final,
#             trf52_final,
#             trf53_final,
#             trf54_final)

# A variável "pg" inclui todos os processos em análise
pg <- NA
# Como estamos utilizando somente o TRF2 que só possui um arquivo, "pg" é o mesmo
# que o dataframe do TRF2
pg <- trf2_final

# A variável "case_id" é necessária para diferenciar processos com mesmo número
# mas que possuem classe processual diferente.
case_id <- NA
case_id <- paste(pg$dadosBasicos.numero, "|", pg$dadosBasicos.classeProcessual)
pg$case_id <- c(case_id)
pg$dadosBasicos.numero <- NULL

# Ajustar os formatos de data/hora para viabilizar a geração da variável de
# event log a ser utilizada nos algoritmos de process mining.
x <- NA
x <- c(pg$movimento.dataHora)
movimento.dataHora2 <- NA
movimento.dataHora2 <- ymd_hms(x)
pg$movimento.dataHora2 <- c(movimento.dataHora2)
x <- NA
x <- c(pg$dadosBasicos.dataAjuizamento2)
dadosBasicos.dataAjuizamento2 <- NA
dadosBasicos.dataAjuizamento2 <- ymd_hms(x)
pg$dadosBasicos.dataAjuizamento2 <- c(dadosBasicos.dataAjuizamento2)

# Inserir tipos de responsável pelo movimento: identificação de Magistrado ou 
# do perfil de Serventuário (Arquivista, Auxiliar da Justiça, Contador, 
# Distribuidor, Escrivão/Diretor de Secretaria/Secretário Jurídico e Oficial
# de Justiça).  Essa tabela foi elaborada a partir da tabela de movimentos do
# CNJ no seu primeiro nível (Magistrado) e segundo nível (Serventuário).
tab_movimentos <- NA
tab_movimentos <- read.csv2("./Arquivos/0/movimentos_tipo_responsavel.csv", header=TRUE, sep=";")
head(tab_movimentos)
pg <- left_join(pg, tab_movimentos, #%>% select(codigo, tipo_responsavel_movimento),
                by = c("movimento.movimentoNacional.codigoNacional2" = "codigo"))

# A variável "mc_log" inclui todos os event logs no formato a ser posteriormente
# utilizado nos algoritmos de process mining.
mc_log <- pg %>%
  # Considerando que todos os movimentos são registros de atividades já
  #   concluídas, o código abaixo adiciona o status "complete" para o atributo
  #   lifecycle_id.
  mutate(status = "complete",
         activity_instance = 1:nrow(.)) %>%
  eventlog(
    case_id = "case_id",
    activity_id = "movimento.movimentoNacional.descricao2",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    timestamp = "movimento.dataHora2",
    resource_id = "tipo_responsavel_movimento"
  )

# XX Comentada
# mc_resource <- mc_log

#Criação dos choices com grupo total
trib_gr_orgaos <- tribunal_int
tab_gr_orgaos <- unique(trib_gr_orgaos$cluster2)  %>% sort()
tab_gr_orgaos <- c('Todos',tab_gr_orgaos)

# XX Comentado porque não parece influenciar

#mc_log_resource <- mc_log

#Função filtro ###########################################

filtroGrOrgaos <- 'Todos'



GerarLog0 <- function(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao){
  
  ##Grupo de órgãos
  if (filtroGrOrgaos!= 'Todos'){
    mc_log_resource  <- mc_log %>% filter(cluster2 == as.character(filtroGrOrgaos))
  }else{
    mc_log_resource  <- mc_log 
    # mc_log_resource <- mc_resource %>%
    #   # Considerando que todos os movimentos são registros de atividades já
    #   #   concluídas, o código abaixo adiciona o status "complete" para o atributo
    #   #   lifecycle_id.
    #   mutate(status = "complete",
    #          activity_instance = 1:nrow(.)) %>%
    #   eventlog(
    #     case_id = "case_id",
    #     activity_id = "movimento.movimentoNacional.descricao2",
    #     activity_instance_id = "activity_instance",
    #     lifecycle_id = "status",
    #     timestamp = "movimento.dataHora2",
    #     resource_id = "tipo_responsavel_movimento"
    # )
    
  }
  
  ##Assunto
  if (filtroAssuntos!= 'Todos'){
    mc_log_resource  <- mc_log_resource %>% filter(dadosBasicos.assunto.descricao2 == filtroAssuntos)
  }
  else{
    # mc_log_resource <- mc_resource %>%
    #   # Considerando que todos os movimentos são registros de atividades já
    #   #   concluídas, o código abaixo adiciona o status "complete" para o atributo
    #   #   lifecycle_id.
    #   mutate(status = "complete",
    #          activity_instance = 1:nrow(.)) %>%
    #   eventlog(
    #     case_id = "case_id",
    #     activity_id = "movimento.movimentoNacional.descricao2",
    #     activity_instance_id = "activity_instance",
    #     lifecycle_id = "status",
    #     timestamp = "movimento.dataHora2",
    #     resource_id = "tipo_responsavel_movimento")
  }
  
  ##Classe
  if (filtroClasses!= 'Todos'){
    mc_log_resource  <- mc_log_resource %>% filter(dadosBasicos.descricaoclasseProcessual2 == filtroClasses)
  }
  else{
    # mc_log_resource <- mc_resource %>%
    #   # Considerando que todos os movimentos são registros de atividades já
    #   #   concluídas, o código abaixo adiciona o status "complete" para o atributo
    #   #   lifecycle_id.
    #   mutate(status = "complete",
    #          activity_instance = 1:nrow(.)) %>%
    #   eventlog(
    #     case_id = "case_id",
    #     activity_id = "movimento.movimentoNacional.descricao2",
    #     activity_instance_id = "activity_instance",
    #     lifecycle_id = "status",
    #     timestamp = "movimento.dataHora2",
    #     resource_id = "tipo_responsavel_movimento")
  }
  
  ##Grau
  if (filtroGrau != 'Todos'){
    mc_log_resource  <- mc_log_resource %>% filter(grau2 == filtroGrau )
  }
  else{
    # mc_log_resource <- mc_resource %>%
    #   # Considerando que todos os movimentos são registros de atividades já
    #   #   concluídas, o código abaixo adiciona o status "complete" para o atributo
    #   #   lifecycle_id.
    #   mutate(status = "complete",
    #          activity_instance = 1:nrow(.)) %>%
    #   eventlog(
    #     case_id = "case_id",
    #     activity_id = "movimento.movimentoNacional.descricao2",
    #     activity_instance_id = "activity_instance",
    #     lifecycle_id = "status",
    #     timestamp = "movimento.dataHora2",
    #     resource_id = "tipo_responsavel_movimento")
  }
  
  ##Órgão julgador
  if (filtroOrgao != 'Todos'){
    mc_log_resource  <- mc_log_resource %>% filter(dadosBasicos.orgaoJulgador.nomeOrgao2 == filtroOrgao )
  }
  else{
    # mc_log_resource <- mc_resource %>%
    #   # Considerando que todos os movimentos são registros de atividades já
    #   #   concluídas, o código abaixo adiciona o status "complete" para o atributo
    #   #   lifecycle_id.
    #   mutate(status = "complete",
    #          activity_instance = 1:nrow(.)) %>%
    #   eventlog(
    #     case_id = "case_id",
    #     activity_id = "movimento.movimentoNacional.descricao2",
    #     activity_instance_id = "activity_instance",
    #     lifecycle_id = "status",
    #     timestamp = "movimento.dataHora2",
    #     resource_id = "tipo_responsavel_movimento")
  }
  
  return(mc_log_resource)
}

# UI ##########################################################################
ui <- fluidPage(
  
  titlePanel('Mapeamento de Processos'),
  sidebarLayout(
    
    sidebarPanel(
      h3('Última seleção:'),
      h4(textOutput("caption0"),
         textOutput("caption1"),
         textOutput("caption2"),
         textOutput("caption3"),
         textOutput("caption4")),
      selectInput(
        'e0', '0. Selecione o grupo de órgãos julgadores', choices = tab_gr_orgaos, selected = 'Todos',
        selectize = FALSE
      ),
      selectInput(
        'e1', '1. Selecione o assunto', choices = "",selected = "" ,
        selectize = FALSE
      ),
      selectInput(
        'e2', '2. Selecione a classe', choices = "",selected = "" ,
        selectize = FALSE
      ),
      selectInput(
        'e3', '3. Selecione o grau', choices = "",selected = "" ,
        selectize = FALSE
      ),
      selectInput(
        'e4', '. Selecione o órgão julgador', choices = "",selected = "" ,
        selectize = FALSE
      ),
      
      actionButton(inputId = "Nova_Pesquisa",
                   label = "Nova Pesquisa")
      
    ),
    
    mainPanel(
      # titlePanel(textOutput("caption")),
      tabsetPanel(type = "pills",
                  
                  tabPanel(title = "Instruções",
                           #br(), h1(textOutput("Instruções")),
                           br(), h4('Para começar sua análise, selecione ao lado os filtros desejados e clique nos menus acima.'),
                           br(),h4('Para recomeçar uma nova pesquisa, volte a esta página e clique no botão Nova Pesquisa.'),
                           br(),h4('Certifique-se que todas as seleções encontram-se na opção Todos antes de selecionar novo Menu.')),
                  
                  tabPanel(title = "Sumário",
                           br(), 
                           fixedRow(
                             column(width = 4,
                                    h4("Total de Processos: "),
                                    h4("Total de Movimentos: "),
                                    h4("Tipos de Movimentos: "),
                                    h4("Diferentes Fluxos: ")
                             ),
                             column(width = 1,
                                    h4(textOutput("tot_processos")),
                                    h4(textOutput("tot_movimentos")),
                                    h4(textOutput("tipos_movimentos")),
                                    h4(textOutput("tipos_fluxos"))
                             )),
                           br(),
                           fixedRow(
                             column(width = 12,
                                    h3("Perfis de responsáveis pelos movimentos"),
                                    tableOutput("perfis_mov"),
                             ),
                             column(width = 12,
                                    h3("Tempo de Processamento"),
                                    plotOutput("tempo_proc"),
                             ),
                             br(),
                             column(width = 12,
                                    h3("Frequência de movimentos"),
                                    plotOutput("freq_mov")
                             ))
                  ),
                  
                  tabPanel(title = "Process map",
                           br(), uiOutput("process_map")),
                  
                  tabPanel(title = "Mapa com animação",
                           br(), uiOutput("process_animate")),
                  
                  tabPanel(title = "Fluxos frequentes",
                           br(), uiOutput("Fluxos_frequentes")),
                  
                  tabPanel(title = "Mapa de transferência de trabalho",
                           br(),uiOutput("Mapa_trabalho")),
                  
                  tabPanel(title = "Dependency Matrix",
                           br(), grVizOutput("Dependency_Tot"))
                  
                  
      )
    )
  )
) # end of ui

# SERVER ######################################################################
server <- function(input, output, session) {
  
  #Botões laterais
  ########################  
  
  
  #Resetar dados e escolhas
  observeEvent(input$Nova_Pesquisa,{
    mc_log_resource <- mc_log
    updateSelectInput(session, 'e0', '0. Selecione o grupo de órgãos julgadores', 
                      choices = tab_gr_orgaos, selected = 'Todos'
    )
  })
  
  #Assunto
  observeEvent(
    input$e0,
    updateSelectInput(session, 'e1', '1. Selecione o assunto', 
                      choices = if (input$e0 == 'Todos'){
                        sort(unique(c(tribunal_int$dadosBasicos.assunto.descricao2,'Todos')))

                      }else{
                        sort(unique(c(tribunal_int$dadosBasicos.assunto.descricao2[tribunal_int$cluster2==input$e0],'Todos')))
                      },
                      selected = 'Todos'))
  
  #Classe
  observeEvent(
    input$e1,
    updateSelectInput(session, 'e2', '2. Selecione a classe',
                      choices =
                        if(input$e1 == 'Todos'){
                          sort(unique(c(tribunal_int$dadosBasicos.descricaoclasseProcessual2,'Todos')))
                        } else{
                          sort(unique(c(tribunal_int$dadosBasicos.descricaoclasseProcessual2[tribunal_int$dadosBasicos.assunto.descricao2==input$e1 &
                                                                                               tribunal_int$cluster2==input$e0],'Todos')))

                        },
                      selected = 'Todos'))
  
  
  #Grau
  observeEvent(
    input$e2,
    updateSelectInput(session, 'e3', '3. Selecione o grau',
                      choices = 
                        if(input$e2 == 'Todos'){
                          sort(unique(c(tribunal_int$grau2,'Todos')))
                        } else{
                          sort(unique(c(tribunal_int$grau2[tribunal_int$dadosBasicos.descricaoclasseProcessual2==input$e2 &
                                                             tribunal_int$dadosBasicos.assunto.descricao2==input$e1 & 
                                                             tribunal_int$cluster2==input$e0],'Todos')))
                          
                        },
                      selected = 'Todos'))
 
  
  #Órgão Julgador
  observeEvent(
    input$e3,
    updateSelectInput(session, 'e4', '4. Selecione o órgão julgador',
                      choices = 
                        if(input$e3 == 'Todos'){
                          sort(unique(c(tribunal_int$dadosBasicos.orgaoJulgador.nomeOrgao2,'Todos')))
                        } else{
                          sort(unique(c(tribunal_int$dadosBasicos.orgaoJulgador.nomeOrgao2[tribunal_int$grau2==input$e3 &
                                                                                             tribunal_int$dadosBasicos.descricaoclasseProcessual2==input$e2 &
                                                                                             tribunal_int$dadosBasicos.assunto.descricao2==input$e1 &
                                                                                             tribunal_int$cluster2==input$e0 ],'Todos')))
                        },
                      selected = 'Todos'))
  
  
  formulaText0 <- reactive({
    paste(input$e0)
  })
  
  output$caption0 <- renderText({
    formulaText0()
  })
  
  formulaText1 <- reactive({
    paste(input$e1)
  })
  
  output$caption1 <- renderText({
    formulaText1()
  })
  
  formulaText2 <- reactive({
    paste(input$e2)
  })
  
  output$caption2 <- renderText({
    formulaText2()
  })
  
  formulaText3 <- reactive({
    paste(input$e3)
  })
  
  output$caption3 <- renderText({
    formulaText3()
  })
  
  formulaText4 <- reactive({
    paste(input$e4)
  })
  
  output$caption4 <- renderText({
    formulaText4()
  })
  
  
  # Sumario---------------------------------------------------------------
  output$tot_processos <- renderText({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroClasses <- as.character(formulaText2())
    filtroGrau <- as.character(formulaText3())
    filtroOrgao <- as.character(formulaText4())
    
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
    
    formatC(mc_log_resource %>% n_cases, format="f", big.mark = ".", decimal.mark = ",", digits=0)
  })
  
  output$tot_movimentos <- renderText({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroClasses <- as.character(formulaText2())
    filtroGrau <- as.character(formulaText3())
    filtroOrgao <- as.character(formulaText4())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
    
    formatC(mc_log_resource %>% n_activity_instances, format="f", big.mark = ".", decimal.mark = ",", digits=0)
  })
  
  output$tipos_movimentos <- renderText({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroClasses <- as.character(formulaText2())
    filtroGrau <- as.character(formulaText3())
    filtroOrgao <- as.character(formulaText4())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
    
    formatC(mc_log_resource %>% n_activities, format="f", big.mark = ".", decimal.mark = ",", digits=0)
  })
  
  output$tipos_fluxos <- renderText({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroClasses <- as.character(formulaText2())
    filtroGrau <- as.character(formulaText3())
    filtroOrgao <- as.character(formulaText4())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
    
    formatC(mc_log_resource %>% n_traces, format="f", big.mark = ".", decimal.mark = ",", digits=0)
  })
  
  output$perfis_mov <- renderTable({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroClasses <- as.character(formulaText2())
    filtroGrau <- as.character(formulaText3())
    filtroOrgao <- as.character(formulaText4())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
    
    mc_log_resource %>% resource_frequency("resource")
  })
  
  output$tempo_proc <- renderPlot({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroClasses <- as.character(formulaText2())
    filtroGrau <- as.character(formulaText3())
    filtroOrgao <- as.character(formulaText4())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
    
    prev <- mc_log_resource %>% throughput_time("log")
    prev %>% plot()
  })
  
  output$freq_mov <- renderPlot({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroClasses <- as.character(formulaText2())
    filtroGrau <- as.character(formulaText3())
    filtroOrgao <- as.character(formulaText4())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
    
    prev <- mc_log_resource %>% activity_frequency("activity")
    prev %>% plot()
  })
  
  
  #process_map-------------------------------------------------------------
  output$process_map <- renderUI({
    
    tagList(
      p("Mapa de frequência do processo (absoluta)"),
      renderGrViz({
        ##filtros
        filtroGrOrgaos <- as.character(formulaText0())
        filtroAssuntos <- as.character(formulaText1())
        filtroClasses <- as.character(formulaText2())
        filtroGrau <- as.character(formulaText3())
        filtroOrgao <- as.character(formulaText4())
        
        ##filtragem
        mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
        
        ##Gera??o do Log  
        mc_log_resource %>%
          process_map(type = frequency("absolute"))
      }),
      
      p("Mapa de frequência do processo (relativa)"),
      renderGrViz({
        ##filtros
        filtroGrOrgaos <- as.character(formulaText0())
        filtroAssuntos <- as.character(formulaText1())
        filtroClasses <- as.character(formulaText2())
        filtroGrau <- as.character(formulaText3())
        filtroOrgao <- as.character(formulaText4())
        
        ##filtragem
        mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
        
        ##Gera??o do Log  
        mc_log_resource %>%
          process_map(type = frequency("relative_case"))
      }),
      
      p("Mapa de frequência do processo (mediana em dias)"),
      renderGrViz({
        ##filtros
        filtroGrOrgaos <- as.character(formulaText0())
        filtroAssuntos <- as.character(formulaText1())
        filtroClasses <- as.character(formulaText2())
        filtroGrau <- as.character(formulaText3())
        filtroOrgao <- as.character(formulaText4())
        
        ##filtragem
        mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
        
        ##Gera??o do Log  
        mc_log_resource %>%
          process_map(performance(median, "days"))
      })
    )
  })
  
  
  # Animação de Processo-----------------------------------------------------
  output$process_animate <- renderUI({
    
    tagList(
      
      p("Mapa com animação"),
      
      renderProcessanimater({
        ##filtros
        filtroGrOrgaos <- as.character(formulaText0())
        filtroAssuntos <- as.character(formulaText1())
        filtroClasses <- as.character(formulaText2())
        filtroGrau <- as.character(formulaText3())
        filtroOrgao <- as.character(formulaText4())
        
        ##filtragem
        mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
        
        ##Gera??o do Log  
        animate_process(mc_log_resource, 
                        mode = "relative", 
                        duration = 300,
                        legend = "color", 
                        mapping = token_aes(color = token_scale(#lactic, 
                          scale = "linear", 
                          range = c("#fff5eb","#7f2704")))) 
      })
    )
  })
  
  #Fluxos_frequentes------------------------------------------------------
  output$Fluxos_frequentes <- renderUI({
    
    tagList(
      
      p("Mapa de frequência do processo (mediana em dias)"),
      renderPlot({
        ##filtros
        filtroGrOrgaos <- as.character(formulaText0())
        filtroAssuntos <- as.character(formulaText1())
        filtroClasses <- as.character(formulaText2())
        filtroGrau <- as.character(formulaText3())
        filtroOrgao <- as.character(formulaText4())
        
        ##filtragem
        mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
        
        ##Gera??o do Log  
        mc_log_resource %>% 
          trace_explorer(coverage = 0.4) %>%
          plot()
      })
    )
  })
  
  
  #Mapa_trabalho--------------------------------------------------------------
  output$Mapa_trabalho <- renderUI({
    
    tagList(
      
      p("Mapa de transferência de trabalho"),
      renderGrViz({
        ##filtros
        filtroGrOrgaos <- as.character(formulaText0())
        filtroAssuntos <- as.character(formulaText1())
        filtroClasses <- as.character(formulaText2())
        filtroGrau <- as.character(formulaText3())
        filtroOrgao <- as.character(formulaText4())
        
        ##filtragem
        mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
        
        ##Gera??o do Log  
        mc_log_resource %>%
          resource_map() 
      })
    )
  })
  
  
  #Fluxos_frequentes------------------------------------------------------
  output$Dependency_Tot <- #renderUI({
    
    #tagList(
    
    # p("Dependency Matrix s/Threshold"),
    renderGrViz({
      ##filtros
      filtroGrOrgaos <- as.character(formulaText0())
      filtroAssuntos <- as.character(formulaText1())
      filtroClasses <- as.character(formulaText2())
      filtroGrau <- as.character(formulaText3())
      filtroOrgao <- as.character(formulaText4())
      
      ##filtragem
      mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
      
      ##Geração do Log  
      mc_log_resource %>% dependency_matrix() %>% render_dependency_matrix() 
    })
  # )
  #})
  
  
  
  # INDUCTIVE MINER-------------------------------------------------------
  

  output$dep_matrix <- renderGrViz({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroClasses <- as.character(formulaText2())
    filtroGrau <- as.character(formulaText3())
    filtroOrgao <- as.character(formulaText4())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
    
    dependency_matrix(mc_log_resource) %>% render_dependency_matrix()
  })
  
  output$causal_net <- renderGrViz({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroClasses <- as.character(formulaText2())
    filtroGrau <- as.character(formulaText3())
    filtroOrgao <- as.character(formulaText4())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
    
    causal_net(mc_log_resource) %>% render_causal_net()
  })
  
  output$dep_matrix_filter1 <- renderGrViz({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroClasses <- as.character(formulaText2())
    filtroGrau <- as.character(formulaText3())
    filtroOrgao <- as.character(formulaText4())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
    
    dependency_matrix(mc_log_resource, threshold = .7) %>% render_dependency_matrix()
  })
  
  # Verificação de conformidade
  
  output$check_rules1 <- renderTable({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroClasses <- as.character(formulaText2())
    filtroGrau <- as.character(formulaText3())
    filtroOrgao <- as.character(formulaText4())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
    
    mc_log_resource %>%
      check_rule(starts("Distribuição"), label = "Distribuição") %>%
      check_rule(ends("Baixa Definitiva"), label = "Baixa_definitiva") %>%
      group_by(Distribui??o, Baixa_definitiva) %>%
      n_cases()
  })
  
  output$check_rules2 <- renderTable({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroClasses <- as.character(formulaText2())
    filtroGrau <- as.character(formulaText3())
    filtroOrgao <- as.character(formulaText4())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroClasses,filtroGrau,filtroOrgao)
    
    mc_log_resource %>%
      check_rule(contains_exactly("Distribuição", n = 1), label = "Distribuição") %>%
      check_rule(contains_exactly("Baixa Definitiva", n = 1), label = "Baixa_definitiva") %>%
      group_by(Distribui??o, Baixa_definitiva) %>%
      n_cases()
  })  
  
} # end of server

shinyApp(ui = ui, server = server)
