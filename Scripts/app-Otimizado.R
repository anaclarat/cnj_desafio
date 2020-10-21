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
#library(flipTime)
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

#Diretorio onde estão os arquivos-----------------------------------------------

diretorio <- 'D:/Inova/Dados-CNJ_disponibilizados/base/'

#-------------------------------------------------------------------------------

#setar diretorio
setwd(diretorio)

# 0.BASE DE DADOS
tribunal_orig <- NA

tribunal_orig <- read.csv2("./Arquivos/1/TRF2.csv", header=TRUE, sep=";")

tribunal_int <- NA

tribunal_int <- tribunal_orig %>%
  mutate(dadosBasicos.assunto.codigoNacional2 = dadosBasicos.assunto.codigoNacional) %>%
  mutate(dadosBasicos.assunto.descricao2 = dadosBasicos.assunto.descricao) %>%
  mutate(dadosBasicos.assunto.principal2 = dadosBasicos.assunto.principal) %>%
  mutate(dadosBasicos.classeProcessual2 = dadosBasicos.classeProcessual) %>%
  mutate(dadosBasicos.codigoLocalidade2 = dadosBasicos.codigoLocalidade) %>%
  mutate(dadosBasicos.competencia2 = dadosBasicos.competencia) %>%
  mutate(dadosBasicos.dataAjuizamento2 = dadosBasicos.dataAjuizamento) %>%
  mutate(dadosBasicos.descricaoclasseProcessual2 = dadosBasicos.descricaoclasseProcessual) %>%
  mutate(dadosBasicos.nivelSigilo2 = dadosBasicos.nivelSigilo) %>%
  mutate(dadosBasicos.numero2 = dadosBasicos.numero) %>%
  mutate(dadosBasicos.orgaoJulgador.codigoMunicipioIBGE2 = dadosBasicos.orgaoJulgador.codigoMunicipioIBGE) %>%
  mutate(dadosBasicos.orgaoJulgador.codigoOrgao2 = dadosBasicos.orgaoJulgador.codigoOrgao) %>%
  mutate(dadosBasicos.orgaoJulgador.instancia2 = dadosBasicos.orgaoJulgador.instancia) %>%
  mutate(dadosBasicos.orgaoJulgador.nomeOrgao2 = dadosBasicos.orgaoJulgador.nomeOrgao) %>%
  mutate(dadosBasicos.siglaclasse2 = dadosBasicos.siglaclasse) %>%
  mutate(dadosBasicos.valorCausa2 = dadosBasicos.valorCausa) %>%
  mutate(grau2 = grau) %>%
  mutate(IBGE.Nome_Mesorregiao2 = IBGE.Nome_Mesorregiao) %>%
  mutate(IBGE.Nome_Municipio2 = IBGE.Nome_Municipio) %>%
  mutate(millisInsercao2 = millisInsercao) %>%
  mutate(movimento.dataHora2 = movimento.dataHora) %>%
  mutate(movimento.movimentoNacional.codigoNacional2 = movimento.movimentoNacional.codigoNacional) %>%
  mutate(movimento.movimentoNacional.descricao2 = movimento.movimentoNacional.descricao) %>%
  mutate(siglaTribunal2 = siglaTribunal) %>%
  mutate(cluster2=cluster)

tribunal_int[1:ncol(tribunal_orig)] <- NULL

pg <- NA

pg <- tribunal_int

case_id <- NA
case_id <- paste(pg$'dadosBasicos.numero', "|", pg$'dadosBasicos.classeProcessual')
pg$'case_id' <- c(case_id)

# Ajustar os formatos de data/hora
x <- NA
x <- c(pg$'movimento.dataHora')
movimento.dataHora2 <- NA
movimento.dataHora2 <- ymd_hms(x)
pg$'movimento.dataHora2' <- c(movimento.dataHora2)
x <- NA
x <- c(pg$'dadosBasicos.dataAjuizamento2')
dadosBasicos.dataAjuizamento2 <- NA
dadosBasicos.dataAjuizamento2 <- ymd_hms(x)
pg$'dadosBasicos.dataAjuizamento2' <- c(dadosBasicos.dataAjuizamento2)

# Inserir tipos de responsÃ¡vel pelo movimento: magistrado ou servidor
tab_movimentos <- NA
tab_movimentos <- read.csv2("./Arquivos/movimentos_tipo_responsavel.csv", header=TRUE, sep=";")
head(tab_movimentos)
pg <- left_join(pg, tab_movimentos, #%>% select(codigo, tipo_responsavel_movimento),
                by = c("movimento.movimentoNacional.codigoNacional2" = "codigo"))

mc <- NA
mc <- pg

mc_log <- mc %>%
  # Considerando que todos os movimentos sÃ£o registros de atividades jÃ¡
  #   concluÃdas, o cÃ³digo abaixo adiciona o status "complete" para o atributo
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



mc_resource <- mc_log

#Criação dos choices com grupo total
trib_gr_orgaos <- tribunal_int
tab_gr_orgaos <- unique(trib_gr_orgaos$cluster2)  %>% sort()
tab_gr_orgaos <- c('Todos',tab_gr_orgaos)

mc_log_resource <- mc_resource

#Função filtro ###########################################

filtroGrOrgaos <- 'Todos'

GerarLog0 <- function(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao){
  
  ##Grupo de örgãos
  if (filtroGrOrgaos!= 'Todos'){
    mc_log_resource  <- mc_resource %>% filter(cluster2 == as.character(filtroGrOrgaos))
  }else{
    mc_log_resource <- mc_resource %>%
      # Considerando que todos os movimentos sÃ£o registros de atividades jÃ¡
      #   concluÃdas, o cÃ³digo abaixo adiciona o status "complete" para o atributo
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
    
  }
  
  ##Assunto
  if (filtroAssuntos!= 'Todos'){
    mc_log_resource  <- mc_log_resource %>% filter(dadosBasicos.assunto.descricao2 == filtroAssuntos)
  }
  else{
    mc_log_resource <- mc_resource %>%
      # Considerando que todos os movimentos sÃ£o registros de atividades jÃ¡
      #   concluÃdas, o cÃ³digo abaixo adiciona o status "complete" para o atributo
      #   lifecycle_id.
      mutate(status = "complete",
             activity_instance = 1:nrow(.)) %>%
      eventlog(
        case_id = "case_id",
        activity_id = "movimento.movimentoNacional.descricao2",
        activity_instance_id = "activity_instance",
        lifecycle_id = "status",
        timestamp = "movimento.dataHora2",
        resource_id = "tipo_responsavel_movimento")
  }
  
  ##Grau
  if (filtroGrau != 'Todos'){
    mc_log_resource  <- mc_log_resource %>% filter(grau2 == filtroGrau )
  }
  else{
    mc_log_resource <- mc_resource %>%
      # Considerando que todos os movimentos sÃ£o registros de atividades jÃ¡
      #   concluÃdas, o cÃ³digo abaixo adiciona o status "complete" para o atributo
      #   lifecycle_id.
      mutate(status = "complete",
             activity_instance = 1:nrow(.)) %>%
      eventlog(
        case_id = "case_id",
        activity_id = "movimento.movimentoNacional.descricao2",
        activity_instance_id = "activity_instance",
        lifecycle_id = "status",
        timestamp = "movimento.dataHora2",
        resource_id = "tipo_responsavel_movimento")
  }
  
  ##Órgão judiciário
  if (filtroOrgao != 'Todos'){
    mc_log_resource  <- mc_log_resource %>% filter(dadosBasicos.orgaoJulgador.nomeOrgao2 == filtroOrgao )
  }
  else{
    mc_log_resource <- mc_resource %>%
      # Considerando que todos os movimentos sÃ£o registros de atividades jÃ¡
      #   concluÃdas, o cÃ³digo abaixo adiciona o status "complete" para o atributo
      #   lifecycle_id.
      mutate(status = "complete",
             activity_instance = 1:nrow(.)) %>%
      eventlog(
        case_id = "case_id",
        activity_id = "movimento.movimentoNacional.descricao2",
        activity_instance_id = "activity_instance",
        lifecycle_id = "status",
        timestamp = "movimento.dataHora2",
        resource_id = "tipo_responsavel_movimento")
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
         textOutput("caption3")),
      selectInput(
        'e0', '0. Selecione o grupo de órgãos julgadores', choices = tab_gr_orgaos, selected = 'Todos',
        selectize = FALSE
      ),
      selectInput(
        'e1', '1. Selecione o assunto', choices = "",selected = "" ,
        selectize = FALSE
      ),
      selectInput(
        'e2', '2. Selecione o grau', choices = "",selected = "" ,
        selectize = FALSE
      ),
      selectInput(
        'e3', '3. Selecione a unidade judiciária', choices = "",selected = "" ,
        selectize = FALSE
      ),
      
      actionButton(inputId = "reload_eventlog",
                   label = "Reload eventlog")
      
    ),
    
    mainPanel(
      # titlePanel(textOutput("caption")),
      tabsetPanel(type = "pills",
                  
                  tabPanel(title = "Instruções",
                           br(), textOutput("Instruções")),
                  
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
                           br(), uiOutput("Dependency_Tot")),
                  
                  tabPanel(title = "Sequence analysis",
                           br(),uiOutput("sequence_analysis")),
                  
                  tabPanel(title = "Resource analysis",
                           uiOutput("resource_analysis")),
                  
                  tabPanel(title = "Top users' actions",
                           uiOutput("top_users_actions"))
                  
      )#,
      # h3(textOutput("caption"))
      #,grVizOutput("process_map")
    )
  )
) # end of ui

# SERVER ######################################################################
server <- function(input, output, session) {
  
  #   #Interação com usuário
  #   #########################
  #   N <- 10
  #   
  #   result_val <- reactiveVal()
  #   observeEvent(input$run,{
  #     result_val(NULL)
  #     future({
  #       print("Running...")
  #       for(i in 1:N){
  #         Sys.sleep(1)
  #       }
  #       quantile(rnorm(1000))
  #     }) %...>% result_val()
  #   })
  #   output$result <- renderTable({
  #     req(result_val())
  #   })
  # }
  #   
  
  #Reset botões
  #############
  
  #Botões laterais
  ########################  
  
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
  #Grau
  observeEvent(
    input$e1,
    updateSelectInput(session, 'e2', '2. Selecione o grau',
                      choices = 
                        #   if (input$e0 == 'Todos'){
                        #   sort(unique(c(tribunal_int$grau2,'Todos')))
                        # }else 
                        if(input$e1 == 'Todos'){
                          sort(unique(c(tribunal_int$grau2,'Todos')))
                        } else{
                          sort(unique(c(tribunal_int$grau2[tribunal_int$dadosBasicos.assunto.descricao2==input$e1
                                                           & tribunal_int$cluster2==input$e0],'Todos')))
                          
                        },
                      selected = 'Todos'))
  
  #Órgão Julgador
  observeEvent(
    input$e2,
    updateSelectInput(session, 'e3', '3. Selecione a unidade judiciária',
                      choices = 
                        #   if (input$e0 == 'Todos'){
                        #   sort(unique(c(tribunal_int$grau2,'Todos')))
                        # }else 
                        if(input$e2 == 'Todos'){
                          sort(unique(c(tribunal_int$dadosBasicos.orgaoJulgador.nomeOrgao2,'Todos')))
                        } else{
                          sort(unique(c(tribunal_int$dadosBasicos.orgaoJulgador.nomeOrgao2[
                            tribunal_int$grau2==input$e2 &
                              tribunal_int$dadosBasicos.assunto.descricao2==input$e1 & 
                              tribunal_int$cluster2==input$e0 
                          ],'Todos')))
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
  
  
  # Sumario---------------------------------------------------------------
  output$tot_processos <- renderText({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroGrau <- as.character(formulaText2())
    filtroOrgao <- as.character(formulaText3())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
    
    formatC(mc_log_resource %>% n_cases, format="f", big.mark = ".", decimal.mark = ",", digits=0)
  })
  
  output$tot_movimentos <- renderText({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroGrau <- as.character(formulaText2())
    filtroOrgao <- as.character(formulaText3())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
    
    formatC(mc_log_resource %>% n_activity_instances, format="f", big.mark = ".", decimal.mark = ",", digits=0)
  })
  
  output$tipos_movimentos <- renderText({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroGrau <- as.character(formulaText2())
    filtroOrgao <- as.character(formulaText3())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
    
    formatC(mc_log_resource %>% n_activities, format="f", big.mark = ".", decimal.mark = ",", digits=0)
  })
  
  output$tipos_fluxos <- renderText({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroGrau <- as.character(formulaText2())
    filtroOrgao <- as.character(formulaText3())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
    
    formatC(mc_log_resource %>% n_traces, format="f", big.mark = ".", decimal.mark = ",", digits=0)
  })
  
  output$perfis_mov <- renderTable({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroGrau <- as.character(formulaText2())
    filtroOrgao <- as.character(formulaText3())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
    
    mc_log_resource %>% resource_frequency("resource")
  })
  
  output$tempo_proc <- renderPlot({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroGrau <- as.character(formulaText2())
    filtroOrgao <- as.character(formulaText3())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
    
    prev <- mc_log_resource %>% throughput_time("log")
    prev %>% plot()
  })
  
  output$freq_mov <- renderPlot({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroGrau <- as.character(formulaText2())
    filtroOrgao <- as.character(formulaText3())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
    
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
        filtroGrau <- as.character(formulaText2())
        filtroOrgao <- as.character(formulaText3())
        
        ##filtragem
        mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
        
        ##Geração do Log  
        mc_log_resource %>%
          process_map(type = frequency("absolute"))
      }),
      
      p("Mapa de frequência do processo (relativa)"),
      renderGrViz({
        ##filtros
        filtroGrOrgaos <- as.character(formulaText0())
        filtroAssuntos <- as.character(formulaText1())
        filtroGrau <- as.character(formulaText2())
        filtroOrgao <- as.character(formulaText3())
        
        ##filtragem
        mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
        
        ##Geração do Log  
        mc_log_resource %>%
          process_map(type = frequency("relative_case"))
      }),
      
      p("Mapa de frequência do processo (mediana em dias)"),
      renderGrViz({
        ##filtros
        filtroGrOrgaos <- as.character(formulaText0())
        filtroAssuntos <- as.character(formulaText1())
        filtroGrau <- as.character(formulaText2())
        filtroOrgao <- as.character(formulaText3())
        
        ##filtragem
        mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
        
        ##Geração do Log  
        mc_log_resource %>%
          process_map(performance(median, "days"))
      })
    )
  })
  
  
  # Animação de Processo-----------------------------------------------------
  output$process_animate <- renderUI({
    
    tagList(
      
      p("Mapa com animação"),
      p(paste(as.character(formulaText0()),' ',as.character(formulaText1()),' ', 
              as.character(formulaText2()),' ',as.character(formulaText3()))),
      
      renderProcessanimater({
        ##filtros
        filtroGrOrgaos <- as.character(formulaText0())
        filtroAssuntos <- as.character(formulaText1())
        filtroGrau <- as.character(formulaText2())
        filtroOrgao <- as.character(formulaText3())
        
        ##filtragem
        mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
        
        ##Geração do Log  
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
        filtroGrau <- as.character(formulaText2())
        filtroOrgao <- as.character(formulaText3())
        
        ##filtragem
        mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
        
        ##Geração do Log  
        mc_log_resource %>% 
          trace_explorer(coverage = 0.4) %>%
          plot()
      })
    )
  })
  
  ########################Não renderiza!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #Mapa_trabalho--------------------------------------------------------------
  output$Mapa_trabalho <- renderUI({
    
    tagList(
      
      p("Mapa de transferência de trabalho"),
      renderGrViz({
        ##filtros
        filtroGrOrgaos <- as.character(formulaText0())
        filtroAssuntos <- as.character(formulaText1())
        filtroGrau <- as.character(formulaText2())
        filtroOrgao <- as.character(formulaText3())
        
        ##filtragem
        mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
        
        ##Geração do Log  
        mc_log_resource %>%
          resource_map() 
      })
    )
  })
  
  
  #Fluxos_frequentes------------------------------------------------------
  output$Dependency_Tot <- renderUI({
    
    tagList(
      
      p("Dependency Matrix s/Threshold"),
      renderGrViz({
        ##filtros
        filtroGrOrgaos <- as.character(formulaText0())
        filtroAssuntos <- as.character(formulaText1())
        filtroGrau <- as.character(formulaText2())
        filtroOrgao <- as.character(formulaText3())
        
        ##filtragem
        mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
        
        ##Geração do Log  
        mc_log_resource %>% dependency_matrix() %>% render_dependency_matrix() 
      })
    )
  })
  
  
  
  # INDUCTIVE MINER-------------------------------------------------------
  
  library(heuristicsmineR)
  library(petrinetR)
  
  output$dep_matrix <- renderGrViz({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroGrau <- as.character(formulaText2())
    filtroOrgao <- as.character(formulaText3())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
    
    dependency_matrix(mc_log_resource) %>% render_dependency_matrix()
  })
  
  output$causal_net <- renderGrViz({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroGrau <- as.character(formulaText2())
    filtroOrgao <- as.character(formulaText3())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
    
    causal_net(mc_log_resource) %>% render_causal_net()
  })
  
  output$dep_matrix_filter <- renderGrViz({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroGrau <- as.character(formulaText2())
    filtroOrgao <- as.character(formulaText3())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
    
    dependency_matrix(mc_log_resource, threshold = .7) %>% render_dependency_matrix()
  })
  
  # Verificaçãoo de conformidade
  
  output$check_rules1 <- renderTable({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroGrau <- as.character(formulaText2())
    filtroOrgao <- as.character(formulaText3())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
    
    mc_log_resource %>%
      check_rule(starts("Distribuição"), label = "Distribuição") %>%
      check_rule(ends("Baixa Definitiva"), label = "Baixa_definitiva") %>%
      group_by(Distribuição, Baixa_definitiva) %>%
      n_cases()
  })
  
  output$check_rules2 <- renderTable({
    ##filtros
    filtroGrOrgaos <- as.character(formulaText0())
    filtroAssuntos <- as.character(formulaText1())
    filtroGrau <- as.character(formulaText2())
    filtroOrgao <- as.character(formulaText3())
    
    ##filtragem
    mc_log_resource <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
    
    mc_log_resource %>%
      check_rule(contains_exactly("Distribuiçãoo", n = 1), label = "Distribuição") %>%
      check_rule(contains_exactly("Baixa Definitiva", n = 1), label = "Baixa_definitiva") %>%
      group_by(Distribuição, Baixa_definitiva) %>%
      n_cases()
  })  
  
  
  
  
  
  # eventlog_unfiltered ------------------------------------------------------
  eventlog_unfiltered <- reactive({
    
    input$reload_eventlog
    
    if (file.exists(".db_url")) {
      
      message("Loading data from mongoDB...")
      data <- read_eventlog(db = readLines(".db_url")[1],
                            verbose  = FALSE)
      
    } else {
      
      message("Loading data from a filelog...")
      data <- read_eventlog(
        verbose = FALSE,
        file = system.file("shiny", "demoapp/events.log",
                           package = "shinyEventLogger")
      )
      
    }
    
  })
  
  # eventlog -----------------------------------------------------------------
  eventlog <- reactive({
    
    req(input$build_version)
    
    eventlog_unfiltered() %>%
      filter(between(build,
                     input$build_version[1],
                     input$build_version[2]))
    
  })
  
  # eventlog_summary ---------------------------------------------------------
  output$eventlog_summary <- renderUI({
    
    eventlog <- eventlog_unfiltered()
    min_build <- min(eventlog$build, na.rm = TRUE)
    max_build <- max(eventlog$build, na.rm = TRUE)
    
    tagList(
      
      p("There are ", strong(n_cases(eventlog)), " cases, ",
        strong(n_activities(eventlog)), " activities,", br(),
        " and ", strong(n_events(eventlog)), "events in the event log."),
      
      sliderInput(inputId = "build_version",
                  label = "Events from DemoApp build version",
                  min = min_build,
                  max = max_build,
                  value = c(max_build, max_build),
                  step = 1, ticks = FALSE))
    
  })
  
  # eventlog_summary_filtered ------------------------------------------------
  output$eventlog_summary_filtered <- renderUI({
    
    eventlog <- eventlog()
    
    p("Currently, we are using data from ",
      strong(n_cases(eventlog)), " cases, ", br(),
      strong(n_activities(eventlog)), " activities,",
      " and ", strong(n_events(eventlog)), "events.")
    
  })
  
  # top_users_actions ---------------------------------------------------------
  output$top_users_actions <- renderUI({
    
    plot_height <- 250
    
    tagList(
      
      renderPlot(height = plot_height, {
        
        data <-
          eventlog() %>%
          filter(event_name == "Dataset was selected" & !is.na(dataset)) %>%
          count(dataset) %>%
          arrange(desc(n)) %>%
          mutate(n = if_else(dataset == "iris", n - n_cases(eventlog()), n))
        
        barplot(data$n,
                names.arg = data$dataset,
                col = 'darkgray',
                border = 'white',
                main = "Dataset most often selected*",
                sub = "(*) without iris dataset selected by default")
        
      }), hr(),
      
      renderPlot(height = plot_height, {
        
        data <-
          eventlog() %>%
          filter(event_name == "input$variable" & output != "") %>%
          filter(!is.na(dataset)) %>%
          count(event_body, dataset) %>%
          arrange(desc(n)) %>%
          mutate(
            n = if_else(
              event_body == "Sepal.Length", n - n_cases(eventlog()), n)
          )
        
        barplot(data$n,
                names.arg = paste0(data$event_body, " (", data$dataset, ")"),
                col = 'darkgray',
                border = 'white',
                main = "Variables most often selected*",
                sub = "(*) without Sepal.Length selected by default")
        
      }), hr(),
      
      renderPlot(height = plot_height, {
        
        data <-
          eventlog() %>%
          filter(event_name == "input$bins") %>%
          count(event_body) %>%
          mutate(event_body = as.integer(event_body)) %>%
          arrange(event_body) %>%
          mutate(n = if_else(event_body == 10, n - n_cases(eventlog()), n))
        
        barplot(data$n,
                names.arg = data$event_body,
                col = 'darkgray',
                border = 'white',
                main = "Number of bins most often selected*",
                sub = "(*) without 10 bins selected by default")
        
      }), hr(), br()
      
    )
    
  })
  
  
  # unit_tests ---------------------------------------------------------------
  output$unit_tests <- renderTable({
    
    eventlog() %>%
      filter(event_type == "TEST") %>%
      count(event_type, event_name, event_status,
            variable, bins, fun, resource) %>%
      arrange(event_status, event_name, variable, bins)
    
  })
  
  # top_traces ---------------------------------------------------------------
  output$top_traces <- renderPlot(height = 400, {
    
    eventlog() %>%
      # group_activites(act = "collapse") %>%
      group_activites(act = "unite") %>%
      # trace_explorer(coverage = 0.5)
      trace_explorer(coverage = 1)
    
  })
  
  # time_analysis -------------------------------------------------------------
  output$time_analysis <- renderUI({
    
    plot_height <- 350
    
    fluidPage(
      
      fluidRow(
        column(width = 6,
               
               br(), p("Throughput time per dataset"),
               renderPlot(height = plot_height, {
                 
                 eventlog() %>%
                   filter(!is.na(dataset)) %>%
                   group_by(dataset) %>%
                   throughput_time(level = "log", units = "mins") %>%
                   plot()
                 
               })
               
        ),
        column(width = 6,
               
               br(), p("Idle time per dataset"),
               renderPlot(height = plot_height, {
                 
                 eventlog() %>%
                   filter(!is.na(dataset)) %>%
                   group_by(dataset) %>%
                   idle_time(level = "log", units = "mins") %>%
                   plot()
                 
               })
        )
      ),
      
      fluidRow(
        column(width = 6, offset = 0,
               
               br(), p("Processing time per dataset"),
               renderPlot(height = plot_height, {
                 
                 eventlog() %>%
                   filter(!is.na(dataset)) %>%
                   group_by(dataset) %>%
                   processing_time(level = "log", units = "mins") %>%
                   plot()
                 
               })
               
        ),
        column(width = 6, offset = 0,
               
               br(), p("Dotted chart of cases"),
               renderPlot(height = plot_height, {
                 
                 eventlog() %>%
                   group_activites() %>%
                   dotted_chart(units = "mins",
                                x = "relative",
                                sort = "duration")
                 
               })
               
        )
      )
    )
  })
  
  # sequence_analysis --------------------------------------------------------
  output$sequence_analysis <- renderUI({
    
    plot_height <- 350
    
    fluidPage(
      fluidRow(
        column(width = 8, offset = 2,
               
               br(), p("Precedence matrix"),
               renderPlot(height = plot_height, {
                 
                 eventlog() %>%
                   group_activites(act = "unite") %>%
                   precedence_matrix(type = "relative") %>%
                   plot()
                 
               })
               
        )
      ),
      
      fluidRow(
        column(width = 6,
               
               br(), p("Repetitions"),
               renderPlot(height = plot_height * 0.6, {
                 
                 eventlog() %>%
                   group_activites(act = "unite") %>%
                   number_of_repetitions(level = "activity") %>%
                   plot()
                 
               })
        ),
        column(width = 6,
               
               br(), p("Selfloops"),
               renderPlot(height = plot_height * 0.6, {
                 
                 eventlog() %>%
                   group_activites(act = "unite") %>%
                   number_of_selfloops(level = "activity") %>%
                   plot()
                 
               })
        ),
        column(width = 6,
               
               br(), p("End activities (grouped)"),
               renderPlot(height = plot_height * 0.4, {
                 
                 eventlog() %>%
                   group_activites(act = "unite") %>%
                   end_activities(level = "activity") %>%
                   plot()
                 
               })
        ),
        column(width = 6,
               
               br(), p("End activities (ungrouped)"),
               renderPlot(height = plot_height * 0.4, {
                 
                 eventlog() %>%
                   # group_activites(act = "unite") %>%
                   end_activities(level = "activity") %>%
                   plot()
                 
               })
        )
      )
    )
  })
  
  # resource_analysis --------------------------------------------------------
  output$resource_analysis <- renderUI({
    
    plot_height <- 600
    
    fluidRow(
      column(width = 6,
             
             br(),
             renderPlot(height = plot_height, {
               
               eventlog() %>%
                 filter(!is.na(fun), !is.na(resource)) %>%
                 group_by(fun) %>%
                 resource_frequency(level = "resource") %>%
                 plot() + ggplot2::theme(
                   legend.position = "bottom",
                   axis.title = ggplot2::element_blank()
                 )
               
             })
      ),
      column(width = 6,
             
             br(), renderPlot(height = plot_height, {
               
               eventlog() %>%
                 filter(!is.na(fun), !is.na(resource)) %>%
                 group_by(fun) %>%
                 resource_involvement(level = "resource") %>%
                 plot() + ggplot2::theme(
                   legend.position = "bottom",
                   axis.title = ggplot2::element_blank()
                 )
             })
      )
    )
  })
  
} # end of server

shinyApp(ui = ui, server = server)
