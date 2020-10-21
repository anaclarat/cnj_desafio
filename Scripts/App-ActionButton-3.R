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

#Diretorio onde estão os arquivos
diretorio <- 'C:/Users/edw_s/code/cnj_inova/Dados-CNJ_disponibilizados/base/'

#setar diretorio
setwd(diretorio)

# 0.BASE DE DADOS
tribunal_orig <- NA

tribunal_orig <- read.csv2("./Arquivos/1/TRF2.csv", header=TRUE, sep=";")

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

# Adicionar case_id
case_id <- NA
case_id <- paste(tribunal_int$'dadosBasicos.numero', "|", tribunal_int$'dadosBasicos.descricaoclasseProcessual2')
tribunal_int$case_id <- c(case_id)
tribunal_int$dadosBasicos.numero <- NULL

# Filtra o dataframe para somente processos baixados e inclui campo de duração
# do processo
proc_baixados <- NA
proc_baixados <- tribunal_int %>% filter(movimento.movimentoNacional.codigoNacional2 == "22")
lista_baixados <- NA
lista_baixados <- proc_baixados$case_id
# XX Implantar o filtro de baixados
pg$movimento.movimentoNacional.codigoNacional2 <- NULL

pg <- NA

pg <- tribunal_int

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

# Inserir tipos de responsável pelo movimento: magistrado e perfis de servidor
tab_movimentos <- NA
tab_movimentos <- read.csv2("./Arquivos/movimentos_tipo_responsavel.csv", header=TRUE, sep=";")
head(tab_movimentos)
pg <- left_join(pg, tab_movimentos, #%>% select(codigo, tipo_responsavel_movimento),
                by = c("movimento.movimentoNacional.codigoNacional2" = "codigo"))

mc <- NA

mc <- pg

mc_log <- mc %>%
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

mc_resource <- pg %>% filter(dadosBasicos.orgaoJulgador.nomeOrgao=="8ª Vara de Execução Fiscal do Rio de Janeiro")

# mc_resource <- pg

#Criação dos choices com grupo total
trib_gr_orgaos <- tribunal_int
tab_gr_orgaos <- unique(trib_gr_orgaos$cluster2)  %>% sort()
tab_gr_orgaos <- c('Todos',tab_gr_orgaos)

mc_log_resource <- data.frame()

#Função filtro ###########################################

filtroGrOrgaos <- 'Todos'

GerarLog0 <- function(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao){
  
  ##Grupo de örgãos
  if (filtroGrOrgaos!= 'Todos'){
    mc_resource  <- mc_resource %>% filter(cluster2 == as.character(filtroGrOrgaos))
    
    # mc_log_resource <- mc_resource %>%
    #   mutate(status = "complete",
    #          activity_instance = 1:nrow(.)) %>%
    #   eventlog(
    #     case_id = "case_id",
    #     activity_id = "movimento.movimentoNacional.descricao2",
    #     activity_instance_id = "activity_instance",
    #     lifecycle_id = "status",
    #     timestamp = "movimento.dataHora2",
    #     resource_id = "tipo_responsavel_movimento")
    
  }else{
    # mc_log_resource <- mc_resource %>%
    #   mutate(status = "complete",
    #          activity_instance = 1:nrow(.)) %>%
    #   eventlog(
    #     case_id = "case_id",
    #     activity_id = "movimento.movimentoNacional.descricao2",
    #     activity_instance_id = "activity_instance",
    #     lifecycle_id = "status",
    #     timestamp = "movimento.dataHora2",
    #     resource_id = "tipo_responsavel_movimento"
    #   )
  }
  
  ##Assunto
  if (filtroAssuntos!= 'Todos'){
    # mc_log_resource <- as.data.frame(mc_log_resource)

    mc_resource  <- mc_resource %>% filter(dadosBasicos.assunto.descricao2 == filtroAssuntos)

    # mc_log_resource <- mc_log_resource %>%
    #   mutate(status = "complete",
    #          activity_instance = 1:nrow(.)) %>%
    #   eventlog(
    #     case_id = "case_id",
    #     activity_id = "movimento.movimentoNacional.descricao2",
    #     activity_instance_id = "activity_instance",
    #     lifecycle_id = "status",
    #     timestamp = "movimento.dataHora2",
    #     resource_id = "tipo_responsavel_movimento")

  }else{
    # mc_log_resource <- mc_log_resource %>%
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
    # mc_log_resource <- as.data.frame(mc_log_resource)

    mc_resource  <- mc_resource %>% filter(grau2 == filtroGrau )

    # mc_log_resource <- mc_log_resource %>%
    #   mutate(status = "complete",
    #          activity_instance = 1:nrow(.)) %>%
    #   eventlog(
    #     case_id = "case_id",
    #     activity_id = "movimento.movimentoNacional.descricao2",
    #     activity_instance_id = "activity_instance",
    #     lifecycle_id = "status",
    #     timestamp = "movimento.dataHora2",
    #     resource_id = "tipo_responsavel_movimento")
  }else{
    # mc_log_resource <- mc_log_resource %>%
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
  
  ##Órgão judiciário
  if (filtroOrgao != 'Todos'){
    # mc_log_resource <- as.data.frame(mc_log_resource)

    mc_resource <- mc_resource %>% filter(dadosBasicos.orgaoJulgador.nomeOrgao2 == filtroOrgao )

    # mc_log_resource <- mc_log_resource %>%
    #   mutate(status = "complete",
    #          activity_instance = 1:nrow(.)) %>%
    #   eventlog(
    #     case_id = "case_id",
    #     activity_id = "movimento.movimentoNacional.descricao2",
    #     activity_instance_id = "activity_instance",
    #     lifecycle_id = "status",
    #     timestamp = "movimento.dataHora2",
    #     resource_id = "tipo_responsavel_movimento")
  }else{
    # mc_log_resource <- mc_log_resource %>%
  #     mutate(status = "complete",
  #            activity_instance = 1:nrow(.)) %>%
  #     eventlog(
  #       case_id = "case_id",
  #       activity_id = "movimento.movimentoNacional.descricao2",
  #       activity_instance_id = "activity_instance",
  #       lifecycle_id = "status",
  #       timestamp = "movimento.dataHora2",
  #       resource_id = "tipo_responsavel_movimento")
  }

  return(mc_resource)
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
        selectize = TRUE
      ),
      
      selectInput(
        'e1', '1. Selecione o assunto', choices = "",selected = "" ,
         selectize = TRUE
      ),
      selectInput(
        'e2', '2. Selecione o grau', choices = "",selected = "" ,
         selectize = TRUE
      ),
      selectInput(
        'e3', '3. Selecione a unidade judiciária', choices = "",selected = "" ,
         selectize = TRUE
      ),
      
      actionButton("botaoPesquisar", "Pesquisar")
      
    ),
    
    mainPanel(
      # titlePanel(textOutput("caption")),
      tabsetPanel(type = "pills",
                  
                  tabPanel(title = "Sumário de Informações",
                           br(),
                           p("Total de Processos: "),
                           p(textOutput("tot_processos")),
                           p("Total de Movimentos: "),
                           p(textOutput("tot_movimentos"),
                           p("Tipos de Movimentos: "),
                           p(textOutput("tipos_movimentos")),
                           p("Diferentes Fluxos: "),
                           p(textOutput("tipos_fluxos")),
                           h4("Perfis de responsáveis pelos movimentos"),
                           tableOutput("perfis_mov")
                           # h5("Tempo de Processamento"),
                           # plotOutput("tempo_proc"),
                           # h5("Frequência de movimentos"),
                           # plotOutput("freq_mov")
                  ),
                  
                  
                  # tabPanel(title = "Process map",
                  #          br(), uiOutput("process_map")),
                  # 
                  # 
                  # tabPanel(title = "Mapa com animação",
                  #          br(), uiOutput("process_animate")),
                  # 
                  # tabPanel(title = "Top traces",
                  #          plotOutput("top_traces")),
                  # 
                  # tabPanel(title = "Time analysis",
                  #          uiOutput("time_analysis")),
                  # 
                  # tabPanel(title = "Sequence analysis",
                  #          uiOutput("sequence_analysis")),
                  # 
                  # tabPanel(title = "Resource analysis",
                  #          uiOutput("resource_analysis")),
                  # 
                  # tabPanel(title = "Top users' actions",
                  #          uiOutput("top_users_actions"))
                  
      )#,
      # h3(textOutput("caption"))
      #,grVizOutput("process_map")
    )
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
  
  #process_map-------------------------------------------------------------
  output$process_map <- renderUI({
    
    tagList(
      p(paste(as.character(formulaText0()),' ',as.character(formulaText1()),' ', 
              as.character(formulaText2()),' ',as.character(formulaText3()))),
      
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
  
  
  
  # Sumario---------------------------------------------------------------
    # output$Sumario <- renderText({
    #   formulaText <- reactive({
    #     paste(input$e0)
    #   })

    mostra_sumario <- eventReactive(input$botaoPesquisar, {
      filtroGrOrgaos <- as.character(formulaText0())
      filtroAssuntos <- as.character(formulaText1())
      filtroGrau <- as.character(formulaText2())
      filtroOrgao <- as.character(formulaText3())
      #filtragem
      mc_resource_filtrado <- GerarLog0(filtroGrOrgaos,filtroAssuntos,filtroGrau,filtroOrgao)
      
      mc_log_resource <- mc_resource_filtrado %>%
        mutate(status = "complete",
               activity_instance = 1:nrow(.)) %>%
        eventlog(
          case_id = "case_id",
          activity_id = "movimento.movimentoNacional.descricao2",
          activity_instance_id = "activity_instance",
          lifecycle_id = "status",
          timestamp = "movimento.dataHora2",
          resource_id = "tipo_responsavel_movimento")
    })

    # output$caption <- renderText({
    #   formulaText()
    # })
    
    output$tot_processos <- renderText({
      # filtroclasse <- as.character(mostra_sumario())
      # mc_log_resource <- GerarLog(filtroclasse)
      # mc_log_resource <- mostra_sumario
      formatC(mostra_sumario %>% n_cases, format="f", big.mark = ".", decimal.mark = ",", digits=0)
    })
    
    output$tot_movimentos <- renderText({
      # filtroclasse <- as.character(mostra_sumario())
      # mc_log_resource <- GerarLog(filtroclasse)
      mc_log_resource <- mostra_sumario
      formatC(mc_log_resource %>% n_activity_instances, format="f", big.mark = ".", decimal.mark = ",", digits=0)
    })
    
    output$tipos_movimentos <- renderText({
      # filtroclasse <- as.character(mostra_sumario())
      # mc_log_resource <- GerarLog(filtroclasse)
      mc_log_resource <- mostra_sumario
      formatC(mc_log_resource %>% n_activities, format="f", big.mark = ".", decimal.mark = ",", digits=0)
    })
    
    output$tipos_fluxos <- renderText({
      # filtroclasse <- as.character(mostra_sumario())
      # mc_log_resource <- GerarLog(filtroclasse)
      mc_log_resource <- mostra_sumario
      formatC(mc_log_resource %>% n_traces, format="f", big.mark = ".", decimal.mark = ",", digits=0)
    })
    
    output$perfis_mov <- renderTable({
      filtroclasse <- as.character(mostra_sumario())
      mc_log_resource <- GerarLog(filtroclasse)
      mc_log_resource %>% resource_frequency("resource")
    })
    
    # output$tempo_proc <- renderPlot({
    #   filtroclasse <- as.character(mostra_sumario())
    #   mc_log_resource <- GerarLog(filtroclasse)
    #   prev <- mc_log_resource %>% throughput_time("log")
    #   prev %>% plot()
    # })
    # 
    # output$freq_mov <- renderPlot({
    #   filtroclasse <- as.character(mostra_sumario())
    #   mc_log_resource <- GerarLog(filtroclasse)
    #   prev <- mc_log_resource %>% activity_frequency("activity")
    #   prev %>% plot()
    # })
    
  # })
  
  
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
  
  
  
  
  
  
  
  # eventlog_unfiltered ------------------------------------------------------
  # eventlog_unfiltered <- reactive({
  #   
  #   input$reload_eventlog
  #   
  #   if (file.exists(".db_url")) {
  #     
  #     message("Loading data from mongoDB...")
  #     data <- read_eventlog(db = readLines(".db_url")[1],
  #                           verbose  = FALSE)
  #     
  #   } else {
  #     
  #     message("Loading data from a filelog...")
  #     data <- read_eventlog(
  #       verbose = FALSE,
  #       file = system.file("shiny", "demoapp/events.log",
  #                          package = "shinyEventLogger")
  #     )
  #     
  #   }
  #   
  # })
  
  # eventlog -----------------------------------------------------------------
  # eventlog <- reactive({
  #   
  #   req(input$build_version)
  #   
  #   eventlog_unfiltered() %>%
  #     filter(between(build,
  #                    input$build_version[1],
  #                    input$build_version[2]))
  #   
  # })
  
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




#######################################################
#Script Original
# ######################################################
# library(shiny)
# library(shinyEventLogger)
# 
# library(dplyr)
# library(DiagrammeR)
# 
# library(bupaR)
# library(processmapR)
# 
# group_activites <- function(data, act = c("unite", "collapse")) {
#   
#   act_type <-
#     if (act[1] == "unite") {
#       act_unite
#     } else if (act[1] == "collapse") {
#       act_collapse
#     }
#   
#   data %>%
#     act_type(
#       "Bins selected" =
#         c("Very close to 50 bins!", "50 bins are comming...",
#           "Number of bins are safe",
#           "testthat::expect_lt(input$bins, 50)",
#           "50 bins are not allowed!",
#           "input$bins"),
#       "Dataset selected" =
#         c("NROW(dataset)", "head(dataset)", "input$dataset", "str(dataset)",
#           "Loading dataset",
#           "Dataset was selected"),
#       "Variable selected" =
#         c("input$variable", "Variable was selected"),
#       "Plotting histogram" =
#         c("testthat::expect_is(x, `numeric`)", "Plotting histogram")
#     )
#   
# } # end of group_activities()
# 
# # UI ##########################################################################
# ui <- fluidPage(
#   
#   titlePanel("ShinyEventLogger: EVENT DASHBOARD"),
#   
#   sidebarLayout(
#     
#     sidebarPanel(width = 3,
#                  
#                  uiOutput("eventlog_summary"),
#                  uiOutput("eventlog_summary_filtered"),
#                  actionButton(inputId = "reload_eventlog",
#                               label = "Reload eventlog")
#                  
#     ),
#     
#     mainPanel(
#       
#       tabsetPanel(type = "pills",
#                   
#                   tabPanel(title = "Last N Events",
#                            br(), tableOutput("last_events")),
#                   
#                   tabPanel(title = "Process map",
#                            br(), uiOutput("process_map")),
#                   
#                   tabPanel(title = "Unit tests",
#                            br(), uiOutput("unit_tests")),
#                   
#                   tabPanel(title = "Top traces",
#                            plotOutput("top_traces")),
#                   
#                   tabPanel(title = "Time analysis",
#                            uiOutput("time_analysis")),
#                   
#                   tabPanel(title = "Sequence analysis",
#                            uiOutput("sequence_analysis")),
#                   
#                   tabPanel(title = "Resource analysis",
#                            uiOutput("resource_analysis")),
#                   
#                   tabPanel(title = "Top users' actions",
#                            uiOutput("top_users_actions"))
#                   
#       )
#     )
#   )
# ) # end of ui
# 
# # SERVER ######################################################################
# server <- function(input, output, session) {
#   
#   # last_events ---------------------------------------------------------------
#   output$last_events <- renderTable({
#     
#     invalidateLater(2000)
#     
#     if (file.exists(".db_url")) {
#       
#       data <- read_eventlog(db = readLines(".db_url")[1],
#                             last_n = 25,
#                             verbose  = FALSE)
#       
#     } else {
#       
#       data <- read_eventlog(
#         last_n = 25,
#         verbose = FALSE,
#         file = system.file("shiny", "demoapp/events.log",
#                            package = "shinyEventLogger")
#       )
#       
#     }
#     
#     data <- data[, c("event_counter", "event_type", "event_name",
#                      "event_status", "event_body",
#                      "dataset", "fun", "resource", "build")]
#     
#     # trimming the output length
#     max_output_length <- 12
#     
#     data$event_body <- ifelse(
#       nchar(data$event_body) <= max_output_length,
#       data$event_body,
#       paste(strtrim(data$event_body, width = max_output_length - 5), "[...]")
#     )
#     
#     data
#     
#   })
#   
#   # eventlog_unfiltered ------------------------------------------------------
#   eventlog_unfiltered <- reactive({
#     
#     input$reload_eventlog
#     
#     if (file.exists(".db_url")) {
#       
#       message("Loading data from mongoDB...")
#       data <- read_eventlog(db = readLines(".db_url")[1],
#                             verbose  = FALSE)
#       
#     } else {
#       
#       message("Loading data from a filelog...")
#       data <- read_eventlog(
#         verbose = FALSE,
#         file = system.file("shiny", "demoapp/events.log",
#                            package = "shinyEventLogger")
#       )
#       
#     }
#     
#   })
#   
#   # eventlog -----------------------------------------------------------------
#   eventlog <- reactive({
#     
#     req(input$build_version)
#     
#     eventlog_unfiltered() %>%
#       filter(between(build,
#                      input$build_version[1],
#                      input$build_version[2]))
#     
#   })
#   
#   # eventlog_summary ---------------------------------------------------------
#   output$eventlog_summary <- renderUI({
#     
#     eventlog <- eventlog_unfiltered()
#     min_build <- min(eventlog$build, na.rm = TRUE)
#     max_build <- max(eventlog$build, na.rm = TRUE)
#     
#     tagList(
#       
#       p("There are ", strong(n_cases(eventlog)), " cases, ",
#         strong(n_activities(eventlog)), " activities,", br(),
#         " and ", strong(n_events(eventlog)), "events in the event log."),
#       
#       sliderInput(inputId = "build_version",
#                   label = "Events from DemoApp build version",
#                   min = min_build,
#                   max = max_build,
#                   value = c(max_build, max_build),
#                   step = 1, ticks = FALSE))
#     
#   })
#   
#   # eventlog_summary_filtered ------------------------------------------------
#   output$eventlog_summary_filtered <- renderUI({
#     
#     eventlog <- eventlog()
#     
#     p("Currently, we are using data from ",
#       strong(n_cases(eventlog)), " cases, ", br(),
#       strong(n_activities(eventlog)), " activities,",
#       " and ", strong(n_events(eventlog)), "events.")
#     
#   })
#   
#   # top_users_actions ---------------------------------------------------------
#   output$top_users_actions <- renderUI({
#     
#     plot_height <- 250
#     
#     tagList(
#       
#       renderPlot(height = plot_height, {
#         
#         data <-
#           eventlog() %>%
#           filter(event_name == "Dataset was selected" & !is.na(dataset)) %>%
#           count(dataset) %>%
#           arrange(desc(n)) %>%
#           mutate(n = if_else(dataset == "iris", n - n_cases(eventlog()), n))
#         
#         barplot(data$n,
#                 names.arg = data$dataset,
#                 col = 'darkgray',
#                 border = 'white',
#                 main = "Dataset most often selected*",
#                 sub = "(*) without iris dataset selected by default")
#         
#       }), hr(),
#       
#       renderPlot(height = plot_height, {
#         
#         data <-
#           eventlog() %>%
#           filter(event_name == "input$variable" & output != "") %>%
#           filter(!is.na(dataset)) %>%
#           count(event_body, dataset) %>%
#           arrange(desc(n)) %>%
#           mutate(
#             n = if_else(
#               event_body == "Sepal.Length", n - n_cases(eventlog()), n)
#           )
#         
#         barplot(data$n,
#                 names.arg = paste0(data$event_body, " (", data$dataset, ")"),
#                 col = 'darkgray',
#                 border = 'white',
#                 main = "Variables most often selected*",
#                 sub = "(*) without Sepal.Length selected by default")
#         
#       }), hr(),
#       
#       renderPlot(height = plot_height, {
#         
#         data <-
#           eventlog() %>%
#           filter(event_name == "input$bins") %>%
#           count(event_body) %>%
#           mutate(event_body = as.integer(event_body)) %>%
#           arrange(event_body) %>%
#           mutate(n = if_else(event_body == 10, n - n_cases(eventlog()), n))
#         
#         barplot(data$n,
#                 names.arg = data$event_body,
#                 col = 'darkgray',
#                 border = 'white',
#                 main = "Number of bins most often selected*",
#                 sub = "(*) without 10 bins selected by default")
#         
#       }), hr(), br()
#       
#     )
#     
#   })
#   
#   # process_map ---------------------------------------------------------------
#   output$process_map <- renderUI({
#     
#     tagList(
#       
#       p("Maximum time"),
#       renderGrViz({
#         
#         eventlog() %>%
#           group_activites(act = "collapse") %>%
#           processmapR::process_map(
#             type_edge = frequency(),
#             type_nodes = performance(units = "secs", FUN = max)
#           )
#         
#       }),
#       
#       p("Mean time"),
#       renderGrViz({
#         
#         eventlog() %>%
#           group_activites(act = "collapse") %>%
#           processmapR::process_map(
#             type_edge = frequency(),
#             type_nodes = performance(units = "secs", FUN = mean)
#           )
#         
#       })
#       
#     )
#   })
#   
#   # unit_tests ---------------------------------------------------------------
#   output$unit_tests <- renderTable({
#     
#     eventlog() %>%
#       filter(event_type == "TEST") %>%
#       count(event_type, event_name, event_status,
#             variable, bins, fun, resource) %>%
#       arrange(event_status, event_name, variable, bins)
#     
#   })
#   
#   # top_traces ---------------------------------------------------------------
#   output$top_traces <- renderPlot(height = 400, {
#     
#     eventlog() %>%
#       # group_activites(act = "collapse") %>%
#       group_activites(act = "unite") %>%
#       # trace_explorer(coverage = 0.5)
#       trace_explorer(coverage = 1)
#     
#   })
#   
#   # time_analysis -------------------------------------------------------------
#   output$time_analysis <- renderUI({
#     
#     plot_height <- 350
#     
#     fluidPage(
#       
#       fluidRow(
#         column(width = 6,
#                
#                br(), p("Throughput time per dataset"),
#                renderPlot(height = plot_height, {
#                  
#                  eventlog() %>%
#                    filter(!is.na(dataset)) %>%
#                    group_by(dataset) %>%
#                    throughput_time(level = "log", units = "mins") %>%
#                    plot()
#                  
#                })
#                
#         ),
#         column(width = 6,
#                
#                br(), p("Idle time per dataset"),
#                renderPlot(height = plot_height, {
#                  
#                  eventlog() %>%
#                    filter(!is.na(dataset)) %>%
#                    group_by(dataset) %>%
#                    idle_time(level = "log", units = "mins") %>%
#                    plot()
#                  
#                })
#         )
#       ),
#       
#       fluidRow(
#         column(width = 6, offset = 0,
#                
#                br(), p("Processing time per dataset"),
#                renderPlot(height = plot_height, {
#                  
#                  eventlog() %>%
#                    filter(!is.na(dataset)) %>%
#                    group_by(dataset) %>%
#                    processing_time(level = "log", units = "mins") %>%
#                    plot()
#                  
#                })
#                
#         ),
#         column(width = 6, offset = 0,
#                
#                br(), p("Dotted chart of cases"),
#                renderPlot(height = plot_height, {
#                  
#                  eventlog() %>%
#                    group_activites() %>%
#                    dotted_chart(units = "mins",
#                                 x = "relative",
#                                 sort = "duration")
#                  
#                })
#                
#         )
#       )
#     )
#   })
#   
#   # sequence_analysis --------------------------------------------------------
#   output$sequence_analysis <- renderUI({
#     
#     plot_height <- 350
#     
#     fluidPage(
#       fluidRow(
#         column(width = 8, offset = 2,
#                
#                br(), p("Precedence matrix"),
#                renderPlot(height = plot_height, {
#                  
#                  eventlog() %>%
#                    group_activites(act = "unite") %>%
#                    precedence_matrix(type = "relative") %>%
#                    plot()
#                  
#                })
#                
#         )
#       ),
#       
#       fluidRow(
#         column(width = 6,
#                
#                br(), p("Repetitions"),
#                renderPlot(height = plot_height * 0.6, {
#                  
#                  eventlog() %>%
#                    group_activites(act = "unite") %>%
#                    number_of_repetitions(level = "activity") %>%
#                    plot()
#                  
#                })
#         ),
#         column(width = 6,
#                
#                br(), p("Selfloops"),
#                renderPlot(height = plot_height * 0.6, {
#                  
#                  eventlog() %>%
#                    group_activites(act = "unite") %>%
#                    number_of_selfloops(level = "activity") %>%
#                    plot()
#                  
#                })
#         ),
#         column(width = 6,
#                
#                br(), p("End activities (grouped)"),
#                renderPlot(height = plot_height * 0.4, {
#                  
#                  eventlog() %>%
#                    group_activites(act = "unite") %>%
#                    end_activities(level = "activity") %>%
#                    plot()
#                  
#                })
#         ),
#         column(width = 6,
#                
#                br(), p("End activities (ungrouped)"),
#                renderPlot(height = plot_height * 0.4, {
#                  
#                  eventlog() %>%
#                    # group_activites(act = "unite") %>%
#                    end_activities(level = "activity") %>%
#                    plot()
#                  
#                })
#         )
#       )
#     )
#   })
#   
#   # resource_analysis --------------------------------------------------------
#   output$resource_analysis <- renderUI({
#     
#     plot_height <- 600
#     
#     fluidRow(
#       column(width = 6,
#              
#              br(),
#              renderPlot(height = plot_height, {
#                
#                eventlog() %>%
#                  filter(!is.na(fun), !is.na(resource)) %>%
#                  group_by(fun) %>%
#                  resource_frequency(level = "resource") %>%
#                  plot() + ggplot2::theme(
#                    legend.position = "bottom",
#                    axis.title = ggplot2::element_blank()
#                  )
#                
#              })
#       ),
#       column(width = 6,
#              
#              br(), renderPlot(height = plot_height, {
#                
#                eventlog() %>%
#                  filter(!is.na(fun), !is.na(resource)) %>%
#                  group_by(fun) %>%
#                  resource_involvement(level = "resource") %>%
#                  plot() + ggplot2::theme(
#                    legend.position = "bottom",
#                    axis.title = ggplot2::element_blank()
#                  )
#              })
#       )
#     )
#   })
#   
# } # end of server
# 
# shinyApp(ui = ui, server = server)