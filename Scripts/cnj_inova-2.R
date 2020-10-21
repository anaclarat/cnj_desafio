# TESTE BUPAR - Biblioteca R para process mining (https://www.bupar.net/)

# Instalar packages bupaR
install.packages("bupaR")
install.packages("edeaR")
install.packages("eventdataR")
install.packages("processmapR")
install.packages("processmonitR")
install.packages("xesreadR")
install.packages("petrinetR")
install.packages("daqapo")
install.packages("processanimateR")

# Executar bupaR e pacotes de process mining
library(bupaR)
library(edeaR)
library(eventdataR)
library(processmapR)
library(processmonitR)
library(petrinetR)
library(processanimateR)

# Executar leitor de arquivo XES
library(xesreadR)

# Executar analisador de qualidade de dados
library(daqapo)

#TESTE DADOS DO REPOSITÓRIO BUPAR
  #1.  Visualizar dados
       eventdataR::patients
    #2.  Display metadata of event log objects
       patients %>% mapping
  #3.  General summary of the event log
       patients %>% summary
  #3.1 Basic counts summary 
       patients %>% n_activities
       patients %>% n_activity_instances
       patients %>% n_cases
       patients %>% n_events
       patients %>% n_traces
       patients %>% n_resources
  #3.2 Cases summary
       patients %>% cases
       
       
#IMPORTAR DADOS DO XES
  #1. File > Import Dataset > From Text(readr)
      # Movimentos_2013_TRF4
      # só para selecionar o arquivo no browse e copiar o endereço para colar abaixo
      m <- read_xes("cnj_inova/Movimentos_2013_TRF4_desafio.xes")
      View(m)

      #TESTE DADOS REAIS
  #1.  Visualizar a tabela de dados
      View(m)
  #2.  Display metadata of event log objects
      m %>% mapping
  #3.  General summary of the event log
      m %>% summary
      #3.1 Basic counts summary 
      m %>% n_activities
      m %>% n_activity_instances
      m %>% n_cases
      m %>% n_events
      m %>% n_traces
      m %>% n_resources
      #3.2 Cases summary
      m %>% cases

# I. Data Quality Assessment
    #I.1  (NÃO FUNCIONANDO) Activity Frequency Violations
          m %>%
            detect_activity_frequency_violations("DISTRIBUIÇÃO" = 1,
                                                 "BAIXA DEFINITIVA" = 1)

    #I.2  (NÃO FUNCIONANDO) Activity Order Violations
          m %>%
            detect_activity_order_violations(activity_order = c("DISTRIBUIÇÃO", "BAIXA DEFINITIVA"))

    #I.3  Attribute Dependencies
          m %>% 
            detect_attribute_dependencies(antecedent = activity_id == "DISTRIBUIÇÃO",
                                          consequent = activity_id == "BAIXA DEFINITIVA")
    
    #I.4  Case ID Sequence Gaps
          m %>%
            detect_case_id_sequence_gaps()

    #I.5  Conditional Activity Presence
          m %>%
            detect_conditional_activity_presence(condition = ASSUNTO_PRINCIPAL == "GRATIFICAÇÃO DE INCENTIVO",
                                                 activities = "DISTRIBUIÇÃO")    
    
    #I.6  Duration Outliers
          m %>%
            detect_duration_outliers(Treatment = duration_within(bound_sd = 1))

    #I.7  Inactive Periods
          m %>%
            detect_inactive_periods(threshold = 30)
          m %>%
            detect_multiregistration(threshold_in_seconds = 10)


          m %>%
            idle_time("resource", units = "days")
          
          m %>%
            idle_time("resource", units = "days") %>%
            plot()
          
          m %>% 
            processing_time("activity") %>%
            plot
          
          m %>%
            throughput_time("log") %>%
            plot()
          
          m %>% activity_presence() %>%
            plot
          
          m %>%
            trace_coverage("trace") %>%
            plot()
          
          m %>%
            trace_length("log") %>%
            plot
          
          m %>%
            process_map()

          m %>%
            process_map(type = frequency("relative"))
          
          m %>%
            filter_trim(start_activities = "DISTRIBUIÇÃO", end_activities =  "BAIXA DEFINITIVA") %>%
            process_map(type = performance())
          m %>%
            filter_endpoints(start_activities = "DISTRIBUIÇÃO", end_activities = "BAIXA DEFINITIVA") %>%
            process_map()

          animate_process(m)
          
          animate_process(m, mapping = token_aes(size = token_scale(12), shape = "rect"))
          
# TESTE SHINY - Biblioteca R para (https://shiny.rstudio.com/)

# Instalar package Shiny
install.packages("shiny")

# Executar Shiny
library(shiny)

runExample("01_hello")
