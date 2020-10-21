rm(list = ls())

#setwd('D:/Inova/Dados-CNJ_disponibilizados/base/justica_federal/processos-trf1')

#Atenção
#Não gerado:
#Arquivo Coluna de Atenção(para comentar): 
#TRF1-5 - dadosBasicos.valorCausa
#TRF1-6 - dadosBasicos.valorCausa

setwd('D:/Inova/Dados-CNJ_disponibilizados/base')
library(Hmisc)
library(tidyr)
library(RJSONIO)
library(xlsx)
library(dplyr)
library(purrr)

read_files <- function(directory){
  
  dir_orig <- getwd()
  
  directory <- paste(c(dir_orig,'/',directory),collapse= '')
  
  setwd(directory)
  rm(directory)
  files <-list.files(full.names = TRUE,recursive = TRUE)
  id <- length(files)
  json = list()
  
  for (i in 1:id){
    json[[i]] <- jsonlite::fromJSON(file(files[i]),simplifyDataFrame = TRUE,flatten = TRUE) 
    #json[[i]] <- jsonlite::flatten(json[[i]],recursive = TRUE)

    json[[i]] <-as_data_frame(json[[i]])
  }
  rm(files,i,id)
  
  data <- jsonlite::rbind_pages(json)
  rm(json)

  data <- unnest_longer(data,col = movimento)
  data <- unnest_longer(data,col = dadosBasicos.assunto)

  data_Mov <- data$dadosBasicos.numero 
  # data.frame()
  # data_Mov <- rep(NA,nrow(data))

  
  if('movimentoNacional.codigoNacional' %in% (names(data$movimento))){
    data_mov_cod_Nac <- data$movimento$movimentoNacional.codigoNacional
  }
  if('movimentoLocal.codigoPaiNacional' %in% (names(data$movimento))){
    data_mov_cod_PNac <- data$movimento$movimentoLocal.codigoPaiNacional
  }
  
  if('movimentoNacional.codigoNacional' %in% (names(data$movimento))){
    data_Mov <- cbind(data_Mov,data_mov_cod_Nac)
    rm(data_mov_cod_Nac)
  }
  if('movimentoLocal.codigoPaiNacional' %in% (names(data$movimento))){
    data_Mov <- cbind(data_Mov,data_mov_cod_PNac)
    rm(data_mov_cod_PNac)
  }
  
  data_out = data.frame()
  
  if (('dadosBasicos.valorCausa' %nin% (names(data))) & ('dadosBasicos.competencia' %nin% (names(data)))){
    data_out <- select(data,dadosBasicos.classeProcessual,dadosBasicos.codigoLocalidade,
                       dadosBasicos.dataAjuizamento,dadosBasicos.nivelSigilo,
                       dadosBasicos.orgaoJulgador.codigoMunicipioIBGE,dadosBasicos.orgaoJulgador.codigoOrgao,
                       dadosBasicos.orgaoJulgador.instancia,dadosBasicos.orgaoJulgador.nomeOrgao,
                       grau,millisInsercao,siglaTribunal)
    data_out <- cbind(data_out,rep(NA,nrow(data_out)))
    names(data_out)[names(data_out)=='rep(NA, nrow(data_out))'] <- 'dadosBasicos.valorCausa'
    
    data_out <- select(data_out,dadosBasicos.classeProcessual,dadosBasicos.codigoLocalidade,#########data->data_out
                       dadosBasicos.dataAjuizamento,dadosBasicos.nivelSigilo,
                       dadosBasicos.orgaoJulgador.codigoMunicipioIBGE,dadosBasicos.orgaoJulgador.codigoOrgao,
                       dadosBasicos.orgaoJulgador.instancia,dadosBasicos.orgaoJulgador.nomeOrgao,
                       grau,millisInsercao,siglaTribunal,dadosBasicos.valorCausa)
    
    data_out <- cbind(data_out,rep(NA,nrow(data_out)))
    names(data_out)[names(data_out)=='rep(NA, nrow(data_out))'] <- 'dadosBasicos.competencia'
    #break
  }else if('dadosBasicos.valorCausa' %nin% (names(data))){
    data_out <- select(data,dadosBasicos.classeProcessual,dadosBasicos.codigoLocalidade,
                       dadosBasicos.competencia,dadosBasicos.dataAjuizamento,dadosBasicos.nivelSigilo,
                       dadosBasicos.orgaoJulgador.codigoMunicipioIBGE,dadosBasicos.orgaoJulgador.codigoOrgao,
                       dadosBasicos.orgaoJulgador.instancia,dadosBasicos.orgaoJulgador.nomeOrgao,
                       grau,millisInsercao,siglaTribunal)
    
    data_out <- cbind(data_out,rep(NA,nrow(data_out)))
    names(data_out)[names(data_out)=='rep(NA, nrow(data_out))'] <- 'dadosBasicos.valorCausa'
    
  }else if('dadosBasicos.competencia' %nin% (names(data))){
    data_out <- select(data,dadosBasicos.classeProcessual,dadosBasicos.codigoLocalidade,
                       dadosBasicos.dataAjuizamento,dadosBasicos.nivelSigilo,
                       dadosBasicos.orgaoJulgador.codigoMunicipioIBGE,dadosBasicos.orgaoJulgador.codigoOrgao,
                       dadosBasicos.orgaoJulgador.instancia,dadosBasicos.orgaoJulgador.nomeOrgao,
                       grau,millisInsercao,siglaTribunal,dadosBasicos.valorCausa)
    
    data_out <- cbind(data_out,rep(NA,nrow(data_out)))
    names(data_out)[names(data_out)=='rep(NA, nrow(data_out))'] <- 'dadosBasicos.competencia'
  }else{
    data_out <- select(data,dadosBasicos.classeProcessual,dadosBasicos.codigoLocalidade,
                       dadosBasicos.competencia,dadosBasicos.dataAjuizamento,dadosBasicos.nivelSigilo,
                       dadosBasicos.orgaoJulgador.codigoMunicipioIBGE,dadosBasicos.orgaoJulgador.codigoOrgao,
                       dadosBasicos.orgaoJulgador.instancia,
                       dadosBasicos.orgaoJulgador.nomeOrgao,dadosBasicos.valorCausa,
                       grau,millisInsercao,siglaTribunal)
  }
  

  data_Mov <- as.data.frame(data_Mov)
  
  if ('data_mov_cod_Nac' %in% names(data_Mov) & ('data_mov_cod_PNac' %in% (names(data_Mov)))){
    data_Mov$data_mov_cod_Nac <- ifelse(is.na(data_Mov$data_mov_cod_Nac),data_Mov$data_mov_cod_PNac,data_Mov$data_mov_cod_Nac)
    
  }else if('data_mov_cod_Nac' %in% names(data_Mov)){
    
  }else{
    names(data_Mov)[names(data_Mov)=='data_mov_cod_PNac'] <- 'data_mov_cod_Nac'
  }
    
  names(data_Mov)[names(data_Mov)=='data_mov_cod_Nac'] <- 'movimento.movimentoNacional.codigoNacional'
  names(data_Mov)[names(data_Mov)=='data_Mov'] <- 'dadosBasicos.numero'
  
  data_mov_dtHr <- (data$movimento$dataHora)
  data_mov_dtHr <-as.data.frame(data_mov_dtHr)
  
  names(data_mov_dtHr)[names(data_mov_dtHr)=='data_mov_dtHr'] <- 'movimento.dataHora'
  
  data_Mov <- cbind(data_Mov,data_mov_dtHr)
  rm(data_mov_dtHr)
  
  data_Mov <- cbind(data_Mov,data_out)
  
  rm(data_out)
 
  data_ass_Princ <- data$dadosBasicos.assunto$principal
  data_Mov <- cbind(data_Mov,data_ass_Princ)
  names(data_Mov)[names(data_Mov)=='data_ass_Princ'] <- 'dadosBasicos.assunto.principal'
  rm(data_ass_Princ)

  
  if('codigoNacional' %in% (names(data$dadosBasicos.assunto))){
    data_ass_cod_Nac <- data$dadosBasicos.assunto$codigoNacional
  }
  if('assuntoLocal.codigoPaiNacional' %in% (names(data$dadosBasicos.assunto))){
    data_ass_cod_PNac <- data$dadosBasicos.assunto$assuntoLocal.codigoPaiNacional
  }
  
  if('codigoNacional' %in% (names(data$dadosBasicos.assunto))){
    data_Mov <- cbind(data_Mov,data_ass_cod_Nac)
    rm(data_ass_cod_Nac)
  }
  if('assuntoLocal.codigoPaiNacional' %in% (names(data$dadosBasicos.assunto))){
    data_Mov <- cbind(data_Mov,data_ass_cod_PNac)
    rm(data_ass_cod_PNac)
  }
  
  if ('data_ass_cod_Nac' %in% names(data_Mov) & ('data_ass_cod_PNac' %in% (names(data_Mov)))){
    data_Mov$data_ass_cod_Nac <- ifelse(is.na(data_Mov$data_ass_cod_Nac),data_Mov$data_ass_cod_PNac,data_Mov$data_ass_cod_Nac)
  }else if('data_ass_cod_Nac' %in% names(data_Mov)){
    
  }else{
    names(data_Mov)[names(data_Mov)=='data_ass_cod_PNac'] <- 'data_ass_cod_Nac'
  }
  
  names(data_Mov)[names(data_Mov)=='data_ass_cod_Nac'] <- 'dadosBasicos.assunto.codigoNacional'

  
  
  setwd(dir_orig)
  data_Mov <- select(data_Mov,'dadosBasicos.numero','movimento.movimentoNacional.codigoNacional',
  'movimento.dataHora','dadosBasicos.classeProcessual','dadosBasicos.codigoLocalidade',
  'dadosBasicos.competencia','dadosBasicos.dataAjuizamento','dadosBasicos.nivelSigilo',
  'dadosBasicos.orgaoJulgador.codigoMunicipioIBGE','dadosBasicos.orgaoJulgador.codigoOrgao',
  'dadosBasicos.orgaoJulgador.instancia','dadosBasicos.orgaoJulgador.nomeOrgao',
  'dadosBasicos.valorCausa','grau','millisInsercao','siglaTribunal','dadosBasicos.assunto.principal',
  'dadosBasicos.assunto.codigoNacional')

  # data_Mov <- format(as.data.frame(data_Mov),digits=20)
  
  #Acrescentar descrição Tabela -  Movimentos
  mov <- read.csv2('./sgt_movimentos.csv',encoding = 'UTF-8')   
  mov <- subset(select(mov,codigo:descricao))
  mov$codigo <- as.character(mov$codigo)
  
  data_Mov <- merge(data_Mov,mov, by.x = 'movimento.movimentoNacional.codigoNacional', by.y = 'codigo',all.x = TRUE)
  data_Mov <- data_Mov %>% 
    rename(movimento.movimentoNacional.descricao = descricao)
  
  rm(mov)
  
  #Acrescentar descrição e Sigla - Tabela Classes
  clas <- read.csv2('./sgt_classes.csv',encoding = 'UTF-8')
  clas <- subset(select(clas,codigo:sigla))
  
  data_Mov <- merge(data_Mov,clas, by.x = 'dadosBasicos.classeProcessual', by.y = 'codigo',all.x = TRUE)
  data_Mov <- data_Mov %>% 
    rename(dadosBasicos.descricaoclasseProcessual = descricao,
           dadosBasicos.siglaclasse = sigla)
  
  rm(clas)
  
  #Acrescentar descrição Tabela Assuntos
  ass <- read.csv2('./sgt_assuntos.csv',encoding = 'UTF-8')
  ass <- subset(select(ass,codigo:descricao))
  
  data_Mov <- merge(data_Mov,ass, by.x = 'dadosBasicos.assunto.codigoNacional', by.y = 'codigo',all.x = TRUE)
  data_Mov <- data_Mov %>% 
    rename(dadosBasicos.assunto.descricao = descricao)
  
  rm(ass)
  
  #Acrescentar descrição Tabela IBGE
  reg <- read.xlsx('./RELATORIO_DTB_BRASIL_MUNICIPIO.xls',sheetName = 'DTB_2018_Municipio' ,encoding = 'UTF-8',all.x = TRUE)
  reg <- subset(select(reg,Nome_Mesorregi�o,C�digo.Munic�pio.Completo,Nome_Munic�pio))
  reg <- reg %>% 
    rename(IBGE.Nome_Mesorregiao = Nome_Mesorregi�o,
           IBGE.Codigo.Municipio.Completo = C�digo.Munic�pio.Completo,
           IBGE.Nome_Municipio = Nome_Munic�pio)
  
  data_Mov <- merge(data_Mov,reg, by.x = 'dadosBasicos.orgaoJulgador.codigoMunicipioIBGE', by.y = 'IBGE.Codigo.Municipio.Completo',all.x = TRUE)
  
  rm(reg)
  
  #Gerando arquivo
  data_Mov <- format(as.data.frame(data_Mov),digits=20)
  if (!file.exists('./Arquivos/0/'))
  {dir.create('./Arquivos/0/')}
  write.csv2(data_Mov, file='./Arquivos/TRF2.csv',row.names = FALSE)
  # data
}

#se arquivos Justiça Federal

directory = '/justica_federal/processos-trf1/1' 

d <- read_files(directory)

#Agrupamentos para geração de arquivos:
#TRF1
#TRF1-1: '/justica_federal/processos-trf1/1'
 #processos-trf1_1
 #processos-trf1_2
#TRF1-2: '/justica_federal/processos-trf1/2'
 #processos-trf1_3
 #processos-trf1_4
#TRF1-3: '/justica_federal/processos-trf1/3'
 #processos-trf1_5
#TRF1-4: '/justica_federal/processos-trf1/4'
  #processos-trf1_6
#TRF1-5: '/justica_federal/processos-trf1/5'
 #processos-trf1_7
 #processos-trf1_8
 #processos-trf1_9
 #processos-trf1_10
 #processos-trf1_11

#TRF2 - gerado por completo :'/justica_federal/processos-trf2'

#TRF3
#TRF3-1: '/justica_federal/processos-trf3/1'
 #processos-trf3_1
 #processos-trf3_5
#TRF3-2: '/justica_federal/processos-trf3/2'
 #processos-trf3_2
 #processos-trf3_3
 #processos-trf3_4
 #processos-trf3_8
 #processos-trf3_9
 #processos-trf3_10
 #processos-trf3_11
#TRF3-3: '/justica_federal/processos-trf3/3'
 #processos-trf3_6
 #processos-trf3_7

#TRF4
#TRF4-1: '/justica_federal/processos-trf4/1'
 #processos-trf4_1
#TRF4-2: '/justica_federal/processos-trf4/2'
 #processos-trf4_2
#TRF4-3: '/justica_federal/processos-trf4/3'
 #processos-trf4_3
 #processos-trf4_4
 #processos-trf4_5

#TRF5
#TRF5-1: '/justica_federal/processos-trf5/1'
 #processos-trf5_1
 #processos-trf5_4

#TRF5-2: '/justica_federal/processos-trf5/2'
 #processos-trf5_2

#TRF5-3: '/justica_federal/processos-trf5/3'
 #processos-trf5_3

#TRF5-4: '/justica_federal/processos-trf5/4'
 #processos-trf5_10
 #processos-trf5_11
 #processos-trf5_12

#TRF5-5: '/justica_federal/processos-trf5/5' 
#Error: Can't combine `..1$movimento$dataHora` <character> and `..21$movimento$dataHora` <double>.
 #processos-trf5_8
 #processos-trf5_9

#TRF5-6: '/justica_federal/processos-trf5/6'
#Error: Can't combine `..1$movimento$dataHora` <character> and `..29$movimento$dataHora` <double>.
 #processos-trf5_7

#TRF5-7: '/justica_federal/processos-trf5/7'
#Error: Can't combine `..1$movimento$dataHora` <character> and `..53$movimento$dataHora` <double>.
 #processos-trf5_6

#TRF5-8: '/justica_federal/processos-trf5/8'
#Error: Can't combine `..1$movimento$dataHora` <character> and `..269$movimento$dataHora` <double>.
 #processos-trf5_5

#Ainda por fazer:
#se arquivos Justiça Eleitoral

#TRE-1:
 #TRE-AC: '/justica_eleitoral/processos-tre-ac'
 #TRE-AL: '/justica_eleitoral/processos-tre-al'
 #TRE-AM: '/justica_eleitoral/processos-tre-am'
#TRE-2: VERIFICAR TIPODECISÃO TIPO MUTATE
 #TRE-ba: '/justica_eleitoral/processos-tre-ba'
#TRE-3: 
 #TRE-AP: '/justica_eleitoral/processos-tre-ap'
 #TRE-DF: '/justica_eleitoral/processos-tre-df'
#TRE-4: 
 #TRE-ES: '/justica_eleitoral/processos-tre-es'
 #TRE-GO: '/justica_eleitoral/processos-tre-GO'

