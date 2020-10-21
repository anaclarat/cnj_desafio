#install.packages("klaR")
library(klaR)
library(xlsx)
library(dplyr)
library(factoextra)
library(tidyr)
library(reshape2)

setwd('D:/Inova/Dados-CNJ_disponibilizados/base')
plot.new()

df <- read.csv2('./Arquivos/0/TRF2/TRF2.csv')

df1 <- df %>% 
    filter(grau=='G1') %>%
  group_by(dadosBasicos.orgaoJulgador.codigoOrgao,
           dadosBasicos.orgaoJulgador.nomeOrgao,
           dadosBasicos.assunto.codigoNacional,
           dadosBasicos.assunto.descricao
           ) %>% 
  summarise(n_distinct(dadosBasicos.numero)) 

names(df1)[names(df1)=='n_distinct(dadosBasicos.numero)'] <- 'contagem'

ag<- aggregate(x = df1$contagem, by = list(paste(df1$dadosBasicos.orgaoJulgador.codigoOrgao,'-',df1$dadosBasicos.orgaoJulgador.nomeOrgao),df1$dadosBasicos.assunto.descricao), FUN = "sum")


jt <-reshape2::dcast(ag,Group.1~Group.2)

jt1 <- jt %>% 
  mutate_all(replace_na, 0) 

jt2<- jt1[,-1]
row.names(jt2) <- jt1[,1]

jt2 <- scale(jt2)

res <- eclust(jt2, "kmeans", nstart = 25)

#Merge do DF com os clusters

nomes <- rownames(res$silinfo$widths) %>%
  colsplit('-',names = c('codigo','descricao')) 

arquivo <- cbind(res$silinfo$widths[,1],nomes) %>% 
  rename(cluster='res$silinfo$widths[, 1]') 

arquivo <- merge(df,arquivo,by.x = 'dadosBasicos.orgaoJulgador.codigoOrgao' ,by.y = 'codigo',all.x = TRUE)

arquivo <- arquivo[,-26]

#Filtragem para que conste apenas processos que tenham sido baixados
proc_baixados <- arquivo %>% filter(movimento.movimentoNacional.codigoNacional == "22") 
proc_baixados <- unique(proc_baixados$dadosBasicos.numero)

####vetor lógico indicando processos baixados
trib1 <- arquivo$dadosBasicos.numero   %in% proc_baixados

####Registros com os processos baixados
arquivo <- arquivo[trib1,]

rm(trib1,proc_baixados)

#Geração dos arquivos
png(filename = 'cluster.png', width=800, height=800)
plot(res$clust_plot)

dev.off()
if (!file.exists('./Arquivos/1/'))
{dir.create('./Arquivos/1/')}
arquivo <-format(as.data.frame(arquivo),digits=20)
write.csv2(arquivo,'./Arquivos/1/TRF2.csv',row.names = FALSE)
