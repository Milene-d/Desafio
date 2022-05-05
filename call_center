#library
library(dplyr)
library(stringr)
library(cluster) 
library(Rtsne) 
library(ggplot2) 

#Dados
creds<-read.csv("C:\\Users\\milen\\Downloads\\creds.csv", encoding = "UTF-8")
cases<-read.csv("C:\\Users\\milen\\Downloads\\cases.csv", encoding = "UTF-8")

#Organização
creds$X<-NULL
cases$X<-NULL
cases<-na.omit(cases)
cases$date_ref<-as.Date(cases$date_ref)
creds$cred_date<-as.Date(creds$cred_date)
colnames(creds)[1]<-"date_ref"

#Caso 1

#Caso depois do evento:
#filtro por data, assunto e média agrupada por dias
depois<-cases %>% filter(cases$date_ref>="2020-08-01")
total_depois<-depois %>% group_by(date_ref) %>% summarise(length (assunto))
depois_produto<-filter(depois, grepl("Produto",assunto)) 
total_produto_depois<-depois_produto %>% group_by(date_ref) %>% summarise(length(assunto))
media_depois<-left_join(total_depois,total_produto_depois, by ="date_ref")
colnames(media_depois)<-c("date_ref", "total", "produto")
media_depois$media_diaria<-media_depois$produto/media_depois$total
d_media<-media_depois$media_diaria

#Caso antes do evento
antes<-cases %>% filter(cases$date_ref<"2020-08-01")
total_antes<-antes %>% group_by(date_ref) %>% summarise(length (assunto))
antes_produto<-filter(antes, grepl("Produto",assunto)) 
total_produto_antes<-antes_produto %>% group_by(date_ref) %>% summarise(length(assunto))
#Tiveram dias sem reclamação e precisa aparecer zerado
media_antes<-left_join(total_antes,total_produto_antes, by ="date_ref")
colnames(media_antes)<-c("date_ref", "total", "produto")
media_antes$media_diaria<-media_antes$produto/media_antes$total
media_antes$media_diaria[is.na (media_antes$media_diaria)]<-  0
a_media<-media_antes$media_diaria

#Padronizar o tamanho das amostras para 90 dias
media_antes<- media_antes[order(media_antes$date_ref, decreasing = TRUE),]
media_antes<-media_antes[1:90,]
a_media<-media_antes$media_diaria

#Testes estatisticos
shapiro.test(a_media)
shapiro.test(d_media)
#Teste não parametrico wilcoxon
wilcox.test(a_media, d_media, alternative = "two.sided")
#Não houve diferença significativa



#Caso2

caso_2<-left_join(cases,creds, by = c("accountid","date_ref"))
caso_2<-unique(caso_2)
qtd_chamados<-caso_2 %>% group_by(date_ref) %>% summarise(length(assunto))
plot(qtd_chamados, main = "Total")

#Aumentou o número de chamados ao longo do tempo
qtd_clientes_antigos<-caso_2 %>% group_by(date_ref) %>% summarise(length(na.omit(shipping_address_city)))
plot(qtd_clientes_antigos, main = "Clientes Antigos")
qtd_clientes_recentes<-caso_2 %>% group_by(date_ref) %>% summarise(length(!is.na(shipping_address_city)))
plot(qtd_clientes_recentes, main = "Clientes Recentes")
#Curva de clientes recentes é mais íngrime 

sum(qtd_clientes_antigos$`length(na.omit(shipping_address_city))`)
sum(qtd_clientes_recentes$`length(!is.na(shipping_address_city))`)
#Clientes recentes tiveram maior quantidade de chamados e esse comportamento se agravou ao longo do tempo.

#Caso 3
caso_3<-left_join(cases,creds, by = c("accountid","date_ref"))
caso_3$cliente_recente<- ifelse (is.na(caso_3$shipping_address_city), 0, 1)
caso_3<-left_join(caso_3,creds, by = c("accountid"))
caso_3<-caso_3[,c(1,3,4,5,6,7,12,14,15,16)]
caso_3$assunto <- word(caso_3$assunto, 1, sep = fixed(":"))
#Pesquisa está com valores de qualidade mas também tem como dado "Enviado". Os que estão em branco 
#foram pesquisas não enviadas ou não respondidas? Por essas questões, a variável foi excluída do modelo
caso_3$pesquisa_de_satisfa_o__c<-NULL


caso_3$channelid<-as.factor(caso_3$channelid)
caso_3$cliente_recente<-as.factor(caso_3$cliente_recente)
caso_3$missed<-as.factor(caso_3$missed)
caso_3$assunto<-as.factor(caso_3$assunto)
caso_3$accountid<-as.factor(caso_3$accountid)
caso_3$shipping_address_city.y<-as.factor(caso_3$shipping_address_city.y)
caso_3$shipping_address_state.y<-as.factor(caso_3$shipping_address_state.y)
caso_3$max_machine.y<-as.factor(caso_3$max_machine.y)
caso_3$shipping_address_city.y<-NULL
caso_3$accountid<-NULL
caso_3$max_machine.y<-NULL
str(caso_3)

#K mode
gower_dist <- daisy(caso_3,metric = "gower")

memory.size()
memory.limit(size = 543)
gc(caso_3)
#Continuo tendo problemas de memória, como minha máquina não aguenta, não farei k mode.
#O agrupamento será manual e analisarei por indicadores

getmode <- function(caso_3) {
  uniqv <- unique(caso_3)
  uniqv[which.max(tabulate(match(caso_3, uniqv)))]
}

help_assunto<-caso_3 %>% group_by(assunto) %>% summarise(getmode(channelid),mean(waitingtime),
                                                         getmode(missed), getmode(cliente_recente),
                                                         getmode(shipping_address_state.y), length(assunto))
colnames(help_assunto)<-c("ASSUNTO","MODA CANAL", "MÉDIA TEMPO", "MODA CHAMADO ATENDIDO", "MODA CLIENTE RECENTE", "ESTADO", "QTD DE CHAMADOS")
#Resumo: Em todos os assuntos, a maioria dos casos não foram solucionados e a maioria é de SP.
#Critério para escolha da prioridade dos casos de atendimento:
#Maior número de ligações
#Tempo de espera elevado
