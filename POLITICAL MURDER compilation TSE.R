rm(list=ls())

#library(rmarkdown)
#rmarkdown::render('C:/Users/Kelly/OneDrive - Fundacao Getulio Vargas - FGV/Paper Gender Inequality UNICAMP/Code/competition.R')

#' ---
#' title: "Political Murders - TSE dataset compilation"
#' author: Kelly G. & Antonio Leon
#' output:
#'   html_document:
#'     toc: true
#'     number_sections: true
#' ---
#' 
#' 

library(here)
library(dplyr)
library(readtext)
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(haven)
library(plyr)


#' Opening
setwd("/Users/Kelly/OneDrive - Fundacao Getulio Vargas - FGV/Paper Morte de Vereadores/database")
load("candidate.Rda")


#' Drop mun with 'segundo turno'

#' YEARS:
year<-2016
candidate.prefeito <- candidate %>% subset(ANO_ELEICAO==year-4)


#' ## (1) Prefeito antigo por municipio 
candidate.prefeito <-
  candidate.prefeito %>%  subset(CD_CARGO == 11) %>% subset(CD_SIT_TOT_TURNO==1)
candidate.prefeito$ANO_ELEICAO<-candidate.prefeito$ANO_ELEICAO+4
candidate.prefeito <- candidate.prefeito <- candidate.prefeito %>%  subset(ANO_ELEICAO!=2024)
candidate.prefeito <- candidate.prefeito %>%  subset(select=c("ANO_ELEICAO",
                                                              "SG_UE","SG_PARTIDO",
                                                              "DS_COMPOSICAO_COLIGACAO"))
names(candidate.prefeito)[names(candidate.prefeito) == 'SG_PARTIDO'] <- 'PARTIDO_ANTERIOR'
names(candidate.prefeito)[names(candidate.prefeito) == 'DS_COMPOSICAO_COLIGACAO'] <- 'COLIGACAO_ANTERIOR'

candidate.prefeito.antigo <- candidate %>%  subset(ANO_ELEICAO==year) %>%  subset(ANO_ELEICAO!=2000)
# candidate.prefeito.antigo <-candidate.prefeito.antigo %>%  subset(select=c("ANO_ELEICAO",
#                                                                    "SG_UE","SG_PARTIDO",
#                                                                    "DS_COMPOSICAO_COLIGACAO",
#                                                                    "CD_SIT_TOT_TURNO",
#                                                                    "DS_SIT_TOT_TURNO",
#                                                                    "CD_DETALHE_SITUACAO_CAND",
#                                                                    "DS_DETALHE_SITUACAO_CAND"))
candidate.prefeito.antigo <-merge(candidate.prefeito.antigo,candidate.prefeito,by=c("ANO_ELEICAO", "SG_UE"))
##############VERIFICAR OBS POR MUNICIPIO DUPLICATA!

#' ## (2) Treatment

#' ### (2.1) Mesmo PARTIDO (igual a 1)
candidate.prefeito.antigo$treatment_partido<-0
candidate.prefeito.antigo$treatment_partido[candidate.prefeito.antigo$SG_PARTIDO==candidate.prefeito.antigo$PARTIDO_ANTERIOR] <- 1
candidate.prefeito.antigo$treatment_partido[is.na(candidate.prefeito.antigo$SG_PARTIDO) | 
                                              is.na(candidate.prefeito.antigo$PARTIDO_ANTERIOR)] <-NA


#' ### (2.1) Mesma COLIGACAO (igual a 1)

out <- strsplit(as.character(candidate.prefeito.antigo$DS_COMPOSICAO_COLIGACAO), '/') 
candidate.prefeito.antigo<-data.frame(candidate.prefeito.antigo, do.call(rbind, out))

name<-sprintf("X%s",seq(1:24))
res <- lapply(candidate.prefeito.antigo[name],
              `%in%`, candidate.prefeito.antigo$SG_PARTIDO)

out<- ldply(res, data.frame)
out<-data.frame(matrix(unlist(res), ncol=max(length(res)), byrow=TRUE))
out<-out %>% 
  mutate_all(~as.numeric(.)) 
out<-out %>% mutate(sum = rowSums(.))

candidate.prefeito.antigo<-cbind(candidate.prefeito.antigo,out$sum)

candidate.prefeito.antigo$treatment_coligacao<-0
candidate.prefeito.antigo$treatment_coligacao[candidate.prefeito.antigo$sum >0] <- 1
candidate.prefeito.antigo$treatment_coligacao[is.na(candidate.prefeito.antigo$DS_COMPOSICAO_COLIGACAO)] <-NA



#' ## (3) Taxa de mortalidade de politicos por municipio
#' ### (3.1) Mortalidade por municipio

#' Codigo de ocupacao de vereadores
ocup.vereador.n<-c(111120)
ocup.vereador<-c('Vereador')

#CBO 11 - membros superiores do poder publico
# 111 - servidor executivo, legislativo, judiciario

legisladores <- c("111105", sprintf("1111%s", seq(10, 20, 5)))
dirigentes.gerais <- c("111205", sprintf("1112%s", seq(10, 55, 5)))
magistrados <- c("111305", sprintf("1113%s", seq(10, 45, 5)))
dirigentes <- c("111405", sprintf("1114%s", seq(10, 15, 5)))
gestores <- c("111505", sprintf("1115%s", seq(10, 10, 5)))

ocup.servpublic.n <-
  as.numeric(c (
    legisladores,
    dirigentes.gerais,
    magistrados,
    dirigentes,
    gestores
  ))

#113 - CHEFES DE PEQUENAS POP

chefes<-c("113005", sprintf("1130%s", seq(10, 15, 5)))

#114- DIRIGENTES E ADMINISTRADORES DE ORG PUBLICAS

partido.politico <-c("114105")
patronais<-c("114205", sprintf("1142%s", seq(10, 10, 5)))
religioso <-c("114305")
ong<-c("114405")

dirigentes.public.n <-
  as.numeric(c (
    legisladores,
    dirigentes.gerais,
    magistrados,
    dirigentes,
    gestores,
    chefes,
    partido.politico,
    patronais,
    religioso,
    ong
  ))

#Geral: 11, 111, 1111, 1141, 1141, 114105, 1112
mortalidade.vereador<-NA
mortalidade.politicos<-NA
  
for(i in 2014:2018) {
  load(
    str_c(
      "mortalidade_",
      i,
      ".RData"
    )
  )
  
  aux <- mortalidade %>% subset(OCUP %in% ocup.vereador.n)
  mortalidade.vereador <- rbind(mortalidade.vereador, aux)
  
  aux <- mortalidade %>% subset(OCUP %in% ocup.servpublic.n)
  mortalidade.politicos <- rbind(mortalidade.politicos, aux)
}

mortalidade.politicos$DTOBITO<-as.Date(mortalidade.politicos$DTOBITO, "%d%m%Y")

data<- strsplit(as.character(mortalidade.politicos$DTOBITO), '-')
mortalidade.politicos<-data.frame(mortalidade.politicos, do.call(rbind, data))
name<-sprintf("X%s",seq(1:3))

setnames(mortalidade.politicos, name, c('ANO','MES','DIA'))
mortalidade.politicos$count<-1


#' (3.1.1) Mortalidade total
mortalidade.politico.municipio.ano <-
  mortalidade.politicos %>% group_by(ANO, CODMUNRES) %>%
  dplyr::summarise(across(count, sum))

mortalidade.politico.municipio<-mortalidade.politicos %>% group_by(CODMUNRES) %>% 
  dplyr::summarise(across(count, sum))

#' (3.1.2) Mortalidade considerando causa da morte





#' ### (3.2)  Census 2010 
#' $Politicians Mortality_m$ = mortes / pop
load("pop2012.Rda")

mortalidade.politico.municipio$CODMUNRES<-as.numeric(mortalidade.politico.municipio$CODMUNRES)
mortalidade.politico.municipio.pop <-
  merge(mortalidade.politico.municipio,
        pop,
        by.x = "CODMUNRES",
        by.y="MUN_id")
mortalidade.politico.municipio.pop <-
  transform(mortalidade.politico.municipio.pop, mortality_rate=(count/POP)*100000)
mortalidade.politico.municipio.pop<- mortalidade.politico.municipio.pop %>% 
  subset(select=c("CODMUNRES", "mortality_rate"))

names(mortalidade.politico.municipio.pop)[names(mortalidade.politico.municipio.pop) == 'CODMUNRES'] <-
  'SIGLA_UE'



#' ## (4) OLS
#' ### (4.1) All Elections 
#' $Cassada_m = /alpha + /beta Treatment_m + u_m$
#' Esperado: beta<0 mesmo partido faz com que candidatura seja cassada com menor prob.


#' 2016 -> codigos correspondem a esse ano

problema_candidatura_cod<-c(4,5,6,7,8,9,10,18,11,13,20,14)
candidate.prefeito.antigo$problema_cand<-0
candidate.prefeito.antigo$problema_cand[candidate.prefeito.antigo$CD_DETALHE_SITUACAO_CAND  %in% 
                                          problema_candidatura_cod] <-  1
candidate.prefeito.antigo$problema_cand[is.na(candidate.prefeito.antigo$CD_DETALHE_SITUACAO_CAND)] <-
  NA



treat1 <-"treatment_partido"

treat2 <-"treatment_coligacao"

control <- c("factor(CD_GENERO)","factor(CD_COR_RACA)")

fe1 <- "| 0"
fe2 <- "| 0"
cluster<- "| 0"

formula <- paste('problema_cand ~', 
                 paste(c( treat1, control), collapse = "+"),
                 fe1, "| 0", cluster )

reg.1<-felm(formula(formula), data=candidate.prefeito.antigo)

t<- stargazer(
  reg.1,
  type = "text",
  omit = c("Constant", control),
  covariate.labels = c(
    "Treatment"
  ),
  keep.stat = "n",
  label = "reg1",
  add.lines=list(c("Year Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes"), 
                 c("Candidate Characteristics", "Yes", "Yes", "Yes", "Yes", "Yes"),
                 c("Municipality Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes")
  ),
  style = "AER",
  notes.append=FALSE,
  dep.var.labels.include = F
)


stargazernote(t, note=note.latex, outputfile=str_c("results/reg/reg1", ".tex"))





#' ### (4.2) Dummy Close elections
#' $Politicians Mortality_m = /alpha + /beta Close_m+ u_m$
#' #' Esperado: beta>0 mesmo partido faz com que candidatura seja cassada com menor prob.

load("results.Rda")

votacao.prefeito <- votacao %>% subset(ANO_ELEICAO == year - 4)
votacao.prefeito <-
  votacao.prefeito %>%  subset(DESCRICAO_CARGO == "PREFEITO")
votacao.prefeito$SIGLA_UE <-as.numeric(votacao.prefeito$SIGLA_UE)

detach(package:plyr)

#' SUM

votacao.prefeito.sum <- votacao.prefeito %>% 
group_by(SIGLA_UE,ANO_ELEICAO) %>% 
  summarize(SUM_VOTOS = sum(TOTAL_VOTOS))

#' MAX

votacao.prefeito.loser <- votacao.prefeito %>% subset(CODIGO_SIT_CAND_TOT != 1)
votacao.prefeito.loser <- votacao.prefeito.loser %>% 
  group_by(SIGLA_UE,ANO_ELEICAO) %>% 
  summarize(MAX_VOTO_LOSER  = max(TOTAL_VOTOS))

#' MIN

votacao.prefeito.winner <- votacao.prefeito %>% subset(CODIGO_SIT_CAND_TOT == 1)
votacao.prefeito.winner <- votacao.prefeito.winner %>% 
  subset(select=c("SIGLA_UE","ANO_ELEICAO","TOTAL_VOTOS")) 
names(votacao.prefeito.winner)[names(votacao.prefeito.winner) == 'TOTAL_VOTOS'] <-
     'MIN_VOTO_WINNER'

#' MERGE

votacao.prefeito.merge <-
  merge(votacao.prefeito,
        votacao.prefeito.sum,
        by = c("ANO_ELEICAO", "SIGLA_UE"))

votacao.prefeito.merge <-
  merge(votacao.prefeito.merge,
        votacao.prefeito.loser,
        by = c("ANO_ELEICAO", "SIGLA_UE"))

votacao.prefeito.merge <-
  merge(votacao.prefeito.merge,
        votacao.prefeito.winner,
        by = c("ANO_ELEICAO", "SIGLA_UE"))


votacao.prefeito.merge <-
  transform(votacao.prefeito.merge, share_munano =(TOTAL_VOTOS/SUM_VOTOS))  

votacao.prefeito.merge <-
  transform(votacao.prefeito.merge, share_min_winner   =(MIN_VOTO_WINNER/SUM_VOTOS))  

votacao.prefeito.merge <-
  transform(votacao.prefeito.merge, share_max_loser    =(MAX_VOTO_LOSER/SUM_VOTOS))  

votacao.prefeito.merge$running_variable <-
  with(
    votacao.prefeito.merge,
    ifelse(
      DESC_SIT_CAND_TOT == 1,
      share_munano - share_max_loser,
      share_munano - share_min_winner
    )
  )


library(rddensity)
summary(rdbwdensity(X = votacao.prefeito.merge$running_variable))

band<-rdbwdensity(X = votacao.prefeito.merge$running_variable)
band_l<-band$h[1,1]
band_r<-band$h[2,1]

votacao.prefeito.merge$close_election<- 0
votacao.prefeito.merge$close_election[votacao.prefeito.merge$running_variable <
                                        band_l &
                                        votacao.prefeito.merge$running_variable > -band_l] <- 1
votacao.prefeito.merge$close_election[is.na(votacao.prefeito.merge$running_variable)] <-NA

votacao.prefeito.merge$SIGLA_UE <-as.numeric(votacao.prefeito.merge$SIGLA_UE)
votacao.prefeito.merge.final <-
  merge(votacao.prefeito.merge,
        mortalidade.politico.municipio.pop,
        by = "SIGLA_UE")



#' Regressao

treat1 <-"close_election"

formula <- paste('problema_cand ~', 
                 paste(c( treat1), collapse = "+"),
                 fe1, "| 0", cluster )

reg.2<-felm(formula(formula), data=votacao.prefeito.merge.final)

t<- stargazer(
  reg.2,
  type = "text",
  omit = c("Constant", control),
  covariate.labels = c(
    "Treatment"
  ),
  keep.stat = "n",
  label = "reg2",
  add.lines=list(c("Year Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes"), 
                 c("Candidate Characteristics", "Yes", "Yes", "Yes", "Yes", "Yes"),
                 c("Municipality Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes")
  ),
  style = "AER",
  notes.append=FALSE,
  dep.var.labels.include = F
)


stargazernote(t, note=note.latex, outputfile=str_c("results/reg/reg2", ".tex"))


#' ### (4.3)
#' $Politicians Mortality_m = /alpha + /beta Treatment_m+ u_m$
#' Esperado: beta<0 mesmo partido faz mortalidade seja menor.





#' ## (5) RDD - eleicao passada

  
  
