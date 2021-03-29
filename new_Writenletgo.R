library (readxl)
library(stringr)
library(dplyr)
library(tidyverse)
library(Hmisc)
library(RVAideMemoire)
library(car)
library(rstatix)
library(PMCMRplus)



# carga dos dados CSV

# carrega o registos.csv ignorando os headers
registos<-read.csv("registos.csv",header=TRUE )          #registos

#cria nomes novos para as colunas, respeitando o set de caracteres do MAC (não consigo testar aqui, ok ?)
names(registos)<-c("Registro",
                   "X.Hash",
                   "Genero",
                   "Data.Nascimento",
                   "Estado.Civil",
                   "Residencia",
                   "Nacionalidade",
                   "Instituicao.onde.estuda",
                   "Tipo.de.curso",
                   "Qual.curso.frequenta.atualmente",
                   "Ajuda.psicologica.no.passado",
                   "Ha.quanto.tempo",
                   "Ajuda.psicologica.na.atualidade",
                   "Uso.de.medicacao",
                   "Consome.bebidas.alcoolicas",
                   "Com.que.regularidade.ingere.bebidas.alcoolicas",
                   "Consumo.de.alcool.prejudica.o.desempenho.da.sua.vida.normal",
                   "Prejudica.em.que.areas.da.sua.vida",
                   "Ja.sentiu.que.precisa.de.ajuda.para.diminuir.o.consumo.de.alcool",
                   "Consome.substâncias.psicoativas.ilegais",
                   "Com.que.regularidade",
                   "Consumo.de.drogas.prejudica.o.desempenho.da.sua.vida.normal",
                   "Em.que.areas.da.sua.vida",
                   "Ja.sentiu.que.precisa.de.ajuda.para.diminuir.o.consumo.de.drogas",
                   "Tem.pensamentos.de.se.magoar.a.si.mesmo.a",
                   "Opcao.que.melhor.descreve.os.pensamentos.de.se.magoar",
                   "Tem.pensamentos.de.por.fim.a.sua.vida",
                   "Opcao.que.melhor.descreve.os.pensamentos.de.se.matar",
                   "Grupo",
                   "Data.criacao.do.login",
                   "Motivo.de.exclusao",
                   "Data.de.exclusao",
                   "Inicio.triagem",
                   "Fim.triagem",
                   "Data.de.assinatura.do.consentimento.informado",
                   "Planejada.D0",
                   "Realizada.D0",
                   "Planejada.D1",
                   "Realizada.D1",
                   "Planejada.D2",
                   "Realizada.D2",
                   "Planejada.D3",
                   "Realizada.D3",
                   "Planejada.D4",
                   "Realizada.D4",
                   "Planejada.D5",
                   "Realizada.D5",
                   "Planejada.D9",
                   "Realizada.D9",
                   "Planejada.D11",
                   "Realizada.D11",
                   "Planejada.D12",
                   "Realizada.D12",
                   "Planejada.D14",
                   "Realizada.D14",
                   "Planejada.D15",
                   "Realizada.D15",
                   "Planejada.D16",
                   "Realizada.D16",
                   "Planejada.D17",
                   "Realizada.D17",
                   "Planejada.D18",
                   "Realizada.D18",
                   "Planejada.D19",
                   "Realizada.D19",
                   "Planejada.D23",
                   "Realizada.D23",
                   "Planejada.D25",
                   "Realizada.D25",
                   "Planejada.D26",
                   "Realizada.D26",
                   "Planejada.D30",
                   "Realizada.D30",
                   "Planejada.D32",
                   "Realizada.D32",
                   "Planejada.D35",
                   "Realizada.D35",
                   "Planejada.D39",
                   "Realizada.D39",
                   "Planejada.D42",
                   "Realizada.D42",
                   "Planejada.D46",
                   "Realizada.D46",
                   "Planejada.D49",
                   "Realizada.D49",
                   "Planejada.D53",
                   "Realizada.D53",
                   "Planejada.D56",
                   "Realizada.D56",
                   "Planejada.D60",
                   "Realizada.D60",
                   "Planejada.D83",
                   "Realizada.D83",
                   "Data.de.cancelamento.voluntario",
                   "Data.ultimo.acesso",
                   "Removido.pela.Cron")

questionarios<-read.csv("questionarios.csv",header=TRUE )        #questionarios
#cria nomes novos para as colunas, respeitando o set de caracteres do MAC (não consigo testar aqui, ok ?)
names(questionarios)<-c("X.Hash",
                        "Grupo",
                        "Momento",
                        "Nome.questionario",
                        "Perguntas",
                        "Resposta",
                        "Pontos",
                        "Data.prevista",
                        "Data.realizada")

intervencoes<-read.csv("intervencoes.csv",header=TRUE )          #intervençoes

#cria nomes novos para as colunas, respeitando o set de caracteres do MAC (não consigo testar aqui, ok ?)
names(intervencoes)<-c("X.Hash",
                       "Grupo",
                       "Momento",
                       "Nome.intervencao",
                       "Texto",
                       "Data.prevista",
                       "Data.realizada",
                       "Acesso.a.lupa",
                       "Data.de.acesso.a.lupa",
                       "Data.de.acesso.ao.zoom")

# cria data frame com registos a serem eliminados da base

registos_teste <-registos[seq(1:19),]       # primeiros 19 registros teste

# adicionaos registros de teste que foram criados 
registos_teste<-rbind(registos_teste,registos[which(registos$X.Hash =="#05fba85d6a7821"),])
registos_teste<-rbind(registos_teste,registos[which(registos$X.Hash =="#05fb3381462bb5"),])
registos_teste<-rbind(registos_teste,registos[which(registos$X.Hash =="#05fafd9818e70f"),])
registos_teste<-rbind(registos_teste,registos[which(registos$X.Hash =="#05faf8b5c9bf92"),])
registos_teste<-rbind(registos_teste,registos[which(registos$X.Hash =="#05fb33946e5c8a"),])
registos_teste<-rbind(registos_teste,registos[which(registos$X.Hash =="#05fb3f6082e530"),])
registos_teste<-rbind(registos_teste,registos[which(registos$X.Hash =="#05fb3f9524a843"),])
registos_teste<-rbind(registos_teste,registos[which(registos$X.Hash =="#060083ac8e15c1"),])
registos_teste<-rbind(registos_teste,registos[which(registos$X.Hash =="#0601f153864315"),])
registos_teste<-rbind(registos_teste,registos[which(registos$X.Hash =="#0601f1575b248b"),])
registos_teste<-rbind(registos_teste,registos[which(registos$X.Hash =="#0601f15c0bb46d"),])
registos_teste<-rbind(registos_teste,registos[which(registos$X.Hash =="#05fb80cc2145db"),])

# elimina registos,questionarios e intervençoes dos participantes de teste
for(i in 1:nrow(registos_teste)) {
  questionarios<-questionarios[which(questionarios$X.Hash != registos_teste$X.Hash[i] ),]
  intervencoes<-intervencoes[which(intervencoes$X.Hash != registos_teste$X.Hash[i] ),]
  registos<-registos[which(registos$X.Hash != registos_teste$X.Hash[i] ),]
}

#grava arquivos com dados dos participantes puros
write.csv2(registos,"registos_completo.csv") 
write.csv2(questionarios,"questionarios_completo.csv")
write.csv2(intervencoes,"intervencoes_completo.csv")


# daqui para frente so trata os Grupos da Fase Dois

registos<-registos[which(registos$Grupo=="Experimental FaseDois"
                         | registos$Grupo=="Controlo FaseDois"),]

intervencoes<-intervencoes[which(intervencoes$Grupo=="Experimental FaseDois"
                                 | intervencoes$Grupo=="Controlo FaseDois"),]

questionarios<-questionarios[which(questionarios$Grupo=="Experimental FaseDois"
                                   | questionarios$Grupo=="Controlo FaseDois"),]


# torna o campo um objeto de formato "date" 
registos$Data.criacao.do.login<-as.Date(registos$Data.criacao.do.login)
#cria coluna com idade de participantes
# antes prepara o objeto Data.Nascimento
registos$Data.Nascimento <-as.Date(paste(
  str_sub(registos$Data.Nascimento,7,10),
  str_sub(registos$Data.Nascimento,4,5),
  str_sub(registos$Data.Nascimento,1,2) ,
  sep="-" ))
registos$Idade<-as.integer((Sys.Date()-registos$Data.Nascimento)/365)

# ajusta objeto Pontos nos questionarios para formato numerico

questionarios$Pontos<- as.integer(questionarios$Pontos)

# corrige nome do questionario Satisfaction With Life Scale
questionarios$Nome.questionario<-replace(questionarios$Nome.questionario, 
                                         questionarios$Nome.questionario=="SLWS",
                                         "SWLS")

#ajusta questionario Auto Distanciamento
questionarios$Nome.questionario<-replace(questionarios$Nome.questionario, 
                                         questionarios$Nome.questionario=="Auto Distanciamento" &
                                           str_sub(questionarios$Perguntas,1,5)=="1. En",
                                         "Auto Distanciamento 1")

questionarios$Nome.questionario<-replace(questionarios$Nome.questionario, 
                                         questionarios$Nome.questionario=="Auto Distanciamento" &
                                           str_sub(questionarios$Perguntas,1,11)=="2. Enquanto",
                                         "Auto Distanciamento 2")

# ajusta questionario OQ10 perguntas 1 a 5, inverte pontua??o
for(i in 1:nrow(questionarios)) {
  if (questionarios$Nome.questionario[i]=="OQ10" & questionarios$Perguntas[i]=="1. Sou uma pessoa feliz") {
    questionarios$Pontos[i]<-4-questionarios$Pontos[i]
  } else if (questionarios$Nome.questionario[i]=="OQ10" & questionarios$Perguntas[i]=="2. Estou satisfeito(a) com a minha vida") {
    questionarios$Pontos[i]<-4-questionarios$Pontos[i]
  } else if (questionarios$Nome.questionario[i]=="OQ10" & questionarios$Perguntas[i]=="3. Estou satisfeito(a) com as minhas relações com os outros") {
    questionarios$Pontos[i]<-4-questionarios$Pontos[i]
  } else if (questionarios$Nome.questionario[i]=="OQ10" & questionarios$Perguntas[i]=="4. Sinto-me amado(a) e desejado(a)") {
    questionarios$Pontos[i]<-4-questionarios$Pontos[i]
  } else if (questionarios$Nome.questionario[i]=="OQ10" & questionarios$Perguntas[i]=="5. Sinto-me satisfeito(a) com as minhas relações afetivas / amorosas"){
    questionarios$Pontos[i]<-4-questionarios$Pontos[i]
  }
}

# ajusta pontua??o do questionario RRQ perguntas 0>1,1>2,2>3,3,4
#RRQ_r perguntas 2,4,5,9 e 10
# RRQ_c restante das perguntas

for(i in 1:nrow(questionarios)) {
  if (questionarios$Nome.questionario[i]=="RRQ" & str_sub(questionarios$Perguntas[i],1,8)=="1. Penso") {
    questionarios$Pontos[i]<-questionarios$Pontos[i]+1
    questionarios$Nome.questionario[i]<- "RRQ_C"
  } else if (questionarios$Nome.questionario[i]=="RRQ" & str_sub(questionarios$Perguntas[i],1,18)=="2. Analiso eventos") {
    questionarios$Pontos[i]<-questionarios$Pontos[i]+1
    questionarios$Nome.questionario[i]<- "RRQ_R"
    
  } else if (questionarios$Nome.questionario[i]=="RRQ" & str_sub(questionarios$Perguntas[i],1,9)=="3. Penso:") {
    questionarios$Pontos[i]<-questionarios$Pontos[i]+1
    questionarios$Nome.questionario[i] <-"RRQ_C"
    
  } else if (questionarios$Nome.questionario[i]=="RRQ" & str_sub(questionarios$Perguntas[i],1,17)=="4. Deixo-me levar") {
    questionarios$Pontos[i]<-questionarios$Pontos[i]+1
    questionarios$Nome.questionario[i]="RRQ_R"
    
  } else if (questionarios$Nome.questionario[i]=="RRQ" & str_sub(questionarios$Perguntas[i],1,17)=="5. Escrevo aquilo"){
    questionarios$Pontos[i]<-questionarios$Pontos[i]+1
    questionarios$Nome.questionario[i]="RRQ_R"
    
  } else if (questionarios$Nome.questionario[i]=="RRQ" & str_sub(questionarios$Perguntas[i],1,18)=="6. Penso acerca de"){
    questionarios$Pontos[i]<-questionarios$Pontos[i]+1
    questionarios$Nome.questionario[i]="RRQ_C"
    
  }  else if (questionarios$Nome.questionario[i]=="RRQ" & str_sub(questionarios$Perguntas[i],1,9)=="7. Penso:"){
    questionarios$Pontos[i]<-questionarios$Pontos[i]+1
    questionarios$Nome.questionario[i]="RRQ_C"
    
  }  else if (questionarios$Nome.questionario[i]=="RRQ" & str_sub(questionarios$Perguntas[i],1,9)=="8. Penso:"){
    questionarios$Pontos[i]<-questionarios$Pontos[i]+1
    questionarios$Nome.questionario[i]="RRQ_C"
    
  }else if (questionarios$Nome.questionario[i]=="RRQ" & str_sub(questionarios$Perguntas[i],1,10)=="9. Analiso"){
    questionarios$Pontos[i]<-questionarios$Pontos[i]+1
    questionarios$Nome.questionario[i]="RRQ_R"
    
  } else if (questionarios$Nome.questionario[i]=="RRQ" & str_sub(questionarios$Perguntas[i],1,12)=="10. Vou para"){
    questionarios$Pontos[i]<-questionarios$Pontos[i]+1
    questionarios$Nome.questionario[i]="RRQ_R"
    
  }
}


# ajusta questionario PANAS perguntas positivas e negativas
for(i in 1:nrow(questionarios)) {
  if (questionarios$Perguntas[i]=="Agradavelmente surpreendido") {
    questionarios$Nome.questionario[i]<-"PANAS_positivo"
  } else if (questionarios$Perguntas[i]=="Ativo") {
    questionarios$Nome.questionario[i]<-"PANAS_positivo"
  } else if (questionarios$Perguntas[i]=="Caloroso") {
    questionarios$Nome.questionario[i]<-"PANAS_positivo"
  } else if (questionarios$Perguntas[i]=="Determinado") {
    questionarios$Nome.questionario[i]<-"PANAS_positivo"
  } else if (questionarios$Perguntas[i]=="Encantado") {
    questionarios$Nome.questionario[i]<-"PANAS_positivo"
  } else if (questionarios$Perguntas[i]=="Entusiasmado") {
    questionarios$Nome.questionario[i]<-"PANAS_positivo"
  } else if (questionarios$Perguntas[i]=="Excitado") {
    questionarios$Nome.questionario[i]<-"PANAS_positivo"
  } else if (questionarios$Perguntas[i]=="Inspirado") {
    questionarios$Nome.questionario[i]<-"PANAS_positivo"
  } else if (questionarios$Perguntas[i]=="Interessado") {
    questionarios$Nome.questionario[i]<-"PANAS_positivo"
  } else if (questionarios$Perguntas[i]=="Orgulhoso"){
    questionarios$Nome.questionario[i]<-"PANAS_positivo"
  }
}

# ajusta questionario IIP32 para clusters de perguntas
for(i in 1:nrow(questionarios)) {
  if (questionarios$Perguntas[i]=="10. Mostrar afeto pelos outros") {
    questionarios$Nome.questionario[i]<-"IIP32_Cold/Distant"
  } else if (questionarios$Perguntas[i]=="4. Dizer a uma pessoa que pare de me aborrecer") {
    questionarios$Nome.questionario[i]<-"IIP32_Nonassertive"
  } else if (str_sub(questionarios$Perguntas[i],1,4)=="1. D") {
    questionarios$Nome.questionario[i]<-"IIP32_Overly_accomodating"
  } else if (questionarios$Perguntas[i]=="11. Relacionar-me com as outras pessoas") {
    questionarios$Nome.questionario[i]<-"IIP32_Cold/Distant"
  } else if (questionarios$Perguntas[i]=="13. Experienciar um sentimento de amor por outra pessoa") {
    questionarios$Nome.questionario[i]<-"IIP32_Cold/Distant"
  } else if (questionarios$Perguntas[i]=="15. Sentir-me próximo/a de outras pessoas") {
    questionarios$Nome.questionario[i]<-"IIP32_Cold/Distant"
  } else if (questionarios$Perguntas[i]=="22. Sou demasiado agressivo/a com as outras pessoas") {
    questionarios$Nome.questionario[i]<-"IIP32_Domineering/controlling"
  } else if (questionarios$Perguntas[i]=="25. Tento controlar demasiadamente as outras pessoas") {
    questionarios$Nome.questionario[i]<-"IIP32_Domineering/controlling"
  } else if (questionarios$Perguntas[i]=="28. Frequentemente manipulo as outras pessoas para obter o que eu quero") {
    questionarios$Nome.questionario[i]<-"IIP32_Domineering/controlling"
  } else if (questionarios$Perguntas[i]=="30. Discuto muito com as outras pessoas") {
    questionarios$Nome.questionario[i]<-"IIP32_Domineering/controlling"
  } else if (questionarios$Perguntas[i]=="3. Manter a privacidade dos meus assuntos") {
    questionarios$Nome.questionario[i]<-"IIP32_Intrusive/Needy"
  } else if (questionarios$Perguntas[i]=="21. Abro-me demais com as outras pessoas"){
    questionarios$Nome.questionario[i]<-"IIP32_Intrusive/Needy"
  } else if (questionarios$Perguntas[i]=="24. Quero muito que reparem em mim") {
    questionarios$Nome.questionario[i]<-"IIP32_Intrusive/Needy"
  } else if (str_sub(questionarios$Perguntas[i],1,6)=="29. Co") {
    questionarios$Nome.questionario[i]<-"IIP32_Intrusive/Needy"
  } else if (questionarios$Perguntas[i]=="27. Sou demasiado generoso/a com as outras pessoas") {
    questionarios$Nome.questionario[i]<-"IIP32_Self_sacrifficing"
  }else if (questionarios$Perguntas[i]=="6. Confrontar as pessoas com os problemas que aparecem") {
    questionarios$Nome.questionario[i]<-"IIP32_Nonassertive"
  }else if (questionarios$Perguntas[i]=="7. Ser assertivo/a com as outras pessoas") {
    questionarios$Nome.questionario[i]<-"IIP32_Nonassertive"
  }else if (questionarios$Perguntas[i]=="12. Ser firme quando preciso de o ser") {
    questionarios$Nome.questionario[i]<-"IIP32_Nonassertive"
  }else if (questionarios$Perguntas[i]=="1. Dizer não ? s outras pessoas") {
    questionarios$Nome.questionario[i]<-"IIP32_Overly_accomodating"
  }else if (questionarios$Perguntas[i]=="8. Deixar que os outros se apercebam quando estou zangado/a") {
    questionarios$Nome.questionario[i]<-"IIP32_Overly_accomodating"
  }else if (questionarios$Perguntas[i]=="20. Ser assertivo/a sem me preocupar em magoar os outros") {
    questionarios$Nome.questionario[i]<-"IIP32_Overly_accomodating"
  }else if (questionarios$Perguntas[i]=="31. Frequentemente deixo que os outros tirem partido de mim") {
    questionarios$Nome.questionario[i]<-"IIP32_Overly_accomodating"
  }else if (questionarios$Perguntas[i]=="23. Tento agradar demasiadamente aos outros") {
    questionarios$Nome.questionario[i]<-"IIP32_Self_sacrifficing"
  }else if (str_sub(questionarios$Perguntas[i],1,6)=="26. Fr") {
    questionarios$Nome.questionario[i]<-"IIP32_Self_sacrifficing"
  }else if (questionarios$Perguntas[i]=="22. Sou demasiado agressivo/a com as outras pessoas") {
    questionarios$Nome.questionario[i]<-"IIP32_Self_sacrifficing"
  }else if (questionarios$Perguntas[i]=="32. Sou muito afectado/a pelo sofrimento das outras pessoas") {
    questionarios$Nome.questionario[i]<-"IIP32_Self_sacrifficing"
  }else if (questionarios$Perguntas[i]=="2. Juntar-me em grupo") {
    questionarios$Nome.questionario[i]<-"IIP32_Socially_inhibited"
  }else if (questionarios$Perguntas[i]=="5. Apresentar-me a pessoas novas") {
    questionarios$Nome.questionario[i]<-"IIP32_Socially_inhibited"
  }else if (questionarios$Perguntas[i]=="9. Socializar com as outras pessoas") {
    questionarios$Nome.questionario[i]<-"IIP32_Socially_inhibited"
  }else if (questionarios$Perguntas[i]=="19. Pedir a outras pessoas para se envolverem socialmente comigo") {
    questionarios$Nome.questionario[i]<-"IIP32_Socially_inhibited"
  }else if (questionarios$Perguntas[i]=="14. Ser apoiante relativamente aos objetivos de vida de outra pessoa") {
    questionarios$Nome.questionario[i]<-"IIP32_Vindictive_Self-centered"
  }else if (questionarios$Perguntas[i]=="16. Preocupar-me realmente com os problemas dos outros") {
    questionarios$Nome.questionario[i]<-"IIP32_Vindictive_Self-centered"
  }else if (str_sub(questionarios$Perguntas[i],1,6)=="17. P?") {
    questionarios$Nome.questionario[i]<-"IIP32_Vindictive_Self-centered"
  }else if (questionarios$Perguntas[i]=="18. Sentir-me bem com a felicidade de outra pessoa") {
    questionarios$Nome.questionario[i]<-"IIP32_Vindictive_Self-centered"
  } 
}

# 13/03/2021 ajusta questionario QAP para clusters QAP_W (perguntas 2, 3, 4 e 5) e QAP_D (perguntas 1,6,7,8 e9)
for(i in 1:nrow(questionarios)) {
  if (questionarios$Perguntas[i]=="1. Não tenho conseguido mudar") {
    questionarios$Nome.questionario[i]<-"QAP_D"
  } else if (questionarios$Perguntas[i]=="2. Por vezes coloco tudo que envolve a mudança em causa") {
    questionarios$Nome.questionario[i]<-"QAP_W"
  } else if (questionarios$Perguntas[i]=="3. Umas vezes penso que tudo vai correr bem, outras que tudo vai ficar na mesma ou piorar") {
    questionarios$Nome.questionario[i]<-"QAP_W"
  } else if (questionarios$Perguntas[i]=="4. Tudo pode piorar de um momento para o outro") {
    questionarios$Nome.questionario[i]<-"QAP_W"
  } else if (questionarios$Perguntas[i]=="5. Umas vezes sinto que tenho mudado e outras sinto que estou na mesma") {
    questionarios$Nome.questionario[i]<-"QAP_W"
  } else if (questionarios$Perguntas[i]=="6. Volto sempre ao mesmo") {
    questionarios$Nome.questionario[i]<-"QAP_D"
  } else if (questionarios$Perguntas[i]=="7. Há sempre algo que me puxa para trás") {
    questionarios$Nome.questionario[i]<-"QAP_D"
  } else if (questionarios$Perguntas[i]=="8. Tanto tenho a certeza do que quero mudar como logo a seguir sinto-me perdido(a)") {
    questionarios$Nome.questionario[i]<-"QAP_D"
  } else if (questionarios$Perguntas[i]=="9. Tudo permanece igual apesar dos meus esforços") {
    questionarios$Nome.questionario[i]<-"QAP_D"
  }
}

df<-data.frame(ordem=seq(nrow(questionarios)))
str(df)

questionarios<-cbind(questionarios,df)

#Grava arquivos somente com participantes de facto
write.csv2(registos[-1],"registos_true.csv")              #registos.csv
write.csv2(questionarios,"questionarios_true.csv")
write.csv2(intervencoes,"intervencoes_true.csv")



#preserva o dataframe questionarios
questoes<-questionarios
# Ajeitando a df Quest?es
questoes$X.Hash<-as.factor(questoes$X.Hash)
questoes$Grupo<-as.factor(questoes$Grupo)
questoes$Momento<-as.factor(questoes$Momento)
questoes$Nome.questionario<-as.factor(questoes$Nome.questionario)
questoes$Pontos<-as.integer(questoes$Pontos)

sum_questoes<-aggregate(Pontos~X.Hash+Grupo+Momento+Nome.questionario,questoes,sum)


#cria linhas para o RRQ agregado
df1<-sum_questoes[which(sum_questoes$Nome.questionario=="RRQ_C"),]
df2<-sum_questoes[which(sum_questoes$Nome.questionario=="RRQ_R"),]
df1$Pontos<-df1$Pontos+df2$Pontos
df1$Nome.questionario<-"RRQ"
sum_questoes<-rbind(sum_questoes,df1)

#cria linhas para o IIP agregado
df1<-sum_questoes[which(sum_questoes$Nome.questionario=="IIP32_Domineering/controlling"),]
df2<-sum_questoes[which(sum_questoes$Nome.questionario=="IIP32_Socially_inhibited"),]
df3<-sum_questoes[which(sum_questoes$Nome.questionario=="IIP32_Intrusive/Needy"),]
df4<-sum_questoes[which(sum_questoes$Nome.questionario=="IIP32_Overly_accomodating"),]
df5<-sum_questoes[which(sum_questoes$Nome.questionario=="IIP32_Cold/Distant"),]
df6<-sum_questoes[which(sum_questoes$Nome.questionario=="IIP32_Vindictive_Self-centered"),]
df7<-sum_questoes[which(sum_questoes$Nome.questionario=="IIP32_Self_sacrifficing"),]
df8<-sum_questoes[which(sum_questoes$Nome.questionario=="IIP32_Nonassertive"),]
df1$Pontos<-df1$Pontos+df2$Pontos+df3$Pontos+df4$Pontos+df5$Pontos+df6$Pontos+df7$Pontos+df8$Pontos
df1$Nome.questionario<-"IIP32"
sum_questoes<-rbind(sum_questoes,df1)

#cria linhas para o Auto distanciamento agregado
df1<-sum_questoes[which(sum_questoes$Nome.questionario=="Auto Distanciamento 1"),]
df2<-sum_questoes[which(sum_questoes$Nome.questionario=="Auto Distanciamento 2"),]
df1$Pontos<-df1$Pontos+df2$Pontos
df1$Nome.questionario<-"Auto Distanciamento"
sum_questoes<-rbind(sum_questoes,df1)

#cria linhas para o QAP agregado
df1<-sum_questoes[which(sum_questoes$Nome.questionario=="QAP_D"),]
df2<-sum_questoes[which(sum_questoes$Nome.questionario=="QAP_W"),]
df1$Pontos<-df1$Pontos+df2$Pontos
df1$Nome.questionario<-"QAP"
sum_questoes<-rbind(sum_questoes,df1)



#duplica as hashs para grupo de Controlo EX
df1<-sum_questoes[which(sum_questoes$Grupo=="Controlo FaseDois"),]
df1$X.Hash<-paste(df1$X.Hash,"C",sep="")
sum_questoes<-rbind(sum_questoes,df1)

#s? interessa grupos Experimental FaseDois e Controlo FaseDois
#df<-df[which(df$Grupo=="Experimental FaseDois" |df$Grupo=="Controlo FaseDois"), ]

#Carrega as matrizes refer?ncia
Matriz_Experimental<-read_excel("Matrix_referencias.xlsx", sheet = "Matriz_Experimental" )
Matriz_Experimental<-as.data.frame(Matriz_Experimental)

Matriz_Controlo<-read_excel("Matrix_referencias.xlsx", sheet = "Matriz_Controlo")
Matriz_Controlo<-as.data.frame(Matriz_Controlo)


Matriz_Controlo_Ex<-read_excel("Matrix_referencias.xlsx", sheet = "Matriz_Controlo_Ex")
Matriz_Controlo_Ex<-as.data.frame(Matriz_Controlo_Ex)

Nomes<-as.vector(read_excel("Matrix_referencias.xlsx", sheet = "Matriz_master"))


#Cria um dataframe para a Matriz_master
Matriz_master<-data.frame(matrix(ncol=ncol(Nomes),nrow=nrow(registos)))
colnames(Matriz_master)<-names(Nomes)

#Carrega as hashs na Matriz_Master com dados do registos
for(i in 1:nrow(registos)){
  Matriz_master$hash[i]<-registos$X.Hash[i]
  Matriz_master$grupo[i]<-registos$Grupo[i]
  Matriz_master$idade[i]<-registos$Idade[i]
  Matriz_master$genero[i] <-registos$Genero[i]
  Matriz_master$estado_civil[i]  <-registos$Estado.Civil[i]
  Matriz_master$residencia[i]  <-registos$Residencia[i]
  Matriz_master$nacionalidade[i]  <-registos$Nacionalidade[i]
  Matriz_master$tipo_curso[i]  <-registos$Tipo.de.curso[i]
  Matriz_master$psicoterapia_no_passado[i] <-registos$Ajuda.psicologica.no.passado[i] 
  Matriz_master$psicoterapia_no_presente[i] <-registos$Ajuda.psicologica.na.atualidade[i] 
}



#Carrega as hashs na Matriz_Master com dados do questionarios

# grau de impacto do problema pre
df2<-questionarios[which(str_sub(questionarios$Perguntas,1,29)=="Qual o impacto deste problema"),]
for(i in 1:nrow(df2)){
  Matriz_master[which(Matriz_master$hash==df2$X.Hash[i]),]$grau_impacto_problema_PRE<-df2$Resposta[i]
}

# natureza do problema pre
df1<-questionarios[which(str_sub(questionarios$Perguntas,1,36)=="Qual(is) o(s) domínio(s) do problema"),]
for(i in 1:nrow(df1)){
  Matriz_master[which(Matriz_master$hash==df1$X.Hash[i]),]$Natureza_Problema_Pre<-df1$Resposta[i]
}


# Cria o Grupo Controlo_EX
df1<-Matriz_master[which(Matriz_master$grupo=="Controlo FaseDois" ),]
df1$grupo<-"Controlo Ex"
df1$hash<-paste(df1$hash,"C",sep="")
Matriz_master<- rbind(Matriz_master,df1)

#Carregar os FUP?s para Problema
#cria df com as respostas x strings

vetor_resposta<-list("O problema previamente identificado se mantém"="se manteve",
                     "O problema previamente identificado se mantem"="se manteve",
                     "O problema previamente identificado melhorou"="melhorou",
                     "O problema previamente identificado se resolveu"="se resolveu",
                     "O problema previamente identificado piorou"="piorou",
                     "Tenho outro problema mais relevante"="outro mais relevante"
)


# Problema Fup 1 e 2 para grupo Experimental
df1<-questionarios[which(questionarios$Grupo=="Experimental FaseDois"
                         & questionarios$Momento == "11"
                         & questionarios$Nome.questionario=="Problema 2"),]
for(i in 1:nrow(df1)){
  Matriz_master[which(Matriz_master$hash==df1$X.Hash[i]),]$Status_Problema_Fup1<-vetor_resposta[[df1$Resposta[i]]]
}

df1<-questionarios[which(questionarios$Grupo=="Experimental FaseDois"
                         & questionarios$Momento == "18"
                         & questionarios$Nome.questionario=="Problema 2"),]
for(i in 1:nrow(df1)){
  Matriz_master[which(Matriz_master$hash==df1$X.Hash[i]),]$Status_Problema_Fup2<-vetor_resposta[[df1$Resposta[i]]]
}

# Problema Fup 1 para grupo Controlo FaseDois
df1<-questionarios[which(questionarios$Grupo=="Controlo FaseDois"
                         & questionarios$Nome.questionario=="Problema - Follow up Grupo Controle"
                         & questionarios$Perguntas=="Sobre o problema identificado na primeira sessão, por favor escolha uma das opções:" ),]

for(i in 1:nrow(df1)){
  Matriz_master[which(Matriz_master$hash==df1$X.Hash[i]),]$Status_Problema_Fup1<-vetor_resposta[[df1$Resposta[i]]]
}

# Problema Fup 1 e Fup 2para grupo Controlo Ex
df1<-questionarios[which(questionarios$Grupo=="Controlo FaseDois"
                         & questionarios$Nome.questionario=="Problema 2"
                         & questionarios$Momento == "25"),]
for(i in 1:nrow(df1)){
  Matriz_master[which(Matriz_master$hash==paste(df1$X.Hash[i],"C",sep="")),]$Status_Problema_Fup1<-vetor_resposta[[df1$Resposta[i]]]
}

df1<-questionarios[which(questionarios$Grupo=="Controlo FaseDois"
                         & questionarios$Nome.questionario=="Problema 2"
                         & questionarios$Momento == "32"),]
for(i in 1:nrow(df1)){
  Matriz_master[which(Matriz_master$hash==paste(df1$X.Hash[i],"C",sep="")),]$Status_Problema_Fup2<-vetor_resposta[[df1$Resposta[i]]]
}






hashs_index<-1:nrow(Matriz_master)
c_vector<-(Matriz_master$hash)
names(hashs_index)<-c_vector



### cria uma df com indexa??o dos nomes de colunas na Matriz_master
nomes_index<-1:ncol(Matriz_master)
c_vector<-names(Matriz_master)
names(nomes_index)<-c_vector
nomes_index

hashs_index<-1:nrow(Matriz_master)
c_vector<-(Matriz_master$hash)
names(hashs_index)<-c_vector


### trata grupo Experimental FaseDois
df1<-Matriz_master[which(Matriz_master$grupo=="Experimental FaseDois" ),]

# trata cada hash desse grupo
for(index_lin_de in 1:nrow(df1))  
{
  
  for(i in 1:nrow(Matriz_Experimental))
  {
    
    for(j in 2:ncol(Matriz_Experimental))
    {
      if(!is.na(Matriz_Experimental[i,j])){
        #determina a celula na Matriz_master
        index_col_para<-nomes_index[Matriz_Experimental[i,j]]
        index_lin_para<-hashs_index[df1$hash[index_lin_de]]
        #procura o valor na df de resultados dos questionarios
        df<-sum_questoes[which(sum_questoes$X.Hash==df1$hash[index_lin_de]),]
        df<-df[which(df$Nome.questionario ==Matriz_Experimental$Questionario[i] ),] 
        df<-df[which(df$Momento == str_sub(names(Matriz_Experimental[j]),star=2)),]
        if(nrow(df)!=0){
          Matriz_master[index_lin_para,index_col_para]<-df$Pontos  
        }
      }
    }
  }
}



### trata grupo Controlo FaseDois
df1<-Matriz_master[which(Matriz_master$grupo=="Controlo FaseDois" ),]

# trata cada hash desse grupo
for(index_lin_de in 1:nrow(df1))  
{
  
  for(i in 1:nrow(Matriz_Controlo))
  {
    for(j in 2:ncol(Matriz_Controlo))
    {
      if(!is.na(Matriz_Controlo[i,j])){
        #determina a celula na Matriz_master
        index_col_para<-nomes_index[Matriz_Controlo[i,j]]
        index_lin_para<-hashs_index[df1$hash[index_lin_de]]
        #procura o valor na df de resultados dos questionarios
        df<-sum_questoes[which(sum_questoes$X.Hash==df1$hash[index_lin_de]),]
        df<-df[which(df$Nome.questionario ==Matriz_Controlo$Questionario[i] ),] 
        df<-df[which(df$Momento == str_sub(names(Matriz_Controlo[j]),star=2)),]
        if(nrow(df)!=0){
          Matriz_master[index_lin_para,index_col_para]<-df$Pontos  
        }
      }
    }
  }
}



### trata grupo Controlo Ex
df1<-Matriz_master[which(Matriz_master$grupo=="Controlo Ex" ),]

# trata cada hash desse grupo
for(index_lin_de in 1:nrow(df1))  
{
  
  for(i in 1:nrow(Matriz_Controlo_Ex))
  {
    for(j in 2:ncol(Matriz_Controlo_Ex))
    {
      if(!is.na(Matriz_Controlo_Ex[i,j])){
        #determina a celula na Matriz_master
        index_col_para<-nomes_index[Matriz_Controlo_Ex[i,j]]
        index_lin_para<-hashs_index[df1$hash[index_lin_de]]
        #procura o valor na df de resultados dos questionarios
        df<-sum_questoes[which(sum_questoes$X.Hash==df1$hash[index_lin_de]),]
        df<-df[which(df$Nome.questionario ==Matriz_Controlo_Ex$Questionario[i] ),] 
        df<-df[which(df$Momento == str_sub(names(Matriz_Controlo_Ex[j]),star=2)),]
        if(nrow(df)!=0){
          Matriz_master[index_lin_para,index_col_para]<-df$Pontos  
        }
      }
    }
  }
}

for(i in 1:nrow(Matriz_master)){
  for (j in 1:ncol(Matriz_master)){
    if(is.na(Matriz_master[i,j])){
      Matriz_master[i,j]<-""
    }
  }
}

#### flaga colune Terminou
for (i in 1:nrow(Matriz_master)){
  if(Matriz_master$grupo[i]=="Experimental FaseDois" & Matriz_master$QAP_fup2[i]!=""){
    Matriz_master$Terminou[i]<-"s"
  }
  else if (Matriz_master$grupo[i]=="Controlo FaseDois" & Matriz_master$QAP_fup1[i]!=""){
    Matriz_master$Terminou[i]<-"s"
  }  
  else if (Matriz_master$grupo[i]=="Controlo Ex" & Matriz_master$QAP_fup2[i]!=""){
    Matriz_master$Terminou[i]<-"s"
  }  
  
}

write.csv2(Matriz_master,"Matriz_master.csv")


####################### correlacoes ######################
## limita o universo a quem terminou ##

# Para o grupo experimental
df<-read_csv2("Matriz_master.csv")
df_ex<-Matriz_master[which(Matriz_master$Terminou== "s" & Matriz_master$grupo=="Experimental FaseDois" ),]
df_cont<-Matriz_master[which(Matriz_master$Terminou== "s" & Matriz_master$grupo=="Controlo FaseDois"),]

#seleciona as as colunas com questionarios preparando a matriz
#df<-df[c(17:25)]
df<-df[-c(1:16)]


df<-as.matrix(df)
res<-rcorr(df)
res

write.csv2(res$r,"matriz_correlacao.csv")
write.csv2(res$P,"matriz_correlacao_P.csv")

library(corrplot)
corrplot(res$r, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


#################### t test #####################
df_ex<-Matriz_master[which(Matriz_master$Terminou== "s" & Matriz_master$grupo=="Experimental FaseDois" ),]
df_cont<-Matriz_master[which(Matriz_master$Terminou== "s" & Matriz_master$grupo=="Controlo FaseDois"),]
df_ex$GAD_pre<- as.numeric (df_ex$GAD_pre)
df_cont$GAD_pre<-as.numeric(df_cont$GAD_pre) 

t.test(as.numeric(df_ex$GAD_pre), as.numeric(df_cont$GAD_pre), alternative = "two.sided", var.equal = FALSE)

gad_ex<-as.data.frame(df_ex$GAD_pre)
gad_ex<-cbind(gad_ex,rep(1,length(df_ex$GAD_pre)))
names(gad_ex)<-c("pre_gad","grupo")
gad_cont<-as.data.frame(df_cont$GAD_pre)
gad_cont<-cbind(gad_cont,rep(2,length(df_cont$GAD_pre)))
names(gad_cont)<-c("pre_gad","grupo")
gad_ex<-rbind(gad_ex,gad_cont)
wilcox.test(pre_gad ~ grupo, data=gad_ex, paired=FALSE)

df1<-as.data.frame(df_ex$GAD_pre,rep(1,length(df_ex$GAD_pre)))
df<-as.data.frame(Nivel,GAD_pre,GAD_pos)


######################### Teste t para Amostras Independentes #########################
df_ex<-Matriz_master[which(Matriz_master$Terminou== "s" & Matriz_master$grupo=="Experimental FaseDois" ),]
df_cont<-Matriz_master[which(Matriz_master$Terminou== "s" & Matriz_master$grupo=="Controlo FaseDois"),]
dados<-as.data.frame(df_ex$GAD_pre)
dados<-cbind(dados,rep("Experimental",length(df_ex$GAD_pre)))
names(dados)<-c("GAD_pre","grupo")
df<-as.data.frame(df_cont$GAD_pre)
df<-cbind(df,rep("Controlo",length(df_cont$GAD_pre)))
names(df)<-c("GAD_pre","grupo")
dados<-rbind(dados,df)
dados$grupo<-as.factor(dados$grupo)
dados$GAD_pre<-as.numeric(dados$GAD_pre)


# Verificar se distribuicao normal
# Shapiro por grupo (pacote RVAideMemoire)
# variavel dependente GAD_pre, variavel independente Grupo 
# teste de Shapiro tem H0= a variavel tem distribui??o normal
# H1 distribuicao diferente da normal
# resultado     W     p-value
# se p-value > nivel de significancia n?o rejeita a hipotese nula
# value of the Shapiro-Wilk Test is greater than 0.05, the data is normal.

byf.shapiro(GAD_pre ~ grupo, dados)               # essa funcao eh do pacote RVAideMemorie

# Verificar homogeneidade de variancias
# Teste de Levene (pacote car)
# H0 as variancias dos grupos sao homogeneas
# H1 as variancias dos grupos nao sao homogeneas
# se o Pvalor for maior que o nivel de significancia nao rejeita H)
leveneTest(GAD_pre ~ grupo, dados)

# teste t para amostras independentes
t.test(GAD_pre ~ grupo, dados, var.equal=TRUE)

boxplot(GAD_pre ~ grupo, data = dados, ylab="GAD_pre", xlab="Grupo")


###### testes de MANN-WHITNEY para amostras independentes ##########
# teste bicaudal
# para testes unicaudal, necessario incluir: alternative = "greater" ou alternative = "less"
# wilcox.test(GAD_pre ~ grupo, data = dados, alternative="greater")
# o teste verificarah se eh a mediana do primeiro grupo eh maior que a mediana do segundo
# O R est? considerando "Experimental" como primeiro grupo

# da uma olhada nos dados das duas amostras
# como a distribuicao nao e normal faz mais sentido
# reportar a mediana e a "interquartil range" do que a media

dados %>% group_by(grupo) %>%
  get_summary_stats(GAD_pre, show = c("mean", "sd", "median", "iqr"))

# condicoes para o teste
# variavel dependente numerica
# variavel independente categorica 2 grupos independentes (n?o pareados)
# H0 mediana das diferen?as eh zero
# H1 mediana das diferencas eh diferente de zero
# se p-value > 0,05 n?o rejeita a hipotese nula (as medianas s?o iguais)
# se p-value < 0,05 as medianas s?o diferentes 
# bicaudal por default 

df_ex$GAD_pre<-as.numeric(df_ex$GAD_pre)
df_cont$GAD_pre<-as.numeric(df_cont$GAD_pre)

wilcox.test(GAD_pre ~ grupo, data = dados,paired=FALSE,alternative = "two.sided")

wilcox.test(df_ex$GAD_pre,df_cont$GAD_pre,paired=FALSE,alternative = "two.sided")



par(mfrow=c(1,2))
hist(dados$GAD_pre[dados$grupo == "Experimental"],
     ylab="Frequ?ncia", xlab="GAD Score - pre", main="Experimental")
hist(dados$GAD_pre[dados$grupo == "Controlo"],
     ylab="Frequ?ncia", xlab="GAD Score - pre", main="Controlo")


######################### Teste de Wilcoxon #########################

df_ex<-Matriz_master[which(Matriz_master$Terminou== "s" & Matriz_master$grupo=="Experimental FaseDois" ),]
df_ex$GAD_pre<-as.numeric(df_ex$GAD_pre)
df_ex$GAD_fup1<-as.numeric(df_ex$GAD_fup1)
glimpse(df_ex)
# Variavel dependente tem distribui??o diferente de normal
# variavel independente 2 grupos 
# H0 mediana das diferen?as eh zero
# h1 mediana das diferencas eh diferente de zero
# se p-value > 0,05 n?o rejeita a hipotese nula (as medianas s?o iguais)
# se p-value < 0,05 as medianas s?o diferentes 
# bicaudal por default

wilcox.test(df_ex$GAD_pre,df_ex$GAD_fup1, paired = TRUE,alternative = "two.sided")

# obeserva que n?o ha, estatisticamente falando, diferencas
df_ex$dif <- df_ex$GAD_pre - df_ex$GAD_fup1

df_ex %>% get_summary_stats(GAD_pre,GAD_fup1, dif, show = c("mean", "sd", "median", "iqr"))




### teste de normalidade para todas as amostras ##########
# Shapiro teste para normalidade
# H0 - distribui??o normal 
# H1 distribui??o n?o normal
# se p-valor > 0,05 , distribui??o normal
# se p-value < 0,05 , distribui??o nao normal

df<-read_csv2("Matriz_master.csv")

df_ex<-Matriz_master[which(Matriz_master$Terminou== "s" & Matriz_master$grupo=="Experimental FaseDois" ),]
df_cont<-Matriz_master[which(Matriz_master$Terminou== "s" & Matriz_master$grupo=="Controlo FaseDois"),]
df_cont_ex<-Matriz_master[which(Matriz_master$Terminou== "s" & Matriz_master$grupo=="Controlo Ex"),]

#seleciona as as colunas com questionarios preparando a matriz

df_ex<-df_ex[-c(1:15)]
vetor_normalidade_ex<-df_ex[1,]
for (i in 1:length(vetor_normalidade_ex)){
  s<-shapiro.test(as.numeric(df_ex[,i]))
  vetor_normalidade_ex[i]<-s$p.value
}
write.csv2(vetor_normalidade_ex,file="vetor_normalidade_ex.csv")

# vetor de teste de normalidade para grupo Controlo

df_cont<-df_cont[-c(1:15)]
vetor_normalidade_cont<-df_cont[1,]
elimina<-c()
for (i in 1:length(vetor_normalidade_cont)){
  if(sum(is.na(as.numeric(df_cont[,i])))==nrow(df_cont)){
    elimina<-c(elimina,i)
  }
  else{
    s<-shapiro.test(as.numeric(df_cont[,i]))
    vetor_normalidade_cont[i]<-s$p.value
  }
}
vetor_normalidade_cont<-vetor_normalidade_cont[-elimina]
write.csv2(vetor_normalidade_cont,file="vetor_normalidade_cont.csv")

# vetor de teste de normalidade para grupo Controlo EX
df_cont_ex<-df_cont_ex[-c(1:15)]
vetor_normalidade_cont_ex<-df_cont_ex[1,]
for (i in 1:length(vetor_normalidade_cont_ex)){
  s<-shapiro.test(as.numeric(df_cont_ex[,i]))
  vetor_normalidade_cont_ex[i]<-s$p.value
}
write.csv2(vetor_normalidade_cont_ex,file="vetor_normalidade_cont_ex.csv")


################################Friedman#################################
# ANOVA para amostra pareada + de 2 medidas
# p-value < 0.05 sao diferentes


df_ex<-Matriz_master[which(Matriz_master$Terminou== "s" & Matriz_master$grupo=="Experimental FaseDois" ),]
df_ex<-df_ex[,77:81]

df_ex$QAP_pre<-as.numeric(df_ex$QAP_pre)
df_ex$QAP_fup1<-as.numeric(df_ex$QAP_fup1)
df_ex$QAP_s1<-as.numeric(df_ex$QAP_s1)
df_ex$QAP_s2<-as.numeric(df_ex$QAP_s2)
df_ex$QAP_fup2<-as.numeric(df_ex$QAP_fup2)
df_ex<-as.matrix(df_ex)

summary(df_ex)

friedman.test(df_ex)

posthoc.friedman.nemenyi.test(df_ex)


