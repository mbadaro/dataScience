# Projeto 1 - Detecção de Fraudes no Tráfego de Cliques em Propagandas de Aplicações Mobile

# Obs: Essa é a versão 0. Não funcionou pois a quantidade de classes nas variáveis preditoras
# era muito grande. Sendo assim, ao invés de apagar tudo e começar novamente, decidi manter
# essa versão e fazer as mudanças em uma nova.


## 1 - Definição e Compreensão do Problema de Negócio:

# Criar um algoritmo para a https://www.talkingdata.com/ que possa prever se um usuário fará o
# download de um aplicativo depois de clicar em um anúncio de aplicativo p/ dipositivos móveis.

# Em resumo, neste projeto, você deverá construir um modelo de aprendizado de máquina para 
# determinar se um clique é fraudulento ou não.

# Configurando o diretório de trabalho
setwd("C:/dev/DSAFCD/RAzure/ProjetosFinais/projeto1Classificacao/")
getwd()


## 2 - Coleta/Obtenção dos Dados:

# Para a construção desse projeto, utilizar o dataset disponível no Kaggle em:
# https://www.kaggle.com/c/talkingdata-adtracking-fraud-detection/data

#Data fields - Each row contains a click record, with the following features:
#ip: ip address of click.
#app: app id for marketing.
#device: device type id of user mobile phone (ex: iphone 6 plus, iphone 7, huawei mate 7, etc.)
#os: os version id of user mobile phone
#channel: channel id of mobile ad publisher
#click_time: timestamp of click (UTC)
#attributed_time: if user download the app for after clicking an ad, this is the time of the 
#                 app download
#is_attributed: the target that is to be predicted, indicating the app was downloaded
#Note that ip, app, device, os, and channel are encoded.

clicks <- read.csv("train_sample.csv", header = TRUE, sep = ",")
head(clicks)
str(clicks)
View(clicks)

# Percepções iniciais:
# 1 - app, device, os, channel e is_attributed estão como inteiros mas são categóricas. 
# 2 - click_time e attributed_time estão como categóricas (fatores), mas são datas.
# 3 - A variável attributed_time está diretamente ligada à variável target


## 3 - Data Munging/Transformação/ETL:
require(lubridate)

# Verificando se há valores missing
any(is.na(clicks)) 

# Convertendo variáveis numéricas para fator
clicks$app <- as.factor(clicks$app)
clicks$device <- as.factor(clicks$device)
clicks$os <- as.factor(clicks$os)
clicks$channel <- as.factor(clicks$channel)
clicks$is_attributed <- as.factor(clicks$is_attributed)

# Convetendo variáveis fator para data
clicks$click_time <- as.POSIXct(clicks$click_time)

str(clicks)

# Criando uma variável de hora, com as horas onde os clique foram efetuados
clicks$hour <- hour(clicks$click_time)

# Excluindo a varíavel attributed_time por estar diretamente associada à variável target
clicks$attributed_time <- NULL

View (clicks)


## 4 - Normalização
# Só há uma variável do tipo numérica no DF, então não será feita normalização


## 5 - Análise Exploratória dos Dados:
library(ggplot2)
library(gmodels)

# Distribuição dos dados pelas variáveis

# Pela distruibuição da variável target (abaixo), nitidamente se nota a necessidade da 
# realização do balanceamento de cargas, pois 99.77% do clicks não resultam em download
table(clicks$is_attributed) 
round(prop.table(table(clicks$is_attributed)) * 100, digits = 2)

table(clicks$app) 
round(prop.table(table(clicks$app)) * 100, digits = 2)

table(clicks$device) 
round(prop.table(table(clicks$device)) * 100, digits = 2)

table(clicks$os) 
round(prop.table(table(clicks$os)) * 100, digits = 2)

table(clicks$channel) 
round(prop.table(table(clicks$channel)) * 100, digits = 2)

# Verificando o relacionamento entre 2 variáveis categóricas
CrossTable(x = clicks$app, y = clicks$is_attributed)

CrossTable(x = clicks$device, y = clicks$is_attributed)

CrossTable(x = clicks$os, y = clicks$is_attributed)

CrossTable(x = clicks$channel, y = clicks$is_attributed)

# Gráficos 
# Tentei gerar alguns gráficos, mas com o dataset ainda desbalanceado, não traz muitos
# insigths
ggplot(clicks,aes(app)) + geom_bar()

ggplot(clicks,aes(ip)) + geom_histogram(fill = 'green', color = 'black', alpha = 0.5)

ggplot(clicks,aes(is_attributed)) + geom_bar(aes(fill = factor(is_attributed)), alpha = 0.5)

# Alguns canais fazem mais cliques que outros
ggplot(clicks, aes(x=channel)) + ggtitle("Canal") + xlab("Código Canais") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + 
  coord_flip() + theme_minimal()

# O mesmo acontece para sistemas operacionais
ggplot(clicks, aes(x=os)) + ggtitle("Sistemas Operacionais") + xlab("Códigos SOs") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + 
  coord_flip() + theme_minimal()


## 7 -Seleção de Atributos/Feature Selection:
# O algoritmo não funciona devido à grande quantidade de classes nas variáveis preditoras,
# terei que começar novamente.

library(randomForest)

modeloFS <- randomForest(is_attributed ~ ., data = clicks, 
                        ntree = 100, nodesize = 10, importance = T)

varImpPlot(modeloFS)


## 8 -Divisão dos Dados em Treino e Teste:
linhas <- sample(1:nrow(clicks), 0.7 * nrow(clicks))
dados_treino <- clicks[linhas,]
dados_teste <- clicks[-linhas,]

# Distribuição dos clicks em dados de treino e de teste
prop.table(table(dados_treino$is_attributed))
prop.table(table(dados_teste$is_attributed))

# Graficamente a diferença
barplot(prop.table(table(dados_treino$is_attributed)))
barplot(prop.table(table(dados_teste$is_attributed)))

## 9 - Balanceamento de Classes:
# O balanceamento com Rose não funcionou pq tenho uma variável data e o algoritmo precisa que
# as variáveis seja numéricas ou categoricas
library(ROSE)

rose_treino <- ROSE(is_attributed ~ ., data = dados_treino, seed = 1)$data
prop.table(table(rose_treino$is_attributed))

rose_teste <- ROSE(is_attributed ~ ., data = dados_teste, seed = 1)$data
prop.table(table(rose_teste$is_attributed))

## 9 - Construção do Modelo e Treinamento do Modelo:
modeloRose <- C5.0(is_attributed ~ ., data = rose_treino)

## 10 - Teste do Modelo:
previsoes <- predict(modeloRose, rose_teste)

## 11 - Avaliação Performance do Modelo:
caret::confusionMatrix(rose_teste$is_attributed, previsoes, positive = '1')

# Calculamos o Score AUC
roc.curve(rose_teste$is_attributed, previsoes, plotit = T, col = "green", add.roc = T)

## 12 - Otimização do Modelo:


