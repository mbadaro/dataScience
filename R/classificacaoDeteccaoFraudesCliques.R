# Projeto 1 - Detecção de Fraudes no Tráfego de Cliques em Propagandas de Aplicações Mobile


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
# 1 - app, device, os, channel estão como inteiros mas são categóricas, porém, possuem um 
#     número muito grande de categorias por variável, segundo o Azure ML, app e channel tem
#     161 categorias, device e os, tem 100.
# 2 - is_attributed está como inteira, mas deve ser categória para a classificação.
# 2 - click_time e attributed_time estão como categóricas (fatores), mas são datas.
# 3 - A variável attributed_time está diretamente ligada à variável target, se ela existe, o
#     a variável target é 1, se não existe, é 0.


## 3 - Data Munging/Transformação/ETL:

require(lubridate)

# Verificando se há valores missing
any(is.na(clicks)) 

# Convertendo a variável target para fator
clicks$is_attributed <- as.factor(clicks$is_attributed)

# Convertendo as variáveis numéricas para números

# Convetendo a variável click_time de fator para data
clicks$click_time <- as.POSIXct(clicks$click_time)

# Criando categorias das variáveis numéricas
quantize.num <- function(x, nlevs = 20, maxval = 10000, 
                         minval = 0, ordered = TRUE){
  cuts <- seq(min(x), max(x), length.out = nlevs + 1)
  cuts[1] <- minval
  cuts[nlevs + 1] <- maxval
  print(cuts)
  x <- cut(x, breaks = cuts, order_result = ordered)
}

toFactors <- c("ip","app", "device", "os", "channel")
maxVals   <- c(400000, 600, 4000, 900, 500)
facNames  <- unlist(lapply(toFactors, function(x) paste(x, "_groups", sep = "")))
clicks[, facNames] <- Map(function(x, y) quantize.num(clicks[, x], maxval = y), 
                          toFactors, maxVals)

# A criação de variáveis categoricas gerou valores missing
any(is.na(clicks)) 

# Retriando valores missing
sum(is.na(clicks))
clicks <- na.omit(clicks)
any(is.na(clicks)) 

# Excluindo a varíavel attributed_time por estar diretamente associada à variável target
clicks$attributed_time <- NULL

str(clicks)
View (clicks)


## 4 - Normalização:

# Normalizando as variáveis numéricas
maxs <- apply(clicks[1:5], 2, max)
mins <- apply(clicks[1:5], 2, min)

# Imprimindo os valores
maxs
mins

dados_normalizados <- as.data.frame(scale(clicks[1:5], center = mins, scale = maxs - mins))
View(dados_normalizados)

clicks$ip <- dados_normalizados$ip
clicks$app <- dados_normalizados$app
clicks$device <- dados_normalizados$device
clicks$os <- dados_normalizados$os
clicks$channel <- dados_normalizados$channel

View(clicks)
str(clicks)

## 5 - Análise Exploratória dos Dados:
# Embora exista no DF algumas variáveis do tipo numéricas, elas são na verdade, categóricas 
# (e o ip é um ID), portanto, a mairo parte da análise exploratória será das variáveis 
# categóricas geradas a partir das numéricas
library(ggplot2)
library(gmodels)

# Distribuição dos dados pelas variáveis

# Pela distruibuição da variável target (abaixo), nitidamente se nota a necessidade da 
# realização do balanceamento de cargas, pois 99.83% do clicks não resultam em download
table(clicks$is_attributed) 
round(prop.table(table(clicks$is_attributed)) * 100, digits = 2)

table(clicks$ip_groups) 
round(prop.table(table(clicks$ip_groups)) * 100, digits = 2)

table(clicks$app_groups) 
round(prop.table(table(clicks$app_groups)) * 100, digits = 2)

table(clicks$device_groups) 
round(prop.table(table(clicks$device_groups)) * 100, digits = 2)

table(clicks$os_groups) 
round(prop.table(table(clicks$os_groups)) * 100, digits = 2)

table(clicks$channel_groups) 
round(prop.table(table(clicks$channel_groups)) * 100, digits = 2)

# Verificando o relacionamento entre variáveis categóricas e a target
CrossTable(x = clicks$app_groups, y = clicks$is_attributed)

CrossTable(x = clicks$device_groups, y = clicks$is_attributed)

CrossTable(x = clicks$os_groups, y = clicks$is_attributed)

CrossTable(x = clicks$channel_groups, y = clicks$is_attributed)

# Gráficos 
# Tentei gerar alguns gráficos, mas com o dataset ainda desbalanceado, não traz muitos
# insigths

# Barras
ggplot(clicks,aes(app_groups)) + geom_bar()

ggplot(clicks,aes(device_groups)) + geom_bar()

ggplot(clicks,aes(os_groups)) + geom_bar()

ggplot(clicks,aes(channel_groups)) + geom_bar()

ggplot(clicks,aes(channel)) + geom_bar(aes(fill = factor(is_attributed)), alpha = 0.5)

# Histogramas
ggplot(clicks,aes(ip)) + geom_histogram(fill = 'green', color = 'black', alpha = 0.5)

ggplot(clicks,aes(app)) + geom_histogram(fill = 'green', color = 'black', alpha = 0.5)

ggplot(clicks,aes(device)) + geom_histogram(fill = 'green', color = 'black', alpha = 0.5)

ggplot(clicks,aes(os)) + geom_histogram(fill = 'green', color = 'black', alpha = 0.5)

ggplot(clicks,aes(channel)) + geom_histogram(fill = 'green', color = 'black', alpha = 0.5)

# Percentuais
# Sistemas operacionais, devices e apps tem uma grande concentração em uma mesma faixa
ggplot(clicks, aes(x=ip_groups)) + ggtitle("IPs") + xlab("Grupos IPs") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + 
  coord_flip() + theme_minimal()

ggplot(clicks, aes(x=app_groups)) + ggtitle("Apps") + xlab("Grupos Apps") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + 
  coord_flip() + theme_minimal()

ggplot(clicks, aes(x=device_groups)) + ggtitle("Devices") + xlab("Grupos Devices") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + 
  coord_flip() + theme_minimal()

ggplot(clicks, aes(x=os_groups)) + ggtitle("Sistemas Operacionais") + xlab("Grupos SOs") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + 
  coord_flip() + theme_minimal()

ggplot(clicks, aes(x=channel_groups)) + ggtitle("Canal") + xlab("Grupos Canais") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + 
  coord_flip() + theme_minimal()

# Correlação
# Definindo as colunas para a análise de correlação 
colunas <- c("ip", "app", "device", "os", "channel")

# Métodos de Correlação
metodos <- c("pearson", "spearman")

# Aplicando os métodos de correlação
cors <- lapply(metodos, function(method) {
  cor(clicks[, colunas], method = method)}
)

cors

# Gráfico de Correlação
require(lattice)

plot.cors <- function(x, labs){
  diag(x) <- 0.0 
  plot( levelplot(x, 
                  main = paste("Plot de Correlação usando Método", labs),
                  scales = list(x = list(rot = 90), cex = 1.0)) )
}

# Mapa de Correlação
Map(plot.cors, cors, metodos)

# A única relação aparentemente um pouco mais forte, ou melhor é entre os e device


## 6 - Seleção de Atributos/Feature Selection:
# O algoritmo não funciona devido à grande quantidade de classes nas variáveis preditoras,
# terei que começar novamente.

library(randomForest)

set.seed(4050)

modeloFS <- randomForest(is_attributed ~ ., data = clicks, 
                        ntree = 100, nodesize = 10, importance = T)

varImpPlot(modeloFS)

# As variáveis importantes foram bem diferentes entre os dois modelos, na verdade, cada vez
# que rodo o algoritmo, ele muda a ordem das variáveis, mas:
# Accuracy indicou: channel_groups, app_groups, app, channel e device
# Gini indicou:     ip, channel, clikc_time, ip_groups e channel_groups
# Talvez, ip não seja uma boa variável preditora pois existem 34.857 números únicos em um 
# universo de 100.000 clicks, ou seja, é quase um id


## 7 - Divisão dos Dados em Treino e Teste:
linhas <- sample(1:nrow(clicks), 0.7 * nrow(clicks))
dados_treino <- clicks[linhas,]
dados_teste <- clicks[-linhas,]

# Distribuição dos clicks em dados de treino e de teste
prop.table(table(dados_treino$is_attributed))
prop.table(table(dados_teste$is_attributed))

# Graficamente a diferença
barplot(prop.table(table(dados_treino$is_attributed)))
barplot(prop.table(table(dados_teste$is_attributed)))


# 8 - Balanceamento de Classes:
library(ROSE)

# Vou retirar a variável click_time para poder fazer o balanceamento de classe com ROSE,até
# porque, de acordo com o Feature Selection, ela não é uma variável tão forte no algoritmo.
# Porém, vou tentar utilizar modelos com e sem balanceamento para ver se difere muito.

dados_treinoROSE <- dados_treino
dados_treinoROSE$click_time = NULL
str(dados_treinoROSE)

rose_treino <- ROSE(is_attributed ~ ., data = dados_treinoROSE, seed = 1)$data
any(is.na(rose_treino))
prop.table(table(rose_treino$is_attributed))

dados_testeROSE <- dados_teste
dados_testeROSE$click_time = NULL
str(dados_testeROSE)

rose_teste <- ROSE(is_attributed ~ ., data = dados_testeROSE, seed = 1)$data
any(is.na(rose_teste))
prop.table(table(rose_teste$is_attributed))


## 9 - Construção do Modelo e Treinamento do Modelo:
# Vou criar vários modelos, com algoritmos diferentes para ver como cada um se comporta e a
# precisão dos mesmos

library(caret)

# C5.0
# Eu tentei de todas as formas fazer esse algoritmo funcionar, revise as variáveis, fiz 
# alterações, mas não funcionou de jeito nenhum. O retorno sempre foi:
# c50 code called exit with value 1

# Random Forest 1 - Sem dados balanceados
set.seed(1555)
modeloRandomForest1 <- randomForest(is_attributed ~ ., data = dados_treino, 
                                   ntree = 100, nodesize = 10)

print(modeloRandomForest1)
summary(modeloRandomForest1)

# Random Forest 2 - Com dados balanceados
modeloRandomForest2 <- randomForest(is_attributed ~ ., data = rose_treino, 
                        ntree = 100, nodesize = 10)

print(modeloRandomForest2)
summary(modeloRandomForest2)

# KNN
# O KNN foi outro que não consegui fazer funcionar, mesmo seguindo os exemplos, com os dados
# normalizados, sem valores missing identificados, sempre recebi o retorno abaixo:
# Error in knn(train = rose_treino, test = rose_teste, cl = dadosTreinoLabels,  : 
#NA/NaN/Inf in foreign function call (arg 6)

#library(class)

#dadosTreinoLabels <- rose_treino[, 6]

#modeloKNN <- knn(train = rose_treino, test = rose_teste,
#                    cl = dadosTreinoLabels, 
#                     k = 11)

# Linear Model
modeloLinear <- glm(formula = is_attributed ~ ., data = rose_treino, family = "binomial")

summary(modeloLinear)

# SVM - Support Vector Machine
library(e1071)

modeloSVM <- svm(is_attributed ~ .,  data = rose_treino, type = 'C-classification', 
                     kernel = 'radial') 

print(modeloSVM)
summary(modeloSVM)

# NaiveBayes
modeloNaiveBayes<- naiveBayes(is_attributed ~ ., rose_treino)

print(modeloNaiveBayes)
summary(modeloNaiveBayes)


## 10 - Teste do Modelo:

# Random Forest 1 - Sem dados balanceados
previsoesRandomForest1 <- data.frame(observado = dados_teste$is_attributed,
                                     previsto = predict(modeloRandomForest1, 
                                                        newdata = dados_teste))

View(previsoesRandomForest1)

# Random Forest 2 - Com dados balanceados
previsoesRandomForest2 <- data.frame(observado = rose_teste$is_attributed,
                        previsto = predict(modeloRandomForest2, newdata = rose_teste))

View(previsoesRandomForest2)

# Linear Model
# As previsoes com Linear Modelo não funcionaram. O erro abaixo ocorreu:
#Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
#  factor device_groups has new levels (580,773], (773,967], (1.55e+03,1.74e+03]

#previsoesLinear <- predict(modeloLinear, rose_teste)
#previsoesLinear <- round(previsoesLinear)

# SVM - Support Vector Machine
previsoesSVM <- predict(modeloSVM, rose_teste)

# NaiveBayes
previsoesNaiveBayes <- predict(modeloNaiveBayes, rose_teste)
previsoesNaiveBayes


## 11 - Avaliação Performance do Modelo:

# Random Forest 1 - Sem dados balanceados
confusionMatrix(previsoesRandomForest1$observado, previsoesRandomForest1$previsto)

# Accuracy : 0.9987
#Prediction     0     1
#         0 29778     5
#         1    34     9

# Random Forest 2 - Com dados balanceados
confMatrixRandomForest <- confusionMatrix(previsoesRandomForest2$observado, 
                                          previsoesRandomForest2$previsto)
confMatrixRandomForest

# Accuracy : 0.8679
#Prediction     0     1
#         0 14478   555
#         1  3386 11407

# Obs1: Embora o modelo sem os dados balanceado tenha acertado muito mais, provavelmente isso
# ocorreu pois conhece muito sobre o target 0 e quase nada sobre o target 1. Sendo assim, o
# modelo com os dados balanceados tende a ser mais acertado e também teve um nível muito bom 
# de acerto.

# SVM - Support Vector Machine
mean(previsoesSVM == rose_teste$is_attributed)
# Accuracy : 0.8930799

confMatrixSVM <- table(previsoesSVM, rose_teste$is_attributed)
confMatrixSVM
#previsoesSVM     0     1
#           0 14128  2284
#           1   905 12509

# NaiveBayes
mean(previsoesNaiveBayes == rose_teste$is_attributed)
# Accuracy : 0.8564675

confMatrixNayveBayes <- table(previsoesNaiveBayes, rose_teste$is_attributed)
confMatrixNayveBayes
#previsoesNaiveBayes     0     1
#                  0 12160  1408
#                  1  2873 13385

# Obs: Ao final da avaliação dos 4 modelos gerados, irei excluir o RF sem dados balanceados,
# pois como expliquei acima, entendo que ele sabe muito sobre target 0 e quase nada sobre 
# target1. Dos demais, segue o ranking pela Acurácia:

# SVM:            0.8930799
# Random Forest:  0.8679
# NaiveBayes:     0.8564675

# Avaliando outros indicadores (funções do pacote caret):

#Precision
precision(confMatrixRandomForest$table)   # 0.9630812
precision(confMatrixNayveBayes)           # 0.8962264
precision(confMatrixSVM)                  # 0.8608335

#Recall
recall(confMatrixSVM)                     # 0.9397991
recall(confMatrixRandomForest$table)      # 0.8104568
recall(confMatrixNayveBayes)              # 0.8088871

#F-Score
F_meas(confMatrixSVM)                     # 0.8985848
F_meas(confMatrixRandomForest$table)      # 0.8802018
F_meas(confMatrixNayveBayes)              # 0.8503199

# Obs: Modelo Random Forest se saiu melhor no precision e o SVM no Recall, no F-Score que é a 
# média ponderada entre ambos, o SVM ficou ligeriamente à frente.
# Sendo assim e como já havia ficado à frente na acurácia, foi seguir com o modelo SVM, 
# embora, os modelos Random Forest e NayveBayes tenham ficado com números bons também.

#AUC - Area Under the Curve
library("ROCR")

#valoresReais <- as.data.frame(rose_teste$is_attributed)

#sapply(c(is.vector, is.matrix, is.list, is.data.frame), do.call, list(valoresReais))
#FALSE FALSE  TRUE  TRUE

# Mesmo os objetos sendo das classes lista e data frame, está ocorrendo o erro abaixo:
# Currently, only continuous predictions are supported by ROCR. 
# Portanto, não consegui avançar com a curva ROCR

#predSVM          <- prediction(as.data.frame(previsoesSVM), valoresReais)
#predRandomForest <- prediction(as.data.frame(previsoesRandomForest2$previsto), valoresReais) 
#predNaiveBayes   <- prediction(as.data.frame(previsoesNaiveBayes), valoresReais)

#perfSVM          <- performance(predSVM, "tpr","fpr") 
#perfRandomForest <- performance(predRandomForest, "tpr","fpr") 
#perfNaiveBayes   <- performance(predNaiveBayes, "tpr","fpr") 

# plot(perfSVM,...)


## 12 - Otimização do Modelo:
# Vou seguir com o modelo SVM. Como ele já tem um bom percentual de acertos (em torno de 89%),
# irei alterar algumas variáveis preditoras para ver se aumenta a acurácia, ou não.

modeloSVMOtimizado1 <- svm(is_attributed ~ . - ip,  
                          data = rose_treino, 
                          type = 'C-classification', 
                          kernel = 'radial') 

print(modeloSVMOtimizado1)
summary(modeloSVMOtimizado1)

previsoesSVMOtimizado1 <- predict(modeloSVMOtimizado1, rose_teste)

mean(previsoesSVMOtimizado1 == rose_teste$is_attributed)
# Accuracy : 0.8906323

confMatrixSVMOtimizado1 <- table(previsoesSVMOtimizado1, rose_teste$is_attributed)
confMatrixSVMOtimizado1

precision(confMatrixSVMOtimizado1)    # 0.8601236
recall(confMatrixSVMOtimizado1)       # 0.9350762
F_meas(confMatrixSVMOtimizado1)       # 0.8960352

# O modelo sem a variável ip mudou muito pouco os indicadores de acurácia, precisão, recall
# e f_meas

# Tentando um novo modelo agora somente com as variáveis preditoras mais fortes indicadas
# no método Accuracy no Feature Selection com o algortimo Random Forest

modeloSVMOtimizado2 <- svm(is_attributed ~ channel_groups + app_groups + app +
                                           channel + device,  
                           data = rose_treino, 
                           type = 'C-classification', 
                           kernel = 'radial') 

print(modeloSVMOtimizado2)
summary(modeloSVMOtimizado2)

previsoesSVMOtimizado2 <- predict(modeloSVMOtimizado2, rose_teste)

mean(previsoesSVMOtimizado2 == rose_teste$is_attributed)
# Accuracy : 0.8609267

confMatrixSVMOtimizado2 <- table(previsoesSVMOtimizado2, rose_teste$is_attributed)
confMatrixSVMOtimizado2

precision(confMatrixSVMOtimizado2)    # 0.8668194
recall(confMatrixSVMOtimizado2)       # 0.8555179
F_meas(confMatrixSVMOtimizado2)       # 0.8611316

# Trabalhando somente com as variáveis selecionadas no Feature Selection, houve uma diminuição
# na acurácia, um leve aumento na precision, uma baixa no recall, e uma queda no F1 Score

# Executando o modelo com uma mudança no parâmetro do kernel, de radial para sigmoid.
modeloSVMOtimizado3 <- svm(is_attributed ~ .,  
                           data = rose_treino, 
                           type = 'C-classification', 
                           kernel = 'sigmoid') 

print(modeloSVMOtimizado3)
summary(modeloSVMOtimizado3)

previsoesSVMOtimizado3 <- predict(modeloSVMOtimizado3, rose_teste)

mean(previsoesSVMOtimizado3 == rose_teste$is_attributed)
# Accuracy : 0.8256219

confMatrixSVMOtimizado3 <- table(previsoesSVMOtimizado3, rose_teste$is_attributed)
confMatrixSVMOtimizado2

precision(confMatrixSVMOtimizado3)    # 0.8188067
recall(confMatrixSVMOtimizado3)       # 0.8398856
F_meas(confMatrixSVMOtimizado3)       # 0.8292122

# Os números apresentados na terceira tentativa de mudança do modelo também apresentou nrs
# abaixo do modelo original.

# Sendo assim, o modelo mais acertado ficou sendo o primeiro, com todas as variáveis e
# kernel = radial