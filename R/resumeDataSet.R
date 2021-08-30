# Os arquivos originais são muito grandes, portanto, para trabalhar melhor de maneira local,
# vou fazer retirar uma amostra deles e gravar em outro arquivo, mas mantendo os formatos 
# originais

# Configurando o diretório de trabalho
setwd("C:/Temp/")
getwd()

# Lendo o arquivo de treino
treino <- read.csv("train.csv", header = TRUE, sep = ",")
dim(treino) # quase 75 milhões de linhas (74.180.464 ) e 11 colunas
str(treino)

# Com o código abaixo o carregamento é muito mais rápido, embora o tamanho na memória seja o
# mesmo
#library(data.table)
#treino <- fread("train.csv")

#$ Semana           : int  3 3 3 3 3 3 3 3 3 3 ...
#$ Agencia_ID       : int  1110 1110 1110 1110 1110 1110 1110 1110 1110 1110 ...
#$ Canal_ID         : int  7 7 7 7 7 7 7 7 7 7 ...
#$ Ruta_SAK         : int  3301 3301 3301 3301 3301 3301 3301 3301 3301 3301 ...
#$ Cliente_ID       : int  15766 15766 15766 15766 15766 15766 15766 15766 15766 15766 ...
#$ Producto_ID      : int  1212 1216 1238 1240 1242 1250 1309 3894 4085 5310 ...
#$ Venta_uni_hoy    : int  3 4 4 4 3 5 3 6 4 6 ...
#$ Venta_hoy        : num  25.1 33.5 39.3 33.5 22.9 ...
#$ Dev_uni_proxima  : int  0 0 0 0 0 0 0 0 0 0 ...
#$ Dev_proxima      : num  0 0 0 0 0 0 0 0 0 0 ...
#$ Demanda_uni_equil: int  3 4 4 4 3 5 3 6 4 6 ...

# Lendo o arquivo de teste
teste <- read.csv("test.csv", header = TRUE, sep = ",")
dim(teste) # quase 7 milhões de linhas (6.999.251) e 7 colunas
str(teste)

#$ id         : int  0 1 2 3 4 5 6 7 8 9 ...
#$ Semana     : int  11 11 10 11 11 11 11 10 10 11 ...
#$ Agencia_ID : int  4037 2237 2045 1227 1219 1146 2057 1612 1349 1461 ...
#$ Canal_ID   : int  1 1 1 1 1 4 1 1 1 1 ...
#$ Ruta_SAK   : int  2209 1226 2831 4448 1130 6601 4507 2837 1223 1203 ...
#$ Cliente_ID : int  4639078 4705135 4549769 4717855 966351 1741414 4659766 4414012 1646915 ...
#$ Producto_ID: int  35305 1238 32940 43066 1277 972 1232 35305 1240 43203 ...

# Retirando 0.25% do arquivo de treino
dsTreino <- sample(1:nrow(treino), 0.0025 * nrow(treino))
dadosTreino <- treino[dsTreino,]

dim(dadosTreino) # 185.451 linhas e 11 colunas

# Gravando o novo arquivo de treino
write.csv(dadosTreino, "dadosTreino.csv")

# Retirando 1% do arquivo de teste
dsTeste <- sample(1:nrow(teste), 0.0025 * nrow(teste))
dadosTeste <- teste[dsTeste,]

dim(dadosTeste) # 69.992 linhas e 11 colunas

# Gravando o novo arquivo de teste
write.csv(dadosTeste, "dadosTeste.csv")