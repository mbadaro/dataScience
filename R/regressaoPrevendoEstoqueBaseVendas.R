# Projeto 2 - Prevendo Demanda de Estoque com Base em Vendas


## 1 - Definição e Compreensão do Problema de Negócio:

# Desenvolver um modelo para prever com precisão a demanda de estoque com base nos dados
# históricos de vendas.

# Neste projeto de aprendizado de máquina, você deve desenvolver um modelo para prever com 
# precisão a demanda de estoque com base nos dados históricos de vendas. Isso fará com que os
# consumidores dos mais de 100 produtos de panificação não fiquem olhando para as prateleiras 
# vazias, além de reduzir o valor gasto com reembolsos para os proprietários de lojas com 
# produtos excedentes impróprios para venda.

# Obs: No Kaggle (e também através da verificação nos datasets) é explicitado que as variáveis
# existentes nos datasets de treino e teste são diferentes (existem variáveis no dataset de
# treino que não estão no de teste). Além do fato de no dataset de teste haver produtos que 
# não existem no dataset de treino.
# Para esse cenário, seria preciso uma abordagem mais complexa, uma vez que não há como fazer
# previsões sem dados históricos, além de não estar disponível no dataset de testes muitos dos
# dados históricos disponívies para treino, tampouco a variável target está disponível, não 
# sendo possível, dessa maneira, avaliar a performance do modelo.
# Assim sendo, o que irei fazer nesse projeto será trabalhar com o dataset de treino e 
# dividí-lo como padrão em treino e teste para trabalhar o algoritmo.
# Eu uma outra versão seria possível tentar realizar o que está apresentado no Kaggle.

# Configurando o diretório de trabalho
setwd("C:/dev/DSAFCD/RAzure/ProjetosFinais/projeto2Regressao/")
getwd()



## 2 - Coleta/Obtenção dos Dados:

# Para a construção desse projeto, utilizar o dataset disponível no Kaggle em:
# https://www.kaggle.com/c/grupo-bimbo-inventory-demand

#Things to note:
# . There are duplicate Cliente_ID's in cliente_tabla, which means one Cliente_ID may have
# multiple NombreCliente that are very similar. This is due to the NombreCliente being noisy 
# and not standardized in the raw data, so it is up to you to decide how to clean up and use 
# this information. 
# . The adjusted demand (Demanda_uni_equil) is always >= 0 since demand should be either 0 or 
# a positive value. The reason that Venta_uni_hoy - Dev_uni_proxima sometimes has negative 
# values is that the returns records sometimes carry over a few weeks.

# Data fields: 
# Semana — Week number (From Thursday to Wednesday)
# Agencia_ID — Sales Depot ID
# Canal_ID — Sales Channel ID
# Ruta_SAK — Route ID (Several routes = Sales Depot)
# Cliente_ID — Client ID
# NombreCliente — Client name
# Producto_ID — Product ID
# NombreProducto — Product Name
# Venta_uni_hoy — Sales unit this week (integer)
# Venta_hoy — Sales this week (unit: pesos)
# Dev_uni_proxima — Returns unit next week (integer)
# Dev_proxima — Returns next week (unit: pesos)
# Demanda_uni_equil — Adjusted Demand (integer) (This is the target you will predict)

# Obs: Os arquivos originais eram muito grandes, da ordem de 3GB e quase 75 milhões registros.
# Para poder trabalhar melhor localmente, retirei amostras do arquivo de treino, mantendo a
# estrutura original do mesmo. Os arquivos de clientes, produtos e regioes são os originais.
# Os scripts dessa redução estão no script "resumeDataSet.R" nessa mesma pasta.

vendas <- read.csv("dadosTreino.csv", header = TRUE, sep = ",")
str(vendas)

clientes <- read.csv("cliente_tabla.csv", header = TRUE, sep = ",")
str(clientes)

produtos <- read.csv("producto_tabla.csv", header = TRUE, sep = ",")
str(produtos)

regioes <- read.csv("town_state.csv", header = TRUE, sep = ",")
str(regioes)



## 3 - Data Munging/Transformação/ETL:

library(dplyr)
library(tidyr)
library(stringr)

# Tirando a coluna X das vendas, que identificam o nr da linha original da amostra
vendas$X <- NULL

# Retirando a duplicidade de id dos clientes
clientesSemDuplicados <- distinct(clientes, Cliente_ID, .keep_all = TRUE)

# Separando o código da cidade do Nome e fazendo alguns ajustes nos nomes
regioes <- regioes %>%
  separate(Town, into = c("Town_ID", "Town"), sep = 5)

regioes$State = str_replace_all(regioes$State, "MÃ‰XICO", "MEXICO")
regioes$State = str_replace_all(regioes$State, "NUEVO LEÃ“N", "NUEVO LEON")
regioes$State = str_replace_all(regioes$State, "Ã", "A") 
regioes$State = str_replace_all(regioes$State, ", D.F.", " DF") 

# Unindo os nomes aos Ids (foi mais simples fazer assim do que com "%>%")
vendas1 <- left_join(vendas, clientesSemDuplicados)
vendas2 <- left_join(vendas1, produtos)
vendas3 <- left_join(vendas2, regioes)

# Colocando os nomes aos lados dos ids
vendasCompletas <- vendas3[,c(1,2,14:16,3:5,12,6,13,7:11)]
View(vendasCompletas)

# Verificando se há valores missing até o momento
any(is.na(vendasCompletas))   # FALSE

# Fazendo ajustes nas variáveis strings do dataset
# Colocando em maiúsculas
vendasCompletas$Town = toupper(vendasCompletas$Town)
vendasCompletas$State = toupper(vendasCompletas$State)
vendasCompletas$NombreCliente = toupper(vendasCompletas$NombreCliente)
vendasCompletas$NombreProducto = toupper(vendasCompletas$NombreProducto)

# Substituindo espaço, pontos e traços por underline para ter uma padronização
vendasCompletas$Town = str_replace_all(vendasCompletas$Town, "\\s", "_")
vendasCompletas$Town = str_replace_all(vendasCompletas$Town, "\\.", "_")
vendasCompletas$Town = str_replace_all(vendasCompletas$Town, "-", "_")

vendasCompletas$State = str_replace_all(vendasCompletas$State, "\\s", "_")
vendasCompletas$State = str_replace_all(vendasCompletas$State, "\\.", "_")
vendasCompletas$State = str_replace_all(vendasCompletas$State, "-", "_")

vendasCompletas$NombreCliente = str_replace_all(vendasCompletas$NombreCliente, "\\s", "_")
vendasCompletas$NombreCliente = str_replace_all(vendasCompletas$NombreCliente, "\\.", "_")
vendasCompletas$NombreCliente = str_replace_all(vendasCompletas$NombreCliente, "-", "_")

vendasCompletas$NombreProducto = str_replace_all(vendasCompletas$NombreProducto, "\\s", "_")
vendasCompletas$NombreProducto = str_replace_all(vendasCompletas$NombreProducto, "\\.", "_")
vendasCompletas$NombreProducto = str_replace_all(vendasCompletas$NombreProducto, "-", "_")

# Convertendo os tipos de dados
str(vendasCompletas)

vendasCompletas$Town_ID <- as.integer(vendasCompletas$Town_ID)
vendasCompletas$Semana      <- as.factor(vendasCompletas$Semana)
vendasCompletas$State       <- as.factor(vendasCompletas$State)
vendasCompletas$Canal_ID    <- as.factor(vendasCompletas$Canal_ID)

str(vendasCompletas)

# Os demais campos numéricos, são identificadores que deveria ser transformados em fatores,
# porém, haveria muitos níveis para cada fator. 
# Sendo assim, vou manter como numéricos e criar variáveis com faixa do tipo fator para
# representar esses campos.

quantize.num <- function(x, nlevs = 20, maxval = 10000, 
                         minval = 0, ordered = TRUE){
  cuts <- seq(min(x), max(x), length.out = nlevs + 1)
  cuts[1] <- minval
  cuts[nlevs + 1] <- maxval
  print(cuts)
  x <- cut(x, breaks = cuts, order_result = ordered)
}

toFactors <- c("Agencia_ID","Town_ID", "Ruta_SAK", "Cliente_ID", "Producto_ID")
maxVals   <- c(26000, 4000, 10000, 12000000, 50000)
facNames  <- unlist(lapply(toFactors, function(x) paste(x, "_groups", sep = "")))
vendasCompletas[, facNames] <- Map(function(x, y) quantize.num(vendasCompletas[, x], maxval = y), 
                               toFactors, maxVals)

any(is.na(vendasCompletas)) # Não gerou valores missing

# Verificando valores negativos e outliers
summary(vendasCompletas)

# Embora no enunciado diga que algumas colunas podem conter valores negativos, nenhuma foi
# encontrada com essa característica. 
# De qualquer forma, vou criar uma função e aplicar às variáveis numéricas para evitar esse 
# problema caso algum negativo apareça.

trataNegativos <- function(x) {
  if (x < 0) {
    return (0)
  } else {
    return (x)
  }
}

vendasCompletas$Venta_uni_hoy     <- sapply(vendasCompletas$Venta_uni_hoy, trataNegativos)
vendasCompletas$Venta_hoy         <- sapply(vendasCompletas$Venta_hoy, trataNegativos)
vendasCompletas$Dev_uni_proxima   <- sapply(vendasCompletas$Dev_uni_proxima, trataNegativos)
vendasCompletas$Dev_proxima       <- sapply(vendasCompletas$Dev_proxima, trataNegativos)
vendasCompletas$Demanda_uni_equil <- sapply(vendasCompletas$Demanda_uni_equil, trataNegativos)

# Por último, observando o summary e algumas análise feitas no Azure ML, verifiquei que há 
# alguns outliers, sendo assim, farei a retirada dos mesmos.
dim(vendasCompletas)        # 185.451 linhas
any(is.na(vendasCompletas)) # Sem valores missing

# Para as variáveis abaixo os valores de percentil parece identificar bem os outliers
percentilUndDia   <- quantile(vendasCompletas$Venta_uni_hoy, c(.95))     # 24
percentilPesoDia  <- quantile(vendasCompletas$Venta_hoy, c(.90))         # 112
percentilDevProx <- quantile(vendasCompletas$Dev_proxima, c(.97))        # 8.15

vendasCompletas$Venta_uni_hoy <- ifelse(vendasCompletas$Venta_uni_hoy > percentilUndDia, 
                                        NA, 
                                        vendasCompletas$Venta_uni_hoy)

vendasCompletas$Venta_hoy <- ifelse(vendasCompletas$Venta_hoy > percentilPesoDia, 
                                    NA, 
                                    vendasCompletas$Venta_hoy)

vendasCompletas$Dev_proxima <- ifelse(vendasCompletas$Dev_proxima > percentilDevProx, 
                                      NA, 
                                      vendasCompletas$Dev_proxima)

# Já para a variável Dev_uni_proxima, mesmo um percentil de .9999 acaba por eliminar todos os 
# valores acima de 0, então, vou fazer uma eliminação mais direcionada

# Quantidade de registros com devoluções acima de 50 (121 registros)
vendasCompletas %>% 
  filter(Dev_uni_proxima > 50) %>% 
  summarise(n())

vendasCompletas$Dev_uni_proxima <- ifelse(vendasCompletas$Dev_uni_proxima > 50, 
                                          NA, 
                                          vendasCompletas$Dev_uni_proxima)

# Verificando e retirando os valores missing criados para eliminar os outliers
any(is.na(vendasCompletas))  # TRUE

sapply(vendasCompletas, function(x) sum(is.na(x))) 

vendasCompletas <- na.omit(vendasCompletas)

dim(vendasCompletas) # O dataset final ficou com 160.520 linhas



## 4 - Normalização:

# Normalizando as variáveis numéricas
maxs <- apply(vendasCompletas[c(2,3,7,8,10,12:16)], 2, max)
mins <- apply(vendasCompletas[c(2,3,7,8,10,12:16)], 2, min)

# Imprimindo os valores
maxs
mins

dados_normalizados <- as.data.frame(scale(vendasCompletas[c(2,3,7,8,10,12:16)], 
                                          center = mins, 
                                          scale = maxs - mins))

vendasCompletasNormalizada <- vendasCompletas

vendasCompletasNormalizada$Agencia_ID        <- dados_normalizados$Agencia_ID
vendasCompletasNormalizada$Town_ID           <- dados_normalizados$Town_ID
vendasCompletasNormalizada$Ruta_SAK          <- dados_normalizados$Ruta_SAK
vendasCompletasNormalizada$Cliente_ID        <- dados_normalizados$Cliente_ID
vendasCompletasNormalizada$Producto_ID       <- dados_normalizados$Producto_ID
vendasCompletasNormalizada$Venta_uni_hoy     <- dados_normalizados$Venta_uni_hoy
vendasCompletasNormalizada$Venta_hoy         <- dados_normalizados$Venta_hoy
vendasCompletasNormalizada$Dev_uni_proxima   <- dados_normalizados$Dev_uni_proxima
vendasCompletasNormalizada$Dev_proxima       <- dados_normalizados$Dev_proxima
vendasCompletasNormalizada$Demanda_uni_equil <- dados_normalizados$Demanda_uni_equil

View(vendasCompletas)
View(vendasCompletasNormalizada)
str(vendasCompletasNormalizada)



## 5 - Análise Exploratória dos Dados:

library(sqldf)

# Tabela de contingência das variáveis fatores
table(vendasCompletas$Semana)               #  7 levels
table(vendasCompletas$State)                # 33 levels
table(vendasCompletas$Canal_ID)             #  9 levels        
table(vendasCompletas$Agencia_ID_groups)    # 20 levels
table(vendasCompletas$Town_ID_groups)       # 20 levels
table(vendasCompletas$Ruta_SAK_groups)      # 20 levels
table(vendasCompletas$Cliente_ID_groups)    # 20 levels
table(vendasCompletas$Producto_ID_groups)   # 20 levels


# Medidas de Tendência Central
colunasNumericas <- sapply(vendasCompletas, is.numeric)
summary(vendasCompletas[c(colunasNumericas)])

#Venta_uni_hoy    Venta_hoy        Dev_uni_proxima   Dev_proxima       Demanda_uni_equil
#Min.   : 0.000   Min.   :  0.00   Min.   :0.00000   Min.   :0.00000   Min.   : 0.000   
#1st Qu.: 2.000   1st Qu.: 16.30   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.: 2.000   
#Median : 3.000   Median : 26.64   Median :0.00000   Median :0.00000   Median : 3.000   
#Mean   : 4.144   Mean   : 33.58   Mean   :0.00542   Mean   :0.03485   Mean   : 4.139   
#3rd Qu.: 5.000   3rd Qu.: 45.00   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.: 5.000   
#Max.   :24.000   Max.   :112.00   Max.   :10.0000   Max.   :8.15000   Max.   :24.000 

### Medidas de Dispersão
var(vendasCompletas$Venta_uni_hoy)              # 13.23601
var(vendasCompletas$Venta_hoy)                  # 562.3734
var(vendasCompletas$Dev_uni_proxima)            # 0.006275174
var(vendasCompletas$Dev_proxima)                # 0.24274
var(vendasCompletas$Demanda_uni_equil)          # 13.24642

sd(vendasCompletas$Venta_uni_hoy)               # 3.638132
sd(vendasCompletas$Venta_hoy)                   # 23.71441
sd(vendasCompletas$Dev_uni_proxima)             # 0.079216
sd(vendasCompletas$Dev_proxima)                 # 0.4926865
sd(vendasCompletas$Demanda_uni_equil)           # 3.639563

cvVenta_uni_hoy <- (sd(vendasCompletas$Venta_uni_hoy)/mean(vendasCompletas$Venta_uni_hoy)) * 100
cvVenta_hoy <- (sd(vendasCompletas$Venta_hoy)/mean(vendasCompletas$Venta_hoy)) * 100
cvDev_uni_proxima <- (sd(vendasCompletas$Dev_uni_proxima)/mean(vendasCompletas$Dev_uni_proxima)) * 100
cvDev_proxima <- (sd(vendasCompletas$Dev_proxima)/mean(vendasCompletas$Dev_proxima)) * 100
cvDemanda_uni_equil <- (sd(vendasCompletas$Demanda_uni_equil)/mean(vendasCompletas$Demanda_uni_equil)) * 100

cvVenta_uni_hoy          # 87.80054
cvVenta_hoy              # 70.62118
cvDev_uni_proxima        # 1461.581
cvDev_proxima            # 1413.869
cvDemanda_uni_equil      # 87.93104

# Boxplots
boxplot(vendasCompletas$Venta_uni_hoy, main = "Boxplot 1", ylab = "Vendas Diárias em Unidade")
boxplot(vendasCompletas$Venta_hoy, main = "Boxplot 1", ylab = "Vendas Diárias por Peso")
boxplot(vendasCompletas$Dev_uni_proxima, main = "Boxplot 1", ylab = "Devoluções Diárias Und")
boxplot(vendasCompletas$Dev_proxima, main = "Boxplot 1", ylab = "Devoluções Diárias por Peso")

# Boxplots x Variável Preditora
library(ggplot2)

plot.boxes  <- function(X, label){ 
  ggplot(vendasCompletas, aes_string(x = X, y = "Demanda_uni_equil", group = X)) + 
    geom_boxplot( ) + 
    ggtitle(label) +
    theme(text = element_text(size = 18)) 
}

labels <- c("Demanda vs Unidades",
            "Demanda vs Peso",
            "Demanda vs Devolucoes",
            "Demanda vs Devolucoes em Peso")

xAxis <- c("Venta_uni_hoy", "Venta_hoy", "Dev_uni_proxima", "Dev_proxima")

Map(plot.boxes, xAxis, labels)

# Obs: Embora esses gráficos não tenham ajudando tanto, mas já dá para perceber um forte 
# correlação entre vendas e a variável preditora

# Histogramas
ggplot(vendasCompletas, aes (x = Venta_uni_hoy)) +
  geom_histogram(bins = 20, alpha = 0.5, fill = 'blue') + theme_minimal()

ggplot(vendasCompletas, aes (x = Venta_hoy)) +
  geom_histogram(bins = 20, alpha = 0.5, fill = 'blue') + theme_minimal()

ggplot(vendasCompletas, aes (x = Dev_uni_proxima)) +
  geom_histogram(bins = 20, alpha = 0.5, fill = 'blue') + theme_minimal()

ggplot(vendasCompletas, aes (x = Dev_proxima)) +
  geom_histogram(bins = 20, alpha = 0.5, fill = 'blue') + theme_minimal()

# Scatterplot
plot(x = vendasCompletas$Venta_uni_hoy, y = vendasCompletas$Demanda_uni_equil,
     main = "Scatterplot - Vendas x Demanda",
     xlab = "Vendas diárias",
     ylab = "Demanda")

# Obs: Aqui fica muito clara a correlação entre as vendas e a demanda

# Gráficos de Barras
# Estados com mais vendas
sqldf("SELECT State, SUM(Venta_uni_hoy) as totalVendas 
       FROM vendasCompletas
       GROUP BY State
       ORDER BY totalVendas DESC")

# Estados com mais vendas:
#1       ESTADO_DE_MEXICO       96033
#2              MEXICO_DF       73183
#3                JALISCO       58591
#4                 PUEBLA       37799
#5             GUANAJUATO       36064
#6               VERACRUZ       35808
#7             NUEVO_LEON       34268
#8               MICHOACA       30077
#9                HIDALGO       19357
#10            TAMAULIPAS       18595

vendasCompletas %>% 
  ggplot(aes(x = State, y = Venta_uni_hoy)) + 
  geom_bar(stat = "identity") +
  ylab("Quantidade Vendida") + 
  xlab("Estado") + 
  ggtitle("Quantidade de Vendas por Estado")

# Cidades com mais vendas:
sqldf("SELECT Town, SUM(Venta_uni_hoy) as totalVendas 
       FROM vendasCompletas
      GROUP BY Town
      ORDER BY totalVendas DESC")

#1                  AG__SANTA_CLARA        7932
#2                            NORTE        7749
#3                  AG__SAN_ANTONIO        7389
#4                     AG__ATIZAPAN        7272
#5              PUEBLA_SUR_MARINELA        6803
#6                     AG__XALOSTOC        6802
#7               AG__MEGA_NAUCALPAN        6552
#8                        CHALCO_BM        6408
#9                     AG__LA_VILLA        6282
#10                AG__IXTAPALUCA_1        6275
#11                  AGENCIA_SUANDY        6133
#12                        PINOTEPA        6079
#13        AGUASCALIENTES_SIGLO_XXI        5905
#14                   ZAPOPAN_BIMBO        5847
#15                         LINCOLN        5832
#16                   ZAMORA_MADERO        5741
#17                      AG__CEYLAN        5703
#18            AGUASCALIENTES_NORTE        5682
#19                         PALOMAR        5587
#20                      AEROPUERTO        5547

vendasCompletas %>% 
  ggplot(aes(x = Town_ID, y = Venta_uni_hoy)) + 
  geom_bar(stat = "identity") +
  ylab("Quantidade Vendida") + 
  xlab("Cidade") + 
  ggtitle("Quantidade de Vendas por Cidade")

# Produtos mais vendidos:
sqldf("SELECT NombreProducto, SUM(Venta_uni_hoy) as totalVendas 
       FROM vendasCompletas
      GROUP BY NombreProducto
      ORDER BY totalVendas DESC")

#1                         REBANADA_2P_55G_BIM_1284       35089
#2                             NITO_1P_62G_BIM_1278       28996
#3                     GANSITO_1P_50G_MTB_MLA_43285       24162
#4           DONITAS_ESPOLVOREADAS_6P_105G_BIM_1242       20935
#5             MANTECADAS_VAINILLA_4P_125G_BIM_1240       20204
#6                    DONAS_AZUCAR_4P_105G_BIM_1250       18778
#7                     NITO_1P_62G_CENTRAL_BIM_2425       18288
#8                       MADALENAS_3P_93G_BIM_35651       14446
#9             PRINCIPE_10P_106G_PROM_MTB_MLA_30532       12669
#10                  PINGUINOS_2P_80G_MTA_MLA_43069       12012
#11                     BIMBUNUELOS_4P_66G_BIM_1309       11161
#12                CHOCO_ROLES_2P_80G_MTA_MLA_37058       11025
#13           BARRITAS_FRESA_67G_PROM_MTB_MLA_30572       10643
#14                    TORTILLINAS_10P_255G_TR_1125        9805
#15                  MANTECADAS_NUEZ_123G_BIM_41938        9377
#16           BARRITAS_FRESA_75G_PROM_MTB_MLA_35305        8270
#17                        PAN_BLANCO_640G_BIM_2233        8138
#18           GANSITO_1P_50G_CCHAROLA_MTA_MLA_43307        7879
#19                   BRAN_FRUT_FRESA_48G_BIM_48417        7684
#20           PANQUECITO_GOTA_CHOC_2P_140G_BIM_1238        7618

vendasCompletas %>% 
  ggplot(aes(x = Producto_ID, y = Venta_uni_hoy)) + 
  geom_bar(stat = "identity") +
  ylab("Quantidade Vendida") + 
  xlab("Produto") + 
  ggtitle("Quantidade de Vendas por Produto")

# Produtos mais devolvidos:
sqldf("SELECT NombreProducto, SUM(Dev_uni_proxima) as totalDevolucoes 
       FROM vendasCompletas
      GROUP BY NombreProducto
      ORDER BY totalDevolucoes DESC")

#1                       MADALENAS_3P_93G_BIM_35651              65
#2                    DONAS_AZUCAR_4P_105G_BIM_1250              53
#3           DONITAS_ESPOLVOREADAS_6P_105G_BIM_1242              51
#4                       COLCHONES_6P_130G_BIM_1220              47
#5                         REBANADA_2P_55G_BIM_1284              34
#6                   PINGUINOS_2P_80G_MTA_MLA_43069              32
#7     SUBMARINOS_VAINILLA_3P_105G_SP_MTA_MLA_43316              30
#8        SUBMARINOS_FRESA_3P_105G_SP_MTA_MLA_43064              28
#9                      MANTECADAS_2P_105G_TR_31719              26
#10                     BIMBUNUELOS_4P_66G_BIM_1309              24
#11                ROLLO_FRESA_2P_75G_MTA_MLA_43065              22
#12   SUBMARINOS_CHOCOLATE_3P_105G_SP_MTA_MLA_43084              18
#13                 NAPOLITANO_1P_70G_MTB_MLA_43274              18
#14                CHOCO_ROLES_2P_80G_MTA_MLA_37058              16
#15                            NITO_1P_62G_BIM_1278              15
#16                  BIGOTES_CHOCOLATE_60G_TR_31717              15
#17        MINI_BIGOTES_CHOCOLATE_5P_65G_SP_TR_2505              13
#18           REBANADA_SANDWICH_MP_2P_55G_BIM_34868              12
#19           BOLSA_MINI_ROCKO_40P_13G_CU_MLA_36610              12
#20         BIMBUNUELOS_CON_CANELA_2P_33G_BIM_30946              12

vendasCompletas %>% 
  ggplot(aes(x = Producto_ID, y = Dev_uni_proxima)) + 
  geom_bar(stat = "identity") +
  ylab("Quantidade Devolvida") + 
  xlab("Produto") + 
  ggtitle("Quantidade de Devolucoes por Produto")

# Correlações
library(corrplot)
library(corrgram)

x <- cor(vendasCompletasNormalizada[c("Venta_uni_hoy", "Venta_hoy", "Dev_uni_proxima", 
                                 "Dev_proxima", "Demanda_uni_equil")])
x

pairs(vendasCompletasNormalizada[c("Venta_uni_hoy", "Venta_hoy", "Dev_uni_proxima", 
                                   "Dev_proxima", "Demanda_uni_equil")])

corrplot(x, method = 'color')
corrgram(vendasCompletasNormalizada)

# Obs: Somente entre as variáveis numéricas o que dá para perceber, como já havia analisado
# acima é um fortíssimo relacionaemnto entre vendas e demanda (o que era de se esperar).



## 6 - Seleção de Atributos/Feature Selection:
library(caret)

# Eu tentei executar o modelo com todas as variáveis, mas devido ao tamanho do mesmo, não foi
# possível, sendo assim, eu irei retirar as variáveis que representam IDs como Agencia_ID,
# Ruta_SAK e que já tem uma variável categórica associada, além dos nomes

vendasCompletasNormalizada$Agencia_ID <- NULL
vendasCompletasNormalizada$Town_ID <- NULL
vendasCompletasNormalizada$Town <- NULL
vendasCompletasNormalizada$Ruta_SAK <- NULL
vendasCompletasNormalizada$Cliente_ID <- NULL
vendasCompletasNormalizada$NombreCliente <- NULL
vendasCompletasNormalizada$Producto_ID <- NULL
vendasCompletasNormalizada$NombreProducto <- NULL

set.seed(4050)

modeloFS01lm <- train(Demanda_uni_equil ~ ., data = vendasCompletasNormalizada, method = "lm")
varImp(modeloFS01lm)
summary(modeloFS01lm)
#plot(varImp(modeloFS01lm))

# Obs: As variávies mais importantes apresentadas acabaram sendo as mais esperadas:
# Venta_uni_hoy, Dev_proxima, Dev_uni_proxima, seguidas por alguns grupos de produtos e canais



## 7 - Divisão dos Dados em Treino e Teste:

linhas <- sample(1:nrow(vendasCompletasNormalizada), 0.7 * nrow(vendasCompletasNormalizada))
dados_treino <- vendasCompletasNormalizada[linhas,]
dados_teste <- vendasCompletasNormalizada[-linhas,]



## 8 - Construção e Treinamento do Modelo:

library(randomForest)
library(caret)
library(C50)
library(neuralnet)

# Devido ao problema de performance para processar todas as variáveis, irei utilizar na 
# construção dos modelos somente as variáveis indicadas como mais importantes identificadas
# no feature selection
formulaModelo <- as.formula(Demanda_uni_equil ~ Venta_uni_hoy + Dev_proxima + Dev_uni_proxima 
                            + Producto_ID_groups + Canal_ID)

# Linear Model
modelo1LM <- lm (formulaModelo, data = dados_treino)
modelo1LM

# Random Forest
modelo2RF <- randomForest(formulaModelo, data = dados_treino, ntree = 40, nodesize = 10)
modelo2RF  # 79.63

# Neuralnet - Não consegui implementar esse modelo. Mesmo seguindo todos os passos, sempre
# retornou o erro:
# Error in neurons[[i]] %*% weights[[i]] : requires numeric/complex matrix/vector arguments
#modelo3NN <- neuralnet(formulaModelo, dados_treino, linear.output = FALSE)
#modelo3NN
#plot(modelo3NN)



## 9 - Teste do Modelo:

# Linear Model
previsao1LM <- predict(modelo1LM, dados_teste)
View(previsao1LM)

# Random Forest
previsao2RF <- predict(modelo1RF, newdata = dados_teste)
View(previsao2RF)



## 10 - Avaliação Performance do Modelo:

# Linear Model
# Calculando os resíduos de forma manual:
resultados1LM           <- cbind(previsao1LM, dados_teste$Demanda_uni_equil) 
colnames(resultados1LM) <- c('Previsto','Real')
resultados1LM           <- as.data.frame(resultados1LM)
resultados1LM
min(resultados1LM)

# Aplicando a função que trata os valores negativos
resultados1LM$Previsto <- sapply(resultados1LM$Previsto, trataNegativos)
resultados1LM

# Calculando o erro médio - Distancia entre os valores previstos e reais (observados)
# MSE - Mean Squared Error
mseModelo1LM <- mean((resultados1LM$Real - resultados1LM$Previsto)^2)
mseModelo1LM

#RMSE - Root Mean Squared Error
rmseModelo1LM <- sqrt(mseModelo1LM)
rmseModelo1LM

#R Squared
SSE1LM <- sum((resultados1LM$Real - resultados1LM$Previsto)^2)
SST1LM <- sum((mean(dados_teste$Demanda_uni_equil) - resultados1LM$Real)^2)

R21LM = 1 - (SSE1LM/SST1LM)
R21LM   #0.9999916

summary(modelo1LM)
#Residual standard error: 0.001134 on 112341 degrees of freedom
#Multiple R-squared:  0.9999,	Adjusted R-squared:  0.9999 
#F-statistic: 9.081e+07 on 22 and 112341 DF,  p-value: < 2.2e-16

# Obs: Com um R-squared de cerca de 0.9999 está muito claro que deve estar ocorrendo um 
# overfitting, provavelmente por haver uma ligação muito forte entre a variável preditora e a 
# coluna Venta_uni_hoy, que são praticamente iguais na maioria dos registros.
# Uma solução poderia ser não utilizar essa coluna no modelo, porém, poderá trazer outros 
# efeitos colaterais

# Random Forest
resultados2RF           <- cbind(previsao2RF, dados_teste$Demanda_uni_equil) 
colnames(resultados2RF) <- c('Previsto','Real')
resultados2RF           <- as.data.frame(resultados2RF)
resultados2RF
min(resultados2RF)

# Calculando o erro médio - Distancia entre os valores previstos e reais (observados)
# MSE - Mean Squared Error
mseModelo2RF <- mean((resultados2RF$Real - resultados2RF$Previsto)^2)
mseModelo2RF

#RMSE - Root Mean Squared Error
rmseModelo2RF <- sqrt(mseModelo2RF)
rmseModelo2RF

#R Squared
SSE2RF <- sum((resultados2RF$Real - resultados2RF$Previsto)^2)
SST2RF <- sum((mean(dados_teste$Demanda_uni_equil) - resultados2RF$Real)^2)

R22RF = 1 - (SSE2RF/SST2RF)
R22RF   # 0.7884916

# Gerando um plot dos resíduos
residuos2RF <- resultados2RF$Real - resultados2RF$Previsto
View(residuos2RF)

qqnorm(residuos2RF)
qqline(residuos2RF)



## 11 - Otimização do Modelo:

# Tentei utilizar uma divisao dos dados para ver se mudava alguma coisa na regressão linear,
# porém, não mudou nada, o modelo continua com overfitting
controle <- trainControl(method = "cv", number = 10)

modeloLMv2 <- train(formula, 
                    data = dados_treino, 
                    method = "lm", 
                    trControl = controle, 
                    metric = "Rsquared")

# Resumo do modelo
summary(modeloLMv2)     # Multiple R-squared:  0.9999

# Coletando os residuos
residualsLMv2 <- resid(modeloLMv2)
residualsLMv2

# Previsoes
previsaoLMv2 <- predict(modeloLMv2, dados_teste)
previsaoLMv2
plot(dados_teste$Demanda_uni_equil, previsaoLMv2)

# Plot das variáveis mais relevantes no modelo
plot(varImp(modeloLMv2))

# Obs: Como os modelos que utilizarm o algoritimo de regressão linear aparentam estar com 
# overfitting, talvez o ideal fosse seguir com o modelo Random Forest, que ficou com R-Squared
# de 0.7884916


# Random Forest - Otimizacao
# Estou incluindo novas variáveis para tentar melhorar o percentual de acertos e diminuir o
modeloRFv2 <- randomForest(Demanda_uni_equil ~ Venta_uni_hoy + Dev_proxima + 
                           Dev_uni_proxima + Producto_ID_groups + Canal_ID +
                           Venta_hoy + Ruta_SAK_groups + Agencia_ID_groups, 
                           data = dados_treino, 
                           ntree = 40, nodesize = 10)
modeloRFv2  # 98.84

previsaoRFv2 <- predict(modeloRFv2, newdata = dados_teste)
View(previsaoRFv2)

resultadosRFv2           <- cbind(previsaoRFv2, dados_teste$Demanda_uni_equil) 
colnames(resultadosRFv2) <- c('Previsto','Real')
resultadosRFv2           <- as.data.frame(resultadosRFv2)
resultadosRFv2
min(resultadosRFv2)

# Aplicando a função que trata os valores negativos
resultadosRFv2$Previsto <- sapply(resultadosRFv2$Previsto, trataNegativos)
resultadosRFv2

# Calculando o erro médio - Distancia entre os valores previstos e reais (observados)
# MSE - Mean Squared Error
mseModeloRFv2 <- mean((resultadosRFv2$Real - resultadosRFv2$Previsto)^2)
mseModeloRFv2

#RMSE - Root Mean Squared Error
rmseModeloRFv2 <- sqrt(mseModeloRFv2)
rmseModeloRFv2

#R Squared
SSERFv2 <- sum((resultadosRFv2$Real - resultadosRFv2$Previsto)^2)
SSTRFv2 <- sum((mean(dados_teste$Demanda_uni_equil) - resultadosRFv2$Real)^2)

R22RF = 1 - (SSERFv2/SSTRFv2)
R22RF   # 0.990819

# Com a inclusão de novas variáveis na montagem do modelo Random Forest, os resíduos diminuiram
# muito aumentando bastante o percentual de acerto que saiu de cerca de 78% para 98%. O que 
# também pode indicar um overfitting.

# Por último, vou transformar as previsões que estão normalizadas para os valores originais
# Retirando a normalização para voltar ao padrão original

previsoesDesnormalizadas <- resultadosRFv2$Previsto * 
                           (max(vendasCompletas$Demanda_uni_equil) - 
                            min(vendasCompletas$Demanda_uni_equil)) +  
                            min(vendasCompletas$Demanda_uni_equil)
View(previsoesDesnormalizadas)

targetDesnormalizadas <- (dados_teste$Demanda_uni_equil) * 
                         (max(vendasCompletas$Demanda_uni_equil) - 
                          min(vendasCompletas$Demanda_uni_equil)) +
                          min(vendasCompletas$Demanda_uni_equil)
View(targetDesnormalizadas)

previsoesFinais           <- cbind(targetDesnormalizadas, previsoesDesnormalizadas) 
colnames(previsoesFinais) <- c('Real', 'Previsto')
previsoesFinais           <- as.data.frame(previsoesFinais)

View(previsoesFinais)