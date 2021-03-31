#TRABALHO ANALISE : WINE
#Executado por : Luiz Gustavo S. Gonçalves / Fernando Spalter / Douglas Lima

install.packages("dplyr", dependencies = TRUE)
library(dplyr)
install.packages("caret")
library(caret)
library(gbm)
library(randomForest)
#Adicionado para o Gradient Boosting

#Leitura da Base de Dados
#setwd('C:/Users/dlima23/Desktop/MBA 2019/Modelagem estatistica/Trabalhos/Grupo/Wine')
setwd('D:/R/WINE_2/WINE_2/')
DT1 <- read.csv("winequality-red.csv", sep = ";",dec = '.')
DT2 <- read.csv("winequality-white.csv",sep = ";",dec = '.')

#Criando a coluna Type
DT1$type <- "red"
DT2$type <- "white"

#Unindo os dois Dataset
remove.packages(rlang)
install.packages("rlang")
library(rlang)
DATA <- dplyr::bind_rows(DT1,DT2)
write.csv(summary(DATA), "summary.csv")

#Verficando a existencia de NAs
table(is.na(DATA))


boxplot(DATA$fixed.acidity,main = "Fixed Acidity",col=c("cyan"))
DATA_FILTER <- filter(DATA, DATA_FILTER$fixed.acidity < 14) # adicionada Fernando 18/03

boxplot(DATA$volatile.acidity,main = "Volatile Acidity",col=c("cyan"))
DATA_FILTER <- filter(DATA, DATA_FILTER$volatile.acidity < 1.2) # adicionada Fernando 18/03

boxplot(DATA$citric.acid,main = "Citric Acid",col=c("cyan"))
DATA_FILTER <- filter(DATA, DATA_FILTER$citric.acid <= 1) # adicionada Fernando 18/03

boxplot(DATA$residual.sugar,main = "Residual Sugar",col=c("cyan"))
DATA_FILTER <- filter(DATA, DATA_FILTER$residual.sugar < 22) # adicionada Fernando 18/03

boxplot(DATA$chlorides,main = "chlorides",col=c("cyan"))
DATA_FILTER <- filter(DATA, DATA_FILTER$chlorides < 0.25) # adicionada Fernando 18/03

boxplot(DATA$total.sulfur.dioxide,main = "Total Sulfur Dioxide",col=c("cyan"))
DATA_FILTER <- filter(DATA, DATA_FILTER$total.sulfur.dioxide <= 260 ) # adicionada Fernando 18/03

boxplot(DATA$free.sulfur.dioxide,main = "Free Sulfur Dioxide",col=c("cyan"))
DATA_FILTER <- filter(DATA, DATA_FILTER$free.sulfur.dioxide < 90 ) # adicionada Fernando 18/03

boxplot(DATA$pH,main = "PH",col=c("cyan"))
DATA_FILTER <- filter(DATA, DATA_FILTER$pH < 3.85 ) # adicionada Fernando 18/03

boxplot(DATA$sulphates,main = "Sulfate",col=c("cyan"))
DATA_FILTER <- filter(DATA, DATA_FILTER$sulphates < 1.3 ) # adicionada Fernando 18/03

#Outliers
boxplot(DATA$volatile.acidity, main = "Volatile Acidity", col=c("cyan"))
DATA_FILTER <- filter(DATA, DATA$volatile.acidity < 1.4)
boxplot(DATA_FILTER$volatile.acidity, main = "Volatile Acidity", col=c("cyan"))


boxplot(DATA$citric.acid, main = "Citric Acid",col=c("cyan"))
DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$citric.acid < 1.3)

boxplot(DATA_FILTER$residual.sugar, main = "Residual Sugar",col=c("cyan"))
DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$residual.sugar < 40)


boxplot(DATA$free.sulfur.dioxide, main = "Free Sulfur Dioxide",col=c("cyan"))
DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$free.sulfur.dioxide < 150)


boxplot(DATA$total.sulfur.dioxide, main = "Total Sulfur Dioxide",col=c("cyan"))
filter(DATA_FILTER, DATA_FILTER$total.sulfur.dioxide > 300 )


boxplot(DATA_FILTER$density,main = "Density",col=c("cyan"))
filter(DATA_FILTER, DATA_FILTER$density > 1.02)

boxplot(DATA_FILTER$sulphates,main = "Sulfate",col=c("cyan"))
filter(DATA_FILTER, DATA_FILTER$residual.sugar < 40)

boxplot(DATA$alcohol,main = "Alcohol",col=c("cyan"))
filter(DATA_FILTER, DATA_FILTER$residual.sugar < 40)

DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$fixed.acidity < 14) # adicionada Fernando 18/03
DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$volatile.acidity < 1.2) # adicionada Fernando 18/03
DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$citric.acid <= 1) # adicionada Fernando 18/03
DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$residual.sugar < 22) # adicionada Fernando 18/03
DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$chlorides < 0.25) # adicionada Fernando 18/03
DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$total.sulfur.dioxide <= 260 ) # adicionada Fernando 18/03
DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$free.sulfur.dioxide < 90 ) # adicionada Fernando 18/03
DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$pH < 3.85 ) # adicionada Fernando 18/03
DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$sulphates < 1.3 ) # adicionada Fernando 18/03

par(mfrow = c(1,1))
boxplot(DATA_FILTER$fixed.acidity,main = "Fixed Acidity",col=c("cyan"))
#DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$fixed.acidity < 14) # adicionada Fernando 18/03

boxplot(DATA_FILTER$volatile.acidity,main = "Volatile Acidity",col=c("cyan"))
#DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$volatile.acidity < 1.2) # adicionada Fernando 18/03

boxplot(DATA_FILTER$citric.acid,main = "Citric Acid",col=c("cyan"))
#DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$citric.acid <= 1) # adicionada Fernando 18/03

boxplot(DATA_FILTER$residual.sugar,main = "Residual Sugar",col=c("cyan"))
#DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$residual.sugar < 22) # adicionada Fernando 18/03

boxplot(DATA_FILTER$chlorides,main = "chlorides",col=c("cyan"))
#DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$chlorides < 0.25) # adicionada Fernando 18/03

boxplot(DATA_FILTER$total.sulfur.dioxide,main = "Total Sulfur Dioxide",col=c("cyan"))
#DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$total.sulfur.dioxide <= 260 ) # adicionada Fernando 18/03

boxplot(DATA_FILTER$free.sulfur.dioxide,main = "Free Sulfur Dioxide",col=c("cyan"))
#DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$free.sulfur.dioxide < 90 ) # adicionada Fernando 18/03

boxplot(DATA_FILTER$pH,main = "PH",col=c("cyan"))
#DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$pH < 3.85 ) # adicionada Fernando 18/03

boxplot(DATA_FILTER$sulphates,main = "Sulfate",col=c("cyan"))
#DATA_FILTER <- filter(DATA_FILTER, DATA_FILTER$sulphates < 1.3 ) # adicionada Fernando 18/03




######## Existem linhas duplicadas!

DATA_FILTER <- unique(DATA_FILTER)


DATA <- DATA_FILTER
rm(DATA_FILTER)

names(DATA) <- gsub(x = names(DATA), pattern = "\\.", replacement = "_")  
View(DATA)

DATA= DATA %>% mutate_if(is.character, as.factor)


View(DATA)

# tabela para consolidar e comparar modelos

reg_tbl <- 
  data.frame(regressão = character(),
             RMSE_TRAIN = numeric(),
             RMSE_TEST = numeric(),
             R2_TRAIN = numeric(),
             R2_TEST = numeric()
  )


############################ANÁLISE EXPLORATÓRIA################################

par(mfrow = c(1,1))

# 1) TARGET
hist((DATA$quality), breaks = 7,
     xlab = 'quality', ylab = 'Frequency (#)',
     main = 'Target variable',
     cex.axis= 1.2, cex.main= 1.6, cex.axis = 1.2, cex.lab = 1.3,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 

par(mfrow = c(1,2)) # parametros graficos

# 2) FEATURES

hist(DATA$fixed_acidity, breaks = 10, main = '', cex.lab = 1.3,
     ylab = 'Frequency (#)', xlab = 'Univariate Analysis', cex.axis = 1.2,  
     ylim = c(0,4000), col = 'cyan', border = 'darkcyan')

boxplot(fixed_acidity ~ quality, data = DATA,
        main = '', ylab = 'fixed_acidity', xlab = 'Bivariate Analysis', 
        cex.main = 1.2, cex.axis = 1.2, cex.lab = 1.3,
        col = 'cyan', border = 'gray20')

mtext('fixed_acidity', outer = TRUE,  cex = 1.6, line=-2)


hist(DATA$volatile_acidity, breaks = 10, main = '', cex.lab = 1.3,
     ylab = 'Frequency (#)', xlab = 'Univariate Analysis', cex.axis = 1.2,  
     ylim = c(0,2300), col = 'cyan', border = 'darkcyan')

boxplot(volatile_acidity ~ quality, data = DATA,
        main = '', ylab = 'volatile_acidity', xlab = 'Bivariate Analysis', 
        cex.main = 1.2, cex.axis = 1.2, cex.lab = 1.3,
        col = 'cyan', border = 'gray20')

mtext('volatile_acidity', outer = TRUE,  cex = 1.6, line=-2)

hist(DATA$citric_acid, breaks = 10, main = '', cex.lab = 1.3,
     ylab = 'Frequency (#)', xlab = 'Univariate Analysis', cex.axis = 1.2,  
     ylim = c(0,2500), col = 'cyan', border = 'darkcyan')

boxplot(citric_acid ~ quality, data = DATA,
        main = '', ylab = 'citric_acid', xlab = 'Bivariate Analysis', 
        cex.main = 1.2, cex.axis = 1.2, cex.lab = 1.3,
        col = 'cyan', border = 'gray20')

mtext('citric_acid', outer = TRUE,  cex = 1.6, line=-2)

hist(DATA$residual_sugar, breaks = 10, main = '', cex.lab = 1.3,
     ylab = 'Frequency (#)', xlab = 'Univariate Analysis', cex.axis = 1.2,  
     ylim = c(0,4000), col = 'cyan', border = 'darkcyan')

boxplot(residual_sugar ~ quality, data = DATA,
        main = '', ylab = 'residual_sugar', xlab = 'Bivariate Analysis', 
        cex.main = 1.2, cex.axis = 1.2, cex.lab = 1.3,
        col = 'cyan', border = 'gray20')

mtext('residual_sugar', outer = TRUE,  cex = 1.6, line=-2)

hist(DATA$chlorides, breaks = 10, main = '', cex.lab = 1.3,
     ylab = 'Frequency (#)', xlab = 'Univariate Analysis', cex.axis = 1.2,  
     ylim = c(0,4000), col = 'cyan', border = 'darkcyan')

boxplot(chlorides ~ quality, data = DATA,
        main = '', ylab = 'chlorides', xlab = 'Bivariate Analysis', 
        cex.main = 1.2, cex.axis = 1.2, cex.lab = 1.3,
        col = 'cyan', border = 'gray20')

mtext('chlorides', outer = TRUE,  cex = 1.6, line=-2)

hist(DATA$free_sulfur_dioxide, breaks = 10, main = '', cex.lab = 1.3,
     ylab = 'Frequency (#)', xlab = 'Univariate Analysis', cex.axis = 1.2,  
     ylim = c(0,1500), col = 'cyan', border = 'darkcyan')

boxplot(free_sulfur_dioxide ~ quality, data = DATA,
        main = '', ylab = 'free_sulfur_dioxide', xlab = 'Bivariate Analysis', 
        cex.main = 1.2, cex.axis = 1.2, cex.lab = 1.3,
        col = 'cyan', border = 'gray20')

mtext('free_sulfur_dioxide', outer = TRUE,  cex = 1.6, line=-2)

hist(DATA$total_sulfur_dioxide, breaks = 10, main = '', cex.lab = 1.3,
     ylab = 'Frequency (#)', xlab = 'Univariate Analysis', cex.axis = 1.2,  
     ylim = c(0,1000), col = 'cyan', border = 'darkcyan')

boxplot(total_sulfur_dioxide ~ quality, data = DATA,
        main = '', ylab = 'total_sulfur_dioxide', xlab = 'Bivariate Analysis', 
        cex.main = 1.2, cex.axis = 1.2, cex.lab = 1.3,
        col = 'cyan', border = 'gray20')

mtext('total_sulfur_dioxide', outer = TRUE,  cex = 1.6, line=-2)

hist(DATA$density, breaks = 10, main = '', cex.lab = 1.3,
     ylab = 'Frequency (#)', xlab = 'Univariate Analysis', cex.axis = 1.2,  
     ylim = c(0,2000), col = 'cyan', border = 'darkcyan')

boxplot(density ~ quality, data = DATA,
        main = '', ylab = 'density', xlab = 'Bivariate Analysis', 
        cex.main = 1.2, cex.axis = 1.2, cex.lab = 1.3,
        col = 'cyan', border = 'gray20')

mtext('density', outer = TRUE,  cex = 1.6, line=-2)

hist(DATA$pH, breaks = 10, main = '', cex.lab = 1.3,
     ylab = 'Frequency (#)', xlab = 'Univariate Analysis', cex.axis = 1.2,  
     ylim = c(0,12000), col = 'cyan', border = 'darkcyan')

boxplot(pH ~ quality, data = DATA,
        main = '', ylab = 'pH', xlab = 'Bivariate Analysis', 
        cex.main = 1.2, cex.axis = 1.2, cex.lab = 1.3,
        col = 'cyan', border = 'gray20')

mtext('pH', outer = TRUE,  cex = 1.6, line=-2)

hist(DATA$sulphates, breaks = 10, main = '', cex.lab = 1.3,
     ylab = 'Frequency (#)', xlab = 'Univariate Analysis', cex.axis = 1.2,  
     ylim = c(0,2000), col = 'cyan', border = 'darkcyan')

boxplot(sulphates ~ quality, data = DATA,
        main = '', ylab = 'sulphates', xlab = 'Bivariate Analysis', 
        cex.main = 1.2, cex.axis = 1.2, cex.lab = 1.3,
        col = 'cyan', border = 'gray20')

mtext('sulphates', outer = TRUE,  cex = 1.6, line=-2)

hist(DATA$alcohol, breaks = 5, main = '', cex.lab = 1.3,
     ylab = 'Frequency (#)', xlab = 'Univariate Analysis', cex.axis = 1.2,  
     ylim = c(0,2000), col = 'cyan', border = 'darkcyan')

boxplot(alcohol ~ quality, data = DATA,
        main = '', ylab = 'alcohol', xlab = 'Bivariate Analysis', 
        cex.main = 1.2, cex.axis = 1.2, cex.lab = 1.3,
        col = 'cyan', border = 'gray20')

mtext('alcohol', outer = TRUE,  cex = 1.6, line=-2)


barplot(table(DATA$type),
        main = '', cex.lab = 1.3,
        ylab = 'Frequency (#)', xlab = 'Univariate Analysis', cex.axis = 1.2,  
        ylim = c(0,12000), col = 'cyan', border = 'darkcyan')

barplot(prop.table(table(DATA$type,DATA$quality),margin = 2),
        main = '', cex.lab = 1.3,
        ylab = 'Frequency (%)', xlab = 'Bivariate Analysis', cex.axis = 1.2, 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

mtext('type', outer = TRUE,  cex = 1.6, line=-2)



############################RANDOM FOREST PARA REGRESSÃO#######################
set.seed(123)

INDEX_TRAIN <- createDataPartition(DATA$quality, p = 0.7, list = F)
TRAIN_SET <- DATA[INDEX_TRAIN, ] # base de desenvolvimento: 70%
TEST_SET  <- DATA[-INDEX_TRAIN,] # base de teste: 30%

summary(TRAIN_SET$quality);summary(TEST_SET$quality)


library(randomForest)
MDL_FIT <- randomForest(quality ~ .,
                        data = TRAIN_SET,
                        importance = T)

# SAÃDA
MDL_FIT
plot(MDL_FIT, main = 'Out-of-bag error')

#HIPERPARAMETROS 

PARMS_GRID <- expand.grid(mtry     = c(2,4,8,13),
                          nodesize = c(10,50,100),
                          ntree    = c(100,300, 500),
                          maxnodes = c(500,1000))

PARMS_GRID; dim(PARMS_GRID)

START_TIME <- Sys.time()
# fazendo o treino iterativo
for (i in 1:nrow(PARMS_GRID)) {
  
  # reprodutibilidade
  set.seed(123)
  
  # treinando o modelo
  MDL_FIT <- randomForest(quality ~ .,
                          data = TRAIN_SET,
                          importance = T,
                          mtry       = PARMS_GRID$mtry[i],
                          nodesize   = PARMS_GRID$nodesize[i], 
                          ntree      = PARMS_GRID$ntree[i],
                          maxnodes = PARMS_GRID$maxnodes[i])
  
  # avaliando a performance da combinaÃ§Ã£o de parametros 
  METRICS_TRAIN <- postResample(pred = predict(MDL_FIT), 
                                obs = TRAIN_SET$quality)
  
  METRICS_TEST  <- postResample(pred = predict(MDL_FIT, newdata = TEST_SET),
                                obs = TEST_SET$quality) 
  
  # adicionando as mÃ©tricas ao grid
  PARMS_GRID$RMSE_TRAIN[i] <- METRICS_TRAIN[[1]]
  PARMS_GRID$RMSE_TEST[i]  <- METRICS_TEST[[1]]
  PARMS_GRID$R2_TRAIN [i]  <- METRICS_TRAIN[[2]]
  PARMS_GRID$R2_TEST[i]    <- METRICS_TEST[[2]]        
  
}

END_TIME <- Sys.time()
END_TIME - START_TIME # elapsed time: 6.380893 mins

PARMS_GRID = as.data.frame(PARMS_GRID) %>% dplyr::arrange(abs(RMSE_TRAIN-RMSE_TEST),
                                                          RMSE_TEST)

View(PARMS_GRID)

#################################Fim RANDOM FOREST


# treinando o modelo com os parÃ¢metros finalistas
set.seed(123)
library(randomForest)
MDL_FIT <- randomForest(quality ~ .,
                        data = TRAIN_SET,
                        importance = T,
                        mtry       = 8,
                        nodesize   = 10, 
                        ntree      = 100,
                        maxnodes = 500)

MDL_FIT
plot(MDL_FIT, main = 'Out-of-bag error')


# 4) Realizando as predicoes

# Valor de LOG_PRICE pelo random forest 
Y_VAL_TRAIN <- predict(MDL_FIT) 
Y_VAL_TEST  <- predict(MDL_FIT, newdata = TEST_SET)

library(caret)

METRIC_FORTRAIN <- postResample(pred = Y_VAL_TRAIN, obs = TRAIN_SET$quality)
METRIC_FORTEST <- postResample(pred = Y_VAL_TEST,  obs = TEST_SET$quality)

MDL_FINAL <- MDL_FIT

varImpPlot(MDL_FINAL, sort= T, main = 'Importancia das Variaveis')

MDL_FINAL$importance

# tabela comparando modelos

reg_tbl[1,] <- list("Vencedor Random Forest",
                    unname(METRIC_FORTRAIN[1]),
                    unname(METRIC_FORTEST[1]),
                    unname(METRIC_FORTRAIN[2]),
                    unname(METRIC_FORTEST[2])
)

#InspeÃ§Ã£o dos valores finais


RESULT_TRAIN <- data.frame(Y_OBS  = TRAIN_SET$quality,
                           Y_PRED = Y_VAL_TRAIN) %>%
  mutate(ERROR = Y_PRED - Y_OBS)

RESULT_TEST  <- data.frame(Y_OBS  = TEST_SET$quality,
                           Y_PRED = Y_VAL_TEST) %>%
  mutate(ERROR = Y_PRED - Y_OBS)


par(mfrow = c(2,2))

#plotando os resultados :


hist(RESULT_TRAIN$ERROR, breaks = 40, 
     main = 'Train set',   xlab = 'Error', ylab = 'Frequency (#)',   
     col = 'darkorange', border = 'brown')

hist(RESULT_TEST$ERROR, breaks = 40, 
     main = 'Test set', cex.main = 1.3,  cex.lab = 1.3,
     xlab = 'Error', ylab = 'Frequency (#)', cex.axis = 1.2,  
     col = 'darkorange', border = 'brown')

plot(RESULT_TRAIN$Y_PRED,RESULT_TRAIN$Y_OBS, 
     xlab = 'QUALITY (Predicted)', ylab = 'QUALITY(Observed)',
     col = 'darkorange')
abline(0, 1, col = 'blue', lwd = 3, lty = "dashed")

plot(RESULT_TEST$Y_PRED, RESULT_TEST$Y_OBS,  cex.lab = 1.3, 
     xlab = 'QUALITY (Predicted)', ylab = 'QUALITY (Observed)',
     cex.axis = 1.2, pch = 19, cex = 0.5,# ylim = c(0,6000),
     col = 'darkorange')
abline(0, 1, col = 'blue', lwd = 3, lty = "dashed")

#################################Fim RANDOM FOREST
#--------------------------------------------------------------------------------#
############################GRADIENT BOOSTING PARA REGRESSÃO#######################

# 1) Divisao da base de modelagem em treino e teste com amostragem

# Reduzindo a dimensao do dataset (algortimo pode demorar para rodar)
set.seed(123)
DATA2 = DATA
View(DATA2)

INDEX_TRAIN <- createDataPartition(DATA2$quality, p = 0.7, list = F)
TRAIN_SET <- DATA2[INDEX_TRAIN, ] # base de desenvolvimento: 70%
TEST_SET  <- DATA2[-INDEX_TRAIN,] # base de teste: 30%

# Avaliando a distribuicao da variavel resposta
summary(TRAIN_SET$quality);summary(TEST_SET$quality)

#--------------------------------------------------------------------------------#
# 2) Treino do algoritmo do gradient boosting para regresssao

MDL_FIT  <- gbm(formula = quality ~ .,
                data = TRAIN_SET,
                distribution = 'gaussian',
                train.fraction = 0.75,
                verbose = TRUE) 

# saida do modelo
print(MDL_FIT)

# pegar MSE e calcular RMSE via Cross validation (caso tenha sido escolhida
# essa opção)
#sqrt(min(MDL_FIT$cv.error))

# plot loss function as a result of n trees added to the ensemble
gbm.perf(MDL_FIT, plot.it = T, method = 'OOB')
legend("topright", c('Train error',"CV error","OOB error"), 
       lty=1, col=c("black","green","red"))

#--------------------------------------------------------------------------------#
# 3) Busca pelos melhores parâmetros

# ajuste dos hiperparâmetros
# criando o grid para fazer as combinações dos parâmetros
PARMS_GRID <- expand.grid(shrinkage         = c(0.01, 0.05, 0.1),
                          interaction.depth = c(1, 2, 3, 4),
                          n.minobsinnode    = c(50),
                          n.trees           = c(1000),
                          bag.fraction      = c(0.75, 1.00),
                          train.fraction    = c(0.75, 1.00))


PARMS_GRID; dim(PARMS_GRID)

library(gbm)
START_TIME <- Sys.time()
# fazendo o treino iterativo
for (i in 1:nrow(PARMS_GRID)) {
  
  # reprodutibilidade
  set.seed(123)
  
  # treinando o modelo
  MDL_FIT  <- gbm(formula = quality ~ .,
                  data = TRAIN_SET,
                  distribution = 'gaussian',
                  n.trees           = PARMS_GRID$n.trees[i],
                  interaction.depth = PARMS_GRID$interaction.depth[i],
                  n.minobsinnode    = PARMS_GRID$n.minobsinnode[i],
                  shrinkage         = PARMS_GRID$shrinkage[i],
                  bag.fraction      = PARMS_GRID$bag.fraction[i],
                  train.fraction    = PARMS_GRID$train.fraction[i],
                  cv.folds = 1,
                  n.cores = NULL, # will use all cores by default
                  verbose = FALSE) 
  
  # avaliando a performance da combinação de parametros 
  METRICS_TRAIN <- postResample(pred = predict(MDL_FIT, n.trees = 1000), 
                                obs = TRAIN_SET$quality)
  
  METRICS_TEST  <- postResample(pred = predict(MDL_FIT, newdata = TEST_SET,
                                               n.trees = 1000),
                                obs = TEST_SET$quality) 
  
  # adicionando as métricas ao grid
  PARMS_GRID$RMSE_TRAIN[i] <- METRICS_TRAIN[[1]]
  PARMS_GRID$RMSE_TEST[i]  <- METRICS_TEST[[1]]
  PARMS_GRID$R2_TRAIN [i]  <- METRICS_TRAIN[[2]]
  PARMS_GRID$R2_TEST[i]    <- METRICS_TEST[[2]]        
  
}

END_TIME <- Sys.time()
END_TIME - START_TIME # elapsed time: 4.691175 mins

# analisando os resultados e escolha da melhor combinação
PARMS_GRID = as.data.frame(PARMS_GRID) %>% dplyr::arrange(abs(RMSE_TRAIN-RMSE_TEST),RMSE_TEST)


View(PARMS_GRID)
# treinando o modelo com os parâmetros finalistas
set.seed(123)
MDL_FIT  <- gbm(formula = quality ~ .,
                data = TRAIN_SET,
                distribution = 'gaussian',
                n.trees = 1000,
                interaction.depth = 1,
                n.minobsinnode = 50,
                shrinkage = 0.01,
                bag.fraction = 0.75,
                train.fraction = 1,
                cv.folds = 1,
                n.cores = NULL, # will use all cores by default
                verbose = TRUE) 
?gbm
# analisando o número ótimo de árvores
gbm.perf(MDL_FIT, plot.it = T, method = 'OOB')
legend("topright", c('Train error',"CV error","OOB error"), 
       lty=1, col=c("black","green","red"))

#--------------------------------------------------------------------------------#
# 4) Realizando as predicoes

# Valor de LOG_PRICE pelo random forest 
Y_VAL_TRAIN <- predict(MDL_FIT, n.trees = 730)
Y_VAL_TEST  <- predict(MDL_FIT, newdata = TEST_SET, n.trees = 730)

#--------------------------------------------------------------------------------#
# 5) Avaliando a performance dos modelos e existencia de overfitting

# Gradient Boosting para regressão
METRICS_GBMTRAIN <- postResample(pred = Y_VAL_TRAIN, obs = TRAIN_SET$quality)
METRICS_GBMTEST <- postResample(pred = Y_VAL_TEST,  obs = TEST_SET$quality)


# tabela comparando modelos

reg_tbl[2,] <- list("Vencedor GBM",
                    unname(METRICS_GBMTRAIN[1]),
                    unname(METRICS_GBMTEST[1]),
                    unname(METRICS_GBMTRAIN[2]),
                    unname(METRICS_GBMTEST[2])
)

# sinais de overfitting entre as amostras de treino e teste? 
MDL_FINAL <- MDL_FIT

#--------------------------------------------------------------------------------#
# 6) Importancia das variaveis (Modelo final)

# o pacote gbm também possui uma forma de ver a importancia das 
# variaveis
par(mar = c(5, 15, 1, 1))
summary(MDL_FINAL,
        cBars = 10,
        method = relative.influence, # also can use permutation.test.gbm
        las = 1)

#--------------------------------------------------------------------------------#
# 7) Inspecao dos valores previstos vs observados (modelo final)

# Convertendo a variavel para unidade original
RESULT_TRAIN <- data.frame(Y_OBS  = TRAIN_SET$quality,
                           Y_PRED = Y_VAL_TRAIN) %>%
  mutate(ERROR = Y_PRED - Y_OBS)

RESULT_TEST  <- data.frame(Y_OBS  = TEST_SET$quality,
                           Y_PRED = Y_VAL_TEST) %>%
  mutate(ERROR = Y_PRED - Y_OBS)

# Plotando os resultados
par(oma = c(1,1,0,0),  mar = c(4,5,2,1))

hist(RESULT_TRAIN$ERROR, breaks = 40, 
     main = 'Train set',   xlab = 'Error', ylab = 'Frequency (#)',   
     col = 'darkorange', border = 'brown')

hist(RESULT_TEST$ERROR, breaks = 40, 
     main = 'Test set', cex.main = 1.3,  cex.lab = 1.3,
     xlab = 'Error', ylab = 'Frequency (#)', cex.axis = 1.2,  
     col = 'darkorange', border = 'brown')

plot(RESULT_TRAIN$Y_PRED,RESULT_TRAIN$Y_OBS, 
     xlab = 'QUALITY (Predicted)', ylab = 'QUALITY(Observed)',
     col = 'darkorange')
abline(0, 1, col = 'blue', lwd = 3, lty = "dashed")

plot(RESULT_TEST$Y_PRED, RESULT_TEST$Y_OBS,  cex.lab = 1.3, 
     xlab = 'QUALITY (Predicted)', ylab = 'QUALITY (Observed)',
     cex.axis = 1.2, pch = 19, cex = 0.5,# ylim = c(0,6000),
     col = 'darkorange')
abline(0, 1, col = 'blue', lwd = 3, lty = "dashed")


############################FIM GRADIENT BOOSTING
#--------------------------------------------------------------------------------#
############################REGRESSÃO LINEAR PARA REGRESSÃO#######################

# 1) Divisao da base de modelagem em treino e teste com amostragem

set.seed(123) # garantindo reprodutibilidade da amostra

INDEX_TRAIN <- createDataPartition(DATA$quality, p = 0.7, list = F)
TRAIN_SET <- DATA[INDEX_TRAIN, ] # base de desenvolvimento: 70%
TEST_SET  <- DATA[-INDEX_TRAIN,] # base de teste: 30%

# Avaliando a distribuicao da variavel resposta
summary(TRAIN_SET$quality);summary(TEST_SET$quality)

# 2) Treino do algoritmo de regressao linear
MDL_FIT <- lm(quality ~ .,data = TRAIN_SET)
MDL_FIT
summary(MDL_FIT)

# 3) Através da EDA Univariada e Bivariada, avaliar oportunidades de melhoria 
# do modelo

##### Remoção de outliers em 18/03

# fixed_acidity -> assimétrica
# volatile acidity -> assimétrica
# citric_acid -> assimétrica
# residual_sugar -> muito concentrado em 0, linearização não vai funcionar 
# chlorides -> assimetria
# free sulfur -> assimetria
# total sulfur -> ok
# density -> ok
# pH -> ok
# sulhpate -> assimetria
# alcohol -> assimetria

# linearizar:
### fixed_acidity
### volatile_acidity
### citric_acidity
### chlorides
### free_sulfur
### sulphate
### alcohol

#### fixed_acidity

par(mfrow = c(2,2))

hist((DATA$fixed_acidity), breaks = 10,
     main = 'Original distribution', cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 
hist(log10(DATA$fixed_acidity), breaks = 10,
     main = expression(log(fixed_acidity)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 
hist(log(DATA$fixed_acidity), breaks = 10,
     main = expression(ln(fixed_acidity)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4')
hist(sqrt(DATA$fixed_acidity), breaks = 10,
     main = expression(sqrt(fixed_acidity)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 

#### volatile_acidity

hist((DATA$volatile_acidity), breaks = 10,
     main = 'Original distribution', cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 
hist(log10(DATA$volatile_acidity), breaks = 10,
     main = expression(log(volatile_acidity)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 
hist(log(DATA$volatile_acidity), breaks = 10,
     main = expression(ln(volatile_acidity)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4')
hist(sqrt(DATA$volatile_acidity), breaks = 10,
     main = expression(sqrt(volatile_acidity)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 

#### citric_acidity -> tnão ficou bom,segue sem linearizar

hist((DATA$citric_acid), breaks = 10,
     main = 'Original distribution', cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 
hist(log10(DATA$citric_acid), breaks = 10,
     main = expression(log(citric_acid)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 
hist(log(DATA$citric_acid), breaks = 10,
     main = expression(ln(citric_acid)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4')
hist(sqrt(DATA$citric_acid), breaks = 10,
     main = expression(sqrt(citric_acid)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 

#### chlorides 

hist((DATA$chlorides), breaks = 10,
     main = 'Original distribution', cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 
hist(log10(DATA$chlorides), breaks = 10,
     main = expression(log(chlorides)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 
hist(log(DATA$chlorides), breaks = 10,
     main = expression(ln(chlorides)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4')
hist(sqrt(DATA$chlorides), breaks = 10,
     main = expression(sqrt(chlorides)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4')

#### free_sulfur_dioxide -> raiz quadrada melhorou

hist((DATA$free_sulfur_dioxide), breaks = 10,
     main = 'Original distribution', cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 
hist(log10(DATA$free_sulfur_dioxide), breaks = 10,
     main = expression(log(free_sulfur_dioxide)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 
hist(log(DATA$free_sulfur_dioxide), breaks = 10,
     main = expression(ln(free_sulfur_dioxide)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4')
hist(sqrt(DATA$free_sulfur_dioxide), breaks = 10,
     main = expression(sqrt(free_sulfur_dioxide)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4')

#### sulphates 

hist((DATA$sulphates), breaks = 10,
     main = 'Original distribution', cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 
hist(log10(DATA$sulphates), breaks = 10,
     main = expression(log(sulphates)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 
hist(log(DATA$sulphates), breaks = 10,
     main = expression(ln(sulphates)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4')
hist(sqrt(DATA$sulphates), breaks = 10,
     main = expression(sqrt(sulphates)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4')

#### alcohol -> insensível a mudanças, segue sem linearizar

hist((DATA$alcohol), breaks = 10,
     main = 'Original distribution', cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 
hist(log10(DATA$alcohol), breaks = 10,
     main = expression(log(alcohol)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4') 
hist(log(DATA$alcohol), breaks = 10,
     main = expression(ln(alcohol)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4')
hist(sqrt(DATA$alcohol), breaks = 10,
     main = expression(sqrt(alcohol)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,4000), col = 'darkorange', border = 'darkorange4')

# aplicar mudanças

DATA %>%
  mutate(
    fixed_acidity = 
      log(fixed_acidity),
    volatile_acidity =
      log(volatile_acidity),
    chlorides = 
      log10(chlorides),
    free_sulfur_dioxide = 
      sqrt(free_sulfur_dioxide),
    sulphates = 
      log(sulphates)
  ) -> DATA_REG

# 4) Reavaliar regressão após linearização
set.seed(123) # garantindo reprodutibilidade da amostra

INDEX_TRAIN_REG <- createDataPartition(DATA_REG$quality, p = 0.7, list = F)
TRAIN_SET_REG <- DATA_REG[INDEX_TRAIN_REG, ] # base de desenvolvimento: 70%
TEST_SET_REG  <- DATA_REG[-INDEX_TRAIN_REG,] # base de teste: 30%

# Avaliando a distribuicao da variavel resposta
summary(TRAIN_SET_REG$quality);summary(TEST_SET_REG$quality)

# 2) Treino do algoritmo de regressao linear
MDL_FIT_REG <- lm(quality ~ .,data = TRAIN_SET_REG)
MDL_FIT_REG
summary(MDL_FIT_REG);summary(MDL_FIT)

# 5) Reavaliar com stepwise

library(MASS)
MDL_FIT_REG.STEP <- stepAIC(MDL_FIT_REG,
                            scope = list(upper = as.formula("quality ~ ."), 
                                         lower = as.formula("quality ~ 1")),
                            direction = 'both', trace = TRUE)

MDL_FIT_REG.STEP
anova(MDL_FIT_REG)

plot(MDL_FIT_REG)


Y_VAL_TRAIN.STEP <- predict(MDL_FIT_REG.STEP) 
Y_VAL_TEST.STEP  <- predict(MDL_FIT_REG.STEP, newdata = TEST_SET_REG)

postResample(pred = Y_VAL_TRAIN, obs = TRAIN_SET_REG$quality)
postResample(pred = Y_VAL_TEST,  obs = TEST_SET_REG$quality)


RESULT_TRAIN <- data.frame(Y_OBS  = TRAIN_SET_REG$quality,
                           Y_PRED = Y_VAL_TRAIN.STEP) %>%
  mutate(ERROR = Y_PRED - Y_OBS)

RESULT_TEST  <- data.frame(Y_OBS  = TEST_SET_REG$quality,
                           Y_PRED = Y_VAL_TEST.STEP) %>%
  mutate(ERROR = Y_PRED - Y_OBS)




layout(matrix(c(1,2,3,4,3,4), nrow = 3, ncol = 2, byrow = TRUE))
par(oma = c(1,1,0,0),  mar = c(4,5,2,1))

hist(RESULT_TRAIN$ERROR, breaks = 25, 
     main = 'Train set', cex.main = 1.3,  cex.lab = 1.3,
     xlab = 'Error', ylab = 'Frequency (#)', cex.axis = 1.2,  
     col = 'darkorange', border = 'brown')

hist(RESULT_TEST$ERROR, breaks = 25,
     main = 'Test set', cex.main = 1.3,  cex.lab = 1.3,
     xlab = 'Error', ylab = 'Frequency (#)', cex.axis = 1.2,  
     col = 'darkorange', border = 'brown')

plot(RESULT_TRAIN$Y_PRED,RESULT_TRAIN$Y_OBS,  cex.lab = 1.3,
     xlab = 'Quality (Predicted)', ylab = 'Quality (Observed)',
     cex.axis = 1.2, pch = 19, cex = 0.5,# ylim = c(0,6000),
     col = 'darkorange')
abline(0, 1, col = 'blue', lwd = 3, lty = "dashed")

plot(RESULT_TEST$Y_PRED, RESULT_TEST$Y_OBS,  cex.lab = 1.3,
     xlab = 'Quality (Predicted)', ylab = 'Quality (Observed)',
     cex.axis = 1.2, cex = 0.5,# ylim = c(0,6000),
     col = 'darkorange')
abline(0,1, col = 'blue', lwd = 3, lty = "dashed")
?abline

# Determinando os coeficientes das variaveis explicativas
summary(MDL_FIT_REG.STEP)



# 6) Avaliar normalização de features e targets

DATA_REG %>%
  mutate(
    fixed_acidity = 
      (fixed_acidity-mean(fixed_acidity))/sd(fixed_acidity),
    volatile_acidity = 
      (volatile_acidity-mean(volatile_acidity))/sd(volatile_acidity),
    citric_acid = 
      (citric_acid-mean(citric_acid))/sd(citric_acid),
    residual_sugar = 
      (residual_sugar-mean(residual_sugar))/sd(residual_sugar),
    chlorides = 
      (chlorides-mean(chlorides))/sd(chlorides),
    free_sulfur_dioxide = 
      (free_sulfur_dioxide-mean(free_sulfur_dioxide))/sd(free_sulfur_dioxide),
    total_sulfur_dioxide = 
      (total_sulfur_dioxide-mean(total_sulfur_dioxide))/sd(total_sulfur_dioxide),
    density = 
      (density-mean(density))/sd(density),
    pH = 
      (pH-mean(pH))/sd(pH),
    sulphates = 
      (sulphates-mean(sulphates))/sd(sulphates),
    alcohol = 
      (alcohol-mean(alcohol))/sd(alcohol),
    quality =
      (quality-mean(quality))/sd(quality)
  ) -> DATA_REG_NORM

set.seed(123) # garantindo reprodutibilidade da amostra

INDEX_TRAIN_REG_NORM <- createDataPartition(DATA_REG_NORM$quality, p = 0.7, list = F)
TRAIN_SET_REG_NORM <- DATA_REG_NORM[INDEX_TRAIN_REG_NORM, ] # base de desenvolvimento: 70%
TEST_SET_REG_NORM  <- DATA_REG_NORM[-INDEX_TRAIN_REG_NORM,] # base de teste: 30%

# Avaliando a distribuicao da variavel resposta
summary(TRAIN_SET_REG_NORM$quality);summary(TEST_SET_REG_NORM$quality)

# 7) Treino do algoritmo de regressao linear após linearização e normalização
MDL_FIT_REG_NORM <- lm(quality ~ .,data = TRAIN_SET_REG_NORM)
MDL_FIT_REG_NORM
summary(MDL_FIT_REG_NORM);summary(MDL_FIT_REG.STEP)


# 8) Reavaliar com stepwise

MDL_FIT_REG_NORM.STEP <- stepAIC(MDL_FIT_REG_NORM,
                                 scope = list(upper = as.formula("quality ~ ."), 
                                              lower = as.formula("quality ~ 1")),
                                 direction = 'both', trace = TRUE)

MDL_FIT_REG_NORM.STEP

# Determinando os coeficientes das variaveis explicativas
summary(MDL_FIT_REG_NORM.STEP)


Y_REG_TRAIN_1 <- predict(MDL_FIT)
Y_REG_TEST_1 <- predict(MDL_FIT, newdata = TEST_SET)

Y_REG_TRAIN_2 <- predict(MDL_FIT_REG)
Y_REG_TEST_2 <- predict(MDL_FIT_REG, newdata = TEST_SET_REG)

Y_REG_TRAIN_3 <- predict(MDL_FIT_REG.STEP)
Y_REG_TEST_3 <- predict(MDL_FIT_REG.STEP, newdata = TEST_SET_REG)

Y_REG_TRAIN_4 <- predict(MDL_FIT_REG_NORM)
Y_REG_TEST_4 <- predict(MDL_FIT_REG_NORM, newdata = TEST_SET_REG_NORM)

Y_REG_TRAIN_5 <- predict(MDL_FIT_REG_NORM.STEP)
Y_REG_TEST_5 <- predict(MDL_FIT_REG_NORM.STEP, newdata = TEST_SET_REG_NORM)


METRIC_REGTRAIN_1 <- postResample(pred = Y_REG_TRAIN_1, obs = TRAIN_SET$quality)
METRIC_REGTEST_1 <- postResample(pred = Y_REG_TEST_1,  obs = TEST_SET$quality)

METRIC_REGTRAIN_2 <- postResample(pred = Y_REG_TRAIN_2, obs = TRAIN_SET_REG$quality)
METRIC_REGTEST_2 <- postResample(pred = Y_REG_TEST_2,  obs = TEST_SET_REG$quality)

METRIC_REGTRAIN_3 <- postResample(pred = Y_REG_TRAIN_3, obs = TRAIN_SET_REG$quality)
METRIC_REGTEST_3 <- postResample(pred = Y_REG_TEST_3,  obs = TEST_SET_REG$quality)

METRIC_REGTRAIN_4 <- postResample(pred = Y_REG_TRAIN_4, obs = TRAIN_SET_REG_NORM$quality)
METRIC_REGTEST_4 <- postResample(pred = Y_REG_TEST_4,  obs = TEST_SET_REG_NORM$quality)

METRIC_REGTRAIN_5 <- postResample(pred = Y_REG_TRAIN_5, obs = TRAIN_SET_REG_NORM$quality)
METRIC_REGTEST_5 <- postResample(pred = Y_REG_TEST_5,  obs = TEST_SET_REG_NORM$quality)


reg_tbl[3,] <- list("Regressão - Aplicação Direta",
                    unname(METRIC_REGTRAIN_1[1]),
                    unname(METRIC_REGTEST_1[1]),
                    unname(METRIC_REGTRAIN_1[2]),
                    unname(METRIC_REGTEST_1[2])
)

reg_tbl[4,] <- list("Regressão com Linearização",
                    unname(METRIC_REGTRAIN_2[1]),
                    unname(METRIC_REGTEST_2[1]),
                    unname(METRIC_REGTRAIN_2[2]),
                    unname(METRIC_REGTEST_2[2])
)

reg_tbl[5,] <- list("Regressão com Linearização e Stepwise",
                    unname(METRIC_REGTRAIN_3[1]),
                    unname(METRIC_REGTEST_3[1]),
                    unname(METRIC_REGTRAIN_3[2]),
                    unname(METRIC_REGTEST_3[2])
)

reg_tbl[6,] <- list("Regressão com Linearização e Normalização",
                    unname(METRIC_REGTRAIN_4[1]),
                    unname(METRIC_REGTEST_4[1]),
                    unname(METRIC_REGTRAIN_4[2]),
                    unname(METRIC_REGTEST_4[2])
)

reg_tbl[7,] <- list("Regressão com Linearização, Normalização e Stepwise",
                    unname(METRIC_REGTRAIN_5[1]),
                    unname(METRIC_REGTEST_5[1]),
                    unname(METRIC_REGTRAIN_5[2]),
                    unname(METRIC_REGTEST_5[2])
)
############################FIM REGRESSÃO LINEAR
#--------------------------------------------------------------------------------#
############################ÁRVORE DE DECISÃO PARA REGRESSÃO#######################

# 1) Divisao da base de modelagem em treino e teste com amostragem

set.seed(123) # garantindo reprodutibilidade da amostra

INDEX_TRAIN <- createDataPartition(DATA$quality, p = 0.7, list = F)
TRAIN_SET <- DATA[INDEX_TRAIN, ] # base de desenvolvimento: 70%
TEST_SET  <- DATA[-INDEX_TRAIN,] # base de teste: 30%

# Avaliando a distribuicao da variavel resposta
summary(TRAIN_SET$quality);summary(TEST_SET$quality)

#--------------------------------------------------------------------------------#
# 2) Treino do algoritmo de arvore de regresssao
library(rpart)

# aqui comecamos a arvore o mais completa possivel

MDL_FIT <- rpart(quality ~.,
                 data = TRAIN_SET,
                 method = 'anova',
                 control = rpart.control(minbucket = 15, cp = -1))

MDL_FIT
summary(MDL_FIT)


printcp(MDL_FIT)
plotcp(MDL_FIT)

MDL_FIT.PRUNE <- prune(MDL_FIT, cp = 0.0017)

printcp(MDL_FIT.PRUNE)
plotcp(MDL_FIT.PRUNE)

# saida da arrvore
MDL_FIT.PRUNE
summary(MDL_FIT.PRUNE)

# 3) Realizando as predicoes

# Valor de AMOUNT pela arvore regressao com maior desenvolvimento
Y_VAL_TRAIN <- predict(MDL_FIT.PRUNE) 
Y_VAL_TEST  <- predict(MDL_FIT.PRUNE, newdata = TEST_SET)

postResample(pred = Y_VAL_TRAIN, obs = TRAIN_SET$quality)
postResample(pred = Y_VAL_TEST,  obs = TEST_SET$quality)
MDL_FINAL <- MDL_FIT.PRUNE


round(MDL_FINAL$variable.importance, 3)


RESULT_TRAIN <- data.frame(Y_OBS  = TRAIN_SET$quality,
                           Y_PRED = Y_VAL_TRAIN) %>%
  mutate(ERROR = Y_PRED - Y_OBS)

RESULT_TEST  <- data.frame(Y_OBS  = TEST_SET$quality,
                           Y_PRED = Y_VAL_TEST) %>%
  mutate(ERROR = Y_PRED - Y_OBS)


layout(matrix(c(1,2,3,4,3,4), nrow = 3, ncol = 2, byrow = TRUE))
par(oma = c(1,1,0,0),  mar = c(4,5,2,1))

hist(RESULT_TRAIN$ERROR, breaks = 25,
     main = 'Train set', cex.main = 1.3,  cex.lab = 1.3,
     xlab = 'Error', ylab = 'Frequency (#)', cex.axis = 1.2,  
     col = 'darkorange', border = 'brown')

hist(RESULT_TEST$ERROR, breaks = 25,
     main = 'Test set', cex.main = 1.3,  cex.lab = 1.3,
     xlab = 'Error', ylab = 'Frequency (#)', cex.axis = 1.2,  
     col = 'darkorange', border = 'brown')


plot(RESULT_TRAIN$Y_PRED,RESULT_TRAIN$Y_OBS,  cex.lab = 1.3,
     xlab = 'Quality (Predicted)', ylab = 'Quality (Observed)',
     cex.axis = 1.2, pch = 19, cex = 0.5,# ylim = c(0,6000),
     col = 'darkorange')
abline(0, 1, col = 'blue', lwd = 3, lty = "dashed")

plot(RESULT_TEST$Y_PRED, RESULT_TEST$Y_OBS,  cex.lab = 1.3, 
     xlab = 'Quality (Predicted)', ylab = 'Quality (Observed)',
     cex.axis = 1.2, pch = 19, cex = 0.5,# ylim = c(0,6000),
     col = 'darkorange')
abline(0, 1, col = 'blue', lwd = 3, lty = "dashed")



reg_tbl[8,] <- list("Árvore de Regressão",
                    unname(METRICS_TREETRAIN[1]),
                    unname(METRICS_TREETEST[1]),
                    unname(METRICS_TREETRAIN[2]),
                    unname(METRICS_TREETEST[2])
)

MDL_FINAL <- MDL_FIT.PRUNE

#--------------------------------------------------------------------------------#
# 6) Importancia das variaveis (Modelo final)

# o algoritmo de arvore tambem possui uma saida com a importancia das variaveis
round(MDL_FINAL$variable.importance, 3)

############################FIM ÁRVORE DE DECISÃO
#--------------------------------------------------------------------------------#






