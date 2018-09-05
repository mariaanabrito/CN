library("readr")
library("neuralnet")
library("caTools")
library("hydroGOF")

# Diretoria onde se encontra o ficheiro
setwd("C:\\path")

useDataset <- function(choice) {
  
  print(choice)

  # Latitude Norte e Longitude Este -> sinal positivo
  # Latitude Sul e Longitude Oeste -> sinal negativo

  # Campos de Jordão: Lat -> 22º45'S ; Long -> 45º36'W
  # Cruzeiro do Sul: Lat -> 7º37'S ; Long -> 72º40'W
  # Picos: Lat -> 7º04'S ; Long -> 41º28'W

  if(choice == "Campos de Jordão") {
    ds <- read.csv("Campos de Jordão.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
    lat <- -(22 + (45/60) + (0/3600))
    ds$NoonAngle <- 90 - abs(-23.44 * cos(2*pi / 365.25) * (ds$SolarNoonAngle + 8.5) - lat)
  }
  if(choice == "Cruzeiro do Sul") {
    ds <- read.csv("Cruzeiro do Sul.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
    lat <- -(7 + (37/60) + (0/3600))
    ds$NoonAngle <- 90 - abs(-23.44 * cos(2*pi / 365.25) * (ds$SolarNoonAngle + 8.5) - lat)
  }
  if(choice == "Picos") {
    ds <- read.csv("Picos.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
    lat <- -(7 + (4/60) + (0/3600))
    ds$NoonAngle <- 90 - abs(-23.44 * cos(2*pi / 365.25) * (ds$SolarNoonAngle + 8.5) - lat)
  }
  return(ds)
}

# Remover rows com missing values
removeAllMissing <- function(dataset) {
  
  aux <- ds
  
  if(dataset == "Campos de Jordão") {
    aux <- aux[!(aux$Date == 0          | is.na(aux$Precipitation) |
                 is.na(aux$MaxTemp)     | is.na(aux$MinTemp)       |
                 is.na(aux$Evaporation) | is.na(aux$AvgTemp)       |
                 is.na(aux$AvgHumidity) | is.na(aux$SolarNoonAngle)),]
  }
  else {
    aux <- aux[!(aux$Date == 0          | is.na(aux$Precipitation) |
                 is.na(aux$MaxTemp)     | is.na(aux$MinTemp)       |
                 is.na(aux$Insolation)  | is.na(aux$Evaporation)   |
                 is.na(aux$AvgTemp)     | is.na(aux$AvgHumidity)   |
                 is.na(aux$WindSpeed)   | is.na(aux$SolarNoonAngle)),]
  }
  
  print("DONE!")
  return(aux)
}

# Converter valores de char para num
convertValues <- function(dataset) {
  if(dataset != "Campos de Jordão") {
    ds <- transform(ds, Insolation = as.numeric(Insolation),
                         WindSpeed = as.numeric(WindSpeed))
  }
  ds <- transform(ds, Precipitation = as.numeric(Precipitation),
                       MaxTemp = as.numeric(MaxTemp),
                       MinTemp = as.numeric(MinTemp),
                       Evaporation = as.numeric(Evaporation),
                       AvgTemp = as.numeric(AvgTemp),
                       AvgHumidity = as.numeric(AvgHumidity))
  
  print("DONE!")
  return(ds)
}


# Seleção dos melhores atributos
selectAtr <- function(dataset) {
  
  aux <- removeAllMissing(dataset)
  
  correlacoes <- data.frame("Precipitation" = numeric(1), "MaxTemp" = numeric(1),
                             "MinTemp" = numeric(1), "Evaporation" = numeric(1),
                             "AvgTemp" = numeric(1), "AvgHumidity" = numeric(1),
                             "NoonAngle" = numeric(1))
  
  if(dataset != "Campos de Jordão") {
    correlacoes$Insolation <- 0
    correlacoes$WindSpeed <- 0
  }
  
  # Média e Desvio Padrão do atributo 'MinTemp'
  mediaMinTemp <- mean(aux$MinTemp)
  desvioPadraoMinTemp <- sqrt(var(aux$MinTemp))
  
  # Média e Desvio Padrão do atributo 'MaxTemp'
  mediaMaxTemp <- mean(aux$MaxTemp)
  desvioPadraoMaxTemp <- sqrt(var(aux$MaxTemp))
  
  if(dataset != "Campos de Jordão") {
    
    # Média e Desvio Padrão do atributo 'Insolation'
    mediaIns <- mean(aux$Insolation)
    desvioPadraoIns <- sqrt(var(aux$Insolation))
    
    aux$MinIns <- aux$MinTemp * aux$Insolation
    mediaMinIns <- mean(aux$MinIns)
    aux$MaxIns <- aux$MaxTemp * aux$Insolation
    mediaMaxIns <- mean(aux$MaxIns)

    # Média e Desvio Padrão do atributo 'WindSpeed'
    mediaWS <- mean(aux$WindSpeed)
    desvioPadraoWS <- sqrt(var(aux$WindSpeed))
    
    aux$MinWS <- aux$MinTemp * aux$WindSpeed
    mediaMinWS <- mean(aux$MinWS)
    aux$MaxWS <- aux$MaxTemp * aux$WindSpeed
    mediaMaxWS <- mean(aux$MaxWS)
  }
  
  # Média e Desvio Padrão do atributo 'Precipiation'
  mediaPrec <- mean(aux$Precipitation)
  desvioPadraoPrec <- sqrt(var(aux$Precipitation))
  aux$MinPrec <- aux$MinTemp * aux$Precipitation
  mediaMinPrec <- mean(aux$MinPrec)
  aux$MaxPrec <- aux$MaxTemp * aux$Precipitation
  mediaMaxPrec <- mean(aux$MaxPrec)
  
  # Média e Desvio Padrão do atributo 'Evaporation'
  mediaEvap <- mean(aux$Evaporation)
  desvioPadraoEvap <- sqrt(var(aux$Evaporation))
  aux$MinEvap <- aux$MinTemp * aux$Evaporation
  mediaMinEvap <- mean(aux$MinEvap)
  aux$MaxEvap <- aux$MaxTemp * aux$Evaporation
  mediaMaxEvap <- mean(aux$MaxEvap)
  
  # Média e Desvio Padrão do atributo 'AvgTemp'
  mediaAvgT <- mean(aux$AvgTemp)
  desvioPadraoAvgT <- sqrt(var(aux$AvgTemp))
  aux$MinAvgT <- aux$MinTemp * aux$AvgTemp
  mediaMinAvgT <- mean(aux$MinAvgT)
  aux$MaxAvgT <- aux$MaxTemp * aux$AvgTemp
  mediaMaxAvgT <- mean(aux$MaxAvgT)
  
  # Média e Desvio Padrão do atributo 'AvgHumidity'
  mediaAvgH <- mean(aux$AvgHumidity)
  desvioPadraoAvgH <- sqrt(var(aux$AvgHumidity))
  aux$MinAvgH <- aux$MinTemp * aux$AvgHumidity
  mediaMinAvgH <- mean(aux$MinAvgH)
  aux$MaxAvgH <- aux$MaxTemp * aux$AvgHumidity
  mediaMaxAvgH <- mean(aux$MaxAvgH)
  
  # Média e Desvio Padrão do atributo 'NoonAngle'
  mediaNA <- mean(aux$NoonAngle)
  desvioPadraoNA <- sqrt(var(aux$NoonAngle))
  aux$MinNA <- aux$MinTemp * aux$NoonAngle
  mediaMinNA <- mean(aux$MinNA)
  aux$MaxNA <- aux$MaxTemp * aux$NoonAngle
  mediaMaxNA <- mean(aux$MaxNA)
  
  if(dataset != "Campos de Jordão") {
    c_MinIns <- (mediaMinIns - mediaMinTemp * mediaIns)/(desvioPadraoMinTemp * desvioPadraoIns)
    c_MinWS <- (mediaMinWS - mediaMinTemp * mediaWS)/(desvioPadraoMinTemp * desvioPadraoWS)
    c_MaxIns <- (mediaMaxIns - mediaMaxTemp * mediaIns)/(desvioPadraoMaxTemp * desvioPadraoIns)
    c_MaxWS <- (mediaMaxWS - mediaMaxTemp * mediaWS)/(desvioPadraoMaxTemp * desvioPadraoWS)
  }
  
  c_MinPrec <- (mediaMinPrec - mediaMinTemp * mediaPrec)/(desvioPadraoMinTemp * desvioPadraoPrec)
  c_MinEvap <- (mediaMinEvap - mediaMinTemp * mediaEvap)/(desvioPadraoMinTemp * desvioPadraoEvap)
  c_MinAvgT <- (mediaMinAvgT - mediaMinTemp * mediaAvgT)/(desvioPadraoMinTemp * desvioPadraoAvgT)
  c_MinAvgH <- (mediaMinAvgH - mediaMinTemp * mediaAvgH)/(desvioPadraoMinTemp * desvioPadraoAvgH)
  c_MinNA <- (mediaMinNA - mediaMinTemp * mediaNA)/(desvioPadraoMinTemp * desvioPadraoNA)
  c_MaxPrec <- (mediaMaxPrec - mediaMaxTemp * mediaPrec)/(desvioPadraoMaxTemp * desvioPadraoPrec)
  c_MaxEvap <- (mediaMaxEvap - mediaMaxTemp * mediaEvap)/(desvioPadraoMaxTemp * desvioPadraoEvap)
  c_MaxAvgT <- (mediaMaxAvgT - mediaMaxTemp * mediaAvgT)/(desvioPadraoMaxTemp * desvioPadraoAvgT)
  c_MaxAvgH <- (mediaMaxAvgH - mediaMaxTemp * mediaAvgH)/(desvioPadraoMaxTemp * desvioPadraoAvgH)
  c_MaxNA <- (mediaMaxNA - mediaMaxTemp * mediaNA)/(desvioPadraoMaxTemp * desvioPadraoNA)
  
  if(dataset == "Campos de Jordão") {
    correlacoes <- rbind(correlacoes, data.frame(Precipitation = as.numeric(c_MaxPrec), MaxTemp = 1,
                                                  MinTemp = 0, Evaporation = as.numeric(c_MaxEvap),
                                                  AvgTemp = as.numeric(c_MaxAvgT), AvgHumidity = as.numeric(c_MaxAvgH),
                                                  NoonAngle = as.numeric(c_MaxNA)))
    correlacoes <- rbind(correlacoes, data.frame(Precipitation = as.numeric(c_MinPrec), MaxTemp = 0,
                                                  MinTemp = 1, Evaporation = as.numeric(c_MinEvap),
                                                  AvgTemp = as.numeric(c_MinAvgT), AvgHumidity = as.numeric(c_MinAvgH),
                                                  NoonAngle = as.numeric(c_MinNA)))
  }
  else {
    correlacoes <- rbind(correlacoes, data.frame(Precipitation = as.numeric(c_MaxPrec), MaxTemp = 1,
                                                  MinTemp = 0, Evaporation = as.numeric(c_MaxEvap),
                                                  AvgTemp = as.numeric(c_MaxAvgT), AvgHumidity = as.numeric(c_MaxAvgH),
                                                  Insolation = as.numeric(c_MaxIns), WindSpeed = as.numeric(c_MaxWS),
                                                  NoonAngle = as.numeric(c_MaxNA)))
    correlacoes <- rbind(correlacoes, data.frame(Precipitation = as.numeric(c_MinPrec), MaxTemp = 0,
                                                  MinTemp = 1, Evaporation = as.numeric(c_MinEvap),
                                                  AvgTemp = as.numeric(c_MinAvgT), AvgHumidity = as.numeric(c_MinAvgH),
                                                  Insolation = as.numeric(c_MinIns), WindSpeed = as.numeric(c_MinWS),
                                                  NoonAngle = as.numeric(c_MinNA)))
  }
  correlacoes <- correlacoes[-1,]
  
  print("DONE!")
  return(correlacoes)
}

# Listar atributos que apresentam boa correlação com os outputs pretendidos
listAtr <- function() {
  
  listaAtr <- NULL
  
  for(i in 1:length(correlacoes)) {
    if(names(correlacoes[i]) != "MinTemp" && names(correlacoes[i]) != "MaxTemp") {
      if(correlacoes[1,i] < 0 || correlacoes[2,i] < 0) {
        if(correlacoes[1,i] < 0) {
          if(correlacoes[2,i] > abs(correlacoes[1,i])) {
            listaAtr[length(listaAtr) + 1] <- names(correlacoes[i])
          }
        }
        else {
          if(correlacoes[1,i] > abs(correlacoes[2,i])) {
            listaAtr[length(listaAtr) + 1] <- names(correlacoes[i])
          }
        }
      }
      else {
        listaAtr[length(listaAtr) + 1] <- names(correlacoes[i])
      }
    }
  }
  
print("DONE!")
return(listaAtr)
  
}

# Remover rows com missing values nos atributos importantes para os outputs
removeMissing <- function() {
  
  listaAtr <- c(listaAtr, "MaxTemp", "MinTemp")
  for(i in 1:length(listaAtr)) {
    
    x <- NULL
    
    for(j in 1:nrow(ds)) {
      if(is.na(ds[j,listaAtr[i]])) {
        x <- c(x, FALSE)
      }
      else {
        x <- c(x, TRUE)
      }
    }
    ds <- ds[x,]
  }
  print("DONE!")
  return(ds)
}

# Construir o dataframe final
constructDF <- function(dataset) {
  
  listaAllAtr <- names(ds)
  df <- data.frame(matrix(ncol = length(listaAllAtr) * 4, nrow = 0))
  x <- NULL
  
  for(i in 1:length(listaAllAtr)) {
    atr <- listaAllAtr[i]
    atr1 <- paste(listaAllAtr[i], "_1", sep="")
    atr2 <- paste(listaAllAtr[i], "_2", sep="")
    atr3 <- paste(listaAllAtr[i], "_3", sep="")
    x <- c(x, atr, atr1, atr2, atr3)
  }
  
  colnames(df) <- x

  for(i in 4:nrow(ds)) {
    
    if(dataset != "Campos de Jordão") {
      insol     <- ds[i, "Insolation"]
      insol_1   <- ds[i - 1, "Insolation"]
      insol_2   <- ds[i - 2, "Insolation"]
      insol_3   <- ds[i - 3, "Insolation"]
      windS     <- ds[i, "WindSpeed"]
      windS_1   <- ds[i - 1, "WindSpeed"]
      windS_2   <- ds[i - 2, "WindSpeed"]
      windS_3   <- ds[i - 3, "WindSpeed"]
    }
    
    day       <- ds[i, "Date"]
    day_1     <- ds[i - 1, "Date"]
    day_2     <- ds[i - 2, "Date"]
    day_3     <- ds[i - 3, "Date"]
    prec      <- ds[i, "Precipitation"]
    prec_1    <- ds[i - 1, "Precipitation"]
    prec_2    <- ds[i - 2, "Precipitation"]
    prec_3    <- ds[i - 3, "Precipitation"]
    maxTemp   <- ds[i, "MaxTemp"]
    maxTemp_1 <- ds[i - 1, "MaxTemp"]
    maxTemp_2 <- ds[i - 2, "MaxTemp"]
    maxTemp_3 <- ds[i - 3, "MaxTemp"]
    minTemp   <- ds[i, "MinTemp"]
    minTemp_1 <- ds[i - 1, "MinTemp"]
    minTemp_2 <- ds[i - 2, "MinTemp"]
    minTemp_3 <- ds[i - 3, "MinTemp"]
    evap      <- ds[i, "Evaporation"]
    evap_1    <- ds[i - 1, "Evaporation"]
    evap_2    <- ds[i - 2, "Evaporation"]
    evap_3    <- ds[i - 3, "Evaporation"]
    avgTemp   <- ds[i, "AvgTemp"]
    avgTemp_1 <- ds[i - 1, "AvgTemp"]
    avgTemp_2 <- ds[i - 2, "AvgTemp"]
    avgTemp_3 <- ds[i - 3, "AvgTemp"]
    avgHum    <- ds[i, "AvgHumidity"]
    avgHum_1  <- ds[i - 1, "AvgHumidity"]
    avgHum_2  <- ds[i - 2, "AvgHumidity"]
    avgHum_3  <- ds[i - 3, "AvgHumidity"]
    sna       <- ds[i, "SolarNoonAngle"]
    sna_1     <- ds[i - 1, "SolarNoonAngle"]
    sna_2     <- ds[i - 2, "SolarNoonAngle"]
    sna_3     <- ds[i - 3, "SolarNoonAngle"]
    na        <- ds[i, "NoonAngle"]
    na_1      <- ds[i - 1, "NoonAngle"]
    na_2      <- ds[i - 2, "NoonAngle"]
    na_3      <- ds[i - 3, "NoonAngle"]

    if(day == day_1 + 1 && day == day_2 + 2 && day == day_3 + 3) {
      if(dataset != "Campos de Jordão") {
        df <- rbind(df, data.frame(Date = day, Date_1 = day_1, Date_2 = day_2, Date_3 = day_3, 
                                    Precipitation = as.numeric(prec), Precipitation_1 =  as.numeric(prec_1), Precipitation_2 =  as.numeric(prec_2), Precipitation_3 =  as.numeric(prec_3),
                                    MaxTemp = as.numeric(maxTemp), MaxTemp_1 = as.numeric(maxTemp_1), MaxTemp_2 = as.numeric(maxTemp_2), MaxTemp_3 = as.numeric(maxTemp_3),
                                    MinTemp = as.numeric(minTemp), MinTemp_1 = as.numeric(minTemp_1), MinTemp_2 = as.numeric(minTemp_2), MinTemp_3 = as.numeric(minTemp_3),
                                    Insolation = as.numeric(insol), Insolation_1 = as.numeric(insol_1), Insolation_2 = as.numeric(insol_2), Insolation_3 = as.numeric(insol_3),
                                    Evaporation = as.numeric(evap), Evaporation_1 = as.numeric(evap_1), Evaporation_2 = as.numeric(evap_2), Evaporation_3 = as.numeric(evap_3),                                 
                                    AvgTemp = as.numeric(avgTemp), AvgTemp_1 = as.numeric(avgTemp_1), AvgTemp_2 = as.numeric(avgTemp_2), AvgTemp_3 = as.numeric(avgTemp_3),
                                    AvgHumidity = as.numeric(avgHum), AvgHumidity_1 = as.numeric(avgHum_1), AvgHumidity_2 = as.numeric(avgHum_2), AvgHumidity_3 = as.numeric(avgHum_3),
                                    WindSpeed = windS, WindSpeed_1 = windS_1, WindSpeed_2 = windS_2, WindSpeed_3 = windS_3,
                                    SolarNoonAngle = sna, SolarNoonAngle_1 = sna_1, SolarNoonAngle_2 = sna_2, SolarNoonAngle_3 = sna_3,
                                    NoonAngle = na, NoonAngle_1 = na_1, NoonAngle_2 = na_2, NoonAngle_3 = na_3))
      }
      else {
        df <- rbind(df, data.frame(Date = day, Date_1 = day_1, Date_2 = day_2, Date_3 = day_3, 
                                    Precipitation = as.numeric(prec), Precipitation_1 =  as.numeric(prec_1), Precipitation_2 =  as.numeric(prec_2), Precipitation_3 =  as.numeric(prec_3),
                                    MaxTemp = as.numeric(maxTemp), MaxTemp_1 = as.numeric(maxTemp_1), MaxTemp_2 = as.numeric(maxTemp_2), MaxTemp_3 = as.numeric(maxTemp_3),
                                    MinTemp = as.numeric(minTemp), MinTemp_1 = as.numeric(minTemp_1), MinTemp_2 = as.numeric(minTemp_2), MinTemp_3 = as.numeric(minTemp_3),
                                    Evaporation = as.numeric(evap), Evaporation_1 = as.numeric(evap_1), Evaporation_2 = as.numeric(evap_2), Evaporation_3 = as.numeric(evap_3),                                 
                                    AvgTemp = as.numeric(avgTemp), AvgTemp_1 = as.numeric(avgTemp_1), AvgTemp_2 = as.numeric(avgTemp_2), AvgTemp_3 = as.numeric(avgTemp_3),
                                    AvgHumidity = as.numeric(avgHum), AvgHumidity_1 = as.numeric(avgHum_1), AvgHumidity_2 = as.numeric(avgHum_2), AvgHumidity_3 = as.numeric(avgHum_3),
                                    SolarNoonAngle = sna, SolarNoonAngle_1 = sna_1, SolarNoonAngle_2 = sna_2, SolarNoonAngle_3 = sna_3,
                                    NoonAngle = na, NoonAngle_1 = na_1, NoonAngle_2 = na_2, NoonAngle_3 = na_3))
      }
    }
  }
  
  aux <- NULL
  i <- 1
  while(i < length(df)) {
    print(names(df[i]))
    print(names(df[i]) %in% listaAtr)
    if(names(df[i]) == "MinTemp" | names(df[i]) == "MaxTemp") {
      aux <- c(aux, TRUE, TRUE, TRUE, TRUE)
    }
    else {
      if(names(df[i]) %in% listaAtr) {
        aux <- c(aux, FALSE, TRUE, TRUE, TRUE)
      }
      else {
        aux <- c(aux, FALSE, FALSE, FALSE, FALSE)
      }
    }
    i <- i + 4
  }
  
  df <- df[,aux]
  print("DONE!")
  return(df)
}

writeFinalCSV <- function(dataset) {
  name <- paste(dataset, " Final.csv", sep="")
  write.csv(df, file = name, row.names = FALSE)
  print(paste("Gerado ficheiro: ", name, sep = ''))
}

normalizeData <- function() {
  max <- apply(df,2,max)
  min <- apply(df,2,min)
  df <- as.data.frame(scale(df, center = min, scale = max-min))
  print("DONE!")
  return(df)
}

trainNeuralNet <- function(algorithm = "rprop+", hidden = c(1), threshold = 0.01) {

  # Gerador de números aleatórios
  set.seed(7896129)

  # Separa o dataset em dois sets (70%/30%)
  split <- sample.split(df, SplitRatio = 0.7)

  # Cria o subset de treino com o primeiro set do split
  train <- subset(df, split == T)

  # O segundo set do split constitui o subset de teste
  testAll <<- subset(df, split == F)
  
  #aux_input <- -c("Date", "MaxTemp", "MinTemp")
  aux_output <- c("Date", "MaxTemp", "MinTemp")
  #i <- 1
  #while(i < length(df)) {
  #  aux_input <- c(aux_input, FALSE, TRUE, TRUE, TRUE)
  #  aux_output <- c(aux_output, TRUE, FALSE, FALSE, FALSE)
  #  i <- i + 4
  #}
  
  test <<- subset(testAll, select = setdiff(names(df),aux_output))
  
  # Cabeçalhos do dataset
  feats <- names(df)
  
  # Junta os cabeçalhos com '+' tipo header1+header5+...
  #res <- paste(feats[9], '+', feats[13])
  res <- paste(intersect(feats, aux_output), collapse = '+')
  #f <- paste(feats[-c(1,5,9,13,17,21,25,29,33,37:41)], collapse = '+')
  f <- paste(setdiff(feats, aux_output), collapse = '+')

  # Valor de f (header1+header2+...) torna-se header5~header1+header2+...
  f <- paste(res, '~', f)

  # Converte f numa fórmula
  f <- as.formula(f)

  # Define a rede neuronal (formula, dataset, layers)
  nn <- neuralnet(f, train, algorithm = algorithm, hidden = hidden, stepmax = "1e+05", threshold = threshold, lifesign = "full")
  #nn <- neuralnet(f, train, algorithm = 'backprop', learningrate = 0.1, hidden = c(10,10), threshold = 0.1, lifesign = 'full', linear.output = FALSE)
  
  print("Rede Treinada!")
  return(nn)
    
}

testNeuralNet <- function(nn) {
  # Aplica a rede treinada no dataset de teste
  nn$results <- compute(nn, test)

  resultsMin <- data.frame(actual = testAll$MinTemp, prediction = nn$results$net.result)
  resultsMax <- data.frame(actual = testAll$MaxTemp, prediction = nn$results$net.result)
  nn$prediction1 <- resultsMin$prediction.2
  nn$prediction2 <- resultsMax$prediction.1
  print("Min Temp:")
  rmseTest <<- data.frame(atual = resultsMin$actual, valor = nn$prediction1)
  nn$rmse1 <- rmse(c(resultsMin$actual), c(nn$prediction1))
  print(nn$rmse1)
  print("Max Temp:")
  nn$rmse2 <- rmse(c(resultsMax$actual), c(nn$prediction2))
  print(nn$rmse2)
  mse.nn.min <- sum((testAll$MinTemp - nn$results$net.result)^2 /nrow(testAll))
}
#print(predicted.nn.values$net.result)
#summary(predicted.nn.values$net.result)

# Datasets disponíveis
datasetCruzeiro <- "Cruzeiro do Sul"
datasetCampos <- "Campos de Jordão"
datasetPicos <- "Picos"

dataset <- datasetPicos

ds <- useDataset(dataset)
ds <- convertValues(dataset)
correlacoes <- selectAtr(dataset)
listaAtr <- listAtr()
ds <- removeMissing()
df <- constructDF(dataset)
writeFinalCSV(dataset)
df <- normalizeData()

# Testes de performance
algoritmo_1 = "backprop"
algoritmo_2 = "rprop-"

hidden_1 = c(10)
hidden_2 = c(10,10)
hidden_3 = c(10,10,10)

threshold_1 = 0.07
threshold_2 = 0.03


nNet <- trainNeuralNet(hidden = hidden_2, threshold = threshold_1)
testNeuralNet(nNet)