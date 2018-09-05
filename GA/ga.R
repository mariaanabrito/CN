  library("GA")
  
  setwd("C:/Users/Jo/Desktop/Instances_Tuzun_LRP/")
  
  # Avoid rounding coords
  options(digits = 9)
  
  # Read File
  con <- file("coordP111112.dat", "r", blocking = FALSE)
  input <- readLines(con)
  close(con)
  
  dataset <- NULL
  
  # Ambient Variables
  depotsXCoords <- NULL
  depotsYCoords <- NULL
  clientsXCoords <- NULL
  clientsYCoords <- NULL
  cliDemand <- NULL
  depotsCost <- NULL
  depotsCap <- NULL
  
  
  # Remove newlines
  
  removeNewlines <- function(dataset) {
    for(i in 1:length(input)) {
      if(input[i] != "") {
        dataset <- append(dataset, gsub("\t", " ", input[i]), after = length(dataset))
      }
    }
    return(dataset)
  }
  
  # Depots Coords Arrays
  
  getDepotsCoords <- function() {
    for(i in 1:numDepots) {
      coords <- strsplit(dataset[i + 2], " ")
      coordX <- coords[[1]][1]
      coordY <- coords[[1]][2]
      depotsXCoords <<- append(depotsXCoords, as.double(coordX))
      depotsYCoords <<- append(depotsYCoords, as.double(coordY))
    }
  }
  
  # Clients Coords Arrays
  
  getClientsCoords <- function() {
    for(i in 1:numClients) {
      coords <- strsplit(dataset[i + 2 + numDepots], " ")
      coordX <- coords[[1]][1]
      coordY <- coords[[1]][2]
      clientsXCoords <<- append(clientsXCoords, as.double(coordX))
      clientsYCoords <<- append(clientsYCoords, as.double(coordY))
    }
  }
  
  
  # Function to get clients demands array
  
  getClientsDemand <- function(cliDemand) {
    pos <- 3 + 2 * numDepots + numClients
    for(i in 1:numClients) {
      cap <- as.double(dataset[i + pos])
      cliDemand <- append(cliDemand, cap)
    }
    return(cliDemand)
  }
  
  # Function to get depots capacities array
  
  getDepotsCap <- function(depotsCap) {
    for(i in 1:numDepots) {
      depotsCap <- append(depotsCap, dataset[i + 3 + numDepots + numClients])
    }
    return(depotsCap)
  }
  
  
  getDepotsCost <- function(depotsCost) {
    pos <- 3 + 2 * numDepots + 2 * numClients
    for(i in 1:numDepots) {
      depotsCost <- append(depotsCost, as.double(dataset[pos + i]))
    }
    return(depotsCost)
  }
  
  distanciaEntrePontos <- function(xi, yi, xf, yf) {
    dist <- sqrt((xi-xf)^2 + (yi-yf)^2)
    return(dist)
  }
  
  
  #preenchimento da distmatrix
  
  getDistMatrix <- function(distMatrix) {
    for(i in 1:length(clientsXCoords)){
      distMatrix[i,i] <- 1000
      for(j in 1:length(clientsXCoords)){
        dis <- distanciaEntrePontos(clientsXCoords[i], clientsYCoords[i], clientsXCoords[j], clientsYCoords[j])
        distMatrix[i,j] <- dis
      }
    }
    for(i in 1:length(clientsXCoords)) {
      distMatrix[i,i] <- 1000
    }
    return(distMatrix)
  }
  
  #preenchimento da distDepot
  
  getDistDepot <- function(distDepot) {
    for(i in 1:length(clientsXCoords)) {
      for(j in 1:length(depotsXCoords)) {
        dis <- distanciaEntrePontos(clientsXCoords[i], clientsYCoords[i], depotsXCoords[j], depotsYCoords[j])
        distDepot[i,j] <- dis
      }
    }
    return(distDepot)
  }
  
  
  
  # HERE ENDS DATASET TREATMENT
  # ===========================
  
  
  distanceTour <- function(tour, distMatrix, distDepot, depot) {
    dist <- 0
    if(length(tour) != 1) {
      route <- embed(tour,2)[,2:1]
      dist <- sum(distMatrix[route])
    }
    #print("new -------")
    #print(paste(distDepot[tour[1]], "!=", distDepot[tour[1], depot]))
    #print(paste(distDepot[tour[length(tour)]], "!=", distDepot[tour[length(tour)], depot]))
    totalDist <- dist + distDepot[tour[1], depot] + distDepot[tour[length(tour)], depot]
    return(totalDist * 1)
  }
  
  totalCost <- function(chromossome, distMatrix, distDepot, depot) {
    routes <- matrix(chromossome, nrow = numClients, ncol = nGenes)
    totalDist <- 0
    totalCost <- 0
    vehNumber <- 0
    cliServed <- vector()
    for(vehicle in 1:nGenes) {
      t <- routes[,vehicle]
      remove <- (numClients + 1):(length(indv))
      tour <- setdiff(t, remove)
      if(length(tour) != 0) {
        for (i in 1:length(tour)){
          cliServed <- c(cliServed, tour[i])
        }
        totalDist <- totalDist + distanceTour(tour, distMatrix, distDepot, depot)
        vehNumber <- vehNumber + 1
      }
    }
    #totalCost <- totalDist + vehCost
    if(vehNumber != 0) {
      totalCost <- sum(totalDist, depotsCost[depot], vehNumber * vehicleCost)
    }
    return(list(totalCost, cliServed))
  }
  
  totalCostPerDepot <- function(chromossome, distMatrix, distDepot) {
    count <- nGenes * nAllele
    total <- 0
    for(i in 0:(numDepots - 1)) {
      res <- totalCost(chromossome = chromossome[(i * count + 1):((i + 1) * count)], distMatrix, distDepot, (i + 1))
      # CHECK IF CLIENTS DEMANDS SUM IS ABOVE DEPOTS' CAPACITY
      #demandSum <- 0
      #for (j in 1:length(res[[2]])){
      #  demandSum <- demandSum + cliDemand[res[[2]][j]]
      #}
      #cap <- strtoi(depotsCap[i+1])
      #print(paste("Depot", i+1))
      #print(demandSum)
      #print(cap)
      #if (demandSum > cap) {
      #  penalization <- (demandSum - cap)*500
      #  total <- total + res[[1]] + penalization
      #} else {
      #  total <- total + res[[1]]
      #}
      total <- sum(total, res[[1]])
    }
    return(total)
  }
  
  fitness <- function(chromossome, distMatrix, distDepot) {
    1 / (totalCostPerDepot(chromossome, distMatrix, distDepot))
  }
  
  analyze <- function(solution, vehiclesUsed, depotsUsed) {
    count <- nGenes * nAllele
    flagDepot <- FALSE
    flagVehicle <- FALSE
    depotsIds <- vector()
    routes <- vector()
    for(depot in 0:(numDepots - 1)) {
      toAnalyzeDepot <- solution[((depot * count) + 1):((depot + 1) * count)]
      for(vehicle in 0:(nGenes - 1)) {
        route <- ""
        toAnalyzeVehicle <- toAnalyzeDepot[((vehicle * nAllele) + 1):((vehicle + 1) * nAllele)]
        for(cli in 1:length(toAnalyzeVehicle)) {
          if(toAnalyzeVehicle[cli] < numClients) {
            flagVehicle <- TRUE
            flagDepot <- TRUE
            route <- paste(route, toAnalyzeVehicle[cli])
          }
        }
        if(flagVehicle == TRUE) {
          vehiclesUsed <- vehiclesUsed + 1
          routes <- c(routes, route)
          flagVehicle <- FALSE
        }
      }
      if(flagDepot == TRUE) {
        depotsUsed <- depotsUsed + 1
        depotsIds <- c(depotsIds, depot+1)
        flagDepot <- FALSE
      }
    }

    return(list(depotsUsed, depotsIds, vehiclesUsed, routes))
  }
  
  printResults <- function() {
    for(i in 1:nrow(GA.fit@solution)) {
      vehiclesUsed <- 0
      depotsUsed <- 0
      print(paste("SOLUTION ", i))
      res <- analyze(GA.fit@solution[i,], vehiclesUsed, depotsUsed)
      costs <- apply(GA.fit@solution, 1, totalCostPerDepot, distMatrix, distDepot)
      
      print(paste(res[3], "vehicles used!"))
      str <- paste(res[1], "depots opened:")
      for (i in 1:strtoi(res[1])) {
        str <- paste(str, res[[2]][i])
      }
      print(str)
      #for (i in 1:strtoi(res[3])){
      #  print(paste("Route of vehicle", i, "->", res[[4]][i]))
      #}
      print(paste("The final cost was:", format(round(costs[i], 2))))
    }
  }
  
  # MAIN SCRIPT
  
  dataset <- removeNewlines(dataset)
  
  numClients <- as.double(dataset[1])
  numDepots <- as.double(dataset[2])
  vehicleCap <- as.double(dataset[3 + numDepots + numClients])
  vehicleCost <- as.double(dataset[length(dataset) - 1])
  
  getDepotsCoords()
  getClientsCoords()
  depotsCap <- getDepotsCap(depotsCap)
  depotsCost <- getDepotsCost(depotsCost)
  cliDemand <- getClientsDemand(cliDemand)
  
  distMatrix <- matrix( nrow = length(clientsXCoords), ncol = length(clientsXCoords))
  distDepot <- matrix( nrow = length(clientsXCoords), ncol = length(depotsXCoords))
  distMatrix <- getDistMatrix(distMatrix)
  distDepot <- getDistDepot(distDepot)
  
  # -----------------------------------------
  # Chromossome structure
  
  nChrom <- numDepots
  nGenes <- ceiling((sum(cliDemand)) / vehicleCap)
  nAllele <- numClients
  
  indv <- c(1:(nChrom * nGenes * (nAllele)))
  
  start <- proc.time()
  
  GA.fit <- ga(type = "permutation", fitness = fitness,
               distMatrix = distMatrix, distDepot = distDepot,
               min = 1, max = length(indv), popSize = 30,
               maxiter = 100, run = 200, pmutation = 0.2,
               monitor = NULL)

  proc.time() - start
  
  printResults()