file <- "C:\\Users\\PC\\Desktop\\SVM\\HVAC24hS16-11-2016--0.xls"

# Exercício 1
ex1_original <- runSVM(file)

# Exercício 2
ex2 <- calculaMAPE(file, 6)
ex2 <- calculaMAPE(file, 7)
ex2 <- calculaMAPE(file, 8)
ex2 <- calculaMAPE(file, 9)

# Exercício 3
# Testes para treino com 6 instruções
ex3 <- calculaMAPE(file, 6, epsilon = 0.1, kernel = "linear")
ex3 <- calculaMAPE(file, 6, epsilon = 0.5, kernel = "linear")
ex3 <- calculaMAPE(file, 6, epsilon = 0, kernel = "linear")

ex3 <- calculaMAPE(file, 6, epsilon = 0.1, kernel = "polynomial")
ex3 <- calculaMAPE(file, 6, epsilon = 0.5, kernel = "polynomial")
ex3 <- calculaMAPE(file, 6, epsilon = 0.9, kernel = "polynomial")

ex3 <- calculaMAPE(file, 6, epsilon = 0.1, kernel = "radial")
ex3 <- calculaMAPE(file, 6, epsilon = 0.5, kernel = "radial")
ex3 <- calculaMAPE(file, 6, epsilon = 0.9, kernel = "radial")

ex3 <- calculaMAPE(file, 6, epsilon = 0.1, kernel = "sigmoid")
ex3 <- calculaMAPE(file, 6, epsilon = 0.5, kernel = "sigmoid")
ex3 <- calculaMAPE(file, 6, epsilon = 0.9, kernel = "sigmoid")

# Testes para treino com 7 instruções
ex3 <- calculaMAPE(file, 7, epsilon = 0.1, kernel = "linear")
ex3 <- calculaMAPE(file, 7, epsilon = 0.5, kernel = "linear")
ex3 <- calculaMAPE(file, 7, epsilon = 0.9, kernel = "linear")

ex3 <- calculaMAPE(file, 7, epsilon = 0.1, kernel = "polynomial")
ex3 <- calculaMAPE(file, 7, epsilon = 0.5, kernel = "polynomial")
ex3 <- calculaMAPE(file, 7, epsilon = 0.9, kernel = "polynomial")

ex3 <- calculaMAPE(file, 7, epsilon = 0.1, kernel = "radial")
ex3 <- calculaMAPE(file, 7, epsilon = 0.5, kernel = "radial")
ex3 <- calculaMAPE(file, 7, epsilon = 0.9, kernel = "radial")

ex3 <- calculaMAPE(file, 7, epsilon = 0.1, kernel = "sigmoid")
ex3 <- calculaMAPE(file, 7, epsilon = 0.5, kernel = "sigmoid")
ex3 <- calculaMAPE(file, 7, epsilon = 0.9, kernel = "sigmoid")

# Testes para treino com 8 instruções
ex3 <- calculaMAPE(file, 8, epsilon = 0.1, kernel = "linear")
ex3 <- calculaMAPE(file, 8, epsilon = 0.5, kernel = "linear")
ex3 <- calculaMAPE(file, 8, epsilon = 0.9, kernel = "linear")

ex3 <- calculaMAPE(file, 8, epsilon = 0.1, kernel = "polynomial")
ex3 <- calculaMAPE(file, 8, epsilon = 0.5, kernel = "polynomial")
ex3 <- calculaMAPE(file, 8, epsilon = 0.9, kernel = "polynomial")

ex3 <- calculaMAPE(file, 8, epsilon = 0.1, kernel = "radial")
ex3 <- calculaMAPE(file, 8, epsilon = 0.5, kernel = "radial")
ex3 <- calculaMAPE(file, 8, epsilon = 0.9, kernel = "radial")

ex3 <- calculaMAPE(file, 8, epsilon = 0.1, kernel = "sigmoid")
ex3 <- calculaMAPE(file, 8, epsilon = 0.5, kernel = "sigmoid")
ex3 <- calculaMAPE(file, 8, epsilon = 0.9, kernel = "sigmoid")

# Testes para treino com 9 instruções
ex3 <- calculaMAPE(file, 9, epsilon = 0.1, kernel = "linear")
ex3 <- calculaMAPE(file, 9, epsilon = 0.5, kernel = "linear")
ex3 <- calculaMAPE(file, 9, epsilon = 0.9, kernel = "linear")

ex3 <- calculaMAPE(file, 9, epsilon = 0.1, kernel = "polynomial")
ex3 <- calculaMAPE(file, 9, epsilon = 0.5, kernel = "polynomial")
ex3 <- calculaMAPE(file, 9, epsilon = 0.9, kernel = "polynomial")

ex3 <- calculaMAPE(file, 9, epsilon = 0.1, kernel = "radial")
ex3 <- calculaMAPE(file, 9, epsilon = 0.5, kernel = "radial")
ex3 <- calculaMAPE(file, 9, epsilon = 0.9, kernel = "radial")

ex3 <- calculaMAPE(file, 9, epsilon = 0.1, kernel = "sigmoid")
ex3 <- calculaMAPE(file, 9, epsilon = 0.5, kernel = "sigmoid")
ex3 <- calculaMAPE(file, 9, epsilon = 0.9, kernel = "sigmoid")