file <- "C:\\Users\\PC\\Desktop\\SVM\\HVAC24hS16-11-2016--0.xls"

# Exerc�cio 1
ex1_original <- runSVM(file)

# Exerc�cio 2
ex2 <- calculaMAPE(file, 6)
ex2 <- calculaMAPE(file, 7)
ex2 <- calculaMAPE(file, 8)
ex2 <- calculaMAPE(file, 9)

# Exerc�cio 3
# Testes para treino com 6 instru��es
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

# Testes para treino com 7 instru��es
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

# Testes para treino com 8 instru��es
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

# Testes para treino com 9 instru��es
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