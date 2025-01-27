library(readxl)
setwd("D:/FIIT20Semenov")
getwd()
data = read_excel("Lab9data.xls")
data
x = data$x1
y = data$x2

x_mean <- mean(x)
y_mean <- mean(y)
x_centered <- x - x_mean
y_centered <- y - y_mean
sd_x <- sd(x)
sd_y <- sd(y)
x_normalized = x_centered/sd_x
y_normalized = y_centered/sd_y

Z <- cbind(x_normalized, y_normalized)
Z

correlation <- cor(x, y)
correlation

cov_matrix <- matrix(c(var(x), cov(x, y), cov(x, y), var(y)), nrow = 2)
cov_matrix

eigen_info <- eigen(cov_matrix)
eigenvalues <- eigen_info$values
eigenvalues

eigenvectors <- eigen_info$vectors
eigenvectors

A <- eigenvectors %*% diag(sqrt(eigenvalues))

pc_matrix <- solve(A) %*% t(Z)
pc_matrix



data <- matrix(c(x, y), nrow = length(x), byrow = FALSE)
data.pca <- prcomp(data)
data.pca