datos_vinos<-read.csv(file = "datasets/winequality-white.csv",sep = ";")

?read.csv

?corrplot

M<-cor(datos_vinos)
corrplot(M)

N<-cor(mtcars)
corrplot(N)
