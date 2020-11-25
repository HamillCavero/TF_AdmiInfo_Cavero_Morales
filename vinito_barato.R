datos_vinos<-read.csv(file = "datasets/winequality-white.csv",sep = ";")
?read.csv

?corrplot

M<-cor(datos_vinos)
corrplot(M)

N<-cor(mtcars)
corrplot(N)

#extraído de
#http://archive.ics.uci.edu/ml/datasets/seeds
# Attribute Information:
#   
# To construct the data, seven geometric parameters of wheat kernels were measured:
# 1. area A,
# 2. perimeter P,
# 3. compactness C = 4*pi*A/P^2,
# 4. length of kernel,
# 5. width of kernel,
# 6. asymmetry coefficient
# 7. length of kernel groove.
#tipos : Kama, Rosa and Canadian
# All of these parameters were real-valued continuous.
semillas<-read.delim(file = "datasets/seeds_dataset1.txt",header = FALSE)
O<-cor(semillas)
corrplot(O)
plot(semillas)

names(semillas)[1]<-"Area"
names(semillas)[2]<-"Perimetro"
names(semillas)[3]<-"Compact"
names(semillas)[4]<-"largo"
names(semillas)[5]<-"ancho"
names(semillas)[6]<-"Coeficiente_de_Asimetria"
names(semillas)[7]<-"Longitud_semilla"
names(semillas)[8]<-"Tipo"



colnames(semillas)

library(DMwR)

## Split in train + test set
idxs <- sample(1:nrow(semillas),as.integer(0.7*nrow(semillas)))
trainSemillas <- semillas[idxs,]
testSemillas <- semillas[-idxs,]

## A 3-nearest neighbours model with no normalization
nn3 <- kNN(Tipo ~ .,trainSemillas,testSemillas,norm=FALSE,k=3)
?kNN

## The resulting confusion matrix
table(testSemillas[,'Tipo'],nn3)

## Now a 5-nearest neighbours model with normalization
nn5 <- kNN(Tipo ~ .,trainSemillas,testSemillas,,norm=TRUE,k=5)

## The resulting confusion matrix
table(testSemillas[,'Tipo'],nn5)

library(stats)

plot(semillas$Perimetro,semillas$Coeficiente_de_Asimetria)
semillas1<-semillas

aaaa<-kmeans(x = semillas1,centers = 3,iter.max = 10,nstart = 30)
aaaa

semillas1 <- semillas1 %>% mutate(cluster = aaaa$cluster)
semillas1 <- semillas1 %>% mutate(cluster = as.factor(cluster),
                          #grupo   = as.factor(grupo))
                          grupo   = 3)

ggplot(data = semillas1, aes(x = semillas1$Perimetro, y = semillas1$Coeficiente_de_Asimetria, color = semillas1$Tipo)) +
  geom_text(aes(label = cluster), size = 5) +
  theme_bw() +
  theme(legend.position = "none")

colnames(datos_violencia)


plot(semillas1$largo,semillas1$ancho)
reg_semillas1<-abline(lm(semillas1$ancho~semillas1$largo),col="red")

plot(semillas1$Perimetro,semillas1$ancho)
reg_semillas1<-abline(lm(semillas1$ancho~semillas1$Perimetro),col="red")

plot(semillas1$Perimetro,semillas1$Area)
reg_smlls1<-lm(semillas1$Area~semillas1$Perimetro)
reg_semillas1<-abline(lm(semillas1$Area~semillas1$Perimetro),col="red")

ggplot(dtModeloR2, aes(x=AHumanitarFam, y=EDanosVivienda)) + geom_point() + ggtitle('Gráfica de Regresion') + xlab('Ayuda Humanitaria por Familia') + ylab('Estimacion de Daños por Vivienda') + geom_smooth(method=lm)

