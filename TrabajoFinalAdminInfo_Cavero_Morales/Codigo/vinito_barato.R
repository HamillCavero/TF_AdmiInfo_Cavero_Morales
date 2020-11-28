
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

names(semillas)[1]<-"Area"
names(semillas)[2]<-"Perimetro"
names(semillas)[3]<-"Compact"
names(semillas)[4]<-"largo"
names(semillas)[5]<-"ancho"
names(semillas)[6]<-"Coeficiente_de_Asimetria"
names(semillas)[7]<-"Longitud_semilla"
names(semillas)[8]<-"Tipo"


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
nn31 <- as.numeric(levels(nn3))[nn3]



#plot(nn3)
#nn3[,1]
sum(as.integer(testSemillas==1))
sum(as.integer(testSemillas==2))
sum(as.integer(testSemillas==3))

#qplot(nn3)
#qplot(nn31)
#qplot(testSemillas$Tipo)

#plot(testSemillas$Tipo)
ggplot(data = semillas,aes(x = semillas$Tipo, y = semillas$Area))+geom_col(stat="identity", width=1,color="white")
#plot(nn31)
class(nn31[])

#plot(semillas)
ggplot(data =semillas,aes(x = semillas$largo,y = semillas$ancho,color=semillas$Tipo ) )+geom_jitter()+theme(legend.position = "none")


## Now a 5-nearest neighbours model with normalization
nn5 <- kNN(Tipo ~ .,trainSemillas,testSemillas,,norm=TRUE,k=5)

## The resulting confusion matrix
table(testSemillas[,'Tipo'],nn5)

library(stats)

plot(semillas$Perimetro,semillas$Coeficiente_de_Asimetria)
semillas1<-semillas

aaaa<-kmeans(x = semillas1,centers = 3,iter.max = 10,nstart = 30)
aaaa

####
bbbb <- kmeans(semillas[,c(1,6)], 3, iter.max = 1000, nstart = 10)
#dataset$cluster <- kmeans$cluster
semillas2<-semillas
semillas2$cluster<-bbbb$cluster
# ggplot() + geom_point(aes(x = semillas2$Area, y = semillas2$Coeficiente_de_Asimetria, color = cluster), data = semillas2, size = 2) +
#   scale_colour_gradientn(colours=rainbow(4)) +
#   geom_point(aes(x = bbbb$centers[, 1], y = bbbb$centers[, 2]), color = 'black', size = 3) + 
#   ggtitle('Clusters de Datos con k = 3 / K-Medios') + 
#   xlab('X') + ylab('Y')
####

semillas1 <- semillas1 %>% mutate(cluster = aaaa$cluster)
semillas1 <- semillas1 %>% mutate(cluster = as.factor(cluster),
                          #grupo   = as.factor(grupo))
                          grupo   = 3)

# ggplot(data = semillas1, aes(x = semillas1$Perimetro, y = semillas1$Coeficiente_de_Asimetria, color = semillas1$cluster)) +
#   geom_text(aes(label = cluster), size = 5) +
#   theme_bw() +
#   theme(legend.position = "none")

#colnames(datos_violencia)


#plot(semillas1$largo,semillas1$ancho)
reg_semillas1<-abline(lm(semillas1$ancho~semillas1$largo),col="red")
#ggplot(data =semillas1, aes(largo,ancho))+ geom_point() +   geom_smooth()
#ggplot(data =semillas1, aes(largo,ancho))+ geom_point() +   geom_smooth(method = "lm")

#plot(semillas1$Perimetro,semillas1$ancho)
#reg_semillas1<-abline(lm(semillas1$ancho~semillas1$Perimetro),col="red")

#ggplot(data =semillas1, aes(Perimetro,ancho))+ geom_point() +   geom_smooth()


#plot(semillas1$Perimetro,semillas1$Area)
#reg_smlls1<-lm(semillas1$Area~semillas1$Perimetro)
#reg_semillas1<-abline(lm(semillas1$Area~semillas1$Perimetro),col="red")

#ggplot(data =semillas1, aes(Perimetro,Area))+ geom_point() +   geom_smooth()

########regresión multivariable
#f=lm(mpg~disp+hp+drat+wt,data = mtcars)

regre_multi_semillas<-lm(data = semillas,semillas$Area~semillas$Perimetro+semillas$largo+semillas$ancho)
# ggplot(data=regre_multi_semillas,aes(x = regre_multi_semillas$fitted.values,y = regre_multi_semillas$residuals))+
#   geom_point() + geom_smooth()

