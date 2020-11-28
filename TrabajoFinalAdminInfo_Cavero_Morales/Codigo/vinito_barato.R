# 1. area A,
# 2. perimeter P,
# 3. compactness C = 4*pi*A/P^2,
# 4. length of kernel,
# 5. width of kernel,
# 6. asymmetry coefficient
# 7. length of kernel groove.
#tipos : Kama, Rosa and Canadian
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
#?kNN

## The resulting confusion matrix
#table(testSemillas[,'Tipo'],nn3)
nn31 <- as.numeric(levels(nn3))[nn3]

## Now a 5-nearest neighbours model with normalization
nn5 <- kNN(Tipo ~ .,trainSemillas,testSemillas,,norm=TRUE,k=5)

 
library(stats)

#plot(semillas$Perimetro,semillas$Coeficiente_de_Asimetria)
semillas1<-semillas

aaaa<-kmeans(x = semillas1,centers = 3,iter.max = 10,nstart = 30)
 
####
bbbb <- kmeans(semillas[,c(1,6)], 3, iter.max = 1000, nstart = 10)

semillas1 <- semillas1 %>% mutate(cluster = aaaa$cluster)
semillas1 <- semillas1 %>% mutate(cluster = as.factor(cluster),
                          #grupo   = as.factor(grupo))
                          grupo   = 3)

regre_multi_semillas<-lm(data = semillas,semillas$Area~semillas$Perimetro+semillas$largo+semillas$ancho)

