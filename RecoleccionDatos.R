
datos_violencia<-read.csv("datasets/SIDPOL_2017_Violencia_familiar.csv")
View(datos_violencia)

#Guardando Datos Backup#
save(datos_violencia,file = "datasets/DF_violencia.Rda")

load("datasets/DF_violencia.Rda")
