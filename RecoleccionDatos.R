datos_violencia<-read.csv("datasets/SIDPOL_2017_Violencia_familiar.csv")
View(datos_violencia)

#Guardando Datos Backup#
save.image("Datos_TF_Violencia_Admi.RData") 

load("dataset/Datos_TF_Violencia_Admi.RData")