lectura<-function(direccion)
  {
    return(read.csv(direccion))
  }
datos_violencia<-lectura("datasets/SIDPOL_2017_Violencia_familiar.csv")
#Guardando Datos Backup#
save.image("Datos_TF_Violencia_Admi.RData") 

load("dataset/Datos_TF_Violencia_Admi.RData")


