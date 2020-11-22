lectura<-function(direccion)
  {
    return(read.csv(direccion))
  }
datos_violencia<-lectura("datasets/SIDPOL_2017_Violencia_familiar.csv")

#Guardando Datos Backup#
save(datos_violencia,file = "datasets/DF_violencia.Rda")

load("datasets/DF_violencia.Rda")


