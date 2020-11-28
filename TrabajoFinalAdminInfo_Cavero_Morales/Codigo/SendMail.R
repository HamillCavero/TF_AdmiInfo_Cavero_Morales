library(rJava)
library(mailR)

mail <- function(correo) 
{
  send.mail(from = 'upcedgarelvis@gmail.com',
            to = c(correo), # Colocar correos
            subject = 'Envio Datos de las funciones en archivo html',
            body = 'Envio resumen html de las funciones',
            smtp = list(host.name = 'smtp.gmail.com',port = 465,
                        user.name = 'upcedgarelvis@gmail.com',
                        passwd = '123admininfo', ssl = TRUE),  # Cambiar contraseña
            authenticate = TRUE,
            send = TRUE,
            attach.files = c('Codigo/DetalleFunciones.html'),
            file.names = c('Codigo/DetalleFunciones.html'), # El nombre con el que se quiere adjuntar el correo
            file.descriptions = c('Archivo que contiene nube de palabras'),
            # optional parameter
            debug = TRUE) 
}

