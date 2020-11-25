
library(shiny)
library(corrplot)
library(ggplot2)
library(factoextra)

options(shiny.maxRequestSize=103*1024^2)
shinyServer(function(input, output) {
    getwd()
    
    output$tablaS2 <- renderTable({
        tablaS1 <- input$file1
        if (is.null(tablaS1))
        {return(NULL)}
        box<- input$select
        if(box == "1")    {
            read.csv(tablaS1$datapath)
        } else{  if(box == "2") {
            read.csv(tablaS1$datapath)
        } else { if(box == "3") {
            read_xlsx(tablaS1$datapath)
        } 
        } }
    })
   
    output$consulta1 <- renderText({ '
    #LECTURA DE DATOS

    datos_violencia<-lectura("datasets/SIDPOL_2017_Violencia_familiar.csv")
---
    
    #Limpieza de datos
    
    limpiar_violencia<-function(df)
{
  return(df[,-c(16,17,18,19,42,43,44,30,28)])
}

conversion_fecha<-function(df)
{
  #Modificando las fechas para poder trabajar con ellas
  
  df$FECHA_HORA_HECHO<-strptime(df$FECHA_HORA_HECHO,"%m/%d/%Y")
  
  df$fec_registro<-parse_date_time2(df$fec_registro,orders="dmy")
  
  return(df)
}
  #Borrado de valores NA
borrar_na<-function(df)
{
  dv<-na.omit(df)
  return(dv)  
}
limpiar_edades<-function(df)
{
  temporal<-df%>%filter(EDAD>8 & EDAD<101 & PERSONA>9999 & ID_PERSONA>9999)  
  return(temporal)
}' })
    
    output$consulta2 <- renderText({
        opc<-input$selectdp
        if(is.null(box))
        {
          return(NULL)  
        }
        if(opc == "1")    {
          txt1<-"#1)Cantidad de casos registrados por año y que sean de modalidad violencia fisica#
      casos_anhos_modalidad<-function(df,anho,modalidad,totdatos)
      {
  #Se le podria poner un if si quieres que se guarde con todos los datos o no.
  if(totdatos==TRUE)
  {
  return(q1<-df%>%filter(year(df$fec_registro)==anho & df$MODALIDAD ==modalidad)%>%summarise(Total=n()))
  }
  else
  {
  return(q1<-df%>%filter(year(df$fec_registro)==anho & df$MODALIDAD ==modalidad)%>%group_by(MODALIDAD,fec_registro)%>%summarise(Total=n()))
    #%>%select(c(3,20,22,28,14,19))
  }
    } "
          return (txt1)
        } else{  if(opc == "2") {
          txt2<- "#2) Cantidad de casos por cada tipo de modalidad #
casos_totales_por_modalidad<-function(df)
{
  return(q2<-df%>%group_by(Modalidad=df$MODALIDAD)%>%summarise(Cantidad_Casos=n()))  
}" 
          return (txt2)
        } else { if(opc == "3") {
          txt3<- "#3)Conteo de tipos de denuncias por departamento
group_by_denuncias<-function(df)
{
  return(df%>%group_by(DPTO_CIA)%>%summarise(Total=n())%>%arrange(desc(Total)))
}"
          return (txt3)
        } else { if(opc == "4")
        { txt4 <- "#4)Cantidad de personas según la relación con la denuncia#

relacion_denuncia<-function(df,cant_top)
{
  q<-df%>%group_by(SIT_PERSONA)%>%summarise(Total=n())%>%arrange(desc(Total))
  if(cant_top==0)
  {
    return(q[,c(1,2)])
  }
  else{
    return(q[c(1:cant_top),c(1,2)])
    
  }
}"
        return (txt4)
        } else { if(opc=="5")
        { txt5 <- "#5 Cantidad de casos segun paises

cant_paises<-function(df,cant_top)
{
  q<-df%>%group_by(pais_natal)%>%summarise(Total=n())%>%arrange(desc(Total))
  if(cant_top==0)
  {
    return(q[,c(1,2)])
  }
  else{
  return(q[c(1:cant_top),c(1,2)])

  }
}"
        return (txt5)
        }else { if(opc=="6")
        {
          txt6<-"#6 Cantidad de casos por edad
casos_Edad<-function(df)
{
  return(df%>%group_by(EDAD)%>%summarise(Total=n())%>%arrange(desc(Total)))
}

q6<-casos_Edad(datos_violencia)"
          return (txt6)
        }else { if(opc=="7")
        {
          txt7<-"#7 Cantidad de casos por sexo

casos_sexo<-function(df)
{
  return(df%>%group_by(SEXO)%>%summarise(Total=n())%>%arrange(desc(Total)))
}"
          return (txt7)
        }else { if(opc=="8")
        {
          txt8<-"#8 Cantidad de casos registrados por Distrito top
casos_distritos<-function(df,cant_top)
{
  q<-df%>%group_by(DIST_CIA)%>%summarise(Total=n())%>%arrange(desc(Total))
  
  if(cant_top==0)
  {
    return(q[,c(1,2)])
  }
  else{
    return(q[c(1:cant_top),c(1,2)])
    
  }
}"
          return (txt8)
        }
        else { if(opc=="9")
        {
          txt9<-"#9 Fiscalia Derivadas
fiscalias<-function(df)
{
  return(df%>%group_by(DERIVADA_FISCALIA)%>%
  filter(DERIVADA_FISCALIA!='NULL')%>%
  summarise(Total=n())%>%arrange(desc(Total)))
}"
          return (txt9)
        } 
        else { if(opc=="10")
        {
          txt10<-"#10 cantidad de denunciantes y denuncoados separados por sexos

situcion_persona_por_Sexo<-function(df)
{
  return(df%>%group_by(SIT_PERSONA,SEXO)%>%
  filter(SIT_PERSONA=='DENUNCIANTE' |SIT_PERSONA=='DENUNCIADO')%>%
  summarise(Total=n())%>%arrange(desc(Total)))
}"
          return (txt10)
        }
        
        else { if(opc=="11")
        {
          txt11<-"#11 Mes en la que hubo mayor cantidad de violencia,denuncia

mayor_mes<-function(df)
{
  return(df%>%group_by(MES)%>%summarise(Total=n())%>%arrange(desc(Total)))
}"
          return (txt11)
        }
        
        else { if(opc=="12")
        {
          txt12<-"#12 Cantidad casos por estado civil

estado_civil<-function(df)
{
  return(df%>%group_by(EST_CIVIL)%>%filter(EST_CIVIL!='NULL')%>%summarise(Total=n())%>%arrange(desc(Total)))
}"
          return (txt12)
        }
        
        else { if(opc=="13")
        {
          txt13<-"#13 Busqueda por tipo de semilla, con mayor area

tipo_semilla<-function(df,area)
{
  dv<-df%>%group_by(Tipo,Area)%>%filter(Area>area)%>%select(c(1,8))
  return(dv)
}"
          return (txt13)
        }
        
        else { if(opc=="14")
        {
          txt14<-"#14 Promedio de personas por edad que denuncia.

promedio_Edad<-function(df)
{
  return(mean(df$EDAD))
}"
          return (txt14)
        }
        
        else { if(opc=="15")
        {
          txt15<-"#15) Muestra de porcentaje de denuncias por edad y tipo de denuncia
porcentaje_Edades<-function(df,edad,todos) # PORCENTAJE EN GENERAL CON TODO EL DATASET
{
  q15<-df%>%group_by(Edad=df$EDAD)%>%summarise(total=n())%>%arrange(desc(total))
  q15$Porcentaje=(q15$total/NROW(df))*100
  if(todos==FALSE)
  {
  return(q15<-q15%>%filter(Edad==edad))
  }
  else
  {
   return(q15) 
  }
}"
          return (txt15)
        }
        
        else { if(opc=="16")
        {
          txt16<-"#16)cantidad de casos de 18 años y CON FILTRO SEGÚN TIPO DE DENUNCIA TOTAL

porc_tip_denuncia<-function(df,edad,denuncia_tipo)
{
  q<-df%>%group_by(EDAD,TIPO_DENUNCIA)%>%filter(EDAD==edad & TIPO_DENUNCIA==denuncia_tipo)%>%summarise(total=n())%>%arrange(desc(total))
  da<-df%>%group_by(TIPO_DENUNCIA)%>%filter(TIPO_DENUNCIA==denuncia_tipo)%>%summarise(tot=n())
  q$Porcentaje=(q$total/da$tot)*100  
  return (q)
}"
          return (txt16)
        }
        
        else { if(opc=="17")
        {
          txt17<-"#17)cantidad de casos de 18 años y CON FILTRO SEGÚN TIPO DE DENUNCIA TOTAL, 
          PERO QUE EL DETERMINANTE DEL PORCENTAJE SERA LA EDAD

porc_tip_edad<-function(df,edad)
{
  da<-df%>%group_by(EDAD,TIPO_DENUNCIA)%>%filter(EDAD==edad)%>%summarise(tot=n())
  dto<-df%>%group_by(EDAD)%>%filter(EDAD==edad)%>%summarise(tot=n())
  da$tot=(da$tot/dto$tot)*100
  return (da)
}"
          return (txt17)
        }
        
          
        else { if(opc=="18")
        {
          txt18<-"# 18) Obtención de datos  de denuncia por año especifico

datosobtenidos_porfecha<-function(df,fecha)
{
  return(df%>%filter(FECHA_HORA_HECHO==fecha))
}"
          return (txt18)
        }
          
    }}}}}}}}}}}}}}}}}})
    
    output$tablaS3 <- renderDataTable({
        box<- input$selectdp
        if (is.null(box))
        {return(NULL)}
        
        if(box == "1")    {
            return (q1)
        } else{  if(box == "2") {
            return (q2)
        } else { if(box == "3") {
            return (q3)
        } else { if(box == "4")
        { 
        return (q4)
        } else { if(box=="5")
        { 
        return (q5)
        } 
          else { if(box=="6")
          { 
            return (q6)
          } 
          
          else { if(box=="7")
          { 
            return (q7)
          } 
          
          else { if(box=="8")
          { 
            return (q8)
          } 
          
          else { if(box=="9")
          { 
            return (q9)
          } 
          
          else { if(box=="10")
          { 
            return (q10)
          } 
          
          else { if(box=="11")
          { 
            return (q11)
          } 
          
          else { if(box=="12")
          { 
            return (q12)
          } 
          
          else { if(box=="13")
          { 
            return (q13)
          } 
          
          else { if(box=="14")
          { 
            return (q14)
          } 
          
          else { if(box=="15")
          { 
            return (q15)
          } 
          
          else { if(box=="16")
          { 
            return (q16)
          } 
          
          else { if(box=="17")
          { 
            return (q17)
          } 
          
          else { if(box=="18")
          { 
            return (q18)
          } 
          }
        } } }}}}}}}}}}}}}}})
    
    
    output$consulta4 <- renderText({
      box<- input$selectgg
      if (is.null(box))
      {return(NULL)}
      
      if(box == "1")  {
        gp1<-"qplot(x=Modalidad,y=Cantidad_Casos,data = q2,geom = 'col)+aes(fill= Modalidad)"  
        return (gp1)
        } else  
          if(box == "2")  {
        gp2<-"qplot(x=Total,y=DPTO_CIA,data = q3,geom = 'col')+aes(fill= DPTO_CIA)"
        return (gp2)
        } else  
          if(box == "3")  {
        gp3<-"qplot(x=SIT_PERSONA,y=Total,data = q4%>%slice(1:5),geom = 'col')+aes(fill= SIT_PERSONA)"
        return (gp3)
        } else 
          if(box == "4")  { 
        gp4<-"qplot(x=pais_natal  ,y=Total,data = q5%>%slice(1:5),geom = 'col')+aes(fill= pais_natal)"
        return (gp4)
        } else   
          if(box=="5")  { 
        gp5<-"qplot(x=EDAD   ,y=Total,data = q6,geom = 'col')+aes(fill= EDAD )"
        return (gp5)
        } else  
          if(box=="6")  {
        gp6<-"ggplot(data = q7, aes(x='', y=q7$Total, fill=q7$SEXO)) + geom_bar(stat='identity', width=1) + coord_polar('y', start=0)"
        return(gp6)
        } else  
          if(box=="7")  {
        gp7<-"qplot(x=q8$DIST_CIA   ,y=Total,data = q8,geom = 'col')+aes(fill= q8$DIST_CIA )" 
        return(gp7)
        } else  
          if(box=="8")  {
        gp8<-"ggplot(data = q9, aes(x='',y= Total, fill= DERIVADA_FISCALIA)) +
        geom_bar(stat='identity',width=1,color='white') +  coord_polar('y', start=0)+theme_void()"
        return(gp8)
        } else  
          if(box=="9")  {
          gp9<-"qplot(x=SIT_PERSONA  ,y=Total,data = q10,geom = 'col')+aes(fill= SEXO)"
          return(gp9)
        } else  
          if(box=="10")  {
          gp10<-"qplot(x= MES  ,y=Total,data = q11,geom = 'col')+aes(fill= MES)+theme_minimal()"
          return(gp10)
        } else
          if(box=="11")  {
          gp11<-"qplot(x=EST_CIVIL,y=Total,data = q12)+aes(fill= EST_CIVIL ) +
                    geom_point(size=4, color='#aaaaaa') +
          theme_minimal()+
            theme(legend.position = 'none'') +
            geom_text(aes(label=q12$Total),hjust=0, vjust=0)"
          return(gp11)
          } else 
            if(box=="12")  {
              gp12<-"qplot(x=Tipo   ,y=Area,data = q13,geom = 'boxplot')+theme_classic()+xlab(label = '')"
              return(gp12)
            }else 
              if(box=="13")  {
                gp13<-"qplot(x= Edad  ,y= total,data = q15,geom = 'line')+aes(fill= total)"
                return(gp13)
              }else 
                if(box=="14")  {
                  gp14<-"ggplot(data = q17, aes(x='', y= tot, fill= TIPO_DENUNCIA)) +
                          geom_bar(stat='identity', width=1,color='white') +
                          coord_polar('y', start=0)+theme_void()+
                          geom_text(aes(label = paste(round(tot, digits = 2)
                                                      , '%'), x = 1.3),
                                    position = position_stack(vjust = 0.5))"
                  return(gp14)
                }
      
      })
    
    output$plot1 <- renderPlot({
        box<- input$selectgg
        if (is.null(box))
        {return(NULL)}
        
        if(box == "1")    {
          gp1<-qplot(x=Modalidad,y=Cantidad_Casos,data = q2,geom = "col")+aes(fill= Modalidad)  
          return (gp1)
        } else  { if(box == "2") {
          gp2<-qplot(x=Total,y=DPTO_CIA,data = q3,geom = "col")+aes(fill= DPTO_CIA)
            return (gp2)
        } else  { if(box == "3") {
          gp3<-qplot(x=SIT_PERSONA,y=Total,data = q4%>%slice(1:5),geom = "col")+aes(fill= SIT_PERSONA)
            return (gp3)
        } else  { if(box == "4")  {
          gp4<-qplot(x=pais_natal  ,y=Total,data = q5%>%slice(1:5),geom = "col")+aes(fill= pais_natal)
            return (gp4)
        } else  { 
          if(box=="5")  {
          gp5<-qplot(x=EDAD   ,y=Total,data = q6,geom = "line")+aes(fill= EDAD )
            return (gp5)
        } else  { 
            if(box=="6")  {
          gp6<-ggplot(data = q7, aes(x="", y=q7$Total, fill=SEXO)) +
          geom_bar(stat='identity', width=1) +
          coord_polar('y', start=0)+theme_void()
          return(gp6)
          
          } else  { 
              if(box=="7")  {
          gp7<-qplot(x=q8$DIST_CIA   ,y=Total,data = q8,geom = 'col')+aes(fill= q8$DIST_CIA )
          return(gp7)
        
          } else  { 
                if(box=="8")  {
          gp8<-ggplot(data = q9, aes(x="", y= Total, fill= DERIVADA_FISCALIA)) +
            geom_bar(stat="identity", width=1,color="white") +
            coord_polar("y", start=0)+theme_void()
          return(gp8)
        
          } else  { 
                  if(box=="9")  {
              gp9<-qplot(x=SIT_PERSONA  ,y=Total,data = q10,geom = "col")+aes(fill= SEXO)
              return(gp9)
              
            }  else  { 
                    if(box=="10")  {
                gp10<-qplot(x= MES  ,y=Total,data = q11,geom = "col")+aes(fill= MES)+theme_minimal()
                return(gp10)
                
              } else  { 
                      if(box=="11")  {
                  gp11<-qplot(x=EST_CIVIL,y=Total,data = q12)+aes(fill= EST_CIVIL ) +
                    geom_point(size=4, color="#aaaaaa") +
                    theme_minimal()+
                    theme(legend.position = "none") +
                    geom_text(aes(label=q12$Total),hjust=0, vjust=0)
                  return(gp11)
                  
                } else  { 
                        if(box=="12")  {
                    gp12<-qplot(x=Tipo   ,y=Area,data = q13,geom = "boxplot")+theme_classic()+xlab(label = "")
                    return(gp12)
                  } else  { 
                          if(box=="13")  {
                      gp13<-qplot(x= Edad  ,y= total,data = q15,geom = "line")+aes(fill= total)
                      return(gp13)
                    } else  { 
                            if(box=="14")  {
                        gp14<-ggplot(data = q17, aes(x="", y= tot, fill= TIPO_DENUNCIA)) +
                          geom_bar(stat="identity", width=1,color="white") +
                          coord_polar("y", start=0)+theme_void()+
                          geom_text(aes(label = paste(round(tot, digits = 2)
                                                      , "%"), x = 1.3),
                                    position = position_stack(vjust = 0.5))
                        return(gp14)
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          }
          }
          }
          }
          }
          }
        })
    
    
    output$consulta6 <- renderText({
      box<- input$selectmod
      if (is.null(box))    {return(NULL)}
      if(box == "1")    {
        gm1<-"semillas1<-semillas
        aaaa<-kmeans(x = semillas1,centers = 3,iter.max = 10,nstart = 30)
        aaaa
        semillas1 <- semillas1 %>% mutate(cluster = aaaa$cluster)
        semillas1 <- semillas1 %>% mutate(cluster = as.factor(cluster),
                                          #grupo   = as.factor(grupo))
                                          grupo   = 3)
        ggplot(data = semillas1, aes(x = semillas1$Perimetro, y = semillas1$Coeficiente_de_Asimetria, color = semillas1$cluster)) +
          geom_text(aes(label = cluster), size = 5) +
          theme_bw() +
          theme(legend.position = 'none')"
        return (gm1)
      } else
        if(box == "2")    {
          gm2<-"plot(semillas1$largo,semillas1$ancho)
          gm2<-ggplot(data =semillas1, aes(largo,ancho))+ geom_point() +   geom_smooth(method = 'lm')"
          return (gm2)
        }else
          if(box == "3")    {
            gm3<-"gm3<-ggplot(data =semillas1, aes(largo,ancho))+ geom_point() +   geom_smooth()"
            return (gm3)
          }else
            if(box == "4")    {
              gm4<-"regre_multi_semillas<-lm(data = semillas,semillas$Area~semillas$Perimetro+semillas$largo+semillas$ancho)
          gm4<-ggplot(data=regre_multi_semillas,aes(x = regre_multi_semillas$fitted.values,y = regre_multi_semillas$residuals))+
            geom_point() + geom_smooth()"
              return (gm4)
            } 
      
    })
  
    output$tablaS6 <- renderTable({
        
        if(is.null(input$file1)){return ()}
        input$file1
    })
    
    output$plot8 <- renderPlot({
      box<- input$selectmod
      if (is.null(box))    {return(NULL)}
      if(box == "1")    {
        semillas1<-semillas
        aaaa<-kmeans(x = semillas1,centers = 3,iter.max = 10,nstart = 30)
        aaaa
        semillas1 <- semillas1 %>% mutate(cluster = aaaa$cluster)
        semillas1 <- semillas1 %>% mutate(cluster = as.factor(cluster),
                                          #grupo   = as.factor(grupo))
                                          grupo   = 3)
        gm1<-ggplot(data = semillas1, aes(x = semillas1$Perimetro, y = semillas1$Coeficiente_de_Asimetria, color = semillas1$cluster)) +
          geom_text(aes(label = cluster), size = 5) +
          #geom_point(aes(x = aaaa$centers[, 1], y = aaaa$centers[, 2]), color = 'black', size = 3)+
          theme_bw() +
          theme(legend.position = "none")
        
        return (gm1)
      } else 
        if(box == "2") {
          plot(semillas1$largo,semillas1$ancho)
          
          gm2<-ggplot(data =semillas1, aes(largo,ancho))+ geom_point() +   geom_smooth(method = "lm")

        return (gm2)
      } else
        if(box == "3") {
          gm3<-ggplot(data =semillas1, aes(largo,ancho))+ geom_point() +   geom_smooth()
        return (gm3)
      } else
        if(box == "4")  {
          regre_multi_semillas<-lm(data = semillas,semillas$Area~semillas$Perimetro+semillas$largo+semillas$ancho)
          gm4<-ggplot(data=regre_multi_semillas,aes(x = regre_multi_semillas$fitted.values,y = regre_multi_semillas$residuals))+
            geom_point() + geom_smooth()

        return (gm4)
        }
      if(box == "5")  {
        ## Split in train + test set
        idxs <- sample(1:nrow(semillas),as.integer(0.7*nrow(semillas)))
        trainSemillas <- semillas[idxs,]
        testSemillas <- semillas[-idxs,]
        
        ## A 3-nearest neighbours model with no normalization
        nn3 <- kNN(Tipo ~ .,trainSemillas,testSemillas,norm=FALSE,k=3)
        nn31 <- as.numeric(levels(nn3))[nn3]
        gm5<- qplot(nn31)
        
        return (gm5)
      }
      
      
        
    })
    
    observeEvent(input$control1, {
        
        if(input$control1==TRUE)
        {toggle("consulta1") } else {toggle("consulta1")}
        
    })
    
    observeEvent(input$download,
    {
      write.csv(datos_violencia,"datosprocesados.csv")
        
    })
    observeEvent(input$control2, {
        
        if(input$control2==TRUE)
        {toggle("consulta2")} else {toggle("consulta2")}
        
    })
    
    observeEvent(input$control4, {
        
        if(input$control4==TRUE)
        {toggle("consulta4")} else {toggle("consulta4")}
        
    })
    observeEvent(input$control5,{
      
      if(input$control5==TRUE)
      {toggle("consulta5")} else {toggle("consulta5")}
      
    })
    output$tablaS5 <- renderTable({
     datos_violencia[1:150,1:10]
    })
    
    })
