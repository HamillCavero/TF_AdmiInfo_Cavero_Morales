#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(corrplot)
library(ggplot2)
# Define server logic required to draw a histogram
options(shiny.maxRequestSize=110*1024^2)
shinyServer(function(input, output) {

    getwd()
    datos_violencia
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
        }
    }}}}})
    
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
        } }
        } } })
    
    
    output$consulta4 <- renderText({
      box<- input$selectgg
      if (is.null(box))
      {return(NULL)}
      
      if(box == "1")    {
        gp1<-"qplot(x=Modalidad,y=Cantidad_Casos,data = q2,geom = 'col)+aes(fill= Modalidad)"  
        return (gp1)
      } else{  if(box == "2") {
        gp2<-"qplot(x=Total,y=DPTO_CIA,data = q3,geom = 'col')+aes(fill= DPTO_CIA)"
        return (gp2)
      } else { if(box == "3") {
        gp3<-"qplot(x=SIT_PERSONA,y=Total,data = q4%>%slice(1:5),geom = 'col')+aes(fill= SIT_PERSONA)"
        return (gp3)
      } else { if(box == "4")
      { gp4<-"qplot(x=pais_natal  ,y=Total,data = q5%>%slice(1:5),geom = 'col')+aes(fill= pais_natal)"
      return (gp4)
      } else { if(box=="5")
      { gp5<-"qplot(x=EDAD   ,y=Total,data = q6,geom = 'col')+aes(fill= EDAD )"
      return (gp5)
      } 
      } }
      } }
    })
    
    output$plot1 <- renderPlot({
        box<- input$selectgg
        if (is.null(box))
        {return(NULL)}
        
        if(box == "1")    {
          gp1<-qplot(x=Modalidad,y=Cantidad_Casos,data = q2,geom = "col")+aes(fill= Modalidad)  
          return (gp1)
        } else{  if(box == "2") {
          gp2<-qplot(x=Total,y=DPTO_CIA,data = q3,geom = "col")+aes(fill= DPTO_CIA)
            return (gp2)
        } else { if(box == "3") {
           gp3<-qplot(x=SIT_PERSONA,y=Total,data = q4%>%slice(1:5),geom = "col")+aes(fill= SIT_PERSONA)
            return (gp3)
        } else { if(box == "4")
        { gp4<-qplot(x=pais_natal  ,y=Total,data = q5%>%slice(1:5),geom = "col")+aes(fill= pais_natal)
        return (gp4)
        } else { if(box=="5")
        { gp5<-qplot(x=EDAD   ,y=Total,data = q6,geom = "col")+aes(fill= EDAD )
        return (gp5)
        } 
        } }
        } } }
    )
    
    
    output$consulta6 <- renderText({
        "dtModeloR2<-data.frame(dtFinal%>%group_by(DesDpto)%>%summarise(AHumanitarFam=sum(AHumanitarFam),EDanosVivienda=sum(EDanosVivienda)))
dtModeloR2.1<-dtModeloR2%>%select(AHumanitarFam,EDanosVivienda)

regresion2 <- lm(AHumanitarFam  ~  EDanosVivienda, data = dtModeloR2)
summary(regresion2)
    
ggplot(dtModeloR2, aes(x=AHumanitarFam, y=EDanosVivienda)) + geom_point() + ggtitle('Gráfica de Regresion') + xlab('Ayuda Humanitaria por Familia') + ylab('Estimacion de Daños por Vivienda') + geom_smooth(method=lm)
    "
    })
    
    
    
    
    
    
    output$tablaS6 <- renderTable({
        
        if(is.null(input$file1)){return ()}
        return(input$file1)
    })
    
    
    
    
    output$plot8 <- renderPlot({
        
        dtModeloR2<-data.frame(dtFinal%>%group_by(DesDpto)%>%summarise(AHumanitarFam=sum(AHumanitarFam),EDanosVivienda=sum(EDanosVivienda)))
        
        return( 
            
            ggplot(dtModeloR2, aes(x=AHumanitarFam, y=EDanosVivienda)) + geom_point() + ggtitle("Gráfica de Regresion") + xlab("Ayuda Humanitaria") + ylab("Estimacion de Daños por Vivienda") + geom_smooth(method=lm)
        )
        
    })
    
    observeEvent(input$control1, {
        if(input$control1==TRUE)
        {toggle("consulta1")} else {toggle("consulta1")}
        
    })
    
    
    
    observeEvent(input$control2, {
        
        if(input$control2==TRUE)
        {toggle("consulta2")} else {toggle("consulta2")}
        
    })
    
    
    observeEvent(input$control4, {
        
        if(input$control4==TRUE)
        {toggle("consulta4")} else {toggle("consulta4")}
        
    })
  
    
    
    observeEvent(input$control6, {
        
        if(input$control6==TRUE)
        {toggle("consulta6")} else {toggle("consulta6")}
        
    })
    
    
    output$tablaS5 <- renderTable({
        #Pro<- read.csv(file = 'datasets/SIDPOL_2017_Violencia_familiar.csv')
        if (is.null(datos_violencia)) 
            return(NULL)
        else head(datos_violencia)

})})
