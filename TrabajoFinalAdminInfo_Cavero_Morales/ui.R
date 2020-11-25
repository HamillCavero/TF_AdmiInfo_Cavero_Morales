#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("shinythemes")
library("shinyjs")
# Define UI for application that draws a histogram
shinyUI<-fluidPage(
    theme = shinytheme("cyborg"),
    navbarPage( "Proyecto sobre Violencia Familiar-Obtenido de Datos abiertos",
                tabPanel("Presentacion",fluidRow(
                    column(12,align="center",h3("Universidad Peruana de Ciencias Aplicadas"),
                           tags$b(h4("proyecto demostracion")))
                )),
                tabPanel("Recoleccion",
                sidebarLayout (
                sidebarPanel (
                selectInput("select", label = h3("Tipo de archivo: "), 
                            choices = c('text'='1','csv'='2','xlsx'='3')),hr(),
                        fileInput("file1", "Seleccione Archivo",accept = c("text/csv","text/json",
                                 "text/comma-separated-values,text/plain",".csv","XLSX file", 
                                 ".json",".xlsx",".xls",".xml")),
                                 helpText ( " Max. Tamaño de archivo: 110MB " )),
                                 mainPanel(
                                 tabsetPanel(  tabPanel("Info",tableOutput("tablaS6")),
                                               tabPanel("Data",tableOutput("tablaS2"))
                                 ))))
                
                ,tabPanel("Preprocesamiento",
                          navlistPanel(
                              tabPanel("Limpieza e Imputacion",
                                       h4("Unir Dataset's Separados"),hr(),checkboxInput("control1", "Mostrar Codigo", FALSE),hr(),
                                       verbatimTextOutput("consulta1"), tableOutput("tablaS5"))
                              
                          )
                )
                
                ,tabPanel('Consultas Exploracion',
                          navlistPanel(
                              tabPanel("Consultas DPLYR",
                                       h4("DPLYR"), hr(),selectInput("selectdp", label = NULL, choices = c('consulta 1'='1','consulta 2'='2','consulta 3'='3','consulta 4'='4','consulta 5'='5')),checkboxInput("control2", "Mostrar Codigo", FALSE),hr(),verbatimTextOutput("consulta2"),dataTableOutput("tablaS3"))
                              )
                              
                          )
                
                
                ,tabPanel('Graficos',
                          navlistPanel( 
                              tabPanel("Graficos GGPLOT2",h4("GGPLOT2"),hr(),selectInput("selectgg", label = NULL, choices = c('consulta 1'='1','consulta 2'='2','consulta 3'='3','consulta 4'='4','consulta 5'='5')),checkboxInput("control4", "Mostrar Codigo", FALSE),hr(),verbatimTextOutput("consulta4"),plotOutput('plot1'))
                              )
                
                ,tabPanel('Modelo',
                          navlistPanel(  
                              tabPanel("Ayuda vs Estimacion de Daño",hr(),selectInput("selectmod",label=NULL,choices = c('modelo 1'='1','modelo 2'='2','modelo 3'='3','modelo 4'='4')),checkboxInput("control6", "Mostrar Codigo", FALSE),verbatimTextOutput("consulta6"),hr(), plotOutput("plot8"))  ) )
    )
))


