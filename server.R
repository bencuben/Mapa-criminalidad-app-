library(shiny)
library(rgdal)
library(leaflet)
library(raster)
library(colorRamps)
library(dplyr)
library(ggplot2)
library(car)

#cargar las bases de datos previamente procesadas

load("Entorno_general_completo_6")


#definicion del server

shinyServer(function(input, output) {
  
  #forma reactiva de redirigir a una pagina
  
  observeEvent(input$navibar,{
    if(input$navibar == "home"){
      browseURL("https://github.com/bencuben/Mapa-criminalidad-app-" )
      
    }
  })
  
   
  #-------------------creacion de los atributos del mapa de recuperaciones de automotores 2018-----------
  
  output$mapa1 <- renderLeaflet({
    
    #caso base
    mapa@data$frec1<-as.vector(table(datos.carros$Departamento))
    
    #creacion de los filtro pertinententes para los carritos
    
      if(input$franjacarros!="SIN FILTRO"){
        aux1<-datos.carros %>%  filter(datos.carros$franja==input$franjacarros)
        
      }else{
        aux1<-datos.carros
      }
      
    
    
    #condicional sobre las frecuencias relativas o absolutas
    
    if(input$frecuencias1==TRUE){
      mapa@data$frec1<-as.vector(round(table(aux1$Departamento)/poblacion*100000,2))
      texto<-"Proporcion de recuperaciones por cada 100.000:"
      
    }else{
      
      mapa@data$frec1<-as.vector(table(aux1$Departamento))
      texto<-"Numero de recuperaciones:"
    }
    
    #atributos del mapa
      
    pal <-colorNumeric(palette=blue2red(max(mapa@data$frec1)),domain=c(0,mapa@data$frec1))
    popup<-paste(mapa@data$admin1RefN,paste(texto, mapa@data$frec1,sep = "<br/>"),sep="<br/>")
    
    
    m1<-leaflet(data = mapa) %>% addTiles() %>% addProviderTiles(providers$OpenStreetMap) %>% 
      addPolygons(popup=popup,weight = 2, color = pal(as.integer(mapa@data$frec1)),opacity = 0.5,fillOpacity = 0.5) %>% addLegend("topright",pal=pal,values=mapa@data$frec1,title="Frecuencias",
    
                                                                                                                                                                                                                                                               opacity = 1)
    m1

    
  })
  
  #-------------------creacion de los atributos del mapa de homicidios en el transito 2018-----------
  
  output$mapa2 <- renderLeaflet({
    
    #caso base
    
    mapa@data$frec2<-as.vector(table(datoshomit$Departamento))
    
    
    
    #creacion de  los filtro pertinentes para los homicidios
    
    
    #filtro1
    if(input$franjahomit!="SIN FILTRO"){
      aux2.1<-datoshomit %>%  filter(datoshomit$franja==input$franjahomit)
      
    }else{
      aux2.1<-datoshomit}
      
      
    #filtro2
      aux2.2<-aux2.1 %>%  filter(aux2.1$Edad>=input$edadhomit[1] & aux2.1$Edad<=input$edadhomit[2])
    
      
      
    #filtro3
      
      if(input$clasehomit!="SIN FILTRO"){
        aux2.3<- aux2.2 %>%  filter(aux2.2$Clase.de.sitio==input$clasehomit)
        
      }else{
        aux2.3<-aux2.2
      } 
     
    
    
    #pregunta si son frecuencias relativas
      
    if(input$frecuencias2==TRUE){
      mapa@data$frec2<-as.vector(round(table(aux2.3$Departamento)*100000/poblacion,2))
      texto<-"Proporcion de recuperaciones por cada 100.000:"
      
    }else{
      
      mapa@data$frec2<-as.vector(table(aux2.3$Departamento))
      texto<-"Numero de recuperaciones:"
    }
    
    
      
    
    
    #atributos del mapa
      
    pal <-colorNumeric(palette=blue2red(max(mapa@data$frec2)),domain=c(0,mapa@data$frec2))
    popup<-paste(mapa@data$admin1RefN,paste(texto, mapa@data$frec2,sep="<br/>"),sep="<br/>")
    
    
    m2<-leaflet(data = mapa) %>% addTiles() %>% addProviderTiles(providers$OpenStreetMap) %>% 
      addPolygons(popup=popup,weight = 2, color = pal(as.integer(mapa@data$frec2)),opacity = 0.5,fillOpacity = 0.5) %>% addLegend("topright",pal=pal,values=mapa@data$frec2,title="Frecuencias",
                                                                                                                                  opacity = 1)
    m2
    })
  
  
  
  #-------------------creacion de los atributos del mapa de hurtos a personas 2018-----------
  
  
  output$mapa3 <- renderLeaflet({
    
    #caso base
    
    mapa@data$frec3<-as.vector(table(datoshurto$Departamento))
    
    
    #creacion de  los filtro pertinentes para los hurtos
    
    
    #filtro1
    
    if(input$franjahurto!="SIN FILTRO"){
      aux3.1<-datoshurto %>%  filter(datoshurto$franja==input$franjahurto)
      
    }else{
      aux3.1<-datoshurto}
    
    
    #filtro2
    
    aux3.2<-aux3.1 %>%  filter(aux3.1$Edad>=input$edadhurto[1] & aux3.1$Edad<=input$edadhurto[2])
    
    
    #condicional sobre frecuencia relativa o absoluta
    
    
    if(input$frecuencias3==TRUE){
      mapa@data$frec3<-as.vector(round(table(aux3.2$Departamento)*10000/poblacion,2))
      texto<-"Proporcion de recuperaciones por cada 10.000:"
      
    }else{
      
      mapa@data$frec3<-as.vector(table(aux3.2$Departamento))
      texto<-"Numero de recuperaciones:"
    }
    
    #atributos del mapa
    
    
    pal <-colorNumeric(palette=blue2red(max(mapa@data$frec3)),domain=c(0,mapa@data$frec3))
    popup<-paste(mapa@data$admin1RefN,paste(texto, mapa@data$frec3,sep="<br/>"),sep="<br/>")
    
    
    m3<-leaflet(data = mapa) %>% addTiles() %>% addProviderTiles(providers$OpenStreetMap) %>% 
      addPolygons(popup=popup,weight = 2, color = pal(as.integer(mapa@data$frec3)),opacity = 0.5,fillOpacity = 0.5) %>% addLegend("topright",pal=pal,values=mapa@data$frec3,title="Frecuencias",
                                                                                                                                  opacity = 1)
    m3
  })
  
  
 #------------------------------ renderizar los graficos GGPLOT2 -------------------------------------------
  
  #grafico 1 
 output$grafico1<-renderPlot({
   
   #Verifica que el reemplazamiento no sea vacio
   
   if(nrow(datos.c[which(datos.c$Departamento==input$departamento1),])>0){
     
   datos.c[which(datos.c$Departamento==input$departamento1),]$recode<-input$departamento1
   }
   
   #asigan el complemento como resto de colombia
   
   datos.c[which(datos.c$Departamento!=input$departamento1),]$recode<-"RESTO DE COLOMBIA"
   
   #organiza los niveles para el plot
   datos.c$recode <- factor(datos.c$recode,levels =c(input$departamento1,"RESTO DE COLOMBIA") )
   
   ggplot(datos.c, aes(datos.c$Dia,fill=datos.c$recode)) + 
     geom_bar(position = "dodge")+ theme_classic()+ggtitle("Numero de incidentes")+
     xlab("Dias")+ylab("Numero de vehiculos recuperados")+
     theme(plot.title = element_text(hjust = 0.5))+
     labs(fill="")+scale_fill_manual(values=c("#2FA4E7", "gray"))
   
 })
 
 
 #grafico 2
 
 output$grafico2<-renderPlot({
   
   #Verifica que el reemplazamiento no sea vacio
   
   if(nrow(datos.c[which(datos.c$Departamento==input$departamento1),])>0){
   datos.c[which(datos.c$Departamento==input$departamento1),]$recode<-input$departamento1
   }
   
   #asigan el complemento como resto de colombia
   
   datos.c[which(datos.c$Departamento!=input$departamento1),]$recode<-"RESTO DE COLOMBIA"
   
   #organiza los niveles para el plot
   
   datos.c$recode <- factor(datos.c$recode,levels =c(input$departamento1,"RESTO DE COLOMBIA") )
   
   
   ggplot(datos.c, aes(datos.c$Zona,fill=datos.c$recode)) + 
     geom_bar(position = "dodge")+theme_classic()+ggtitle("Numero de incidentes por zona")+
     xlab("Zona")+ylab("Numero de casos")+
     theme(plot.title = element_text(hjust = 0.5))+
     labs(fill="")+scale_fill_manual(values=c("#2FA4E7", "gray"))
 })

 
 #grafico 3
 
 output$grafico3<-renderPlot({
   
   
   if(nrow(datos.cfiltrado[which(datos.cfiltrado$Departamento==input$departamento1),])>0){
   datos.cfiltrado[which(datos.cfiltrado$Departamento==input$departamento1),]$recode<-input$departamento1
   }
   
   datos.cfiltrado[which(datos.cfiltrado$Departamento!=input$departamento1),]$recode<-"RESTO DE COLOMBIA"
   
   datos.cfiltrado$recode <- factor(datos.cfiltrado$recode,levels =c(input$departamento1,"RESTO DE COLOMBIA") )
   
   ## Clase de sitio
   #se organizan los niveles descendentemente segun la frecuencia
   
   datos.cfiltrado$Clase.de.sitio<- factor(datos.cfiltrado$Clase.de.sitio,
                                           levels=attributes(sort(table(datos.cfiltrado[which(datos.cfiltrado$recode==input$departamento1),]$Clase.de.sitio),
                                                                  decreasing = T))$dimnames[[1]] )
   
   ggplot(datos.cfiltrado, aes(datos.cfiltrado$Clase.de.sitio,fill=datos.cfiltrado$recode)) + 
     geom_bar(position = "dodge")+ theme_classic()+ggtitle("Numero de incidentes por zona")+ xlab("Tipo de sitio")+
     ylab("Numero de casos")+theme(plot.title = element_text(hjust = 0.5))+
     labs(fill="")+scale_fill_manual(values=c("#2FA4E7", "gray"))+ theme(axis.text.x = element_text(angle=0,size=10),
                                                                         axis.text.y = element_text(angle=0))
   
   
 })
 
 #### Gráficos de la segunda página (mapa 2)
 
 # Gráfico de días de la semana

 ## Gráfico del dia 
 output$grafico4<-renderPlot({
   
   if(nrow(datos.h[which(datos.h$Departamento==input$departamento2),])>0){
     
   datos.h[which(datos.h$Departamento==input$departamento2),]$recode<-input$departamento2
   }
   
   datos.h[which(datos.h$Departamento!=input$departamento2),]$recode<-"RESTO DE COLOMBIA"
   datos.h$recode <- factor(datos.h$recode,levels =c(input$departamento2,"RESTO DE COLOMBIA") )
   
   datos.h.new<- datos.h[which(!is.na(datos.h$Departamento)),]
   
   ggplot(datos.h.new,aes(datos.h.new$Dia,fill=datos.h.new$recode))+
     geom_bar(position = "dodge")+ theme_classic()+ggtitle("Numero de incidentes")+
     xlab("Dias")+ylab("Numero de vehiculos recuperados")+
     theme(plot.title = element_text(hjust = 0.5))+
     labs(fill="")+scale_fill_manual(values=c("#2FA4E7", "gray"))
   
   
 })

 ## Gráfico del sexo
 
 output$grafico5<-renderPlot({
   
   
   if(nrow(datos.h[which(datos.h$Departamento==input$departamento2),])>0){
     
   datos.h[which(datos.h$Departamento==input$departamento2),]$recode<-input$departamento2
   }
   
   datos.h[which(datos.h$Departamento!=input$departamento2),]$recode<-"RESTO DE COLOMBIA"
   datos.h$recode <- factor(datos.h$recode,levels =c(input$departamento2,"RESTO DE COLOMBIA") )
   
   datos.h.new<- datos.h[which(!is.na(datos.h$Departamento)),]
   
   ggplot(datos.h.new,aes(datos.h.new$Sexo,fill=datos.h.new$recode))+
     geom_bar(position = "dodge")+ theme_classic()+ggtitle("Numero de homicidios")+
     xlab("Sexo")+ylab("Numero de personas fallecidas")+
     theme(plot.title = element_text(hjust = 0.5))+
     labs(fill="")+scale_fill_manual(values=c("#2FA4E7", "gray"))
   
   
 })
 
 #### Gráficos de la segunda página (mapa 2)
 
 # Gráfico de días de la semana
 
 ## Gráfico del dia 
 
 output$grafico6<-renderPlot({
   
   if(nrow(datos.hurto[which(datos.hurto$Departamento==input$departamento3),])>0){
     
   datos.hurto[which(datos.hurto$Departamento==input$departamento3),]$recode<-input$departamento3
   }
   
   datos.hurto[which(datos.hurto$Departamento!=input$departamento3),]$recode<-"RESTO DE COLOMBIA"
   datos.hurto$recode <- factor(datos.hurto$recode,levels =c(input$departamento3,"RESTO DE COLOMBIA") )
   
   datos.hurto.new<- datos.hurto[which(!is.na(datos.hurto$Departamento)),]
   
   ggplot(datos.hurto.new,aes(datos.hurto.new$Dia,fill=datos.hurto.new$recode))+
     geom_bar(position = "dodge")+ theme_classic()+ggtitle("Numero de incidentes")+
     xlab("Dias")+ylab("Numero de vehiculos recuperados")+
     theme(plot.title = element_text(hjust = 0.5))+
     labs(fill="")+scale_fill_manual(values=c("#2FA4E7", "gray"))

 })
 
 ## Gráfico del sexo
 
 output$grafico7<-renderPlot({
   
   if(nrow(datos.hurto[which(datos.hurto$Departamento==input$departamento3),])>0){
   
   datos.hurto[which(datos.hurto$Departamento==input$departamento3),]$recode<-input$departamento3
   }
   
   datos.hurto[which(datos.hurto$Departamento!=input$departamento3),]$recode<-"RESTO DE COLOMBIA"
   datos.hurto$recode <- factor(datos.hurto$recode,levels =c(input$departamento3,"RESTO DE COLOMBIA") )
   
   datos.hurto.new<- datos.hurto[which(!is.na(datos.hurto$Departamento)),]
   
   ggplot(datos.hurto.new,aes(datos.hurto.new$Sexo,fill=datos.hurto.new$recode))+
     geom_bar(position = "dodge")+ theme_classic()+ggtitle("Numero de hurtos por persona.")+
     xlab("Sexo")+ylab("Numero de hurtos")+
     theme(plot.title = element_text(hjust = 0.5))+
     labs(fill="")+scale_fill_manual(values=c("#2FA4E7", "gray"))
   
   
 })
 
})
