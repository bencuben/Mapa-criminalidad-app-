library(shiny)
library(rgdal)
library(leaflet)
library(raster)
library(colorRamps)
library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(car)


load("Entorno_general_completo_6")


shinyUI(
  
  
  navbarPage(title = tags$b("Criminalidad"),id="navibar",
    
        
             
     # ------------------------------------ tabulacion de Inducción------------------------------------------------------                  
             
    tabPanel(title="Sobre la App",
             
             #se agrega el tema de la pagina
             
             fluidPage(theme = shinytheme("cerulean"),
                       
                       
                       fluidRow(
                         column(3,img(src="logo_u.png", height=150, width=250)),
                         column(8,tags$p(tags$h1("CRIMEN.STAT",align="center"),
                                         tags$br(),
                                         tags$p("La problemática sobre el orden público en el país ha sido de gran importancia para toda la comunidad en general, es por ello que se crea la necesidad de conocer más a fondo sobre todos estos flagelos. Sin embargo es un tema de gran extensión, es por ello que se decidió trabajar con tres conjuntos de datos alojados en la página de DATOS ABIERTOS, lo cuales nos permiten darle una mirada a un mundo nuevo de posibilidades que nos brindan el análisis de datos y las aplicaciones web y aprovechar todas estas herramientas e información para crear una cultura social, además de educar en las personas que desconocen el tema y adicionalmente permite a los entes gubernamentales encargados tomar mejores decisiones respecto a estas problemáticas. ",tags$br(),
                                         tags$br("Esta aplicación fue desarrollada con el fin de proporcionar información clara  y oportuna sobre algunos de los eventos delictivos del país o acciones realizadas por las autoridades judiciales, además de proporcionar una alternativa didáctica y dinámica para la divulgación de información a partir de estadísticas descriptivas interactuando con aplicaciones web"),tags$br(),
                                         
                                                tags$b("Brahian Cano Urrego"),tags$br(),tags$b("Yeison Yovany Ocampo Naranjo"),align="center"),
                                         tags$p("en acompañamiento del profesor:",tags$b("Freddy Hernández Barajas"),align="center")))
                         
                       ),
                       
                       #se agregan los correos de contacto con un separador de color del tema
                       
                       tags$hr(size=20,style="border-color: #2FA4E7;"),
                       tags$p("correo1: ",tags$a(href="mailto:bcanou@unal.edu.co", "bcanou@unal.edu.co"),tags$br(),
                              "correo2:",tags$a(href="mailto:yyocampon@unal.edu.co", "yyocampon@unal.edu.co") ,align="center"   )
                       
                        )
                      ), 

     # ------------------------------------Primera tabulacion ------------------------------------------------------

             
    tabPanel(title= "Recuperacion de carros",
      
      
      #titulo de la app
    
      
      
      fluidPage(theme = shinytheme("cerulean"),
        
      #Titulo+imagen+e intruccion el app  
        
        fluidRow(
          column(3,img(src="logo_u.png", height=150, width=250)),
          column(8,tags$p(tags$h1("CRIMEN.STAT",align="center"),
                          tags$br(),
                          tags$p("En esta pestaña podrás encontrar información acerca de las recuperaciones de vehículos efectuada por la Policía Nacional, además podrás elaborar con algunos de los botones dispuestos para tu segmentación las características que te interesan.",tags$br(),
                                 tags$b("Brahian Cano Urrego"),tags$br(),tags$b("Yeison Yovany Ocampo Naranjo"),align="center"),
                          tags$p("en acompañamiento del profesor:",tags$b("Freddy Hernández Barajas"),align="center")))
          
        ),
        
      #CONTENIDO DEL MAPA
        
        fluidRow(
        sidebarLayout(
          sidebarPanel(tags$style(".well {background-color: #2FA4E7;}"),
            
            #Texto introductorio sobre las frecuencias           
                                  
            tags$p(tags$h4("Frecuencias",align="center",style="color:#FFFFFF"),tags$br(),
                              tags$b("Seleccione el recuadro para ver las frecuencias en forma relativa respecto a la cantidad de habitantes de cada departamento")),            
            
            #Creación de check box de frecuencias
            
            checkboxInput(inputId="frecuencias1",label=tags$p("Frecuencia Relativa",align="center",style="color:#FFFFFF")),
            
            
            #filtro
            
            selectInput(inputId="franjacarros", label=tags$p(tags$h4("Franja",align="center",style="color:#FFFFFF"),tags$br(),
                        tags$p("Las franjas horarias te permiten observar la frecuencia de hurtos es determinada hora del día")),  
                        
                        choices= c("SIN FILTRO", levels(as.factor(datos.carros$franja))),
                            selected="SIN FILTRO")
            
            ),
          
          #mapa1
          
          mainPanel(
            leafletOutput(outputId = "mapa1",height = 500,width = "90%")
              )
            )
          ),
    
      #TEXTO SEPARATIVO
      
      fluidRow(
        column(3,""),
        column(8,titlePanel(tags$p(tags$h2("ESTADISTICAS",align="center"),tags$p("A continuación se presentan algunas gráficas descriptivas con los ítem más relevantes de la recuperación de autos.",tags$br(),tags$br(),align="center"))))
      ),
    
    #GRAFICOS DESCRIPTIVOS
    
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          #filtro
          
          selectInput(inputId="departamento1",label=tags$p(tags$h4("Departamento",align="center",style="color:#FFFFFF"),tags$br(),
                                                            tags$p("En esta opción podrás elegír que departamento usar para los gráficos descriptivos")),
                      choices=levels(datoshomit$Departamento),
                      selected="ANTIOQUIA")
          
        ),
        
        
        mainPanel(
          
            plotOutput("grafico1",width = "100%"),
            plotOutput("grafico2",width = "100%"),
            plotOutput("grafico3",width = "100%")
            
        )
      )
    ),
    tags$hr(size=20,style="border-color: #2FA4E7;"),
    tags$p("correo1: ",tags$a(href="mailto:bcanou@unal.edu.co", "bcanou@unal.edu.co"),tags$br(),
           "correo2:",tags$a(href="mailto:yyocampon@unal.edu.co", "yyocampon@unal.edu.co") ,align="center"   )
    
    
    
    
  )
),   


    # ----------------------------------------Segunda tabulacion ------------------------------------------------------

    
  tabPanel(title="Homicidios en accidentes de tránsito",
              
     
                      
      fluidPage(
        
        #Titulo+imagen+e intruccion el app  
        
        fluidRow(
          column(3,img(src="logo_u.png", height=150, width=250)),
          column(8,tags$p(tags$h1("CRIMEN.STAT",align="center"),
                          tags$br(),
                          tags$p("En esta pestaña se aborda la frecuencia de los homicidios en accidentes de tránsito en cada uno de los diferentes departamentos del país, adicionalmente encontrarás una serie de botones que te permitirán hacer una segmentación de los casos que te interesan. ",tags$br(),
                                 tags$b("Brahian Cano Urrego"),tags$br(),tags$b("Yeison Yovany Ocampo Naranjo"),align="center"),
                          tags$p("en acompañamiento del profesor:",tags$b("Freddy Hernández Barajas"),align="center")))
          
        ),
        
        #CONTENIDO DEL MAPA
        
        fluidRow(
        sidebarLayout(
          sidebarPanel(
            
            #Texto introductorio sobre las frecuencias           
            
            tags$p(tags$h4("Frecuencias",align="center",style="color:#FFFFFF"),tags$br(),
                   tags$b("Seleccione el recuadro para ver las frecuencias en forma relativa respecto a la cantidad de habitantes de cada departamento")),            
            
            #Creación de check box de frecuencias
            
            checkboxInput(inputId="frecuencias2",label=tags$p("Frecuencia Relativa",align="center",style="color:#FFFFFF")),
            
            
            #Filtro por franja horaria

            selectInput(inputId="franjahomit", label=tags$p(tags$h4("Franja",style="color:#FFFFFF",align="center"),tags$br(),
                                                            tags$p("Las franjas horarias te permiten observar la frecuencia de hurtos es determinada hora del día")),
                        choices= c("SIN FILTRO", levels(as.factor(datoshomit$franja))),
                            selected="SIN FILTRO"),

            # Filtro para el rango de edades

            sliderInput("edadhomit",label =tags$p(tags$h4("Rango de edad",style="color:#FFFFFF",align="center"),tags$br(),tags$p("Escoge el rango de edades que te interese observar")),
                        min = 0,max = 93,value = c(0,93) ),

            # Filtro para los lugares donde ocurrieron los homicidios

            selectInput(inputId="clasehomit", label=tags$p(tags$h4("Clase de sitio",style="color:#FFFFFF",align="center"), tags$br(),
                        tags$p("El lugar donde se reportó el homicidio es de vital importancia, escoge el que desees:")),
            choices=  c("SIN FILTRO",levels(datoshomit$Clase.de.sitio)),
                            selected="SIN FILTRO")
            ),
          
          #mapa 2
          
          mainPanel(
            leafletOutput(outputId = "mapa2",height = 500,width = "90%")
          )
          
          
        )
      ),
      
      #TEXTO SEPARATIVO
      
      fluidRow(
        column(3,""),
        column(8,titlePanel(tags$p(tags$h2("ESTADISTICAS",align="center"),tags$p("A continuación se presentan algunas gráficas descriptivas con los ítem más relevantes de homicidios en accidentes de tránsito.",tags$br(),tags$br(),align="center"))))
      ),
      
      #GRAFICOS DESCRIPTIVOS
      
      fluidRow(
        sidebarLayout(
          sidebarPanel(
            #filtro
            
            selectInput(inputId="departamento2",label=tags$p(tags$h4("Departamento",align="center",style="color:#FFFFFF"),tags$br(),
                                                             tags$p("En esta opción podrás elegír que departamento usar para los gráficos descriptivos")),
                        choices=levels(datoshomit$Departamento),
                        selected="ANTIOQUIA")
            
          ),
          
          #Graficos descriptivos
          mainPanel(
            
            plotOutput("grafico4",width = "100%"),
            plotOutput("grafico5",width = "100%")
            
            
          )
        )
      ),
      
      tags$hr(size=20,style="border-color: #2FA4E7;"),
      tags$p("correo1: ",tags$a(href="mailto:bcanou@unal.edu.co", "bcanou@unal.edu.co"),tags$br(),
             "correo2:",tags$a(href="mailto:yyocampon@unal.edu.co", "yyocampon@unal.edu.co") ,align="center"   )
      
      
      )
  
),     

    # -------------------------------tercera tabulacion ------------------------------------------------------

    
    tabPanel(title="Hurto a Peatones",
             

             
             fluidPage(

               #Titulo+imagen+e intruccion el app  
               
               fluidRow(
                 column(3,img(src="logo_u.png", height=150, width=250)),
                 column(8,tags$p(tags$h1("CRIMEN.STAT",align="center"),
                                 tags$br(),
                                 tags$p("En esta pestaña encontrarás información acerca del número de hurtos reportados hasta el mes de abril del 2018, además puedes encontrar algunos botones que te permiten filtrar y analizar  tus características de interés",tags$br(),
                                        tags$b("Brahian Cano Urrego"),tags$br(),tags$b("Yeison Yovany Ocampo Naranjo"),align="center"),
                                 tags$p("en acompañamiento del profesor:",tags$b("Freddy Hernández Barajas"),align="center")))
                 
               ),
               
               #CONTENIDO DEL MAPA
               
               fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   
                   #Texto introductorio sobre las frecuencias           
                   
                   tags$p(tags$h4("Frecuencias",align="center",style="color:#FFFFFF"),tags$br(),
                          tags$b("Seleccione el recuadro para ver las frecuencias en forma relativa respecto a la cantidad de habitantes de cada departamento")),            
                   
                   #Creación de check box de frecuencias
                   
                   checkboxInput(inputId="frecuencias3",label=tags$p("Frecuencia Relativa",align="center",style="color:#FFFFFF")),
                   
                   
                   #Filtro por franja horaria
                    
                   selectInput(inputId="franjahurto", label=tags$p(tags$h4("Franja",style="color:#FFFFFF",align="center"),tags$br(),
                               tags$p("Las franjas horarias te permiten observar la frecuencia de hurtos es determinada hora del día")),
                   choices= c("SIN FILTRO", levels(as.factor(datoshurto$franja))),
                                selected="SIN FILTRO"),
                    
                    # Filtro para el rango de edades
                    
                   sliderInput("edadhurto",label = tags$p(tags$h4("Rango de edad",style="color:#FFFFFF",align="center"),tags$br(),
                               tags$p("Determina el rango de edades que deseas observar:")),
                   min = 0,max = 116,value = c(0,116) )
                  
                 ),
                 
                 #mapa 3
                 mainPanel(
                   leafletOutput(outputId = "mapa3",height = 500,width = "90%")
                   
                 
                 )
               )
             ),
             
             #TEXTO SEPARATIVO
             
             fluidRow(
               column(3,""),
               column(8,titlePanel(tags$p(tags$h2("ESTADISTICAS",align="center"),tags$p("A continuación se presentan algunas gráficas descriptivas con los ítem más relevantes de hurto a personas.",tags$br(),tags$br(),align="center"))))
             ),
             
             
             #GRAFICOS DESCRIPTIVOS
             
             fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   #filtro
                   
                   selectInput(inputId="departamento3",label=tags$p(tags$h4("Departamento",align="center",style="color:#FFFFFF"),tags$br(),
                                                                    tags$p("En esta opción podrás elegír que departamento usar para los gráficos descriptivos")),
                               choices=levels(datoshomit$Departamento),
                               selected="ANTIOQUIA")
                   
                 ),
                 
                 #Graficos descriptivos
                 mainPanel(
                   plotOutput("grafico6",width = "100%"),
                   plotOutput("grafico7",width = "100%")
                   
                   
                 )
               )
             ),
             tags$hr(size=20,style="border-color: #2FA4E7;"),
             tags$p("correo1: ",tags$a(href="mailto:bcanou@unal.edu.co", "bcanou@unal.edu.co"),tags$br(),
                    "correo2:",tags$a(href="mailto:yyocampon@unal.edu.co", "yyocampon@unal.edu.co") ,align="center"   )
             
             
             
             )
             
             ),
# -------------------------------link hacia el github de la app ------------------------------------------
       
        
        tabPanel(title="",icon=icon("github","fa-2x"),value="home")



)
)
  