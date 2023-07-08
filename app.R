library(here)
library(readxl)
library(tidyverse)
library(sf)
library(shiny)
library(ggiraph)
library(networkD3)
library(shinyjs)

#CARGA DATOS
datos <- read.csv2(here("Datos","aprovechamientos_app.csv"),encoding='utf-8')

#LIMPIAR DATOS PARA LA APP
datos_c1 <- datos %>%
            group_by(codcuenca1, uso,destino) %>%
            summarize(n_reg = n(),v_anual = sum(vol_anual)) %>%
            rename("codcuenca"="codcuenca1")

datos_c2 <- datos %>%
            group_by(codcuenca2, uso,destino) %>%
            summarize(n_reg = n(),v_anual = sum(vol_anual)) %>%
            rename("codcuenca"="codcuenca2")

#SE CARGAN LOS MAPAS
mapa_c1 <- st_read(here("Mapas","mapa_c1.shp"))
mapa_c2 <- st_read(here("Mapas","mapa_c2.shp"))


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f5f5;
        width: 100%;
      }
      
      h1 {
        color: #ffffff; 
        font-weight: bold;
      }
      
      h3 {
        margin: 5px;
      }
      
      .container-fluid {
        padding-left: 0px;
      }
      
      .encabezado {
        background-color: #004e80; 
        color: #ffffff;
        margin-bottom: 10px;
        padding: 5px;
      }
      
      .col-sm-3{
        margin-left:15px;
      }
      
      .mapa {
        background-color: #ffffff;
        border-radius: 10px; 
        margin-left: 15px;
      }
      
      .tabla {
        background-color: #ffffff;
        border-radius: 10px; 
      }
    "))
  ),
  
  useShinyjs(),
  
  fluidRow(
    column(
      12,
      div(
        class = "encabezado",
        h1("Visualizador de aprovechamiento de recursos hídricos en Uruguay"),
        actionButton("info", "Información")
      )
    )
  ),
  
  fluidRow(
    column(
      3,
      selectInput(
        "mapa_selector",
        "Visualizar por nivel de cuenca:",
        choices = c("Cuencas nivel 1", "Cuencas nivel 2"),
        selected = "Cuencas nivel 2"
      )
    ),
    column(
      6,
      selectInput(
        "uso_selector",
        "Filtrar por uso:",
        choices = c("Todos", "Riego", "Industrial", "Otros Usos Agropecuarios", "Otros Usos"),
        selected = "Todos"
      )
    )
  ),
  
  fluidRow(
    column(
      5,
      div(
        class = "mapa",
        h3("Uso principal por cuenca hidrográfica"),
        girafeOutput("mapa")
      )
    ),
    column(
      6,
      div(
        class = "tabla",
        h3("Distribución del volumen anual por uso"),
        sankeyNetworkOutput("tabla")
      )
    )
  )
)

#SE DEFINE EL SERVER
server <- function(input, output, session) {
  
  mapa <- reactive({
    switch(input$mapa_selector,
           "Cuencas nivel 1" = mapa_c1,
           "Cuencas nivel 2" = mapa_c2
    )
  })
  
  datos <- reactive({
    switch(input$mapa_selector,
           "Cuencas nivel 1" = datos_c1 %>% 
             filter(uso %in% ifelse(input$uso_selector == "Todos", unique(uso), input$uso_selector)),
           "Cuencas nivel 2" = datos_c2 %>% 
             filter(uso %in% ifelse(input$uso_selector == "Todos", unique(uso), input$uso_selector))
    )
  })

  output$mapa <- renderGirafe({

    reg_cuenca <- datos() %>%
      group_by(codcuenca) %>%
      summarise(n_reg = sum(n_reg)) %>%
      select(codcuenca,n_reg)
    
    uso_cuenca <- datos() %>%
      group_by(codcuenca,uso,destino) %>%
      summarise(v_anual = sum(v_anual)) %>%
      group_by(codcuenca) %>%
      filter(v_anual == max(v_anual)) %>%
      select(codcuenca,uso,destino)
    
    mapa_fill <- ifelse(input$uso_selector == "Todos","uso","destino")
    
    df_mapa <- mapa()
    
    df_mapa <- left_join(df_mapa,reg_cuenca,by="codcuenca") %>%
               left_join(uso_cuenca,by="codcuenca",multiple = "all")
    
    #HAGO EL MAPA
    mapa <- ggplot() +
      geom_sf_interactive(data = df_mapa, color = "black",
                          aes(tooltip = paste(nomcuenca, "\nCantidad de registros: ", n_reg,
                                              "\nUso principal: ", uso,
                                              "\nDestino principal: ", destino),fill=!!sym(mapa_fill),
                              data_id = codcuenca),
                          hover_css = "fill:darkblue;", size = 1.5, alpha = 0.8) +  
      theme_void()
    
    #LO VUELVO INTERACTIVO CON ggiraph    
    girafe(ggobj = mapa)
  })
  
  output$tabla <- renderSankeyNetwork({
  
    #DEFINO LOS DATOS QUE SE USAN DEPENDIENDO DEL MAPA  
    if (is.null(input$mapa_selected)) {
      datos_sk <- datos()
    } else {
      datos_sk <- datos() %>%
      filter(codcuenca %in% input$mapa_selected)
    }
    
    datos_sankey <- datos_sk %>%
                    ungroup() %>%
                    group_by(uso,destino) %>%
                    summarise(vol_anual = sum(v_anual)) 
    
    #CREO LOS LINKS DEL DIAGRAMA SANKEY
    links_sankey <- data.frame(
      source = c(rep("Volumen Anual Disponible",length(datos_sankey$uso)),
                 datos_sankey$uso), 
      target = c(datos_sankey$uso,datos_sankey$destino), 
      value = c(datos_sankey$vol_anual,datos_sankey$vol_anual)  
    ) 
    
    #DEFINO LOS LINKS EN CASO QUE SE SELECCIONE TODOS O SE FILTRE POR USO
    if(input$uso_selector != "Todos"){links_sankey  <- filter(links_sankey,source != "Volumen Anual Disponible")}
    else{links_sankey  <- filter(links_sankey,target != "Otros")}
    
    #DEFINO LOS NODOS DEL DIAGRAMA
    nodos_sankey <- data.frame(
      name=c(as.character(links_sankey$source), 
             as.character(links_sankey$target)) %>% unique()
    )
    links_sankey$IDsource <- match(links_sankey$source, nodos_sankey$name)-1 
    links_sankey$IDtarget <- match(links_sankey$target, nodos_sankey$name)-1
    
    #CREO EL DIAGRAMA
    sankey_vol <- sankeyNetwork(Links = links_sankey, Nodes = nodos_sankey,
                                Source = "IDsource", Target = "IDtarget",
                                Value = "value", NodeID = "name", 
                                sinksRight=FALSE)
    
    sankey_vol
  })
  
  observeEvent(input$info, {
    showModal(
      modalDialog(
        title = div(
          style = "text-align: center;",
          tags$img(src = "https://pbs.twimg.com/profile_images/960924836137750529/r-XG6uEM_400x400.jpg", height = "100px", width = "100px"),
          "Proyecto final curso Ciencia de Datos con R"
        ),
        HTML("<p>Esta aplicación busca informar en que se utiliza principalmente el agua en Uruguay.</p>
              <p>En el mapa de 'Uso principal por cuenca hidrográfica', se representa el destino principal que se le da al agua en las diferentes cuencas de Uruguay. 
              Al pasar por las distintas cuencas se proporciona información adicional, como la cantidad de registros (permisos de extracción).</p>
              <p>El diagrama de 'Distribución del volumen anual por uso' muestra cómo se distribuye el volumen anual entre los diferentes usos que se le da al agua.</p>
              <p>Utiliza los selectores ubicados en la parte superior para filtrar los datos según el nivel de cuenca y el uso deseado.
              También se pueden seleccionar cuencas en el mapa para ver el diagrama de uso específico para esas cuencas.</p>
              <small>Los volúmenes presentados en esta aplicación se refieren al volumen anual máximo permitido para extraer en m3.</small>
              <small></small>
              <br/>
              <hr/>
              <p><strong>Fuente de los datos:</strong> DINAGUA, Ministerio de Ambiente.</p>
              <p><strong>Creadores app:</strong> Facundo Morini, Mariana Ceresa.</p>"
             ),
        footer = modalButton("Aceptar"),
        easyClose = TRUE
      )
    )
  })
  
}

shinyApp(ui = ui, server = server) 
