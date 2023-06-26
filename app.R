library(here)
library(readxl)
library(tidyverse)
library(geouy)
library(shiny)
library(ggiraph)
library(networkD3)

#CARGA DATOS
datos <- read_csv2(here("Datos","aprovechamientos_app.csv"))

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
mapa_c1 <- load_geouy("Cuencas hidro N1") %>%
  mutate(nombrec1=iconv(nombrec1, from = "ISO-8859-1", to = "UTF-8"),
         popup=iconv(popup, from = "ISO-8859-1", to = "UTF-8")) %>%
  rename("nomcuenca"="nombrec1")

mapa_c2 <- load_geouy("Cuencas hidro N2") %>%
  mutate(nombrec2=iconv(nombrec2, from = "ISO-8859-1", to = "UTF-8"),
         popup=iconv(popup, from = "ISO-8859-1", to = "UTF-8"))%>%
  rename("nomcuenca"="nombrec2")

#DEFINO LA UI
ui <- fluidPage(
  titlePanel("Visualizador de Cuencas"),
  sidebarLayout(
    sidebarPanel(
      selectInput("mapa_selector","Seleccionar Nivel de Cuenca:",
                  choices = c("Cuencas nivel 1", "Cuencas nivel 2"),
                  selected = "Cuencas nivel 2"),
      selectInput("uso_selector", "Filtrar por uso:",
                  choices = c("Todos","Riego","Industrial","Otros Usos Agropecuarios","Otros Usos"),
                  selected = "Todos")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Uso por cuenca",
          girafeOutput("mapa"),
          sankeyNetworkOutput("tabla")),
        tabPanel("Distribucion de registros",
                 girafeOutput("registros")))
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
                                              "\nUso: ", uso,
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
}

shinyApp(ui = ui, server = server) 
