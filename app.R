library(here)
library(readxl)
library(tidyverse)
library(geouy)
library(shiny)
library(ggiraph)

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
      girafeOutput("mapa")
    )
  )
)

#SE DEFINE EL SERVER
server <- function(input, output) {

#VARIABLE REACTIVA DE LOS DATOS QUE SE VAN A UTILIZAR PARA CADA INPUT
  datos_r <- reactive({
    if (input$mapa_selector == "Cuencas nivel 1" && input$uso_selector == "Todos") {
       datos_c1
    } else if (input$mapa_selector == "Cuencas nivel 2" && input$uso_selector == "Todos") {
       datos_c2
    } else if (input$mapa_selector == "Cuencas nivel 1" && input$uso_selector != "Todos") {
      datos_c1 %>%
      filter(uso == input$uso_selector)
    } else if (input$mapa_selector == "Cuencas nivel 2" && input$uso_selector != "Todos") {
      datos_c2 %>%
      filter(uso == input$uso_selector)
    }
  })

#DEFINO QUE MAPA SE VA A UTILIZAR EN CADA INPUT  
  mapa_r <- reactive({
    if (input$mapa_selector == "Cuencas nivel 1") {
      mapa_c1
    } else if (input$mapa_selector == "Cuencas nivel 2") {
      mapa_c2
    }
  })

#SE DEFINE EL MAPA
  output$mapa <- renderGirafe({

#ENCUENTRO EL USO DESTINO PRINCIPAL POR CUENCA
  reg_cuenca <- datos_r() %>%
                group_by(codcuenca) %>%
                summarise(n_reg = sum(n_reg)) %>%
                select(codcuenca,n_reg)
    
  uso_cuenca <- datos_r() %>%
                group_by(codcuenca,uso,destino) %>%
                summarise(v_anual = sum(v_anual)) %>%
                ungroup() %>%
                group_by(codcuenca) %>%
                filter(v_anual == max(v_anual)) %>%
                select(codcuenca,uso,destino)
  
  mapa_fill <- ifelse(input$uso_selector == "Todos","uso","destino")
  df_mapa <- mapa_r()
  df_mapa <- left_join(df_mapa,reg_cuenca,by="codcuenca") %>%
             left_join(uso_cuenca,by="codcuenca",multiple = "all")
  
#HAGO EL MAPA
  mapa <- ggplot() +
      geom_sf_interactive(data = df_mapa, color = "black",
                          aes(tooltip = paste(nomcuenca, "\nCantidad de registros: ", n_reg,
                                              "\nUso: ", uso,
                                              "\nDestino principal: ", destino),fill=!!sym(mapa_fill),
                              data_id = codcuenca),
                          hover_css = "fill:blue;", size = 1.5, alpha = 0.8) +  
      theme_void()

#LO VUELVO INTERACTIVO CON ggiraph    
    girafe(ggobj = mapa)
  })
}

shinyApp(ui = ui, server = server) 
