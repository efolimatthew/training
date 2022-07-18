library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(tidyverse)
library(shinyWidgets)
library(shinyjs)
library(readr)
library(here)

source(here("training_helper_UI.R"))
source(here("training_module.R"))



ui_data <- get_ui_data3()
params<-list(ui_data = NULL)


ui <- shinydashboardPlus::dashboardPage(
  shinydashboardPlus::dashboardHeader(
    # Title
    title = tagList(span(class = "logo-lg", span(img(src = "img/odapes_logowhite.png"))),
                    img(src = "img/odapes.jpg", width = 40))

  ),
  
  ###  Sidebar Menu--------------------------------------------------------------------------- 
  shinydashboardPlus::dashboardSidebar(
    collapsed = TRUE, minified = TRUE,
    sidebarMenu(id="pages",
                menuItem("MAP", tabName = "map", icon = icon("map-marked-alt")),
                menuItem("Training", tabName = "train", icon = icon("data")),
                menuItem("Settings", tabName = "set", icon = icon("cogs"))
    )
  ),
  ###  Main Body Content ----------------------------------------------------------------- 
  dashboardBody(
    ##  Style and JS stuff ---------
    useShinyjs(),
    
    # Style sheets and CSS Element which defines the Artwork
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "odapes_stylesheet.css")),

    ##  UI Tabsets (Modules)---------
    tabItems(
      tabItem(tabName = "map", actionButton(inputId = "btn1", label = "show windown in Training")),
      tabItem(tabName = "train", ods_training_UI("trainid", params)),
      tabItem(tabName = "set")
      )
  ) 
)  

server <- function(input, output, session) {
  
  r_data <- reactiveValues(
    event = readr::read_csv2(here("event_data_2.csv"))
  )
  
  r_control <- reactiveValues(
    trigger_delete = 0,
    MS_delete = NULL
  )

  #modules server part
  ods_training_SERVER("trainid", r_data, r_control, params)
  
  observeEvent(input$btn1,{
    print("HELLO WORLD WE ARE IN THE APP")
    r_control$trigger_delete <- r_control$trigger_delete + 1
    r_control$MS_delete <- "MSEVENT_delete-user_event-ES6255c368"
  })
}

shinyApp(ui, server)

  