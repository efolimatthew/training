### THIS MODULE HELPS RENDERING AND UPDATING THE UI IN THE MICROSERVICE CONTEXT


## UI DATA: Defines the UI component data-------------------


get_ui_data3 <- function(){

  MS_data <- read_csv2(here('www', 'MS_MENU_ELEMENTS.csv'))
  table_ui_data <- read.csv2(here("www", "table_ui.csv"))#%>%dplyr::filter(table_id == tblid)%>%dplyr::select(ui_id)
  X <- read_csv2(here('www', 'ui_element_data.csv'))#

  ui_data <- list(MS_data = MS_data,
             ui_elems = X,
             table_ui_data = table_ui_data
                   )
  
  return(ui_data)
}






## UI FUNCTION: MICROSERVICE Button ------------------------------
# binds the js element ID to the shiny input$... typically used onclick in DT UI elements
shiny_js_binder <- function(ns, bt){
  btn <- paste0('"',ns(bt),'"')
  paste0('Shiny.setInputValue(',btn,', this.id, {priority: \"event\"})')
}

# returns table from button id
which_table <- function(btn_id){
  sub(".*-(.+)-.*", "\\1", btn_id)
}

# returns action (MS) from  button id
which_ms <- function(btn_id){
  btn_id %>% sub("-.*", "", .)%>%sub("_.*", "", .)
}

# returns row from  button id
which_row <- function(btn_id){
  sub(".*-", "", btn_id)
}

# returns action (MS) from  button id
which_action <- function(btn_id){
  btn_id %>% sub("-.*", "", .)%>%sub(".*_", "", .)
}

# makes a button FOR MS WITH JS Binding via js // mandatory needs ns, ui_id, ttl, rest is optional
make_MS_btn <- function(ns, ui_id, character = FALSE, ttl = NULL, txt = NULL, icn = NULL, cls = NULL, css = NULL, pop = FALSE,...){
  if (is.na(ui_id)){return("")}
  
  if (is.na(ttl)){
    ttl <- NULL
  }
  elem <- tagList(
    actionButton(inputId = ui_id, label = ttl,
                 icon = icon(icn), style = css, class = cls,
                 onclick = shiny_js_binder(ns, "MS_btn_listener")
                 ),
    if (as.logical(pop)){
      bsPopover(ns(ui_id),title= ttl,content= txt, 
                placement = "left", trigger = "hover", options = list(container = 'body')
      )
    }
  )
  if (character){
    return(as.character(elem))
  } else {
    return(elem)
  }
}

ods_MS_btn <- function(btns,ns, params){
  params$ui_data$ui_elems %>% 
    filter(ui_id %in% btns)%>%
    arrange(factor(ui_id, btns))%>%
    pmap(., ns = ns, .f = make_MS_btn)
}

## UI FUNCTION: NORMAL Button ------------------------------

# makes a normal button without JS binding // mandatory needs ns, ui_id, ttl, rest is optional
make_btn <- function(ns, ui_id, character = FALSE, ttl = NULL, txt = NULL, icn = NULL, cls = NULL, css = NULL, pop = FALSE,...){
  if (is.na(ui_id)){return("")}

  if (is.na(ttl)){
    ttl <- NULL
  }
  
  elem <- tagList(
    actionButton(inputId = ns(ui_id), label = ttl,
                 icon = icon(icn),style = css, class = cls
                 ),
    if (as.logical(pop)){
      bsPopover(ns(ui_id),title= ttl,content= txt, 
                placement = "left", trigger = "hover", options = list(container = 'body')
      )
    }
  )
  if (character){
    return(as.character(elem))
  } else {
    return(elem)
  }
}

#takes a vector of button ids bnts and creates a actionbutton. Attention the ids need to be defined ui_elems
ods_btn <- function(btns,ns ,params){
  a <- params$ui_data$ui_elems %>% 
    filter(ui_id %in% btns)%>%
    arrange(factor(ui_id, btns))%>%
    pmap(., ns = ns, .f = make_btn)
}