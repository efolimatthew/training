# this is for training purpose

## FUNCTIONS: Create the Menus --------------------
# This checks live the correct inputs in ui forms of a Microservice // User recursive testing and finds the first wrong

# creates + translates + glues message for delete warning (returns ttl, txt, icn)
delete_warn_msg <- function(tbl, row){
  browser()
  list(
    ttl="Delete Event",
    icn ="trash",
    txt = "Are you sure you want to delete {tbl} with id {row}? All Data will be deleted and cannot be recovered!"%>%
      glue::glue()
  )
}

# asks confirmation of an action / receives list x with icn, ttl and txt
warning_confirmation_modal<-function(x, ns, params){
  showModal(
    tags$div(id="pos_inv_mod", modalDialog(
      
      title = div(class = "ui header", icon(x$icn), div(class = "content",(x$ttl))),
      
      #body Taglist
      tagList(fluidRow(icon("question-circle"), style="font-size:150px; margin-top:-10px; margin-buttom:-30px; color:#A31F34; text-align: center;"),
              fluidRow(h3((x$txt)), style="text-align: center;"),
              tags$br()
      ),
      #footer taglist
      # footer = tagList(
      #   #ods_btn( c("btn_del_no", "btn_del_ok"), ns, params)
      # ),
      easyClose = T,
      fade = T
    )
    )
  )
  
}

## UI FUNCTON: Delete Modal---------------------
### MODULE UI FUNCTION ---------------
ods_training_UI <- function(id, params) {
  
  ns <- NS(id)
  # Namespaces are ALWAYS required for ALL ui elements INSIDE MODULES IN THE UI PART ONLY!!!!
  
  # This comment is to make sure it updated
  tagList(
    actionButton(inputId = ns("btn3"), label = "random button in MODULE") # trainid-btn3
  )
  
}


## MODULE SERVER: FUNCTION ODAPES TRAINING -------------------
ods_training_SERVER <- function(id, r_data, r_control, params) {
  moduleServer(id,function(input, output, session) {
    
    ##  APERO
    ns <- session$ns
    temp_vals <- reactiveValues()
    
    
    
    observeEvent(r_control$trigger_delete,{
      req(r_control$trigger_delete > 0)
      print("HELLO WORLD WE ARE IN THE MODULE")
    })
    
    

    ## DELETE PART----------
    # asks user confirmation for delete item delete
    observeEvent(r_control$trigger_delete, {
      req(r_control$MS_delete)
      browser()
      x <- r_control$MS_delete
      delete_warn_msg(which_table(r_control$MS_delete), which_row(r_control$MS_delete)) %>%
        warning_confirmation_modal(ns, params)
    })

    # confirmed delete // pass button id on
    observeEvent(input$btn_del_ok, {
      r_control$MS_del <- r_control$MS_delete
    })


    ## MODAL HANDLER + RESET PART----------
    # modal closer
    observeEvent(input$btn_ms_no, {
      removeModal()
      r_control$btn_ms_reset <- r_control$btn_ms_reset + 1
    })

#     # modal closer
#     observeEvent(input$btn_del_no, {
#       removeModal()
#       r_control$btn_ms_reset <- r_control$btn_ms_reset + 1
#     })
#     

  })
}