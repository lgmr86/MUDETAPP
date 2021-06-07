library(shiny)
library(shinyModulesTuto)
library(shinymanager)
library(markdown)
library(shinyFiles)
library(shinythemes)
library(jrc)
library(readxl)
library(tidyverse)
library(here)
library(skimr)
library(DT)
library(stringr)
library(magrittr)
library(dplyr)
library(tidyr)
source("apply_function.R")


# define some credentials
credentials <- data.frame(
  user = c("stephane.berthault@coopergay.fr","e.mazeau@mudetaf.fr", "admin"), # mandatory
  password = c("Mucg21!","Mucg21!", "Marly1986!"), # mandatory
  start = c(NA,NA,"2019-04-15"), # optinal (all others)
  expire = c(NA, NA, "2022-12-31"),
  admin = c(FALSE,FALSE, TRUE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

#-------------------------------------------------------------------------------------------------------------

#~/GitHub/MUDETAPP/MUDETAF_APP/

#setwd("~/GitHub/MUDETAPP/MUDETAF_APP/MUDETAF_1")
df_triangle <- transform_df(readRDS("df.rds"))
triangle <- list(pt_paye(df_triangle),pt_provision(df_triangle),pt_total(df_triangle))
names(triangle)<-c("Sinistres payés","Réserves","Sinistres Totals")


ui <- fluidPage(title="CG MUDETAF", theme = shinytheme("flatly"),
  loadModulesCSS("modules_styling.css"),
  
  navbarPage("CG MUDETAF",
             tabPanel("Triangulation",
                      sidebarLayout(
                        sidebarPanel(
                          tags$div(class="form-group shiny-input-container", 
                                   tags$div(tags$label("Dossier de Triangulation")),
                                   tags$br(),
                                   tags$div(tags$label("Selection du dossier", class="btn btn-primary",
                                                       tags$input(id = "fileIn", webkitdirectory = TRUE, type = "file", style="display: none;", onchange="pressed()"))),
                                   tags$br(),
                                   tags$div(id="fileIn_progress", class="progress progress-striped active shiny-file-input-progress",
                                            tags$div(class="progress-bar")),
                                   
                                   tags$i("Selectionner le dossier comportant les exercices à utiliser pour la triangulation")),
                                  selectInput("SI_colname", label = "Choisir son jeux de données",
                                              choices = c("Sinistres payés","Réserves","Sinistres Totals"),selected ="Sinistres payés" ),
                                  apply_functionUI(id = "id1")

                          
                        ),
                        mainPanel(
                          tags$div(tags$label(textOutput("titre"))),
                          tags$i(textOutput("soustitre")), 
                          DT::dataTableOutput("PR_results_print")
                          
                        )
                      )),
             tabPanel("Table Sinistres",
                      mainPanel(fluidRow(
                        column(6,
                        uiOutput('choose_exercice'))
                        ),

                        fluidRow(
                          column(12,
                        
                      DT::dataTableOutput('fulldf'))))
             ),
             navbarMenu("Autre",

                        tabPanel("A propos","Outil réalisé par COOPER GAY SAS au bénéfice exclusif de MUDETAF.", tags$br(),
                                 "L'utilisation de cet outil ne peut, en aucun cas, engager la responsabilité de COOPER GAY SAS."
                        )
             )
  ))








#-------------------------------------------------------------------------------------------------------------------------
ui <- secure_app(ui,theme = shinythemes::shinytheme("flatly"),language = 'fr', background  = "linear-gradient(rgba(255, 255, 255, 0.1),rgba(255, 255, 255, 0.01)),url('https://www.coopergay.eu/wp-content/uploads/2020/01/cooper-gay-logo.png')  no-repeat 50% 1%/15% fixed;")



#-------------------------------------------------

server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2)
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  #-------------------------------------
  
df <- reactiveValues(variable_path = NULL)
  

  observe({
    inFiles <- input$fileIn
    df <- data.frame()
    if (is.null(inFiles))
      return(NULL)
    withProgress(message = 'Génération de la Base', value = 0, {
      for (i in seq_along(inFiles$datapath)) {
        tmp <- read_excel(inFiles$datapath[i],col_names= TRUE, col_types = "text")
        incProgress(1/40)
        df <- rbind(df, tmp)
        incProgress(1/40)
        
      }
    })
    df
    
    
  })
  observeEvent(df$trigger,{saveRDS(transform_df(df),"df.rds")})
    

  #-----------------------------------------------------------------------------------------------------
  
  rv <- reactiveValues(variable = NULL, fun_historic = NULL)

  observe({
    test <- ifelse(is.null(input$SI_colname[1]),"Sinistres payés",input$SI_colname[1])
    rv$variable <- triangle[[test]]
    rv$fun_historic <- NULL
  })
  


  modified_data <-    callModule(module = apply_function, id = "id1",
                                 variable = reactive(rv$variable))
  
  
  df_search <- df_triangle %>% select("Exercice"=exercice,"Vue"=annee_n,"Nom"=nom, "Raison"=raison, "Type"=type, "CP"=cp, "Département"=dépsinistré, "Cause"=cause, "Numéro de Sinistre" = numero, "Date de survenance"=dtsurv, "Payé"=paye, "Réserve"=provisions,"Total"=total)
  

  
  output$choose_exercice <- renderUI({
    selectizeInput('var2', 'Exercice', choices = unique(df_search$Exercice))
  })
  

  
  tab <- reactive({ 
    
    datatable(df_search %>% filter(Exercice == input$var2), extensions = 'Buttons', options = list(pageLength = 100,dom = 'Bfrtip',stateSave = FALSE, buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    
  })
  
  output$fulldf <- DT::renderDataTable(tab())
  
  
  
  
  
  
  
  #-----------------------------------------
  
  output$PR_results_print <- DT::renderDataTable(modified_data$result)
                                                                                      
  output$titre <- renderText(input$SI_colname)
  
  output$soustitre <- renderText(if(input$SI_colname=="Sinistres payés"){"Sinistres réglés - Recours perçu"}else if(input$SI_colname=="Réserves"){"Provisions Sinistre + Provisions Accessoire - Provisions Recours"}else{"Total Réglement + Total Réserve"})

  
}
#----------------------------------------------------------------



#------------------------------------------

# Run the application
shinyApp(ui = ui, server = server, options = list(display.mode = "normal"))