#' @export
#' @import shiny
#' @importFrom shinyjs enable disable disabled
#' @title apply_functionUI
#' @description This function has to be set in the UI part of a shiny application
#'     It add a windows containing a radioButton to apply a function
#'     on a numeric vector.
#' apply_function function has to be set in the Server part.
#' @param id An id that will be used to create a namespace
#' @return UI page
#' @examples
#' \dontrun{
#' # In UI :
#' apply_functionUI(id = "mod2")
#' # In Server
#' data_module2   <- callModule(module = apply_function,
#'                              id = "mod2",
#'                              variable = reactive(dataset$data_var_x))
#'}
apply_functionUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12,
           uiOutput(ns("ui_RB_funs")),
           uiOutput(ns("ui_AB_apply")),
           uiOutput(ns("ui_DIV_warn"))
    )
  )
}

#' @export
#' @import shiny
#' @importFrom shinyjs enable disable disabled
#' @title apply_function
#' @description This function has to be set in the Server part of a shiny application
#'     It add a windows containing a radioButton to apply a function
#'     on a numeric vector.
#' @param input Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param output Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param session Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param variable Numeric. Vector containing dataset to apply function on.
#' @return Server logic
#' @examples
#' \dontrun{
#' # In UI :
#' apply_functionUI(id = "mod2")
#' # In Server
#' data_module2   <- callModule(module = apply_function,
#'                              id = "mod2",
#'                              variable = reactive(dataset$data_var_x))
#'}
apply_function <- function(input, output, session, variable = NULL) {
  
  ns <- session$ns
  
  # Warning if no data loaded
  output$ui_DIV_warn <- renderUI({
    if (is.null(variable())) {
      div(
        tags$br(),
        span(class = "warn", "No dataset loaded")
      )
    }
  })
  
  # Numeric Inpu
  output$ui_RB_funs <- renderUI({
    
    numericInput(ns("RB_funs"), "Nombre d'année pour le calcul du coefficient de passage", 5, min = 1, max = 100)
  })
  
  # apply button
  output$ui_AB_apply <- renderUI({
    if (is.null(variable())) {
      shinyjs::disabled(
        actionButton(ns("AB_apply"), label = "Génération Triangle")
      )
    } else {
      actionButton(ns("AB_apply"), label = "Génération Triangle")
    }
  })
 #%>% formatCurrency(2:length(toReturn$result), currency = '',interval = 3,mark = " ", digits = 0, before = FALSE) 
  # ReactiveValue to return
  toReturn <- reactiveValues( result = NULL,
                              trigger = NULL,
                              fun = NULL)

  # Apply function on variable
  observeEvent(input$AB_apply, {
    if (is.null(input$RB_funs)) 
      {toReturn$result <- variable()*1}
    else{
      
      toReturn$result <- result(na_df_pt = tx_pass(df_pt=variable(),tx=input$RB_funs)$table,df_pt = variable(),tx_list = tx_pass(df_pt=variable(),tx=input$RB_funs)$taux)
      toReturn$result <- datatable(toReturn$temp, options = list(autoWidth = TRUE,columnDefs = list(list(width = '200px', targets = list(1:length(toReturn$result)))),pageLength = 15, scrollX=TRUE, dom = 't'))%>%
        formatRound(2:length(toReturn$result),2)%>%formatStyle(3:length(toReturn$result),target= 'cell',color = styleRow(nrow(toReturn$result)-1,'red'))%>%
        formatStyle(4:length(toReturn$result),target= 'cell',color = styleRow(nrow(toReturn$result)-2,'red'))%>%
        formatStyle(5:length(toReturn$result),target= 'cell',color = styleRow(nrow(toReturn$result)-3,'red'))%>%
        formatStyle(6:length(toReturn$result),target= 'cell',color = styleRow(nrow(toReturn$result)-4,'red'))%>%
        formatStyle(7:length(toReturn$result),target= 'cell',color = styleRow(nrow(toReturn$result)-5,'red'))%>%
        formatStyle(8:length(toReturn$result),target= 'cell',color = styleRow(nrow(toReturn$result)-6,'red'))%>%
        formatStyle(9:length(toReturn$result),target= 'cell',color = styleRow(nrow(toReturn$result)-7,'red'))%>%
        formatStyle(10:length(toReturn$result),target= 'cell',color = styleRow(nrow(toReturn$result)-8,'red'))%>%
        formatStyle(11:length(toReturn$result),target= 'cell',color = styleRow(nrow(toReturn$result)-9,'red'))%>%
        formatStyle(12:length(toReturn$result),target= 'cell',color = styleRow(nrow(toReturn$result)-10,'red'))

      }
    toReturn$trigger <- ifelse(is.null(toReturn$trigger), 0, toReturn$trigger) + 1
    toReturn$fun     <- input$RB_funs 
  })
  
  return(toReturn)
}



#--------------------------------------------------------------

result <- function(na_df_pt,df_pt,tx_list){
  df_pt <- df_pt %>% replace(is.na(.),0)
  result <- df_pt[,3:length(df_pt)] + na_df_pt[,2:length(na_df_pt)]
  result <- cbind(df_pt[,1:2],result)
  colnames(tx_list) <- colnames(result)
  result$`n+exercice` <- as.character(result$`n+exercice`)
  result<-rbind(result,tx_list)
return(result)
}

t_moy <- function(row,df_pt,na_df_pt,tx){rang_n_x <- as.numeric(na_df_pt$row[row])-tx
rang_n_1 <- as.numeric(na_df_pt$row[row])-1
txa <- sum(df_pt[ifelse(rang_n_x <1,1,rang_n_x):rang_n_1,na_df_pt$col[row]])/sum(df_pt[ifelse(rang_n_x<1,1,rang_n_x):rang_n_1,na_df_pt$colone_cible[row]])
txa <- ifelse(na_df_pt$row[row]==1,na_df_pt$taux_moyen[row-1],txa) 
return(txa)}

tx_pass <- function(df_pt,tx){
  na_df_pt <- as.data.frame(which(is.na(df_pt), arr.ind = TRUE)) %>% mutate(colone_cible= as.numeric(col)-1)
  for (row in 1:nrow(na_df_pt)){na_df_pt$'taux_moyen'[row]<- ifelse(is.na(t_moy(row,df_pt,na_df_pt,tx)),na_df_pt$taux_moyen[row-1],t_moy(row,df_pt,na_df_pt,tx))}
  na_df_pt <- na_df_pt  %>% arrange(row)
  for (row in 1:nrow(na_df_pt)){na_df_pt$'value'[row]<- ifelse( is.na(df_pt[as.numeric(na_df_pt$row[row]),as.numeric(na_df_pt$col[row]-1)])
                                                                ,as.numeric(na_df_pt$value[row-1])*na_df_pt$taux_moyen[row],
                                                                as.numeric(df_pt[as.numeric(na_df_pt$row[row]),as.numeric(na_df_pt$col[row]-1)])*na_df_pt$taux_moyen[row])}
  for (row in 1:nrow(na_df_pt)){na_df_pt$'nom_colonne'[row]<- colnames(df_pt)[na_df_pt$col[row]]}
  for (row in 1:nrow(na_df_pt)){na_df_pt$'annee_n'[row]<- df_pt[na_df_pt$row[row],1] }
  tx_list <- na_df_pt %>% summarise(nom_colonne,taux_moyen) %>% distinct()
  tx_list <- setNames(data.frame(t(tx_list[,-1])), tx_list[,1])
  tx_list <- cbind(data.frame(annee_n = c("Taux de Passage Moyen"),'n+0'=c(1)),tx_list)%>% select(annee_n,'n.0','n+1','n+2','n+3','n+4','n+5','n+6','n+7','n+8','n+9','n+10','n+11','n+12')
  i <- list(2:length(tx_list))[[1]]
  tx_list[,i] <- apply(tx_list[,i], 2, function(x) as.numeric(as.character(x)))
  na_df_pt <- na_df_pt %>% select(value,nom_colonne,annee_n) %>% spread( nom_colonne, value) %>% replace(is.na(.),0) %>% select(annee_n,'n+1','n+2','n+3','n+4','n+5','n+6','n+7','n+8','n+9','n+10','n+11','n+12')%>% arrange(as.numeric(substring(annee_n,3)))
  colnames(tx_list) <- colnames(na_df_pt)
  tx_pass_result <- list("table"=na_df_pt,"taux"=tx_list)
  
  return(tx_pass_result)
}

pt_paye <- function(df){  
  df_pt <- df %>% group_by(df$exercice,df$annee_n) %>% summarize(Somme_par_vue = sum(as.numeric(paye), na.rm = TRUE))
  df_pt <- df_pt %>% rename(annee_n = 'df$annee_n', exercice = 'df$exercice') %>% pivot_wider(names_from = annee_n, values_from = Somme_par_vue, names_sort = TRUE) 
  df_pt <- df_pt %>% drop_na('0')%>% select(1:14)
  names(df_pt)<-paste("n+", names(df_pt), sep = "")
  return(df_pt)}

pt_provision <- function(df){  
  df_pt <- df %>% group_by(df$annee_n, df$exercice) %>% summarize(Somme_par_vue = sum(as.numeric(provisions), na.rm = TRUE))
  df_pt <- df_pt %>% rename(annee_n = 'df$annee_n', exercice = 'df$exercice') %>% pivot_wider(names_from = annee_n, values_from = Somme_par_vue, names_sort = TRUE) 
  df_pt <- df_pt %>% drop_na('0')%>% select(1:14)
  names(df_pt)<-paste("n+", names(df_pt), sep = "")
  return(df_pt)}

pt_total <- function(df){  
  df_pt <- df %>% group_by(df$annee_n, df$exercice) %>% summarize(Somme_par_vue = sum(as.numeric(total), na.rm = TRUE))
  df_pt <- df_pt %>% rename(annee_n = 'df$annee_n', exercice = 'df$exercice') %>% pivot_wider(names_from = annee_n, values_from = Somme_par_vue, names_sort = TRUE) 
  df_pt <- df_pt %>% drop_na('0')%>% select(1:14)
  names(df_pt)<-paste("n+", names(df_pt), sep = "")
  return(df_pt)}

transform_df <- function(df){  
  names(df)<-str_replace_all(names(df), c(" " = "_" , "," = "" ))
  nabdx <- which(is.na(df),arr.ind = TRUE)
  indice <- nabdx[[1, 2]]
  nabdx <- nabdx[nabdx[,2]==indice,]
  nabdx <- nabdx[,1]
  df <- df %>% mutate(annee_traitement = as.numeric(substr(df$date_traitement,0,4))) 
  df<-df[-nabdx,]
  nplus <- df$annee_traitement - as.numeric(df$exercice)
  #nplus <- paste("n+", nplus, sep = "")
  df$annee_n=nplus
  df$paye = as.numeric(df$reglement_principal)+as.numeric(df$reglement_accessoire)-as.numeric(df$reglement_recours)
  df$provisions = as.numeric(df$provision)-as.numeric(df$recours)
  df$total = as.numeric(df$paye) + as.numeric(df$provisions)
  return(df)}

sqlitePath <- "www/mudetapp_database2.db"
table <- "table_tx"


saveData <- function(data) {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

loadData <- function() {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

