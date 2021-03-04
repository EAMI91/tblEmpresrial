#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinydashboard magrittr dplyr lubridate highcharter quanteda quanteda.textplots
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(
      header = dashboardHeader(title = "Tablero empresarial",
                               tags$li(class = "dropdown",
                                       tags$li(class = "dropdown",
                                               div(
                                                 selectInput("entidad",label = NULL,choices = c("Michoacán","Nuevo León"))   
                                               )
                                       )
                               ),
                               dropdownMenuOutput("notificaciones")
      ),
      sidebar =  dashboardSidebar(
        sidebarMenu(
          menuItem("Candidatos", tabName = "candidatos",
                   icon = icon("user")),
          menuItem("Encuestas", tabName = "enc",
                   icon = icon("poll-h"),
                   menuSubItem(text = "Gráficas",tabName = "encuestas"),
                   menuSubItem(text = "Análisis",tabName = "enc_pdf")),
          menuItem("Redes sociales", tabName = "rs",
                   icon = icon("twitter-square"),
                   menuSubItem(text = "Gráficas",tabName = "red_social"),
                   menuSubItem(text = "Análisis",tabName = "rs_pdf")),
          menuItem("Noticias", tabName = "not",
                   icon = icon("newspaper"),
                   menuSubItem(text = "Gráficas",tabName = "noticias"),
                   menuSubItem(text = "Análisis",tabName = "not_pdf")),
          menuItem("Análisis electoral", tabName = "ae",
                   icon = icon("file-alt")
                   )
        )
        
      ),
      body = dashboardBody(
        tabItems(
          tabItem(tabName = "candidatos",
                  mod_candidatos_ui("candidatos_ui_1")
          ),
          tabItem(tabName = "encuestas",
                  mod_encuestas_ui("encuestas_ui_1")
          ),
          tabItem(tabName = "enc_pdf",
                  mod_análisis_ui("análisis_ui_1")
          ),
          tabItem(tabName = "red_social",
                  mod_redes_sociales_ui("redes_sociales_ui_1")
          ),
          tabItem(tabName = "rs_pdf",
                  mod_análisis_ui("análisis_ui_2")
          ),
          tabItem(tabName = "noticias",
                  mod_noticias_ui("noticias_ui_1")
          ),
          tabItem(tabName = "not_pdf",
                  mod_análisis_ui("análisis_ui_3")
          ),
          tabItem(tabName = "ae",
                  mod_análisis_ui("análisis_ui_4")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'tblEmpresrial'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

