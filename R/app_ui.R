#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinydashboard
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
          menuItem("Encuestas", tabName = "encuestas",
                   icon = icon("poll-h")),
          menuItem("Redes sociales", tabName = "red_social",
                   icon = icon("twitter-square")),
          menuItem("Noticias", tabName = "noticias",
                   icon = icon("newspaper")),
          menuItem("Análisis electoral", tabName = "a_electoral",
                   icon = icon("file-alt"))
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
          tabItem(tabName = "red_social",
                  mod_redes_sociales_ui("redes_sociales_ui_1")
          ),
          tabItem(tabName = "noticias",
                  mod_noticias_ui("noticias_ui_1")
          ),
          tabItem(tabName = "a_electoral",
                  mod_a_electoral_ui("a_electoral_ui_1")
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

