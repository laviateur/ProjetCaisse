
## BIBLIOTHEQUE ----
# ####################### #
library(rsconnect)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(lubridate)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(data.table)
library(shinyWidgets)
#library(tidyverse)
deployApp()
# ####################### #
## UI ----
# ####################### # 
rm(list = ls())

ui <- dashboardPage(
  dashboardHeader(title = "ELAN INFO", titleWidth = 300),
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu(id = "tabs",
                               menuItem("Lecture des informations ", tabName = "tab_readData", icon = icon("bar-chart-o")),
                               menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("dashboard")),
                               menuItem("Visualiser des informations", tabName = "tab_visualization", icon = icon("eye")),
                               menuItem("Gestion des familles", tabName = "tab_familles", icon = icon("fas fa-cog"))
                   )
  ),
  body <- dashboardBody(
    tabItems(
      tabItem(tabName = "tab_visualization",
              
              fluidRow(
                column(
                  width = 2, offset = -1,
                  h3("Filtre des ventes"), DTOutput("dataFileSum")
                ),
                column(
                  width = 3, offset = 1,
                  tags$br(),
                  dateRangeInput("DateRange",label = "Selectionner une plage de date :", start = now(), separator = " / "),
                  selectizeInput('SelectCode', 'Selectionner Code Article', choices = " ", multiple = TRUE),
                  (actionButton(inputId = "Dashboard", label = "dashboard", icon = icon("dashboard")))
                ),
                column(
                  width = 3, offset = 1,
                  tags$br(),
                  selectizeInput('SelectFamilles', 'Selectionner les familles', choices = " ", multiple = TRUE),
                  selectizeInput('SelectTVA', 'Selectionner la TVA', choices = " ", multiple = TRUE)
                  
                )
              ),
              DTOutput("dataFile")
      ),
      tabItem(
        tabName = "tab_dashboard", 
        h2("Tableau de bord"),
        fluidRow( 
          column(width = 12, offset = -1,
                 (actionButton(inputId = "UploadFile", label = "Charger des informations", icon = icon("play"))),
                 (actionButton(inputId = "ModFile", label = "Changer les filtres", icon = icon("play"))),
                 shinythemes::themeSelector(),
                 tags$br(),
                 box(title = "Synthèse des ventes par famille",
                     fluidRow(
                       column(width = 9, offset = 1,
                              DTOutput("MyDataBis")
                       ))),
                 box(title = "Synthèse des ventes par TVA",
                     fluidRow(
                       column(width = 9, offset = 1,
                              DTOutput("MyDataTVA")
                       ),
                       tags$br(),
                       fluidRow(
                         column(width = 9, offset = 1,
                                plotOutput("MyDataBisGraph")
                         ))
                     )
                 )))),
      tabItem(
        tabName = "tab_readData",
        box(title = "Informations d'importation",
            status = "warning",
            fluidRow(
              column(width = 5, offset = 1),
              column(
                width = 9,
                offset = 1,
                radioButtons(
                  inputId = "header",
                  label = "Entete",
                  choices = c("oui"= TRUE, "non"= FALSE),
                  selected = TRUE, inline=T
                ),
                radioButtons(
                  inputId = "sep", 
                  label = "Separateur",
                  choices = c(Virgule = ",",'Point Virgule' = ";",Tabulation = "\t"),
                  selected = ";", 
                  inline=T
                ),
                radioButtons(
                  inputId = "quote",
                  label = "Guillemet",
                  choices = c(Aucun = "","Double Guillemets" = '"',"Single Guillemet" = "'"),
                  selected = "", inline=T
                )
              )
            )
        ),
        box(title = "Informations sur le TPV",
            status = "warning",
            fluidRow(
              column(width = 5, offset = 1),
              column(
                width = 9,
                offset = 1,
                radioButtons(
                  inputId = "marque",
                  label = "Marque",
                  choices = c(Aucun = "","Marques" = '"',"EXA" = "'"),
                  selected = TRUE, inline=T
                )))),
        fluidRow(
          column(
            width = 3, offset = 1,
            fileInput("dataFile",label = NULL,buttonLabel = "Navigateur...", placeholder = "Selectionner un fichier"),
            column( 
              width = 2, offset = 3,
              (actionButton(inputId = "visualisation", label = "visualiser", icon = icon("play")))),
            #  tags$br(),
            h3(""))),
        fluidRow(
          column(
            width = 12, offset = -1,
            DTOutput("tab_preview")
          )
        )  
      ),
      tabItem(
        tabName = "tab_familles",
        fluidRow(
          column(
            width = 9,
            offset = -1,
            h3("Gestion des familles")),
          column( 
            width = 4, 
            offset = 1,
            fileInput("dataFamilles",label = "Familles",buttonLabel = "Navigateur...", placeholder = "Selectionner un fichier"),
            actionButton(inputId = "mergingF", label = "Charger la table", icon = icon("upload")), 
            actionButton(inputId = "saveFBtn", label = "Sauvegarder", icon = icon("wrench"))
          ),
          column(
            width = 5, 
            offset = 1,
            fileInput("dataCod.Rayons",label = "Codes Rayons",buttonLabel = "Navigateur...", placeholder = "Selectionner un fichier"),
            actionButton(inputId = "merging", label = "Charger la table", icon = icon("upload")),
            actionButton(inputId = "saveRBtn", label = "Sauvegarder", icon = icon("wrench"))
          ),
          fluidRow(
            column(
              width = 9, offset = 1,
              tabsetPanel(
                tabPanel("Table des familles", DT::dataTableOutput("dataFamilles") ),
                tabPanel("Table des rayons", DT::dataTableOutput("dataCod.Rayons") )
                
              )
            )
          )
        )
      )
    )
  ),
  
  skin = c("green")
)
