
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(DT)
#library(plotly)
library(rhandsontable)

shinyUI(dashboardPage(

  # Application title
  dashboardHeader(title="CSA Calculator"),

  # Sidebar with a slider input for number of bins
  dashboardSidebar(
    sidebarMenu(
      menuItem("Get Addynamix Data",tabName = "tab_setup"),
      menuItem("Cost per Thousands",tabName = "tab_cpts"),
      menuItem("Media Cost Inflation",tabName = "tab_inflation"),
      menuItem("Reach Calculator",tabName = "tab_reach")
    )
    ),

    # Show a plot of the generated distribution
    dashboardBody(
      tabItems(
        tabItem("tab_setup",
                fluidRow(
                box(width=3,solidHeader = T,title = "Get Data",status = "primary",
                    uiOutput("select_advertiser"),
                    uiOutput("select_brands"),
                    uiOutput("date_select"),
                    actionButton("btn_getaddy","Get Data")
                    ),
                box(width=9,solidHeader = T,title = "Chart",status = "primary",
                    selectInput("select_frequency",label="Frequency",choices = c("Daily","Weekly","Monthly","Annual")),
                    plotOutput("plot_spend")
                    )
                ),
                fluidRow(
                  column(width=3),
                  box(width=9,solidHeader = T,title = "Data Table",status = "primary",
                      DT::dataTableOutput("tbl_addy")
                  )
                )
        ),
        tabItem("tab_cpts",
                fluidRow(
                  box(width=12,solidHeader = T,title = "Cost per Thousands - 2017 Base Year",status = "primary",
                    uiOutput("select_region"),
                    rHandsontableOutput("rhotbl_cpts"),
                    tableOutput("tbl_test")
                  )
                )
        ),
        tabItem("tab_inflation",
                fluidRow(
                  box(width = 12,solidHeader = T,title="Inflation - 2017 Base Year",status="primary",
                      rHandsontableOutput("rhotbl_inflation")
                      )
                ),
                fluidRow(
                  box(width = 12,solidHeader = T,title="Chart",status="primary",
                      uiOutput("select_inf_channel"),
                      plotOutput("plot_inflation")
                  )
                )
                ),
        tabItem("tab_reach",
                fluidRow(
                  box(width=4,solidHeader = T,title = "Options",status = "primary",
                      uiOutput("date_select_reach"),
                      uiOutput("select_channels"),
                      uiOutput("select_regions"),
                      numericInput("num_universe",label = "Universe Size",value = 40000000,min = 1,max = 100000000)
                      ),
                  box(width=8,solidHeader = T,title = "Chart",status = "primary",
                    #  downloadButton("download_results",label=""),
                      plotOutput("plot_ratings")
                  )
                ),
                fluidRow(
                  column(width=4),
                  box(width=8,solidHeader = T,title = "Data Table",status = "primary",
                      downloadButton("download_results",label=""),
                      dataTableOutput("tbl_reach")
                  )
                
                  )
                )
      )
    )
  ))
