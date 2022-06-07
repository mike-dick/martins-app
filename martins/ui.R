library (dplyr)
library (ggplot2)
library (plotly)
library(shiny)
library(stringr)
source ("utils.R")
library (shinydashboard)
library (purrr)
library (htmltools)
options(shiny.maxRequestSize=30*1024^2) # data max 30 mb 

## ui.R ##
sidebar <- dashboardSidebar(
  fileInput("file", "File input:"),
  textInput("filtering", "Enter your filter below:"),
  h5 ("Separate each expresion with ; ") ,
  h5("e.g. : MDV == 0 ; CMT %in% c (1,2,3)"),
  hr (), hr (),
  
  sidebarMenu(
    menuItem("Point Plot", tabName = "tab_plot", icon = icon("chart-line")),
    menuItem("Box Plot", tabName = "tab_box", icon = icon("box")),
    menuItem("Histogram", tabName = "tab_hist", icon = icon("chart-bar")),
    menuItem("Data Summary", tabName = "tab_summ", icon = icon("table")),
    menuItem("Result", tabName = "tab_res", icon = icon("file"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "tab_plot",
            
            fluidRow(box (width = 12, 
                          
                          column(4,
                                 selectInput("x_plot", "X Columns",NA),
                                 selectInput("y_plot", "Y Columns",NA),
                                 selectInput("color_plot", "Color Columns", NA)
                                 
                          ),
                          
                          column(4,
                                 selectInput("group_plot", "Grouping", NA),
                                 
                                 column(6, h1 (),
                                        checkboxGroupInput("ptype_plot", "Plotting",
                                                    c("Points" = "p_plot",
                                                      "Lines" = "l_plot"),
                                                    selected = c ("p_plot",
                                                                  "l_plot"))),
                                 column(6,radioButtons("ltype_plot", h4("Lines"),
                                              choiceNames = list("Median","Mean", "GM"),
                                              choiceValues = list("median", "mean", "geom"))),
                                 
                                 
                                 column (4, h5 ("Log Axis : ")),
                                 column (4, checkboxInput("logx_plot", "log x")),
                                 column (4, checkboxInput("logy_plot", "log y"))
                          ),
                          
                          column(4,
                                 textInput("xlab_plot", "Label x :"),
                                 textInput("ylab_plot", "Label y :"),
                                 textInput("main_plot", "Title :"),
                                 column(12,actionButton("addres_plot", "Add to Result!", class = "btn-success"),
                                  align = 'right')
                                 
                                 
                          ),
            ) # the end of box
            ), # the end of FluidRow
            
            verbatimTextOutput("warning_plot"),
            plotlyOutput('plot_plot',height = "540px")
            
    ),
    tabItem(tabName = "tab_box",
            fluidRow(box (width = 12, 
                          
                          column(4,
                                 selectInput("x_box", "X Columns",NA),
                                 selectInput("y_box", "Y Columns",NA)
                                 
                          ),
                          column(4,
                                 textInput("xlab_box", "Label x :"),
                                 textInput("ylab_box", "Label y :")
                                 
                          ),
                          column(4,
                                 textInput("main_box", "Title :"),
                                 selectInput("id_box", "ID Columns",NA),
                                 h6 ("Data will be filtered by '!duplicated (ID)'"),
                                 column(12,actionButton("addres_box", "Add to Result!", class = "btn-success"),
                                        align = 'right')
                                 
                          )
            )),
            verbatimTextOutput("warning_box"),
            plotlyOutput('plot_box',height = "540px")
    ),
    tabItem(tabName = "tab_hist",
            fluidRow(box (width = 7, 
                          
                          column(6,
                                 selectInput("x_hist", "X Columns",NA),
                                 textInput("main_hist", "Title :")
                                 
                          ),
                          column(6,
                                 textInput("xlab_hist", "Label x :"),
                                 textInput("ylab_hist", "Label y :"),
                                 column(12,actionButton("addres_hist", "Add to Result!", class = "btn-success"),
                                        align = 'right')
                                 
                                 
                          )
            )),
            verbatimTextOutput("warning_hist"),
            plotlyOutput('plot_hist',height = "540px")
    ),
    tabItem(tabName = "tab_summ",
            selectInput("id_summ", "ID Columns",NA),
            selectInput("tar_summ", "Target Columns",NA,multiple = TRUE),
            downloadButton("download_summ", "Download Table"),
            h1(),
            tableOutput("table_summ")
            
    ),
    tabItem(tabName = "tab_res",
            fluidRow(box (width = 7, 
                          
                          column(6,
                                 textInput("title_res", "Title :"),
                                 downloadButton("download_res", "Download Result")
                                 
                          ),
                          column(5, offset = 1,
                                 radioButtons("desc_res", h5("Description"),
                                              choiceNames = list("With Description", "None"),
                                              choiceValues = list("with", "none"))
                                 
                          ),
                          
            )),
            uiOutput("multi_plt_res")
    )
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "A2PG"),
  sidebar,
  body
)


shinyUI(ui)