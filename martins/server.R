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


shinyServer(function(input, output, session) {
  
  # data frame before get filtered 
  df_unfiltered <- eventReactive(input$file,{
    req (input$file)
    
    return (reading_df(input$file$datapath))
  })
  
  
  # updating UI based on unfiltered data
  # should be updated based on filtered data !!!!!
  observeEvent(df_unfiltered(),{
    
    req(df_unfiltered())
    
    ### POINT AND LINE TAB ###
    updateSelectInput(session, "x_plot", label = "X Columns",
                      selected = "NTLD",
                      choices = colnames_numint(df_unfiltered()))
    updateSelectInput(session, "y_plot", label = "Y Columns",
                      selected = "CONC",
                      choices = colnames_numint(df_unfiltered()))
    
    updateSelectInput(session, "color_plot", label = "Color Columns", 
                      choices = c(union (colnames_unique (df_unfiltered()),
                                         colnames_char (df_unfiltered())), "NA"))
    
    # potential bug : NA as string or NA as empty data
    updateSelectInput(session, "group_plot", label = "Grouping", 
                      choices = c("NA",colnames_unique (df_unfiltered(), n = 30)))
    
    ### BOX PLOT TAB ###
    updateSelectInput(session, "x_box", label = "X Columns", 
                      choices = colnames_unique(df_unfiltered(), n = 15))
    updateSelectInput(session, "y_box", label = "Y Columns", 
                      choices = colnames_numint(df_unfiltered()))
    updateSelectInput(session, "id_box", label = "ID Columns", 
                      choices = c("NA",names (df_unfiltered())))
    
    ### HISTOGRAM TAB ###
    updateSelectInput(session, "x_hist", label = "X Columns", 
                      choices = colnames_numint(df_unfiltered()))
    
    ### SUMMARY TABLE ###
    updateSelectInput(session, "id_summ", label = "ID Columns", 
                      choices = names(df_unfiltered()))
    updateSelectInput(session, "tar_summ", label = "Target Columns", 
                      choices = names(df_unfiltered()))
  })
  
  
  # filter raw data based on filter 
  # all plots use df  
  df <- reactive( return (filter_dplyr (df_unfiltered(), input$filtering)))
  
  # return list of parameter for point and line plot 
  plot_reac <- reactive ({
    point_line_plot (df         = df (), 
                     x_col      = input$x_plot, 
                     y_col      = input$y_plot, 
                     x_lab      = input$xlab_plot, 
                     y_lab      = input$ylab_plot, 
                     title_lab  = input$main_plot,
                     plot_type  = input$ptype_plot, 
                     color_col  = input$color_plot, 
                     group_col  = input$group_plot, 
                     lines_type = input$ltype_plot, 
                     log_x      = input$logx_plot,
                     log_y      = input$logy_plot)
  })
    
  # return list of parameter for box plot 
  box_reac <-  reactive ({
    box_plot_func   (df         = df (), 
                     x_col      = input$x_box, 
                     y_col      = input$y_box,
                     id_col     = input$id_box,
                     x_lab      = input$xlab_box, 
                     y_lab      = input$ylab_box, 
                     title_lab  = input$main_box)
  })
  
  # return list of parameter for histogram plot 
  hist_reac <-  reactive ({
    hist_plot_func  (df         = df (), 
                     x_col      = input$x_hist, 
                     x_lab      = input$xlab_hist, 
                     y_lab      = input$ylab_hist, 
                     title_lab  = input$main_hist)
  })
  
  # creating table summary
  table_summ_reac <- reactive ({
    id_obs_func (df = df(), 
                 grp = input$tar_summ, 
                 id_col = input$id_summ)
    })
  
  # render Plot - point , box, hist
  output$plot_plot <- renderPlotly(ggplot_and_plotly(df(),plot_reac()))
  output$plot_box <- renderPlotly(ggplot_and_plotly(df(),box_reac(),input$id_box))
  output$plot_hist <- renderPlotly(ggplot_and_plotly(df(),hist_reac()))
  
  # table summ output
  output$table_summ <- renderTable({
    table_summ_reac()
  })
  
  ### Saving Summary Table ###
  output$download_summ <- downloadHandler(
    filename = function() {
      paste("Summary_Table_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(table_summ_reac (), 
                file, row.names = FALSE)
    }
  )
  
  # warning message #
  output$warning_plot <- renderText({
    warning_message (df(), input$filtering, input$x_plot, input$y_plot)
  })
  
  output$warning_box <- renderText({
    warning_message (df(), input$filtering, input$x_box, input$y_box, input$id_box)
  })
  
  output$warning_hist <- renderText({
    warning_message (df(), input$filtering, input$x_hist)
  })
  
  ### RESULT TAB (REACTIVE VALUE) ###
  
  li_res <- reactiveValues()
  # filtered from input$filtering
  # id_col for extra filter in box plot
  # gg_param : list of parameter for ggplot 
  
  n_li_res <- reactiveVal(0) # how many plots in result tab
  
  # "Add Plot Button" for point tab 
  observeEvent(input$addres_plot,{
    n_li_res(isolate(n_li_res() + 1))
    li_res[[paste0 ("gg_param_", n_li_res())]] <- isolate (plot_reac ())
    li_res[[paste0 ("filtered_", n_li_res())]] <- isolate (input$filtering)
    li_res[[paste0 ("id_col_", n_li_res())]] <- "NA"
  })
  
  # "Add Plot Button" for box tab
  observeEvent(input$addres_box,{
    n_li_res(isolate(n_li_res() + 1))
    li_res[[paste0 ("gg_param_", n_li_res())]] <- isolate (box_reac ())
    li_res[[paste0 ("filtered_", n_li_res())]] <- isolate (input$filtering)
    li_res[[paste0 ("id_col_", n_li_res())]] <- isolate (input$id_box)
  })
  
  # "Add Plot Button" for hist tab
  observeEvent(input$addres_hist,{
    n_li_res(isolate(n_li_res() + 1))
    li_res[[paste0 ("gg_param_", n_li_res())]] <- isolate (hist_reac ())
    li_res[[paste0 ("filtered_", n_li_res())]] <- isolate (input$filtering)
    li_res[[paste0 ("id_col_", n_li_res())]] <- "NA"
  })
  
  # reset n_li_res if file_input change 
  observeEvent(df_unfiltered(),{
    n_li_res(0)
    })
  
  # function for plotting
  multi_plotting_res <- function (num){
    output[[paste0("plot_res_",num)]] <- renderPlot({
      data <- filter_dplyr (df_unfiltered(), li_res[[paste0("filtered_",num)]])
      
      ggplot_only_fun(data, 
                      li_res[[paste0("gg_param_",num)]],
                      li_res[[paste0("id_col_",num)]]
                      )
      })
  }
  
  # create UI and plot using multi_plotting_res function
  output$multi_plt_res <- renderUI({
    req (n_li_res () != 0)
    map(paste0 ("plot_res_",1:n_li_res()), ~ plotOutput(.x))
    map (1:n_li_res(), ~ multi_plotting_res(.x))
  })
  
  
  output$download_res <- downloadHandler(
    filename = function() {
      paste("EDA_plots_", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
      # copy to temp directory to get away with write permission
      tempReport <- file.path(tempdir(), "result.Rmd")
      file.copy("result.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(li_res = reactiveValuesToList(li_res),
                     df_unfiltered = isolate (df_unfiltered()),
                     n = isolate (n_li_res()),
                     title_text = isolate (input$title_res),
                     desc = isolate (input$desc_res)
                     )
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
})
