library (dplyr)
library (ggplot2)
library (plotly)
library(shiny)
library(stringr)
library (purrr)
library (htmltools)

# need to confirm that only return numeric, integer, character 
reading_df <- function (file_path){
  return (read.csv(file_path, na.strings = "."))
}

# count unique value 
counting_unique <- function (vec) {
  return (length (unique (vec) ))
}

# return sort unique value
s_unique <- function (nums){
  return (sort (unique (nums)))
}

# return column names with class numeric or integer 
colnames_numint  <- function (df){
  return (names (sapply (df, class )[sapply (df, class ) == "integer" | 
                                       sapply (df, class ) == "numeric"] ))
}

# return column names with class character
colnames_char  <- function (df){
  return (names (sapply (df, class )[sapply (df, class ) == "character"] ))
}

# return column names with disctict unique value less than n
colnames_unique  <- function (df, n = 20, less_than = TRUE){
  if (less_than){return (names (sapply (df, counting_unique )[sapply (df, counting_unique ) < n] ))}
  else {return (names (sapply (df, counting_unique )[sapply (df, counting_unique ) > n] ))}
}

# summary table 
id_obs_func <- function (df, grp, id_col ) {
  nobs <- df %>% group_by_at (grp) %>% summarise (NOBS = n()) %>% ungroup () %>% 
    mutate (percent_NOBS = round (100 * NOBS / sum(NOBS),2))
  
  nid <- grp_id <- append (grp, id_col)
  nid <- df %>% select (all_of(grp_id)) %>% unique () %>% 
    group_by_at (grp) %>% summarise (NID = n()) %>% ungroup () 
  
  result <- merge (nobs, nid, by = grp)
  
  total_id <- df[[id_col]] %>% unique () %>% length ()
  
  result <- result %>% mutate (percent_ID = round((NID / total_id)*100,2))
  result <- result %>% mutate (NOBS = paste0 (NOBS , " (", percent_NOBS,"%)"),
                               NID = paste0 (NID , " (", percent_ID,"%)"),) %>%
    select (-percent_NOBS, -percent_ID)
  
  return (result)
}

# filtering data frame
filter_dplyr <- function (df, filter_str){
  filter_exprs <- filter_str %>% rlang::parse_exprs()
  return (df %>% filter(!!!filter_exprs))
} 

# count NA in certain column of dataframe
count_na <- function (df, column) {
  return (sum(is.na(df[[column]])))
}

# geom mean
GM <- function(x){exp(mean(log(x[which(x > 0)])))}

# plotting function 
point_line_plot <- function (df, x_col, y_col, x_lab, y_lab, title_lab, 
                             plot_type, color_col, group_col, lines_type, log_x, log_y) { 
  aes_fun <- aes_string (x = x_col, y = y_col) 
  
  if (color_col %in% colnames_char(df)){
    col_fun <- aes_string (color = color_col)
  }
  else (col_fun <- aes (color = as.character (get(color_col))))
  
  point_fun <- geom_point (shape = 1, alpha = 6/10)
  lab_fun <- labs(x = x_lab,y = y_lab,  title =title_lab)
  
  facet_fun <- facet_wrap(as.formula(paste("~", group_col)))
  logx_fun <- scale_x_continuous(trans='log10')
  logy_fun <- scale_y_continuous(trans='log10')
  
  brewer_color_fun <- scale_color_brewer(palette="Set1")
  
  if (lines_type == "median"){line_fun <- stat_summary (fun = median, geom = "line")}
  else if (lines_type == "geom"){line_fun <- stat_summary (fun = GM, geom = "line")}
  else {line_fun <- stat_summary (fun = mean, geom = "line")}
  
  # boolean TRUE or FALSE
  if ("p_plot" %in% plot_type) {p_bool = TRUE}
  else {p_bool = FALSE}
  if ("l_plot" %in% plot_type) {l_bool = TRUE}
  else {l_bool = FALSE}
  if ( color_col == "NA") {col_bool = FALSE}
  else {col_bool = TRUE}
  if ( group_col == "NA") {f_bool = FALSE}
  else {f_bool = TRUE}
  if (df[[color_col]] %>% unique () %>% length () < 10){brewer_bool = TRUE}
  else {brewer_bool = FALSE}
  
  labcol_fun <- labs (color = paste0 (color_col,
                                      ifelse (!l_bool, "", 
                                              ifelse (lines_type == "median" , " Median",
                                                      ifelse (lines_type == "geom", " Geom Mean", " Mean")) 
                                      )))
  
  
  li <- list (aes_fun, col_fun, point_fun, line_fun, lab_fun, facet_fun, logx_fun, logy_fun, labcol_fun,brewer_color_fun)
  li <- li [c(TRUE, col_bool, p_bool, l_bool, TRUE, f_bool, log_x, log_y,col_bool, brewer_bool)]
  return (li)
  #gg_plt <- ggplot (df) + li
  #ggplotly (gg_plt) %>% config(toImageButtonOptions = list(format = "svg"))
}

# box plot function 
box_plot_func <- function (df, x_col, y_col, id_col, x_lab, y_lab, title_lab){
  #if (id_col != "NA") {df <- df %>% filter (!duplicated (get (id_col)))}
  aes_fun <- aes (x = as.character(get(x_col)), y = get (y_col), fill = as.character(get(x_col)))
  box_fun <- geom_boxplot () 
  lab_fun <- labs(x = x_lab,y = y_lab,  title =title_lab, fill = x_col)
  li = list (aes_fun,lab_fun,box_fun)
  return (li)
  #gg_plt <- ggplot (df) + li
  #ggplotly (gg_plt) %>% config(toImageButtonOptions = list(format = "svg"))
}

# histogram function 
hist_plot_func <- function (df, x_col, x_lab, y_lab, title_lab){
  aes_fun <- aes_string (x = x_col)
  hist_fun <- geom_histogram(color = "black", fill = "white")
  lab_fun <- labs(x = x_lab,y = y_lab,  title =title_lab)
  li = list (aes_fun,lab_fun,hist_fun)
  return (li)
  #gg_plt <- ggplot (df) + li
  #ggplotly (gg_plt) %>% config(toImageButtonOptions = list(format = "svg"))
}

ggplot_and_plotly <- function (df, li, id_col = "NA"){
  if (id_col != "NA") {df <- df %>% filter (!duplicated (get (id_col)))}
  gg_plt <- ggplot (df) + li
  ggplotly (gg_plt) %>% config(toImageButtonOptions = list(format = "svg"))
}

ggplot_only_fun <- function (df, li, id_col = "NA"){
  if (id_col != "NA") {df <- df %>% filter (!duplicated (get (id_col)))}
  ggplot (df) + li
}

warning_message <- function (df, filter_inp, x_col, y_col = "", id_col = "NA"){    
  # filter warning
  if (str_trim(filter_inp, side = "both") ==  "" & id_col == "NA") {
    filter_res <- "There are no filter applied."}
  else {filter_res <- paste0 ("Data is filtered by : ", 
                              filter_inp,
                              ifelse(id_col =="NA","",
                                     paste0(ifelse (str_trim(filter_inp, side = "both") ==  "",
                                                    ""," and "),"!duplicated (",id_col,")"))
  )}
  
  # x column warning 
  xna <- count_na (df,x_col)
  x_res <- ifelse (xna == 0 , "",
                   paste0 ("There are ", xna, " NA in column ", x_col))
  
  # y column warning 
  if (y_col == ""){y_res <- ""}
  else {
    yna <- count_na (df,y_col) 
    y_res <- ifelse (yna == 0 , "",
                     paste0 ("There are ", yna, " NA in column ", y_col))
  }
  
  # combine all warning 
  result <- filter_res
  if (x_res != ""){result <- paste (result, x_res, sep = "\n")}
  if (y_res != ""){result <- paste (result, y_res, sep = "\n")}
  
  return (result)
}

description_fun <- function (li, n){
  # create combined message
  combined_li <- list ()
  for (i in 1 : n){
    filter_text <- str_trim(li[[paste0("filtered_",i)]], side = "both")
    id_text <- ifelse(li[[paste0("id_col_",i)]] == "NA", "", 
                      paste0 ("!duplicated (",li[[paste0("id_col_",i)]], ")")
    )
    and_text <- ifelse (filter_text!= "" & id_text != "", " and ", "")
    res <- paste0 (filter_text,and_text,id_text)
    res <- ifelse (res == "", "None", res)
    if (res %in% names (combined_li)){combined_li[[res]] <- append (combined_li[[res]], i)}
    else {combined_li[[res]] <- c(i)}
  }
  result = list (title = "Plot Description",
                 date =  paste0 ("Created on : ", Sys.time ()))
  i = 1
  for (ele in names (combined_li)){
    result[[paste0("txt_",i)]] <- paste0 ("Data is filtered by : ", 
                                          ele, " in plot ",
                                          paste(combined_li[[ele]], collapse = ", "))
    i<- i + 1
  }
  return (result)
}
