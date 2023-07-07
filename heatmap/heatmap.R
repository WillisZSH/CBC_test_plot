# Script for CBC heatmap plot

suppressWarnings(suppressMessages( require( rjson ) ) )
suppressWarnings(suppressMessages( require( stringr ) ) )
suppressWarnings(suppressMessages( require( openxlsx ) ) )
suppressWarnings(suppressMessages( require( tidyverse ) ) )  
suppressWarnings(suppressMessages( require( ggplot2 ) ) )
suppressWarnings(suppressMessages( require( viridis ) ) )
suppressWarnings(suppressMessages( require( plotly ) ) )
suppressWarnings(suppressMessages( require( d3heatmap ) ) )
suppressWarnings(suppressMessages( require( pheatmap ) ) ) 
suppressWarnings(suppressMessages( require( ggplotify ) ) )  
suppressWarnings(suppressMessages( require( lubridate ) ) ) 

# Define the function ----
# Edit the custom function in a way that it accommodates both bold face and italic
# both bold face and italic 
make_face_names <- function(mat, rc_fun, rc_names_b = NA, 
                            rc_names_i = NA) {
  f_names <- rc_fun(mat)
  ids_b <- rc_names_b %>% match(rc_fun(mat))
  ids_i <- rc_names_i %>% match(rc_fun(mat))
  
  ids_b %>%
    walk(
      function(i)
        f_names[i] <<-
        bquote(bold(.(rc_fun(mat)[i]))) %>%
        as.expression()
    )
  ids_i %>%
    walk(
      function(i)
        f_names[i] <<-
        bquote(italic(.(rc_fun(mat)[i]))) %>%
        as.expression()
    )
  
  f_names
}
# bold
make_bold_names <- function(mat, rc_fun, rc_names) {
  bold_names <- rc_fun(mat)
  ids <- rc_names %>% match(rc_fun(mat))
  ids %>%
    walk(
      function(i)
        bold_names[i] <<-
        bquote(bold(.(rc_fun(mat)[i]))) %>%
        as.expression()
    )
  bold_names
}
# italic
make_italic_names <- function(mat, rc_fun, rc_names) {
  italic_names <- rc_fun(mat)
  ids <- rc_names %>% match(rc_fun(mat))
  ids %>%
    walk(
      function(i)
        italic_names[i] <<-
        bquote(italic(.(rc_fun(mat)[i]))) %>%
        as.expression()
    )
  italic_names
}

# Calculate the mean of every group in multiple test 
Group_mean <- function(df)
{
  colname <- colnames(df)
  group_uniq <- unique(df[,1])
  df_partial_end <- unique(df[,c(27:31)])
  df_mean <- data.frame()
  df_mean_all <- data.frame()
  for (i in 1:length(group_uniq))
  {
    df_2 <- df[str_which(df[,1], paste0("^", group_uniq[i], "$")),]
    df_2 <- as.data.frame(lapply( df_2, as.numeric))
    uniq_group_mean <- colMeans(df_2[c(2:26)], na.rm = TRUE)
    df_mean <- data.frame(c(group_uniq[i], uniq_group_mean, df_partial_end[i,]))
    colnames(df_mean) <- colname
    df_mean_all <- rbind(df_mean_all, df_mean)
  }
  return(df_mean_all)
}

# File read in and sorting ----
args <- commandArgs( trailingOnly = TRUE )

file_dir_default <- "/path/to/example.json"
file_dir_args <- c()
file_dir_args <- as.character(args[1])
file_dir <- ifelse(is.na(file_dir_args), file_dir_default, file_dir_args)
config = rjson::fromJSON( file = file_dir )
df <- read.xlsx(config$files$input, sheet = 1)

title_for_plot <-c("", "", "WBC count", "NEUT count", "LYMPH count", "MONO count", "EOS count", "BASO count", "NEUT value", "LYMPH value", "Mono value", "EOS value", "BASO value", 
                   "NLR value", "RBC count", "HGB value", "HCT value", "MCV value", "MCH value", "MCHC value", "RDW-CV", "RET count", "RET value", "PLT count", "MPV value", 
                   "PCT value", "PDW value", "", "")

transpose_df <- as.data.frame(t(df), stringsAsFactors = F)
D3_df <- transpose_df[,3:31]
D3_df[1,2] <- "Sample ID"
Items_D3 <- title_for_plot
Items_D3[c(1, 2, 28,29)] <- c("Grouping", "Sample_ID", "Hemolytic", "Clots")
Items_D3 <- str_replace_all(Items_D3, " ", "_")
colnames(D3_df) <- Items_D3
D3_df <- D3_df[-1,]
Grouping_assign_D3 <- str_match(D3_df$Grouping, pattern = "(.*)_(.*)_(.*)")
D3_df$Mouse <- Grouping_assign_D3[,2]
D3_df$Challenge <- Grouping_assign_D3[,3]
D3_df$Day <- Grouping_assign_D3[,4]

D6_df <- transpose_df[,37:65]
D6_df[1,2] <- "Sample ID"
Items_D6 <- title_for_plot
Items_D6[c(1, 2, 28,29)] <- c("Grouping", "Sample_ID", "Hemolytic", "Clots")
Items_D6 <- str_replace_all(Items_D6, " ", "_")
colnames(D6_df) <- Items_D6
D6_df <- D6_df[-1,]
Grouping_assign_D6 <- str_match(D6_df$Grouping, pattern = "(.*)_(.*)_(.*)")
D6_df$Mouse <- Grouping_assign_D6[,2]
D6_df$Challenge <- Grouping_assign_D6[,3]
D6_df$Day <- Grouping_assign_D6[,4]
Df_3_6 <- rbind(D3_df, D6_df)
Df_3_6$Day_ch <- paste0(Df_3_6$Day, "_", Df_3_6$Challenge)

# Detect the empty in data frame, delete the raw of empty
True_NA <- apply(Df_3_6, 2, function(x) is.na(x))[,1]
if (any(True_NA)) {
  Df_3_6_no_ref <- Df_3_6[-which(True_NA),]
}else{
  Df_3_6_no_ref <- Df_3_6
}

# Combined two mouse group in one plot ----
Df_3_6_no_ref$Day_Ms_Ch <- paste0(Df_3_6_no_ref$Day, "_", Df_3_6_no_ref$Mouse, "_", Df_3_6_no_ref$Challenge)
DMC_unique <- unique(Df_3_6_no_ref$Day_Ms_Ch)
lth_uniq_Day <- length(unique(Df_3_6_no_ref$Day))
lth_uniq_mouse <- length(unique(Df_3_6_no_ref$Mouse))
lth_unique_challenge <- length(unique(Df_3_6_no_ref$Challenge))
Day_rep <- rep(unique(Df_3_6_no_ref$Day), each = lth_uniq_mouse*lth_unique_challenge)
Ms_rep <- rep(rep(sort(unique(Df_3_6_no_ref$Mouse)), each = lth_unique_challenge), lth_uniq_Day)
challenge_rep <- rep(unique(Df_3_6_no_ref$Challenge), lth_uniq_Day*lth_uniq_mouse)
ls_colnames_2 <- colnames(Df_3_6_no_ref)

# set the order of x-axis title
Df_3_6_no_ref$Day_Ms_Ch <- factor( as.character(Df_3_6_no_ref$Day_Ms_Ch), c(DMC_unique))
Df_3_6_heatmap <- Df_3_6_no_ref[,-c(2,28,29)]

# Calculate the mean of every group in multiple test 
df_mean_com <- Group_mean(Df_3_6_heatmap)
rownames(df_mean_com) <- df_mean_com[, 1]
df_com_no_group <- df_mean_com[,-c(1,27:31)]


# Customize condition----
# CBC test order
custom_name_cbc <- c()
if(is.null(config$order$CBC_order$name)) 
{
  base::print("---- No customized CBC order parameters were detected ----")
}else{
  custom_name_cbc <- c(config$order$CBC_order$name)
  df_com_no_group <- df_com_no_group[, custom_name_cbc]
  colnames(df_com_no_group) <- str_replace_all(colnames(df_com_no_group), "_", " ")
}

# Grouping order test
custom_name_group <- c()
if(is.null(config$order$Group_order$name))
{
  base::print("---- No customized group order parameters were detected ----")
}else{
  custom_name_group <- c(config$order$Group_order$name)
  df_com_no_group <- df_com_no_group[custom_name_group, ]
  rownames(df_com_no_group) <- str_replace_all(rownames(df_com_no_group), "_", "/")
}

# Determining the appropriate clustering orientation by specifying whether the clustering should be applied to the rows or columns of the dataset
col_cluster_logical <- as.logical(config$cluster$CBC$logical)
row_cluster_logical <- as.logical(config$cluster$Grouping$logical)

# Customize of margin width 
cellwidth_no_correlate <- 29
cellwidth_correlate <- 26
cellwidth_cus <- ifelse(isTRUE(row_cluster_logical), cellwidth_correlate, cellwidth_no_correlate)

# pheatmap ----

hm_pheat <- pheatmap(df_com_no_group, cluster_cols = col_cluster_logical, cluster_rows = row_cluster_logical, scale = "column", angle_col = 45, 
                     color=colorRampPalette(c("#538235", "#fff2cc", "#ff0200"))(50), cellwidth = cellwidth_cus, 
                     labels_row = make_bold_names(df_com_no_group, rownames, rownames(df_com_no_group)), labels_col = make_bold_names(df_com_no_group, colnames, colnames(df_com_no_group)))


ggsave( plot = hm_pheat, filename = paste0(config$files$output_folder,  "/","CBC_heatmap_", now(),".pdf"), width = round((cellwidth_cus/2)-(cellwidth_cus/2)*0.1), height = 10, units = "in", dpi = "retina" )




