# Script for CBC result plot
# version same as CBC_result_plot_command_line_tool_Ko_V3.7.R
# updated at 2022/03/22 
# Add 1.Conditional control of True_NA 2. Conditional control of config$stype$custom with or without parameters
# for individual and combined plot
# Usage: Rscript CBC_barplot.R ~/Desktop/CBC_json_R/CBC_arguments.json
# Waring: Grouping order must be fixed (Mouse_treatment_Day)
# Waring:
suppressWarnings(suppressMessages( require( rjson ) ) )
suppressWarnings(suppressMessages( require( stringr ) ) )
#suppressWarnings(suppressMessages( require( seqinr ) ) )
suppressWarnings(suppressMessages( require( openxlsx ) ) )
suppressWarnings(suppressMessages( require( tidyverse ) ) )
suppressWarnings(suppressMessages( require( ggplot2 ) ) )
suppressWarnings(suppressMessages( require( ggthemes ) ) )
suppressWarnings(suppressMessages( require( ggpubr ) ) )

args <- commandArgs( trailingOnly = TRUE )

#file_dir_default <- "~/Desktop/CBC_anaysis/CBC_arguments.json"
file_dir_default <- "~/Desktop/.../CBC_arguments_test_20230314_test4.json"
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
# Combined two df
Df_3_6 <- rbind(D3_df, D6_df)
Df_3_6$Day_ch <- paste0(Df_3_6$Day, "_", Df_3_6$Challenge)

# Detect the empty in data frame, delete the raw of empty
True_NA <- apply(Df_3_6, 2, function(x) is.na(x))[,1]
if (any(True_NA)) {
  Df_3_6_no_ref <- Df_3_6[-which(True_NA),]
}else{
  Df_3_6_no_ref <- Df_3_6
}

# combined two mouse group in one plot ----
Df_3_6_no_ref$Day_Ms_Ch <- paste0(Df_3_6_no_ref$Day, "_", Df_3_6_no_ref$Mouse, "_", Df_3_6_no_ref$Challenge)
DMC_unique <- unique(Df_3_6_no_ref$Day_Ms_Ch)
lth_uniq_Day <- length(unique(Df_3_6_no_ref$Day))
lth_uniq_mouse <- length(unique(Df_3_6_no_ref$Mouse))
lth_unique_challenge <- length(unique(Df_3_6_no_ref$Challenge))
Day_rep <- rep(unique(Df_3_6_no_ref$Day), each = lth_uniq_mouse*lth_unique_challenge)
Ms_rep <- rep(rep(sort(unique(Df_3_6_no_ref$Mouse)), each = lth_unique_challenge), lth_uniq_Day)
challenge_rep <- rep(unique(Df_3_6_no_ref$Challenge), lth_uniq_Day*lth_uniq_mouse)
ls_colnames_2 <- colnames(Df_3_6_no_ref)
color <- rep(c("gray38", "gray70"), 20)
fill_color <- color[1:(length(unique(Df_3_6_no_ref$Grouping)))]
line_size <- 1 # defining variable upfront as we will re-use it
base_size <- 12 # defining separately, same as for line_size
axis_text_rel_size = -1
title_text_rel_size = +2
y_axis <- c("", "", "K/uL", "K/uL", "K/uL", "K/uL", "K/uL", "K/uL", "%", "%", "%", "%", "%", "Ratio", "M/uL", "g/dL", "%", "fL", "pg", "g/dL", "%", "K/uL", "%", "K/uL", "fL", "%", "fL", "",
            "")
plot_list = c()
list_of_all_plot = list()
plot_number <- c(paste0("plot", c(3:27)))

# check the levels of factor
levels(factor( as.character(Df_3_6_no_ref$Day_Ms_Ch), c(DMC_unique)))

# set the order of x-axis title
Df_3_6_no_ref$Day_Ms_Ch = factor( as.character(Df_3_6_no_ref$Day_Ms_Ch), c(DMC_unique))

# custom name must be the upper case ex: "WBC"
line_number <- 0
custom_name <- c()
if(is.null(config$stype$custom)) 
{
  print("---- No customized parameters were detected ----")
}else{
  for (i in 1:length(config$stype$custom))
  {
    line_number <- line_number + 1
    name <- config$stype$custom[[i]]$name
    custom_name <- c(custom_name, name)
  }
}

#for loop can not save multiple plot in one list properly, so i use lapply instead, which can save multiple plot in ine list, will not show weird error or bad present of plot
for (i in 3:27)
{
  print(paste0("i = ", i))
  colname_temp <- ls_colnames_2[i]
  colname_temp_short <- str_match(colname_temp, "(([A-Za-z]*)(-|_))([A-Za-z]*)")[,3]   #str_match(colname_temp, "([A-Za-z]*)(-| )([A-Za-z]*)")[,2]  #(-| ) space can also be match
  colname_temp_short2 <- str_match(colname_temp, "(([A-Za-z]*)(-|_))([A-Za-z]*)")[,5]
  colnames_temp_partial <- str_match(colname_temp, "(([A-Za-z]*)(-|_))([A-Za-z]*)")[,2]
  colnames_temp_partial2 <- gsub("_", " ", colnames_temp_partial)
  #y_axis_min <- as.numeric(Df_3_6_no_ref[which.min(as.numeric(Df_3_6_no_ref[,i])), i])
  y_axis_min <- 0
  y_axis_max <- as.numeric(Df_3_6_no_ref[which.max(as.numeric(Df_3_6_no_ref[,i])), i])* 1.2
  default_y_value <- c(y_axis_min, y_axis_max)
  if(any(str_detect(custom_name, colname_temp_short, negate = FALSE) & str_detect(custom_name, colname_temp_short2, negate = FALSE)))
    {
      match_pos <- intersect(str_which(custom_name,  colnames_temp_partial2,  negate = FALSE ), str_which(custom_name,  colname_temp_short2,  negate = FALSE ))
      input_ymin_value <- c()
      input_ymax_value <- c()
      input_ymin_value <- config$stype$custom[[match_pos]]$y_limit[1]
      input_ymax_value <- config$stype$custom[[match_pos]]$y_limit[2]
      y_axis_min_fin <- ifelse(is.null(input_ymin_value), y_axis_min, input_ymin_value)
      y_axis_max_fin <- ifelse(is.null(input_ymax_value), y_axis_max, input_ymax_value)
      y_axis_limit <- c(y_axis_min_fin, y_axis_max_fin)
      print(paste0("--------Change ", colname_temp_short, " ", colname_temp_short2, " y-axis doned -------"))
    }else{
      y_axis_limit <- default_y_value
    }
  #ggbreak for y-axis jump
  temp_plot =
    Df_3_6_no_ref%>%
    ggplot(aes(x = Day_Ms_Ch, y = as.numeric(Df_3_6_no_ref[,i])), na.rm = TRUE) +
    # plot bars
    geom_bar(stat="summary", fun = "mean", fill = fill_color, color = "black", size = 1, width = 0.7, na.rm = TRUE)+
    # stat_summary(fun = mean, geom = "bar", width = 0.7) +
    # plot error bars
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
                 geom = "errorbar", width = 0.3, size = line_size, na.rm = FALSE) +
    # plot individual points
    geom_jitter(width = 0.1, na.rm = FALSE) +
    #  geom_jitter(position = position_jitter(width = 0.1), na.rm = FALSE)v+
    #geom_point(na.rm = FALSE) +
    # set scale limits
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    coord_cartesian(ylim = as.numeric(y_axis_limit))+
    #set x-axis text label
    scale_x_discrete(labels = DMC_unique)+
    # set labs
    labs(title = title_for_plot[i], x = "", y = y_axis[i]) +
    # theme
    theme_foundation(base_size = base_size, base_family = "sans") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      text = element_text(colour = "black"),
      plot.title = element_text(face = "bold",
                                size = rel((title_text_rel_size + base_size) / base_size), hjust = 0.5),
      axis.line = element_line(colour="black", size = line_size),
      axis.ticks = element_line(colour="black", size = line_size),
      axis.title = element_text(face = "bold", size = rel(1)),
      axis.title.y = element_text(angle = 90, vjust = 2),
      axis.title.x = element_text(vjust = -0.2),
      axis.text = element_text(face = "bold", size = rel((axis_text_rel_size + base_size) / base_size)),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      plot.background = element_blank()
    )
  title_for_plot[i] <- gsub("\\s", "_", title_for_plot[i])
  title_for_plot[i] <- gsub("-", "_", title_for_plot[i])
  plot_list = c(plot_list, paste(title_for_plot[i], "_plot", sep = ""))
  assign(paste(title_for_plot[i], "_plot", sep = ""), temp_plot)
  print(paste("--------", title_for_plot[i],  "_plot finished -------------", sep = ""))
  #figure_save = "~/Desktop/.../Mouse_combined"
  figure_save = config$files$output_folder
  filename = paste0(figure_save, "/", title_for_plot[i], "_plot.pdf")
  ggsave( plot = temp_plot, filename = filename,  height = 6, width = 8, units = "in", dpi = "retina" )
}


rm(list = ls())


args <- commandArgs( trailingOnly = TRUE )

#file_dir_default <- "~/Desktop/CBC_anaysis/CBC_arguments.json"
file_dir_default <- "~/Desktop/.../CBC_arguments_test_20230314_test4.json"
file_dir_args <- c()
file_dir_args <- as.character(args[1])
file_dir <- ifelse(is.na(file_dir_args), file_dir_default, file_dir_args)
config = rjson::fromJSON( file = file_dir )

#Path <- as.character(commandArgs( trailingOnly = TRUE ))
#df <- read.xlsx(Path, sheet = 1)
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

True_NA <- apply(Df_3_6, 2, function(x) is.na(x))[,1]
if (any(True_NA)) {
  Df_3_6_no_ref <- Df_3_6[-which(True_NA),]
}else{
  Df_3_6_no_ref <- Df_3_6
}

# combined two mouse group in one plot ----
Df_3_6_no_ref$Day_Ms_Ch <- paste0(Df_3_6_no_ref$Day, "_", Df_3_6_no_ref$Mouse, "_", Df_3_6_no_ref$Challenge)
DMC_unique <- unique(Df_3_6_no_ref$Day_Ms_Ch)
lth_uniq_Day <- length(unique(Df_3_6_no_ref$Day))
lth_uniq_mouse <- length(unique(Df_3_6_no_ref$Mouse))
lth_unique_challenge <- length(unique(Df_3_6_no_ref$Challenge))
Day_rep <- rep(unique(Df_3_6_no_ref$Day), each = lth_uniq_mouse*lth_unique_challenge)
Ms_rep <- rep(rep(sort(unique(Df_3_6_no_ref$Mouse)), each = lth_unique_challenge), lth_uniq_Day)
challenge_rep <- rep(unique(Df_3_6_no_ref$Challenge), lth_uniq_Day*lth_uniq_mouse)
ls_colnames_2 <- colnames(Df_3_6_no_ref)
color <- rep(c("gray38", "gray70"), 20)
fill_color <- color[1:(length(unique(Df_3_6_no_ref$Grouping)))]
line_size <- 1 # defining variable upfront as we will re-use it
base_size <- 12 # defining separately, same as for line_size
axis_text_rel_size = -1
title_text_rel_size = +2
y_axis <- c("", "", "K/uL", "K/uL", "K/uL", "K/uL", "K/uL", "K/uL", "%", "%", "%", "%", "%", "Ratio", "M/uL", "g/dL", "%", "fL", "pg", "g/dL", "%", "K/uL", "%", "K/uL", "fL", "%", "fL", "",
            "")
plot_list = c()
list_of_all_plot = list()
plot_number <- c(paste0("plot", c(3:27)))

# check the levels of factor
levels(factor( as.character(Df_3_6_no_ref$Day_Ms_Ch), c(DMC_unique)))

# set the order of x-axis title
Df_3_6_no_ref$Day_Ms_Ch = factor( as.character(Df_3_6_no_ref$Day_Ms_Ch), c(DMC_unique))

# custom name must be the upper case ex: "WBC"
line_number <- 0
custom_name <- c()
if(is.null(config$stype$custom)) 
{
  print("---- No customized parameters were detected ----")
}else{
  for (i in 1:length(config$stype$custom))
  {
    line_number <- line_number + 1
    name <- config$stype$custom[[i]]$name
    custom_name <- c(custom_name, name)
  }
}

plotList <- lapply(
  c(3:27),
  function(i) {
    print(paste0("i = ", i))
    colname_temp <- ls_colnames_2[i]
    colname_temp_short <- str_match(colname_temp, "(([A-Za-z]*)(-|_))([A-Za-z]*)")[,3]   #str_match(colname_temp, "([A-Za-z]*)(-| )([A-Za-z]*)")[,2]  #(-| ) space can also be match
    colname_temp_short2 <- str_match(colname_temp, "(([A-Za-z]*)(-|_))([A-Za-z]*)")[,5]
    colnames_temp_partial <- str_match(colname_temp, "(([A-Za-z]*)(-|_))([A-Za-z]*)")[,2]
    colnames_temp_partial2 <- gsub("_", " ", colnames_temp_partial)
    #y_axis_min <- as.numeric(Df_3_6_no_ref[which.min(as.numeric(Df_3_6_no_ref[,i])), i])
    y_axis_min <- 0
    y_axis_max <- as.numeric(Df_3_6_no_ref[which.max(as.numeric(Df_3_6_no_ref[,i])), i])* 1.2
    default_y_value <- c(y_axis_min, y_axis_max)
    if(any(str_detect(custom_name, colname_temp_short, negate = FALSE) & str_detect(custom_name, colname_temp_short2, negate = FALSE)))
    {
      match_pos <- intersect(str_which(custom_name,  colnames_temp_partial2,  negate = FALSE ), str_which(custom_name,  colname_temp_short2,  negate = FALSE ))
      input_ymin_value <- c()
      input_ymax_value <- c()
      input_ymin_value <- config$stype$custom[[match_pos]]$y_limit[1]   
      input_ymax_value <- config$stype$custom[[match_pos]]$y_limit[2]
      y_axis_min_fin <- ifelse(is.null(input_ymin_value), y_axis_min, input_ymin_value)
      y_axis_max_fin <- ifelse(is.null(input_ymax_value), y_axis_max, input_ymax_value)
      y_axis_limit <- c(y_axis_min_fin, y_axis_max_fin)
      print(paste0("--------Change ", colname_temp_short, " ", colname_temp_short2, " y-axis doned -------"))
    }else{
      y_axis_limit <- default_y_value 
    }
    
    # temp_plot2 is for multiple plot export
    temp_plot2 =
      Df_3_6_no_ref%>%
      ggplot(aes(x = Day_Ms_Ch, y = as.numeric(Df_3_6_no_ref[,i])), na.rm = TRUE) +
      # plot bars
      geom_bar(stat="summary", fun = "mean", fill = fill_color, color = "black", size = 1, width = 0.7, na.rm = TRUE)+
      # stat_summary(fun = mean, geom = "bar", width = 0.7) +
      # plot error bars
      stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
                   geom = "errorbar", width = 0.3, size = line_size, na.rm = FALSE) +
      # plot individual points
      geom_jitter(size = 1, width = 0.1, na.rm = FALSE) +
      #  geom_jitter(position = position_jitter(width = 0.1), na.rm = FALSE)v+
     # geom_point(na.rm = FALSE) +
      # set scale limits
      scale_y_continuous(expand = expansion(mult = c(0, 0))) + 
      coord_cartesian(ylim = as.numeric(y_axis_limit))+
      #set x-axis text label
      scale_x_discrete(labels = DMC_unique)+
      # set labs
      labs(title = title_for_plot[i], x = "", y = y_axis[i]) + 
      ggtitle(title_for_plot[i])+
      # theme
      theme_foundation(base_size = base_size, base_family = "sans") + 
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(colour = "black"),
        plot.title = element_text(face = "bold", 
                                  size = 10, hjust = 0.5),
        axis.line = element_line(colour="black", size = line_size),
        axis.ticks = element_line(colour="black", size = line_size),
        axis.title = element_text(face = "bold", size = rel(1)),
        axis.title.y = element_text(size = 8, angle = 90, vjust = 2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(face = "bold", size = rel((axis_text_rel_size + base_size) / base_size)),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 8),
        plot.background = element_blank()
      )
    title_for_plot[i] <- gsub("\\s", "_", title_for_plot[i])
    title_for_plot[i] <- gsub("-", "_", title_for_plot[i])
    plot_list = c(plot_list, paste(title_for_plot[i], "_plot", sep = ""))
    assign(paste(title_for_plot[i], "_plot", sep = ""), temp_plot2)
    print(paste("--------", title_for_plot[i],  " multiple plot combined finished -------------", sep = ""))
    #figure_save = "~/Desktop/.../Mouse_combined"
    figure_save = config$files$output_folder
    filename = paste0(figure_save, "/", title_for_plot[i], "_plot2.pdf")
    #ggsave( plot = temp_plot2, filename = filename,  height = 6, width = 8, units = "in", dpi = "retina" )
    temp_plot2 #put temp_plot in list
  }
)

allplots <- ggarrange(plotlist=plotList,
                      #labels = c("A", "B", "C", "D"),
                      common.legend = TRUE,
                      ncol = 3, nrow = 3)
figure_save = config$files$output_folder
ggexport(allplots, filename = paste0(figure_save, "/allplots.pdf"))







