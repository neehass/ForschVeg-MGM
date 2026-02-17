# --------------------------
# Vertical Temp 
# --------------------------
# Plotting 
# --------------------------
# Author: Neele 
# --------------------------

# packages 
library(ggplot2)
library(patchwork) # grids for ggplots
# graphics.off()

getwd()

# ---- preprocess data ---------------------------
# load files
files <- list.files("data/seen-vertical-temp/", pattern = "csv", full.names = T)

for(l in files){
  sub <- sub("data/seen-vertical-temp/", "",l)
  nr <- substr(sub, 1, 5)
  
  # preprocess 
  lake <- prepro(l)
  
  # aggregate by seasons and years (meterological)
  result <- agg_seasons(dates = lake$Datum, lake= lake)
  
  lake <- result$lake
  lake_agg <- result$lake_agg
  cl_lake <- result$lake_agg_clean # NAs removed
  
  # View(clean_lake)
  # View(lake_agg)
  # View(lake)
  
  # check if data without na is existing !
  if(is.null(cl_lake) == FALSE ){
    
# ---- plot --------------------------------------
    
    years <- unique(cl_lake$Year)
    
    plot_list <- list()
    for(i in 1:length(years)){
      data_year <- cl_lake[(cl_lake$Year == years[i]),]
      
      if(length(data_year$Season) > 1){
        
        dep_level <- grep("Tiefe", colnames(data_year), value = TRUE)
        
        # long formate
        df_long <- data_year %>%
          pivot_longer(
            cols = dep_level,
            names_to = "Depth",
            values_to = "Temp"
          ) %>%
          mutate(
            # Optional: clean Depth names to numeric depth in meters
            Depth_num = as.numeric(substr(Depth, 1, 3))*-1
          )
        
        # Plot with legend by Season
        p <- ggplot(df_long, aes(x = Temp, y = Depth_num,  ,color = Season)) +
          geom_line() +
          geom_point() +
          coord_cartesian(xlim = c(min(cl_lake[,3:(2+length(dep_level))]), max(cl_lake[,3:(2+length(dep_level))]))) +
          scale_color_manual(values = c(
            "spring" = "green",
            "summer" = "orange",
            "fall" = "black",
            "winter" = "blue"
          ))+
          labs(x = "Temp",y = "Depth (m)", color = "Season",
               title = years[i]) +
          theme_minimal()
        
        plot_list[[i]] <- p # save plot
        
        
        
        
        
        
      } else { print(paste("not all seasons",years[i])) }# plot
    } # for loop 
    # save 
    
    plot_list <- plot_list[!sapply(plot_list, is.null)]
    # patchwork function, automatically arranges
    wrap_plots <- wrap_plots(plot_list) + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
    # print(wrap_plots)
    ggsave(paste("output/",nr,".png"), plot = wrap_plots, width = 10, height = 6, dpi = 300)
    
  } else { print(paste("no data", nr, sep = " ")) }
  
  
}

