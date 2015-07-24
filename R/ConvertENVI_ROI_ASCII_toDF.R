library(stringr)
library(dplyr)
library(magrittr)


# ascii_file_path <- "ROIs/pan_spot_subset_ROI.txt"
# InputPath <- "../PAN_SPOT/ROIs/pan_spot_ROI.txt"
# OutputPath <- "../PAN_SPOT/ROIs/pan_spot_ROI_df.txt"



asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                                   asNumeric))

ConvertEnviROI_toDataFrame <- function(ascii_file_path) {
  a <- readLines(ascii_file_path)
  n_rois <- str_sub(a[2], start = nchar(a[2]), end = nchar(a[2])) %>%
    as.numeric()
  # Get the names of the ROIs
  lines_w_names <- a[seq(from = 5, to = 5+4*(n_rois-1), by = 4)]
  remove <- "; ROI name:  "
  names_rois <- str_sub(lines_w_names,nchar(remove),nchar(lines_w_names))
  
  # Get the number of pixels for each ROI
  lines_w_npts <- a[seq(from = 7, to = 7+4*(n_rois-1), by = 4)]
  remove <- "; ROI npts:  "  
  npts_rois <- str_sub(lines_w_npts, nchar(remove), nchar(lines_w_npts)) %>%
    as.numeric()
  
  w <- matrix(c(names_rois,npts_rois),ncol = 2)
  classes <- apply(w,1, FUN = function(X) rep(X[1],each = X[2])) %>%
    unlist()
  
  col_names <- a[(9+(n_rois-1)*4)-1] %>%
    str_split(pattern = "+  +") %>%
    unlist()
  
  c <- a[(9+(n_rois-1)*4) : length(a)]
  d <- sapply(c,FUN = function(X) str_split(X,pattern = "+  +"))
  e <- data.frame(do.call(rbind, d))
  names(e) <- col_names
  e <- e %>% filter(Y != "")
  e$Class <- classes
  e %<>% dplyr::select(`Map X`, `Map Y`, B1, B2, B3, B4, Class)
  row.names(e) <- NULL
  e <- factorsNumeric(e)
  return(e)
}


# classified_roi_df <- ConvertEnviROI_toDataFrame(InputPath)
# 
# write.csv(classified_roi_df, OutputPath)
# 
# 
# classified_roi_df %>% 
#   group_by(Class) %>%
#   summarize(meanb1 = mean(B1),
#             meanb2 = mean(B2),
#             meanb3 = mean(B3),
#             meanb4 = mean(B4)
#             )
























# 
# 
# 
# 
# ConvertEnviROI_toDataFrame <- function(ascii_file_path, name_of_Class) {
#   a <- readLines(ascii_file_path)
#   c <- a[9:length(a)]
#   d <- sapply(c,FUN = function(X) str_split(X,pattern = "+  +"))
#   e <- data.frame(do.call(rbind, d))
#   names(e) <- unlist(str_split(a[8],pattern = "+  +"))
#   e$Class <- name_of_Class
#   e %<>% dplyr::select(`Map X`, `Map Y`, B1, B2, B3, B4, Class)
#   row.names(e) <- NULL
#   return(e)
# }
# 
# 
# ROI_Soil <- ConvertEnviROI_toDataFrame("ROIs/Pan_SPOT/pan_spot_soil_ROI.txt", "Soil")
# ROI_Tree <- ConvertEnviROI_toDataFrame("ROIs/Pan_SPOT/pan_spot_tree_ROI.txt", "Tree")
# ROI_Grass <-ConvertEnviROI_toDataFrame("ROIs/Pan_SPOT/pan_spot_grass_ROI.txt", "Grass")
# ROI_Impervious <- ConvertEnviROI_toDataFrame("ROIs/Pan_SPOT/pan_spot_impervious_ROI.txt", "Impervious")
# ROI_Water <- ConvertEnviROI_toDataFrame("ROIs/Pan_SPOT/pan_spot_water_ROI.txt", "Water")
# 
# ROI_list <- list(ROI_Soil,ROI_Tree, ROI_Grass, ROI_Impervious, ROI_Water)
# 
# # total number of pixels
# l <- sum(unlist(lapply(ROI_list, nrow)))
# w <- length(ROI_Soil)
# 
# ROI_combined <- matrix(data = do.call("rbind",ROI_list), nrow = l, ncol = w)
# 
# str(ROI_combined)
# 
# nrow(ROI_Water)
# 
# ROI <- data.frame()
# 
# View(ROI_Soil)







