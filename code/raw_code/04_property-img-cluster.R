###############################################################################
## Author: Andrew Rosa                                                       
##                                                                           
## Notes:                                                                    
##                                                                           
##                                                                           
##                                                                           
###############################################################################

library(RPostgreSQL)
library(dplyr)
library(aws.s3)
library(magick)
library(EBImage)
library(optCluster)
library(fpc)
library(filesstrings)
source("helper_scripts/helper00_project-db-connection.R")


# Load in master building data frame from Data Warehouse.
buildings_df <- dbGetQuery(defaultdb, "SELECT * from properties_w_neighborhoods")

pins <- buildings_df$PIN

set.seed(12459)
samp <- sample(pins, 600)

samp2 <- samp[!grepl("E0001568002", samp)] 
samp2 <- samp2[!grepl("C0060400030", samp2)]


base_dst_file_pth <- "../../data/temp_data/"

for(i in 1:length(samp2)){
  img_file <- paste0(samp2[i], ".jpeg")
  dst_path <- paste0(base_dst_file_pth, img_file)
  
  save_object(img_file, file = dst_path, 
              bucket = "anrosa-capstone-bucket/property-imgs", overwrite = TRUE)
  print(paste0(i, " of ", length(samp2)))
}

img_paths <- list.files("../../data/temp_data", full.names = TRUE )
img_paths <- img_paths[!grepl("N0000740006.jpeg", img_paths)]
img_paths <- img_paths[!grepl("N0002046001.jpeg", img_paths)]
img_paths <- img_paths[!grepl("C0010891144.jpeg", img_paths)]


samp3 <- sample(img_paths, 140)
samp3 <- samp3[!grepl("C0010891144.jpeg", samp3)]

imgs <- image_read(samp3)

# reduce size
imgs_cropped <- image_crop(imgs, "300x300")

# rescale, all the same size
imgs_scaled <- image_scale(imgs_cropped, "50x50")


x2 <- vector("list", length = length(imgs_scaled))
for(i in seq_along(imgs_scaled)){
  x1 <- as_EBImage(imgs_scaled[i])
  x2[[i]] <-  as.vector(imageData(x1))
}


names(x2) <- samp3

x3 <- bind_rows(x2)
x4 <- t(x3)

opclust_out <- optCluster(x4, nClust = 2:10, clMethods = c("kmeans"), 
                          validation = "stability")

k_out <- kmeans(x4, max(optAssign(opclust_out)$cluster))
x5 <- discrcoord(x4, k_out$cluster)
plot(x5$proj[ , 1:2], col = k_out$cluster)

pins_names <- rownames(x5$proj)

splits <- str_split_fixed(str_split_fixed(pins_names, "/", n = 5)[,5], "\\.", 
                          n = 2)[,1]
clusters <- k_out$cluster

clst <- data.frame(PIN = splits, group = clusters, row.names = NULL)

d_list <- img_paths[!img_paths %in% samp3]

for(i in 1:1155){
  file.remove(d_list[i])
}

g1 <- clst %>%
  dplyr::filter(group == 9)
g1 <- g1[,1]
  
for(i in 1:length(g1)){
  file_to_move <- paste0("../../data/temp_data/", g1[i], ".jpeg")
  
  file.move(file_to_move, "../../data/temp_data/group9")
}

