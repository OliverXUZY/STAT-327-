rm(list = ls())
if (!require("png")) {
  install.packages("png")
  stopifnot(require("png"))
}
str(vg <- readPNG("Van_Gogh.png"))
vg = readPNG("Van_Gogh.png")
dim(vg)
vg_red = vg[,,1]
vg_green = vg[,,2]
vg_blue = vg[,,3]

image(t(vg_red[nrow(vg_red):1,]),col = gray((1:12)/13),main = "red channel")

filter = function(mat,k=1){
  nrow = dim(mat)[1]
  ncol = dim(mat)[2]
  pad = matrix(0,nrow + 2*k, ncol + 2*k)
  pad[ (k+1):(nrow + k),(k+1):(ncol + k)  ] = mat
  mat2 = pad
  for(i in (k+1):(nrow + k)){
    for(j in (k+1):(ncol + k)){
      mat2[i,j] = mean(pad[(i-k):(i+k),(j-k):(j+k)])
    }
  }
  mat2 = mat2[ (k+1):(nrow + k),(k+1):(ncol + k)  ]
  return(mat2)
}
#filter(matrix(1,2,2))
vg_li = list(vg[,,1],vg[,,2],vg[,,3])

## unicore == lapply(vg_li,filter)

require("parallel")
n_cores = detectCores()
cluster = makePSOCKcluster(names=3)

## clusterMap usage is the same as mapply
## parLapply usage is the same as lapply

filtered_list = parLapply(cl = cluster, fun = filter,X = vg_li,k=3)
#stopCluster(cl=cluster)

### assemble
final_array = array(c(filtered_list[[1]],filtered_list[[2]],filtered_list[[3]]),
                    dim = dim(vg))

### write
writePNG(final_array,"Oliver_3.png")

#################-------------------------

read_image = function(file_name)
{
  ima = readPNG(file_name)
  #dim(ima)
  ima_red = ima[,,1]
  ima_green = ima[,,2]
  ima_blue = ima[,,3]
  
  ima_li = list(ima[,,1],ima[,,2],ima[,,3])
  return(ima_li)
}

filter_assemble_smooth = function(cores,ima_li)
{
  require("parallel")
  n_cores = detectCores()
  cluster = makePSOCKcluster(names=cores)
  
  filtered_list1 = parLapply(cl = cluster, fun = filter,X = ima_li)  ## k=1
  stopCluster(cl=cluster)
  
  ### assemble
  final_array = array(c(filtered_list[[1]],filtered_list[[2]],filtered_list[[3]]),
                      dim = dim(vg))
  return(final_array)
}

#########------------------


#################  detect edges
#################  detect edges
filter_sd = function(mat,k=1,top = 0.01){
  nrow = dim(mat)[1]
  ncol = dim(mat)[2]
  pad = matrix(0,nrow + 2*k, ncol + 2*k)
  pad[ (k+1):(nrow + k),(k+1):(ncol + k)  ] = mat
  mat2 = pad
  for(i in (k+1):(nrow + k)){
    for(j in (k+1):(ncol + k)){
      mat2[i,j] = sd(pad[(i-k):(i+k),(j-k):(j+k)])
    }
  }
  
  mat2 = mat2[ (k+1):(nrow + k),(k+1):(ncol + k)  ]
  ## identify edge
  mat2[mat2 > quantile(mat2,1-top)] = 1    ##   identify edge by select top 1% or 10% 
  ##  values in matrix and replace them by 1 
  return(mat2)
}
#filter_sd(matrix(1,2,2))

read_image = function(file_name)
{
  ima = readPNG(file_name)
  #dim(ima)
  ima_red = ima[,,1]
  ima_green = ima[,,2]
  ima_blue = ima[,,3]
  
  dim_original = dim(ima)
  ima_li = list(ima[,,1],ima[,,2],ima[,,3])
  return(list(ima_li,dim_original))   ## return list and original array                                                    # dimension
}

## cores: the number of cores to be used
## ima_li list to be apply filter function
## top: percentage pixesto be replaced by 1
## dim_final: the dimension final array(the one can be used on the writePNG)
filter_assemble_edge = function(cores,ima_li,top,dim_final)
{
  require("parallel")
  n_cores = detectCores()
  cluster = makePSOCKcluster(names=cores)
  
  filtered_list = parLapply(cl = cluster,fun = filter_sd,X = ima_li,top = top)
  stopCluster(cl=cluster)
  
  ### assemble
  final_array = array(c(filtered_list[[1]],filtered_list[[2]],filtered_list[[3]]),
                      dim = dim_final)
  
  return(final_array)
}




#### Detect edges for Van_Gogh
ima_li_Van = read_image('Van_Gogh.png')[[1]]
dim_final = read_image('Van_Gogh.png')[[2]]
final_array_Van = filter_assemble_edge(3,ima_li_Van,0.1,dim_final)

### write
writePNG(final_array_Van,"edge_Van.png")





#### Detect edges for Madison
ima_li_Van = read_image('Madison.png')[[1]]
dim_final = read_image('Madison.png')[[2]]
final_array_Van = filter_assemble_edge(3,ima_li_Van,0.1,dim_final)

### write
writePNG(final_array_Van,"edge_Madison.png")








