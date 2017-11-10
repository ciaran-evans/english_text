library(jpeg)
library(vegan)
library(protoclust)
library(png)


extlog <- function(x){
  if(x > 0){
    return(log(x))
  } else {
    return(0)
  }
}

get_probs <- function(dat){
  return(colSums(dat)/nrow(dat))
}

get_probdist <- function(filelist, H, W){
  dat = matrix(0,nrow=length(filelist),ncol=H*W)
  for(i in 1:length(filelist)){
    a = readPNG(filelist[i], native=FALSE)
    dat[i,] = as.numeric(a)
  }
  dat <- round(dat)
  
  logprobs <- sapply(get_probs(dat), extlog)
  
  probdist <- matrix(0, nrow = nrow(dat), ncol = nrow(dat))
  for(i in 1:nrow(dat)){
    for(j in 1:nrow(dat)){
      probdist[i,j] <- -1*sum(abs(dat[i,] - dat[j,]) * logprobs)
    }
  }
  return(as.dist(probdist))
  
}

get_idist <- function(filelist, H, W, k){
  probdist <- get_probdist(filelist, H, W)
  return(isomapdist(probdist, k=k))
}

plot_cut <- function(cut, filelist){
  count = 1
  for(id in cut$protos){
    img <- readPNG(filelist[id])
    plot(as.raster(img))
    title(table(cut$cl)[count])
    count = count + 1
  }
}

plot_cluster <- function(cut, filelist, clust){
  subfiles <- filelist[cut$cl == clust]
  for(file in subfiles){
    img <- readPNG(file)
    plot(as.raster(img))
  }
}

sample_filelist <- function(filelist, cut, samplesizes){
  new_filelist <- c()
  count = 1
  for(clust in unique(cut$cl)){
    new_filelist <- c(new_filelist, sample(filelist[cut$cl == clust], 
                                           size=samplesizes[count]))
  }
}

sample_filelist_combine_clusts <- function(filelist, cut, to_combine, size){
  new_filelist <- sample(filelist[cut$cl %in% to_combine], size)
  new_filelist <- c(new_filelist, filelist[!(cut$cl %in% to_combine)])
}