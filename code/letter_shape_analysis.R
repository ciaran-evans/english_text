library(jpeg)
library(reshape2)
library(dplyr)
library(fdasrvf)


order_points_for_curve <- function(beta){
  current <- beta[,1]
  beta <- beta[,-1]
  newmat <- current
  while(ncol(beta) > 2){
    ind <- which.min(colSums((beta - current)^2))
    current <- beta[,ind]
    beta <- beta[,-ind]
    newmat <- cbind(newmat, current)
  }
  newmat <- cbind(newmat, beta[,1])
  return(newmat)
}


fileNames <- Sys.glob("../images/contour_G/*.jpg")

curvelist <- list()
i <- 1
for(name in fileNames){
  im <- readJPEG(name)
  immat <- im[,,1] + im[,,2] + im[,,3]
  
  image <- melt(immat)
  names(image) <- c("x", "y", "val")
  curvelist[[i]] <- image %>%
    mutate(val = ifelse(val < 1.5, 1, 0)) %>%
    filter(val == 1, x > min(x) + 5, x < max(x) - 5, y > min(y) + 5, y < max(y) - 5) %>%
    select(x,y) %>%
    t() %>%
    order_points_for_curve() %>%
    resamplecurve(N=200)
  
  i <- i + 1
}


letter_distmat <- matrix(nrow=length(curvelist), ncol=length(curvelist))

for(j in 1:length(curvelist)){
  for(k in 1:length(curvelist)){
    letter_distmat[j,k] <- calc_shape_dist(curvelist[[j]], curvelist[[k]])
    print(paste(j,k))
  }
}

letter_distmat <- data.frame(letter_distmat)
save(letter_distmat, file="../data/letter_distmat.RData")

# protoclust
library(protoclust)
distmat <- as.matrix(letter_distmat)
distmat <- 0.5*(distmat + t(distmat))
distmat <- distmat - diag(diag(distmat))

prot <- protoclust(distmat)
cut <- protocut(prot, k = 5)
protos <- cut$protos


plot(curvelist[[protos[5]]][1,], curvelist[[protos[5]]][2,], type="l")


library(rsconnect)
rsconnect::deployApp('letter_explore')
