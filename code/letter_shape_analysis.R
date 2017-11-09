library(jpeg)
library(reshape2)
library(dplyr)
library(fdasrvf)


# order_points_for_curve <- function(beta){
#   current <- beta[,1]
#   beta <- beta[,-1]
#   newmat <- current
#   while(ncol(beta) > 2){
#     ind <- which.min(colSums((beta - current)^2))
#     current <- beta[,ind]
#     beta <- beta[,-ind]
#     newmat <- cbind(newmat, current)
#   }
#   newmat <- cbind(newmat, beta[,1])
#   return(newmat)
# }


fileNames <- Sys.glob("../data/character*")

curvelist <- list()
i <- 1
num_comps <- c()
for(name in fileNames){
  df <- read_csv(name)
  names(df) <- c("junk", "x", "y", "contour")
  df <- df %>%
    select(-junk) %>%
    mutate(file = basename(name))
  curvelist[[i]] <- df
  num_comps[i] <- max(df$contour) +1
  i <- i + 1
}

curvelist <- curvelist[num_comps==1]

curve_names <- c()
for(i in 1:length(curvelist)){
  curve_names[i] <- curvelist[[i]]$file[1]
  curvelist[[i]] <- resamplecurve(t(curvelist[[i]][,1:2]), 400)
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
