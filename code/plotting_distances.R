
tfit <- Rtsne(letter_distmat, perplexity = 10, is_distance=T)
plot(tfit$Y[,1], tfit$Y[,2])

xscale = (max(tfit$Y[,1])/8)/2
yscale = (max(tfit$Y[,2])/8)/2

#Draw on the images
for(i in 1:length(curve_names)){
  a = readJPEG(paste('../images/G/',curve_names[i], '.jpg', sep=''), native=TRUE)
  x = tfit$Y[i,1]; y=tfit$Y[i,2]
  rasterImage(a, x-xscale, y-yscale, x+xscale, y+yscale)
}


library(jpeg)

filelist = list.files(path='../images/G/test2',pattern='character.*jpg')
H = 64
W = 96
dat = matrix(0,nrow=length(filelist),ncol=H*W)
for(i in 1:length(filelist)){
  a = readJPEG(paste('../images/G/test2/',filelist[i],sep=''), native=FALSE)
  dat[i,] = as.numeric(a)
}
dat <- round(dat)
for(col in which(colSums(dat) > 46)){
  dat[,col] = rep(1, nrow(dat))
}
# dat <- 1 - dat
# empty = which(colSums(dat)==0)
# dat = dat[,-empty]
# dim(dat)


# library(Rtsne)
# tfit = Rtsne(dat,dims = 2, perplexity=5)
# 
# scale = 15
# plot(tfit$Y[,1]*scale,tfit$Y[,2]*scale,pch=19)


library(TangentDistance)
tdist = tangentDistMatrix(dat,xtest=NULL, H,W,c(1,1,1,1,1,1,1,1,0))
dmat = matrix(0,nrow=nrow(dat),ncol=nrow(dat))
for(j in 2:nrow(dat)){
  for(i in 1:(j-1)){
    n = nrow(dat)
    dmat[i,j] = tdist[n*(i-1) - i*(i-1)/2 + j - i]
    dmat[j,i] = dmat[i,j]
  }
}
dmatD = as.dist(dmat)

b = cmdscale(dmatD, k=2)

library(Rtsne)
tfit = Rtsne(dmatD,dims = 2, perplexity=5)


tfit = list(Y=b)

scale = 2
plot(tfit$Y[,1]*scale,tfit$Y[,2]*scale,pch=19)

#Draw on the images
for(i in 1:length(filelist)){
  a = readJPEG(paste('../images/G/test2/',filelist[i],sep=''), native=TRUE)
  x = tfit$Y[i,1]*scale; y=tfit$Y[i,2]*scale
  rasterImage(a, x-W/2, y-H/2, x+W/2, y+H/2)
}

#plot known funny things.  They seem to be at the boundary.  I wonder if we'll do better
weirdidx = which(filelist %in% c('character1_queries.jpg','character1_queries_mod.jpg','character13_divorce.jpg', 'character430.jpg'))
weirdidx <- c(52:58)
points(tfit$Y[weirdidx,1]*scale,tfit$Y[weirdidx,2]*scale,col='red',pch=16,cex=1)


probs <- colSums(dat)/nrow(dat)
rasterImage(matrix(probs, nrow = 64), 0, 0, 200, 200)

extlog <- function(x){
  if(x > 0){
    return(log(x))
  } else {
    return(0)
  }
}

logprobs <- sapply(probs, extlog)

probdist <- matrix(0, nrow = nrow(dat), ncol = nrow(dat))
for(i in 1:nrow(dat)){
  for(j in 1:nrow(dat)){
    probdist[i,j] <- -1*sum(abs(dat[i,] - dat[j,]) * logprobs)
  }
}
probdist <- as.dist(probdist)


tfit = Rtsne(probdist,dims = 2, perplexity=5)

mdsfit = cmdscale(probdist,k=2)
tfit= list(); tfit$Y=mdsfit

library(vegan)
idist = isomapdist(probdist,k=4)
mdsfit = cmdscale(idist,k=2)
tfit= list(); tfit$Y=mdsfit


probdist2 <- as.matrix(probdist)[-41,]
probdist2 <- as.dist(probdist2[,-41])

library(protoclust)
proto = protoclust(probdist2)
proto = protoclust(idist)
cut = protocut(proto, 16)

par(mfrow=c(4,4))
flist2 <- filelist[-41]
count = 1
for(id in cut$protos){
  img <- readJPEG(paste('../images/G/', flist2[id], sep=''))
  plot(as.raster(img))
  title(table(cut$cl)[count])
  count = count + 1
}

for(id in which(cut$cl == 5)){
  img <- readJPEG(paste('../images/G/', flist2[id], sep=''))
  plot(as.raster(img))
}

par(mfrow=c(3,2))
for(id in cut$protos){
  img <- readJPEG(paste('../images/G/', filelist[id], sep=''))
  plot(as.raster(img))
}
