source("letter_helper_funs.R")

k = 4
num_clust = 16

filelist_quer <- Sys.glob('../images/G/queries/*.jpg')
n_quer <- length(filelist_quer)
idist_quer <- get_idist(filelist_quer, 64, 96, k)
proto_quer = protoclust(idist_quer)
cut_quer = protocut(proto_quer, h = 312.5061)

par(mfrow=c(4,4))
plot_cut(cut_quer, filelist_quer)


filelist_div <- Sys.glob('../images/G/divorce/*.jpg')
n_div <- length(filelist_div)
idist_div <- get_idist(filelist_div, 64, 96, k)
proto_div = protoclust(idist_div)
cut_div = protocut(proto_div, k = 16)

par(mfrow=c(4,4))
plot_cut(cut_div, filelist_div)
plot_cluster(cut_div, filelist_div, 13)


filelist_full <- c(filelist_quer, filelist_div)
idist_full <- get_idist(filelist_full, 64, 96, k)

idist_quer <- as.dist(as.matrix(idist_full)[1:n_quer, 1:n_quer])
idist_div <- as.dist(as.matrix(idist_full)[(n_quer + 1):(n_quer + n_div), 
                                           (n_quer + 1):(n_quer + n_div)])

proto_quer <- protoclust(idist_quer)
proto_div <- protoclust(idist_div)
proto_full <- protoclust(idist_full)

cut_quer <- protocut(proto_quer, h = 527.7565)
cut_div <- protocut(proto_div, 15)
cut_full <- protocut(proto_full, 16)

plot_cut(cut_quer, filelist_quer)
plot_cut(cut_div, filelist_div)
plot_cut(cut_full, filelist_full)

plot_cluster(cut_div, filelist_div, 1)

proto_quer$height[n_quer - 16]
proto_div$height[n_div - 15]

proto_full <- protoclust(idist_full)
cut_full <- protocut(proto_full, 16)
plot_cut(cut_full, filelist_full)
plot_cluster(cut_full, filelist_full, 3)




## do some sub-sampling
k = 3
filelist_quer <- Sys.glob('../images/G/queries/*.jpg')
n_quer <- length(filelist_quer)
filelist_div <- Sys.glob('../images/G/divorce/*.jpg')
n_div <- length(filelist_div)

filelist_full <- c(filelist_quer, filelist_div)
idist_full <- get_idist(filelist_full, 64, 96, k)

idist_div <- as.dist(as.matrix(idist_full)[(n_quer + 1):(n_quer + n_div), 
                                           (n_quer + 1):(n_quer + n_div)])
proto_div <- protoclust(idist_div)
cut_div <- protocut(proto_div, k = 16)

filelist_div_samp <- sample_filelist_combine_clusts(filelist_div, cut_div,
                                                    c(1:16)[-c(9, 11, 13)],
                                                    34)
n_div_samp <- length(filelist_div_samp)

filelist_full_samp <- c(filelist_quer, filelist_div_samp)
idist_full_samp <- get_idist(filelist_full_samp, 64, 96, k)

idist_quer_samp <- as.dist(as.matrix(idist_full_samp)[1:n_quer, 1:n_quer])
idist_div_samp <- as.dist(as.matrix(idist_full_samp)[(n_quer + 1):(n_quer + n_div_samp), 
                                           (n_quer + 1):(n_quer + n_div_samp)])

proto_quer_samp <- protoclust(idist_quer_samp)
proto_div_samp <- protoclust(idist_div_samp)

cut_quer_samp <- protocut(proto_quer_samp, k = 16)
hc = proto_quer$height[n_quer - 16]
cut_div_samp <- protocut(proto_div_samp, h = hc)

plot_cut(cut_quer_samp, filelist_quer)
plot_cut(cut_div_samp, filelist_div_samp)

merge_mat <- matrix(0, nrow=length(cut_quer_samp$protos), ncol=length(cut_div_samp$protos))
for(row in 1:nrow(merge_mat)){
  for(col in 1:ncol(merge_mat)){
    full_row <- which(filelist_full_samp == filelist_quer[cut_quer_samp$protos[row]])
    full_col <- which(filelist_full_samp == filelist_div_samp[cut_div_samp$protos[col]])
    merge_mat[row, col] <- (as.matrix(idist_full_samp)[full_row, full_col] <= 3*hc)
  }
}





plot_cluster(cut_quer_samp, filelist_quer, 6)
plot_cluster(cut_div_samp, filelist_div_samp, 3)


proto_div_samp$height[n_div_samp - 16]
