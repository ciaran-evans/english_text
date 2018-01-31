source("letter_helper_funs.R") # might have to change readPNG to readJPEG

k = 3
num_clust = 16

filelist_quer <- Sys.glob('../images/trimmed_queries_divorce/queries/y/*.png')
n_quer <- length(filelist_quer)

filelist_div <- Sys.glob('../images/trimmed_queries_divorce/divorce/y/*.png')
n_div <- length(filelist_div)

# this assumes that the images have been centered via padding,
# and are all same size (64 x 96)
# e.g., with mogrify -gravity Center -extent 96x64 *.png
filelist_full <- c(filelist_quer, filelist_div)
idist_full <- get_idist(filelist_full, 64, 96, k)
#saveRDS(idist_full, file='idist_full_G.RDS')
saveRDS(idist_full, file='idist_full_y.RDS')
proto_full <- protoclust(idist_full)
cut_full <- protocut(proto_full, 16)

plot_cut(cut_full, filelist_full)
plot_cluster(cut_full, filelist_full, 13)


# do the analysis again on one cluster
#subfiles <- filelist_full[cut_full$cl %in% c(2:12)]
subfiles <- filelist_full[cut_full$cl %in% c(4, 5, 6, 7, 8, 11, 12)]
idist_sub <- get_idist(subfiles, 64, 96, k)
#saveRDS(idist_full, file='idist_sub_G.RDS')
saveRDS(idist_full, file='idist_sub_y.RDS')

proto_sub <- protoclust(idist_sub)
cut_sub <- protocut(proto_sub, num_clust)

plot_cut(cut_sub, subfiles)
plot_cluster(cut_sub, subfiles, 7)
