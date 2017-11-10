library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(fdasrvf)
library(protoclust)
library(Rtsne)
library(jpeg)
library(readr)
library(reshape2)

theme_set(theme_minimal())

load("../../data/letter_distmat.RData")

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

#fileNames <- Sys.glob("images/contour_G/*.jpg")
#origFiles <- Sys.glob("images/G/character*.jpg")

fileNames <- Sys.glob("../../data/character*")
print(fileNames)

#print(fileNames)
#print(origFiles)

curvelist <- list()
i <- 1
num_comps <- c()
curve_names <- c()
for(name in fileNames){
  df <- read_csv(name)
  names(df) <- c("junk", "x", "y", "contour")
  df <- df %>%
    select(-junk) %>%
    mutate(file = basename(name))
  curvelist[[i]] <- df
  num_comps[i] <- max(df$contour) +1
  curve_names[i] <- basename(name)
  i <- i + 1
}

curvelist <- curvelist[num_comps==1]
curve_names <- curve_names[num_comps==1]

#curvelist <- curvelist[-c(16, 36, 38)]

#letter_distmat <- letter_distmat[-c(16, 36, 38), -c(16, 36, 38)]

#fileNames <- fileNames[num_comps==1]
#origFiles <- origFiles[num_comps==1]

origCurves <- curvelist

for(i in 1:length(curvelist)){
  curvelist[[i]] <- resamplecurve(t(curvelist[[i]][,1:2]), 400)
}

shinyServer(function(input, output, session) {
  
  proto_curves <- reactive({
    distmat <- as.matrix(letter_distmat)
    distmat <- 0.5*(distmat + t(distmat))
    distmat <- distmat - diag(diag(distmat))
    
    prot <- protoclust(distmat)
    cut <- protocut(prot, k = input$num_clust_proto)
    protos <- cut$protos
    
    curvedat <- data.frame()
    for(i in 1:input$num_clust_proto){
      temp <- curvelist[[protos[i]]]
      npoints <- ncol(temp)
      temp <- data.frame(t(temp), clust = rep(i, npoints))
      curvedat <- rbind(curvedat, temp)
    }
    names(curvedat) <- c("x", "y", "clust")
    return(curvedat)
  })
  
  clust_counts <- reactive({
    distmat <- as.matrix(letter_distmat)
    distmat <- 0.5*(distmat + t(distmat))
    distmat <- distmat - diag(diag(distmat))
    
    prot <- protoclust(distmat)
    cut <- protocut(prot, k = input$num_clust_proto)
    return(data.frame(table(cut$cl))$Freq)
  })
  
  output$protos_plot <- renderPlot({
    proto_curves() %>%
      ggplot(aes(x = x, y = -1*y)) +
      geom_path() +
      facet_wrap(~clust) +
      annotate("text", 
               label = paste("Number of letters:",
                             clust_counts()), x = 40, y = 0)
  })
  
  
  output$clust_choice_proto <- renderUI({
    sliderInput("clustChoiceProt", "Choose cluster",
                min=1, max=input$num_clust_proto, value = 1, step = 1)
  })
  
  output$image_choice_proto <- renderUI({
    distmat <- as.matrix(letter_distmat)
    distmat <- 0.5*(distmat + t(distmat))
    distmat <- distmat - diag(diag(distmat))
    
    prot <- protoclust(distmat)
    cut <- protocut(prot, k = input$num_clust_proto)
    ids <- data.frame(which(cut$cl == input$clustChoiceProt))[,1]
    
    selectInput("imageChoiceProt", "Choose image",
                choices = ids, selected = ids[1])
  })
  
  output$image_plot <- renderPlot({
    
    if(input$plot_jpg == "Original JPEG"){
      image = readJPEG(paste('../../images/G/',curve_names[as.numeric(input$imageChoiceProt)],
                             '.jpg', sep=''))
      plot(as.raster(image))
    } else {
      temp <- curvelist[[as.numeric(input$imageChoiceProt)]] %>%
        t() %>%
        data.frame()
      names(temp) <- c("x", "y")
      temp %>%
        ggplot(aes(x = x, y = -1*y)) +
        geom_path() +
        scale_x_continuous(limits=c(0, 65)) +
        scale_y_continuous(limits=c(-65, 0))
    }
    
  }, width=400, height=400)
  
  
  output$clust_choice_moreclust <- renderUI({
    sliderInput("clustChoiceMoreClust", "Choose cluster",
                min=1, max=input$num_clust_proto, value = 1, step = 1)
  })
  
  output$clusterGrid <- renderPlot({
    
    distmat <- as.matrix(letter_distmat)
    distmat <- 0.5*(distmat + t(distmat))
    distmat <- distmat - diag(diag(distmat))
    
    prot <- protoclust(distmat)
    cut <- protocut(prot, k = input$num_clust_proto)
    ids <- which(cut$cl == input$clustChoiceMoreClust)
    
    curve_data = data.frame()
    for(i in 1:length(curvelist)){
      curve_data <- curve_data %>%
        rbind(curvelist[[i]] %>% t() %>% data.frame() %>% mutate(id = i))
    }
    
      temp <- curve_data %>%
        filter(id %in% ids)
      names(temp) <- c("x", "y", "id")
      temp %>%
        ggplot(aes(x = x, y = -1*y)) +
        geom_path() +
        scale_x_continuous(limits=c(0, 65)) +
        scale_y_continuous(limits=c(-65, 0)) +
        facet_wrap(~id)
    
  })
  
  
})
