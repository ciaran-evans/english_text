library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(fdasrvf)
library(protoclust)
library(Rtsne)
library(jpeg)
library(reshape2)

theme_set(theme_minimal())

load("data/letter_distmat.RData")

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

fileNames <- Sys.glob("images/contour_G/*.jpg")
origFiles <- Sys.glob("images/G/*.jpg")

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
      ggplot(aes(x = x, y = y)) +
      geom_path() +
      facet_wrap(~clust) +
      annotate("text", 
               label = paste("Number of letters:",
                             clust_counts()), x = 40, y = 65)
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
    
    if(input$plot_jpg){
      image <- readJPEG(origFiles[as.numeric(input$imageChoiceProt)])
      plot(as.raster(image))
    } else {
      temp <- curvelist[[as.numeric(input$imageChoiceProt)]] %>%
        t() %>%
        data.frame()
      names(temp) <- c("x", "y")
      temp %>%
        ggplot(aes(x = x, y = y)) +
        geom_path()
    }
    
  })
  
  
})
