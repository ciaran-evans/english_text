library(shiny)


navbarPage("Visualizing Puffs",
           tabPanel("Prototype Clustering",
                    sidebarLayout(
                      sidebarPanel(
                       sliderInput("num_clust_proto", "Number of clusters",
                                   min = 1, max = 10, value = 5, step = 1),
                       uiOutput("clust_choice_proto"),
                       uiOutput("image_choice_proto"),
                       radioButtons("plot_jpg", "Letter display preference",
                                    choices = c("Curves", "Original JPEG", "Contour JPEG"))
                        
                        ),
                      mainPanel(
                        plotOutput("image_plot"),
                        plotOutput("protos_plot")
                      )
                    )
           )
)