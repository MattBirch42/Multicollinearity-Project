#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#raw code copied from r file. Mostly putting this here to try committing document from R to Github. 
library(plot3D)
library(tidyverse)
library(ggplot2)


# Let's start by generating fake data. 
# There are 2 variable/feature values and one error term per observation.
# There are nObs observations in each data set
# the app will have nSets numbers of datasets with different 
#          degrees of multicollinearity
nObs <- 10
nSets <- 4

# random draws for features (xs) and targets (y)
# column 3 (targets): y = x1+2x2+e
# I am lazy, so I ommitted the intercept term. Doesn't affect the pont of this code.
set.seed(42)
features_raw <- matrix(runif(2*nObs, -10,10), nrow = nObs)
set.seed(11235)
target_raw <- x_raw[,1]+2*x_raw[,2]+matrix(runif(nObs,-2,2))

# Now we make the nSets of data based on these draws.
# The first matrix has no multicollinearity and is the one we already drew
# Matrices will have increasing multicollinearity along 3rd dimension
# Same target vector in every matrix.
# same information in x1 and x2, just increasingly shared.
data_sets <- array(0, dim = c(nObs,3,nSets))
data_sets[,1:2,1] <- features_raw
data_sets[,3,] <- target_raw


# I chose to model multicollinearity by making x1 and x2 be linear combinations of draw1 and draw2. 
# you could easily make them correlated in some other way.
for ( i in 2:nSets) {
  data_sets[,1,i] <- data_sets[,1,1]+data_sets[,2,1]*nSets/(i-1)
  data_sets[,2,i] <- data_sets[,2,1]+data_sets[,1,1]*nSets/(i-1)*.75
}

head(data_sets)
# In summary, the same feature information is increasingly overlapped, but the target never changes. 

#Now lets move on. Each of the nSets data sets will have a 3D plot,
# squared sum of errors plotted on a range of beta1 and beta2 values.
num_betas <- 20
beta1 <- seq(-20,20,length.out = num_betas)
beta2 <- seq(-20,20,length.out = num_betas)
sse_array <- array(0, dim = c(num_betas,num_betas,nSets))
for (k in 1:nSets) {
  for (i in 1:num_betas) {
    for (j in 1:num_betas) {
      betas <- rbind(beta1[i],beta2[j])
      #calculating SSE for every beta pair in each matrix
      sse_array[i,j,k] <- sum((target_raw[]-data_sets[,1:2,k]%*%betas)^2)
    }
  }
}


persp3D(beta1,beta2,sse_array[,,1],
        phi = 10,
        theta = 35,
        label = TRUE,
        xlab = "Beta 1",
        ylab = "Beta 2",
        zlab = "SSE",
        scale = TRUE,
        axes = TRUE,
        ticktype = "detailed")








# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
