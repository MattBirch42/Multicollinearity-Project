library(shiny)
library(plot3D)
library(tidyverse)
library(ggplot2)


# Let's start by generating fake data. 
# There are 2 variable/feature values and one error term per observation.
# There are nObs observations in each data set
# the app will have nSets numbers of datasets with different 
#          degrees of multicollinearity
nObs <- 100
nSets <- 10

# random draws for features (xs) and targets (y)
# column 3 (targets): y = x1+2x2+e
# I am lazy, so I ommitted the intercept term. Doesn't affect the pont of this code.
set.seed(42)
features_raw <- matrix(runif(2*nObs, -10,10), nrow = nObs)
set.seed(11235)
target_raw <- features_raw[,1]+2*features_raw[,2]+matrix(runif(nObs,-2,2))

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
# squared sum of errors (SSE) plotted on a range of beta1 and beta2 values.
#SSE is the sum of the squared values of the errors/noise terms, (y-yhat)

num_betas <- 20
beta1 <- seq(-50,50,length.out = num_betas)
beta2 <- seq(-50,50,length.out = num_betas)
sse_array <- array(0, dim = c(num_betas,num_betas,nSets))
for (k in 1:nSets) {
  for (i in 1:num_betas) {
    for (j in 1:num_betas) {
      betas <- rbind(beta1[i],beta2[j])
      #calculating SSE for every beta pair in each matrix
      #Disclaimer: technically, this calculates the negative of the SSE.
      #For visualization, it is easier for me to display maximizing a negative than
      #      to depict minimizing. So the high points on these graphs are where 
      #      SSE is minimized. It is mathematically equivalent.  
      sse_array[i,j,k] <- -sum((target_raw[]-data_sets[,1:2,k]%*%betas)^2)
    }
  }
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Multicollinearity Visualization App"),
  
  sidebarLayout(
    sidebarPanel(
      #User chooses which matrix to work with using this slider.
      sliderInput("mcSet",
                  "Multicollinearity Factor: Higher numbers for more multicollinearity:",
                  min = 1,
                  max = nSets,
                  value = 1),
      #user chooses display angles
      sliderInput("userPhi",
                   "Vertical Display Angle:",
                   min = 0,
                   max =  90,
                   step = 5,
                   value = 20),
      sliderInput("userTheta",
                  "Horizontal Display Angle:",
                  min = 0,
                  max =  180,
                  step = 5,
                  value = 20)
    ),
    
    # Show graph and brief explanation. 
    mainPanel(
      plotOutput("ssePlot"),
      textOutput("explanation")
    )
  )
)

#app logic

server <- function(input, output) {
  
  output$ssePlot <- renderPlot({
    # output is this fancy 3d graph. Inputs flexible for data matrix and viewing angles.
    persp3D(beta1,beta2,sse_array[,,input$mcSet],
            phi = input$userPhi,
            theta = input$userTheta,
            label = TRUE,
            xlab = "Beta 1",
            ylab = "Beta 2",
            zlab = "SSE",
            scale = TRUE,
            axes = TRUE,
            ticktype = "simple",
            resfac = 3,
            contour = TRUE,
            main = "Sum of Squared Errors (-) for Multiple Regression with Two Inputs",
            las = 0,
            cex.lab = 1.5,
            cex.main = 2,
            d=3)
  })
  output$explanation <- renderText("This graph shows the negative sum of squared errors (SSE) for many different combinations of beta1 and beta2 in a bivariate regression framework. This is easily generalized to higher dimensional regressions. When you run a regression, it chooses the betas to minimize the SSE. In reality, this is a minimization problem and the graph should look like a hole, but I graphed the negative of the SSE for visualization purposes. In this case, the highest (reddest) point/s on the graph show where SSE is minimized, and the associated beta1 and beta2 would be the optimized betas from the regression. Note that when whe have datasets with more multicollinearity (slide that 'Multicollinearity Factor' to the right), the set of red points grows to where there is no unique solution. That is why we have problems with multicollinearity. It makes it so regular regression cannot identify unique beta values, because multiple solutions exist. I also made it so you can fiddle around with the viewing angles. The code for this App can be found here: https://github.com/MattBirch42/Multicollinearity-Project.git")
}

# Run the application 
shinyApp(ui = ui, server = server)
