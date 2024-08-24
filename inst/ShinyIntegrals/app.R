library(shiny)

integralBayes <- function(meanval, sdval) {
  meanval <- as.numeric(meanval)
  sdval <- as.numeric(sdval)
  xlow  = meanval - 3.5*sdval # Specify low end of x-axis.
  xhigh = meanval + 3.5*sdval # Specify high end of x-axis.
  dx = sdval/10               # Specify interval width on x-axis
  # Specify comb of points along the x axis:
  x = seq( from = xlow , to = xhigh , by = dx )
  # Compute y values, i.e., probability density at each value of x:
  y = ( 1/(sdval*sqrt(2*pi)) ) * exp( -.5 * ((x-meanval)/sdval)^2 )
  # Plot the function. "plot" draws the intervals. "lines" draws the bell curve.
  #openStuff::openGraph(width=7,height=5)
  plot( x , y , type="h" , lwd=1 , cex.axis=1.5
        , xlab="x" , ylab="p(x)" , cex.lab=1.5
        , main="Normal Probability Density" , cex.main=1.5 )
  lines( x , y , lwd=3 ,  col="darkgreen" )
  # Approximate the integral as the sum of width * height for each interval.
  area = sum( dx * y )
  # Display info in the graph.
  text( meanval-sdval , .9*max(y) , bquote( paste(mu ," = " ,.(meanval)) )
        , adj=c(1,.5) , cex=1.5 )
  text( meanval-sdval , .75*max(y) , bquote( paste(sigma ," = " ,.(sdval)) )
        , adj=c(1,.5) , cex=1.5 )
  text( meanval+sdval , .9*max(y) , bquote( paste(Delta , "x = " ,.(dx)) )
        , adj=c(0,.5) , cex=1.5 )
  text( meanval+sdval , .75*max(y) ,
        bquote( sum({},x,{}) *" "* Delta *"x p(x) = "* .(signif(area,3)) ) ,
        adj=c(0,.5) , cex=1.5 )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),

    # Application title
    titlePanel("Shiny Integrals"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("mean",
                        "Mean Value:",
                        min = -50,
                        max = 50,
                        value = 0),

            sliderInput("sd",
                        "SD Value:",
                        min = 0.01,
                        max = 50,
                        step = 0.1,
                        value = 0.2)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  meann <- reactive(input$mean)
  sdd <- reactive(input$sd)

  output$distPlot <- renderPlot({

    integralBayes(meann(), sdd())
    })
}

# Run the application
shinyApp(ui = ui, server = server)
