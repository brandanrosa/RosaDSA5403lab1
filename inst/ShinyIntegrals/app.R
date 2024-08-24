library(shiny)

integralBayes <- function(meanval, sdval) {
  meanval <- as.numeric(meanval)
  sdval <- as.numeric(sdval)
  xlow  = meanval - 3.5*sdval
  xhigh = meanval + 3.5*sdval
  dx = sdval/10
  x = seq( from = xlow , to = xhigh , by = dx )
  y = ( 1/(sdval*sqrt(2*pi)) ) * exp( -.5 * ((x-meanval)/sdval)^2 )
  #openStuff::openGraph(width=7,height=5)
  plot( x , y , type="h" , lwd=1 , cex.axis=1.5
        , xlab="x" , ylab="p(x)" , cex.lab=1.5
        , main="Normal Probability Density" , cex.main=1.5 )  lines( x , y , lwd=3 ,  col="darkgreen" )
  area = sum( dx * y )
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


ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),

    titlePanel("Shiny Integrals"),

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

        mainPanel(
           plotOutput("distPlot")
        )
    )
)

server <- function(input, output) {

  meann <- reactive(input$mean)
  sdd <- reactive(input$sd)

  output$distPlot <- renderPlot({

    integralBayes(meann(), sdd())
    })
}

shinyApp(ui = ui, server = server)
