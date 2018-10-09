library(shiny)

library(rAviExp)
ui <- fluidPage(
  
  titlePanel("Angle of Attack Analysis"),
  
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("rg", "Range of alpha:",
                  min = -15, max = 15,
                  value = c(-5,10)),
      textInput("Cla",
                "Cla(Lift coefficient gradient)\n
                [Deg^-1]",
                value = '.1'),
      textInput('alpha0',
                'alpha0(Zero-lift Angle of attack)',
                value = '-5'),
      textInput('CdiF',
                'Cdif(Lift-induced Drag Factor)',
                value = '.0398'),
      textInput('Cd0',
                'Cd0 (Zero-lift Drag coefficient)',
                value = '.02')
    ),
    
    mainPanel(
      plotOutput("Plot"),
      tableOutput('Optim'),
      titlePanel('This UI is based on rAviExp package\n
                 for more informations,please visit\n
                 https://aviexptemp.weebly.com/ \n
                 https://github.com/HaoLi111/rAviExp')
      )
      )
      )

server <- function(input, output) {
  output$Plot <- renderPlot({
    alpha<-list(Cla=eval(parse(text=input$Cla)),
                alpha0=eval(parse(text=input$alpha0)),
                CdiF=eval(parse(text=input$CdiF)),
                Cd0=eval(parse(text=input$Cd0)))
    class(alpha)='Alpha_lin'
    alpha=create(alpha,alpha=seq(from=min(input$rg),to=max(input$rg),by=.1))
    lines(alpha)
  })
  output$Optim<-renderTable({
    alpha<-list(Cla=eval(parse(text=input$Cla)),
                alpha0=eval(parse(text=input$alpha0)),
                CdiF=eval(parse(text=input$CdiF)),
                Cd0=eval(parse(text=input$Cd0)))
    class(alpha)='Alpha_lin'
    o=Optim(create(alpha,alpha=seq(from=min(input$rg),to=max(input$rg),by=.1)))
    rbind(o[[1]],o[[2]],o[[3]])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
