
defC<-function(x){
  WM = list(x = x$WMx,
            y = x$WMy,
            z = x$WMz,
            ChordR =x$WMChordR,
            ChordT =x$WMChordT,
            Span =x$WMSpan,
            Sweep = x$WMSweep,
            Gamma =x$WMGamma,
            Type=0)
  class(WM) ='wing'
  WH = list(x =x$WHx,
            y = x$WHy,
            z= x$WHz,
            ChordR =x$WHChordR,
            ChordT =x$WHChordT,
            Span =x$WHSpan,
            Sweep = x$WHSweep,
            Gamma =x$WHGamma,
            Type=0)
  class(WH) = 'wing'
  WV =list(x =x$WVx,
           y = x$WVy,
           z= x$WVz,
           ChordR =x$WVChordR,
           ChordT =x$WVChordT,
           Span =x$WVSpan,
           Sweep = x$WVSweep,
           Gamma =x$WVGamma,
           Type=1)
  class(WV) = wing
  Fuse = list(x=0,y=0,z=0)#needed update
  
  re = list(Fuse=Fuse,WM=WM,WH=WH,WV=WV)
  class(re)= 'conventionalConcept'
  re
}

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("rAviExp_conventionalConcept"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput('ConceptName',
                   label = 'Concept Name',
                   value = 'New Concept'),
         textInput('WMx',
                   label = 'Main wing X location:',
                   value = .2),
         textInput('WMy',
                   label = 'Main wing y location:',
                   value = 0),
         textInput('WMz',
                   label = 'Main wing z location:',
                   value = 0),
         textInput('WMChordR',
                   label = 'Main wing Root Chord length [m]',
                   value = .15),
         textInput('WMChordT',
                   label = 'Main wing Tip Chord length [m]',
                   value = .1),
         textInput('WMSpan',
                   label = 'Main wing Span [m]',
                   value = .6),
         textInput('WMSweep',
                   label = 'Main wing sweep angle from front [DEG]',
                   value = 10),
         textInput('WMGamma',
                   label = 'Main wing Dihedral[DEG]',
                   value = 1),
         #
         textInput('WHx',
                   label = 'Horizontal tail X location:',
                   value = .2),
         textInput('WHy',
                   label = 'Horizontal tail y location:',
                   value = 0),
         textInput('WHz',
                   label = 'Horizontal tail z location:',
                   value = 0),
         textInput('WHChordR',
                   label = 'Horizontal tail Root Chord length [m]',
                   value = .15),
         textInput('WHChordT',
                   label = 'Horizontal tail Tip Chord length [m]',
                   value = .1),
         textInput('WHSpan',
                   label = 'Horizontal tail Span [m]',
                   value = .6),
         textInput('WHSweep',
                   label = 'Horizontal tail sweep angle from front [DEG]',
                   value = 10),
         textInput('WHGamma',
                   label = 'Horizontal tail Dihedral[DEG]',
                   value = 1),
         #
         textInput('WVx',
                   label = 'Vertical tail X location:',
                   value = .2),
         textInput('WVy',
                   label = 'Vertical tail y location:',
                   value = 0),
         textInput('WVz',
                   label = 'Vertical tail z location:',
                   value = 0),
         textInput('WVChordR',
                   label = 'Vertical tail Root Chord length [m]',
                   value = .15),
         textInput('WVChordT',
                   label = 'Vertical tail Tip Chord length [m]',
                   value = .1),
         textInput('WVSpan',
                   label = 'Vertical tail Span [m]',
                   value = .6),
         textInput('WVSweep',
                   label = 'Vertical tail sweep angle from front [DEG]',
                   value = 10),
         textInput('WVGamma',
                   label = 'Vertical tail Dihedral[DEG]',
                   value = 1),
         #Default Cilindrical fuselage
         textInput('FuseL',
                   label = 'Fuselage Length',
                   value =1.3
                   ),
         textInput('x1',
                   label = 'Fuselage section x 1:',
                   value = 0),
         textInput('r1',
                   label = 'Fuselage section radius r 1',
                   value = 0),
         textInput('x2',
                   label = 'Fuselage section x 2:',
                   value = 0),
         textInput('r2',
                   label = 'Fuselage section radius r 2',
                   value = 0),
         textInput('x3',
                   label = 'Fuselage section x 3:',
                   value = 0),
         textInput('r3',
                   label = 'Fuselage section radius r 3',
                   value = 0),
         textInput('x4',
                   label = 'Fuselage section x 4:',
                   value = 0),
         textInput('r4',
                   label = 'Fuselage section radius r 4',
                   value = 0),
         textInput('x5',
                   label = 'Fuselage section x 5:',
                   value = 0),
         textInput('r5',
                   label = 'Fuselage section radius r 5',
                   value = 0),
         #End of Fuse
         sliderInput('Xcg :',
                     label ='Trail CG position [MM]',
                     value=230,
                     min =1,
                     max = 3000),
         #Save file section
         selectInput("dataset", "Contents",
                     choices = c("Basic Geom", "Rating","All")),
         
         width = 4
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput('')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Basic Geom" = kitchk(c(input$Xa/1000,input$Ya/1000),
                                 c(input$Xb/1000,input$Yb/1000),
                                 c(input$Xc/1000,input$Yc/1000))[[1]],
           "Rating" = kitchk(c(input$Xa/1000,input$Ya/1000),
                             c(input$Xb/1000,input$Yb/1000),
                             c(input$Xc/1000,input$Yc/1000))[[2]],
           "All" = kitchk(c(input$Xa/1000,input$Ya/1000),
                          c(input$Xb/1000,input$Yb/1000),
                          c(input$Xc/1000,input$Yc/1000)))
  })
  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$concept_name,' ',input$dataset, ".rds")
    },
    content = function(file) {
      saveRDS(datasetInput(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

