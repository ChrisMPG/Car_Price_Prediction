#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(h2o)

h2o.init(ip = "localhost",
         port = 54321,
         startH2O = T)

df3 <- read.csv(file = "df3.csv", header = TRUE, sep = ",")

train.hex <- as.h2o(df3, destination_frame = "train")

independent <- c(
  "Brand",
  "Model",
  "fueltype",
  "doornumber",
  "carbody",
  "drivewheel",
  "wheelbase",
  "carlength",
  "carwidth",
  "curbweight",
  "cylindernumber",
  "horsepower",
  "enginesize",
  "boreratio"
)
dependent <- "price"
model <- h2o.randomForest(
  x = independent,
  y = dependent,
  score_each_iteration = FALSE,
  training_frame = train.hex,
  model_id = "model_1"
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel(
    title = div(img(src = "logo.png", height = 70), "GeelyAuto"),
    windowTitle = "GeelyAuto"
  ),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("Change the values in order to make a prediction."),
      
          selectInput(
            "B",
            "Brand:",
            choices = list(
              "alfa",
              "audi",
              "bmw",
              "chevrolet",
              "dodge",
              "honda",
              "isuzu",
              "mazda",
              "buick",
              "mercury",
              "mitsubishi",
              "nissan",
              "peugeot",
              "plymouth",
              "porsche",
              "renault",
              "saab",
              "subaru",
              "toyota",
              "volkswagen",
              "volvo"
            ),
            selected = "toyota"
          ),
          
          textInput("Mdl", "Model:"),
          
          selectInput(
            "Cb",
            "Segment:",
            choices = list("Hardtop",
                           "Hatchback",
                           "Sedan",
                           "Wagon",
                           "convertible"),
            selected = "Hatchback"
          ),
          
          sliderInput(
            "Dn",
            "Number of doors:",
            min = 2,
            max = 4,
            value = 4
          ),
          
          sliderInput(
            "Cn",
            "Cylinder number inside the engine:",
            min = 2,
            max = 12,
            value = 4
          ),
          
          radioButtons("Ft", "Fuel:",
                       c("Gas", "Diesel"), "Gas"),
          
          selectInput(
            "Drw",
            "Traction:",
            choices = list("FWD",
                           "RWD",
                           "4WD"),
            selected = "RWD"
          ),
          
          sliderInput(
            "Wb",
            "Wheelbase:",
            min = 85,
            max = 120,
            value = 93
          ),
          
          sliderInput(
            "Cl",
            "Car length:",
            min = 140,
            max = 200,
            value = 170
          ),
          
          sliderInput(
            "Cw",
            "Car witdh:",
            min = 60,
            max = 80,
            value = 70
          ),
          
          sliderInput(
            "Cubw",
            "Curbwheight:",
            min = 1600,
            max = 4000,
            value = 2300
          ),
          
          sliderInput(
            "Es",
            "Engine size:",
            min = 80,
            max = 250,
            value = 150
          ),
          
          sliderInput(
            "Br",
            "Boreratio:",
            min = 2.5,
            max = 5.5,
            value = 3.19
          ),
          
          sliderInput(
            "Hp",
            "Horse power:",
            min = 55,
            max = 700,
            value = 120
          ),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          div(
            h1("Geely Auto", align = "center")
          ),
          
          h2("What will be the price of the car ?"),
          h4("The price will be:", align = "center"),
          div(textOutput("distPlot"), style = "font-size:300%; font-weight:200", align = "center"),
          
          div(
            img(src = "Fernando.jpg", width = 500, style = "display: block; margin-left: auto; margin-right: auto")
          )
        )
      )
    )
    
# Define server logic required to draw a histogram
server <- function(input, output) {
  build_predict_frame <- reactive({
    Brand <- input$B
    Model <- input$Mdl
    doornumber <- input$Dn
    carbody <- input$Cb
    drivewheel <- input$Drw
    wheelbase <- input$Wb
    carlength <- input$Cl
    carwitdh <- input$Cw
    curbweight <- input$Cubw
    cylindernumber <- input$Cn
    enginesize <- input$Es
    boreratio <- input$Br
    horsepower <- input$Hp
    fueltype <- input$Ft
    
    data.frame(
      Brand,
      Model,
      doornumber,
      carbody,
      drivewheel,
      wheelbase,
      carlength,
      carwitdh,
      curbweight,
      cylindernumber,
      enginesize,
      boreratio,
      horsepower,
      fueltype
    )
    
  })
  
  output$distPlot <- renderText({
    predict_frame <- build_predict_frame()
    
    prediction <- h2o.predict(model, as.h2o(predict_frame))
    prediction <- as.data.frame(prediction)
    
    paste(floor(prediction$predict), "$")
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
