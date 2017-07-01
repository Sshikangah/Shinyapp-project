

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Explore how ingredients of wine affect its quality"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
              selectInput("variable", "Variable:",
                          c("fixed.acidity", "volatile.acidity", "residual.sugar", "citric.acid","chlorides",
                            "density", "pH", "sulphates", "alcohol")),
              tableOutput("data")

      ),

      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
                tabPanel("plot",
                   h3("Visualize relationship between Quality and wine ingredient:"),
                   plotOutput("plot")
                ),
                tabPanel("summary",
                         verbatimTextOutput("model1")
                         )
        )

      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
        dat <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep = ";")
        dat$quality <- as.factor(dat$quality)
        output$data <- renderTable({
                dat[, c("quality", input$variable), drop = FALSE]
        }, rownames = TRUE)
        output$plot <- renderPlot({
                set1 <- dat[, c("quality", input$variable), drop = FALSE]
                plot(set1)
        })
        output$model1 <- renderPrint({
                set1 <- dat[, c("quality", input$variable), drop = FALSE]
                model1 <- glm(set1$quality ~., family = binomial(link = 'logit'), data = dat)
                summary(model1)
        })


}



# Run the application
shinyApp(ui = ui, server = server)

