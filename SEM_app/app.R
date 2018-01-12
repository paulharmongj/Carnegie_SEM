#
# README: The Shiny App created here is written by Paul Harmon. Its intent is to assess the sensitivity of the 
# Carnegie Classifications to small changes in the underlying variables used to calculate each index. 
#
# Note that the functions and data must be read in prior to running this code. The easiest way to do this right now is 
# probably is to just run all of the code in the R-Code for Carnegie Stuff.R file. We might source it in later .
#

#reads in the dataset needed
cc2015 <- read.csv("C:/Users/paulh/Documents/Carnegie-SEM/data/CC2015data.csv",header = TRUE)
names <- unique(cc2015$NAME)
#gets the right package going
library(shiny)

# Define UI for application that allows you to pick a college and see how it would change
ui <- fluidPage(
  
  # Application title
  titlePanel("Sensitivity of the Carnegie Classifications"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "school", "School", choices = names, selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL)
      
      , 
      
      sliderInput("sosc",
                  "Number of Additional Social Science PhDs from 0:",
                  min = 0,
                  max = 60,
                  value = 0)
      ,
      
      sliderInput("other",
                  "Number of Additional Other PhDs from 9:",
                  min = -9,
                  max = 50,
                  value = 0)
      ,
      sliderInput("stem",
                  "Number of Additional STEM PhDs from 45:",
                  min = -45,
                  max = 50,
                  step = 5,
                  value = 0)
      ,
      sliderInput("hum",
                  "Number of Additional Humanities PhDs from 2:",
                  min = -2,
                  max = 50,
                  value = 0)
      ,
      sliderInput("staff",
                  "Additional Research Staff from 75:",
                  min = -75,
                  max = 2000,
                  value = 0,
                  step = 100)
      ,
      sliderInput("serd",
                  "Additional Stem Research Expenditures:",
                  min = -105000,
                  max = 2300000,
                  value = 0,
                  step = 10000)
      ,
      sliderInput("nonserd",
                  "Additional Non-Stem Research Expenditures:",
                  min = -9000,
                  max = 1000000,
                  value = 0,
                  step = 1000)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to change the classifications
cc2015 <- read.csv("C:/Users/paulh/Documents/Carnegie-SEM/data/CC2015data.csv",header = TRUE)

#function for ranking the data
minrank <- function(x){rank(x, ties.method = "min")}

#dataset that we want to use
cc2015Ps<-
  na.omit(cc2015[,c("NAME","BASIC2010","BASIC2015","FACNUM","HUM_RSD","OTHER_RSD","SOCSC_RSD","STEM_RSD","PDNFRSTAFF","S.ER.D","NONS.ER.D")])

#calculate the ranked data
cc2015.r <- data.frame(cc2015Ps[,1:3],sapply(cc2015Ps[,-c(1:3)],minrank)) 


model_alt <- '
#latent factors
STEM=~STEM_RSD+PDNFRSTAFF+S.ER.D + FACNUM
HUM=~HUM_RSD + OTHER_RSD + SOCSC_RSD + NONS.ER.D + FACNUM
#factor of factors
Overall=~STEM+HUM

'
lavaan_sem_r_alternate <- lavaan::sem(model_alt, data=cc2015.r, std.lv=TRUE, orthogonal=FALSE, se="robust.huber.white")
lavaan::summary(lavaan_sem_r_alternate, fit.measures=TRUE)

#predicts the scores
CCScores_r_cov <- as.data.frame(lavaan::predict(lavaan_sem_r_alternate))
CCScores_r_cov_scale <- apply(CCScores_r_cov[,c(1,2)], 2, scale)
range_scores <- max(CCScores_r_cov$Overall) - min(CCScores_r_cov$Overall)
CCScores_r_cov$rate_2015 <- ifelse(CCScores_r_cov$Overall < min(CCScores_r_cov$Overall)+((1/3)*range_scores), 'A', ifelse(CCScores_r_cov$Overall > max(CCScores_r_cov$Overall)-((1/3)*range_scores), 'C', 'B'))
#CC_table <- as.data.frame(cbind(cc2015_new$NAME, cc2015_new$BASIC2015, CCScores_r_cov$rate_2015))
#colnames(CC_table) <- c("name", "basic2015", "rate2015")

#we want to reference the correctly input school


#misclassification rate
library(ggplot2)

server <- function(input, output) {
  # cc2015.full <- read.csv("Updated2015.csv", header = TRUE)
  
  current_school <- reactive(as.character(input$school))
  a <- which(cc2015Ps$NAME %in% c(current_school))
  
  output$distPlot <- renderPlot({
    
    ##To Move Left or Right
    # adds one phd to the number of social science phds
    cc2015Ps[a,"SOCSC_RSD"] <- cc2015Ps[a,"SOCSC_RSD"] + input$sosc
    #adds one phd to the number of other phds
    cc2015Ps[a,"OTHER_RSD"] <- cc2015Ps[a,"OTHER_RSD"] + input$other
    #adds one phd to the number of stem phds
    cc2015Ps[a,"STEM_RSD"] <- cc2015Ps[a,"STEM_RSD"] + input$stem
    #adds one phd to the number of humanities phds
    cc2015Ps[a,"HUM_RSD"] <- cc2015Ps[a,"HUM_RSD"] + input$hum
    ###To Move Up or Down:
    #adds research staff by 1 ; also adds to the FACNUM
    cc2015Ps[a,"PDNFRSTAFF"] <- cc2015Ps[a,"PDNFRSTAFF"] + input$staff
    
    #adds to NonSERD expenditures
    cc2015Ps[a,"NONS.ER.D"] <- cc2015Ps[a,"NONS.ER.D"] + input$nonserd
    #adds to SERD expenditures
    cc2015Ps[a,"S.ER.D"] <- cc2015Ps[a,"S.ER.D"] + input$serd
    
    ##now re-calculate the per-capita scores, with additional staff added in
    
    cc2015.r <- data.frame(cc2015Ps[,1:3],sapply(cc2015Ps[,-c(1:3)],minrank)) 
    
    
    lavaan_sem_r_alternate_new <- lavaan::sem(model_alt, data=cc2015.r, std.lv=TRUE, orthogonal=FALSE, se="robust.huber.white")
    lavaan::summary(lavaan_sem_r_alternate_new, fit.measures=TRUE)
    
    #predicts the scores
    CCScores_r_cov_new <- as.data.frame(lavaan::predict(lavaan_sem_r_alternate_new))
    range_scores_new <- max(CCScores_r_cov_new$Overall) - min(CCScores_r_cov_new$Overall)
    CCScores_r_cov_new$rate_2015 <- ifelse(CCScores_r_cov_new$Overall < min(CCScores_r_cov_new$Overall)+((1/3)*range_scores), 'A', ifelse(CCScores_r_cov$Overall > max(CCScores_r_cov$Overall)-((1/3)*range_scores), 'C', 'B'))
   
  CCScores_r_cov_new$symbols <- rep(0,length(CCScores_r_cov_new)) 
  CCScores_r_cov_new$symbols[a] <- 1
  #creates a plot and colors by Carnegie Classification Colors  
  ggplot(CCScores_r_cov_new) + geom_point(aes(x = STEM, y = HUM, color = factor(symbols), symbol = factor(symbols)))+ 
   ggtitle("Predicted vs Actual Classifications") + theme_bw() + coord_fixed(ratio = 1)
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

