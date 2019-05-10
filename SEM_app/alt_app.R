#
# Paul Harmon
# README: The Shiny App created here is written by Paul Harmon. Its intent is to assess the sensitivity of the 
# Carnegie Classifications to small changes in the underlying variables used to calculate each index. 
#
# Note that the functions and data must be read in prior to running this code. The easiest way to do this right now is 
# probably is to just run all of the code in the R-Code for Carnegie Stuff.R file. We might source it in later .
#

#reads in the dataset needed
library(dplyr);library(ggplot2);library(ggthemes);library(mclust);library(ggforce);library(shinyjs)
cc2015 <- filter(read.csv("CC2015data.csv",header = TRUE),BASIC2015 %in%c(15,16,17))

names <- unique(cc2015$NAME)
#gets the right package going
library(shiny); library(shinythemes)


###UI #######################################
# Define UI for application that allows you to pick a college and see how it would change
ui <- fluidPage(
  #shiny theme
  theme = shinytheme("superhero"),
  
  # Application title
  titlePanel("Sensitivity of the SEM Scale Classifications"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "school", "School", choices = names, selected = NULL, multiple = FALSE,
                  selectize = TRUE), 
      useShinyjs(),
      hr(),
      actionButton("resetAll", "Reset all",style = "background-color: gray;"),
      hr(),
      div( id = "form",
           sliderInput("sosc",
                       "Number of Additional Social Science PhDs:",
                       min = 0,
                       max = 60,
                       value = 0)
           ,
           
           sliderInput("other",
                       "Number of Additional Other PhDs:",
                       min = -9,
                       max = 50,
                       value = 0)
           ,
           sliderInput("stem",
                       "Number of Additional STEM PhDs:",
                       min = -45,
                       max = 5000,
                       step = 5,
                       value = 0)
           ,
           sliderInput("hum",
                       "Number of Additional Humanities PhDs:",
                       min = -2,
                       max = 50,
                       value = 0)
           ,
           sliderInput("staff",
                       "Additional Research Staff:",
                       min = -75,
                       max = 20000,
                       value = 0,
                       step = 100)
           ,
           sliderInput("serd",
                       "Additional Stem Research Exp. (Thousands of Dollars):",
                       min = -105000,
                       max = 2300000,
                       value = 0,
                       step = 10000)
           ,
           sliderInput("nonserd",
                       "Additional Non-Stem Research Exp. (Thousands of Dollars):",
                       min = -9000,
                       max = 1000000,
                       value = 0,
                       step = 1000)
      )#div
    ), #sidebar panel
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "pills",
                  tabPanel("SEM Classifications",plotOutput("classPlot")),
                  tabPanel("Carnegie Classifications", plotOutput("ccPlot")),
                  tabPanel("Similar Institutions", tableOutput("swag"))
                  
                  
      )
    )
  )
)



# GLOBAL CODE #######################################################
cc2015 <- read.csv("CC2015data.csv",header = TRUE)

#function for ranking the data
minrank <- function(x){rank(x, ties.method = "min")}

#dataset that we want to use
cc2015Ps<-
  na.omit(cc2015[,c("NAME","BASIC2010","BASIC2015","FACNUM","HUM_RSD","OTHER_RSD","SOCSC_RSD","STEM_RSD","PDNFRSTAFF","S.ER.D","NONS.ER.D")])

#calculate the ranked data
cc2015.r <- data.frame(cc2015Ps[,1:3],sapply(cc2015Ps[,-c(1:3)],minrank)) 
cc2015percap <- cc2015Ps[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/cc2015Ps$FACNUM
colnames(cc2015percap) <- c("PDNRSTAFF_PC", "S.ER.D_PC", "NONS.ER.D_PC")
cc2015percap.r<-data.frame(sapply(cc2015percap,minrank))

#sem using raw data
cc2015_new <- cbind(cc2015Ps, cc2015percap)


model_alt <- '
#latent factors
STEM=~STEM_RSD+PDNFRSTAFF+S.ER.D + FACNUM + S.ER.D_PC + PDNRSTAFF_PC
HUM=~HUM_RSD + OTHER_RSD + SOCSC_RSD + NONS.ER.D + FACNUM + NONS.ER.D + PDNRSTAFF_PC
#factor of factors
Overall=~STEM+HUM
'



lavaan_sem_r_alternate <- lavaan::sem(model_alt, data=cc2015_new, std.lv=TRUE, orthogonal=FALSE, se="robust.huber.white")
lavaan::summary(lavaan_sem_r_alternate, fit.measures=TRUE)

#predicts the scores
CCScores_r_cov <- as.data.frame(lavaan::predict(lavaan_sem_r_alternate))
CCScores_r_cov_scale <- apply(CCScores_r_cov[,c(1,2)], 2, scale)
range_scores <- max(CCScores_r_cov$Overall) - min(CCScores_r_cov$Overall)
CCScores_r_cov$rate_2015 <- ifelse(CCScores_r_cov$Overall < min(CCScores_r_cov$Overall)+((1/3)*range_scores), 'A', ifelse(CCScores_r_cov$Overall > max(CCScores_r_cov$Overall)-((1/3)*range_scores), 'C', 'B'))
#CC_table <- as.data.frame(cbind(cc2015_new$NAME, cc2015_new$BASIC2015, CCScores_r_cov$rate_2015))
#colnames(CC_table) <- c("name", "basic2015", "rate2015")

#we want to reference the correctly input school





server <- function(input, output,session) {
  # cc2015.full <- read.csv("Updated2015.csv", header = TRUE)
  
  #Defines a Reactive Expression for each school selected
  new_school <- reactive({
    # Change when the "update" button is pressed...
    input$school
  })
  
  
  #reset sliders: 
  observeEvent(input$resetAll,{
    reset("form")
  })
  
  
  output$classPlot <- renderPlot({
    
    inst_name <- new_school()
    new_dat <- cc2015Ps
    
    
    ##References School of Interest  
    a <- which(as.character(new_dat$NAME) == as.character(inst_name))
    #current_school <- as.character(input$school)
    
    ##To Move Left or Right
    # adds one phd to the number of social science phds
    new_dat[a,"SOCSC_RSD"] <- new_dat[a,"SOCSC_RSD"] + input$sosc
    #adds one phd to the number of other phds
    new_dat[a,"OTHER_RSD"] <- new_dat[a,"OTHER_RSD"] + input$other
    #adds one phd to the number of stem phds
    new_dat[a,"STEM_RSD"] <- new_dat[a,"STEM_RSD"] + input$stem
    #adds one phd to the number of humanities phds
    new_dat[a,"HUM_RSD"] <- new_dat[a,"HUM_RSD"] + input$hum
    
    
    #adds research staff by 1 ; also adds to the FACNUM
    new_dat[a,"PDNFRSTAFF"] <- new_dat[a,"PDNFRSTAFF"] + input$staff
    
    #adds to NonSERD expenditures
    new_dat[a,"NONS.ER.D"] <- new_dat[a,"NONS.ER.D"] + input$nonserd
    #adds to SERD expenditures
    new_dat[a,"S.ER.D"] <- new_dat[a,"S.ER.D"] + input$serd
    
    ##########REMOVE THIS CODE#########################################    
    COLOR <- rep(1,nrow(new_dat)); COLOR[a] <- 2 
    #plot(new_dat$PDNFRSTAFF, pch = 19 + COLOR, col = COLOR, cex = COLOR)
    ####################################################################  
    
    new_dat_r <- data.frame(new_dat[,1:3],sapply(new_dat[,-c(1:3)],minrank))
    #calculate the ranked data for PC
    cc2015percap <- cc2015Ps[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/cc2015Ps$FACNUM
    colnames(cc2015percap) <- c("PDNRSTAFF_PC", "S.ER.D_PC", "NONS.ER.D_PC")
    cc2015percap.r<-data.frame(sapply(cc2015percap,minrank))
    
    #sem using raw data
    new_dat_r <- cbind(new_dat_r, cc2015percap.r)
    
    lavaan_NEW<- lavaan::sem(model_alt, data=new_dat_r, std.lv=TRUE, orthogonal=FALSE, se="robust.huber.white")
    lavaan::summary(lavaan_NEW, fit.measures=TRUE)
    
    ##predicts the scores
    CCScores_r_cov_new <- as.data.frame(lavaan::predict(lavaan_NEW))
    range_scores_new <- max(CCScores_r_cov_new$Overall) - min(CCScores_r_cov_new$Overall)
    CCScores_r_cov_new$rate_2015 <- ifelse(CCScores_r_cov_new$Overall < min(CCScores_r_cov_new$Overall)+((1/3)*range_scores), 'A', ifelse(CCScores_r_cov$Overall > max(CCScores_r_cov$Overall)-((1/3)*range_scores), 'C', 'B'))
    
    ##Code to generate the plot and change the indices 
    CCScores_r_cov_new$symbols <- rep(0,length(CCScores_r_cov_new)) 
    CCScores_r_cov_new$symbols[a] <- 1
    
    ##Now we classify based on the scores
    
    mcres<-Mclust(CCScores_r_cov_new$Overall)
    #summary(mcres)
    
    
    Classifications <- mcres$classification
    #rownames(Classifications) <- cc2015Ps$NAME
    
    #create a table of nearest neighbors
    dist <- abs(CCScores_r_cov_new$Overall[a] - CCScores_r_cov_new$Overall)
    new_dat_r[order(dist)[1:11],]
    
    #creates a plot and colors by Carnegie Classification Colors  
    ggplot(CCScores_r_cov_new) + geom_point(aes(x = STEM, y = HUM, color = factor(Classifications), shape = factor(symbols), size = factor(symbols)))+ 
      ggtitle("Predicted vs Actual Classifications") + theme_bw() + coord_fixed(ratio = 1)+
      theme_classic() + guides(shape = FALSE) + guides(size = FALSE) + 
      labs(color = "Classification")+xlab("STEM") + ylab("Non-STEM")
    
    
  })
  
  #Carnegie Classifications Version of this
  output$ccPlot <- renderPlot({
    
    inst_name <- new_school()
    new_dat <- cc2015Ps
    
    
    ##References School of Interest  
    a <- which(as.character(new_dat$NAME) == as.character(inst_name))
    #current_school <- as.character(input$school)
    
    ##To Move Left or Right
    # adds one phd to the number of social science phds
    new_dat[a,"SOCSC_RSD"] <- new_dat[a,"SOCSC_RSD"] + input$sosc
    #adds one phd to the number of other phds
    new_dat[a,"OTHER_RSD"] <- new_dat[a,"OTHER_RSD"] + input$other
    #adds one phd to the number of stem phds
    new_dat[a,"STEM_RSD"] <- new_dat[a,"STEM_RSD"] + input$stem
    #adds one phd to the number of humanities phds
    new_dat[a,"HUM_RSD"] <- new_dat[a,"HUM_RSD"] + input$hum
    
    
    #adds research staff by 1 ; also adds to the FACNUM
    new_dat[a,"PDNFRSTAFF"] <- new_dat[a,"PDNFRSTAFF"] + input$staff
    
    #adds to NonSERD expenditures
    new_dat[a,"NONS.ER.D"] <- new_dat[a,"NONS.ER.D"] + input$nonserd
    #adds to SERD expenditures
    new_dat[a,"S.ER.D"] <- new_dat[a,"S.ER.D"] + input$serd
    
    #creates the newdat pc object 
    new_dat_pc <- new_dat[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/new_dat$FACNUM
    
    
    AGcc <- function(x){
      #rank the data
      ranked <- data.frame(x[,1:3],sapply(x[,-c(1:3)],minrank)) 
      #get pc's
      pca.ranked <- prcomp(ranked[,-c(1:4)], scale = TRUE)
      summary <- summary(pca.ranked)
      standard.score <- scale(pca.ranked$x[,1], scale = TRUE, center = TRUE)
      #needs to return the standardized scores
      return(list(scorez = standard.score, sum =summary))
    }
    #function for percap
    PCcc <- function(x){
      #rank the data
      ranked.dat <- data.frame(sapply(x,minrank)) 
      #get pc's
      pc.ranked <- prcomp(ranked.dat, scale = TRUE)
      summary <- summary(pc.ranked)
      standard.score <- scale(pc.ranked$x[,1], scale = TRUE, center = TRUE)
      return(list(scorez = standard.score, sum = summary))
    }
    
    
    percap <- PCcc(new_dat_pc)
    ag <- AGcc(new_dat)
    
    mean.percap <- 340.52
    sd.percap <- 170.51
    mean.ag <- 780.74
    sd.ag <- 413.10
    
    rawscores.percap <- sd.percap * -percap$scorez + mean.percap
    rawscores.ag <- sd.ag * ag$scorez + mean.ag
    
    #X1 <- seq(500,984,by =1)
    R1 <- 984.007
    #X2 <- seq(0,409,by =1)
    R2<- 409.461
    
    #establish symbols
    cdat <- data.frame(cbind(rawscores.ag,rawscores.percap,factor(cc2015Ps$BASIC2015))); names(cdat) <- c("AG","PC","Basic")
    cdat$Symbols <- rep(0, nrow(cdat))
    cdat$Symbols[a] <- 1
    #creates a plot and colors by Carnegie Classification Colors  
    ggplot(cdat) + geom_point(aes(x =AG ,y = PC, color = factor(Basic),shape = factor(Symbols),size = factor(Symbols))) + theme_classic() + coord_fixed(ratio = 1) +
      xlab("Aggregate Index") + ylab("Per-Capita Index") + ggtitle("2015 Carnegie Classifications") + 
      scale_color_discrete(name = "Classification",labels = c("R1","R2","R3")) + guides(shape = FALSE, size = FALSE)+
      stat_arc(aes(x0 = 0, y0 = 0, r = R1, start = 0,end = 1.58)) + 
      stat_arc(aes(x0 = 0,y0 = 0, r = R2, start = 0, end = 1.555 ))
  })
  
  
  #render some test text
  output$swag <- renderTable({
    
    inst_name <- new_school()
    
    a <- which(as.character(cc2015Ps$NAME) == as.character(inst_name))
    
    #new_table <- rbind(cc2015Ps[a,],topten)
    inst_name <- new_school()
    new_dat <- cc2015Ps
    
    
    ##References School of Interest  
    a <- which(as.character(new_dat$NAME) == as.character(inst_name))
    #current_school <- as.character(input$school)
    
    ##To Move Left or Right
    # adds one phd to the number of social science phds
    new_dat[a,"SOCSC_RSD"] <- new_dat[a,"SOCSC_RSD"] + input$sosc
    #adds one phd to the number of other phds
    new_dat[a,"OTHER_RSD"] <- new_dat[a,"OTHER_RSD"] + input$other
    #adds one phd to the number of stem phds
    new_dat[a,"STEM_RSD"] <- new_dat[a,"STEM_RSD"] + input$stem
    #adds one phd to the number of humanities phds
    new_dat[a,"HUM_RSD"] <- new_dat[a,"HUM_RSD"] + input$hum
    
    
    #adds research staff by 1 ; also adds to the FACNUM
    new_dat[a,"PDNFRSTAFF"] <- new_dat[a,"PDNFRSTAFF"] + input$staff
    
    #adds to NonSERD expenditures
    new_dat[a,"NONS.ER.D"] <- new_dat[a,"NONS.ER.D"] + input$nonserd
    #adds to SERD expenditures
    new_dat[a,"S.ER.D"] <- new_dat[a,"S.ER.D"] + input$serd
    
    ##########REMOVE THIS CODE#########################################    
    # COLOR <- rep(1,nrow(new_dat)); COLOR[a] <- 2 
    #plot(new_dat$PDNFRSTAFF, pch = 19 + COLOR, col = COLOR, cex = COLOR)
    ####################################################################  
    
    new_dat_r <- data.frame(new_dat[,1:3],sapply(new_dat[,-c(1:3)],minrank)) 
    #calculate the ranked data
    cc2015percap <- cc2015Ps[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/cc2015Ps$FACNUM
    colnames(cc2015percap) <- c("PDNRSTAFF_PC", "S.ER.D_PC", "NONS.ER.D_PC")
    cc2015percap.r<-data.frame(sapply(cc2015percap,minrank))
    
    #sem using raw data
    new_dat_r <- cbind(new_dat_r, cc2015percap.r)
    
    
    lavaan_NEW<- lavaan::sem(model_alt, data=new_dat_r, std.lv=TRUE, orthogonal=FALSE, se="robust.huber.white")
    lavaan::summary(lavaan_NEW, fit.measures=TRUE)
    
    ##predicts the scores
    CCScores_r_cov_new <- as.data.frame(lavaan::predict(lavaan_NEW))
    range_scores_new <- max(CCScores_r_cov_new$Overall) - min(CCScores_r_cov_new$Overall)
    #CCScores_r_cov_new$rate_2015 <- ifelse(CCScores_r_cov_new$Overall < min(CCScores_r_cov_new$Overall)+((1/3)*range_scores), 'A', ifelse(CCScores_r_cov$Overall > max(CCScores_r_cov$Overall)-((1/3)*range_scores), 'C', 'B'))
    
    ##Code to generate the plot and change the indices 
    CCScores_r_cov_new$symbols <- rep(0,length(CCScores_r_cov_new)) 
    CCScores_r_cov_new$symbols[a] <- 1
    
    ##Now we classify based on the scores
    
    mcres<-Mclust(CCScores_r_cov_new$Overall)
    #summary(mcres)
    
    
    Classifications <- mcres$classification
    #rownames(Classifications) <- cc2015Ps$NAME
    
    #create a table of nearest neighbors
    dist1 <- abs(CCScores_r_cov_new$Overall[a] - CCScores_r_cov_new$Overall)
    new_dat_r$SCORE <- CCScores_r_cov_new$Overall
    new_dat_r$SEMCLASS <- Classifications
    ordered_dat <- new_dat_r[order(dist1),]
    peer_inst_presort <- ordered_dat[2:5,]
    peer_inst <- peer_inst_presort[order(peer_inst_presort$SCORE,decreasing = TRUE),]
    new_table <- rbind(ordered_dat[1,],peer_inst)
    aspire_f <- function(x){ifelse(new_table$SCORE[1] < x,"ASPIRE","PEER")}
    new_table$STATUS <- c("CHOSEN SCHOOL",sapply(new_table$SCORE[2:5],aspire_f))
    
    names(new_table)<- c("NAME","x","CARN2015","FACULTY","HUM PHD","OTHER PHD","SOC.SCI PHD","STEM PHD",
                         "POSTDOC","STEM.EXP","NONSTEM.EXP","SEMSCORE","SEMCLASS","STATUS")
    new_table[,c(1,3,13,4:12,14)]
  }) 
  
}

# Run the application 
shinyApp(ui = ui, server = server)

