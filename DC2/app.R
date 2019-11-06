#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#######################################################################################################################
library(shiny)
library(arrow)
library(tidyverse)

#######################################################################################################################

metadata = arrow::read_feather('CDs_and_Vinyl', as_data_frame = TRUE)
cds <- read.csv(file="CDs_and_Vinyl_5.csv", header=TRUE, sep=",")
leftJoinDf <- left_join(cds,metadata,by="asin")
leftJoinDf <- leftJoinDf %>% mutate(time = as.POSIXct(as.numeric(as.character(unixReviewTime)),origin="1970-01-01",tz="GMT"))

top_reviewed_product <- cds %>% filter(asin == "B00008OWZG")

# interesting asins
# B002NACY22

#######################################################################################################################
# UI Definition
ui <- fluidPage(

    fluidRow(
        column(2),
        column(8, align="center",
               # Application title
               titlePanel("CDs and Vinyl Rating Breakdown"),
               
               div("By Katie Mummah"),
               
               div("Built with",
                   img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                   "by",
                   img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", 
                       height = "25px"),
                   ".")
        ),
        column(2)
    ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
    
        # Show a plot of the generated distribution
        mainPanel(
           #plotOutput("distPlot"),
            plotOutput("testplot2",
                       click = "plot1_click",
                       brush = brushOpts(
                           id = "plot1_brush")
                       ),
            plotOutput("testplot"),
            plotOutput("timeseries")
        ),
        sidebarPanel(
            selectInput("choosecategory",
                        label="Choose a category",
                        choices=c("Pop" = "CDs & Vinyl,Pop", 
                                  "Country" = 'CDs & Vinyl,Country', 
                                  "Death Metal" = "CDs & Vinyl,Metal,Death Metal",
                                  "Metal" = "CDs & Vinyl,Metal",
                                  "Children's Music" = "CDs & Vinyl,Children's Music",
                                  "Classical" = "CDs & Vinyl,Classical",
                                  "Christian" = "CDs & Vinyl,Christian",
                                  "Blues" = "CDs & Vinyl,Blues",
                                  "Jazz" = "CDs & Vinyl,Jazz",
                                  "Soundtracks" = "CDs & Vinyl,Soundtracks",
                                  "Disney" = "CDs & Vinyl,Children's Music,Disney",
                                  "Dance & Electronic" = "CDs & Vinyl,Dance & Electronic",
                                  "Soul" = "CDs & Vinyl,R&B,Soul",
                                  "Reggae" = "CDs & Vinyl,Reggae",
                                  "R&B" = "CDs & Vinyl,R&B"
                                  ),
                        selected="CDs & Vinyl,Pop"),
            h4("Points near click"),
            verbatimTextOutput("click_info"),
            h4("Brushed points"),
            verbatimTextOutput("brush_info"),
            verbatimTextOutput("points_asin")
            
        )
    )
)

#######################################################################################################################

# Define server logic required to draw a histogram
server <- function(input, output) {

########################################################### clicks
    
    output$click_info <- renderPrint({
        # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
        # were a base graphics plot, we'd need those.
        nearPoints(average_review(), input$plot1_click, addDist = TRUE)
    })
    
    output$brush_info <- renderPrint({
        brushedPoints(average_review(), input$plot1_brush)$asin
    })
    
########################################################### reactive data
    
    data_category <- reactive({
        req(input$choosecategory)
        leftJoinDf %>% filter(categoriesstring == input$choosecategory)
    })
    
    top_reviewed_product <- reactive({
        data_category() %>% filter(asin %in% nearPoints(average_review(), input$plot1_click, addDist = TRUE)$asin)
    })
    
    average_review <- reactive({
        data_category() %>% group_by(asin) %>% summarise(avr_rating = mean(overall), count=n(), price=mean(price))
    })
    
########################################################### plots
    
    output$testplot <- renderPlot({
        ggplot(data=top_reviewed_product(), aes(overall, fill=factor(overall))) + 
            geom_bar(position = position_stack(reverse = TRUE)) + coord_flip() +
            scale_fill_viridis(discrete=TRUE) + theme_bw() + 
            theme(panel.grid.minor.y=element_blank(),panel.grid.major.y=element_blank())
    })
    
    output$testplot2 <- renderPlot({
        #my_breaks = c(0.1, 10, 100, 400, 800)
        ggplot(data=average_review(), aes(avr_rating, count)) + geom_point(aes(color=price)) +
            scale_color_viridis()
    })
    
    output$timeseries <- renderPlot({
        ggplot(data=top_reviewed_product(), aes(time, overall)) + geom_point(aes(color=factor(overall)),position = "jitter") +
        scale_color_viridis(discrete=TRUE) + theme_bw() + theme(panel.grid.minor.y=element_blank(),
                                                                panel.grid.major.y=element_blank())
    })
    
}

#######################################################################################################################

# Run the application 
shinyApp(ui = ui, server = server)
