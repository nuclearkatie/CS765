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
library(shinyWidgets)
#library(arrow)
library(tidyverse)
library(viridis)

#######################################################################################################################

#metadata = arrow::read_feather('CDs_and_Vinyl', as_data_frame = TRUE)
#cds <- read.csv(file="CDs_and_Vinyl_5.csv", header=TRUE, sep=",")
#leftJoinDf <- left_join(cds,metadata,by="asin")
#leftJoinDf <- leftJoinDf %>% mutate(time = as.POSIXct(as.numeric(as.character(unixReviewTime)),origin="1970-01-01",tz="GMT"))

leftJoinDf <- read.csv(file="CDs_and_Vinyl_joined.csv", header=TRUE, sep=",")

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
    tabsetPanel(type = "tabs",
            tabPanel(tags$b("Products"),
        
        # Show a plot of the generated distribution
        mainPanel(
           #plotOutput("distPlot"),
            plotOutput("plot1",
                       click = "plot1_click",
                       brush = brushOpts(
                           id = "plot1_brush")
                       ),
            plotOutput("plot2"),
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
                                  "R&B" = "CDs & Vinyl,R&B",
                                  "New Age" = "CDs & Vinyl,New Age",
                                  "Alternative Rock" = "CDs & Vinyl,Alternative Rock",
                                  "Folk" = "CDs & Vinyl,Folk",
                                  "Rap" = "CDs & Vinyl,Rap",
                                  "Broadway & Vocalists" = "CDs & Vinyl,Broadway & Vocalists",
                                  "Holidays & Weddings" = "CDs & Vinyl,Holiday & Wedding",
                                  "Gospel" = "CDs & Vinyl,Gospel",
                                  "Latin" = "CDs & Vinyl,Latin Music"
                                  ),
                        selected="CDs & Vinyl,Pop"),
            sliderInput("sizeslider", "Number of reviews to show (randomly sampled):",
                        min = 1, max = 30993,
                        value = 30993),
            sliderInput("pricerange", label="Select a price range",
                        min=0, max=800.47,
                        step=5, value=c(0,800.47),
                        pre="$"),
            radioButtons(
                inputId = "clickorbrush",
                label = "How to select data",
                choices = c("Click", "Brush"),
                selected = "Click"
            ),
            h4("Point nearest click/brush"),
            verbatimTextOutput("click_info"),
            #h4("Brushed points"),
            #verbatimTextOutput("brush_info"),
            h5("Title of selected data"),
            htmlOutput("printtitle"),
            h5("Price"),
            htmlOutput("pricedescription"),
            h5("Description"),
            htmlOutput("printdescription")
        )
    ),
    tabPanel(tags$b("Reviewers"),
    mainPanel(
        plotOutput("reviewer1",
                   click = "reviewer1_click",
                   brush = brushOpts(
                       id = "reviewer1_brush")
        ),
        plotOutput("reviewer2"),
        plotOutput("timeseriesreviewer")
    ),
    sidebarPanel(selectInput("choosecategoryreviewer",
                             label="Choose a category",
                             choices=c("Pop" = "CDs & Vinyl,Pop", 
                                       "Country" = 'CDs & Vinyl,Country', 
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
                                       "R&B" = "CDs & Vinyl,R&B",
                                       "New Age" = "CDs & Vinyl,New Age",
                                       "Alternative Rock" = "CDs & Vinyl,Alternative Rock",
                                       "Folk" = "CDs & Vinyl,Folk",
                                       "Rap" = "CDs & Vinyl,Rap",
                                       "Broadway & Vocalists" = "CDs & Vinyl,Broadway & Vocalists",
                                       "Holidays & Weddings" = "CDs & Vinyl,Holiday & Wedding",
                                       "Gospel" = "CDs & Vinyl,Gospel",
                                       "Latin" = "CDs & Vinyl,Latin Music"
                             ), 
                             selected="CDs & Vinyl,Pop"),
                 sliderInput("sizesliderreviewer", "Number of reviews to show (randomly sampled):",
                             min = 1, max = 74338,
                             value = 74338),
                 radioButtons(
                     inputId = "clickorbrushreviewer",
                     label = "How to select data",
                     choices = c("Click", "Brush"),
                     selected = "Click"
                 ),
                 h4("Reviewers nearest click/brush"),
                 verbatimTextOutput("click_info_product")
    ))
))

#######################################################################################################################

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
###########################################################
########################################################### clicks
###########################################################    
    
    output$click_info <- renderPrint({
        # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
        # were a base graphics plot, we'd need those.
        if (input$clickorbrush == "Click"){
            nearPoints(average_review_price(), input$plot1_click, addDist = TRUE)$asin
            }
        else{
            brushedPoints(average_review_price(), input$plot1_brush)$asin
        }
    })
    
    output$click_info_product <- renderPrint({
        # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
        # were a base graphics plot, we'd need those.
        if (input$clickorbrushreviewer == "Click"){
            nearPoints(average_review_reviewer_sampled(), input$reviewer1_click, addDist = TRUE)$reviewerID
        }
        else{
            brushedPoints(average_review_reviewer_sampled(), input$reviewer1_brush)$reviewerID
        }
    })
    
    #output$brush_info <- renderPrint({
    #    brushedPoints(average_review_sampled(), input$plot1_brush)$asin
    #})
    
    output$printtitle <- renderText({
        if (input$clickorbrush == "Click"){
            clicked_product()$title[1]
        }
        else{
            clicked_product()$title[1]
        }
    })
    
    ##
    
    output$printdescription <- renderText({
        if (input$clickorbrush == "Click"){
            clicked_product()$description[1]
        }
        else{
            clicked_product()$description[1]
        }
    })
    
    output$pricedescription <- renderText({
        if (input$clickorbrush == "Click"){
            paste0("$", clicked_product()$price[1])
        }
        else{
            paste0("$", clicked_product()$price[1])
        }
    })

###########################################################    
########################################################### reactive data
###########################################################
        
    data_category <- reactive({
        req(input$choosecategory)
        #filter(str_detect(rowname, "^L"))
        leftJoinDf %>% filter(str_detect(categoriesstring, paste0("^", input$choosecategory)))
        #leftJoinDf %>% filter(categoriesstring == input$choosecategory)
    })
    
    review_category <- reactive({
        req(input$choosecategoryreviewer)
        #filter(str_detect(rowname, "^L"))
        leftJoinDf %>% filter(str_detect(categoriesstring, paste0("^", input$choosecategoryreviewer)))
        #leftJoinDf %>% filter(categoriesstring == input$choosecategory)
    })
    
    ##
    
    clicked_product <- reactive({
        if (input$clickorbrush == "Click"){
            data_category() %>% filter(asin %in% nearPoints(average_review_price(), input$plot1_click, addDist = TRUE)$asin)
        }
        else{
            data_category() %>% filter(asin %in% brushedPoints(average_review_price(), input$plot1_brush)$asin)
        }
    })
    
    clicked_reviewer <- reactive({
        if (input$clickorbrushreviewer == "Click"){
            review_category() %>% filter(reviewerID %in% nearPoints(average_review_reviewer_sampled(), input$reviewer1_click, addDist = TRUE)$reviewerID)
        }
        else{
            review_category() %>% filter(reviewerID %in% brushedPoints(average_review_reviewer_sampled(), input$reviewer1_brush)$reviewerID)
        }
    })
    
    ##
    
    average_review <- reactive({
        data_category() %>% group_by(asin) %>% summarise(avr_rating = mean(overall), count=n(), price=mean(price))
    })
    
    average_review_by_reviewer <- reactive({
        review_category() %>% group_by(reviewerID) %>% summarise(avr_rating = mean(overall), count=n(), price=mean(price))
    })
    
    ##
    
    n_data <- reactive({nrow(average_review())})
    
    n_product <- reactive({nrow(average_review_by_reviewer())})
    
    ##
    
    observe({
        updateSliderInput(session, "sizeslider", max=n_data(), value=n_data()) 
    })
    
    observe({
        updateSliderInput(session, "sizesliderreviewer", max=n_product(), value=n_product())
    })
    
    ##
    
    observe({
        max_price <- max(average_review_sampled()$price, na.rm=TRUE)
        updateSliderInput(session, "pricerange", max=max_price, value=c(0,max_price))
    })
    
    ##
    
    average_review_sampled <- reactive({
        sample_n(average_review(), size = input$sizeslider, replace=TRUE)
    })
    
    average_review_reviewer_sampled <- reactive({
        sample_n(average_review_by_reviewer(), size = input$sizesliderreviewer, replace=TRUE)
    })
    
    average_review_price <- reactive({
        average_review_sampled() %>% filter(price >= input$pricerange[1] & price <= input$pricerange[2])
    })

###########################################################    
########################################################### plots
###########################################################
    
    output$plot1 <- renderPlot({
        #my_breaks = c(0.1, 10, 100, 400, 800)
        req(n_data())
        ggplot(data=average_review_price(), aes(avr_rating, count)) + geom_point(aes(color=price)) +
            scale_color_viridis() + xlab("Average star rating") + ylab("Number of reviews") + 
            labs(title = "Average rating and number of reviews for Amazon products")
    })
    
    output$plot2 <- renderPlot({
        ggplot(data=clicked_product(), aes(overall, fill=factor(overall))) + 
            geom_bar(position = position_stack(reverse = TRUE)) + coord_flip() +
            scale_fill_viridis(discrete=TRUE) + theme_bw() + 
            theme(panel.grid.minor.y=element_blank(),panel.grid.major.y=element_blank()) +
            xlab("Rating") + ylab("Number of ratings") + 
            labs(title = "Ratings breakdown for clicked product. If no plot appears, please select points on previous graph")
        
    })
    
    output$timeseries <- renderPlot({
        ggplot(data=clicked_product(), aes(time, overall)) + geom_point(aes(color=factor(overall)),position = "jitter") +
        scale_color_viridis(discrete=TRUE) + theme_bw() + theme(panel.grid.minor.y=element_blank(),
                                                                panel.grid.major.y=element_blank()) +
            xlab("Date of review") + ylab("Rating")
    })
    
    output$reviewer1 <- renderPlot({
        ggplot(data = average_review_reviewer_sampled(), aes(avr_rating, count)) + geom_point() +
            scale_color_viridis() + labs(xlab="Average star rating given", ylab="Number of products review",
                                         "Average stars and total products rated for each Amazon reviewer")
    })
    
    output$reviewer2 <- renderPlot({
        ggplot(data = clicked_reviewer(), aes(overall, fill=factor(overall))) +
            geom_bar(position = position_stack(reverse = TRUE)) + coord_flip() +
            scale_fill_viridis(discrete = TRUE) + theme_bw() +
            theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) +
            xlab("Rating") + ylab("Number of reviews given") +
            labs(title = "Ratings breakdown for clicked reviewer(s). If no plot appears, please select points on previous graph")
    })
    
    output$timeseriesreviewer <- renderPlot({
        ggplot(data=clicked_reviewer(), aes(time, overall)) + geom_point(aes(color=factor(overall)), position="jitter") +
            scale_color_viridis(discrete=TRUE) + theme_bw() + theme(panel.grid.minor.y=element_blank(),
                                                                    panel.grid.major.y=element_blank()) +
            xlab("Date of review") + ylab("Star rating given")
    })
}

#######################################################################################################################

# Run the application 
shinyApp(ui = ui, server = server)
