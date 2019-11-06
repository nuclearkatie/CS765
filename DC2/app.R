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
                                  "R&B" = "CDs & Vinyl,R&B"
                                  ),
                        selected="CDs & Vinyl,Pop"),
            sliderInput("sizeslider", "Number of reviews to show (randomly sampled):",
                        min = 1, max = 30993,
                        value = 15496),
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
            h4("Points near click/brush"),
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
    )
    )
)

#######################################################################################################################

# Define server logic required to draw a histogram
server <- function(input, output, session) {

########################################################### clicks
    
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
    
    #output$brush_info <- renderPrint({
    #    brushedPoints(average_review_sampled(), input$plot1_brush)$asin
    #})
    
    output$printtitle <- renderText({
        if (input$clickorbrush == "Click"){
            top_reviewed_product()$title[1]
        }
        else{
            top_reviewed_product()$title[1]
        }
    })
    
    output$printdescription <- renderText({
        if (input$clickorbrush == "Click"){
            top_reviewed_product()$description[1]
        }
        else{
            top_reviewed_product()$description[1]
        }
    })
    
    output$pricedescription <- renderText({
        if (input$clickorbrush == "Click"){
            paste0("$", top_reviewed_product()$price[1])
        }
        else{
            paste0("$", top_reviewed_product()$price[1])
        }
    })
    
########################################################### reactive data
    
    data_category <- reactive({
        req(input$choosecategory)
        #filter(str_detect(rowname, "^L"))
        leftJoinDf %>% filter(str_detect(categoriesstring, paste0("^", input$choosecategory)))
        #leftJoinDf %>% filter(categoriesstring == input$choosecategory)
    })
    
    
    top_reviewed_product <- reactive({
        if (input$clickorbrush == "Click"){
            data_category() %>% filter(asin %in% nearPoints(average_review(), input$plot1_click, addDist = TRUE)$asin)
        }
        else{
            data_category() %>% filter(asin %in% brushedPoints(average_review_sampled(), input$plot1_brush)$asin)
            
        }
    })
    
    average_review <- reactive({
        data_category() %>% group_by(asin) %>% summarise(avr_rating = mean(overall), count=n(), price=mean(price))
    })
    
    n_data <- reactive({nrow(average_review())})
    
    observe({
        updateSliderInput(session, "sizeslider", max=n_data(), value=n_data()/2) 
    })
    
    observe({
        max_price <- max(average_review_sampled()$price, na.rm=TRUE)
        updateSliderInput(session, "pricerange", max=max_price, value=c(0,max_price))
    })
    
    average_review_sampled <- reactive({
        sample_n(average_review(), size = input$sizeslider, replace=TRUE)
    })
    
    average_review_price <- reactive({
        average_review_sampled() %>% filter(price >= input$pricerange[1] & price <= input$pricerange[2])
    })
    
########################################################### plots
    
    output$plot2 <- renderPlot({
        ggplot(data=top_reviewed_product(), aes(overall, fill=factor(overall))) + 
            geom_bar(position = position_stack(reverse = TRUE)) + coord_flip() +
            scale_fill_viridis(discrete=TRUE) + theme_bw() + 
            theme(panel.grid.minor.y=element_blank(),panel.grid.major.y=element_blank())
    })
    
    output$plot1 <- renderPlot({
        #my_breaks = c(0.1, 10, 100, 400, 800)
        req(n_data())
        ggplot(data=average_review_price(), aes(avr_rating, count)) + geom_point(aes(color=price)) +
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
