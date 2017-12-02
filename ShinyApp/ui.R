library(shiny)
library(shinydashboard)
#library(DT)

# Define UI for dataset viewer app ----
ui <- fluidPage(
        # App title ----
        titlePanel("Text Prediction"),
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                        width = 230,
                        #        sidebarMenu(
                        #                menuItem("Graphs", tabName = "graphs"),
                        #                menuItem("Raw Data", tabName = "data"),
                        #                menuItem("Help", tabName = "help"),
                        #                menuItem("Credits", tabName = "credits")
                        #        ),
                        # Chose your x_var
                        box(width = 12,
                            title = "Parameters",
                            solidHeader = TRUE,
                            status = "primary",
                            
                            # Sidebar panel for inputs
                            sidebarPanel(
                                    
                                    # Input: Slider for the number of words to predict
                                    sliderInput(inputId = "nb_words_to_predict",
                                                label = "Number of words to predict:",
                                                min = 1,
                                                max = 10,
                                                value = 3),
                                    
                                    # Input: Text captured by user
                                    # Note: Changes made to this textInput control
                                    # are updated in the output area immediately as you type
                                    textInput(inputId = "text",
                                              label = "Text:",
                                              value = "this is a test")
                            ))
                ),
                # Main panel for displaying outputs
                mainPanel(
                        width = 230,
                        #                tags$head(tags$style(HTML('.skin-blue .main-header .logo {
                        #                                   #font-family: "Georgia", Times, "Times New Roman", serif;
                        #                                   #font-weight: bold;
                        #                                   font-size: 20px;
                        #                                   background-color: #3c8dbc;}
                        #                                   .skin-blue .main-header .logo:hover {
                        #                                   background-color: #3c8dbc;}
                        #                                   .radio label:first-child {
                        #                                   #font-weight: bold;
                        #                                   #font-size: 20px;
                        #                                   color: #3c8dbc;}
                        #.box.box-solid.box-primary>.box-header {
                        #color:#fff;
                        #background:#666666}
                        #.box.box-solid.box-primary{
                        #border-bottom-color:#666666;
                        #border-left-color:#666666;
                        #border-right-color:#666666;
                        #border-top-color:#666666;}
                        #                                  '))),
                        
                        # Output: Formatted text for prediction
#                        h3(textOutput("captured_text", container = span)),
                        
                        # Output slope
                        box(width = 3,
                            height = 80,
                            solidHeader = FALSE,
                            
                            h4("Best predicted word:")
                        ),
                        # Output slope
                        box(width = 9,
                            height = 80,
                            solidHeader = FALSE,
                            
                            # Output: Formatted text for prediction
                            h4(textOutput("best_word", container = span))
                        ),
                        
                        # Output: HTML table with predicted words
                        #         based on the captured text
                        #                        verbatimTextOutput("predicted_words_print"),
                        
                        # Output slope
                        box(width = 3,
                            height = 80,
                            solidHeader = FALSE,
                            
                            h4("Other predictions:")
                        ),
                        # Output slope
                        box(width = 9,
                            height = 80,
                            solidHeader = FALSE,
                            
                            # Output: HTML table with predicted words
                            #         based on the captured text
                            h4(tableOutput("predicted_words_table")))
                            #tableOutput("predicted_words_table"))
                        ))
)