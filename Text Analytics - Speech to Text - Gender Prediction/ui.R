#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

### Dashboard Page Setup---------------------------------------------------------------------
dashboardPage(
    dashboardHeader(
        title="Male or Female: A Speech Text Analysis",
        titleWidth = 450),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Histogram", tabName="tab_hist", icon=icon("signal")),
            menuItem("Bigram Net", tabName="tab_bigram", icon=icon("refresh")),
            menuItem("Sentiments", tabName="tab_sent", icon=icon("cloud")),
            menuItem("TF-IDF Analysis", tabName="tab_tfidf", icon=icon("link")),
            menuItem("Predictive Sandbox", tabName="tab_pred", icon=icon("info")),
            menuItem("The Team", tabName="tab_about", icon=icon("male"))
        )
    ),
    
    ### Dashboard Body---------------------------------------------------------------------
    dashboardBody(
        #Add static infoboxes
        #infoBox("Respondents", 10*2, icon=icon("users"), color="purple"),
        #infoBox("Male", 10*2, icon=icon("mars"), color="blue"),
        #infoBox("Female", 10*2, icon=icon("venus"), color="red"),
        
        tabItems(
            #Histogram --------------------------------------------------------------------
            tabItem("tab_hist",
                    fluidRow(
                        #Add static infoboxes
                        infoBox("Respondents", 48, icon=icon("users"), color="purple"),
                        infoBox("Male", 23, icon=icon("mars"), color="blue"),
                        infoBox("Female", 25, icon=icon("venus"), color="red"),
                        tabBox(
                            width = 12,
                            tabPanel(
                                status = "primary",
                                title = "Histogram",
                                plotOutput("words_hist")
                            )),
                        box(sliderInput("hist_n",
                                        "Words by Rank",
                                        min = 1,
                                        max = 10,
                                        value = 5),
                            selectInput("h_gender", "Gender", choices = c('Both'= 0, 'Man'=1,'Woman'=2)),
                            selectInput("h_question", "Question Number:", choices = c('All'=0,
                                                                                      '1: How/Where do you watch movies?'=1,
                                                                                      '2: What do you do with your free time?'=2,
                                                                                      '3: Which kind of sports do you play? Why?'=3,
                                                                                      '4: What is your favorite food?'=4,
                                                                                      '5: What is your spirit animal? Why?'=5,
                                                                                      '6: What do you do when you see a cockroach?'=6,
                                                                                      '7: What was your pet\'s name? How did you name it?'=7)))
                    )),
            
            #Bigram ------------------------------------------------------------------------
            tabItem("tab_bigram",
                    fluidRow(
                        #Add static infoboxes
                        infoBox("Respondents", 48, icon=icon("users"), color="purple"),
                        infoBox("Male", 23, icon=icon("mars"), color="blue"),
                        infoBox("Female", 25, icon=icon("venus"), color="red"),
                        tabBox(
                            width = 12,
                            tabPanel(
                                status = "primary",
                                title = "Bigram Net",
                                plotOutput("outbigram")
                            )),
                        box(
                            selectInput("b_gender", "Gender", choices = c('Both'= 0, 'Man'=1,'Woman'=2)),
                            selectInput("b_question", "Question Number:", choices = c('All'=0,
                                                                                      '1: How/Where do you watch movies?'=1,
                                                                                      '2: What do you do with your free time?'=2,
                                                                                      '3: Which kind of sports do you play? Why?'=3,
                                                                                      '4: What is your favorite food?'=4,
                                                                                      '5: What is your spirit animal? Why?'=5,
                                                                                      '6: What do you do when you see a cockroach?'=6,
                                                                                      '7: What was your pet\'s name? How did you name it?'=7)))
                    )),
            
            #Sentiments -------------------------------------------------------------------------
            tabItem("tab_sent",
                    fluidRow(
                        #Add static infoboxes
                        infoBox("Respondents", 48, icon=icon("users"), color="purple"),
                        infoBox("Male", 23, icon=icon("mars"), color="blue"),
                        infoBox("Female", 25, icon=icon("venus"), color="red"),
                        tabBox(
                            width = 12,
                            tabPanel(
                                status = "primary",
                                title = "Sentiments",
                                plotOutput("sentiment_question", height = '1000px')
                            )),
                        box(
                            selectInput("s_gender", "Gender", choices = c('Both'= 0, 'Man'=1,'Woman'=2)),
                            selectInput("s_question", "Question Number:", choices = c('All'=0,
                                                                                      '1: How/Where do you watch movies?'=1,
                                                                                      '2: What do you do with your free time?'=2,
                                                                                      '3: Which kind of sports do you play? Why?'=3,
                                                                                      '4: What is your favorite food?'=4,
                                                                                      '5: What is your spirit animal? Why?'=5,
                                                                                      '6: What do you do when you see a cockroach?'=6,
                                                                                      '7: What was your pet\'s name? How did you name it?'=7)))
                    )),
            
            #TF-IDF ---------------------------------------------------------------------------
            tabItem("tab_tfidf",
                    fluidRow(
                        #Add static infoboxes
                        infoBox("Respondents", 48, icon=icon("users"), color="purple"),
                        infoBox("Male", 23, icon=icon("mars"), color="blue"),
                        infoBox("Female", 25, icon=icon("venus"), color="red"),
                        tabBox(
                            width = 12,
                            tabPanel(
                                status = "primary",
                                title = "TF-IDF Analysis",
                                plotOutput("outtf_idf")
                            )),
                        box(sliderInput("tf_idf",
                                        "Word Ranking",
                                        min = 1,
                                        max = 10,
                                        value = 5),
                            selectInput("t_question", "Question Number:", choices = c('All'=0,
                                                                                      '1: How/Where do you watch movies?'=1,
                                                                                      '2: What do you do with your free time?'=2,
                                                                                      '3: Which kind of sports do you play? Why?'=3,
                                                                                      '4: What is your favorite food?'=4,
                                                                                      '5: What is your spirit animal? Why?'=5,
                                                                                      '6: What do you do when you see a cockroach?'=6,
                                                                                      '7: What was your pet\'s name? How did you name it?'=7)))
            )),
        tabItem("tab_pred",
                fluidRow(
                    box(textInput('answer1',label=h5('Question 1: How/Where do you watch movies?'),  width = "50%"),
                    textInput('answer2',label=h5('Question 2: What do you do with your free time?'),  width = "50%"),
                    textInput('answer3',label=h5('Question 3: Which kind of sports do you play? Why?'),  width = "50%"),
                    textInput('answer4',label=h5('Question 4: What is your favorite food?'),  width = "50%"),
                    textInput('answer5',label=h5('Question 5: What is your spirit animal? Why?'),  width = "50%"),
                    textInput('answer6',label=h5('Question 6: What do you do when you see a cockroach?'),  width = "50%"),
                    textInput('answer7',label=h5('Question 7: What was your pet\'s name? How did you name it?'),  width = "50%"),
                    actionButton('action',label='Submit'),
                    verbatimTextOutput('outModel') 
                ))
        ),    
        tabItem("tab_about",
                fluidRow(
                    box(
                        tags$p(
                            class = "text-center",
                            tags$img(class = "img-circle", src = "rebeca.png", style = "max-width: 150px;")
                        ),
                        tags$p(
                            class = "text-center",
                            tags$strong("Rebeca Martinez"),
                            HTML(paste0("(", tags$a(href = "https://www.linkedin.com/in/rebeca-martinezch/", "LinkedIn"), ")"))
                        )
                    ),
                    box(
                        tags$p(
                            class = "text-center",
                            tags$img(class = "img-circle", src = "francisco.png", style = "max-width: 150px;")
                        ),
                        tags$p(
                            class = "text-center",
                            tags$strong("Francisco Luna"),
                            HTML(paste0("(", tags$a(href = "https://www.linkedin.com/in/francisco-luna-tomas/", "LinkedIn"), ")"))
                        )
                    ),
                    box(
                        tags$p(
                            class = "text-center",
                            tags$img(class = "img-circle", src = "mau.png", style = "max-width: 150px;")
                        ),
                        tags$p(
                            class = "text-center",
                            tags$strong("Mauricio Teles"),
                            HTML(paste0("(", tags$a(href = "https://www.linkedin.com/in/mauricio-marcon-teles/", "LinkedIn"), ")"))
                        )
                    ),
                    box(
                        tags$p(
                            class = "text-center",
                            tags$img(class = "img-circle", src = "chandan.png", style = "max-width: 150px;")
                        ),
                        tags$p(
                            class = "text-center",
                            tags$strong("Chandan Bhandari"),
                            HTML(paste0("(", tags$a(href = "https://www.linkedin.com/in/chandanbhandari/", "LinkedIn"), ")"))
                        )
                    ),
                    box(
                        tags$p(
                            class = "text-center",
                            tags$img(class = "img-circle", src = "shar.png", style = "max-width: 150px;")
                        ),
                        tags$p(
                            class = "text-center",
                            tags$strong("Sharmaine Aguilar"),
                            HTML(paste0("(", tags$a(href = "https://www.linkedin.com/in/sharaguilar/", "LinkedIn"), ")"))
                        )
                    ),
                    box(
                        tags$p(
                            class = "text-center",
                            tags$img(class = "img-circle", src = "elmir.png", style = "max-width: 150px;")
                        ),
                        tags$p(
                            class = "text-center",
                            tags$strong("Elmir Alyrzaev"),
                            HTML(paste0("(", tags$a(href = "https://www.linkedin.com/in/ealyrzaev/", "LinkedIn"), ")"))
                        )
                    )
                ))
        )
    )
)
