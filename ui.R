
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
library(plotly)
#library(shinyIncubator)

shinyUI(
  fluidPage(theme = shinytheme("cosmo"),#Creating a fluid page to include all terms.
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
    navbarPage(#Command used to creat a page with tabs to navegate.
      title="Decrypting substitution ciphers using MCMC",
      tabPanel(title = "See LIVE decryption",#Creating tab to plot
               sidebarPanel(#Creating side bar panel with the information of what you wish to plot
                 radioButtons(inputId="use_type", label= "What are you interested in?", choices = list("I want to see a demo"="demo","I want to decrypt my own scrambled text"="new_text"), selected = "demo"),
                 conditionalPanel(condition = "input.use_type == 'new_text'",
                                  textInput(inputId = "rand_sent", label="Enter the scrambled text", value = "")),
                                  sliderInput(inputId = 'n_iter',label = "Select the number of total iterations:",min = 10000,max=100000,value = 10000,step = 1000,ticks = TRUE),
                 sliderInput(inputId = 'next_iter',label = "Select the number of iterations in next epoch:",min = 100,max=5000,value = 100,step = 100,ticks = TRUE),
                 actionButton(inputId = 'go',label = "Decrypt"),#Button to plot
                 hr(),
                 "Progress in",
                 "number of MCMC iterations: ",
                 textOutput("iter"),
                 hr(),
                 "Elapsed Time (seconds):",
                 textOutput("elapsed")
                 ),
               mainPanel(
                 fluidRow(
                   column(12, align="center",
                          plotlyOutput('plaus')
                   )
                 ),
                 fluidRow(
                   textOutput("sentence")
                 )
               )
      )
    ) 
  )
)
