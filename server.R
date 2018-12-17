
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)

shinyServer(function(input, output,session) {
  #Requiring packages to plot
  withProgress(message = 'Loading data and required functions', style = 'old', value = 0.1, {
    Sys.sleep(1)
    incProgress(0.4)
    source("shinyfunc.R")
    incProgress(0.5)
    Sys.sleep(1)
    setProgress(1)
  })
  require(grid)

  current_sentence<-reactiveValues(val="blank",iteration=0,f=cbind("code_space"=state_space[sample(1:length(state_space),replace = FALSE)],state_space),log_plaus=c(),epoch_iteration=0)
  
  output$iter <- renderText({
    current_sentence$iteration
  })
  
  output$sentence <- renderText({
    current_sentence$val
  })
  
  output$elapsed<-renderText({
    current_sentence$iteration
    return(round(Sys.time()-start_time,digits = 3))
  })
  
  observe({
    input$go
    if(input$next_iter<=100){
      isolate({
        if(input$use_type=="new_text"){
          old_length<-length(unlist(strsplit(random_sentence,split="")))
          new_length<-length(unlist(strsplit(input$rand_sent,split="")))
          print(old_length)
          print(new_length)
          if(old_length!=new_length){
            current_sentence$log_plaus<-c()
            current_sentence$iteration<-0
            random_sentence<<-input$rand_sent
          }
        }else{
          random_sentence<-demo_sentence
        }
        withProgress(message = paste0("Running next ",input$next_iter," MCMC iterations"), style = 'old', value = 0.1, {
          Sys.sleep(0.5)
          incProgress(0.1)
          f_array<-array(NA_real_,dim=c(length(state_space),2,input$next_iter))
          f_array[,,1]<-current_sentence$f
          accept_count<-0
          for(i in 1:(input$next_iter-1)){
            #Sys.sleep(0.01)
            temp<-MCMC_f(current_f = f_array[,,i],sentence = random_sentence,state_space = state_space,transition_mat = transition_mat)
            #print(i)
            f_array[,,i+1]<-temp[[1]]
            accept_count<-accept_count+temp[[2]]
            current_sentence$log_plaus[current_sentence$iteration+i]<-temp[[3]]
            incProgress(i/(1.2*input$next_iter))
          }
          current_sentence$f<-f_array[,,input$next_iter]
          current_sentence$val<-sentence_decoder(sentence = random_sentence,substitution_cipher_code = current_sentence$f)
          current_sentence$iteration<-current_sentence$iteration+input$next_iter
          setProgress(1)
        })
      })
    }else if(input$next_iter>100){
      isolate({
        epoch_n<-input$next_iter/100
        if(input$use_type=="new_text"){
          old_length<-length(unlist(strsplit(random_sentence,split="")))
          new_length<-length(unlist(strsplit(input$rand_sent,split="")))
          print(old_length)
          print(new_length)
          if(old_length!=new_length){
            current_sentence$log_plaus<-c()
            current_sentence$iteration<-0
            random_sentence<<-input$rand_sent
          }
        }else{
          random_sentence<-demo_sentence
        }
        withProgress(message = paste0("Running next ",input$next_iter," MCMC iterations"), style = 'old', value = 0.1, {
          Sys.sleep(0.5)
          incProgress(0.1)
          f_array<-array(NA_real_,dim=c(length(state_space),2,100))
          f_array[,,1]<-current_sentence$f
          accept_count<-0
          for(i in 1:(100-1)){
            #Sys.sleep(0.01)
            temp<-MCMC_f(current_f = f_array[,,i],sentence = random_sentence,state_space = state_space,transition_mat = transition_mat)
            print(i)
            f_array[,,i+1]<-temp[[1]]
            accept_count<-accept_count+temp[[2]]
            current_sentence$log_plaus[current_sentence$iteration+i]<-temp[[3]]
            incProgress(i/(1.2*100))
          }
          current_sentence$f<-f_array[,,100]
          current_sentence$val<-sentence_decoder(sentence = random_sentence,substitution_cipher_code = current_sentence$f)
          current_sentence$iteration<-current_sentence$iteration+100
          setProgress(1)
          current_sentence$epoch_iteration<-current_sentence$epoch_iteration+1
          print(paste("This is current epoch",current_sentence$epoch_iteration))
        })
      })
      ## Activate this loop for automatizing the printing of iterations output instead of pressing decrypt everytime.
      if (isolate(current_sentence$epoch_iteration) < epoch_n){
        invalidateLater(0, session)
        #print(current_sentence$epoch_iteration)
      }else{
        ## This is to reset the current_sentence$epoch_iteration so as to make it run through all epochs in next "decrypt" session
        current_sentence$epoch_iteration<-0
      }
    }
  })
  
  ## Deactivate this loop for automatizing the printing of iterations output instead of pressing decrypt everytime.
  observe({
    #input$go
    if(input$next_iter<=100){
      if (isolate(current_sentence$iteration) < input$n_iter){
        invalidateLater(0, session)
      }
    }
  })
  
  
  output$plaus <- renderPlotly({
    #withProgress(message = 'Plotting Log plausibility of the cipher key', style = 'old', value = 0.1, {
      #Sys.sleep(1)
      #incProgress(0.4)
      df_log_plaus <- data.frame("Iteration_number"=1:length(current_sentence$log_plaus), "Log_Plausibility"=current_sentence$log_plaus)
      plot_out <- plot_ly(df_log_plaus, x = ~Iteration_number, y = ~Log_Plausibility, name = 'Log_Plausibility', type = 'scatter', mode = 'lines+markers')
      #incProgress(0.4)
      #Sys.sleep(1)
      #setProgress(1)
    #})
    plot_out
  })
})

