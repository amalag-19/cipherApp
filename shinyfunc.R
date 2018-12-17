#################################################################################################
start_time <- Sys.time()
load(file = "state_space.RData")
load(file = "transition_mat.RData")
demo_sentence<-"\"If we are ever able to learn what Wickham's debts have been,\" said Elizabeth, \"and how much is settled on his side on our sister, we shall exactly know what Mr. Gardiner has done for them, because Wickham has not sixpence of his own. The kindness of my uncle and aunt can never be requited. Their taking her home, and affording her their personal protection and countenance, is such a sacrifice to her advantage, as years of gratitude cannot enough acknowledge. By this time she is actually with them! If such goodness does not make her miserable now, she will never deserve to be happy! What a meeting for her, when she first sees my aunt!\" \"We must endeavour to forget all that has passed on either side,\" said Jane: \"I hope and trust they will yet be happy. His consenting to marry her is a proof, I will believe, that he is come to a right way of thinking. Their mutual affection will steady them; and I flatter myself they will settle so quietly, and live in so rational a manner, as may in time make their past imprudence forgotten.\" \"Their conduct has been such,\" replied Elizabeth, \"as neither you, nor I, nor any body, can ever forget. It is useless to talk of it.\" It now occurred to the girls that their mother was in all likelihood perfectly ignorant of what had happened. They went to the library, therefore, and asked their father, whether he would not wish them to make it known to her. He was writing, and, without raising his head, coolly replied, \"Just as you please.\" \"May we take my uncle's letter to read to her?\" \"Take whatever you like, and get away.\" Elizabeth took the letter from his writing table, and they went up stairs together. Mary and Kitty were both with Mrs. Bennet: one communication would, therefore, do for all. After a slight preparation for good news, the letter was read aloud. Mrs. Bennet could hardly contain herself. As soon as Jane had read Mr. Gardiner's hope of Lydia's being soon married, her joy burst forth, and every following sentence added to its exuberance. She was now in an irritation as violent from delight, as she had ever been fidgetty from alarm and vexation. To know that her daughter would be married was enough. She was disturbed by no fear for her felicity, nor humbled by any remembrance of her misconduct."

#random_sentence<-"\"Their conduct has been such,\" replied Elizabeth, \"as neither you, nor I, nor any body, can ever forget. It is useless to talk of it.\" It now occurred to the girls that their mother was in all likelihood perfectly ignorant of what had happened."
#################################################################################################
## Defining a function to code a sentence using a randomly generated substitution cipher; returns the coded sentence and the cipher code as a list
sentence_jumbler<-function(sentence,state_space){
  sentence_split<-unlist(strsplit(x = sentence,split = ""))
  substitution_cipher_code_index<-sample(x = 1:length(state_space),replace = FALSE)
  
  substitution_cipher_code_function<-cbind("code_space"=state_space[substitution_cipher_code_index],state_space)
  
  sentence_jumbled_split<-rep(NA_character_,length(sentence_split))
  for (i in 1:length(sentence_split)){
    state_space_id<-which(state_space==sentence_split[i])
    sentence_jumbled_split[i]<-substitution_cipher_code_function[state_space_id,1]
    #print(i)
  }
  sentence_jumbled<-paste(sentence_jumbled_split,sep = "",collapse = "")
  return(list(sentence_jumbled,substitution_cipher_code_function))
}

## Defining a function to decode a sentence using given cipher; returns the decoded sentence
sentence_decoder<-function(sentence,substitution_cipher_code_function){
  sentence_split<-unlist(strsplit(x = sentence,split = ""))
  sentence_decoded_split<-rep(NA_character_,length(sentence_split))
  for (i in 1:length(sentence_split)){
    substitution_id<-which(substitution_cipher_code_function[,1]==sentence_split[i])
    sentence_decoded_split[i]<-substitution_cipher_code_function[substitution_id,2]
  }
  sentence_decoded<-paste(sentence_decoded_split,sep = "",collapse = "")
  return(sentence_decoded)
}

#################################################################################################
## Decoding by MCMC
log_plausibility_cal<-function(sentence,f,state_space,transition_mat){
  sentence_split<-unlist(strsplit(x = sentence,split = ""))
  plausibility_vec<-rep(NA_real_,length(sentence_split)-1)
  for (i in 1:(length(sentence_split)-1)){
    si<-which(f[,1]==sentence_split[i])
    si1<-which(f[,1]==sentence_split[i+1])
    plausibility_vec[i]<-transition_mat[si,si1]
  }
  # plausibility_vec<-apply(X = matrix(1:(length(sentence_split)-1)),MARGIN = 1,FUN = function(x){
  #   si<-which(f[,1]==sentence_split[x])
  #   si1<-which(f[,1]==sentence_split[x+1])
  #   return(transition_mat[si,si1])
  # })
  log_plausibility_out<-sum(log(plausibility_vec))
  return(log_plausibility_out)
}

MCMC_f<-function(current_f,sentence,state_space,transition_mat){
  log_plaus_f<-log_plausibility_cal(sentence = sentence,f = current_f,state_space = state_space,transition_mat = transition_mat)
  ## random tansposition for moving from f to f_star
  random_transposition_ids<-sample(x = 1:length(state_space),size = 2,replace = FALSE)
  f_star<-current_f
  f_star[random_transposition_ids[1],1]<-current_f[random_transposition_ids[2],1]
  f_star[random_transposition_ids[2],1]<-current_f[random_transposition_ids[1],1]
  log_plaus_f_star<-log_plausibility_cal(sentence = sentence,f = f_star,state_space = state_space,transition_mat = transition_mat)
  if(log_plaus_f_star>log_plaus_f){
    next_f<-f_star
    accept_flag<-1
    log_plaus_out<-log_plaus_f_star
  }else if(log_plaus_f_star<=log_plaus_f){
    probab<-exp((log_plaus_f_star-log_plaus_f))
    #print(probab)
    coin_flip<-rbinom(n = 1,size = 1,prob = probab)
    if(coin_flip==1){
      next_f<-f_star
      accept_flag<-1
      log_plaus_out<-log_plaus_f_star
    }else if(coin_flip==0){
      next_f<-current_f
      accept_flag<-0
      log_plaus_out<-log_plaus_f
    }
  }
  return(list(next_f,accept_flag,log_plaus_out))
}

sampler<-function(n,start_f,true_f,sentence_coded,state_space,transition_mat){
  f_array<-array(NA_real_,dim=c(length(state_space),2,n))
  #hamming_dist<-rep(NA_real_,n)
  ## Defining the initial value for the chain
  f_array[,,1]<-start_f
  accept_count<-0
  #hamming_dist[1]<-hamming.distance(x = sentence_coded,y = sentence_iterate)
  ## loop for RWM updates
  for(i in 1:(n-1)){
    temp<-MCMC_f(current_f = f_array[,,i],sentence = sentence_coded,state_space = state_space,transition_mat = transition_mat)
    f_array[,,i+1]<-temp[[1]]
    accept_count<-accept_count+temp[[2]]
    sentence_iterate<-sentence_decoder(sentence = sentence_coded,substitution_cipher_code = f_array[,,i+1])
    #hamming_dist[i+1]<-hamming.distance(x = sentence_coded,y = sentence_iterate)
    if(prod(f_array[,,i+1]==true_f)==1){
      print("Finally I won the game")
      print(sentence_decoder(sentence = sentence_coded,substitution_cipher_code = f_array[,,i+1]))
    }
    if(i%%1000==0){
      print(sentence_iterate)
      #print(hamming_dist[i])
    }
  }
  return(list(f_array,accept_count))
}

#################################################################################################
#n<-50000
#start_f<-cbind("code_space"=state_space[sample(1:length(state_space),replace = FALSE)],state_space)

#undebug(sampler)
#result<-sampler(n = n,start_f = start_f,true_f = temp[[2]],sentence_coded = temp[[1]],state_space = state_space,transition_mat = transition_mat)


