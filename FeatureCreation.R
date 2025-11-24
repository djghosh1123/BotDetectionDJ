if (!require('TSA') || !require('forecast') || !require('stringr') || !require('dplyr') || !require('tibble') || !require('tidytext') || !require('scales') ||
    !require('ngram') || !require('tidyr') || !require('cluster') || !require('gridExtra') || !require('caTools') || !require('e1071') || !require('textdata')){ 
  dir.create(path = Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)
  
  install.packages(c("forecast", "stringr", "dplyr", "tibble", "tidytext","scales",
                     "ngram", "tidyr", "cluster","gridExtra","caTools","e1071","textdata"), 
                   lib = Sys.getenv("R_LIBS_USER"), repos=c('https://cloud.r-project.org'))
  
  install.packages(c('leaps','locfit'),repos = c("http://cloud.r-project.org"))
  install.packages(c('https://cran.r-project.org/src/contrib/Archive/TSA/TSA_1.2.1.tar.gz'), repos=NULL, type="source")
  
}


library(forecast)
library(stringr)
library(dplyr)
library(tibble)
library(tidytext)
library(scales)
library(ngram)
library(tidyr)
library(cluster)
library(gridExtra)
library(caTools)
library(e1071)
library(textdata)
library(TSA)
custom_stop_words <- bind_rows(data_frame(word = c("https","rt","t.co"), 
                                          lexicon = c("custom")), 
                               stop_words)




################ Input: Data and  Selected Users to test
########  Data has 4 columns: userID, tweetID, created_at, text

find_features = function(dat, user, time_grain = 6){
  xs = as.vector(dat$userID)
  bot_pd = numeric(length(user))
  fit_bot = matrix(0, length(user), 6)
  sent_bot = numeric(length(user))
  hash_freq_bot = numeric(length(user))
  wrdcount_bot = numeric(length(user))
  uniq_word_bot = numeric(length(user))
  max_oc_bot = numeric(length(user))
  pdgm_shape_bot = matrix(0,length(user),5)
  pdgm_cmp_bot = numeric(length(user))
  wrdct_var_bot = numeric(length(user))
  
  
  for(i in 1:length(user)){
    w = which(xs == user[i])
    b = dat[w,]
    y = b$created_at
    y = as.vector(y)
    w = which(is.na(y)== TRUE)
    if(length(w)>0){
      y =y[-w]
    }
    ############# Period
    
    if(length(y)>0){
      l= as.integer(diff(range(y))/(time_grain*3600))
      if(l>3){
        len_bot= numeric(l)
        len_bot[1]= length(which(y<((min(y)+time_grain*3600))))
        for(j in 2:l){
          g1 = which(y < (min(y)+time_grain*3600*j+1))
          g2= which(y < (min(y)+time_grain*3600*(j-1)))
          len_bot[j]= length(g1)- length(g2)
          #print(j)
        }
        
        
        len_bot = len_bot[(max(0,length(len_bot) - (24/(time_grain))*60)):(length(len_bot))]
        
        if(length(len_bot[len_bot>0])>0){
          p = periodogram(diff(len_bot), plot = FALSE)
          w= which(p$spec== max(p$spec))
          bot_pd[i] = 1/(p$freq[w])
          
          ############  Periodogram Shape
          
          
          
          w = split(1:(length(p$freq)), cut(1:(length(p$freq)),5))
          s= c(max(p$spec[w[[1]]]), max(p$spec[w[[2]]]), max(p$spec[w[[3]]]), max(p$spec[w[[4]]]), 
               max(p$spec[w[[5]]]))
          
          pdgm_shape_bot[i,] = s
          
          
          
          ps = sort(p$spec, decreasing = TRUE)
          pdgm_cmp_bot[i] =  sum(ps[1:min(5,length(ps))])/sum(ps)
          
          ###########  Fit Parameters
          
          
          fit = auto.arima(diff(len_bot), start.p = 0, max.p = 5, start.q = 0, max.q = 5)
          fit_bot[i,] = c(as.numeric(length(fit$coef)), as.numeric(fit$sigma2), 
                          as.numeric(fit$loglik), as.numeric(fit$aic), 
                          sum(fit$coef^2), as.numeric(arimaorder(fit)[2]))
        }
      }
    }
    ###################  Text Analysis
    
    txt_b = as.vector(b$text)
    
    text_df <- data_frame(line = 1:length(txt_b), text = txt_b)
    
    wrds = text_df%>%unnest_tokens(word, text)%>%anti_join(custom_stop_words)
    
    ct = wrds %>%
      inner_join(get_sentiments("bing")) %>%
      anti_join(custom_stop_words) %>% 
      mutate(method = "Bing et al.")
    
    if(dim(ct)[1]!=0){
      
      wp = which(ct[,3] == "positive")
      wn = which(ct[,3] =="negative")
      
      sent_b = NULL
      sent_n = NULL
      if(length(wp)*length(wn) > 0){
        sent_b<- ct %>%
          count(method, index = row_number() %/% 10, sentiment) %>%
          spread(sentiment, n, fill = 0) %>%
          mutate(sentiment = positive - negative)
        
      }else if(length(wp)>0 && length(wn) == 0){
        sent_b<- ct %>%
          count(method, index = row_number() %/% 10, sentiment) %>%
          spread(sentiment, n, fill = 0) %>%
          mutate(sentiment = positive)
      }else if(length(wp)==0 && length(wn)>0){
        sent_b<- ct %>%
          count(method, index = row_number() %/% 10, sentiment) %>%
          spread(sentiment, n, fill = 0) %>%
          mutate(sentiment = -negative)
      }
      
      sn = wrds %>%
        inner_join(get_sentiments("nrc")) %>%
        anti_join(custom_stop_words)
      wn = NULL
      wp = NULL
      if(dim(sn)[1]>0){
        wn = which(sn[,3] == "negative")
        wp = which(sn[,3] == "positive")
      }
      
      
      if(length(wp)*length(wn) > 0){
        sent_n<- sn %>% 
          filter(sentiment %in% c("positive", 
                                  "negative")) %>%
          mutate(method = "NRC") %>%
          count(method, index = row_number() %/% 10, sentiment) %>%
          spread(sentiment, n, fill = 0) %>%
          mutate(sentiment = positive - negative)
      } else if(length(wp)==0 && length(wn) >0){
        sent_n<- sn %>% 
          filter(sentiment %in% c("positive", 
                                  "negative")) %>%
          mutate(method = "NRC") %>%
          count(method, index = row_number() %/% 10, sentiment) %>%
          spread(sentiment, n, fill = 0) %>%
          mutate(sentiment = - negative)
      }else if(length(wp)>0 && length(wn)==0){
        sent_n<- sn %>% 
          filter(sentiment %in% c("positive", 
                                  "negative")) %>%
          mutate(method = "NRC") %>%
          count(method, index = row_number() %/% 10, sentiment) %>%
          spread(sentiment, n, fill = 0) %>%
          mutate(sentiment = positive)
      }
      
      
      
      
      
      afinn <- wrds %>% 
        inner_join(get_sentiments("afinn")) %>% 
        anti_join(custom_stop_words) %>% 
        group_by(index = row_number() %/% 10) %>% 
        summarise(sentiment = sum(value)) %>% 
        mutate(method = "AFINN")
      
      sent_bot[i] = sum(bind_rows(afinn, sent_b, sent_n)[,2])
      
    }
    
    
    ############### Text Length
    
    w = sapply(txt_b, wordcount)
    w = as.vector(w)
    
    wrdcount_bot[i] = mean(w)
    wrdct_var_bot[i] = var(w)
    
    ############## Unique Words Used
    
    wrd_ct  = wrds %>%  count(word, sort = TRUE) 
    
    uniq_word_bot[i] = dim(wrd_ct)[1]/length(txt_b)
    
    
    ############## Maximum Occurence
    
    
    if(dim(wrd_ct)[1]>0){
      max_oc_bot[i] = sum(wrd_ct[1:min(5,dim(wrd_ct)[1]),2])/sum(wrd_ct[,2])
    }
    
    ###############   Hashtags
    
    hsh = NULL
    for(k in 1:length(txt_b)){
      s = str_extract_all(txt_b[k], "#\\S+")[[1]]
      hsh = c(hsh,s)
    }
    
    
    if(length(hsh)>0){
      text_df <- data_frame(line = 1:length(hsh), text = hsh)
      wrds = text_df%>%unnest_tokens(word, text)%>%anti_join(custom_stop_words)
      wrd_ct = wrds%>%count(word, sort=TRUE)
      
      if(dim(wrd_ct)[1]>0){
        hash_freq_bot[i] = sum(wrd_ct[1:min(5,dim(wrd_ct)[1]),2])/sum(wrd_ct[,2])
      }
      
    }
    
    
    print(i)
    
  }
  
  
  features = cbind(bot_pd,fit_bot, sent_bot, hash_freq_bot, wrdcount_bot, wrdct_var_bot, 
                   uniq_word_bot, max_oc_bot, pdgm_shape_bot, pdgm_cmp_bot)
  
  
  nam = c("Period", "Fit Length", "Fit Var", "Fit LogLik", "Fit AIC", "Fit Coef SumSq", "Fit ARIMA ORDER",
          "Sent Score", "Hash_Freq", "Word Count", "Word Count Var", "Unique Words", "Frequency of top words",
          "Pdgm1", "Pdgm2", "Pdgm3", "Pdgm4", "Pdgm5", "Pdgm_Comp")
  
  features = data.frame(features)
  names(features) = nam
  
  return(features)
  
}

