#Manipulation check is designed for categorizations1_data.csv. It makes sure that every participant
#used the correct pronoun three times. 

manipulation_check <- function(d){
  #manipulation check
  conditions <- c("hen1", "hen2", "hen3", "han1", "han2", "han3", "hon1", "hon2", "hon3", "control1", "control2", "control3")

  #First, create a column called condition in d with the first three letters in each of
  #the three sentences
  d_conditions <- apply( d[,conditions], 2 , substr, start = 1, stop = 3 ) 
  d_conditions <-  t(apply(d_conditions, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))) %>%
    data.frame() %>% select(1:3)
  d$condition_tmp <- paste(d_conditions[,1], d_conditions[,2], d_conditions[,3], sep = "") %>%
    tolower()
  
  #then check if they used the same pronoun in the all three sentencs AND if they weren't
  #in the control condition. This involves creating a function within the function, doublecheck.
  d$completed <- d$condition_tmp == "henhenhen"|d$condition_tmp == "hanhanhan"| d$condition_tmp == "honhonhon"
  doublecheck <- function(x){
    ifelse(d$completed == FALSE, grepl(x, d$condition_tmp), F) 
  }
  doublechecks <- doublecheck("han") == T & doublecheck("hon") == T & doublecheck("hen") == T
  d$condition <- ifelse(d$completed == F, "control", d$condition_tmp) %>% 
    ifelse(doublechecks == T, NA, .) %>% 
    recode(., "henhenhen" = "hen", "honhonhon" = "hon", "hanhanhan" = "han")
  
  for (i in c("hen", "na", "han", "hon")){
    d$condition <- ifelse(d$completed == F & grepl(i, d$condition_tmp) == T, NA, d$condition ) 
  }
  return(d)
}

#standardize a variable
standardize <- function(x){
  (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
}


