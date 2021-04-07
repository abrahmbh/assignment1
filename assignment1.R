pollutantmean <- function(directory,pollutant,id=1:332){
  # this code is heavily inspired from https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-discussPollutantmean.md
  
  #setting the working directory
  #setwd()
  
  # files list
  files_list <- list.files(directory)
  
  #empty data frame
  my_data <- c()
  
  #adding data to my_data
  for(i in id){
    #temp data to read csv files
    temp_data <- read.csv(paste(directory, files_list[i], sep = "/"))
    #subsetting relevant data
    temp_data <- temp_data[, pollutant]
    my_data <- c(my_data,temp_data)
  }
  # Calculate mean(while ignoring na vals) and return it to parent environment
  mean_results <- mean(my_data, na.rm= TRUE)
  mean_results
}

complete <- function(directory, id = 1:332){
  #files list
  files_list <- list.files(directory)
  nobs <- c()
  for(i in id){
    temp_data <- read.csv(paste(directory, files_list[i], sep = "/"))
    nitrate <- temp_data[, "nitrate"]
    #gives the list of logical, telling how many are na. where na=false
    good_n <- complete.cases(nitrate)
    sulfate <- temp_data[, "sulfate"]
    #gives the list of logical, telling how many are na. where na=false
    good_s <- complete.cases(sulfate)
    #total good = good_s and good_n
    t_good <- good_s & good_n
    t_good <- t_good[t_good]
    nobs<-c(nobs,length(t_good))
  }
  final_df <- data.frame(id, nobs)
  final_df
}

corr <- function(directory, threshold=0){
  cr <- c()
  # getting complete data information
  thres_data <- complete(directory)
  thres_data <- thres_data$id[thres_data$nobs>threshold]
  #only find the correlation if we meet the threshold 
  if(length(thres_data)>0){
    files_list <- list.files(directory)
    #empty dataframe for now. will bind with sensor data
    cor_df <- data.frame()
    for(i in thres_data){
      temp_data <- read.csv(paste(directory, files_list[i], sep = "/"))
      temp_sulf <- temp_data[, "sulfate"]
      temp_nitr <- temp_data[, "nitrate"]
      good <- complete.cases(temp_sulf, temp_nitr)
      sulfate <- temp_sulf[good]
      nitrate <-  temp_nitr[good]
      cr <- c(cr, cor(sulfate, nitrate))
    }
    
  }else{
    cr <- c(0)
  }
  #returning cor data
  cr
}