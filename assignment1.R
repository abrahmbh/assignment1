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