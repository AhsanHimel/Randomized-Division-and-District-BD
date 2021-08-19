# Loading packages ####
library(tidyverse)
library(rvest)

# Fetching data ####
content <- read_html("https://en.wikipedia.org/wiki/Districts_of_Bangladesh")
tables <- content %>% html_table(fill = TRUE)
dist_table <- tables[[2]][,-2]
dist_table <- separate(data = dist_table, col = District,
                       into =  c("District", "Status"), sep = "\\s(\\w+)$")
districts <- c(dist_table$District)
divisions <- unique(dist_table$Division)


# Randomizing

RandLocation <- function(division_no,district_no, seed){
  # setting seed if given
  if (!missing(seed)) set.seed(seed)
  
  number_of_divi = division_no
  number_of_dist = district_no
  
  # randomly assigning districts to each divisions from the table
  rand <- by(dist_table$District, 
             dist_table$Division, 
             sample, size = number_of_dist, replace = F)
  
  for(i in sample(divisions, size = number_of_divi)){
    # selecting divisions
    cat("From",i,":\n\t\t", rand[[i]], end = "\n")
  }
}

RandLocation(division_no = 4, district_no = 2)


# Randomizing

RandLocation <- function(division_no,district_no, seed){
  # setting seed if given
  if (!missing(seed)) set.seed(seed)
  
  number_of_divi = division_no
  number_of_dist = district_no
  
  # randomly assigning districts to each divisions from the table
  rand <- by(dist_table$District, 
             dist_table$Division, 
             sample, size = number_of_dist, replace = F)
  
  tab <- data.frame(Division = character(0),
                    Districts = character(0))
  
  row <- 0
  for(i in sample(divisions, size = number_of_divi)){
    # selecting divisions
    # cat("From",i,":\n\t\t", rand[[i]], end = "\n")
    tab[row+1,1] <- i
    tab[row+1,2] <- paste(rand[[i]], collapse = ", ")
    row <- row + 1
  }
  return(tab)
}

RandLocation(division_no = 4, district_no = 4)

