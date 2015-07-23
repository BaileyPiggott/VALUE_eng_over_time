
# set up data frames

library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(RColorBrewer)
# load libraries

## data
df = read.csv("VALUE 1-4.csv") # 1st and 4th year scores
df <- df[4:20] #take out ID columns except for Course_sample 
year_2 = read.csv("APSC200 value rubric data 2015.csv", header = TRUE) # 2nd year marks


#1st year means
eng_1_fall <- df %>% 
  filter(Course_sample == 1) 

n_1f <- nrow(eng_1_fall) # sample size

eng_1_fall <- eng_1_fall %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  select(-Course_sample) %>% # remove course_sample column
  cbind(year = 0.9, .) # add year column



eng_1_winter <- df %>% 
  subset(Course_sample == 2) 

n_1w <- nrow(eng_1_winter) #sample size

eng_1_winter <- eng_1_winter %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))%>% 
  select(-Course_sample) %>%
  cbind(year = 1.3, .)


#year 4 means
eng_4 <- df %>% 
  subset(Course_sample == 480) 

n_4 <- nrow(eng_4)

eng_4 <- eng_4 %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))%>% 
  select(-Course_sample) %>%
  cbind(year = 4, .)

# 2nd year means
#2nd year data is in a differently formatted file and the 2 graders have not been averaged
year_2 <- year_2[1:77 , -grep("Change", colnames(year_2))] %>% 
  subset(Discipline != "CHEE")  # take out chem scores and null rows and change columns
  
score_1 <- year_2 %>% select(PS1.1:WC5.1)
score_2 <- year_2 %>% select(PS1.2:WC5.2)
colnames(score_2) <- colnames(score_1) # change column names for row bind

n_2 <- nrow(score_1) #sample size for second year

eng_2 <- bind_rows(score_1, score_2) %>% #second year data
  summarise_each(funs(mean(., na.rm = TRUE))) %>% # take average of each column
  cbind(year = 2, .) # add year column at start


#reorder 2nd year and change names to match year 1 and 4 data for row bind:
eng_2 <- eng_2[c("year", "PS1.1", "PS2.1", "PS3.1", "PS4.1", "PS5.1", "PS6.1", "CT1.1", "CT2.1", "CT3.1", "CT4.1", "CT5.1", "WC1.1", "WC2.1", "WC3.1", "WC4.1", "WC5.1")]
colnames(eng_2) <- colnames(eng_1_fall)



#combine averages into one data frame of means
eng <- bind_rows(eng_1_fall, eng_1_winter, eng_2, eng_4)


#separate by VALUE:
ps <- eng %>% 
  subset(select = year:PS6) %>% 
  gather(learning_outcome, mean, PS1:PS6, na.rm = TRUE)

ct <- eng %>% 
  subset(select = c(year, CT1:CT5)) %>% 
  gather(learning_outcome, mean, CT1:CT5, na.rm = TRUE)

wc <- eng %>% 
  subset(select = c(year, WC1:WC5)) %>% 
  gather(learning_outcome, mean, WC1:WC5, na.rm = TRUE)



# summary:

#take average over all the subcategories for each outcome
ps_avg <- ps %>% spread(learning_outcome, mean) 
ps_avg <- data.frame(year = ps_avg[,1], mean = rowMeans(ps_avg[,-1], na.rm = TRUE))
colnames(ps_avg) <- c("year", "PS")

ct_avg <- ct %>% spread(learning_outcome, mean) 
ct_avg <- data.frame(year = ct_avg[,1], mean = rowMeans(ct_avg[,-1], na.rm = TRUE))
colnames(ct_avg) <- c("year", "CT")

wc_avg <- wc %>% spread(learning_outcome, mean) 
wc_avg <- data.frame(year = wc_avg[,1], mean = rowMeans(wc_avg[,-1], na.rm = TRUE))
colnames(wc_avg) <- c("year", "WC")

#combine averages into one data frame and tidy
summary <- cbind(ps_avg, ct_avg, wc_avg) %>% #combine columns
  subset(select = c("year", "PS", "CT", "WC")) %>% #take out repeated course_sample column
  gather(learning_outcome, mean, PS:WC, na.rm = TRUE) #tidy



# x axis labels with sample sizes
n_3 <- 0  
year1 <- paste0("First Year\nn = ", n_1f, "   n = ", n_1w) #text string for xlabel including sample size    
year2 <- paste0("Second Year\nn = ", n_2) #text string for xlabel   
year3 <- paste0("Third Year\nn = ", n_3) #text string for xlabel    
year4 <- paste0("Fourth Year\nn = ", n_4) #text string for xlabel



