
library(tidyverse)
library(readxl)
Pull_1 <- read.csv("PULL_1_OG.csv",stringsAsFactors = FALSE)
Pull_2 <- read.csv("PULL2.csv")



Pull_1$RETX <- as.numeric(Pull_1$RETX)

Pull_1_edited <- Pull_1 %>% 
  drop_na(RETX)



#at first we had 166 NAS
##first deleted observations with no COMNAM
#Now we have 159, and we just deleted them
#Second removed 159 NAs, or all the returns with NAs

Pull_2_edited <- Pull_2 %>% 
  filter(COMNAM != "") %>% 
  filter(RETX != "") %>%
  mutate(RETX = as.numeric(RETX))

#Pull 2 edited is fixed and finished

Full1 <- rbind(Pull_2_edited,Pull_1_edited)


COMNAM_PERMNO <- Full1 %>%
  distinct(PERMNO,COMNAM)

x <- unique(Full1$PERMNO)

library(lubridate)

Correct_dates <- Full1 %>%
  mutate(date = mdy(date)) %>% 
  mutate(year = as.numeric(substr(date,1,4))) %>% 
  mutate(month = as.numeric(substr(date,6,7))) %>% 
  mutate(day = as.numeric(substr(date,9,10))) %>% 
  mutate(year = ifelse(year > 2020, year - 100,year)) %>%
  mutate(new_date = paste(month,day,year,sep ="/")) %>%
  mutate(date = mdy(new_date)) %>% 
  select(-new_date,-year,-month,-day)

class(Correct_dates$date)


Dumby_Variable_Creator <- function(Code,date_in,date_out) {
  Codes <- data.frame(subset(Correct_dates, PERMNO == Code))
  for (i in 1:length(date_in)) {
    Codes[,paste("in_or_out",i)] <- ifelse(Codes$date >= date_in[i] & Codes$date <= date_out[i],1,0)
  }
  return(Codes)
}

Goldman_data_frame <- Dumby_Variable_Creator(86868,c("2013-09-23"),c("2019-12-31"))




worthy_data_frame <- Goldman_data_frame %>%
  bind_rows(Dumby_Variable_Creator(17830, c("1930-07-18","1933-08-15","1939-03-04"),c("1932-05-26","1934-08-13","2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(10107,c("1999-11-01"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(22592,c("1976-08-09"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(59176,c("1982-08-30"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(14593,c("2015-03-19"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(19561,c("1987-03-12"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(18542,c("1991-05-06"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(14541,c("1930-07-18","2008-02-19"),c("1999-10-31","2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(76076, c("2009-06-08"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(11308,c("1926-05-26","1987-03-12"),c("1935-11-20","2019-12-31"))) %>%
  bind_rows(Dumby_Variable_Creator(11703, c("1935-11-20"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(11850, c("1928-10-01"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(66181, c("1999-11-01"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(59328, c("1999-11-01"),c("2019-12-31"))) %>%
  bind_rows(Dumby_Variable_Creator(12490, c("1932-05-26","1979-06-29"),c("1939-03-03","2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(22111, c("1999-11-01"),c("2019-12-31"))) %>%
  bind_rows(Dumby_Variable_Creator(48071, c("1991-05-06"), c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(43449, c("1985-10-30"), c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(22752, c("1979-06-29"), c("2019-12-31"))) %>%
  bind_rows(Dumby_Variable_Creator(57665,c("2013-09-23"),c("2019-12-31"))) %>%
  bind_rows(Dumby_Variable_Creator(21936, c("2004-04-08"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(18163, c("1932-05-26"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(92655,c("2012-09-24"),c("2019-12-31"))) %>%
  bind_rows(Dumby_Variable_Creator(65875, c("2004-04-08"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(92611, c("2013-09-23"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(19502, c("2018-06-26"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(55976, c("1997-03-17"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(26403, c("1991-05-06"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(12060, c("1928-10-01"),c("2019-12-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(24643,c("1959-06-01"),c("2013-09-22"))) %>% 
  bind_rows(Dumby_Variable_Creator(59408,c("2008-02-19"),c("2013-09-22"))) %>%
  bind_rows(Dumby_Variable_Creator(27828,c("1997-03-17"),c("2013-09-22"))) %>% 
  bind_rows(Dumby_Variable_Creator(89006,c("2008-09-22"),c("2012-09-23"))) %>% 
  bind_rows(Dumby_Variable_Creator(12079,c("1928-10-01"),c("2009-06-07"))) %>% 
  bind_rows(Dumby_Variable_Creator(66800,c("2004-04-08"),c("2008-09-21"))) %>% 
  bind_rows(Dumby_Variable_Creator(13901,c("1985-10-30"),c("2008-02-18"))) %>%
  bind_rows(Dumby_Variable_Creator(66800,c("2004-04-08"),c("2008-09-21"))) %>%
  bind_rows(Dumby_Variable_Creator(10145,c("1928-10-01"),c("2009-02-18"))) %>%
  bind_rows(Dumby_Variable_Creator(11754,c("1930-07-18"),c("2004-04-07"))) %>% 
  bind_rows(Dumby_Variable_Creator(21573,c("1956-07-03"),c("2004-04-07"))) %>%
  bind_rows(Dumby_Variable_Creator(16432,c("1930-07-18"),c("1999-10-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(14322,c("1928-10-01"),c("1999-10-31"))) %>%
  bind_rows(Dumby_Variable_Creator(15659, c("1928-10-01"),c("1999-10-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(10786,c("1928-10-01"),c("1997-03-16"))) %>%
  bind_rows(Dumby_Variable_Creator(14736,c("1928-10-01"),c("1997-03-16"))) %>% 
  bind_rows(Dumby_Variable_Creator(15368,c("1928-10-01"),c("1997-03-16"))) %>% 
  bind_rows(Dumby_Variable_Creator(15456,c("1928-10-01"),c("1997-03-16"))) %>%
  bind_rows(Dumby_Variable_Creator(10241,c("1928-10-10"),c("1991-05-05"))) %>%
  bind_rows(Dumby_Variable_Creator(12503,c("1928-10-10"),c("1991-05-05"))) %>%
  bind_rows(Dumby_Variable_Creator(15069,c("1928-10-10"),c("1991-05-05"))) %>%
  bind_rows(Dumby_Variable_Creator(12546,c("1928-10-01"),c("1987-03-11"))) %>% 
  bind_rows(Dumby_Variable_Creator(13661,c("1959-05-31"),c("1987-03-11"))) %>% 
  bind_rows(Dumby_Variable_Creator(10225,c("1928-10-01"),c("1985-10-29"))) %>% 
  bind_rows(Dumby_Variable_Creator(16109,c("1928-10-01"),c("1985-10-29"))) %>%
  bind_rows(Dumby_Variable_Creator(16707,c("1930-01-29"),c("1982-08-30"))) %>%
  bind_rows(Dumby_Variable_Creator(11260,c("1928-10-01"),c("1979-06-28"))) %>%
  bind_rows(Dumby_Variable_Creator(19713,c("1959-06-01"),c("1979-06-28"))) %>%
  bind_rows(Dumby_Variable_Creator(10495,c("1959-06-01"),c("1976-08-08"))) %>% 
  bind_rows(Dumby_Variable_Creator(10364,c("1928-10-01"),c("1959-05-31"))) %>%
  bind_rows(Dumby_Variable_Creator(13354, c("1934-08-13"),c("1959-05-31"))) %>%
  bind_rows(Dumby_Variable_Creator(19019, c("1935-11-20"),c("1959-05-31"))) %>%
  bind_rows(Dumby_Variable_Creator(10989,c("1933-08-15"),c("1959-05-31"))) %>% 
  bind_rows(Dumby_Variable_Creator(13143,c("1932-05-26"),c("1956-07-02"))) %>%
  bind_rows(Dumby_Variable_Creator(10321, c("1928-10-01","1932-05-26"),c("1930-07-17","1939-03-03"))) %>%
  bind_rows(Dumby_Variable_Creator(16571, c("1930-07-18"),c("1935-11-19"))) %>%
  bind_rows(Dumby_Variable_Creator(14592, c("1932-05-26"),c("1933-08-14"))) %>% 
  bind_rows(Dumby_Variable_Creator(18278,c("1932-05-26"),c("1933-08-14"))) %>%
  bind_rows(Dumby_Variable_Creator(12378,c("1930-07-18"),c("1932-05-25"))) %>% 
  bind_rows(Dumby_Variable_Creator(14760,c("1928-10-01"),c("1932-05-26"))) %>%
  bind_rows(Dumby_Variable_Creator(12837,c("1930-07-18"),c("1932-05-25"))) %>% 
  bind_rows(Dumby_Variable_Creator(10233,c("1928-10-01"),c("1932-05-26"))) %>%
  bind_rows(Dumby_Variable_Creator(12941,c("1928-10-01"),c("1932-05-26"))) %>% 
  bind_rows(Dumby_Variable_Creator(14090,c("1928-10-01"),c("1932-05-26"))) %>%
  bind_rows(Dumby_Variable_Creator(13549,c("1928-10-01"),c("1930-01-28"))) %>%
  bind_rows(Dumby_Variable_Creator(16002,c("1928-10-01"),c("1929-01-07"))) %>%
  bind_rows(Dumby_Variable_Creator(10604,c("1928-10-01"),c("1930-07-17"))) %>% 
  bind_rows(Dumby_Variable_Creator(12140,c("1928-10-01"),c("1930-07-17"))) %>% 
  bind_rows(Dumby_Variable_Creator(18227,c("1928-10-01"),c("1930-07-17"))) %>% 
  bind_rows(Dumby_Variable_Creator(12095,c("1928-10-01"),c("1930-07-17")))


worthy_data_frame[is.na(worthy_data_frame)] <- 0
worthy_data_frame$in_or_out_total <- worthy_data_frame$`in_or_out 1` + worthy_data_frame$`in_or_out 2` + worthy_data_frame$`in_or_out 3`


sum(is.na(worthy_data_frame$RETX))

Master <- worthy_data_frame %>%
  select(-`in_or_out 1`,-`in_or_out 2`,-`in_or_out 3`) %>% 
  filter(in_or_out_total == 1) %>%
  group_by(date) %>%
  summarise(average= mean(RETX))



Master_2 <- worthy_data_frame %>% 
  left_join(Master, by = "date") %>%
  filter(in_or_out_total == 1) %>% 
  mutate(difference = RETX - average) %>%
  mutate(counter_05_positive = ifelse(difference > .05,1,0)) %>% 
  mutate(counter_10_positive = ifelse(difference >.1,1,0)) %>% 
  mutate(counter_15_positive = ifelse(difference > .15,1,0)) %>% 
  mutate(counter_20_positive = ifelse(difference >.2,1,0)) %>% 
  mutate(counter_05_negative = ifelse(difference < -.05,1,0)) %>% 
  mutate(counter_10_negative = ifelse(difference < -.1,1,0)) %>% 
  mutate(counter_15_negative = ifelse(difference < -.15,1,0)) %>% 
  mutate(counter_20_negative = ifelse(difference < -.2,1,0))
  

#positive returns greater than 5%,10%,15%,and 20%
sum(Master_2$counter_05_positive)
# 3342
sum(Master_2$counter_10_positive)
#378
sum(Master_2$counter_15_positive)
#101
sum(Master_2$counter_20_positive)
#39

sum(Master_2$counter_05_negative)
#2625
sum(Master_2$counter_10_negative)
#299
sum(Master_2$counter_15_negative)
#83
sum(Master_2$counter_20_negative)
#35i have n


#Normal Distribution Calculations

mean(Master_2$RETX)
#0.0002998885

sd(Master_2$RETX)
#0.01862142



distinct_permno <- c()

unique_dates <- c(unique(Master_2$date))

worth <- data.frame()
full_dates <- data.frame()
final_loop <- data.frame()

#way 1
# create a list of all other companies and then calculate remove
# I got it to work for an individual date, but it ends up returning only the relative average of the first one in the subsetted data frame and the last one 


for (i in 1:length(unique_dates)) {
  dates <- subset(Master_2, unique_dates[1] == Master_2$date)
  distinct_permno <- c(unique(dates$PERMNO))
  for (j in 1:length(distinct_permno)){
    if (j==1){
      dates_minus_29 <- subset(dates,distinct_permno[j] == dates$PERMNO)
      dates_minus_1 <- subset(dates,distinct_permno[j] != dates$PERMNO)
      dates_minus_1[,paste("rel_mean",distinct_permno[j])] <- sum(dates_minus_1$RETX)/length(dates_minus_1$RETX)
      dates_minus_29[,paste("rel_mean",distinct_permno[j])] <- 0
      full_dates <- rbind(dates_minus_1,dates_minus_29)
      full_dates_1 <- full_dates %>% clean_names
      } else {
      dates_minus_29_2 <- subset(dates,distinct_permno[j] == dates$PERMNO)
      dates_minus_1_2 <- subset(dates,distinct_permno[j] != dates$PERMNO)
      dates_minus_1_2[,paste("rel_mean",distinct_permno[j])] <- sum(dates_minus_1$RETX)/length(dates_minus_1$RETX)
      dates_minus_29_2[,paste("rel_mean",distinct_permno[j])] <- 0
      full_dates_2 <- rbind(dates_minus_1_2,dates_minus_29_2)
      full_dates_3 <- full_dates_2 %>% clean_names()
      worth <- select(full_dates_3,permno,starts_with('rel_mean'))
      final_loop <- full_dates_1 %>% left_join(worth, by = "permno")
    }
  }
}





#way 2
for (i in 1:length(unique_dates)) {
  dates <- subset(Master_2, unique_dates[1] == Master_2$date)
    for (j in 1:length(distinct_permno)){
      if (distinct_permno[j] == dates$PERMNO){
        dates[,paste("rel_mean",distinct_permno[j])] <- (sum(dates$RETX) - dates$RETX[j])/29
      }
    }
}





