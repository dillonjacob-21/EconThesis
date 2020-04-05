library(janitor)

library(tidyverse)


Full_1928 <- read.csv("FULL_1928.csv")


Full_1928_1933 <- read.csv("FULL1928_1933.csv")



Full_1933_1999 <- read.csv("FULL_1933_1999.csv")



FUll_2000_Present <- read.csv("FULL2000_Present.csv")


FULL_Stragglers <- read.csv("Stragglers.csv")


#Add National Cash Registers

Full_data_set_binded <- rbind(Full_1928,Full_1928_1933,Full_1933_1999,FUll_2000_Present, FULL_Stragglers)


Comanm_PERMNO <- Full_data_set_binded %>%
  distinct(PERMNO,COMNAM) 


library(lubridate)

Correct_dates <- Full_data_set_binded %>%
  mutate(date = mdy(date)) %>% 
  mutate(year = as.numeric(substr(date,1,4))) %>% 
  mutate(month = as.numeric(substr(date,6,7))) %>% 
  mutate(day = as.numeric(substr(date,9,10))) %>% 
  mutate(year = ifelse(year > 2020, year - 100,year)) %>%
  mutate(new_date = paste(month,day,year,sep ="/")) %>%
  mutate(date = mdy(new_date)) %>% 
  select(-new_date,-year,-month,-day)




# is there a faster way to do this



worthy_data_frame <- data.frame()

Chevron_data_frame <- data.frame()


library(tidyverse)
Dumby_Variable_Creator <- function(Code,date_in,date_out) {
  Codes <- data.frame(subset(Correct_dates, PERMNO == Code))
  for (i in 1:length(date_in)) {
    Codes[,paste("in_or_out",i)] <- ifelse(Codes$date >= date_in[i] & Codes$date <= date_out[i],1,0)
  }
  return(Codes)
}

Chevron_data_frame <- Dumby_Variable_Creator(14541,c("1985-10-30","2008-02-19"),c("1999-11-01","2019-12-31"))

Goldman_data_frame <- Dumby_Variable_Creator(86868,c("2013-09-23"),c("2019-12-31"))


Test_1 <- Dumby_Variable_Creator(17830, c("1930-07-18","1933-08-15","1939-03-04"),c("1932-05-26","1934-08-13","2019-12-31"))

library(tidyverse)



##note for a company taken out of the index before the end of 2019 you need to enter the day before they left the index
#1 Goldman
#2 Microsoft
#3 3M
#4 American Express
#5 Apple
#6 Boeing
#7 Caterpillar
#8 Chevron
#9 Cisco
#10 coca-cola
#11 Dowdupont inc
#12 exon mobil
#13 home depot
#14 intel
#15 IBM
#16 Johnson and Johnson
#17 JP morgan
#18 Mcdonalds
#19 Merck
#20 Nike
#21 Pfizer
#22 Proctor and Gamble
#####23a travelers I do not know how to handle
##23b citigroup cant figure out
#24 United healthgroup
#25 United aircraft/united technologies
#26 verizon
#27 Visa
#28 walgreens
#29 walmart
#30 walt disney
#31 general electric
######32 AT&T can't figure out
#33 Alcoa
#34 Bank of America
#35 HP
#36 kraft
#37 citigroup don't know how to handle
#38 General motors
#39 American international
#40 altria (phillip morris)
#######41 SBC not sure has to do with AT&T
#42 American International
#43 allied chemical
#44 honeywell allied chemical
#45 Eastman Kodak
#46 International Paper
#47 Goodyear
#48 Sears
#49 Union Carbide
#50 Bethlehem steel
#51 Texaco (texas co)
#52 Westinghouse
#53 woolworth
#54 american can
#55 navistar (international harvester company)
#56 USX or United states steel corporation
#57 Inco limited or International nickel company
#58 american tobacco company
#59 General foods
#60 Johns-Manville
#61 Chrysler
#62 Esmark
#63 american copper mining company
#64 American Smelting and Refining
#65 National distillers
#66 National steel
#67 Corn Products
#68 Loews
#69 Nash motors
#70 Borden
#71 united aircraft
#72 Drug Inc
#73 Hudson Motor Car
#######74 National Cash register company
#75 Texas gulf sulphur company
#76 Paramount company
#77 Mack Trucks
#78 Radio corporation
#79 Victor talking machine
#80 Atlantic Refining
#81 BF goodrich
#82 Wright
#83 General railway






library(gtools)


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


Master <- worthy_data_frame %>% 
  select(-`in_or_out 1`,-`in_or_out 2`,-`in_or_out 3`) %>%
  filter(in_or_out_total == 1) %>% 
  group_by(date) %>% 
  summarise(average= mean(ewretd))











  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  

  
  




  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  




  

