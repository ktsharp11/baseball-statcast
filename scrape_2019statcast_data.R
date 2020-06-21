#### Scraping Statcast Data ####
## https://www.coverfoursports.com/post/working-with-data-getting-statcast-data-in-r

# Remove existing objects from global environment
rm(list=ls())

# Install and load packages
install.packages("devtools")
library(devtools)
install_github("BillPetti/baseballr")
library(baseballr)

# Get 2019 pitcher data, scrape one week at a time (maximum allowed)
a0 <- scrape_statcast_savant_pitcher_all("2019-03-02",
                                         "2019-04-01")

a1 <- scrape_statcast_savant_pitcher_all("2019-04-02",
                                         "2019-04-08")

a2 <- scrape_statcast_savant_pitcher_all("2019-04-09",
                                         "2019-04-15")

a3 <- scrape_statcast_savant_pitcher_all("2019-04-16",
                                         "2019-04-22")

a4 <- scrape_statcast_savant_pitcher_all("2019-04-23",
                                         "2019-04-29")

a5 <- scrape_statcast_savant_pitcher_all("2019-04-30",
                                         "2019-05-06")

a6 <- scrape_statcast_savant_pitcher_all("2019-05-07",
                                         "2019-05-13")

a7 <- scrape_statcast_savant_pitcher_all("2019-05-14",
                                         "2019-05-20")

a8 <- scrape_statcast_savant_pitcher_all("2019-05-21",
                                         "2019-05-27")

a9 <- scrape_statcast_savant_pitcher_all("2019-05-28", 
                                         "2019-06-03")

a10 <- scrape_statcast_savant_pitcher_all("2019-06-04", 
                                          "2019-06-10")

a11 <- scrape_statcast_savant_pitcher_all("2019-06-11",
                                          "2019-06-17")

a12 <- scrape_statcast_savant_pitcher_all("2019-06-18",
                                          "2019-06-24")

a13 <- scrape_statcast_savant_pitcher_all("2019-06-25",
                                          "2019-07-01")

a14 <- scrape_statcast_savant_pitcher_all("2019-07-02",
                                          "2019-07-08")

a15 <- scrape_statcast_savant_pitcher_all("2019-07-09",
                                          "2019-07-15")

a16 <- scrape_statcast_savant_pitcher_all("2019-07-16", 
                                          "2019-07-22")

a17 <- scrape_statcast_savant_pitcher_all("2019-07-23", 
                                          "2019-07-29")

a18 <- scrape_statcast_savant_pitcher_all("2019-07-30",
                                          "2019-08-05")

a19 <- scrape_statcast_savant_pitcher_all("2019-08-06",
                                          "2019-08-12")

a20 <- scrape_statcast_savant_pitcher_all("2019-08-13", 
                                          "2019-08-19")

a21 <- scrape_statcast_savant_pitcher_all("2019-08-20", 
                                          "2019-08-26")

a22 <- scrape_statcast_savant_pitcher_all("2019-08-27",
                                          "2019-09-02")

a23 <- scrape_statcast_savant_pitcher_all("2019-09-03",
                                          "2019-09-09")

a24 <- scrape_statcast_savant_pitcher_all("2019-09-10",
                                          "2019-09-16")

a25 <- scrape_statcast_savant_pitcher_all("2019-09-17",
                                          "2019-09-23")

a26 <- scrape_statcast_savant_pitcher_all("2019-09-24",
                                          "2019-09-30")

a27 <- scrape_statcast_savant_pitcher_all("2019-10-01",
                                          "2019-11-01")

# Compile each week of data into two tables, then combine into master
ac1 <- rbind(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10,
             a11, a12, a13, a14)
ac2 <- rbind(a15, a16, a17, a18, a19, a20, a21,
             a22, a23, a24, a25, a26, a27)
ac_all <- rbind(ac1, ac2)

# write csv with all the data to file
write_csv(ac_all, "statcast2019_pit.csv")
