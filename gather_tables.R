# R script to collate information from tables in a set of sub-directories
library(tidyverse)
library(vroom)

# running script from "C:/Users/raf018/OneDrive - CSIRO/Working/PhD/Observations"
base_dir <- "./GSDR_analysis/station_trends/minlength_0yr/"

sig_level <- "0.05"
min_pc <- "67"

# period_list <- c("1910-2015", "1920-2015", "1930-2015", "1940-2015", "1950-2015", "1951-1980",
#                  "1960-2015", "1961-1990", "1970-1992", "1970-2015", "1971-2000", "1976-2005",
#                  "1980-2015", "1981-2010", "1981-2015", "1986-2015", "1990-2015", "1993-2015",
#                  "1966-1989", "1990-2013", "1973-2009")

# period_list <- c("1951-1980", "1960-2015", "1961-1990", "1970-1992", "1970-2015", "1971-2000", "1976-2005", "1980-2015", "1981-2010", "1981-2015", "1986-2015", "1990-2015", "1993-2015")
# period_list <- c("1910-2015", "1950-2015", "1960-2015", "1970-1992", "1970-2015")
# period_list <- c("1960-2015")
# dur_list <- c(1)
dur_list <- c(1, 3, 6, 12, 24)
dur_list_char <- as.character(dur_list)
# domain_list <- c("aus", "vic", "syd", "seq")
domain_list <- c("aus", "nrm_clust_sub")
# domain_list <- c("aus", "nrm_clust", "nrm_clust_super", "nrm_clust_sub", "ncra", "state")
# domain_list <- c("nrm_clust", "nrm_clust_super", "nrm_clust_sub", "ncra", "state")
dom_label <- paste(domain_list, collapse = '-')
# period_type_list <- c("Annual", "Seasonal", "Monthly")
period_type_list <- c("Annual", "Seasonal")
# period_type_list <- c("Annual")

# type_list <- c("mk", "t")
# covariate_list <- c("year", "temperature")
covariate_list <- c("temperature")


# num_rows <- 17 * length(period_list) * length(dur_list) * length(domain_list) * length(covariate_list)

first_year <- 1960
last_year <- 2015
# for (year1 in seq(first_year, last_year-4)) {
#   # print(y_start)

# }
max_len <- last_year - first_year

# period_len <- seq(4, max_len)
# period_list <- as.character(paste(first_year,first_year+period_len,sep="-"))
# period_list


# period_list <- as.character(paste(seq(first_year,last_year-4),seq(first_year+4,2015),sep="-"))
# period_list

period_list <- c()
for (period_len in seq(4, max_len)) {
  # print(period_len)
  last_start <- last_year - period_len
  # for (year1 in seq(first_year, last_start)) {
  #   print(year1)
  # }
  temp_period_list <- paste(seq(first_year,last_start),seq(first_year+period_len,last_year),sep="-")
  period_list <- c(period_list, temp_period_list)
}

n_periods <- length(period_list)

t1 <- now()

new <- TRUE
missing_list <- c()
n_epoch <- 0

for (epoch in period_list) {

  epoch_len <- as.integer(str_split_i(string = epoch, pattern = "-", i = 2)) -
               as.integer(str_split_i(string = epoch, pattern = "-", i = 1)) + 1

  n_epoch <- n_epoch + 1
  print(paste0("[ epoch ", n_epoch, " / ", n_periods," ] ", epoch))

  for (cov in covariate_list) {

    if (cov == "temperature") {
      type_suffix <- "-temperature"
    } else {
      type_suffix <- ""
    }

    for (dur in dur_list_char) {

      dur_dir <- paste0(base_dir,epoch,"/",dur,"hr/tables/")

      for (period_type in period_type_list) {

        for (dom in domain_list) {

          filename <- paste0("station_trends-mk",sig_level,type_suffix,"_",dur,"hr_",
                             epoch,"_",toupper(dom),"_",period_type,"_min",min_pc,"pc.csv")
          # print(filename)

          if (length(list.files(path = dur_dir, pattern = filename)) != 0) {
            # n <- n + 1
            # print(paste0("[ ", n, " ]  ", filename))

            if (dom %in% c("aus", "vic", "syd", "seq")) {
              dom_type <- "rectangular"
              if (period_type == "Annual") {
                infile_coltypes <- "cciiidiiddddddc"
              } else {
                infile_coltypes <- "ccciiidiidddddd"
              }
            } else {
              dom_type <- "landmask"
              if (period_type == "Annual") {
                infile_coltypes <- "cciiiidiiddddddc" 
              } else {
                infile_coltypes <- "cccciiidiidddddd" 
              }
            }

            in_table <- vroom(file = paste0(dur_dir,filename),
              col_types = infile_coltypes) %>%
              rename(period = time_period) %>% 
              {if (period_type == "Annual")
                rename(., season = Annual) else . } %>%
              {if (period_type == "Seasonal")
                rename(., season = Season) else . } %>%
              {if (period_type == "Monthly")
                rename(., season = Month) else . } %>%
              # for landmask domains remove 'long name' version of domain
              {if (dom_type == "landmask") 
                # rename 1st col to 'domain' and remove 2nd col
                rename(., domain = 1) %>% select (-2) else . } %>% 
              mutate(period = epoch,
                n_years = as.integer(epoch_len),
                accumulation = as.integer(dur),
                covariate = cov,
                sig_level = sig_level,
                min_percent_valid = min_pc,
                domain_type = toupper(dom),
                season_type = period_type,
                .before = season
              )

            if (new == FALSE) {
              df <- bind_rows(df, in_table)
            } else {
              df <- in_table
              new <- FALSE
            }
          } else {
            print(paste("File missing:",filename))
            missing_list <- append(missing_list, filename)
          }

        }
      }
    }
  }
}

t2 <- now()
# print time taken to run through the above loops
elapsed_time <- t2 - t1
print(elapsed_time)

# missing_list_file <- "combined_tables_missing.csv"
missing_list_file <- paste0("combined_tables_",dom_label,"_missing.csv")

n_missing <- length(missing_list)
print(paste("Writing list of",n_missing,"missing files to",missing_list_file))
write_lines(missing_list, file = missing_list_file)


outfile <- paste0("combined_tables_",dom_label,".csv")
# outfile <- "combined_tables_nrm_clust_plus_ncra.csv"
# outfile <- "combined_tables_all2.csv"

print(paste("Writing tables to",outfile))

# vroom_write(df, file = "combined_tables_1951-onwards.csv", delim = ",")
vroom_write(df, file = outfile, delim = ",", )

# test_in <- vroom(file = "combined_table.csv", col_types = "cicddccciiidiidddddd")
