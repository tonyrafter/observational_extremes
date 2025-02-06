# R script to collate information from tables in a set of sub-directories
library(tidyverse)
library(vroom)
library(paletteer)

# running script from "C:/Users/raf018/OneDrive - CSIRO/Working/PhD/Observations"
base_dir <- "./GSDR_analysis/NINO34/tables/"

# sig_level <- "0.05"
# min_pc <- "67"

# period_list <- c("1910-2015", "1920-2015", "1930-2015", "1940-2015", "1950-2015", "1951-1980",
#                  "1960-2015", "1961-1990", "1970-1992", "1970-2015", "1971-2000", "1976-2005",
#                  "1980-2015", "1981-2010", "1981-2015", "1986-2015", "1990-2015", "1993-2015",
#                  "1966-1989", "1990-2013", "1973-2009")

# period_list <- c("1951-1980", "1960-2015", "1961-1990", "1970-1992", "1970-2015", "1971-2000", "1976-2005", "1980-2015", "1981-2010", "1981-2015", "1986-2015", "1990-2015", "1993-2015")
# period_list <- c("1910-2015", "1950-2015", "1960-2015", "1970-1992", "1970-2015")
# period_list <- c("1960-2015")
# dur_list <- c(1)
# dur_list <- c(1, 3, 6, 12, 24)
# dur_list_char <- as.character(dur_list)
# domain_list <- c("aus", "vic", "syd", "seq")
# domain_list <- c("aus", "nrm_clust_sub")
# domain_list <- c("aus", "nrm_clust", "nrm_clust_super", "nrm_clust_sub", "ncra", "state")
# domain_list <- c("nrm_clust", "nrm_clust_super", "nrm_clust_sub", "ncra", "state")
# dom_label <- paste(domain_list, collapse = '-')
# period_type_list <- c("Annual", "Seasonal", "Monthly")
period_type_list <- c("Seasonal")
# period_type_list <- c("Annual", "Seasonal")
# period_type_list <- c("Annual")

# type_list <- c("mk", "t")
# covariate_list <- c("year", "temperature")
# covariate_list <- c("temperature")


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
for (period_len in seq(9, max_len)) {
  # print(period_len)
  last_start <- last_year - period_len
  # for (year1 in seq(first_year, last_start)) {
  #   print(year1)
  # }
  temp_period_list <- paste(seq(first_year,last_start),seq(first_year+period_len,last_year),sep="-")
  period_list <- c(period_list, temp_period_list)
}
head(period_list)
tail(period_list)
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

  # for (cov in covariate_list) {

  #   if (cov == "temperature") {
  #     type_suffix <- "-temperature"
  #   } else {
  #     type_suffix <- ""
  #   }

  #   for (dur in dur_list_char) {

  #     dur_dir <- paste0(base_dir,epoch,"/",dur,"hr/tables/")

      for (period_type in period_type_list) {

        # for (dom in domain_list) {

          filename <- paste0("nino34_trends-mk_",epoch,"_",tolower(period_type),".csv")
          # print(filename)

          if (length(list.files(path = base_dir, pattern = filename)) != 0) {
            # n <- n + 1
            # print(paste0("[ ", n, " ]  ", filename))

            infile_coltypes <- "iicddddd"

            in_table <- vroom(file = paste0(base_dir,filename),
              col_types = infile_coltypes) %>%
              mutate(period = epoch, 
                n_years = as.integer(epoch_len),
                .before = year_start)
              
              # rename(period = time_period) %>% 
              # {if (period_type == "Seasonal")
              #   rename(., season = Season) else . } %>%
              # mutate(period = epoch,
              #   n_years = as.integer(epoch_len),
              #   accumulation = as.integer(dur),
              #   covariate = cov,
              #   sig_level = sig_level,
              #   min_percent_valid = min_pc,
              #   domain_type = toupper(dom),
              #   season_type = period_type,
              #   .before = season
              # )

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

    #     }
    #   }
    # }
  }
}

t2 <- now()
# print time taken to run through the above loops
elapsed_time <- t2 - t1
print(elapsed_time)

# missing_list_file <- "combined_tables_missing.csv"
missing_list_file <- paste0("combined_tables_nino34_missing.csv")

n_missing <- length(missing_list)
print(paste("Writing list of",n_missing,"missing files to",missing_list_file))
write_lines(missing_list, file = missing_list_file)


outfile <- "combined_tables_nino34.csv"
# outfile <- "combined_tables_nrm_clust_plus_ncra.csv"
# outfile <- "combined_tables_all2.csv"

print(paste("Writing tables to",outfile))

# vroom_write(df, file = "combined_tables_1951-onwards.csv", delim = ",")
vroom_write(df, file = outfile, delim = ",", )

# test_in <- vroom(file = "combined_table.csv", col_types = "cicddccciiidiidddddd")
# test_in <- vroom(file = outfile, col_types = "ciiicddddd")
# df <- test_in

#################################################
# Plot triangles:
require(tidyverse)
require(scales)

# plot_list <- list()
for (facet_by in c("season", "annual")) {
# for (facet_by in c("annual", "season")) {

plot_nino34_triangles <- function(df, facet_by, title = FALSE) {
  
  # facet_by <- "season"
  # facet_by <- "annual"

  if (facet_by == "season") {
    # also need to filter one _domain_ only
    facet_var <- sym("Season")
    df_in <- df %>% filter(Season != "Annual")
    facet_levels <- c("DJF", "MAM", "JJA", "SON")
    lim_value <- 0.02
    # lim_value <- 0.0025
  }
  if (facet_by == "annual") {
    # also need to filter one _domain_ only
    facet_var <- sym("Season")
    df_in <- df %>% filter(Season == "Annual")
    facet_levels <- c("Annual")
    lim_value <-   0.02
    # lim_value <-   0.0005
    # lim_value <- 0.00125
  }

  title <- FALSE
  min_years <- 10
  max_years <- 56

  lim_range <- c(-1 * lim_value, lim_value)
  # custom_palette <- c("coral1", paletteer::paletteer_c("scico::vik", direction = 1, n = 255), "skyblue")
  custom_palette <- c("skyblue", paletteer::paletteer_c("scico::vik", direction = 1, n = 255), "coral1")

  xlabel <- "Middle year"
  xvar <- sym("mid_year")

  ylabel <- "Number of years"
  yvar <- sym("n_years")

  fillvar <- sym("trend")
  fill_label <- "NINO3.4\nper year"
  title_start <- "Trend magnitudes in"

  table_input <- df_in %>% 
    filter(
      between(n_years, min_years, max_years)
    ) %>% 
    mutate(
      start_year = year_start,
      end_year = year_end,
      mid_year = (start_year + end_year)/2,
      .after = period) 

  p1 <- table_input %>% 
    ggplot(aes(x = !!xvar, y = !!yvar, fill = !!fillvar)) +

      geom_tile(width = 1, height = 1) +
      scale_fill_gradientn(
        colours = custom_palette,
      oob = scales::oob_squish,
      limits = lim_range) + 
      
      coord_fixed() + 
      labs(x = xlabel, y = ylabel, fill = fill_label) +
    facet_wrap(vars(factor({{facet_var}}, levels = facet_levels))) +
    theme(axis.text=element_text(size=8),axis.title=element_text(size=8),
          strip.text.x = element_text(size = 8),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 9),
          legend.box.spacing = unit(0, "cm"))

}

  p_seas <- plot_nino34_triangles(df, "season") + 
    scale_x_continuous(breaks = c(1970,1990,2010)) +
      theme(axis.title.y = element_blank())
  p_ann <- plot_nino34_triangles(df, "annual") + theme(legend.position = "none")
  p_both <- p_ann + p_seas
  p_both
  
  ggsave(filename = paste0('GSDR_analysis/trend_matrix/matrix_nino34_ann-plus-seasons',title_label,'.png'), 
  plot=p_both, height=2.75, width=5.5, dpi=300)
  ggsave(filename = paste0('GSDR_analysis/trend_matrix/matrix_nino34_ann-plus-seasons',title_label,'.pdf'), 
  plot=p_both, height=2.75, width=5.5, dpi=300)

    # add title if requested
  if (title == TRUE) {
      title_label <- '_titled'
      if (facet_by == "season") {
        title_text <- paste(title_start, "seasonal trends in NINO3.4 index")
      } else if (facet_by == "annual") {
        title_text <- paste(title_start, "annual trends in NINO3.4 index")
      }
      title_label <- ''
      p1 +
        ggtitle(title_text)
    } else {
      title_label <- ''
      p1
    }

  ggsave(filename = paste0('GSDR_analysis/trend_matrix/matrix_nino34_',facet_by,title_label,'.png'), 
          plot=p1, height=14, width=14, units=c("cm"), dpi=600)

  if (facet_by == "season") {plot_season <- p1 + scale_x_continuous(breaks = c(1970,1990,2010))}
  if (facet_by == "annual") {plot_annual <- p1 + theme(legend.position = "none")}
    # plot_list[[facet_by]] <- p1
  # rm(p1)
}

# library(patchwork)
# plot_both <- plot_annual + plot_season
# plot_both
library(cowplot)
cowplot::plot_grid(plot_annual, plot_season, labels = "AUTO")
ggsave(filename = 'GSDR_analysis/NINO34/figures/combined_trend_triangles.png', 
          plot=plot_both, height=14, width=14, units=c("cm"), dpi=600)
