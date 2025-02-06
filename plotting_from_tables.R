# R script to create figures from combined tables of all periods, domains, seasons etc.
# library(optparse)
#
# option_list <- list(
#
#
# )
library(tidyverse)
library(vroom)
library(paletteer)
library(scico)

# all_tables <- vroom(file = "combined_table.csv",
# all_tables <- vroom(file = "combined_tables_1951-onwards.csv",
# all_tables <- vroom(file = "combined_tables_all.csv",
# all_tables <- vroom(file = "combined_tables_all2.csv",
#                     col_types = "cicddccciiidiidddddd")
all_tables_nrm_clust <- vroom(file = "combined_tables_nrm_clust.csv",
                    col_types = "cciiidiiddddddiicdiccc")

all_tables_aus_nrm_clust <- vroom(file = "combined_tables_aus-nrm_clust.csv",
                    col_types = "cciiidiiddddddiicdiccc")

all_tables_nrm_clust_sub <- vroom(file = "combined_tables_nrm_clust_sub.csv",
                    col_types = "cciiidiiddddddiicdiccc")

all_tables_aus_nrm_clust_sub <- vroom(file = "combined_tables_aus-nrm_clust_sub.csv",
                    col_types = "cciiidiiddddddiicdiccc")

all_tables_nrm_clust_super <- vroom(file = "combined_tables_nrm_clust_super.csv",
                    col_types = "cciiidiiddddddiicdiccc", na = "")

all_tables_ncra <- vroom(file = "combined_tables_ncra.csv",
                    col_types = "cciiidiiddddddiicdiccc")

all_tables_state <- vroom(file = "combined_tables_state.csv",
                    col_types = "cciiidiiddddddiicdiccc")

# p <- "1981-2015"
# period_list <- c("1951-1980", "1960-2015", "1961-1990", "1970-1992", "1970-2015", "1971-2000", "1976-2005", "1980-2015", "1981-2010", "1981-2015", "1986-2015", "1990-2015", "1993-2015")
period_list <- c("1910-2015", "1920-2015", "1930-2015", "1940-2015", "1950-2015", "1951-1980",
                 "1960-2015", "1961-1990", "1970-1992", "1970-2015", "1971-2000", "1976-2005",
                 "1980-2015", "1981-2010", "1981-2015", "1986-2015", "1990-2015", "1993-2015",
                 "1966-1989", "1990-2013", "1973-2009")
# > length(period_list)
# [1] 19

# p_list <- period_list[c(1,9,10,4,13,5)]
# list for comparing faceted periods next to each other in a particular order
p_list_3rows <- period_list[c(6, 15, 5, 8, 17, 7, 9, 18, 10)]
p_list_50s <- period_list[c(6, 15, 5)]
p_list_no50s <- period_list[c(8, 17, 7, 9, 18, 10)]

p_list <- p_list_no50s
p_list <- period_list[10]
p_list <- period_list[18]
p_list <- period_list[7]
p_list <- period_list[21]

if (length(p_list) == 1) {
  title_suffix <- paste0(": ",p_list[1])
} else {
  title_suffix <- ""
}

dom <- "AUS"
# dom <- "AUS"
seas <- "DJF"
# cov <- "year"
cov <- "temperature"
seas <- "Annual"
seas <- "Seasonal"

if (cov == "year") {
  cov_label <- "mm/decade"
} else {
  cov_label <- "mm/\u00B0C"
}





symmetric_limits <- function (x) {
  max <- max(abs(x))
  c(-max, max)
}

# specific list of periods for annual analysis
filtered_df <- all_tables %>%
  filter(period %in% p_list) %>%
  mutate(period = as_factor(period)) %>%
  group_by(period) %>%
  filter(covariate == cov,
         domain == dom,
         season == seas) %>%
  mutate(accumulation = as_factor(accumulation))

# for seasonal, looking at one period at a time:
filtered_df <- all_tables %>%
  filter(period %in% p_list, season_type == "Seasonal") %>%
  mutate(season = as_factor(season)) %>%
  group_by(season) %>%
  filter(covariate == cov,
         domain == dom,
         season_type == seas) %>%
  mutate(accumulation = as_factor(accumulation))




# # filtered_df <- all_tables %>%
# #   filter(period == p,
# #          covariate == "year",
# #          domain == "AUS",
# #          season_type == "Annual",
# #          min_percent_valid == 67) %>%
# #   mutate(accumulation = as_factor(accumulation))
#
# filtered_df %>%
#   ggplot(aes(x = accumulation, y = n_neg_sig_pc)) +
#   geom_area(fill = "blue", alpha = 0.5) +
#   geom_area(data = filtered_df,
#             aes(x = accumulation, y = -1 * n_neg_sig_pc),
#             fill = "red", alpha = 0.5)




coeff <- 2

# plot of proportion of sig pos/neg trended stations, faceted by period
p2 <- filtered_df %>%
  ggplot(aes(x = accumulation, y = n_pos_sig_pc)) +
  geom_col(fill = "blue", alpha = 0.6, width = 0.8) +
  xlab("Accumulation (hr)") +
  # ylab("Percentage of stations (%)") +
  ggtitle(paste0("Percentage of stations with significant trends",title_suffix),
          subtitle = paste0("Trends in ",toupper(dom)," ",tolower(season_type),
                            " maximum precipitation for various accumulations"))
p2 +
  geom_col(data = filtered_df,
         aes(x = accumulation, y = -1 * n_neg_sig_pc),
         fill = "red", alpha = 0.6, width = 0.8) +
  geom_hline(yintercept = c(2.5, -2.5), linetype = "dashed") +
  geom_hline(yintercept = 7 / coeff, linetype = "dotted") +
  geom_hline(yintercept = 14 / coeff, linetype = "dotted") +
  geom_hline(yintercept = 21 / coeff, linetype = "dotted") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = symmetric_limits) +
  # facet_wrap(~factor(period, levels=p_list)) +
  facet_wrap(~factor(season)) +
  scale_y_continuous(
    # Features of the first axis
    name = "Percentage of stations (%)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(
      transform = ~.*coeff,
      # for temperature scaling plots
      breaks = c(7, 14, 21), labels = c(7, 14, 21),
      name=paste0("Mean trend (",cov_label,") as % of station mean"))
    # , limits = symmetric_limits
  ) +
  geom_point(data = filtered_df, aes(x = accumulation, y = mean_pc_mean / coeff),
             shape = 16,
             size = 3) +
  # geom_point(data = filtered_df, aes(x = accumulation, y = mean_pc_mean),
  #          shape = 3,
  #          size = 3) +
# facet_wrap(~ period) +
  labs(caption = "Values above zero are for positive trends; below zero are for negative trends")






# p3 <- filtered_df %>%
#   ggplot(aes(x = accumulation, y = n_pos_sig_pc)) +
#   geom_area(fill = "blue", alpha = 0.6)
# p3 +
#   geom_col(data = filtered_df,
#            aes(x = accumulation, y = -1 * n_neg_sig_pc),
#            fill = "red", alpha = 0.6, width = 0.8) +
#   geom_hline(yintercept = c(2.5, -2.5))

p4 <- filtered_df %>%
  ggplot(aes(x = accumulation, y = median_pc_mean)) +
  geom_col()






### Plots of areal trends by start/end/mid year of trend



# lim_value <- 100
lim_value <- 50

### function is now in separate file: trend_matrix_plot_by_domain.R
# trend_matrix_plot_by_domain <- function(df, accum = 1, 
#                               seas = "Annual", dom = NULL, 
#                               min_years = 10, max_years = Inf,
#                               xtype = "start", ytype = "end", 
#                               start = 1960, end = 2015,
#                               fill_var = "mean_pc_mean",
#                               lim_value = 50, facet_by = "domain",
#                               title = FALSE) {

source('trend_matrix_plot_by_domain.R')
# trend_matrix_plot_by_domain(all_tables_nrm_clust, xtype = 'mid', ytype = 'num_years', title = T, lim_value = 50, facet_by = "domain", fill_var = 'mean', accum = 1, dom = "AUS", seas = "Annual", domains = "nrm_clust")
# trend_matrix_plot_by_domain(all_tables_aus_nrm_clust, xtype = 'mid', ytype = 'num_years', title = T, lim_value = 50, facet_by = "domain", fill_var = 'mean', accum = 1, dom = "AUS", seas = "Annual", domains = "aus_nrm_clust")
# trend_matrix_plot_by_domain(all_tables_ncra, xtype = 'mid', ytype = 'num_years', title = T, lim_value = 50, facet_by = "domain", fill_var = 'mean', accum = 1, dom = "AUS", seas = "Annual", domains = "ncra")
# trend_matrix_plot_by_domain(all_tables_state, xtype = 'mid', ytype = 'num_years', title = T, lim_value = 50, facet_by = "domain", fill_var = 'mean', accum = 1, dom = "AUS", seas = "Annual", domains = "state")
# trend_matrix_plot_by_domain(all_tables_nrm_clust_super, xtype = 'mid', ytype = 'num_years', title = T, lim_value = 50, facet_by = "domain", fill_var = 'mean', accum = 1, dom = "AUS", seas = "Annual", domains = "nrm_super_clust")
# trend_matrix_plot_by_domain(all_tables_aus_nrm_clust_sub, xtype = 'mid', ytype = 'num_years', title = T, lim_value = 50, facet_by = "domain", fill_var = 'mean', accum = 1, dom = "AUS", seas = "Annual", domains = "aus_nrm_sub_clust")
# trend_matrix_plot_by_domain(all_tables_aus_nrm_clust_sub, xtype = 'mid', ytype = 'num_years', title = T, lim_value = 100, facet_by = "domain", fill_var = 'nstations', accum = 1, dom = "AUS", seas = "Annual", domains = "aus_nrm_sub_clust", lim_range_type = "positive")

# Annual for AUS only
for (nh in c(1,3,6,12,24)) {
  # pt <- trend_matrix_plot_by_domain(all_tables_aus_nrm_clust_sub, xtype = 'mid', ytype = 'num_years', title = F, lim_value = 50, facet_by = "annual", fill_var = 'mean', accum = nh, dom = "AUS", seas = "Annual", domains = "aus_nrm_sub_clust")
  pt <- trend_matrix_plot_by_domain(all_tables_aus_nrm_clust, xtype = 'mid', ytype = 'num_years', title = F, lim_value = 50, facet_by = "annual", fill_var = 'mean', accum = nh, dom = "AUS", seas = "Annual", domains = "aus_nrm_clust")
  ggsave(paste0('GSDR_analysis/trend_matrix/matrix_mean-pc_AUS_Annual_',sprintf("%02d", nh),'hr.png'), plot = pt, width = 7, height = 7)
}

# annual by domain
for (nh in c(1,3,6,12,24)) {
  pt <- trend_matrix_plot_by_domain(all_tables_aus_nrm_clust, xtype = 'mid', ytype = 'num_years', title = F, lim_value = 50, facet_by = "domain", fill_var = 'mean', accum = nh, seas = "Annual", domains = "aus_nrm_clust")
  ggsave(paste0('GSDR_analysis/trend_matrix/matrix_mean-pc_ALL_Annual_',sprintf("%02d", nh),'hr.png'), plot = pt, width = 9, height = 9)
}

# seasonal for AUS
for (nh in c(1,3,6,12,24)) {
  pt <- trend_matrix_plot_by_domain(all_tables_aus_nrm_clust, xtype = 'mid', ytype = 'num_years', title = F, lim_value = 50, facet_by = "season", fill_var = 'mean', accum = nh, dom = "AUS")
  ggsave(paste0('GSDR_analysis/trend_matrix/matrix_mean-pc_AUS_seasonal_',sprintf("%02d", nh),'hr.png'), plot = pt, width = 9, height = 9)
}

# seasonal by duration
for (ssn in c("Annual", "DJF", "MAM", "JJA", "SON")) {
  pt <- trend_matrix_plot_by_domain(all_tables_aus_nrm_clust, xtype = 'mid', ytype = 'num_years', title = F, lim_value = 50, facet_by = "accum", fill_var = 'mean', seas = ssn, dom = "AUS")
  ggsave(paste0('GSDR_analysis/trend_matrix/matrix_mean-pc_AUS_',ssn,'_all.png'), plot = pt, width = 9, height = 6)
}

# seasonal by domain
for (ssn in c("DJF", "MAM", "JJA", "SON")) {
  for (nh in c(1,3,6,12,24)) {
    pt <- trend_matrix_plot_by_domain(all_tables_aus_nrm_clust, 
      xtype = 'mid', ytype = 'num_years', title = F, lim_value = 50, 
      facet_by = "domain", fill_var = 'mean', accum = nh, 
      seas = ssn, domains = "aus_nrm_clust")
    ggsave(paste0('GSDR_analysis/trend_matrix/matrix_mean-pc_ALL_',ssn,'_',sprintf("%02d", nh),'hr.png'), plot = pt, width = 9, height = 9)
  }
}  


# difference in significant trend stations
for (nh in c(1,3,6,12,24)) {
  pt <- trend_matrix_plot_by_domain(all_tables_aus_nrm_clust, xtype = 'mid', ytype = 'num_years', title = F, lim_value = 25, facet_by = "domain", fill_var = 'sigdiff', accum = nh, dom = "AUS", seas = "Annual", domains = "aus_nrm_clust")
  ggsave(paste0('GSDR_analysis/trend_matrix/matrix_diff-sig-pc_ALL_Annual_',sprintf("%02d", nh),'hr.png'), plot = pt, width = 9, height = 9)
}

year_label <- "Mid"
year_var <- sym('mid_year')
# year_label <- "End"
# year_var <- sym('end_year')
# year_label <- "Start"
# year_var <- sym('start_year')

# try to make a heatmap-style plot for x=central_year, y=n_years
# mod_table <- in_table %>% 
# mid_20_to_30_year_trends <- 
  all_tables_nrm_clust %>% 
    filter(
      # n_years == 25,
       season == "Annual", 
       domain == "CS",
       between(n_years, 20, Inf)
      #  abs(mean_pc_mean) > 100
    ) %>% 
  mutate(
    start_year = as.numeric(str_split_i(period, "-", 1)),
    end_year = as.numeric(str_split_i(period, "-", 2)),
    # mid_year = (as.numeric(str_split_i(period, "-", 1)) + as.numeric(n_years/2) - 0.5),
    mid_year = (start_year + end_year)/2,
    .after = period) %>% 
    # head()
# %>% 
  # ggplot(aes(x = end_year, y = n_years, fill = mean_pc_mean)) +
  ggplot(aes(x = !!year_var, y = n_years, fill = mean_pc_mean)) +
  # geom_point(shape = 22, size = 10, alpha = 0.8) +
    # geom_raster() +
  geom_tile(width = 1, height = 1) +
  # scale_fill_viridis_c(values = c(-100,100))
  scale_fill_scico(palette = "vik", direction = -1, oob=squish,
    limits = c(-1 * lim_value , lim_value)) + 
    # limits = c(-100,100)) + 
  # scale_colour_scico(palette = "vik", direction = -1, 
  #   limits = c(-1 * lim_value , lim_value)) + 
  # scale_fill_gradient2(low = "darkred", high = "darkblue", limits=c(-1 * lim_value, lim_value), oob=squish) +
  # scale_fill_viridis_c() + 
  # scale_fill_paletteer_c("ggthemes::Orange-Blue") +
  # scale_fill_paletteer_c("dichromat::DarkRedtoBlue.18", n = 10) +
    # scale_fill_paletteer_c("scico::berlin", n = 10) +
  # scale_colour_viridis_c() + 
  coord_fixed() + 
  labs(x = paste0(year_label, "-year of trend"), y = "Number of years in trend", 
# labs(x = "Mid-year of trend", y = "Number of years in trend", 
       fill = "Mean\n% change", colour = "Mean\n% change")




# +
table_input <- all_tables_aus_nrm_clust %>% 
  # all_tables_nrm_clust %>% 
  filter(
    # n_years == 25,
    accumulation == 1,
    season == "Annual", 
    # domain == "CS",
    between(n_years, 10, Inf)
    #  abs(mean_pc_mean) > 100
  ) %>% 
  mutate(
    start_year = as.numeric(str_split_i(period, "-", 1)),
    end_year = as.numeric(str_split_i(period, "-", 2)),
    # mid_year = (as.numeric(str_split_i(period, "-", 1)) + as.numeric(n_years/2) - 0.5),
    mid_year = (start_year + end_year)/2,
    .after = period) 

table_input %>% 
  # ggplot(aes(x = start_year, y = end_year, fill = mean_pc_mean)) +
  ggplot(aes(x = mid_year, y = n_years, fill = (n_pos_sig_pc - n_neg_sig_pc))) +
    # ggplot(aes(x = mid_year, y = n_years, fill = mean_pc_mean)) +

    geom_tile(width = 1, height = 1) +
    scale_fill_scico(palette = "vik", direction = -1
  # , 
  # oob = oob_keep,
  # oob = oob_squish,
    # limits = c(0, 2)
    # limits = c(-1 * lim_value , lim_value)
  ) + 
    
    # geom_tile(data = filter(table_input, mean_pc_mean > 50), width = 1, height = 1, fill = "skyblue") +
    # geom_tile(data = filter(table_input, mean_pc_mean < -50), width = 1, height = 1, fill = "coral1") +
    coord_fixed() + 
    # theme_bw() +
    # labs(x = "Start year", y = "End year", 
    labs(x = "Middle year", y = "Number of years", 
    fill = "Mean\n% change", colour = "Mean\n% change") +
  facet_wrap(~ domain) 
# +
  # theme_clean()
  # theme_bw()

all_tables_aus_nrm_clust <- 
  vroom(file = "combined_tables_aus-nrm_clust.csv",
  # col_types = "cciiiiiiidddddicdiccc")
  col_types = "cciiiiiiidddddiicdiccc")
  # col_types = "cicddccciiidiidddddd")



