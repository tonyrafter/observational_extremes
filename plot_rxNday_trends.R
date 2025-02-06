plot_rxNday_trends <- function(df,
                               oldest = first_year, newest = last_year,
                               periodtype = period_type,
                               duration = dur,
                               valid_prop = proportion,
                               type = "location",
                               sig_only = FALSE,
                               siglevel = sig_level,
                               sig_type = c("t", "MK"),
                               trend_period = c("decade", "year"),
                               trend_measure = c("mm", "percent_median", "percent_mean"),
                               covariate = c("year", "temperature"),
                               domain = c("aus", "vic", "syd", "seq"),
                               lon_lims, lat_lims) {

  df <- df %>% filter(year_start == oldest, year_end == newest)

  season_list <- c("DJF", "MAM", "JJA", "SON")
  month_list <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  df <- df %>% ungroup() %>%
    {if (periodtype == "Seasonal")
      mutate(., Season = lvls_revalue(as.factor(Season), season_list)) else . } %>%
    {if (periodtype == "Monthly")
      mutate(., Month = lvls_revalue(as.factor(Month), month_list)) else . }

  trend_period <- match.arg(trend_period)
  sig_type <- match.arg(sig_type)
  periodtype <- match.arg(periodtype)
  domain <- match.arg(domain)
  covariate <- match.arg(covariate)
  trend_measure <- match.arg(trend_measure)

  if (covariate == "year") {
    if (trend_period == "decade") {
      df <- df %>% mutate(trend = trend * 10,
                          trend_pc_median = trend_pc_median * 10,
                          trend_pc_mean = trend_pc_mean * 10)
    }
    if (trend_measure == "mm") {
      trend_label <- "Trend\n(mm/\nyear)"
    }
    if (trend_measure == "percent_median") {
      trend_label <- "Trend\n(% of median\nper year)"
    }
    if (trend_measure == "percent_mean") {
      trend_label <- "Trend\n(% of mean\nper year)"
    }
  }
  if (covariate == "temperature") {
    if (trend_measure == "mm") {
      trend_label <- "Trend\n(mm/\u00B0C)"
    }
    if (trend_measure == "percent_median") {
      trend_label <- "Trend\n(% of median\nper \u00B0C)"
    }
    if (trend_measure == "percent_mean") {
      trend_label <- "Trend\n(% of mean\nper \u00B0C)"
    }
  }

  if (sig_type == "MK") {
    p_var <- sym("p.value_mk")
    sig_label <- "Mann-Kendall test"
  } else {
    if (sig_type == "t") {
      p_var <- sym("p.value")
      sig_label <- "t-test"
    } else {
      stop("Invalid value for 'sig_type': Must be one of 'MK', 't'")
    }
  }

  if (domain == "aus") {
    dom <- "Australia"
    lon_breaks <- seq(110,156,5)
    lat_breaks <- seq(-45,-10,5)
  }
  if (domain == "vic") {
    dom <- "Victoria"
    lon_breaks <- seq(140,151,2)
    lat_breaks <- seq(-40,-30,2)
  }
  if (domain == "syd") {
    dom <- "Sydney"
    lon_breaks <- seq(148,153,1)
    lat_breaks <- seq(-36,-31,1)
  }
  if (domain == "seq") {
    dom <- "SE Qld"
    lon_breaks <- seq(150,154,1)
    lat_breaks <- seq(-29,-24,1)
  }

  if (trend_measure == "mm") {
    trend_var <- sym("trend")
    metric_label <- ""
  }
  if (trend_measure == "percent_median") {
    trend_var <- sym("trend_pc_median")
    metric_label <- paste0("Percentage of median ",tolower(periodtype)," maxima")
  }
  if (trend_measure == "percent_mean") {
    trend_var <- sym("trend_pc_mean")
    metric_label <- paste0("Percentage of mean ",tolower(periodtype)," maxima")
  }


  baseplot <- df %>% ungroup() %>%
    filter(!!trend_var > 0, !!p_var > siglevel) %>%
    ggplot(aes(x = Longitude, y = Latitude, size = abs(!!trend_var))) +
    annotation_map(map_data("worldHires"), fill = "antiquewhite", col = "grey25") +
    coord_sf(xlim = lon_lims, ylim = lat_lims, expand = FALSE) +
    scale_x_continuous(breaks = lon_breaks) +
    scale_y_continuous(breaks = lat_breaks) +
    theme(panel.background = element_rect(fill = "lightsteelblue1"))

  if (type == "magnitude" || type == "m") {

    if (sig_only) {

      plot_final <- baseplot +
        geom_point(data = filter(df, !!trend_var < 0, !!p_var <= siglevel),
                   shape = 25, alpha = 0.65, fill = "red", colour = "red") +
        geom_point(data = filter(df, !!trend_var > 0, !!p_var <= siglevel),
                   shape = 24, alpha = 0.65, fill = "navyblue", colour = "navyblue") +
        ggtitle(paste0("Magnitudes of significant linear trends in ",periodtype,
                       " Rx",duration,"hr: ", dom),
                subtitle = paste0("Period ",oldest,"-",newest,"; significance at ",
                                  siglevel," level via ", sig_label))

    } else {

      plot_final <- baseplot +
        geom_point(shape = 2, alpha = 0.5, colour = "navyblue") +
        geom_point(data = filter(df, !!trend_var < 0, !!p_var > siglevel),
                   shape = 6, alpha = 0.5, colour = "red") +
        geom_point(data = filter(df, !!trend_var < 0, !!p_var <= siglevel),
                   shape = 25, alpha = 0.65, fill = "red", colour = "red") +
        geom_point(data = filter(df, !!trend_var > 0, !!p_var <= siglevel),
                   shape = 24, alpha = 0.65, fill = "navyblue", colour = "navyblue") +
        ggtitle(paste0("Magnitudes of linear trends in ",periodtype," Rx",duration,"hr: ", dom),
                subtitle = paste0("Period ",oldest,"-",newest,"; significance at ",
                                  siglevel," level via ", sig_label))

    }

  } else if (type == "location" || type == "l") {

    if (sig_only) {

      plot_final <- baseplot +
        geom_point(data = filter(df, !!trend_var < 0, !!p_var <= siglevel),
                   shape = 25, alpha = 0.65, size = 1.5, fill = "red", colour = "red") +
        geom_point(data = filter(df, !!trend_var > 0, !!p_var <= siglevel),
                   shape = 24, alpha = 0.65, size = 1.5, fill = "navyblue", colour = "navyblue") +
        ggtitle(paste0("Significant linear trends in ",periodtype," Rx",duration,"hr: ", dom),
                subtitle = paste0("Period ",oldest,"-",newest,"; significance at ",siglevel,
                                  " level via ", sig_label))

    } else {

      plot_final <- baseplot +
        geom_point(shape = 2, alpha = 0.5, size = 1.5, colour = "navyblue") +
        geom_point(data = filter(df, !!trend_var < 0, !!p_var > siglevel),
                   shape = 6, alpha = 0.5, size = 1.5, colour = "red") +
        geom_point(data = filter(df, !!trend_var < 0, !!p_var <= siglevel),
                   shape = 25, alpha = 0.65, size = 1.5, fill = "red", colour = "red") +
        geom_point(data = filter(df, !!trend_var > 0, !!p_var <= siglevel),
                   shape = 24, alpha = 0.65, size = 1.5, fill = "navyblue", colour = "navyblue") +
        ggtitle(paste0("Linear trends in ",periodtype," Rx",duration,"hr: ", dom),
                subtitle = paste0("Period ",oldest,"-",newest,"; significance at ",siglevel,
                                  " level via ", sig_label))

    }

  } else {

    stop("Invalid value for 'type': Must be one of 'location', 'magnitude', 'l', 'm'")

  }

  if (periodtype == "Annual") {
    # do nothing!
    # plot_final <- plot_final
    # +
    #   # facet_wrap(~ Station, scales = "free_y", nrow = 4)
    #   facet_wrap(~ Station)
  } else {
    if (periodtype == "Seasonal") {
      period_type_plotting <- sym("Season")
    } else {
      # if (periodtype == "Monthly") {
      period_type_plotting <- sym("Month")
    }
    plot_final <- plot_final +
      # facet_wrap(period_type_plotting, scales = "free_y", nrow = 4)
      facet_wrap(period_type_plotting)
  }

  if (trend_measure != "mm") {
    caption_suffix <- paste0("\n",metric_label)
  } else {
    caption_suffix <- ""
  }

  if (is.null(valid_prop)) {
    plot_final +
      labs(size = trend_label,
           caption = metric_label)
  } else {
    plot_final +
      labs(size = trend_label,
           caption = paste0("Minimum proportion of years with valid data: ",
                            valid_prop,"%",caption_suffix))
  }

}
