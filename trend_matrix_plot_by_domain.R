trend_matrix_plot_by_domain <- function(df, accum = c(1, 3, 6, 12, 24), 
                              seas = c("Annual", "DJF", "MAM", "JJA", "SON"),
                              min_years = 10, max_years = Inf,
                              xtype = c("mid", "start", "end"), 
                              ytype = c("num_years", "end", "start"), 
                              start = 1960, end = 2015,
                              fill_var = c("mean", "sigdiff", "nstations"),
                              lim_value = 50, lim_range_type = c("split", "positive"),
                              facet_by = c("domain", "season", "annual", "accum"),
                              dom = NULL, 
                              domains = c("aus_nrm_clust", "nrm_clust", 
                                          "aus_nrm_sub_clust", "nrm_sub_clust",
                                          "nrm_super_clust", "state", "ncra"),
                              title = FALSE) {

  require(tidyverse)
  require(scico)

  # source('my_triangle_colourbar.R')

  # match args:
  accum <- match.arg(as.character(accum[1]), choices = c(1, 3, 6, 12, 24))
  seas <- match.arg(seas)
  xtype <- match.arg(xtype)
  ytype <- match.arg(ytype)
  fill_var <- match.arg(fill_var)
  facet_by <- match.arg(facet_by)
  domains <- match.arg(domains)
  lim_range_type <- match.arg(lim_range_type)
  
  
  ### Usually use combination
  ### xtype 'start year'  and ytype 'end year', or
  ### xtype 'middle year' and ytype 'nyears' (triangle)

  # set up x and y vars
  if (xtype == "start") {
    xlabel <- "Start year"
    xvar <- sym("start_year")
  }
  if (xtype == "mid") {
    xlabel <- "Middle year"
    xvar <- sym("mid_year")
  }
  if (xtype == "end") {
    xlabel <- "End year"
    xvar <- sym("end_year")
  }
  if (ytype == "start") {
    ylabel <- "Start year"
    yvar <- sym("start_year")
  }
  if (ytype == "end") {
    ylabel <- "End year"
    yvar <- sym("end_year")
  }
  if (ytype == "num_years") {
    ylabel <- "Number of years"
    yvar <- sym("n_years")
  }

  # set up fill vars
  if (fill_var == "mean") {
    fillvar <- sym("mean_pc_mean")
    fill_label <- "Mean trend\nchange (%)"
    title_start <- "Trend magnitudes in"
  }
  if (fill_var == "sigdiff") {
    fillvar <- sym("sig_pc_diff")
    df <- df %>% mutate(sig_pc_diff = n_pos_sig_pc - n_neg_sig_pc)
    # fill_label <- "Difference in percentage of stations with positive\nand negative significant trends (%)"
    fill_label <- "%"
    title_start <- "Difference in percentage of stations with positive and negative trends in\n"
  }
  if (fill_var == "nstations") {
    fillvar <- sym("n_total")
    fill_label <- "Number of\nstations"
    title_start <- "Total number of stations in trends of"
  }

  # set up facetting var
  if (facet_by == "domain") {
    # also need to filter one _season_ only
    facet_var <- sym("domain")
    df <- df %>% filter(season == seas)
    if (domains == "aus_nrm_clust") {
      facet_levels <- c("AUS", "MN", "WT", "R", "CS", "EC", "SSWF", "MB", "SS")
    }
    if (domains == "nrm_clust") {
      facet_levels <- c("MN", "WT", "R", "CS", "EC", "SSWF", "MB", "SS")
    }
    if (domains == "nrm_sub_clust") {
      facet_levels <- c("MNW", "MNE", "WT", "RS", "RN", "CS", "ECN", 
                        "SSWFW", "SSWFE", "MB", "ECS", "SSTW", "SSTE", "SSVW", "SSVE")
    }
    if (domains == "aus_nrm_sub_clust") {
      facet_levels <- c("AUS", "MNW", "MNE", "WT", "RS", "RN", "CS", "ECN", 
                        "SSWFW", "SSWFE", "MB", "ECS", "SSTW", "SSTE", "SSVW", "SSVE")
    }
    if (domains == "nrm_super_clust") {
      facet_levels <- c("NA", "EA", "R", "SA")
    }
    if (domains == "ncra") {
      facet_levels <- c("WA North", "NT", "QLD North", "WA South", "SA", "QLD South", "TAS", "VIC", "NSW")
    }
    if (domains == "state") {
      facet_levels <- c("WA", "NT", "QLD", "SA", "NSW", "ACT", "TAS", "VIC", "other")
    }
}
  if (facet_by == "season") {
    # also need to filter one _domain_ only
    facet_var <- sym(facet_by)
    df <- df %>% filter(domain == dom, season_type == "Seasonal")
    facet_levels <- c("DJF", "MAM", "JJA", "SON")
  }
  if (facet_by == "annual") {
    # also need to filter one _domain_ only
    facet_var <- sym("season")
    df <- df %>% filter(domain == dom, season == "Annual")
    facet_levels <- c("Annual")
  }
  if (facet_by == "accum") {
    # also need to filter one _domain_, _season_,  only
    facet_var <- sym("accumulation")
    df <- df %>% filter(domain == dom, season == seas)
    facet_levels <- c(1, 3, 6, 12, 24)
  }

  # set up correct facetting for domains
  if (lim_range_type == "positive") {
    lim_range <- c(0, lim_value)
    custom_palette <- c(paletteer::paletteer_c("viridis::inferno", n = 255))
  } else {
    lim_range <- c(-1 * lim_value, lim_value)
    custom_palette <- c("coral1", paletteer::paletteer_c("scico::vik", direction = -1, n = 255), "skyblue")
  }


  # filter and add vars to df for plotting
  table_input <- df %>% 
    # all_tables_nrm_clust %>% 
    {if (facet_by != "accum") filter(., accumulation == accum) else . } %>% 
    filter(
      # accumulation == accum,
      # season == seas, 
      between(n_years, min_years, max_years)
    ) %>% 
    mutate(
      start_year = as.numeric(str_split_i(period, "-", 1)),
      end_year = as.numeric(str_split_i(period, "-", 2)),
      mid_year = (start_year + end_year)/2,
      .after = period) 
  
  # create plot
  p1 <- table_input %>% 
    ggplot(aes(x = !!xvar, y = !!yvar, fill = !!fillvar)) +
  
      geom_tile(width = 1, height = 1) +
      scale_fill_gradientn(
        colours = custom_palette,
        # palette = "vik", direction = -1, 
      # scale_fill_scico(palette = "vik", direction = -1, 
    # oob = oob_keep,
      oob = scales::oob_squish,
      # guide = my_triangle_colourbar(),
      limits = lim_range) + 
      
      # geom_tile(data = filter(table_input, !!fillvar > 7), width = 1, height = 1, shape = '+') +
      # geom_point(data = subset(table_input, !!fillvar >= 7), shape = "+", size = 1, colour = 'black') +
      # geom_tile(data = filter(table_input, !!fillvar > lim_value), width = 1, height = 1, fill = "skyblue") +
      # geom_tile(data = filter(table_input, !!fillvar < -1*lim_value), width = 1, height = 1, fill = "coral1") +
      coord_fixed() + 
      # theme_bw() +
      labs(x = xlabel, y = ylabel, 
      fill = fill_label) +
    facet_wrap(vars(factor({{facet_var}}, levels = facet_levels)))
  # ~factor(size, levels=c('50%','100%','150%','200%'))
    # add title if requested
    if (title == TRUE) {
      if (facet_by == "domain") {
        title_text <- paste(title_start, seas, "maximum", as.character(accum), "hr precipitation")
      } else if (facet_by == "season") {
        title_text <- paste(title_start, dom, "seasonal maximum", as.character(accum), "hr precipitation")
      } else if (facet_by == "annual") {
        title_text <- paste(title_start, dom, "annual maximum", as.character(accum), "hr precipitation")
      } else if (facet_by == "accum") {
        title_text <- paste(title_start, dom, seas, "maximum precipitation for all accumulations")
      }
      p1 +
        ggtitle(title_text)
      } else {
      p1
    }
  
}
