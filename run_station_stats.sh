#!/bin/csh
#
# run_station_stats.csh
#
# Process through all the various combinations of start/end years,
# minimum record lengths, durations, and seasonal aggregations

# set echo

# for testing/debugging:
set testing

# set figs_or_tabs = tabs
set figs_or_tabs = drivers
set first_start = 1960
# set dt = 4
set dt_s = 10
set dt_e = 10

foreach dt ( `seq $dt_s $dt_e` )
# foreach dt ( $dt )
  @ last_start = 2015 - $dt
  # @ last_start = $first_start + 9

  echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
  echo "                 Starting ${dt} year windows"
  echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
  echo ""

  # set dryrun
  set run_echo = ""
  if ($?dryrun) set run_echo = "echo "

  if ($?testing) then
    #set record_lengths = (20 25 30)
    #set start_years = (1950)
    #set end_years = (1980 1986)
    #set durations = (1 3 6 12 24)
    #set aggregations = (Annual Seasonal Monthly)
    #set min_proportions = (0 80)
    set record_lengths = ( 0 )
    # set start_years = ( 1940 )
    # set start_years = ( 1910 1920 1930 1940 1950 1960 1970 1980 1990 1981 1986 1993)
    # set start_years = ( 1951 1956 1961 1966 1971 1976 1981 1986)
    set start_years = ( `seq $first_start $last_start` )
    # set start_years = ( 1960 1970 1980 1988 1990 1993 )
    # set start_years = ( 1970 )
    # set end_years = ( 1992 )
    set durations = ( 24 )
    # set durations = ( 1 3 6 12 24 )
    # set aggregations = ( Monthly )
    set aggregations = ( Seasonal )
    # set aggregations = ( Annual Seasonal )
      set min_proportions = ( 67 )
    # set sig_level = 0.05
    set sig_level = 0.1
  else
    set record_lengths = (0)
    set start_years = (1981 1986 1991)
    set end_years = (2015 2010)
    # set durations = (1)
    set durations = (1 3 6 12 24)
    # set aggregations = (Annual )
    set aggregations = (Annual Seasonal Monthly)
    set min_proportions = (50 67 80)
    # set sig_level = 0.05
    set sig_level = 0.1
  endif

  foreach record_length ($record_lengths)
    # if ($record_length == 0) then
    #   set record_length_alt = "NULL"
    # else
    #   set record_length_alt = $record_length
    # endif
    foreach start_year ($start_years)
      @ end_year = $start_year + $dt
      # if (`echo $start_year | cut -c4` == 1 || `echo $start_year | cut -c4` == 6) continue
      # foreach end_year ($end_years)
        set max_record = `expr $end_year - $start_year + 1`
        if ($max_record < $record_length) continue
        foreach duration ($durations)
          foreach aggregation ($aggregations)
            foreach min_proportion ($min_proportions)
              
              # what is the expected output
              if ($figs_or_tabs != drivers) then
                set basepath = "GSDR_analysis/station_trends/minlength_${record_length}yr/${start_year}-${end_year}/${duration}hr"
                set filename_table = "tables/station_trends-mk${sig_level}-temperature_${duration}hr_${start_year}-${end_year}_AUS_${aggregation}_min${min_proportion}pc.csv"
                set expected_file = $basepath/$filename_table
              else
                set basepath = "GSDR_analysis/NINO34"
                set filename_table = "tables/nino34_trends-mk_${start_year}-${end_year}_seasonal.csv"
                set expected_file = $basepath/$filename_table
              endif
              # if ($?dryrun) echo $expected_file
              if (-f $expected_file) then
                echo "Expected output file exists - skip this combination"
              else
                # echo "that didn't work - exit"
                # exit 1

                echo ""
                echo ""
                echo "################################################"
                echo \
                Rscript GSDR_processing.R \
                  -d $duration \
                  -p $aggregation \
                  # -r $record_length_alt \
                  -r $record_length \
                  -f $start_year \
                  -l $end_year \
                  -x $min_proportion \
                  -s $sig_level \
                  -o $figs_or_tabs
                echo "################################################"
                echo ""
                echo ""

                $run_echo \
                Rscript GSDR_processing.R \
                  -d $duration \
                  -p $aggregation \
                  -r $record_length \
                  # -r $record_length_alt \
                  -f $start_year \
                  -l $end_year \
                  -x $min_proportion \
                  -s $sig_level \
                  -o $figs_or_tabs

                echo ""
                echo "run complete"
                echo "################################################"
                echo ""

                echo "Rscript GSDR_processing.R -d $duration -p $aggregation -r $record_length -f $start_year -l $end_year -x $min_proportion -s $sig_level -o $figs_or_tabs " >> progress.log

              endif

            end
          end
        end
      # end
    end
  end

end # dt

exit
