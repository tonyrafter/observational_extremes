#!/bin/csh
#
# run_station_stats.csh
#
# Process through all the various combinations of start/end years,
# minimum record lengths, durations, and seasonal aggregations

# set echo

# for testing/debugging:
set testing

# set figs_or_tabs = figs
# set figs_or_tabs = tabs
set figs_or_tabs = drivers

set dryrun
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
  # set start_years = ( 1951 1961 1971 1976 1981 1986)
  set start_years = ( 1960 )
  set end_years = ( 2015 )
  set durations = ( 24 )
  # set durations = ( 1 3 6 12 24)
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
  foreach start_year ($start_years)
    # @ end_year = $start_year + 29
    foreach end_year ($end_years)
      set max_record = `expr $end_year - $start_year + 1`
      if ($max_record < $record_length) continue
      foreach duration ($durations)
        foreach aggregation ($aggregations)
          foreach min_proportion ($min_proportions)

            if (! $?dryrun) then
              echo ""
              echo ""
              echo "################################################"
              echo \
              Rscript GSDR_processing.R \
                -d $duration \
                -p $aggregation \
                -r $record_length \
                -f $start_year \
                -l $end_year \
                -x $min_proportion \
                -s $sig_level \
                -o $figs_or_tabs
              echo "################################################"
              echo ""
              echo ""
            endif

            $run_echo \
            Rscript GSDR_processing.R \
              -d $duration \
              -p $aggregation \
              -r $record_length \
              -f $start_year \
              -l $end_year \
              -x $min_proportion \
              -s $sig_level \
              -o $figs_or_tabs

            echo ""
            echo "run complete"
            echo "################################################"
            echo ""

            if (! $?dryrun) echo "Rscript GSDR_processing.R -d $duration -p $aggregation -r $record_length -f $start_year -l $end_year -x $min_proportion -s $sig_level -o $figs_or_tabs " >> progress.log

          end
        end
      end
    end
  end
end
exit
