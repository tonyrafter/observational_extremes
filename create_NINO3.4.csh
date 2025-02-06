#!/bin/csh
#
#    create_NINO3.4.csh
#
#    Script to create monthly NINO3.4 index from HadISST data on NCI
#
#
# module load cdo
# set indir = /g/data/ia39/aus-ref-clim-data-nci/hadisst/data
# set indir = "/mnt/c/Users/raf018/OneDrive - CSIRO/Working/PhD/Observations"
set indir = .
cdo -L -fldmean -sellonlatbox,-60.0,-10.0,-5.0,5.0 -selname,sst "$indir/HadISST_sst.nc" NINO3.4.nc

# # create 15-year centred running mean, and subtract from NINO3.4 value at that time
# # to create detrended anomaly series
# # 15 year running mean = (15*12)+1 months = 181 months
# cdo runmean,181 NINO3.4.nc NINO3.4_15y_runmean.nc

# # create anomaly time series
# # subtract NINO3.4 from 15-yr running mean:
# # cdo sub NINO3.4.nc NINO3.4_15y_runmean.nc NINO3.4_15y_runmean_anomaly2.nc 
# ### doesn't deal with end period well (leaves last ~7.5 years as normal, uncorrected SST?)

# # cdo -L -sub -selname,sst NINO3.4.nc -runmean,97 NINO3.4.nc NINO3.4_15y_runmean_anomaly.nc
# # cdo -L -setname,nino34 -sub -selname,sst NINO3.4.nc -runmean,181 -selname,sst NINO3.4.nc NINO3.4_15y_runmean_anomaly.nc
# cdo -L -setname,nino34 -sub -selname,sst NINO3.4.nc NINO3.4_15y_runmean.nc NINO3.4_15y_runmean_anomaly.nc

# # try reversing order:
# cdo sub NINO3.4_15y_runmean.nc -selname,sst NINO3.4.nc negtest.nc

# # try 'cdo detrend':
# cdo detrend NINO3.4.nc detrended_nino34.nc
