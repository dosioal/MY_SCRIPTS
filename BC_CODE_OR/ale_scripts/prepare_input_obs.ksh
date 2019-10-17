#!/bin/ksh
set -x

typeset -Z2 MMA MME DDA DDE HHA HHE
typeset -Z4 YYA YYE YYN
typeset -Z3 MMM III

JOB_ID=DMI-HIRHAM5 # VEDI SOTTO pr
#CASE=A1B_ECHAM5 #DMI
CASE=A1B_ ARPEGE #DMI

#JOB_ID=C4IRCA3
#JOB_ID=CNRM-RM4.5 #ALADIN # VEDI SOTTO
#JOB_ID=ETHZ-CLM
#JOB_ID=GKSS-CLM #---> SEE BELOW
#JOB_ID=INMRCA3
#JOB_ID=KNMI-RACMO2
#JOB_ID=METNOHIRHAM
#JOB_ID=METO-HC_HadRM3Q0
#JOB_ID=MPI-M-REMO   # NB nc ---> nc AND SEE BELOW
#JOB_ID=SMHIRCA
#JOB_ID=UCLM-PROMES # VEDI SOTTO pr

#JOB_ID=ICTP-REGCM3

#CASE=CTL_ERA40
#CASE=CTR_ERA40 #METNOHIRHAM SMHRCA

#CASE=A1B_HadCM3Q16 #C1A
#CASE=SCN_HadCM3Q0 #ETHZ A1B
#CASE=SRESA1B_HadCM3Q0 #METNO
#CASE=SRESA1B_BCM #METNO
#CASE=A1B_HadCM3Q0 #METO-HC
#CASE=A1B_HadCM3Q3 #SMHIRCA
#CASE=A1B_BCM #SMHIRCA
#CASE=A1B_ECHAM5-r3 #SMHIRCA
#CASE=SCN_ECHAM5 #MPI-REMO

var=tx # tg,tn,tx,rr

HOME_DIR=/media/disk/DATA/ENSEMBLES_FLOODS
HOME_DIR_OBS=/media/disk/DATA/ENSOBS

WORK_DIR=${HOME_DIR}/${JOB_ID}/work
WORK_DIR_OBS=${HOME_DIR_OBS}/work

OPATH=${HOME_DIR_OBS}/ENS_MODELS

YYA=1961
YYE=1990
date_ini=${YYA}-01-01
date_end=${YYE}-12-31

# LON LAT BOS DEF
# cdo sinfo file.nc
#DMI 
lon1=-22.16
#lon2 =15.8999996
lon2=15.95
#lat1=-20.6800003
lat1=-20.75
#lat2=20.8999996
lat2=20.95

cd ${WORK_DIR_OBS}
rm -f *
#cdo splityear ${HOME_DIR_OBS}/${var}_0.22deg_rot_v3.0.nc ${var}_0.22deg_rot_v3.0_

cdo seldate,${date_ini},${date_end} ${HOME_DIR_OBS}/${var}_0.22deg_rot_v3.0.nc ${var}_0.22deg_rot_v3.0_${YYA}-${YYE}.nc

# 30 years files
#cdo mergetime ${var}_0.22deg_rot_v3.0_1961.nc ${var}_0.22deg_rot_v3.0_1962.nc ${var}_0.22deg_rot_v3.0_1963.nc ${var}_0.22deg_rot_v3.0_1964.nc ${var}_0.22deg_rot_v3.0_1965.nc ${var}_0.22deg_rot_v3.0_1966.nc ${var}_0.22deg_rot_v3.0_1967.nc ${var}_0.22deg_rot_v3.0_1968.nc ${var}_0.22deg_rot_v3.0_1969.nc ${var}_0.22deg_rot_v3.0_1970.nc ${var}_0.22deg_rot_v3.0_1971.nc ${var}_0.22deg_rot_v3.0_1972.nc ${var}_0.22deg_rot_v3.0_1973.nc ${var}_0.22deg_rot_v3.0_1974.nc ${var}_0.22deg_rot_v3.0_1975.nc ${var}_0.22deg_rot_v3.0_1976.nc ${var}_0.22deg_rot_v3.0_1977.nc ${var}_0.22deg_rot_v3.0_1978.nc ${var}_0.22deg_rot_v3.0_1979.nc ${var}_0.22deg_rot_v3.0_1980.nc ${var}_0.22deg_rot_v3.0_1981.nc ${var}_0.22deg_rot_v3.0_1982.nc ${var}_0.22deg_rot_v3.0_1983.nc ${var}_0.22deg_rot_v3.0_1984.nc ${var}_0.22deg_rot_v3.0_1985.nc ${var}_0.22deg_rot_v3.0_1986.nc ${var}_0.22deg_rot_v3.0_1987.nc ${var}_0.22deg_rot_v3.0_1988.nc ${var}_0.22deg_rot_v3.0_1989.nc ${var}_0.22deg_rot_v3.0_1990.nc  ${var}_0.22deg_rot_v3.0_${YYA}-${YYE}.nc

# 10 years files
#cdo mergetime ${var}_0.22deg_rot_v3.0_1961.nc ${var}_0.22deg_rot_v3.0_1962.nc ${var}_0.22deg_rot_v3.0_1963.nc ${var}_0.22deg_rot_v3.0_1964.nc ${var}_0.22deg_rot_v3.0_1965.nc ${var}_0.22deg_rot_v3.0_1966.nc ${var}_0.22deg_rot_v3.0_1967.nc ${var}_0.22deg_rot_v3.0_1968.nc ${var}_0.22deg_rot_v3.0_1969.nc ${var}_0.22deg_rot_v3.0_1970.nc  ${var}_0.22deg_rot_v3.0_${YYA}-${YYE}.nc
#${var}_0.22deg_rot_v3.0_1971.nc ${var}_0.22deg_rot_v3.0_1972.nc ${var}_0.22deg_rot_v3.0_1973.nc ${var}_0.22deg_rot_v3.0_1974.nc ${var}_0.22deg_rot_v3.0_1975.nc ${var}_0.22deg_rot_v3.0_1976.nc ${var}_0.22deg_rot_v3.0_1977.nc ${var}_0.22deg_rot_v3.0_1978.nc ${var}_0.22deg_rot_v3.0_1979.nc ${var}_0.22deg_rot_v3.0_1980.nc ${var}_0.22deg_rot_v3.0_1981.nc ${var}_0.22deg_rot_v3.0_1982.nc ${var}_0.22deg_rot_v3.0_1983.nc ${var}_0.22deg_rot_v3.0_1984.nc ${var}_0.22deg_rot_v3.0_1985.nc ${var}_0.22deg_rot_v3.0_1986.nc ${var}_0.22deg_rot_v3.0_1987.nc ${var}_0.22deg_rot_v3.0_1988.nc ${var}_0.22deg_rot_v3.0_1989.nc ${var}_0.22deg_rot_v3.0_1990.nc  ${var}_0.22deg_rot_v3.0_${YYA}-${YYE}.nc

cdo sellonlatbox,${lon1},${lon2},${lat1},${lat2} ${var}_0.22deg_rot_v3.0_${YYA}-${YYE}.nc ${var}_0.22deg_rot_v3.0_${JOB_ID}_${YYA}-${YYE}.nc

if [ ${var} = 'tg' ]   
then
cdo addc,273.15 ${var}_0.22deg_rot_v3.0_${JOB_ID}_${YYA}-${YYE}.nc ${var}_0.22deg_rot_v3.0_${JOB_ID}_${YYA}-${YYE}_K.nc
fi
if [ ${var} = 'tx' ]   
then
cdo addc,273.15 ${var}_0.22deg_rot_v3.0_${JOB_ID}_${YYA}-${YYE}.nc ${var}_0.22deg_rot_v3.0_${JOB_ID}_${YYA}-${YYE}_K.nc
fi
if [ ${var} = 'tn' ]   
then
cdo addc,273.15 ${var}_0.22deg_rot_v3.0_${JOB_ID}_${YYA}-${YYE}.nc ${var}_0.22deg_rot_v3.0_${JOB_ID}_${YYA}-${YYE}_K.nc
fi

#PREPARE LAND-SEA MASK FILE if var=tg
if [ ${var} = 'tg' ]   
then
cdo setmisstoc,0 ${var}_0.22deg_rot_v3.0_${JOB_ID}_${YYA}-${YYE}_K.nc file2.nc
cdo gtc,1 file2.nc file3.nc
(( timesteps=(${YYE}-${YYA}+1)*365+2*(${YYE}-${YYA+1})/10 ))
#cdo timselsum,${timesteps} file3.nc file4.nc
cdo timselmean,${timesteps} file3.nc file4.nc
cdo gtc,0.9 file4.nc read_in_${JOB_ID}_${YYA}-${YYE}.nc
ncks -v Actual_latitude,Actual_longitude ${var}_0.22deg_rot_v3.0_${JOB_ID}_${YYA}-${YYE}_K.nc ./latlon.nc
ncks -A ./latlon.nc read_in_${JOB_ID}_${YYA}-${YYE}.nc

mv read_in_${JOB_ID}_${YYA}-${YYE}.nc ${OPATH}/
fi

mv ${var}_0.22deg_rot_v3.0_${JOB_ID}_${YYA}-${YYE}*.nc ${OPATH}/
rm -f ${OPATH}/t*_0.22deg_rot_v3.0_${JOB_ID}_${YYA}-${YYE}.nc
exit



