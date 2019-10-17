#!/bin/ksh
set -x

typeset -Z4 YEND

# CALC PERCENTILES FOR 1961-1990
#lon1=-21.7199993 
lon1=-21.5  # FOR C4I
lon2=15.56 
lat1=-20.6800003 
lat2=20.9999996

#VAR=T
#SEAS=DJF 
#SEAS=JJA 

#VAR=Tmax
#EAS=DJF 
#SEAS=JJA 

VAR=Tmin
SEAS=DJF 
SEAS=JJA 

#VAR=Ptot  ### VEDI SOTTO MULC!!!!!!!!!!!!!!!!!1
#SEAS=DJF 
#SEAS=JJA 

JOB_ID=C4IRCA3
CASE=A1B_HadCM3Q16 #C1A

JOB_ID=CNRM-RM5.1
CASE=SCN_ARPEGE 

JOB_ID=DMI-HIRHAM5 # VEDI SOTTO pr
CASE=A1B_ECHAM5 
CASE=A1B_ARPEGE
CASE=A1B_BCM

JOB_ID=ETHZ-CLM
CASE=SCN_HadCM3Q0 #ETHZ A1B

JOB_ID=KNMI-RACMO2
CASE=A1B_ECHAM5-r3

JOB_ID=METO-HC_HadRM3Q0
CASE=A1B_HadCM3Q0 #METO-HC

JOB_ID=MPI-M-REMO   # NB nc ---> nc AND SEE BELOW
CASE=SCN_ECHAM5 #MPI-REMO

JOB_ID=SMHIRCA
CASE=A1B_BCM #SMHIRCA
CASE=A1B_ECHAM5-r3 #SMHIRCA
CASE=A1B_HadCM3Q3 #SMHIRCA

 
HOME_DIR=/media/disk_BC/POSTPROC/BC_Regional/${JOB_ID}_${CASE}_EOBS_1961-1990_1961-2100/finalOutputData
WORK_DIR=${HOME_DIR}/work

IPATH=${HOME_DIR}
OPATH=${HOME_DIR}/PERC


cd ${HOME_DIR}
if [ ! -d PERC ] ; then
	mkdir PERC 
fi
if [ ! -d work ] ; then
	mkdir work
fi

rm -f work/*

gunzip ${VAR}_BCed_1961_1990_1961_1990.nc.gz
file=${HOME_DIR}/${VAR}_BCed_1961_1990_1961_1990.nc

cd work
set +x

cp /media/disk_BC/POSTPROC/BC_Regional/GRID_EU_022_${JOB_ID}.txt  ./
cdo remapcon,GRID_EU_022_${JOB_ID}.txt ${file} ${VAR}_BCed_1961_1990_022.nc

#PTOT
#cdo selseas,${SEAS} ${VAR}_BCed_1961_1990_022.nc ${VAR}_BCed_1961_1990_022_${SEAS}_work.nc
#cdo mulc,86400. ${VAR}_BCed_1961_1990_022_${SEAS}_work.nc ${VAR}_BCed_1961_1990_022_${SEAS}.nc
#ELSE
cdo selseas,${SEAS} ${VAR}_BCed_1961_1990_022.nc ${VAR}_BCed_1961_1990_022_${SEAS}.nc

cdo ydrunmin,5 ${VAR}_BCed_1961_1990_022_${SEAS}.nc minfile_${SEAS}.nc
cdo ydrunmax,5 ${VAR}_BCed_1961_1990_022_${SEAS}.nc maxfile_${SEAS}.nc

#TMIN
cdo ydrunpctl,5,5  ${VAR}_BCed_1961_1990_022_${SEAS}.nc minfile_${SEAS}.nc maxfile_${SEAS}.nc ${VAR}_BCed_1961_1990_022_5PRCT_${SEAS}.nc
cdo ydrunpctl,10,5 ${VAR}_BCed_1961_1990_022_${SEAS}.nc minfile_${SEAS}.nc maxfile_${SEAS}.nc ${VAR}_BCed_1961_1990_022_10PRCT_${SEAS}.nc
ncks -d lon,${lon1},${lon2} -d lat,${lat1},${lat2} ${VAR}_BCed_1961_1990_022_5PRCT_${SEAS}.nc ${OPATH}/${VAR}_BCed_${JOB_ID}_${CASE}_1961_1990_022_5PRCT_COM_${SEAS}.nc
ncks -d lon,${lon1},${lon2} -d lat,${lat1},${lat2} ${VAR}_BCed_1961_1990_022_10PRCT_${SEAS}.nc ${OPATH}/${VAR}_BCed_${JOB_ID}_${CASE}_1961_1990_022_10PRCT_COM_${SEAS}.nc
#
#TMAX AND PREC
cdo ydrunpctl,90,5 ${VAR}_BCed_1961_1990_022_${SEAS}.nc minfile_${SEAS}.nc maxfile_${SEAS}.nc ${VAR}_BCed_1961_1990_022_90PRCT_${SEAS}.nc
cdo ydrunpctl,95,5 ${VAR}_BCed_1961_1990_022_${SEAS}.nc minfile_${SEAS}.nc maxfile_${SEAS}.nc ${VAR}_BCed_1961_1990_022_95PRCT_${SEAS}.nc
ncks -d lon,${lon1},${lon2} -d lat,${lat1},${lat2} ${VAR}_BCed_1961_1990_022_90PRCT_${SEAS}.nc ${OPATH}/${VAR}_BCed_${JOB_ID}_${CASE}_1961_1990_022_90PRCT_COM_${SEAS}.nc
ncks -d lon,${lon1},${lon2} -d lat,${lat1},${lat2} ${VAR}_BCed_1961_1990_022_95PRCT_${SEAS}.nc ${OPATH}/${VAR}_BCed_${JOB_ID}_${CASE}_1961_1990_022_95PRCT_COM_${SEAS}.nc
#
rm -f *
#gzip ${HOME_DIR}/${VAR}_BCed_1961_1990_????_????.nc

exit

