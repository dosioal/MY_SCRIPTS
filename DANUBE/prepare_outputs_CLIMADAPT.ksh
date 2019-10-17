#!/bin/ksh
set -x

typeset -Z4 YEND

#lon1=-21.7199993 
lon1=-21.5  # FOR C4I
lon2=15.56 
lat1=-20.6800003 
lat2=20.9999996

VAR=T
VAR=Tmax
VAR=Tmin
VAR=Ptot

YEND=2098 #METO
#YEND=2099 
#YEND=2100

JOB_ID=C4IRCA3
CASE=A1B_HadCM3Q16 #C1A

#JOB_ID=CNRM-RM5.1
#CASE=SCN_ARPEGE 

#JOB_ID=DMI-HIRHAM5 # VEDI SOTTO pr
#CASE=A1B_ECHAM5 
#CASE=A1B_ARPEGE
#CASE=A1B_BCM

#JOB_ID=ETHZ-CLM
#CASE=SCN_HadCM3Q0 #ETHZ A1B

#JOB_ID=KNMI-RACMO2
#CASE=A1B_ECHAM5-r3

#JOB_ID=METO-HC_HadRM3Q0
#CASE=A1B_HadCM3Q0 #METO-HC

#JOB_ID=MPI-M-REMO   # NB nc ---> nc AND SEE BELOW
#CASE=SCN_ECHAM5 #MPI-REMO

#JOB_ID=SMHIRCA
#CASE=A1B_BCM #SMHIRCA
#CASE=A1B_ECHAM5-r3 #SMHIRCA
#CASE=A1B_HadCM3Q3 #SMHIRCA

 
HOME_DIR=/media/disk/POSTPROC/BC_Regional/${JOB_ID}_${CASE}_EOBS_1961-1990_1961-2100/finalOutputData
WORK_DIR=${HOME_DIR}/work

IPATH=${HOME_DIR}
OPATH=${HOME_DIR}/YM


cd ${HOME_DIR}
if [ ! -d YM ] ; then
	mkdir YM
fi
if [ ! -d work ] ; then
	mkdir work
fi

rm -f work/*

file=BCed_1961-1990
gunzip ${VAR}_BCed_1961_1990_????_????.nc.gz

cdo mergetime ${VAR}_BCed_1961_1990_????_????.nc work/${VAR}_BCed_1961_${YEND}.nc
cd work
set +x

cp /media/disk/POSTPROC/BC_Regional/GRID_EU_022_${JOB_ID}.txt  ./
cdo remapcon,GRID_EU_022_${JOB_ID}.txt ${VAR}_BCed_1961_${YEND}.nc ${VAR}_BCed_1961_${YEND}_022.nc
#cdo sellonlatbox,${lon1},${lon2},${lat1},${lat2} ${VAR}_BCed_1961_${YEND}_022.nc ${VAR}_BCed_1961_${YEND}_022_COM.nc
cdo yearmean ${VAR}_BCed_1961_${YEND}_022.nc ${VAR}_BCed_1961_${YEND}_022_YM.nc
ncks -d lon,${lon1},${lon2} -d lat,${lat1},${lat2} ${VAR}_BCed_1961_${YEND}_022_YM.nc ${OPATH}/${VAR}_BCed_${JOB_ID}_${CASE}_1961_${YEND}_022_YM_COM.nc

rm -f *
#gzip ${HOME_DIR}/${VAR}_BCed_1961_1990_????_????.nc

exit

