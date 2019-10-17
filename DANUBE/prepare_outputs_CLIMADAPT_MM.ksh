#!/bin/ksh
set -x

typeset -Z4 YEND
typeset -Z2 IM

#lon1=-21.7199993 
lon1=-21.5  # FOR C4I
lon2=15.56 
lat1=-20.6800003 
lat2=20.9999996

VAR=T
VAR=Tmax
VAR=Tmin
VAR=Ptot

#YEND=2098 #METO
YEND=2099 #ARPEGE BCM
#YEND=2100 #ECHAM

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
#CASE=A1B_ECHAM5-r3 #SMHIRCA
#CASE=A1B_HadCM3Q3 #SMHIRCA

 
HOME_DIR=/media/disk/POSTPROC/BC_Regional/${JOB_ID}_${CASE}_EOBS_1961-1990_1961-2100/BC_data
WORK_DIR=${HOME_DIR}/work

IPATH=${HOME_DIR}
OPATH=${HOME_DIR}/MM


cd ${HOME_DIR}
if [ ! -d MM ] ; then
	mkdir MM
fi
if [ ! -d work ] ; then
	mkdir work
fi


file=BCed_1961-1990

IM=01

while [ ${IM} -le 12 ]
do
rm -f work/*
cd ${HOME_DIR}
echo ${IM}

gunzip ${VAR}_BCed_1961_1990_????_????_${IM}.nc.gz
cdo mergetime ${VAR}_BCed_1961_1990_????_????_${IM}.nc work/${VAR}_BCed_1961_${YEND}_${IM}.nc

cd ${WORK_DIR}
set +x

cp /media/disk/POSTPROC/BC_Regional/GRID_EU_022_${JOB_ID}.txt  ./
cdo remapcon,GRID_EU_022_${JOB_ID}.txt ${VAR}_BCed_1961_${YEND}_${IM}.nc ${VAR}_BCed_1961_${YEND}_${IM}_022.nc
cdo monmean ${VAR}_BCed_1961_${YEND}_${IM}_022.nc ${VAR}_BCed_1961_${YEND}_${IM}_022_MM.nc
ncks -d lon,${lon1},${lon2} -d lat,${lat1},${lat2} ${VAR}_BCed_1961_${YEND}_${IM}_022_MM.nc ${OPATH}/${VAR}_BCed_${JOB_ID}_${CASE}_1961_${YEND}_${IM}_022_MM_COM.nc

 (( IM=IM+1 ))

done
rm -f *
#gzip ${HOME_DIR}/${VAR}_BCed_1961_1990_????_????.nc


exit

