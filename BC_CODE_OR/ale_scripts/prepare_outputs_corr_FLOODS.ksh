#!/bin/ksh
set -x

typeset -Z2 MMA MME DDA DDE HHA HHE
typeset -Z4 YYA YYE YYN
typeset -Z3 MMM III

#JOB_ID=C4IRCA3
#CASE=A1B_HadCM3Q16 #C1A

#JOB_ID=DMI-HIRHAM5 # VEDI SOTTO pr
#CASE=A1B_ECHAM5 
#CASE=A1B_ARPEGE

#JOB_ID=ETHZ-CLM
#CASE=SCN_HadCM3Q0 #ETHZ A1B

#JOB_ID=KNMI-RACMO2
#CASE=A1B_ECHAM5-r3

JOB_ID=MPI-M-REMO   # NB nc ---> nc AND SEE BELOW
CASE=SCN_ECHAM5 #MPI-REMO

#JOB_ID=CNRM-RM4.5 #ALADIN # VEDI SOTTO
#JOB_ID=GKSS-CLM #---> SEE BELOW
#JOB_ID=INMRCA3
#JOB_ID=METNOHIRHAM
#JOB_ID=METO-HC_HadRM3Q0
#JOB_ID=SMHIRCA
#JOB_ID=UCLM-PROMES # VEDI SOTTO pr

#JOB_ID=ICTP-REGCM3

#CASE=CTL_ERA40
#CASE=CTR_ERA40 #METNOHIRHAM SMHRCA
#CASE=SRESA1B_HadCM3Q0 #METNO
#CASE=SRESA1B_BCM #METNO
#CASE=A1B_HadCM3Q0 #METO-HC
#CASE=A1B_HadCM3Q3 #SMHIRCA
#CASE=A1B_BCM #SMHIRCA
#CASE=A1B_ECHAM5-r3 #SMHIRCA
 
YYA=2041
MMA=01

YYE=2050
MME=12

YYA_o=1961	# period of construction of BC
YYE_o=1990

#HOME_DIR=/media/disk/POSTPROC/BC_Regional/${JOB_ID}_${CASE}_EOBS_${YYA_o}-${YYE_o}_${YYA}-${YYE}/finalOutputData
HOME_DIR=/media/disk/POSTPROC/BC_Regional/${JOB_ID}_${CASE}_EOBS_${YYA_o}-${YYE_o}_1961-2100/finalOutputData
WORK_DIR=${HOME_DIR}/work
IPATH=${HOME_DIR}
OPATH=${HOME_DIR}/output_floods

ORO_PATH=/media/disk/DATA/ENSEMBLES_FLOODS/${JOB_ID}/
ORO_FILE=${JOB_ID}_CTL_ERA40_FIX_25km_orog.nc # FILE WITH LON AND LAT INFO
if [ ! ${ORO_PATH}/${ORO_FILE} ] ; then
	echo 'ORO FILE ' ${ORO_PATH}/${ORO_FILE} ' does not exist!'
	exit
fi

cd ${HOME_DIR}
if [ ! -d output_floods ] ; then
	mkdir output_floods
fi
if [ ! -d work ] ; then
	mkdir work
fi

file=BCed_${YYA_o}_${YYE_o}_${YYA}_${YYE}

cd work
mkdir ${YYA}
cd ${WORK_DIR}/${YYA}
rm -f *
set +x

cdo splityear  ${IPATH}/T_${file}.nc T_${file}_
cdo splityear  ${IPATH}/Tmax_${file}.nc Tmax_${file}_
cdo splityear  ${IPATH}/Tmin_${file}.nc Tmin_${file}_
cdo splityear  ${IPATH}/Ptot_${file}.nc Ptot_${file}_

ncks -v lat,lon ${ORO_PATH}/${ORO_FILE} ./latlon.nc

III=000
while [ ${YYA} -le ${YYE} ]
do

#C$I, METNO, SMHI + METOFFICE
#if [ -f ${JOB_ID}_${CASE}_DM_25km_as_000.nc2 ]
#then
#echo III ${III}
#cdo splitsel,30  ${JOB_ID}_${CASE}_DM_25km_pr_${III}.nc2 ${JOB_ID}_${CASE}_DM_25km_pr_${YYA}_
#cdo splitsel,30  ${JOB_ID}_${CASE}_DM_25km_tas_${III}.nc2 ${JOB_ID}_${CASE}_DM_25km_tas_${YYA}_
#cdo splitsel,30  ${JOB_ID}_${CASE}_DM_25km_tasmin_${III}.nc2 ${JOB_ID}_${CASE}_DM_25km_tasmin_${YYA}_
#cdo splitsel,30  ${JOB_ID}_${CASE}_DM_25km_tasmax_${III}.nc2 ${JOB_ID}_${CASE}_DM_25km_tasmax_${YYA}_
#
#OTHER
if [ -f ${JOB_ID}_${CASE}_DM_25km_pr_${YYA}.nc2 ]
then
cdo splitmon  ${JOB_ID}_${CASE}_DM_25km_pr_${YYA}.nc2 ${JOB_ID}_${CASE}_DM_25km_pr_${YYA}_
cdo splitmon  ${JOB_ID}_${CASE}_DM_25km_tas_${YYA}.nc2 ${JOB_ID}_${CASE}_DM_25km_tas_${YYA}_
cdo splitmon  ${JOB_ID}_${CASE}_DM_25km_tasmin_${YYA}.nc2 ${JOB_ID}_${CASE}_DM_25km_tasmin_${YYA}_
cdo splitmon  ${JOB_ID}_${CASE}_DM_25km_tasmax_${YYA}.nc2 ${JOB_ID}_${CASE}_DM_25km_tasmax_${YYA}_

else

#C$I, METNO, SMHI + METOFFICE
#echo III ${III}
#cdo splitsel,30  ${JOB_ID}_${CASE}_DM_25km_pr_${III}.nc ${JOB_ID}_${CASE}_DM_25km_pr_${YYA}_
#cdo splitsel,30  ${JOB_ID}_${CASE}_DM_25km_tas_${III}.nc ${JOB_ID}_${CASE}_DM_25km_tas_${YYA}_
#cdo splitsel,30  ${JOB_ID}_${CASE}_DM_25km_tasmin_${III}.nc ${JOB_ID}_${CASE}_DM_25km_tasmin_${YYA}_
#cdo splitsel,30  ${JOB_ID}_${CASE}_DM_25km_tasmax_${III}.nc ${JOB_ID}_${CASE}_DM_25km_tasmax_${YYA}_

#OTHER
cdo splitmon T_${file}_${YYA}.nc T_${file}_${YYA}_
cdo splitmon Tmin_${file}_${YYA}.nc Tmin_${file}_${YYA}_
cdo splitmon Tmax_${file}_${YYA}.nc Tmax_${file}_${YYA}_
cdo splitmon Ptot_${file}_${YYA}.nc Ptot_${file}_${YYA}_
fi

MMM=000
while [ ${MMA} -le ${MME} ]
do

mkdir ${OPATH}/${YYA}_${MMA}	

#C$I, METNO, SMHI + METOFFICE
#if [ -f ${JOB_ID}_${CASE}_DM_25km_pr_${YYA}_000.nc2 ]
#then
#	echo 'MMA'  ${MMA}
#	echo 'MMM' ${MMM}
#ncks -A latlon.nc ${JOB_ID}_${CASE}_DM_25km_pr_${YYA}_${MMM}.nc2 
#cdo -f nc copy ${JOB_ID}_${CASE}_DM_25km_pr_${YYA}_${MMM}.nc2 TOT_PREC.lffd${YYA}${MMA}.dm.nc
#ncks -A latlon.nc ${JOB_ID}_${CASE}_DM_25km_tas_${YYA}_${MMM}.nc2 
#cdo -f nc copy ${JOB_ID}_${CASE}_DM_25km_tas_${YYA}_${MMM}.nc2 T_2M_AV.lffd${YYA}${MMA}.dm.nc
#ncks -A latlon.nc ${JOB_ID}_${CASE}_DM_25km_tasmin_${YYA}_${MMM}.nc2 
#cdo -f nc copy ${JOB_ID}_${CASE}_DM_25km_tasmin_${YYA}_${MMM}.nc2 TMIN_2M.lffd${YYA}${MMA}.dm.nc
#ncks -A latlon.nc ${JOB_ID}_${CASE}_DM_25km_tasmax_${YYA}_${MMM}.nc2 
#cdo -f nc copy ${JOB_ID}_${CASE}_DM_25km_tasmax_${YYA}_${MMM}.nc2 TMAX_2M.lffd${YYA}${MMA}.dm.nc
##else
#	echo 'MMA'  ${MMA}
#	echo 'MMM' ${MMM}
#ncks -A latlon.nc ${JOB_ID}_${CASE}_DM_25km_pr_${YYA}_${MMM}.nc 
#mv ${JOB_ID}_${CASE}_DM_25km_pr_${YYA}_${MMM}.nc TOT_PREC.lffd${YYA}${MMA}.dm.nc
#ncks -A latlon.nc ${JOB_ID}_${CASE}_DM_25km_tas_${YYA}_${MMM}.nc 
#mv ${JOB_ID}_${CASE}_DM_25km_tas_${YYA}_${MMM}.nc T_2M_AV.lffd${YYA}${MMA}.dm.nc
#ncks -A latlon.nc ${JOB_ID}_${CASE}_DM_25km_tasmin_${YYA}_${MMM}.nc 
#mv ${JOB_ID}_${CASE}_DM_25km_tasmin_${YYA}_${MMM}.nc TMIN_2M.lffd${YYA}${MMA}.dm.nc
#ncks -A latlon.nc ${JOB_ID}_${CASE}_DM_25km_tasmax_${YYA}_${MMM}.nc 
#mv ${JOB_ID}_${CASE}_DM_25km_tasmax_${YYA}_${MMM}.nc TMAX_2M.lffd${YYA}${MMA}.dm.nc
#fi

#OTHERS
if [ -f T_${file}_${YYA}_${MMA}.nc2 ]
then
ncks -A latlon.nc ${JOB_ID}_${CASE}_DM_25km_pr_${YYA}_${MMA}.nc2 
cdo -f nc copy ${JOB_ID}_${CASE}_DM_25km_pr_${YYA}_${MMA}.nc2 TOT_PREC.lffd${YYA}${MMA}.dm.nc
ncks -A latlon.nc ${JOB_ID}_${CASE}_DM_25km_tas_${YYA}_${MMA}.nc2 
cdo -f nc copy ${JOB_ID}_${CASE}_DM_25km_tas_${YYA}_${MMA}.nc2 T_2M_AV.lffd${YYA}${MMA}.dm.nc
ncks -A latlon.nc ${JOB_ID}_${CASE}_DM_25km_tasmin_${YYA}_${MMA}.nc2 
cdo -f nc copy ${JOB_ID}_${CASE}_DM_25km_tasmin_${YYA}_${MMA}.nc2 TMIN_2M.lffd${YYA}${MMA}.dm.nc
ncks -A latlon.nc ${JOB_ID}_${CASE}_DM_25km_tasmax_${YYA}_${MMA}.nc2 
cdo -f nc copy ${JOB_ID}_${CASE}_DM_25km_tasmax_${YYA}_${MMA}.nc2 TMAX_2M.lffd${YYA}${MMA}.dm.nc
else
ncks -A latlon.nc T_${file}_${YYA}_${MMA}.nc 
mv T_${file}_${YYA}_${MMA}.nc T_2M_AV.lffd${YYA}${MMA}.dm.nc
ncks -A latlon.nc Tmin_${file}_${YYA}_${MMA}.nc 
mv Tmin_${file}_${YYA}_${MMA}.nc TMIN_2M.lffd${YYA}${MMA}.dm.nc
ncks -A latlon.nc Tmax_${file}_${YYA}_${MMA}.nc 
mv Tmax_${file}_${YYA}_${MMA}.nc TMAX_2M.lffd${YYA}${MMA}.dm.nc
ncks -A latlon.nc Ptot_${file}_${YYA}_${MMA}.nc 
mv Ptot_${file}_${YYA}_${MMA}.nc TOT_PREC.lffd${YYA}${MMA}.dm.nc
fi

tar cvfz ${YYA}${MMA}.dm.tar.gz *lffd${YYA}${MMA}.dm.nc

mv ${YYA}${MMA}.dm.tar.gz  ${OPATH}/${YYA}_${MMA}

 (( MMA=MMA+1 ))
 (( MMM=MMM+1 ))

done

if [ ${MMA} -gt 12 ]
then
 (( YYA=YYA+1 ))
 (( MMA=1 ))
fi

 (( III=III+1 ))

done


exit

