#!/bin/ksh
set -x

# PREPARES SNOW FAL FILE FROM CONVECTIVE + LARGE SCALE NOW FILES

typeset -Z2 MMA MME DDA DDE HHA HHE
typeset -Z4 YYA YYE YYN YYI
typeset -Z3 MMM III


JOB_ID=SMHIRCA

#CASE=A1B_HadCM3Q3 #SMHIRCA
#CASE=A1B_BCM #SMHIRCA
CASE=A1B_ECHAM5-r3 #SMHIRCA

YYI=1961
YYE=2100

HOME_DIR=/media/disk/DATA/ENSEMBLES_FLOODS
OPATH=${HOME_DIR}/${JOB_ID}/${CASE}
WPATH=${HOME_DIR}/${JOB_ID}/${CASE}/work

cd ${OPATH}
mkdir work

(( YYA=YYI ))
cd ${WPATH}
rm -f *
while [ ${YYA} -le ${YYE} ]
do
(( YYN=YYA+9 ))
mv ${OPATH}/${JOB_ID}_${CASE}_DM_25km_${YYA}-${YYN}*_prsn*.nc ./
ncks -v lat,lon ${JOB_ID}_${CASE}_DM_25km_${YYA}-${YYN}_prsnc.nc ./latlon.nc

cdo add ${JOB_ID}_${CASE}_DM_25km_${YYA}-${YYN}_prsnc.nc ${JOB_ID}_${CASE}_DM_25km_${YYA}-${YYN}_prsnls.nc dummy.nc
ncks -A latlon.nc dummy.nc
cdo chname,prsnc,prsn dummy.nc ${JOB_ID}_${CASE}_DM_25km_${YYA}-${YYN}_prsn.nc
#gzip *.nc
rm -f dummy.nc
rm -f latlon.nc
mv *.nc ${OPATH}

(( YYA=YYA+10 ))
done


exit



