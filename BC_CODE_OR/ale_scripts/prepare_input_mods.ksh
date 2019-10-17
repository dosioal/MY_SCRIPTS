#!/bin/ksh
set -x

typeset -Z2 MMA MME DDA DDE HHA HHE
typeset -Z4 YYA YYE YYN YYI
typeset -Z3 MMM III


#JOB_ID=DMI-HIRHAM5 # VEDI SOTTO pr
#CASE=A1B_ECHAM5 #DMI
#CASE=A1B_ARPEGE #DMI

#JOB_ID=C4IRCA3
#CASE=A1B_HadCM3Q16

#JOB_ID=ETHZ-CLM
#CASE=SCN_HadCM3Q0 #ETHZ A1B

#JOB_ID=KNMI-RACMO2
#CASE=A1B_ECHAM5-r3

#JOB_ID=MPI-M-REMO   # NB nc ---> nc AND SEE BELOW
#CASE=SCN_ECHAM5 #MPI-REMO

#JOB_ID=SMHIRCA
#CASE=A1B_BCM #SMHIRCA
#CASE=A1B_ECHAM5-r3 #SMHIRCA
#CASE=A1B_HadCM3Q3 #SMHIRCA

JOB_ID=METO-HC_HadRM3Q0
CASE=A1B_HadCM3Q0 #METO-HC

#JOB_ID=CNRM-RM4.5 #ALADIN # VEDI SOTTO
#JOB_ID=GKSS-CLM #---> SEE BELOW
#JOB_ID=INMRCA3
#JOB_ID=METNOHIRHAM
#JOB_ID=UCLM-PROMES # VEDI SOTTO pr
#JOB_ID=ICTP-REGCM3

#CASE=CTL_ERA40
#CASE=CTR_ERA40 #METNOHIRHAM SMHRCA

#CASE=SRESA1B_HadCM3Q0 #METNO
#CASE=SRESA1B_BCM #METNO

YYI=1961
YYE=1990

HOME_DIR=/media/disk/DATA/ENSEMBLES_FLOODS
OPATH=${HOME_DIR}/${JOB_ID}/${CASE}
WPATH=${HOME_DIR}/${JOB_ID}/${CASE}/work

cd ${OPATH}
mkdir work

(( YYN=${YYE}-${YYI}+1 ))
(( YYA=YYI ))
cd ${WPATH}
rm -f *
while [ ${YYA} -le ${YYE} ]
do
	echo unzipping ${JOB_ID}_${CASE}_DM_25km_${YYA}
	gunzip ${OPATH}/${JOB_ID}_${CASE}_DM_25km_${YYA}*_pr*.gz ./ 
	gunzip ${OPATH}/${JOB_ID}_${CASE}_DM_25km_${YYA}*_tas*.gz ./
        mv ${OPATH}/${JOB_ID}_${CASE}_DM_25km_${YYA}*_tas*.nc ./
	mv ${OPATH}/${JOB_ID}_${CASE}_DM_25km_${YYA}*_pr*.nc ./
	(( YYA=YYA+10 ))
done

# 30 years files
cdo mergetime ${JOB_ID}_${CASE}_DM_25km_*_tas.nc  ${OPATH}/${JOB_ID}_${CASE}_DM_25km_${YYI}-${YYE}_tas.nc
cdo mergetime ${JOB_ID}_${CASE}_DM_25km_*_tasmax.nc  ${OPATH}/${JOB_ID}_${CASE}_DM_25km_${YYI}-${YYE}_tasmax.nc
cdo mergetime ${JOB_ID}_${CASE}_DM_25km_*_tasmin.nc  ${OPATH}/${JOB_ID}_${CASE}_DM_25km_${YYI}-${YYE}_tasmin.nc
cdo mergetime ${JOB_ID}_${CASE}_DM_25km_*_pr.nc  ${OPATH}/${JOB_ID}_${CASE}_DM_25km_${YYI}-${YYE}_pr.nc
cdo mergetime ${JOB_ID}_${CASE}_DM_25km_*_prsn.nc  ${OPATH}/${JOB_ID}_${CASE}_DM_25km_${YYI}-${YYE}_prsn.nc

#cdo mergetime ${JOB_ID}_${CASE}_DM_25km_${YYI}-${YYE1}_tas.nc ${JOB_ID}_${CASE}_DM_25km_${YYA2}-${YYE2}_tas.nc ${JOB_ID}_${CASE}_DM_25km_${YYA3}-${YYE}_tas.nc ${JOB_ID}_${CASE}_DM_25km_${YYI}-${YYE}_tas.nc
#cdo mergetime ${JOB_ID}_${CASE}_DM_25km_${YYI}-${YYE1}_tasmax.nc ${JOB_ID}_${CASE}_DM_25km_${YYA2}-${YYE2}_tasmax.nc ${JOB_ID}_${CASE}_DM_25km_${YYA3}-${YYE}_tasmax.nc ${JOB_ID}_${CASE}_DM_25km_${YYI}-${YYE}_tasmax.nc
#cdo mergetime ${JOB_ID}_${CASE}_DM_25km_${YYI}-${YYE1}_tasmin.nc ${JOB_ID}_${CASE}_DM_25km_${YYA2}-${YYE2}_tasmin.nc ${JOB_ID}_${CASE}_DM_25km_${YYA3}-${YYE}_tasmin.nc ${JOB_ID}_${CASE}_DM_25km_${YYI}-${YYE}_tasmin.nc
#cdo mergetime ${JOB_ID}_${CASE}_DM_25km_${YYI}-${YYE1}_pr.nc ${JOB_ID}_${CASE}_DM_25km_${YYA2}-${YYE2}_pr.nc ${JOB_ID}_${CASE}_DM_25km_${YYA3}-${YYE}_pr.nc ${JOB_ID}_${CASE}_DM_25km_${YYI}-${YYE}_pr.nc
#cdo mergetime ${JOB_ID}_${CASE}_DM_25km_${YYI}-${YYE1}_prsn.nc ${JOB_ID}_${CASE}_DM_25km_${YYA2}-${YYE2}_prsn.nc ${JOB_ID}_${CASE}_DM_25km_${YYA3}-${YYE}_prsn.nc ${JOB_ID}_${CASE}_DM_25km_${YYI}-${YYE}_prsn.nc

gzip *.nc
mv *.gz ${OPATH}

exit



