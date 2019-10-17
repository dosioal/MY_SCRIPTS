#!/bin/ksh
set -ex

typeset -Z2 MMA MME DDA DDE HHA HHE
typeset -Z4 YYA YYE YYN

#JOB_ID=CNRM-RM4.5 #ALADIN # VEDI SOTTO
JOB_ID=DMI-HIRHAM # VEDI SOTTO pr
#JOB_ID=ETHZ-CLM
#JOB_ID=GKSS-CLM #---> SEE BELOW
#JOB_ID=INMRCA3
#JOB_ID=KNMI-RACMO2
#JOB_ID=METNOHIRHAM
#JOB_ID=METO-HC_HadRM3.0
#JOB_ID=MPI-M-REMO   # NB nc ---> nc AND SEE BELOW
#JOB_ID=SMHIRCA
#JOB_ID=UCLM-PROMES # VEDI SOTTO pr

#JOB_ID=ICTP-REGCM3


HOME_DIR=/media/ext_disk/DATA/ENSEMBLES
WORK_DIR=${HOME_DIR}/${JOB_ID}
IPATH=${HOME_DIR}/${JOB_ID}/input
OPATH=${HOME_DIR}/${JOB_ID}/output

cd ${HOME_DIR}
if [ ! -d ${JOB_ID} ] ; then
	mkdir ${JOB_ID}
fi
cd ${WORK_DIR}
if [ ! -d output ] ; then
	mkdir output
fi
if [ ! -d work ] ; then
	mkdir work
fi

YYA=1977
MMA=01
YYA0=1977

YYE=1980
MME=12

cd ${WORK_DIR}/work
rm -f *
#cp ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_* ./
set +e
gunzip ${IPATH}/*.nc.gz

#DMI and UCLM
cdo expr,'pr=pr/3600./24.;'  ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_pr_ORIG.nc ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_pr.nc

#DMI and CNRM
cdo expr,'hfss=hfss*(-1);'  ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_hfss_ORIG.nc ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_hfss.nc
cdo expr,'hfls=hfls*(-1);'  ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_hfls_ORIG.nc ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_hfls.nc

#GKSS
#ncrename -v hfls_neu,hfls  ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_hfls_ORIG.nc ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_hfls.nc

cdo splityear  ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_tas.nc ${JOB_ID}_CTL_ERA40_DM_50km_tas_
cdo splityear  ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_as.nc ${JOB_ID}_CTL_ERA40_DM_50km_as_
cdo splityear  ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_ps.nc ${JOB_ID}_CTL_ERA40_DM_50km_ps_
cdo splityear  ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_pr.nc ${JOB_ID}_CTL_ERA40_DM_50km_pr_
cdo splityear  ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_hfss.nc ${JOB_ID}_CTL_ERA40_DM_50km_hfss_
cdo splityear  ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_hfls.nc ${JOB_ID}_CTL_ERA40_DM_50km_hfls_
cdo splityear  ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_clt.nc ${JOB_ID}_CTL_ERA40_DM_50km_clt_
cdo splityear  ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_rss.nc ${JOB_ID}_CTL_ERA40_DM_50km_rss_
cdo splityear  ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_rls.nc ${JOB_ID}_CTL_ERA40_DM_50km_rls_
cdo splityear  ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_rsds.nc ${JOB_ID}_CTL_ERA40_DM_50km_rsds_
cdo splityear  ${IPATH}/${JOB_ID}_CTL_ERA40_DM_50km_1971-1980_rlds.nc ${JOB_ID}_CTL_ERA40_DM_50km_rlds_

#UCLM
#while [ ${YYA} -le ${YYE} ]
#do
#ncks -v time_bnds,tas ${JOB_ID}_CTL_ERA40_DM_50km_tas_${YYA}.nc test_tas_${YYA}.nc 
#ncks -v time_bnds,as ${JOB_ID}_CTL_ERA40_DM_50km_as_${YYA}.nc test_as_${YYA}.nc 
#ncks -v time_bnds,ps ${JOB_ID}_CTL_ERA40_DM_50km_ps_${YYA}.nc test_ps_${YYA}.nc 
#ncks -v time_bnds,pr ${JOB_ID}_CTL_ERA40_DM_50km_pr_${YYA}.nc test_pr_${YYA}.nc 
#ncks -v time_bnds,hfss ${JOB_ID}_CTL_ERA40_DM_50km_hfss_${YYA}.nc test_hfss_${YYA}.nc 
#ncks -v time_bnds,hfls ${JOB_ID}_CTL_ERA40_DM_50km_hfls_${YYA}.nc test_hfls_${YYA}.nc 
#ncks -v time_bnds,clt ${JOB_ID}_CTL_ERA40_DM_50km_clt_${YYA}.nc test_clt_${YYA}.nc 
#ncks -v time_bnds,rss ${JOB_ID}_CTL_ERA40_DM_50km_rss_${YYA}.nc test_rss_${YYA}.nc 
#ncks -v time_bnds,rls ${JOB_ID}_CTL_ERA40_DM_50km_rls_${YYA}.nc test_rls_${YYA}.nc 
#ncks -v time_bnds,rsds ${JOB_ID}_CTL_ERA40_DM_50km_rsds_${YYA}.nc test_rsds_${YYA}.nc 
#ncks -v time_bnds,rsls ${JOB_ID}_CTL_ERA40_DM_50km_rsls_${YYA}.nc test_rsls_${YYA}.nc 
#  (( YYA=YYA+1 ))
#done
#YYA=${YYA0}
# END UCLM

while [ ${YYA}${MMA} -le ${YYE}${MME} ]
do
#UCLM
#cdo splitmon test_tas_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_tas_${YYA}_
#cdo splitmon test_as_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_as_${YYA}_
#cdo splitmon test_ps_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_ps_${YYA}_
#cdo splitmon test_pr_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_pr_${YYA}_
#cdo splitmon test_hfss_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_hfss_${YYA}_
#cdo splitmon test_hfls_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_hfls_${YYA}_
#cdo splitmon test_clt_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_clt_${YYA}_
#cdo splitmon test_rss_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_rss_${YYA}_
#cdo splitmon test_rls_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_rls_${YYA}_
#cdo splitmon test_rsds_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_rsds_${YYA}_
#cdo splitmon test_rsls_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_rsls_${YYA}_
#cdo merge ${JOB_ID}_*_${YYA}_${MMA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_${YYA}_${MMA}.nc
#cp  ${JOB_ID}_CTL_ERA40_DM_50km_${YYA}_${MMA}.nc ${OPATH}/lfdd${YYA}${MMA}.nc

#MPI-REMO
#cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_tas_${YYA}.nc2 ${JOB_ID}_CTL_ERA40_DM_50km_tas_${YYA}_
#cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_as_${YYA}.nc2 ${JOB_ID}_CTL_ERA40_DM_50km_as_${YYA}_
#cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_ps_${YYA}.nc2 ${JOB_ID}_CTL_ERA40_DM_50km_ps_${YYA}_
#cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_pr_${YYA}.nc2 ${JOB_ID}_CTL_ERA40_DM_50km_pr_${YYA}_
#cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_hfss_${YYA}.nc2 ${JOB_ID}_CTL_ERA40_DM_50km_hfss_${YYA}_
#cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_hfls_${YYA}.nc2 ${JOB_ID}_CTL_ERA40_DM_50km_hfls_${YYA}_
#cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_clt_${YYA}.nc2 ${JOB_ID}_CTL_ERA40_DM_50km_clt_${YYA}_
#cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_rss_${YYA}.nc2 ${JOB_ID}_CTL_ERA40_DM_50km_rss_${YYA}_
#cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_rls_${YYA}.nc2 ${JOB_ID}_CTL_ERA40_DM_50km_rls_${YYA}_
#cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_rsds_${YYA}.nc2 ${JOB_ID}_CTL_ERA40_DM_50km_rsds_${YYA}_
#cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_rlds_${YYA}.nc2 ${JOB_ID}_CTL_ERA40_DM_50km_rlds_${YYA}_
#cdo merge ${JOB_ID}_*_${YYA}_${MMA}.nc2 ${JOB_ID}_CTL_ERA40_DM_50km_${YYA}_${MMA}.nc
#cdo -f nc copy ${JOB_ID}_CTL_ERA40_DM_50km_${YYA}_${MMA}.nc ${OPATH}/lfdd${YYA}${MMA}.nc

#OTHERS
cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_tas_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_tas_${YYA}_
cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_as_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_as_${YYA}_
cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_ps_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_ps_${YYA}_
cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_pr_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_pr_${YYA}_
cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_hfss_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_hfss_${YYA}_
cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_hfls_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_hfls_${YYA}_
cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_clt_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_clt_${YYA}_
cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_rss_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_rss_${YYA}_
cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_rls_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_rls_${YYA}_
cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_rsds_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_rsds_${YYA}_
cdo splitmon  ${JOB_ID}_CTL_ERA40_DM_50km_rlds_${YYA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_rlds_${YYA}_
cdo merge ${JOB_ID}_*_${YYA}_${MMA}.nc ${JOB_ID}_CTL_ERA40_DM_50km_${YYA}_${MMA}.nc
cp  ${JOB_ID}_CTL_ERA40_DM_50km_${YYA}_${MMA}.nc ${OPATH}/lfdd${YYA}${MMA}.nc

 (( MMA=MMA+1 ))

if [ ${MMA} -gt 12 ]
then
        (( YYA=YYA+1 ))
         (( MMA=1 ))

fi
done

rm -f ${WORK_DIR}/work/*
gzip ${IPATH}/${JOB_ID}_CTL_ERA40_DM*.nc

exit

