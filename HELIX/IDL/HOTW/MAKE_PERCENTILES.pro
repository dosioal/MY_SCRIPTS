PRO MAKE_PERCENTILES
DEVICE,decomposed=0

; MAKE PERCENTILES_OVER REFERNCE PERIOD 1961-1990
;=============================================
RES='0.44'
;lon1 = '-29.0400009' 
lon1 = '-29.4400009' 
;lon2 = '64.6800003' 
lon2 = '64.8800003' 
;lat1 = '-50.1599998'  
lat1 = '-50.5599998'  
;lat2 = '46.6399994' 
lat2 = '46.6399994' 
;SIZE 0.44
nx_o = 194
ny_o = 201
lon1='-24.94' 
lon2='60.48'  
lat1='-45.76'  
lat2='42.64'
;=============================================

VAR_CLM='tasmax'
;VAR_CLM='tasmin'

RCM=['CCLM4-8-17','DMI-HIRHAM5','KNMI-RACMO22T','SMHI-RCA4']
NRCM=N_ELEMENTS(RCM)

SCEN='historical' ;1951-2005' 
YEAR_INI=19610101 ; CLIM
YEAR_END=19901231

SCEN='rcp45' ;1951-2005' 
SCEN='rcp85' ;1951-2005' 

;YEAR_INI=19810101 ; REF
;YEAR_END=20101231

YEAR_INI=20710101 ; SCEN3
YEAR_END=21001231
;===============================================
;CLM-GCM
;===============================================
FOR ir=0,NRCM-1 DO BEGIN ;RCM LOOP
 IF RCM(ir) EQ 'CCLM4-8-17' THEN GCM=['EC-EARTH','HadGEM2-ES','CNRM-CM5','MPI-ESM-LR']
 IF RCM(ir) EQ 'SMHI-RCA4'  THEN GCM=['ICHEC-EC-EARTH','MOHC-HadGEM2-ES','CNRM-CERFACS-CNRM-CM5','MPI-M-MPI-ESM-LR','CCCma-CanESM2','MIROC-MIROC5','NCC-NorESM1-M','IPSL-IPSL-CM5A-MR','NOAA-GFDL-GFDL-ESM2M']
 IF RCM(ir) EQ 'DMI-HIRHAM5' OR RCM(ir) EQ 'KNMI-RACMO22T'  THEN GCM=['ICHEC-EC-EARTH']
 NGCM=N_ELEMENTS(GCM)

FOR im=0,NGCM-1 DO BEGIN ;GCM LOOP

print,'MODEL= ',GCM(im)+RCM(ir)
print,'YSTART ',YEAR_INI
print,'YEAR_END ',YEAR_END

DIR_CLM_GCM_H='/media/NAS/CORDEX-AFRICA/RCM/'+RCM(ir)+'/'+GCM(im)+'/historical/day/'+VAR_CLM
DIR_CLM_GCM='/media/NAS/CORDEX-AFRICA/RCM/'+RCM(ir)+'/'+GCM(im)+'/'+SCEN+'/day/'+VAR_CLM

spawn,'mkdir '+DIR_CLM_GCM+'/WORK'
DIR_WORK=DIR_CLM_GCM+'/WORK'
spawn,'mkdir '+DIR_CLM_GCM+'/PP'
spawn,'mkdir '+DIR_CLM_GCM+'/PP/PERC'
DIR_PERC=DIR_CLM_GCM+'/PP/PERC' ;OUTPUT DIR
spawn,'rm -f '+DIR_CLM_GCM+'/WORK/*'

IF GCM(im) EQ 'HadGEM2-ES' OR GCM(im) EQ 'MOHC-HadGEM2-ES' THEN YEND='1230' ELSE YEND='1231'

YEAR_1=YEAR_INI
WHILE YEAR_1 LT YEAR_END DO BEGIN
IF GCM(im) EQ 'HadGEM2-ES' OR GCM(im) EQ 'MOHC-HadGEM2-ES' THEN YEAR_2=YEAR_1+41129 ELSE YEAR_2=YEAR_1+41130
IF YEAR_2 GT YEAR_END THEN YEAR_2=YEAR_END
IF (GCM(im) EQ 'HadGEM2-ES' OR GCM(im) EQ 'MOHC-HadGEM2-ES') AND YEAR_1 EQ 20960101 THEN YEAR_2=20991130
print,YEAR_1,' ',YEAR_2


RRR='r1i1p1'
IF RCM(ir) EQ 'CCLM4-8-17'  AND GCM(im) EQ 'EC-EARTH' THEN  RRR='r12i1p1'
IF RCM(ir) EQ 'DMI-HIRHAM5'  AND GCM(im) EQ 'ICHEC-EC-EARTH' THEN  RRR='r3i1p1'
IF RCM(ir) EQ 'SMHI-RCA4'  AND GCM(im) EQ 'ICHEC-EC-EARTH' THEN  RRR='r12i1p1'

IF SCEN EQ 'historical' OR YEAR_END NE '20101231 'THEN BEGIN
FILE_CLM_GCM=VAR_CLM+'_AFR-44_'+GCM(im)+'_'+SCEN+'_'+RRR+'_'+RCM(ir)+'_v1_day_'+STRMID(YEAR_1,4)+'-'+STRMID(YEAR_2,4) 
spawn,'gunzip '+DIR_CLM_GCM+'/'+FILE_CLM_GCM+'.nc.gz'
spawn,'cp '+DIR_CLM_GCM+'/'+FILE_CLM_GCM+'.nc '+DIR_WORK
ENDIF
IF SCEN NE 'historical' AND YEAR_END EQ '20101231' THEN BEGIN ;copy file_historical_1981-2005 and rename to scen
FILE_CLM_GCM=VAR_CLM+'_AFR-44_'+GCM(im)+'_historical_'+RRR+'_'+RCM(ir)+'_v1_day_'+STRMID(YEAR_1,4)+'-'+STRMID(YEAR_2,4) 
spawn,'gunzip '+DIR_CLM_GCM_H+'/'+FILE_CLM_GCM+'.nc.gz'
spawn,'cp '+DIR_CLM_GCM_H+'/'+FILE_CLM_GCM+'.nc '+DIR_WORK+'/'+VAR_CLM+'_AFR-44_'+GCM(im)+'_'+SCEN+'_'+RRR+'_'+RCM(ir)+'_v1_day_'+STRMID(YEAR_1,4)+'-'+STRMID(YEAR_2,4)+'.nc'
spawn,'gunzip '+DIR_CLM_GCM+'/'+VAR_CLM+'_AFR-44_'+GCM(im)+'_'+SCEN+'_'+RRR+'_'+RCM(ir)+'_v1_day_20060101-2010'+YEND+'.nc,gz'
spawn,'cp '+DIR_CLM_GCM+'/'+VAR_CLM+'_AFR-44_'+GCM(im)+'_'+SCEN+'_'+RRR+'_'+RCM(ir)+'_v1_day_20060101-2010'+YEND+'.nc '+DIR_WORK
ENDIF

FILE_CLM_GCM=VAR_CLM+'_AFR-44_'+GCM(im)+'_'+SCEN+'_'+RRR+'_'+RCM(ir)+'_v1_day_'+STRMID(YEAR_1,4)+'-'+STRMID(YEAR_2,4)
print,'FILE= ',FILE_CLM_GCM

;SELECT SEASON FOR DECADES
print,'SELECTING SEASONS'
spawn,'cdo selmon,7,8,9 '+DIR_WORK+'/'+FILE_CLM_GCM+'.nc '+DIR_WORK+'/test'+STRMID(YEAR_1,4)+'-'+STRMID(YEAR_2,4)+'_JAS.nc'
spawn,'cdo selmon,1,2,3 '+DIR_WORK+'/'+FILE_CLM_GCM+'.nc '+DIR_WORK+'/test'+STRMID(YEAR_1,4)+'-'+STRMID(YEAR_2,4)+'_JFM.nc'
print,'creating',DIR_WORK+'/test'+STRMID(YEAR_1,4)+'-'+STRMID(YEAR_2,4)+'_JFM.nc'

YEAR_1=YEAR_1+50000
ENDWHILE

JUMP10:

FILE_CLM_GCM=VAR_CLM+'_AFR-44_'+GCM(im)+'_'+SCEN+'_'+RCM(ir)+'_v1_day_'+STRMID(YEAR_INI,4)+'-'+STRMID(YEAR_END,4) 
print,'FILE TO MERGE:'
spawn,'ls -al '+DIR_WORK+'/test*_JFM.nc'

spawn,'cdo mergetime '+DIR_WORK+'/test*_JFM.nc '+DIR_WORK+'/'+FILE_CLM_GCM+'_JFM.nc'
spawn,'cdo mergetime '+DIR_WORK+'/test*_JAS.nc '+DIR_WORK+'/'+FILE_CLM_GCM+'_JAS.nc'

print,'CALC STATS'

; PERCENTILES
print,'PERCENTILES'

IF VAR_CLM EQ 'pr' THEN BEGIN
spawn,'cdo mulc,86400. '+DIR_WORK+'/'+FILE_CLM_GCM+'_JFM.nc '+DIR_WORK+'/'+FILE_CLM_GCM+'_JFM_mm.nc'
spawn,'cdo mulc,86400. '+DIR_WORK+'/'+FILE_CLM_GCM+'_JAS.nc '+DIR_WORK+'/'+FILE_CLM_GCM+'_JAS_mm.nc'
spawn,'mv '+DIR_WORK+'/'+FILE_CLM_GCM+'_JFM_mm.nc '+DIR_WORK+'/'+FILE_CLM_GCM+'_JFM.nc'
spawn,'mv '+DIR_WORK+'/'+FILE_CLM_GCM+'_JAS_mm.nc '+DIR_WORK+'/'+FILE_CLM_GCM+'_JAS.nc'
ENDIF
;PERC WITH RUNNING 5day WINDOW
spawn,'cdo ydrunmin,5 '+DIR_WORK+'/'+FILE_CLM_GCM+'_JAS.nc '+DIR_WORK+'/minfile.nc'
spawn,'cdo ydrunmax,5 '+DIR_WORK+'/'+FILE_CLM_GCM+'_JAS.nc '+DIR_WORK+'/maxfile.nc'
spawn,'cdo ydrunpctl,10,5 '+DIR_WORK+'/'+FILE_CLM_GCM+'_JAS.nc '+DIR_WORK+'/minfile.nc '+DIR_WORK+'/maxfile.nc '+DIR_PERC+'/'+FILE_CLM_GCM+'_JAS_10p.nc'
spawn,'cdo ydrunpctl,50,5 '+DIR_WORK+'/'+FILE_CLM_GCM+'_JAS.nc '+DIR_WORK+'/minfile.nc '+DIR_WORK+'/maxfile.nc '+DIR_PERC+'/'+FILE_CLM_GCM+'_JAS_50p.nc'
spawn,'cdo ydrunpctl,90,5 '+DIR_WORK+'/'+FILE_CLM_GCM+'_JAS.nc '+DIR_WORK+'/minfile.nc '+DIR_WORK+'/maxfile.nc '+DIR_PERC+'/'+FILE_CLM_GCM+'_JAS_90p.nc'
spawn,'cdo ydrunpctl,95,5 '+DIR_WORK+'/'+FILE_CLM_GCM+'_JAS.nc '+DIR_WORK+'/minfile.nc '+DIR_WORK+'/maxfile.nc '+DIR_PERC+'/'+FILE_CLM_GCM+'_JAS_95p.nc'
spawn,'cdo ydrunpctl,99,5 '+DIR_WORK+'/'+FILE_CLM_GCM+'_JAS.nc '+DIR_WORK+'/minfile.nc '+DIR_WORK+'/maxfile.nc '+DIR_PERC+'/'+FILE_CLM_GCM+'_JAS_99p.nc'

;PERC WITH RUNNING 5day WINDOW
spawn,'cdo ydrunmin,5 '+DIR_WORK+'/'+FILE_CLM_GCM+'_JFM.nc '+DIR_WORK+'/minfile.nc'
spawn,'cdo ydrunmax,5 '+DIR_WORK+'/'+FILE_CLM_GCM+'_JFM.nc '+DIR_WORK+'/maxfile.nc'
spawn,'cdo ydrunpctl,10,5 '+DIR_WORK+'/'+FILE_CLM_GCM+'_JFM.nc '+DIR_WORK+'/minfile.nc '+DIR_WORK+'/maxfile.nc '+DIR_PERC+'/'+FILE_CLM_GCM+'_JFM_10p.nc'
spawn,'cdo ydrunpctl,50,5 '+DIR_WORK+'/'+FILE_CLM_GCM+'_JFM.nc '+DIR_WORK+'/minfile.nc '+DIR_WORK+'/maxfile.nc '+DIR_PERC+'/'+FILE_CLM_GCM+'_JFM_50p.nc'
spawn,'cdo ydrunpctl,90,5 '+DIR_WORK+'/'+FILE_CLM_GCM+'_JFM.nc '+DIR_WORK+'/minfile.nc '+DIR_WORK+'/maxfile.nc '+DIR_PERC+'/'+FILE_CLM_GCM+'_JFM_90p.nc'
spawn,'cdo ydrunpctl,95,5 '+DIR_WORK+'/'+FILE_CLM_GCM+'_JFM.nc '+DIR_WORK+'/minfile.nc '+DIR_WORK+'/maxfile.nc '+DIR_PERC+'/'+FILE_CLM_GCM+'_JFM_95p.nc'
spawn,'cdo ydrunpctl,99,5 '+DIR_WORK+'/'+FILE_CLM_GCM+'_JFM.nc '+DIR_WORK+'/minfile.nc '+DIR_WORK+'/maxfile.nc '+DIR_PERC+'/'+FILE_CLM_GCM+'_JFM_99p.nc'
;
spawn,'rm -f '+DIR_WORK+'/*'
spawn,'gzip '+DIR_CLM_GCM_H+'/*.nc'
spawn,'gzip '+DIR_CLM_GCM+'/*.nc'
ENDFOR; END GCM
ENDFOR; END RCM
STOP
END
