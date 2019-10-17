PRO MAKE_PERCENTILES_YEAR
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

VAR_CLM='tas'
VAR_CLM='tasmax'
;VAR_CLM='tasmin'
;VAR_CLM='pr'

RCM=['CCLM4-8-17','DMI-HIRHAM5','KNMI-RACMO22T','SMHI-RCA4']
NRCM=N_ELEMENTS(RCM)

SCEN='historical' ;1951-2005' 

YEAR_INI=19810101 ; REF
YEAR_END=20101231
MONTH=['01','02','03','04','05','06','07','08','09','10','11','12']
NMONTHS=N_ELEMENTS(MONTH)

;===============================================
;CLM-GCM
;===============================================
FOR ir=0,NRCM-1 DO BEGIN ;RCM LOOP
 IF RCM(ir) EQ 'CCLM4-8-17' THEN GCM=['EC-EARTH','HadGEM2-ES','CNRM-CM5','MPI-ESM-LR']
 IF RCM(ir) EQ 'SMHI-RCA4'  THEN GCM=['ICHEC-EC-EARTH','MOHC-HadGEM2-ES','CNRM-CERFACS-CNRM-CM5','MPI-M-MPI-ESM-LR','CCCma-CanESM2','MIROC-MIROC5','NCC-NorESM1-M','IPSL-IPSL-CM5A-MR','NOAA-GFDL-GFDL-ESM2M']
 IF RCM(ir) EQ 'DMI-HIRHAM5' OR RCM(ir) EQ 'KNMI-RACMO22T'  THEN GCM=['ICHEC-EC-EARTH']
 NGCM=N_ELEMENTS(GCM)

FOR im=0,NGCM-1 DO BEGIN ;GCM LOOP

IF SCEN NE 'historical' THEN YEAR_END=21001231
IF (GCM(im) EQ 'MOHC-HadGEM2-ES' OR GCM(im) EQ 'HadGEM2-ES') AND SCEN NE 'historical' THEN YEAR_END=20991130 
IF (GCM(im) EQ 'MOHC-HadGEM2-ES' OR GCM(im) EQ 'HadGEM2-ES') AND SCEN EQ 'historical' THEN YEAR_END=20101230 ELSE YEAR_END=20101231
print,''
print,'===================================='
print,'MODEL= ',RCM(ir)+'-'+GCM(im)
print,'===================================='
print,'YSTART ',YEAR_INI
print,'YEND ',YEAR_END

DIR_CLM_GCM='/media/NAS/CORDEX-AFRICA/RCM/'+RCM(ir)+'/'+GCM(im)+'/'+SCEN+'/day/'+VAR_CLM
DIR_CLM_GCM_H='/media/NAS/CORDEX-AFRICA/RCM/'+RCM(ir)+'/'+GCM(im)+'/rcp45/day/'+VAR_CLM
spawn,'mkdir '+DIR_CLM_GCM+'/WORK'
DIR_WORK=DIR_CLM_GCM+'/WORK'
spawn,'mkdir '+DIR_CLM_GCM+'/PP'
spawn,'mkdir '+DIR_CLM_GCM+'/PP/PERC'
DIR_PERC=DIR_CLM_GCM+'/PP/PERC' ;OUTPUT DIR
spawn,'rm -f '+DIR_CLM_GCM+'/WORK/*'

YEAR_1=YEAR_INI
WHILE YEAR_1 LT YEAR_END DO BEGIN
IF GCM(im) EQ 'HadGEM2-ES' OR GCM(im) EQ 'MOHC-HadGEM2-ES' THEN YEAR_2=YEAR_1+41129 ELSE YEAR_2=YEAR_1+41130
IF YEAR_2 GT YEAR_END THEN YEAR_2=YEAR_END
IF (GCM(im) EQ 'HadGEM2-ES' OR GCM(im) EQ 'MOHC-HadGEM2-ES') AND YEAR_1 EQ 20960101 THEN YEAR_2=20991130
print,YEAR_1,' ',YEAR_2

ENS='r1i1p1'
IF RCM(ir) EQ 'CCLM4-8-17'  AND GCM(im) EQ 'EC-EARTH' THEN  ENS='r12i1p1'
IF RCM(ir) EQ 'DMI-HIRHAM5'  AND GCM(im) EQ 'ICHEC-EC-EARTH' THEN  ENS='r3i1p1'
IF RCM(ir) EQ 'SMHI-RCA4'  AND GCM(im) EQ 'ICHEC-EC-EARTH' THEN  ENS='r12i1p1'

FILE_CLM_GCM=VAR_CLM+'_AFR-44_'+GCM(im)+'_'+SCEN+'_'+ENS+'_'+RCM(ir)+'_v1_day_'+STRMID(YEAR_1,4)+'-'+STRMID(YEAR_2,4)
FILE_CLM_GCM_H=VAR_CLM+'_AFR-44_'+GCM(im)+'_rcp45_'+ENS+'_'+RCM(ir)+'_v1_day_20060101-'+STRMID(YEAR_END,4)
print,'FILE= ',FILE_CLM_GCM

spawn,'gzip '+DIR_CLM_GCM+'/'+FILE_CLM_GCM+'.nc'
spawn,'cp '+DIR_CLM_GCM+'/'+FILE_CLM_GCM+'.nc.gz '+DIR_WORK+'/'
IF SCEN eq 'historical' THEN spawn,'gzip '+DIR_CLM_GCM_H+'/'+FILE_CLM_GCM_H+'.nc'
IF SCEN eq 'historical' THEN spawn,'cp '+DIR_CLM_GCM_H+'/'+FILE_CLM_GCM_H+'.nc.gz '+DIR_WORK+'/'
print,'FILE= ',FILE_CLM_GCM

YEAR_1=YEAR_1+50000
ENDWHILE

JUMP10:

FILE_CLM_GCM=VAR_CLM+'_AFR-44_'+GCM(im)+'_'+SCEN+'_'+RCM(ir)+'_v1_day_'+STRMID(YEAR_INI,4)+'-'+STRMID(YEAR_END,4) 
spawn,'gunzip '+DIR_WORK+'/*.nc.gz'
print,'FILE TO MERGE:'
spawn,'ls -al '+DIR_WORK+'/*.nc'

spawn,'cdo mergetime '+DIR_WORK+'/*.nc '+DIR_WORK+'/'+FILE_CLM_GCM+'.nc'

print,'CALC STATS'

; PERCENTILES
print,'PERCENTILES'

spawn,'rm -f '+DIR_WORK+'/test*.*'
spawn,'cdo splitday '+DIR_WORK+'/'+FILE_CLM_GCM+'.nc '+DIR_WORK+'/test_'
FOR id=1,31 DO BEGIN
IF id LT 10 THEN spawn,'cdo splitmon '+DIR_WORK+'/test_0'+STRMID(STRCOMPRESS(id),1,2)+'.nc4 '+DIR_WORK+'/test_0'+STRMID(STRCOMPRESS(id),1,2)+'_' ELSE $
spawn,'cdo splitmon '+DIR_WORK+'/test_'+STRMID(STRCOMPRESS(id),1,2)+'.nc4 '+DIR_WORK+'/test_'+STRMID(STRCOMPRESS(id),1,2)+'_'
ENDFOR
FOR is=0,NMONTHS-1 DO BEGIN
FOR id=1,31 DO BEGIN
spawn,'rm -f '+DIR_WORK+'minfile/.nc'
spawn,'rm -f '+DIR_WORK+'maxfile/.nc'

IF MONTH(is) EQ '02' AND  id EQ 29  AND GCM(im) NE 'MOHC-HadGEM2-ES' THEN BEGIN ; FEB 29th!
spawn,'cdo ydrunmin,1 '+DIR_WORK+'/test_'+STRMID(STRCOMPRESS(id),1,2)+'_'+MONTH(is)+'.nc4 '+DIR_WORK+'/minfile.nc'
spawn,'cdo ydrunmax,1 '+DIR_WORK+'/test_'+STRMID(STRCOMPRESS(id),1,2)+'_'+MONTH(is)+'.nc4 '+DIR_WORK+'/maxfile.nc'
spawn,'cdo ydrunpctl,10,1 '+DIR_WORK+'/test_'+STRMID(STRCOMPRESS(id),1,2)+'_'+MONTH(is)+'.nc4 '+DIR_WORK+'/minfile.nc '+DIR_WORK+'/maxfile.nc '+DIR_WORK+'/test_2902_10p.nc'
spawn,'cdo ydrunpctl,90,1 '+DIR_WORK+'/test_'+STRMID(STRCOMPRESS(id),1,2)+'_'+MONTH(is)+'.nc4 '+DIR_WORK+'/minfile.nc '+DIR_WORK+'/maxfile.nc '+DIR_WORK+'/test_2902_90p.nc'
ENDIF ELSE BEGIN
IF id LT 10 THEN BEGIN
spawn,'cdo ydrunmin,3 '+DIR_WORK+'/test_0'+STRMID(STRCOMPRESS(id),1,2)+'_'+MONTH(is)+'.nc4 '+DIR_WORK+'/minfile.nc'
spawn,'cdo ydrunmax,3 '+DIR_WORK+'/test_0'+STRMID(STRCOMPRESS(id),1,2)+'_'+MONTH(is)+'.nc4 '+DIR_WORK+'/maxfile.nc'
spawn,'cdo ydrunpctl,90,3 '+DIR_WORK+'/test_0'+STRMID(STRCOMPRESS(id),1,2)+'_'+MONTH(is)+'.nc4 '+DIR_WORK+'/minfile.nc '+DIR_WORK+'/maxfile.nc '+DIR_WORK+'/test_0'+STRMID(STRCOMPRESS(id),1,2)+MONTH(is)+'_90p.nc'
ENDIF ELSE BEGIN
spawn,'cdo ydrunmin,3 '+DIR_WORK+'/test_'+STRMID(STRCOMPRESS(id),1,2)+'_'+MONTH(is)+'.nc4 '+DIR_WORK+'/minfile.nc'
 spawn,'cdo ydrunmax,3 '+DIR_WORK+'/test_'+STRMID(STRCOMPRESS(id),1,2)+'_'+MONTH(is)+'.nc4 '+DIR_WORK+'/maxfile.nc'
 spawn,'cdo ydrunpctl,90,3 '+DIR_WORK+'/test_'+STRMID(STRCOMPRESS(id),1,2)+'_'+MONTH(is)+'.nc4 '+DIR_WORK+'/minfile.nc '+DIR_WORK+'/maxfile.nc '+DIR_WORK+'/test_'+STRMID(STRCOMPRESS(id),1,2)+MONTH(is)+'_90p.nc'
ENDELSE
ENDELSE
IF id LT 10 THEN BEGIN ;SET COMMON TIME AXIS
spawn,'cdo settaxis,2008-'+MONTH(is)+'-0'+STRMID(STRCOMPRESS(id),1,2)+',12:00:00 '+DIR_WORK+'/test_0'+STRMID(STRCOMPRESS(id),1,2)+MONTH(is)+'_90p.nc '+DIR_WORK+'/test_0'+STRMID(STRCOMPRESS(id),1,2)+'_'+MONTH(is)+'_90p.nc'
ENDIF ELSE BEGIN
spawn,'cdo settaxis,2008-'+MONTH(is)+'-'+STRMID(STRCOMPRESS(id),1,2)+',12:00:00 '+DIR_WORK+'/test_'+STRMID(STRCOMPRESS(id),1,2)+MONTH(is)+'_90p.nc '+DIR_WORK+'/test_'+STRMID(STRCOMPRESS(id),1,2)+'_'+MONTH(is)+'_90p.nc'
ENDELSE

ENDFOR
ENDFOR
spawn,'rm -f '+DIR_PERC+'/'+FILE_CLM_GCM+'_90p.nc'
spawn,'cdo mergetime '+DIR_WORK+'/test_??_??_90p.nc '+DIR_PERC+'/'+FILE_CLM_GCM+'_90p.nc'


spawn,'rm -f '+DIR_WORK+'/*'
spawn,'gzip '+DIR_CLM_GCM_H+'/*.nc'
spawn,'gzip '+DIR_CLM_GCM+'/*.nc'
ENDFOR; END GCM
ENDFOR; END RCM
STOP
END
