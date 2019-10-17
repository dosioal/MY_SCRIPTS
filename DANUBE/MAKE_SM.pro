PRO MAKE_SM
DEVICE,decomposed=0

; MAKE SEANSONAL MEANS- YEARLY
;=============================================
RES='0.44'
lon1='-21.5' 
lon2='15.56'  
lat1='-20.6800003'  
lat2='20.9999996'
;=============================================

VAR='T'
;VAR='Tmax'
;VAR='Tmin'
;VAR='Ptot'

;STAT='SM'
STAT='PDF'

;STAT='HWFI' ;ONLY IF VAR = tas

;STAT='HWDI' ;ONLY IF VAR = tasmax
;STAT='Tx90p' ;ONLY IF VAR = tasmax

;STAT='Tn90p' ;ONLY IF VAR = tasmin
;STAT='Tn10p' ;ONLY IF VAR = tasmin

;STAT='CDD' ;ONLY IF VAR = Ptot
;STAT='CWD' ;ONLY IF VAR = Ptot
;STAT='R90p' ;ONLY IF VAR = Ptot
;STAT='R90ptot' ;ONLY IF VAR = Ptot

IF STAT EQ 'Tn10p' THEN PRC='10PRCT' ELSE PRC='90PRCT'

SEAS='DJF'
;SEAS='JJA'

IF VAR EQ 'Ptot' THEN bins='1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200' $
ELSE BEGIN
IF SEAS EQ 'DJF' THEN bins='-20,-19,-18,-17,-16,-15,-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30'
IF SEAS EQ 'JJA' THEN bins='0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50'
ENDELSE
print,'BINS=',bins

home_data='/media/disk_BC/POSTPROC/BC_Regional/'
CONSTR='1961_1990'

MODEL_ID=['METO-HC_HadRM3Q0','MPI-M-REMO','C4IRCA3','ETHZ-CLM','KNMI-RACMO2','SMHIRCA','SMHIRCA','SMHIRCA','DMI-HIRHAM5','DMI-HIRHAM5','DMI-HIRHAM5','CNRM-RM5.1']
CASE_ID=['A1B_HadCM3Q0','SCN_ECHAM5','A1B_HadCM3Q16','SCN_HadCM3Q0','A1B_ECHAM5-r3','A1B_BCM','A1B_HadCM3Q3','A1B_ECHAM5-r3','A1B_BCM','A1B_ARPEGE','A1B_ECHAM5','SCN_ARPEGE']

NGCM=N_ELEMENTS(MODEL_ID)

YSTART='1981'
YSTOP=['2010','2010','2010','2010','2010','2010','2010','2010','2010','2010','2010','2010']

YSTART='2021'
YSTOP=['2050','2050','2050','2050','2050','2050','2050','2050','2050','2050','2050','2050']

;YSTART='2071'
;;;;;;YSTOP=['2098','2100','2098','2098','2100','2099','2098','2100','2098','2099','2099','2099']
;YSTOP=['2098','2100','2098','2098','2100','2100','2100','2100','2099','2099','2099','2099']

;=============
;LANDMASK_COM AND DIMENSION
;=============
LM_FILE='/media/disk_BC/DATA/ENSOBS/ENS_MODELS/read_in_KNMI-RACMO2_1961-1990.nc'

my_cutoff=0.5
fileID = NCDF_Open(LM_FILE)
varID = NCDF_VarID(fileID,'tg')
varInfo = NCDF_VarInq(fileID, varID)
dimIDs = varInfo.dim
nDims = N_Elements(dimIDs)
dims_a = IntArr(nDims)
FOR j=0,nDims-1 DO BEGIN &$
NCDF_DimInq, fileID, dimIDs[j], dname, dsize &$
dims_a[j] = dsize &$
ENDFOR
nx=dims_a(0)
ny=dims_a(1)
landmask_com=dblarr(nx,ny)
;NCDF_VARGET,fileID,'tg',landmask_com,count=[nx,ny],offset=[0,0]
NCDF_VARGET,fileID,varId,landmask_com;,count=[nx,ny];,offset=[0,0]
NCDF_CLOSE, fileID
help,landmask_com
print,nx,ny

;===============================================
;CLM-GCM
;===============================================

DIR_CLM_GCM=STRARR(NGCM)
FILE_CLM_GCM=STRARR(NGCM)
DIR_GCM=STRARR(NGCM)
FILE_GCM=STRARR(NGCM)

FOR im=0,NGCM-1 DO BEGIN ;GCM LOOP

GRID_FILE='/media/disk_BC/POSTPROC/BC_Regional/GRID_EU_022_'+MODEL_ID(im)+'.txt'
print,'MODEL= ',MODEL_ID(im)

DIR_DATA=home_data+MODEL_ID(im)+'_'+CASE_ID(im)+'_EOBS_1961-1990_1961-2100/finalOutputData/'

DIR_WORK='./work/'
spawn,'mkdir '+DIR_DATA+'/PP'
DIR_PP=DIR_DATA+'/PP'
spawn,'mkdir '+DIR_DATA+'/PP/'+STAT
DIR_SM=DIR_DATA+'/PP/'+STAT
spawn,'rm -f ./work/*'
DIR_PERC=DIR_DATA+'PERC/'

;GOTO,JUMP10
YEAR_1=FIX(YSTART)

WHILE YEAR_1 LT YSTOP(im) DO BEGIN;LOOP OF 30 YEAR
YEAR_2=YEAR_1+9
IF YEAR_2 GT YSTOP(im) THEN YEAR_2=FIX(YSTOP(im))
print,YEAR_1,' ',YEAR_2,' ',YSTOP(im)

IF YEAR_1 LT 1991 THEN BEGIN
FILE_CLM_GCM(im)=DIR_DATA+VAR+'_BCed_1961_1990_1961_1990'
print,'FILE= ',FILE_CLM_GCM(im)
;PREPARE DATA
spawn,'gunzip '+DIR_CLM_GCM(im)+'/'+FILE_CLM_GCM(im)+'.nc.gz'
;SELECT SEASON FOR DECADES
print,'SELECTING SEASONS'
spawn,'cdo selseas,'+SEAS+' '+FILE_CLM_GCM(im)+'.nc '+DIR_WORK+VAR+'_BCed_1961_1990_1961_1990_'+SEAS+'.nc'
;SPLITTING YEAR
spawn,'cdo splityear '+DIR_WORK+VAR+'_BCed_1961_1990_1961_1990_'+SEAS+'.nc '+DIR_WORK+'/test_'+SEAS+'_'
ENDIF ELSE BEGIN
FILE_CLM_GCM(im)=DIR_DATA+VAR+'_BCed_1961_1990_'+STRMID(YEAR_1,4)+'_'+STRMID(YEAR_2,4)
print,'FILE= ',FILE_CLM_GCM(im)
YI=FIX(STRMID(YEAR_1,4,4))
;PREPARE DATA
spawn,'gunzip '+DIR_CLM_GCM(im)+'/'+FILE_CLM_GCM(im)+'.nc.gz'
;SELECT SEASON FOR DECADES
print,'SELECTING SEASONS'
spawn,'cdo selseas,'+SEAS+' '+FILE_CLM_GCM(im)+'.nc '+DIR_WORK+VAR+'_BCed_1961_1990_'+STRMID(YEAR_1,4)+'_'+STRMID(YEAR_2,4)+'_'+SEAS+'.nc'
;SPLITTING YEAR
spawn,'cdo splityear '+DIR_WORK+VAR+'_BCed_1961_1990_'+STRMID(YEAR_1,4)+'_'+STRMID(YEAR_2,4)+'_'+SEAS+'.nc '+DIR_WORK+'/test_'+SEAS+'_'
ENDELSE

YI=FIX(STRMID(YEAR_1,4,4))
ntt=9
print,'YI=',YI
IF MODEL_ID(im) EQ 'SMHIRCA'  AND YI EQ 2091 THEN ntt=7  ; FOR SMHI  AND DMI ONLY UNTIL 2098!!!
IF MODEL_ID(im) EQ 'DMI-HIRHAM5' AND YI EQ 2091 THEN ntt=7  ; FOR SMHI  AND DMI ONLY UNTIL 2098!!!
print,'ntt=',ntt
FOR iy=0,ntt DO BEGIN ; 10 year cycle
FILE=VAR+'_BCed_'+MODEL_ID(im)+'_'+CASE_ID(im)+'_'+STRMID(YI,4)
print,'YEAR= ',STRMID(YI,4)

;REMAPPING ON COMMON GRID
spawn,'cdo remapcon,'+GRID_FILE+' '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'.nc '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022.nc'
spawn,'ncks -d lon,'+lon1+','+lon2+' -d lat,'+lat1+','+lat2+' '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022.nc '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM.nc'

;CALCULATING STATISTICS EVERY YEAR
FILE_PERC=VAR+'_BCed_'+MODEL_ID(im)+'_'+CASE_ID(im)+'_1961_1990_022_'+PRC+'_COM_'+SEAS+'.nc'
CASE STAT of
'SM': BEGIN
print,'CREATING SM: ',FILE
spawn,'cdo timmean '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM.nc '+DIR_SM+'/'+FILE+'_'+SEAS+'_'+STAT+'.nc' 
END
'HWDI': BEGIN ; NEED MAKE_PERCENTILES FIRST!!!!
spawn,'cdo eca_hwdi,5,5 '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM.nc '+DIR_PERC+'/'+FILE_PERC+' '+DIR_SM+'/'+FILE+'_'+SEAS+'_'+STAT+'.nc'
END
'HWFI': BEGIN ; NEED MAKE_PERCENTILES FIRST!!!!
spawn,'cdo eca_hwfi,5 '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM.nc '+DIR_PERC+'/'+FILE_PERC+' '+DIR_SM+'/'+FILE+'_'+SEAS+'_'+STAT+'.nc'
END
'Tx90p': BEGIN ; NEED MAKE_PERCENTILES FIRST!!!!
spawn,'cdo eca_tx90p '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM.nc '+DIR_PERC+'/'+FILE_PERC+' '+DIR_SM+'/'+FILE+'_'+SEAS+'_'+STAT+'.nc'
END
'Tn90p': BEGIN ; NEED MAKE_PERCENTILES FIRST!!!!
spawn,'cdo eca_tn90p '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM.nc '+DIR_PERC+'/'+FILE_PERC+' '+DIR_SM+'/'+FILE+'_'+SEAS+'_'+STAT+'.nc'
END
'Tn10p': BEGIN ; NEED MAKE_PERCENTILES FIRST!!!!
spawn,'cdo eca_tn10p '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM.nc '+DIR_PERC+'/'+FILE_PERC+' '+DIR_SM+'/'+FILE+'_'+SEAS+'_'+STAT+'.nc'
END
'CDD': BEGIN
spawn,'cdo mulc,86400. '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM.nc '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM_mm.nc'
spawn,'cdo eca_cdd '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM_mm.nc '+DIR_SM+'/'+FILE+'_'+SEAS+'_'+STAT+'.nc' 
END
'CWD': BEGIN
spawn,'cdo mulc,86400. '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM.nc '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM_mm.nc'
spawn,'cdo eca_cwd '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM_mm.nc '+DIR_SM+'/'+FILE+'_'+SEAS+'_'+STAT+'.nc' 
END
'R90p': BEGIN ; NEED MAKE_PERCENTILES FIRST!!!!
spawn,'cdo mulc,86400. '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM.nc '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM_mm.nc'
spawn,'cdo eca_r90p '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM_mm.nc '+DIR_PERC+'/'+FILE_PERC+' '+DIR_SM+'/'+FILE+'_'+SEAS+'_'+STAT+'.nc'
END
'R90ptot': BEGIN ; NEED MAKE_PERCENTILES FIRST!!!!
spawn,'cdo mulc,86400. '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM.nc '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM_mm.nc'
spawn,'cdo eca_r90ptot '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM_mm.nc '+DIR_PERC+'/'+FILE_PERC+' '+DIR_SM+'/'+FILE+'_'+SEAS+'_'+STAT+'.nc'
END
'PDF': BEGIn
IF VAR EQ 'Ptot'  THEN BEGIN
spawn,'cdo mulc,86400. '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM.nc '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM_mm.nc'
spawn,'cdo histcount,'+bins+' '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM_mm.nc '+DIR_SM+'/'+FILE+'_'+SEAS+'_'+STAT+'.nc'
ENDIF ELSE BEGIN
spawn,'cdo addc,-273.14 '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM.nc '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM_C.nc'
spawn,'cdo histcount,'+bins+' '+DIR_WORK+'/test_'+SEAS+'_'+STRMID(YI,4)+'_022_COM_C.nc '+DIR_SM+'/'+FILE+'_'+SEAS+'_'+STAT+'.nc'
ENDELSE
ENd

ENDCASE

YI=YI+1
ENDFOR ;END 10 YEARS LOOP

YEAR_1=YEAR_1+10; loop of 10 years
ENDWHILE

;MAKE SINGLE FILE
;CHECK FOR N OF FILES TO BE MERGED
;spawn,'echo filecount=$(ls '+DIR_SM+'/'+VAR+'_BCed_'+MODEL_ID(im)+'_'+CASE_ID(im)+'_????_'+SEAS+'_'+STAT+'.nc 2>/dev/null | wc -l)'
spawn,'echo $(ls '+DIR_SM+'/'+VAR+'_BCed_'+MODEL_ID(im)+'_'+CASE_ID(im)+'_????_'+SEAS+'_'+STAT+'.nc 2>/dev/null | wc -l) > count.txt'
openr,1,'count.txt'
readf,1,fcount
close,1
print,MODEL_ID(im)+'_'+CASE_ID(im),fcount
spawn,'rm -f count.txt'
IF fcount LT 25 THEN STOP ELSE $
spawn,'cdo mergetime '+DIR_SM+'/'+VAR+'_BCed_'+MODEL_ID(im)+'_'+CASE_ID(im)+'_????_'+SEAS+'_'+STAT+'.nc '+DIR_SM+'/'+VAR+'_BCed_'+MODEL_ID(im)+'_'+CASE_ID(im)+'_'+YSTART+'-'+YSTOP(im)+'_'+SEAS+'_'+STAT+'.nc'

spawn,'rm -f '+DIR_SM+'/'+VAR+'_BCed_'+MODEL_ID(im)+'_'+CASE_ID(im)+'_????_'+SEAS+'_'+STAT+'.nc'
spawn,'rm -f '+DIR_WORK+'/*'


ENDFOR ; END GCM

STOP
END
