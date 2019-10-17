PRO PLOT_SM

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
VAR='DT'
;VAR='Ptot'

STAT='SM'

;STAT='HWFI' ;ONLY IF VAR = tas

;STAT='HWDI' ;ONLY IF VAR = tasmax
;STAT='Tx90p' ;ONLY IF VAR = tasmax

TAT='Tn90p' ;ONLY IF VAR = tasmin
TAT='Tn10p' ;ONLY IF VAR = tasmin

;STAT='CDD' ;ONLY IF VAR = Ptot
;STAT='CWD' ;ONLY IF VAR = Ptot
;STAT='R90p' ;ONLY IF VAR = Ptot
;STAT='R90ptot' ;ONLY IF VAR = Ptot

CASE STAT OF
'SM': BEGIN
IF VAR EQ 'T' THEN BEGIN
thevariable='temp2' 
ENDIF ELSE BEGIN
IF VAR EQ 'Ptot' THEN thevariable='precip' ELSE thevariable=VAR
ENDELSE
IF VAR EQ 'DT' THEN thevariable='Tmax' 
END
'HWDI':thevariable='heat_wave_duration_index_wrt_mean_of_reference_period'
'HWFI':thevariable='warm_spell_days_index_wrt_90th_percentile_of_reference_period'
;'HWFI':thevariable='warm_spell_periods_per_time_period'
'Tx90p':thevariable='very_warm_days_percent_wrt_90th_percentile_of_reference_period'
'Tn90p':thevariable='warm_nights_percent_wrt_90th_percentile_of_reference_period'
'Tn10p':thevariable='cold_nights_percent_wrt_10th_percentile_of_reference_period'
'CDD':thevariable='consecutive_dry_days_index_per_time_period'
'CWD':thevariable='consecutive_wet_days_index_per_time_period'
'R90p':thevariable='wet_days_wrt_90th_percentile_of_reference_period' ;percent
'R90ptot':thevariable='precipitation_percent_due_to_R90p_days' ;percent
ENDCASE

PRC='90PRCT'

SEAS=['DJF','JJA']
;IF VAR EQ 'Tmax' AND STAT NE 'SM' THEN SEAS=['JJA']
;IF VAR EQ 'Tmin' AND STAT NE 'SM' THEN SEAS=['DJF']
NSEAS=N_ELEMENTS(SEAS)

home_data='/media/disk_BC/POSTPROC/BC_Regional/'
CONSTR='1961_1990'

MODEL_ID=['METO-HC_HadRM3Q0','MPI-M-REMO','C4IRCA3','ETHZ-CLM','KNMI-RACMO2','SMHIRCA','SMHIRCA','SMHIRCA','DMI-HIRHAM5','DMI-HIRHAM5','DMI-HIRHAM5','CNRM-RM5.1']
CASE_ID=['A1B_HadCM3Q0','SCN_ECHAM5','A1B_HadCM3Q16','SCN_HadCM3Q0','A1B_ECHAM5-r3','A1B_BCM','A1B_HadCM3Q3','A1B_ECHAM5-r3','A1B_BCM','A1B_ARPEGE','A1B_ECHAM5','SCN_ARPEGE']
NGCM=N_ELEMENTS(MODEL_ID)

YSTART_1='1981'
YSTOP_1=['2010','2010','2010','2010','2010','2010','2010','2010','2010','2010','2010','2010']

YSTART_2='2021'
YSTOP_2=['2050','2050','2050','2050','2050','2050','2050','2050','2050','2050','2050','2050']

YSTART_3='2071'
;YSTOP_3=['2098','2100','2098','2098','2100','2099','2098','2100','2098','2099','2099','2099']
YSTOP_3=['2098','2100','2098','2098','2100','2100','2100','2100','2099','2099','2099','2099']

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
NCDF_VARGET,fileID,'tg',landmask_com;,count=[nx,ny];,offset=[0,0]
lat=fltarr(nx,ny)
NCDF_VARGET,fileID,'Actual_latitude',lat
lon=fltarr(nx,ny)
NCDF_VARGET,fileID,'Actual_longitude',lon
NCDF_VARGET,fileID,'latitude',lat_r
NCDF_VARGET,fileID,'longitude',lon_r

NCDF_CLOSE, fileID
help,landmask_com
print,nx,ny

nx=nx-1
landmask_com=REFORM(landmask_com(1:nx,*))
help,landmask_com
VAR_SM_1=FLTARR(nx,ny,NGCM,NSEAS)
VAR_SM_2=FLTARR(nx,ny,NGCM,NSEAS)
VAR_SM_3=FLTARR(nx,ny,NGCM,NSEAS)

;===============================================
;CLM-GCM
;===============================================

DIR_CLM_GCM=STRARR(NGCM)
FILE_CLM_GCM=STRARR(NGCM)
DIR_GCM=STRARR(NGCM)
FILE_GCM=STRARR(NGCM)

FOR im=0,NGCM-1 DO BEGIN ;GCM LOOP
FOR iss=0,NSEAs-1 DO BEGIN ;SEAS

GRID_FILE='/media/disk_BC/POSTPROC/BC_Regional/GRID_EU_022_'+MODEL_ID(im)+'.txt'
print,'MODEL= ',MODEL_ID(im)

DIR_DATA=home_data+MODEL_ID(im)+'_'+CASE_ID(im)+'_EOBS_1961-1990_1961-2100/finalOutputData/'

DIR_SM=DIR_DATA+'/PP/'+STAT
DIR_PERC=DIR_DATA+'PERC/'

;MAKE SINGLE FILE
FILE_1=DIR_SM+'/'+VAR+'_BCed_'+MODEL_ID(im)+'_'+CASE_ID(im)+'_'+YSTART_1+'-'+YSTOP_1(im)+'_'+SEAS(iss)+'_'+STAT+'.nc'
FILE_2=DIR_SM+'/'+VAR+'_BCed_'+MODEL_ID(im)+'_'+CASE_ID(im)+'_'+YSTART_2+'-'+YSTOP_2(im)+'_'+SEAS(iss)+'_'+STAT+'.nc'
FILE_3=DIR_SM+'/'+VAR+'_BCed_'+MODEL_ID(im)+'_'+CASE_ID(im)+'_'+YSTART_3+'-'+YSTOP_3(im)+'_'+SEAS(iss)+'_'+STAT+'.nc'

spawn,'rm -f test*.nc'

spawn,'cdo timmean '+FILE_1+' test_1.nc'
print,'DATA FILE = ',FILE_1
fileID = NCDF_Open('test_1.nc')
varID = NCDF_VarID(fileID,thevariable)
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
;landmask_com=dblarr(nx,ny)
NCDF_VARGET,fileID,thevariable,WORK;,count=[nx,ny];,offset=[0,0]
NCDF_CLOSE, fileID
IF STAT EQ 'SM' THEN BEGIN
IF VAR EQ 'Ptot' THEN WORK=WORK*3600.*24
IF VAR EQ 'Tmax' OR VAR EQ 'Tmin' OR VAR EQ 'T'  THEN WORK=WORK-273.15
ENDIF
;IF STAT EQ 'HWFI' THEN WORK=WORK/93.*100. ; CONVERT IN PERCENT (mdays/n days per season)
;IF VAR EQ 'Ptot' THEN BEGIN
;WORK(WHERE(WORK GT 999))=!VALUES.F_NaN
;WORK(WHERE(WORK LE -999))=!VALUES.F_NaN
;ENDIF ELSE BEGIN
WORK(WHERE(WORK GT 999))=0.
WORK(WHERE(WORK LE -999))=0.
;ENDELSE
VAR_SM_1(*,*,im,iss)=WORK

spawn,'cdo timmean '+FILE_2+' test_2.nc'
print,'DATA FILE = ',FILE_2
fileID = NCDF_Open('test_2.nc')
varID = NCDF_VarID(fileID,thevariable)
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
;landmask_com=dblarr(nx,ny)
NCDF_VARGET,fileID,thevariable,WORK;,count=[nx,ny];,offset=[0,0]
NCDF_CLOSE, fileID
IF STAT EQ 'SM' THEN BEGIN
IF VAR EQ 'Ptot' THEN WORK=WORK*3600.*24
IF VAR EQ 'Tmax' OR VAR EQ 'Tmin' OR VAR EQ 'T'  THEN WORK=WORK-273.15
ENDIF
;IF STAT EQ 'HWFI' THEN WORK=WORK/93.*100. ; CONVERT IN PERCENT (mdays/n days per season)
;IF VAR EQ 'Ptot' THEN BEGIN
;WORK(WHERE(WORK GT 999))=!VALUES.F_NaN
;WORK(WHERE(WORK LE -999))=!VALUES.F_NaN
;ENDIF ELSE BEGIN
WORK(WHERE(WORK GT 999))=0.
WORK(WHERE(WORK LE -999))=0.
;ENDELSE
VAR_SM_2(*,*,im,iss)=WORK

spawn,'cdo timmean '+FILE_3+' test_3.nc'
print,'DATA FILE = ',FILE_3
fileID = NCDF_Open('test_3.nc')
varID = NCDF_VarID(fileID,thevariable)
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
;landmask_com=dblarr(nx,ny)
NCDF_VARGET,fileID,thevariable,WORK;,count=[nx,ny];,offset=[0,0]
IF STAT EQ 'SM' THEN BEGIN
IF VAR EQ 'Ptot' THEN WORK=WORK*3600.*24
IF VAR EQ 'Tmax' OR VAR EQ 'Tmin' OR VAR EQ 'T'  THEN WORK=WORK-273.15
ENDIF
;IF STAT EQ 'HWFI' THEN WORK=WORK/93.*100. ; CONVERT IN PERCENT (mdays/n days per season)
NCDF_CLOSE, fileID
;IF VAR EQ 'Ptot' THEN BEGIN
;WORK(WHERE(WORK GT 999))=!VALUES.F_NaN
;WORK(WHERE(WORK LE -999))=!VALUES.F_NaN
;ENDIF ELSE BEGIN
WORK(WHERE(WORK GT 999))=0.
WORK(WHERE(WORK LE -999))=0.
;ENDELSE
VAR_SM_3(*,*,im,iss)=WORK

ENDFOR ; END GCM
ENDFOR ;END SEASON

mask_dir='/media/disk_BC/POSTPROC/PESETA2/'
COUNTRY=['AUT','BGR','CZE','DEU','HUN','ROU','SVK','SVN','HRV','MDA','SCG','UKR','BIH']
POINTS=[140,190,130,608,159,405,83,33,108,58,175,1028,88]
COUNTRY=['AUT','BGR','CZE','HUN','ROU','SVK','SVN','HRV','MDA','SCG','BIH']
POINTS=[140,190,130,159,405,83,33,108,58,175,88]
N_COUNTRY=N_ELEMENTS(COUNTRY)

loadct,39
!p.color=0
!p.background=255
window,0,retain=2
!p.multi=0
contour,landmask_com,xstyle=1,ystyle=1,color=0,levels=[0,1]

land_country=fltarr(nx,ny,n_country)
land_country_dan=fltarr(nx,ny)

FOR ic=0,n_country-1 do begin ;START COUNTRY LOOP
file_mask=mask_dir+COUNTRY(ic)+'.txt'

print,'READING ',file_mask,' (',ic+1,'/',N_COUNTRY,')'
nc=POINTS(ic)
lon_m=fltarr(nc)
lat_m=fltarr(nc)
a=0.
b=0.
line=''
openr,3,file_mask
readf,3,line
print,line
for icc=0,nc-1 do begin &$
readf,3,a,b &$
lat_m(icc)=a &$
lon_m(icc)=b &$
endfor
close,3

;help,land_country
print,'COUNTRY DIM',min(lat_m),max(lat_m),min(lon_m),max(lon_m)
print,'DOMAIN DIM',min(lat),max(lat),min(lon),max(lon)

FOR icc=0,nc-1 DO BEGIN &$
FOR iy=0,ny-1 do begin &$
FOR ix=0,nx-1 do begin &$
IF lon_m(icc) EQ lon(ix,iy) AND lat_m(icc) EQ lat(ix,iy) THEN land_country(ix,iy,ic)=1 &$
ENDFOR &$
ENDFOR &$
ENDFOR

contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR ;COUNTRIES

FOR iy=0,ny-1 do begin 
FOR ix=0,nx-1 do begin 
land_country_dan(ix,iy)=TOTAL(land_country(ix,iy,*))
ENDFOR
ENDFOR
contour,land_country_dan(*,*),/overplot,colo=r250,levels=[0,1]

;=============================
;PLOTS
;=============================
loadct,39
!p.background=255
!p.color=0

CASE STAT Of
'SM': BEGIN
IF VAR EQ 'Ptot' THEN BEGIN
MinCol=20
MaxCol=160
rgrContValues_pr=[0,0.5,1,2,3,4,7,10,14,18,24]
rgrColValues_pr = INDGEN(N_ELEMENTS(rgrContValues_pr)-1) * (MaxCol - MinCol) / (N_ELEMENTS(rgrContValues_pr)-2) + MinCol
rgrColValues_pr=REVERSE(rgrColValues_pr)
rgrColValues_pr(0) =255 
rgrContValues_b=[-10,-4,-3,-2,-1.5,-1,-0.75,-0.5,-0.25,-0.1,0,0.1,0.25,.5,0.75,1,1.5,2,3,4,10]
rgrContValues_b=[-60,-50,-40,-30,-20,-15,-10,-5,5,10,15,20,30,40,50,60]  ;PERC
rgrColValues_b=INDGEN(N_ELEMENTS(rgrContValues_b)-1)*16+18
rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2)=255
rgrColValues_b=REVERSE(rgrColValues_b)
rgrContValues_b2=[0,0.25,.5,0.75,1,1.25,1.5,1.75,2,2.5,3,4,5] ;SD
rgrContValues_b2=[0,0.25,.5,0.75,1,1.25,1.5,1.75,2,2.5,3,4,5]*10. ;SD PERC
rgrColValues_b2=INDGEN(N_ELEMENTS(rgrContValues_b2)-1)*18+44
rgrColValues_b2(0)=255
ENDIF ELSE BEGIN ;T
MinCol=20
MaxCol=250
minvalue=-20
Maxvalue=40
ContourDist=5
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues_pr = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrColValues_pr = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
;rgrContValues_b=[-2,-1.5,-1,-0.5,-0.25,0,0.25,.5,1,1.5,2,3,4,5,7.5,10]
;rgrContValues_b=[-1,-0.5,-0.25,0,0.25,.5,1,1.5,2,2.5,3,3.5,4,4.5,5,10]
rgrContValues_b=[0,0.25,.5,1,1.5,2,2.5,3,3.5,4,4.5,5,10]
rgrColValues_b=INDGEN(N_ELEMENTS(rgrContValues_b)-1)*19+24 ;CHANGE
rgrColValues_b(0)=255
;rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2)=255
;rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2-1)=255
rgrContValues_b2=[0,0.25,.5,0.75,1,1.25,1.5,1.75,2,2.5,3,4,5] ;SD
rgrContValues_b2=FINDGEN(10)*0.2 ;SD
rgrColValues_b2=INDGEN(N_ELEMENTS(rgrContValues_b2)-1)*21+44
rgrColValues_b2(0)=255
ENDELSE
IF VAR eq 'DT' THEN BEGIN
MinCol=20
MaxCol=250
minvalue=0
Maxvalue=20
ContourDist=2
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues_pr = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrColValues_pr = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
rgrColValues_pr(0)=255
;rgrContValues_b=[-2,-1.5,-1,-0.5,-0.25,0,0.25,.5,1,1.5,2,3,4,5,7.5,10]
;rgrContValues_b=[-1,-0.5,-0.25,0,0.25,.5,1,1.5,2,2.5,3,3.5,4,4.5,5,10]
rgrContValues_b=[-1.5,-1.25,-1,-0.75,-.5,-.25,-0.1,0.1,0.25,.5,.75,1,1.25,1.5]
rgrColValues_b=INDGEN(N_ELEMENTS(rgrContValues_b)-1)*18+15 ;CHANGE
rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2)=255
rgrContValues_b2=[0,0.1,0.2,0.3,0.4,.5,0.75,1,1.25,1.5,2] ;SD
;rgrContValues_b2=FINDGEN(10)*0.2 ;SD
rgrColValues_b2=INDGEN(N_ELEMENTS(rgrContValues_b2)-1)*21+44
rgrColValues_b2(0)=255
ENDIF
END
'HWDI': BEGIN
MinCol=20
MaxCol=250
minvalue=0
Maxvalue=16
ContourDist=2
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues_pr = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrColValues_pr = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
rgrColValues_pr(0)=255
rgrContValues_b=INDGEN(10)
rgrColValues_b=INDGEN(N_ELEMENTS(rgrContValues_b)-1)*25 ;CHANGE
rgrColValues_b(0)=255
;rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2)=255
;rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2-1)=255
rgrContValues_b2=[0,1,2,3,4,5,8,12,16] ;SD
rgrColValues_b2=INDGEN(N_ELEMENTS(rgrContValues_b2)-1)*22+44
rgrColValues_b2(0)=255
END
'HWFI': BEGIN
MinCol=5
MaxCol=254
minvalue=0
Maxvalue=75
ContourDist=6
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues_pr = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrColValues_pr = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
rgrColValues_pr(0)=255
rgrContValues_b=INDGEN(14)*5.
rgrColValues_b=INDGEN(N_ELEMENTS(rgrContValues_b)-1)*19 ;CHANGE
rgrColValues_b(0)=255
;rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2)=255
;rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2-1)=255
rgrContValues_b2=[0,2,4,6,8,10,12,14,16,20] ;SD
rgrColValues_b2=INDGEN(N_ELEMENTS(rgrContValues_b2)-1)*22+44
rgrColValues_b2(0)=255
END
'Tx90p': BEGIN
MinCol=5
MaxCol=254
minvalue=0
Maxvalue=72
ContourDist=6
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues_pr = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrColValues_pr = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
rgrColValues_pr(0)=255
rgrContValues_b=INDGEN(12)*5.
rgrColValues_b=INDGEN(N_ELEMENTS(rgrContValues_b)-1)*22 ;CHANGE
rgrColValues_b(0)=255
;rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2)=255
;rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2-1)=255
rgrContValues_b2=[0,2,4,6,8,10,12,14,16,20] ;SD
rgrColValues_b2=INDGEN(N_ELEMENTS(rgrContValues_b2)-1)*23+44
rgrColValues_b2(0)=255
END
'Tn90p': BEGIN
MinCol=5
MaxCol=254
minvalue=0
Maxvalue=72
ContourDist=6
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues_pr = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrColValues_pr = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
rgrColValues_pr(0)=255
rgrContValues_b=INDGEN(12)*5.
rgrColValues_b=INDGEN(N_ELEMENTS(rgrContValues_b)-1)*22 ;CHANGE
rgrColValues_b(0)=255
;rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2)=255
;rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2-1)=255
rgrContValues_b2=[0,2,4,6,8,10,12,14,16,20] ;SD
rgrColValues_b2=INDGEN(N_ELEMENTS(rgrContValues_b2)-1)*23+44
rgrColValues_b2(0)=255
END
'Tn10p': BEGIN
MinCol=5
MaxCol=254
minvalue=0
Maxvalue=12
ContourDist=1
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues_pr = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrColValues_pr = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
rgrColValues_pr(0)=255
rgrContValues_b=INDGEN(13)-11
rgrColValues_b=INDGEN(N_ELEMENTS(rgrContValues_b)-1)*22 ;CHANGE
rgrColValues_b=REVERSE(rgrColValues_b)
rgrColValues_b(N_ELEMENTS(rgrColValues_b)-1)=255
;rgrContValues_b2=[0,2,4,6,8,10,12,14,16,20] ;SD
rgrContValues_b2=INDGEN(7) ;SD
rgrColValues_b2=INDGEN(N_ELEMENTS(rgrContValues_b2)-1)*33+44
rgrColValues_b2(0)=255
END
'CDD': BEGIN
MinCol=5
MaxCol=254
minvalue=0
Maxvalue=60
ContourDist=6
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues_pr = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrColValues_pr = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
rgrColValues_pr(0)=255
rgrContValues_b=INDGEN(17)
rgrContValues_b(N_ELEMENTS(rgrContValues_b)-1)=20
rgrColValues_b=INDGEN(N_ELEMENTS(rgrContValues_b)-1)*16 ;CHANGE
rgrColValues_b(0)=255
;rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2)=255
;rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2-1)=255
rgrContValues_b2=[0,2,4,6,8,10,12,14,16,20] ;SD
rgrColValues_b2=INDGEN(N_ELEMENTS(rgrContValues_b2)-1)*23+44
rgrColValues_b2(0)=255
END
'CWD': BEGIN
MinCol=5
MaxCol=254
minvalue=0
Maxvalue=20
ContourDist=2
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues_pr = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrColValues_pr = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
rgrColValues_pr(0)=255
rgrContValues_b=INDGEN(10)*1.-4.5
rgrColValues_b=INDGEN(N_ELEMENTS(rgrContValues_b)-1)*25+20 ;CHANGE
;rgrColValues_b(0)=255
rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2)=255
rgrContValues_b2=INDGEN(10) ;SD
rgrColValues_b2=INDGEN(N_ELEMENTS(rgrContValues_b2)-1)*23+44
rgrColValues_b2(0)=255
END
'R90p': BEGIN
MinCol=5
MaxCol=254
minvalue=0
Maxvalue=100
ContourDist=10
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues_pr = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrColValues_pr = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
rgrColValues_pr=REVERSE(rgrColValues_pr)
rgrColValues_pr(0)=255
rgrContValues_b=INDGEN(12)*2.-11
rgrColValues_b=INDGEN(N_ELEMENTS(rgrContValues_b)-1)*21 ;CHANGE
rgrColValues_b(N_ELEMENTS(rgrContValues_b)/2-1)=255
rgrColValues_b=REVERSE(rgrColValues_b)
;rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2)=255
;rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2-1)=255
rgrContValues_b2=INDGEN(11)
rgrColValues_b2=INDGEN(N_ELEMENTS(rgrContValues_b2)-1)*23+44
rgrColValues_b2(0)=255
END
'R90ptot': BEGIN
MinCol=5
MaxCol=254
minvalue=0
Maxvalue=100
ContourDist=10
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues_pr = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrColValues_pr = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
rgrColValues_pr=REVERSE(rgrColValues_pr)
rgrColValues_pr(0)=255
rgrContValues_b=INDGEN(10)*2.-5
rgrColValues_b=INDGEN(N_ELEMENTS(rgrContValues_b)-1)*28 ;CHANGE
rgrColValues_b=REVERSE(rgrColValues_b)
rgrColValues_b(N_ELEMENTS(rgrContValues_b)/2-3)=255
;rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2)=255
;rgrColValues_b(N_ELEMENTS(rgrColValues_b)/2-1)=255
rgrContValues_b2=INDGEN(11)
rgrColValues_b2=INDGEN(N_ELEMENTS(rgrContValues_b2)-1)*23+44
rgrColValues_b2(0)=255
END
ENDCASE

lev_c_pr=STRCOMPRESS(STRMID(rgrContValues_pr,4,5))
lev_c_b=STRCOMPRESS(STRMID(rgrContValues_b,4,5))
lev_c_b2=STRCOMPRESS(STRMID(rgrContValues_b2,4,5))


window,0,retain=2,ysize=1100,xsize=900,title=VAR+' '+SEAS(0)+' '+YSTART_1+'-'+YSTOP_1(0)
!p.multi=[0,3,4]
FOR im=0,NGCM-1 DO BEGIN &$
contour,landmask_com,xstyle=1,ystyle=1,title=MODEL_ID(im)+'_'+CASE_ID(im)
contour,VAR_SM_1(*,*,im,0),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot &$
contour,landmask_com,/overplot &$
ENDFOR

window,1,retain=2,ysize=1200,xsize=900,title=VAR+' '+SEAS(0)+' '+YSTART_2+'-'+YSTOP_2(0)
!p.multi=[0,3,4]
FOR im=0,NGCM-1 DO BEGIN &$
contour,landmask_com,xstyle=1,ystyle=1,title=MODEL_ID(im)+'_'+CASE_ID(im)
contour,VAR_SM_2(*,*,im,0),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot &$
contour,landmask_com,/overplot &$
ENDFOR

window,2,retain=2,ysize=1200,xsize=900,title=VAR+' '+SEAS(0)+' '+YSTART_3+'-'+YSTOP_3(0)
!p.multi=[0,3,4]
FOR im=0,NGCM-1 DO BEGIN &$
contour,landmask_com,xstyle=1,ystyle=1,title=MODEL_ID(im)+'_'+CASE_ID(im)
contour,VAR_SM_3(*,*,im,0),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot &$
contour,landmask_com,/overplot &$
ENDFOR

IF VAR EQ 'SM' THEN BEGIN
window,3,retain=2,ysize=1200,xsize=900,title=VAR+' '+SEAS(1)+' '+YSTART_1+'-'+YSTOP_1(0)
!p.multi=[0,3,4]
FOR im=0,NGCM-1 DO BEGIN &$
contour,landmask_com,xstyle=1,ystyle=1,title=MODEL_ID(im)+'_'+CASE_ID(im)
contour,VAR_SM_1(*,*,im,1),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot &$
contour,landmask_com,/overplot &$
ENDFOR

window,4,retain=2,ysize=1200,xsize=900,title=VAR+' '+SEAS(1)+' '+YSTART_2+'-'+YSTOP_2(0)
!p.multi=[0,3,4]
FOR im=0,NGCM-1 DO BEGIN &$
contour,landmask_com,xstyle=1,ystyle=1,title=MODEL_ID(im)+'_'+CASE_ID(im)
contour,VAR_SM_2(*,*,im,1),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot &$
contour,landmask_com,/overplot &$
ENDFOR

window,5,retain=2,ysize=1200,xsize=900,title=VAR+' '+SEAS(1)+' '+YSTART_3+'-'+YSTOP_3(0)
!p.multi=[0,3,4]
FOR im=0,NGCM-1 DO BEGIN &$
contour,landmask_com,xstyle=1,ystyle=1,title=MODEL_ID(im)+'_'+CASE_ID(im)
contour,VAR_SM_3(*,*,im,1),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot &$
contour,landmask_com,/overplot &$
ENDFOR
ENDIF

;ENSEMBLE MEASN
VAR_1_EM=FLTARR(nx,ny,NSEAS)
VAR_2_EM=FLTARR(nx,ny,NSEAS)
VAR_3_EM=FLTARR(nx,ny,NSEAS)
VAR_1_SD=FLTARR(nx,ny,NSEAS)
VAR_2_SD=FLTARR(nx,ny,NSEAS)
VAR_3_SD=FLTARR(nx,ny,NSEAS)

FOR iss=0,NSEAS-1 DO BEGIN
FOR ix=0,nx-1 DO BEGIN
FOR iy=0,ny-1 DO BEGIN
VAR_1_EM(ix,iy,iss)=MEAN(VAR_SM_1(ix,iy,*,iss),/NaN)
VAR_1_SD(ix,iy,iss)=STDDEV(VAR_SM_1(ix,iy,*,iss),/NaN)
VAR_2_EM(ix,iy,iss)=MEAN(VAR_SM_2(ix,iy,*,iss),/NaN)
VAR_2_SD(ix,iy,iss)=STDDEV(VAR_SM_2(ix,iy,*,iss),/NaN)
VAR_3_EM(ix,iy,iss)=MEAN(VAR_SM_3(ix,iy,*,iss),/NaN)
VAR_3_SD(ix,iy,iss)=STDDEV(VAR_SM_3(ix,iy,*,iss),/NaN)
ENDFOr
ENDFOr
ENDFOr

!x.margin=[2,2]
!y.margin=[2,2]

window,6,retain=2,ysize=1100,xsize=900,title=VAR+' '+SEAS(0)+' '+YSTART_3+'-'+YSTOP_3(0)
!p.multi=[0,3,5]
contour,landmask_com,xstyle=1,ystyle=1,title='ENS MEAN '+VAR+' '+SEAS(0)+' '+YSTART_1+'-'+YSTOP_1(0),charsize=2
contour,VAR_1_EM(*,*,0),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com,xstyle=1,ystyle=1,title='ENS MEAN '+VAR+' '+SEAS(0)+' '+YSTART_2+'-'+YSTOP_2(0),charsize=2
contour,VAR_2_EM(*,*,0),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com,xstyle=1,ystyle=1,title='ENS MEAN '+VAR+' '+SEAS(0)+' '+YSTART_3+'-'+YSTOP_3(0),charsize=2
contour,VAR_3_EM(*,*,0),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR

contour,landmask_com,xstyle=1,ystyle=1,title='STD DEV '+VAR+' '+SEAS(0)+' '+YSTART_1+'-'+YSTOP_1(0),charsize=2
contour,VAR_1_SD(*,*,0),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com,xstyle=1,ystyle=1,title='STD DEV '+VAR+' '+SEAS(0)+' '+YSTART_2+'-'+YSTOP_2(0),charsize=2
contour,VAR_2_SD(*,*,0),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com,xstyle=1,ystyle=1,title='STD DEV '+VAR+' '+SEAS(0)+' '+YSTART_3+'-'+YSTOP_3(0),charsize=2
contour,VAR_3_SD(*,*,0),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR

contour,landmask_com,xstyle=1,ystyle=1,title='ENS MEAN '+VAR+' '+SEAS(1)+' '+YSTART_1+'-'+YSTOP_1(0),charsize=2
contour,VAR_1_EM(*,*,1),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com,xstyle=1,ystyle=1,title='ENS MEAN '+VAR+' '+SEAS(1)+' '+YSTART_2+'-'+YSTOP_2(0),charsize=2
contour,VAR_2_EM(*,*,1),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com,xstyle=1,ystyle=1,title='ENS MEAN '+VAR+' '+SEAS(1)+' '+YSTART_3+'-'+YSTOP_3(0),charsize=2
contour,VAR_3_EM(*,*,1),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR

contour,landmask_com,xstyle=1,ystyle=1,title='STD DEV '+VAR+' '+SEAS(1)+' '+YSTART_1+'-'+YSTOP_1(0),charsize=2
contour,VAR_1_SD(*,*,1),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com,xstyle=1,ystyle=1,title='STD DEV '+VAR+' '+SEAS(1)+' '+YSTART_2+'-'+YSTOP_2(0),charsize=2
contour,VAR_2_SD(*,*,1),levels=rgrContValues_b2,c_colors=rgrColValues_b,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com,xstyle=1,ystyle=1,title='STD DEV '+VAR+' '+SEAS(1)+' '+YSTART_3+'-'+YSTOP_3(0),charsize=2
contour,VAR_3_SD(*,*,1),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
colorbar,levels=lev_c_pr,c_colors=rgrColValues_pr,divisions=N_ELEMENTS(rgrColValues_pr),charsize=2,color=0,position=[0.35,0.14,0.95,0.16],PSCOLOR=1
colorbar,levels=lev_c_b2,c_colors=rgrColValues_b2,divisions=N_ELEMENTS(rgrColValues_b2),charsize=2,color=0,position=[0.35,0.08,0.95,0.1],PSCOLOR=1

XYOUTS,0.2,0.15,'T (C)',color=0,/NORMAL
XYOUTS,0.2,0.09,'St. Dev. (C)',color=0,/NORMAL

; CLIMATe CHANGE
VAR_2_CC_EM=FLTARR(nx,ny,NSEAS)
VAR_3_CC_EM=FLTARR(nx,ny,NSEAS)
VAR_2_CC_SD=FLTARR(nx,ny,NSEAS)
VAR_3_CC_SD=FLTARR(nx,ny,NSEAS)

IF VAR EQ 'Ptot' AND STAT EQ 'SM' THEN BEGIN
FOR iss=0,NSEAS-1 DO BEGIN
FOR ix=0,nx-1 DO BEGIN
FOR iy=0,ny-1 DO BEGIN
IF VAR_SM_1(ix,iy,0,iss) NE 0. THEN BEGIN
VAR_2_CC_EM(ix,iy,iss)=MEAN((VAR_SM_2(ix,iy,*,iss)-VAR_SM_1(ix,iy,*,iss))/VAR_SM_1(ix,iy,*,iss),/NaN)*100.
VAR_3_CC_EM(ix,iy,iss)=MEAN((VAR_SM_3(ix,iy,*,iss)-VAR_SM_1(ix,iy,*,iss))/VAR_SM_1(ix,iy,*,iss),/NaN)*100.
VAR_2_CC_SD(ix,iy,iss)=STDDEV((VAR_SM_2(ix,iy,*,iss)-VAR_SM_1(ix,iy,*,iss))*100./VAR_SM_1(ix,iy,*,iss),/NaN)
VAR_3_CC_SD(ix,iy,iss)=STDDEV((VAR_SM_3(ix,iy,*,iss)-VAR_SM_1(ix,iy,*,iss))*100./VAR_SM_1(ix,iy,*,iss),/NaN)
ENDIF
ENDFOr
ENDFOr
ENDFOr
ENDIF ELSE BEGIN
FOR iss=0,NSEAS-1 DO BEGIN
FOR ix=0,nx-1 DO BEGIN
FOR iy=0,ny-1 DO BEGIN
VAR_2_CC_EM(ix,iy,iss)=MEAN((VAR_SM_2(ix,iy,*,iss)-VAR_SM_1(ix,iy,*,iss)),/NaN)
VAR_3_CC_EM(ix,iy,iss)=MEAN((VAR_SM_3(ix,iy,*,iss)-VAR_SM_1(ix,iy,*,iss)),/NaN)
VAR_2_CC_SD(ix,iy,iss)=STDDEV((VAR_SM_2(ix,iy,*,iss)-VAR_SM_1(ix,iy,*,iss)),/NaN)
VAR_3_CC_SD(ix,iy,iss)=STDDEV((VAR_SM_3(ix,iy,*,iss)-VAR_SM_1(ix,iy,*,iss)),/NaN)
ENDFOr
ENDFOr
ENDFOr
ENDELSE

window,7,retain=1,ysize=1100,xsize=900,title='CHANGE '+VAR+' '+SEAS(0)+' '+YSTART_3+'-'+YSTOP_3(0)
!p.multi=[0,3,5]
contour,landmask_com,xstyle=1,ystyle=1,title='ENS MEAN '+VAR+' '+SEAS(0)+' '+YSTART_1+'-'+YSTOP_1(0),charsize=2
contour,VAR_1_EM(*,*,0),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(*,*),/overplot,color=0,levels=[0,1]
contour,landmask_com,xstyle=1,ystyle=1,title='ENS MEAN '+VAR+' '+SEAS(0)+' '+YSTART_2+'-'+YSTOP_2(0),charsize=2
contour,VAR_2_CC_EM(*,*,0),levels=rgrContValues_b,c_colors=rgrColValues_b,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(*,*),/overplot,color=0,levels=[0,1]
contour,landmask_com,xstyle=1,ystyle=1,title='ENS MEAN '+VAR+' '+SEAS(0)+' '+YSTART_3+'-'+YSTOP_3(0),charsize=2
contour,VAR_3_CC_EM(*,*,0),levels=rgrContValues_b,c_colors=rgrColValues_b,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(*,*),/overplot,color=0,levels=[0,1]

contour,landmask_com,xstyle=1,ystyle=1,title='STD DEV '+VAR+' '+SEAS(0)+' '+YSTART_1+'-'+YSTOP_1(0),charsize=2
contour,VAR_1_SD(*,*,0),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(*,*),/overplot,color=0,levels=[0,1]
contour,land_country_dan(*,*),/overplot,color=255,levels=[0,1]
contour,landmask_com,xstyle=1,ystyle=1,title='STD DEV '+VAR+' '+SEAS(0)+' '+YSTART_2+'-'+YSTOP_2(0),charsize=2
contour,VAR_2_CC_SD(*,*,0),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(*,*),/overplot,color=0,levels=[0,1]
contour,land_country_dan(*,*),/overplot,color=255,levels=[0,1]
contour,landmask_com,xstyle=1,ystyle=1,title='STD DEV '+VAR+' '+SEAS(0)+' '+YSTART_3+'-'+YSTOP_3(0),charsize=2
contour,VAR_3_CC_SD(*,*,0),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(*,*),/overplot,color=0,levels=[0,1]

contour,landmask_com,xstyle=1,ystyle=1,title='ENS MEAN '+VAR+' '+SEAS(1)+' '+YSTART_1+'-'+YSTOP_1(0),charsize=2
contour,VAR_1_EM(*,*,1),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(*,*),/overplot,color=0,levels=[0,1]
contour,landmask_com,xstyle=1,ystyle=1,title='ENS MEAN '+VAR+' '+SEAS(1)+' '+YSTART_2+'-'+YSTOP_2(0),charsize=2
contour,VAR_2_CC_EM(*,*,1),levels=rgrContValues_b,c_colors=rgrColValues_b,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(*,*),/overplot,color=0,levels=[0,1]
contour,landmask_com,xstyle=1,ystyle=1,title='ENS MEAN '+VAR+' '+SEAS(1)+' '+YSTART_3+'-'+YSTOP_3(0),charsize=2
contour,VAR_3_CC_EM(*,*,1),levels=rgrContValues_b,c_colors=rgrColValues_b,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(*,*),/overplot,color=0,levels=[0,1]

contour,landmask_com,xstyle=1,ystyle=1,title='STD DEV '+VAR+' '+SEAS(1)+' '+YSTART_1+'-'+YSTOP_1(0),charsize=2
contour,VAR_1_SD(*,*,1),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(*,*),/overplot,color=0,levels=[0,1]
contour,landmask_com,xstyle=1,ystyle=1,title='STD DEV '+VAR+' '+SEAS(1)+' '+YSTART_2+'-'+YSTOP_2(0),charsize=2
contour,VAR_2_CC_SD(*,*,1),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(*,*),/overplot,color=0,levels=[0,1]
contour,landmask_com,xstyle=1,ystyle=1,title='STD DEV '+VAR+' '+SEAS(1)+' '+YSTART_3+'-'+YSTOP_3(0),charsize=2
contour,VAR_3_CC_SD(*,*,1),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(*,*,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(*,*),/overplot,color=0,levels=[0,1]

colorbar,levels=lev_c_pr,c_colors=rgrColValues_pr,divisions=N_ELEMENTS(rgrColValues_pr),charsize=2,color=0,position=[0.35,0.14,0.95,0.16],PSCOLOR=1
colorbar,levels=lev_c_b,c_colors=rgrColValues_b,divisions=N_ELEMENTS(rgrColValues_b),charsize=2,color=0,position=[0.35,0.08,0.95,0.1],PSCOLOR=1
colorbar,levels=lev_c_b2,c_colors=rgrColValues_b2,divisions=N_ELEMENTS(rgrColValues_b2),charsize=2,color=0,position=[0.35,0.02,0.95,0.04],PSCOLOR=1

XYOUTS,0.2,0.15,'T (C)',color=0,/NORMAL
XYOUTS,0.2,0.09,'Change (C)',color=0,/NORMAL
XYOUTS,0.2,0.03,'Standard Dev. (C)',color=0,/NORMAL

x0=0
x1=149
y0=25
y1=ny-1

IF STAT EQ 'HWFI' OR STAT EQ 'Tx90p' OR STAT EQ 'Tn90p' OR STAT EQ 'Tn10p' THEN BEGIN
set_plot,'ps'
device,file='./PLOT_'+VAR+'_'+STAT+'.eps',/color,/portrait,xoffset=2,yoffset=1,xsize=18,ysize=25,/encapsulated,BITS_PER_PIXEL=8,DECOMPOSED=0

!p.multi=[0,3,5]
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='ENS MEAN '+SEAS(0)+' '+YSTART_1+'-'+YSTOP_1(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_1_EM(x0:x1,y0:y1,0),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='ENS MEAN '+YSTART_2+'-'+YSTOP_2(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_2_EM(x0:x1,y0:y1,0),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='ENS MEAN '+YSTART_3+'-'+YSTOP_3(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_3_EM(x0:x1,y0:y1,0),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]

contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='STD DEV '+SEAS(0)+' '+YSTART_1+'-'+YSTOP_1(0),charsize=2,/nodata,color=255
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='STD DEV '+YSTART_2+'-'+YSTOP_2(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_2_SD(x0:x1,y0:y1,0),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]
;contour,land_country_dan(x0:x1,y0:y1),/overplot,color=255,levels=[0,1]
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='STD DEV '+YSTART_3+'-'+YSTOP_3(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_3_SD(x0:x1,y0:y1,0),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]

contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='ENS MEAN '+SEAS(1)+' '+YSTART_1+'-'+YSTOP_1(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_1_EM(x0:x1,y0:y1,1),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='ENS MEAN '+YSTART_2+'-'+YSTOP_2(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_2_EM(x0:x1,y0:y1,1),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='ENS MEAN '+YSTART_3+'-'+YSTOP_3(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_3_EM(x0:x1,y0:y1,1),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]

contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='STD DEV '+SEAS(1)+' '+YSTART_1+'-'+YSTOP_1(0),charsize=2,/nodata,color=255
;contour,VAR_1_SD(x0:x1,y0:y1,1),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
;FOR ic=0,N_COUNTRY-1 DO BEGIN
;contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
;ENDFOR
;contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='STD DEV '+YSTART_2+'-'+YSTOP_2(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_2_SD(x0:x1,y0:y1,1),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='STD DEV '+YSTART_3+'-'+YSTOP_3(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_3_SD(x0:x1,y0:y1,1),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]

colorbar,levels=lev_c_pr,c_colors=rgrColValues_pr,divisions=N_ELEMENTS(rgrColValues_pr),charsize=1.5,color=0,position=[0.35,0.14,0.95,0.16],PSCOLOR=1
colorbar,levels=lev_c_b2,c_colors=rgrColValues_b2,divisions=N_ELEMENTS(rgrColValues_b2),charsize=1.5,color=0,position=[0.35,0.02,0.95,0.04],PSCOLOR=1

XYOUTS,0.15,0.15,STAT,color=0,/NORMAL
;XYOUTS,0.15,0.09,'Change (%)',color=0,/NORMAL
XYOUTS,0.15,0.03,'Standard Dev.',color=0,/NORMAL
device,/close
set_plot,'x'

ENDIF ELSE BEGIN

set_plot,'ps'
device,file='./PLOT_'+VAR+'_'+STAT+'.eps',/color,/portrait,xoffset=2,yoffset=1,xsize=18,ysize=25,/encapsulated,BITS_PER_PIXEL=8,DECOMPOSED=0

!p.multi=[0,3,5]
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='ENS MEAN '+SEAS(0)+' '+YSTART_1+'-'+YSTOP_1(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_1_EM(x0:x1,y0:y1,0),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='ENS MEAN CHANGE '+YSTART_2+'-'+YSTOP_2(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_2_CC_EM(x0:x1,y0:y1,0),levels=rgrContValues_b,c_colors=rgrColValues_b,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='ENS MEAN CHANGE '+YSTART_3+'-'+YSTOP_3(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_3_CC_EM(x0:x1,y0:y1,0),levels=rgrContValues_b,c_colors=rgrColValues_b,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]

contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='STD DEV '+SEAS(0)+' '+YSTART_1+'-'+YSTOP_1(0),charsize=2,/nodata,color=255
;contour,VAR_1_SD(x0:x1,y0:y1,0),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
;FOR ic=0,N_COUNTRY-1 DO BEGIN
;contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
;ENDFOR
;contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]
;contour,land_country_dan(x0:x1,y0:y1),/overplot,color=255,levels=[0,1]
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='STD DEV CHANGE '+YSTART_2+'-'+YSTOP_2(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_2_CC_SD(x0:x1,y0:y1,0),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]
;contour,land_country_dan(x0:x1,y0:y1),/overplot,color=255,levels=[0,1]
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='STD DEV CHANGE '+YSTART_3+'-'+YSTOP_3(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_3_CC_SD(x0:x1,y0:y1,0),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]

contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='ENS MEAN '+SEAS(1)+' '+YSTART_1+'-'+YSTOP_1(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_1_EM(x0:x1,y0:y1,1),levels=rgrContValues_pr,c_colors=rgrColValues_pr,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='ENS MEAN CHANGE '+YSTART_2+'-'+YSTOP_2(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_2_CC_EM(x0:x1,y0:y1,1),levels=rgrContValues_b,c_colors=rgrColValues_b,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='ENS MEAN CHANGE '+YSTART_3+'-'+YSTOP_3(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_3_CC_EM(x0:x1,y0:y1,1),levels=rgrContValues_b,c_colors=rgrColValues_b,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]

contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='STD DEV '+SEAS(1)+' '+YSTART_1+'-'+YSTOP_1(0),charsize=2,/nodata,color=255
;contour,VAR_1_SD(x0:x1,y0:y1,1),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
;FOR ic=0,N_COUNTRY-1 DO BEGIN
;contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
;ENDFOR
;contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='STD DEV CHANGE '+YSTART_2+'-'+YSTOP_2(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_2_CC_SD(x0:x1,y0:y1,1),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='STD DEV CHANGE '+YSTART_3+'-'+YSTOP_3(0),charsize=1,xtickname=[' ',' ',' ',' ',' ',' ',' ',' '],ytickname=[' ',' ',' ',' ']
contour,VAR_3_CC_SD(x0:x1,y0:y1,1),levels=rgrContValues_b2,c_colors=rgrColValues_b2,/fill,/overplot 
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR
contour,landmask_com(x0:x1,y0:y1),/overplot,color=0,levels=[0,1]

colorbar,levels=lev_c_pr,c_colors=rgrColValues_pr,divisions=N_ELEMENTS(rgrColValues_pr),charsize=1.5,color=0,position=[0.35,0.14,0.95,0.16],PSCOLOR=1
colorbar,levels=lev_c_b,c_colors=rgrColValues_b,divisions=N_ELEMENTS(rgrColValues_b),charsize=1.5,color=0,position=[0.35,0.08,0.95,0.1],PSCOLOR=1
colorbar,levels=lev_c_b2,c_colors=rgrColValues_b2,divisions=N_ELEMENTS(rgrColValues_b2),charsize=1.5,color=0,position=[0.35,0.02,0.95,0.04],PSCOLOR=1

IF STAT EQ 'SM' THEN BEGIN
IF VAR EQ 'T' OR VAR EQ 'Tmax' OR VAr EQ 'Tmin' THEN BEGIN
XYOUTS,0.15,0.15,'T (C)',color=0,/NORMAL
XYOUTS,0.15,0.09,'Change (C)',color=0,/NORMAL
XYOUTS,0.15,0.03,'Standard Dev. (C)',color=0,/NORMAL
ENDIF
IF VAR EQ 'DT' THEN BEGIN
XYOUTS,0.15,0.15,'Diurnal Range (C)',color=0,/NORMAL
XYOUTS,0.15,0.09,'Change (C)',color=0,/NORMAL
XYOUTS,0.15,0.03,'Standard Dev. (C)',color=0,/NORMAL
ENDIF
IF VAR EQ 'Ptot' THEN BEGIN
XYOUTS,0.15,0.15,'Prec. (mmm/day)',color=0,/NORMAL
XYOUTS,0.15,0.09,'Change (%)',color=0,/NORMAL
XYOUTS,0.15,0.03,'Standard Dev. (%)',color=0,/NORMAL
ENDIF
ENDIF ELSE BEGIN
IF STAT EQ 'HWFI' THEN BEGIN
XYOUTS,0.15,0.15,STAT+' (%)',color=0,/NORMAL
;XYOUTS,0.15,0.09,'Change (%)',color=0,/NORMAL
XYOUTS,0.15,0.03,'Standard Dev. (%)',color=0,/NORMAL
ENDIF
IF STAT EQ 'Tx90p' OR  STAT EQ 'Tn90p' THEN BEGIN
XYOUTS,0.15,0.15,STAT+' (%)',color=0,/NORMAL
XYOUTS,0.15,0.09,'Change (%)',color=0,/NORMAL
XYOUTS,0.15,0.03,'Standard Dev. (%)',color=0,/NORMAL
ENDIF
IF STAT EQ 'CDD' OR STAT EQ 'CWD' THEN BEGIN
XYOUTS,0.15,0.15,STAT,color=0,/NORMAL
XYOUTS,0.15,0.09,'Change',color=0,/NORMAL
XYOUTS,0.15,0.03,'Standard Dev.',color=0,/NORMAL
ENDIF
IF STAT EQ 'R90p'  OR STAT EQ 'R90ptot' THEN BEGIN
XYOUTS,0.15,0.15,STAT+' (%)',color=0,/NORMAL
XYOUTS,0.15,0.09,'Change'+' (%)',color=0,/NORMAL
XYOUTS,0.15,0.03,'Standard Dev.'+' (%)',color=0,/NORMAL
ENDIF
ENDELSE

device,/close
set_plot,'x'

ENDELSE

;DATA PER COUNTRY
fileo=VAR+'_'+STAT+'_COUNTRIES.txt'
openw,1,fileo
FOR ic=0,N_COUNTRY-1 DO BEGIN
lc=REFORM(land_country(*,*,ic))
lc(WHERE(lc LT 1))=!VALUES.F_NaN
printf,1,COUNTRY(ic)
FOR iss=0,NSEAS-1 DO BEGIN
work_1=REFORM(VAR_1_EM(*,*,iss))*lc
work_1(WHERE(work_1 LE -999))=!VALUES.F_NaN
work_1_sd=REFORM(VAR_1_SD(*,*,iss))*lc
printf,1,'ENS MEAN '+VAR+' '+SEAS(iss)+' '+YSTART_1+'-'+YSTOP_1(0),MEAN(work_1,/NaN),MEAN(work_1_sd,/NaN)
work_2=REFORM(VAR_2_CC_EM(*,*,iss))*lc
work_2_sd=REFORM(VAR_2_CC_SD(*,*,iss))*lc
printf,1,'ENS MEAN CC '+VAR+' '+SEAS(iss)+' '+YSTART_2+'-'+YSTOP_2(0),MEAN(work_2,/NaN),MEAN(work_2_sd,/NaN)
work_3=REFORM(VAR_3_CC_EM(*,*,iss))*lc
work_3_sd=REFORM(VAR_3_CC_SD(*,*,iss))*lc
printf,1,'ENS MEAN CC '+VAR+' '+SEAS(iss)+' '+YSTART_3+'-'+YSTOP_3(0),MEAN(work_3,/NaN),MEAN(work_3_sd,/NaN)
ENDFOR
ENDFOR
print,'=============='
printf,1,'DAN'
FOR iss=0,NSEAS-1 DO BEGIN &$
lc=REFORM(land_country_dan(*,*)) &$
lc(WHERE(lc LT 1))=!VALUES.F_NaN &$
work_1=REFORM(VAR_1_EM(*,*,iss))*lc &$
work_1(WHERE(work_1 LE -999))=!VALUES.F_NaN &$
work_1_sd=REFORM(VAR_1_SD(*,*,iss))*lc &$
printf,1,'ENS MEAN '+VAR+' '+SEAS(iss)+' '+YSTART_1+'-'+YSTOP_1(0),MEAN(work_1,/NaN),MEAN(work_1_sd,/NaN) &$
work_2=REFORM(VAR_2_CC_EM(*,*,iss))*lc &$
work_2_sd=REFORM(VAR_2_CC_SD(*,*,iss))*lc &$
printf,1,'ENS MEAN CC '+VAR+' '+SEAS(iss)+' '+YSTART_2+'-'+YSTOP_2(0),MEAN(work_2,/NaN),MEAN(work_2_sd,/NaN) &$
work_3=REFORM(VAR_3_CC_EM(*,*,iss))*lc &$
work_3_sd=REFORM(VAR_3_CC_SD(*,*,iss))*lc &$
printf,1,'ENS MEAN CC '+VAR+' '+SEAS(iss)+' '+YSTART_3+'-'+YSTOP_3(0),MEAN(work_3,/NaN),MEAN(work_3_sd,/NaN) &$
ENDFOR
close,1


IF VAR EQ 'Ptot' AND STAT EQ 'SM' THEN BEGIN &$
VAR_2_CC=VAR_SM_2-VAR_SM_1 &$
VAR_3_CC=VAR_SM_3-VAR_SM_1 &$
NMOD_P_2=FLTARR(nx,ny,nseas)*0. &$
NMOD_P_3=FLTARR(nx,ny,nseas)*0. &$

help,landmask_com
we=(WHERE(landmask_com LT 1))
FOR iss=0,nseas-1 DO BEGIN &$
FOR ix=0,nx-1 DO BEGIN &$
FOR iy=0,ny-1 DO BEGIN &$
FOR im=0,NGCM-1 DO BEGIN &$
IF VAR_2_CC(ix,iy,im,iss) GT 0 THEN NMOD_P_2(ix,iy,iss)=NMOD_P_2(ix,iy,iss)+1 &$
IF VAR_3_CC(ix,iy,im,iss) GT 0 THEN NMOD_P_3(ix,iy,iss)=NMOD_P_3(ix,iy,iss)+1 &$
IF NMOD_P_2(ix,iy,iss) EQ 12 THEN NMOD_P_2(ix,iy,iss)=11.9
IF NMOD_P_3(ix,iy,iss) EQ 12 THEN NMOD_P_3(ix,iy,iss)=11.9
ENDFOR &$
ENDFOR &$
ENDFOR &$
work=(REFORM(NMOD_P_2(*,*,iss)))
work(we)=-0.9
NMOD_P_2(*,*,iss)=work
work=(REFORM(NMOD_P_3(*,*,iss)))
work(we)=-0.90
NMOD_P_3(*,*,iss)=work
ENDFOR &$
;NMOD_P_2=NMOD_P_2-0.1
;NMOD_P_3=NMOD_P_3-0.1

loadct,39
!p.background=255
!p.color=0
window,10,retain=2 &$
!p.multi=[0,2,3] &$
levels=FINDGEN(NGCM+1)
levels_c=STRCOMPRESS(STRMID(levels,4,5))
colors=FINDGEN(NGCM)*20+30
colors=REVERSE(colors)
colors(6)=255
colors(5)=255
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='MODEL AGREEMENT '+SEAS(0)+' '+YSTART_2+'-'+YSTOP_2(0),charsize=2,/nodata,color=0 &$
contour,NMOD_P_2(x0:x1,y0:y1,0),levels=levels,c_colors=colors,/fill,/overplot &$
contour,landmask_com(x0:x1,y0:y1),color=0,/overplot,levels=[0,1]
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR

contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='MODEL AGREEMENT '+SEAS(1)+' '+YSTART_2+'-'+YSTOP_2(0),charsize=2,/nodata,color=0 &$
contour,NMOD_P_2(x0:x1,y0:y1,1),levels=levels,c_colors=colors,/fill,/overplot &$
contour,landmask_com(x0:x1,y0:y1),color=0,/overplot,levels=[0,1]
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR

contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='MODEL AGREEMENT '+SEAS(0)+' '+YSTART_3+'-'+YSTOP_3(0),charsize=2,/nodata,color=0 &$
contour,NMOD_P_3(x0:x1,y0:y1,0),levels=levels,c_colors=colors,/fill,/overplot &$
contour,landmask_com(x0:x1,y0:y1),color=0,/overplot,levels=[0,1]
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR

contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='MODEL AGREEMENT '+SEAS(1)+' '+YSTART_3+'-'+YSTOP_3(0),charsize=2,/nodata,color=0 &$
contour,NMOD_P_3(x0:x1,y0:y1,1),levels=levels,c_colors=colors,/fill,/overplot &$
contour,landmask_com(x0:x1,y0:y1),color=0,/overplot,levels=[0,1]
FOR ic=0,N_COUNTRY-1 DO BEGIN
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1]
ENDFOR

colorbar,levels=levels_c,c_colors=colors,divisions=N_ELEMENTS(colors),charsize=1.5,color=0,position=[0.35,0.14,0.95,0.16],PSCOLOR=1
XYOUTS,0.2,0.15,'Nr. of models',charsize=2,color=0

set_plot,'ps'
device,file='./PLOT_MODEL_AGREEMENT_'+VAR+'_'+STAT+'.eps',/color,/portrait,xoffset=2,yoffset=1,xsize=18,ysize=25,/encapsulated,BITS_PER_PIXEL=8,DECOMPOSED=0
!p.multi=[0,2,3] &$
levels=FINDGEN(NGCM+1)
levels_c=STRCOMPRESS(STRMID(levels,4,5))
colors=FINDGEN(NGCM)*20+30
colors=REVERSE(colors)
colors(6)=255
colors(5)=255
contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='MODEL AGREEMENT '+SEAS(0)+' '+YSTART_2+'-'+YSTOP_2(0),charsize=2,/nodata,color=0 &$
contour,NMOD_P_2(x0:x1,y0:y1,0),levels=levels,c_colors=colors,/fill,/overplot &$
contour,landmask_com(x0:x1,y0:y1),color=0,/overplot,levels=[0,1]
FOR ic=0,N_COUNTRY-1 DO BEGIN &$
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1] &$
ENDFOR

contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='MODEL AGREEMENT '+SEAS(1)+' '+YSTART_2+'-'+YSTOP_2(0),charsize=2,/nodata,color=0 &$
contour,NMOD_P_2(x0:x1,y0:y1,1),levels=levels,c_colors=colors,/fill,/overplot &$
contour,landmask_com(x0:x1,y0:y1),color=0,/overplot,levels=[0,1]
FOR ic=0,N_COUNTRY-1 DO BEGIN &$
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1] &$
ENDFOR

contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='MODEL AGREEMENT '+SEAS(0)+' '+YSTART_3+'-'+YSTOP_3(0),charsize=2,/nodata,color=0 &$
contour,NMOD_P_3(x0:x1,y0:y1,0),levels=levels,c_colors=colors,/fill,/overplot &$
contour,landmask_com(x0:x1,y0:y1),color=0,/overplot,levels=[0,1]
FOR ic=0,N_COUNTRY-1 DO BEGIN &$
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1] &$
ENDFOR

contour,landmask_com(x0:x1,y0:y1),xstyle=1,ystyle=1,title='MODEL AGREEMENT '+SEAS(1)+' '+YSTART_3+'-'+YSTOP_3(0),charsize=2,/nodata,color=0 &$
contour,NMOD_P_3(x0:x1,y0:y1,1),levels=levels,c_colors=colors,/fill,/overplot &$
contour,landmask_com(x0:x1,y0:y1),color=0,/overplot,levels=[0,1]
FOR ic=0,N_COUNTRY-1 DO BEGIN &$
contour,land_country(x0:x1,y0:y1,ic),/overplot,color=0,levels=[0,1] &$
ENDFOR

colorbar,levels=levels_c,c_colors=colors,divisions=N_ELEMENTS(colors),charsize=1.5,color=0,position=[0.35,0.14,0.95,0.16],PSCOLOR=1
XYOUTS,0.05,0.14,'Nr. of models',charsize=2,color=0,/NORMAL
device,/close
set_plot,'x'



ENDIF	

STOP
END
