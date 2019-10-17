;
PRO make_grid_multi
;
DEVICE,decompose=0
;
;
;-----------------------------------------------------------------------------------------------------------
home_data='/media/disk/POSTPROC/BC_Regional/'
home_const='/media/disk/DATA/ENSOBS/ENS_MODELS/'

MODEL_ID=['C4IRCA3','DMI-HIRHAM5','DMI-HIRHAM5','ETHZ-CLM','KNMI-RACMO2','MPI-M-REMO','SMHIRCA','SMHIRCA']
CASE_ID=['A1B_HadCM3Q16','A1B_ARPEGE','A1B_ECHAM5','SCN_HadCM3Q0','A1B_ECHAM5-r3','SCN_ECHAM5','A1B_BCM','A1B_ECHAM5-r3']
nmod=1.*N_ELEMENTS(MODEL_ID)

MODEL_OBS='KNMI-RACMO2'
CASE_OBS='A1B_ECHAM5-r3'

;VARIABLE (T,Ptot,Tmax,Tmin)
;VAR='Ptot'
;VAR='T'
;VAR='Tmin'
VAR='Tmax'

CONSTRUCTION_PERIOD_START  =  '1961'
CONSTRUCTION_PERIOD_STOP   =  '1990'

APPLICATION_PERIOD_START   =  '1991'
APPLICATION_PERIOD_STOP    =  '2000'

NYEARS=fix(APPLICATION_PERIOD_STOP)-fix(APPLICATION_PERIOD_START)+1

month=['01','02','03','04','05','06','07','08','09','10','11','12']
NDAYS=fltarr(12)
MM_I=0
MM_E=11

nx=fltarr(nmod)
ny=fltarr(nmod)

x0=fltarr(nmod)
y0=fltarr(nmod)
x1=fltarr(nmod)
y1=fltarr(nmod)
nx0=fltarr(nmod)
ny0=fltarr(nmod)

lm_var     = 'tg'
mv_cutoff  = 1.0

;
;===============================================
;COMMON GRID SIZE AND DIMENSIONS
FOR imd=0,nmod-1 DO BEGIN ;START MODEL LOOP
; COMMON LAND SEA MASK
LM_FILE_C=home_const+'read_in_'+MODEL_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'.nc'
print,'READING ',LM_FILE_C

    theVariable = lm_var
    fileID = NCDF_Open(LM_FILE_C)
    varID = NCDF_VarID(fileID, theVariable)
    varInfo = NCDF_VarInq(fileID, varID)
    dimIDs = varInfo.dim
    nDims = N_Elements(dimIDs)
    dims_c = IntArr(nDims)
    FOR j=0,nDims-1 DO BEGIN
        NCDF_DimInq, fileID, dimIDs[j], dname, dsize
        dims_c[j] = dsize
    ENDFOR

    nx(imd)=dims_c(0)
    ny(imd)=dims_c(1)

    NCDF_VARGET,fileID,'latitude',lat
    NCDF_VARGET,fileID,'longitude',lon
    print,MODEL_ID(imd)
    print,'NX (lon) ',nx(imd),min(lon),max(lon)
    print,'NY (lat) ',ny(imd),min(lat),max(lat)
    x0(imd)=min(lon)
    y0(imd)=min(lat)
    x1(imd)=max(lon)
    y1(imd)=max(lat)
    dx=lon(1)-lon(0)
    dy=lat(1)-lat(0)
    print,nx0(imd),ny0(imd)
    NCDF_CLOSE, fileID

ENDFOR ;END MODEL LOOP

    ;nxm=max(nx)
    ;nym=max(ny)
    nxm=ROUND((max(x1)-min(x0))/dx)+1
    nym=ROUND((max(y1)-min(y0))/dy)+1
    x0m=min(x0)
    y0m=min(y0)

    print,'NXM ',nxm
    print,'NYM ',nym

   landmask_m=dblarr(nxm,nym,nmod)
   landmask_m(*,*,*)=0

;===============================================
tas2d_m = FLTARR(nxm,nym,31.*NYEARS,nmod)
tas2d_m(*,*,*,*) = !VALUES.F_NAN
tasc2d_m = FLTARR(nxm,nym,31.*NYEARS,nmod)
tasc2d_m(*,*,*,*) = !VALUES.F_NAN
obs2d_m = FLTARR(nxm,nym,31.*NYEARS)
obs2d_m(*,*,*) = !VALUES.F_NAN

;LOOP OVER MONTHS
FOR im=MM_I,MM_E DO BEGIN
PRINT,'MONTH= ',month(im)

window,im,retain=2,xsize=750,ysize=900
!p.multi=[0,3,nmod+1]
ColorTBL=62
SetColorTable,ColorTBL
!p.background=0
!p.color=255

IF VAR EQ 'Ptot' THEN BEGIN
MinCol=20
MaxCol=155
minvalue=0
Maxvalue=6
ContourDist=1
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrCenterValues = FLTARR(N_ELEMENTS(rgrContValues)-1)
rgrColValues = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
rgrColValues=REVERSE(rgrColValues)
ENDIF ELSE BEGIN
MinCol=20
MaxCol=245
Minvalue=230
Maxvalue=310
ContourDist=5
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrCenterValues = FLTARR(N_ELEMENTS(rgrContValues)-1)
rgrColValues = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
ENDELSE

;==================================
;OBS
OBS_DIR=home_data+MODEL_OBS+'_'+CASE_OBS+'_EOBS_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/OBS_idl/'
FILE_OBS='obs_'+VAR+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'

LM_FILE_A=home_const+'read_in_'+MODEL_OBS+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'.nc'
print,'READING ',LM_FILE_A

theVariable = lm_var
fileID = NCDF_Open(LM_FILE_A)
varID = NCDF_VarID(fileID, theVariable)
varInfo = NCDF_VarInq(fileID, varID)
dimIDs = varInfo.dim
nDims = N_Elements(dimIDs)
dims_a = IntArr(nDims)
FOR j=0,nDims-1 DO BEGIN
NCDF_DimInq, fileID, dimIDs[j], dname, dsize
dims_a[j] = dsize
ENDFOR

nx_o=dims_a(0)
ny_o=dims_a(1)
landmask_a=dblarr(nx_o,ny_o)
NCDF_VARGET,fileID,lm_var,landmask_a,count=[dims_a(0),dims_a(1),1],offset=[0,0,0]
we_a=where(landmask_a ge mv_cutoff)
land_a=reform(we_a(sort(we_a)))+1
lat_a=land_a*0.0
lon_a=land_a*0.0

count=0L
count1=0L
FOR ix=0L,dims_a(0)-1 DO BEGIN
FOR iy=0L,dims_a(1)-1 DO BEGIN
  IF (landmask_a(ix,iy) gt mv_cutoff) THEN BEGIN
  lon_a(count)=ix
  lat_a(count)=iy
  land_a(count)=iy*dims_a(0)+ix+1
  count=count+1
  ENDIF
  count1=count1+1
ENDFOR
ENDFOR

landmask_a=landmask_a/100
NCDF_VARGET,fileID,'latitude',lat
NCDF_VARGET,fileID,'longitude',lon
    dx=lon(1)-lon(0)
    dy=lat(1)-lat(0)
    nx0_o=ABS(ROUND((x0m-lon(0))/dx))
    ny0_o=ABS(ROUND((y0m-lat(0))/dy))
    print,nx0_o,ny0_o
NCDF_CLOSE, fileID

print,'OBS ',OBS_DIR+FILE_OBS
restore,OBS_DIR+FILE_OBS
CASE VAR OF
    'T': obs=idldata
    'Ptot': obs=idldata
    'Tmin': obs=idldata
    'Tmax': obs=idldata
ENDCASE
help,obs
IF VAR EQ 'Ptot' THEN BEGIN
	obs=obs*3600.*24.
ENDIF

NDAYS_o=n_elements(REFORM(obs(0,*)))
obs2d = FLTARR(nx_o,ny_o,ndays_o)
obs2d(*,*,*)=!VALUES.F_NAN

FOR i=0L,n_elements(land_a)-1 DO BEGIN &$
        obs2d(lon_a(i),lat_a(i),*)=obs(i,0:NDAYS_o-1)
ENDFOR

obs2d_m(nx0_o:nx0_o+nx_o-1,ny0_o:ny0_o+ny_o-1,0:NDAYS_o-1)=obs2d

;SAVING OBS
print,'SAVING OBS MONTH '+month(im)
ofile='DATA/OBS_'+VAR+'_'+MODEL_OBS+'_'+CASE_OBS+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,obs2d,nxm,nym,nx_o,ny_o,nx0_o,ny0_o,landmask_a,ndays_o

;==================================
;LOOP OVER MODELS
FOR imd=0,nmod-1 do begin

;MOD_BC
IF CONSTRUCTION_PERIOD_START EQ '1961' AND CONSTRUCTION_PERIOD_STOP EQ '1990' THEN BEGIN 
MOD_BC_DIR=home_data+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_1961-2100/BC_data/'
ENDIF ELSE BEGIN
MOD_BC_DIR=home_data+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/BC_data/'
ENDELSE
FILE_BC=VAR+'_'+'BCed_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'

;MOD_OR
MOD_DIR=MOD_BC_DIR
FILE_MOD='mod_'+VAR+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'

; TASC
LM_FILE_C=home_const+'read_in_'+MODEL_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'.nc'
print,'READING ',LM_FILE_C

    theVariable = lm_var
    fileID = NCDF_Open(LM_FILE_C)
    varID = NCDF_VarID(fileID, theVariable)
    varInfo = NCDF_VarInq(fileID, varID)
    dimIDs = varInfo.dim
    nDims = N_Elements(dimIDs)
    dims_c = IntArr(nDims)
    FOR j=0,nDims-1 DO BEGIN
        NCDF_DimInq, fileID, dimIDs[j], dname, dsize
        dims_c[j] = dsize
    ENDFOR

  nx(imd)=dims_c(0)
  ny(imd)=dims_c(1)
  landmask_c=dblarr(nx(imd),ny(imd))
  NCDF_VARGET,fileID,lm_var,landmask_c,count=[dims_c(0),dims_c(1),1],offset=[0,0,0]
   we_c=where(landmask_c ge mv_cutoff)
   land_c=reform(we_c(sort(we_c)))+1
   lat_c=land_c*0.0
   lon_c=land_c*0.0

   count=0L
   count1=0L
   FOR ix=0L,dims_c(0)-1 DO BEGIN
   FOR iy=0L,dims_c(1)-1 DO BEGIN
   IF (landmask_c(ix,iy) gt mv_cutoff) THEN BEGIN
      lon_c(count)=ix
      lat_c(count)=iy
      land_c(count)=iy*dims_c(0)+ix+1
      count=count+1
   ENDIF
   count1=count1+1
   ENDFOR
   ENDFOR

;MODEL LANDMASK
  landmask_c=landmask_c/100

  NCDF_VARGET,fileID,'latitude',lat
  NCDF_VARGET,fileID,'longitude',lon
  dx=lon(1)-lon(0)
  dy=lat(1)-lat(0)
  nx0(imd)=ABS(ROUND((x0m-lon(0))/dx))
  ny0(imd)=ABS(ROUND((y0m-lat(0))/dy))
  print,nx0(imd),ny0(imd)

;COMMON LANDMASK
landmask_m(nx0(imd):nx0(imd)+nx(imd)-1,ny0(imd):ny0(imd)+ny(imd)-1,imd)=landmask_c(*,*)

NCDF_CLOSE, fileID

;READING DATA
print,'MOD CORR ',MOD_BC_DIR+FILE_BC
idldata=0
restore,MOD_BC_DIR+FILE_BC  ;TAS_C
CASE VAR OF
    'T': tas_c=tas_c
    'Ptot': tas_c=PR_C
    'Tmin': tas_c=Tmin_c
    'Tmax': tas_c=Tmax_c
ENDCASE
help,tas_c

print,'MOD ORIG ',MOD_DIR+FILE_MOD
idldata=0
restore,MOD_DIR+FILE_MOD
CASE VAR OF
    'T': tas=idldata
    'Ptot': tas=idldata
    'Tmin': tas=idldata
    'Tmax': tas=idldata
ENDCASE
help,tas

IF VAR EQ 'Ptot' THEN BEGIN
	tas=tas*3600.*24.
	tas_c=tas_c*3600.*24.
ENDIF

;NDAYS(im)=min([n_elements(REFORM(tas_c(0,*))),n_elements(REFORM(obs(0,*)))])
NDAYS(im)=n_elements(REFORM(tas_c(0,*)))
tas2d = FLTARR(nx(imd),ny(imd),ndays(im))
tas2d(*,*,*)=!VALUES.F_NAN
tasc2d = FLTARR(nx(imd),ny(imd),ndays(im))
tasc2d(*,*,*)=!VALUES.F_NAN

;CORRECTION FOR PREP <0 (e.g KNMI)
FOR i=0,NDAYS(im)-1 DO BEGIN
IF var EQ 'Ptot' THEN BEGIN
	p0=where(tas(*,i) LT 0)
	IF N_ELEMENTS(p0) GT 1 THEN tas(p0,i)=0.
ENDIF
ENDFOR

FOR i=0L,n_elements(land_c)-1 DO BEGIN &$
;		print,i,lon_c(i),lat_c(i)
         tasc2d(lon_c(i),lat_c(i),*)=tas_c(i,0:NDAYS(im)-1)
         tas2d(lon_c(i),lat_c(i),*)=tas(i,0:NDAYS(im)-1)
ENDFOR

tas2d_m(nx0(imd):nx0(imd)+nx(imd)-1,ny0(imd):ny0(imd)+ny(imd)-1,0:NDAYS(im)-1,imd)=tas2d
tasc2d_m(nx0(imd):nx0(imd)+nx(imd)-1,ny0(imd):ny0(imd)+ny(imd)-1,0:NDAYS(im)-1,imd)=tasc2d


;SAVING DATA
mx0=x0(imd)
my0=y0(imd)
mnx=nx(imd)
mny=ny(imd)
mnx0=nx0(imd)
mny0=ny0(imd)
mndays=ndays(im)

print,''
print,'-------------------------'
print,'SAVING MONTH ',month(im),' MODEL ',MODEL_ID(imd)+'_'+CASE_ID(imd)
print,'-------------------------'
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
ofile='DATA/MOD_BC_'+VAR+'_CORR_'+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays

;PLOT ON COMON GRID FOR CHECK
tas2d_m(nx0(imd):nx0(imd)+nx(imd)-1,ny0(imd):ny0(imd)+ny(imd)-1,0:NDAYS(im)-1,imd)=tas2d
tasc2d_m(nx0(imd):nx0(imd)+nx(imd)-1,ny0(imd):ny0(imd)+ny(imd)-1,0:NDAYS(im)-1,imd)=tasc2d

landmask_com=fltarr(nxm,nym)
FOR ix=0,nxm-1 do begin &$
FOR iy=0,nym-1 do begin &$
      landmask_com(ix,iy)=MEAN(landmask_m(ix,iy,*)) &$
      IF landmask_com(ix,iy) GT 0 THEN landmask_com(ix,iy)=1 &$
ENDFOR &$
ENDFOR

p_tas2d_m=fltarr(nxm,nym)
p_tasc2d_m=fltarr(nxm,nym)
p_obs2d_m=fltarr(nxm,nym)
p_tas2d_m(*,*)=-999.
p_tasc2d_m(*,*)=-999.
p_obs2d_m(*,*)=-999.
FOR ix=0,nxm-1 do begin 
FOR iy=0,nym-1 do begin 
	IF landmask_com(ix,iy) EQ 1 AND tas2d_m(ix,iy,0,imd) GE 0. THEN BEGIN
	p_tas2d_m(ix,iy)=MEAN(tas2d_m(ix,iy,*,imd),/NaN)
	p_tasc2d_m(ix,iy)=MEAN(tasc2d_m(ix,iy,*,imd),/NaN)
        ENDIF 
	IF landmask_com(ix,iy) EQ 1 AND obs2d_m(ix,iy,0) GE 0. THEN BEGIN
	p_obs2d_m(ix,iy)=MEAN(obs2d_m(ix,iy,*),/NaN)
        ENDIF
ENDFOR 
ENDFOR



contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS',charthick=1.5 ;,position=[0.05,0.55,0.45,0.95] 
contour,p_obs2d_m(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot 
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MODEL '+MODEL_ID(imd)+'_'+CASE_ID(imd),charthick=1.5 ;,position=[0.05,0.55,0.45,0.95] 
contour,p_tas2d_m(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot 
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORR '+MODEL_ID(imd)+'_'+CASE_ID(imd),charthick=1.5 ;,position=[0.05,0.55,0.45,0.95] 
contour,p_tasc2d_m(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot 
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 


ENDFOR ;END MODEL LOOP

;ENSEMBLE AVERAGE
tas2d_ave=fltarr(nxm,nym,mndays)
tasc2d_ave=fltarr(nxm,nym,mndays)
tas2d_ave(*,*,*)=!VALUES.F_NAN
tasc2d_ave(*,*,*)=!VALUES.F_NAN
tas2d_ave_sd=fltarr(nxm,nym,mndays) ;ST DEV
tasc2d_ave_sd=fltarr(nxm,nym,mndays)
tas2d_ave_sd(*,*,*)=!VALUES.F_NAN
tasc2d_ave_sd(*,*,*)=!VALUES.F_NAN

FOR i=0,mndays-1 do begin &$
FOR iy=0,nym-1 do begin &$
FOR ix=0,nxm-1 do begin &$
	IF landmask_com(ix,iy) EQ 1 THEN BEGIN
        tas2d_ave(ix,iy,i)=MEAN(tas2d_m(ix,iy,i,*),/NAN) &$
        tasc2d_ave(ix,iy,i)=MEAN(tasc2d_m(ix,iy,i,*),/NAN) &$
        tas2d_ave_sd(ix,iy,i)=STDDEV(tas2d_m(ix,iy,i,*),/NAN) &$
        tasc2d_ave_sd(ix,iy,i)=STDDEV(tasc2d_m(ix,iy,i,*),/NAN) &$
        ENDIF
ENDFOR &$
ENDFOR &$
ENDFOR

;SAVING ENSE AVERAGE DATA
print,'SAVING ENSEMBLE AVERAGE',month(im)
ofile='DATA/ENS_AVE_MOD_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,tas2d_ave,tas2d_ave_sd,nxm,nym,landmask_com,mndays
ofile='DATA/ENS_AVE_MOD_'+VAR+'_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,tasc2d_ave,tasc2d_ave_sd,nxm,nym,landmask_com,mndays


p_tas2d_ave=fltarr(nxm,nym)
p_tasc2d_ave=fltarr(nxm,nym)
p_tas2d_ave(*,*)=-999.
p_tasc2d_ave(*,*)=-999.
FOR ix=0,nxm-1 do begin 
FOR iy=0,nym-1 do begin 
	IF landmask_com(ix,iy) EQ 1 THEN BEGIN
	p_tas2d_ave(ix,iy)=MEAN(tas2d_ave(ix,iy,*),/NaN)
	p_tasc2d_ave(ix,iy)=MEAN(tasc2d_ave(ix,iy,*),/NaN)
        ENDIF 
ENDFOR 
ENDFOR

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS',charthick=1.5 ;,position=[0.05,0.55,0.45,0.95] 
contour,p_obs2d_m(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot 
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' ENS AVER',charthick=1.5 ;,position=[0.05,0.55,0.45,0.95] 
contour,p_tas2d_ave(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot 
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORR ENS AVER',charthick=1.5 ;,position=[0.05,0.55,0.45,0.95] 
contour,p_tasc2d_ave(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot 
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 



ENDFOR ;END MONTH LOOP

STOP






END
