;
PRO make_grid_multi_scen
;
DEVICE,decompose=0
;
;
;-----------------------------------------------------------------------------------------------------------
home_data='/media/disk/POSTPROC/BC_Regional/'
home_const='/media/disk/DATA/ENSOBS/ENS_MODELS/'

MODEL_ID=['ETHZ-CLM','DMI-HIRHAM5','KNMI-RACMO2']
CASE_ID=['SCN_HadCM3Q0','A1B_ECHAM5','A1B_ECHAM5-r3']
nmod=1.*N_ELEMENTS(MODEL_ID)

;VARIABLE (T,Ptot,Tmax,Tmin)
VAR='Ptot'
;VAR='T'
;VAR='Tmin'
;VAR='Tmax'

CONSTRUCTION_PERIOD_START  =  '1961'
CONSTRUCTION_PERIOD_STOP   =  '1990'

;APPLICATION_PERIOD_START   =  '1991'
;APPLICATION_PERIOD_STOP    =  '2000'
APPLICATION_PERIOD_START   =  '2081'
APPLICATION_PERIOD_STOP    =  '2090'

NYEARS=fix(APPLICATION_PERIOD_STOP)-fix(APPLICATION_PERIOD_START)+1

month=['01','02','03','04','05','06','07','08','09','10','11','12']
NDAYS=fltarr(12)
MM_I=0
MM_E=11

nx=fltarr(nmod)
ny=fltarr(nmod)

x0=fltarr(nmod)
y0=fltarr(nmod)
nx0=fltarr(nmod)
ny0=fltarr(nmod)

lm_var     = 'tg'
mv_cutoff  = 1.0



;
; construct a land mask file from the given data file
;LM_FILE_A=home_const+'read_in_'+MODEL_ID+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'.nc'
LM_FILE_A=home_const+'read_in_'+MODEL_ID+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'.nc'

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
    NCDF_CLOSE, fileID

ENDFOR ;END MODEL LOOP

    nxm=max(nx)
    nym=max(ny)
    x0m=min(x0)
    y0m=min(y0)

   landmask_m=dblarr(nxm,nym,nmod)
   landmask_m(*,*,*)=0

;===============================================
tas2d_m = FLTARR(nxm,nym,31.*NYEARS,nmod)
tas2d_m(*,*,*,*) = !VALUES.F_NAN
;tas2d_m(*,*,*,*) = -999.
tasc2d_m = FLTARR(nxm,nym,31.*NYEARS,nmod)
tasc2d_m(*,*,*,*) = !VALUES.F_NAN
;tasc2d_m(*,*,*,*) = -999.
obs2d_m = FLTARR(nxm,nym,31.*NYEARS,nmod)
obs2d_m(*,*,*,*) = !VALUES.F_NAN
;obs2d_m(*,*,*,*) = -999.

;MONTHLY MEAN
;tas2d_mm=fltarr(nxm,nym,nmod)
;tasc2d_mm=fltarr(nxm,nym,nmod)
;obs2d_mm=fltarr(nxm,nym,nmod)

;LOOP OVER MONTHS
FOR im=MM_I,MM_E DO BEGIN
;im=6
PRINT,'MONTH= ',month(im)

;LOOP OVER MODELS
FOR imd=0,nmod-1 do begin


IF CONSTRUCTION_PERIOD_START EQ '1961' AND CONSTRUCTION_PERIOD_STOP EQ '1990' THEN BEGIN 
MOD_DIR=home_data+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_1961-2100/BC_data/'
MOD_BC_DIR=home_data+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_1961-2100/BC_data/'
;MOD_BC_DIR=home_data+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'/BC_data/'
ENDIF ELSE BEGIN
MOD_DIR=home_data+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/BC_data/'
MOD_BC_DIR=home_data+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/BC_data/'
ENDELSE
print,MOD_DIR
print,MOD_BC_DIR

FILE_MOD='mod_'+VAR+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
FILE_BC=VAR+'_'+'BCed_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'

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


  landmask_c=landmask_c/100
  NCDF_VARGET,fileID,'latitude',lat
  NCDF_VARGET,fileID,'longitude',lon
  dx=lon(1)-lon(0)
  dy=lat(1)-lat(0)
  nx0(imd)=ABS(ROUND((x0m-lon(0))/dx))
  ny0(imd)=ABS(ROUND((y0m-lat(0))/dy))
  print,nx0(imd),ny0(imd)

  landmask_m(nx0(imd):nx0(imd)+nx(imd)-1,ny0(imd):ny0(imd)+ny(imd)-1,imd)=landmask_c(*,*)

    NCDF_CLOSE, fileID

;TAS AND OBS
LM_FILE_A=home_const+'/read_in_'+MODEL_ID(imd)+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'.nc'
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

  nx(imd)=dims_a(0)
  ny(imd)=dims_a(1)
  landmask_a=dblarr(nx(imd),ny(imd))
  NCDF_VARGET,fileID,lm_var,landmask_a,count=[dims_a(0),dims_a(1),1],offset=[0,0,0]
   we_a=where(landmask_a ge mv_cutoff)
   land_a=reform(we_c(sort(we_a)))+1
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

print,'MOD ORIG ',MOD_DIR+FILE_MOD
idldata=0
restore,MOD_DIR+FILE_MOD

CASE VAR OF
    'T': tas=idldata
    'Ptot': tas=idldata
    'Tmin': tas=idldata
    'Tmax': tas=idldata
ENDCASE

NDAYS(im)=min([n_elements(REFORM(tas_c(0,*))),n_elements(REFORM(tas(0,*)))])
tas2d = FLTARR(nx(imd),ny(imd),ndays(im))
tas2d(*,*,*)=!VALUES.F_NAN
;tas2d(*,*,*)=-999.
tasc2d = FLTARR(nx(imd),ny(imd),ndays(im))
tasc2d(*,*,*)=!VALUES.F_NAN
;tasc2d(*,*,*)=-999.

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
ENDFOR
FOR i=0L,n_elements(land_c)-1 DO BEGIN &$
;		print,i,lon_c(i),lat_c(i)
         tas2d(lon_c(i),lat_c(i),*)=tas(i,0:NDAYS(im)-1)
ENDFOR

tas2d_m(nx0(imd):nx0(imd)+nx(imd)-1,ny0(imd):ny0(imd)+ny(imd)-1,0:NDAYS(im)-1,imd)=tas2d
tasc2d_m(nx0(imd):nx0(imd)+nx(imd)-1,ny0(imd):ny0(imd)+ny(imd)-1,0:NDAYS(im)-1,imd)=tasc2d

;MONTHLY MEAN
;FOR iy=0,nym-1 do begin
;FOR ix=0,nxm-1 do begin
;	tas2d_mm(ix,iy,imd)=MEAN(tas2d_m(ix,iy,*,imd),/NAN)
;	tasc2d_mm(ix,iy,imd)=MEAN(tasc2d_m(ix,iy,*,imd),/NAN)
;ENDFOR
;ENDFOR

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

ENDFOR ;END MODEL LOOP

;ENS AVE
;tas2d_ave=fltarr(nxm,nym,ndays(im))
;tasc2d_ave=fltarr(nxm,nym,ndays(im))
;tas2d_mm_ave=fltarr(nxm,nym)
;tasc2d_mm_ave=fltarr(nxm,nym)

;FOR i=0,ndays(im)-1 do begin &$
;FOR iy=0,nym-1 do begin &$
;FOR ix=0,nxm-1 do begin &$
;	tas2d_ave(ix,iy,i)=MEAN(tas2d_m(ix,iy,i,*),/NAN) &$
;	tasc2d_ave(ix,iy,i)=MEAN(tasc2d_m(ix,iy,i,*),/NAN) &$
;ENDFOR &$
;ENDFOR &$
;ENDFOR

;MONTHLY MEAN
;FOR iy=0,nym-1 do begin &$
;FOR ix=0,nxm-1 do begin &$
;	tas2d_mm_ave(ix,iy)=MEAN(tas2d_mm(ix,iy,*),/NAN) &$
;	tasc2d_mm_ave(ix,iy)=MEAN(tasc2d_mm(ix,iy,*),/NAN) &$
;ENDFOR &$
;ENDFOR

;SAVING ENSE AVERAGE DATA
;print,'SAVING ENSEMBLE AVERAGE',month(im)
;ofile='DATA/ENS_AVE_MOD_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;save,filename=ofile,tas2d_ave,nxm,nym,landmask_c,mndays
;ofile='DATA/ENS_AVE_MOD_'+VAR+'_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;save,filename=ofile,tasc2d_ave,nxm,nym,landmask_c,mndays
;
ENDFOR ;END MONTH LOOP

landmask_com=fltarr(nxm,nym)
FOR ix=0,nxm-1 do begin
FOR iy=0,nym-1 do begin
      landmask_com(ix,iy)=MEAN(landmask_m(ix,iy,*))
      IF landmask_com(ix,iy) GT 0 THEN landmask_com(ix,iy)=1
ENDFOR
ENDFOR

STOP

p_tas2d_m=fltarr(nxm,nym,NDAYS(im),nmod)
p_tasc2d_m=fltarr(nxm,nym,NDAYS(im),nmod)
;p_tas2d_mm=fltarr(nxm,nym,nmod)
;p_tasc2d_mm=fltarr(nxm,nym,nmod)
p_tas2d_ave=fltarr(nxm,nym,NDAYS(im))
p_tasc2d_ave=fltarr(nxm,nym,NDAYS(im))
;p_tas2d_mm_ave=fltarr(nxm,nym)
;p_tasc2d_mm_ave=fltarr(nxm,nym)
p_tas2d_m(*,*,*,*)=-999.
p_tasc2d_m(*,*,*,*)=-999.
;p_tas2d_mm(*,*,*)=-999.
;p_tasc2d_mm(*,*,*)=-999.
p_tas2d_ave(*,*,*)=-999.
p_tasc2d_ave(*,*,*)=-999.
;p_tas2d_mm_ave(*,*)=-999.
;p_tasc2d_mm_ave(*,*)=-999.

FOR id=0,NDAYS(im)-1 do begin &$
FOR ix=0,nxm-1 do begin &$
FOR iy=0,nym-1 do begin &$
FOR imd=0,nmod-1 do begin &$
	IF tas2d_m(ix,iy,id,imd) GE 0 THEN p_tas2d_m(ix,iy,id,imd)=tas2d_m(ix,iy,id,imd) &$
	IF tasc2d_m(ix,iy,id,imd) GE 0 THEN p_tasc2d_m(ix,iy,id,imd)=tasc2d_m(ix,iy,id,imd) &$
ENDFOR &$
	IF tas2d_ave(ix,iy,id) GE 0 THEN p_tas2d_ave(ix,iy,id)=tas2d_ave(ix,iy,id) &$
	IF tasc2d_ave(ix,iy,id) GE 0 THEN p_tasc2d_ave(ix,iy,id)=tasc2d_ave(ix,iy,id) &$
ENDFOR &$
ENDFOR &$
ENDFOR

;FOR ix=0,nxm-1 do begin &$
;FOR iy=0,nym-1 do begin &$
;FOR imd=0,nmod-1 do begin &$
;	IF tas2d_mm(ix,iy,imd) GE 0 THEN p_tas2d_mm(ix,iy,imd)=tas2d_mm(ix,iy,imd)  &$
;	IF tasc2d_mm(ix,iy,imd) GE 0 THEN p_tasc2d_mm(ix,iy,imd)=tasc2d_mm(ix,iy,imd) &$
;ENDFOR &$
;	IF tas2d_mm_ave(ix,iy) GE 0 THEN p_tas2d_mm_ave(ix,iy)=tas2d_mm_ave(ix,iy)  &$
;	IF tasc2d_mm_ave(ix,iy) GE 0 THEN p_tasc2d_mm_ave(ix,iy)=tasc2d_mm_ave(ix,iy) &$
;ENDFOR &$
;ENDFOR

MinCol=20
MaxCol=245
Minvalue=230
Maxvalue=310
;Minvalue=270
;Maxvalue=320
ContourDist=5
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrCenterValues = FLTARR(N_ELEMENTS(rgrContValues)-1)
rgrColValues = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
ColorTBL=62


window,0,retain=2,xsize=1250,ysize=900
SetColorTable,ColorTBL
!p.background=0
!p.color=255
;!p.multi=[0,5,nmod+1]
!p.multi=[0,2,nmod+1]
	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MODEL DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_tas2d_ave(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$

	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORR DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_tasc2d_ave(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$

window,1,retain=2,xsize=1250,ysize=900
SetColorTable,ColorTBL
!p.background=0
!p.color=255
;!p.multi=[0,5,nmod+1]
!p.multi=[0,2,nmod+1]
;OBS'
FOR imd=0,nmod-1 DO BEGIN &$
	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MODEL DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_tas2d_mm(*,*,imd),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$

	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORR DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_tasc2d_mm(*,*,imd),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$
ENDFOR
	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MODEL DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_tas2d_mm_ave(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$

	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORR DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_tasc2d_mm_ave(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$


STOP

tas2d_com = FLTARR(nxm,nym,nmod,31.*NYEARS)
tasc2d_com = FLTARR(nxm,nym,nmod,31.*NYEARS)
ave_tas2d_com = FLTARR(nxm,nym,31.*NYEARS)
ave_tasc2d_com = FLTARR(nxm,nym,31.*NYEARS)
tas2d_com_sm(*,*,*,*)=-999.
tasc2d_com_sm(*,*,*,*)=-999.
ave_tas2d_com_sm(*,*,*)=-999.
ave_tasc2d_com_sm(*,*,*)=-999.

;==============================================
; DATA

MM_I=0  ;STARTING MONTH
MM_E=11 ;ENDING MONTH
NMONTHS=MM_E-MM_I+1

;LOOP OVER MODELS
FOR imd=0,nmod-1 DO BEGIN 
print,'MODEL = ',MODEL_ID(imd)+'_'+CASE_ID(imd)



NUMLANDPOINTS_c=n_elements(land_c)
NUMLANDPOINTS_a=n_elements(land_a)

help,NUMLANDPOINTS_c,NUMLANDPOINTS_a

nx=1.*dims_a(0)
ny=1.*dims_a(1)
NPOINTS=nx*ny


;FILE_COR=VAR+'_cor_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+month(im)+'.dat'

print,'OBS ',OBS_DIR+FILE_OBS
print,'MOD ORIG ',MOD_DIR+FILE_MOD
;print,'FACTORS ',FAC_DIR+FILE_COR
idldata=0
restore,MOD_DIR+FILE_MOD
CASE VAR OF
    'T': tas=idldata
    'Ptot': tas=idldata
    'Tmin': tas=idldata
    'Tmax': tas=idldata
ENDCASE

IF VAR EQ 'Ptot' THEN BEGIN
	tas=tas*3600.*24.
	tas_c=tas_c*3600.*24.
ENDIF

print,'MONTH= ',month(im),' NDAYS(im)= ',NDAYS(im)
tas_f=fltarr(nx*ny,NDAYS(im))
tasc_f=fltarr(nx*ny,NDAYS(im))
tas_f(*,*)=!VALUES.F_NAN
tasc_f(*,*)=!VALUES.F_NAN

FOR i=0,NDAYS(im)-1 DO BEGIN
tas_f(land_a,i)=tas(*,i)
tasc_f(land_c,i)=tas_c(*,i)

;CORRECTION FOR PREP <0 (e.g KNMI)
IF var EQ 'Ptot' THEN BEGIN
p0=where(tas_f(*,i) LT 0) 
IF N_ELEMENTS(p0) GT 1 THEN tas_f(p0,i)=0.
ENDIF

land_t=where(tas_f(*,i) GE 0)
land_tc=where(tasc_f(*,i) GE 0)
ENDFOR
print,tas_f(9000,0)

loadct,39
!p.multi=0
window,3,retain=2
contour,land2d_a,title='OBS'
plots,lon_a,lat_a,psym=4,color=30

window,4,retain=2
contour,land2d_c,title='MOD'
plots,lon_c,lat_c,psym=4,color=30

tasm2d_f = FLTARR(nx,ny);,ndays(im))
tascm2d_f = FLTARR(nx,ny);,ndays(im))
;tasm2d_f(*,*)=!VALUES.F_NAN
;tascm2d_f(*,*)=!VALUES.F_NAN
tasm2d_f(*,*)=-999.
tascm2d_f(*,*)=-999.

print,tas_f(9000,0),lon_a(9000),lat_a(9000)
FOR i=0L,NUMLANDPOINTS_A-1 DO BEGIN &$
	 ww=MEAN(tas(i,*),/NaN) &$
         ;tasm2d_f(lon_a(i),lat_a(i),*)=tas &$
         tasm2d_f(lon_a(i),lat_a(i))=ww &$
ENDFOR
print,tasm2d_f(lon_a(9000),lat_a(9000))
b=WHERE(tasm2d_f GE 0,count)
print,count
b=WHERE(tas_f(*,0) GE 0,count)
print,count
print,tasm2d_f(100,100)
FOR i=0L,NUMLANDPOINTS_c-1 DO BEGIN &$
	 ww=MEAN(tas_c(i,*),/NaN) &$
         tascm2d_f(lon_c(i),lat_c(i))=ww &$
ENDFOR
print,tascm2d_f(lon_a(9000),lat_a(9000))
print,tascm2d_f(100,100)


;PLOTS FOR CHECKS
DEVICE,decompose=0

IF VAR EQ 'Ptot' THEN BEGIN
MinCol=20
MaxCol=235
minvalue=0
Maxvalue=6
rInterval=Maxvalue-Minvalue
ContourDist=.5
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrCenterValues = FLTARR(N_ELEMENTS(rgrContValues)-1)
FOR ii = 0, N_ELEMENTS(rgrContValues)-2 DO $
	       rgrCenterValues[ii] = (rgrContValues[ii] + rgrContValues[ii+1]) / 2.
rgrColValues = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
rgrColValues=REVERSE(rgrColValues)
ColorTBL=62
SetColorTable,ColorTBL
!p.background=0
!p.color=255
ENDIF ELSE BEGIN
MinCol=20
MaxCol=245
Minvalue=250
Maxvalue=300
;Minvalue=270
;Maxvalue=320
ContourDist=5
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrCenterValues = FLTARR(N_ELEMENTS(rgrContValues)-1)
FOR ii = 0, N_ELEMENTS(rgrContValues)-2 DO $
                        rgrCenterValues[ii] = (rgrContValues[ii] + rgrContValues[ii+1]) / 2.
rgrColValues = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
ColorTBL=62
SetColorTable,ColorTBL
!p.background=0
!p.color=255
ENDELSE

window,1,retain=2
xsize=750.
ysize=550.
!p.multi=[0,1,2]
;TAS'
contour,land2d_a,xstyle=1,ystyle=1,c_colors=255,title=VAR+' '+month(im),position=[0.05,0.2,0.3,0.8],color=250
contour,tasm2d_f(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_a,xstyle=1,ystyle=1,/overplot,thick=0.5,color=250
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,/vertical,position=[0.95,0.2,0.97,0.8],charsize=1.5,color=255,/pscolor

;TAS'
contour,land2d_c,xstyle=1,ystyle=1,title=VAR+' CORECTED '+month(im),position=[0.35,0.2,0.6,0.8],color=250
;contour,tascm2d(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,tascm2d_f(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=250


;SAVING DATA
;ofile='DATA/MOD_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;save,filename=ofile,tas_f,tasm2d_f,lon_m,lat_m,land2d_m,land_com
;help,land_com
;ofile='DATA/MOD_BC_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;save,filename=ofile,tasc_f,tascm2d_f,lon_m,lat_m,land2d_m,land_com
;help,land_com
;ofile='DATA/OBS_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;save,filename=ofile,obs_f,obsm2d_f,lon_m,lat_m,land2d_m,land_com
;help,land_com
;
;ENDFOR ;END MONTH LOOP
;STOP

;##########################
;rLMOGOROV STATISTICS
;##########################

stat_tas=fltarr(NPOINTS)
prob_tas=fltarr(NPOINTS)
fn_tas=fltarr(NPOINTS,NDAYS(im))
tas_sor=fltarr(NPOINTS,NDAYS(im))

stat_tasc=fltarr(NPOINTS)
prob_tasc=fltarr(NPOINTS)
fn_tasc=fltarr(NPOINTS,NDAYS(im))
tasc_sor=fltarr(NPOINTS,NDAYS(im))

fn_obs=fltarr(NPOINTS,NDAYS(im))
obs_sor=fltarr(NPOINTS,NDAYS(im))

stat_tasc2d(*,*) =-999.
prob_tasc2d(*,*) =-999.

;fnm_tas=FLTARR(NPOINTS)
;fnm_tasc=FLTARR(NPOINTS)
;fnm_obs=FLTARR(NPOINTS)

;fnm_tas2d=FLTARR(nx,ny)
;fnm_tasc2d=FLTARR(nx,ny)
;fnm_obs2d=FLTARR(nx,ny)
;fnm_tas2d(*,*) = -999.;VALUES.F_NAN
;fnm_tasc2d(*,*) = -999.;VALUES.F_NAN
;fnm_obs2d(*,*) = -999.;VALUES.F_NAN

FOR i=0L,NPOINTS-1 DO BEGIN
	IF VAR EQ 'Ptot' THEN BEGIN
	ww_tas=TOTAL(fn_tas(i,*))
	ww_tasc=TOTAL(fn_tasc(i,*))
	ww_obs=TOTAL(fn_obs(i,*))
        ENDIF ELSE BEGIN
	ww_tas=MEAN(fn_tas(i,*),/NaN)
	ww_tasc=MEAN(fn_tasc(i,*),/NaN)
	ww_obs=MEAN(fn_obs(i,*),/NaN)
        ENDELSE
	 stat_tas2d(lon_a(i),lat_a(i))=stat_tas(i)
	 stat_tasc2d(lon_c(i),lat_c(i))=stat_tasc(i)
	 prob_tas2d(lon_a(i),lat_a(i))=prob_tas(i)
	 prob_tasc2d(lon_c(i),lat_c(i))=prob_tasc(i)
;	 fnm_tas2d(lon_a(i),lat_a(i))=ww_tas
;	 fnm_tasc2d(lon_c(i),lat_c(i))=ww_tasc
;	 fnm_obs2d(lon_a(i),lat_a(i))=ww_obs
ENDFOR

print,lon_m(npoints-1),lat_m(npoints-1)

;SAVING DATA
;ofile='DATA/ks_stat_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;save,filename=ofile,stat_tas,prob_tas,fn_tas,tas_sor,stat_tas2d,prob_tas2d,fnm_tas2d
;;save,filename=ofile,stat_tas,prob_tas,stat_tas2d,prob_tas2d
;ofile='DATA/ks_stat_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;save,filename=ofile,stat_tasc,prob_tasc,fn_tasc,tasc_sor,stat_tasc2d,prob_tasc2d,fnm_tasc2d
;;save,filename=ofile,stat_tasc,prob_tasc,stat_tasc2d,prob_tasc2d
;ofile='DATA/ks_stat_OBS_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;save,filename=ofile,fn_obs,obs_sor,fnm_obs2d

j=9000
PRINT,'KS STAT TAS= ',stat_tas(j),' PROB TAS= ',prob_tas(j)
PRINT,'KS STAT TAS COR= ',stat_tasc(j),' PROB TAS COR= ',prob_tasc(j)

ENDFOR ;END MONTH LOOP

;PLOTS FOR CHECK
window,4,retain=2
;STAT AND PROB
loadct,39
!p.background=255
!p.color=0
!p.multi=[0,2,2]

levels=findgen(11)/10.
colors=reverse(findgen(10)*19+15)
contour,land2d_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.05,0.55,0.45,0.95]
contour,stat_tas2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,min=0,max=1,divisions=N_ELEMENTS(levels)-1,c_colors=colors,charsize=1.5,color=0,position=[0.98,0.55,0.99,0.95],/vertical

contour,land2d_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR+' CORRECTED',position=[0.5,0.55,0.9,0.95]
contour,stat_tasc2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

levels=[0,1e-6,5e-5,1e-4,5e-4,1e-3,5e-3,1e-2,5e-2,1e-1,5e-1,1]
lev_c=['0','1e-5','5e-5','1e-4','5e-4','1e-3','5e-3','1e-2','5e-2','1e-1','5e-1','1']
colors=findgen(11)*20+30
contour,land2d_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+vAR,position=[0.05,0.05,0.45,0.45]
contour,prob_tas2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,c_colors=colors,levels=lev_c,divisions=N_ELEMENTS(levels)-1,charsize=1.5,color=0,position=[0.98,0.05,0.99,0.45],/vertical

contour,land2d_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR+' CORRECTED',position=[0.5,0.05,0.9,0.45]
contour,prob_tasc2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5



STOP
END
