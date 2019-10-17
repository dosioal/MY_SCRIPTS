PRO MAKE_TREND 
DEVICE,decomposed=0
loadct,39
!p.background=255
!p.color=0

; MAKE WSDI - YEARLY
;=============================================
VAR='tasmax'
;VAR='tasmin'

STAT='SM'
STAT='TXx'
;STAT='WSDI' ; TOTAL N OF DAYS WITH SPELL LENGHT> THRES  DASY (= ETCCDI WSDI)
;STAT='WSDI_M' ;  MAX WARM SPEL LENGHT
STAT='HWMId'

DIR_HOME='/media/NAS/LUC/SMHI/LR/'
RUN=['r1','r2','r3','r4','r5','r6','r7']
NRUN=N_ELEMENTS(RUN)

;SCEN=['hist','rcp85'] ;1951-2005'
;YEAR_INI=[1976,2006]
;YEAR_END=[2005,2100]

SCEN='hist' ;1951-2005'
IF STAT EQ 'TXx' THEN YEAR_INI=1976 ELSE YEAR_INI=1971 
YEAR_END=2005

NSCEN=N_ELEMENTS(SCEN)

;===============================================
;OBS
OBS_1='NCEP-2'
YEAR_INI_OBS_1=1979
YEAR_END_OBS_1=2010
IF VAR EQ 'tasmax' THEN VAR_OBS_1='tmax'
DIR_OBS_1='/media/NAS/AFRICA-OBS/'+OBS_1+'/'+VAR_OBS_1

OBS_2='ERAINT'
YEAR_INI_OBS_2=1979
YEAR_END_OBS_2=2010
IF VAR EQ 'tasmax' THEN VAR_OBS_2='mx2t'
DIR_OBS_2='/media/NAS/AFRICA-OBS/'+OBS_2+'/tasmax'

;===============================================
YEAR_INI_T=MIN([YEAR_INI,YEAR_INI_OBS_1])
YEAR_END_T=MAX([YEAR_END,YEAR_END_OBS_1])
NYEAR_T=FIX(YEAR_END_T)-FIX(YEAR_INI_T)+1
print,YEAR_INI_T,YEAR_END_T,NYEAR_T

;NYEAR_1=FIX(YEAR_END)-FIX(YEAR_INI)+1
;;NYEAR_2=FIX(YEAR_END(1))-FIX(YEAR_INI(1))+1
;NYEAR_T=NYEAR_1;+NYEAR_2


;MASK
MASK=['AUS', 'AMZ', 'SSA', 'CAM', 'WNA', 'CNA', 'ENA', 'ALA', 'GRL', 'MED', 'NEU', 'WAF', $
     'EAF', 'SAF', 'SAH', 'SEA', 'EAS', 'SAS', 'CAS', 'TIB', 'NAS', 'WORLD']

NMASK=N_ELEMENTS(MASK)
LON1=[110, -82, -76, -116, -130, -103, -85, -170, -103, -10, -10, -20, $
      22, -10, -20, 95, 100, 65, 40, 75, 40, -180] 
LON2=[155, -34, -40, -83, -103, -85, -60, -103, -10, 40, 40, 22, $
      52, 52, 65, 155, 145, 100, 75, 100, 180, 180]
LAT1=[-45, -20, -56, 10, 30, 30, 25, 60, 50, 30, 48, -12, $
      -12, -35, 18, -11, 20, 5, 30, 30, 50, -56]
LAT2=[-11, 12, -20, 30, 60, 50, 50, 72, 85, 48, 75, 18, $
      18, -12, 30, 20, 50, 30, 50, 50, 70, 85]

;===============================================
;LANDMASK AND DIMENSIONS
;===============================================
FILE_LAND='lsm-t511_05.nc'
LM_FILE=DIR_HOME+FILE_LAND
fileID = NCDF_Open(LM_FILE)
varID = NCDF_VarID(fileID,'LSM')
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
NCDF_VARGET,fileID,'LSM',landmask;,count=[nx,ny],offset=[0,0]
NCDF_CLOSE, fileID
landmask=REVERSE(landmask,2)
help,landmask

lon=FLTARR(nx,ny)
lat=FLTARR(nx,ny)

close,1
openr,1,DIR_HOME+'lon_05.txt'
WORK=FLTARR(nx)
FOR iy=0,ny-1 DO BEGIN
readf,1,work
lon(*,iy)=WORK
ENDFOR
close,1
lon=REVERSE(lon,2)

openr,1,DIR_HOME+'lat_05.txt'
WORK=FLTARR(nx)
FOR iy=0,ny-1 DO BEGIN
readf,1,work
lat(*,iy)=WORK
ENDFOR
close,1
lat=REVERSE(lat,2)

;===============================================
; READ OBS
;===============================================
WSDI_Y_O=FLTARR(nx,ny,NYEAR_T,2)*!VALUES.F_NaN
WSDI_O=FLTARR(nx,ny,2) ; 1975-2005,1.5C, 2C, 2071-2100
WSDI_Md_O=FLTARR(nx,ny,2) ; MEDIAN
WSDI_Mx_O=FLTARR(nx,ny,2) ; MAX

;==========
;NCEP
;==========
DIR_SM=DIR_OBS_1+'/PP/'+STAT

IF STAT EQ 'TXx' THEN BEGIN
;OPEN NCETCDF FILE'
add_offset = 447.65
scale_factor = 0.01

DATA_FILE=DIR_SM+'/'+VAR_OBS_1+'_TXx.nc'
fileID = NCDF_Open(DATA_FILE)
varID = NCDF_VarID(fileID, VAR_OBS_1)
varInfo = NCDF_VarInq(fileID, varID)
dimIDs = varInfo.dim
nDims = N_Elements(dimIDs)
dims_a = IntArr(nDims)
FOR j=0,nDims-1 DO BEGIN &$
NCDF_DimInq, fileID, dimIDs[j], dname, dsize &$
dims_a[j] = dsize &$
ENDFOR
IF ndims EQ 3 THEN BEGIN
nx=dims_a(0)
ny=dims_a(1)
nt=dims_a(2)
print,nx,ny,nt
NCDF_VARGET,fileID,VAR_OBS_1,WORK
help,work
WSDI_Y_O(*,*,YEAR_INI_OBS_1-YEAR_INI_T:YEAR_INI_OBS_1-YEAR_INI_T+NT-1,0)=REVERSE(REFORM(WORK(*,*,*))*scale_factor+add_offset,2)
ENDIF ELSE BEGIN
nx=dims_a(0)
ny=dims_a(1)
nt=dims_a(3)
print,nx,ny,nt
NCDF_VARGET,fileID,VAR_OBS_1,WORK
help,work
WSDI_Y_O(*,*,YEAR_INI_OBS_1-YEAR_INI_T:YEAR_INI_OBS_1-YEAR_INI_T+NT-1,0)=REVERSE(REFORM(WORK(*,*,0,*))*scale_factor+add_offset,2)
ENDELSE
NCDF_CLOSE, fileID
help,WSDI_Y_O

ENDIF ELSE BEGIN ;READ IDL FILE
FILEO=DIR_SM+'/'+VAR_OBS_1+'_'+STRMID(YEAR_INI_OBS_1,4,4)+'-'+STRMID(YEAR_END_OBS_1,4,4)+'_'+STAT+'.dat'
restore,fileo
NT=N_ELEMENTS(REFORM(WSDI_Y(0,0,*)))
WSDI_Y_O(*,*,YEAR_INI_OBS_1-YEAR_INI_T:YEAR_INI_OBS_1-YEAR_INI_T+NT-1,0)=WSDI_Y
help,WSDI_Y_O
ENDELSE

;==========
;ERAINT
;==========
DIR_SM=DIR_OBS_2+'/PP/'+STAT

IF STAT EQ 'TXx' THEN BEGIN
;OPEN NCETCDF FILE'
add_offset = 266.618892781034 ;
scale_factor = 0.00232674261932562

DATA_FILE=DIR_SM+'/'+VAR+'_TXx.nc'
fileID = NCDF_Open(DATA_FILE)
varID = NCDF_VarID(fileID, VAR_OBS_2)
varInfo = NCDF_VarInq(fileID, varID)
dimIDs = varInfo.dim
nDims = N_Elements(dimIDs)
dims_a = IntArr(nDims)
FOR j=0,nDims-1 DO BEGIN &$
NCDF_DimInq, fileID, dimIDs[j], dname, dsize &$
dims_a[j] = dsize &$
ENDFOR
IF ndims EQ 3 THEN BEGIN
nx=dims_a(0)
ny=dims_a(1)
nt=dims_a(2)
print,nx,ny,nt
NCDF_VARGET,fileID,VAR_OBS_2,WORK
help,work
WSDI_Y_O(*,*,YEAR_INI_OBS_2-YEAR_INI_T:YEAR_INI_OBS_2-YEAR_INI_T+NT-1,1)=REVERSE(REFORM(WORK(*,*,*))*scale_factor+add_offset,2)
ENDIF ELSE BEGIN
nx=dims_a(0)
ny=dims_a(1)
nt=dims_a(3)
print,nx,ny,nt
NCDF_VARGET,fileID,VAR_OBS_2,WORK
help,work
WSDI_Y_O(*,*,YEAR_INI_OBS_2-YEAR_INI_T:YEAR_INI_OBS_2-YEAR_INI_T+NT-1,1)=REVERSE(REFORM(WORK(*,*,0,*))*scale_factor+add_offset,2)
ENDELSE
NCDF_CLOSE, fileID
help,WSDI_Y_O

ENDIF ELSE BEGIN ;READ IDL FILE
FILEO=DIR_SM+'/'+VAR+'_'+STRMID(YEAR_INI_OBS_2,4,4)+'-'+STRMID(YEAR_END_OBS_2,4,4)+'_'+STAT+'.dat'
restore,fileo
NT=N_ELEMENTS(REFORM(WSDI_Y(0,0,*)))
WSDI_Y_O(*,*,YEAR_INI_OBS_2-YEAR_INI_T:YEAR_INI_OBS_2-YEAR_INI_T+NT-1,1)=WSDI_Y
help,WSDI_Y_O
ENDELSE

;===============================================
; READ MODELS
;===============================================
WSDI_Y_T=FLTARR(nx,ny,NYEAR_T,NRUN)*0.-999.
WSDI_M=FLTARR(nx,ny,NRUN) ; 1975-2005,1.5C, 2C, 2071-2100
WSDI_Md_M=FLTARR(nx,ny,NRUN) ; 1975-2005,1.5C, 2C, 2071-2100
WSDI_Mx_M=FLTARR(nx,ny,NRUN) ; 1975-2005,1.5C, 2C, 2071-2100

ic=0 ;counter for window plot
;===============================================
FOR ir=0,NRUN-1 DO BEGIN ;RCM LOOP
;===============================================

;===============================================
;DEF OF VARIABLES
;===============================================

;DIR MODELS
DIR_RUN=DIR_HOME+RUN(ir)+'/'+VAR+'/'
DIR_SM=DIR_RUN+'PP/'+STAT

print,''
print,'====================================='
print,'READING ',STAT,' FOR '
print,'MODEL= ',RUN(ir)
print,'YSTART ',YEAR_INI
print,'YEND ',YEAR_END
PRINT,'NYEAR=',NYEAR_T
print,'====================================='

IF STAT EQ 'TXx' THEN BEGIN
;OPEN NCETCDF FILE'
DATA_FILE=DIR_SM+'/'+VAR+'_TXx.nc'
fileID = NCDF_Open(DATA_FILE)
varID = NCDF_VarID(fileID, VAR)
varInfo = NCDF_VarInq(fileID, varID)
dimIDs = varInfo.dim
nDims = N_Elements(dimIDs)
dims_a = IntArr(nDims)
FOR j=0,nDims-1 DO BEGIN &$
NCDF_DimInq, fileID, dimIDs[j], dname, dsize &$
dims_a[j] = dsize &$
ENDFOR
IF ndims EQ 3 THEN BEGIN
nx=dims_a(0)
ny=dims_a(1)
nt=dims_a(2)
print,nx,ny,nt
NCDF_VARGET,fileID,VAR,WORK
help,work
WSDI_Y_T(*,*,YEAR_INI-YEAR_INI_T:YEAR_INI-YEAR_INI_T+NT-1,ir)=REVERSE(REFORM(WORK(*,*,*)),2)
ENDIF ELSE BEGIN
nx=dims_a(0)
ny=dims_a(1)
nt=dims_a(3)
print,nx,ny,nt
NCDF_VARGET,fileID,VAR,WORK
help,work
WSDI_Y_T(*,*,YEAR_INI-YEAR_INI_T:YEAR_INI-YEAR_INI_T+NT-1,ir)=REVERSE(REFORM(WORK(*,*,0,*)),2)
ENDELSE
NCDF_CLOSE, fileID
help,WSDI_Y_T
;WSDI_Y_T=REVERSE(WSDI_Y_T,2)

ENDIF ELSE BEGIN ;READ IDL FILE
FILEO=DIR_SM+'/'+VAR+'_hist_'+STRMID(YEAR_INI,4,4)+'-'+STRMID(YEAR_END,4,4)+'_'+STAT+'.dat'
restore,fileo
NT=N_ELEMENTS(REFORM(WSDI_Y(0,0,*)))
WSDI_Y_T(*,*,YEAR_INI-YEAR_INI_T:YEAR_INI-YEAR_INI_T+NT-1,ir)=WSDI_Y
ENDELSE

ic=ic+1
ENDFOR ; END RCM

; COMMON PERIOD
t1=MAX([YEAR_INI,YEAR_INI_OBS_1])-YEAR_INI_T+1   ;STARTS IN 1980
t2=YEAR_END_T-MIN([YEAR_END,YEAR_END_OBS_1])

;====================
;TRENDS
;====================
xvec=INDGEN(nyear_t-t2-t1)
VAR_TREND=FLTARR(nx,ny,NRUN+2,2)
For ix=0,nx-1 DO BEGIN &$
For iy=0,ny-1 DO BEGIN &$
IF landmask(ix,iy) GT 0.5 THEN BEGIN &$
FOR io=0,1 DO BEGIN &$
yvec=REFORM(WSDI_Y_O(ix,iy,t1:nyear_t-t2-1,io)) &$
we=WHERE(yvec LT 0) &$
IF we(0) NE -1 THEN yvec(we)=!VALUES.F_NAN &$
t=TREND(xvec,yvec,SIGLEV=siglev,FIT=yfit)  &$
VAR_TREND(ix,iy,io,0)=t &$
VAR_TREND(ix,iy,io,1)=siglev &$
ENDFOR &$
FOR ir=0,NRUN-1 DO BEGIN &$
yvec=REFORM(WSDI_Y_T(ix,iy,t1:nyear_t-t2-1,ir)) &$
we=WHERE(yvec LT 0) &$
IF we(0) NE -1 THEN yvec(we)=!VALUES.F_NAN &$
t=TREND(xvec,yvec,SIGLEV=siglev,FIT=yfit)  &$
VAR_TREND(ix,iy,ir+2,0)=t &$
VAR_TREND(ix,iy,ir+2,1)=siglev &$
ENDFOR &$
ENDIF &$
ENDFOR &$
ENDFOR

ct_number=70
LOADCT,ct_number,RGB_TABLE=ct, /SILENT
MINCOL=0
MAXCOL=250
minvalue=-.5
maxvalue=.5
ContourDist=.1
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
levels_t = INDGEN(iNumOfConts)*ContourDist + MinValue
colors_t= REVERSE(INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol)

;STIPPELING
pattern_r = Obj_New('idlgrpattern', 1, ORIENTATION=-45,SPACING=4,THICK=0.8)


w = WINDOW(WINDOW_TITLE='TREND '+STAT,DIMENSIONS=[1200,1000])

w_plot= CONTOUR(VAR_TREND(*,*,0,0),lon,lat,c_value=levels_t,RGB_INDICES=colors_t,/FILL,/CURRENT,RGB_TABLE=ct_number, $
FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,3,1],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
       YRANGE=[-60,90],XRANGE=[-180,180],title='TREND '+STAT+' '+OBS_1+' 1979-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
ks_sig=REFORM(VAR_TREND(*,*,0,1))
WE=WHERE(KS_SIG GE 0.05 AND FINITE(KS_SIG,/NaN) EQ 0)
WE_N=WHERE(KS_SIG LT  0.05 AND FINITE(KS_SIG,/NaN) EQ 0)
KS_SIG(WE)=0
KS_SIG(WE_N)=1
KS_SIG(WHERE(landmask LT 0.5))=0
s_plot= CONTOUR(KS_SIG,lon,lat,/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black")
;s_plot= CONTOUR(VAR_TREND(*,*,0,1),lon,lat,c_value=[0.,0.05],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black")

tickname = STRING(levels_t, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-82,180,-77],/DATA,FONT_SIZE=8)


w_plot= CONTOUR(VAR_TREND(*,*,1,0),lon,lat,c_value=levels_t,RGB_INDICES=colors_t,/FILL,/CURRENT,RGB_TABLE=ct_number, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,3,2],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title='TREND '+STAT+' '+OBS_2+' 1979-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
ks_sig=REFORM(VAR_TREND(*,*,1,1))
WE=WHERE(KS_SIG GE 0.05 AND FINITE(KS_SIG,/NaN) EQ 0)
WE_N=WHERE(KS_SIG LT  0.05 AND FINITE(KS_SIG,/NaN) EQ 0)
KS_SIG(WE)=0
KS_SIG(WE_N)=1
KS_SIG(WHERE(landmask LT 0.5))=0
s_plot= CONTOUR(KS_SIG,lon,lat,/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black")

FOR ir=0,NRUN-1 DO BEGIN &$
w_plot= CONTOUR(VAR_TREND(*,*,ir+2,0),lon,lat,c_value=levels_t,RGB_INDICES=colors_t,/FILL,/CURRENT,RGB_TABLE=ct_number, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,3,ir+3],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title='TREND '+STAT+' '+RUN(ir)+' 1979-2005') &$
mc = MAPCONTINENTS(THICK=0.5) &$
w_plot['axis0'].GRIDSTYLE=6 &$
w_plot['axis1'].GRIDSTYLE=6 &$
w_plot['axis0'].TITLE="lon" &$
w_plot['axis1'].TITLE="lat" &$
w_plot['axis0'].TICKFONT_SIZE=8 &$
w_plot['axis1'].TICKFONT_SIZE=8 &$
ks_sig=REFORM(VAR_TREND(*,*,ir+2,1)) &$
WE=WHERE(KS_SIG GE 0.05 AND FINITE(KS_SIG,/NaN) EQ 0) &$
WE_N=WHERE(KS_SIG LT  0.05 AND FINITE(KS_SIG,/NaN) EQ 0) &$
KS_SIG(WE)=0 &$
KS_SIG(WE_N)=1 &$
KS_SIG(WHERE(landmask LT 0.5))=0 &$
s_plot= CONTOUR(KS_SIG,lon,lat,/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black") &$
ENDFOR

time=FIX(FINDGEN(NYEAR_T)+YEAR_INI_T)


STOP
END
