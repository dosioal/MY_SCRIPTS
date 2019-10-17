PRO PLOT_EVAL 
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

OBS_1='NCEP'
YEAR_INI_OBS_1=1979
YEAR_END_OBS_1=2016
IF VAR EQ 'tasmax' THEN VAR_OBS_1='TMAX'
DIR_OBS_1='/media/NAS/LUC/'+OBS_1+'/'+VAR_OBS_1

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
;FILE_LAND='landmask_05.dat'
;restore,DIR_HOME+FILE_LAND
;help,landmask
;print,nx,ny

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

;================
;MULTI-YEAR MEAN,MED AND MAX
;================
WSDI_M_MED=FLTARR(nx,ny)
WSDI_Md_MED=FLTARR(nx,ny)
WSDI_Mx_MED=FLTARR(nx,ny)

;AVEAGE ONLY OVER COMMON PERIOD
t1=MAX([YEAR_INI,YEAR_INI_OBS_2])-YEAR_INI_T+1   ;STARTS IN 1980
t2=YEAR_END_T-MIN([YEAR_END,YEAR_END_OBS_2])
NYEARS=NYEAR_T-T2-T1

FOR ix=0,nx-1 DO BEGIN &$
FOR iy=0,ny-1 DO BEGIN &$
IF STAT EQ 'SM' OR STAT EQ 'TXx' THEN BEGIN
FOR ir=0,NRUN-1 DO BEGIN
WORK=REFORM(WSDI_Y_T(ix,iy,t1:nyear_t-t2-1,ir))
WORK(WHERE( WORK EQ -999.))=!VALUES.F_NaN
WSDI_M(ix,iy,ir)=MEAN(WORK,/NaN)-273.14 &$
WSDI_Md_M(ix,iy,ir)=MEDIAN(WORK)-273.14 &$
WSDI_Mx_M(ix,iy,ir)=MAX(WORK,/NaN)-273.14 &$
ENDFOR
FOR io=0,1 Do BEGIN
WORK=REFORM(WSDI_Y_O(ix,iy,t1:nyear_t-t2-1,io))
WORK(WHERE( WORK EQ -999.))=!VALUES.F_NaN
WSDI_O(ix,iy,io)=MEAN(WORK,/NaN)-273.14 &$
WSDI_Md_O(ix,iy,io)=MEDIAN(WORK)-273.14 &$
WSDI_Mx_O(ix,iy,io)=MAX(WORK,/NaN)-273.14 &$
ENDFOR
ENDIF ELSE BEGIN
FOR ir=0,NRUN-1 DO BEGIN
WORK=REFORM(WSDI_Y_T(ix,iy,t1:nyear_t-t2-1,ir))
WORK(WHERE( WORK EQ -999.))=!VALUES.F_NaN
WSDI_M(ix,iy,ir)=MEAN(WORK,/NaN) &$
WSDI_Md_M(ix,iy,ir)=MEDIAN(WORK) &$
WSDI_Mx_M(ix,iy,ir)=MAX(WORK,/NaN) &$
ENDFOR
FOR io=0,1 Do BEGIN
WORK=REFORM(WSDI_Y_O(ix,iy,t1:nyear_t-t2-1,io))
WORK(WHERE( WORK EQ -999.))=!VALUES.F_NaN
WSDI_O(ix,iy,io)=MEAN(WORK,/NaN) &$
WSDI_Md_O(ix,iy,io)=MEDIAN(WORK) &$
WSDI_Mx_O(ix,iy,io)=MAX(WORK,/NaN) &$
ENDFOR
ENDELSE

WSDI_M_MED(ix,iy)=MEDIAN(WSDI_M(ix,iy,*))
WSDI_Md_MED(ix,iy)=MEDIAN(WSDI_Md_M(ix,iy,*))
WSDI_Mx_MED(ix,iy)=MEDIAN(WSDI_Mx_M(ix,iy,*))

ENDFOR &$  ;ix
ENDFOR     ;iy


GOTO,JUMP_KS
;================
;K-S
;================
KS_EVAL=FLTARR(nx,ny,NRUN,2)

print,'KOLMOGOROV'
FOR ir=0,NRUN-1 DO BEGIN
FOR ix=0,nx-1 DO BEGIN &$
FOR iy=0,ny-1 DO BEGIN &$
IF landmaks(ix,iy) GT 0.5 THEN BEGIN
WORK=REFORM(WSDI_Y_T(ix,iy,t1:nyear_t-t2-1,ir))
WORK(WHERE( WORK EQ -999.))=!VALUES.F_NaN
WORK_1=REFORM(WSDI_Y_O(ix,iy,t1:nyear_t-t2-1,0))
WORK_1(WHERE( WORK_1 EQ -999.))=!VALUES.F_NaN
WORK_2=REFORM(WSDI_Y_O(ix,iy,t1:nyear_t-t2-1,1))
WORK_2(WHERE( WORK_2 EQ -999.))=!VALUES.F_NaN
NYEARS_KS=nyear_t-t2-t1
ks2,WORK,NYEARS_KS,WORK_1,NYEARS_KS,stat_v,prob_v,fn1,fn2,sor1,sor2
KS_EVAL(ix,iy,ir,0)=prob_v
ks2,WORK,NYEARS_KS,WORK_2,NYEARS_KS,stat_v,prob_v,fn1,fn2,sor1,sor2
KS_EVAL(ix,iy,ir,1)=prob_v
ENDIF
ENDFOR 
ENDFOR 
ENDFOR 

;MOD AGRRMENT KS
MOD_AGR_KS=FLTARR(NX,NY,NRUN,2)
THRES=0.05
FOR io=0,1 DO BEGIN &$
FOR ix=0,NX-1 DO BEGIN
FOR iy=0,NY-1 DO BEGIN
IF landmask(ix,iy) GT 0.5 THEN BEGIN  &$
FOR ir=0,NRUN -1 DO BEGIN &$
IF KS_EVAL(ix,iy,ir,0) LE THRES THEN MOD_AGR_KS(ix,iy,ir,0)=MOD_AGR_KS(ix,iy,ir,0)+1  &$
IF KS_EVAL(ix,iy,ir,1) LE THRES THEN MOD_AGR_KS(ix,iy,ir,1)=MOD_AGR_KS(ix,iy,ir,1)+1  &$
ENDFOR &$
ENDIF &$
ENDFOR &$
ENDFOR &$
ENDFOR

JUMP_KS:

;======================
;PLOT
;======================

loadct,39,RGB_TABLE=rgb
MINCOL=50.
MAXCOL=254

CASE STAT OF
'SM':BEGIN
minvalue=-10
maxvalue=50
ContourDist=5
minvalue_d=-5
maxvalue_d=5
ContourDist_d=.5
END
'TXx':BEGIN
minvalue=-10
maxvalue=50
ContourDist=5
minvalue_d=-5
maxvalue_d=5
ContourDist_d=.5
END
'WSDI':BEGIN
minvalue=0
maxvalue=60
ContourDist=6
minvalue_d=0
maxvalue_d=300
ContourDist_d=30
END
'WSDI_M':BEGIN
minvalue=0
maxvalue=360
ContourDist=36
minvalue_d=0
maxvalue_d=360
ContourDist_d=36
END
'HWMId':BEGIN
minvalue=250
maxvalue=320
ContourDist=5
minvalue_d=250
maxvalue_d=320
ContourDist_d=5
END
ENDCASE

rInterval=Maxvalue-Minvalue &$
iNumOfConts=FIX(rInterval/ContourDist)+1 &$
levels = INDGEN(iNumOfConts)*ContourDist + MinValue &$
colors = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol &$

rInterval_d=Maxvalue_d-Minvalue_d &$
iNumOfConts_d=FIX(rInterval_d/ContourDist_d)+1 &$
levels_d= INDGEN(iNumOfConts_d)*ContourDist_d + MinValue_d &$
colors_d = INDGEN(iNumOfConts_d-1) * (MaxCol - MinCol) / (iNumOfConts_d-2) + MinCol &$

IF STAT EQ 'HWMId' THEN BEGIN
levels=[0,12,24,48,96,192,384]/2
colors=[80,100,150,190,210,250]
levels_d=[0,12,24,48,96,192,384]
colors_d=[80,100,150,190,210,250]
ENDIF

;MEAN
w = WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[1200,900])
w_plot= CONTOUR(WSDI_O(*,*,0),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,3,1],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title='MEAN '+STAT+' '+OBS_1+' 1979-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-80,180,-75],/DATA,FONT_SIZE=8,TITLE=STAT)

w_plot= CONTOUR(WSDI_O(*,*,1),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,3,2],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title='MEAN '+STAT+' '+OBS_2+' 1979-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-80,180,-75],/DATA,FONT_SIZE=8,TITLE=STAT)

w_plot= CONTOUR(WSDI_M_MED(*,*),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,3,3],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title=STAT+' Models median 1979-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-80,180,-75],/DATA,FONT_SIZE=8,TITLE=STAT)

;MEDIAN
w_plot= CONTOUR(WSDI_Md_O(*,*,0),lon,lat,c_value=levels/6.,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,3,4],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title='MEDIAN '+STAT+' '+OBS_1+' 1979-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels/6., FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-80,180,-75],/DATA,FONT_SIZE=8,TITLE=STAT)

w_plot= CONTOUR(WSDI_Md_O(*,*,1),lon,lat,c_value=levels/6.,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,3,5],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title='MEDIAN '+STAT+' '+OBS_2+' 1979-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels/6., FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-80,180,-75],/DATA,FONT_SIZE=8,TITLE=STAT)

w_plot= CONTOUR(WSDI_Md_MED(*,*),lon,lat,c_value=levels/6.,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,3,6],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title='MEAN '+STAT+' Models median 1979-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels/6., FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-80,180,-75],/DATA,FONT_SIZE=8,TITLE=STAT)

;MAX
w_plot= CONTOUR(WSDI_Mx_O(*,*,0),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,3,7],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title='MAX '+STAT+' '+OBS_1+' 1979-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-80,180,-75],/DATA,FONT_SIZE=8,TITLE=STAT)

w_plot= CONTOUR(WSDI_Mx_O(*,*,1),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,3,8],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title='MAX '+STAT+' '+OBS_2+' 1979-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-80,180,-75],/DATA,FONT_SIZE=8,TITLE=STAT)

w_plot= CONTOUR(WSDI_Mx_MED(*,*),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,3,9],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title='MAX '+STAT+' GCMs MEDIAN 1979-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-80,180,-75],/DATA,FONT_SIZE=8,TITLE=STAT)

GOTO,JUMP_SINGLE

FOR ir=0,NRUN-1 DO BEGIN
w_plot= CONTOUR(WSDI_M(*,*,ir),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,2,2],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title=STAT+' '+RUN(ir)+' 1976-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-80,180,-75],/DATA,FONT_SIZE=8,TITLE=STAT)

w_plot= CONTOUR(WSDI_M(*,*,ir)-WSDI_O,lon,lat,c_value=levels_d,RGB_INDICES=colors_d,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,2,3],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title=STAT+' MODEL - OBS')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels_d, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-80,180,-75],/DATA,FONT_SIZE=8,TITLE=STAT)
ENDFOR

JUMP_SINGLE:

;w0.Save,'./PLOTS/PLOT_STAT_HW_MAPS_EOBS_NEW.pdf',BITMAP=1,PAGE_SIZE="A4"

;======================
;AREA AVERAGES
;======================
window,3,retain=2
!p.multi=0

WE=WHERE(landmask EQ 0)
WSDI_YEAR_M=FLTARR(NMASK+1,NYEAR_T,NRUN)
WSDI_YEAR_O=FLTARR(NMASK+1,NYEAR_T,2)
WSDI_MED=FLTARR(NMASK+1,NYEAR_T)

FOR iyy=0,NYEAR_T-1 DO BEGIN &$  ;YEAR
FOR ir=0,NRUN-1 DO BEGIN
IF STAT EQ 'SM' OR STAT EQ 'TXx' THEN WORK=REFORM(WSDI_Y_T(*,*,iyy,ir))-273.15 ELSE WORK=REFORM(WSDI_Y_T(*,*,iyy,ir)) &$
WORK(WE)=!VALUES.F_NaN &$
WORK(WHERE( WORK LE -999.))=!VALUES.F_NaN
WSDI_YEAR_M(0,iyy,ir)=MEAN(WORK,/NaN) &$
WSDI_MED(0,iyy)=MEDIAN(WSDI_YEAR_M(0,iyy,*))
ENDFOR
FOR io=0,1 DO BEGIN
IF STAT EQ 'SM' OR STAT EQ 'TXx' THEN WORK=REFORM(WSDI_Y_O(*,*,iyy,io))-273.15 ELSE WORK=REFORM(WSDI_Y_O(*,*,iyy,io)) &$
WORK(WE)=!VALUES.F_NaN &$
WORK(WHERE( WORK LE -999.))=!VALUES.F_NaN
WSDI_YEAR_O(0,iyy,io)=MEAN(WORK,/NaN) &$
ENDFOR
IF iyy EQ 0 THEN contour,landmask &$  
FOR imm=0,NMASK-1 DO BEGIN &$ ;MASK
IF iyy EQ 0 THEN BEGIN  &$
print,'========================' &$
print,'MASK ',MASK(imm) &$
print,'========================' &$
ENDIF &$
WORK_LAND=landmask*0. &$
we_lon=WHERE(lon GE LON1(imm) AND lon LE LON2(imm)  AND lat GE LAT1(imm)  AND lat LE LAT2(imm) AND landmask GT 0.5,COMPLEMENT=WE_NOLAND ) &$
WORK_LAND=landmask*0.+1 &$
WORK_LAND(we_noland)=0. &$
contour,WORK_LAND,/fill,levels=[1],/overplot &$
FOR ir=0,NRUN-1 DO BEGIN
IF STAT EQ 'SM' OR STAT EQ 'TXx' THEN WORK=REFORM(WSDI_Y_T(*,*,iyy,ir))-273.15 ELSE WORK=REFORM(WSDI_Y_T(*,*,iyy)) &$
WORK(WE_NOLAND)=!VALUES.F_NaN &$
WORK(WHERE( WORK LE -999.))=!VALUES.F_NaN
WSDI_YEAR_M(imm+1,iyy,ir)=MEAN(WORK,/NaN) &$
ENDFOR
FOR io=0,1 DO BEGIN
IF STAT EQ 'SM' OR STAT EQ 'TXx' THEN WORK=REFORM(WSDI_Y_O(*,*,iyy,io))-273.15 ELSE WORK=REFORM(WSDI_Y_O(*,*,iyy,io)) &$
WORK(WE_NOLAND)=!VALUES.F_NaN &$
WORK(WHERE( WORK LE -999.))=!VALUES.F_NaN
WSDI_YEAR_O(imm+1,iyy,io)=MEAN(WORK,/NaN) &$
ENDFOR

;MODEL MEDIAN
WSDI_MED(imm+1,iyy)=MEDIAN(WSDI_YEAR_M(imm+1,iyy,*))

ENDFOR &$ ;MASK
ENDFOR ;YEAR

;========================
;AREA FRACTION FOR HWMId
;========================
IF STAT EQ 'HWMId' THEN BEGIN

BINS=[3,6,12,24,48,96,96*2]

BINSIZE_T=3.
BINS_T=FINDGEN(100)*BINSIZE_T
NBINS_T=N_ELEMENTS(BINS_T)

BINSIZE_T=1.
BINS_T=FINDGEN(200)*BINSIZE_T
NBINS_T=N_ELEMENTS(BINS_T)


;AREA_F_Md=FLTARR(NMASK,NBINS,NRUN+2)
AREA_F_Mx=FLTARR(NMASK,NBINS_T,NRUN+2)
AREA_F_Mx_Y=FLTARR(NMASK,NBINS_T,2,NYEARS)*!VALUES.F_NaN
AREA_F_Mx_MAX=FLTARR(NMASK,NBINS_T,2)
AREA_F_Mx_MIN=FLTARR(NMASK,NBINS_T,2)
;AREA_F_Y=FLTARR(NMASK,NYEAR_T,NBINS,NRUN+2)

NPOINTS=FLTARR(NMASK)
FOR imm=0,NMASK-1 DO BEGIN &$ ;MASK
print,'========================' &$
print,'MASK ',MASK(imm) &$
print,'========================' &$


we_lon=WHERE(lon GE LON1(imm) AND lon LE LON2(imm)  AND lat GE LAT1(imm)  AND lat LE LAT2(imm) AND landmask GT 0.5,COMPLEMENT=WE_NOLAND ) &$
NPOINTS(imm)=N_ELEMENTS(WE_LON) &$
WORK_LAND=landmask*0.+1 &$
WORK_LAND(we_noland)=0. 

AREA_Y=FLTARR(NY)
CORR=FLTARR(NY)
;CALC AREA TOT IN KM
FOR iy=0,ny-1 DO BEGIN &$
; AREA=((SIN(LAT(ix,iy)(+.25)*!DtoR)-SIN((LAT(ix,iy)-.25)*!DtoR))*0.5*!DtoR*6371^2.) 
CORR(iy)=SIN((LAT(0,iy)+.25)*!DtoR)-SIN((LAT(0,iy)-.25)*!DtoR) &$
AREA_Y(iy)=TOTAL(WORK_LAND(*,iy)*CORR(iy)) &$
ENDFOR
AREA_MASK=TOTAL(AREA_Y)

;OBS
FOR io=0,1 DO BEGIN

;FOR iyy=0,NYEAR_T-1 DO BEGIN
;WORK=REFORM(WSDI_Y_O(*,*,iyy,io)) &$
;WORK(WE_NOLAND)=!VALUES.F_NaN &$
;WORK(WHERE( WORK LE -999.))=!VALUES.F_NaN
;PDF_Y=FLTARR(ny,nbins) &$
;FOR iy=0,ny-1 DO BEGIN &$
;FOR ib=0,NBINS-1 DO BEGIN
;WE_BIN_Y=WHERE(WORK(*,iy) GE BINS(ib) AND FINITE(WORK(*,iy),/NaN) EQ 0,count)
;IF WE_BIN(0) NE -1 THEN AREA_F_Y(imm,iyy,ib,io)=count*CORR(iy)
;ENDFOR &$ ;bins
;ENDFOR &$ ;iy
;ENDFOR ;YEAR

WORK=REFORM(WSDI_Y_O(*,*,t1:nyear_t-t2-1,io)) &$
;WORK=REFORM(WSDI_Mx_O(*,*,io)) &$
FOR iyy=0,NYEARS-1 DO BEGIN &$ 
WORK_Y=REFORM(WORK(*,*,iyy)) &$
WORK_Y(WE_NOLAND)=!VALUES.F_NaN &$
WORK_Y(WHERE( WORK_Y LE -999.))=!VALUES.F_NaN &$
WORK(*,*,iyy)=WORK_Y &$
ENDFOR &$

;METHOD POINTS
;FOR ib=0,NBINS_t-1 DO BEGIN &$
;WE_BIN=WHERE(WORK GE BINS_T(ib) AND FINITE(WORK,/NaN) EQ 0,count) &$
;IF WE_BIN(0) NE -1 THEN AREA_F_Mx(imm,ib,io)=count;/NPOINTS(imm)/NYEARS &$
;ENDFOR &$ ;bins
;print,AREA_F_Mx(imm,0,io),AREA_F_Mx(imm,0,io)/NPOINTS(imm)/NYEARS; ,AREA_F_Mx(imm,0,io)/TOTAL(AREA_F_Mx(imm,*,io))

;METHOD HISTGRAM
;pdf=HISTOGRAM(WORK,BINSIZE=BINSIZE_T,NBINS=NBINS_T,MIN=BINS_T(0),/NaN)
;AREA_F_Mx(imm,*,io)=TOTAL(pdf,/CUMULATIVE)/NPOINTS(imm)/NYEARS

;print,pdf(0),pdf(0)/TOTAL(pdf),pdf(0)/NPOINTS(imm)/NYEARS

;METHOD AREA
FOR iyy=0,NYEARS-1 DO BEGIN &$
PDF_Y=FLTARR(ny,nbins_t) &$
FOR iy=0,ny-1 DO BEGIN &$
;pdf_y(iy,*)=HISTOGRAM(REFORM(WORK(*,iy,*)),BINSIZE=BINSIZE_T,NBINS=NBINS_T,MIN=BINS_T(0),/NaN)*CORR(iy) &$
pdf_y(iy,*)=HISTOGRAM(REFORM(WORK(*,iy,iyy)),BINSIZE=BINSIZE_T,NBINS=NBINS_T,MIN=BINS_T(0),/NaN)*CORR(iy) &$

;;FOR ib=0,NBINS_t-1 DO BEGIN &$
;;WE_BIN=WHERE(WORK(*,iy,*) GE BINS_T(ib) AND FINITE(WORK(*,iy,*),/NaN) EQ 0,count) &$
;;IF WE_BIN(0) NE -1 THEN pdf_y(iy,*)=count
;;;IF WE_BIN(0) NE -1 THEN PDF_Y(iy,ib)=count*CORR(iy) &$
;;;IF WE_BIN(0) NE -1 THEN AREA_F_Mx(imm,ib,io)=count*CORR(iy) &$
;;ENDFOR &$ ;bins
ENDFOR &$ ;iy
PDF=FLTARR(nbins_T) &$
FOR ib=0,NBINS_T-1 DO BEGIN &$
PDF(ib)=TOTAL(pdf_y(*,ib)) &$
ENDFOR &$
;AREA_F_Mx(imm,*,io)=TOTAL(pdf,/CUMULATIVE)/AREA_MASK/NYEARS
AREA_F_Mx_Y(imm,*,io,iyy)=TOTAL(pdf,/CUMULATIVE)/AREA_MASK;/NYEARS
ENDFOR  &$;YEAR
FOR ib=0,NBINS_T-1 DO BEGIN &$
AREA_F_Mx(imm,ib,io)=MEAN(AREA_F_Mx_Y(imm,ib,io,*),/NaN) &$
AREA_F_Mx_MAX(imm,ib,io)=AREA_F_Mx(imm,ib,io)+STDDEV(AREA_F_Mx_Y(imm,ib,io,*),/NaN) &$
AREA_F_Mx_MIN(imm,ib,io)=AREA_F_Mx(imm,ib,io)-STDDEV(AREA_F_Mx_Y(imm,ib,io,*),/NaN) &$
ENDFOR &$

;STOP
;;FOR ib=0,NBINS_T-1 DO BEGIN &$
;;AREA_F_Mx(imm,ib,io)=TOTAL(pdf_y(*,ib))/AREA_MASK/NYEARS &$
;;ENDFOR &$ ;ib
;;AREA_F_Mx(imm,*,io)=TOTAL(pdf,/CUMULATIVE)/AREA_MASK/NYEARS
;;AREA_F_Mx(imm,*,io)=TOTAL(pdf,/CUMULATIVE)/NPOINTS(imm)/NYEARS
;AREA_F_Mx(imm,*,io)=pdf/NPOINTS(imm)/NYEARS

ENDFOR ;OBS

;MODELS
FOR ir=0,NRUN-1 DO BEGIN

;FOR iyy=0,NYEAR_T-1 DO BEGIN
;WORK=REFORM(WSDI_Y_T(*,*,iyy,ir)) &$
;;WORK=REFORM(WSDI_Md_M(*,*,ir)) &$
;WORK(WE_NOLAND)=!VALUES.F_NaN &$
;WORK(WHERE( WORK LE -999.))=!VALUES.F_NaN
;FOR ib=0,NBINS-1 DO BEGIN
;WE_BIN=WHERE(WORK GE BINS(ib) AND FINITE(WORK,/NaN) EQ 0,count)
;IF WE_BIN(0) NE -1 THEN AREA_F_Y(imm,iyy,ib,ir+2)=count
;ENDFOR ;bins
;ENDFOR ;YEAR

WORK=REFORM(WSDI_Y_T(*,*,t1:nyear_t-t2-1,ir)) &$
NYEARS=nyear_t-t2-t1
FOR iyy=0,NYEARS-1 DO BEGIN &$ 
WORK_Y=REFORM(WORK(*,*,iyy)) &$
WORK_Y(WE_NOLAND)=!VALUES.F_NaN &$
WORK_Y(WHERE( WORK_Y LE -999.))=!VALUES.F_NaN &$
WORK(*,*,iyy)=WORK_Y &$
ENDFOR &$
;MAX HW
;WORK_MAX=FLTARR(NX,NY)
;FOR iy=0,NY-1 DO BEGIN
;FOR ix=0,NX-1 DO BEGIN
;WORK_MAX(ix,iy)=MAX(WORK(ix,iy,*),/NaN)
;ENDFOR
;ENDFOR

;METHOD POINTS
;FOR ib=0,NBINS_t-1 DO BEGIN &$
;WE_BIN=WHERE(WORK GE BINS_T(ib) AND FINITE(WORK,/NaN) EQ 0,count) &$
;IF WE_BIN(0) NE -1 THEN AREA_F_Mx(imm,ib,ir+2)=count/NPOINTS(imm)/NYEARS &$
;ENDFOR &$ ;bins

;METHOD HISTGRAM
;pdf=HISTOGRAM(WORK,BINSIZE=BINSIZE_T,NBINS=NBINS_T,MIN=BINS_T(0),/NaN)
;AREA_F_Mx(imm,*,ir+2)=TOTAL(pdf,/CUMULATIVE)/NPOINTS(imm)/NYEARS

;METHOD AREA
PDF_Y=FLTARR(ny,nbins_t) &$
FOR iy=0,ny-1 DO BEGIN &$
pdf_y(iy,*)=HISTOGRAM(REFORM(WORK(*,iy,*)),BINSIZE=BINSIZE_T,NBINS=NBINS_T,MIN=BINS_T(0),/NaN)*CORR(iy) &$
;pdf_y(iy,*)=HISTOGRAM(REFORM(WORK_MAX(*,iy)),BINSIZE=BINSIZE_T,NBINS=NBINS_T,MIN=BINS_T(0),/NaN)*CORR(iy) &$
;FOR ib=0,NBINS_t-1 DO BEGIN &$
;WE_BIN=WHERE(WORK(*,iy,*) GE BINS_T(ib) AND FINITE(WORK(*,iy,*),/NaN) EQ 0,count) &$
;IF WE_BIN(0) NE -1 THEN pdf_y(iy,*)=count
;;IF WE_BIN(0) NE -1 THEN AREA_F_Mx(imm,ib,ir+2)=count &$
;;IF WE_BIN(0) NE -1 THEN PDF_Y(iy,ib)=count*CORR(iy) &$
;ENDFOR &$ ;bins
ENDFOR &$ ;iy
PDF=FLTARR(nbins_T) &$
FOR ib=0,NBINS_T-1 DO BEGIN &$
PDF(ib)=TOTAL(pdf_y(*,ib)) &$
ENDFOR &$
;;FOR ib=0,NBINS_T-1 DO BEGIN &$
;;AREA_F_Mx(imm,ib,ir+2)=TOTAL(pdf_y(*,ib),/CUMULATIVE)/AREA_MASK/NYEARS &$
;;ENDFOR &$ ;ib
AREA_F_Mx(imm,*,ir+2)=TOTAL(pdf,/CUMULATIVE)/AREA_MASK/NYEARS
;AREA_F_Mx(imm,*,ir+2)=TOTAL(pdf,/CUMULATIVE)/AREA_MASK;/NYEARS
ENDFOR ;MODELS

ENDFOR ;MASK

;time=FIX(FINDGEN(NYEAR_T)+YEAR_INI_T)
;FOR ib=0,NBINS-1 DO BEGIN &$
;window,ib,retain=2,xsize=1000,ysize=1200,TITLE='HWMId >'+STRING(FIX(BINS(ib))) &$
;!p.multi=[0,4,6] &$
;FOR imm=0,NMASK-1 DO BEGIN &$
;plot,time,AREA_F_Y(imm,*,ib,0)/NPOINTS(imm)*100.,color=0,title=MASK(imm),thick=2,yrange=[0,60] &$
;oplot,time,AREA_F_Y(imm,*,ib,1)/NPOINTS(imm)*100.,color=30,thick=2 &$
;MIN_R=FLTARR(NYEAR_T) &$
;MAX_R=FLTARR(NYEAR_T) &$
;MED_R=FLTARR(NYEAR_T) &$
;FOR iyy=0,NYEAR_T-1 DO BEGIN &$
;MIN_R(iyy)=MIN(AREA_F_Y(imm,iyy,ib,2:NRUN+1)) &$
;MAX_R(iyy)=MAX(AREA_F_Y(imm,iyy,ib,2:NRUN+1)) &$
;MED_R(iyy)=MEDIAN(AREA_F_Y(imm,iyy,ib,2:NRUN+1)) &$
;ENDFOR &$
;oplot,time,MIN_R/NPOINTS(imm)*100.,color=250 &$
;oplot,time,MAX_R/NPOINTS(imm)*100.,color=250 &$
;oplot,time,MED_R/NPOINTS(imm)*100.,color=250,thick=2 &$
;;FOR ir=2,NRUN+1 DO BEGIN &$
;;oplot,time,AREA_F_Y(imm,*,ib,ir)/NPOINTS(imm)*100.,color=30+ir*30 &$
;;ENDFOR &$
;ENDFOR &$
;ENDFOR

;=============
;PLOT PDFs
;=============
TICKV_X=[3,5,10,20]
TICKV_X=[3,6,12,24]
TICKV_X=[3,6,12,24,48]
TICKV_X=[10,20,40,80]
TICKV_Y=[1,5,15,20,30]
TICKV_Y=[1,5,10,15,20,25,30]
;TICKV_Y=[5,25,50,75,100]

YRANGE=[0,30]
;TICKV_Y=[70,90,100]
YRANGE=[1,30]
XRANGE=[3,48]
;YRANGE=[5,120]
;XRANGE=[10,100]
PANEL=['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v,','w','x','y','z']
w3=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[900,1200])

FOR imm=0,NMASK-2 DO BEGIN &$
p=PLOT(bins_t(1:NBINS_T-1),100-[REFORM(AREA_F_Mx(imm,0:98,0))]*100.,/CURRENT,XRANGE=XRANGE,color="black",YRANGE=YRANGE, $
XTITLE='HWMId',LAYOUT=[4,6,imm+1],MARGIN=[0.15,0.05,0.05,0.15],XMINOR=0,XTICKVALUES=TICKV_X,YTICKVALUES=TICKV_Y,YMINOR=0,THICK=2, $
AXIS_STYLE=1,FONT_SIZE=6,YTITLE='Land area fraction (%)',/YLOG,/XLOG) &$
p=PLOT(bins_t(1:NBINS_T-1),100-[REFORM(AREA_F_Mx_MIN(imm,0:98,0))]*100.,color="black",/OVERPLOT,THICK=1,/XLOG)  &$
p=PLOT(bins_t(1:NBINS_T-1),100-[REFORM(AREA_F_Mx_MAX(imm,0:98,0))]*100.,color="black",/OVERPLOT,THICK=1,/XLOG)  &$
p=PLOT(bins_t(1:NBINS_T-1),100-[REFORM(AREA_F_Mx(imm,0:98,1))]*100.,color="blue",/OVERPLOT,THICK=2,/XLOG)  &$
p=PLOT(bins_t(1:NBINS_T-1),100-[REFORM(AREA_F_Mx_MIN(imm,0:98,1))]*100.,color="blue",/OVERPLOT,THICK=1,/XLOG)  &$
p=PLOT(bins_t(1:NBINS_T-1),100-[REFORM(AREA_F_Mx_MAX(imm,0:98,1))]*100.,color="blue",/OVERPLOT,THICK=1,/XLOG)  &$
MIN_R=FLTARR(NBINS_T) &$
MAX_R=FLTARR(NBINS_T) &$
MED_R=FLTARR(NBINS_T) &$
FOR ib=0,NBINS_T-1 DO BEGIN &$
WORK=FLTARR(NRUN) &$
FOR ir=2,NRUN+1 DO BEGIN &$
WORK(ir-2)=REFORM(AREA_F_Mx(imm,ib,ir)) &$
ENDFOR &$
MIN_R(ib)=MIN(WORK) &$
MAX_R(ib)=MAX(WORK) &$
MED_R(ib)=MEDIAN(WORK) &$
ENDFOR &$
p=PLOT(BINS_T(1:NBINS_T-1),100-[MED_R(0:98)]*100.,color="red",/OVERPLOT,THICK=2,/XLOG,/YLOG)  &$
a=MIN_R(0:NBINS_T-1) &$
a(WHERE(a EQ 0))=1E-3 &$ 
a(WHERE(a GE 1))=0.99999 &$ 
b=MAX_R(0:NBINS_T-1) &$
b(WHERE(b EQ 0))=1E-3 &$ 
b(WHERE(b GE 1))=0.9999 &$ 
poly_p=POLYGON([BINS_T(1:NBINS_T-1),REVERSE(BINS_T(1:NBINS_T-1))],[100-b(0:NBINS_T-2)*100.,100-REVERSE(a(0:NBINS_T-2))*100.],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p) &$

t=TEXT(4,20,PANEL(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$
t=TEXT(8,20,MASK(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$
ENDFOR
text1=TEXT(.3,.12,'___ NCEP-2',/NORMAL,FONT_SIZE=12,COLOR="black",TARGET=p)
text1=TEXT(.3,.08,'___ ERA-Interim',/NORMAL,FONT_SIZE=12,COLOR="blue",TARGET=p)
text1=TEXT(.3,.04,'___ Models median ',/NORMAL,FONT_SIZE=12,COLOR="red",TARGET=p)
text2=POLYGON([.55,.6,.6,.55],[0.04,.04,.06,.06],/NORMAL,/FILL_BACKGROUND,FILL_COLOR="red",color="red",FILL_TRANSPARENCY=80,/OVERPLOT)
text2=text(.62,.04,'Models range',/NORMAL,FONT_SIZE=12,COLOR="red",TARGET=p)

w3.Save,'./PLOTS/PLOT_'+STAT+'_PDF_MASK_REF.pdf',BITMAP=1,PAGE_SIZE="A4"
w3.Save,'./PLOTS/PLOT_'+STAT+'_PDF_MASK_REF.eps',BITMAP=1,PAGE_SIZE="A4"

;OJLY WORLD
imm=NMASK-1
w3=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[600,600])
p=PLOT(bins_t(1:NBINS_T-1),100-[REFORM(AREA_F_Mx(imm,0:98,0))]*100.,/CURRENT,XRANGE=XRANGE,color="black",YRANGE=YRANGE, $
XTITLE='HWMId',XMINOR=0,XTICKVALUES=TICKV_X,YTICKVALUES=TICKV_Y,YMINOR=0,THICK=2, $
AXIS_STYLE=1,FONT_SIZE=6,YTITLE='Land area fraction (%)',/YLOG,/XLOG) &$
p=PLOT(bins_t(1:NBINS_T-1),100-[REFORM(AREA_F_Mx(imm,0:98,1))]*100.,color="blue",/OVERPLOT,THICK=2,/XLOG)  &$
MIN_R=FLTARR(NBINS_T) &$
MAX_R=FLTARR(NBINS_T) &$
MED_R=FLTARR(NBINS_T) &$
FOR ib=0,NBINS_T-1 DO BEGIN &$
WORK=FLTARR(NRUN) &$
FOR ir=2,NRUN+1 DO BEGIN &$
WORK(ir-2)=REFORM(AREA_F_Mx(imm,ib,ir)) &$
ENDFOR &$
MIN_R(ib)=MIN(WORK) &$
MAX_R(ib)=MAX(WORK) &$
MED_R(ib)=MEDIAN(WORK) &$
ENDFOR &$
p=PLOT(BINS_T(1:NBINS_T-1),100-[MED_R(0:98)]*100.,color="red",/OVERPLOT,THICK=2,/XLOG,/YLOG)  &$
a=MIN_R(0:NBINS_T-1) &$
a(WHERE(a EQ 0))=1E-3 &$ 
a(WHERE(a GE 1))=0.99999 &$ 
b=MAX_R(0:NBINS_T-1) &$
b(WHERE(b EQ 0))=1E-3 &$ 
b(WHERE(b GE 1))=0.9999 &$ 
poly_p=POLYGON([BINS_T(1:NBINS_T-1),REVERSE(BINS_T(1:NBINS_T-1))],[100-b(0:NBINS_T-2)*100.,100-REVERSE(a(0:NBINS_T-2))*100.],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p) &$

t=TEXT(4,20,PANEL(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$
t=TEXT(8,20,MASK(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$
text1=TEXT(.3,.12,'___ NCEP-2',/NORMAL,FONT_SIZE=12,COLOR="black",TARGET=p)

STOP

window,6,retain=2,xsize=1000,ysize=1200,TITLE='HWMId'
!p.multi=[0,4,6]
FOR imm=0,NMASK-1 DO BEGIN &$
;plot,bins_t,AREA_F_Mx(imm,*,0)/TOTAL(AREA_F_Mx(imm,*,0))*100.,color=0,title=MASK(imm),xrange=[0,20],thick=2,charsize=2,yrange=[-0,20],xstyle=1,ystyle=1,xtickinterval=3,psym=2 &$
plot,bins_t,AREA_F_Mx(imm,*,0)*100.,color=0,title=MASK(imm),xrange=[0,20],thick=2,charsize=2,yrange=[-0,20],xstyle=1,ystyle=1,xtickinterval=3,psym=2 &$
;oplot,bins_t,AREA_F_Mx(imm,*,1)/TOTAL(AREA_F_Mx(imm,*,1))*100.,color=30,thick=2,psym=2 &$
oplot,bins_t,AREA_F_Mx(imm,*,1)*100.,color=30,thick=2,psym=2 &$
MIN_R=FLTARR(NBINS_T) &$
MAX_R=FLTARR(NBINS_T) &$
MED_R=FLTARR(NBINS_T) &$
FOR ib=0,NBINS_T-1 DO BEGIN &$
WORK=FLTARR(NRUN) &$
FOR ir=2,NRUN+1 DO BEGIN &$
;WORK(ir-2)=REFORM(AREA_F_Mx(imm,ib,ir)/TOTAL(AREA_F_Mx(imm,*,ir))*100.) &$
WORK(ir-2)=REFORM(AREA_F_Mx(imm,ib,ir)*100.) &$
ENDFOR &$
MIN_R(ib)=MIN(WORK) &$
MAX_R(ib)=MAX(WORK) &$
MED_R(ib)=MEDIAN(WORK) &$
ENDFOR &$
;FOR ir=2,NRUN+1 DO BEGIN &$
;oplot,bins_t,AREA_F_Mx(imm,*,ir)/TOTAL(AREA_F_Mx(imm,*,ir))*100.,color=30+ir*30,psym=2 &$
;ENDFOR &$
oplot,bins_t,MED_R,color=250,psym=2 &$
FOR ib=1,4 DO BEGIN &$
plots,[bins_t(ib),bins_T(ib)],[MIN_R(ib),MAX_R(ib)],color=250 &$
ENDFOR &$
ENDFOR

ENDIF ;HWMId


GOTO,JUMP_TREND
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
ENDFOR

JUMP_TREND:


time=FIX(FINDGEN(NYEAR_T)+YEAR_INI_T)

window,1,retain=2,xsize=1000,ysize=1200
!p.multi=[0,4,6]
FOR imm=0,NMASK DO BEGIN &$
IF imm EQ 0 THEN TITLE='WORLD' ELSE TITLE=MASK(imm-1) &$
plot,time,WSDI_YEAR_O(imm,*,0),XSTYLE=1,YSTYLE=1,YRANGE=[MIN(WSDI_YEAR_M(imm,*),/NaN)-2,MAX(WSDI_YEAR_M(imm,*),/NaN)+2],thick=2,TITLE=TITLE,CHARSIZE=2 &$
FOR ir=0,NRUN -1 DO BEGIN &$
oplot,time,WSDI_YEAR_M(imm,*,ir),color=30+ir*30 &$
ENDFOR &$
oplot,time,WSDI_MED(imm,*),color=250,thick=3 &$
oplot,time,WSDI_YEAR_O(imm,*,0),color=0,thick=3 &$
oplot,time,WSDI_YEAR_O(imm,*,1),color=30,thick=3 &$
ENDFOR

;MEAN MASKS ANNUAL MEANS ON COMMON YEARS
WSDI_M_TOT=FLTARR(NMASK+1,NRUN+2)
FOR imm=0,NMASK DO BEGIN &$
WSDI_M_TOT(imm,0)=MEAN(WSDI_YEAR_O(imm,t1:nyear_t-t2-1,0),/NAN) &$
WSDI_M_TOT(imm,1)=MEAN(WSDI_YEAR_O(imm,t1:nyear_t-t2-1,1),/NAN) &$
FOR ir=0,NRUN-1 DO BEGIN &$
WSDI_M_TOT(imm,ir+2)=MEAN(WSDI_YEAR_M(imm,t1:nyear_t-t2-1,ir),/NAN) &$
ENDFOR &$
ENDFOR

;MODEL MEDIAN
WSDI_M_TOT_MED=FLTARR(NMASK+1)
FOR imm=0,NMASK DO BEGIN &$
WSDI_M_TOT_MED(imm)=MEDIAN(WSDI_M_TOT(imm,2:NRUN+1)) &$
ENDFOR


IF STAT EQ 'HWMId'  THEN BEGIN

;PLOT EVAL 
w1=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[1000,300])
loadct,39,RGB_TABLE=rgb
!p.background=255
!p.color=0

levels=[0,12,24,48,96,192,384]/2
colors=[255,100,150,190,210,250]
MARGIN=[0.02,0.02,0.02,0.02]
w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,1,1],MARGIN=MARGIN, $
        title='NECP-2 1980-2005',/CURRENT)
c_plot= CONTOUR(WSDI_Mx_O(*,*,0),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
mc = MAPCONTINENTS(THICK=0.5)
t=TEXT(0.1,1,'a',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)

w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,1,2],MARGIN=MARGIN, $
        title='ERA-Interim 1980-2005',/CURRENT)
c_plot= CONTOUR(WSDI_Mx_O(*,*,1),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
mc = MAPCONTINENTS(THICK=0.5)
t=TEXT(0.1,1,'b',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)

w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	 TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	 FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,1,3],MARGIN=[0.02,0,0.02,0], $
	 title='Models median 1980-2005',/CURRENT)
c_plot= CONTOUR(WSDI_Mx_MED(*,*),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
mc = MAPCONTINENTS(THICK=0.5)
t=TEXT(0.1,1,'c',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)
tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=c_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[.3,0.09,.7,0.11],FONT_SIZE=8,TITLE='HWMId')

w1.Save,'./PLOTS/PLOT_'+STAT+'_MAX_MAP_REF.pdf',BITMAP=1,PAGE_SIZE="A4"
w1.Save,'./PLOTS/PLOT_'+STAT+'_MAX_MAP_REF.eps',BITMAP=1,PAGE_SIZE="A4"




;READ EOBS STAT
AREA_F_Mx_EOBS=FLTARR(NMASK,NBINS_T)
FILE_EOBS='/media/NAS/EU-OBS/EOBS10/PP/HWMId/tx_1981-2010_HWMId_MASK_FR.dat'
restore,filename=FILE_EOBS
AREA_F_Mx_EOBS(9:10,*)=AREA_F

loadct,39,RGB_TABLE=rgb
!p.background=255
!p.color=0

levels=[0,12,24,48,96,192,384]/2
colors=[80,100,150,190,210,250]
w = WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[1200,900])
;MEAN
w_plot= CONTOUR(WSDI_Mx_O(*,*,0),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,2,1],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title='Max '+STAT+' '+OBS_1+' 1980-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-82,180,-77],/DATA,FONT_SIZE=8,TITLE=STAT)

w_plot= CONTOUR(WSDI_Mx_O(*,*,1),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,2,2],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title='Max '+STAT+' '+OBS_2+' 1980-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-82,180,-77],/DATA,FONT_SIZE=8,TITLE=STAT)

w_plot= CONTOUR(WSDI_Mx_MED(*,*),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,2,3],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title='Max '+STAT+' GCMs MEDIAN 1980-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-82,180,-77],/DATA,FONT_SIZE=8,TITLE=STAT)

; BINS_T=FINDGEN(100)*3
;BIN=6
MM=FINDGEN(NMASK)
AREA_PLOT=FLTARR(NMASK,NRUN+2)
AREA_PLOT_EOBS=FLTARR(NMASK)
FOR imm=0,NMASK-1 DO BEGIN &$
AREA_PLOT_EOBS(imm)=AREA_F_Mx_EOBS(imm,1)/TOTAL(AREA_F_Mx_EOBS(imm,*))*100. &$
FOR ir=0,NRUN+1 DO BEGIN &$
;AREA_PLOT(imm,ir)=AREA_F_Mx(imm,2,ir)/TOTAL(AREA_F_Mx(imm,*,ir))*100. &$
AREA_PLOT(imm,ir)=AREA_F_Mx(imm,1,ir)*100. &$
ENDFOR &$
ENDFOR
p=PLOT(MM,AREA_PLOT(*,0),LINESTYLE=6,SYMBOL="x",SYM_SIZE=0.7,SYM_THICK=2,xtickname=[' ',MASK,' '], $
	       FONT_SIZE=8,FONT_STYLE=1,xrange=[-1,NMASK],xtickinterval=1,YTICKLEN=0.02,XTICKLEN=0.02,XTEXT_ORIENTATION=90, $
               xmajor=1,xminor=0,yminor=1,yrange=[0,10], $
	       XSTYLE=1,LAYOUT=[3,2,4],/CURRENT,MARGIN=[0.05,0.15,0.05,0.25],TITLE='Percentage of area with HWMId > '+STRING(FIX(BINS_T(2))))
p=PLOT(MM,AREA_PLOT(*,1),LINESTYLE=6,SYMBOL="x",SYM_SIZE=0.7,SYM_THICK=2,SYM_COLOR="blue",/OVERPLOT)

;EOBS
p=PLOT(MM(9:10),AREA_PLOT_EOBS(9:10),LINESTYLE=6,SYMBOL="x",SYM_SIZE=0.7,SYM_THICK=2,SYM_COLOR="green",/OVERPLOT)

MED_R=FLTARR(NMASK) &$
FOR imm=0,NMASK-1 DO BEGIN &$
MED_R(imm)=MEDIAN(AREA_PLOT(imm,2:NRUN+1)) &$
line=POLYLINE([imm,imm],[MIN(AREA_PLOT(imm,2:NRUN+1)),MAX(AREA_PLOT(imm,2:NRUN+1))],/DATA,color="red",thick=2,TARGET=p) &$
ENDFOR
p=PLOT(MM,MED_R,LINESTYLE=6,SYMBOL="_",SYM_SIZE=0.7,SYM_THICK=2,SYM_COLOR="red",/OVERPLOT)

;BIN=12
MM=FINDGEN(NMASK)
AREA_PLOT=FLTARR(NMASK,NRUN+2)
AREA_PLOT_EOBS=FLTARR(NMASK)
FOR imm=0,NMASK-1 DO BEGIN &$
AREA_PLOT_EOBS(imm)=AREA_F_Mx_EOBS(imm,5)/TOTAL(AREA_F_Mx_EOBS(imm,*))*100. &$
FOR ir=0,NRUN+1 DO BEGIN &$
;AREA_PLOT(imm,ir)=AREA_F_Mx(imm,5,ir)/TOTAL(AREA_F_Mx(imm,*,ir))*100. &$
AREA_PLOT(imm,ir)=AREA_F_Mx(imm,5,ir)*100. &$
ENDFOR &$
ENDFOR
p=PLOT(MM,AREA_PLOT(*,0),LINESTYLE=6,SYMBOL="x",SYM_SIZE=0.7,SYM_THICK=2,xtickname=[' ',MASK,' '], $
	       FONT_SIZE=8,FONT_STYLE=1,xrange=[-1,NMASK],xtickinterval=1,YTICKLEN=0.02,XTICKLEN=0.02,XTEXT_ORIENTATION=90, $
               xmajor=1,xminor=0,yminor=1,yrange=[0,4], $
	       XSTYLE=1,LAYOUT=[3,2,5],/CURRENT,MARGIN=[0.05,0.15,0.05,0.25],TITLE='Percentage of area with HWMId > '+STRING(FIX(BINS_T(5))))
p=PLOT(MM,AREA_PLOT(*,1),LINESTYLE=6,SYMBOL="x",SYM_SIZE=0.7,SYM_THICK=2,SYM_COLOR="blue",/OVERPLOT)

;EOBS
p=PLOT(MM(9:10),AREA_PLOT_EOBS(9:10),LINESTYLE=6,SYMBOL="x",SYM_SIZE=0.7,SYM_THICK=2,SYM_COLOR="green",/OVERPLOT)

MED_R=FLTARR(NMASK) &$
FOR imm=0,NMASK-1 DO BEGIN &$
MED_R(imm)=MEDIAN(AREA_PLOT(imm,2:NRUN+1)) &$
line=POLYLINE([imm,imm],[MIN(AREA_PLOT(imm,2:NRUN+1)),MAX(AREA_PLOT(imm,2:NRUN+1))],/DATA,color="red",thick=2,TARGET=p) &$
ENDFOR
p=PLOT(MM,MED_R,LINESTYLE=6,SYMBOL="_",SYM_SIZE=0.7,SYM_THICK=2,SYM_COLOR="red",/OVERPLOT)

;BION=24
MM=FINDGEN(NMASK)
AREA_PLOT=FLTARR(NMASK,NRUN+2)
AREA_PLOT_EOBS=FLTARR(NMASK)
FOR imm=0,NMASK-1 DO BEGIN &$
AREA_PLOT_EOBS(imm)=AREA_F_Mx_EOBS(imm,8)/TOTAL(AREA_F_Mx_EOBS(imm,*))*100. &$
FOR ir=0,NRUN+1 DO BEGIN &$
;AREA_PLOT(imm,ir)=AREA_F_Mx(imm,8,ir)/TOTAL(AREA_F_Mx(imm,*,ir))*100. &$
AREA_PLOT(imm,ir)=AREA_F_Mx(imm,8,ir)*100. &$
ENDFOR &$
ENDFOR
p=PLOT(MM,AREA_PLOT(*,0),LINESTYLE=6,SYMBOL="x",SYM_SIZE=0.7,SYM_THICK=2,xtickname=[' ',MASK,' '], $
	       FONT_SIZE=8,FONT_STYLE=1,xrange=[-1,NMASK],xtickinterval=1,YTICKLEN=0.02,XTICKLEN=0.02,XTEXT_ORIENTATION=90, $
               xmajor=1,xminor=0,yminor=1,yrange=[0,2], $
	       XSTYLE=1,LAYOUT=[3,2,6],/CURRENT,MARGIN=[0.05,0.15,0.05,0.25],TITLE='Percentage of area with HWMId > '+STRING(FIX(BINS_T(8))))
p=PLOT(MM,AREA_PLOT(*,1),LINESTYLE=6,SYMBOL="x",SYM_SIZE=0.7,SYM_THICK=2,SYM_COLOR="blue",/OVERPLOT)

;EOBS
p=PLOT(MM(9:10),AREA_PLOT_EOBS(9:10),LINESTYLE=6,SYMBOL="x",SYM_SIZE=0.7,SYM_THICK=2,SYM_COLOR="green",/OVERPLOT)

MED_R=FLTARR(NMASK) &$
FOR imm=0,NMASK-1 DO BEGIN &$
MED_R(imm)=MEDIAN(AREA_PLOT(imm,2:NRUN+1)) &$
line=POLYLINE([imm,imm],[MIN(AREA_PLOT(imm,2:NRUN+1)),MAX(AREA_PLOT(imm,2:NRUN+1))],/DATA,color="red",thick=2,TARGET=p) &$
ENDFOR
p=PLOT(MM,MED_R,LINESTYLE=6,SYMBOL="_",SYM_SIZE=0.7,SYM_THICK=2,SYM_COLOR="red",/OVERPLOT)

w.Save,'./PLOTS/PLOT_'+STAT+'_EVAL_COMBINED.pdf',BITMAP=1,PAGE_SIZE="A4"
ENDIF ELSE BEGIN

loadct,39,RGB_TABLE=rgb
!p.background=255
!p.color=0

YRANGE=[-20,40]

w = WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[900,1200])
;MEAN
w_plot= CONTOUR(WSDI_O(*,*,0),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,3,1],MARGIN=[0.1,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title='MEAN '+STAT+' '+OBS_1+' 1979-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-82,180,-77],/DATA,FONT_SIZE=8,TITLE=STAT+' ($\circ$C)' )
t=TEXT(0,1.05,'a',/RELATIVE,color="black",TARGET=w_plot,FONT_SIZE=15) 

w_plot= CONTOUR(WSDI_O(*,*,1),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,3,3],MARGIN=[0.1,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title=+STAT+' '+OBS_2+' 1979-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-82,180,-77],/DATA,FONT_SIZE=8,TITLE=STAT+' ($\circ$C)')
t=TEXT(0,1.05,'c',/RELATIVE,color="black",TARGET=w_plot,FONT_SIZE=15) 

w_plot= CONTOUR(WSDI_M_MED(*,*),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,3,5],MARGIN=[0.1,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title=STAT+' GCMs MEDIAN 1979-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-82,180,-77],/DATA,FONT_SIZE=8,TITLE=STAT+' ($\circ$C)')
t=TEXT(0,1.05,'e',/RELATIVE,color="black",TARGET=w_plot,FONT_SIZE=15) 

;WORLD MAP
w_plot= CONTOUR(landmask(*,*),lon,lat,c_value=[0,1],/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=8,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,3,2],MARGIN=[0.05,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title='LAND SUB-REGIONS')
FOR imm=0,NMASK-2 DO BEGIN &$ ;MASK
WORK_LAND=landmask*0. &$
we_lon=WHERE(lon GE LON1(imm) AND lon LE LON2(imm)  AND lat GE LAT1(imm)  AND lat LE LAT2(imm) AND landmask GT 0.5,COMPLEMENT=WE_NOLAND ) &$
WORK_LAND=landmask*0.+1 &$
WORK_LAND(we_noland)=0. &$
line=POLYLINE([LON1(imm),LON2(imm)],[LAT1(imm),LAT1(imm)],/DATA,color="black",thick=1,TARGET=w_plot) &$
line=POLYLINE([LON1(imm),LON2(imm)],[LAT2(imm),LAT2(imm)],/DATA,color="black",thick=1,TARGET=w_plot) &$
line=POLYLINE([LON1(imm),LON1(imm)],[LAT1(imm),LAT2(imm)],/DATA,color="black",thick=1,TARGET=w_plot) &$
line=POLYLINE([LON2(imm),LON2(imm)],[LAT1(imm),LAT2(imm)],/DATA,color="black",thick=1,TARGET=w_plot) &$
t=TEXT(LON1(imm)+(LON2(imm)-LON1(imm))/4,LAT1(imm)+(LAT2(imm)-LAT1(imm))/2,MASK(imm),/DATA,color="blue",TARGET=w_plot,FONT_SIZE=6) &$
ENDFOR
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
t=TEXT(0,1.05,'b',/RELATIVE,color="black",TARGET=w_plot,FONT_SIZE=15) 


MM=FINDGEN(NMASK)
yrange=[FIX(MIN(WSDI_M_TOT,/NaN)-2),FIX(MAX(WSDI_M_TOT,/NaN)+2)]
p_plot=PLOT(MM,WSDI_M_TOT(1:NMASK,0),LINESTYLE=6,SYMBOL="x",SYM_SIZE=0.7,SYM_THICK=1,xtickname=[' ',MASK,' '], $
	       FONT_SIZE=8,FONT_STYLE=1,xrange=[-1,NMASK],xtickinterval=1,YTICKLEN=0.02,XTICKLEN=0.02,XTEXT_ORIENTATION=90, $
               xmajor=1,xminor=0,yminor=1,yrange=yrange, $
	       XSTYLE=1,LAYOUT=[2,3,4],/CURRENT,MARGIN=[0.05,0.15,0.05,0.25],TITLE='AREA MEAN 1979-2005',YTITLE='$\circ$C')
p=PLOT(MM,WSDI_M_TOT(1:NMASK,1),LINESTYLE=6,SYMBOL="x",SYM_SIZE=0.7,SYM_THICK=1,SYM_COLOR="blue",/OVERPLOT)
p=PLOT(MM,WSDI_M_TOT_MED(1:NMASK),LINESTYLE=6,SYMBOL="_",SYM_SIZE=0.7,SYM_THICK=1,SYM_COLOR="red",/OVERPLOT)
FOR imm=1,NMASK DO BEGIN &$
line=POLYLINE([imm-1,imm-1],[MIN(WSDI_M_TOT(imm,*)),MAX(WSDI_M_TOT(imm,*))],/DATA,color="red",thick=1,TARGET=p) &$
ENDFOr
t=TEXT(0,1.05,'d',/RELATIVE,color="black",TARGET=p_plot,FONT_SIZE=15) 
text1=TEXT(13,22,'x NCEP-2',/DATA,FONT_SIZE=12,COLOR="black",TARGET=p_plot)
text1=TEXT(13,18,'x ERA-INT',/DATA,FONT_SIZE=12,COLOR="blue",TARGET=p_plot)
text1=TEXT(13,14,'- Models median',/DATA,FONT_SIZE=12,COLOR="red",TARGET=p_plot)
text1=TEXT(13,10,'| Models range',/DATA,FONT_SIZE=12,COLOR="red",TARGET=p_plot)


yrange=[FIX(MIN(WSDI_YEAR_M(0,*,*),/NaN)-2),FIX(MAX(WSDI_YEAR_M(0,*,*),/NaN)+2)]
p_plot=PLOT(time,WSDI_YEAR_O(0,*,0),FONT_SIZE=8,FONT_STYLE=1,YTICKLEN=0.02,XTICKLEN=0.02, $
                 xmajor=1,xminor=1,xrange=[1970,2010],xtickinterval=5, $
		 ymajor=1,yminor=1,yrange=yrange,ytickinterval=1, $
		 XSTYLE=1,LAYOUT=[2,3,6],/CURRENT,MARGIN=[0.05,0.15,0.05,0.25],TITLE=STAT+' GLOBAL MEAN (LAND)',YTITLE='$\circ$C',XTITLE='Year')
FOR ir=0,NRUN -1 DO BEGIN &$
;p=PLOT(time,WSDI_YEAR_M(0,*,ir),color=30+ir*30,/OVERPLOT) &$
p=PLOT(time,WSDI_YEAR_M(0,*,ir),color="red",/OVERPLOT) &$
ENDFOR &$
p=PLOT(time,WSDI_YEAR_O(0,*,0),color="black",THICK=2,/OVERPLOT)
p=PLOT(time,WSDI_YEAR_O(0,*,1),color="blue",THICK=2,/OVERPLOT)
p=PLOT(time,WSDI_MED(0,*),color="red",THICK=2,/OVERPLOT)
t=TEXT(0,1.05,'f',/RELATIVE,color="black",TARGET=p_plot,FONT_SIZE=15) 
text1=TEXT(1972,33,'___ NCEP-2',/DATA,FONT_SIZE=12,COLOR="black",TARGET=p_plot)
text1=TEXT(1972,32,'___ ERA-INT',/DATA,FONT_SIZE=12,COLOR="blue",TARGET=p_plot)
text1=TEXT(1972,27,'___ Single models run',/DATA,FONT_SIZE=12,COLOR="red",TARGET=p_plot)
text1=TEXT(1995,27,'___',/DATA,FONT_SIZE=14,COLOR="red",TARGET=p_plot)
text1=TEXT(1995,27.01,'___',/DATA,FONT_SIZE=14,COLOR="red",TARGET=p_plot)
text1=TEXT(1995,27.02,'___',/DATA,FONT_SIZE=14,COLOR="red",TARGET=p_plot)
text1=TEXT(1995,27.03,'___',/DATA,FONT_SIZE=14,COLOR="red",TARGET=p_plot)
text1=TEXT(1995,27.04,'___',/DATA,FONT_SIZE=14,COLOR="red",TARGET=p_plot)
text1=TEXT(1995,27.05,'___',/DATA,FONT_SIZE=14,COLOR="red",TARGET=p_plot)
text1=TEXT(1995,27.06,'___',/DATA,FONT_SIZE=14,COLOR="red",TARGET=p_plot)
text1=TEXT(1998,27,'Models median',/DATA,FONT_SIZE=12,COLOR="red",TARGET=p_plot)

w.Save,'./PLOTS/PLOT_'+STAT+'_EVAL_COMBINED.pdf',BITMAP=1,PAGE_SIZE="A4"

ENDELSE

window,2,retain=2,xsize=800,ysize=500
!p.multi=0
PLOT,MM,WSDI_M_TOT(*,0),psym=2,color=0,xtickname=['WRL',MASK],xminor=1,xticks=21,title=STAT
FOR ir=2,NRUN DO BEGIN &$
OPLOT,MM,WSDI_M_TOT(*,ir),psym=2,color=30+ir*30 &$
ENDFOR
OPLOT,MM,WSDI_M_TOT(*,0),psym=2,color=0,thick=2
OPLOT,MM,WSDI_M_TOT(*,2),psym=2,color=30,thick=2



STOP
time=FIX(FINDGEN(NYEAR)+YEAR_INI)
WINDOW,8+ic,RETAIN=2,title=RUN(ir)
!p.multi=[0,2,2]
PLOT,time,WSDI_Y(150,260,*),xstyle=1,ystyle=1,title=STAT+' North America',YRANGE=[0,MAX(WSDI_Y(150,260,*))*1.1]
PLOT,time,WSDI_Y(380,280,*),xstyle=1,ystyle=1,title=STAT+' Central EU ',YRANGE=[0,MAX(WSDI_Y(390,280,*))*1.1]
PLOT,time,WSDI_Y(400,130,*),xstyle=1,ystyle=1,title=STAT+' South Africa',YRANGE=[0,MAX(WSDI_Y(400,130,*))*1.1]
PLOT,time,WSDI_Y(600,120,*),xstyle=1,ystyle=1,title=STAT+' Australia',YRANGE=[0,MAX(WSDI_Y(600,120,*))*1.1]





STOP
END
