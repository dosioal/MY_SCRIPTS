PRO PLOT_WSDI_YEAR
DEVICE,decomposed=0
loadct,39
!p.background=255
!p.color=0

; MAKE WSDI - YEARLY
;=============================================
VAR='tasmax'
;VAR='tasmin'

STAT='SM'
;STAT='WSDI' ; TOTAL N OF DAYS WITH SPELL LENGHT> THRES  DASY (= ETCCDI WSDI)
;STAT='WSDI_M' ;  MAX WARM SPEL LENGHT
STAT='HWMId'

DIR_HOME='/media/NAS/LUC/SMHI/LR/'
RUN=['r1','r2','r3','r4','r5','r6','r7']
NRUN=N_ELEMENTS(RUN)

SCEN=['hist','rcp85'] ;1951-2005'
YEAR_INI=[1971,2006]
YEAR_END=[2005,2100]

NSCEN=N_ELEMENTS(SCEN)

NYEARS=30

;===============================================
NYEAR_1=FIX(YEAR_END(0))-FIX(YEAR_INI(0))+1
NYEAR_2=FIX(YEAR_END(1))-FIX(YEAR_INI(1))+1
NYEAR_TOT=NYEAR_1+NYEAR_2

;MASK
MASK=['AUS', 'AMZ', 'SSA', 'CAM', 'WNA', 'CNA', 'ENA', 'ALA', 'GRL', 'MED', 'NEU', 'WAF', $
	      'EAF', 'SAF', 'SAH', 'SEA', 'EAS', 'SAS', 'CAS', 'TIB', 'NAS','WORLD' ]
NMASK=N_ELEMENTS(MASK)
LON1=[110, -82, -76, -116, -130, -103, -85, -170, -103, -10, -10, -20, $
       22, -10, -20, 95, 100, 65, 40, 75, 40, -180]
LON2=[155, -34, -40, -83, -103, -85, -60, -103, -10, 40, 40, 22, $
      52, 52, 65, 155, 145, 100, 75, 100, 180, 180]
LAT1=[-45, -20, -56, 10, 30, 30, 25, 60, 50, 30, 48, -12, $
     -12, -35, 18, -11, 20, 5, 30, 30, 50, -56]
LAT2=[-11, 12, -20, 30, 60, 50, 50, 72, 85, 48, 75, 18, $
      18, -12, 30, 20, 50, 30, 50, 50, 70, 85]

;=====================
;OBS VALUES
;=====================
;EOBS  ERA  NCEP

FR2003=[47.48,33.6];,38.2]
RU2010=[69.16,100];,76.7]
EA2007=[22.11,21];,28.2]
USA1980=[!VALUES.F_NaN,39];,48.2]

;MEAN OF OBS
FR2003=[39.8]
RU2010=[81.9]
USA1980=[43.6]
EA2007=[23.8]

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

WSDI_M_TOT=FLTARR(nx,ny,4,NRUN) ; 1975-2005,1.5C, 2C, 2071-2100
WSDI_Mx_TOT=FLTARR(nx,ny,4,NRUN) ; 1975-2005,1.5C, 2C, 2071-2100
WSDI_Y_TOT=FLTARR(nx,ny,4,30,NRUN) ;4 periods of 30 years
WSDI_Y_TOT_Md=FLTARR(nx,ny,4,30) ;model median of 4 periods of 30 years
SIG_KS_TOT=FLTARR(nx,ny,3,NRUN) ;KS STAT
SIG_KS_AGR_05=FLTARR(nx,ny,3) ;KS STAT MODEL AGR
SIG_KS_AGR_10=FLTARR(nx,ny,3) ;KS STAT MODEL AGR

;===============================================
; READ MODELS
;===============================================

ic=0 ;counter for window plot
;===============================================
FOR ir=0,NRUN-1 DO BEGIN ;RCM LOOP
;===============================================

;===============================================
;DEF OF VARIABLES
;===============================================
WSDI_Y_T=FLTARR(nx,ny,NYEAR_TOT)*0.-999.

;DIR MODELS
DIR_RUN=DIR_HOME+RUN(ir)+'/'+VAR+'/'
DIR_SM=DIR_RUN+'PP/'+STAT

print,''
print,'====================================='
print,'READING ',STAT,' FOR '
print,'MODEL= ',RUN(ir)
print,'YSTART ',YEAR_INI(0)
print,'YEND ',YEAR_END(1)
PRINT,'NYEAR=',NYEAR_TOT
print,'====================================='


;===============================================
;CHECK IF IDL FILE WITH WSDI STAT EXISITS ALREADY
;===============================================
FOR isc=0,NSCEN-1 DO BEGIN
FILEO=DIR_SM+'/'+VAR+'_'+SCEN(isc)+'_'+STRMID(YEAR_INI(isc),4,4)+'-'+STRMID(YEAR_END(isc),4,4)+'_'+STAT+'.dat'
print,'====================================='
print,'CHECKING ',fileo
print,'====================================='
;FILE_EX=FINDFILE(fileo,count=count)
;count=0
;count=N_ELEMENTS(FILE_SEARCH(FILEO))
count=""
count=FILE_SEARCH(FILEO)

;IF COUNT EQ 1 THEN BEGIN
IF COUNT NE "" THEN BEGIN
print,'FILE EXISTS!'
print,''
restore,fileo
IF isc EQ 0 THEN WSDI_Y_T(*,*,0:NYEAR_1-1)=WSDI_Y ELSE WSDI_Y_T(*,*,NYEAR_1:NYEAR_TOT-1)=WSDI_Y
ENDIF ELSE BEGIN
print,'FILE DOES NOT EXIST!'
STOP
ENDELSE

ENDFOR ;SCEN

;================
;MULTI-YEAR MEAN
;================
WSDI_M=FLTARR(nx,ny,4) ; 1975-2005,1.5C, 2C, 2071-2100
WSDI_Mx=FLTARR(nx,ny,4) ; 1975-2005,1.5C, 2C, 2071-2100

;ORIGINAL HELIX
;Y_C_15=[2015,2040,2027,2019,2022,2020,2003]
;Y_C_20=[2030,2055,2039,2035,2038,2034,2020]
;Y_C_40=[2068,2113,2074,2083,2102,2069,2065]

Y_C_15=[2024,2033,2025,2028,2029,2025,2027]
Y_C_20=[2033,2049,2037,2042,2046,2035,2039]
Y_C_40=[2057,2077,2055,2066,2074,2052,2061] ; 3C

FOR ix=0,nx-1 DO BEGIN &$
FOR iy=0,ny-1 DO BEGIN &$
IF landmask(ix,iy) GT 0.5 THEN BEGIN
IF STAT EQ 'SM' THEN BEGIN &$
WSDI_M(ix,iy,0)=MEAN(WSDI_Y_T(ix,iy,5:34)) &$
WSDI_M(ix,iy,1)=MEAN(WSDI_Y_T(ix,iy,Y_C_15(ir)-15-YEAR_INI(0):Y_C_15(ir)+14-YEAR_INI(0)))-WSDI_M(ix,iy,0)
WSDI_M(ix,iy,2)=MEAN(WSDI_Y_T(ix,iy,Y_C_20(ir)-15-YEAR_INI(0):Y_C_20(ir)+14-YEAR_INI(0)))-WSDI_M(ix,iy,0)
WSDI_M(ix,iy,3)=MEAN(WSDI_Y_T(ix,iy,NYEAR_TOT-30:NYEAR_TOT-1))-WSDI_M(ix,iy,0)
ENDIF ELSE BEGIN &$
WSDI_M(ix,iy,0)=MEDIAN(WSDI_Y_T(ix,iy,5:34)) &$
WSDI_M(ix,iy,1)=MEDIAN(WSDI_Y_T(ix,iy,Y_C_15(ir)-15-YEAR_INI(0):Y_C_15(ir)+14-YEAR_INI(0)))
WSDI_M(ix,iy,2)=MEDIAN(WSDI_Y_T(ix,iy,Y_C_20(ir)-15-YEAR_INI(0):Y_C_20(ir)+14-YEAR_INI(0)))
WSDI_M(ix,iy,3)=MEDIAN(WSDI_Y_T(ix,iy,NYEAR_TOT-30:NYEAR_TOT-1))
ENDELSE
WSDI_Mx(ix,iy,0)=MAX(WSDI_Y_T(ix,iy,5:34)) &$
WSDI_Mx(ix,iy,1)=MAX(WSDI_Y_T(ix,iy,Y_C_15(ir)-15-YEAR_INI(0):Y_C_15(ir)+14-YEAR_INI(0)))
WSDI_Mx(ix,iy,2)=MAX(WSDI_Y_T(ix,iy,Y_C_20(ir)-15-YEAR_INI(0):Y_C_20(ir)+14-YEAR_INI(0)))
WSDI_Mx(ix,iy,3)=MAX(WSDI_Y_T(ix,iy,NYEAR_TOT-30:NYEAR_TOT-1))
WSDI_Y_TOT(ix,iy,0,*,ir)=WSDI_Y_T(ix,iy,5:34) &$
WSDI_Y_TOT(ix,iy,1,*,ir)=WSDI_Y_T(ix,iy,Y_C_15(ir)-15-YEAR_INI(0):Y_C_15(ir)+14-YEAR_INI(0)) &$
WSDI_Y_TOT(ix,iy,2,*,ir)=WSDI_Y_T(ix,iy,Y_C_20(ir)-15-YEAR_INI(0):Y_C_20(ir)+14-YEAR_INI(0)) &$
WSDI_Y_TOT(ix,iy,3,*,ir)=WSDI_Y_T(ix,iy,NYEAR_TOT-30:NYEAR_TOT-1) &$
ENDIF
ENDFOR &$
ENDFOR


;===================
;K-S
;===================
fileo_ks='./DATA/K-S_'+VAR+'_'+RUN(ir)+'.dat'
count=""
count=FILE_SEARCH(FILEO_KS)
IF COUNT NE "" THEN BEGIN
print,'K-S FILE EXISTS!'
print,''
restore,fileo_ks
GOTO,JUMP_KS
ENDIF 

print,'KOLMOGOROV'
SIG_KS=FLTARR(nx,ny,3)

FOR ix=0,nx-1 DO BEGIN &$
FOR iy=0,ny-1 DO BEGIN &$
IF landmask(ix,iy) GT 0.5 THEN BEGIN
ww_ref=REFORM(WSDI_Y_TOT(ix,iy,0,*,ir))
ww_1=REFORM(WSDI_Y_TOT(ix,iy,1,*,ir))
ww_2=REFORM(WSDI_Y_TOT(ix,iy,2,*,ir))
;ww_3=REFORM(WSDI_Y_TOT(ix,iy,3,*,ir))
ks2,ww_1,NYEARS,ww_ref,NYEARS,stat_v,prob_v,fn1,fn2,sor1,sor2 ;1.5C
SIG_KS(ix,iy,0)=prob_v
ks2,ww_2,NYEARS,ww_ref,NYEARS,stat_v,prob_v,fn1,fn2,sor1,sor2; 2C
SIG_KS(ix,iy,1)=prob_v
ks2,ww_2,NYEARS,ww_1,NYEARS,stat_v,prob_v,fn1,fn2,sor1,sor2 ; 2C - 1.5C
SIG_KS(ix,iy,2)=prob_v
ENDIF
ENDFOR
ENDFOR

;SAVE K-S
save,filename=fileo_ks,SIG_KS

JUMP_KS:

SIG_KS_TOT(*,*,*,ic)=SIG_KS

;GOTO,JUMP_PLOT_2
;======================
;PLOT
;======================

loadct,39,RGB_TABLE=rgb
MINCOL=0.
MAXCOL=254

CASE STAT OF
'SM':BEGIN
minvalue=260
maxvalue=320
ContourDist=5
minvalue_d=0
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
maxvalue=24
ContourDist=3
minvalue_d=0
maxvalue_d=240
ContourDist_d=24
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
levels=[0,5,10,20,40,80,160]
colors=[80,100,150,190,210,250]
levels_d=[0,12,24,48,96,192,384]
levels_d=levels
colors_d=[80,100,150,190,210,250]
ENDIF

GOTO,JUMP_PLOT
w = WINDOW(WINDOW_TITLE=STAT+' '+RUN(ir),DIMENSIONS=[900,900])
w_plot= CONTOUR(WSDI_M(*,*,0),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,2,1],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title=STAT+' 1976-2005')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-80,180,-75],/DATA,FONT_SIZE=8,TITLE=STAT)

w_plot= CONTOUR(WSDI_M(*,*,1),lon,lat,c_value=levels_d,RGB_INDICES=colors_d,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,2,2],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title=STAT+' 1.5C')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels_d, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-80,180,-75],/DATA,FONT_SIZE=8,TITLE=STAT)

w_plot= CONTOUR(WSDI_M(*,*,2),lon,lat,c_value=levels_d,RGB_INDICES=colors_d,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,2,3],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title=STAT+' 2C')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels_d, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-80,180,-75],/DATA,FONT_SIZE=8,TITLE=STAT)

w_plot= CONTOUR(WSDI_M(*,*,3),lon,lat,c_value=levels_d,RGB_INDICES=colors_d,/FILL,/CURRENT,RGB_TABLE=rgb, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,2,4],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
        YRANGE=[-60,90],XRANGE=[-180,180],title=STAT+' 2071-2100')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8 
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels_d, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-80,180,-75],/DATA,FONT_SIZE=8,TITLE=STAT)

JUMP_PLOT:

WSDI_M_TOT(*,*,*,ic)=WSDI_M
WSDI_Mx_TOT(*,*,*,ic)=WSDI_Mx

;print,GLOBAL MEAN VALUE

WORK=REFORM(WSDI_M(*,*,1))
WORK(WHERE(landmask LT 0.5))=!VALUES.F_NaN
print,'WARMING 1.5C ',MEAN(WORK,/NaN),MEAN(WSDI_M(*,*,1),/NaN)
WORK=REFORM(WSDI_M(*,*,2))
WORK(WHERE(landmask LT 0.5))=!VALUES.F_NaN
print,'WARMING 2C ',MEAN(WORK,/NaN),MEAN(WSDI_M(*,*,2),/NaN)


;close,1
;FILEW='./DATA/'+VAR+'_'+RUN(ir)+'_'+SCEN(1)+'_'+STRMID(YEAR_INI(0),4,4)+'-'+STRMID(YEAR_END(1),4,4)+'_'+STAT+'.txt'
;openw,1,FILEW
;FOR ix=0,nx-1 DO BEGIN
;FOR iy=0,ny-1 DO BEGIN
;IF landmask(ix,iy) GT 0.5 THEN  BEGIN
;printf,1,ix,iy,lon(ix,iy),lat(ix,iy),WSDI_Y_T(ix,iy,*),FORMAT='(134F12.6)'
;ENDIF
;ENDFOR
;ENDFOR
;close,1

ic=ic+1
ENDFOR ; END RCM

;STOP

;=============
;K-S STAT AGREEMENT
;=============
SIG_LEV=0.05
SIG_LEV=0.1
FOR ix=0,nx-1 DO BEGIN &$
FOR iy=0,ny-1 DO BEGIN &$
IF landmask(ix,iy) GT 0.5 THEN BEGIN &$
FOR it=0,2 DO BEGIN &$
FOR ir=0,NRUN-1 DO BEGIN &$
IF SIG_KS_TOT(ix,iy,it,ir) LE 0.1 THEN SIG_KS_AGR_10(ix,iy,it)=SIG_KS_AGR_10(ix,iy,it)+1 &$
IF SIG_KS_TOT(ix,iy,it,ir) LE 0.05 THEN SIG_KS_AGR_05(ix,iy,it)=SIG_KS_AGR_05(ix,iy,it)+1 &$
ENDFOR &$
ENDFOR &$
ENDIF &$
ENDFOR &$
ENDFOR 

;=============
;DIFFERENCE
;=============
WSDI_DIFF_TOT=REFORM(WSDI_Y_TOT(*,*,2,*,*)-WSDI_Y_TOT(*,*,1,*,*)) ;2C-1-5 C
WSDI_DIFF_TOT_Md=FLTARR(nx,ny,NYEARS)
WSDI_DIFF_TOT_Md_Md=FLTARR(nx,ny)
WSDI_DIFF_TOT_Md_Mx=FLTARR(nx,ny)

;=================
;CALC MODEL MEDIAN
;=================

;calc model median first
FOR ix=0,nx-1 DO BEGIN &$
FOR iy=0,ny-1 DO BEGIN &$
IF landmask(ix,iy) GT 0.5 THEN BEGIN
FOR it=0,30-1 DO BEGIN
FOR ib=0,3 DO BEGIN
WSDI_Y_TOT_Md(ix,iy,ib,it)=MEDIAN(WSDI_Y_TOT(ix,iy,ib,it,*))
;WSDI_Y_TOT_25p(ix,iy,it,*)=cgPercentiles(REFORM(WSDI_M_TOT(ix,iy,it,*)), Percentiles=[0.25,0.5,0.75])
;WSDI_Y_TOT_75p(ix,iy,it,*)=cgPercentiles(REFORM(WSDI_Mx_TOT(ix,iy,it,*)), Percentiles=[0.25,0.5,0.75])
ENDFOR
WSDI_DIFF_TOT_Md(ix,iy,it)=MEDIAN(WSDI_DIFF_TOT(ix,iy,it,*))
ENDFOR
WSDI_DIFF_TOT_Md_Md(ix,iy)=MEDIAN(WSDI_DIFF_TOT_Md(ix,iy,*))
WSDI_DIFF_TOT_Md_Mx(ix,iy)=MAX(WSDI_DIFF_TOT_Md(ix,iy,*))
ENDIF
ENDFOR
ENDFOR

;=================
;MULTI MODEL PLOT
;=================

WSDI_M_p=FLTARR(nx,ny,4,3) ;25 50 and 75th perc
WSDI_Mx_p=FLTARR(nx,ny,4,3)

FOR ix=0,nx-1 DO BEGIN &$
FOR iy=0,ny-1 DO BEGIN &$
FOR it=0,3 DO BEGIN &$
IF landmask(ix,iy) GT 0.5 THEN BEGIN &$
WSDI_M_p(ix,iy,it,*)=cgPercentiles(REFORM(WSDI_M_TOT(ix,iy,it,*)), Percentiles=[0.25,0.5,0.75]) &$
WSDI_Mx_p(ix,iy,it,*)=cgPercentiles(REFORM(WSDI_Mx_TOT(ix,iy,it,*)), Percentiles=[0.25,0.5,0.75]) &$
ENDIF &$
ENDFOR &$
ENDFOR &$
ENDFOR

;STIPPELING
pattern_r = Obj_New('idlgrpattern', 1, ORIENTATION=-45,SPACING=4.)
pattern_l = Obj_New('idlgrpattern', 1, ORIENTATION=45,SPACING=4.)

GOTO,JUMP_PLOT_1
w1=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[1000,800])
PERIOD=['REF','1.5C','2C']

MARGIN=[0.02,0.02,0.02,0.02]
FOR it=0,2 DO BEGIN

w_plot= image(landmask,LIMIT=[-70,-180,80,180], GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	TRANSPARENCY=80,MAP_PROJECTION='Mollweide',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
;	FONT_SIZE=10,FONT_STYLE=1,LAYOUT=[3,4,1+it*3],MARGIN=[0.15,0.15,0.05,0.25], $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,4,1+it*3],MARGIN=MARGIN, $
	title='MEDIAN '+STAT+' '+PERIOD(it)+' 25p',/CURRENT) 
mc = MAPCONTINENTS(THICK=0.5)
c_plot= CONTOUR(WSDI_M_p(*,*,it,0),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)

w_plot= image(landmask,LIMIT=[-70,-180,80,180], GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	TRANSPARENCY=80,MAP_PROJECTION='Mollweide',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
;	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,4,2+it*3],MARGIN=[0.15,0.15,0.05,0.25], $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,4,2+it*3],MARGIN=MARGIN, $
	title='MEDIAN '+STAT+' '+PERIOD(it)+' 50p',/CURRENT) 
w_plot= CONTOUR(WSDI_M_p(*,*,it,1),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)

IF it NE 0 THEN BEGIN
;w_plot= CONTOUR(SIG_KS(*,*,it),lon,lat,c_value=[0.,0.05],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2) 
w_plot= CONTOUR(SIG_KS_AGR_05(*,*,it),lon,lat,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2) 
ENDIF

w_plot= image(landmask,LIMIT=[-70,-180,80,180], GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	TRANSPARENCY=80,MAP_PROJECTION='Mollweide',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	;FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,4,3+it*3],MARGIN=[0.15,0.15,0.05,0.25], $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,4,3+it*3],MARGIN=MARGIN, $
	title='MEDIAN '+STAT+' '+PERIOD(it)+' 75p',/CURRENT) 
w_plot= CONTOUR(WSDI_M_p(*,*,it,2),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)

ENDFOR

w_plot= image(landmask,LIMIT=[-70,-180,80,180], GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	TRANSPARENCY=80,MAP_PROJECTION='Mollweide',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,4,11],MARGIN=MARGIN, $
	title='MEDIAN '+STAT+' 2C - 1.5C',/CURRENT) 
w_plot= CONTOUR(WSDI_DIFF_TOT_Md_Md(*,*),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
;w_plot= CONTOUR(SIG_KS(*,*,2),lon,lat,c_value=[0.,0.05],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black") 
w_plot= CONTOUR(SIG_KS_AGR_05(*,*,2),lon,lat,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2) 
;
tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=c_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[.2,.05,.8,.06],FONT_SIZE=8,TITLE=STAT)

w1.Save,'./PLOTS/PLOT_'+STAT+'_MEDIAN_SCEN.pdf',BITMAP=1,PAGE_SIZE="A4"

w2=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[900,1200])
PERIOD=['REF','1.5C','2C']
FOR it=0,2 DO BEGIN
w_plot= CONTOUR(WSDI_Mx_p(*,*,it,0),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
                FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,4,1+it*3],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
	        YRANGE=[-60,90],XRANGE=[-180,180],title='MAX '+STAT+' '+PERIOD(it)+' 25p')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=w_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[-180,-80,180,-75],/DATA,FONT_SIZE=8,TITLE=STAT)
		
w_plot= CONTOUR(WSDI_Mx_p(*,*,it,1),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
                FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,4,2+it*3],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
	        YRANGE=[-60,90],XRANGE=[-180,180],title='MAX '+STAT+' '+PERIOD(it)+' 50p')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
IF it NE 0 THEN BEGIN
;w_plot= CONTOUR(SIG_KS(*,*,it),lon,lat,c_value=[0.,0.05],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black") 
w_plot= CONTOUR(SIG_KS_AGR_05(*,*,it),lon,lat,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black") 
w_plot= CONTOUR(ROB(*,*,0),lon,lat,c_value=[0.8,1],/OVERPLOT,C_FILL_PATTERN=pattern_l,/FILL,COLOR="black") 
ENDIF

w_plot= CONTOUR(WSDI_Mx_p(*,*,it,2),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
                FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,4,3+it*3],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
	        YRANGE=[-60,90],XRANGE=[-180,180],title='MAX '+STAT+' '+PERIOD(it)+' 75p')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
ENDFOR

w_plot= CONTOUR(WSDI_DIFF_TOT_Md_Mx(*,*),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,/CURRENT,RGB_TABLE=rgb, $
                FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,4,11],MARGIN=[0.15,0.15,0.05,0.25],XMINOR=0,YMINOR=0, $
	        YRANGE=[-60,90],XRANGE=[-180,180],title='MAX '+STAT+' 2C - 1.5C')
mc = MAPCONTINENTS(/countries,THICK=0.5)
w_plot['axis0'].GRIDSTYLE=6
w_plot['axis1'].GRIDSTYLE=6
w_plot['axis0'].TITLE="lon"
w_plot['axis1'].TITLE="lat"
w_plot['axis0'].TICKFONT_SIZE=8
w_plot['axis1'].TICKFONT_SIZE=8
;w_plot= CONTOUR(SIG_KS(*,*,2),lon,lat,c_value=[0.,0.05],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black") 
w_plot= CONTOUR(SIG_KS_AGR_05(*,*,2),lon,lat,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black") 

w2.Save,'./PLOTS/PLOT_'+STAT+'_MAX_SCEN.pdf',BITMAP=1,PAGE_SIZE="A4"

JUMP_PLOT_1:

;=================
;PLOT COMBINED
;=================

;READ ROBUSTNESS
ofile_r='./DATA/ROBUSTNESS_'+STAT+'_SCEN.dat'
restore,filename=ofile_r

;READ ERAINT
OBS_2='ERAINT'
YEAR_INI_OBS_2=1979
YEAR_END_OBS_2=2010
VAR_OBS_2='mx2t'
DIR_OBS_2='/media/NAS/AFRICA-OBS/'+OBS_2+'/mx2t'
DIR_SM=DIR_OBS_2+'/PP/'+STAT
FILEO=DIR_SM+'/'+VAR+'_'+STRMID(YEAR_INI_OBS_2,4,4)+'-'+STRMID(YEAR_END_OBS_2,4,4)+'_'+STAT+'.dat'
restore,filename=fileo ;WSDI_Y

WSDI_OBS=FLTARR(nx,ny)-999.
FOR ix=0,nx-1 DO BEGIN &$
FOR iy=0,ny-1 DO BEGIN &$
IF landmask(ix,iy) GT 0.5 THEN BEGIN &$
WSDI_OBS(ix,iy)=MAX(WSDI_Y(ix,iy,*),/NaN)  &$
ENDIF &$
ENDFOR &$
ENDFOR

FILEO_W='/media/NAS/AFRICA-OBS/'+OBS_2+'/mx2t/PP/'+STAT+'/HWMDI_OBS.txt'
close,2
openw,2,FILEO_W
printf,2,nx,ny
FOR iy=0,NY-1 DO BEGIN &$
printf,2,WSDI_OBS(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY-1 DO BEGIN &$
printf,2,LON(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY-1 DO BEGIN &$
printf,2,LAT(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
CLOSE,2

FILEO_W='./HWMDI_pres.txt'
close,2
openw,2,FILEO_W
printf,2,nx,ny
FOR iy=0,NY-1 DO BEGIN &$
printf,2,WSDI_Mx_p(*,iy,0,1),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY-1 DO BEGIN &$
printf,2,LON(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY-1 DO BEGIN &$
printf,2,LAT(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
CLOSE,2

FILEO_W='./HWMDI_1.5C.txt'
close,2
openw,2,FILEO_W
printf,2,nx,ny
FOR iy=0,NY-1 DO BEGIN &$
printf,2,WSDI_Mx_p(*,iy,1,1),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY-1 DO BEGIN &$
printf,2,LON(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY-1 DO BEGIN &$
printf,2,LAT(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
CLOSE,2

FILEO_W='./HWMDI_2C.txt'
close,2
openw,2,FILEO_W
printf,2,nx,ny
FOR iy=0,NY-1 DO BEGIN &$
printf,2,WSDI_Mx_p(*,iy,2,1),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY-1 DO BEGIN &$
printf,2,LON(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY-1 DO BEGIN &$
printf,2,LAT(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
CLOSE,2

STOP

colors(0)=255

;STIPPELING
pattern_r = Obj_New('idlgrpattern', 1, ORIENTATION=-45,SPACING=4,THICK=0.8)
pattern_l = Obj_New('idlgrpattern', 1, ORIENTATION=45,SPACING=4,THICK=0.8)

ROB(*,0:60,*)=!VALUES.F_NaN
SIG_KS_AGR_05(*,0:60,*)=!VALUES.F_NaN

w1=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[1000,700])
MARGIN=[0.02,0.02,0.02,0.02]
w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	;TRANSPARENCY=80,MAP_PROJECTION='Mollweide',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,2,1],MARGIN=MARGIN, $
	title='ERA Interim 1980-2010',/CURRENT) 
c_plot= CONTOUR(WSDI_OBS(*,*),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
mc = MAPCONTINENTS(THICK=0.5)
t=TEXT(0.1,1,'a',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)

;w_plot= image(landmask,LIMIT=[-60,-180,80,180], GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,2,2],MARGIN=[0.02,0,0.02,0], $
	title='Models 1976-2005',/CURRENT) 
c_plot= CONTOUR(WSDI_Mx_p(*,*,0,1),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
mc = MAPCONTINENTS(THICK=0.5)
t=TEXT(0.1,1,'b',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)

w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,2,3],MARGIN=MARGIN, $
	title='Models +1.5$\circ$C ',/CURRENT) 
c_plot= CONTOUR(WSDI_Mx_p(*,*,1,1),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
s_plot= CONTOUR(SIG_KS_AGR_05(*,*,0),lon,lat,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2) 
s_plot= CONTOUR(ROB(*,*,0),lon,lat,c_value=[0.8,1],/OVERPLOT,C_FILL_PATTERN=pattern_l,/FILL,COLOR="black",GRID_UNITS=2) 
mc = MAPCONTINENTS(THICK=0.5)
t=TEXT(0.1,1,'c',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)

w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,2,4],MARGIN=MARGIN, $
	title='Models +2$\circ$C',/CURRENT) 
c_plot= CONTOUR(WSDI_Mx_p(*,*,2,1),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
s_plot= CONTOUR(SIG_KS_AGR_05(*,*,1),lon,lat,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2) 
s_plot= CONTOUR(ROB(*,*,1),lon,lat,c_value=[0.8,1],/OVERPLOT,C_FILL_PATTERN=pattern_l,/FILL,COLOR="black",GRID_UNITS=2) 
mc = MAPCONTINENTS(THICK=0.5)
t=TEXT(0.1,1,'d',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)

tickname = STRING(levels, FORMAT='(f6.2)') &$
;cb0 = Colorbar(TARGET=c_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[.3,0.05,.7,0.06],FONT_SIZE=8,TITLE='HWMId Max.')
cb0 = Colorbar(TARGET=c_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[.05,0.05,.45,0.06],FONT_SIZE=8,TITLE='HWMId Max.')

box=POLYGON([.5,.55,.55,.5],[0.02,.02,.05,.05],/NORMAL,/FILL_BACKGROUND,FILL_COLOR="black",PATTERN_ORIENTATION=-45,PATTERN_SPACING=4,PATTERN_THICK=0.8)
text2=text(.56,.03,'Change is significant',/NORMAL,FONT_SIZE=12,COLOR="black",TARGET=w_plot)
box=POLYGON([.75,.8,.8,.75],[0.02,.02,.05,.05],/NORMAL,/FILL_BACKGROUND,FILL_COLOR="black",PATTERN_ORIENTATION=45,PATTERN_SPACING=4,PATTERN_THICK=0.8)
text2=text(.81,.03,'Change is robust ',/NORMAL,FONT_SIZE=12,COLOR="black",TARGET=w_plot)


;w_plot= image(landmask,LIMIT=[-70,-180,80,180], GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
;	TRANSPARENCY=80,MAP_PROJECTION='Mollweide',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
;	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,3,5],MARGIN=MARGIN, $
;	title='K-S level=0.1',/CURRENT) 
;c_plot= CONTOUR(SIG_KS_AGR_10(*,*,2),lon,lat,c_value=findgen(8),RGB_INDICES=findgen(7)*32+30.,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
;mc = MAPCONTINENTS(THICK=0.5)

GOTO,JUMP_JUMP
w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,3,5],MARGIN=MARGIN, $
	title='2$\circ$C - 1.5$\circ$C',/CURRENT) 
c_plot= CONTOUR(WSDI_Mx_p(*,*,2,1)-WSDI_Mx_p(*,*,1,1),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
s_plot= CONTOUR(SIG_KS_AGR_05(*,*,2),lon,lat,c_value=[3,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2)
s_plot= CONTOUR(ROB(*,*,0),lon,lat,c_value=[0.8,1],/OVERPLOT,C_FILL_PATTERN=pattern_l,/FILL,COLOR="black") 
mc = MAPCONTINENTS(THICK=0.5)
t=TEXT(0.1,1,'e',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)

w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,3,6],MARGIN=MARGIN, $
	title='Models agreement',/CURRENT) 
s_plot= CONTOUR(SIG_KS_AGR_05(*,*,2),lon,lat,c_value=findgen(8),RGB_INDICES=findgen(7)*32+30.,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
s_plot= CONTOUR(ROB(*,*,1),lon,lat,c_value=[0.8,1],/OVERPLOT,C_FILL_PATTERN=pattern_l,/FILL,COLOR="black") 
mc = MAPCONTINENTS(THICK=0.5)
t=TEXT(0.1,1,'f',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)

tickname = STRING(findgen(7)+1, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=c_plot,BORDER=1,TAPER=0,TICKNAME=tickname,position=[.55,0.02,.95,0.03],FONT_SIZE=8,TITLE='Nr of models')
JUMP_JUMP:

w1.Save,'./PLOTS/PLOT_'+STAT+'_MAX_SCEN_COMBINED.pdf',BITMAP=1,PAGE_SIZE="A4"
w1.Save,'./PLOTS/PLOT_'+STAT+'_MAX_SCEN_COMBINED.eps',BITMAP=1,PAGE_SIZE="A4"

STOP

JUMP_PLOT_2:

;===================
;CALC Nr OF EVENTS
;===================
N24=FLTARR(nx,ny,NRUN,3)
N48=FLTARR(nx,ny,NRUN,3)
N96=FLTARR(nx,ny,NRUN,3)
N24_Md=FLTARR(nx,ny,3)
N48_Md=FLTARR(nx,ny,3)
N96_Md=FLTARR(nx,ny,3)

FOR ix=0,NX-1 DO BEGIN &$
FOR iy=0,NY-1 DO BEGIN &$
IF landmask(ix,iy) GE 0.5 THEN BEGIN &$
FOR ir=0,NRUN-1 DO BEGIN &$
WORK=REFORM(WSDI_Y_TOT(ix,iy,0,*,ir)) &$
WE=WHERE(WORK GE 24. ,count) &$
N24(ix,iy,ir,0)=count &$
WE=WHERE(WORK GE 48. ,count) &$
N48(ix,iy,ir,0)=count &$
WE=WHERE(WORK GE 96. ,count) &$
N96(ix,iy,ir,0)=count &$
WORK=REFORM(WSDI_Y_TOT(ix,iy,1,*,ir)) &$
WE=WHERE(WORK GE 24. ,count) &$
N24(ix,iy,ir,1)=count &$
WE=WHERE(WORK GE 48. ,count) &$
N48(ix,iy,ir,1)=count &$
WE=WHERE(WORK GE 96. ,count) &$
N96(ix,iy,ir,1)=count &$
WORK=REFORM(WSDI_Y_TOT(ix,iy,2,*,ir)) &$
WE=WHERE(WORK GE 24. ,count) &$
N24(ix,iy,ir,2)=count &$
WE=WHERE(WORK GE 48. ,count) &$
N48(ix,iy,ir,2)=count &$
WE=WHERE(WORK GE 96. ,count) &$
N96(ix,iy,ir,2)=count &$
ENDFOR  &$;runs
FOR it=0,2 DO BEGIN &$
N24_Md(ix,iy,it)=MEDIAN(N24(ix,iy,*,it)) &$
N48_Md(ix,iy,it)=MEDIAN(N48(ix,iy,*,it)) &$
N96_Md(ix,iy,it)=MEDIAN(N96(ix,iy,*,it)) &$
ENDFOR &$
ENDIF &$ ;land
ENDFOR &$
ENDFOR 

levels=FINDGEN(14)+1
colors=FINDGEN(14)*18+20

ct_number=39
LOADCT,ct_number,RGB_TABLE=ct, /SILENT

MIN_COLOR=20
MAX_COLOR=254
N_LEVELS=14
ctindices=REVERSE((INDGEN(N_LEVELS)*(MAX_COLOR-MIN_COLOR)/(N_LEVELS-1)+MIN_COLOR))
step_ct = CONGRID(ct[ctindices, *], 256, 3)


w5=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[1000,900])
MARGIN=[0.02,0.02,0.02,0.02]
w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,3,1],MARGIN=MARGIN, $
	title='N24 +1.5$\circ$C',/CURRENT) 
c_plot= CONTOUR(N24_Md(*,*,1)+0.1,lon,lat,c_value=levels,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
mc = MAPCONTINENTS(THICK=0.5)
t=TEXT(0.1,1,'a',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)

w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,3,2],MARGIN=MARGIN, $
	title='N24 +2$\circ$C',/CURRENT) 
c_plot= CONTOUR(N24_Md(*,*,2)+0.1,lon,lat,c_value=levels,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
mc = MAPCONTINENTS(THICK=0.5)
t=TEXT(0.1,1,'b',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)

w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,3,3],MARGIN=MARGIN, $
	title='N48 +1.5$\circ$C',/CURRENT) 
c_plot= CONTOUR(N48_Md(*,*,1)+0.1,lon,lat,c_value=levels,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
mc = MAPCONTINENTS(THICK=0.5)
t=TEXT(0.1,1,'c',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)

w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,3,4],MARGIN=MARGIN, $
	title='N48 +2$\circ$C',/CURRENT) 
c_plot= CONTOUR(N48_Md(*,*,2)+0.1,lon,lat,c_value=levels,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
mc = MAPCONTINENTS(THICK=0.5)
t=TEXT(0.1,1,'d',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)

w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,3,5],MARGIN=MARGIN, $
	title='N96 +1.5$\circ$C',/CURRENT) 
c_plot= CONTOUR(N96_Md(*,*,1)+0.1,lon,lat,c_value=levels,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
mc = MAPCONTINENTS(THICK=0.5)
t=TEXT(0.1,1,'e',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)

w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,3,6],MARGIN=MARGIN, $
	title='N96 +2$\circ$C',/CURRENT) 
c_plot= CONTOUR(N96_Md(*,*,2)+0.1,lon,lat,c_value=levels,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
mc = MAPCONTINENTS(THICK=0.5)
t=TEXT(0.1,1,'f',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)

tickname = STRING(levels, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=c_plot,BORDER=1,TAPER=0,TICKNAME=tickname,position=[.55,0.02,.95,0.03],FONT_SIZE=8,TITLE='Nr of events')
w5.Save,'./PLOTS/PLOT_'+STAT+'_N_EVENTS.pdf',BITMAP=1,PAGE_SIZE="A4"

;PLOT K-S SIG
w2=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[1200,600])
w_plot= image(landmask,LIMIT=[-70,-180,80,180], GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	TRANSPARENCY=80,MAP_PROJECTION='Mollweide',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,1,1],MARGIN=MARGIN, $
	title='K-S level=0.1',/CURRENT) 
c_plot= CONTOUR(SIG_KS_AGR_10(*,*,2),lon,lat,c_value=findgen(8),RGB_INDICES=findgen(7)*32+30.,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
mc = MAPCONTINENTS(THICK=0.5)

w_plot= image(landmask,LIMIT=[-70,-180,80,180], GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
	TRANSPARENCY=80,MAP_PROJECTION='Mollweide',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
	FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,1,2],MARGIN=MARGIN, $
	title='K-S level=0.05',/CURRENT) 
c_plot= CONTOUR(SIG_KS_AGR_05(*,*,2),lon,lat,c_value=findgen(8),RGB_INDICES=findgen(7)*32+30.,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
mc = MAPCONTINENTS(THICK=0.5)

tickname = STRING(findgen(7)+1, FORMAT='(f6.2)') &$
cb0 = Colorbar(TARGET=c_plot,BORDER=1,TAPER=0,TICKNAME=tickname,position=[.3,0.05,.7,0.06],FONT_SIZE=8,TITLE='Nr of models')


;STOP

;================
;CDF ON MASK
;================

BINSIZE=3
NBINS=100
BINS=FINDGEN(NBINS)*BINSIZE

WSDI_CDF=FLTARR(NMASK+1,4,NBINS)
WSDI_CDF_R=FLTARR(NMASK+1,4,NBINS,NRUN)

KS_CDF_Y=FLTARR(NMASK,NBINS,3) ;1980-2100
SIG_KS_AREA=FLTARR(NMASK,NBINS,3,NRUN)
MOD_AGR_KS=FLTARR(NMASK,NBINS,3)

NPOINTs=FLTARR(NMASK+1)

FOR imm=0,NMASK-1 DO BEGIN &$ ;MASK
 print,'========================' &$
 print,'MASK ',MASK(imm) &$
 print,'========================' &$
WORK_LAND=landmask*0. &$
we_lon=WHERE(lon GE LON1(imm) AND lon LE LON2(imm)  AND lat GE LAT1(imm)  AND lat LE LAT2(imm) AND landmask GT 0.5,COMPLEMENT=WE_NOLAND ) &$
WORK_LAND=landmask*0.+1 &$
WORK_LAND(we_noland)=0. &$
NPOINTS(imm)=N_ELEMENTS(WE_LON)

AREA_Y=FLTARR(NY)
CORR=FLTARR(NY)
;CALC AREA TOT IN KM
FOR iy=0,ny-1 DO BEGIN &$
; AREA=((SIN(LAT(ix,iy)(+.25)*!DtoR)-SIN((LAT(ix,iy)-.25)*!DtoR))*0.5*!DtoR*6371^2.) 
CORR(iy)=SIN((LAT(0,iy)+.25)*!DtoR)-SIN((LAT(0,iy)-.25)*!DtoR) &$
AREA_Y(iy)=TOTAL(WORK_LAND(*,iy)*CORR(iy)) &$
ENDFOR

AREA_MASK=TOTAL(AREA_Y)

FOR ir=0,NRUN-1 DO BEGIN &$
PDF=FLTARR(nbins,4,NYEARS) &$
PDF_W=FLTARR(nbins,4) &$
FOR it=0,3 DO BEGIN &$
PDF_Y=FLTARR(ny,nbins,NYEARS) &$
WORK=REFORM(WSDI_Y_TOT(*,*,it,*,ir)) &$
FOR iyy=0,NYEARS-1 DO BEGIN &$
WORK_W=REFORM(WORK(*,*,iyy)) &$
WORK_W(we_noland)=!VALUES.F_NaN &$
WORK(*,*,iyy)=WORK_W &$
FOR iy=0,ny-1 DO BEGIN &$
pdf_y(iy,*,iyy)=HISTOGRAM(REFORM(WORK_W(*,iy)),BINSIZE=BINSIZE,NBINS=NBINS,MIN=BINS(0),/NaN)*CORR(iy) &$
ENDFOR &$ ;AREA iy
FOR ib=0,NBINS-1 DO BEGIN &$
PDF(ib,it,iyy)=TOTAL(pdf_y(*,ib,iyy)) &$
ENDFOR &$ ; ib
ENDFOR &$ ;YEAR iyy
FOR ib=0,NBINS-1 DO BEGIN &$
PDF_W(ib,it)=TOTAL(pdf(ib,it,*)/NYEARS) &$
ENDFOR &$ ; ib
WSDI_CDF_R(imm,it,*,ir)=TOTAL(pdf_W(*,it),/CUMULATIVE)/AREA_MASK &$; &$ /NPOINTS(imm) &$; /NYEARS &$

ENDFOR &$ ;SCEN it

;=================
;CALC K-S
;=================
print,'KOLMOGOROV'
FOR ib=0,NBINS-1 DO BEGIN
ww_ref=REFORM(PDF(ib,0,*))
ww_1=REFORM(PDF(ib,1,*))
ww_2=REFORM(PDF(ib,2,*))
ks2,ww_1,NYEARS,ww_ref,NYEARS,stat_v,prob_v,fn1,fn2,sor1,sor2 ;1.5C
KS_CDF_Y(imm,ib,0)=prob_v
ks2,ww_2,NYEARS,ww_ref,NYEARS,stat_v,prob_v,fn1,fn2,sor1,sor2; 2C
KS_CDF_Y(imm,ib,1)=prob_v
ks2,ww_2,NYEARS,ww_1,NYEARS,stat_v,prob_v,fn1,fn2,sor1,sor2 ; 2C - 1.5C
KS_CDF_Y(imm,ib,2)=prob_v
ENDFOR ;bins

SIG_KS_AREA(imm,*,*,ir)=KS_CDF_Y(imm,*,*)

ENDFOR &$ ;RUN ir

THRES=0.05
FOR it=0,2 DO BEGIN &$
FOR ib=0,NBINS-1 DO BEGIN &$
FOR ir=0,NRUN -1 DO BEGIN &$
IF SIG_KS_AREA(imm,ib,it,ir) LE THRES THEN MOD_AGR_KS(imm,ib,it)=MOD_AGR_KS(imm,ib,it)+1 &$
ENDFOR &$
ENDFOR &$
ENDFOR

;CONSIDER ALL MODELS
FOR it=0,3 DO BEGIN &$
WORK=REFORM(WSDI_Y_TOT(*,*,it,*,*)) &$
FOR iyy=0,NYEARS-1 DO BEGIN &$
FOR ir=0,NRUN-1 DO  BEGIN &$
WORK_W=REFORM(WORK(*,*,iyy,ir)) &$
WORK_W(we_noland)=!VALUES.F_NaN &$
WORK(*,*,iyy,ir)=WORK_W &$
ENDFOR ;ir
ENDFOR ;iyy
FOR iy=0,ny-1 DO BEGIN &$
pdf_y(iy,*)=HISTOGRAM(REFORM(WORK(*,iy,*,*)),BINSIZE=BINSIZE,NBINS=NBINS,MIN=BINS(0),/NaN)*CORR(iy) &$
ENDFOR &$
PDF=FLTARR(nbins) &$
FOR ib=0,NBINS-1 DO BEGIN &$
PDF(ib)=TOTAL(pdf_y(*,ib)) &$
ENDFOR &$
;WSDI_CDF(imm,it,*)=TOTAL(pdf,/CUMULATIVE)/NPOINTS(imm)/NYEARS/NRUN &$
WSDI_CDF(imm,it,*)=TOTAL(pdf,/CUMULATIVE)/AREA_MASK/NYEARS/NRUN &$
ENDFOR &$ ;SCEN it

ENDFOR ;MASK

;===========================
;PLOT ALL MASKS
;===========================
w3=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[800,1200])
FOR imm=0,NMASK-1 DO BEGIN  &$

p=PLOT(BINS(1:NBINS-1),WSDI_CDF(imm,0,0:NBINS-2)*100.,/CURRENT,TITLE=MASK(imm),XRANGE=[3,110],color="black",YRANGE=[0,100], $
XTITLE='HWMId',YTITLE='Area fraction (%)',LAYOUT=[4,6,imm+1],MARGIN=[0.15,0.15,0.05,0.15],XMINOR=0,XTICKVALUES=TICKV,YMINOR=0,THICK=2, $
AXIS_STYLE=2,FONT_SIZE=8,/XLOG)   &$
;FOR iyy=0,NYEARS-1 DO BEGIN &$
FOR ir=0,NRUN-1 dO BEGIN &$
p=PLOT(BINS(1:NBINS-1),WSDI_CDF_R(imm,0,0:NBINS-2,ir)*100.,color="black",/OVERPLOT,THICK=1,/XLOG) &$
ENDFOR &$
;ENDFOR &$

MAX_P=FLTARR(NBINS) &$
MIN_P=FLTARR(NBINS) &$
FOR ib=0,NBINS-1 DO BEGIN &$
MAX_P(ib)=MAX(REFORM(WSDI_CDF_R(imm,0,ib,*,*)*100.)) &$
MIN_P(ib)=MIN(REFORM(WSDI_CDF_R(imm,0,ib,*,*)*100.)) &$
ENDFOR &$
poly_p=POLYGON([BINS(1:NBINS-1),REVERSE(BINS(1:NBINS-1))],[MAX_P(0:NBINS-2),REVERSE(MIN_P(0:NBINS-2))],/DATA,/FILL_BACKGROUND, COLOR="gray",FILL_COLOR="gray",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$

p=PLOT(BINS(1:NBINS-1),WSDI_CDF(imm,1,0:NBINS-2)*100.,color="blue",/OVERPLOT,THICK=2,/XLOG) &$
;FOR iyy=0,NYEARS-1 DO BEGIN &$
FOR ir=0,NRUN-1 dO BEGIN &$
p=PLOT(BINS(1:NBINS-1),WSDI_CDF_R(imm,1,0:NBINS-2,ir)*100.,color="blue",/OVERPLOT,THICK=1,/XLOG) &$
ENDFOR &$
;ENDFOR &$
FOR ib=0,NBINS-1 DO BEGIN &$
MAX_P(ib)=MAX(REFORM(WSDI_CDF_R(imm,1,ib,*)*100.)) &$
MIN_P(ib)=MIN(REFORM(WSDI_CDF_R(imm,1,ib,*)*100.)) &$
ENDFOR &$
poly_p=POLYGON([BINS(1:NBINS-1),REVERSE(BINS(1:NBINS-1))],[MAX_P(0:NBINS-2),REVERSE(MIN_P(0:NBINS-2))],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$

p=PLOT(BINS(1:NBINS-1),WSDI_CDF(imm,2,0:NBINS-2)*100.,color="red",/OVERPLOT,THICK=2,/XLOG) &$
;FOR iyy=0,NYEARS-1 DO BEGIN &$
FOR ir=0,NRUN-1 dO BEGIN &$
p=PLOT(BINS(1:NBINS-1),WSDI_CDF_R(imm,2,0:NBINS-2,ir)*100.,color="red",/OVERPLOT,THICK=1,/XLOG) &$
ENDFOR &$
;ENDFOR &$
FOR ib=0,NBINS-1 DO BEGIN &$
MAX_P(ib)=MAX(REFORM(WSDI_CDF_R(imm,2,ib,*)*100.)) &$
MIN_P(ib)=MIN(REFORM(WSDI_CDF_R(imm,2,ib,*)*100.)) &$
ENDFOR &$
poly_p=POLYGON([BINS(1:NBINS-1),REVERSE(BINS(1:NBINS-1))],[MAX_P(0:NBINS-2),REVERSE(MIN_P(0:NBINS-2))],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$

poly_p=POLYGON([MIN(FR2003),MAX(FR2003),MAX(FR2003),MIN(FR2003)],[60,60,100,100],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$
poly_p=POLYGON([MIN(RU2010),MAX(RU2010),MAX(RU2010),MIN(RU2010)],[60,60,100,100],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$
poly_p=POLYGON([MIN(EA2007),MAX(EA2007),MAX(EA2007),MIN(EA2007)],[60,60,100,100],/DATA,/FILL_BACKGROUND, COLOR="black",FILL_COLOR="black",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$
poly_p=POLYGON([MIN(USA1980,/NaN),MAX(USA1980,/NaN),MAX(USA1980,/NaN),MIN(USA1980,/NaN)],[60,60,100,100],/DATA,/FILL_BACKGROUND, COLOR="green",FILL_COLOR="green",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$

t1=TEXT(4,94,'1976-2005',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p) &$
t2=TEXT(4,89,'+1.5C',/DATA,COLOR="Blue",FONT_SIZE=10,TARGET=p) &$
t3=TEXT(6,70,'+2C',/DATA,COLOR="Red",FONT_SIZE=10,TARGET=p) &$

t1=TEXT(26,78,'EA 2007',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p,ORIENTATION=90) &$
t2=TEXT(39,78,'FR 2003',/DATA,COLOR="Blue",FONT_SIZE=10,TARGET=p,ORIENTATION=90) &$
t3=TEXT(46,78,'US 1980',/DATA,COLOR="Green",FONT_SIZE=10,TARGET=p,ORIENTATION=90) &$
t4=TEXT(94,78,'RU 2010',/DATA,COLOR="Red",FONT_SIZE=10,TARGET=p,ORIENTATION=90) &$
ENDFOR
w3.Save,'./PLOTS/PLOT_'+STAT+'_CDF_MASK_SCEN_LAND-CORR.pdf',BITMAP=1,PAGE_SIZE="A4"

;================
;PLOT DIFF
;================

TICKV=[12,24,48,96]
TICKV_Y=[1,5,10,20,50]

w5=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[800,1200])
FOR imm=0,NMASK-2 DO BEGIN &$
WORK_DIFF=FLTARR(nbins,nrun) &$
FOR ir=0,NRUN-1 DO BEGIN &$
WORK_DIFF(*,ir)=REFORM(WSDI_CDF_R(imm,1,*,ir)-WSDI_CDF_R(imm,0,*,ir)) &$
ENDFOR &$
FOR ib=0,NBINS-1 DO BEGIN &$
MAX_P(ib)=MAX(REFORM(-100.*WORK_DIFF(ib,*))) &$
MIN_P(ib)=MIN(REFORM(-100.*WORK_DIFF(ib,*))) &$
ENDFOR &$
p=PLOT(BINS(1:NBINS-1),-100.*(WSDI_CDF(imm,1,0:NBINS-2)-WSDI_CDF(imm,0,0:NBINS-2)),/CURRENT,TITLE=MASK(imm),XRANGE=[12,110],color="blue",YRANGE=[1,50], $
XTITLE='HWMId',YTITLE='Difference in Area Fraction (%)',LAYOUT=[4,6,imm+1],MARGIN=[0.15,0.15,0.05,0.15],XMINOR=0,XTICKVALUES=TICKV,YMINOR=0,THICK=2, $
AXIS_STYLE=2,FONT_SIZE=8,YTICKVALUES=TICKV_Y,/XLOG,/YLOG)  &$
;poly_p=POLYGON([BINS(1:NBINS-1),REVERSE(BINS(1:NBINS-1))],[MAX_P(0:NBINS-2),REVERSE(MIN_P(0:NBINS-2))],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$
ww_p=WHERE(MAX_P LT 0.5) &$
ww_m=WHERE(MIN_P LT 0.5) &$
poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[MAX_P(0:ww_p(0)-1),REVERSE(MIN_P(0:ww_m(0)-1))],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$

FOR ir=0,NRUN-1 DO BEGIN &$
WORK_DIFF(*,ir)=REFORM(WSDI_CDF_R(imm,2,*,ir)-WSDI_CDF_R(imm,0,*,ir)) &$
ENDFOR &$
FOR ib=0,NBINS-1 DO BEGIN &$
MAX_P(ib)=MAX(REFORM(-100.*WORK_DIFF(ib,*))) &$
MIN_P(ib)=MIN(REFORM(-100.*WORK_DIFF(ib,*))) &$
ENDFOR &$
p=PLOT(BINS(1:NBINS-1),-100.*(WSDI_CDF(imm,2,0:NBINS-2)-WSDI_CDF(imm,0,0:NBINS-2)),THICK=2,COLOR="red",/OVERPLOT,/XLOG,/YLOG)  &$
;poly_p=POLYGON([BINS(1:32),REVERSE(BINS(1:32))],[MAX_P(0:31),REVERSE(MIN_P(0:31))],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$
ww_p=WHERE(MAX_P LT 0.5) &$
ww_m=WHERE(MIN_P LT 0.5) &$
poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[MAX_P(0:ww_p(0)-1),REVERSE(MIN_P(0:ww_m(0)-1))],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$

FOR ir=0,NRUN-1 DO BEGIN &$
WORK_DIFF(*,ir)=REFORM(WSDI_CDF_R(imm,2,*,ir)-WSDI_CDF_R(imm,1,*,ir)) &$
ENDFOR &$
FOR ib=0,NBINS-1 DO BEGIN &$
MAX_P(ib)=MAX(REFORM(-100.*WORK_DIFF(ib,*))) &$
MIN_P(ib)=MIN(REFORM(-100.*WORK_DIFF(ib,*))) &$
ENDFOR &$
p=PLOT(BINS(1:NBINS-1),-100.*(WSDI_CDF(imm,2,0:NBINS-2)-WSDI_CDF(imm,1,0:NBINS-2)),THICK=2,COLOR="green",/OVERPLOT,/XLOG,/YLOG)  &$
;poly_p=POLYGON([BINS(1:32),REVERSE(BINS(1:32))],[MAX_P(0:31),REVERSE(MIN_P(0:31))],/DATA,/FILL_BACKGROUND, COLOR="green",FILL_COLOR="green",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$
ww_p=WHERE(MAX_P LT 0.5) &$
ww_m=WHERE(MIN_P LT 0.5) &$
poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[MAX_P(0:ww_p(0)-1),REVERSE(MIN_P(0:ww_m(0)-1))],/DATA,/FILL_BACKGROUND, COLOR="green",FILL_COLOR="green",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$


;;poly_p=POLYGON([MIN(FR2003),MAX(FR2003),MAX(FR2003),MIN(FR2003)],[1,1,50,50],/DATA,/FILL_BACKGROUND,COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$
;poly_p=POLYGON([MIN(FR2003),MAX(FR2003),MAX(FR2003),MIN(FR2003)],[1,1,50,50],/DATA,COLOR="blue",TARGET=p,LAYOUT=[4,6,imm+1],FILL_TRANSPARENCY=100) &$
;poly_p=POLYGON([MIN(RU2010),MAX(RU2010),MAX(RU2010),MIN(RU2010)],[1,1,50,50],/DATA,COLOR="red",TARGET=p,LAYOUT=[4,6,imm+1],FILL_TRANSPARENCY=100) &$
;poly_p=POLYGON([MIN(EA2007),MAX(EA2007),MAX(EA2007),MIN(EA2007)],[1,1,50,50],/DATA,COLOR="black",TARGET=p,LAYOUT=[4,6,imm+1],FILL_TRANSPARENCY=100) &$
;poly_p=POLYGON([MIN(USA1980,/NaN),MAX(USA1980,/NaN),MAX(USA1980,/NaN),MIN(USA1980,/NaN)],[1,1,50,50],/DATA,COLOR="green",TARGET=p,LAYOUT=[],FILL_TRANSPARENCY=100) &$

;t1=TEXT(7,25,'+2C - REF',/DATA,COLOR="Red",FONT_SIZE=10,TARGET=p) &$
;t2=TEXT(3,21,'+1.5C - REF',/DATA,COLOR="Blue",FONT_SIZE=10,TARGET=p) &$
;t3=TEXT(4,5,'+2C - 1.5C',/DATA,COLOR="Green",FONT_SIZE=10,TARGET=p) &$

;t1=TEXT(26,13,'EA 2007',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p,ORIENTATION=90) &$
;t2=TEXT(39,13,'FR 2003',/DATA,COLOR="Blue",FONT_SIZE=10,TARGET=p,ORIENTATION=90) &$
;t3=TEXT(46,13,'US 1980',/DATA,COLOR="Green",FONT_SIZE=10,TARGET=p,ORIENTATION=90) &$
;t4=TEXT(94,13,'RU 2010',/DATA,COLOR="Red",FONT_SIZE=10,TARGET=p,ORIENTATION=90) &$
ENDFOR

w5.Save,'./PLOTS/PLOT_'+STAT+'_CDF_DIFF_MASK_SCEN.pdf',BITMAP=1,PAGE_SIZE="A4"

;====================
;PLOT ONLY WORLD
;====================
w4=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[800,400])
imm=NMASK-1 ;WORLD

TICKV=[6,12,24,48,96]
TICKV_Y=[1,2,5,10,20,50]

p=PLOT(BINS(1:NBINS-1),100.-WSDI_CDF(imm,0,0:NBINS-2)*100.,/CURRENT,XRANGE=[6,103],color="black",YRANGE=[1,50], $
XTITLE='HWMId',YTITLE='Area fraction (%)',LAYOUT=[2,1,1],MARGIN=[0.15,0.15,0.05,0.15],XMINOR=0,XTICKVALUES=TICKV,YMINOR=0,THICK=2, $
AXIS_STYLE=2,FONT_SIZE=8,/XLOG,/YLOG,YTICKVALUES=TICKV_Y) 
MAX_P=FLTARR(NBINS) &$
MIN_P=FLTARR(NBINS) &$
FOR ib=0,NBINS-1 DO BEGIN &$
MAX_P(ib)=MAX(REFORM(100.-WSDI_CDF_R(imm,0,ib,*)*100.)) &$
MIN_P(ib)=MIN(REFORM(100.-WSDI_CDF_R(imm,0,ib,*)*100.)) &$
ENDFOR &$
ww_p=WHERE(MAX_P(*) LT 0.09) &$
ww_m=WHERE(MIN_P(*) LT 0.09) &$
;poly_p=POLYGON([BINS(1:NBINS-1),REVERSE(BINS(1:NBINS-1))],[MAX_P(0:NBINS-2),REVERSE(MIN_P(0:NBINS-2))],/DATA,/FILL_BACKGROUND, COLOR="gray",FILL_COLOR="gray",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,1]) &$
poly_p=POLYGON([BINS(1:ww_p(0)+1),REVERSE(BINS(1:ww_m(0)+1))],[MAX_P(0:ww_p(0)),REVERSE(MIN_P(0:ww_m(0)))],/DATA,/FILL_BACKGROUND, COLOR="gray",FILL_COLOR="gray",FILL_TRANSPARENCY=80,TARGET=p)

p=PLOT(BINS(1:NBINS-1),100.-WSDI_CDF(imm,1,0:NBINS-2)*100.,color="blue",/OVERPLOT,THICK=2,/XLOG,/YLOG) &$
FOR ib=0,NBINS-1 DO BEGIN &$
MAX_P(ib)=MAX(REFORM(100.-WSDI_CDF_R(imm,1,ib,*)*100.)) &$
MIN_P(ib)=MIN(REFORM(100.-WSDI_CDF_R(imm,1,ib,*)*100.)) &$
ENDFOR &$
ww_p=WHERE(MAX_P(*) LT 0.09) &$
ww_m=WHERE(MIN_P(*) LT 0.09) &$
poly_p=POLYGON([BINS(1:ww_p(0)+1),REVERSE(BINS(1:ww_m(0)+1))],[MAX_P(0:ww_p(0)),REVERSE(MIN_P(0:ww_m(0)))],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p)
;poly_p=POLYGON([BINS(1:NBINS-1),REVERSE(BINS(1:NBINS-1))],[MAX_P(0:NBINS-2),REVERSE(MIN_P(0:NBINS-2))],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,11]) &$

p=PLOT(BINS(1:NBINS-1),100.-WSDI_CDF(imm,2,0:NBINS-2)*100.,color="red",/OVERPLOT,THICK=2,/XLOG,/YLOG) &$
FOR ib=0,NBINS-1 DO BEGIN &$
MAX_P(ib)=MAX(REFORM(100.-WSDI_CDF_R(imm,2,ib,*)*100.)) &$
MIN_P(ib)=MIN(REFORM(100.-WSDI_CDF_R(imm,2,ib,*)*100.)) &$
ENDFOR &$
ww_p=WHERE(MAX_P(*) LT 0.09) &$
ww_m=WHERE(MIN_P(*) LT 0.09) &$
poly_p=POLYGON([BINS(1:ww_p(0)+1),REVERSE(BINS(1:ww_m(0)+1))],[MAX_P(0:ww_p(0)),REVERSE(MIN_P(0:ww_m(0)))],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p)
;poly_p=POLYGON([BINS(1:NBINS-1),REVERSE(BINS(1:NBINS-1))],[MAX_P(0:NBINS-2),REVERSE(MIN_P(0:NBINS-2))],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,1]) &$

poly_p=POLYGON([MIN(FR2003),MAX(FR2003),MAX(FR2003),MIN(FR2003)],[1,1,50,50],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,1]) &$
poly_p=POLYGON([MIN(RU2010),MAX(RU2010),MAX(RU2010),MIN(RU2010)],[1,1,50,50],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,1]) &$
poly_p=POLYGON([MIN(EA2007),MAX(EA2007),MAX(EA2007),MIN(EA2007)],[1,1,50,50],/DATA,/FILL_BACKGROUND, COLOR="black",FILL_COLOR="black",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,1]) &$
poly_p=POLYGON([MIN(USA1980,/NaN),MAX(USA1980,/NaN),MAX(USA1980,/NaN),MIN(USA1980,/NaN)],[1,1,50,50],/DATA,/FILL_BACKGROUND, COLOR="green",FILL_COLOR="green",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,1]) &$

t1=TEXT(4,86,'1976-2005',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p)
t2=TEXT(4,78,'+1.5C',/DATA,COLOR="Blue",FONT_SIZE=10,TARGET=p)
t3=TEXT(6,50,'+2C',/DATA,COLOR="Red",FONT_SIZE=10,TARGET=p)

t1=TEXT(26,58,'EA 2007',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p,ORIENTATION=90)
t2=TEXT(39,58,'FR 2003',/DATA,COLOR="Blue",FONT_SIZE=10,TARGET=p,ORIENTATION=90)
t3=TEXT(46,58,'US 1980',/DATA,COLOR="Green",FONT_SIZE=10,TARGET=p,ORIENTATION=90)
t4=TEXT(94,58,'RU 2010',/DATA,COLOR="Red",FONT_SIZE=10,TARGET=p,ORIENTATION=90)

FILEO_DATA='./DATA/DATA_'+STAT+'_CDF_SCEN_LAND-CORR.dat'
save,filename=FILEO_DATA,BINS,MASK,WSDI_CDF,WSDI_CDF_R,MOD_AGR_KS

STOP

;TICKV_Y=[1,5,10,15,30]
;PLOT DIFF
WORK_DIFF=FLTARR(nbins,nrun) &$
FOR ir=0,NRUN-1 DO BEGIN &$
WORK_DIFF(*,ir)=REFORM(WSDI_CDF_R(imm,1,*,ir)-WSDI_CDF_R(imm,0,*,ir)) &$
ENDFOR &$
FOR ib=0,NBINS-1 DO BEGIN &$
MAX_P(ib)=MAX(REFORM(-100.*WORK_DIFF(ib,*))) &$
MIN_P(ib)=MIN(REFORM(-100.*WORK_DIFF(ib,*))) &$
ENDFOR &$
p=PLOT(BINS(1:NBINS-1),-100.*(WSDI_CDF(imm,1,0:NBINS-2)-WSDI_CDF(imm,0,0:NBINS-2)),/CURRENT,XRANGE=[3,110],color="blue",YRANGE=[1,50], $
XTITLE='HWMId',YTITLE='Difference in Area Fraction (%)',LAYOUT=[2,1,2],MARGIN=[0.15,0.15,0.05,0.15],XMINOR=0,XTICKVALUES=TICKV,YMINOR=0,THICK=2, $
AXIS_STYLE=2,FONT_SIZE=8,YTICKVALUES=TICKV_Y,/XLOG,/YLOG) 
poly_p=POLYGON([BINS(1:NBINS-1),REVERSE(BINS(1:NBINS-1))],[MAX_P(0:NBINS-2),REVERSE(MIN_P(0:NBINS-2))],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,2]) &$

FOR ir=0,NRUN-1 DO BEGIN &$
WORK_DIFF(*,ir)=REFORM(WSDI_CDF_R(imm,2,*,ir)-WSDI_CDF_R(imm,0,*,ir)) &$
ENDFOR &$
FOR ib=0,NBINS-1 DO BEGIN &$
MAX_P(ib)=MAX(REFORM(-100.*WORK_DIFF(ib,*))) &$
MIN_P(ib)=MIN(REFORM(-100.*WORK_DIFF(ib,*))) &$
ENDFOR &$
p=PLOT(BINS(1:NBINS-1),-100.*(WSDI_CDF(imm,2,0:NBINS-2)-WSDI_CDF(imm,0,0:NBINS-2)),THICK=2,COLOR="red",/OVERPLOT,/XLOG,/YLOG) 
poly_p=POLYGON([BINS(1:NBINS-1),REVERSE(BINS(1:NBINS-1))],[MAX_P(0:NBINS-2),REVERSE(MIN_P(0:NBINS-2))],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,2]) &$

FOR ir=0,NRUN-1 DO BEGIN &$
WORK_DIFF(*,ir)=REFORM(WSDI_CDF_R(imm,2,*,ir)-WSDI_CDF_R(imm,1,*,ir)) &$
ENDFOR &$
FOR ib=0,NBINS-1 DO BEGIN &$
MAX_P(ib)=MAX(REFORM(-100.*WORK_DIFF(ib,*))) &$
MIN_P(ib)=MIN(REFORM(-100.*WORK_DIFF(ib,*))) &$
ENDFOR &$
p=PLOT(BINS(1:NBINS-1),-100.*(WSDI_CDF(imm,2,0:NBINS-2)-WSDI_CDF(imm,1,0:NBINS-2)),THICK=2,COLOR="green",/OVERPLOT,/XLOG,/YLOG) 
poly_p=POLYGON([BINS(1:NBINS-1),REVERSE(BINS(1:NBINS-1))],[MAX_P(0:NBINS-2),REVERSE(MIN_P(0:NBINS-2))],/DATA,/FILL_BACKGROUND, COLOR="green",FILL_COLOR="green",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,2]) &$

poly_p=POLYGON([MIN(FR2003),MAX(FR2003),MAX(FR2003),MIN(FR2003)],[1,1,50,50],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,2]) &$
poly_p=POLYGON([MIN(RU2010),MAX(RU2010),MAX(RU2010),MIN(RU2010)],[1,1,50,50],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,2]) &$
poly_p=POLYGON([MIN(EA2007),MAX(EA2007),MAX(EA2007),MIN(EA2007)],[1,1,50,50],/DATA,/FILL_BACKGROUND, COLOR="black",FILL_COLOR="black",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,2]) &$
poly_p=POLYGON([MIN(USA1980,/NaN),MAX(USA1980,/NaN),MAX(USA1980,/NaN),MIN(USA1980,/NaN)],[1,1,50,50],/DATA,/FILL_BACKGROUND, COLOR="green",FILL_COLOR="green",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,2]) &$

t1=TEXT(7,25,'+2C - REF',/DATA,COLOR="Red",FONT_SIZE=10,TARGET=p)
t2=TEXT(3,21,'+1.5C - REF',/DATA,COLOR="Blue",FONT_SIZE=10,TARGET=p)
t3=TEXT(4,5,'+2C - 1.5C',/DATA,COLOR="Green",FONT_SIZE=10,TARGET=p)

t1=TEXT(26,13,'EA 2007',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p,ORIENTATION=90)
t2=TEXT(39,13,'FR 2003',/DATA,COLOR="Blue",FONT_SIZE=10,TARGET=p,ORIENTATION=90)
t3=TEXT(46,13,'US 1980',/DATA,COLOR="Green",FONT_SIZE=10,TARGET=p,ORIENTATION=90)
t4=TEXT(94,13,'RU 2010',/DATA,COLOR="Red",FONT_SIZE=10,TARGET=p,ORIENTATION=90)

w4.Save,'./PLOTS/PLOT_'+STAT+'_CDF_WORLD_SCEN_LAND-CORR.pdf',BITMAP=1,PAGE_SIZE="A4"




STOP
END
