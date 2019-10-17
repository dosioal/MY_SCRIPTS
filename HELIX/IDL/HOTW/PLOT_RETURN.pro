PRO PLOT_RETURN
DEVICE,decomposed=0
loadct,39
!p.background=255
!p.color=0

; MAKE WSDI - YEARLY
;=============================================
VAR='tasmax'
;VAR='tasmin'

SSP='SSP3'

STAT='SM'
;STAT='WSDI' ; TOTAL N OF DAYS WITH SPELL LENGHT> THRES  DASY (= ETCCDI WSDI)
;STAT='WSDI_M' ;  MAX WARM SPEL LENGHT
STAT='HWMId'

DIR_HOME='/media/NAS/LUC/SMHI/LR/'
RUN=['r1','r2','r3','r4','r5','r6','r7']
NRUN=N_ELEMENTS(RUN)

SCEN='rcp85' ;1951-2005'
YEAR_INI=1971
YEAR_END=2100

NYEARS=30

;=============================================
BINSIZE=3
NBINS=35
BINS=FINDGEN(NBINS)*BINSIZE

;===============================================
NYEAR_T=FIX(YEAR_END)-FIX(YEAR_INI)

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

;====================================
;TIMING OF 2C
;====================================
;ORIGINAL HELIX
;Y_C_15=[2015,2040,2027,2019,2022,2020,2003]
;Y_C_20=[2030,2055,2039,2035,2038,2034,2020]
;Y_C_40=[2068,2113,2074,2083,2102,2069,2065]

Y_C_15=[2024,2033,2025,2028,2029,2025,2027]
Y_C_20=[2033,2049,2037,2042,2046,2035,2039]
Y_C_40=[2057,2077,2055,2066,2074,2052,2061] ; 3C

;=====================
;OBS VALUES
;=====================
;EOBS  ERA  NCEP

EU2003=[47.48,33.6];,38.2]
RU2010=[69.16,100];,76.7]
EA2007=[22.11,21];,28.2]
USA1980=[!VALUES.F_NaN,39];,48.2]

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
lon_o=REVERSE(lon,2)

openr,1,DIR_HOME+'lat_05.txt'
WORK=FLTARR(nx)
FOR iy=0,ny-1 DO BEGIN
readf,1,work
lat(*,iy)=WORK
ENDFOR
close,1
lat_o=REVERSE(lat,2)

;===========================
;CHECK OUT FILE
;===========================

FILEO_DATA='./DATA/RET_LEV_M.dat'
print,'====================================='
print,'CHECKING ',fileo_data
print,'====================================='
count=""
count=FILE_SEARCH(FILEO_DATA)

IF COUNT NE "" THEN BEGIN
print,'FILE EXISTS!'
print,''
GOTO,JUMP_CALC
ENDIF

;===============================================
; READ MODELS
;===============================================
RET_LEV_T=FLTARR(nx,280,7,3,NRUN)
RET_LEV_ERR_T=FLTARR(nx,280,7,3,NRUN)

ic=0 ;counter for window plot
;===============================================
FOR ir=0,NRUN-1 DO BEGIN ;RCM LOOP
;===============================================

FILEO='./DATA/RETURN_HWMId_'+RUN(ir)+'.dat'

print,'====================================='
print,'CHECKING ',fileo
print,'====================================='
count=""
count=FILE_SEARCH(FILEO)

;IF COUNT EQ 1 THEN BEGIN
IF COUNT NE "" THEN BEGIN
print,'FILE EXISTS!'
print,''
ENDIF

;===============================================
;DEF OF VARIABLES
;===============================================

;DIR MODELS
DIR_SM=DIR_HOME+'RETURN_PERIOD'

print,''
print,'====================================='
print,'READING ',STAT,' FOR '
print,'MODEL= ',RUN(ir)
print,'YSTART ',YEAR_INI
print,'YEND ',YEAR_END
PRINT,'NYEAR=',NYEAR_T
print,'====================================='

FILEI=DIR_SM+'/'+VAR+'_'+RUN(ir)+'_'+SCEN+'_'+STRMID(YEAR_INI,4,4)+'-'+STRMID(YEAR_END,4,4)+'_'+STAT+'_extremes.nc'

print,'====================================='
print,'CHECKING ',filei
print,'====================================='

fileID = NCDF_Open(FILEI)
varID = NCDF_VarID(fileID,'returnlevel')
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
np=dims_a(2)
NCDF_VARGET,fileID,'returnlevel',RET_LEV;,count=[nx,ny],offset=[0,0]
NCDF_VARGET,fileID,'returnlevelError',RET_LEV_ERR;,count=[nx,ny],offset=[0,0]
NCDF_VARGET,fileID,'returnperiod',RET_PER;,count=[nx,ny],offset=[0,0]
NCDF_VARGET,fileID,'lon',lon;,count=[nx,ny],offset=[0,0]
NCDF_VARGET,fileID,'lat',lat;,count=[nx,ny],offset=[0,0]
NCDF_CLOSE, fileID
help,lon,lat

FOR ix=0,nx-1 DO BEGIN
FOR iy=0,ny-1 DO BEGIN
FOR ip=0,NP-1 DO BEGIN
RET_LEV_T(ix,iy,ip,0,ir)=MEAN(RET_LEV(ix,iy,ip,5:34),/NaN)
RET_LEV_T(ix,iy,ip,1,ir)=MEAN(RET_LEV(ix,iy,ip,Y_C_15(ir)-15-YEAR_INI:Y_C_15(ir)+14-YEAR_INI),/NaN)
RET_LEV_T(ix,iy,ip,2,ir)=MEAN(RET_LEV(ix,iy,ip,Y_C_20(ir)-15-YEAR_INI:Y_C_20(ir)+14-YEAR_INI),/NaN)
RET_LEV_ERR_T(ix,iy,ip,0,ir)=MEAN(RET_LEV_ERR(ix,iy,ip,5:34),/NaN)*2.  ; 66%-->95%
RET_LEV_ERR_T(ix,iy,ip,1,ir)=MEAN(RET_LEV_ERR(ix,iy,ip,Y_C_15(ir)-15-YEAR_INI:Y_C_15(ir)+14-YEAR_INI),/NaN)*2.
RET_LEV_ERR_T(ix,iy,ip,2,ir)=MEAN(RET_LEV_ERR(ix,iy,ip,Y_C_20(ir)-15-YEAR_INI:Y_C_20(ir)+14-YEAR_INI),/NaN)*2.
ENDFOR
ENDFOR
ENDFOR


;GOTO,JUMP_PLOT
!p.multi=0
!p.color=0
!p.background=255
window,0,retain=2
ix=360
iy=130
PLOT,RET_PER,RET_LEV_T(ix,iy,*,0,ir),yrange=[0,100],xrange=[1,100]
OPLOT,RET_PER,RET_LEV_T(ix,iy,*,1,ir),color=50
OPLOT,RET_PER,RET_LEV_T(ix,iy,*,2,ir),color=250

levels=[0,12,24,48,96,192,384]/2
colors=[255,80,100,150,190,210,250]

window,ir+1,retain=2,title=RUN(ir),xsize=1200,ysize=900
!p.multi=[0,3,3]
contour,landmask,lon_o,lat_o
contour,RET_LEV_T(*,*,0,0,ir),lon,lat,levels=levels,c_colors=colors,/FILL,/OVERPLOT
contour,landmask,lon_o,lat_o
contour,RET_LEV_T(*,*,0,1,ir),lon,lat,levels=levels,c_colors=colors,/FILL,/OVERPLOT
contour,landmask,lon_o,lat_o
contour,RET_LEV_T(*,*,0,2,ir),lon,lat,levels=levels,c_colors=colors,/FILL,/OVERPLOT

contour,landmask,lon_o,lat_o
contour,RET_LEV_T(*,*,2,0,ir),lon,lat,levels=levels,c_colors=colors,/FILL,/OVERPLOT
contour,landmask,lon_o,lat_o
contour,RET_LEV_T(*,*,2,1,ir),lon,lat,levels=levels,c_colors=colors,/FILL,/OVERPLOT
contour,landmask,lon_o,lat_o
contour,RET_LEV_T(*,*,2,2,ir),lon,lat,levels=levels,c_colors=colors,/FILL,/OVERPLOT

contour,landmask,lon_o,lat_o
contour,RET_LEV_T(*,*,3,0,ir),lon,lat,levels=levels,c_colors=colors,/FILL,/OVERPLOT
contour,landmask,lon_o,lat_o
contour,RET_LEV_T(*,*,3,1,ir),lon,lat,levels=levels,c_colors=colors,/FILL,/OVERPLOT
contour,landmask,lon_o,lat_o
contour,RET_LEV_T(*,*,3,2,ir),lon,lat,levels=levels,c_colors=colors,/FILL,/OVERPLOT
JUMP_PLOT:


RET_LEV_ERR=1 ;FREE MEM
RET_LEV=1 ;FREE MEM
ENDFOR ;RCM

;MODLES MEDIAN
print,'MODEL MEDIAN'
RET_LEV_M=FLTARR(nx,ny,NP,3)
FOR ip=0,NP-1 DO BEGIN &$
FOR it=0,2 DO BEGIN &$
FOR ix=0,nx-1 DO BEGIN &$
FOR iy=0,ny-1 DO BEGIN &$
RET_LEV_M(ix,iy,ip,it)=MEDIAN(RET_LEV_T(ix,iy,ip,it,*)) &$
ENDFOR &$
ENDFOR &$
WORK=REFORM(RET_LEV_M(*,*,ip,it)) &$
WORK(WHERE(FINITE(WORK,/NaN) EQ 1))=0 &$
RET_LEV_M(*,*,ip,it)=WORK &$
ENDFOR &$
ENDFOR

;MODLES ERR AGREEMENT
print,'MODEL ERR AGR'
MOD_AGR_RET_LEV=FLTARR(nx,ny,NP,3)
FOR ip=0,NP-1 DO BEGIN &$
FOR ix=0,Nx-1 DO BEGIN &$
FOR iy=0,Ny-1 DO BEGIN &$
IF landmask(ix,iy+69) GT 0.5 THEN BEGIN &$
FOR ir=0,NRUN -1 DO BEGIN
IF RET_LEV_T(ix,iy,ip,1,ir) GT RET_LEV_T(ix,iy,ip,0,ir)+RET_LEV_ERR_T(ix,iy,ip,0,ir) THEN MOD_AGR_RET_LEV(ix,iy,ip,0)=MOD_AGR_RET_LEV(ix,iy,ip,0)+1
IF RET_LEV_T(ix,iy,ip,2,ir) GT RET_LEV_T(ix,iy,ip,0,ir)+RET_LEV_ERR_T(ix,iy,ip,0,ir) THEN MOD_AGR_RET_LEV(ix,iy,ip,1)=MOD_AGR_RET_LEV(ix,iy,ip,1)+1
IF RET_LEV_T(ix,iy,ip,2,ir) GT RET_LEV_T(ix,iy,ip,1,ir)+RET_LEV_ERR_T(ix,iy,ip,1,ir) THEN MOD_AGR_RET_LEV(ix,iy,ip,2)=MOD_AGR_RET_LEV(ix,iy,ip,2)+1
ENDFOR
ENDIF 
ENDFOR
ENDFOR
ENDFOR

!p.multi=0
!p.color=0
!p.background=255
window,11,retain=2,xsize=600,ysize=1200
!p.multi=[0,1,2]
ix=360
iy=130
PLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,0,0))],yrange=[0,100],xrange=[1,100]
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,0,0))+REFORM(RET_LEV_ERR_T(ix,iy,*,0,0))/2.],linestyle=2
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,0,0))-REFORM(RET_LEV_ERR_T(ix,iy,*,0,0))/2.],linestyle=2
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,1,0))],color=50
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,1,0))+REFORM(RET_LEV_ERR_T(ix,iy,*,1,0))/2.],linestyle=2,color=50
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,1,0))-REFORM(RET_LEV_ERR_T(ix,iy,*,1,0))/2.],linestyle=2,color=50
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,2,0))],color=250
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,2,0))+REFORM(RET_LEV_ERR_T(ix,iy,*,2,0))/2.],linestyle=2,color=250
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,2,0))-REFORM(RET_LEV_ERR_T(ix,iy,*,2,0))/2.],linestyle=2,color=250
ix=570
iy=180
PLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,0,0))],yrange=[0,100],xrange=[1,100]
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,0,0))+REFORM(RET_LEV_ERR_T(ix,iy,*,0,0))/2.],linestyle=2
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,0,0))-REFORM(RET_LEV_ERR_T(ix,iy,*,0,0))/2.],linestyle=2
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,1,0))],color=50
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,1,0))+REFORM(RET_LEV_ERR_T(ix,iy,*,1,0))/2.],linestyle=2,color=50
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,1,0))-REFORM(RET_LEV_ERR_T(ix,iy,*,1,0))/2.],linestyle=2,color=50
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,2,0))],color=250
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,2,0))+REFORM(RET_LEV_ERR_T(ix,iy,*,2,0))/2.],linestyle=2,color=250
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_T(ix,iy,*,2,0))-REFORM(RET_LEV_ERR_T(ix,iy,*,2,0))/2.],linestyle=2,color=250


;INTERPOLATION AND REVERSE FUNCTION
print,'REVERSE FUNCTION'
;zz0_M=FLTARR(nx,ny,3,100,NRUN)*!VALUES.F_NaN
;zz0_ERR_M=FLTARR(nx,ny,3,100,NRUN)*!VALUES.F_NaN
zz0=FLTARR(nx,ny,100,3)*!VALUES.F_NaN
zz0_ERR=FLTARR(nx,ny,100,3)*!VALUES.F_NaN
MOD_AGR_zz0=FLTARR(nx,ny,100,3) 

FOR ix=0,NX-1 DO BEGIN &$
FOR iy=0,Ny-1 DO BEGIN &$

IF landmask(ix,iy+69) GT 0.5 THEN BEGIN &$
zz0_M=FLTARR(100,3,NRUN)*!VALUES.F_NaN &$
zz0_ERR_M=FLTARR(100,3,NRUN)*!VALUES.F_NaN &$
FOR it=0,2 DO BEGIN &$
FOR ir=0,NRUN-1 DO BEGIN &$
y=[0,REFORM(RET_LEV_T(ix,iy,*,it,ir))] &$
z0=SPLINE([0,RET_PER],y,FINDGEN(999),30) &$ ; RET PER INTERPOLATED
zz0_M(*,it,ir)=SPLINE(z0,FINDGEN(999),FINDGEN(100),0) &$ ;INVERSE FUNCTION
y=[0,REFORM(RET_LEV_T(ix,iy,*,it,ir))+REFORM(RET_LEV_ERR_T(ix,iy,*,it,ir))] &$
z0=SPLINE([0,RET_PER],y,FINDGEN(999)) &$
zz0_ERR_M(*,it,ir)=SPLINE(z0,FINDGEN(999),FINDGEN(100),0) &$
ENDFOR &$ ;ir

FOR ip=0,99 DO BEGIN &$
zz0(ix,iy,ip,it)=MEDIAN(zz0_M(ip,it,*)) &$
zz0_ERR(ix,iy,ip,it)=MEDIAN(zz0_ERR_M(ip,it,*)) &$
ENDFOR &$ ;ip

ENDFOR &$ ;it

;a=[0,REFORM(RET_LEV_T(ix,iy,*,0,ir))]
;aaz=SPLINE([0,RET_PER],a,FINDGEN(999),30)
;b=[0,REFORM(RET_LEV_T(ix,iy,*,1,ir))]
;bbz=SPLINE([0,RET_PER],b,FINDGEN(999),30)
;zzzb=SPLINE(bbz,FINDGEN(999),FINDGEN(100),0)
;INTERPOL(zzzb,findgen(100),aaz(99))


FOR ip=0,99 DO BEGIN &$
FOR ir=0,NRUN -1 DO BEGIN &$
IF zz0_M(ip,1,ir) LE zz0_ERR_M(ip,0,ir) THEN MOD_AGR_zz0(ix,iy,ip,0)=MOD_AGR_zz0(ix,iy,ip,0)+1 &$
IF zz0_M(ip,2,ir) LE zz0_ERR_M(ip,0,ir) THEN MOD_AGR_zz0(ix,iy,ip,1)=MOD_AGR_zz0(ix,iy,ip,1)+1 &$
IF zz0_M(ip,2,ir) LE zz0_ERR_M(ip,1,ir) THEN MOD_AGR_zz0(ix,iy,ip,2)=MOD_AGR_zz0(ix,iy,ip,2)+1 &$
ENDFOR &$ ;ir
ENDFOR &$ ;ip


IF ix EQ 360 AND iy EQ 130 THEN BEGIN
window,12,retain=2
plot,FINDGEN(100),zz0_M(*,0,0),color=0,thick=2,linestyle=0,yrange=[0,100]
oplot,FINDGEN(100),zz0_ERR_M(*,0,0),color=0,thick=1,linestyle=2
oplot,FINDGEN(100),zz0_M(*,1,0),color=30,thick=2,linestyle=0
oplot,FINDGEN(100),zz0_ERR_M(*,1,0),color=30,thick=1,linestyle=2
oplot,FINDGEN(100),zz0_M(*,2,0),color=250,thick=2,linestyle=0
oplot,FINDGEN(100),zz0_ERR_M(*,2,0),color=250,thick=1,linestyle=2
STOP
ENDIF
;

ENDIF &$
ENDFOR &$ ;iy
ENDFOR ;ix

RET_LEV_T=0.


!p.multi=0
!p.color=0
!p.background=255
;window,11,retain=2
ix=360
iy=130
PLOT,[0,RET_PER],[0,REFORM(RET_LEV_M(ix,iy,*,0))],yrange=[0,100],xrange=[1,100]
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_M(ix,iy,*,1))],color=50
OPLOT,[0,RET_PER],[0,REFORM(RET_LEV_M(ix,iy,*,2))],color=250
;oplot,FINDGEN(999),z0,linestyle=2

window,12,retain=2
plot,FINDGEN(100),zz0(ix,iy,*,0),color=0,thick=2,linestyle=0,yrange=[0,100]
oplot,FINDGEN(100),zz0_ERR(ix,iy,*,0),color=0,thick=1,linestyle=2
;oplot,FINDGEN(100),zz0(ix,iy,*,0)-zz0_ERR(ix,iy,*,0),color=0,thick=1,linestyle=0
oplot,FINDGEN(100),zz0(ix,iy,*,1),color=30,thick=2,linestyle=0
oplot,FINDGEN(100),zz0_ERR(ix,iy,*,1),color=30,thick=1,linestyle=2
;oplot,FINDGEN(100),zz0(ix,iy,*,1)-zz0_ERR(ix,iy,*,1),color=30,thick=1,linestyle=0
oplot,FINDGEN(100),zz0(ix,iy,*,2),color=250,thick=2,linestyle=0
oplot,FINDGEN(100),zz0_ERR(ix,iy,*,2),color=250,thick=1,linestyle=2
;oplot,FINDGEN(100),zz0(ix,iy,*,2)-zz0_ERR(ix,iy,*,2),color=250,thick=1,linestyle=0

save,filename=fileo_data,nx,ny,lon,lat,zz0,RET_LEV_M,MOD_AGR_RET_LEV,MOD_AGR_zz0,CC,RET_LEV_MASK

JUMP_CALC:
restore,filename=fileo_data;,nx,ny,lon,lat,zz0,RET_LEV_M

;=================
;PLOTS
;=================
RET_PER=[5,10,20,50,100,300,1000]

loadct,39,RGB_TABLE=rgb
levels=[0,12,24,48,96,192,384]/2
colors=[255,100,150,190,210,250]

pattern_r = Obj_New('idlgrpattern', 1, ORIENTATION=-45,SPACING=4,THICK=0.8)

w = WINDOW(WINDOW_TITLE='Median',DIMENSIONS=[1200,1200])
MARGIN=[0.02,0.02,0.02,0.02]
FOR it=0,2 DO BEGIN &$
w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,5,it+1],MARGIN=MARGIN, $
        title='R.P. 5 yrs',/CURRENT) &$
c_plot= CONTOUR(RET_LEV_M(*,*,0,it),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
	mc = MAPCONTINENTS(THICK=0.5) &$
IF it NE 0 THEN s_plot= CONTOUR(MOD_AGR_RET_LEV(*,*,0,it-1),lon,lat,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2) &$
ENDFOR
FOR it=0,2 DO BEGIN &$
w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,5,it+1+3],MARGIN=MARGIN, $
        title='R.P. 10 yrs',/CURRENT) &$
c_plot= CONTOUR(RET_LEV_M(*,*,1,it),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
IF it NE 0 THEN s_plot= CONTOUR(MOD_AGR_RET_LEV(*,*,1,it-1),lon,lat,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2) &$
	mc = MAPCONTINENTS(THICK=0.5) &$
ENDFOR
FOR it=0,2 DO BEGIN &$
w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,5,it+1+6],MARGIN=MARGIN, $
        title='R.P. 20 yrs',/CURRENT) &$
c_plot= CONTOUR(RET_LEV_M(*,*,2,it),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
IF it NE 0 THEN s_plot= CONTOUR(MOD_AGR_RET_LEV(*,*,2,it-1),lon,lat,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2) &$
	mc = MAPCONTINENTS(THICK=0.5) &$
ENDFOR
FOR it=0,2 DO BEGIN &$
w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,5,it+1+9],MARGIN=MARGIN, $
        title='R.P. 50 yrs',/CURRENT) &$
c_plot= CONTOUR(RET_LEV_M(*,*,3,it),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
IF it NE 0 THEN s_plot= CONTOUR(MOD_AGR_RET_LEV(*,*,3,it-1),lon,lat,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2) &$
	mc = MAPCONTINENTS(THICK=0.5) &$
ENDFOR
FOR it=0,2 DO BEGIN &$
w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,5,it+1+12],MARGIN=MARGIN, $
        title='R.P. 100 yrs',/CURRENT) &$
c_plot= CONTOUR(RET_LEV_M(*,*,4,it),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
IF it NE 0 THEN s_plot= CONTOUR(MOD_AGR_RET_LEV(*,*,4,it-1),lon,lat,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2) &$
	mc = MAPCONTINENTS(THICK=0.5) &$
ENDFOR

tickname = STRING(levels, FORMAT='(f6.2)') 
cb0 = Colorbar(TARGET=c_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[.3,0.05,.7,0.06],FONT_SIZE=8,TITLE='HWMId Max.')

w.Save,'./PLOTS/PLOT_'+STAT+'_RET-LEV_SCEN.pdf',BITMAP=1,PAGE_SIZE="A4"


FOR it=0,2 DO BEGIN &$
FOR ib=0,100-1 DO BEGIN &$
WORK=REFORM(zz0(*,*,ib,it)) &$
WORK(WHERE(WORK GT 300))=!VALUES.F_NaN &$
zz0(*,*,ib,it)=WORK &$
ENDFOR &$
ENDFOR
;FOR ix=0,nx-1 DO BEGIN &$
;FOR iy=0,ny-1 DO BEGIN &$
;WORK=REFORM(zz0(ix,iy,it,*)) &$
;MIN_W=MIN(WORK) &$
;IF MIN_W LT 0 AND FINITE(MIN_w,/NaN) EQ 1 THEN WORK(0:WHERE(WORK EQ MIN_W))=!VALUES.F_NaN &$
;;WORK(WHERE(WORK LT 2))=!VALUES.F_NaN &$
;WORK(WHERE(WORK LT 0))=!VALUES.F_NaN &$
;WORK(WHERE(WORK GT 100))=!VALUES.F_NaN &$
;zz0(ix,iy,it,*)=WORK &$
;ENDFOR &$
;ENDFOR &$
;ENDFOR


loadct,39,RGB_TABLE=rgb
levels=[1,5,10,20,50,100,300]
colors=REVERSE([35,55,100,150,190,250])
levels=[1,5,10,20,30,50,100,300]
colors=REVERSE([10,35,55,100,150,190,250])
levels_d=REVERSE([1,5,10,20,50,100,300]*(-1))
colors_d=REVERSE(([35,55,100,150,190,250]))

w1 = WINDOW(WINDOW_TITLE='Median',DIMENSIONS=[1200,900])
w1 = WINDOW(WINDOW_TITLE='Median',DIMENSIONS=[800,900])
MARGIN=[0.02,0.02,0.02,0.02]
LEV_L=[24,48,96]

PANEL_l=['a','c','e']
PANEL_r=['b','d','f']

FOR il=0,2 DO BEGIN &$
;w1_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
;        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
;        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,3,il*3+1],MARGIN=MARGIN, $
;        title='1.5 - REF',/CURRENT) &$
;c_plot= CONTOUR(zz0(*,*,1,LEV_L(il)-1)-zz0(*,*,0,LEV_L(il)-1),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
;c1_plot= CONTOUR(zz0(*,*,0,LEV_L(il)-1),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
;mc = MAPCONTINENTS(THICK=0.5) &$

w1_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
        ;FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,3,il*3+1],MARGIN=MARGIN, $
        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,3,il*2+1],MARGIN=MARGIN, $
        title='HWMId='+STRMID(LEV_L(il),6,2)+', +1.5$\circ$C',/CURRENT) &$
;c_plot= CONTOUR(zz0(*,*,2,LEV_L(il)-1)-zz0(*,*,0,LEV_L(il)-1),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
c1_plot= CONTOUR(zz0(*,*,LEV_L(il)-1,1),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
SIG=REFORM(MOD_AGR_zz0(*,*,LEV_L(il)-1,0)) &$
SIG(WHERE(zz0(*,*,LEV_L(il)-1,1) GT 300))=!VALUES.F_NaN &$
SIG(WHERE(FINITE(zz0(*,*,LEV_L(il)-1,1),/NaN) EQ 1))=!VALUES.F_NaN &$
s_plot= CONTOUR(SIG,lon,lat,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2) &$
mc = MAPCONTINENTS(THICK=0.5) &$
t=TEXT(0.1,1,PANEL_l(il),/RELATIVE,FONT_SIZE=14,TARGET=w1_plot) &$

w1_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
        ;FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,3,il*3+2],MARGIN=MARGIN, $
        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,3,il*2+2],MARGIN=MARGIN, $
        title='HWMId='+STRMID(LEV_L(il),6,2)+', +2$\circ$C',/CURRENT) &$
;c_plot= CONTOUR(zz0(*,*,LEV_L(il)-1,1)-zz0(*,*,LEV_L(il)-1,2),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
c1_plot= CONTOUR(zz0(*,*,LEV_L(il)-1,2),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
SIG=REFORM(MOD_AGR_zz0(*,*,LEV_L(il)-1,1)) &$
SIG(WHERE(zz0(*,*,LEV_L(il)-1,2) GT 300))=!VALUES.F_NaN &$
SIG(WHERE(FINITE(zz0(*,*,LEV_L(il)-1,2),/NaN) EQ 1))=!VALUES.F_NaN &$
s_plot= CONTOUR(SIG,lon,lat,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2) &$
mc = MAPCONTINENTS(THICK=0.5) &$
t=TEXT(0.1,1,PANEL_r(il),/RELATIVE,FONT_SIZE=14,TARGET=w1_plot) &$
ENDFOR

tickname = STRING(levels, FORMAT='(f7.0)') &$
;cb0 = Colorbar(TARGET=c1_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[.1,.04,.55,.05],FONT_SIZE=8,TITLE='Return level (years)')
cb0 = Colorbar(TARGET=c1_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[.25,.04,.75,.05],FONT_SIZE=8,TITLE='Return period (years)')

w1.Save,'./PLOTS/PLOT_'+STAT+'_RET-PER_SCEN.pdf',BITMAP=1,PAGE_SIZE="A4"




;============
STOP
;============

FOR il=0,2 DO BEGIN &$
w2_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,3,il*3+3],MARGIN=MARGIN, $
        title='2$\circ$C - 1.5$\circ$C',/CURRENT) &$
c2_plot= CONTOUR(zz0(*,*,2,LEV_L(il)-1)-zz0(*,*,1,LEV_L(il)-1),lon,lat,c_value=levels_d,RGB_INDICES=colors_d,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
;c1_plot= CONTOUR(zz0(*,*,2,LEV_L(il)-1),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
mc = MAPCONTINENTS(THICK=0.5) &$
ENDFOR

tickname = STRING(levels_d, FORMAT='(f8.3)') &$
cb0 = Colorbar(TARGET=c2_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[.7,.04,.95,.05],FONT_SIZE=8,TITLE='Difference in return period (years)')



STOP
END
