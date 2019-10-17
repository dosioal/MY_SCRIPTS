PRO PLOT_METHOD_PESETA4
DEVICE,decomposed=0
loadct,39
!p.background=255
!p.color=0

; MAKE WSDI - YEARLY
;=============================================
VAR_1='tasmax'
VAR_2='tas'
;VAR='tasmin'

STAT_1='TXx'
STAT_2='SM'

DIR_HOME='/media/NAS/LUC/SMHI/LR/'
RUN=['r1','r2','r3','r4','r5','r6','r7']
RUN=['r2']
NRUN=N_ELEMENTS(RUN)


SCEN='rcp85' ;1951-2005'
YEAR_INI=1976
YEAR_END=2100

NYEARS=30

Y_C_15=[2024,2033,2025,2028,2029,2025,2027]
Y_C_15=[2033]
Y_C_20=[2049]

NYEARS_T=FIX(YEAR_END)-FIX(YEAR_INI)

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
; READ MODELS
;===============================================
NT=3;REF,1.5,2.5
WSDI_Y_T_1=FLTARR(nx,ny,NYEARS_T,NRUN)
WSDI_Y_T_2=FLTARR(nx,ny,NYEARS_T,NRUN)
;WSDI_M=FLTARR(nx,ny,NT,NRUN) ; 1975-2005,1.5C, 2C, 2071-2100

;KS_EVAL=FLTARR(nx,ny,NRUN,NT)

ic=0 ;counter for window plot
;===============================================
;FOR ir=0,NRUN-1 DO BEGIN ;RCM LOOP
	ir=0
;===============================================

;===============================================
;DEF OF VARIABLES
;===============================================

;DIR MODELS
DIR_RUN=DIR_HOME+RUN(ir)+'/'+VAR_1+'/'
DIR_SM=DIR_RUN+'PP/'+STAT_1

print,''
print,'====================================='
print,'READING ',STAT_1,' FOR '
print,'MODEL= ',RUN(ir)
print,'YSTART ',YEAR_INI
print,'YEND ',YEAR_END
PRINT,'NYEAR=',NYEARS_T
print,'====================================='

;REF PER
;OPEN NCETCDF FILE'
DATA_FILE=DIR_SM+'/'+VAR_1+'_'+STAT_1+'.nc'
fileID = NCDF_Open(DATA_FILE)
varID = NCDF_VarID(fileID, VAR_1)
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
print,nx,ny,nt
NCDF_VARGET,fileID,VAR_1,WORK
help,work
IF ndims EQ 3 THEN BEGIN
WSDI_Y_T_1(*,*,0:29,ir)=REVERSE(REFORM(WORK(*,*,*)),2)
ENDIF ELSE BEGIN
WSDI_Y_T_1(*,*,0:29,ir)=REVERSE(REFORM(WORK(*,*,0,*)),2)
ENDELSE
NCDF_CLOSE, fileID

;
;Y_C_15=[2024,2033,2025,2028,2029,2025,2027]
;Y_C_20=[2033,2049,2037,2042,2046,2035,2039]
;OPEN NCETCDF FILE'
DATA_FILE=DIR_SM+'/'+VAR_1+'_rcp85_'+STAT_1+'.nc'
fileID = NCDF_Open(DATA_FILE)
varID = NCDF_VarID(fileID, VAR_1)
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
print,nx,ny
NCDF_VARGET,fileID,VAR_1,WORK
help,work
IF ndims EQ 3 THEN BEGIN
WSDI_Y_T_1(*,*,30:NYEARS_T-1,ir)=REVERSE(REFORM(WORK(*,*,*)),2)
;YSTART=Y_C_15(ir)-14-2006
;WSDI_Y_T(*,*,*,1,ir)=REVERSE(REFORM(WORK(*,*,YSTART:YSTART+NYEARS-1)),2)
;YSTART=Y_C_20(ir)-14-2006
;WSDI_Y_T(*,*,*,2,ir)=REVERSE(REFORM(WORK(*,*,YSTART:YSTART+NYEARS-1)),2)
ENDIF ELSE BEGIN
WSDI_Y_T_1(*,*,30:NYEARS_T-1,ir)=REVERSE(REFORM(WORK(*,*,0,*)),2)
;YSTART=Y_C_15(ir)-14-2006
;WSDI_Y_T(*,*,*,1,ir)=REVERSE(REFORM(WORK(*,*,0,YSTART:YSTART+NYEARS-1)),2)
;YSTART=Y_C_20(ir)-14-2006
;WSDI_Y_T(*,*,*,2,ir)=REVERSE(REFORM(WORK(*,*,0,YSTART:YSTART+NYEARS-1)),2)
ENDELSE

;=============
;READ VAR 2
;=============
;DIR MODELS
DIR_RUN=DIR_HOME+RUN(ir)+'/'+VAR_2+'/'
DIR_SM=DIR_RUN+'PP/'+STAT_2

print,''
print,'====================================='
print,'READING ',STAT_2,' FOR '
print,'MODEL= ',RUN(ir)
print,'YSTART ',YEAR_INI
print,'YEND ',YEAR_END
PRINT,'NYEAR=',NYEARS_T
print,'====================================='

;REF PER
;OPEN NCETCDF FILE'
DATA_FILE=DIR_SM+'/'+VAR_2+'_hist_'+STAT_2+'.nc'
fileID = NCDF_Open(DATA_FILE)
varID = NCDF_VarID(fileID, VAR_2)
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
print,nx,ny,nt
NCDF_VARGET,fileID,VAR_2,WORK
help,work
IF ndims EQ 3 THEN BEGIN
WSDI_Y_T_2(*,*,0:29,ir)=REVERSE(REFORM(WORK(*,*,*)),2)
ENDIF ELSE BEGIN
WSDI_Y_T_2(*,*,0:29,ir)=REVERSE(REFORM(WORK(*,*,0,*)),2)
ENDELSE
NCDF_CLOSE, fileID

;
;OPEN NCETCDF FILE'
DATA_FILE=DIR_SM+'/'+VAR_2+'_rcp85_'+STAT_2+'.nc'
fileID = NCDF_Open(DATA_FILE)
varID = NCDF_VarID(fileID, VAR_2)
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
print,nx,ny
NCDF_VARGET,fileID,VAR_2,WORK
help,work
IF ndims EQ 3 THEN BEGIN
WSDI_Y_T_2(*,*,30:NYEARS_T-1,ir)=REVERSE(REFORM(WORK(*,*,*)),2)
ENDIF ELSE BEGIN
WSDI_Y_T_2(*,*,30:NYEARS_T-1,ir)=REVERSE(REFORM(WORK(*,*,0,*)),2)
ENDELSE

;GLOBAL MEAN
TX_MEAN=FLTARR(NYEARS_T,NRUN)
FOR ir=0,NRUN-1 DO BEGIN &$
FOR iy=0,NYEARS_T-1 DO BEGIN &$
	WORK=FLTARR(NX,NY)*!VALUES.F_NaN &$
	WORK(*,59:359)=REFORM(WSDI_Y_T_1(*,59:359,iy,ir)) &$ ;EXCLUDE POLES
	WORK(WHERE(WORK GT 1E10))=!VALUES.F_NaN &$
	WORK(WHERE(LANDMASK LT 1))=!VALUES.F_NaN &$
TX_MEAN(iy,ir)=MEAN(WORK,/NaN) &$
ENDFOR &$
ENDFOR
TX_REF=MEAN(TX_MEAN(0:29,0))
TX_2005=MEAN(TX_MEAN(2005-14-1976:2005+15-1976,0))
TX_15=MEAN(TX_MEAN(Y_C_15-14-1976:Y_C_15+15-1976,0))
TX_20=MEAN(TX_MEAN(Y_C_20-14-1976:Y_C_20+15-1976,0))
print,TX_REF-273.15,TX_2005-273.15,TX_15-273.15,TX_20-273.15

T_MEAN=FLTARR(NYEARS_T,NRUN)
T_MEAN_LAND=FLTARR(NYEARS_T,NRUN)
FOR ir=0,NRUN-1 DO BEGIN &$
FOR iy=0,NYEARS_T-1 DO BEGIN &$
	WORK=FLTARR(NX,NY)*!VALUES.F_NaN &$
	WORK(*,*)=REFORM(WSDI_Y_T_2(*,*,iy,ir)) &$ ;EXCLUDE POLES
	WORK(WHERE(WORK GT 1E10))=!VALUES.F_NaN &$
T_MEAN(iy,ir)=MEAN(WORK,/NaN) &$
	WORK=FLTARR(NX,NY)*!VALUES.F_NaN &$
	WORK(*,59:359)=REFORM(WSDI_Y_T_2(*,59:359,iy,ir)) &$ ;EXCLUDE POLES
	WORK(WHERE(WORK GT 1E10))=!VALUES.F_NaN &$
	WORK(WHERE(LANDMASK LT 1))=!VALUES.F_NaN &$
;	WORK(*,0:59,*,*)=!VALUES.F_NaN &$ ;EXCLUDE ANTARTICA
;	WORK(*,349:359,*,*)=!VALUES.F_NaN &$ ;EXCLUDE ARTICA
T_MEAN_LAND(iy,ir)=MEAN(WORK,/NaN) &$
ENDFOR &$
ENDFOR
T_REF=MEAN(T_MEAN(0:29,0))
T_2005=MEAN(T_MEAN(2005-14-1976:2005+15-1976,0))
T_15=MEAN(T_MEAN(Y_C_15-14-1976:Y_C_15+15-1976,0))
T_20=MEAN(T_MEAN(Y_C_20-14-1976:Y_C_20+15-1976,0))

YEARS=FINDGEN(NYEARS_T)+YEAR_INI

PLOT,YEARS,T_MEAN(*,0)-T_REF,yrange=[-1,5]
;OPLOT,YEARS,T_MEAN_LAND(*,0)-T_MEAN_LAND(0,0),COLOR=50
OPLOT,YEARS,TX_MEAN(*,0)-TX_REF,COLOR=250
PLOTS,[1976,2005],[0,0],color=250
;PLOTS,[1990,2020],[TX_2005-TX_REF,TX_2005-TX_REF],color=250
PLOTS,[Y_C_15-14,Y_C_15+15],[TX_15-TX_REF,TX_15-TX_REF],color=250
PLOTS,[Y_C_20-14,Y_C_20+15],[TX_20-TX_REF,TX_20-TX_REF],color=250
PLOTS,[1976,2005],[0,0],color=0
PLOTS,[1990,2020],[T_2005-T_REF,T_2005-T_REF],color=0
PLOTS,[Y_C_15-14,Y_C_15+15],[T_15-T_REF,T_15-T_REF],color=0
PLOTS,[Y_C_20-14,Y_C_20+15],[T_20-T_REF,T_20-T_REF],color=0

;======================
;PLOT
;======================

loadct,39,RGB_TABLE=rgb

w1=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[900,800])
XRANGE=[1970,2070]
YRANGE=[-1,4]

p=PLOT(YEARS,T_MEAN(*,0)-T_REF,/CURRENT,XRANGE=XRANGE,YRANGE=YRANGE,color="black", $
YTITLE='Temperature anomaly vs. reference period ($\circ$C)',MARGIN=[0.08,0.05,0.05,0.05],THICK=1, $
;XTICKVALUES=TICKV_X,YTICKVALUES=TICKV_Y,YMINOR=0,THICK=2, $
AXIS_STYLE=1,FONT_SIZE=10,XTITLE='Years') &$
p=PLOT(YEARS,SMOOTH(T_MEAN(*,0)-T_REF,20,/EDGE_MIRROR),color="black",/OVERPLOT,THICK=2)  &$
p=PLOT(YEARS,TX_MEAN(*,0)-TX_REF,color="green",/OVERPLOT,THICK=1)  &$

;l=POLYLINE([Y_C_15-14,Y_C_15+15],[TX_15-TX_REF,TX_15-TX_REF],/DATA,color="green",TARGET=p,THICK=2)
;l=POLYLINE([Y_C_20-14,Y_C_20+15],[TX_20-TX_REF,TX_20-TX_REF],/DATA,color="green",TARGET=p,THICK=2)

l=POLYLINE([1975,1990],[3.5,3.5],/DATA,color="black",TARGET=p,THICK=1)
l=POLYLINE([1975,1990],[3.2,3.2],/DATA,color="black",TARGET=p,THICK=2)
l=POLYLINE([1975,1990],[2.9,2.9],/DATA,color="green",TARGET=p,THICK=1)
t=TEXT(1995,3.5,'Global annual mean temperature',/DATA,FONT_SIZE=12,COLOR="black",TARGET=p) 
t=TEXT(1995,3.2,'Global 20-year mean temperature',/DATA,FONT_SIZE=12,COLOR="black",TARGET=p) 
t=TEXT(1995,2.9,'TXx (land only)',/DATA,FONT_SIZE=12,COLOR="green",TARGET=p) 

;l=POLYLINE([1976,2005],[0,0],/DATA,color="black",TARGET=p,THICK=2)
;l=POLYLINE([1990,2020],[T_2005-T_REF,T_2005-T_REF],/DATA,color="black",TARGET=p,THICK=2)
;l=POLYLINE([Y_C_15-14,Y_C_15+15],[T_15-T_REF,T_15-T_REF],/DATA,color="black",TARGET=p,THICK=2)
;l=POLYLINE([Y_C_20-14,Y_C_20+15],[T_20-T_REF,T_20-T_REF],/DATA,color="black",TARGET=p,THICK=2)

pp=POLYGON([1981,2010,2010,1981],[-1,-1,1,1],/DATA,/FILL_BACKGROUND, COLOR="gray",FILL_COLOR="gray",FILL_TRANSPARENCY=80,TARGET=p)
t=TEXT(1981,1.1,'Reference period',/DATA,FONT_SIZE=12,COLOR="black",TARGET=p) 

;lv=POLYLINE([1976,1976],[-1,0],/DATA,color="black",TARGET=p,THICK=2,LINESTYLE=2)
;lv=POLYLINE([2005,2005],[-1,0],/DATA,color="black",TARGET=p,THICK=2,LINESTYLE=2)
;t=TEXT(1980,-0.8,'Reference period',/DATA,FONT_SIZE=12,COLOR="black",TARGET=p) 

;pp=POLYGON([Y_C_15-14,Y_C_15+15,Y_C_15+15,Y_C_15-14],[-1,-1,TX_15-TX_REF,TX_15-TX_REF],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p)
pp=POLYGON([Y_C_15-14,Y_C_15+15,Y_C_15+15,Y_C_15-14],[-1,-1,2,2],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p)
;lv=POLYLINE([Y_C_15-14,Y_C_15-14],[-1,TX_15-TX_REF],/DATA,color="blue",TARGET=p,THICK=2,LINESTYLE=2)
;lv=POLYLINE([Y_C_15+15,Y_C_15+15],[-1,TX_15-TX_REF],/DATA,color="blue",TARGET=p,THICK=2,LINESTYLE=2)
t=TEXT(Y_C_15-14,2.1,'+1.5$\circ$C period',/DATA,FONT_SIZE=12,COLOR="blue",TARGET=p) 

;pp=POLYGON([Y_C_20-14,Y_C_20+15,Y_C_20+15,Y_C_20-14],[-1,-1,TX_20-TX_REF,TX_20-TX_REF],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p)
pp=POLYGON([Y_C_20-14,Y_C_20+15,Y_C_20+15,Y_C_20-14],[-1,-1,3,3],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p)
;lv=POLYLINE([Y_C_20-14,Y_C_20-14],[-1,TX_20-TX_REF],/DATA,color="red",TARGET=p,THICK=2,LINESTYLE=2)
;lv=POLYLINE([Y_C_20+15,Y_C_20+15],[-1,TX_20-TX_REF],/DATA,color="red",TARGET=p,THICK=2,LINESTYLE=2)
t=TEXT(Y_C_20-14,3.1,'+2$\circ$C period',/DATA,FONT_SIZE=12,COLOR="red",TARGET=p) 

lv=POLYLINE([1970,2070],[T_2005-T_REF-0.8,T_2005-T_REF-0.8],/DATA,color="black",TARGET=p,THICK=2,LINESTYLE=2)
t=TEXT(2055,-0.5,'Pre-industrial',/DATA,FONT_SIZE=12,COLOR="black",TARGET=p) 
;a1=ARROW([1995,1995],[T_2005-T_REF-0.8,T_2005-T_REF],/DATA,color="black",TARGET=p,THICK=1,ARROW_STYLE=3)
a1=ARROW([1995,1995],[T_2005-T_REF-0.8,0],/DATA,color="black",TARGET=p,THICK=1,ARROW_STYLE=3)
t=TEXT(2000,-0.3,'+0.7$\circ$C',/DATA,FONT_SIZE=12,COLOR="black",TARGET=p)  
a2=ARROW([Y_C_15,Y_C_15],[T_2005-T_REF-0.8,T_15-T_REF-0.03],/DATA,color="blue",TARGET=p,THICK=1,ARROW_STYLE=3)
t=TEXT(Y_C_15-8,-0.3,'+1.5$\circ$C',/DATA,FONT_SIZE=12,COLOR="blue",TARGET=p)  
a3=ARROW([Y_C_20,Y_C_20],[T_2005-T_REF-0.8,T_20-T_REF],/DATA,color="red",TARGET=p,THICK=1,ARROW_STYLE=3)
t=TEXT(Y_C_20+1,-0.3,'+2$\circ$C',/DATA,FONT_SIZE=12,COLOR="red",TARGET=p)  

;t=TEXT(0,0.18,MASK(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$
;t=TEXT(-3.5,0.14,STRMID(STRING(MEAN(WSDI_YEAR_M(imm,*,1,*),/NaN)-MEAN(WSDI_YEAR_M(imm,*,0,*),/NaN)),6,3),/DATA,FONT_SIZE=10,COLOR="blue",TARGET=p) &$
;t=TEXT(-3.5,0.12,STRMID(STRING(MEAN(WSDI_YEAR_M(imm,*,2,*),/NaN))-MEAN(WSDI_YEAR_M(imm,*,0,*),/NaN),6,3),/DATA,FONT_SIZE=10,COLOR="red",TARGET=p) &$
;text1=TEXT(.53,.12,'___ 1976-2005 ',/NORMAL,FONT_SIZE=12,COLOR="black",TARGET=p)
;text1=TEXT(.53,.08,'___ +1.5$\circ$C',/NORMAL,FONT_SIZE=12,COLOR="blue",TARGET=p)
;text1=TEXT(.53,.04,'___+2$\circ$C',/NORMAL,FONT_SIZE=12,COLOR="red",TARGET=p)


w1.Save,'./PLOTS/PLOT_METHOD_2C_PESETA4.pdf',BITMAP=1,PAGE_SIZE="A4"
STOP





STOP
END
