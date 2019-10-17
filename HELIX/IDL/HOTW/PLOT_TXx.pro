PRO PLOT_TXx
DEVICE,decomposed=0
loadct,39
!p.background=255
!p.color=0

; MAKE WSDI - YEARLY
;=============================================
VAR='tasmax'
;VAR='tasmin'

STAT='TXx'

DIR_HOME='/media/NAS/LUC/SMHI/LR/'
RUN=['r1','r2','r3','r4','r5','r6','r7']
NRUN=N_ELEMENTS(RUN)

SCEN='hist' ;1951-2005'
IF STAT EQ 'TXx' THEN YEAR_INI=1976 ELSE YEAR_INI=1971 
YEAR_END=2005

SCEN='rcp85' ;1951-2005'
YEAR_INI=1976
YEAR_END=2100

NYEARS=30

Y_C_15=[2024,2033,2025,2028,2029,2025,2027]
Y_C_20=[2033,2049,2037,2042,2046,2035,2039]

NYEAR_T=FIX(YEAR_END)-FIX(YEAR_INI)

;FOR ROBUSTNESS
BINSIZE=0.5
NBINS=201
BINS=FINDGEN(NBINS)*BINSIZE-35

;FOR PDF
BINSIZE_T=0.2
NBINS_T=501
BINS_T=FINDGEN(NBINS_T)*BINSIZE_T-35


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
WSDI_Y_T=FLTARR(nx,ny,NYEARS,NT,NRUN)
WSDI_M=FLTARR(nx,ny,NT,NRUN) ; 1975-2005,1.5C, 2C, 2071-2100

KS_EVAL=FLTARR(nx,ny,NRUN,NT)

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

;REF PER
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
nx=dims_a(0)
ny=dims_a(1)
print,nx,ny,nt
NCDF_VARGET,fileID,VAR,WORK
help,work
IF ndims EQ 3 THEN BEGIN
WSDI_Y_T(*,*,*,0,ir)=REVERSE(REFORM(WORK(*,*,*)),2)
ENDIF ELSE BEGIN
WSDI_Y_T(*,*,*,0,ir)=REVERSE(REFORM(WORK(*,*,0,*)),2)
ENDELSE
NCDF_CLOSE, fileID

;
;Y_C_15=[2024,2033,2025,2028,2029,2025,2027]
;Y_C_20=[2033,2049,2037,2042,2046,2035,2039]
;OPEN NCETCDF FILE'
DATA_FILE=DIR_SM+'/'+VAR+'_rcp85_TXx.nc'
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
nx=dims_a(0)
ny=dims_a(1)
print,nx,ny
NCDF_VARGET,fileID,VAR,WORK
help,work
IF ndims EQ 3 THEN BEGIN
YSTART=Y_C_15(ir)-14-2006
WSDI_Y_T(*,*,*,1,ir)=REVERSE(REFORM(WORK(*,*,YSTART:YSTART+NYEARS-1)),2)
YSTART=Y_C_20(ir)-14-2006
WSDI_Y_T(*,*,*,2,ir)=REVERSE(REFORM(WORK(*,*,YSTART:YSTART+NYEARS-1)),2)
ENDIF ELSE BEGIN
YSTART=Y_C_15(ir)-14-2006
WSDI_Y_T(*,*,*,1,ir)=REVERSE(REFORM(WORK(*,*,0,YSTART:YSTART+NYEARS-1)),2)
YSTART=Y_C_20(ir)-14-2006
WSDI_Y_T(*,*,*,2,ir)=REVERSE(REFORM(WORK(*,*,0,YSTART:YSTART+NYEARS-1)),2)
ENDELSE

;================
;K-S
;================

;CHECK IF ILE EXIST
FILEO_DATA=DIR_SM+'/'+VAR+'_TXx_KS.dat'
print,'====================================='
print,'CHECKING ',fileo_data
print,'====================================='
count=""
count=FILE_SEARCH(FILEO_DATA)
IF COUNT NE "" THEN  GOTO,JUMP_KS

print,'KOLMOGOROV'
KS=FLTARR(NX,NY,3)
FOR ix=0,nx-1 DO BEGIN &$ ;
FOR iy=60,ny-2 DO BEGIN &$  ;EXLUDE ARTIC AND ANTARTIC
IF landmask(ix,iy) GT 0.5 THEN BEGIN
WORK=REFORM(WSDI_Y_T(ix,iy,*,0,ir))
WORK_1=REFORM(WSDI_Y_T(ix,iy,*,1,ir))
WORK_2=REFORM(WSDI_Y_T(ix,iy,*,2,ir))
NYEARS_KS=NYEARS
ks2,WORK,NYEARS_KS,WORK_1,NYEARS_KS,stat_v,prob_v,fn1,fn2,sor1,sor2
KS(ix,iy,0)=prob_v
ks2,WORK,NYEARS_KS,WORK_2,NYEARS_KS,stat_v,prob_v,fn1,fn2,sor1,sor2
KS(ix,iy,1)=prob_v
ks2,WORK_1,NYEARS_KS,WORK_2,NYEARS_KS,stat_v,prob_v,fn1,fn2,sor1,sor2
KS(ix,iy,2)=prob_v
ENDIF
ENDFOR 
ENDFOR 

print,'SAVING',FILEO_DATA
save,filename=FILEO_DATA,nx,ny,KS
JUMP_KS:

;RESTORE KS
IF COUNT NE "" THEN  restore,filename=FILEO_DATA
KS_EVAL(*,*,ir,*)=KS

ic=ic+1
ENDFOR ; END RCM



;================
;MULTI-YEAR MEAN,MED AND MAX
;================
print,'MEDIAN AND MODEL AGREEMENT'
WSDI_M_MED=FLTARR(nx,ny,NT)

FOR ix=0,nx-1 DO BEGIN &$
FOR iy=60,ny-1 DO BEGIN &$
IF landmask(ix,iy) GT 0.5 THEN BEGIN
FOR it=0,NT-1 DO BEGIN
FOR ir=0,NRUN -1 DO BEGIN
WSDI_M(ix,iy,it,ir)=MEAN(WSDI_Y_T(ix,iy,*,it,ir))
ENDFOR
WSDI_M_MED(ix,iy,it)=MEDIAN(WSDI_M(ix,iy,it,*))
ENDFOR
ENDIF
ENDFOR
ENDFOR

;MOD AGRRMENT KS
MOD_AGR_KS=FLTARR(NX,NY,3)
THRES=0.05
FOR ix=0,NX-1 DO BEGIN
FOR iy=60,NY-1 DO BEGIN
IF landmask(ix,iy) GT 0.5 THEN BEGIN  &$
FOR ir=0,NRUN -1 DO BEGIN &$
IF KS_EVAL(ix,iy,ir,0) LE THRES THEN MOD_AGR_KS(ix,iy,0)=MOD_AGR_KS(ix,iy,0)+1  &$
IF KS_EVAL(ix,iy,ir,1) LE THRES THEN MOD_AGR_KS(ix,iy,1)=MOD_AGR_KS(ix,iy,1)+1  &$
IF KS_EVAL(ix,iy,ir,2) LE THRES THEN MOD_AGR_KS(ix,iy,2)=MOD_AGR_KS(ix,iy,2)+1  &$
ENDFOR &$
ENDIF &$
ENDFOR &$
ENDFOR

;MAKE ROBUST

fileo='./DATA/ROBUSTNESS_'+STAT+'_SCEN.dat'
print,'====================================='
print,'CHECKING ',fileo
print,'====================================='
count=""
count=FILE_SEARCH(FILEO)

IF COUNT NE "" THEN BEGIN
restore,fileo 
GOTO,JUMP_ROB
ENDIF

print,'ROBUSTNESSS'

ROB=FLTARR(nx,ny,NT)

FOR ix=0,NX-1 DO BEGIN
FOR iy=60,NY-2 DO BEGIN
IF landmask(ix,iy) GE 0.5 THEN BEGIN
cdf_w=FLTARR(NBINS,NT,NRUN)
cdf_m=FLTARR(NBINS,NT) ;MEAN
cdf_t=FLTARR(NBINS,NT) ;MEAN
ww=FLTARR(NBINS)
am=FLTARR(NYEARS)
FOR it=0,2 DO BEGIN
FOR ir=0,NRUN -1 DO BEGIN
a=REFORM(WSDI_Y_T(ix,iy,*,it,ir)-273.15)
a(WHERE(a GT 1E3))=!VALUES.F_NaN
ww(*)=histogram(a,BINSIZE=BINSIZE,NBINS=NBINS,MIN=BINS(0),/NaN)
cdf_w(*,it,ir)=TOTAL(ww,/CUMULATIVE)/TOTAL(ww)
ENDFOR
FOR itt=0,NYEARS-1 DO BEGIN
am(itt)=MEAN(REFORM(WSDI_Y_T(ix,iy,itt,it,*)-273.15),/NaN) ;MODEL MEAN
ENDFOR
ww(*)=histogram(am,BINSIZE=BINSIZE,NBINS=NBINS,MIN=BINS(0),/NaN)
cdf_m(*,it)=TOTAL(ww,/CUMULATIVE)/TOTAL(ww)
at=REFORM(WSDI_Y_T(ix,iy,*,it,*)-273.15,NYEARS*NRUN) ;CDF MADE WITH TOTAL RUNS AND YEARS
at(WHERE(at GT 1E3))=!VALUES.F_NaN
ww(*)=histogram(at,BINSIZE=BINSIZE,NBINS=NBINS,MIN=BINS(0),/NaN)
cdf_t(*,it)=TOTAL(ww,/CUMULATIVE)/TOTAL(ww)
ENDFOR

A2=TOTAL(([0,cdf_m(*,1)]-[0,cdf_m(*,0)])^2.)
A1=TOTAL(([0,cdf_t(*,1)]-[0,cdf_m(*,1)])^2.)
;PRINT,A1,A2,1-A1/A2
ROB(ix,iy,0)=1-A1/A2

A2=TOTAL(([0,cdf_m(*,2)]-[0,cdf_m(*,0)])^2.)
A1=TOTAL(([0,cdf_t(*,2)]-[0,cdf_m(*,2)])^2.)
;PRINT,A1,A2,1-A1/A2
ROB(ix,iy,1)=1-A1/A2

A2=TOTAL(([0,cdf_m(*,2)]-[0,cdf_m(*,1)])^2.)
A1=TOTAL(([0,cdf_t(*,2)]-[0,cdf_m(*,2)])^2.)
;PRINT,A1,A2,1-A1/A2
ROB(ix,iy,2)=1-A1/A2

ENDIF
ENDFOR
ENDFOR

save,filename=fileo,nx,ny,ROB

JUMP_ROB:

;======================
;PLOT
;======================

loadct,39,RGB_TABLE=rgb
MINCOL=70.
MAXCOL=254

minvalue=0
maxvalue=3.5
ContourDist=0.5

rInterval=Maxvalue-Minvalue &$
iNumOfConts=FIX(rInterval/ContourDist)+1 &$
levels = INDGEN(iNumOfConts)*ContourDist + MinValue &$
colors = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol &$
;colors(N_ELEMENTS(levels)/2-1)=255
;colors(N_ELEMENTS(levels)/2)=255
;colors(0)=255

;STIPPELING
pattern_r = Obj_New('idlgrpattern', 1, ORIENTATION=-45,SPACING=4,THICK=0.8)
pattern_l = Obj_New('idlgrpattern', 1, ORIENTATION=45,SPACING=4,THICK=0.8)

w1=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[600,1000])
w1=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[600,800])
MARGIN=[0.02,0.22,0.02,0.1]

w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[1,2,1],MARGIN=MARGIN, $
        title='1.5$\circ$C vs. Reference',/CURRENT)
WSDI_PLOT=WSDI_M_MED(*,*,1)-WSDI_M_MED(*,*,0)
WSDI_PLOT(where(landmask LT 0.5))=!VALUES.F_NaN
c_plot= CONTOUR(WSDI_PLOT,lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
s_plot= CONTOUR(MOD_AGR_KS(*,*,0),lon,lat,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2)
s_plot= CONTOUR(ROB(*,*,0),lon,lat,c_value=[0.8,1],/OVERPLOT,C_FILL_PATTERN=pattern_l,/FILL,COLOR="black",GRID_UNITS=2)
mc = MAPCONTINENTS(THICK=0.5)
t=TEXT(0.1,1,'a',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)

w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[1,2,2],MARGIN=MARGIN, $
        title='2$\circ$C vs. Reference',/CURRENT)
WSDI_PLOT=WSDI_M_MED(*,*,2)-WSDI_M_MED(*,*,0)
WSDI_PLOT(where(landmask LT 0.5))=!VALUES.F_NaN
c_plot= CONTOUR(WSDI_PLOT,lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
s_plot= CONTOUR(MOD_AGR_KS(*,*,1),lon,lat,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2)
s_plot= CONTOUR(ROB(*,*,1),lon,lat,c_value=[0.8,1],/OVERPLOT,C_FILL_PATTERN=pattern_l,/FILL,COLOR="black",GRID_UNITS=2)

mc = MAPCONTINENTS(THICK=0.5)
t=TEXT(0.1,1,'b',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)

;w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
;        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
;        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[1,3,3],MARGIN=MARGIN, $
;        title='2$\circ$C - 1.5$\circ$C',/CURRENT)
;WSDI_PLOT=WSDI_M_MED(*,*,2)-WSDI_M_MED(*,*,1)
;WSDI_PLOT(where(landmask LT 0.5))=!VALUES.F_NaN
;c_plot= CONTOUR(WSDI_PLOT,lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2)
;s_plot= CONTOUR(MOD_AGR_KS(*,*,2),lon,lat,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2)
;s_plot= CONTOUR(ROB(*,*,2),lon,lat,c_value=[0.8,1],/OVERPLOT,C_FILL_PATTERN=pattern_l,/FILL,COLOR="black",GRID_UNITS=2)
;mc = MAPCONTINENTS(THICK=0.5)
;t=TEXT(0.1,1,'c',/RELATIVE,FONT_SIZE=14,TARGET=w_plot)

tickname = STRING(levels, FORMAT='(f5.1)') &$
cb0 = Colorbar(TARGET=c_plot,BORDER=1,TAPER=0,TICKNAME=tickname,position=[.05,.05,.45,.07],FONT_SIZE=8,TITLE='TXx change ($\circ$C)')

box=POLYGON([.5,.55,.55,.5],[0.05,.05,.07,.07],/NORMAL,/FILL_BACKGROUND,FILL_COLOR="black",PATTERN_ORIENTATION=-45,PATTERN_SPACING=4,PATTERN_THICK=0.8)
text2=text(.56,.05,'Change is significant',/NORMAL,FONT_SIZE=12,COLOR="black",TARGET=w_plot)
box=POLYGON([.5,.55,.55,.5],[0.02,.02,.04,.04],/NORMAL,/FILL_BACKGROUND,FILL_COLOR="black",PATTERN_ORIENTATION=45,PATTERN_SPACING=4,PATTERN_THICK=0.8)
text2=text(.56,.02,'Change is robust ',/NORMAL,FONT_SIZE=12,COLOR="black",TARGET=w_plot)

w1.Save,'./PLOTS/PLOT_'+STAT+'_MAPS_CHANGE.pdf',BITMAP=1,PAGE_SIZE="A4"


;PDF OF CHANGE OVER MASK

;======================
;AREA AVERAGES
;======================
window,3,retain=2
!p.multi=0

WE=WHERE(landmask EQ 0)

WSDI_YEAR_M=FLTARR(NMASK,NYEARS,NT,NRUN)
PDF_M=FLTARR(NMASK,NBINS_T,NT,NRUN)
PDF_M_MED=FLTARR(NMASK,NBINS_T,NT)

NPOINTS=FLTARR(NMASK)
FOR imm=0,NMASK-1 DO BEGIN &$ ;MASK
a=FLTARR(NYEARS,NT,NRUN) &$
print,'========================' &$
print,'MASK ',MASK(imm) &$
print,'========================' &$
WORK_LAND=landmask*0. &$
we_lon=WHERE(lon GE LON1(imm) AND lon LE LON2(imm)  AND lat GE LAT1(imm)  AND lat LE LAT2(imm) AND landmask GT 0.5,COMPLEMENT=WE_NOLAND ) &$
NPOINTS(imm)=N_ELEMENTS(WE_LON) &$
WORK_LAND=landmask*0.+1 &$
WORK_LAND(we_noland)=0. &$
contour,WORK_LAND,/fill,levels=[1],/overplot &$
FOR it=0,NT-1 DO BEGIN &$
WORK_W_R=FLTARR(NX,NY,NYEARS,NRUN) &$
FOR ir=0,NRUN-1 DO BEGIN &$
WORK_W=FLTARR(NX,NY,NYEARS) &$
FOR iyy=0,NYEARS-1 DO BEGIN &$  ;YEAR
WORK=REFORM(WSDI_Y_T(*,*,iyy,it,ir))-273.15 &$
;IF it EQ 0 THEN WORK=REFORM(WSDI_Y_T(*,*,iyy,it,ir)) $
;ELSE WORK=REFORM(WSDI_Y_T(*,*,iyy,it,ir)-WSDI_Y_T(*,*,iyy,0,ir)) &$
WORK(WE_NOLAND)=!VALUES.F_NaN &$
WORK(WHERE( WORK LE -999.))=!VALUES.F_NaN &$
WSDI_YEAR_M(imm,iyy,it,ir)=MEAN(WORK,/NaN) &$
WORK_W(*,*,iyy)=WORK &$
WORK_W_R(*,*,iyy,ir)=WORK &$
ENDFOR &$
;PDF
;PDF_M(imm,*,it,ir)=HISTOGRAM(REFORM(WORK_W),BINSIZE=BINSIZE_T,NBINS=NBINS_T,MIN=BINS_T(0),/NaN)/NPOINTS(imm)/NYEARS &$
IF it EQ 0 THEN PDF_M(imm,*,it,ir)=HISTOGRAM(REFORM(WSDI_YEAR_M(imm,*,it,ir)-MEAN(WSDI_YEAR_M(imm,*,it,ir),/NaN) ),BINSIZE=BINSIZE_T,NBINS=NBINS_T,MIN=BINS_T(0),/NaN)/DOUBLE(NRUN*NYEARS)  $
ELSE PDF_M(imm,*,it,ir)=HISTOGRAM(REFORM(WSDI_YEAR_M(imm,*,it,ir)-MEAN(WSDI_YEAR_M(imm,*,0,ir),/NaN)),BINSIZE=BINSIZE_T,NBINS=NBINS_T,MIN=BINS_T(0),/NaN)/DOUBLE(NRUN*NYEARS) &$

a(*,it,ir)=REFORM(WSDI_YEAR_M(imm,*,it,ir)-MEAN(WSDI_YEAR_M(imm,*,0,ir),/NaN)) &$
ENDFOR  &$;RUNS
b=REFORM(a(*,it,*),NYEARS*NRUN) &$
;PDF_M_MED(imm,*,it)=HISTOGRAM(REFORM(WORK_W_R),BINSIZE=BINSIZE_T,NBINS=NBINS_T,MIN=BINS_T(0),/NaN)/NPOINTS(imm)/NYEARS &$
;PDF_M_MED(imm,*,it)=HISTOGRAM(REFORM(WSDI_YEAR_M(imm,*,it,*)),BINSIZE=BINSIZE_T,NBINS=NBINS_T,MIN=BINS_T(0),/NaN)/NPOINTS(imm)/NYEARS &$
;IF it EQ 0 THEN PDF_M_MED(imm,*,it)=HISTOGRAM(REFORM(WSDI_YEAR_M(imm,*,it,*)-MEAN(WSDI_YEAR_M(imm,*,it,*),/NaN) ),BINSIZE=BINSIZE_T,NBINS=NBINS_T,MIN=BINS_T(0),/NaN)/DOUBLE(NRUN*NYEARS)  $
;ELSE PDF_M_MED(imm,*,it)=HISTOGRAM(REFORM(WSDI_YEAR_M(imm,*,it,*)-MEAN(WSDI_YEAR_M(imm,*,0,*),/NaN)),BINSIZE=BINSIZE_T,NBINS=NBINS_T,MIN=BINS_T(0),/NaN)/DOUBLE(NRUN*NYEARS) &$
PDF_M_MED(imm,*,it)=HISTOGRAM(b,BINSIZE=BINSIZE_T,NBINS=NBINS_T,MIN=BINS_T(0),/NaN)/DOUBLE(NRUN*NYEARS) &$
ENDFOR  &$;it

ENDFOR  ;MASK

;========================
;AREA FRACTION FOR HWMId
;========================

;=============
;PLOT PDFs
;=============
TICKV_X=[3,5,10,20]
TICKV_X=[3,6,12,24]
TICKV_X=[3,6,12,24,48]
TICKV_Y=[1,5,15,20,30]
TICKV_Y=[1,5,10,15,20,25]

XRANGE=[-4,6]
;XRANGE=[-10,10]
YRANGE=[1,25]
PANEL=['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
w3=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[900,1200])

FOR imm=0,NMASK-1 DO BEGIN &$
;p=PLOT(BINS_T-MEAN(WSDI_YEAR_M(imm,*,0,*)-273.15),PDF_M_MED(imm,*,0),/CURRENT,XRANGE=XRANGE,YRANGE=[0,0.2],color="black", $
p=PLOT(BINS_T,SMOOTH(PDF_M_MED(imm,*,0),2),/CURRENT,XRANGE=XRANGE,YRANGE=[0,0.2],color="black", $
XTITLE='TXx anomaly ($\circ$C',LAYOUT=[4,6,imm+1],MARGIN=[0.15,0.05,0.05,0.15],XMINOR=0,THICK=2, $
;XTICKVALUES=TICKV_X,YTICKVALUES=TICKV_Y,YMINOR=0,THICK=2, $
AXIS_STYLE=1,FONT_SIZE=6,YTITLE='Probability') &$
p=PLOT(BINS_T,SMOOTH(PDF_M_MED(imm,*,1),2),color="blue",/OVERPLOT,THICK=2)  &$
p=PLOT(BINS_T,SMOOTH(PDF_M_MED(imm,*,2),2),color="red",/OVERPLOT,THICK=2)  &$
MIN_R=FLTARR(NBINS_T,NT) &$
MAX_R=FLTARR(NBINS_T,NT) &$
FOR it=0,NT-1 DO BEGIN &$
FOR ib=0,NBINS_T-1 DO BEGIN &$
MIN_R(ib,it)=MIN(PDF_M(imm,ib,it,*)*NRUN) &$
MAX_R(ib,it)=MAX(PDF_M(imm,ib,it,*)*NRUN) &$
ENDFOR &$
ENDFOR &$
;poly_p=POLYGON([BINS_T,REVERSE(BINS_T)],[SMOOTH(MAX_R(*,0),3),REVERSE(SMOOTH(MIN_R(*,0),3))],/DATA,/FILL_BACKGROUND, COLOR="gray",FILL_COLOR="gray",FILL_TRANSPARENCY=80,TARGET=p) &$
;poly_p=POLYGON([BINS_T,REVERSE(BINS_T)],[SMOOTH(MAX_R(*,1),3),REVERSE(SMOOTH(MIN_R(*,1),3))],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p) &$
;poly_p=POLYGON([BINS_T,REVERSE(BINS_T)],[SMOOTH(MAX_R(*,2),3),REVERSE(SMOOTH(MIN_R(*,2),3))],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p) &$
;
t=TEXT(-3.5,0.19,PANEL(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$
t=TEXT(0,0.18,MASK(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$
t=TEXT(-3.5,0.14,STRMID(STRING(MEAN(WSDI_YEAR_M(imm,*,1,*),/NaN)-MEAN(WSDI_YEAR_M(imm,*,0,*),/NaN)),6,3),/DATA,FONT_SIZE=10,COLOR="blue",TARGET=p) &$
t=TEXT(-3.5,0.12,STRMID(STRING(MEAN(WSDI_YEAR_M(imm,*,2,*),/NaN))-MEAN(WSDI_YEAR_M(imm,*,0,*),/NaN),6,3),/DATA,FONT_SIZE=10,COLOR="red",TARGET=p) &$
ENDFOR
text1=TEXT(.53,.12,'___ 1976-2005 ',/NORMAL,FONT_SIZE=12,COLOR="black",TARGET=p)
text1=TEXT(.53,.08,'___ +1.5$\circ$C',/NORMAL,FONT_SIZE=12,COLOR="blue",TARGET=p)
text1=TEXT(.53,.04,'___+2$\circ$C',/NORMAL,FONT_SIZE=12,COLOR="red",TARGET=p)


w3.Save,'./PLOTS/PLOT_'+STAT+'_PDF_MASK_SCEN.pdf',BITMAP=1,PAGE_SIZE="A4"
STOP


text1=TEXT(.3,.12,'___ NCEP-2',/NORMAL,FONT_SIZE=12,COLOR="black",TARGET=p)
text1=TEXT(.3,.08,'___ ERA-Interim',/NORMAL,FONT_SIZE=12,COLOR="blue",TARGET=p)
text1=TEXT(.3,.04,'___ Models median ',/NORMAL,FONT_SIZE=12,COLOR="red",TARGET=p)
text2=POLYGON([.55,.6,.6,.55],[0.04,.04,.06,.06],/NORMAL,/FILL_BACKGROUND,FILL_COLOR="red",color="red",FILL_TRANSPARENCY=80,/OVERPLOT)
text2=text(.62,.04,'Models range',/NORMAL,FONT_SIZE=12,COLOR="red",TARGET=p)

;w3.Save,'./PLOTS/PLOT_'+STAT+'_PDF_MASK_REF.pdf',BITMAP=1,PAGE_SIZE="A4"
;w3.Save,'./PLOTS/PLOT_'+STAT+'_PDF_MASK_REF.eps',BITMAP=1,PAGE_SIZE="A4"

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



STOP
END
