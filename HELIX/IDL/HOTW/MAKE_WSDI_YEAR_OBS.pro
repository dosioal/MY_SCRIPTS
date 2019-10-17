PRO MAKE_WSDI_YEAR_OBS
DEVICE,decomposed=0
loadct,39
!p.background=255
!p.color=0

; MAKE WSDI - YEARLY
;=============================================
VAR='tmax'
;VAR='tasmin'

STAT='SM'
;STAT='WSDI' ; TOTAL N OF DAYS WITH SPELL LENGHT> THRES  DASY (= ETCCDI WSDI)
;STAT='WSDI_M' ;  MAX WARM SPEL LENGHT
STAT='HWMId'

THRES=3; N OF CONSEC DAYS FORA HEAT WAVE

OBS='NCEP-2'
DIR_HOME='/media/NAS/AFRICA-OBS/'

YEAR_INI=1979
YEAR_END=2010

;===============================================
NYEAR=FIX(YEAR_END)-FIX(YEAR_INI)+1

;===============================================
;LANDMASK AND DIMENSIONS
;===============================================
FILE_LAND='/media/NAS/LUC/SMHI/LR/landmask_05.dat'
restore,FILE_LAND
help,landmask
print,nx,ny

loadct,39
!p.background=255
!p.color=0
!p.font=1
!x.margin=4.5
!y.margin=4.5

;===============================================
; READ MODELS
;===============================================

;===============================================
;DEF OF VARIABLES
;===============================================
WSDI_Y=FLTARR(nx,ny,NYEAR)*0.-999.

;DIR MODELS

DIR_RUN=DIR_HOME+OBS+'/'+VAR+'/'
spawn,'mkdir '+DIR_RUN+'WORK'
spawn,'mkdir '+DIR_RUN+'YEAR'

print,''
print,'====================================='
print,'CALCULATING ',STAT,' FOR '
print,'YSTART ',YEAR_INI
print,'YEND ',YEAR_END
PRINT,'NYEAR=',NYEAR
print,'====================================='

DIR_WORK=DIR_RUN+'WORK'
spawn,'mkdir '+DIR_RUN+'PP/'
spawn,'mkdir '+DIR_RUN+'PP/'+STAT
DIR_SM=DIR_RUN+'PP/'+STAT

;===============================================
;CHECK IF IDL FILE WITH WSDI STAT EXISITS ALREADY
;===============================================
FILEO=DIR_SM+'/'+VAR+'_'+STRMID(YEAR_INI,4,4)+'-'+STRMID(YEAR_END,4,4)+'_'+STAT+'.dat'
print,'====================================='
print,'CHECKING ',fileo
print,'====================================='
FILE_EX=FINDFILE(fileo,count=count)

IF COUNT EQ 1 THEN BEGIN
print,'FILE EXISTS!'
print,''
restore,fileo
GOTO,JUMP10 
ENDIF
print,'FILE DOES NOT EXIST!'
print,''

;=============================================
;TXx
;=============================================
DIR_TXx=DIR_HOME+OBS+'/'+VAR+'/PP/TXx/'

add_offset = 447.65
scale_factor = 0.01

;OPEN TXx FILE
DATA_FILE=DIR_TXx+VAR+'_p25_TXx.nc'
print,'PERC FILE = ',DATA_FILE
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
nt2=dims_a(2)
print,nx,ny,nt2
NCDF_VARGET,fileID,VAR,WORK
help,WORK
VAR_TXx_25p=REFORM(WORK(*,*,0:nt2-1))*scale_factor+add_offset
ENDIF ELSE BEGIN
nx=dims_a(0)
ny=dims_a(1)
nt2=dims_a(3)
print,nx,ny,nt2
NCDF_VARGET,fileID,VAR,WORK
help,WORK
VAR_TXx_25p=REFORM(WORK(*,*,0,0:nt2-1))*scale_factor+add_offset
ENDELSE
NCDF_CLOSE, fileID
help,VAR_TXx_25p
VAR_TXx_25p=REVERSE(VAR_TXx_25p,2)

window,0,retain=2,xsize=800,ysize=800
!p.multi=[0,2,2]
contour,VAR_TXx_25p(*,*)-273.15,xstyle=1,ystyle=1,/fill,levels=FINDGEN(10)+34.,title='TXX_p25'
contour,landmask,/OVERPLOT,levels=[0,1],color=0

;OPEN TXx FILE
DATA_FILE=DIR_TXx+VAR+'_p75_TXx.nc'
print,'PERC FILE = ',DATA_FILE
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
nt2=dims_a(2)
print,nx,ny,nt2
NCDF_VARGET,fileID,VAR,WORK
help,WORK
VAR_TXx_75p=REFORM(WORK(*,*,0:nt2-1))*scale_factor+add_offset
ENDIF ELSE BEGIN
nx=dims_a(0)
ny=dims_a(1)
nt2=dims_a(3)
print,nx,ny,nt2
NCDF_VARGET,fileID,VAR,WORK
help,WORK
VAR_TXx_75p=REFORM(WORK(*,*,0,0:nt2-1))*scale_factor+add_offset
ENDELSE
NCDF_CLOSE, fileID
help,VAR_TXx_75p
VAR_TXx_75p=REVERSE(VAR_TXx_75p,2)

contour,VAR_TXx_75p(*,*)-273.15,xstyle=1,ystyle=1,/fill,levels=FINDGEN(10)+34.,title='TXX_p75'
contour,landmask,/OVERPLOT,levels=[0,1],color=0

contour,VAR_TXx_75p(*,*)-VAR_TXx_25p(*,*),xstyle=1,ystyle=1,/fill,levels=FINDGEN(21)*.5-5,title='TXX_p75-p25'
contour,VAR_TXx_75p(*,*)-VAR_TXx_25p(*,*),xstyle=1,ystyle=1,levels=[1],/OVERPLOT
contour,landmask,/OVERPLOT,levels=[0,1],color=0


IF STAT EQ 'SM' OR STAT EQ 'HWMId' THEN GOTO,JUMP_PERC
;==============================================
;MAKE ANOMALY FILE
;==============================================
;PERCENTILE
DIR_PERC=DIR_HOME+OBS+'/'+VAR+'/PP/PERC/'
FILE_PERC=VAR+'_p90_1980-2005.nc'  ;CLIMATOLOGICAL PERCENTILES
FILE_PERC_SHIFT=VAR+'_p90_1980-2005_shift.nc'  ;CLIMATOLOGICAL PERCENTILES
FILE_EX=FINDFILE(DIR_PERC+FILE_PERC_SHIFT,count=count)
IF count EQ 0 THEN spawn,'cdo shifttime,3months '+DIR_PERC+FILE_PERC+' '+DIR_PERC+FILE_PERC_SHIFT

;OPEN PERC FILE
DATA_FILE=DIR_PERC+FILE_PERC_SHIFT
print,'PERC FILE = ',DATA_FILE
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
nt2=dims_a(2)
print,nx,ny,nt2
NCDF_VARGET,fileID,VAR,WORK
help,WORK
VAR_90p=REFORM(WORK(*,*,0:nt2-1))*scale_factor+add_offset
ENDIF ELSE BEGIN
nx=dims_a(0)
ny=dims_a(1)
nt2=dims_a(3)
print,nx,ny,nt2
NCDF_VARGET,fileID,VAR,WORK
help,WORK
VAR_90p=REFORM(WORK(*,*,0,0:nt2-1))*scale_factor+add_offset
ENDELSE
help,VAR_90p
NCDF_CLOSE, fileID

VAR_90p=REVERSE(VAR_90p,2)

contour,VAR_90p(*,*,0)-273.15,xstyle=1,ystyle=1,/fill,levels=FINDGEN(10)+34.,title='Tx_p90'
contour,landmask,/OVERPLOT,levels=[0,1],color=0

JUMP_PERC:
;==================================
;PREPARE DATA
;==================================

;SHIFT TIME
IF STAT EQ 'SM' OR STAT EQ 'HWMID' THEN  BEGIN 
FILE_RUN=VAR+'_'+STRMID(YEAR_INI,4)+'_2010_0.5.nc'  ;CLIMATOLOGICAL 
FILE_SHIFT=VAR+'_'+STRMID(YEAR_INI,4)+'_2010_0.5_shift.nc'  
FILE_EX=FINDFILE(DIR_RUN+FILE_SHIFT,count=count)
IF count EQ 0 THEN spawn,'cdo  shifttime,3months '+DIR_RUN+FILE_RUN+' '+DIR_RUN+FILE_SHIFT
;SPLITTING YEAR (DO IT ONLY ONCE!!!)
FILE_EX=FINDFILE(DIR_RUN+'/YEAR/test_'+STRMID(YEAR_END,4)+'.nc',count=count)
IF count EQ 0 THEN BEGIN
print,'YEAR FILES NOT FOUHD'
spawn,'cdo splityear '+DIR_RUN+FILE_SHIFT+' '+DIR_WORK+'/test_'
spawn,'mv '+ DIR_WORK+'/test_*.nc '+DIR_RUN+'YEAR/'
ENDIF
ENDIF ELSE BEGIN ;CLIMATOLOGICAL ANOMALY
FILE_RUN=VAR+'_'+STRMID(YEAR_INI,4)+'_2010_0.5.nc'  
FILE_AN=VAR+'_'+STRMID(YEAR_INI,4)+'_2010_0.5_an.nc'  ;CLIMATOLOGICAL ANOMALY
FILE_EX=FINDFILE(DIR_RUN+FILE_AN,count=count)
IF count EQ 0 THEN spawn,'cdo -b 32 sub '+DIR_RUN+FILE_RUN+' '+DIR_PERC+FILE_PERC+' '+DIR_RUN+FILE_AN
FILE_SHIFT=VAR+'_'+STRMID(YEAR_INI,4)+'_2010_0.5_an_shift.nc'  ; CLIMATOLOGICAL  ANOMALY SHIFTED
FILE_EX=FINDFILE(DIR_RUN+FILE_SHIFT,count=count)
IF count EQ 0 THEN spawn,'cdo  shifttime,3months '+DIR_RUN+FILE_AN+' '+DIR_RUN+FILE_SHIFT
;SPLITTING YEAR (DO IT ONLY ONCE!!!)
FILE_EX=FINDFILE(DIR_RUN+'/YEAR/test_'+STRMID(YEAR_END,4)+'.nc',count=count)
IF count EQ 0 THEN BEGIN
print,'YEAR FILES NOT FOUHD'
spawn,'cdo splityear '+DIR_RUN+FILE_SHIFT+' '+DIR_WORK+'/test_an_'
spawn,'mv '+ DIR_WORK+'/test_an_*.nc '+DIR_RUN+'YEAR/'
ENDIF
ENDELSE

YI=FIX(STRMID(YEAR_INI,4))+1  ;WHEN SHIFTED STARTS FROM YI+1

window,1,retain=2,xsize=1600,ysize=1000
!p.multi=[0,7,5]

;================================
;YEAR LOOP
;================================

YI=2003
WHILE YI LT 2004 DO BEGIN
;WHILE YI LT YEAR_END+1 DO BEGIN

;OPEN FILE
IF STAT EQ 'SM' OR STAT EQ 'HWMId' THEN DATA_FILE=DIR_RUN+'YEAR/test_'+STRMID(YI,4)+'.nc' ELSE DATA_FILE=DIR_RUN+'YEAR/test_an_'+STRMID(YI,4)+'.nc'
print,'MODEL FILE = ',DATA_FILE
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
IF ndims EQ 3 THEN BEGIN &$
nx=dims_a(0) &$
ny=dims_a(1) &$
nt=dims_a(2) &$
;print,nx,ny,nt
NCDF_VARGET,fileID,VAR,WORK &$
help,WORK &$
IF STAT EQ 'SM' OR STAT EQ 'HWMId' THEN VAR_SM=REFORM(WORK(*,*,0:nt-1))*scale_factor+add_offset ELSE  VAR_SM=REFORM(WORK(*,*,0:nt-1)) &$
ENDIF ELSE BEGIN &$
nx=dims_a(0) &$
ny=dims_a(1) &$
nt=dims_a(3) &$
;print,nx,ny,nt
NCDF_VARGET,fileID,VAR,WORK &$
help,WORK &$
IF STAT EQ 'SM' OR STAT EQ 'HWMId' THEN VAR_SM=REFORM(WORK(*,*,0,0:nt-1))*scale_factor+add_offset ELSE  VAR_SM=REFORM(WORK(*,*,0,0:nt-1)) &$
ENDELSE &$
help,VAR_SM
NCDF_CLOSE, fileID

VAR_SM=REVERSE(VAR_SM,2)

iyyy=YI-FIX(YEAR_INI)
PRINT,'YEAR=',YI,YI-FIX(YEAR_INI),iyyy
IF nt EQ 366 THEN nt=365

WORK_WSDI=FLTARR(nx,ny,nt)*0.

;contour,VAR_SM(*,*,186+90),xstyle=1,ystyle=1,/fill,levels=FINDGEN(21)*.4-4,title='Tx an 1st July'+STRMID(YI,4)
;contour,landmask,/OVERPLOT,levels=[0,1],color=0

;======================================
;CALC STAT
;======================================
;FOR iy=0,ny-1 DO BEGIN &$
;FOR ix=0,nx-1 DO BEGIN &$
FOR ix=376,376 DO BEGIN
FOR iy=262,262 DO BEGIN



IF  LANDMASK(ix,iy) GT 0.5 THEN BEGIN &$

IF STAT EQ 'WSDI_M' OR STAT EQ 'WSDI' THEN BEGIN
WORK_WSDI=FLTARR(nt)*0.
IF REFORM(VAR_SM(ix,iy,0)) GT 0. THEN WORK_WSDI(0)=1 ELSE WORK_WSDI(0)=0 &$
FOR it=1,nt-1 DO BEGIN &$
IF REFORM(VAR_SM(ix,iy,it)) GT 0. THEN $
WORK_WSDI(it)=WORK_WSDI(it-1)+1 ELSE WORK_WSDI(it)=0. &$
ENDFOR &$
ENDIF

CASE STAT OF &$
;==================
'SM': BEGIN &$
;==================
WSDI_Y(ix,iy,iyyy)=MEAN(VAR_SM(ix,iy,*),/NaN)  &$;SEASONAL MEAN FROM ANOMALY TO T &$
END &$
;==================
'WSDI_M': BEGIN &$
;==================
WSDI_Y(ix,iy,iyyy)=MAX(WORK_WSDI)  &$;MAX WARM SPEL LENGHT IN A YEAR
END &$
;==================
'WSDI': BEGIN &$
;==================
WORK_2=WORK_WSDI*0. &$
FOR it=0,nt-2 DO BEGIN &$
IF WORK_WSDI(it) GE THRES AND WORK_WSDI(it+1) EQ 0. THEN BEGIN &$
WORK_2(it)=WORK_WSDI(it) &$
ENDIF &$
ENDFOR &$
IF WORK_WSDI(nt-1) GE THRES THEN WORK_2(nt-1)=WORK_WSDI(nt-1) &$
we_c=WHERE(WORK_2 GT 0,count) &$
N_HW=count &$
IF ix eq 100 and iy eq 100 THEN print,N_HW &$
WSDI_Y(ix,iy,iyyy)=TOTAL(WORK_2)  &$;TOTAL DAYS WITH SPELL LENGHT MORE THAN 3 DAYS
END &$
;==================
'HWMId': BEGIN &$
;==================
a=REFORM((VAR_SM(ix,iy,*)-VAR_TXx_25p(ix,iy))/(VAR_TXx_75p(ix,iy)-VAR_TXx_25p(ix,iy)))
a(where(a LT 0))=0.
WORK=FLTARR(NT)
WORK_HWMI=FLTARR(NT)
IF a(0)  GT 0. THEN WORK(0)=1 ELSE WORK(0)=0 &$
FOR it=1,nt-1 DO BEGIN &$
IF a(it) GT 0. THEN $
WORK(it)=WORK(it-1)+1 ELSE WORK(it)=0. &$
ENDFOR 
FOR it=0,nt-2 DO BEGIN &$
IF WORK(it) GE THRES AND WORK(it+1) EQ 0. THEN BEGIN &$
WORK_HWMI(it)=TOTAL(a(it-WORK(it)+1:it)) &$
ENDIF &$
ENDFOR
WSDI_Y(ix,iy,iyyy)=MAX(WORK_HWMI) &$

END &$
ENDCASE &$

ENDIF  &$;LAND

STOP

ENDFOR  &$;ix
ENDFOR ;iy

;WINDOW,0+isc,retain=2,title=RUN(ir)+'-'+SCEN
;!p.multi=[0,2,2]
;PLOT,VAR_SM(100,100,*),yrange=[280,320];,xrange=[200,259]
;;OPLOT,SMOOTH(VAR_PERC_90(100,100,*),15,/EDGE_TRUNCATE),color=250
;a=my_cgPercentiles(REFORM(VAR_TXx(100,100,*)),Percentiles=[0.25])
;PLOTS,[0,360],[a,a],color=250
;a=my_cgPercentiles(REFORM(VAR_TXx(100,100,*)),Percentiles=[0.75])
;PLOTS,[0,360],[a,a],color=250
;PLOT,WORK_WSDI(100,100,*);,xrange=[200,259]
;PLOTS,[0,360],[3,3]
;PLOT,VAR_SM(50,150,*),yrange=[280,320];,xrange=[200,259]
;OPLOT,SMOOTH(VAR_PERC_90(50,150,*),15,/EDGE_TRUNCATE),color=250
;a=my_cgPercentiles(REFORM(VAR_TXx(50,150,*)),Percentiles=[0.25])
;PLOTS,[0,360],[a,a],color=250
;a=my_cgPercentiles(REFORM(VAR_TXx(50,150,*)),Percentiles=[0.75])
;PLOTS,[0,360],[a,a],color=250
;PLOT,WORK_WSDI(50,150,*);,xrange=[200,259]
;PLOTS,[0,360],[3,3]

IF STAT EQ 'HWMId' THEN levels=[0,3,6,9,15,24,36,48] ELSE levels=[0,60,120,180,240,300,366] &$
IF STAT EQ 'SM' THEN levels=FINDGEN(7)*8+250  &$
IF STAT EQ 'WSDI_M' THEN levels=[0,60,120,180,240,300,366]/12  &$
colors=[80,100,150,190,210,250]
colors=[255,100,150,190,210,250]
IF STAT EQ 'HWMId' THEN lev_c=STRCOMPRESS(STRMID(levels,4,5)) ELSE lev_c=STRCOMPRESS(STRMID(levels,4,4))

CONTOUR,landmask,xstyle=1,ystyle=1,title=STAT+' '+STRMID(YEAR_INI+iyyy,4),charsize=2 &$;,xrange=[300,500],yrange=[230,350] &$
CONTOUR,WSDI_Y(*,*,iyyy),level=levels,/fill,/OVERPLOT,c_colors=colors &$
CONTOUR,landmask,/overplot &$


YI=YI+1

ENDWHILE ;YEAR

;spawn,'rm -f '+DIR_WORK+'/*'

;=======================================
;SAVING
;=======================================
print,'====================================='
print,'SAVING ',fileo
print,'====================================='
print,''
save,filename=fileo,nx,ny,WSDI_Y

JUMP10:

;GOTO,JUMP_PLOT

IF STAT EQ 'HWMId' THEN levels=[0,3,6,9,15,24,36,48] ELSE levels=[0,60,120,180,240,300,366] &$
IF STAT EQ 'SM' THEN levels=FINDGEN(7)*8+250  &$
IF STAT EQ 'WSDI_M' THEN levels=[0,60,120,180,240,300,366]/12  &$
colors=[80,100,150,190,210,250]
colors=[255,100,150,190,210,250]
IF STAT EQ 'HWMId' THEN lev_c=STRCOMPRESS(STRMID(levels,4,5)) ELSE lev_c=STRCOMPRESS(STRMID(levels,4,4))

WINDOW,4,RETAIN=2,title=OBS,xsize=1800,ysize=1000
!p.multi=[0,7,5]
FOR iy=0,NYEAR-1 DO BEGIN &$
CONTOUR,landmask,xstyle=1,ystyle=1,title=STAT+' '+STRMID(YEAR_INI+iy,4),charsize=2,xrange=[300,500],yrange=[230,350] &$
CONTOUR,WSDI_Y(*,*,iy),levels=levels,/fill,/OVERPLOT,c_color=colors &$
CONTOUR,landmask,/overplot &$
ENDFOR

CONTOUR,landmask,xstyle=1,ystyle=1,title=STAT+' '+STRMID(YEAR_INI+24,4)
CONTOUR,WSDI_Y(*,*,24),levels=levels,/fill,/OVERPLOT,c_colors=colors
CONTOUR,landmask,/overplot

CONTOUR,landmask,xstyle=1,ystyle=1,title=STAT+' '+STRMID(YEAR_INI+28,4)
CONTOUR,WSDI_Y(*,*,28),levels=levels,/fill,/OVERPLOT,c_colors=colors
CONTOUR,landmask,/overplot

CONTOUR,landmask,xstyle=1,ystyle=1,title=STAT+' '+STRMID(YEAR_INI+31,4)
CONTOUR,WSDI_Y(*,*,31),levels=levels,/fill,/OVERPLOT,c_colors=colors
CONTOUR,landmask,/overplot

time=FIX(FINDGEN(NYEAR)+YEAR_INI)
WINDOW,8,RETAIN=2,title=OBS
!p.multi=[0,2,2]
PLOT,time,WSDI_Y(150,260,*),xstyle=1,ystyle=1,title=STAT+' North America',YRANGE=[0,MAX(WSDI_Y(150,260,*))*1.1]
PLOT,time,WSDI_Y(380,280,*),xstyle=1,ystyle=1,title=STAT+' Central EU ',YRANGE=[0,MAX(WSDI_Y(390,280,*))*1.1]
PLOT,time,WSDI_Y(400,130,*),xstyle=1,ystyle=1,title=STAT+' South Africa',YRANGE=[0,MAX(WSDI_Y(400,130,*))*1.1]
PLOT,time,WSDI_Y(600,120,*),xstyle=1,ystyle=1,title=STAT+' Australia',YRANGE=[0,MAX(WSDI_Y(600,120,*))*1.1]



JUMP_PLOT:





STOP


;MEDIAN AND INTERQUANTILE
MAX_HWMI_SCN_MED=FLTARR(nx,ny,NSCEN)*!VALUES.F_NaN
p50_HWMI_SCN_MED=FLTARR(nx,ny,NSCEN)*!VALUES.F_NaN
MAX_HWMI_SCN_25p=FLTARR(nx,ny,NSCEN)*!VALUES.F_NaN
p50_HWMI_SCN_25p=FLTARR(nx,ny,NSCEN)*!VALUES.F_NaN
MAX_HWMI_SCN_75p=FLTARR(nx,ny,NSCEN)*!VALUES.F_NaN
p50_HWMI_SCN_75p=FLTARR(nx,ny,NSCEN)*!VALUES.F_NaN

N_NGCM=9
FOR isc=0,NSCEN-1 DO BEGIN
FOR iy=0,ny-1 DO BEGIN
FOR ix=0,nx-1 DO BEGIN
a=REFORM(REFORM(MAX_HWMI_SCN(ix,iy,*,*,isc),N_NGCM*NRUN))
b=REFORM(a(WHERE(a LT 1e10)))
MAX_HWMI_SCN_MED(ix,iy,isc)=MEDIAN(b)
MAX_HWMI_SCN_25p(ix,iy,isc)=my_cgPercentiles(b, Percentiles=[0.25])
MAX_HWMI_SCN_75p(ix,iy,isc)=my_cgPercentiles(b, Percentiles=[0.75])

a=REFORM(REFORM(p50_HWMI_SCN(ix,iy,*,*,isc),N_NGCM*NRUN))
b=REFORM(a(WHERE(a LT 1e10)))
p50_HWMI_SCN_MED(ix,iy,isc)=MEDIAN(b)
p50_HWMI_SCN_25p(ix,iy,isc)=my_cgPercentiles(b, Percentiles=[0.25])
p50_HWMI_SCN_75p(ix,iy,isc)=my_cgPercentiles(b, Percentiles=[0.75])
ENDFOR
ENDFOR

we=WHERE(landmask LE .5)
wrk=REFORM(p50_HWMI_SCN_MED(*,*,isc))
wrk(we)=-999
p50_HWMI_SCN_MED(*,*,isc)=wrk
wrk=REFORM(p50_HWMI_SCN_25p(*,*,isc))
wrk(we)=-999
p50_HWMI_SCN_25p(*,*,isc)=wrk
wrk=REFORM(p50_HWMI_SCN_75p(*,*,isc))
wrk(we)=-999
p50_HWMI_SCN_75p(*,*,isc)=wrk

wrk=REFORM(MAX_HWMI_SCN_MED(*,*,isc))
wrk(we)=-999
MAX_HWMI_SCN_MED(*,*,isc)=wrk
wrk=REFORM(MAX_HWMI_SCN_25p(*,*,isc))
wrk(we)=-999
MAX_HWMI_SCN_25p(*,*,isc)=wrk
wrk=REFORM(MAX_HWMI_SCN_75p(*,*,isc))
wrk(we)=-999
MAX_HWMI_SCN_75p(*,*,isc)=wrk

ENDFOR

WINDOW,0,RETAIN=2,title='RCM MEDIAN'
!p.multi=[0,2,2]
IF SCEN NE 'historical' THEN BEGIN
IF STAT EQ 'HWMId' THEN levels=[0,12,24,48,96,192,384]*2. ELSE levels=[0,60,120,180,240,300,366]
ENDIF ELSE BEGIN
IF STAT EQ 'HWMId' THEN levels=[0,3,6,9,15,24,36,48] ELSE levels=[0,60,120,180,240,300,366]/3.
ENDELSE
colors=[80,100,150,190,210,250]
IF STAT EQ 'HWMId' THEN lev_c=STRCOMPRESS(STRMID(levels,4,5)) ELSE lev_c=STRCOMPRESS(STRMID(levels,4,4))
CONTOUR,landmask,xstyle=1,ystyle=1,title='MED '+SCEN(0)
CONTOUR,p50_HWMI_SCN_MED(*,*,0),levels=levels,/fill,/OVERPLOT,c_colors=colors
CONTOUR,landmask,/overplot
CONTOUR,landmask,xstyle=1,ystyle=1,title='MAX '+SCEN(0)
CONTOUR,MAX_HWMI_SCN_MED(*,*,0),levels=levels,/fill,/OVERPLOT,c_colors=colors
CONTOUR,landmask,/overplot
IF NSCEN GT 1 THEN BEGIN
CONTOUR,landmask,xstyle=1,ystyle=1,title='MED '+SCEN(1)
CONTOUR,p50_HWMI_SCN(*,*,1),levels=levels,/fill,/OVERPLOT,c_colors=colors
CONTOUR,landmask,/overplot
CONTOUR,landmask,xstyle=1,ystyle=1,title='MAX '+SCEN(1)
CONTOUR,MAX_HWMI_SCN(*,*,1),levels=levels,/fill,/OVERPLOT,c_colors=colors
CONTOUR,landmask,/overplot
ENDIF

STOP

;PLOT
lon1=-24.94
lat1=-45.76

lon=lon1+FINDGEN(nx)*0.44
lat=lat1+FINDGEN(ny)*0.44

w = WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[600,1000])
loadct,39,RGB_TABLE=rgb
m = MAP('Geographic', LIMIT=[-45,-24,42,60],/CURRENT,LAYOUT=[3,5,1],MARGIN=0.08)
contour,p50_HWMI_SCN_25p(*,*,0),levels=levels,/fill,/OVERPLOT,c_colors=colors
c_ref = CONTOUR(p50_HWMI_SCN_25p(*,*,0),lon,lat,c_value=levels,RGB_INDICES=colors, /FILL,overplot=m,RGB_TABLE=39, $
       title=STAT+' MEDIAN rcp45 25p',FONT_SIZE=8,FONT_STYLE=0) &$
mc = MAPCONTINENTS(/countries,THICK=0.5)
grid = m.MAPGRID
grid.LINESTYLE = 6
grid.LABEL_POSITION = 0
grid.FONT_SIZE =3

m = MAP('Geographic', LIMIT=[-45,-24,42,60],/CURRENT,LAYOUT=[3,5,2],MARGIN=0.08)
contour,p50_HWMI_SCN_MED(*,*,0),levels=levels,/fill,/OVERPLOT,c_colors=colors
c_ref = CONTOUR(p50_HWMI_SCN_MED(*,*,0),lon,lat,c_value=levels,RGB_INDICES=colors, /FILL,overplot=m,RGB_TABLE=39, $
        title=STAT+' MEDIAN rcp45 50p',FONT_SIZE=8,FONT_STYLE=0) &$
mc = MAPCONTINENTS(/countries,THICK=0.5)
grid = m.MAPGRID
grid.LINESTYLE = 6
grid.LABEL_POSITION = 0
grid.FONT_SIZE =3

m = MAP('Geographic', LIMIT=[-45,-24,42,60],/CURRENT,LAYOUT=[3,5,3],MARGIN=0.08)
contour,p50_HWMI_SCN_75p(*,*,0),levels=levels,/fill,/OVERPLOT,c_colors=colors
c_ref = CONTOUR(p50_HWMI_SCN_75p(*,*,0),lon,lat,c_value=levels,RGB_INDICES=colors, /FILL,overplot=m,RGB_TABLE=39, $
        title=STAT+' MEDIAN rcp45 75p',FONT_SIZE=8,FONT_STYLE=0) &$
	mc = MAPCONTINENTS(/countries,THICK=0.5)
grid = m.MAPGRID
grid.LINESTYLE = 6
grid.LABEL_POSITION = 0
grid.FONT_SIZE =3

m = MAP('Geographic', LIMIT=[-45,-24,42,60],/CURRENT,LAYOUT=[3,5,4],MARGIN=0.08)
contour,p50_HWMI_SCN_25p(*,*,1),levels=levels,/fill,/OVERPLOT,c_colors=colors
c_ref = CONTOUR(p50_HWMI_SCN_25p(*,*,1),lon,lat,c_value=levels,RGB_INDICES=colors, /FILL,overplot=m,RGB_TABLE=39, $
        title=STAT+' MEDIAN rcp85 25p',FONT_SIZE=8,FONT_STYLE=0) &$
	mc = MAPCONTINENTS(/countries,THICK=0.5)
grid = m.MAPGRID
grid.LINESTYLE = 6
grid.LABEL_POSITION = 0
grid.FONT_SIZE =3

m = MAP('Geographic', LIMIT=[-45,-24,42,60],/CURRENT,LAYOUT=[3,5,5],MARGIN=0.08)
contour,p50_HWMI_SCN_MED(*,*,1),levels=levels,/fill,/OVERPLOT,c_colors=colors
c_ref = CONTOUR(p50_HWMI_SCN_MED(*,*,1),lon,lat,c_value=levels,RGB_INDICES=colors, /FILL,overplot=m,RGB_TABLE=39, $
        title=STAT+' MEDIAN rcp85 50p',FONT_SIZE=8,FONT_STYLE=0) &$
	mc = MAPCONTINENTS(/countries,THICK=0.5)
grid = m.MAPGRID
grid.LINESTYLE = 6
grid.LABEL_POSITION = 0
grid.FONT_SIZE =3

m = MAP('Geographic', LIMIT=[-45,-24,42,60],/CURRENT,LAYOUT=[3,5,6],MARGIN=0.08)
contour,p50_HWMI_SCN_75p(*,*,1),levels=levels,/fill,/OVERPLOT,c_colors=colors
c_ref = CONTOUR(p50_HWMI_SCN_75p(*,*,1),lon,lat,c_value=levels,RGB_INDICES=colors, /FILL,overplot=m,RGB_TABLE=39, $
        title=STAT+' MEDIAN rcp85 75p',FONT_SIZE=8,FONT_STYLE=0) &$
	mc = MAPCONTINENTS(/countries,THICK=0.5)
grid = m.MAPGRID
grid.LINESTYLE = 6
grid.LABEL_POSITION = 0
grid.FONT_SIZE =3

m = MAP('Geographic', LIMIT=[-45,-24,42,60],/CURRENT,LAYOUT=[3,5,7],MARGIN=0.08)
contour,MAX_HWMI_SCN_25p(*,*,0),levels=levels,/fill,/OVERPLOT,c_colors=colors
c_ref = CONTOUR(MAX_HWMI_SCN_25p(*,*,0),lon,lat,c_value=levels,RGB_INDICES=colors, /FILL,overplot=m,RGB_TABLE=39, $
        title=STAT+' MAX rcp45 25p',FONT_SIZE=8,FONT_STYLE=0) &$
	mc = MAPCONTINENTS(/countries,THICK=0.5)
grid = m.MAPGRID
grid.LINESTYLE = 6
grid.LABEL_POSITION = 0
grid.FONT_SIZE =3

m = MAP('Geographic', LIMIT=[-45,-24,42,60],/CURRENT,LAYOUT=[3,5,8],MARGIN=0.08)
contour,MAX_HWMI_SCN_MED(*,*,0),levels=levels,/fill,/OVERPLOT,c_colors=colors
c_ref = CONTOUR(MAX_HWMI_SCN_MED(*,*,0),lon,lat,c_value=levels,RGB_INDICES=colors, /FILL,overplot=m,RGB_TABLE=39, $
        title=STAT+' MAX rcp45 50p',FONT_SIZE=8,FONT_STYLE=0) &$
	mc = MAPCONTINENTS(/countries,THICK=0.5)
grid = m.MAPGRID
grid.LINESTYLE = 6
grid.LABEL_POSITION = 0
grid.FONT_SIZE =3

m = MAP('Geographic', LIMIT=[-45,-24,42,60],/CURRENT,LAYOUT=[3,5,9],MARGIN=0.08)
contour,MAX_HWMI_SCN_75p(*,*,0),levels=levels,/fill,/OVERPLOT,c_colors=colors
c_ref = CONTOUR(MAX_HWMI_SCN_75p(*,*,0),lon,lat,c_value=levels,RGB_INDICES=colors, /FILL,overplot=m,RGB_TABLE=39, $
        title=STAT+' MAX rcp45 75p',FONT_SIZE=8,FONT_STYLE=0) &$
	mc = MAPCONTINENTS(/countries,THICK=0.5)
grid = m.MAPGRID
grid.LINESTYLE = 6
grid.LABEL_POSITION = 0
grid.FONT_SIZE =3

m = MAP('Geographic', LIMIT=[-45,-24,42,60],/CURRENT,LAYOUT=[3,5,10],MARGIN=0.08)
contour,MAX_HWMI_SCN_25p(*,*,1),levels=levels,/fill,/OVERPLOT,c_colors=colors
c_ref = CONTOUR(MAX_HWMI_SCN_25p(*,*,1),lon,lat,c_value=levels,RGB_INDICES=colors, /FILL,overplot=m,RGB_TABLE=39, $
        title=STAT+' MAX rcp85 25p',FONT_SIZE=8,FONT_STYLE=0) &$
	mc = MAPCONTINENTS(/countries,THICK=0.5)
grid = m.MAPGRID
grid.LINESTYLE = 6
grid.LABEL_POSITION = 0
grid.FONT_SIZE =3

m = MAP('Geographic', LIMIT=[-45,-24,42,60],/CURRENT,LAYOUT=[3,5,11],MARGIN=0.08)
contour,MAX_HWMI_SCN_MED(*,*,1),levels=levels,/fill,/OVERPLOT,c_colors=colors
c_ref = CONTOUR(MAX_HWMI_SCN_MED(*,*,1),lon,lat,c_value=levels,RGB_INDICES=colors, /FILL,overplot=m,RGB_TABLE=39, $
        title=STAT+' MAX rcp85 50p',FONT_SIZE=8,FONT_STYLE=0) &$
	mc = MAPCONTINENTS(/countries,THICK=0.5)
grid = m.MAPGRID
grid.LINESTYLE = 6
grid.LABEL_POSITION = 0
grid.FONT_SIZE =3

m = MAP('Geographic', LIMIT=[-45,-24,42,60],/CURRENT,LAYOUT=[3,5,12],MARGIN=0.08)
contour,MAX_HWMI_SCN_75p(*,*,1),levels=levels,/fill,/OVERPLOT,c_colors=colors
c_ref = CONTOUR(MAX_HWMI_SCN_75p(*,*,1),lon,lat,c_value=levels,RGB_INDICES=colors, /FILL,overplot=m,RGB_TABLE=39, $
     title=STAT+' MAX rcp85 75p',FONT_SIZE=8,FONT_STYLE=0) &$
     mc = MAPCONTINENTS(/countries,THICK=0.5)
grid = m.MAPGRID
grid.LINESTYLE = 6
grid.LABEL_POSITION = 0
grid.FONT_SIZE =3

rgb_pr=rgb[colors,*]
IF STAT EQ 'HWMId' THEN lev_p=STRMID(levels,4,5) ELSE lev_p=STRMID(levels,4,4)
cb=COLORBAR(TICKNAME=lev_p,position=[0.35,0.14,0.95,0.16],/BORDER,rgb_table=rgb_pr)
cb.color=0
cb.thick=0.8

t1=TEXT(0.1,0.145,STAT,color=0,/NORMAL,TARGET=w)
w.Save,'./PLOTS/MAPS/'+STAT+'_ENS_MEAN.eps'



STOP
END
