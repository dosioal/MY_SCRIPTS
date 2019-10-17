PRO PLOT_WSDI_POP_OBS
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

YEAR_INI=1980
YEAR_END=2010
NRUN=2
;===============================================
YEAR_INI_T=MAX([YEAR_INI,YEAR_INI_OBS_1])
YEAR_END_T=MAX([YEAR_END,YEAR_END_OBS_1])
NYEAR_T=FIX(YEAR_END_T)-FIX(YEAR_INI_T)+1
print,YEAR_INI_T,YEAR_END_T,NYEAR_T

;================================================
BINSIZE=3
NBINS=35
BINS=FINDGEN(NBINS)*BINSIZE

;================================================
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

;===============================================
;LANDMASK AND DIMENSIONS
;===============================================
DIR_HOME='/media/NAS/LUC/SMHI/LR/'
FILE_LAND='landmask_05.dat'
restore,DIR_HOME+FILE_LAND
help,landmask
print,nx,ny

;GOTO,JUMP_LAND
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
JUMP_LAND:


FILE_POP='population_05_SSP1_int.dat'
restore,DIR_HOME+FILE_POP
nt0_int=1980
help,population_int
print,nx,ny,nt_int

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


FILEO_POP='./DATA/POP_HWMId_MASK_OBS.dat'
print,'====================================='
print,'CHECKING ',fileo_pop
print,'====================================='
count=""
count=FILE_SEARCH(FILEO_POP)

;IF COUNT EQ 1 THEN BEGIN
IF COUNT NE "" THEN BEGIN
print,'FILE EXISTS!'
print,''
;GOTO,JUMP_CALC
ENDIF


;===============================================
;DEF OF VARIABLES
;===============================================
WSDI_Y_O=FLTARR(nx,ny,NYEAR_T,NRUN)*0.-999.

;==========
;NCEP
;==========
DIR_SM=DIR_OBS_1+'/PP/'+STAT
FILEO=DIR_SM+'/'+VAR_OBS_1+'_'+STRMID(YEAR_INI_OBS_1,4,4)+'-'+STRMID(YEAR_END_OBS_1,4,4)+'_'+STAT+'.dat'
;===============================================
;CHECK IF IDL FILE WITH WSDI STAT EXISITS ALREADY
;===============================================
print,'====================================='
print,'CHECKING ',fileo
print,'====================================='
;FILE_EX=FINDFILE(fileo,count=count)
count=""
count=FILE_SEARCH(FILEO)

;IF COUNT EQ 1 THEN BEGIN
IF COUNT NE "" THEN BEGIN
print,'FILE EXISTS!'
print,''
restore,fileo
NT=N_ELEMENTS(REFORM(WSDI_Y(0,0,*)))
print,nt
WSDI_Y_O(*,*,*,0)=WSDI_Y(*,*,YEAR_INI_T-YEAR_INI_OBS_1:YEAR_INI_OBS_1-YEAR_INI_T+NT)
help,WSDI_Y_O
ENDIF ELSE BEGIN
print,'FILE DOES NOT EXIST!'
ENDELSE

;==========
;ERAINT
;==========
DIR_SM=DIR_OBS_2+'/PP/'+STAT
FILEO=DIR_SM+'/'+VAR+'_'+STRMID(YEAR_INI_OBS_2,4,4)+'-'+STRMID(YEAR_END_OBS_2,4,4)+'_'+STAT+'.dat'
;===============================================
;CHECK IF IDL FILE WITH WSDI STAT EXISITS ALREADY
;===============================================
print,'====================================='
print,'CHECKING ',fileo
print,'====================================='
;FILE_EX=FINDFILE(fileo,count=count)
count=""
count=FILE_SEARCH(FILEO)

;IF COUNT EQ 1 THEN BEGIN
IF COUNT NE "" THEN BEGIN
print,'FILE EXISTS!'
print,''
restore,fileo
NT=N_ELEMENTS(REFORM(WSDI_Y(0,0,*)))
WSDI_Y_O(*,*,*,1)=WSDI_Y(*,*,YEAR_INI_T-YEAR_INI_OBS_2:YEAR_INI_OBS_2-YEAR_INI_T+NT)
help,WSDI_Y_O
ENDIF ELSE BEGIN
print,'FILE DOES NOT EXIST!'
STOP
ENDELSE


;PLOT HW>24 YEAR BY YEAR

window,0,retain=2,xsize=1500,ysize=1000
!p.multi=[0,6,5]
FOR iyy=1,NYEAR_T-1 DO BEGIN &$
WW=REFORM(WSDI_Y_O(*,*,iyy,0)) &$
WW_P=REFORM(population_int(*,*,iyy)) &$
WWW=WHERE(ww GE 48,count) &$
contour,landmask  &$
IF WWW(0) NE -1 THEN BEGIN &$
cc=ARRAY_INDICES(WW,www)  &$
PLOTS,cc,psym=2 ,color=250 &$
print,iyy+1980,TOTAL(WW_P(WWW)) &$
ENDIF &$
ENDFOR

window,1,retain=2,xsize=1500,ysize=1000
!p.multi=[0,6,5]
FOR iyy=1,NYEAR_T-1 DO BEGIN &$
WW=REFORM(WSDI_Y_O(*,*,iyy,1)) &$
WW_P=REFORM(population_int(*,*,iyy)) &$
WWW=WHERE(ww GE 48,count) &$
contour,landmask  &$
IF WWW(0) NE -1 THEN BEGIN &$
cc=ARRAY_INDICES(WW,www)  &$
PLOTS,cc,psym=2 ,color=250 &$
print,1980+iyy,TOTAL(WW_P(WWW)) &$
ENDIF &$
ENDFOR
;STOP

;================
;CDF ON MASK
;================

POP_CDF=FLTARR(NMASK,NBINS,NRUN)
POP_CDF_Y=FLTARR(NMASK,NBINS,NYEAR_T-1,NRUN)  ;1981-2010

FOR imm=0,NMASK-1 DO BEGIN &$ ;MASK
 print,'========================' &$
 print,'MASK ',MASK(imm) &$
 print,'========================' &$
WORK_LAND=landmask*0. &$
we_lon=WHERE(lon GE LON1(imm) AND lon LE LON2(imm)  AND lat GE LAT1(imm)  AND lat LE LAT2(imm) AND landmask GT 0.5,COMPLEMENT=WE_NOLAND ) &$
WORK_LAND=landmask*0.+1 &$
WORK_LAND(we_noland)=0. &$

WORK=REFORM(WSDI_Y_O(*,*,*,*)) &$
FOR ir=0,NRUN-1 DO BEGIN &$
FOR iyy=0,NYEAR_T-1-1 DO BEGIN &$
WORK_W=REFORM(WORK(*,*,iyy+1,ir)) &$; WSDI STARTS IN 1980
WORK_W(we_noland)=!VALUES.F_NaN &$
;METHOD POINTS
FOR ib=0,NBINS-1 DO BEGIN &$
WE_BIN=WHERE(WORK_W GE BINS(ib) AND FINITE(WORK_W,/NaN) EQ 0,count) &$ ;
POP_WORK=REFORM(POPULATION_INT(*,*,iyy+1)) &$ ; POP STARTS in 1980
IF WE_BIN(0) NE -1 THEN POP_CDF_Y(imm,ib,iyy,ir)=TOTAL(POP_WORK(WE_BIN),/NaN) &$
ENDFOR &$ ;bins
ENDFOR &$ ;YEAR iyy
FOR ib=0,NBINS-1 DO BEGIN &$
POP_CDF(imm,ib,ir)=TOTAL(POP_CDF_Y(imm,ib,*,ir),/NaN)/(NYEAR_T-1) &$  ;30 YEARS
ENDFOR &$ ;bins


ENDFOR &$ ;RUN ir

ENDFOR ;MASK
PRINT,TOTAL(population_int(*,*,1:30))/30.
PRINT,POP_CDF(21,0,*)
save,filename=FILEO_POP,bins,mask,POP_CDF,POP_CDF_Y
JUMP_CALC:
restore,filename=FILEO_POP

POP_CDF_2=FLTARR(NMASK,NBINS,2) ;ONLY 1980-2005
FOR ir=0,1 DO BEGIN &$
FOR imm=0,NMASK-1 DO BEGIN &$
FOR ib=0,NBINS-1 DO BEGIN &$
POP_CDF_2(imm,ib,ir)=TOTAL(POP_CDF_Y(imm,ib,0:25,ir),/NaN)/26.
ENDFOR &$ ;bins
ENDFOR &$ ;bins
ENDFOR 

;===========================
;PLOT ALL MASKS
;===========================
TICKV=[12,24,48,96]
w3=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[800,1200])
FOR imm=0,NMASK-1 DO BEGIN  &$

p=PLOT(BINS,POP_CDF(imm,*,0),/CURRENT,TITLE=MASK(imm),XRANGE=[3,103],color="black",YRANGE=[1E-2,1E4], $
XTITLE='HWMId',YTITLE='Population (Million)',LAYOUT=[4,6,imm+1],MARGIN=[0.15,0.15,0.05,0.15],XMINOR=0,XTICKVALUES=TICKV,YMINOR=0,THICK=2, $
AXIS_STYLE=2,FONT_SIZE=8,/XLOG,/YLOG)   &$
p=PLOT(BINS,POP_CDF(imm,*,1),color="blue",/OVERPLOT,THICK=2,/XLOG) &$
p=PLOT(BINS,POP_CDF_2(imm,*,0),color="black",/OVERPLOT,THICK=2,/XLOG,LINESTYLE=2) &$
p=PLOT(BINS,POP_CDF_2(imm,*,1),color="blue",/OVERPLOT,THICK=2,/XLOG,LINESTYLE=2) &$
ENDFOR
;w3.Save,'./PLOTS/PLOT_'+STAT+'_CDF_MASK_SCEN_POP.pdf',BITMAP=1,PAGE_SIZE="A4"


;====================
;PLOT ONLY WORLD
;====================
w4=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[800,400])
imm=21 ;WORLD

TICKV=[3,6,12,24,48,96]
p=PLOT(BINS,POP_CDF(imm,*,0),/CURRENT,TITLE='Global average',XRANGE=[3,103],color="black",YRANGE=[1E-2,1E4], $
XTITLE='HWMId',YTITLE='Population (Million)',LAYOUT=[2,1,1],MARGIN=[0.15,0.15,0.05,0.15],XMINOR=0,XTICKVALUES=TICKV,YMINOR=0,THICK=2, $
AXIS_STYLE=2,FONT_SIZE=8,/XLOG,/YLOG) 
p=PLOT(BINS,POP_CDF(imm,*,1),color="blue",/OVERPLOT,THICK=2,/XLOG) &$
p=PLOT(BINS,POP_CDF_2(imm,*,0),color="black",/OVERPLOT,THICK=2,/XLOG,LINESTYLE=2) &$
p=PLOT(BINS,POP_CDF_2(imm,*,1),color="blue",/OVERPLOT,THICK=2,/XLOG,LINESTYLE=2) &$





STOP
END
