PRO PLOT_RETURN_POP
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
;BINSIZE=1
;NBINS=105
;BINS=FINDGEN(NBINS)*BINSIZE

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
;MEAN OF OBS
FR2003=[39.8]
RU2010=[81.9]
USA1980=[43.6]
EA2007=[23.8]

;====================
;POPULATION DATA
;====================
SSP='SSP3'
FILE_POP='population_05_'+SSP+'_int.dat'
restore,DIR_HOME+FILE_POP
nt0_int=1980
help,POPULATION_INT
print,nx,ny,NT_INT
POP_INT_NEW=FLTARR(Nx,NY,NT_INT+9) ;1980--->1971
FOR ix=0,Nx-1 DO BEGIN &$
FOR iy=0,Ny-1 DO BEGIN &$
POP_INT_NEW(ix,iy,0:8)=POPULATION_INT(ix,iy,0)
POP_INT_NEW(ix,iy,9:NT_INT+9-1)=POPULATION_INT(ix,iy,0:NT_INT-1)
ENDFOR
ENDFOR


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

FILEO_DATA='./DATA/RET_LEV_M_POP.dat'
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

POP_CDF_RET=FLTARR(NBINS,7,3,NRUN)
POP_CDF_DIF=FLTARR(NBINS,7,3,NRUN) ;POP ON SIG POINTS
POP_CDF_SIG=FLTARR(NBINS,7,3,NRUN) ;POP ON SIG POINTS

HW_LEV_SAVE=FLTARR(720,280,3,3,NRUN) ;ONLY HWMId=24,48,96 and RET_LEV=5,10,20
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
NX_R=dims_a(0)
NY_R=dims_a(1)
NP=dims_a(2)
NCDF_VARGET,fileID,'returnlevel',RET_LEV;,count=[nx,ny],offset=[0,0]
NCDF_VARGET,fileID,'returnlevelError',RET_LEV_ERR;,count=[nx,ny],offset=[0,0]
NCDF_VARGET,fileID,'returnperiod',RET_PER;,count=[nx,ny],offset=[0,0]
NCDF_VARGET,fileID,'lon',lon;,count=[nx,ny],offset=[0,0]
NCDF_VARGET,fileID,'lat',lat;,count=[nx,ny],offset=[0,0]
NCDF_CLOSE, fileID

nt0=[5,Y_C_15(ir)-15-YEAR_INI(0),Y_C_20(ir)-15-YEAR_INI(0)]

LANDMASK_R=REFORM(LANDMASK(*,69:69+NY_R-1))
LANDMASK_R(WHERE(LANDMASK_R NE 1))=0.
;POP MAP REGRIDDING (ONLY ONCE!)
IF ir EQ 0 THEN BEGIN
POP_INT_NEW=REFORM(POP_INT_NEW(*,69:69+NY_R-1,*)) ;ON THE SAME GRID THAN THE RET LEV
FOR it=0,129 DO BEGIN
WORK=REFORM(POP_INT_NEW(*,*,it))
WORK(WHERE(LANDMASK_R NE 1))=0.
POP_INT_NEW(*,*,it)=WORK
ENDFOR
ENDIF

;SIGNIFICANCE MAP
SIG_RET_LEV=FLTARR(NX_R,NY_R,NP,3)
FOR ip=0,NP-1 DO BEGIN &$
FOR ix=0,NX_R-1 DO BEGIN &$
FOR iy=0,NY_R-1 DO BEGIN &$
IF landmask_r(ix,iy) EQ 1 THEN BEGIN &$
IF RET_LEV(ix,iy,ip,nt0(1)+15) GT RET_LEV(ix,iy,ip,nt0(0)+15)+RET_LEV_ERR(ix,iy,ip,nt0(0)+15) THEN SIG_RET_LEV(ix,iy,ip,0)=1
IF RET_LEV(ix,iy,ip,nt0(2)+15) GT RET_LEV(ix,iy,ip,nt0(0)+15)+RET_LEV_ERR(ix,iy,ip,nt0(0)+15) THEN SIG_RET_LEV(ix,iy,ip,1)=1
IF RET_LEV(ix,iy,ip,nt0(2)+15) GT RET_LEV(ix,iy,ip,nt0(1)+15)+RET_LEV_ERR(ix,iy,ip,nt0(1)+15) THEN SIG_RET_LEV(ix,iy,ip,2)=1
ENDIF
ENDFOR
ENDFOR
ENDFOR

;COMPUTE PDF
FOR it=0,2 DO BEGIN &$
FOR ip=0,NP-1 DO BEGIN &$
WORK_HH=FLTARR(NX_R,NY_R) &$
WORK=REFORM(RET_LEV(*,*,ip,nt0(it)+15)) &$ ; *POP_INT_NEW(*,*,iyy+nt0(it)) &$  ; 1976 CHECK!!!
;WORK(WHERE(LANDMASK_R NE 1))=!VALUES.F_NaN &$
FOR ib=0,NBINS-1 DO BEGIN &$
WORK_HH=FLTARR(NX_R,NY_R) &$
;METHOD POINTS
;WORK_H=FLTARR(NX,NY) &$
;WE_BIN=WHERE(WORK GE BINS(ib) AND FINITE(WORK,/NaN) EQ 0,count) &$
WE_BIN=WHERE(WORK GE BINS(ib) AND LANDMASK_R EQ 1 ,count) &$
IF WE_BIN(0) NE -1 THEN WORK_HH(WE_BIN)=1 &$
;POP_CDF_RET(ib,ip,it,ir)=TOTAL(WORK_HH*REFORM(POP_INT_NEW(*,*,nt0(it)+15))) &$
IF it EQ 0 THEN POP_CDF_RET(ib,ip,it,ir)=TOTAL(WORK_HH*REFORM(POP_INT_NEW(*,*,nt0(0)+15))) ELSE $
POP_CDF_RET(ib,ip,it,ir)=TOTAL(WORK_HH*REFORM(POP_INT_NEW(*,*,nt0(1)+15)))&$  ; SAME POP 1.5 ANd 2C!
ENDFOR &$  ; ip
ENDFOR &$ ;bins
ENDFOR  ; it


;COMPUTE PDF OF DIFF

FOR ip=0,NP-1 DO BEGIN &$
WORK_HH=FLTARR(NX_R,NY_R) &$
WORK0=REFORM(RET_LEV(*,*,ip,nt0(0)+15)) &$ ; *POP_INT_NEW(*,*,iyy+nt0(it)) &$  ; 1976 CHECK!!!
WORK1=REFORM(RET_LEV(*,*,ip,nt0(1)+15)) &$ ; *POP_INT_NEW(*,*,iyy+nt0(it)) &$  ; 1976 CHECK!!!
WORK2=REFORM(RET_LEV(*,*,ip,nt0(2)+15)) &$ ; *POP_INT_NEW(*,*,iyy+nt0(it)) &$  ; 1976 CHECK!!!
;WORK0(WHERE(LANDMASK_R EQ 0))=!VALUES.F_NaN &$
;WORK1(WHERE(LANDMASK_R EQ 0))=!VALUES.F_NaN &$
;WORK2(WHERE(LANDMASK_R EQ 0))=!VALUES.F_NaN &$
FOR ib=0,NBINS-1 DO BEGIN &$
WORK_HH0=FLTARR(NX_R,NY_R) &$
WORK_HH1=FLTARR(NX_R,NY_R) &$
WORK_HH2=FLTARR(NX_R,NY_R) &$
;METHOD POINTS
;WORK_H=FLTARR(NX,NY) &$
;WE0_BIN=WHERE(WORK0 GE BINS(ib) AND FINITE(WORK0,/NaN) EQ 0) &$
;WE1_BIN=WHERE(WORK1 GE BINS(ib) AND FINITE(WORK1,/NaN) EQ 0) &$
;WE2_BIN=WHERE(WORK2 GE BINS(ib) AND FINITE(WORK2,/NaN) EQ 0) &$
WE0_BIN=WHERE(WORK0 GE BINS(ib) AND LANDMASK_R EQ 1) &$
WE1_BIN=WHERE(WORK1 GE BINS(ib) AND LANDMASK_R EQ 1) &$
WE2_BIN=WHERE(WORK2 GE BINS(ib) AND LANDMASK_R EQ 1) &$
IF WE0_BIN(0) NE -1 THEN WORK_HH0(WE0_BIN)=1 &$
IF WE1_BIN(0) NE -1 THEN WORK_HH1(WE1_BIN)=1 &$
IF WE2_BIN(0) NE -1 THEN WORK_HH2(WE2_BIN)=1 &$

;DIF0=WORK_HH1*REFORM(POP_INT_NEW(*,*,nt0(1)+15))-WORK_HH0*REFORM(POP_INT_NEW(*,*,nt0(0)+15))
;DIF1=WORK_HH2*REFORM(POP_INT_NEW(*,*,nt0(2)+15))-WORK_HH0*REFORM(POP_INT_NEW(*,*,nt0(0)+15))
;DIF2=WORK_HH2*REFORM(POP_INT_NEW(*,*,nt0(2)+15))-WORK_HH1*REFORM(POP_INT_NEW(*,*,nt0(1)+15))
DIF0=WORK_HH1*REFORM(POP_INT_NEW(*,*,nt0(1)+15))-WORK_HH0*REFORM(POP_INT_NEW(*,*,nt0(0)+15))  ;SAME POP 1.5 AND 2C!
DIF1=WORK_HH2*REFORM(POP_INT_NEW(*,*,nt0(1)+15))-WORK_HH0*REFORM(POP_INT_NEW(*,*,nt0(0)+15))  ;SAMEPOP 1.5 AND 2C!
DIF2=WORK_HH2*REFORM(POP_INT_NEW(*,*,nt0(1)+15))-WORK_HH1*REFORM(POP_INT_NEW(*,*,nt0(1)+15))  ;SAME POP 1.5 AND 2C!
SIG0=DIF0
SIG0(WHERE(SIG_RET_LEV(*,*,ip,0) NE 1))=0
POP_CDF_DIF(ib,ip,0,ir)=TOTAL(DIF0) &$
POP_CDF_SIG(ib,ip,0,ir)=TOTAL(SIG0) &$
SIG1=DIF1
SIG1(WHERE(SIG_RET_LEV(*,*,ip,1) NE 1))=0
POP_CDF_DIF(ib,ip,1,ir)=TOTAL(DIF1) &$
POP_CDF_SIG(ib,ip,1,ir)=TOTAL(SIG1) &$
SIG2=DIF2
SIG2(WHERE(SIG_RET_LEV(*,*,ip,2) NE 1))=0
POP_CDF_DIF(ib,ip,2,ir)=TOTAL(DIF2) &$
POP_CDF_SIG(ib,ip,2,ir)=TOTAL(SIG2) &$
GOTO,JUMP_HW_LEV
IF ib EQ 8 AND ip EQ 0 THEN HW_LEV_SAVE(*,*,0,0,ir)=WORK_HH2-WORK_HH1 ;DIFF HW WITH MAG=24 AND RET_PER=5 2c-1.5
IF ib EQ 8 AND ip EQ 1 THEN HW_LEV_SAVE(*,*,0,1,ir)=WORK_HH2-WORK_HH1 ;DIFF HW WITH MAG=24 AND RET_PER=5 2c-1.5
IF ib EQ 8 AND ip EQ 2 THEN HW_LEV_SAVE(*,*,0,2,ir)=WORK_HH2-WORK_HH1 ;DIFF HW WITH MAG=24 AND RET_PER=5 2c-1.5
IF ib EQ 16 AND ip EQ 0 THEN HW_LEV_SAVE(*,*,1,0,ir)=WORK_HH2-WORK_HH1 ;DIFF HW WITH MAG=24 AND RET_PER=5 2c-1.5
IF ib EQ 16 AND ip EQ 1 THEN HW_LEV_SAVE(*,*,1,1,ir)=WORK_HH2-WORK_HH1 ;DIFF HW WITH MAG=24 AND RET_PER=5 2c-1.5
IF ib EQ 16 AND ip EQ 2 THEN HW_LEV_SAVE(*,*,1,2,ir)=WORK_HH2-WORK_HH1 ;DIFF HW WITH MAG=24 AND RET_PER=5 2c-1.5
IF ib EQ 32 AND ip EQ 0 THEN HW_LEV_SAVE(*,*,2,0,ir)=WORK_HH2-WORK_HH1 ;DIFF HW WITH MAG=24 AND RET_PER=5 2c-1.5
IF ib EQ 32 AND ip EQ 1 THEN HW_LEV_SAVE(*,*,2,1,ir)=WORK_HH2-WORK_HH1 ;DIFF HW WITH MAG=24 AND RET_PER=5 2c-1.5
IF ib EQ 32 AND ip EQ 2 THEN HW_LEV_SAVE(*,*,2,2,ir)=WORK_HH2-WORK_HH1 ;DIFF HW WITH MAG=24 AND RET_PER=5 2c-1.5
JUMP_HW_VEL:
ENDFOR &$  ; ip
ENDFOR &$ ;bins


GOTO,JUMP_P
loadct,39,RGB_TABLE=rgb
levels=[0,12,24,48,96,192,384]/2
colors=[255,100,150,190,210,250]

LEV_POP=[0.001,0.01,0.1,1,5,10,50] 

pattern_r = Obj_New('idlgrpattern', 1, ORIENTATION=-45,SPACING=4,THICK=0.8)

w = WINDOW(WINDOW_TITLE='Median',DIMENSIONS=[1200,800])
MARGIN=[0.02,0.02,0.02,0.02]
FOR it=0,2 DO BEGIN &$
w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
       TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
       FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,2,it+1],MARGIN=MARGIN, $
       title='R.P. 5 yrs',/CURRENT) &$
c_plot= CONTOUR(RET_LEV(*,*,0,nt0(it)+15),lon,lat,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
       mc = MAPCONTINENTS(THICK=0.5) &$
IF it GE 1 THEN s_plot= CONTOUR(SIG_RET_LEV(*,*,0,it-1),lon,lat,c_value=[0,1],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2) &$

w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
       TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
       FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,2,it+1+3],MARGIN=MARGIN, $
       title='R.P. 5 yrs',/CURRENT) &$
c_plot= CONTOUR(POP_INT_NEW(*,*,nt0(it)+15),lon,lat,c_value=lev_pop,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
IF it GE 1 THEN s_plot= CONTOUR(SIG_RET_LEV(*,*,0,it-1),lon,lat,c_value=[0,1],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2) &$
ENDFOR
JUMP_P:

print,POP_CDF_RET(32,0,0,ir),POP_CDF_RET(32,0,1,ir),POP_CDF_RET(32,0,2,ir)
print,POP_CDF_DIF(32,0,0,ir),POP_CDF_DIF(32,0,1,ir),POP_CDF_DIF(32,0,2,ir)
print,POP_CDF_SIG(32,0,0,ir),POP_CDF_SIG(32,0,1,ir),POP_CDF_SIG(32,0,2,ir)


ENDFOR ;RCM

JUMP_CALC:


GOTO,JUMP_HW_LEV_M
HW_LEV_SAVE_M=FLTARR(NX_R,NY_R,3,3)
FOR ix=0,NX_R-1 DO BEGIN &$
FOR iy=0,NY_R-1 DO BEGIN &$
FOR ip=0,2 DO BEGIN &$
FOR il=0,2 DO BEGIN &$
IF landmask_r(ix,iy) EQ 1 THEN BEGIN &$
HW_LEV_SAVE_M(ix,iy,il,ip)=MEDIAN(HW_LEV_SAVE(ix,iy,il,ip,*)) &$
ENDIF &$
ENDFOR &$
ENDFOR &$
ENDFOR &$
ENDFOR
JUMP_HW_LEV_M:


;CALC PDF FOR MODEL MEAN



;PLOTS
RET_PER=[5,10,20,50,100,300,1000]

MAX_P=FLTARR(NBINS,NP,3) &$
MIN_P=FLTARR(NBINS,NP,3) &$
MED_P=FLTARR(NBINS,NP,3) &$
STD_P=FLTARR(NBINS,NP,3) &$
FOR it=0,2 DO BEGIN &$
FOR ip=0,NP-1 DO BEGIN &$
FOR ib=0,NBINS-1 DO BEGIN &$
;MAX_P(ib,ip,it)=MAX(REFORM(POP_CDF_RET(ib,ip,it,*))) &$
;MIN_P(ib,ip,it)=MIN(REFORM(POP_CDF_RET(ib,ip,it,*))) &$
MED_P(ib,ip,it)=MEAN(REFORM(POP_CDF_RET(ib,ip,it,*))) &$
STD_P(ib,ip,it)=STDDEV(REFORM(POP_CDF_RET(ib,ip,it,*))) &$
MAX_P(ib,ip,it)=MED_P(ib,ip,it)+STD_P(ib,ip,it) &$
MIN_P(ib,ip,it)=MED_P(ib,ip,it)-STD_P(ib,ip,it) &$
ENDFOR &$
ENDFOR &$
ENDFOR

;DIFF
WORK_DIFF=FLTARR(NBINS,NP,NRUN) &$
MAX_D=FLTARR(NBINS,NP) &$
MIN_D=FLTARR(NBINS,NP) &$
MED_D=FLTARR(NBINS,NP) &$
STD_D=FLTARR(NBINS,NP) &$

FOR ip=0,NP-1 DO BEGIN &$
FOR ir=0,NRUN-1 DO BEGIN &$
WORK_DIFF(*,ip,ir)=REFORM(POP_CDF_RET(*,ip,2,ir)-POP_CDF_RET(*,ip,1,ir)) &$
ENDFOR &$
FOR ib=0,NBINS-1 DO BEGIN &$
MED_D(ib,ip)=MEAN(REFORM(WORK_DIFF(ib,ip,*))) &$
STD_D(ib,ip)=STDDEV(REFORM(WORK_DIFF(ib,ip,*))) &$
MAX_D(ib,ip)=MED_D(ib,ip)+STD_D(ib,ip)
MIN_D(ib,ip)=MED_D(ib,ip)-STD_D(ib,ip)
ENDFOR &$
ENDFOR



w6=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[900,900])
TICKV=[12,24,48,96]
LABEL_P=['a','b','c','d','e','f']

NPP=2 ; nr of ret per plotted
FOR ip=0,NPP DO BEGIN &$
p=PLOT(BINS,SMOOTH(MED_P(*,ip,0),3),/CURRENT,TITLE='Return period'+STRING(RET_PER(ip))+' years',XRANGE=[12,102],color="black",YRANGE=[10,5E4], $
XTITLE='HWMId',YTITLE='Population (Million)',LAYOUT=[2,NPP+1,2*ip+1],MARGIN=[0.25,0.15,0.05,0.15],XMINOR=0,XTICKVALUES=TICKV,YMINOR=0,THICK=2, $
AXIS_STYLE=2,FONT_SIZE=8,/XLOG,/YLOG,XTICKLEN=0.02,YTICKLEN=0.02) &$
ww_p=WHERE(MAX_P(*,ip,0) LT 5) &$
ww_m=WHERE(MIN_P(*,ip,0) LT 5) &$
poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[SMOOTH(MAX_P(1:ww_p(0),ip,0),3),REVERSE(SMOOTH(MIN_P(1:ww_m(0),ip,0),3))],/DATA,/FILL_BACKGROUND, COLOR="gray",FILL_COLOR="gray",FILL_TRANSPARENCY=80,TARGET=p) &$

p=PLOT(BINS,SMOOTH(MED_P(*,ip,1),3),color="blue",/OVERPLOT,THICK=2,/XLOG) &$
ww_p=WHERE(MAX_P(*,ip,1) LT 5) &$
ww_m=WHERE(MIN_P(*,ip,1) LT 5) &$
IF ww_m(0) EQ -1 THEN ww_m(0)=WHERE(BINS EQ 102) &$
IF ww_p(0) EQ -1 THEN ww_p(0)=WHERE(BINS EQ 102) &$
IF ww_p(0) EQ  WHERE(BINS EQ 102) THEN ww_m(0)=WHERE(BINS EQ 102) &$
MIN_P(WHERE(MIN_P LT 0))=10 &$
print,ww_m(0),MIN_P(1:ww_m(0),ip,1),ww_p(0),MAX_P(1:ww_p(0),ip,1) &$
poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[SMOOTH(MAX_P(1:ww_p(0),ip,1),3),REVERSE(SMOOTH(MIN_P(1:ww_m(0),ip,1),3))],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p) &$

p=PLOT(BINS,SMOOTH(MED_P(*,ip,2),3),color="red",/OVERPLOT,THICK=2,/XLOG) &$
ww_p=WHERE(MAX_P(*,ip,2) LT .05) &$
ww_m=WHERE(MIN_P(*,ip,2) LT .05) &$
IF ww_p(0) EQ -1 THEN ww_p(0)=WHERE(BINS EQ 102) &$
IF ww_m(0) EQ -1 THEN ww_m(0)=WHERE(BINS EQ 102) &$
poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[SMOOTH(MAX_P(1:ww_p(0),ip,2),3),REVERSE(SMOOTH(MIN_P(1:ww_m(0),ip,2),3))],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p) &$

SIG_P=FLTARR(NBINS,3) &$
FOR it=0,2 DO BEGIN &$
FOR ib=0,NBINS-1 DO BEGIN &$
SIG=POP_CDF_SIG(ib,ip,it,*)/POP_CDF_DIF(ib,ip,it,*)*100. &$
WW=SIG(SORT(SIG)) &$
SIG_P(ib,it)=WW(3) &$
ENDFOR &$
ENDFOR &$

DIFF1=REFORM(MED_P(*,ip,1)-MED_P(*,ip,0)) &$
DIFF2=REFORM(MED_P(*,ip,2)-MED_P(*,ip,0)) &$
;p_s=TEXT(bins(4)-2,3E3,STRMID(DIFF1(4),6,3),/DATA,COLOR="blue",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(8)-4,1E4,STRMID(DIFF1(8),5,5)+' ('+STRMID(SIG_P(8,0),6,3)+')',/DATA,COLOR="blue",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(16)-8,1E4,STRMID(DIFF1(16),5,5)+' ('+STRMID(SIG_P(16,0),6,3)+')',/DATA,COLOR="blue",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(32)-26,1E4,STRMID(DIFF1(32),5,5)+' ('+STRMID(SIG_P(32,0),6,3)+')',/DATA,COLOR="blue",TARGET=p,FONT_SIZE=8) &$
;p_s=TEXT(bins(4)-2,5E3,STRMID(DIFF2(4),6,4),/DATA,COLOR="red",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(8)-4,2E4,STRMID(DIFF2(8),5,5)+' ('+STRMID(SIG_P(8,1),6,3)+')',/DATA,COLOR="red",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(16)-8,2E4,STRMID(DIFF2(16),5,5)+' ('+STRMID(SIG_P(16,1),6,3)+')',/DATA,COLOR="red",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(32)-26,2E4,STRMID(DIFF2(32),5,5)+' ('+STRMID(SIG_P(32,1),6,3)+')',/DATA,COLOR="red",TARGET=p,FONT_SIZE=8) &$

IF ip EQ NPP THEN BEGIN &$
t1=TEXT(13,30,'1976-2005',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p) &$
t2=TEXT(50,30,'+1.5 $\circ$C',/DATA,COLOR="Blue",FONT_SIZE=10,TARGET=p) &$
t3=TEXT(50,1500,'+2 $\circ$C',/DATA,COLOR="Red",FONT_SIZE=10,TARGET=p) &$
ENDIF &$

t=TEXT(0.,1.1,LABEL_P(2*ip),/RELATIVE,FONT_SIZE=14,TARGET=p)

;DIFF
p=PLOT(BINS,SMOOTH(MED_D(*,ip),3),/CURRENT,XRANGE=[12,102],color="green",YRANGE=[10,50000], $
	XTITLE='HWMId',YTITLE='Difference in population (million)',LAYOUT=[2,NPP+1,2*ip+2],MARGIN=[0.15,0.15,0.05,0.15],XMINOR=0,XTICKVALUES=TICKV,YMINOR=0,THICK=2, $
	AXIS_STYLE=2,FONT_SIZE=8,/XLOG,/YLOG,XTICKLEN=0.02,YTICKLEN=0.02) &$
ww_p=WHERE(MAX_D(*,ip) LT 0.1) &$
ww_m=WHERE(MIN_D(*,ip) LT 0.1) &$
IF ww_p(0) EQ -1 THEN ww_p(0)=WHERE(BINS EQ 102) &$
IF ww_m(0) EQ -1 THEN ww_m(0)=WHERE(BINS EQ 102) &$
IF ww_p(0) EQ 0 THEN ww_p(0)=WHERE(BINS EQ 102) &$
IF ww_m(0) EQ 0 THEN ww_m(0)=WHERE(BINS EQ 102) &$
poly_p=POLYGON([BINS(4:ww_p(0)),REVERSE(BINS(4:ww_m(0)))],[SMOOTH(MAX_D(4:ww_p(0),ip),3),REVERSE(SMOOTH(MIN_D(4:ww_m(0),ip),3))],/DATA,/FILL_BACKGROUND, COLOR="green",FILL_COLOR="green",FILL_TRANSPARENCY=80,TARGET=p) &$

DIFF1=MED_D(*,ip) &$
;p_s=TEXT(bins(4)-2,5E3,STRMID(DIFF1(4),6,3),/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(8)-4,2E4,STRMID(DIFF1(8),5,5)+' ('+STRMID(SIG_P(8,2),6,3)+')',/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(16)-8,2E4,STRMID(DIFF1(16),5,5)+' ('+STRMID(SIG_P(16,2),6,3)+')',/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(32)-23,2E4,STRMID(DIFF1(32),5,5)+' ('+STRMID(SIG_P(32,2),6,3)+')',/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$

IF ip EQ NPP THEN BEGIN &$
poly_p=POLYGON([MIN(EA2007),MAX(EA2007),MAX(EA2007),MIN(EA2007)],[10,10,10000,10000],/DATA,/FILL_BACKGROUND, COLOR="black",FILL_COLOR="black",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,1]) &$
t1=TEXT(23,52,'Bal. 2007',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p,ORIENTATION=90) &$

poly_p=POLYGON([MIN(FR2003),MAX(FR2003),MAX(FR2003),MIN(FR2003)],[10,10,10000,10000],/DATA,/FILL_BACKGROUND, COLOR="black",FILL_COLOR="black",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,1]) &$
t2=TEXT(38,52,'Fr. 2003',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p,ORIENTATION=90) &$

poly_p=POLYGON([MIN(USA1980,/NaN),MAX(USA1980,/NaN),MAX(USA1980,/NaN),MIN(USA1980,/NaN)],[10,10,10000,10000],/DATA,/FILL_BACKGROUND, COLOR="black",FILL_COLOR="black",FILL_TRANSPARENCY=80,TARGET=p) &$
t3=TEXT(50,52,'U.S. 1980',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p,ORIENTATION=90) &$

poly_p=POLYGON([MIN(RU2010),MAX(RU2010),MAX(RU2010),MIN(RU2010)],[10,10,10000,10000],/DATA,/FILL_BACKGROUND, COLOR="black",FILL_COLOR="black",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,1]) &$
t4=TEXT(79,400,'Ru. 2010',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p,ORIENTATION=90) &$

t5=TEXT(13,2000,'+2 $\circ$C - 1.5 $\circ$C',/DATA,COLOR="green",FONT_SIZE=10,TARGET=p) &$
ENDIF &$
t=TEXT(0.,1.1,LABEL_P(2*ip),/RELATIVE,FONT_SIZE=14,TARGET=p)

ENDFOR


w = WINDOW(WINDOW_TITLE='Median',DIMENSIONS=[600,900])

loadct,39,RGB_TABLE=rgb
LEV_POP=[0.05,0.1,0.5,1,5,10,50] 
colors=[10,35,55,100,150,190,250]

FOR ip=0,2 DO BEGIN &$
w_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
       TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
       FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[1,3,1+ip],MARGIN=MARGIN, $
       title='R.P. 5 yrs',/CURRENT) &$
;c_plot= CONTOUR(POP_INT_NEW(*,*,nt0(1)+15)-POP_INT_NEW(*,*,nt0(0)+15),lon,lat,c_value=lev_pop,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
;tickname = STRING(lev_pop,FORMAT='(f7.3)') &$
;IF ip EQ 2 THEN cb0 = Colorbar(TARGET=c_plot,BORDER=1,TAPER=0,TICKNAME=tickname,position=[.2,.05,.8,.06],FONT_SIZE=8,TITLE='Population (Million)') &$
h_plot= CONTOUR(HW_LEV_SAVE_M(*,*,0,ip),lon,lat,c_value=[0,1],color="black",RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2,/FILL) &$
h_plot= CONTOUR(HW_LEV_SAVE_M(*,*,1,ip),lon,lat,c_value=[0,1],color="blue",RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2,/FILL) &$
h_plot= CONTOUR(HW_LEV_SAVE_M(*,*,2,ip),lon,lat,c_value=[0,1],color="red",RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2,/FILL) &$
ENDFOR


STOP
END
