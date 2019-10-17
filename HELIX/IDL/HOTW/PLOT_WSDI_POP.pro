PRO PLOT_WSDI_POP
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

SCEN=['hist','rcp85'] ;1951-2005'
YEAR_INI=[1971,2006]
YEAR_END=[2005,2100]

NSCEN=N_ELEMENTS(SCEN)

NYEARS=30

;=============================================
BINSIZE=3
NBINS=35
BINS=FINDGEN(NBINS)*BINSIZE

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
FILE_LAND='landmask_05.dat'
restore,DIR_HOME+FILE_LAND
help,landmask
print,nx,ny

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


FILE_POP='population_05_'+SSP+'_int.dat'
restore,DIR_HOME+FILE_POP
nt0_int=1980
help,population_int
print,nx,ny,NT_INT

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
;READ OBS DATA
;===============================================
POP_CDF_OBS=FLTARR(NMASK,NBINS,2)
POP_CDF_oBS_2=FLTARR(NMASK,NBINS,2) ;ONLY 1980-2005

FILEO_POP='./DATA/POP_HWMId_MASK_OBS.dat'
restore,filename=FILEO_POP
POP_CDF_OBS=POP_CDF
FOR ir=0,1 DO BEGIN &$
FOR imm=0,NMASK-1 DO BEGIN &$
FOR ib=0,NBINS-1 DO BEGIN &$
POP_CDF_OBS_2(imm,ib,ir)=TOTAL(POP_CDF_Y(imm,ib,0:24,ir),/NaN)/25.
ENDFOR &$ ;bins
ENDFOR &$ ;bins
ENDFOR

;===============================================
; READ MODELS
;===============================================
POP_CDF=FLTARR(NMASK,NBINS,3,NRUN)
SIG_KS=FLTARR(NMASK,NBINS,3,NRUN) 
ww_ref=FLTARR(NMASK,NBINS,30*NRUN)
ww_1=FLTARR(NMASK,NBINS,30*NRUN)
ww_2=FLTARR(NMASK,NBINS,30*NRUN)

ic=0 ;counter for window plot
;===============================================
FOR ir=0,NRUN-1 DO BEGIN ;RCM LOOP
;===============================================

FILEO_POP='./DATA/POP_HWMId_MASK_'+RUN(ir)+'_'+SSP+'_NEW.dat'

print,'====================================='
print,'CHECKING ',fileo_pop
print,'====================================='
count=""
count=FILE_SEARCH(FILEO_POP)

IF COUNT NE "" THEN BEGIN
print,'FILE EXISTS!'
print,''
GOTO,JUMP_CALC
ENDIF

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

window,ir,retain=2,xsize=1500,ysize=1000,title=run(ir)
!p.multi=[0,6,5]
FOR iyy=9,38 DO BEGIN &$
WW=REFORM(WSDI_Y_T(*,*,iyy)) &$
WW_P=REFORM(population_int(*,*,iyy-9)) &$
WWW=WHERE(ww GE 48,count) &$
WWW_N=WHERE(ww GE 48  AND landmask GT 0.5) &$
contour,landmask  &$
IF WWW(0) NE -1 THEN BEGIN &$
cc=ARRAY_INDICES(WW,www)  &$
PLOTS,cc,psym=2 ,color=250 &$
print,iyy+1980-9,TOTAL(WW_P(WWW)),TOTAL(WW_P(WWW_N)) &$
ENDIF &$
IF WWW_N(0) NE -1 THEN BEGIN &$
cc_N=ARRAY_INDICES(WW,www_N)  &$
PLOTS,cc_N,psym=2 ,color=50 &$
ENDIF &$
;iIF ir EQ 6 and iyy EQ 32 THEN BEGIN &$
;for iy=0,191 do begin &$                                                                       
;print,cc(0,iy),cc(1,iy),WSDI_Y_T(cc(0,iy),cc(1,iy),iyy),population_int(cc(0,iy),cc(1,iy),-9),landmask(cc(0,iy),cc(1,iy)),landmask_05(cc(0,iy),cc(1,iy)) &$
;endfor &$
;ENDIF &$
ENDFOR


;GOTO,JUMP_ALL

POP_TOT_Y=FLTARR(NX,NY,NBINS,NT_INT)*!VALUES.F_NaN ;1980-2100
FOR iyy=0,NT_INT-1 DO BEGIN &$
print,'YEAR ',YEAR_INI(0)+iyy+nt0_int-YEAR_INI(0)
WORK=REFORM(WSDI_Y_T(*,*,iyy+nt0_int-YEAR_INI(0))) &$  ; 1971--->1980 CHECK!!!
;METHOD POINTS
FOR ib=0,NBINS-1 DO BEGIN &$
WE_BIN=WHERE(WORK GE BINS(ib) AND landmask GT 0.5,count) &$
POP_WORK=REFORM(POPULATION_INT(*,*,iyy))
WW=FLTARR(Nx,NY)*!VALUES.F_NaN
IF WE_BIN(0) NE -1 THEN BEGIN
ww(WE_BIN)=POP_WORK(WE_BIN)
POP_TOT_Y(*,*,ib,iyy)=WW
ENDIF
ENDFOR &$ ;bins
ENDFOR  ;YEAR iyy

;================
;CDF ON MASK
;================

POP_CDF_Y=FLTARR(NMASK,NBINS,NT_INT) ;1980-2100

FOR imm=0,NMASK-1 DO BEGIN &$ ;MASK
 print,'========================' &$
 print,'MASK ',MASK(imm) &$
 print,'========================' &$
WORK_LAND=landmask*0. &$
we_lon=WHERE(lon GE LON1(imm) AND lon LE LON2(imm)  AND lat GE LAT1(imm)  AND lat LE LAT2(imm) AND landmask GT 0.5,COMPLEMENT=WE_NOLAND ) &$
WORK_LAND=landmask*0.+1 &$
WORK_LAND(we_noland)=0. &$

FOR iyy=0,NT_INT-1 DO BEGIN &$
print,'YEAR ',YEAR_INI(0)+iyy+nt0_int-YEAR_INI(0)
WORK=REFORM(WSDI_Y_T(*,*,iyy+nt0_int-YEAR_INI(0))) &$  ; 1971--->1980 CHECK!!!
WORK(we_noland)=!VALUES.F_NaN &$
;METHOD POINTS
FOR ib=0,NBINS-1 DO BEGIN &$
WE_BIN=WHERE(WORK GE BINS(ib) AND FINITE(WORK,/NaN) EQ 0,count) &$
POP_WORK=REFORM(POPULATION_INT(*,*,iyy))
IF WE_BIN(0) NE -1 THEN POP_CDF_Y(imm,ib,iyy)=TOTAL(POP_WORK(WE_BIN),/NaN)
ENDFOR &$ ;bins
ENDFOR &$ ;YEAR iyy

ENDFOR ;MASK

save,filename=FILEO_POP,bins,mask,POP_CDF_Y,POP_TOT_Y

JUMP_CALC:
;restore,filename=FILEO_POP;,bins,mask,POP_CDF_Y

ic=ic+1
ENDFOR ; END RCM


;AVERAGE ON 30 YEAR PERIOD
FOR imm=0,NMASK-1 DO BEGIN
FOR ib=0,NBINS-1 DO BEGIN &$
FOR it=0,2 DO BEGIN
IF it EQ 0 THEN POP_CDF(imm,ib,it,ir)=TOTAL(POP_CDF_Y(imm,ib,1:30))/NYEARS ;  1981-2010
IF it EQ 1 THEN POP_CDF(imm,ib,it,ir)=TOTAL(POP_CDF_Y(imm,ib,Y_C_15(ir)-15-nt0_int:Y_C_15(ir)-15-nt0_int+NYEARS-1))/NYEARS 
IF it EQ 2 THEN POP_CDF(imm,ib,it,ir)=TOTAL(POP_CDF_Y(imm,ib,Y_C_20(ir)-15-nt0_int:Y_C_20(ir)-15-nt0_int+NYEARS-1))/NYEARS
ENDFOR
ENDFOR
ENDFOR

;=================
;CALC K-S
;=================
KS_CDF_Y=FLTARR(NMASK,NBINS,3) ;1980-2100


GOTO,JUMP_ALE
FILEO_KS='./DATA/K-S_'+VAR+'_'+RUN(ir)+'_POP_'+SSP+'.dat'
count=""
count=FILE_SEARCH(FILEO_KS)
IF COUNT NE "" THEN BEGIN
print,'K-S FILE EXISTS!'
GOTO,JUMP_KS
ENDIF
print,'KOLMOGOROV'
FOR imm=0,NMASK-1 DO BEGIN
FOR ib=0,NBINS-1 DO BEGIN
ww_ref=REFORM(POP_CDF_Y(imm,ib,1:30))
ww_1=REFORM(POP_CDF_Y(imm,ib,Y_C_15(ir)-15-nt0_int:Y_C_15(ir)-15-nt0_int+NYEARS-1))
ww_2=REFORM(POP_CDF_Y(imm,ib,Y_C_20(ir)-15-nt0_int:Y_C_20(ir)-15-nt0_int+NYEARS-1))
ks2,ww_1,NYEARS,ww_ref,NYEARS,stat_v,prob_v,fn1,fn2,sor1,sor2 ;1.5C
KS_CDF_Y(imm,ib,0)=prob_v
ks2,ww_2,NYEARS,ww_ref,NYEARS,stat_v,prob_v,fn1,fn2,sor1,sor2; 2C
KS_CDF_Y(imm,ib,1)=prob_v
ks2,ww_2,NYEARS,ww_1,NYEARS,stat_v,prob_v,fn1,fn2,sor1,sor2 ; 2C - 1.5C
KS_CDF_Y(imm,ib,2)=prob_v
ENDFOR ;bins
ENDFOR

save,filename=fileo_ks,bins,mask,KS_CDF_Y
JUMP_KS:

restore,filename=FILEO_KS;,bins,mask,POP_CDF
SIG_KS(*,*,*,ir)=KS_CDF_Y

JUMP_ALL:
JUMP_ALE:



FOR imm=0,NMASK-1 DO BEGIN
FOR ib=0,NBINS-1 DO BEGIN
it=ir*NYEARS
ww_ref(imm,ib,it:it+29)=REFORM(POP_CDF_Y(imm,ib,1:30))
ww_1(imm,ib,it:it+29)=REFORM(POP_CDF_Y(imm,ib,Y_C_15(ir)-15-nt0_int:Y_C_15(ir)-15-nt0_int+NYEARS-1))
ww_2(imm,ib,it:it+29)=REFORM(POP_CDF_Y(imm,ib,Y_C_20(ir)-15-nt0_int:Y_C_20(ir)-15-nt0_int+NYEARS-1))
ENDFOR
ENDFOR


;ic=ic+1
;ENDFOR ; END RCM


STOP
imm=21
;imm=3
ib=8
window,1,retain=2,xsize=1200,ysize=800
!p.multi=[0,4,2]
FOR ir=0,NRUN-1 DO BEGIN &$
print,'RUN ',ir &$
it=ir*NYEARS &$
a=REFORM(ww_ref(imm,ib,it:it+29)) &$
b=REFORM(ww_1(imm,ib,it:it+29)) &$
c=REFORM(ww_2(imm,ib,it:it+29)) &$
;ks2,a,NYEARS,b,NYEARS,stat_v,prob_v,fn1,fn2,sor1,sor2 &$
;print,prob_v &$
;ks2,a,NYEARS,c,NYEARS,stat_v,prob_v,fn1,fn2,sor1,sor2 &$
;print,prob_v &$
plot,b &$
oplot,c,color=250 &$
ks2,c,NYEARS,b,NYEARS,stat_v,prob_v,fn1,fn2,sor1,sor2 &$
print,prob_v &$
ENDFOR

STOP

;K-S ALE
FOR imm=0,NMASK-1 DO BEGIN &$
FOR ib=0,NBINS-1 DO BEGIN &$
ks2,ww_1(imm,ib,*),NYEARS*NRUN,ww_ref(imm,ib,*),NYEARS*NRUN,stat_v,prob_v,fn1,fn2,sor1,sor2  &$;1.5C
KS_CDF_Y(imm,ib,0)=prob_v &$
ks2,ww_2(imm,ib,*),NYEARS*NRUN,ww_ref(imm,ib,*),NYEARS*NRUN,stat_v,prob_v,fn1,fn2,sor1,sor2 &$ ;2C
KS_CDF_Y(imm,ib,1)=prob_v &$
ks2,ww_2(imm,ib,*),NYEARS*NRUN,ww_1(imm,ib,*),NYEARS*NRUN,stat_v,prob_v,fn1,fn2,sor1,sor2  &$; 2C - 1.5C
KS_CDF_Y(imm,ib,2)=prob_v &$
ENDFOR &$
ENDFOR

STOP

print,sig_ks(21,8,2,*)

MOD_AGR_KS=FLTARR(NMASK,NBINS,3)
THRES=0.05
FOR it=0,2 DO BEGIN &$
FOR imm=0,NMASK-1 DO BEGIN &$
FOR ib=0,NBINS-1 DO BEGIN &$
FOR ir=0,NRUN -1 DO BEGIN &$
IF SIG_KS(imm,ib,it,ir) LE THRES THEN MOD_AGR_KS(imm,ib,it)=MOD_AGR_KS(imm,ib,it)+1 &$
ENDFOR &$
ENDFOR &$
ENDFOR &$
ENDFOR

STOP

;===========================
;PLOT ALL MASKS
;===========================
TICKV=[12,24,48,96]
PANEL=['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v,','w','x','y','z']

;EVAL
w3=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[800,1200])
FOR imm=0,NMASK-2 DO BEGIN  &$

MAX_P=FLTARR(NBINS,4) &$
MIN_P=FLTARR(NBINS,4) &$
MED_P=FLTARR(NBINS,4) &$
FOR it=0,2 DO BEGIN &$
FOR ib=0,NBINS-1 DO BEGIN &$
MAX_P(ib,it)=MAX(REFORM(POP_CDF(imm,ib,it,*))) &$
MIN_P(ib,it)=MIN(REFORM(POP_CDF(imm,ib,it,*))) &$
MED_P(ib,it)=MEDIAN(REFORM(POP_CDF(imm,ib,it,*))) &$
ENDFOR &$
ENDFOR &$

IF imm EQ 0 OR imm EQ 4 OR imm EQ 8 OR imm EQ 12 OR imm EQ 16 OR imm EQ 20 THEN BEGIN &$
p=PLOT(BINS,MED_P(*,0),/CURRENT,XRANGE=[3,103],color="red",YRANGE=[0.1,1E4], $
XTITLE='HWMId',YTITLE='Population (Million)',LAYOUT=[4,6,imm+1],MARGIN=[0.32,0.05,0.05,0.15], $
XMINOR=0,XTICKVALUES=TICKV,YMINOR=0,THICK=2,AXIS_STYLE=1,FONT_SIZE=8,/XLOG,/YLOG)   &$
ENDIF ELSE BEGIN &$
p=PLOT(BINS,MED_P(*,0),/CURRENT,XRANGE=[3,103],color="red",YRANGE=[0.1,1E4], $
XTITLE='HWMId',LAYOUT=[4,6,imm+1],MARGIN=[0.3,0.05,0.05,0.15], $
XMINOR=0,XTICKVALUES=TICKV,YMINOR=0,THICK=2,AXIS_STYLE=1,FONT_SIZE=8,/XLOG,/YLOG)   &$
ENDELSE &$
;ww_p=WHERE(MAX_P(*,0) LT 0.005) &$
;ww_m=WHERE(MIN_P(*,0) LT 0.005) &$
;WORK=MIN_P(*,0) &$
;WORK(WHERE(WORK LT 0.1))=0.1 &$
;MIN_P(*,0)=WORK &$
;WORK=MAX_P(*,0) &$
;WORK(WHERE(WORK LT 0.1))=0.1 &$
;MAX_P(*,0)=WORK &$
;poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[MAX_P(1:ww_p(0),0),REVERSE(MIN_P(1:ww_m(0),0))],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$

ww_p=WHERE(MAX_P(*,0) LT 0.005) &$
ww_m=WHERE(MIN_P(*,0) LT 0.005) &$
WORK=MIN_P(*,0) &$
WORK(WHERE(WORK LT 0.1))=0.01 &$
MIN_P(*,0)=WORK &$
WORK=MAX_P(*,0) &$
WORK(WHERE(WORK LT 0.1))=0.01 &$
MAX_P(*,0)=WORK &$
IF ww_p(0) EQ -1 THEN ww_p(0)=WHERE(BINS EQ 102) &$
IF ww_m(0) EQ -1 THEN ww_m(0)=WHERE(BINS EQ 102) &$
poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[MAX_P(1:ww_p(0),0),REVERSE(MIN_P(1:ww_m(0),0))],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$

p=PLOT(BINS,POP_CDF_OBS_2(imm,*,0),color="black",/OVERPLOT,THICK=2,/XLOG) &$
p=PLOT(BINS,POP_CDF_OBS_2(imm,*,1),color="blue",/OVERPLOT,THICK=2,/XLOG) &$

IF MASK(imm) EQ 'NEU' OR MASK(imm) EQ 'NAS' THEN BEGIN &$
p=PLOT(BINS,POP_CDF_OBS(imm,*,0),color="black",/OVERPLOT,THICK=2,/XLOG,LINESTYLE=2) &$
p=PLOT(BINS,POP_CDF_OBS(imm,*,1),color="blue",/OVERPLOT,THICK=2,/XLOG,LINESTYLE=2) &$
ENDIF &$
t=TEXT(4,2800,PANEL(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$
t=TEXT(8,2800,MASK(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$

ENDFOR
t1=text(.3,.12,'___ NCEP-2 1981-2005',/NORMAL,FONT_SIZE=12,COLOR="black",TARGET=p)
t1=text(.3,.08,'___ ERA-INT 1981-2005',/NORMAL,FONT_SIZE=12,COLOR="blue",TARGET=p)
t1=text(.6,.12,'_ _ _ NCEP-2 1981-2010',/NORMAL,FONT_SIZE=12,COLOR="black",TARGET=p)
t1=text(.6,.08,'_ _ _ ERA-INT 1981-2010',/NORMAL,FONT_SIZE=12,COLOR="blue",TARGET=p)
t1=text(.3,.04,'___ Models median 1981-2005',/NORMAL,FONT_SIZE=12,COLOR="red",TARGET=p)
t2=POLYGON([.6,.65,.65,.6],[0.04,.04,.06,.06],/NORMAL,/FILL_BACKGROUND,FILL_COLOR="red",color="red",FILL_TRANSPARENCY=80,/OVERPLOT)
t2=text(.68,.04,'Models range',/NORMAL,FONT_SIZE=12,COLOR="red",TARGET=p)

w3.Save,'./PLOTS/PLOT_'+STAT+'_PDF_POP_MASK_REF_'+SSP+'.pdf',BITMAP=1,PAGE_SIZE="A4"

;PROJECTIONS
w4=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[800,1200])
FOR imm=0,NMASK-2 DO BEGIN  &$

MAX_P=FLTARR(NBINS,4) &$
MIN_P=FLTARR(NBINS,4) &$
MED_P=FLTARR(NBINS,4) &$
FOR it=0,2 DO BEGIN &$
FOR ib=0,NBINS-1 DO BEGIN &$
MAX_P(ib,it)=MAX(REFORM(POP_CDF(imm,ib,it,*))) &$
MIN_P(ib,it)=MIN(REFORM(POP_CDF(imm,ib,it,*))) &$
MED_P(ib,it)=MEDIAN(REFORM(POP_CDF(imm,ib,it,*))) &$
ENDFOR &$
ENDFOR &$

IF imm EQ 0 OR imm EQ 4 OR imm EQ 8 OR imm EQ 12 OR imm EQ 16 OR imm EQ 20 THEN BEGIN &$
p=PLOT(BINS,MED_P(*,0),/CURRENT,XRANGE=[3,103],color="black",YRANGE=[0.1,1E4], $
XTITLE='HWMId',YTITLE='Population (Million)',LAYOUT=[4,6,imm+1],MARGIN=[0.32,0.05,0.05,0.15], $
XMINOR=0,XTICKVALUES=TICKV,YMINOR=0,THICK=2,AXIS_STYLE=1,FONT_SIZE=8,/XLOG,/YLOG)   &$
ENDIF ELSE BEGIN &$
p=PLOT(BINS,MED_P(*,0),/CURRENT,XRANGE=[3,102],color="black",YRANGE=[0.1,1E4], $
XTITLE='HWMId',LAYOUT=[4,6,imm+1],MARGIN=[0.3,0.05,0.05,0.15], $
XMINOR=0,XTICKVALUES=TICKV,YMINOR=0,THICK=2,AXIS_STYLE=1,FONT_SIZE=8,/XLOG,/YLOG)   &$
ENDELSE &$
ww_p=WHERE(MAX_P(*,0) LT 0.005) &$
ww_m=WHERE(MIN_P(*,0) LT 0.005) &$
WORK=MIN_P(*,0) &$
WORK(WHERE(WORK LT 0.1))=0.01 &$
MIN_P(*,0)=WORK &$
WORK=MAX_P(*,0) &$
WORK(WHERE(WORK LT 0.1))=0.01 &$
MAX_P(*,0)=WORK &$
IF ww_p(0) EQ -1 THEN ww_p(0)=WHERE(BINS EQ 102) &$
IF ww_m(0) EQ -1 THEN ww_m(0)=WHERE(BINS EQ 102) &$
poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[MAX_P(1:ww_p(0),0),REVERSE(MIN_P(1:ww_m(0),0))],/DATA,/FILL_BACKGROUND, COLOR="grey",FILL_COLOR="grey",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$

p=PLOT(BINS,MED_P(*,1),color="blue",/OVERPLOT,THICK=2,/XLOG,/YLOG) &$
ww_p=WHERE(MAX_P(*,1) LT 0.005) &$
ww_m=WHERE(MIN_P(*,1) LT 0.005) &$
WORK=MIN_P(*,1) &$
WORK(WHERE(WORK LT 0.1))=0.01 &$
MIN_P(*,1)=WORK &$
WORK=MAX_P(*,1) &$
WORK(WHERE(WORK LT 0.1))=0.01 &$
MAX_P(*,1)=WORK &$
IF ww_p(0) EQ -1 THEN ww_p(0)=WHERE(BINS EQ 102) &$
IF ww_m(0) EQ -1 THEN ww_m(0)=WHERE(BINS EQ 102) &$
poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[MAX_P(1:ww_p(0),1),REVERSE(MIN_P(1:ww_m(0),1))],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$

p=PLOT(BINS,MED_P(*,2),color="red",/OVERPLOT,THICK=2,/XLOG,/YLOG) &$
ww_p=WHERE(MAX_P(*,2) LT 0.005) &$
ww_m=WHERE(MIN_P(*,2) LT 0.005) &$
WORK=MIN_P(*,2) &$
WORK(WHERE(WORK LT 0.1))=0.01 &$
MIN_P(*,2)=WORK &$
WORK=MAX_P(*,2) &$
WORK(WHERE(WORK LT 0.1))=0.01 &$
MAX_P(*,2)=WORK &$
IF ww_p(0) EQ -1 THEN ww_p(0)=WHERE(BINS EQ 102) &$
IF ww_m(0) EQ -1 THEN ww_m(0)=WHERE(BINS EQ 102) &$
poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[MAX_P(1:ww_p(0),2),REVERSE(MIN_P(1:ww_m(0),2))],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$

p_s=TEXT(bins(4),6e2,STRMID(MOD_AGR_KS(imm,4,0),6,1),/DATA,COLOR="blue",TARGET=p,FONT_SIZE=7) &$
p_s=TEXT(bins(8),6e2,STRMID(MOD_AGR_KS(imm,8,0),6,1),/DATA,COLOR="blue",TARGET=p,FONT_SIZE=7) &$
p_s=TEXT(bins(16),6e2,STRMID(MOD_AGR_KS(imm,16,0),6,1),/DATA,COLOR="blue",TARGET=p,FONT_SIZE=7) &$
p_s=TEXT(bins(29),6e2,STRMID(MOD_AGR_KS(imm,32,0),6,1),/DATA,COLOR="blue",TARGET=p,FONT_SIZE=7) &$
p_s=TEXT(bins(4),1e3,STRMID(MOD_AGR_KS(imm,4,1),6,1),/DATA,COLOR="red",TARGET=p,FONT_SIZE=7) &$
p_s=TEXT(bins(8),1e3,STRMID(MOD_AGR_KS(imm,8,1),6,1),/DATA,COLOR="red",TARGET=p,FONT_SIZE=7) &$
p_s=TEXT(bins(16),1e3,STRMID(MOD_AGR_KS(imm,16,1),6,1),/DATA,COLOR="red",TARGET=p,FONT_SIZE=7) &$
p_s=TEXT(bins(29),1e3,STRMID(MOD_AGR_KS(imm,32,1),6,1),/DATA,COLOR="red",TARGET=p,FONT_SIZE=7) &$

t=TEXT(4,2800,PANEL(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$
t=TEXT(8,2800,MASK(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$
ENDFOR
w4.Save,'./PLOTS/PLOT_'+STAT+'_PDF_POP_MASK_SCEN_'+SSP+'.pdf',BITMAP=1,PAGE_SIZE="A4"

;DIFFERNCES
w5=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[800,1200])
FOR imm=0,NMASK-1 DO BEGIN  &$

MAX_P=FLTARR(NBINS,3) &$
MIN_P=FLTARR(NBINS,3) &$
MED_P=FLTARR(NBINS,3) &$
DIFF=FLTARR(NBINS,NRUN,3) &$
FOR ir=0,NRUN-1 DO BEGIN &$
DIFF(*,ir,0)=POP_CDF(imm,*,1,ir)-POP_CDF(imm,*,0,ir) &$
DIFF(*,ir,1)=POP_CDF(imm,*,2,ir)-POP_CDF(imm,*,0,ir) &$
DIFF(*,ir,2)=POP_CDF(imm,*,2,ir)-POP_CDF(imm,*,1,ir) &$
ENDFOR &$
FOR ib=0,NBINS-1 DO BEGIN &$
FOR it=0,2 DO BEGIN &$
MIN_P(ib,it)=MIN(REFORM(DIFF(ib,*,it))) &$
MAX_P(ib,it)=MAX(REFORM(DIFF(ib,*,it))) &$
MED_P(ib,it)=MEDIAN(REFORM(DIFF(ib,*,it))) &$
ENDFOR &$
ENDFOR &$

FOR it=0,2 DO BEGIN &$
WORK=MIN_P(*,it) &$
WORK(WHERE(WORK LT 0.01))=0.01 &$
MIN_P(*,it)=WORK &$
WORK=MAX_P(*,it) &$
WORK(WHERE(WORK LT 0.01))=0.01 &$
MAX_P(*,it)=WORK &$
ENDFOR &$
;ww_p=WHERE(MAX_P(*,0) LT 0.005) &$
;ww_m=WHERE(MIN_P(*,0) LT 0.005) &$
;p=PLOT(BINS,MED_P(*,0),/CURRENT,TITLE=MASK(imm),XRANGE=[12,103],color="blue",YRANGE=[0.01,500], $
;XTITLE='HWMId',YTITLE='Population (Million)',LAYOUT=[4,6,imm+1],MARGIN=[0.15,0.15,0.05,0.15],XMINOR=0,XTICKVALUES=TICKV,YMINOR=0,THICK=2,AXIS_STYLE=2,FONT_SIZE=8,/XLOG,/YLOG)   &$
;poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[MAX_P(1:ww_p(0),0),REVERSE(MIN_P(1:ww_m(0),0))],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$
;ww_p=WHERE(MAX_P(*,1) LT 0.005) &$
;ww_m=WHERE(MIN_P(*,1) LT 0.005) &$
;p=PLOT(BINS,MED_P(*,1),color="red",/OVERPLOT,THICK=2,/XLOG,/YLOG) &$
;poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[MAX_P(1:ww_p(0),1),REVERSE(MIN_P(1:ww_m(0),1))],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$

IF imm EQ 0 OR imm EQ 4 OR imm EQ 8 OR imm EQ 12 OR imm EQ 16 OR imm EQ 20 THEN BEGIN &$
p=PLOT(BINS,MED_P(*,2),/CURRENT,XRANGE=[3,99],color="green",YRANGE=[0.1,1E4], $
XTITLE='HWMId',YTITLE='Population (Million)',LAYOUT=[4,6,imm+1],MARGIN=[0.32,0.05,0.05,0.15], $
XMINOR=0,XTICKVALUES=TICKV,YMINOR=0,THICK=2,AXIS_STYLE=1,FONT_SIZE=8,/XLOG,/YLOG)   &$
ENDIF ELSE BEGIN &$
p=PLOT(BINS,MED_P(*,2),/CURRENT,XRANGE=[3,99],color="green",YRANGE=[0.1,1E4], $
XTITLE='HWMId',LAYOUT=[4,6,imm+1],MARGIN=[0.3,0.05,0.05,0.15], $
XMINOR=0,XTICKVALUES=TICKV,YMINOR=0,THICK=2,AXIS_STYLE=1,FONT_SIZE=8,/XLOG,/YLOG)   &$
ENDELSE &$
ww_p=WHERE(MAX_P(*,2) LT 0.01) &$
ww_m=WHERE(MIN_P(*,2) LT 0.01) &$
IF ww_p(0) EQ -1 THEN ww_p(0)=WHERE(BINS EQ 102) &$
IF ww_m(0) EQ -1 THEN ww_m(0)=WHERE(BINS EQ 102) &$
poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[MAX_P(1:ww_p(0),2),REVERSE(MIN_P(1:ww_m(0),2))],/DATA,/FILL_BACKGROUND, COLOR="green",FILL_COLOR="green",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$

p_s=TEXT(bins(4),1e3,STRMID(MOD_AGR_KS(imm,4,2),6,1),/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(8),1e3,STRMID(MOD_AGR_KS(imm,8,2),6,1),/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(16),1e3,STRMID(MOD_AGR_KS(imm,16,2),6,1),/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(29),1e3,STRMID(MOD_AGR_KS(imm,32,2),6,1),/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$

t=TEXT(4,2800,PANEL(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$
t=TEXT(8,2800,MASK(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$
ENDFOR
w5.Save,'./PLOTS/PLOT_'+STAT+'_PDF_POP_MASK_IDFF_'+SSP+'.pdf',BITMAP=1,PAGE_SIZE="A4"

;====================
;PLOT ONLY WORLD
;====================

w6=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[800,400])
imm=21 ;WORLD

MAX_P=FLTARR(NBINS,4) &$
MIN_P=FLTARR(NBINS,4) &$
MED_P=FLTARR(NBINS,4) &$
FOR it=0,2 DO BEGIN &$
FOR ib=0,NBINS-1 DO BEGIN &$
MAX_P(ib,it)=MAX(REFORM(POP_CDF(imm,ib,it,*))) &$
MIN_P(ib,it)=MIN(REFORM(POP_CDF(imm,ib,it,*))) &$
MED_P(ib,it)=MEDIAN(REFORM(POP_CDF(imm,ib,it,*))) &$
ENDFOR &$
ENDFOR

TICKV=[6,12,24,48,96]
p=PLOT(BINS,MED_P(*,0),/CURRENT,TITLE='Global average',XRANGE=[6,103],color="black",YRANGE=[1E-2,1E4], $
XTITLE='HWMId',YTITLE='Population (Million)',LAYOUT=[2,1,1],MARGIN=[0.25,0.15,0.05,0.15],XMINOR=0,XTICKVALUES=TICKV,YMINOR=0,THICK=2, $
AXIS_STYLE=2,FONT_SIZE=8,/XLOG,/YLOG,XTICKLEN=0.02,YTICKLEN=0.02) 
ww_p=WHERE(MAX_P(*,0) LT 0.005) &$
ww_m=WHERE(MIN_P(*,0) LT 0.005) &$
poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[MAX_P(1:ww_p(0),0),REVERSE(MIN_P(1:ww_m(0),0))],/DATA,/FILL_BACKGROUND, COLOR="gray",FILL_COLOR="gray",FILL_TRANSPARENCY=80,TARGET=p)

;p=PLOT(BINS,POP_CDF_OBS(imm,*,0),color="black",/OVERPLOT,THICK=2,/XLOG,LINESTYLE=2) &$
;p=PLOT(BINS,POP_CDF_OBS(imm,*,1),color="blue",/OVERPLOT,THICK=2,/XLOG,LINESTYLE=2) &$

p=PLOT(BINS,MED_P(*,1),color="blue",/OVERPLOT,THICK=2,/XLOG) &$
ww_p=WHERE(MAX_P(*,1) LT 0.005) &$
ww_m=WHERE(MIN_P(*,1) LT 0.005) &$
poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[MAX_P(1:ww_p(0),1),REVERSE(MIN_P(1:ww_m(0),1))],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p)

p=PLOT(BINS,MED_P(*,2),color="red",/OVERPLOT,THICK=2,/XLOG) &$
ww_p=WHERE(MAX_P(*,2) LT 0.005) &$
ww_m=WHERE(MIN_P(*,2) LT 0.005) &$
poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[MAX_P(1:ww_p(0),2),REVERSE(MIN_P(1:ww_m(0),2))],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p)

p_s=TEXT(bins(4),3e3,STRMID(MOD_AGR_KS(imm,4,0),6,1),/DATA,COLOR="blue",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(8),3e3,STRMID(MOD_AGR_KS(imm,8,0),6,1),/DATA,COLOR="blue",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(16),3e3,STRMID(MOD_AGR_KS(imm,16,0),6,1),/DATA,COLOR="blue",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(29),3e3,STRMID(MOD_AGR_KS(imm,32,0),6,1),/DATA,COLOR="blue",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(4),5e3,STRMID(MOD_AGR_KS(imm,4,1),6,1),/DATA,COLOR="red",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(8),5e3,STRMID(MOD_AGR_KS(imm,8,1),6,1),/DATA,COLOR="red",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(16),5e3,STRMID(MOD_AGR_KS(imm,16,1),6,1),/DATA,COLOR="red",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(29),5e3,STRMID(MOD_AGR_KS(imm,32,1),6,1),/DATA,COLOR="red",TARGET=p,FONT_SIZE=8) &$

;poly_p=POLYGON([MIN(EU2003),MAX(EU2003),MAX(EU2003),MIN(EU2003)],[30,30,100,100],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,1]) &$
;poly_p=POLYGON([MIN(RU2010),MAX(RU2010),MAX(RU2010),MIN(RU2010)],[30,30,100,100],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,1]) &$
;poly_p=POLYGON([MIN(EA2007),MAX(EA2007),MAX(EA2007),MIN(EA2007)],[30,30,100,100],/DATA,/FILL_BACKGROUND, COLOR="black",FILL_COLOR="black",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,1]) &$
;poly_p=POLYGON([MIN(USA1980,/NaN),MAX(USA1980,/NaN),MAX(USA1980,/NaN),MIN(USA1980,/NaN)],[30,30,100,100],/DATA,/FILL_BACKGROUND, COLOR="green",FILL_COLOR="green",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,1]) &$

;t1=TEXT(4,86,'1976-2005',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p)
;t2=TEXT(4,78,'+1.5C',/DATA,COLOR="Blue",FONT_SIZE=10,TARGET=p)
;t3=TEXT(6,50,'+2C',/DATA,COLOR="Red",FONT_SIZE=10,TARGET=p)

;t1=TEXT(26,58,'EA 2007',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p,ORIENTATION=90)
;t2=TEXT(39,58,'FR 2003',/DATA,COLOR="Blue",FONT_SIZE=10,TARGET=p,ORIENTATION=90)
;t3=TEXT(46,58,'US 1980',/DATA,COLOR="Green",FONT_SIZE=10,TARGET=p,ORIENTATION=90)
;t4=TEXT(94,58,'RU 2010',/DATA,COLOR="Red",FONT_SIZE=10,TARGET=p,ORIENTATION=90)

;TICKV_Y=[1,5,10,15,30]
;PLOT DIFF
WORK_DIFF=FLTARR(nbins,nrun) &$
MIN_P=FLTARR(NBINS)
MAX_P=FLTARR(NBINS)
MED_P=FLTARR(NBINS)

FOR ir=0,NRUN-1 DO BEGIN &$
WORK_DIFF(*,ir)=REFORM(POP_CDF(imm,*,2,ir)-POP_CDF(imm,*,1,ir)) &$
ENDFOR &$
FOR ib=0,NBINS-1 DO BEGIN &$
MAX_P(ib)=MAX(REFORM(WORK_DIFF(ib,*))) &$
MIN_P(ib)=MIN(REFORM(WORK_DIFF(ib,*))) &$
MED_P(ib)=MEDIAN(REFORM(WORK_DIFF(ib,*))) &$
ENDFOR &$
p=PLOT(BINS,MED_P,/CURRENT,TITLE='Global average',XRANGE=[6,99],color="green",YRANGE=[1,10000], $
XTITLE='HWMId',YTITLE='Difference in population (million)',LAYOUT=[2,1,2],MARGIN=[0.15,0.15,0.05,0.15],XMINOR=0,XTICKVALUES=TICKV,YMINOR=0,THICK=2, $
AXIS_STYLE=2,FONT_SIZE=8,YTICKVALUES=TICKV_Y,/XLOG,/YLOG,XTICKLEN=0.02,YTICKLEN=0.02) 
ww_p=WHERE(MAX_P(*) LT 0.01) &$
ww_m=WHERE(MIN_P(*) LT 0.01) &$
IF ww_p(0) EQ -1 THEN ww_p(0)=WHERE(BINS EQ 102) &$
IF ww_m(0) EQ -1 THEN ww_m(0)=WHERE(BINS EQ 102) &$
poly_p=POLYGON([BINS(1:ww_p(0)),REVERSE(BINS(1:ww_m(0)))],[MAX_P(1:ww_p(0)),REVERSE(MIN_P(1:ww_m(0)))],/DATA,/FILL_BACKGROUND, COLOR="green",FILL_COLOR="green",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[4,6,imm+1]) &$

p_s=TEXT(bins(4),5e3,STRMID(MOD_AGR_KS(imm,4,2),6,1),/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(8),5e3,STRMID(MOD_AGR_KS(imm,8,2),6,1),/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(16),5e3,STRMID(MOD_AGR_KS(imm,16,2),6,1),/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins(29),5e3,STRMID(MOD_AGR_KS(imm,32,2),6,1),/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$

w6.Save,'./PLOTS/PLOT_'+STAT+'_PDF_POP_WORLD_SCEN_'+SSP+'.pdf',BITMAP=1,PAGE_SIZE="A4"

FILEO_DATA='./DATA/DATA_'+STAT+'_PDF_POP_SCEN.dat'
save,filename=FILEO_DATA,BINS,MASK,POP_CDF,MOD_AGR_KS


STOP
END
