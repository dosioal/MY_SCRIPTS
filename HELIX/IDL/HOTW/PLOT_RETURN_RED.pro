PRO PLOT_RETURN_RED
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

Y_C_15=[2024,2033,2025,2028,2029,2025,2027]
Y_C_20=[2033,2049,2037,2042,2046,2035,2039]

;=============================================
BINSIZE=3
NBINS=35
BINS=FINDGEN(NBINS)*BINSIZE

NL=300  ; RET_LEV MAX=300
RET_PER=[5,10,20,50,100,300,1000]
NP=N_ELEMENTS(RET_PER)
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
FR2003=[39.8]
RU2010=[81.9]
USA1980=[43.6]
EA2007=[23.8]

;PLOT LEVELS HWMId
LEV_L=[24,48,96]
LEV_L=[20,40,80]
NLEV_L=3

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

LANDMASK_R=REFORM(LANDMASK(*,69:69+280-1))
NX_R=N_ELEMENTS(LANDMASK_R(*,0))
NY_R=N_ELEMENTS(LANDMASK_R(0,*))
LON_R=REFORM(LON_O(*,69:69+NY_R-1))
LAT_R=REFORM(LAT_O(*,69:69+NY_R-1))
print,NX_R,NY_R


;===============================================
; READ MODELS
;===============================================
NT=3 ;TIME PER
ic=0 ;counter for window plot
RET_LEV_MASK_T=FLTARR(NMASK,300,3,NRUN)
;===============================================
FOR ir=0,NRUN-1 DO BEGIN ;RCM LOOP
;===============================================

RET_LEV_T=FLTARR(NX_R,NY_R,NP,NT)
RET_LEV_MASK=FLTARR(NMASK,300,3)

FILEO='./DATA/RETURN_LEV_RED_'+RUN(ir)+'.dat'

print,'====================================='
print,'CHECKING ',fileo
print,'====================================='
count=""
count=FILE_SEARCH(FILEO)

;IF COUNT EQ 1 THEN BEGIN
IF COUNT NE "" THEN BEGIN
print,'FILE EXISTS!'
print,''
;GOTO,JUMP_CALC

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

nt0=[5,Y_C_15(ir)-15-YEAR_INI,Y_C_20(ir)-15-YEAR_INI]
nt_m=Y_C_15(ir)+FIX((Y_C_20(ir)-Y_C_15(ir))/2.)

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
nx_r=dims_a(0)
ny_r=dims_a(1)
np=dims_a(2)
NCDF_VARGET,fileID,'returnlevel',RET_LEV;,count=[nx,ny],offset=[0,0]
NCDF_VARGET,fileID,'returnperiod',RET_PER;,count=[nx,ny],offset=[0,0]
NCDF_VARGET,fileID,'lon',lon;,count=[nx,ny],offset=[0,0]
NCDF_VARGET,fileID,'lat',lat;,count=[nx,ny],offset=[0,0]
NCDF_CLOSE, fileID
help,lon,lat

FOR ix=0,nx_r-1 DO BEGIN
FOR iy=0,ny_r-1 DO BEGIN
IF landmask_r(ix,iy) GT 0.5 THEN BEGIN &$
FOR ip=0,NP-1 DO BEGIN &$
RET_LEV_T(ix,iy,ip,0)=MEAN(RET_LEV(ix,iy,ip,5:34),/NaN)
RET_LEV_T(ix,iy,ip,1)=MEAN(RET_LEV(ix,iy,ip,Y_C_15(ir)-15-YEAR_INI:Y_C_15(ir)+14-YEAR_INI),/NaN)
RET_LEV_T(ix,iy,ip,2)=MEAN(RET_LEV(ix,iy,ip,Y_C_20(ir)-15-YEAR_INI:Y_C_20(ir)+14-YEAR_INI),/NaN)
ENDFOR &$
ENDIF
ENDFOR
ENDFOR


;INTERPOLATION AND REVERSE FUNCTION
print,'REVERSE FUNCTION'
zz0=FLTARR(nx_r,ny_r,NL,3)*!VALUES.F_NaN
;zz0_ERR=FLTARR(nx_r,ny_r,NL,3)*!VALUES.F_NaN

FOR ix=0,nx_r-1 DO BEGIN &$
FOR iy=0,ny_r-1 DO BEGIN &$

;ix = 360 
;iy = 130

IF landmask_r(ix,iy) GT 0.5 THEN BEGIN &$
FOR it=0,2 DO BEGIN &$
y=[0,REFORM(RET_LEV_T(ix,iy,*,it))] &$
WE_Y=WHERE(finite(y,/NAN) EQ 0)
IF WE_Y(0) NE -1 THEN BEGIN
z0=SPLINE([0,RET_PER],y,FINDGEN(999),30) &$ ; RET PER INTERPOLATED
b=INTERPOL(FINDGEN(999),z0,FINDGEN(FIX(MAX(z0))+1))
b(WHERE(b LT 0))=!VALUES.F_NaN
IF N_ELEMENTS(b) LE NL-1 THEN zz0(ix,iy,0:N_ELEMENTS(b)-1,it)=b ELSE zz0(ix,iy,*,it)=b(0:NL-1)
ENDIF
;y_e=[0,REFORM(RET_LEV_T(ix,iy,*,it)+RET_LEV_ERR_T(ix,iy,*,it))] &$
;WE_Y_E=WHERE(finite(y_e,/NAN) EQ 0)
;IF WE_Y_E(0) NE -1 THEN BEGIN
;z0_e=SPLINE([0,RET_PER],y_e,FINDGEN(999),30) &$
;b_e=INTERPOL(FINDGEN(999),z0_e,FINDGEN(FIX(MAX(z0_e))+1))
;b_e(WHERE(b_e LT 0))=!VALUES.F_NaN
;IF N_ELEMENTS(b_e) LE NL-1 THEN zz0_ERR(ix,iy,0:N_ELEMENTS(b_e)-1,it)=b_e ELSE zz0_ERR(ix,iy,*,it)=b_e(0:NL-1)
;ENDIF

ENDFOR &$ ;it

ENDIF &$ ;landmask

ENDFOR &$ ;iy
ENDFOR ;ix


RET_LEV_T=0.


;GOTO,JUMP_MASK
;====================================
;AVERAGE ON MASK (ONLY ON SIG POINTS)
;====================================

R_PER=[5,10,20,50,100]
FOR imm=0,NMASK-1 DO BEGIN &$ ;MASK
 print,'========================' &$
  print,'MASK ',MASK(imm) &$
  print,'========================' &$

WORK_LAND=landmask_r*0. &$
we_lon=WHERE(lon_r GE LON1(imm) AND lon_r LE LON2(imm)  AND lat_r GE LAT1(imm)  AND lat_r LE LAT2(imm) AND landmask_r EQ 1,COMPLEMENT=WE_NOLAND ) &$
WORK_LAND=landmask_r*0.+1 &$
WORK_LAND(we_noland)=!VALUES.F_NaN &$
print,N_ELEMENTS(WE_LON) &$

FOR ib=0,299 DO BEGIN &$ ;RET LEV
WORK=REFORM((zz0(*,*,ib,0)-zz0(*,*,ib,1))/zz0(*,*,ib,0))*WORK_LAND &$
WE=WHERE(FINITE(WORK,/NaN) EQ 0,COUNT1) &$ ; LIMIT TO RET PER=300 YEARS
IF WE(0) NE -1 THEN RET_LEV_MASK(imm,ib,0)=MEAN(WORK,/NaN) ELSE RET_LEV_MASK(imm,ib,0)=!VALUES.F_NaN&$
WORK=REFORM((zz0(*,*,ib,0)-zz0(*,*,ib,2))/zz0(*,*,ib,0))*WORK_LAND &$
WE=WHERE(FINITE(WORK,/NaN) EQ 0,COUNT1) &$ ; LIMIT TO RET PER=300 YEARS
IF WE(0) NE -1 THEN RET_LEV_MASK(imm,ib,1)=MEAN(WORK,/NaN) ELSE RET_LEV_MASK(imm,ib,1)=!VALUES.F_NaN&$
WORK=REFORM((zz0(*,*,ib,1)-zz0(*,*,ib,2))/zz0(*,*,ib,1))*WORK_LAND &$
WE=WHERE(FINITE(WORK,/NaN) EQ 0,COUNT1) &$ ; LIMIT TO RET PER=300 YEARS
IF WE(0) NE -1 THEN RET_LEV_MASK(imm,ib,2)=MEAN(WORK,/NaN) ELSE RET_LEV_MASK(imm,ib,2)=!VALUES.F_NaN&$
ENDFOR &$ ;ib

print,MASK(imm),RET_LEV_MASK_T(imm,10,2,ir)

ENDFOR ;MASK

;NB zz0_ERR=zz0+ERR
save,filename=fileo,RET_LEV_MASK

JUMP_CALC:
IF COUNT NE "" THEN restore,filename=fileo;,nx,ny,lon,lat,zz0,RET_LEV_M
;LEV_L=[24,48,96]
;zz0(WHERE( zz0 eq 0))=!VALUES.F_NaN
RET_LEV_MASK_T(*,*,*,ir)=RET_LEV_MASK
print,RET_LEV_MASK_T(0,10,2,ir)

zz0=1
;RET_LEV_MASK=1

ENDFOR ;RCM

NBINS_P=300
BINS_P=FINDGEN(NBINS_P)

;MODLES MEDIAN
print,'MODEL MEDIAN'
MED_P=FLTARR(NMASK,300,NT)*!VALUES.F_NaN
MIN_P=FLTARR(NMASK,300,NT)*!VALUES.F_NaN
MAX_P=FLTARR(NMASK,300,NT)*!VALUES.F_NaN

FOR imm=0,NMASK-1 DO BEGIN &$
FOR it=0,2 DO BEGIN &$
FOR il=0,300-1 DO BEGIN &$
MED_P(imm,il,it)=MEDIAN(RET_LEV_MASK_T(imm,il,it,*)) &$
MAx_P(imm,il,it)=MAX(RET_LEV_MASK_T(imm,il,it,*),/NaN) &$
MIN_P(imm,il,it)=MIN(RET_LEV_MASK_T(imm,il,it,*),/NaN) &$
ENDFOR &$
ENDFOR &$
ENDFOR

;=================
;PLOTS
;=================
LABEL_P=['a','b','c','d','e','f']

RET_PER=[5,10,20,50,100,300,1000]

loadct,39,RGB_TABLE=rgb

MARGIN=[0.02,0.01,0.02,0.01]

PANEL_l=['a','d','g']
PANEL_c=['b','e','j']
PANEL_r=['c','f','k']

;ALL AREAS LAND FRACTION
TICKV=[10,20,40,80]
TICKV_Y=[5,25,50,75,100]
PANEL=['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v,','w','x','y','z']

w11=WINDOW(DIMENSIONS=[900,900]) &$
FOR imm=0,NMASK-1 DO BEGIN &$

p=PLOT(BINS_P,MED_P(imm,*,2)*100.,/CURRENT,XRANGE=[10,99],color="black",YRANGE=[5,120], $
	XTITLE='HWMId',YTITLE='Land area fraction (%)',LAYOUT=[4,6,imm+1],MARGIN=[0.15,0.05,0.05,0.15],XMINOR=0,XTICKVALUES=TICKV,YTICKVALUES=TICKV_Y,YMINOR=0,THICK=2, $
	AXIS_STYLE=1,FONT_SIZE=6,/XLOG,XTICKLEN=0.02,YTICKLEN=0.02) &$
;a=MIN_P(imm,*,2)*100. &$
;a(WHERE(a EQ 0))=1E-5 &$
;b=MAX_P(imm,*,2)*100. &$
;b(WHERE(b EQ 0))=1E-5 &$
;poly_p=POLYGON([BINS_P(9:NBINS_P-1),REVERSE(BINS_P(9:NBINS_P-1))],[SMOOTH(b,5),REVERSE(SMOOTH(a,5))],/DATA,/FILL_BACKGROUND, COLOR="gray",FILL_COLOR="gray",FILL_TRANSPARENCY=80,TARGET=p) &$
ENDFOR

a=MIN_P(9:NBINS_P-1,1) &$
a(WHERE(a EQ 0))=1E-3 &$
b=MAX_P(9:NBINS_P-1,1) &$
b(WHERE(b EQ 0))=1E-3 &$
p=PLOT(BINS_P,SMOOTH(MED_P(*,1),5),color="blue",/OVERPLOT,THICK=2,/XLOG) &$
poly_p=POLYGON([BINS_P(9:NBINS_P-1),REVERSE(BINS_P(9:NBINS_P-1))],[SMOOTH(b,5),REVERSE(SMOOTH(a,5))],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p) &$

a=MIN_P(9:NBINS_P-1,2) &$
a(WHERE(a EQ 0))=1E-3 &$
b=MAX_P(9:NBINS_P-1,2) &$
b(WHERE(b EQ 0))=1E-3 &$
p=PLOT(BINS_P,SMOOTH(MED_P(*,2),5),color="red",/OVERPLOT,THICK=2,/XLOG) &$
poly_p=POLYGON([BINS_P(9:NBINS_P-1),REVERSE(BINS_P(9:NBINS_P-1))],[SMOOTH(b,5),REVERSE(SMOOTH(a,5))],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p) &$

;t=TEXT(4,20,PANEL(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$
t=TEXT(48,80,MASK(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$

;ENDFOR&$
text1=TEXT(.3,.12,'___ 1976-2005 ',/NORMAL,FONT_SIZE=12,COLOR="black",TARGET=p)
text1=TEXT(.3,.08,'___ +1.5$\circ$C',/NORMAL,FONT_SIZE=12,COLOR="blue",TARGET=p)
text1=TEXT(.3,.04,'___+2$\circ$C',/NORMAL,FONT_SIZE=12,COLOR="red",TARGET=p)
text2=POLYGON([.55,.6,.6,.55],[0.04,.04,.06,.06],/NORMAL,/FILL_BACKGROUND,FILL_COLOR="red",color="red",FILL_TRANSPARENCY=80,/OVERPLOT)
text2=text(.62,.04,'Models range',/NORMAL,FONT_SIZE=12,COLOR="red",TARGET=p)

IF ip EQ 0 THEN w11.Save,'./PLOTS/PLOT_'+STAT+'_AREA_FR_RET_'+STRMID(STRING(RET_PER(SEL(ip))),7,1)+'_.pdf',BITMAP=1,PAGE_SIZE="A4" $
	ELSE w11.Save,'./PLOTS/PLOT_'+STAT+'_AREA_FR_RET_'+STRMID(STRING(RET_PER(SEL(ip))),6,2)+'_.pdf',BITMAP=1,PAGE_SIZE="A4"
;ENDFOR
;============
STOP
;============




STOP
END
