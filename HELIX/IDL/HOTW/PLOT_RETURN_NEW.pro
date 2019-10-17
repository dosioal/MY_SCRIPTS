PRO PLOT_RETURN_NEW
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

;POPULATION
FILE_POP='population_05_'+SSP+'_int.dat'
restore,DIR_HOME+FILE_POP
nt0_int=1980
help,POPULATION_INT

NT_POP=N_ELEMENTS(REFORM(POPULATION_INT(0,0,*)))
POP_INT_NEW=FLTARR(NX,NY,NT_POP+9) ;1980--->1971
FOR ix=0,NX-1 DO BEGIN &$
FOR iy=0,NY-1 DO BEGIN &$
POP_INT_NEW(ix,iy,0:8)=POPULATION_INT(ix,iy,0)
POP_INT_NEW(ix,iy,9:NT_POP+9-1)=POPULATION_INT(ix,iy,0:NT_POP-1)
ENDFOR
ENDFOR
POPULATION=REFORM(POP_INT_NEW(*,69:69+NY_R-1,*))
HELP,POPULATION


;===============================================
; READ MODELS
;===============================================
NT=3 ;TIME PER
zz0_T=FLTARR(nx_r,ny_r,NLEV_L,NT,NRUN)*!VALUES.F_NaN
zz0_ERR_T=FLTARR(nx_r,ny_r,NLEV_L,NT,NRUN)*!VALUES.F_NaN
CC_T=FLTARR(NMASK,100,3,5,NRUN)
PP_T=FLTARR(NMASK,100,3,5,NRUN)

ic=0 ;counter for window plot
;===============================================
FOR ir=0,NRUN-1 DO BEGIN ;RCM LOOP
;===============================================

RET_LEV_T=FLTARR(nx_r,ny_r,7,3)
RET_LEV_ERR_T=FLTARR(nx_r,ny_r,7,3)
RET_LEV_MASK=FLTARR(NMASK,100,3)
CC=FLTARR(NMASK,100,3,5)
PP=FLTARR(NMASK,100,3,5)
;PP_SIG=FLTARR(NMASK,100,3,5) ; ONLY ON POINTS WHERE SIGNIFICANT
POP=FLTARR(NX_R,NY_R,3)

FILEO='./DATA/RETURN_LEV_NEW_'+RUN(ir)+'.dat'

print,'====================================='
print,'CHECKING ',fileo
print,'====================================='
count=""
count=FILE_SEARCH(FILEO)

;IF COUNT EQ 1 THEN BEGIN
IF COUNT NE "" THEN BEGIN
print,'FILE EXISTS!'
print,''
GOTO,JUMP_CALC

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
FOR ix=0,NX_R-1 DO BEGIN
FOR iy=0,NY_R-1 DO BEGIN
IF landmask_r(ix,iy) GT 0.5 THEN BEGIN &$
POP(ix,iy,0)=MEAN(POPULATION(ix,iy,5:34)) ;1976-2005
POP(ix,iy,1)=MEAN(POPULATION(ix,iy,nt_m-YEAR_INI-14:nt_m-YEAR_INI+15)) ;MIDDLE BETWEEN YC_15 AND YC_20
POP(ix,iy,2)=MEAN(POPULATION(ix,iy,nt_m-YEAR_INI-14:nt_m-YEAR_INI+15)) ;MIDDLE BETWEEN YC_15 AND YC_20
ENDIF
ENDFOR
ENDFOR

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
NCDF_VARGET,fileID,'returnlevelError',RET_LEV_ERR;,count=[nx,ny],offset=[0,0]
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
RET_LEV_ERR_T(ix,iy,ip,0)=MEAN(RET_LEV_ERR(ix,iy,ip,5:34),/NaN)   &$; 66%-->95%
RET_LEV_ERR_T(ix,iy,ip,1)=MEAN(RET_LEV_ERR(ix,iy,ip,Y_C_15(ir)-15-YEAR_INI:Y_C_15(ir)+14-YEAR_INI),/NaN)
RET_LEV_ERR_T(ix,iy,ip,2)=MEAN(RET_LEV_ERR(ix,iy,ip,Y_C_20(ir)-15-YEAR_INI:Y_C_20(ir)+14-YEAR_INI),/NaN)
ENDFOR &$
ENDIF
ENDFOR
ENDFOR


;INTERPOLATION AND REVERSE FUNCTION
print,'REVERSE FUNCTION'
zz0=FLTARR(nx_r,ny_r,NL,3)*!VALUES.F_NaN
zz0_ERR=FLTARR(nx_r,ny_r,NL,3)*!VALUES.F_NaN

FOR ix=0,nx_r-1 DO BEGIN &$
FOR iy=0,ny_r-1 DO BEGIN &$

;ix = 360 
;iy = 130

IF landmask_r(ix,iy) GT 0.5 THEN BEGIN &$
FOR it=0,2 DO BEGIN &$
;y=[REFORM(RET_LEV_T(ix,iy,*,it))] &$
y=[0,REFORM(RET_LEV_T(ix,iy,*,it))] &$
WE_Y=WHERE(finite(y,/NAN) EQ 0)
IF WE_Y(0) NE -1 THEN BEGIN
;z0=SPLINE([RET_PER],y,FINDGEN(994)+5,30) &$ ; RET PER INTERPOLATED
z0=SPLINE([0,RET_PER],y,FINDGEN(999),30) &$ ; RET PER INTERPOLATED
;b=INTERPOL(FINDGEN(994)+5,z0,FINDGEN(FIX(MAX(z0))+1))
b=INTERPOL(FINDGEN(999),z0,FINDGEN(FIX(MAX(z0))+1))
b(WHERE(b LT 0))=!VALUES.F_NaN
IF N_ELEMENTS(b) LE NL-1 THEN zz0(ix,iy,0:N_ELEMENTS(b)-1,it)=b ELSE zz0(ix,iy,*,it)=b(0:NL-1)
ENDIF
;y_e=[REFORM(RET_LEV_T(ix,iy,*,it)+RET_LEV_ERR_T(ix,iy,*,it))] &$
y_e=[0,REFORM(RET_LEV_T(ix,iy,*,it)+RET_LEV_ERR_T(ix,iy,*,it))] &$
WE_Y_E=WHERE(finite(y_e,/NAN) EQ 0)
IF WE_Y_E(0) NE -1 THEN BEGIN
;z0_e=SPLINE([RET_PER],y_e,FINDGEN(994)+5,30) &$
z0_e=SPLINE([0,RET_PER],y_e,FINDGEN(999),30) &$
;b_e=INTERPOL(FINDGEN(994)+5,z0_e,FINDGEN(FIX(MAX(z0_e))+1))
b_e=INTERPOL(FINDGEN(999),z0_e,FINDGEN(FIX(MAX(z0_e))+1))
b_e(WHERE(b_e LT 0))=!VALUES.F_NaN
IF N_ELEMENTS(b_e) LE NL-1 THEN zz0_ERR(ix,iy,0:N_ELEMENTS(b_e)-1,it)=b_e ELSE zz0_ERR(ix,iy,*,it)=b_e(0:NL-1)
ENDIF

;a=[0,REFORM(RET_LEV_T(ix,iy,*,0,ir))]
;aaz=SPLINE([0,RET_PER],a,FINDGEN(999),30)
;b=[0,REFORM(RET_LEV_T(ix,iy,*,1,ir))]
;bbz=SPLINE([0,RET_PER],b,FINDGEN(999),30)
;zzzb=SPLINE(bbz,FINDGEN(999),FINDGEN(100),0)
;INTERPOL(zzzb,findgen(100),aaz(99))

;IF ix EQ 360 AND iy EQ 130 THEN BEGIN
;!p.multi=0
;!p.color=0
;!p.background=255
;IF it EQ 2 THEN BEGIN
;window,0,retain=2
;plot,FINDGEN(300),zz0(ix,iy,*,it),color=250,xrange=[0,150],yrange=[0,300]
;oplot,y,[0,RET_PER]     
;oplot,z0,FINDGEN(999),color=50
;oplot,FINDGEN(300),zz0_ERR(ix,iy,*,it),color=250,linestyle=2
;oplot,y_e,[0,RET_PER],linestyle=2     
;oplot,z0_e,FINDGEN(999),color=50,linestyle=2
;oplot,FINDGEN(300),zz0(ix,iy,*,1),color=250
;oplot,FINDGEN(300),zz0_ERR(ix,iy,*,1),color=250,linestyle=2

;
;window,1,retain=2
;plot,FINDGEN(95),zz0_err(ix,iy,*,it),color=250,xrange=[5,25],yrange=[0,1000]
;oplot,y_e,RET_PER     
;oplot,z0_e,FINDGEN(994)+5,color=50
;ENDIF
;ENDIF

ENDFOR &$ ;it

ENDIF &$ ;landmask

ENDFOR &$ ;iy
ENDFOR ;ix


RET_PER_INT=FINDGEN(NL-5)

RET_LEV_T=0.
RET_LEV_T_ERR=0.


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

AREA_Y=FLTARR(NY_R) &$
CORR=FLTARR(NY_R) &$
;CALC AREA TOT IN KM
FOR iy=0,ny_r-1 DO BEGIN &$
CORR(iy)=SIN((LAT_R(0,iy)+.25)*!DtoR)-SIN((LAT_R(0,iy)-.25)*!DtoR) &$
AREA_Y(iy)=TOTAL(WORK_LAND(*,iy)*CORR(iy),/NaN) &$
ENDFOR &$

AREA_MASK=TOTAL(AREA_Y,/NaN) &$

FOR it=0,2 DO BEGIN &$
FOR ib=0,99 DO BEGIN &$ ;RET LEV
WORK=REFORM(zz0(*,*,ib,it))*WORK_LAND &$
WORK_ERR=REFORM(zz0_ERR(*,*,ib,it))*WORK_LAND &$
WORK(WHERE( WORK GT 300))=!VALUES.F_NaN &$
WE=WHERE(FINITE(WORK,/NaN) EQ 0 AND WORK LE 300.,COUNT1) &$ ; LIMIT TO RET PER=300 YEARS
IF WE(0) NE -1 THEN RET_LEV_MASK(imm,ib,it)=MEDIAN(WORK(WHERE(FINITE(WORK,/NaN) EQ 0))) ELSE RET_LEV_MASK(imm,ib,it)=!VALUES.F_NaN&$
CC_Y=FLTARR(NY_R) &$
FOR ip=0,4 DO BEGIN &$ ;RET PER
FOR iy=0,NY_R-1 DO BEGIN &$
WE_Y=WHERE(FINITE(WORK(*,iy),/NaN) EQ 0 AND WORK(*,iy) LE R_PER(ip) ,COUNT1) &$ ; LIMIT TO RET PER=100 YEARS
CC_Y(iy)=DOUBLE(COUNT1)*CORR(iy) &$;/N_ELEMENTS(WE_LON)*100. &$
ENDFOR &$
CC(imm,ib,it,ip)=TOTAL(CC_Y)/AREA_MASK*100. &$
;POPULATION COUNT
WE_P=WHERE(FINITE(WORK,/NaN) EQ 0 AND WORK LE R_PER(ip) ,COUNT2) &$ ; LIMIT TO RET PER=100 YEARS
POP_W=REFORM(POP(*,*,it))
PP(imm,ib,it,ip)=TOTAL(POP_W(WE_P))
;POPULATION COUNT ON SIG
;WE_P=WHERE(FINITE(WORK,/NaN) EQ 0 AND WORK LE R_PER(ip) ,COUNT2) &$ ; LIMIT TO RET PER=100 YEARS
;POP_W=REFORM(POP(*,*,it))
;PP(imm,ib,it,ip)=TOTAL(POP_W(WE_P))
ENDFOR &$ ;ip
ENDFOR &$ ;ib
ENDFOR  &$ ;it

;POPULATION


;window,imm,retain=2,title=MASK(imm),xsize=500,ysize=1200 &$
;!p.multi=[0,1,6] &$
;PLOT,FINDGEN(100),RET_LEV_MASK(imm,*,1),xrange=[12,100],xtickv=[12,24,48,96],/XLOG,yrange=[0,100] &$
;OPLOT,FINDGEN(100),RET_LEV_MASK(imm,*,2),color=250    &$
;OPLOT,FINDGEN(100),RET_LEV_MASK(imm,*,1),color=50  &$
;OPLOT,FINDGEN(100),RET_LEV_MASK(imm,*,0),color=0  &$
;FOR ip=0,4 DO BEGIN &$
;PLOT,FINDGEN(100),CC(imm,*,0,ip),xrange=[12,100],xtickv=[12,24,48,96],/XLOG,YRANGE=[0,100] &$
;OPLOT,FINDGEN(100),CC(imm,*,1,ip),color=50 &$
;OPLOT,FINDGEN(100),CC(imm,*,2,ip),color=250  &$
;ENDFOR &$

ENDFOR ;MASK

;NB zz0_ERR=zz0+ERR
save,filename=fileo,nx_r,ny_r,landmask_r,lon_r,lat_r,zz0,zz0_ERR,CC,RET_LEV_MASK,PP

JUMP_CALC:
IF COUNT NE "" THEN restore,filename=fileo;,nx,ny,lon,lat,zz0,RET_LEV_M
;LEV_L=[24,48,96]
;zz0(WHERE( zz0 eq 0))=!VALUES.F_NaN
FOR il=0,NLEV_L-1 DO BEGIN
zz0_T(*,*,il,*,ir)=zz0(*,*,LEV_L(il),*)
zz0_ERR_T(*,*,il,*,ir)=zz0_ERR(*,*,LEV_L(il),*)
ENDFOR

CC_T(*,*,*,*,ir)=CC
PP_T(*,*,*,*,ir)=PP

zz0=1
zz0_ERR=1
CC=1
RET_LEV_MASK=1

JUMP_MASK:
ENDFOR ;RCM

;MODLES MEDIAN
print,'MODEL MEDIAN'
zz0_M=FLTARR(nx_r,ny_r,NLEV_L,3)*!VALUES.F_NaN
FOR il=0,NLEV_L-1 DO BEGIN &$
FOR it=0,2 DO BEGIN &$
FOR ix=0,nx_r-1 DO BEGIN &$
FOR iy=0,ny_r-1 DO BEGIN &$
IF landmask_r(ix,iy) GT 0.5 THEN BEGIN &$
;WORK=REFORM(zz0_T(ix,iy,il,it,*))
;WORK(WHERE(WORK EQ 0))=!VALUES.F_NaN
zz0_M(ix,iy,il,it)=MEDIAN(zz0_T(ix,iy,il,it,*)) &$
;zz0_M(ix,iy,il,it)=MEDIAN(WORK) &$
ENDIF
ENDFOR &$
ENDFOR &$
WORK=REFORM(zz0_M(*,*,il,it)) &$
;WORK(WHERE(FINITE(WORK,/NaN) EQ 1))=0 &$
WORK(WHERE(WORK GT 300))=!VALUES.F_NaN &$
zz0_M(*,*,il,it)=WORK &$
ENDFOR &$
ENDFOR

;MODLES ERR AGREEMENT
print,'MODEL ERR AGR'
MOD_AGR_zz0=FLTARR(nx_r,ny_r,NLEV_L,3)
FOR ip=0,NLEV_L-1 DO BEGIN &$
FOR ix=0,Nx_r-1 DO BEGIN &$
FOR iy=0,Ny_r-1 DO BEGIN &$
IF landmask_r(ix,iy) GT 0.5 THEN BEGIN &$
FOR ir=0,NRUN -1 DO BEGIN
IF (FINITE(zz0_ERR_T(ix,iy,ip,0,ir),/NaN) EQ 1 AND FINITE(zz0_ERR_T(ix,iy,ip,1,ir),/NaN) EQ 0) OR zz0_T(ix,iy,ip,1,ir) LE zz0_ERR_T(ix,iy,ip,0,ir) THEN MOD_AGR_zz0(ix,iy,ip,0)=MOD_AGR_zz0(ix,iy,ip,0)+1 &$
IF (FINITE(zz0_ERR_T(ix,iy,ip,0,ir),/NaN) EQ 1 AND FINITE(zz0_ERR_T(ix,iy,ip,2,ir),/NaN) EQ 0) OR zz0_T(ix,iy,ip,2,ir) LE zz0_ERR_T(ix,iy,ip,0,ir) THEN MOD_AGR_zz0(ix,iy,ip,1)=MOD_AGR_zz0(ix,iy,ip,1)+1 &$
IF (FINITE(zz0_ERR_T(ix,iy,ip,1,ir),/NaN) EQ 1 AND FINITE(zz0_ERR_T(ix,iy,ip,2,ir),/NaN) EQ 0)  OR zz0_T(ix,iy,ip,2,ir) LE zz0_ERR_T(ix,iy,ip,1,ir) THEN MOD_AGR_zz0(ix,iy,ip,2)=MOD_AGR_zz0(ix,iy,ip,2)+1 &$
ENDFOR
ENDIF
ENDFOR
ENDFOR
ENDFOR

STOP

; SAVE TXT FILES
FILEO_W='./HWMDI_pres_RET_20.txt'
close,2
openw,2,FILEO_W
printf,2,nx_r,ny_r
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,zz0_M(*,iy,0,0),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LON_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LAT_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR

FILEO_W='./HWMDI_pres_RET_40.txt'
close,2
openw,2,FILEO_W
printf,2,nx_r,ny_r
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,zz0_M(*,iy,1,0),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LON_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LAT_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR

FILEO_W='./HWMDI_pres_RET_80.txt'
close,2
openw,2,FILEO_W
printf,2,nx_r,ny_r
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,zz0_M(*,iy,2,0),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LON_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LAT_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR

FILEO_W='./HWMDI_15C_RET_20.txt'
close,2
openw,2,FILEO_W
printf,2,nx_r,ny_r
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,zz0_M(*,iy,0,1),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LON_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LAT_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR

FILEO_W='./HWMDI_15C_RET_40.txt'
close,2
openw,2,FILEO_W
printf,2,nx_r,ny_r
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,zz0_M(*,iy,1,1),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LON_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LAT_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR

FILEO_W='./HWMDI_15C_RET_80.txt'
close,2
openw,2,FILEO_W
printf,2,nx_r,ny_r
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,zz0_M(*,iy,2,1),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LON_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LAT_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR

FILEO_W='./HWMDI_2C_RET_20.txt'
close,2
openw,2,FILEO_W
printf,2,nx_r,ny_r
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,zz0_M(*,iy,0,2),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LON_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LAT_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR

FILEO_W='./HWMDI_2C_RET_40.txt'
close,2
openw,2,FILEO_W
printf,2,nx_r,ny_r
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,zz0_M(*,iy,1,2),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LON_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LAT_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR

FILEO_W='./HWMDI_2C_RET_80.txt'
close,2
openw,2,FILEO_W
printf,2,nx_r,ny_r
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,zz0_M(*,iy,2,2),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LON_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR
FOR iy=0,NY_R-1 DO BEGIN &$
printf,2,LAT_R(*,iy),FORMAT='(720F12.6)' &$
ENDFOR


;=================
;PLOTS

;=================
;PLOTS

;=================
;PLOTS
;=================
LABEL_P=['a','b','c','d','e','f']

RET_PER=[5,10,20,50,100,300,1000]

loadct,39,RGB_TABLE=rgb
levels=[0,12,24,48,96,192,384]/2
levels=[0,5,10,20,40,80,160]/2
colors=[255,100,150,190,210,250]

pattern_r = Obj_New('idlgrpattern', 1, ORIENTATION=-45,SPACING=4,THICK=0.8)

loadct,39,RGB_TABLE=rgb
levels=[1,5,10,20,50,100,300]
colors=REVERSE([35,55,100,150,190,250])
levels=[0,5,10,20,30,50,100,1000]
colors=REVERSE([10,35,55,100,150,190,250])
;levels=[1E-3,5,10,20,30,50,100,300]
;colors=REVERSE([255,35,55,100,150,190,250])

levels_d=REVERSE([1,5,10,20,50,100,300]*(-1))
colors_d=REVERSE(([35,55,100,150,190,250]))

MARGIN=[0.02,0.01,0.02,0.01]
;LEV_L=[24,48,96]

PANEL_l=['a','d','g']
PANEL_c=['b','e','j']
PANEL_r=['c','f','k']

w1 = WINDOW(WINDOW_TITLE='Median',DIMENSIONS=[1200,900])
FOR il=0,NLEV_L-1 DO BEGIN &$
w1_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,NLEV_L,il*3+1],MARGIN=MARGIN, $
        title='HWMId $\geq$'+STRMID(LEV_L(il),6,2)+', 1976-2005',/CURRENT) &$
c1_plot= CONTOUR(zz0_M(*,*,il,0),lon_r,lat_r,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
mc = MAPCONTINENTS(THICK=0.5) &$
t=TEXT(0.1,1,PANEL_l(il),/RELATIVE,FONT_SIZE=14,TARGET=w1_plot) &$

w1_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,NLEV_L,il*3+2],MARGIN=MARGIN, $
        title='HWMId $\geq$'+STRMID(LEV_L(il),6,2)+', +1.5$\circ$C',/CURRENT) &$
c1_plot= CONTOUR(zz0_M(*,*,il,1),lon_r,lat_r,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
SIG=REFORM(MOD_AGR_zz0(*,*,il,0)) &$
SIG(WHERE(zz0_M(*,*,il,1) GT 300))=!VALUES.F_NaN &$
SIG(WHERE(FINITE(zz0_M(*,*,il,1),/NaN) EQ 1))=!VALUES.F_NaN &$
s_plot= CONTOUR(SIG,lon_r,lat_r,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2) &$
mc = MAPCONTINENTS(THICK=0.5) &$
t=TEXT(0.1,1,PANEL_c(il),/RELATIVE,FONT_SIZE=14,TARGET=w1_plot) &$

w1_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[3,NLEV_L,il*3+3],MARGIN=MARGIN, $
        title='HWMId $\geq$'+STRMID(LEV_L(il),6,2)+', +2$\circ$C',/CURRENT) &$
c1_plot= CONTOUR(zz0_M(*,*,il,2),lon_r,lat_r,c_value=levels,RGB_INDICES=colors,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
SIG=REFORM(MOD_AGR_zz0(*,*,il,1)) &$
SIG(WHERE(zz0_M(*,*,il,2) GT 300))=!VALUES.F_NaN &$
SIG(WHERE(FINITE(zz0_M(*,*,il,2),/NaN) EQ 1))=!VALUES.F_NaN &$
s_plot= CONTOUR(SIG,lon_r,lat_r,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2) &$
mc = MAPCONTINENTS(THICK=0.5) &$
t=TEXT(0.1,1,PANEL_r(il),/RELATIVE,FONT_SIZE=14,TARGET=w1_plot) &$
ENDFOR

tickname = STRING(levels, FORMAT='(f7.0)') &$
;cb0 = Colorbar(TARGET=c1_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[.1,.04,.55,.05],FONT_SIZE=8,TITLE='Return level (years)')
cb0 = Colorbar(TARGET=c1_plot,BORDER=1,TAPER=1,TICKNAME=tickname,position=[.05,.04,.45,.05],FONT_SIZE=8,TITLE='Return period (years)')

box=POLYGON([.49,.52,.52,.49],[0.03,.03,.05,.05],/NORMAL,/FILL_BACKGROUND,FILL_COLOR="black",PATTERN_ORIENTATION=-45,PATTERN_SPACING=4,PATTERN_THICK=0.8)
text2=text(.54,.03,'Significant change compared to reference period',/NORMAL,FONT_SIZE=12,COLOR="black",TARGET=w1)

w1.Save,'./PLOTS/PLOT_'+STAT+'_RET-PER_SCEN_NEW_NEW.pdf',BITMAP=1,PAGE_SIZE="A4"

;STOP

levels_d=[0,0.2,0.4,0.6,0.8,1]*100.
colors_d=[20,40,100,150,190]
pattern_l = Obj_New('idlgrpattern', 1, ORIENTATION=45,SPACING=4,THICK=0.8)

;PLOT 2-1.5 DIFF
w12= WINDOW(WINDOW_TITLE='Median',DIMENSIONS=[1200,350])
MARGIN=[0.02,0.01,0.02,0.01]
w12= WINDOW(WINDOW_TITLE='Median',DIMENSIONS=[600,1000])
MARGIN=[0.02,0.22,0.02,0.1]
FOR il=0,NLEV_L-1 DO BEGIN &$
w1_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
        ;FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[NLEV_L,1,il+1],MARGIN=MARGIN, $
        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[1,NLEV_L,il+1],MARGIN=MARGIN, $
        title='HWMId $\geq$'+STRMID(LEV_L(il),6,2)+', 2$\circ$C vs. 1.5$\circ$C',/CURRENT) &$
c1_plot= CONTOUR((zz0_M(*,*,il,1)-zz0_M(*,*,il,2))/zz0_M(*,*,il,1)*100.,lon_r,lat_r,c_value=(levels_d),RGB_INDICES=colors_d,/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
SIG=REFORM(MOD_AGR_zz0(*,*,il,2)) &$
SIG(WHERE(zz0_M(*,*,il,2) GT 300))=!VALUES.F_NaN &$
SIG(WHERE(FINITE(zz0_M(*,*,il,2),/NaN) EQ 1))=!VALUES.F_NaN &$
s_plot= CONTOUR(SIG,lon_r,lat_r,c_value=[4,7],/OVERPLOT,C_FILL_PATTERN=pattern_r,/FILL,COLOR="black",GRID_UNITS=2) &$
mc = MAPCONTINENTS(THICK=0.5) &$

WORK1=REFORM(zz0_M(*,*,il,1)) &$
WORK2=REFORM(zz0_M(*,*,il,2)) &$
W_PLOT=WORK1*0. &$
W_PLOT(WHERE(FINITE(WORK1,/NaN) EQ 1 AND WORK2 LE 100))=1 &$
;s1_plot= CONTOUR(W_PLOT,lon_r,lat_r,c_value=[0,1],/OVERPLOT,C_FILL_PATTERN=pattern_l,/FILL,COLOR="black",GRID_UNITS=2) &$
s1_plot= CONTOUR(W_PLOT,lon_r,lat_r,c_value=[0,1],/OVERPLOT,/FILL,COLOR="red",GRID_UNITS=2) &$

;w1_plot= image(landmask, GRID_UNITS=2,IMAGE_LOCATION=[-180,-90],IMAGE_DIMENSIONS=[360,180], $
;        TRANSPARENCY=93,MAP_PROJECTION='Robinson',GRID_LATITUDE=0,GRID_LONGITUDE=0, $
;        FONT_SIZE=10,FONT_STYLE=1,AXIS_STYLE=1,LAYOUT=[2,NLEV_L,il*2+1],MARGIN=MARGIN, $
;        title='HWMId $\geq$'+STRMID(LEV_L(il),6,2)+', +2$\circ$C - 1.5$\circ$C',/CURRENT) &$
;c1_plot= CONTOUR(W_PLOT,lon_r,lat_r,c_value=[0,1],/FILL,RGB_TABLE=rgb,/OVERPLOT,GRID_UNITS=2) &$
mc = MAPCONTINENTS(THICK=0.5) &$
t=TEXT(0.,1.02,LABEL_P(il),/RELATIVE,FONT_SIZE=14,TARGET=w1_plot) &$
ENDFOR
tickname = STRING(levels_d, FORMAT='(I5)') &$
cb0 = Colorbar(TARGET=c1_plot,BORDER=1,TAPER=0,TICKNAME=tickname,position=[.05,.03,.45,.05],FONT_SIZE=8,TITLE='Reduction (%) in return period')

box=POLYGON([.5,.55,.55,.5],[0.03,0.03,0.05,0.05],/NORMAL,/FILL_BACKGROUND,FILL_COLOR="black",PATTERN_ORIENTATION=-45,PATTERN_SPACING=4,PATTERN_THICK=0.8)
text2=text(.56,.04,'Change is significant',/NORMAL,FONT_SIZE=8,COLOR="black",TARGET=w12)
box=POLYGON([.75,.8,.8,.75],[0.03,.03,0.05,0.05],/NORMAL,/FILL_BACKGROUND,FILL_COLOR="red")
text2=text(.81,.04,'New heatwaves',/NORMAL,FONT_SIZE=8,COLOR="black",TARGET=w12)

w12.Save,'./PLOTS/PLOT_'+STAT+'_RET-PER_SCEN_DIFF.pdf',BITMAP=1,PAGE_SIZE="A4"


R_PER=[5,10,20,50,100]
NP=N_ELEMENTS(R_PER)

;FOR imm=0,NMASK-1 DO BEGIN &$
;window,imm,retain=2,title=MASK(imm),xsize=800,ysize=600 &$
;!p.multi=[0,3,2] &$
;;PLOT,FINDGEN(100),RET_LEV_MASK(imm,*,1),xrange=[12,100],xtickv=[12,24,48,96],/XLOG,yrange=[0,100] &$
;;OPLOT,FINDGEN(100),RET_LEV_MASK(imm,*,2),color=250    &$
;;OPLOT,FINDGEN(100),RET_LEV_MASK(imm,*,1),color=50  &$
;;OPLOT,FINDGEN(100),RET_LEV_MASK(imm,*,0),color=0  &$
;FOR ip=0,NP-1 DO BEGIN &$
;PLOT,FINDGEN(100),PP_T(imm,*,0,ip,0)/(PP_T(imm,1,0,ip,0))*100.,xrange=[12,102],xtickv=[12,24,48,96],/XLOG,/YLOG,YRANGE=[10,100],TITLE=R_PER(ip),xstyle=1,ystyle=1 &$
;FOR ir=0,NRUN-1 DO BEGIN &$
;OPLOT,FINDGEN(100),PP_T(imm,*,0,ip,ir)/(PP_T(imm,1,0,ip,ir))*100. &$
;OPLOT,FINDGEN(100),PP_T(imm,*,1,ip,ir)/(PP_T(imm,1,1,ip,ir))*100.,color=50 &$
;OPLOT,FINDGEN(100),PP_T(imm,*,2,ip,ir)/(PP_T(imm,1,2,ip,ir))*100.,color=250  &$
;ENDFOR &$
;ENDFOR &$
;ENDFOR

;====================
;PLOT ONLY WORLD
;====================

imm=21 ;WORLD
BINS_P=FINDGEN(100)
NBINS_P=100

MAX_P=FLTARR(NBINS_P,NP,3) &$
MIN_P=FLTARR(NBINS_P,NP,3) &$
MED_P=FLTARR(NBINS_P,NP,3) &$
MED_P_ASS=FLTARR(NBINS_P,NP,3) &$
FOR ip=0,NP-1 DO BEGIN &$
FOR it=0,2 DO BEGIN &$
FOR ib=0,NBINS_P-1 DO BEGIN &$
WORK=FLTARR(NRUN) &$
WORK_A=FLTARR(NRUN) &$
FOR ir=0,NRUN-1 DO BEGIN &$
WORK(ir)=REFORM(PP_T(imm,ib,it,ip,ir)/(PP_T(imm,1,it,ip,ir))*100.) &$
ENDFOR &$
MAX_P(ib,ip,it)=MAX(WORK) &$
MIN_P(ib,ip,it)=MIN(WORK) &$
MED_P(ib,ip,it)=MEAN(WORK) &$
MED_P_ASS(ib,ip,it)=MEAN(REFORM(PP_T(imm,ib,it,ip,*))) &$
;MIN_P(ib,it,ip)=MIN(REFORM(PP_T(imm,ib,it,ip,*)/(PP_T(imm,1,it,ip,ir))*100.)) &$
;MED_P(ib,it,ip)=MEDIAN(REFORM(PP_T(imm,ib,it,ip,*)/(PP_T(imm,1,it,ip,ir))*100.)) &$
ENDFOR &$
ENDFOR &$
ENDFOR

;DIFF
MAX_D=FLTARR(NBINS_P,NP) &$
MIN_D=FLTARR(NBINS_P,NP) &$
MED_D=FLTARR(NBINS_P,NP) &$
MED_D_ASS=FLTARR(NBINS_P,NP) &$

FOR ip=0,NP-1 DO BEGIN &$
FOR ib=0,NBINS_P-1 DO BEGIN &$
WORK_1=FLTARR(NRUN) &$
WORK_2=FLTARR(NRUN) &$
WORK=FLTARR(NRUN) &$
FOR ir=0,NRUN-1 DO BEGIN &$
WORK_2(ir)=REFORM(PP_T(imm,ib,2,ip,ir)/(PP_T(imm,1,2,ip,ir))*100.) &$
WORK_1(ir)=REFORM(PP_T(imm,ib,1,ip,ir)/(PP_T(imm,1,1,ip,ir))*100.) &$
WORK(ir)=WORK_2(ir)-WORK_1(ir) &$
ENDFOR &$
MAX_D(ib,ip)=MAX(WORK) &$
MIN_D(ib,ip)=MIN(WORK) &$
MED_D(ib,ip)=MEAN(WORK) &$
MED_D_ASS(ib,ip)=MEAN(REFORM(PP_T(imm,ib,2,ip,*)-PP_T(imm,ib,1,ip,*))) &$
ENDFOR &$
ENDFOR


TICKV=[12,24,48,96]
TICKV=[10,20,40,80]
TICKV_Y=[5,25,50,75,100]
LABEL_P=['a','b','c','d','e','f']

SEL=[0,2,3] ;RET_PER=5,20,50
NPP=3 ; nr of ret per plotted

w6=WINDOW(WINDOW_TITLE=STAT,DIMENSIONS=[900,900])
FOR ip=0,NPP-1 DO BEGIN &$
p=PLOT(BINS_P,SMOOTH(MED_P(*,SEL(ip),0),3),/CURRENT,TITLE='Return period $\leq$'+STRING(RET_PER(SEL(ip)))+' years',XRANGE=[10,99],color="black",YRANGE=[5,130], $
	XTITLE='HWMId',YTITLE='World population (%)',LAYOUT=[2,NPP,2*ip+1],MARGIN=[0.25,0.1,0.05,0.1],XMINOR=0,XTICKVALUES=TICKV,YTICKVALUES=TICKV_Y,YMINOR=0,THICK=2, $
	AXIS_STYLE=2,FONT_SIZE=8,/XLOG,/YLOG,XTICKLEN=0.02,YTICKLEN=0.02) &$
a=MIN_P(1:NBINS_P-1,SEL(ip),0) &$
a(WHERE(a EQ 0))=1E-3 &$
poly_p=POLYGON([BINS_P(1:NBINS_P-1),REVERSE(BINS_P(1:NBINS_P-1))],[SMOOTH(MAX_P(1:NBINS_P-1,SEL(ip),0),3),REVERSE(SMOOTH(a,3))],/DATA,/FILL_BACKGROUND, COLOR="gray",FILL_COLOR="gray",FILL_TRANSPARENCY=80,TARGET=p) &$

p=PLOT(BINS_P,SMOOTH(MED_P(*,SEL(ip),1),3),color="blue",/OVERPLOT,THICK=2,/XLOG) &$
poly_p=POLYGON([BINS_P(9:NBINS_P-1),REVERSE(BINS_P(9:NBINS_P-1))],[SMOOTH(MAX_P(9:NBINS_P-1,SEL(ip),1),3),REVERSE(SMOOTH(MIN_P(9:NBINS_P-1,SEL(ip),1),3))],/DATA,/FILL_BACKGROUND, COLOR="blue",FILL_COLOR="blue",FILL_TRANSPARENCY=80,TARGET=p) &$

p=PLOT(BINS_P,SMOOTH(MED_P(*,SEL(ip),2),3),color="red",/OVERPLOT,THICK=2,/XLOG) &$
poly_p=POLYGON([BINS_P(9:NBINS_P-1),REVERSE(BINS_P(9:NBINS_P-1))],[SMOOTH(MAX_P(9:NBINS_P-1,SEL(ip),2),3),REVERSE(SMOOTH(MIN_P(9:NBINS_P-1,SEL(ip),2),3))],/DATA,/FILL_BACKGROUND, COLOR="red",FILL_COLOR="red",FILL_TRANSPARENCY=80,TARGET=p) &$
IF ip EQ 0  THEN BEGIN &$
t1=TEXT(50,6,'1976-2005',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p) &$
t2=TEXT(50,8,'+1.5 $\circ$C',/DATA,COLOR="Blue",FONT_SIZE=10,TARGET=p) &$
t3=TEXT(50,10,'+2 $\circ$C',/DATA,COLOR="Red",FONT_SIZE=10,TARGET=p) &$
ENDIF &$
p_s=TEXT(bins_P(LEV_L(0)-1),85,STRMID(MED_P_ASS(LEV_L(0)-1,SEL(ip),1)-MED_P_ASS(LEV_L(2)-1,SEL(ip),0),5,5),/DATA,COLOR="blue",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins_P(LEV_L(1)-1)-2,85,STRMID(MED_P_ASS(LEV_L(1)-1,SEL(ip),1)-MED_P_ASS(LEV_L(1)-1,SEL(ip),0),5,5),/DATA,COLOR="blue",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins_P(LEV_L(2)-1)-2,85,STRMID(MED_P_ASS(LEV_L(2)-1,SEL(ip),1)-MED_P_ASS(LEV_L(2)-1,SEL(ip),0),5,5),/DATA,COLOR="blue",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins_P(LEV_L(0)-1),105,STRMID(MED_P_ASS(LEV_L(0)-1,SEL(ip),2)-MED_P_ASS(LEV_L(0)-1,SEL(ip),0),5,5),/DATA,COLOR="red",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins_P(LEV_L(1)-1)-2,105,STRMID(MED_P_ASS(LEV_L(1)-1,SEL(ip),2)-MED_P_ASS(LEV_L(1)-1,SEL(ip),0),5,5),/DATA,COLOR="red",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins_P(LEV_L(2)-1)-2,105,STRMID(MED_P_ASS(LEV_L(2)-1,SEL(ip),2)-MED_P_ASS(LEV_L(2)-1,SEL(ip),0),5,5),/DATA,COLOR="red",TARGET=p,FONT_SIZE=8) &$

t=TEXT(0.,1.02,LABEL_P(2*ip),/RELATIVE,FONT_SIZE=14,TARGET=p) &$

;DIFF
p=PLOT(BINS_P,SMOOTH(MED_D(*,SEL(ip)),3),/CURRENT,XRANGE=[12,99],color="green",YRANGE=[5,120], $
	        XTITLE='HWMId',YTITLE='Difference in population (%)',LAYOUT=[2,NPP,2*ip+2],MARGIN=[0.15,0.1,0.05,0.1],XMINOR=0,XTICKVALUES=TICKV,YTICKVALUES=TICKV_Y,YMINOR=0,THICK=2, $
	        AXIS_STYLE=2,FONT_SIZE=8,/XLOG,/YLOG,XTICKLEN=0.02,YTICKLEN=0.02) &$
poly_p=POLYGON([BINS_P(9:NBINS_P-1),REVERSE(BINS_P(9:NBINS_P-1))],[SMOOTH(MAX_D(9:NBINS_P-1,SEL(ip)),3),REVERSE(SMOOTH(MIN_D(9:NBINS_P-1,SEL(ip)),3))],/DATA,/FILL_BACKGROUND, COLOR="green",FILL_COLOR="green",FILL_TRANSPARENCY=80,TARGET=p) &$

DIFF1=MED_D_ASS(*,SEL(ip)) &$
;p_s=TEXT(bins_P(23)-4,2E4,STRMID(DIFF1(23),5,5)+' ('+STRMID(SIG_P(8,2),6,3)+')',/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$
;p_s=TEXT(bins_P(47)-8,2E4,STRMID(DIFF1(47),5,5)+' ('+STRMID(SIG_P(16,2),6,3)+')',/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$
;p_s=TEXT(bins_P(95)-23,2E4,STRMID(DIFF1(95),5,5)+' ('+STRMID(SIG_P(32,2),6,3)+')',/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins_P(LEV_L(0)-1),85,STRMID(DIFF1(LEV_L(0)-1),5,5),/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins_P(LEV_L(1)-1)-2,85,STRMID(DIFF1(LEV_L(1)-1),5,5),/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$
p_s=TEXT(bins_P(LEV_L(2)-1)-2,85,STRMID(DIFF1(LEV_L(2)-1),5,5),/DATA,COLOR="green",TARGET=p,FONT_SIZE=8) &$

IF ip EQ 0  THEN BEGIN &$
poly_p=POLYGON([MIN(EA2007),MAX(EA2007),MAX(EA2007),MIN(EA2007)],[5,5,75,75],/DATA,/FILL_BACKGROUND, COLOR="black",FILL_COLOR="black",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,1]) &$
t1=TEXT(23,28,'Balkans 2007',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p,ORIENTATION=90) &$

poly_p=POLYGON([MIN(FR2003),MAX(FR2003),MAX(FR2003),MIN(FR2003)],[5,5,75,75],/DATA,/FILL_BACKGROUND, COLOR="black",FILL_COLOR="black",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,1]) &$
t2=TEXT(38,28,'France 2003',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p,ORIENTATION=90) &$

poly_p=POLYGON([MIN(USA1980,/NaN),MAX(USA1980,/NaN),MAX(USA1980,/NaN),MIN(USA1980,/NaN)],[5,5,75,75],/DATA,/FILL_BACKGROUND, COLOR="black",FILL_COLOR="black",FILL_TRANSPARENCY=80,TARGET=p) &$
t3=TEXT(50,28,'USA 1980',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p,ORIENTATION=90) &$

poly_p=POLYGON([MIN(RU2010),MAX(RU2010),MAX(RU2010),MIN(RU2010)],[5,5,75,75],/DATA,/FILL_BACKGROUND, COLOR="black",FILL_COLOR="black",FILL_TRANSPARENCY=80,TARGET=p,LAYOUT=[2,1,1]) &$
t4=TEXT(79,28,'Russia 2010',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p,ORIENTATION=90) &$

t5=TEXT(12,6,'+2 $\circ$C - 1.5 $\circ$C',/DATA,COLOR="green",FONT_SIZE=10,TARGET=p) &$
ENDIF &$

t=TEXT(0.,1.02,LABEL_P(2*ip+1),/RELATIVE,FONT_SIZE=14,TARGET=p) &$

ENDFOR ;ip
print,MED_P(LEV_L,SEL(0),0),MIN_P(LEV_L,SEL(0),0),MAX_P(LEV_L,SEL(0),0)
print,MED_P(LEV_L,SEL(0),1),MIN_P(LEV_L,SEL(0),1),MAX_P(LEV_L,SEL(0),1)
print,MED_P(LEV_L,SEL(0),2),MIN_P(LEV_L,SEL(0),2),MAX_P(LEV_L,SEL(0),2)
print,MED_P(LEV_L,SEL(1),0),MIN_P(LEV_L,SEL(1),0),MAX_P(LEV_L,SEL(1),0)
print,MED_P(LEV_L,SEL(1),1),MIN_P(LEV_L,SEL(1),1),MAX_P(LEV_L,SEL(1),1)
print,MED_P(LEV_L,SEL(1),2),MIN_P(LEV_L,SEL(1),2),MAX_P(LEV_L,SEL(1),2)
print,MED_P(LEV_L,SEL(2),0),MIN_P(LEV_L,SEL(2),0),MAX_P(LEV_L,SEL(2),0)
print,MED_P(LEV_L,SEL(2),1),MIN_P(LEV_L,SEL(2),1),MAX_P(LEV_L,SEL(2),1)
print,MED_P(LEV_L,SEL(2),2),MIN_P(LEV_L,SEL(2),2),MAX_P(LEV_L,SEL(2),2)

w6.Save,'./PLOTS/PLOT_'+STAT+'_RET-PER_POP_NEW.pdf',BITMAP=1,PAGE_SIZE="A4"

sTOP


;ALL AREAS POPULATION
TICKV_Y=[5,10,100,1000]
TICKV_Y=[5,25,50,75,100]
FOR ip=0,NPP-1 DO BEGIN &$
w1p=WINDOW(WINDOW_TITLE=STRING(RET_PER(SEL(ip))),DIMENSIONS=[900,900]) &$
FOR imm=0,NMASK-2 DO BEGIN &$
MAX_P=FLTARR(NBINS_P,3) &$
MIN_P=FLTARR(NBINS_P,3) &$
MED_P=FLTARR(NBINS_P,3) &$
MED_P_ASS=FLTARR(NBINS_P,3) &$
FOR it=0,2 DO BEGIN &$
FOR ib=0,NBINS_P-1 DO BEGIN &$
WORK=FLTARR(NRUN) &$
WORK_A=FLTARR(NRUN) &$
FOR ir=0,NRUN-1 DO BEGIN &$
WORK(ir)=REFORM(PP_T(imm,ib,it,SEL(ip),ir)/(PP_T(imm,1,it,SEL(ip),ir))*100.) &$
ENDFOR &$
MAX_P(ib,it)=MAX(WORK) &$
MIN_P(ib,it)=MIN(WORK) &$
MED_P(ib,it)=MEAN(WORK) &$
MED_P_ASS(ib,it)=MEAN(REFORM(PP_T(imm,ib,it,SEL(ip),*))) &$
ENDFOR &$
ENDFOR 
p=PLOT(BINS_P,SMOOTH(MED_P(*,0),5),/CURRENT,XRANGE=[10,99],color="black",YRANGE=[5,120], $
	XTITLE='HWMId',YTITLE='Population (million)',LAYOUT=[4,6,imm+1],MARGIN=[0.15,0.05,0.05,0.15],XMINOR=0,XTICKVALUES=TICKV,YTICKVALUES=TICKV_Y,YMINOR=0,THICK=2, $
	AXIS_STYLE=1,FONT_SIZE=6,/XLOG,/YLOG,XTICKLEN=0.02,YTICKLEN=0.02) &$
a=MIN_P(9:NBINS_P-1,0) &$
a(WHERE(a EQ 0))=1E-3 &$
b=MAX_P(9:NBINS_P-1,0) &$
b(WHERE(b EQ 0))=1E-3 &$
poly_p=POLYGON([BINS_P(9:NBINS_P-1),REVERSE(BINS_P(9:NBINS_P-1))],[SMOOTH(b,5),REVERSE(SMOOTH(a,5))],/DATA,/FILL_BACKGROUND, COLOR="gray",FILL_COLOR="gray",FILL_TRANSPARENCY=80,TARGET=p) &$

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
;IF ip EQ 0  THEN BEGIN &$
;t1=TEXT(50,6,'1976-2005',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p) &$
;t2=TEXT(50,8,'+1.5 $\circ$C',/DATA,COLOR="Blue",FONT_SIZE=10,TARGET=p) &$
;t3=TEXT(50,10,'+2 $\circ$C',/DATA,COLOR="Red",FONT_SIZE=10,TARGET=p) &$
;ENDIF &$

t=TEXT(48,80,MASK(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$

ENDFOR&$ ;MASK
ENDFOR ;ip

;ALL AREAS LAND FRACTION
TICKV_Y=[5,10,100,1000]
TICKV_Y=[5,25,50,75,100]
PANEL=['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v,','w','x','y','z']

openw,1,'RET_PER_LAND.txt'
FOR ip=0,NPP-1 DO BEGIN &$
printf,1,'==================' &$
printf,1,RET_PER(SEL(ip)) &$

w11=WINDOW(WINDOW_TITLE=STRING(RET_PER(SEL(ip))),DIMENSIONS=[900,900]) &$
FOR imm=0,NMASK-2 DO BEGIN &$

MAX_P=FLTARR(NBINS_P,3) &$
MIN_P=FLTARR(NBINS_P,3) &$
MED_P=FLTARR(NBINS_P,3) &$
MED_P_ASS=FLTARR(NBINS_P,3) &$
MAX_P_ASS=FLTARR(NBINS_P,3) &$
MIN_P_ASS=FLTARR(NBINS_P,3) &$
FOR it=0,2 DO BEGIN &$
FOR ib=0,NBINS_P-1 DO BEGIN &$
WORK=FLTARR(NRUN) &$
WORK_A=FLTARR(NRUN) &$
FOR ir=0,NRUN-1 DO BEGIN &$
WORK(ir)=REFORM(CC_T(imm,ib,it,SEL(ip),ir)) &$ ;SHOULD BE ALREADY IN %
ENDFOR &$
MAX_P(ib,it)=MAX(WORK) &$
MIN_P(ib,it)=MIN(WORK) &$
MED_P(ib,it)=MEAN(WORK) &$
MED_P_ASS(ib,it)=MEAN(REFORM(CC_T(imm,ib,it,SEL(ip),*))) &$
MIN_P_ASS(ib,it)=MIN(REFORM(CC_T(imm,ib,it,SEL(ip),*))) &$
MAX_P_ASS(ib,it)=MAX(REFORM(CC_T(imm,ib,it,SEL(ip),*))) &$
ENDFOR &$
ENDFOR &$
p=PLOT(BINS_P,SMOOTH(MED_P(*,0),5),/CURRENT,XRANGE=[10,99],color="black",YRANGE=[5,120], $
	XTITLE='HWMId',YTITLE='Land area fraction (%)',LAYOUT=[4,6,imm+1],MARGIN=[0.15,0.05,0.05,0.15],XMINOR=0,XTICKVALUES=TICKV,YTICKVALUES=TICKV_Y,YMINOR=0,THICK=2, $
	AXIS_STYLE=1,FONT_SIZE=6,/XLOG,/YLOG,XTICKLEN=0.02,YTICKLEN=0.02) &$
a=MIN_P(9:NBINS_P-1,0) &$
a(WHERE(a EQ 0))=1E-3 &$
b=MAX_P(9:NBINS_P-1,0) &$
b(WHERE(b EQ 0))=1E-3 &$
poly_p=POLYGON([BINS_P(9:NBINS_P-1),REVERSE(BINS_P(9:NBINS_P-1))],[SMOOTH(b,5),REVERSE(SMOOTH(a,5))],/DATA,/FILL_BACKGROUND, COLOR="gray",FILL_COLOR="gray",FILL_TRANSPARENCY=80,TARGET=p) &$

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
;IF ip EQ 0  THEN BEGIN &$
;t1=TEXT(50,6,'1976-2005',/DATA,COLOR="Black",FONT_SIZE=10,TARGET=p) &$
;t2=TEXT(50,8,'+1.5 $\circ$C',/DATA,COLOR="Blue",FONT_SIZE=10,TARGET=p) &$
;t3=TEXT(50,10,'+2 $\circ$C',/DATA,COLOR="Red",FONT_SIZE=10,TARGET=p) &$
;ENDIF &$
;p_s=TEXT(bins_P(25)-4,85,STRMID(MED_P_ASS(23,1)-MED_P_ASS(23,0),5,5),/DATA,COLOR="blue",TARGET=p,FONT_SIZE=8) &$
;p_s=TEXT(bins_P(47)-8,85,STRMID(MED_P_ASS(47,1)-MED_P_ASS(47,0),5,5),/DATA,COLOR="blue",TARGET=p,FONT_SIZE=8) &$
;p_s=TEXT(bins_P(96)-23,85,STRMID(MED_P_ASS(95,1)-MED_P_ASS(95,0),5,5),/DATA,COLOR="blue",TARGET=p,FONT_SIZE=8) &$
;p_s=TEXT(bins_P(25)-4,105,STRMID(MED_P_ASS(23,2)-MED_P_ASS(23,0),5,5),/DATA,COLOR="red",TARGET=p,FONT_SIZE=8) &$
;p_s=TEXT(bins_P(47)-8,105,STRMID(MED_P_ASS(47,2)-MED_P_ASS(47,0),5,5),/DATA,COLOR="red",TARGET=p,FONT_SIZE=8) &$
;p_s=TEXT(bins_P(96)-23,105,STRMID(MED_P_ASS(95,2)-MED_P_ASS(95,0),5,5),/DATA,COLOR="red",TARGET=p,FONT_SIZE=8) &$

;t=TEXT(4,20,PANEL(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$
t=TEXT(48,80,MASK(imm),/DATA,FONT_SIZE=10,COLOR="black",TARGET=p) &$

printf,1,'==============' &$
printf,1,MASK(imm) &$
printf,1,MED_P(LEV_L,0) &$
printf,1,MED_P(LEV_L,1) &$
printf,1,MED_P(LEV_L,2) &$
printf,1,'==============' &$
printf,1,MED_P_ASS(LEV_L,0) &$
printf,1,MIN_P_ASS(LEV_L,0) &$
printf,1,MAX_P_ASS(LEV_L,0) &$
printf,1,MED_P_ASS(LEV_L,1) &$
printf,1,MIN_P_ASS(LEV_L,1) &$
printf,1,MAX_P_ASS(LEV_L,1) &$
printf,1,MED_P_ASS(LEV_L,2) &$
printf,1,MIN_P_ASS(LEV_L,2) &$
printf,1,MAX_P_ASS(LEV_L,2) &$
printf,1,'==============' &$
ENDFOR&$ ;MASK
text1=TEXT(.3,.12,'___ 1976-2005 ',/NORMAL,FONT_SIZE=12,COLOR="black",TARGET=p)
text1=TEXT(.3,.08,'___ +1.5$\circ$C',/NORMAL,FONT_SIZE=12,COLOR="blue",TARGET=p)
text1=TEXT(.3,.04,'___+2$\circ$C',/NORMAL,FONT_SIZE=12,COLOR="red",TARGET=p)
text2=POLYGON([.55,.6,.6,.55],[0.04,.04,.06,.06],/NORMAL,/FILL_BACKGROUND,FILL_COLOR="red",color="red",FILL_TRANSPARENCY=80,/OVERPLOT)
text2=text(.62,.04,'Models range',/NORMAL,FONT_SIZE=12,COLOR="red",TARGET=p)

IF ip EQ 0 THEN w11.Save,'./PLOTS/PLOT_'+STAT+'_AREA_FR_RET_'+STRMID(STRING(RET_PER(SEL(ip))),7,1)+'_.pdf',BITMAP=1,PAGE_SIZE="A4" $
	ELSE w11.Save,'./PLOTS/PLOT_'+STAT+'_AREA_FR_RET_'+STRMID(STRING(RET_PER(SEL(ip))),6,2)+'_.pdf',BITMAP=1,PAGE_SIZE="A4"
ENDFOR ;ip

close,1
;============
STOP
;============




STOP
END
