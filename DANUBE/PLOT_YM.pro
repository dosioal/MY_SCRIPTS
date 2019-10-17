PRO PLOT_YM
DEVICE,decomposed=0

VAR='T'
;VAR='Tmax'
;VAR='Tmin'
VAR='Ptot'

STAT='YM'

home_data='/media/disk_BC/POSTPROC/BC_Regional/'
CONSTR='1961_1990'

MODEL_ID=['METO-HC_HadRM3Q0','MPI-M-REMO','C4IRCA3','ETHZ-CLM','KNMI-RACMO2','SMHIRCA','SMHIRCA','SMHIRCA','DMI-HIRHAM5','DMI-HIRHAM5','DMI-HIRHAM5','CNRM-RM5.1']
CASE_ID=['A1B_HadCM3Q0','SCN_ECHAM5','A1B_HadCM3Q16','SCN_HadCM3Q0','A1B_ECHAM5-r3','A1B_BCM','A1B_HadCM3Q3','A1B_ECHAM5-r3','A1B_BCM','A1B_ARPEGE','A1B_ECHAM5','SCN_ARPEGE']
NGCM=N_ELEMENTS(MODEL_ID)

YSTART='1961'
YSTOP=['2098','2100','2098','2099','2099','2100','2100','2100','2099','2099','2100','2099']

NYEARS=MAX(FIX(YSTOP))-FIX(YSTART)+1

;=============
;LANDMASK_COM AND DIMENSION
;=============
LM_FILE='/media/disk_BC/DATA/ENSOBS/ENS_MODELS/read_in_KNMI-RACMO2_1961-1990.nc'

my_cutoff=0.5
fileID = NCDF_Open(LM_FILE)
varID = NCDF_VarID(fileID,'tg')
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
landmask_com=dblarr(nx,ny)
NCDF_VARGET,fileID,'tg',landmask_com;,count=[nx,ny];,offset=[0,0]
lat=fltarr(nx,ny)
NCDF_VARGET,fileID,'Actual_latitude',lat
lon=fltarr(nx,ny)
NCDF_VARGET,fileID,'Actual_longitude',lon
NCDF_VARGET,fileID,'latitude',lat_r
NCDF_VARGET,fileID,'longitude',lon_r

NCDF_CLOSE, fileID
help,landmask_com
print,nx,ny

nx=nx-1
landmask_com=REFORM(landmask_com(1:nx-1,*))
VAR_SM_1=FLTARR(nx,ny,nyears,NGCM)

;=========================================
;DIM OF VARIABLES
;=========================================

FOR im=0,NGCM-1 DO BEGIN ;GCM LOOP

print,'MODEL= ',MODEL_ID(im)

DIR_DATA=home_data+MODEL_ID(im)+'_'+CASE_ID(im)+'_EOBS_1961-1990_1961-2100/finalOutputData/'
DIR_SM=DIR_DATA+STAT
FILE=DIR_SM+'/'+VAR+'_BCed_'+MODEL_ID(im)+'_'+CASE_ID(im)+'_'+YSTART+'_'+YSTOP(im)+'_022_'+STAT+'_COM.nc'
spawn,'gunzip '+FILE+'.gz'

IF VAR EQ 'Ptot' THEN thevariable='precip' ELSE thevariable='temp2'

print,'DATA FILE = ',FILE
fileID = NCDF_Open(FILE)
varID = NCDF_VarID(fileID,thevariable)
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
nt=dims_a(2)
print,nx,ny,nt
NCDF_VARGET,fileID,thevariable,WORK;,count=[nx,ny];,offset=[0,0]
NCDF_CLOSE, fileID
print,nt
VAR_SM_1(*,*,0:nt-1,im)=WORK

;STOP
END ;GCM

;=============================================
;MASK COUNTRY
;=============================================

;PDF OVER DAN AREA
VAR_MASK=FLTARR(nyears,NGCM)

RESTORE,'./LAND_DAN.dat'

loadct,39
!p.color=0
!p.background=255
;window,0,retain=2
;contour,land_country_dan(*,*),levels=[0,1]

lc=REFORM(land_country_dan(*,*)) 
lc(WHERE(lc LT 1))=!VALUES.F_NaN

work11=FLTARR(nx,ny)
FOR im=0,NGCM-1 DO BEGIN &$
FOR it=0,nyears-1 DO BEGIN &$
work1=REFORM(VAR_SM_1(*,*,it,im))*lc &$
work1(WHERE(work1 LT -100))=!VALUES.F_NaN &$
VAR_MASK(it,im)=MEAN(work1,/NaN)  &$  ;NDAYS IN JAS
ENDFOR &$
ENDFOR

;fileo='./YM_'+VAR+'_'+YSTART+'-'+YSTOP+'.dat'
;save,filename=fileo,bins,PDF

years=FINdGEN(MAX(nyears))+1961
xrange=[1965,2095]

window,1,retain=2,xsize=1000,ysize=1200
!p.multi=0
IF VAR EQ 'Ptot' THEN yrange=[0,1500] ELSE yrange=[0,20]
IF VAR EQ 'Ptot' THEN plot,years,SMOOTH(VAR_MASK(0:nyears-3,0)*3600.*24.*365.,10,/EDGE_MIRROR),charsize=2,xtitle='Year',ytitle='Precipitation (mm/year)',ystyle=1,xstyle=1,yrange=yrange ELSE plot,years,SMOOTH(VAR_MASK(0:nyears-3,0)-273.15,10,/EDGE_MIRROR),charsize=2,xtitle='Year',ytitle='Temperature (c)',ystyle=1,xstyle=1,yrange=yrange,xrange=xrange
FOR im=0,NGCM-1 DO BEGIN &$
IF VAR EQ 'Ptot' THEN oplot,years,SMOOTH(VAR_MASK(0:nyears-3,im)*3600.*24.*365.,10,/EDGE_MIRROR),color=im*20,thick=3 ELSE oplot,years,SMOOTH(VAR_MASK(0:nyears-3,im)-273.15,10,/EDGE_MIRROR),color=im*20,thick=3 &$
ENDFOR

set_plot,'ps'
PRINT,'Creating Postscriptfile: '
device,file='./PLOT_'+VAR+'_YM_EVOL.eps',/color,/encapsulated,/landscape,xoffset=1,yoffset=25,xsize=25,ysize=17
!p.multi=0
IF VAR EQ 'Ptot' THEN yrange=[0,1500] ELSE yrange=[0,20]
IF VAR EQ 'Ptot' THEN plot,years,SMOOTH(VAR_MASK(0:nyears-3,0)*3600.*24.*365.,10,/EDGE_MIRROR),charsize=2,xtitle='Year',ytitle='Precipitation (mm/year)',ystyle=1,xstyle=1,yrange=yrange ELSE plot,years,SMOOTH(VAR_MASK(0:nyears-3,0)-273.15,10,/EDGE_MIRROR),charsize=2,xtitle='Year',ytitle='Temperature (c)',ystyle=1,xstyle=1,yrange=yrange,xrange=xrange
FOR im=0,NGCM-1 DO BEGIN &$
IF VAR EQ 'Ptot' THEN oplot,years,SMOOTH(VAR_MASK(0:nyears-3,im)*3600.*24.*365.,10,/EDGE_MIRROR),color=im*20,thick=3 ELSE oplot,years,SMOOTH(VAR_MASK(0:nyears-3,im)-273.15,10,/EDGE_MIRROR),color=im*20,thick=3 &$
ENDFOR
device,/close
set_plot,'x'


STOP

END
