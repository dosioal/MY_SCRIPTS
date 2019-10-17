PRO PLOT_PDF
DEVICE,decomposed=0

;=============================================
RES='0.44'
lon1='-21.5'
lon2='15.56'
lat1='-20.6800003'
lat2='20.9999996'
;=============================================

VAR='T'
;VAR='Tmax'
;VAR='Tmin'
;VAR='DT'
;VAR='Ptot'

STAT='PDF'

SEAS='DJF'
SEAS='JJA'

IF VAR EQ 'Ptot' THEN bins=[1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200] $
ELSE BEGIN
IF SEAS EQ 'DJF' THEN bins=[-20,-19,-18,-17,-16,-15,-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]
IF SEAS EQ 'JJA' THEN bins=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50]
ENDELSE
nbins=N_ELEMENTS(bins)-1
print,'BINS=',bins


home_data='/media/disk_BC/POSTPROC/BC_Regional/'
CONSTR='1961_1990'

MODEL_ID=['METO-HC_HadRM3Q0','MPI-M-REMO','C4IRCA3','ETHZ-CLM','KNMI-RACMO2','SMHIRCA','SMHIRCA','SMHIRCA','DMI-HIRHAM5','DMI-HIRHAM5','DMI-HIRHAM5','CNRM-RM5.1']
CASE_ID=['A1B_HadCM3Q0','SCN_ECHAM5','A1B_HadCM3Q16','SCN_HadCM3Q0','A1B_ECHAM5-r3','A1B_BCM','A1B_HadCM3Q3','A1B_ECHAM5-r3','A1B_BCM','A1B_ARPEGE','A1B_ECHAM5','SCN_ARPEGE']
NGCM=N_ELEMENTS(MODEL_ID)

YSTART='1981'
YSTOP=['2010','2010','2010','2010','2010','2010','2010','2010','2010','2010','2010','2010']

YSTART='2021'
YSTOP=['2050','2050','2050','2050','2050','2050','2050','2050','2050','2050','2050','2050']

YSTART='2071'
YSTOP=['2098','2100','2098','2098','2100','2100','2100','2100','2099','2099','2099','2099']

NYEARS=30

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
VAR_SM_1=FLTARR(nx,ny,nbins,nyears,NGCM)

;=========================================
;DIM OF VARIABLES
;=========================================

FOR im=0,NGCM-1 DO BEGIN ;GCM LOOP

GRID_FILE='/media/disk_BC/POSTPROC/BC_Regional/GRID_EU_022_'+MODEL_ID(im)+'.txt'
print,'MODEL= ',MODEL_ID(im)

DIR_DATA=home_data+MODEL_ID(im)+'_'+CASE_ID(im)+'_EOBS_1961-1990_1961-2100/finalOutputData/'
DIR_SM=DIR_DATA+'/PP/'+STAT

FILE_1=DIR_SM+'/'+VAR+'_BCed_'+MODEL_ID(im)+'_'+CASE_ID(im)+'_'+YSTART+'-'+YSTOP(im)+'_'+SEAS+'_'+STAT+'.nc'

spawn,'rm -f test*.nc'

IF VAR EQ 'Ptot' THEN thevariable='precip' ELSE thevariable='temp2'

print,'DATA FILE = ',FILE_1
fileID = NCDF_Open(FILE_1)
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
nb=dims_a(2)
nt=dims_a(3)
print,nx,ny,nb,nt
NCDF_VARGET,fileID,thevariable,WORK;,count=[nx,ny];,offset=[0,0]
NCDF_CLOSE, fileID
nt1=FIX(YSTOP(im))-FIX(YSTART)+1
print,nt1
VAR_SM_1(*,*,*,0:nt-1,im)=WORK


;STOP
END ;GCM

;=============================================
;MASK COUNTRY
;=============================================

;PDF OVER DAN AREA
PDF=FLTARR(nbins,NGCM)

RESTORE,'./LAND_DAN.dat'

loadct,39
!p.color=0
!p.background=255
;window,0,retain=2
;contour,land_country_dan(*,*),levels=[0,1]

lc=REFORM(land_country_dan(*,*)) 
lc(WHERE(lc LT 1))=!VALUES.F_NaN

work11=FLTARR(nx,ny,nyears)
FOR im=0,NGCM-1 DO BEGIN &$
FOR ib=0,nbins-1 DO BEGIN &$
FOR it=0,nyears-1 DO BEGIN &$
work1=REFORM(VAR_SM_1(*,*,ib,it,im))*lc &$
work11(*,*,it)=work1 &$
ENDFOR &$
PDF(ib,im)=MEAN(work11,/NaN)/93.  &$  ;NDAYS IN JAS
ENDFOR &$
ENDFOR

fileo='./PDF_'+VAR+'_'+SEAS+'_'+YSTART+'-'+YSTOP(0)+'.dat'
save,filename=fileo,bins,PDF

window,2,retain=2,xsize=1000,ysize=1200
!p.multi=[0,3,4]
;xrange=[1,200] 
xrange=[bins(0),bins(nbins-1)] 
yrange=[1e-6,1]
FOR im=0,NGCM-1 DO BEGIN &$
IF VAR EQ 'Ptot' THEN plot,bins,PDF(*,im),xlog=1,ylog=1,charsize=1,xtitle='mm/day',yrange=yrange,ystyle=1,xrange=xrange,xstyle=1 ELSE $
plot,bins,PDF(*,im),charsize=1,xtitle='T (C)',yrange=[0,1],ystyle=1,xrange=xrange,xstyle=1 &$ 

ENDFOR

STOP

END
