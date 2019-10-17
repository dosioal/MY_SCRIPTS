;
PRO make_time_serie
;
DEVICE,decompose=0
;
;
;-----------------------------------------------------------------------------------------------------------

home='/media/disk/POSTPROC/BC_Regional/'
MODEL_ID='DMI-HIRHAM5'
CASE_ID='A1B_ECHAM5'

;VARIABLE (T,Ptot,Tmax,Tmin)
VAR='Ptot'
VAR='T'

CONSTRUCTION_PERIOD_START  =  ['1961','1971','1981','1961','1991']
CONSTRUCTION_PERIOD_STOP   =  ['1970','1980','1990','1990','2000']

APPLICATION_PERIOD_START   =  ['1991','1991','1991','1991','1991']
APPLICATION_PERIOD_STOP    =  ['2000','2000','2000','2000','2000']

nd=N_ELEMENTS(CONSTRUCTION_PERIOD_START)

ns=4
season=['DJF','MAM','JJA','SON']

nm=8
mask=['BI','IP','FR','ME','SC','AL','MD','EA','EU']
BI=[20,60,100,150]
IP=[0,45,40,80]
FR=[30,60,60,100]
ME=[60,100,85,115]
SC=[65,145,115,189]
AL=[60,100,65,85]
MD=[60,130,25,65]
EA=[100,145,65,115]
EU=[-15,60,35,75]

;-----------------------------------------------------------------------------------------------------------

tas_t_m_s=fltarr(nd,nm,ns)
tasc_t_m_s=fltarr(nd,nm,ns)
obs_t_m_s=fltarr(nd,nm,ns)
stat_tas_t_m_s=fltarr(nd,nm,ns)
stat_tasc_t_m_s=fltarr(nd,nm,ns)
prob_tas_t_m_s=fltarr(nd,nm,ns)
prob_tasc_t_m_s=fltarr(nd,nm,ns)

FOR id=0,nd-1 DO BEGIN ; NDATA LOOP

ofile='DATA/'+VAR+'_SM_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START(id)+'_'+CONSTRUCTION_PERIOD_STOP(id)+'_'+APPLICATION_PERIOD_START(id)+'_'+APPLICATION_PERIOD_STOP(id)+'.dat'
;save,filename=ofile,land2d_m,land_com,lat_m,lon_m,tas_sm,stat_tas_sm,prob_tas_sm
print,'RESTORING ',+ofile
restore,filename=ofile
ofile='DATA/'+VAR+'_SM_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START(id)+'_'+CONSTRUCTION_PERIOD_STOP(id)+'_'+APPLICATION_PERIOD_START(id)+'_'+APPLICATION_PERIOD_STOP(id)+'.dat'
;save,filename=ofile,land2d_m,land_com,lat_m,lon_m,tasc_sm,stat_tasc_sm,prob_tasc_sm
print,'RESTORING ',+ofile
restore,filename=ofile
ofile='DATA/OBS_SM_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START(id)+'_'+CONSTRUCTION_PERIOD_STOP(id)+'_'+APPLICATION_PERIOD_START(id)+'_'+APPLICATION_PERIOD_STOP(id)+'.dat'
;save,filename=ofile,land2d_m,land_com,obs_sm
print,'RESTORING ',+ofile
restore,filename=ofile


nx=N_ELEMENTS(land2d_m(*,0))
ny=N_ELEMENTS(land2d_m(0,*))
NPOINTS_M=N_ELEMENTS(land_com)

tas2d_sm = FLTARR(nx,ny,4)
tasc2d_sm = FLTARR(nx,ny,4)
obs2d_sm = FLTARR(nx,ny,4)
stat_tas2d_sm = FLTARR(nx,ny,4)
stat_tasc2d_sm = FLTARR(nx,ny,4)
;stat_tas2d_sm(*,*,*)=!VALUES.F_NAN
prob_tas2d_sm = FLTARR(nx,ny,4)
prob_tasc2d_sm = FLTARR(nx,ny,4)
tas2d_sm(*,*,*)=!VALUES.F_NAN
tasc2d_sm(*,*,*)=!VALUES.F_NAN
obs2d_sm(*,*,*)=!VALUES.F_NAN
stat_tas2d_sm(*,*,*)=!VALUES.F_NAN
stat_tasc2d_sm(*,*,*)=!VALUES.F_NAN
prob_tas2d_sm(*,*,*)=!VALUES.F_NAN
prob_tasc2d_sm(*,*,*)=!VALUES.F_NAN

FOR is=0,ns-1 DO BEGIN ; SEASON LOOP
FOR i=0L,NPOINTS_M-1 DO BEGIN &$
tas2d_sm(lon_m(i),lat_m(i),is)=tas_sm(i,is) &$
tasc2d_sm(lon_m(i),lat_m(i),is)=tasc_sm(i,is) &$
obs2d_sm(lon_m(i),lat_m(i),is)=obs_sm(i,is) &$
stat_tas2d_sm(lon_m(i),lat_m(i),is)=stat_tas_sm(i,is) &$
stat_tasc2d_sm(lon_m(i),lat_m(i),is)=stat_tasc_sm(i,is) &$
prob_tas2d_sm(lon_m(i),lat_m(i),is)=prob_tas_sm(i,is) &$
prob_tasc2d_sm(lon_m(i),lat_m(i),is)=prob_tasc_sm(i,is) &$
ENDFOR 

FOR im=0,nm-1 DO BEGIN ; LOOP OVER MASKS
ma=fltarr(4)
IF mask(im) eq 'BI' then ma=BI
IF mask(im) eq 'IP' then ma=IP
IF mask(im) eq 'FR' then ma=FR
IF mask(im) eq 'ME' then ma=ME
IF mask(im) eq 'SC' then ma=SC
IF mask(im) eq 'AL' then ma=AL
IF mask(im) eq 'MD' then ma=MD
IF mask(im) eq 'EA' then ma=EA

ww=reform(tas2d_sm(ma(0):ma(1),ma(2):ma(3),is))
tas_t_m_s(id,im,is)=MEAN(ww,/NaN)
ww=reform(tasc2d_sm(ma(0):ma(1),ma(2):ma(3),is))
tasc_t_m_s(id,im,is)=MEAN(ww,/NaN)
ww=reform(obs2d_sm(ma(0):ma(1),ma(2):ma(3),is))
obs_t_m_s(id,im,is)=MEAN(ww,/NaN)
ww=reform(stat_tas2d_sm(ma(0):ma(1),ma(2):ma(3),is))
stat_tas_t_m_s(id,im,is)=MEAN(ww,/NaN)
ww=reform(stat_tasc2d_sm(ma(0):ma(1),ma(2):ma(3),is))
stat_tasc_t_m_s(id,im,is)=MEAN(ww,/NaN)
ww=reform(prob_tas2d_sm(ma(0):ma(1),ma(2):ma(3),is))
prob_tas_t_m_s(id,im,is)=MEAN(ww,/NaN)
ww=reform(prob_tasc2d_sm(ma(0):ma(1),ma(2):ma(3),is))
prob_tasc_t_m_s(id,im,is)=MEAN(ww,/NaN)

ENDFOR ; MASKS
ENDFOR ; SEASONS
ENDFOR ; ND

IF VAR NE 'Ptot' THEN tas_t_m_s=tas_t_m_s-273.15
IF VAR NE 'Ptot' THEN tasc_t_m_s=tasc_t_m_s-273.15
IF VAR NE 'Ptot' THEN obs_t_m_s=obs_t_m_s-273.15

MASK:
mm=0
print,'MASK? (0=BI, 1=IP, 2=FR, 3=ME, 4=SC, 5=AL, 6=MD, 7=EA)'
read,mm


loadct,39
!p.background=255
!p.color=0
window,0,retain=2
!p.multi=[0,2,2]
plot,tas_t_m_s(*,mm,0),title=VAR,yrange=[min(tasc_t_m_s(*,mm,0))-1,max(tasc_t_m_s(*,mm,0))+1],color=0,thick=2
oplot,tasc_t_m_s(*,mm,0),color=30,thick=2
oplot,obs_t_m_s(*,mm,0),color=250,thick=2
plot,tas_t_m_s(*,mm,1),title=VAR,yrange=[min(tasc_t_m_s(*,mm,1))-1,max(tasc_t_m_s(*,mm,1))+1],color=0,thick=2
oplot,tasc_t_m_s(*,mm,1),color=30,thick=2
oplot,obs_t_m_s(*,mm,1),color=250,thick=2
plot,tas_t_m_s(*,mm,2),title=VAR,yrange=[min(tasc_t_m_s(*,mm,2))-1,max(tasc_t_m_s(*,mm,2))+1],color=0,thick=2
oplot,tasc_t_m_s(*,mm,2),color=30,thick=2
oplot,obs_t_m_s(*,mm,2),color=250,thick=2
plot,tas_t_m_s(*,mm,3),title=VAR,yrange=[min(tasc_t_m_s(*,mm,3))-1,max(tasc_t_m_s(*,mm,3))+1],color=0,thick=2
oplot,tasc_t_m_s(*,mm,3),color=30,thick=2
oplot,obs_t_m_s(*,mm,3),color=250,thick=2

window,1,retain=2
plot,stat_tas_t_m_s(*,mm,0),title='STAT '+VAR,thick=2,color=0,yrange=[0,0.5]
oplot,stat_tasc_t_m_s(*,mm,0),color=30,thick=2
plot,stat_tas_t_m_s(*,mm,1),thick=2,color=0,yrange=[0,0.5]
oplot,stat_tasc_t_m_s(*,mm,1),color=30,thick=2
plot,stat_tas_t_m_s(*,mm,2),thick=2,color=0,yrange=[0,0.5]
oplot,stat_tasc_t_m_s(*,mm,2),color=30,thick=2
plot,stat_tas_t_m_s(*,mm,3),thick=2,color=0,yrange=[0,0.5]
oplot,stat_tasc_t_m_s(*,mm,3),color=30,thick=2

window,2,retain=2
plot,prob_tas_t_m_s(*,mm,0),title='PROB '+VAR,thick=2,color=0,yrange=[0,0.5]
oplot,prob_tasc_t_m_s(*,mm,0),color=30,thick=2
plot,prob_tas_t_m_s(*,mm,1),thick=2,color=0,yrange=[0,0.5]
oplot,prob_tasc_t_m_s(*,mm,1),color=30,thick=2
plot,prob_tas_t_m_s(*,mm,2),thick=2,color=0,yrange=[0,0.5]
oplot,prob_tasc_t_m_s(*,mm,2),color=30,thick=2
plot,prob_tas_t_m_s(*,mm,3),thick=2,color=0,yrange=[0,0.5]
oplot,prob_tasc_t_m_s(*,mm,3),color=30,thick=2

choice=''
PRINT,'ANOTHER MASK? (y n)'
read,choice
IF choice eq 'y' THEN GOTO,MASK

STOP
END
