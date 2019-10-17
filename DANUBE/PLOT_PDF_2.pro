PRO PLOT_PDF_2
DEVICE,decomposed=0

;=============================================
RES='0.44'
lon1='-21.5'
lon2='15.56'
lat1='-20.6800003'
lat2='20.9999996'
;=============================================

VAR='T'
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

YSTART_1='1981'
YSTOP_1=['2010','2010','2010','2010','2010','2010','2010','2010','2010','2010','2010','2010']

YSTART_2='2021'
YSTOP_2=['2050','2050','2050','2050','2050','2050','2050','2050','2050','2050','2050','2050']

YSTART_3='2071'
YSTOP_3=['2098','2100','2098','2098','2100','2100','2100','2100','2099','2099','2099','2099']

NYEARS=30

;=========================================
;DIM OF VARIABLES
;=========================================

PDF_1=FLTARR(nbins,NGCM)
PDF_2=FLTARR(nbins,NGCM)
PDF_3=FLTARR(nbins,NGCM)

av_pdf_1=FLTARR(nbins)
max_pdf_1=FLTARR(nbins)
min_pdf_1=FLTARR(nbins)
av_pdf_2=FLTARR(nbins)
max_pdf_2=FLTARR(nbins)
min_pdf_2=FLTARR(nbins)
av_pdf_3=FLTARR(nbins)
max_pdf_3=FLTARR(nbins)
min_pdf_3=FLTARR(nbins)

fileo='./PDF_'+VAR+'_'+SEAS+'_'+YSTART_1+'-'+YSTOP_1(0)+'.dat'
restore,filename=fileo;,bins,PDF
PDF_1(*,*)=PDF

fileo='./PDF_'+VAR+'_'+SEAS+'_'+YSTART_2+'-'+YSTOP_2(0)+'.dat'
restore,filename=fileo;,bins,PDF
PDF_2(*,*)=PDF

fileo='./PDF_'+VAR+'_'+SEAS+'_'+YSTART_3+'-'+YSTOP_3(0)+'.dat'
restore,filename=fileo;,bins,PDF
PDF_3(*,*)=PDF

FOR ib=0,nbins-1 DO BEGIN
av_pdf_1(ib)=MEAN(PDF_1(ib,*))
max_pdf_1(ib)=MAX(PDF_1(ib,*))
min_pdf_1(ib)=MIN(PDF_1(ib,*))
av_pdf_2(ib)=MEAN(PDF_2(ib,*))
max_pdf_2(ib)=MAX(PDF_2(ib,*))
min_pdf_2(ib)=MIN(PDF_2(ib,*))
av_pdf_3(ib)=MEAN(PDF_3(ib,*))
max_pdf_3(ib)=MAX(PDF_3(ib,*))
min_pdf_3(ib)=MIN(PDF_3(ib,*))
ENDFOR

wwx=[bins(0:nbins-1),REVERSE(REFORM(bins(0:nbins-1))),bins(0)]
wwy_1=[REFORM(max_pdf_1(0:nbins-1)),REVERSE(REFORM(min_pdf_1(0:nbins-1))),max_pdf_1(0)];/TOTAL(pdf_or(0,*,im))
wwy_2=[REFORM(max_pdf_2(0:nbins-1)),REVERSE(REFORM(min_pdf_2(0:nbins-1))),max_pdf_2(0)];/TOTAL(pdf_or(0,*,im))
wwy_3=[REFORM(max_pdf_3(0:nbins-1)),REVERSE(REFORM(min_pdf_3(0:nbins-1))),max_pdf_3(0)];/TOTAL(pdf_or(0,*,im))


loadct,39
!p.color=0
!p.background=255
!p.multi=0

IF VAR EQ 'Ptot' THEN xrange=[1,60] ELSE xrange=[bins(0),bins(nbins-10)] 
IF VAR EQ 'Ptot' THEN yrange=[0,0.3] ELSE yrange=[0,0.15]
;IF VAR EQ 'Ptot' THEN plot,bins,av_pdf_1(*),xlog=1,ylog=1,charsize=1,xtitle='mm/day',yrange=yrange,ystyle=1,xrange=xrange,xstyle=1,thick=3 ELSE $

window,0,retain=2,xsize=600,ysize=600
;xrange=[1,200] 
IF VAR EQ 'Ptot' THEN plot,bins,av_pdf_1(*),charsize=1,xtitle='mm/day',yrange=yrange,ystyle=1,xrange=xrange,xstyle=1,thick=3 ELSE $
plot,bins,av_pdf_1(*),charsize=1,xtitle='T (C)',yrange=yrange,ystyle=1,xrange=xrange,xstyle=1,thick=3 &$ 
POLYFILL,wwx,wwy_1,color=180
POLYFILL,wwx,wwy_2,color=110
POLYFILL,wwx,wwy_3,color=200
oplot,bins,av_pdf_1(*),color=0,thick=3
oplot,bins,av_pdf_2(*),color=50,thick=3
oplot,bins,av_pdf_3(*),color=250,thick=3

set_plot,'ps'
PRINT,'Creating Postscriptfile: '
device,file='./PLOT_PDF_'+VAR+'.eps',/color,/encapsulated,/landscape,xoffset=1,yoffset=25,xsize=25,ysize=17
!p.multi=0
IF VAR EQ 'Ptot' THEN plot,bins,av_pdf_1(*),charsize=1,xtitle='mm/day',yrange=yrange,ystyle=1,xrange=xrange,xstyle=1,thick=3 ELSE $
plot,bins,av_pdf_1(*),charsize=2,xtitle='T (C)',yrange=yrange,ystyle=1,xrange=xrange,xstyle=1,thick=3 &$
POLYFILL,wwx,wwy_1,color=180
POLYFILL,wwx,wwy_2,color=110
POLYFILL,wwx,wwy_3,color=200
oplot,bins,av_pdf_1(*),color=0,thick=3
oplot,bins,av_pdf_2(*),color=50,thick=3
oplot,bins,av_pdf_3(*),color=250,thick=3

XYOUTS,5,0.13,'1981-2010',color=0,charsize=2
XYOUTS,5,0.12,'2021-2050',color=50,charsize=2
XYOUTS,5,0.11,'2071-2100',color=250,charsize=2


device,/close
set_plot,'x'


STOP

END
