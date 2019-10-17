;;THIS IDL FILE CONTAINS THE MAIN PROCEDURES REQUIRED
;;FOR THE CONSTRUCTION OF THE TEMPERATURE BIAS CORRECTION COEFFICIENTS WITHIN
;;THE MONTHLY BIAS CORRECTION.
;; 
;; WORK BLOCK LEADER: S. HAGEMANN
;;
;;
;;
;; FILENAME: calc_T_cor_coeff.pro
;; AUTHORS:  C. PIANI AND J. O. HAERTER
;; DATE:     NOVEMBER 17, 2009
;; PROJECT:  EU WATCH PROJECT
;; requires: definitions.pro, definitions_internal.pro, functions.pro 
;;
;;
;Calculates Monthly precipitation Bias Correction Factors using the
;specified files for the observations and model data for
;a given time period (construction_period).
;______________________________________________________________
;
print,'CALC_T_COR_COEFF.pro'
;
for mon=0,(n_elements(month)-1) do begin 
;
;*******************************************************************
; specifying the output file for the correction parameters
outfile=outputdir+'tas_cor_'+construction_period+'_'+month(mon)+runToCorrect+'.dat'
;
; specifying the path to the model data for t2, tmin, tmax
pfile_temp2=pathmodel+MODELroot_T+construction_period+'_'+month(mon)+runToCorrect+'.dat'
pfile_tmin=pathmodel+MODELroot_Tmin+construction_period+'_'+month(mon)+runToCorrect+'.dat'
pfile_tmax=pathmodel+MODELroot_Tmax+construction_period+'_'+month(mon)+runToCorrect+'.dat'
;
; ... to the observational data for t2, tmin, tmax
obsfile_temp2 = pathOBS+OBSroot_T+construction_period+'_'+month(mon)+'.dat'
obsfile_tmin  = pathOBS+OBSroot_Tmin+construction_period+'_'+month(mon)+'.dat'
obsfile_tmax  = pathOBS+OBSroot_Tmax+construction_period+'_'+month(mon)+'.dat'
;
print,'RESTORING FILE ',pfile_temp2
restore,pfile_temp2
tas_e=idldata*1.0D
l=n_elements(tas_e(0,*))
print,'TIMESTEPS= ',l
;
print,'RESTORING FILE ',pfile_tmin
restore,pfile_tmin
tmin_e=idldata*1.0D
;
print,'RESTORING FILE ',pfile_tmax
restore,pfile_tmax
tmax_e=idldata*1.0D
;
print,'done restoring '+construction_period+', month '+month(mon)+' model';******************
;*******************************************************************
;
;Get OBS for period*************************************
;
print,'RESTORING FILE ',obsfile_temp2
restore,obsfile_temp2
tas_o=idldata*1.0D
tas=0
print,'RESTORING FILE ',obsfile_tmax
restore,obsfile_tmax
tmax_o=idldata*1.0D
tmax=0
print,'RESTORING FILE ',obsfile_tmin
restore,obsfile_tmin
tmin_o=idldata*1.0D
tmin=0
;******************************************************************
;Define bias correction parameters
;
a_tas=fltarr(NUMLANDPOINTS,2)
a_td=fltarr(NUMLANDPOINTS,2)
a_tf=fltarr(NUMLANDPOINTS,2)
;
;******************************************************************
; define plotting axis
plot,[lon(0)],[lat(0)],xrange=[0,dims(0)],yrange=[0,dims(1)]
xyouts,0,0.1*dims(1),'month = '+month(mon)

; check data
print,'Checking consistency of input data ...'
errors=0
IF (min(tas_e) LT 0.0) THEN BEGIN
	a=where(tas_e LT 0.0)
	b=ARRAY_INDICES(tas_e,a)
	a=where(tas_e LT 0.0,count)
    print,'Model has negative temperature in',count,' points!(',count*100./N_ELEMENTS(tas_e),' %)'
    errors=1
ENDIF

IF (min(tmax_e-tas_e) LT 0.0) THEN BEGIN
    cc=tmax_e-tas_e
    aa=where(cc LT 0.0,count)
    bb=ARRAY_INDICES(cc,aa)
    print,'Model has points with Tmax<Tmean in',count,' points!(',count*100./N_ELEMENTS(cc),' %)'
    print,tmax_e(bb(0),bb(1)),tas_e(bb(0),bb(1))
    print,'correcting the data...'
    tmax_e(bb[0,*],bb[1,*])=tas_e(bb[0,*],bb[1,*])
    print,tmax_e(bb(0),bb(1)),tas_e(bb(0),bb(1))
    errors=1
    ;print,'Rewriting '+pfile_tmax
    ;save,filename=pfile_tmax,tmax_e
ENDIF
IF (min(tmax_e-tas_e) LT 0.0) THEN STOP ;Checking again...

IF (max(tmin_e-tas_e) GT 0.0) THEN BEGIN
    cc=tmin_e-tas_e
    aa=where(cc GT 0.0,count)
    bb=ARRAY_INDICES(cc,aa)
    print,'Model has points with Tmin>Tmean in',count,' points!(',count*100./N_ELEMENTS(cc),' %)'
    print,tmin_e(bb(0),bb(1)),tas_e(bb(0),bb(1))
    print,'correcting the data...'
    tmin_e(bb[0,*],bb[1,*])=tas_e(bb[0,*],bb[1,*])
    print,tmin_e(bb(0),bb(1)),tas_e(bb(0),bb(1))
   ; print,'Rewriting '+pfile_tmin
    ;save,filename=pfile_tmin,tmin_e
    errors=1
ENDIF
IF (max(tmin_e-tas_e) GT 0.0) THEN STOP ;Checking again...

IF (min(tmax_e-tmin_e) LT 0.0) THEN BEGIN ; IT SHOULD NOY HAPPEN WITH THE CORRECTION ABOVE
    cc=tmax_e-tmin_e
    aa=where(cc LT 0.0,count)
    bb=ARRAY_INDICES(cc,aa)
    print,'Model has negative diurnal range in',count,' points!(',count*100./N_ELEMENTS(cc),' %)'
    print,tmin_e(bb(0),bb(1)),tmax_e(bb(0),bb(1))
    print,'correcting the data...'
    tmin_e(bb[0,*],bb[1,*])=tas_e(bb[0,*],bb[1,*])
    tmax_e(bb[0,*],bb[1,*])=tas_e(bb[0,*],bb[1,*])
    print,tmin_e(bb(0),bb(1)),tmax_e(bb(0),bb(1))
    ;print,'Rewriting '+pfile_tmin
    ;save,filename=pfile_tmin,tmin_e
    ;print,'Rewriting '+pfile_tmax
    ;save,filename=pfile_tmax,tmax_e
    errors=1
ENDIF
IF (min(tmax_e-tmin_e) LT 0.0) THEN STOP ;Checking again...

IF (min(tas_o) LT 0.0) THEN BEGIN
	aa=where(tas_o LT 0.0,count)
    print,'OBS has negative temperature in',count,' points!(',count*100./N_ELEMENTS(tas_o),' %)'
    errors=1
ENDIF

IF (min(tmax_o-tas_o) LT 0.0) THEN BEGIN
    cc=tmax_o-tas_o
    aa=where(cc LT 0.0,count)
    bb=ARRAY_INDICES(cc,aa)
    print,'OBS has points with Tmax<Tmean in',count,' points!(',count*100./N_ELEMENTS(cc),' %)'
    print,tmax_o(bb(0),bb(1)),tas_o(bb(0),bb(1))
    print,'correcting the data...'
    tmax_o(bb[0,*],bb[1,*])=tas_o(bb[0,*],bb[1,*])
    print,tmax_o(bb(0),bb(1)),tas_o(bb(0),bb(1))
    ;print,'Rewriting '+obsfile_tmax
    ;save,filename=obsfile_tmax,tmax_o
    errors=1
ENDIF
IF (min(tmax_o-tas_o) LT 0.0) THEN STOP ;Checking again...

IF (max(tmin_o-tas_o) GT 0.0) THEN BEGIN
    cc=tmin_o-tas_o
    aa=where(cc GT 0.0,count)
    bb=ARRAY_INDICES(cc,aa)
    print,'OBS has points with Tmin>Tmean in',count,' points!(',count*100./N_ELEMENTS(cc),' %)'
    print,tmin_o(bb(0),bb(1)),tas_o(bb(0),bb(1))
    print,'correcting the data...'
    tmin_o(bb[0,*],bb[1,*])=tas_o(bb[0,*],bb[1,*])
    print,tmin_o(bb(0),bb(1)),tas_o(bb(0),bb(1))
    ;print,'Rewriting '+obsfile_tmin
    ;save,filename=obsfile_tmin,tmin_e
    errors=1
ENDIF
IF (max(tmin_o-tas_o) GT 0.0) THEN STOP ;Checking again...

IF (min(tmax_o-tmin_o) LT 0.0) THEN BEGIN
    cc=tmax_o-tmin_o
    aa=where(cc LT 0.0,count)
    bb=ARRAY_INDICES(cc,aa)
    print,'OBS has negative diurnal range in',count,' points!(',count*100./N_ELEMENTS(cc),' %)'
    print,tmax_o(bb(0),bb(1)),tmin_o(bb(0),bb(1))
    print,'correcting the data... (assigning a missing value)'
    tmin_o(bb[0,*],bb[1,*])=-99.00
    tmax_o(bb[0,*],bb[1,*])=-99.00
    print,tmax_o(bb(0),bb(1)),tmin_o(bb(0),bb(1))
    ;print,'Rewriting '+obsfile_tmax
    ;save,filename=obsfile_tmax,tmax_o
    ;print,'Rewriting '+obsfile_tmin
    ;save,filename=obsfile_tmin,tmin_e
    errors=1
ENDIF
IF (min(tmax_o-tmin_o) LT 0.0) THEN STOP ;Checking again...

IF (errors eq 0) THEN BEGIN
    print,'Data passed basic consistency check.'
ENDIF ELSE BEGIN
    print,'There are possible data inconsistencies.'
ENDELSE

;
;******************************************************************
;START LOOP
for n=0L,(NUMLANDPOINTS-1) do begin 
oplot,[lon(n)],[lat(n)],psym=3
;
ts_o=reform(tas_o(n,*)) ; obs
ts_e=reform(tas_e(n,*)) ; mod
;
tx_o=reform(tmax_o(n,*)) ; obs
tx_e=reform(tmax_e(n,*)) ; mod
;
tn_o=reform(tmin_o(n,*)) ; obs
tn_e=reform(tmin_e(n,*)) ; mod
;
we_tas_o=where(ts_o gt 0.0)
we_tas_e=where(ts_e gt 0.0)
n_e_o=n_elements(we_tas_o)
n_e_e=n_elements(we_tas_e)
;
min_n=min([n_e_o,n_e_e])
;
if (n_e_o gt 20 and n_e_e gt 20) then begin
;
; selecting the non-missing temperatures
    ts_o = ts_o(we_tas_o)
    ts_e = ts_e(we_tas_e)
;
    ts_o = ts_o(0:min_n-1)
    ts_e = ts_e(0:min_n-1)
;
    tx_o = tx_o(we_tas_o)
    tx_e = tx_e(we_tas_e)
;
    tx_o = tx_o(0:min_n-1)
    tx_e = tx_e(0:min_n-1)
;
    tn_o = tn_o(we_tas_o)
    tn_e = tn_e(we_tas_e)
;
    tn_o = tn_o(0:min_n-1)
    tn_e = tn_e(0:min_n-1)
; These are now the new vectors for mean, max and min that should be
; worked with.
;
;the total diurnal range corresponding to the non-missing temperatures
    delta_o=tx_o-tn_o
    delta_e=tx_e-tn_e
;
;sorting the daily mean observations
y=reform(ts_o(sort(ts_o))) ; obs
x=reform(ts_e(sort(ts_e))) ; mod
;
length_data=n_elements(y) ; the total number of days to correct
;
xd=reform(delta_e(sort(delta_e))) ; sorted obs
yd=reform(delta_o(sort(delta_o))) ; sorted mod
;
we_o=where(delta_o gt 0)          ; obs
we_e=where(delta_e gt 0)          ; mod
;
IF (we_o(0) NE -1 AND we_e(0) NE -1) THEN BEGIN
    diurnal_correction = 1
ENDIF ELSE diurnal_correction = 0
IF (diurnal_correction EQ 1) THEN BEGIN
l0=n_elements(delta_o)  ; the length of the data
l_o=n_elements(we_o)    ; length of the non-zero obs
l_e=n_elements(we_e)    ; length of the non-zero mod
l_min=l0-min([l_o,l_e])
; xd and yd are vectors of the correct number of points, all elements are non zero
xd=xd(l_min:l0-1) 
yd=yd(l_min:l0-1)
;print,'NUMBER OF OBS POINTS ',n_elements(xd)
;print,'NUMBER OF MOD POINTS ',n_elements(yd)
; xd and yd can now be used to construct the correction for delta
;_________________________________________________________
;
;the 'skewness' of the diurnal cycle
    fact_o=(ts_o(we_o)-tn_o(we_o))/delta_o(we_o)
    fact_e=(ts_e(we_e)-tn_e(we_e))/delta_e(we_e)
;
; these are now two vectors of unequal length of skewness values. data
; points where delta=0 are simply ignored.
;
;sorting the skewness
    yf=reform(fact_o(sort(fact_o))) ; obs
    xf=reform(fact_e(sort(fact_e))) ; mod
;
    we1_o=where(fact_o gt 0)    ; obs
    we1_e=where(fact_e gt 0)    ; mod
;
    l1_o=n_elements(we1_o)        ; length of the non-zero obs
    l1_e=n_elements(we1_e)        ; length of the non-zero mod
    l_min_o=l_o-min([l1_o,l1_e])
    l_min_e=l_e-min([l1_o,l1_e])
; xd and yd are vectors of the correct number of points, all elements are non zero
    yf=yf(l_min_o:l_o-1) 
    xf=xf(l_min_e:l_e-1)
;
; xf and yf are now two vectors of equal length that can be
; used to compute the correction to the skewness
; computing the least squares fit for the mean
;
ENDIF
;_________________________________________________________
;
  xm=mean(x)
  ym=mean(y)
  yxm=mean(x*y)
  xxm=mean(x*x)
  divisor=xxm-xm*xm
  IF (divisor NE 0.0 AND xm NE 0.0) THEN BEGIN
      a=[ym+(ym*xm-yxm)/divisor*xm,-(ym*xm-yxm)/divisor]
  ENDIF ELSE BEGIN
      a=[ym-xm,1]
  ENDELSE
  IF (a(0) GT 50000.0 or a(0) LT -50000.0 or a(1) GT 5000.0 or a(1) LT -5000.0) THEN BEGIN
      a=[0,1]
  ENDIF
;
IF (diurnal_correction EQ 1) THEN BEGIN
; here we introduce a condition for cases when skewness and dirurnal
; range should be corrected: 90% of the available points must have
; reasonable skewness values, i.e. delta>0.
  IF (n_elements(yf) gt 0.9*length_data) THEN BEGIN
; for the skewless
      xm=mean(xf)
      ym=mean(yf)
      yxm=mean(xf*yf)
      xxm=mean(xf*xf)
     ;
      divisor=xxm-xm*xm
      IF (divisor NE 0.0 AND xm NE 0.0) THEN BEGIN
          af=[ym+(ym*xm-yxm)/divisor*xm,-(ym*xm-yxm)/divisor]
      ENDIF ELSE BEGIN
          af=[ym-xm,1]
      ENDELSE
      IF (af(0) GT 50000.0 or af(0) LT -50000.0 or af(1) GT 5000.0 or af(1) LT -5000.0) THEN BEGIN
          af=[0,1]
      ENDIF
      if ((af(0) gt 0.) ne 1) and ((af(0) le 0.) ne 1) then begin
          print,'af0',af(0),n
          print,xf
          print,yf
      endif
      if ((af(1) gt 0.) ne 1) and ((af(1) le 0.) ne 1) then print,'af1',af(1),n
;
; for the total diurnal range
      xm=mean(xd)
      ym=mean(yd)
      yxm=mean(xd*yd)
      xxm=mean(xd*xd)
      divisor=xxm-xm*xm
      IF (divisor NE 0.0 AND xm NE 0.0) THEN BEGIN
          ad=[ym+(ym*xm-yxm)/divisor*xm,-(ym*xm-yxm)/divisor]
      ENDIF ELSE BEGIN
          ad=[ym-xm,1]
      ENDELSE
      IF (ad(0) GT 50000.0 or ad(0) LT -50000.0 or ad(1) GT 5000.0 or ad(1) LT -5000.0) THEN BEGIN
          ad=[0,1]
      ENDIF
;
  ENDIF ELSE BEGIN ; if there are missing values in the original data, a trivial correction is performed.
      af=[0,1] 
      ad=[0,1]
  ENDELSE
ENDIF ELSE BEGIN
    af=[0,1] 
    ad=[0,1]
ENDELSE
;
endif else begin
    ; use a trivial correction
    a =[0,1]
    af=[0,1] 
    ad=[0,1]
endelse
;
;******************************************************
;********* end of fitting procedure ************************
; output data for the slope and offset
a_tas(n,*)=a
a_td(n,*)=ad
a_tf(n,*)=af
endfor
;
save,filename=outfile,a_tas,a_tf,a_td
print,'saving factors in'+outfile
;
endfor
;
close,2
end

