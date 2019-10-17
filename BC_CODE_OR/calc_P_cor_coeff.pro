;;THIS IDL FILE CONTAINS THE MAIN PROCEDURES REQUIRED
;;FOR THE CONSTRUCTION OF THE BIAS CORRECTION COEFFICIENTS WITHIN
;;THE MONTHLY BIAS CORRECTION.
;;
;; WORK BLOCK LEADER: S. HAGEMANN
;;
;;
;;
;; FILENAME: calc_P_cor_coeff.pro
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
;
close,2
;
; the main loop over all months
for mon=0,(n_elements(month)-1) do begin
;
; 
;*******************************************************************
; specifying the path to the model data.
pfile   = pathmodel+MODELroot+construction_period+'_'+month(mon)+runToCorrect+'.dat'
; ... to the output file
outfile = outputdir+'pr_cor_'+construction_period+'_'+month(mon)+runToCorrect+'.dat'
; to the observational data
obsfile = pathOBS+OBSroot+construction_period+'_'+month(mon)+'.dat'
;
print,'Using '+pfile+' and '+obsfile+' to produce correction factors. Stored in ' +outfile+'.'
;
;Get OBS for construction_period*************************************
;
restore,obsfile
pr_o=idldata*86400.0D
top_o=n_elements(pr_o(0,*))
;top_o=n_elements(pr_o(*,*))
print,'N_ELEMENTS OBS= ',top_o
;
;******************************************************************
restore,pfile
pr_e=idldata*86400.0D
top_m=n_elements(pr_e(0,*))
;top_m=n_elements(pr_e(*,*))
print,'N_ELEMENTS MOD= ',top_m
top=min([top_m,top_o]) ;ALE
print,'MINIMUM COMMON N O FELEMENTS = ',top
print,'done restoring '+construction_period+'_'+', month '+month(mon)+' model';******************
;*******************************************************************
;
; Performing a basic consistency check and exiting if any data is negative.
print,'Checking consistency of input data ...'
IF (min(pr_e) LT -1e-10 OR min(pr_o) LT -1e-10) THEN BEGIN
    print,'Model or OBS has negative precipitation!'
ENDIF ELSE BEGIN
    print,'Data passed basic consistency check.'
ENDELSE
;
;Define bias correction parameters
;
; initializations of several arrays for the bias correction data.
fit_type=land*0.0        ; to keep track of the type of fit used.
i0_val  =land*0.0        ; the number of dry days at each grid box.
top_val =land*0.0        ; the value where outliers are removed.
error_pr=land*0.0+1e+33  ; an error field.
X0_pr   =land*0.0+1e+33  ; an index above which model precip > 0
a_pr    =land*0.0+1e+33  ; the offset fit parameter
b_pr    =land*0.0+1e+33  ; the slope fit parameter
tau_pr  =land*0.0+1e+33  ; the decay coefficient in the exponential
;
;******************************************************************
; define plotting axis
plot,[lon(0)],[dims(1)-lat(0)],xrange=[0,dims(0)],yrange=[0,dims(1)]
xyouts,0,0.1*dims(1),'month = '+month(mon)
;
;******************************************************************
;START LOOP
; get the number of time steps
l=n_elements(pr_o(0,*))*1.0
for i = 0L,(NUMLANDPOINTS-1) do begin 
n=i
oplot,[lon(n)],[dims(1)-lat(n)],psym=3
;
; y (x) are sorted precip vectors of the current land point n
y=reform(pr_o(n,sort(pr_o(n,*)))) ; OBS
x=reform(pr_e(n,sort(pr_e(n,*)))) ; model data
;

we_y=where(y ge 0.)
we_x=where(x ge 0.)
if (n_elements(we_y) gt 0.9*l and n_elements(we_x) gt 0.9*l) then begin
; in this case a fit is at least attempted.
; set the negatives to zero and re-sort
y1=y*0.0
if (we_y(0) ne -1) then y1(we_y)=y(we_y)
y=reform(y1(sort(y1)))

x1=x*0.0
if (we_x(0) ne -1) then x1(we_x)=x(we_x)
x=reform(x1(sort(x1)))
;print,i
;print,'N of MODEL TIMESTEPS = ',N_ELEMENTS(x)
;print,'N of OBS TIMESTEPS = ',N_ELEMENTS(y)

;
;******************************************************
;  remove the outliers
;*****************************************************
; some of the data may be considered outlier and is removed.
   wx=where(x gt precip_cutoff,countx)
   wy=where(y gt precip_cutoff,county)
; if there are more than 10 values of precip greater than precip_cutoff,
; perform a quick probability analysis: 
if (countx gt 10 and county gt 10) then begin
   xx=x(wx)
   yy=y(wy)
   max_x=mean(xx)+gauss_cvf(0.01*0.5/n_elements(xx))*stddev(xx)
   max_y=mean(yy)+gauss_cvf(0.01*0.5/n_elements(yy))*stddev(yy)
   outliers=min([where([y,max_y+1] gt max_y),where([x,max_x+1] gt max_x)])
   if (outliers ge 0) then top=outliers
; the arrays x and y are shortened by removing the outliers.
   x=x(0:top-1)
   y=y(0:top-1)
endif
;print,'N of MODEL TIMESTEPS (no outlets) X= ',N_ELEMENTS(x)
;print,'N of OBS TIMESTEPS (no outlets) Y= ',N_ELEMENTS(y)
;
;******************************************************
;********* fitting procedure ************************
;
; several fitting algorithms may be useful, depending on number of
; available data points in model and observations and the degree to
; which they converge.
;
; the mean will be used many times in the following, so let's save
; computing time.
meanx=mean(x)
meany=mean(y)
;
i0=min([top-1,max([0,where(y lt precip_cutoff)])  ])
a=[meany-meanx,1]
;
stat=100
;
; a vector for recording the dry day numbers
i0_val(i)=i0
top_val(i)=top
;
; computing a linear fit coefficient to the transfer function.
; The value of 0.01 is hard coded here. It is the value taken as a
; threshold for desert areas, roughly 3.6 mm/year of rain. Regions
; with that little rain are not fitted but the mean is corrected by an
; additive correction.
;help,i0_val
;help,top_val
;help,i0,top
;
if (i0 lt top-20 and meany gt 0.01) then begin 
; in the following a linear fit is computed.
;print,i0,top
;help,x
;help,y
;
;selecting the finite precip values (greater than precip_cutoff but
;not outliers)
    x0=x(i0+1:top-1)
    y0=y(i0+1:top-1)
    x0m=mean(x0)
    y0m=mean(y0)
    yx0m=mean(x0*y0)
    xx0m=mean(x0*x0)
    divisor=x0m^2-xx0m
    IF (divisor NE 0.0) THEN BEGIN ; this should only not be the case of all values are equal. In that case a multiplicative correction of the mean is best.
        a(1)=(y0m*x0m-yx0m)/divisor
        a(0)=y0m-a(1)*x0m
        er=sqrt(    mean(    (y0-a(0)-a(1)*x0)^2       )        )
    ENDIF ELSE BEGIN
        a(0) = 0
        a(1) = meany/meanx
    ENDELSE
    alin=a
; 
; outputting the linear fit coefficients.
    fit_type(n)=1.0
;
; If the linear fit doesn't work, try other options:
;
;  if a(0) (offset) gt 0 or a(1) (slope) too extreme, try an
;  exponential fit. an offset > 0 is problematic, because we don't
;  want to produce finite precip values from zeros in the model.
    if ((a(0) gt 0) or a(1) lt 0.2 or a(1) gt 5 ) then begin
;lin did not work
        er=1e33
        a=[alin(0),alin(1),0.9] ;for funct1
        w=replicate(1.0,n_elements(x0))
; fitting an exponential distribution: y=(A+B.x)*(1-exp(-x/Tau))
; the exponential forces the fit to go to zero in the limit x->0
        g=curvefit(x0-x0(0),y0,w,a,function_name='funct1',/double,$
                   status=stat,yerror=er,itmax=1000,/noderivative,$
                   iter=iter,tol=1e-10,fita=[1,1,1])
        fit_type(n)=2.0
;
        if  ((stat ne 0)  $
             or  (a(0) lt -100 or a(0) gt 100) $
             or  (a(1) lt    0 or a(1) gt  10) $
             or  (a(2) le    0 or a(2) gt Tau_cutoff) ) then begin ;
;exp1 did not work
            er=1e33
            a=[alin(0),alin(1),0.9] ;for funct1
            w=replicate(1.0,n_elements(x0))
;
; we now try a curvefit where the B parameter remains unchanged
            g=curvefit(x0-x0(0),y0,w,a,function_name='funct1',/double,$
                       status=stat,yerror=er,itmax=1000,/noderivative,$
                       iter=iter,tol=1e-10,fita=[1,0,1])
            fit_type(n)=3.0
;
            if  ((stat ne 0)  $
                 or  (a(0) lt -100 or a(0) gt 100) $
                 or  (a(2) le    0 or a(2) gt Tau_cutoff) ) then begin 
;exp2 did not work
; if these fit values for the offset or the decay coefficient are too extreme, we correct the slope only 
                a=[0,meany/meanx,1e33]
                fit_type(n)=4.0
; if the slope is too extreme, correct the means only by shifting the offset.
                if  (a(1) lt 0.2 or a(1) gt 5) then begin 
;lin2 did not work
                    a=[meany-meanx,1,1e33]
                    fit_type(n)=5.0
                endif
            endif
        endif
        tau_pr(n)=a(2)
    endif
    error_pr(n)=er
endif ; i0 < top-20 ________________________________________________________
if (i0 ge top-20 or meany le 0.01) then $
  a=[meany-meanx,1,1e33]
;print,n
X0_pr(n) =x(i0)
a_pr(n)  =a(0)
b_pr(n)  =a(1)
;
endif else begin
    a_pr(n)=0
    b_pr(n)=1
    X0_pr(n)=1e33
    tau_pr(n)=1e33
endelse
endfor  ; END LOOP ON LANDPOINTS


;
save,filename=outfile,$
  a_pr,b_pr,error_pr,tau_pr,x0_pr
idldata=fit_type
save,filename=pathmodel+'fittype_'+month(mon)+'.dat',idldata
idldata=i0_val
save,filename=pathmodel+'i0val_'+month(mon)+'.dat',idldata
idldata=top_val
save,filename=pathmodel+'topval_'+month(mon)+'.dat',idldata
;
endfor
;
close,2
end
