;
pro prob_ks,alam,probks

EPS1=0.001
EPS2=1.e-8 
NITER=100

      a2=-2.*alam^2.  
      fac=2.  
      probks=0.  
      termbf=0.  ;previous term in sum
      end_sum=0

      FOR j=1,NITER DO BEGIN 
      term=fac*exp(DOUBLE(a2*j^2))
      probks=probks+term  
;      print,'STEP= ',j,alam,a2,term,probks

       IF ((abs(term) LE EPS1*termbf) OR (abs(term) LE EPS2*probks)) THEN BEGIN
;       print,'SERIES CONVERGED at n=',j
;       print,'TERM=',alam,a2,term
;      print,'PROBKS=',probks
	       j=NITER
	       end_sum=1
	      ; STOP 
       ENDIF
       fac=-fac  ;alternating signs in sum
       termbf=abs(term)  
      ENDFOR
      IF end_sum EQ 0 THEN BEGIN
;      print,'SERIES DOES NOT CONVERGE'
      probks=1.  ;only if sums does not converge => teh function is very close to 1
;      print,'PROBKS=',probks
      ENDIF
END  
																						      	 
