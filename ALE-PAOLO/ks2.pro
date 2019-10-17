;
pro ks2,data1,n1,data2,n2,d,prob,ffn1,ffn2,data_1,data_2

;data_1=sort(data1)  
;data_2=sort(data2)  
sort_ale,data1,data_1 
sort_ale,data2,data_2 

ffn1=fltarr(n1)
ffn2=fltarr(n2)

    en1=n1  
    en2=n2  
    j1=1.  
    j2=1. 
    fn1=0.  
    fn2=0.  
    d=0.  
;1   IF (j1 LE n1 AND j2 LE n2) THEN BEGIN 
   WHILE (j1 LE n1 AND j2 LE n2) DO BEGIN 
     d1=data_1(j1-1)  
     ;print,'D1',d1
     d2=data_2(j2-1)  
     ;print,'D2',d2
     IF (d1 LE d2) THEN BEGIN 
      fn1=j1/en1  
      ffn1(j1-1)=fn1
      j1=j1+1  
     ENDIF  
     IF (d2 LE d1) THEN BEGIN
      fn2=j2/en2  
      ffn2(j2-1)=fn2
      j2=j2+1  
     ENDIF  
     dt=abs(fn2-fn1)  
     ;print,d,dt
     IF (dt GT d) THEN  d=dt  
;   goto 1  
;    ENDIF  
;print,n1,n2,d1,d2,j1,j2,en1,en2,fn1,fn2,dt,d
    ENDWHILE 
   en=sqrt(en1*en2/(en1+en2))  
;   prob=probks((en+0.12+0.11/en)*d)  
;print,en,en+0.12+0.11/en
   prob_ks,(en+0.12+0.11/en)*d,prob
 END  
																																												      	 
																																												      	
