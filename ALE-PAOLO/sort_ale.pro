;
PRO sort_ale,arr,as

as=arr
aw=arr
as(*)=0.
n=N_ELEMENTS(as)

;print,arr
;print,''
j=0
i=0
;FOR i=0,n-1 DO BEGIN
WHILE  i LT n AND j LT n DO BEGIN
aww=aw

;	aww=reform(aw(i:n-1))
	;aww=reform(aw(j:n-1))
	       ;print,'I',i
	;print,'AWW',aww
;	print,'AWW',aww
;	index=WHERE(aww EQ min(aww))
	index=WHERE((aww EQ min(aww)),count, COMPLEMENT=B_C, NCOMPLEMENT=count_c)
;	IF N_ELEMENTs(index) GT 1 THEN BEGIN
;		;print,i,i+N_ELEMENTs(index)-1
;		;as(i:i+N_ELEMENTs(index)-1)=min(aww)
;	       ;aw(index+i)=aw(i)
;	       aw(index)=aw(i)
;        	as(j:j+N_ELEMENTs(index)-1)=min(aww)
;              ;i=i+N_ELEMENTs(index) 
;              j=j+N_ELEMENTs(index) 
;              i= i+1
;
;        ENDIF ELSE BEGIN
;	;as(i)=min(aww)
;	as(j)=min(aww)
;	aw(index)=aw(i)
;	j=j+1
;	i=i+1
;	ENDELSE
;        IF i EQ 0 THEN  BEGIN
;        as(i:i+count-1)=min(aww)
;        ENDIF ELSE BEGIN
;        as(j:j+count-1)=min(aww)
;	ENDELSE

	;print,'J OLd',j
	j_o=j
        j= j_o+count
	;print,'J',j
	;print,'PLACES',j_o,j-1
        as(j_o:j-1)=min(aww)
        i=i+1
	IF count_c GT 0 THEN BEGIN
       	aw=fltarr(count_c)
	aw=aww(B_C)
        ENDIF

	;print,'INDEX',index,'MIN',min(aww)
	;print,'A',arr
	;print,'AW',aw
	;print,'AS',as
        ;print,''
;ENDFOR
ENDWHILE
;print,arr
;print,as

END
