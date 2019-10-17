
IF (T_prepare EQ 1) THEN BEGIN
    print,'preparing T files'
    spawn,'./preparefiles_T'
ENDIF

IF (P_prepare EQ 1)   THEN  BEGIN
    print,'preparing p files'
    spawn,'./preparefiles_P' 
ENDIF

IF (T_computeCor EQ 1)   THEN  BEGIN
    print,'computing correction coefficients for T'
    spawn,'./construct_T_cor'
ENDIF 

IF (P_computeCor EQ 1)   THEN  BEGIN
    print,'computing correction coefficients for p'
    spawn,'./construct_P_cor'
ENDIF

IF (T_apply EQ 1)     THEN  BEGIN
    print,'applying T correction'
    spawn,'./apply_T_correction'
ENDIF

IF (P_apply EQ 1)     THEN BEGIN
    print,'applying P correction'
    spawn,'./apply_P_correction'
ENDIF

IF (writeoutput EQ 1) THEN BEGIN
    print,'writing final files'
    spawn,'./writeFinalFiles'
ENDIF

END
