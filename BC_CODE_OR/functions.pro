;;THIS IDL FILE CONTAINS SEVERAL FUNCTION DEFINITIONS REQUIRED
;;FOR THE FITTING PROCEDURE WITHIN THE MONTHLY BIAS CORRECTION.
;;
;; FILENAME: functions.pro
;; AUTHORS:  C. PIANI AND J. O. HAERTER
;; DATE:     SEPTEMBER 28, 2009
;; PROJECT:  EU WATCH PROJECT
;;
;;____________________________________________________________
;;
PRO funct1, X, C, Y , pder
;Y = (A' + B*x)*(1-exp(((x-x0)/tau)) this is the curve we
; are trying to fit. However you can calculate X0 from your
; transform curve directly and use (x-xo) as your independent
; variable fitting:
;  Y =  (A + B*(x-xo))*(1-exp(((x-x0)/tau))
; where: A' = A-B*x0
;In the line bellow I will assume you are going to
; use X = (x-xo) as your independent var:
;
;print,'CALCULATING FUNCTIONS'
A=C(0)
B=C(1)
tau=C(2)
Y = (A + B*x)*(1-exp(-x/tau))
dyda=1-exp(-x/tau)
dydb=x*(1-exp(-x/tau))
dydtau=(a+b*x)*exp(-x/tau)*x/tau^2
pder=[[dyda],[dydb],[dydtau]]
end

;***********************************
