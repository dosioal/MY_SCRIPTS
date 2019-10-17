; calls FUNCTION convert_org_data_function_v1, outFile, month, rtc, types, typ
;
prcddir=pathdat+BC_runnumber+'/'+'processed/'
spawn,'mkdir -p '+prcddir
ourmonth=['01','07']
if (T_Correct EQ 1 and P_Correct EQ 1) then types=['Ptot_BCed_','Psno_BCed_','T_BCed_','Tmax_BCed_','Tmin_BCed_']
if (T_Correct EQ 1 and P_Correct EQ 0) then types=['T_BCed_','Tmax_BCed_','Tmin_BCed_']
if (T_Correct EQ 0 and P_Correct EQ 1) then types=['Ptot_BCed_','Psno_BCed_']
;
;
IF (P_CORRECT EQ 1) THEN BEGIN
type='Ptot'
outNameBase=pathOBS +OBSroot+construction_period
outNameBase1=prcddir+OBSroot+construction_period
print,convert_org_data_function_v1(outNameBase,outNameBase1,prcddir, ourmonth,'',type)
;
outNameBase=pathOBS +OBSroot+application_period
outNameBase1=prcddir+OBSroot+application_period
print,convert_org_data_function_v1(outNameBase,outNameBase1,prcddir, ourmonth,'',type)
;
outNameBase=pathmodel+MODELroot+application_period
outNameBase1=prcddir +MODELroot+application_period
print,convert_org_data_function_v1(outNameBase,outNameBase1,prcddir, ourmonth,'_',type)
;
ENDIF
;
IF (T_CORRECT EQ 1) THEN BEGIN
;
type='T'
outNameBase=pathOBS +OBSroot_T+construction_period
outNameBase1=prcddir+OBSroot_T+construction_period
print,convert_org_data_function_v1(outNameBase,outNameBase1,prcddir, ourmonth,'',type)
;
type='T'
outNameBase=pathOBS +OBSroot_T+application_period
outNameBase1=prcddir+OBSroot_T+application_period
print,convert_org_data_function_v1(outNameBase,outNameBase1,prcddir, ourmonth,'',type)
;
type='Tmin'
outNameBase=pathOBS +OBSroot_Tmin+construction_period
outNameBase1=prcddir+OBSroot_Tmin+construction_period
print,convert_org_data_function_v1(outNameBase,outNameBase1,prcddir, ourmonth,'',type)
;
type='Tmin'
outNameBase=pathOBS +OBSroot_Tmin+application_period
outNameBase1=prcddir+OBSroot_Tmin+application_period
print,convert_org_data_function_v1(outNameBase,outNameBase1,prcddir, ourmonth,'',type)
;
type='Tmax'
outNameBase=pathOBS +OBSroot_Tmax+construction_period
outNameBase1=prcddir+OBSroot_Tmax+construction_period
print,convert_org_data_function_v1(outNameBase,outNameBase1,prcddir, ourmonth,'',type)
;
type='Tmax'
outNameBase=pathOBS +OBSroot_Tmax+application_period
outNameBase1=prcddir+OBSroot_Tmax+application_period
print,convert_org_data_function_v1(outNameBase,outNameBase1,prcddir, ourmonth,'',type)
;
type='T'
outNameBase=pathmodel+MODELroot_T+application_period 
outNameBase1=prcddir +MODELroot_T+application_period 
print,convert_org_data_function_v1(outNameBase,outNameBase1,prcddir, ourmonth,'_',type)
;
type='Tmin'
outNameBase=pathmodel+MODELroot_Tmin+application_period 
outNameBase1=prcddir +MODELroot_Tmin+application_period 
print,convert_org_data_function_v1(outNameBase,outNameBase1,prcddir, ourmonth,'_',type)
;
type='Tmax'
outNameBase=pathmodel+MODELroot_Tmax+application_period 
outNameBase1=prcddir +MODELroot_Tmax+application_period 
print,convert_org_data_function_v1(outNameBase,outNameBase1,prcddir, ourmonth,'_',type)
;
ENDIF
;
END
