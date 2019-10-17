;
IF (P_CORRECT EQ 1) THEN BEGIN

header=['precip','mm/s','total daily precipitation','OBS precipitation']

YEAR_START=CONSTRUCTION_PERIOD_START
YEAR_STOP=CONSTRUCTION_PERIOD_STOP
outNameBase=pathOBS+OBSroot+construction_period
print,convert_org_data_function(outNameBase, YEAR_START, YEAR_STOP, month, lat, lon, CONVERT_ORG_DATA,'',header,dims)
;
header=['precip','mm/s','total daily precipitation','model precipitation']
YEAR_START=APPLICATION_PERIOD_START
YEAR_STOP=APPLICATION_PERIOD_STOP
outNameBase=pathmodel+MODELroot+application_period 
print,convert_org_data_function(outNameBase, YEAR_START, YEAR_STOP, month, lat, lon, CONVERT_ORG_DATA,runToCorrect,header,dims)
;
header=['snow','mm/s','total daily snowfall','model snowfall']
YEAR_START=APPLICATION_PERIOD_START
YEAR_STOP=APPLICATION_PERIOD_STOP
outNameBase=pathmodel+MODELroot_PRSN+application_period 
print,convert_org_data_function(outNameBase, YEAR_START, YEAR_STOP, month, lat, lon, CONVERT_ORG_DATA,runToCorrect,header,dims)
;
ENDIF
;
IF (T_CORRECT EQ 1) THEN BEGIN
header=['Tmean','Kelvin','daily mean temperature','OBS mean temperature']
YEAR_START=CONSTRUCTION_PERIOD_START
YEAR_STOP=CONSTRUCTION_PERIOD_STOP
;
outNameBase=pathOBS+OBSroot_T+construction_period
print,convert_org_data_function(outNameBase, YEAR_START, YEAR_STOP, month, lat, lon, CONVERT_ORG_DATA,'',header,dims)
;
header=['Tmin','Kelvin','daily min temperature','OBS min temperature']
outNameBase=pathOBS+OBSroot_Tmin+construction_period
print,convert_org_data_function(outNameBase, YEAR_START, YEAR_STOP, month, lat, lon, CONVERT_ORG_DATA,'',header,dims)
;
header=['Tmax','Kelvin','daily max temperature','OBS max temperature']
outNameBase=pathOBS+OBSroot_Tmax+construction_period
print,convert_org_data_function(outNameBase, YEAR_START, YEAR_STOP, month, lat, lon, CONVERT_ORG_DATA,'',header,dims)
;
YEAR_START=APPLICATION_PERIOD_START
YEAR_STOP=APPLICATION_PERIOD_STOP
;
header=['Tmean','Kelvin','daily mean temperature','model mean temperature']
outNameBase=pathmodel+MODELroot_T+application_period 
print,convert_org_data_function(outNameBase, YEAR_START, YEAR_STOP, month, lat, lon, CONVERT_ORG_DATA, runToCorrect,header,dims)
;
header=['Tmin','Kelvin','daily min temperature','model min temperature']
outNameBase=pathmodel+MODELroot_Tmin+application_period 
print,convert_org_data_function(outNameBase, YEAR_START, YEAR_STOP, month, lat, lon, CONVERT_ORG_DATA, runToCorrect,header,dims)
;
header=['Tmax','Kelvin','daily max temperature','model max temperature']
outNameBase=pathmodel+MODELroot_Tmax+application_period 
print,convert_org_data_function(outNameBase, YEAR_START, YEAR_STOP, month, lat, lon, CONVERT_ORG_DATA, runToCorrect,header,dims)
;
ENDIF
;
END
