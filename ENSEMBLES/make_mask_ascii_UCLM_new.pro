pro make_mask_ascii_UCLM_new  ;infile,loopend

; some settings
DEVICE,decompose=0

;************************************************************
ekenn='UCLM-PROMES'


;ekenn='INMRCA3'

;READ_CONSTANT_FIELDS
	c_file='/media/disk/ENSEMBLES/'+ekenn+'/input/'+ekenn+'_CTL_ERA40_FIX_50km_orog.nc' ;ens044ns044
	c_file2='/media/disk/ENSEMBLES/'+ekenn+'/input/'+ekenn+'_CTL_ERA40_FIX_50km_sftls.nc' ;ens044ns044
	c_file3='/media/disk/ENSEMBLES/'+ekenn+'/input/'+ekenn+'_CTL_ERA40_DM_50km_1971-1980_ps.nc' ;ens044ns044

ncid = NCDF_OPEN(strcompress(c_file, /remove_all))
; Retrieve general information about this netCDF file.
ncidinfo = NCDF_INQUIRE(ncid)   ;ndims,nvars,ngatts,recdim
idname = STRARR(ncidinfo.ndims)
idsize = INTARR(ncidinfo.ndims)
; information about dimensions
for i=0, ncidinfo.ndims-1 do begin
  NCDF_DIMINQ,ncid,i,name,size           ;names, size
  idname(i) = name
  idsize(i) = size
 endfor
; Place the desired variables in local arrays.
 varnames = STRARR(ncidinfo.Nvars)
for i=0, ncidinfo.Nvars-1 do begin
     vardata = NCDF_VARINQ(ncid, i)       ;name,datatype,ndims,natts,dim
     varnames(i) = vardata.Name
endfor
         tnx = 1
         tny = 1
         tnz = 1
         tnt = 1
FOR i=0, ncidinfo.Nvars-1 DO BEGIN
  vardata = NCDF_VARINQ(ncid, i)       ;name,datatype,ndims,natts
  varndims = vardata.ndims
  vardims  = vardata.dim
  FOR j=0,varndims-1 DO BEGIN
    FOR jj=0,ncidinfo.ndims-1 DO BEGIN
      IF (vardims[j] EQ jj) THEN BEGIN
         CASE (idname(jj)) OF
                "lon" : BEGIN
                  tnx    = idsize(jj)
                  xcname = idname(jj)
            END
                "lat" : BEGIN
                   tny    = idsize(jj)
                   ycname = idname(jj)
            END
	        "rlon" : BEGIN
	          tnx    = idsize(jj)
	          xcname = idname(jj)
	    END
	        "rlat" : BEGIN
	         tny    = idsize(jj)
	       ycname = idname(jj)
	    END
                "x" : BEGIN
                  tnx    = idsize(jj)
                  xcname = idname(jj)
            END
                "y" : BEGIN
                   tny    = idsize(jj)
                   ycname = idname(jj)
            END
               "time" : BEGIN
                tnt    = idsize(jj)
                tcname = idname(jj)
            END
            ELSE   : BEGIN
              tnz    = idsize(jj)
              zcname = idname(jj)
            END
          END
  ENDIF
ENDFOR
ENDFOR
;read data
     tmpfield=fltarr(tnx,tny)
         NCDF_VARGET, ncid, vardata.Name, tmpfield
         IF (vardata.Name EQ 'lon') THEN lon=tmpfield
         IF (vardata.Name EQ 'lat') THEN lat=tmpfield
         IF (vardata.Name EQ 'orog') THEN HSURF=tmpfield
ENDFOR
NCDF_CLOSE, ncid

ncid = NCDF_OPEN(strcompress(c_file2, /remove_all))
; Retrieve general information about this netCDF file.
ncidinfo = NCDF_INQUIRE(ncid)   ;ndims,nvars,ngatts,recdim
idname = STRARR(ncidinfo.ndims)
idsize = INTARR(ncidinfo.ndims)
; information about dimensions
for i=0, ncidinfo.ndims-1 do begin
  NCDF_DIMINQ,ncid,i,name,size           ;names, size
  idname(i) = name
  idsize(i) = size
 endfor
; Place the desired variables in local arrays.
 varnames = STRARR(ncidinfo.Nvars)
for i=0, ncidinfo.Nvars-1 do begin
     vardata = NCDF_VARINQ(ncid, i)       ;name,datatype,ndims,natts,dim
     varnames(i) = vardata.Name
endfor
         tnx = 1
         tny = 1
         tnz = 1
         tnt = 1
FOR i=0, ncidinfo.Nvars-1 DO BEGIN
  vardata = NCDF_VARINQ(ncid, i)       ;name,datatype,ndims,natts
  varndims = vardata.ndims
  vardims  = vardata.dim
  FOR j=0,varndims-1 DO BEGIN
    FOR jj=0,ncidinfo.ndims-1 DO BEGIN
      IF (vardims[j] EQ jj) THEN BEGIN
         CASE (idname(jj)) OF
                "lon" : BEGIN
                  tnx1    = idsize(jj)
                  xcname = idname(jj)
            END
                "lat" : BEGIN
                   tny1    = idsize(jj)
                   ycname = idname(jj)
            END
	        "rlon" : BEGIN
	          tnx1    = idsize(jj)
	          xcname = idname(jj)
	    END
	        "rlat" : BEGIN
	         tny1    = idsize(jj)
	       ycname = idname(jj)
	    END
                "x" : BEGIN
                  tnx1    = idsize(jj)
                  xcname = idname(jj)
            END
                "y" : BEGIN
                   tny1    = idsize(jj)
                   ycname = idname(jj)
            END
               "time" : BEGIN
                tnt    = idsize(jj)
                tcname = idname(jj)
            END
            ELSE   : BEGIN
              tnz    = idsize(jj)
              zcname = idname(jj)
            END
          END
  ENDIF
ENDFOR
ENDFOR
;read data
     tmpfield=fltarr(tnx,tny)
         NCDF_VARGET, ncid, vardata.Name, tmpfield
         IF (vardata.Name EQ 'lon') THEN lon1=tmpfield
         IF (vardata.Name EQ 'lat') THEN lat1=tmpfield
         IF (vardata.Name EQ 'sftls') THEN sftls=tmpfield
ENDFOR
NCDF_CLOSE, ncid


print,'DIMENSIONS: nx,ny,nt'
print,tnx,tny
help,hsurf
print,lon(0,0),lat(0,0),lon(tnx-1,tny-1),lat(tnx-1,tny-1)
print,tnx1,tny1
help,sftls
print,lon1(0,0),lat1(0,0),lon1(tnx1-1,tny1-1),lat1(tnx1-1,tny1-1)

ncid = NCDF_OPEN(strcompress(c_file3, /remove_all))
; Retrieve general information about this netCDF file.
ncidinfo = NCDF_INQUIRE(ncid)   ;ndims,nvars,ngatts,recdim
idname = STRARR(ncidinfo.ndims)
idsize = INTARR(ncidinfo.ndims)
; information about dimensions
for i=0, ncidinfo.ndims-1 do begin
  NCDF_DIMINQ,ncid,i,name,size           ;names, size
  idname(i) = name
  idsize(i) = size
 endfor
; Place the desired variables in local arrays.
 varnames = STRARR(ncidinfo.Nvars)
for i=0, ncidinfo.Nvars-1 do begin
     vardata = NCDF_VARINQ(ncid, i)       ;name,datatype,ndims,natts,dim
     varnames(i) = vardata.Name
endfor
         tnx2 = 1
         tny2 = 1
         tnz2 = 1
         tnt2 = 1
FOR i=0, ncidinfo.Nvars-1 DO BEGIN
  vardata = NCDF_VARINQ(ncid, i)       ;name,datatype,ndims,natts
  varndims = vardata.ndims
  vardims  = vardata.dim
  FOR j=0,varndims-1 DO BEGIN
    FOR jj=0,ncidinfo.ndims-1 DO BEGIN
      IF (vardims[j] EQ jj) THEN BEGIN
         CASE (idname(jj)) OF
                "lon" : BEGIN
                  tnx2    = idsize(jj)
                  xcname = idname(jj)
            END
                "lat" : BEGIN
                   tny2    = idsize(jj)
                   ycname = idname(jj)
            END
	        "rlon" : BEGIN
	          tnx2    = idsize(jj)
	          xcname = idname(jj)
	    END
	        "rlat" : BEGIN
	         tny2    = idsize(jj)
	       ycname = idname(jj)
	    END
                "x" : BEGIN
                  tnx2    = idsize(jj)
                  xcname = idname(jj)
            END
                "y" : BEGIN
                   tny2    = idsize(jj)
                   ycname = idname(jj)
            END
               "time" : BEGIN
                tnt    = idsize(jj)
                tcname = idname(jj)
            END
            ELSE   : BEGIN
              tnz    = idsize(jj)
              zcname = idname(jj)
            END
          END
  ENDIF
ENDFOR
ENDFOR
;read data
     tmpfield=fltarr(tnx2,tny2)
     tmpfield1=fltarr(tnx2,tny2,tnt)
         NCDF_VARGET, ncid, vardata.Name, tmpfield
         IF (vardata.Name EQ 'lon') THEN lon2=tmpfield
         IF (vardata.Name EQ 'lat') THEN lat2=tmpfield
         IF (vardata.Name EQ 'ps') THEN ps=tmpfield1
ENDFOR
NCDF_CLOSE, ncid

print,'DIMENSIONS: nx,ny,nt'
print,tnx2,tny2,tnt
help,ps
print,lon2(0,0),lat2(0,0),lon2(tnx2-1,tny2-1),lat1(tnx2-1,tny2-1)

nx0=9
ny0=9
hsurf3=reform(hsurf(nx0:nx0+tnx1-1,ny0:ny0+tny1-1))
;sftls2=reform(sftls(nx0:nx0+tnx1-1,ny0:ny0+tny1-1))
lon3=reform(lon(nx0:nx0+tnx1-1,ny0:ny0+tny1-1))
lat3=reform(lat(nx0:nx0+tnx1-1,ny0:ny0+tny1-1))

id = NCDF_CREATE('test_orog.nc',/CLOBBER)  
xid = NCDF_DIMDEF(id, 'x', tnx1) ; Defne the X dimension.  
yid = NCDF_DIMDEF(id, 'y', tny1) ; Define the Y dimension.  
vid0 = NCDF_VARDEF(id, 'lon', [xid, yid], /FLOAT)  
vid1 = NCDF_VARDEF(id, 'lat', [xid, yid], /FLOAT)  
vid2 = NCDF_VARDEF(id, 'orog', [xid, yid], /FLOAT)  
NCDF_CONTROL, id, /ENDEF 
NCDF_VARPUT, id, vid0,lon3
NCDF_VARPUT, id, vid1,lat3
NCDF_VARPUT, id, vid2,hsurf3
NCDF_CLOSE, id

;id = NCDF_CREATE('test_sftls.nc',/CLOBBER)  
;xid = NCDF_DIMDEF(id, 'x', tnx1) ; Defne the X dimension.  
;yid = NCDF_DIMDEF(id, 'y', tny1) ; Define the Y dimension.  
;vid0 = NCDF_VARDEF(id, 'lon', [xid, yid], /FLOAT)  
;vid1 = NCDF_VARDEF(id, 'lat', [xid, yid], /FLOAT)  
;vid2 = NCDF_VARDEF(id, 'sftls', [xid, yid], /FLOAT)  
;NCDF_CONTROL, id, /ENDEF 
;NCDF_VARPUT, id, vid0,lon2
;NCDF_VARPUT, id, vid1,lat2
;NCDF_VARPUT, id, vid2,sftls2
;NCDF_CLOSE, id

; End procedure.
END
