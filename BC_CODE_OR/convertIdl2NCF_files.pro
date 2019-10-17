filesP_in = outputdir+'pr_cor_'+construction_period+'_'+month+runToCorrect+'.dat'
filesT_in = outputdir+'tas_cor_'+construction_period+'_'+month+runToCorrect+'.dat'
;
ntypes=[4,6]
typesP=['a','b','tau','x0']
typesT=['a_tas','b_tas','a_td','b_td','a_tf','b_tf']
;
standard=1
DATAFORMAT=0
;
for var=0, n_elements(ntypes)-1 do begin
    for type=0,ntypes(var)-1 do begin
        for mon=0,n_elements(month)-1 do begin
            if (var eq 0) then restore,filesP_in(mon)
            if (var eq 0) then restore,filesT_in(mon)
            if (var eq 0) then begin
;                
                if (type eq 0) then begin
                    idldata=a_pr
                    header=['a_pr','unitless','correction factor a_pr','correction factor a_pr']
                endif
                if (type eq 1) then begin
                    idldata=b_pr
                    header=['b_pr','unitless','correction factor b_pr','correction factor b_pr']
                endif
                if (type eq 2) then begin
                    idldata=tau_pr        
                    header=['tau_pr','unitless','correction factor tau_pr','correction factor tau_pr']
                endif
                if (type eq 3) then begin
                    idldata=x0_pr
                    header=['x0_pr','unitless','correction factor x0_pr','correction factor x0_pr']
                endif
                
            endif
            
            if (var eq 1) then begin
                
                if (type eq 0) then begin
                    idldata=a_tas(*,0)
                    header=['a_tas','unitless','correction factor a_tas','correction factor a_tas']
                endif
                if (type eq 1) then begin
                    idldata=a_tas(*,1)
                    header=['b_tas','unitless','correction factor b_tas','correction factor b_tas']
                endif
                if (type eq 2) then begin
                    idldata=a_td(*,0)        
                    header=['a_td','unitless','correction factor a_td','correction factor a_td']
                endif
                if (type eq 3) then begin
                    idldata=a_td(*,1)
                    header=['b_td','unitless','correction factor b_td','correction factor b_td']
                endif
                if (type eq 4) then begin
                    idldata=a_tf(*,0)        
                    header=['a_tf','unitless','correction factor a_tf','correction factor a_tf']
                endif
                if (type eq 5) then begin
                    idldata=a_tf(*,1)
                    header=['b_tf','unitless','correction factor b_tf','correction factor b_tf']
                endif
                
            endif
            save,filename='cor_coeff.dat',idldata
            infile='cor_coeff.dat'
            if (var eq 0) then outfile=outputdir+'pr_cor_'+typesP(type)+'_'+construction_period+'_'+month(mon)+runToCorrect+'.nc'
            if (var eq 1) then outfile=outputdir+'T_cor_'+typesT(type)+'_'+construction_period+'_'+month(mon)+runToCorrect+'.nc'
            YEAR_START=CONSTRUCTION_PERIOD_START
            YEAR_STOP =CONSTRUCTION_PERIOD_STOP
            
            print,idl2latlon_function_v1(1,DATAFORMAT,infile,outfile,YEAR_START,YEAR_STOP,mon,lat,lon,month,header,dims)
        endfor
    endfor
endfor
END
    
