;+
; NAME:
;    IDL_LIB_HTML
;
; PURPOSE:
;    This procedure creates the web page listing of a library's IDL utilities.
;
; CATEGORY:
;    Miscellaneous
;
; CALLING SEQUENCE:
;    IDL_LIB_HTML
;
; KEYWORD PARAMETERS:
;    ADMIN_WEB:  The administrator's web page.  The default is defined in the 
;        Constants section.
;    ADMIN_NAME:  The administrator's name.  The default is defined in the 
;        Constants section.
;    ADMIN_EMAIL:  The administrator's e-mail address.  The default is defined 
;        in the Constants section.
;    CATEG:  A list of function and procedure categories to which to restrict.
;    FIRST_UPDATE:  A vector of strings containing information on updates to be 
;        added before the list of routines.
;    INDIR:  A scalar or vector string containing the directories in which to 
;        search for files.  The default is the current directory.
;    INFILE:  The flag for filenames to include in the library.  The default is 
;        '*.pro'.
;    MODIFIED_AUTHOR:  If set then authors under the "Modified" heading are 
;        included as Contributors in the output webpage.
;    OUTFILE:  The filename for the output webpage.  The default is 
;        'idl_lib.html'.
;    REVERSE_CATEGORY:  Order the categories in the output webpage in reverse 
;        alphabetical order.  The default is alphabetical order.
;    TAR:  If set, then a .tar archive file is created containing all of the 
;        routines in the library.  This archive will be included on the web 
;        page.
;    TITLE_FIRST_UPDATE:  A string contain the title of the FIRST_UPDATE list 
;        of information.  The default is 'UPDATES'.
;    TITLE_PAGE:  The title of the web page.
;    TITLE_UPDATE:  A string contain the title of the UPDATE list of 
;        information.  The default is 'UPDATES'.
;    UPDATE:  A vector of strings containing information on updates to be added 
;        after the list of routines.
;    ZIP:  If set, then a .zip archive file is created containing all of the 
;        routines in the library.  This archive will be included on the web 
;        page.
;
; USES:
;    month_name.pro
;    str.pro
;    string_from_vector.pro
;
; PROCEDURE:
;    This procedure reads the documentation of the IDL files in the group 
;    utility directory, and creates a web page that lists them.
;
; MODIFICATION HISTORY:
;    Written by:  Daithi A. Stone (stoned@atm.ox.ac.uk), 2002-02-08.
;    Modified:  DAS, 2002-04-10 (STD.pro removal update)
;    Modified:  DAS, 2002-08-09 (CONSTANTS.pro changes update)
;    Modified:  DAS, 2002-11-29 (altered directory structure)
;    Modified:  DAS, 2002-12-06 (removed dependency on me)
;    Modified:  DAS, 2003-05-07 (renamed, and added ADMIN* keywords)
;    Modified:  DAS, 2004-06-24 (added old modification date printing)
;    Modified:  DAS, 2005-09-01 (added FIRST_UPDATE, FUTITLE, INDIR, INFILE, 
;        MODIFIED_AUTHOR, OUTFILE, REVERSE_CATEGORY, TITLE, UPDATE, UTITLE 
;        keywords;  permitted multiple input directories;  removed use of 
;        constants.pro)
;    Modified:  DAS, 2006-03-23 (corrected self-reference;  increased 
;        modification highlighting to 90 days)
;    Modified:  DAS, 2007-05-24 (added TAR and ZIP options)
;    Modified:  DAS, 2008-12-01 (updated administrator details)
;    Modified:  DAS, 2011-11-06 (modified documentation;  altered keyword 
;        parameter names;  modified colours;  added CATEG keyword;  introduced 
;        removal of middle initials in names;  allowed multiple categories per 
;        file)
;    Modified:  DAS, 2013-07-19 (switched from use of findfile to file_search)
;-

;***********************************************************************

PRO IDL_LIB_HTML, $
    ADMIN_WEB=admin_web, ADMIN_NAME=admin_name, ADMIN_EMAIL=admin_email, $
    CATEG=categ_0, $
    FIRST_UPDATE=first_update, TITLE_FIRST_UPDATE=title_first_update, $
    INDIR=indir, INFILE=infile, $
    MODIFIED_AUTHOR=modified_author_opt, $
    OUTFILE=outfile, $
    REVERSE_CATEGORY=reverse_category_opt, $
    TAR=tar_opt, ZIP=zip_opt, $
    TITLE_PAGE=title_page, $
    UPDATE=update, TITLE_UPDATE=title_update

;***********************************************************************
; Constants and variables

; Warnings string array
;update = [ '' ]

; Administrator's information
if not( keyword_set( admin_web ) ) then begin
  ;admin_web = 'http://www.atm.ox.ac.uk/user/stoned/'
  admin_web = 'http://www.csag.uct.ac.za/~daithi'
endif
if not( keyword_set( admin_name ) ) then begin
  admin_name = 'D&aacute;ith&iacute; Stone'
endif
if not( keyword_set( admin_email ) ) then begin
  ;admin_email = 'stoned@atm.ox.ac.uk'
  admin_email = 'stoned@csag.uct.ac.za'
endif

; Input directory
if not( keyword_set( indir ) ) then indir = ''
if not( keyword_set( infile ) ) then infile = '*.pro'
; Output file
if not( keyword_set( outfile ) ) then outfile = 'idl_lib.html'

; The time for mentioning modifications (in days)
hist_delay = 90

; Maximum number of categories per routine
n_categ_max = 5

; The default title
if not( keyword_set( title_page ) ) then begin
  title_page = 'A Library of IDL Programs'
endif

; The default title for the update sections
if not( keyword_set( title_first_update ) ) then title_first_update = 'UPDATES'
if not( keyword_set( title_update ) ) then title_update = 'UPDATES'

; Default colours and text style
; Background colour
;color_background = '"black"'
color_background = '"#bbbb99"'
; Text colour
;color_text = '"#dd8800"'
color_text = '"#222222"'
; Header colour
;color_header = '"#ee0000"'
color_header = '"#aa0000"'
; Link colour
;color_link = '"#00bbff"'
color_link = '"#0033aa"'
; Viewed link colour
;color_viewedlink = '"#0099ff"'
color_viewedlink = '"#0000aa"'
; Text font
font_text = '"helvetica"'
; Header font
font_header = font_text
; Text bold
bold_text_start = '<B>'
bold_text_end = '</B>'
; Text size
size_text = '"4"'
; Header size
size_header_start = '<H2>'
size_header_end = '</H2>'
; Title size
size_title_start = '<H1>'
size_title_end = '</H1>'

; Months in a year
mina=12
; Date and time
time = systime()
date = strmid( time, strlen( time ) - 4, 4 )
months = month_name( indgen( mina ), abbreviate=1 )
for i_months = 0, mina - 1 do begin
  if strpos( time, months[i_months] ) ne -1 then begin
    if i_months + 1 lt 10 then begin
      date = date + '0' + str( i_months + 1 )
    endif else begin
      date = date + str( i_months + 1 )
    endelse
    if fix( strmid( time, 8, 2 ) ) lt 10 then begin
      date = date + '0' + strmid( time, 9, 1 )
    endif else begin
      date = date + strmid( time, 8, 2 )
    endelse
    hour = strmid( time, 11, 8 )
  endif
endfor
date = long( date )

;***********************************************************************
; Set-up for reading IDL files

; Find files
list = file_search( indir[0]+infile, count=n_pro )
n_indir = n_elements( indir )
if n_indir ne 1 then begin
  for i_indir = 1, n_indir - 1 do begin
    list = [ list, file_search( indir[i_indir]+infile, count=temp ) ]
    n_pro = n_pro + temp
  endfor
endif

; Program information vectors
pro_name = strarr( n_pro )
pro_purp = strarr( n_pro )
pro_categ = strarr( n_pro, n_categ_max )
pro_hist = strarr( n_pro )
pro_date = lonarr( n_pro )
pro_auth = strarr( n_pro )
;Number of variables to read
n_read = 4

;***********************************************************************
; Read IDL files

; Set temporary variable, counters
temp_str = ''
check_name = 0
check_purp = 0
check_categ = 0
check_hist = 0

; Iterate through routines
for i_pro = 0, n_pro-1 do begin
  ; Open file
  openr, 1, list[i_pro]
  ; Set counters
  check = 0
  ; Run loop until all information retrieved or end of documentation
  while check le n_read + 1 do begin

    ; Read line from file
    readf, 1, temp_str
    temp_str = strtrim( strcompress( temp_str ), 2 )
    if check eq 0 then begin
      if temp_str eq ';+' then check = 1
    endif

    ; If in documentation section
    if check ge 1 then begin
      ; Check for end of documentation
      if temp_str eq ';-' then check = n_read + 2

      ; Read name
      if check_name ne 0 then begin
        pro_name[i_pro] = strupcase( $
             strtrim( strmid( temp_str, 1, strlen( temp_str ) - 1 ), 2 ) )
        check_name = 0
        check = check + 1
        ; Remove ".pro"
        pos = strpos( pro_name[i_pro], '.PRO' )
        if pos ne -1 then pro_name[i_pro] = strmid( pro_name[i_pro], 0, pos )
      endif

      ; Read purpose
      if check_purp ne 0 then begin
        if strlen( temp_str ) gt 3 then begin
          pro_purp[i_pro] = pro_purp[i_pro] $
              + strmid( temp_str, 1, strlen( temp_str ) - 1 )
        endif else begin
          pro_purp[i_pro] = strtrim( strcompress( pro_purp[i_pro] ), 2 )
          check_purp = 0
          check = check + 1
        endelse
      endif

      ; Read category
      if check_categ ne 0 then begin
        ; Extract category
        temp_str = strupcase( $
            strtrim( strmid( temp_str, 1, strlen( temp_str ) - 1 ), 2 ) )
        ; Check for multiple categories
        temp = strtrim( strsplit( temp_str, ',', extract=1, count=n_temp ), 2 )
        pro_categ[i_pro,0:n_temp-1] = temp
        check_categ = 0
        check = check + 1
      endif

      ; Read history
      if check_hist ne 0 then begin
        if strlen( temp_str ) gt 3 then begin
          ; Get author
          pos = strpos( temp_str, 'Written by:' )
          if pos ne -1 then begin
            pos_1 = strpos( temp_str, ', ' )
            pos_2 = strpos( temp_str, ' (' )
            if pos_2 ne -1 then pos_1 = min( [ pos_1, pos_2 ] )
            pro_auth[i_pro] = strmid( temp_str, pos+12, pos_1-pos-12 )
          endif
          ; Get modifying authors if requested
          if keyword_set( modified_author_opt ) then begin
            pos = strpos( temp_str, 'Modified:' )
            if pos ne -1 then begin
              pos_1 = strpos( temp_str, ', ' )
              pos_2 = strpos( temp_str, ' (' )
              if pos_2 ne -1 then pos_1 = min( [ pos_1, pos_2 ] )
              ; Only save this author name if it is not initials because 
              ; initials are often used as a shorthand for the original author 
              ; who will be in PRO_AUTH anyway
              temp_1 = strmid( temp_str, pos+10, pos_1-pos-10 )
              if strupcase( temp_1 ) ne temp_1 then begin
                if keyword_set( mod_auth ) then begin
                  mod_auth = [ mod_auth, temp_1 ]
                endif else begin
                  mod_auth = temp_1
                endelse
              endif
            endif
          endif
          ; Get more recent date
          pos = strpos( temp_str, '-' )
          if pos ne -1 then begin
            pos_1 = strpos( strmid( temp_str, pos+1, strlen( temp_str )-pos ), $
                '-' )
            if pos_1 eq 2 then begin
              pro_date[i_pro] = long( strmid( temp_str, pos-4, 4 ) $
                  + strmid( temp_str, pos+1, 2 ) $
                  + strmid( temp_str, pos+4, 2 ) )
              check_hist = check_hist + 1
            endif
          endif
        endif else begin
          ; Create modification alert
          if check_hist eq 2 then begin
            pro_hist[i_pro] = 'WRITTEN '
          endif else begin
            pro_hist[i_pro] = 'MODIFIED '
          endelse
          pro_hist[i_pro] = pro_hist[i_pro] $
              + strmid( str( pro_date[i_pro] ), 0, 4 ) + '-' $
              + strmid( str( pro_date[i_pro] ), 4, 2 ) + '-' $
              + strmid( str( pro_date[i_pro] ), 6, 2 )
          check_hist = 0
          check = check + 1
        endelse
      endif

      ; Search for start of documentation fields
      if temp_str eq '; NAME:' then check_name = 1 
      if temp_str eq '; PURPOSE:' then check_purp = 1 
      if temp_str eq '; CATEGORY:' then check_categ = 1 
      if temp_str eq '; MODIFICATION HISTORY:' then check_hist = 1 

    endif
  endwhile

  ; Close file
  close, 1
endfor

;***********************************************************************
; Sort Programs

; Category list
categ_name = ['']
n_categ = 1
; Category classification of programs
pro_categ_index = intarr( n_pro, n_categ_max )

; Build categories
; Go through programs
for i_pro = 0, n_pro-1 do begin
  ; Iterage through possible multiple categories
  for i_categ_mult = 0, n_categ_max - 1 do begin
    if pro_categ[i_pro,i_categ_mult] ne '' then begin
      ; Set counter
      check = 0
      ; Compare program category to category list
      for i_categ = 0, n_categ-1 do begin
        if pro_categ[i_pro,i_categ_mult] eq categ_name[i_categ] then begin
          ; Link to existing category
          pro_categ_index[i_pro,i_categ_mult] = i_categ
          check = 1
        endif
      endfor
      ; Create new category and link
      if check eq 0 then begin
        ; But only if it is consistent with restricted list
        if keyword_set( categ_0 ) then begin
          temp_check = max( categ_0 eq pro_categ[i_pro,i_categ_mult] )
        endif else begin
          temp_check = 1
        endelse
        if temp_check eq 1 then begin
          categ_name = [ categ_name, pro_categ[i_pro,i_categ_mult] ]
          n_categ = n_categ + 1
          pro_categ_index[i_pro,i_categ_mult] = n_categ - 1
        endif
      endif
    endif
  endfor
endfor

; Sort categories
categ_name = categ_name[1:n_categ-1]
n_categ = n_categ - 1
pro_categ_index = pro_categ_index - 1
id_categ_sort = sort( categ_name )
; Reverse order if requested
if keyword_set( reverse_category_opt ) then begin
  id_categ_sort = reverse( id_categ_sort )
endif

; Build author list
auth_name = ['']
n_auth = 1
; Go through programs
for i_pro = 0, n_pro-1 do begin
  ; Remove middle initial
  temp = strsplit( pro_auth[i_pro], ' ', extract=1, count=n_temp )
  if n_temp gt 2 then begin
    for i_temp = 1, n_temp - 2 do begin
      if strlen( temp[i_temp] ) eq 1 then temp[i_temp] = ''
      if ( strlen( temp[i_temp] ) eq 2 ) $
          and ( strmid( temp[i_temp], 1, 1 ) eq '.' ) then temp[i_temp] = ''
    endfor
    id = where( temp ne '' )
    temp = temp[id]
  endif
  temp = string_from_vector( temp, spacer=' ', nospace=1 )
  ; Set counter
  check = 0
  ; Compare program author to author list
  for i_auth = 0, n_auth-1 do begin
    if temp eq auth_name[i_auth] then check = 1
  endfor
  ; Add new author
  if check eq 0 then begin
    auth_name = [ auth_name, temp ]
    n_auth = n_auth + 1
  endif
endfor
; Go through modifying authors if requested
if keyword_set( modified_author_opt ) then begin
  for i_mod = 0, n_elements( mod_auth ) - 1 do begin
    ; Remove middle initial
    temp = strsplit( mod_auth[i_mod], ' ', extract=1, count=n_temp )
    if n_temp gt 2 then begin
      for i_temp = 1, n_temp - 2 do begin
        if strlen( temp[i_temp] ) eq 1 then temp[i_temp] = ''
        if ( strlen( temp[i_temp] ) eq 2 ) $
            and ( strmid( temp[i_temp], 1, 1 ) eq '.' ) then temp[i_temp] = ''
      endfor
      id = where( temp ne '' )
      temp = temp[id]
    endif
    temp = string_from_vector( temp, spacer=' ', nospace=1 )
    ; Set counter
    check = 0
    ; Compare program author to author list
    for i_auth = 0, n_auth-1 do begin
      if temp eq auth_name[i_auth] then check = 1
    endfor
    ; Add new author
    if check eq 0 then begin
      auth_name = [ auth_name, temp ]
      n_auth = n_auth + 1
    endif
  endfor
endif
; Remove the initial entry (used for initialisation only)
auth_name = auth_name[1:n_auth-1]
n_auth = n_auth - 1
; Sort alphabetically by surname
temp_surname = strarr( n_auth )
for i_auth = 0, n_auth - 1 do begin
  temp = strsplit( auth_name[i_auth], ' ', extract=1 )
  temp_surname[i_auth] = temp[1]
endfor
id = sort( temp_surname )
auth_name = auth_name[id]

;***********************************************************************
; Creat Archive Files

; Create .tar archive if requested
if keyword_set( tar_opt ) then begin
  ; Create archive file
  spawn, 'tar -cf ' + indir[0] + 'idl_lib.tar ' $
      + string_from_vector( list, spacer=' ' )
  ; Allow access to everyone
  spawn, 'chmod a+r ' + indir[0] + 'idl_lib.tar'
endif

; Create .zip archive if requested
if keyword_set( zip_opt ) then begin
  ; Create archive file
  spawn, 'zip ' + indir[0] + 'idl_lib.zip ' $
      + string_from_vector( list, spacer=' ' )
  ; Allow access to everyone
  spawn, 'chmod a+r ' + indir[0] + 'idl_lib.zip'
endif

;***********************************************************************
; Create Web Page

; Open web page file
openw, 1, outfile

; Web page header
printf, 1, '<!-- *********************************************** /-->'
printf, 1, "<!-- ********** A list of IDL routines ********** /-->"
printf, 1, '<!-- * ' + admin_name + ', ' + admin_email + ' * /-->'
printf, 1, '<!-- * This file was produced by idl_lib_html.pro. * /-->'
printf, 1, '<!-- *********************************************** /-->'
printf, 1
printf, 1, '<HTML>'
printf, 1
printf, 1, '<HEAD>'
printf, 1, '<TITLE>' + title_page + '</TITLE>'
printf, 1, '<STYLE TYPE="text/css">'
printf, 1, '  A { text-decoration: none }'
printf, 1, '</STYLE>'
printf, 1, '</HEAD>'
printf, 1
printf, 1, '<BODY BGCOLOR=' + color_background + ' LINK=' + color_link $
    + ' VLINK=' + color_viewedlink + ' TEXT=' + color_text + ' SIZE=' $
    + size_text + ' FACE=' + font_text + '>'
printf, 1
printf, 1, '<CENTER>'
printf, 1, '  ' + size_title_start + '<FONT FACE=' + font_header + ' COLOR=' $
    + color_header + '>'
;printf, 1, '    <BR>' + title_page + '<BR><BR>'
printf, 1, '    ' + title_page + '<BR>'
printf, 1, '  </FONT>' + size_title_end
printf, 1, '</CENTER>'
printf, 1

; Write information to page
printf, 1, bold_text_start
temp = '<FONT COLOR=' + color_header + '>Maintained by:</FONT>  ' + admin_name $
    + ' (' + '<A HREF="mailto:' + admin_email + '">' + admin_email + '</A>)<BR>'
printf, 1, temp
temp = '<FONT COLOR=' + color_header + '>Last modified:</FONT>  ' $
    + strmid( str( date ), 0, 4 ) + '-' + strmid( str( date ), 4, 2 ) $
    + '-' + strmid( str( date ), 6, 2 ) + ', ' + hour + ' GMT by <A HREF="' $
    + admin_web + '/idl_lib/pro/idl_lib_html.pro">idl_lib_html.pro</A>.<BR>'
printf, 1, temp
temp = '<FONT COLOR=' + color_header + '>Number of routines:</FONT>  ' $
    + str( n_pro ) + '<BR>'
printf, 1, temp
temp = '<FONT COLOR=' + color_header + '>Contributors:</FONT>  ' $
    + string_from_vector( auth_name ) + '<BR><BR>'
printf, 1, temp
temp = '<FONT COLOR=' + color_header $
    + '>Licence:</FONT> The IDL routines available from this page are free ' $
    + 'for non-commercial use under the terms of this ' $
    + '<A HREF="http://creativecommons.org/licenses/by-nc-sa/2.0/">Creative ' $
    + 'Commons License</A> unless otherwise noted in the routine.<BR><BR>' 
printf, 1, temp
temp = 'Please report any bugs to <A HREF="mailto:' + admin_email + '">' $
    + admin_email + '</A>.<BR>'
printf, 1, temp
printf, 1, 'We welcome any modified or new routines you would like to add.<BR>'
temp = 'We would like to know if you have found any of these routines ' $
    + 'useful.<BR>'
printf, 1, temp
if keyword_set( tar_opt ) or keyword_set( zip_opt ) then begin
  printf, 1, '<BR>'
  temp = 'The entire routine library can be downloaded as an archive file.<BR>'
  printf, 1, temp
  if keyword_set( tar_opt ) then begin
    temp = '  Click <A HREF="' + indir[0] $
        + 'idl_lib.tar">here</A> for a .tar archive.<BR>'
    printf, 1, temp
  endif
  if keyword_set( zip_opt ) then begin
    temp = '  Click <A HREF="' + indir[0] $
        + 'idl_lib.zip">here</A> for a .zip archive.<BR>'
    printf, 1, temp
  endif
endif
;if keyword_set( update ) then begin
;  printf, 1, 'See bottom for updates.<BR>'
;endif
printf, 1, '<BR>'
printf, 1, bold_text_end
printf, 1

; List update information now if requested
if keyword_set( first_update ) then begin
  ; Section title
  printf, 1, size_header_start + '<FONT COLOR=' + color_header + '>' $
      + title_first_update + '</FONT>' + size_header_end
  printf, 1, '<UL>' + bold_text_start
  for i_update = 0, n_elements( first_update ) - 1 do begin
    printf, 1, '  <LI>' + first_update[i_update]
  endfor
  printf, 1, bold_text_end + '</UL>'
  printf, 1, '<BR>'
endif

; Write categories to page
for i_categ = 0, n_categ-1 do begin
  ; Category title
  printf, 1, size_header_start + '<FONT COLOR=' + color_header + '>'
  printf, 1, '  ' + categ_name[id_categ_sort[i_categ]]
  printf, 1, '</FONT>' + size_header_end
  ; Program list
  printf, 1, bold_text_start + '<UL>'
  temp = max( pro_categ_index eq id_categ_sort[i_categ], dimension=2 )
  id = where( temp eq 1, n_id )
  id_sort = sort( strlowcase( pro_name[id] ) )
  id = id[id_sort]
  for i_id = 0, n_id-1 do begin
    temp = '  <LI><A HREF="' + strlowcase( list[id[i_id]] ) + '">' $
        + pro_name[id[i_id]] + '</A> ' + pro_purp[id[i_id]]
    printf, 1, temp
    temp = julday( fix( strmid( str( date ), 4, 2 ) ), $
        fix( strmid( str( date ), 6, 2 ) ), $
        fix( strmid( str( date ), 0, 4 ) ) ) $
        - julday( fix( strmid( str( pro_date[id[i_id]] ), 4, 2 ) ), $
        fix( strmid( str( pro_date[id[i_id]] ), 6, 2 ) ), $
        fix( strmid( str( pro_date[id[i_id]] ), 0, 4 ) ) )
    if temp lt hist_delay then begin
      printf, 1, '<FONT COLOR=' + color_header + '>' + pro_hist[id[i_id]] $
          + '</FONT>'
    endif else begin
      printf, 1, pro_hist[id[i_id]]
    endelse
  endfor
  printf, 1, '</UL>'
  printf, 1, bold_text_end
  printf, 1
endfor

; List update information unless already done earlier in the webpage
if keyword_set( update ) then begin
  ; Section title
  printf, 1, '<BR>'
  printf, 1, size_header_start + '<FONT COLOR="' + color_header + '">' $
      + title_update + '</FONT>' + size_header_end
  printf, 1, '<UL>' + bold_text_start
  for i_update = 0, n_elements( update ) - 1 do begin
    printf, 1, '  <LI>' + update[i_update]
  endfor
  printf, 1, bold_text_end + '</UL>'
endif

; Web page ending
printf, 1, '<BR>'
printf, 1
printf, 1, '</BODY>'
printf, 1, '</HTML>'

; Close web page file
close, 1

;***********************************************************************
; The End

return
END
