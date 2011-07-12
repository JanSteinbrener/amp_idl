;+
; NAME:
;    COMMIE_SCRIPTFILE_IO
;
;-


PRO commie_scriptfile_io, script_filename, commie_script_struct, $
                          commie_dataset_struct, error_string, $
                          read=read, write=write, bad_data=bad_data, $
                          bs_pos=bs_pos, $
                          add_motor_positions=add_motor_positions
 

  compile_opt idl2, hidden

  IF Keyword_Set(read) THEN BEGIN ;we want to read from the scriptfile 
     Catch, error_code
     IF (error_code NE 0) THEN BEGIN
        error_string = 'commie_scriptfile_io: Error opening file.'
        Catch, /Cancel
        RETURN
     ENDIF
     OpenR, lun, script_filename, /Get_Lun
     Catch, /Cancel
     
     Catch, error_code
     IF (error_code NE 0) THEN BEGIN
        error_string = 'commie_scriptfile_io: Error reading file.'
        Close, lun
        Free_lun, lun
        Catch, /Cancel
        RETURN
     ENDIF
     
     ;; read the whole file in a byte array
     temp_byte = byte(0)
     temp_bytarr = byte(0)
     WHILE ~ EOF(lun) DO BEGIN
        ReadU, lun, temp_byte
        temp_bytarr = [temp_bytarr,temp_byte]
     ENDWHILE
     Catch, /Cancel
     Close, lun
     Free_lun, lun

     ;; Define the linefeed byte. This is used to split the string into separate
     ;; lines. The linefeed tag is present in both Windows and Unix systems.
     linefeed = String(Byte(10))
     temp_bytarr = temp_bytarr[1:*]
     ;; first separate the positions from the rest of the header
     str_arr = $
        Strsplit(String(temp_bytarr),';',/Extract,count=need_pos)
     ;; check if the scriptfile contains the motor positions or not.
     IF need_pos EQ 2 THEN BEGIN
        ;; in this case we have the motor information in the
        ;; scriptfile
        str_index = 1
        ;; process position-definitions first, every line is one element
        ;; in pos_array
        pos_array = $
           Strsplit(str_arr[0],linefeed,count=pos_count,/Extract)
        ;; prepare arrays to hold the names of the positions and motors 
        positions = Strarr(pos_count)
        motor_names = ''
        ;; next thing is to split at commas
        FOR i=0,pos_count-1 DO BEGIN
           temp_str=Strsplit(pos_array[i],',',count=mot_count,/Extract)
           ;; Note: mot_count also includes position name so number of motors
           ;; specified is actually mot_count -1, so do:
           mot_count = mot_count-1
           positions[i]=Strcompress(temp_str[0],/Remove_All)
           ;; Initialize the motor_values array. We need as many
           ;; columns as  motors and as many rows as positions
           IF (i EQ 0) THEN $
              motor_values=Dblarr(mot_count,pos_count)
           ;; now we need to split at the '=' signs to separate motor
           ;; names from motor values - extract motor_names from first
           ;; line,  then check and rearrange the motors if the user
           ;; changed the order of motors in subsequent lines.
           FOR j=0,mot_count-1 DO BEGIN
              temp_str1=Strsplit(temp_str[j+1],'=',/Extract)
              IF (i EQ 0) THEN BEGIN
                 motor_names=[motor_names,Strcompress(temp_str1[0],/Remove_All)]
                 motor_values[j,i] = Double(temp_str1[1])
              ENDIF ELSE BEGIN
                 FOR k=0,mot_count-1 DO BEGIN
                    IF (Strcmp(Strcompress(temp_str1[0],/Remove_All), $
                               motor_names[k+1]) EQ 1) THEN $
                                  motor_values[k,i] = Double(temp_str1[1])
                 ENDFOR
              ENDELSE
           ENDFOR
        ENDFOR
        commie_dataset_struct.position_names = ptr_new(positions)
        commie_dataset_struct.motor_names = ptr_new(motor_names[1:*])
        commie_dataset_struct.motor_values = ptr_new(motor_values)
     ENDIF ELSE BEGIN
        ;; in this case we do not have any motor positions in the scriptfile
        str_index = 0
     ENDELSE
     ;; every line is one array element in str_arr
     str_arr = $
        Strsplit(str_arr[str_index],linefeed,count=str_count,/Extract)
     ;; get appropriate path separator
     path_sep = path_sep()
     
     tag_count = 0
     ;; now start loop to find matching statements, remove any whitespace
     FOR i=0, str_count-1 DO BEGIN
        IF (Strcmp(str_arr[i],'topdir',6,/FOLD_CASE) EQ 1) THEN BEGIN
           temp_str = Strtrim(Strsplit(str_arr[i],',',/Extract),2)
           ;; Take out any trailing path separator to be consistent
           IF (Strpos(temp_str[1],path_sep,/Reverse_Search) EQ $
               (Strlen(temp_str[1])-1)) THEN BEGIN
              temp_str = Byte(temp_str[1])
              temp_str = String(temp_str[0:N_elements(temp_str)-2])
           ENDIF ELSE BEGIN
              temp_str = temp_str[1]
           ENDELSE
           commie_script_struct.topdir = $
              ptr_new(strcompress(temp_str,/REMOVE_ALL))
        ENDIF ELSE IF (Strcmp(str_arr[i],'bs_saveread_file',16, $
                              /FOLD_CASE) EQ 1) THEN BEGIN
          ;; first look for ',' character. Need to store in different columns
           temp_str = Strtrim(Strsplit(str_arr[i],',',/Extract),2)
           ;; now the first one will be the tagname, then there will
           ;; be subfolder and filename
           ;; take out any leading path separators to be consistent
           IF (Strpos(temp_str[1],path_sep) EQ 0) THEN BEGIN
              temp_str = Byte(temp_str[1])
              temp_str = String(temp_str[1:*])
           ENDIF ELSE BEGIN
              temp_str = temp_str[1]
           ENDELSE
           ;; the topdir will be added in _get_filenames.pro
           commie_script_struct.bs_filename = $
              ptr_new(strcompress(temp_str,/REMOVE_ALL))
        ENDIF ELSE IF (Strcmp(str_arr[i],'dk_saveread_file',16, $
                              /FOLD_CASE) EQ 1) THEN BEGIN
           ;; first look for ',' character. Need to store in different columns
           temp_str = Strtrim(Strsplit(str_arr[i],',',/Extract),2)
           ;; now the first one will be the tagname, then there will
           ;; be subfolder and filename
           ;; take out any leading path separators to be consistent
           IF (Strpos(temp_str[1],path_sep) EQ 0) THEN BEGIN
              temp_str = Byte(temp_str[1])
              temp_str = String(temp_str[1:*])
           ENDIF ELSE BEGIN
              temp_str = temp_str[1]
           ENDELSE
           ;; the topdir will be added in _get_filenames.pro
           commie_script_struct.dk_saveread_file = $
              ptr_new(strcompress(temp_str,/REMOVE_ALL))
        ENDIF ELSE IF (Strcmp(str_arr[i],'mp_saveread_dir',15, $
                              /FOLD_CASE) EQ 1) THEN BEGIN
           ;; first look for ',' character. Need to store in different columns
           temp_str = Strtrim(Strsplit(str_arr[i],',',/Extract),2)
           ;; now the first one will be the tagname, then there will
           ;; be subfolder and filename
           ;; take out any leading path separators to be consistent
           IF (Strpos(temp_str[1],path_sep) EQ 0) THEN BEGIN
              temp_str = Byte(temp_str[1])
              temp_str = String(temp_str[1:*])
           ENDIF ELSE BEGIN
              temp_str = temp_str[1]
           ENDELSE
           ;; the topdir will be added below
           commie_script_struct.mp_saveread_dir = $
              ptr_new(strcompress(temp_str,/REMOVE_ALL))
        ENDIF ELSE IF (Strcmp(str_arr[i],'sample',6,/FOLD_CASE) EQ 1) THEN BEGIN
           ;; first look for ',' character. Need to store in different columns
           temp_str = Strtrim(Strsplit(str_arr[i],',',count=count,/Extract),2)
           ;; now the first one will be the tagname, then there will
           ;; be subfolder
           ;; and header info, followed by ':' and then the range of files
           temp_header = ''
           temp_dir = ''
           temp_array = Intarr(count-1,2)
           FOR j=0,count-2 DO BEGIN
              temp_str1 = Strsplit(temp_str[j+1],':',/Extract)
              ;; now find position of last path separator, this will give the 
              ;; subfolders and the header
              temp_header = $
                 [temp_header,Strmid(temp_str1[0],$
                                     (Strpos(temp_str1[0],path_sep,$
                                             /Reverse_search,$
                                             /Reverse_Offset)+1))]
              ;; take out any leading path separators to be consistent
              IF (Strpos(temp_str1[0],path_sep) EQ 0) THEN BEGIN
                 temp_str3 = Byte(temp_str1[0])
                 temp_str3 = String(temp_str3[1:*])
              ENDIF ELSE BEGIN
                 temp_str3 = temp_str1[0]
              ENDELSE
              temp_dir = $
                 [temp_dir,Strjoin([*(commie_script_struct.topdir),path_sep, $
                                    Strtrim(Strmid(temp_str3,0,$
                                                   Strpos(temp_str3,path_sep,$
                                                          /Reverse_search,$
                                                         /Reverse_Offset)),2)])]
              temp_str2 =  Strsplit(temp_str1[1],'[[:punct:]]',/Extract,/Regex)
              temp_array[j,*]=Fix(temp_str2[0:*])
           ENDFOR
           commie_script_struct.sample_dirs = ptr_new(temp_dir[1:*])
           commie_script_struct.sample_headers = ptr_new(temp_header[1:*])
           commie_script_struct.sample_indices = ptr_new(temp_array)         
        ENDIF ELSE IF (Strcmp(str_arr[i],'no_sample', $
                              9,/FOLD_CASE) EQ 1) THEN BEGIN
           ;; first look for ',' character. Need to store in different columns
           temp_str = Strtrim(Strsplit(str_arr[i],',',count=count,/Extract),2)
           ;; now the first one will be the tagname, then there will
           ;; be subfolder
           ;; and header info, followed by ':' and then the range of files
           temp_array = Intarr(count-1,2)
           temp_header = ''
           temp_dir = ''
           FOR j=0,count-2 DO BEGIN
              temp_str1 = Strsplit(temp_str[j+1],':',/Extract)
              ;; now find position of last path separator, this will give the 
              ;; subfolders and the header
              temp_header = $
                 [temp_header,Strmid(temp_str1[0],$
                                     (Strpos(temp_str1[0],path_sep,$
                                             /Reverse_search,$
                                             /Reverse_Offset)+1))]
              ;; take out any leading path separators to be consistent
              IF (Strpos(temp_str1[0],path_sep) EQ 0) THEN BEGIN
                 temp_str3 = Byte(temp_str1[0])
                 temp_str3 = String(temp_str3[1:*])
              ENDIF ELSE BEGIN
                 temp_str3 = temp_str1[0]
              ENDELSE
              temp_dir = $
                 [temp_dir,Strjoin([*(commie_script_struct.topdir),path_sep, $
                                    Strtrim(Strmid(temp_str3,0,$
                                                   Strpos(temp_str3,path_sep,$
                                                          /Reverse_search,$
                                                         /Reverse_Offset)),2)])]
              temp_str2 =  Strsplit(temp_str1[1],'[[:punct:]]',/Extract,/Regex)
              temp_array[j,*]=Fix(temp_str2[0:*])
           ENDFOR
           commie_script_struct.no_sample_dirs = ptr_new(temp_dir[1:*])
           commie_script_struct.no_sample_headers = ptr_new(temp_header[1:*])
           commie_script_struct.no_sample_indices = ptr_new(temp_array) 
        ENDIF ELSE IF (Strcmp(str_arr[i],'bad_data', $
                              8,/FOLD_CASE) EQ 1) THEN BEGIN
           ;; first look for ',' character. Need to store in different columns
           temp_str = Strtrim(Strsplit(str_arr[i],',',count=count,/Extract),2)
           ;; now check if user indicated that there aren't any bad data files
           ;; This is done by having the bad_data keyword in the scriptfile w/o 
           ;; any header and file range specifications. Note, if user leaves 
           ;; whitespace behind the comma we need to check against strlen too.
           IF (count EQ 1) THEN BEGIN
              commie_script_struct.bad_data_indices = ptr_new(-1)
           ENDIF ELSE IF ((count EQ 2) $
                          AND (strlen(temp_str[1]) EQ 0)) THEN BEGIN
              commie_script_struct.bad_data_indices = ptr_new(-1)
           ENDIF ELSE BEGIN
              ;; now the first one will be the tagname, then there will be 
              ;; subfolder and header info, followed by ':' and then the range
              ;; of files 
              temp_array = Intarr(count-1,2)
              temp_header = ''
              temp_dir = ''
              FOR j=0,count-2 DO BEGIN
                 temp_str1 = Strsplit(temp_str[j+1],':',/Extract)
                 ;; now find position of last path separator, this
                 ;; will give the 
                 ;; subfolders and the header
                 temp_header = $
                    [temp_header,Strmid(temp_str1[0],$
                                        (Strpos(temp_str1[0],path_sep,$
                                                /Reverse_search,$
                                                /Reverse_Offset)+1))]
                 ;; take out any leading path separators to be consistent
                 IF (Strpos(temp_str1[0],path_sep) EQ 0) THEN BEGIN
                    temp_str3 = Byte(temp_str1[0])
                    temp_str3 = String(temp_str3[1:*])
                 ENDIF ELSE BEGIN
                    temp_str3 = temp_str1[0]
                 ENDELSE
                 temp_dir = $
                    [temp_dir,Strjoin([*(commie_script_struct.topdir), $
                                       path_sep, $
                                       Strtrim(Strmid(temp_str3,0,$
                                                      Strpos(temp_str3, $
                                                             path_sep,$
                                                             /Reverse_search,$
                                                         /Reverse_Offset)),2)])]
                 temp_str2 = $
                    Strsplit(temp_str1[1],'[[:punct:]]',/Extract,/Regex)
                 temp_array[j,*]=Fix(temp_str2[0:*])
              ENDFOR
              commie_script_struct.bad_data_dirs = ptr_new(temp_dir[1:*])
              commie_script_struct.bad_data_headers = ptr_new(temp_header[1:*])
              commie_script_struct.bad_data_indices = ptr_new(temp_array)   
           ENDELSE
        ENDIF ELSE IF (Strcmp(str_arr[i],'dark_current', $
                              12,/FOLD_CASE) EQ 1) THEN BEGIN
           ;; first look for ',' character. Need to store in different columns
           temp_str = Strtrim(Strsplit(str_arr[i],',',count=count,/Extract),2)
           ;; now the first one will be the tagname, then there will
           ;; be subfolder
           ;; and header info, followed by ':' and then the range of files
           temp_array = Intarr(count-1,2)
           temp_header = ''
           temp_dir = ''
           FOR j=0,count-2 DO BEGIN
              temp_str1 = Strsplit(temp_str[j+1],':',/Extract)
              ;; now find position of last path separator, this will give the 
              ;; subfolders and the header
              temp_header = $
                 [temp_header,Strmid(temp_str1[0],$
                                     (Strpos(temp_str1[0],path_sep,$
                                             /Reverse_search,$
                                             /Reverse_Offset)+1))]
              ;; take out any leading path separators to be consistent
              IF (Strpos(temp_str1[0],path_sep) EQ 0) THEN BEGIN
                 temp_str3 = Byte(temp_str1[0])
                 temp_str3 = String(temp_str3[1:*])
              ENDIF ELSE BEGIN
                 temp_str3 = temp_str1[0]
              ENDELSE
              temp_dir = $
                 [temp_dir,Strjoin([*(commie_script_struct.topdir),path_sep, $
                                    Strtrim(Strmid(temp_str3,0,$
                                                   Strpos(temp_str3,path_sep,$
                                                          /Reverse_search,$
                                                         /Reverse_Offset)),2)])]
              temp_str2 =  Strsplit(temp_str1[1],'[[:punct:]]',/Extract,/Regex)
              temp_array[j,*]=Fix(temp_str2[0:*])
           ENDFOR
           commie_script_struct.dark_current_dirs = ptr_new(temp_dir[1:*])
           commie_script_struct.dark_current_headers = ptr_new(temp_header[1:*])
           commie_script_struct.dark_current_indices = ptr_new(temp_array)   
        ENDIF ELSE IF (Strcmp(str_arr[i],'filetype', $
                              8,/FOLD_CASE) EQ 1) THEN BEGIN
           temp_str = Strsplit(str_arr[i],',',/Extract)
           commie_script_struct.filetype = ptr_new(Strtrim(temp_str[1],2))
        ENDIF ELSE IF (Strcmp(str_arr[i],'tolerance_meters', $
                              16,/FOLD_CASE) EQ 1) THEN BEGIN
           temp_str = Strsplit(str_arr[i],',',/Extract)
           commie_script_struct.tolerance_meters = $
              ptr_new(Double(strcompress(temp_str[1],/REMOVE_ALL)))
        ENDIF ELSE IF (Strcmp(str_arr[i],'rotate', $
                              6,/FOLD_CASE) EQ 1) THEN BEGIN
           temp_str = Strsplit(str_arr[i],',',/Extract)
           commie_dataset_struct.rotate = $
              ptr_new(Fix(strcompress(temp_str[1],/REMOVE_ALL)))
        ENDIF ELSE IF (Strcmp(str_arr[i],'bs_xs', $
                              5,/FOLD_CASE) EQ 1) THEN BEGIN
           temp_str = Strsplit(str_arr[i],',',/Extract)
           dummy = 0
           FOR j=1, (n_elements(temp_str)-1) DO BEGIN
              dummy = [dummy,Fix(Strcompress(temp_str[j],/REMOVE_ALL))]
           ENDFOR
           commie_dataset_struct.bs_xs = ptr_new(dummy[1:*])
        ENDIF ELSE IF (Strcmp(str_arr[i],'bs_ys', $
                              5,/FOLD_CASE) EQ 1) THEN BEGIN
           temp_str = Strsplit(str_arr[i],',',/Extract)
           dummy = 0
           FOR j=1, (n_elements(temp_str)-1) DO BEGIN
              dummy = [dummy,Fix(Strcompress(temp_str[j],/REMOVE_ALL))]
           ENDFOR
           commie_dataset_struct.bs_ys = ptr_new(dummy[1:*])
        ENDIF ELSE IF (Strcmp(str_arr[i],'energy_ev', $
                              9,/FOLD_CASE) EQ 1) THEN BEGIN
           temp_str = Strsplit(str_arr[i],',',/Extract)
           ev = strcompress(temp_str[1],/REMOVE_ALL)
           commie_dataset_struct.lambda_meters = $
              ptr_new(Double(1239.852)/(Double(1.e9)*Double(ev)))
        ENDIF ELSE IF (Strcmp(str_arr[i],'ring_amps', $
                              9,/FOLD_CASE) EQ 1) THEN BEGIN
           temp_str = Strsplit(str_arr[i],',',/Extract)
           temp_str = strcompress(temp_str[1],/REMOVE_ALL)
           commie_script_struct.ring_amps = ptr_new(DOUBLE(temp_str))
        ENDIF ELSE IF (Strcmp(str_arr[i],'sat_threshold_high', $
                              18,/FOLD_CASE) EQ 1) THEN BEGIN
           temp_str = Strsplit(str_arr[i],',',/Extract)
           commie_script_struct.sat_threshold_high = $
              ptr_new(Long(strcompress(temp_str[1],/REMOVE_ALL)))
        ENDIF ELSE IF (Strcmp(str_arr[i],'sat_threshold_low', $
                              17,/FOLD_CASE) EQ 1) THEN BEGIN
           temp_str = Strsplit(str_arr[i],',',/Extract)
           commie_script_struct.sat_threshold_low = $
              ptr_new(Long(strcompress(temp_str[1],/REMOVE_ALL)))
        ENDIF ELSE IF (Strcmp(str_arr[i],'median_percent', $
                              14,/FOLD_CASE) EQ 1) THEN BEGIN
           temp_str = Strsplit(str_arr[i],',',/Extract)
           commie_script_struct.median_percent = $
              ptr_new(Float(strcompress(temp_str[1],/REMOVE_ALL)))
        ENDIF ELSE IF (Strcmp(str_arr[i],'threshold_factor', $
                              16,/FOLD_CASE) EQ 1) THEN BEGIN
           temp_str = Strsplit(str_arr[i],',',/Extract)
           commie_script_struct.thresh_data_factor = $
              ptr_new(Float(strcompress(temp_str[1],/REMOVE_ALL)))
        ENDIF ELSE IF (Strcmp(str_arr[i],'subset', $
                              6,/FOLD_CASE) EQ 1) THEN BEGIN
           temp_str = Strsplit(str_arr[i],',',/Extract)
            ;; make sure user supplied enough information
           if N_ELEMENTS(temp_str) ne 3 then begin
              status_string = '[WARNING]: COMMIE_SCRIPTFILE_IO: '+ $
                              'Wrong input format of subset_indices. '+ $
                              'Proceeding with full array.'
              COMMIE_MESSAGES, status=status_string
              dummy = [0,-1,-1,-1,-1]
           endif else begin
              dummy = 0
              FOR j=1, (n_elements(temp_str)-1) DO BEGIN
                 ;; parse for the colon
                 temp_str2 = STRCOMPRESS(STRSPLIT(temp_str[j],':',/extract), $
                                         /remove_all)
                 if N_ELEMENTS(temp_str2) gt 2 then begin 
                    status_string = '[WARNING]: COMMIE_SCRIPTFILE_IO: '+ $
                                    'Wrong input format for dimension '+ $
                                    STRTRIM(j-1,2)+'. Using all of it.'
                    COMMIE_MESSAGES, status=status_string
                    dummy = [dummy,-1,-1]
                 endif else begin
                    ;; parse for the *
                    star_ind = WHERE(STRCMP(temp_str2,'*') eq 1, star_count, $
                                     complement=no_star_ind)
                    if (N_ELEMENTS(temp_str2) eq 1) then begin
                       if star_count ne 1 then begin
                          status_string = '[WARNING]: COMMIE_SCRIPTFILE_IO: '+ $
                                          'Wrong input format for dimension '+ $
                                          STRTRIM(j-1,2)+'. Using all of it.'
                          COMMIE_MESSAGES, status=status_string
                       endif
                       dummy = [dummy,-1,-1]
                    endif else begin
                       if star_count eq 0 then begin
                          dummy = [dummy,FIX(temp_str2)]
                       endif else if star_count eq 2 then begin
                          dummy = [dummy,-1,-1]
                       endif else begin
                          superdummy = INTARR(2)
                          superdummy[no_star_ind] = FIX(temp_str2[no_star_ind])
                          superdummy[star_ind] = -1
                          dummy = [dummy,superdummy]
                       endelse
                    endelse
                 endelse
              endfor
           endelse           
           commie_script_struct.subset_indices = PTR_NEW(dummy[1:*])
        ENDIF ELSE IF (Strcmp(str_arr[i],'output', $
                              6,/FOLD_CASE) EQ 1) THEN BEGIN
           temp_str = Strtrim(Strsplit(str_arr[i],',',/Extract),2)
           ;; take out any leading path separators to be consistent
           IF (Strpos(temp_str[1],path_sep) EQ 0) THEN BEGIN
              temp_str1 = Byte(temp_str[1])
              temp_str1 = String(temp_str1[1:*])
           ENDIF ELSE BEGIN
              temp_str1 = temp_str[1]
           ENDELSE
           commie_script_struct.output = $
              ptr_new(Strjoin([*(commie_script_struct.topdir),path_sep,$
                               Strtrim(temp_str1,2)]))
        ENDIF ELSE tag_count++
     ENDFOR
     
     IF (tag_count EQ str_count) THEN BEGIN
        error_string = $
           'commie_scriptfile_io: File has no matching arguments.'
        RETURN
     ENDIF

     ;; Here are some defaults in case the user didn't specify anything here
     ;; check if user supplied topdir, if not then define default
     IF NOT ptr_valid(commie_script_struct.topdir) THEN BEGIN
        cd,current=current_dir
        commie_script_struct.topdir = ptr_new(current_dir)
     ENDIF
     if not PTR_VALID(commie_script_struct.subset_indices) then $
        commie_script_struct.subset_indices = PTR_NEW([-1,-1,-1,-1])

     ;; add topdir to mp_saveread_dir or if nothing else specified then
     ;; use only topdir
     IF NOT ptr_valid(commie_script_struct.mp_saveread_dir) THEN BEGIN
        commie_script_struct.mp_saveread_dir = $
           ptr_new(*commie_script_struct.topdir)
     ENDIF ELSE BEGIN
        *commie_script_struct.mp_saveread_dir = $
           Strjoin([(*commie_script_struct.topdir),path_sep, $
                    (*commie_script_struct.mp_saveread_dir)])
     ENDELSE
     IF NOT ptr_valid(commie_script_struct.tolerance_meters) THEN $
        commie_script_struct.tolerance_meters = ptr_new(Double(0.000002))
     ;; This defaults to a 270 degree ccw rotation as is the case with
     ;; the Stony Brook CCD
     IF NOT ptr_valid(commie_dataset_struct.rotate) THEN $
        commie_dataset_struct.rotate = ptr_new(3)
     ;; the sat_threshold_high depends on the array size (i.e. new
     ;; camera or old camera), so let's define it in initialize
     ;; dataset instead!!
     IF NOT ptr_valid(commie_script_struct.sat_threshold_low) THEN $
        commie_script_struct.sat_threshold_low = ptr_new(long((2L^16-1)*.1))
     IF NOT ptr_valid(commie_script_struct.filetype) THEN $
        commie_script_struct.filetype = ptr_new('dt_nc')
     IF NOT ptr_valid(commie_script_struct.median_percent) THEN $
        commie_script_struct.median_percent = ptr_new(Float(0.25))
     ;; The thresh_data_factor will be determined from the ring
     ;; currents on _initialize_dataset if nothing has been specified
     ;; in the scriptfile
     
     ;; now create the expanded path for the verbose output if
     ;; necessary. 
     IF (*commie_script_struct.verbose) THEN BEGIN
        IF (size(*commie_script_struct.verbose,/type) EQ 7) THEN BEGIN
           path_sep = path_sep()
           *commie_script_struct.verbose = $
              Strjoin([*(commie_script_struct.topdir),path_sep,$
                       *commie_script_struct.verbose])
        ENDIF
     ENDIF

     ;; print out some more info if the user desires so
     IF *commie_script_struct.verbose THEN BEGIN
        commie_verbose_output, commie_dataset_struct, commie_script_struct, $
                               /scriptfile_io
     ENDIF ;; verbose
  ENDIF ;;keyword_set(read)
  
  IF Keyword_Set(bad_data) THEN BEGIN ;; now lets write stuff to the scriptfile
     ;;        i) add bad_data filenames from commie
     
     Catch, error_code
     IF (error_code NE 0) THEN BEGIN
        error_string = 'commie_scriptfile_io: Error opening file.'
        Catch, /Cancel
        RETURN
     ENDIF
     OpenU, lun, script_filename, /Get_Lun,/Append
     Catch, /Cancel
     
     Catch, error_code
     IF (error_code NE 0) THEN BEGIN
        error_string = 'commie_scriptfile_io: Error updating file.'
        Close, lun
        Free_lun, lun
        Catch, /Cancel
        RETURN
     ENDIF
     
     ;; if all files happen to be good, then we can just add 'bad_data' to the 
     ;; scriptfile
     path_sep = path_sep()
     dummy = Where(*(commie_dataset_struct.are_bad) EQ 0,count)
     IF count EQ n_elements(*(commie_dataset_struct.are_bad)) THEN BEGIN
        printf,lun,''
        WriteU,lun,'bad_data'
        Catch, /Cancel
        Close, lun
        free_lun, lun
     ENDIF ELSE BEGIN
        ;; now we need to fiddle it all together - get the indices first
        bad_indices = Where(*(commie_dataset_struct.are_bad) EQ 1, count)
        bad_data_str = ''
        FOR i=0,count-1 DO BEGIN
           ;; now extract the indices and headers, subdirs
           this_dir = $
              File_Dirname((*(commie_dataset_struct.filenames))[bad_indices[i]])
           this_dir = Strjoin([this_dir,path_sep])
           this_base = $
             File_Basename((*(commie_dataset_struct.filenames))[bad_indices[i]])
           ;; now get the subdirs by cutting out the topdir. Make sure that we
           ;; we actually have a subdir (-1 b/c topdir is stored w/o path sep.)
           IF (Strlen(*(commie_script_struct.topdir)) $
               LT (Strlen(this_dir) -1)) THEN BEGIN
              this_subdir = $
                 Strmid(this_dir,Strlen(*(commie_script_struct.topdir)))
              bad_data_str = Strjoin([bad_data_str,this_subdir])
           ENDIF
           ;; get the header and the index number. Here we assume that the index
           ;; is a 4 digit integer with leading zeroes if necessary
           this_header = Strmid(this_base,0,$
                                (Strpos(this_base,'_', /Reverse_search)))
           this_index = Strtrim(Fix(Strmid(this_base,$
                                           (Strpos(this_base,$
                                                   '_',$
                                                   /Reverse_search)+1),4)),2)
           IF i LT count-1 THEN BEGIN
            bad_data_str = $
               Strjoin([bad_data_str,this_header,': ',this_index,', '])
         ENDIF ELSE BEGIN ;; no trailing comma for the last file
            bad_data_str = $
               Strjoin([bad_data_str,this_header,': ',this_index])
         ENDELSE
      ENDFOR
        bad_data_str = Strjoin(['bad_data, ',bad_data_str])
        printf, lun, ''
        WriteU,lun,bad_data_str
        Catch, /Cancel
        Close, lun
        free_lun, lun
     ENDELSE
  ENDIF    ;;keyword_set(bad_data)
  
  IF Keyword_Set(bs_pos) THEN BEGIN
     Catch, error_code
     IF (error_code NE 0) THEN BEGIN
        error_string = 'commie_scriptfile_io: Error opening file.'
        Catch, /Cancel
        RETURN
     ENDIF
     OpenU, lun, script_filename, /Get_Lun,/Append
     Catch, /Cancel
     
     Catch, error_code
     IF (error_code NE 0) THEN BEGIN
        error_string = 'commie_scriptfile_io: Error updating file.'
        Close, lun
        Free_lun, lun
        Catch, /Cancel
        RETURN
     ENDIF
     count = n_elements((*commie_dataset_struct.bs_xs)) 
     bs_xs_string = 'bs_xs'
     bs_ys_string = 'bs_ys'
     FOR i=0,count-1 DO BEGIN
        bs_xs_string = Strjoin([bs_xs_string,', ', $
                               Strtrim((*commie_dataset_struct.bs_xs)[i],2)])
        bs_ys_string = Strjoin([bs_ys_string,', ', $
                               Strtrim((*commie_dataset_struct.bs_ys)[i],2)])
     ENDFOR
     printf, lun, ''
     WriteU,lun,bs_xs_string
     printf,lun,''
     WriteU,lun,bs_ys_string
     Catch, /Cancel
     Close, lun
     free_lun, lun
  ENDIF ;;keyword_set(bs_pos)

  IF KEYWORD_SET(add_motor_positions) THEN BEGIN
     Catch, error_code
     IF (error_code NE 0) THEN BEGIN
        error_string = 'commie_scriptfile_io: Error opening file.'
        Catch, /Cancel
        RETURN
     ENDIF
     OpenR, lun, script_filename, /Get_Lun
     Catch, /Cancel

     ;; first read in the existing file
     Catch, error_code
     IF (error_code NE 0) THEN BEGIN
        error_string = 'commie_scriptfile_io: Error reading file.'
        Close, lun
        Free_lun, lun
        Catch, /Cancel
        RETURN
     ENDIF
     
     ;; read the whole file in a byte array
     temp_byte = byte(0)
     temp_bytarr = byte(0)
     WHILE ~ EOF(lun) DO BEGIN
        ReadU, lun, temp_byte
        temp_bytarr = [temp_bytarr,temp_byte]
     ENDWHILE
     Catch, /Cancel
     temp_bytarr = temp_bytarr[1:*]     
     ;; close the file only to open a new one in the same name
     close, lun
     Free_lun, lun

     Catch, error_code
     IF (error_code NE 0) THEN BEGIN
        error_string = 'commie_scriptfile_io: Error opening file.'
        Catch, /Cancel
        RETURN
     ENDIF
     OpenW, lun, script_filename, /Get_Lun
     Catch, /Cancel

     Catch, error_code
     IF (error_code NE 0) THEN BEGIN
        error_string = 'commie_scriptfile_io: Error updating file.'
        Close, lun
        Free_lun, lun
        Catch, /Cancel
        RETURN
     ENDIF

     count = n_elements((*commie_dataset_struct.position_names)) 
     FOR i=0,count-1 DO BEGIN
        this_name = (*commie_dataset_struct.position_names)[i]
        this_string = Strjoin([this_name,', '])
        FOR j=0,n_elements(*commie_dataset_struct.motor_names) -1 DO BEGIN
           this_motor = (*commie_dataset_struct.motor_names)[j]
           this_value = Strtrim((*commie_dataset_struct.motor_values)[j,i],2)
           IF j LT (n_elements(*commie_dataset_struct.motor_names) -1) THEN $
              BEGIN
              this_string = $
                 Strjoin([this_string,this_motor,'= ',this_value,', '])
           ENDIF ELSE BEGIN
              this_string = $
                 Strjoin([this_string,this_motor,'= ',this_value])
           ENDELSE
        ENDFOR
        WriteU,lun,this_string
        printf, lun, ''
     ENDFOR
     printf, lun, ';'
     ;; now add the old stuff
     WriteU, lun, String(temp_bytarr)
     Catch, /Cancel
     Close, lun
     free_lun, lun
  ENDIF ;; keyword_set(add_motor_positions)
END
