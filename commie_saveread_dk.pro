PRO commie_saveread_dk,commie_dataset_struct,commie_script_struct, $
                       read=read, write=write

  
  compile_opt idl2, hidden

  ;; define the filename basenames first. They will be appended by the
  ;; exposure time in the format **.***
  filenames = ['dk_rms_fast_','dk_rms_slow_','dk_avg_fast_','dk_avg_slow_', $
              'dk_damaged_pix_']
  ;; Get the path separator
  path_sep = path_sep()
  ;; Get the filepath
  filepath = File_dirname(*commie_script_struct.dk_saveread_file)
     
  IF Keyword_Set(read) THEN BEGIN
     ;; check for the existence of the parameter file
     filename = *commie_script_struct.dk_saveread_file
     info = File_info(filename,/Noexpand_path) 
     IF NOT info.exists THEN BEGIN
        commie_messages, commie_dataset_struct, commie_script_struct, /dk_read
        RETURN
     ENDIF ELSE BEGIN
        ;; check for the existence of other files based on wether we
        ;; have adc_slow or fast and if user needs the average arrays
        ;; when dk_by_pix is set. Get the list of filenames that we
        ;; need to have first
        ;; see if we have adc_fast or slow (we will not allow for
        ;; inhomogeneous adc speed right now)
        dummy = Where(*commie_dataset_struct.adc_are_slow EQ 0, fast)
        dummy = Where(*commie_dataset_struct.adc_are_slow EQ 1, slow)
        IF fast GT 0 THEN BEGIN
           ;; see if we have dk_by_pix or not
           IF commie_dataset_struct.dk_by_pix THEN $
              inds = [0,2,4] ELSE inds = [4]
        ENDIF ELSE IF slow GT 0 THEN BEGIN
           ;; see if we have dk_by_pix or not
           IF commie_dataset_struct.dk_by_pix THEN $
              inds = [1,3,4] ELSE inds = [4]
        ENDIF

        ;; now check if all the files are there, except for
        ;; damaged_pixels because there might not be any
        IF commie_dataset_struct.dk_by_pix THEN BEGIN
           FOR i=0,n_elements(inds)-2 DO BEGIN
              FOR j=0,n_elements(*commie_dataset_struct.exp_seconds)-1 DO BEGIN
                 this_exp = String(FORMAT='(F06.3)', $
                                   (*commie_dataset_struct.exp_seconds)[j])
                 this_fname = Strjoin([filepath,path_sep, filenames[inds[i]], $
                                       this_exp, '.bin']) 
                 this_info = File_info(this_fname,/Noexpand_path) 
                 IF NOT this_info.exists THEN BEGIN
                    commie_messages, commie_dataset_struct, $
                                     commie_script_struct, $
                                     /dk_read
                    RETURN
                 ENDIF
              ENDFOR
           ENDFOR
        ENDIF
        
        ;; if all necessary files are present, then check if
        ;; scriptfile contains all necessary parameters.
        Catch, error_code
        IF (error_code NE 0) THEN BEGIN
           error_string = 'commie_saveread_dk: Error opening file.'
           Catch, /Cancel
           RETURN
        ENDIF
        OpenR, lun, filename, /Get_Lun
        Catch, /Cancel
        
        Catch, error_code
        IF (error_code NE 0) THEN BEGIN
           error_string = 'commie_saveread_dk: Error reading file.'
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

        ;; Define the linefeed byte. This is used to split the string
        ;; into separate lines. The linefeed tag is present in both
        ;; Windows and Unix systems.
        linefeed = String(Byte(10))
        temp_bytarr = temp_bytarr[1:*]

        str_arr = $
           Strsplit(String(temp_bytarr),linefeed,count=str_count,/Extract)
        ;; now start loop to find matching statements, remove any
        ;; whitespace. Try to determine if all tags at least are
        ;; present. If there is going to be a distinction between
        ;; adc_fast and slow then we might have to be a little more
        ;; detailed here.
        tag_count = 0
        FOR i=0, str_count-1 DO BEGIN
           IF (Strcmp(str_arr[i],'dk_max_pedestal',15,/FOLD_CASE) $
               EQ 1) THEN BEGIN
              temp_str = Strtrim(Strsplit(str_arr[i],',',/Extract),2)
              commie_dataset_struct.dk_max_pedestal = $
                 ptr_new([Double(temp_str[1]),Double(temp_str[2])])
              tag_count++
           ENDIF ELSE IF (Strcmp(str_arr[i],'dk_max_slope',12,/FOLD_CASE) $
               EQ 1) THEN BEGIN
              temp_str = Strtrim(Strsplit(str_arr[i],',',/Extract),2)
              commie_dataset_struct.dk_max_slope = $
                 ptr_new([Double(temp_str[1]),Double(temp_str[2])])
              tag_count++
           ENDIF ELSE IF (Strcmp(str_arr[i],'dk_sigma_pedestal',17,/FOLD_CASE) $
               EQ 1) THEN BEGIN
              temp_str = Strtrim(Strsplit(str_arr[i],',',/Extract),2)
              commie_dataset_struct.dk_sigma_pedestal = $
                 ptr_new([Double(temp_str[1]),Double(temp_str[2])])
              tag_count++
           ENDIF ELSE IF (Strcmp(str_arr[i],'dk_sigma_slope',14,/FOLD_CASE) $
               EQ 1) THEN BEGIN
              temp_str = Strtrim(Strsplit(str_arr[i],',',/Extract),2)
              commie_dataset_struct.dk_sigma_slope = $
                 ptr_new([Double(temp_str[1]),Double(temp_str[2])])
              tag_count++
           ENDIF ELSE IF (Strcmp(str_arr[i],'dk_exp_seconds',14,/FOLD_CASE) $
               EQ 1) THEN BEGIN
              temp_str = Strtrim(Strsplit(str_arr[i],',',/Extract),2)
              ;; now the first one will be the fast exp_seconds,
              ;; delimited by whitespace
              temp_str1 = Float(Strtrim(Strsplit(temp_str[1],' ', $
                                                  /Extract),2))
              temp_str2 = Float(Strtrim(Strsplit(temp_str[2],' ', $
                                                  /Extract),2))
              commie_dataset_struct.dk_exp_seconds = $
                 ptr_new([ptr_new(temp_str1), ptr_new(temp_str2)])
              tag_count++
           ENDIF
        ENDFOR
        IF tag_count NE 5 THEN BEGIN
           commie_messages, commie_dataset_struct, commie_script_struct, $
                            /dk_read
           RETURN
        ENDIF ELSE BEGIN
           ;; now read in the bin files based on what we need.
           ;; now check if all the files are there. 
           these_fasts_rms = $
              ptrarr(n_elements(*commie_dataset_struct.exp_seconds))
           these_slows_rms = $
              ptrarr(n_elements(*commie_dataset_struct.exp_seconds))
           these_fasts_avg = $
              ptrarr(n_elements(*commie_dataset_struct.exp_seconds))
           these_slows_avg = $
              ptrarr(n_elements(*commie_dataset_struct.exp_seconds))
           these_arrays = ptrarr(n_elements(*commie_dataset_struct.exp_seconds))
           ;; we will try to read in fasts and slow and just hand back
           ;; an empty pointer array to commie
           FOR i=0,n_elements(inds)-1 DO BEGIN
              FOR j=0,n_elements(*commie_dataset_struct.exp_seconds)-1 DO BEGIN
                 this_exp = String(FORMAT='(F06.3)', $
                                   (*commie_dataset_struct.exp_seconds)[j])
                 this_fname = Strjoin([filepath,path_sep, filenames[inds[i]], $
                                       this_exp, '.bin']) 
               
                 ;; need to check existence for the damaged pixels
                 IF inds[i] EQ 4 THEN BEGIN
                    info = File_info(this_fname)
                    IF info.exists THEN BEGIN
                       this_array = read_bin(this_fname)
                    ENDIF ELSE BEGIN
                       this_array = -1
                    ENDELSE
                    these_arrays[j]=ptr_new(this_array)
                 ENDIF ELSE BEGIN
                    this_array = read_bin(this_fname)
                    IF inds[i] EQ 0 THEN $
                       these_fasts_rms[j]=ptr_new(this_array)
                    IF inds[i] EQ 1 THEN $
                       these_slows_rms[j]=ptr_new(this_array)
                    IF inds[i] EQ 2 THEN $
                       these_fasts_avg[j]=ptr_new(this_array)
                    IF inds[i] EQ 3 THEN $
                       these_slows_avg[j]=ptr_new(this_array)
                 ENDELSE
              ENDFOR
           ENDFOR 
           commie_dataset_struct.dk_rms_arrays = $
              ptr_new([ptr_new(these_fasts_rms), ptr_new(these_slows_rms)])
           ;; now do the same for the averages if necessary. Note that
           ;; we don't need to free the ptrarr of the avgs
           ;; because they will all be NULL pointers if dk_by_pix has
           ;; not been set.
           IF commie_dataset_struct.dk_by_pix THEN BEGIN
              commie_dataset_struct.dk_avg_arrays = $
                 ptr_new([ptr_new(these_fasts_avg), ptr_new(these_slows_avg)])
           ENDIF
           ;; now store the damaged pixel files
           commie_dataset_struct.damaged_pixels = ptr_new(these_arrays)
        ENDELSE
        ;; Give us an update b/c this may take a while
        status_string = 'Initialized dark currents.'
        commie_messages,status=status_string
     ENDELSE
  ENDIF ELSE IF Keyword_Set(write) THEN BEGIN
     IF ptr_valid(commie_script_struct.dk_saveread_file) THEN BEGIN
        ;; create the directory
        file_mkdir,filepath
        
        ;; write the parameters into a textfile called dk_params
        filename = *commie_script_struct.dk_saveread_file
        
        Catch, error_code
        IF (error_code NE 0) THEN BEGIN
           print = 'commie_saveread_dk: Error opening file.'
           Catch, /Cancel
           RETURN
        ENDIF
        ;; this will delete any existing file of the same name in this dir.
        OpenW, lun, filename, /Get_Lun
        Catch, /Cancel
        
        Catch, error_code
        IF (error_code NE 0) THEN BEGIN
           print = 'commie_saveread_dk: Error writing file.'
           Close, lun
           Free_lun, lun
           Catch, /Cancel
           RETURN
        ENDIF

        ;; now see what we need to write to the scriptfile
        IF ptr_valid(commie_dataset_struct.dk_max_pedestal) THEN BEGIN
           printf,lun,'dk_max_pedestal,', $
                  Strtrim((*commie_dataset_struct.dk_max_pedestal)[0],2),', ', $
                  Strtrim((*commie_dataset_struct.dk_max_pedestal)[1],2)
        ENDIF
        IF ptr_valid(commie_dataset_struct.dk_max_slope) THEN BEGIN
           printf,lun,'dk_max_slope,', $
                  Strtrim((*commie_dataset_struct.dk_max_slope)[0],2),', ', $
                  Strtrim((*commie_dataset_struct.dk_max_slope)[1],2)
        ENDIF
        IF ptr_valid(commie_dataset_struct.dk_sigma_pedestal) THEN BEGIN
           printf,lun,'dk_sigma_pedestal,', $
                  Strtrim((*commie_dataset_struct.dk_sigma_pedestal)[0],2), $
                  ', ',Strtrim((*commie_dataset_struct.dk_sigma_pedestal)[1],2)
        ENDIF
        IF ptr_valid(commie_dataset_struct.dk_sigma_slope) THEN BEGIN
           printf,lun,'dk_sigma_slope,', $
                  Strtrim((*commie_dataset_struct.dk_sigma_slope)[0],2),', ', $
                  Strtrim((*commie_dataset_struct.dk_sigma_slope)[1],2)
        ENDIF
        IF ptr_valid(commie_dataset_struct.dk_exp_seconds) THEN BEGIN
           string = ' '
           fast_exp = *((*commie_dataset_struct.dk_exp_seconds)[0])
           slow_exp = *((*commie_dataset_struct.dk_exp_seconds)[1])
           FOR i=0,n_elements(fast_exp)-1 DO $
              string = Strjoin([string,Strtrim(fast_exp[i],2),' '])
           string = Strjoin([string,','])
           FOR i=0,n_elements(slow_exp)-1 DO $
              string = Strjoin([string,' ',Strtrim(slow_exp[i],2)])
           string = Strjoin(['dk_exp_seconds,',string])
           printf,lun,string
        ENDIF
        ;; close this file
        Catch,/Cancel
        Close, lun
        free_lun, lun

        ;; now write out the arrays of damaged pixels, rms-noises and
        ;; dk_averages if needed. Write them as bin-files.
        ;; Start with the dk_rms_arrays
        IF ptr_valid(commie_dataset_struct.dk_rms_arrays) THEN BEGIN
           ;; these are the fast arrays
           IF ptr_valid((*commie_dataset_struct.dk_rms_arrays)[0]) THEN BEGIN
              these_fasts = *((*commie_dataset_struct.dk_rms_arrays)[0])
              these_exps = *((*commie_dataset_struct.dk_exp_seconds)[0])
              FOR i=0,n_elements(these_exps)-1 DO BEGIN
                 this_exp = String(FORMAT='(F06.3)', these_exps[i])
                 IF ptr_valid(these_fasts[i]) THEN BEGIN
                    filename = $
                       Strjoin([filepath,path_sep, $
                                'dk_rms_fast_',this_exp,'.bin'])
                   write_bin,filename,*(these_fasts[i])
                 ENDIF
              ENDFOR
           ENDIF 
           ;; these are the slow arrays
           IF ptr_valid((*commie_dataset_struct.dk_rms_arrays)[1]) THEN BEGIN
              these_slows = *((*commie_dataset_struct.dk_rms_arrays)[1])
              these_exps = *((*commie_dataset_struct.dk_exp_seconds)[1])
              FOR i=0,n_elements(these_exps)-1 DO BEGIN
                 this_exp = String(FORMAT='(F06.3)', these_exps[i])
                 IF ptr_valid(these_slows[i]) THEN BEGIN
                    filename = $
                       Strjoin([filepath,path_sep, $
                                'dk_rms_slow_',this_exp,'.bin'])
                    write_bin,filename,*(these_slows[i])
                 ENDIF
              ENDFOR
           ENDIF
        ENDIF
        ;; now the avg arrays if necessary
        IF ptr_valid(commie_dataset_struct.dk_avg_arrays) THEN BEGIN
           ;; these are the fast arrays
           IF ptr_valid((*commie_dataset_struct.dk_avg_arrays)[0]) THEN BEGIN
              these_fasts = *((*commie_dataset_struct.dk_avg_arrays)[0])
              these_exps = *((*commie_dataset_struct.dk_exp_seconds)[0])
              FOR i=0,n_elements(these_exps)-1 DO BEGIN
                 this_exp = String(FORMAT='(F06.3)', these_exps[i])
                 IF ptr_valid(these_fasts[i]) THEN BEGIN
                    filename = $
                       Strjoin([filepath,path_sep, $
                                'dk_avg_fast_',this_exp,'.bin'])
                    write_bin,filename,*(these_fasts[i])
                 ENDIF
              ENDFOR
           ENDIF 
           ;; these are the slow arrays
           IF ptr_valid((*commie_dataset_struct.dk_avg_arrays)[1]) THEN BEGIN
              these_slows = *((*commie_dataset_struct.dk_avg_arrays)[1])
              these_exps = *((*commie_dataset_struct.dk_exp_seconds)[1])
              FOR i=0,n_elements(these_exps)-1 DO BEGIN
                 this_exp = String(FORMAT='(F06.3)', these_exps[i])
                 IF ptr_valid(these_slows[i]) THEN BEGIN
                    filename = $
                       Strjoin([filepath,path_sep, $
                                'dk_avg_slow_',this_exp,'.bin'])
                    write_bin,filename,*(these_slows[i])
                 ENDIF
              ENDFOR
           ENDIF
        ENDIF
        ;; and the damaged pixel arrays
        IF ptr_valid(commie_dataset_struct.damaged_pixels) THEN BEGIN
           these_exps = *((*commie_dataset_struct.dk_exp_seconds)[0])
           FOR i=0,n_elements(these_exps)-1 DO BEGIN
              this_exp = String(FORMAT='(F06.3)', these_exps[i])   
              IF ptr_valid((*commie_dataset_struct.damaged_pixels)[i]) THEN $
                 BEGIN
                 filename = $
                    Strjoin([filepath,path_sep, $
                             'dk_damaged_pix_',this_exp,'.bin'])
                 write_bin,filename, $
                           *((*commie_dataset_struct.damaged_pixels)[i])
              ENDIF
           ENDFOR
        ENDIF 
     ENDIF
  ENDIF
END
