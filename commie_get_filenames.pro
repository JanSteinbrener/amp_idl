;; this routine uses the arrays sample_indices and no_sample_indices together
;; with sample_dirs, sample_headers, no_sample_dirs, no_sample_headers
;; in commie_script_struct to create the array filenames in 
;; commie_dataset_struct. It also sets the appropriate value in 
;; commie_dataset_struct.are_samples


PRO commie_get_filenames, commie_script_struct, commie_dataset_struct, $
                          dark_current=dark_current
  
  compile_opt idl2, hidden

  ;; get appropriate path separator
  path_sep = path_sep()
  ;; get the appropriate file extension
  IF (Strcmp(*(commie_script_struct.filetype),'dt_nc',/Fold_Case) $
      EQ 1) THEN $
         this_ext = '.nc'
  
  ;; start with sample files first
  this_array = *(commie_script_struct.sample_indices)
  this_dir = *(commie_script_struct.sample_dirs)
  this_header = *(commie_script_struct.sample_headers)
  svec = size(this_array)
  
  temp_strarr = ''
  temp_arr = 0
  FOR i=0,(svec[1]-1) DO BEGIN
     FOR j=(this_array[i,0]),(this_array[i,1]) DO BEGIN
        temp_name = Strjoin([this_dir[i],path_sep,this_header[i],'_', $
                             string(FORMAT='(I04)',j),this_ext])
        temp_strarr = [temp_strarr, temp_name]
        temp_arr = [temp_arr,1]
     ENDFOR
  ENDFOR
  temp_strarr = temp_strarr[1:*]
  temp_arr = temp_arr[1:*]
  
  ;; do not try to initialize no_sample files if no_background flag
  ;; has been set.
  IF NOT commie_dataset_struct.no_background THEN BEGIN
     ;; now deal with no_sample files
     this_array = *(commie_script_struct.no_sample_indices)
     this_dir = *(commie_script_struct.no_sample_dirs)
     this_header = *(commie_script_struct.no_sample_headers)
     svec = size(this_array)
     
     temp_strarr2 = ''
     temp_arr2 = 0
     FOR i=0,(svec[1]-1) DO BEGIN
        FOR j=(this_array[i,0]),(this_array[i,1]) DO BEGIN
           temp_name = strjoin([this_dir[i],path_sep,this_header[i], $
                                '_', string(FORMAT='(I04)',j),this_ext])
           temp_strarr2 = [temp_strarr2, temp_name]
           temp_arr2 = [temp_arr2,0]
        ENDFOR
     ENDFOR
     temp_strarr2 = temp_strarr2[1:*]
     temp_arr2 = temp_arr2[1:*]
     commie_dataset_struct.filenames = ptr_new([temp_strarr,temp_strarr2]) 
     commie_dataset_struct.are_samples = ptr_new([temp_arr,temp_arr2])
  ENDIF ELSE BEGIN
     ;; in this case just save the information from the sample files
     ;; from before.
     commie_dataset_struct.filenames = ptr_new(temp_strarr) 
     commie_dataset_struct.are_samples = ptr_new(temp_arr)
  ENDELSE

  ;; now see if the user defined a BS save filename
  IF ptr_valid(commie_script_struct.bs_filename) THEN BEGIN
     this_name = (*commie_script_struct.bs_filename)
     ptr_free, commie_script_struct.bs_filename
     this_name = Strjoin([(*commie_script_struct.topdir),path_sep,this_name])
     ;; store again
     commie_script_struct.bs_filename = ptr_new(this_name)
  ENDIF

  ;; now see if the user defined a dk saveread filename
  IF ptr_valid(commie_script_struct.dk_saveread_file) THEN BEGIN
     this_name = (*commie_script_struct.dk_saveread_file)
     ptr_free, commie_script_struct.dk_saveread_file
     this_name = Strjoin([(*commie_script_struct.topdir),path_sep,this_name])
     ;; store again
     commie_script_struct.dk_saveread_file = ptr_new(this_name)
  ENDIF ELSE BEGIN
     ;; use topdir with filename dk_params
     commie_script_struct.dk_saveread_file = $
        ptr_new(Strjoin([*commie_script_struct.topdir,path_sep,'dk_params']))
  ENDELSE

  ;; now create the dark_current filenames
  IF Keyword_Set(dark_current) THEN BEGIN
     IF ptr_valid(commie_script_struct.dark_current_indices) THEN BEGIN
        this_array = *(commie_script_struct.dark_current_indices)
        this_dir = *(commie_script_struct.dark_current_dirs)
        this_header = *(commie_script_struct.dark_current_headers)
        svec = size(this_array)
        
        temp_strarr = ''
        FOR i=0,(svec[1]-1) DO BEGIN
           FOR j=(this_array[i,0]),(this_array[i,1]) DO BEGIN
              temp_name = strjoin([this_dir[i],path_sep,this_header[i], $
                                   '_', string(FORMAT='(I04)',j),this_ext])
              temp_strarr = [temp_strarr, temp_name]
           ENDFOR
        ENDFOR
        temp_strarr = temp_strarr[1:*]
        commie_dataset_struct.dk_filenames = ptr_new(temp_strarr) 
     ENDIF
  ENDIF
END
