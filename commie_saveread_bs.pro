;; This routine will try to load or write an existing bs_array from/to
;; disk. It is called by commie_find_bs.

PRO commie_saveread_bs, commie_script_struct, commie_dataset_struct, $
                        write=write, read=read
  
  compile_opt idl2, hidden

  IF Keyword_Set(read) THEN BEGIN
     ;; In this case we want to read the bs_array from
     ;; disk. Let's see if it actually exists
     IF ptr_valid(commie_script_struct.bs_filename) THEN BEGIN
        Catch, error_code
        IF (error_code NE 0) THEN BEGIN
           error_string = 'commie_saveread_bs: Error opening file.'
           Catch, /Cancel
           RETURN
        ENDIF
        this_bs_array = Read_tiff(*commie_script_struct.bs_filename)
        Catch, /Cancel
        these_inds = Where(this_bs_array GT 1,count)
        IF count NE 0 THEN $
           this_bs_array[these_inds] = 1
        commie_dataset_struct.bs_array = ptr_new(this_bs_array)
     ENDIF
  ENDIF ELSE IF Keyword_Set(write) THEN BEGIN
     IF ptr_valid(commie_script_struct.bs_filename) THEN BEGIN
        ;; create the folder if necessary
        file_mkdir,file_dirname(*commie_script_struct.bs_filename)
        Catch, error_code
        IF (error_code NE 0) THEN BEGIN
           error_string = 'commie_saveread_bs: Error writing file.'
           Catch, /Cancel
           print,error_string
           RETURN
        ENDIF
        bs_array = *commie_dataset_struct.bs_array
        these_inds = Where(bs_array EQ 1, count)
        IF count NE 0 THEN $
           bs_array[these_inds] =255
        Write_tiff, (*commie_script_struct.bs_filename), bs_array
        Catch, /Cancel
     ENDIF
  ENDIF
END
