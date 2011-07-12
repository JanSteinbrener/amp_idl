PRO commie, script_filename, verbose=verbose, $
            merge_first=merge_first, show_final=show_final, $
            dk_by_pix=dk_by_pix, use_saved_merged_pos=use_saved_merged_pos,$
            no_background=no_background

  compile_opt idl2

  ;; Define the current version of commie. Increment by .1 for minor
  ;; changes and add-ons. Increment by 1 for larger releases.
  COMMIE_VERSION = 1.0
  
  ;; First initialize the structures
  ;; initialize structure that contains all info about raw data
  commie_script_struct = {commie_script_struct}
  ;; initialize structure that contains info about the dataset
  commie_dataset_struct = {commie_dataset_struct}
  
  ;; make sure we copy the version into the script_struct
  commie_script_struct.commie_version = ptr_new(COMMIE_VERSION)
 
  ;; now define if we should merge first or subtract no_sample per
  ;; position per exposure time.
  IF Keyword_Set(merge_first) THEN $
     commie_dataset_struct.merge_first = 1

  ;; see if the user wanted to have the dark current subtracted on a
  ;; per pixel basis. If so then set the dk_per_pix
  IF Keyword_Set(dk_by_pix) THEN $
     commie_dataset_struct.dk_by_pix = 1

  ;; now see if user indicated that no_background should be
  ;; subtracted. In this case set merge_first to 0
  IF Keyword_Set(no_background) THEN BEGIN
     commie_dataset_struct.merge_first = 0
     commie_dataset_struct.no_background = 1
  ENDIF

  ;; see if user set the verbose keyword. If the user specified a filename
  ;; then write verbose output to that file instead of terminal. The default 
  ;; directory is the topdir as specified in commie_script_struct. The user can
  ;; add subdirs similiar to the subdir specifications in the scriptfile.
  IF Keyword_Set(verbose) THEN BEGIN
     IF (size(verbose,/type) EQ 2) THEN BEGIN
        (commie_script_struct.verbose) = ptr_new(fix(1))
     ENDIF ELSE IF (size(verbose,/type) EQ 7) THEN BEGIN
        path_sep = path_sep()
        ;; take out any leading path separators to be consistent 
        IF (Strpos(verbose,path_sep) EQ 0) THEN BEGIN
           verbose = Byte(verbose)
           verbose = String(verbose[1:*])
        ENDIF
        ;; also take out any trailing path separator to be consistent
        IF (Strpos(verbose,path_sep,/Reverse_Search) EQ $
            (Strlen(verbose)-1)) THEN BEGIN
           verbose = Byte(verbose)
           verbose = String(verbose[0:n_elements(verbose)-2])
        ENDIF
        ;; store subfolder for now, the expanded path is then stored
        ;; in _scriptfile_io routine
        commie_script_struct.verbose = ptr_new(verbose)
     ENDIF
  ENDIF ELSE BEGIN
     commie_script_struct.verbose = ptr_new(0)
  ENDELSE

  ;; store filename of scriptfile
  commie_script_struct.script_filename = ptr_new(script_filename)

  ;; read appropriate file-info from scriptfile
  commie_scriptfile_io, script_filename, commie_script_struct, $
                        commie_dataset_struct,error_string, /read
  
  ;; this creates an array of filenames from the info given in scriptfile
  commie_get_filenames,commie_script_struct, commie_dataset_struct, $
                       /dark_current
  
  ;; this associates each file with a position defined in scriptfile
  ;; it also let's the user check for bad data files if not already defined
  ;; in scriptfile. If verbose is defined, then it will print out some 
  ;; additional info.
  commie_initialize_dataset,commie_script_struct, $
                            commie_dataset_struct

  ;; see if we can get the dark current information from disk 
  commie_saveread_dk,commie_dataset_struct,commie_script_struct,/read

  ;; This function determines the scaling of the dark current with
  ;; exposure time. Since it is independent of commie, we need to pass
  ;; on all variables outside their structures. We only call this
  ;; routine if we were not able to find the info on disk. We check
  ;; against the existence of the dk_rms_arrays pointer
  IF NOT ptr_valid(commie_dataset_struct.dk_max_pedestal) THEN BEGIN
     ccd_classify, *(commie_dataset_struct.dk_filenames), $
                   slow_params, fast_params, slow_by_pix, fast_by_pix, $
                   damaged_pixels, $
                   verbose=(*commie_script_struct.verbose), $
                   dk_by_pix=commie_dataset_struct.dk_by_pix, $
                   rotate=*commie_dataset_struct.rotate, $
                   median_percent=*commie_script_struct.median_percent, $
                   subset_indices=*commie_script_struct.subset_indices
     
     ;; store the damaged pixels 
     commie_dataset_struct.damaged_pixels = ptr_new(damaged_pixels)
 
     ;; set the values in commie_dataset_struct. Note that 0 is fast and
     ;; 1 is slow, corresponding to the value in adc_are_slow.
     commie_dataset_struct.dk_max_pedestal = $
        ptr_new([fast_params.max_pedestal,slow_params.max_pedestal])
     commie_dataset_struct.dk_max_slope = $
        ptr_new([fast_params.max_slope, slow_params.max_slope])
     commie_dataset_struct.dk_sigma_pedestal = $
        ptr_new([fast_params.sigma_pedestal,slow_params.sigma_pedestal])
     commie_dataset_struct.dk_sigma_slope = $
        ptr_new([fast_params.sigma_slope, slow_params.sigma_slope])
     commie_dataset_struct.dk_exp_seconds = $
        ptr_new([ptr_new(fast_by_pix.exp_seconds), $
                 ptr_new(slow_by_pix.exp_seconds)])
     IF commie_dataset_struct.dk_by_pix THEN BEGIN
        commie_dataset_struct.dk_rms_arrays = $
           ptr_new([ptr_new(fast_by_pix.rms_arrays_by_exp), $
                    ptr_new(slow_by_pix.rms_arrays_by_exp)])       
        commie_dataset_struct.dk_avg_arrays = $
           ptr_new([ptr_new(fast_by_pix.dk_arrays_by_exp), $
                    ptr_new(slow_by_pix.dk_arrays_by_exp)])
     ENDIF
 
     ;; write the dk_parameters to disk if the user specified that
     commie_saveread_dk, commie_dataset_struct, commie_script_struct, /write

     ;; Give us an update b/c this may take a while
     status_string = 'Initialized dark currents.'
     commie_messages,status=status_string
  ENDIF

  ;; find the shift in positions
  commie_find_pos_shifts, commie_script_struct, commie_dataset_struct

  IF Keyword_Set(use_saved_merged_pos) THEN $
     commie_saveread_merged_pos,commie_script_struct,commie_dataset_struct, $
                                /read
  ;; only merge by position if we don't read them in from disk
  IF NOT ptr_valid(commie_dataset_struct.pos_merged_samples) THEN BEGIN
     ;; this will take all files from one position, subtract the 
     ;; appropriate dark current file and no_sample file and then store 
     ;; the normalised results in pos_merged_arrays.
     commie_merge_by_pos, commie_dataset_struct, commie_script_struct
     commie_saveread_merged_pos,commie_script_struct,commie_dataset_struct, $
                                /write
  ENDIF

  ;; this will determine the bs mask
  commie_find_bs_arr, commie_script_struct, $
                      commie_dataset_struct

  ;; Now we are in the position to apply the mask-files.
  commie_remove_bs, commie_dataset_struct, commie_script_struct

  ;; final step is to remove corner scatter from each pos_merged_array
  ;; and then to throw everything together.
  commie_assemble_merged_pos, commie_dataset_struct, commie_script_struct

  ;; subtract no_sample now if it hasn't already been subtracted.
  IF ptr_valid(commie_dataset_struct.assembled_nosample) THEN BEGIN
     (*commie_dataset_struct.assembled_sample) -= $
        (*commie_dataset_struct.assembled_nosample)
     ;; update the relerror
     these_relerrs = [ptr_new(*commie_dataset_struct.relerror_samples), $
                      ptr_new(*commie_dataset_struct.relerror_no_samples)]
     ptr_free,commie_dataset_struct.relerror_samples
     commie_dataset_struct.relerror_samples = $
        ptr_new(commie_propagate_errors(these_relerrs))
     ;; free these_relerrs
     ptr_free,these_relerrs[0],these_relerrs[1]
     ;; show some more information if the user desires so.
     IF *commie_script_struct.verbose THEN BEGIN
        commie_verbose_output, commie_dataset_struct, commie_script_struct, $
                               /subtract_assembled_ns
     ENDIF ;; verbose
  ENDIF
  
  ;; now zero any negative values.
  commie_zero_negatives, commie_dataset_struct
  
  ;; show some more information if the user desires so
  IF Keyword_Set(show_final) THEN BEGIN
     final_array = (*commie_dataset_struct.assembled_sample)+0.0001
     window,/free,xsize=670,ysize=650,title='assembled array - log scale'
     tvscl,Alog10(Congrid(final_array,670,650))
  ENDIF ELSE IF *commie_script_struct.verbose THEN BEGIN
     commie_verbose_output, commie_dataset_struct, $
                            commie_script_struct, /final_array
  ENDIF
  
  ;; now write the final result to disk as an hdf5 file. The user can
  ;; either specify a filename with the 'output' keyword in the
  ;; scriptfile or commie will generate one automatically.
  commie_write_adi, commie_dataset_struct, commie_script_struct

  ;; free all pointers there might be
  commie_free_all,commie_dataset_struct,commie_script_struct
END
