PRO commie_merge_by_pos, commie_dataset_struct, commie_script_struct
  
  compile_opt idl2, hidden
        
  ;; initialize the pos_merged_samples to hold the result of this
  ;; routine
  commie_dataset_struct.pos_merged_samples = $
     ptr_new(ptrarr(n_elements(*(commie_dataset_struct.position_names))))
  commie_dataset_struct.pos_merged_nosamples = $
     ptr_new(ptrarr(n_elements(*(commie_dataset_struct.position_names))))

  ;; initialize the relerror arrays to carry the final relerror array
  ;; for each position.
  commie_dataset_struct.relerror_samples = $
     ptr_new(ptrarr(n_elements(*(commie_dataset_struct.position_names))))
  commie_dataset_struct.relerror_no_samples = $
     ptr_new(ptrarr(n_elements(*(commie_dataset_struct.position_names))))

  ;; first start the loop through the positions for sample only, 
  ;; immediately filter out all bad data
  FOR i=0, (n_elements(*(commie_dataset_struct.position_names))-1) DO BEGIN

     ;; Give us an update b/c this may take a while
     status_string = $
        Strjoin(['Started merging position ',Strtrim(i+1,2),' of ', $
                 Strtrim(n_elements(*(commie_dataset_struct.position_names)),$
                         2),'.'])
     commie_messages,status=status_string

     ;; Get all arrays from one position
     these_indices = $
        Where(((*(commie_dataset_struct.position_indices) EQ i) $
              AND (*(commie_dataset_struct.are_bad) NE 1)), count) 
     ;; now start the merging process per position
     IF (count NE 0) THEN BEGIN
        ;; first get all the arrays that need to be passed. It is necessary
        ;; to determine the arrays outside of the subroutines if we want
        ;; to be able to change the order of steps. Dump all intermediate
        ;; results in dummy_arrays. That way, we can get all input arrays 
        ;; from dummy_arrays.
        commie_dataset_struct.dummy_arrays = ptr_new(ptrarr(count))
        ;; also store these_indices in dummy_indices to be able to pass it
        ;; along to the subroutines
        commie_dataset_struct.dummy_indices = ptr_new(these_indices)
        FOR j=0,count-1 DO BEGIN
           commie_read_dt, commie_script_struct, commie_dataset_struct, $
                       (*(commie_dataset_struct.filenames))[these_indices[j]], $
                           this_dt_par, this_array
           (*(commie_dataset_struct.dummy_arrays))[j] = $
              ptr_new(this_array)
        ENDFOR

        ;; first remove saturation from the files
        commie_remove_saturation, commie_dataset_struct, $
                                  commie_script_struct
        
        ;; then subtract the dark current signal plus pedestal
        commie_subtract_dks, commie_dataset_struct, commie_script_struct    

        ;; try to remove outlier pixels
        commie_remove_outliers, commie_dataset_struct, commie_script_struct

        ;; now initialize the relerror_arrays. This should happen after
        ;; we subtracted the dark current, removed outliers, and
        ;; saturation. The relerror_arrays are then updated
        ;; on-the-go.
        commie_dataset_struct.dummy_relerror_arrays = ptr_new(ptrarr(count))
        commie_init_relerrs, commie_dataset_struct

        ;; now normalize wrt beam current
        commie_normalize_wrt_current, commie_dataset_struct, $
                                      commie_script_struct

        ;; now merge according to exposure time
        commie_merge_by_exp, commie_dataset_struct, commie_script_struct

        ;; now subtract the no_sample from the samples unless user
        ;; defined to have them merged first separately or not at all
        IF NOT commie_dataset_struct.no_background THEN BEGIN
           IF NOT commie_dataset_struct.merge_first THEN $
              commie_subtract_no_sample, commie_dataset_struct, $
                                         commie_script_struct
        ENDIF
   
        ;; this makes sure that we take out all the noise before we
        ;; normalize wrt exp time. NOTE: this will set all negatives
        ;; in all files to zero. NOTE2: this will also ignore all
        ;; damaged pixels in all but the highest eposure. 
        commie_threshold_data, commie_dataset_struct, commie_script_struct

        ;; now normalize wrt exposure time 
        commie_normalize_wrt_exptime, commie_dataset_struct

        ;; this routine will throw everything from dummy_arrays into
        ;; one array assuming that all other steps necessary per
        ;; position have been taken (except for the masks?), i.e. this
        ;; should be the last routine to be called before freeing the 
        ;; dummy_arrays. 
        commie_merge_dummy_arrays, commie_dataset_struct, commie_script_struct
        ;; now all we need to do is to assign this array to its final
        ;; destination. NOTE: If user did not subtract no_samples yet,
        ;; then dummy_arrays will be a ptrarr of two arrays. The first
        ;; being the sample, the second being the nosample array. Put
        ;; the first one in pos_merged_samples and the second one in
        ;; pos_merged_nosamples. The existance of the latter pointer
        ;; can also be used by later routines to check if nosample has
        ;; been subtracted or not.
        IF size(*commie_dataset_struct.dummy_arrays,/type) EQ 10 THEN BEGIN
           (*commie_dataset_struct.pos_merged_samples)[i] = $
              ptr_new(*((*commie_dataset_struct.dummy_arrays)[0]))
           (*commie_dataset_struct.pos_merged_nosamples)[i] = $
              ptr_new(*((*commie_dataset_struct.dummy_arrays)[1]))
        ENDIF ELSE BEGIN
           (*commie_dataset_struct.pos_merged_samples)[i] = $
              ptr_new((*commie_dataset_struct.dummy_arrays))
        ENDELSE

        ;; same for the relerror arrays
        IF size(*commie_dataset_struct.dummy_relerror_arrays,/type) EQ 10 THEN $
           BEGIN
           (*commie_dataset_struct.relerror_samples)[i] = $
              ptr_new(*((*commie_dataset_struct.dummy_relerror_arrays)[0]))
           (*commie_dataset_struct.relerror_no_samples)[i] = $
              ptr_new(*((*commie_dataset_struct.dummy_relerror_arrays)[1]))
        ENDIF ELSE BEGIN
           (*commie_dataset_struct.relerror_samples)[i] = $
              ptr_new((*commie_dataset_struct.dummy_relerror_arrays))
        ENDELSE

        ;; Need to get rid of the dummy_arrays, dummy_relerror_arrays,
        ;; and dummy_indices of this cycle. Note that there should
        ;; only be one or two dummy_arrays left!!
        IF size(*commie_dataset_struct.dummy_arrays,/type) EQ 10 THEN BEGIN
           ptr_free,(*commie_dataset_struct.dummy_arrays)[0],$
                    (*commie_dataset_struct.dummy_arrays)[1]
        ENDIF           
        ptr_free, commie_dataset_struct.dummy_arrays
        IF size(*commie_dataset_struct.dummy_relerror_arrays,/type) EQ 10 THEN $
           BEGIN
           ptr_free,(*commie_dataset_struct.dummy_relerror_arrays)[0],$
                    (*commie_dataset_struct.dummy_relerror_arrays)[1]
        ENDIF           
        ptr_free, commie_dataset_struct.dummy_relerror_arrays
        ptr_free, commie_dataset_struct.dummy_indices

        ;; Give us an update b/c this may take a while
        status_string = $
           Strjoin(['Merged position ',Strtrim(i+1,2),' of ', $
                   Strtrim(n_elements(*(commie_dataset_struct.position_names)),$
                           2),'.'])
        commie_messages,status=status_string
     ENDIF
  ENDFOR
END
