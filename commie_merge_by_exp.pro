;; this routine takes the arrays from dummy_arrays and merges both samples 
;; and no_samples for each exp. time. The resulting arrays are then stored in 
;; dummy_arrays first all samples in ascending order of exp. time then all
;; no_samples. The array dummy_indices is modified to be able to keep track of
;; which files are samples/no_samples and which exposure time they have 
;; DUMMY_INDICES is the set of indices that the subset had in the original 
;; set of files. We need this to be able to check against exp_indices, 
;; are_samples and are_bad. 
PRO commie_merge_by_exp, commie_dataset_struct, commie_script_struct

  compile_opt idl2, hidden

  ;; first thing to do is to normalize the arrays
  commie_flux_normalization, commie_dataset_struct, commie_script_struct, $
                             /merge_by_exp

  these_indices = $
     *(commie_dataset_struct.dummy_indices)
  ;; Now get all the dummy_arrays into temp_arrays. Then re-allocate
  ;; memory to fit for the reduced size of datafiles after they have been
  ;; merged. Note that we need to hand over every individual array
  temp_arrays = $
     ptr_new(ptrarr(n_elements(*(commie_dataset_struct.dummy_arrays))))
  FOR i=0,(n_elements(*(commie_dataset_struct.dummy_arrays))-1) DO BEGIN
     (*temp_arrays)[i]=$
        ptr_new(*((*(commie_dataset_struct.dummy_arrays))[i]))
  ENDFOR
  ;; now de-allocate the dummy_arrays and re-allocate them. Need to know 
  ;; how many arrays there will be - this will be determined in the first
  ;; for loops. Define the variables n_arrays for that
  n_arrays = 0
  n_times=n_elements(*(commie_dataset_struct.exp_seconds))
  FOR i=0,(n_elements(*(commie_dataset_struct.dummy_arrays))-1) DO BEGIN
     ptr_free, (*(commie_dataset_struct.dummy_arrays))[i]
  ENDFOR
  
  ;; this will hold the reduced set of indices. Not every position has
  ;; all exposure times so the actual number of arrays will be less than
  ;; 2*n_times!
  temp_indices = 0
  ;; Now look at the samples only and per exposure time. 
  FOR i=0, (n_times-1) DO BEGIN
     sample_indices = $
       Where(((*(commie_dataset_struct.are_samples))[these_indices] EQ 1) AND $
             ((*(commie_dataset_struct.exp_indices))[these_indices] EQ i),count)
     IF count NE 0 THEN BEGIN
        ;; add 1 to our n_samples counter
        n_arrays += 1
        ;; store the first index that applies here in the new 
        ;; dummy_indices array
        temp_indices = [temp_indices,these_indices[sample_indices[0]]]
     ENDIF
  ENDFOR
  ;; now do the same for the no_sample files and append them to the array
  ;; of sample arrays
  FOR i=0, (n_times-1) DO BEGIN
     no_sample_indices = $
       Where(((*(commie_dataset_struct.are_samples))[these_indices] EQ 0) AND $
             ((*(commie_dataset_struct.exp_indices))[these_indices]) EQ i,count)
     IF count NE 0 THEN BEGIN
        ;; add 1 to our n_arrays counter
        n_arrays += 1
        ;; append the first index that applies here to the new 
        ;; dummy_indices array
        temp_indices = [temp_indices,these_indices[no_sample_indices[0]]]
     ENDIF
  ENDFOR
  ;; now collapse dummy_indices
  ptr_free, commie_dataset_struct.dummy_indices
  commie_dataset_struct.dummy_indices = ptr_new(temp_indices[1:*])
  
  ;; now allocate memory for the arrays to be averaged and perform averaging
  *(commie_dataset_struct.dummy_arrays) = ptrarr(n_arrays)
  ;; also allocate memory for the reduced set of relerror_arrays
  temp_relerrors = ptrarr(n_arrays)

  ;; define counting variable for reduced number of arrays
  k = 0
  FOR i=0, (n_times-1) DO BEGIN
     these_relerrs = ptr_new()
     these_arrays = ptr_new()
     sample_indices = $
       Where(((*(commie_dataset_struct.are_samples))[these_indices] EQ 1) AND $
             ((*(commie_dataset_struct.exp_indices))[these_indices]) EQ i,count)
     IF count NE 0 THEN BEGIN
        FOR j=0,(count-1) DO BEGIN
           ;; add the new relerr to the existing ones
           these_relerrs = $
              [these_relerrs, $
  ptr_new(*((*commie_dataset_struct.dummy_relerror_arrays)[sample_indices[j]]))]
           ;; add the new array 
           these_arrays = $
              [these_arrays,ptr_new(*((*temp_arrays)[sample_indices[j]]))]
        ENDFOR
        these_arrays = these_arrays[1:*]
        these_relerrs = these_relerrs[1:*]

        ;; get weighted average and new relerror array
        (*(commie_dataset_struct.dummy_arrays))[k] = $
           ptr_new(commie_weighted_averages(these_arrays, these_relerrs, $
                                            /no_threshold)) 
        ;; now store the new relerror_array
        temp_relerrors[k] = ptr_new(these_relerrs)
        ;; add 1 to our counting variable
        k += 1
        ;; free the arrays
        FOR j=0,n_elements(these_arrays)-1 DO BEGIN
           ptr_free,these_arrays[j]
        ENDFOR
     ENDIF
  ENDFOR
  ;; now do the same for the no_sample files and append them to the array
  ;; of sample arrays
  FOR i=0, (n_times-1) DO BEGIN
     no_sample_indices = $
       Where(((*(commie_dataset_struct.are_samples))[these_indices] EQ 0) AND $
             ((*(commie_dataset_struct.exp_indices))[these_indices]) EQ i,count)
     these_relerrs = ptr_new()
     these_arrays = ptr_new()
   IF count NE 0 THEN BEGIN
        FOR j=0,(count-1) DO BEGIN
           ;; add the new relerr to the existing ones
           these_relerrs = $
              [these_relerrs, $
               ptr_new(*((*commie_dataset_struct.dummy_relerror_arrays)[no_sample_indices[j]]))]
           ;; add the new array 
           these_arrays = $
              [these_arrays,ptr_new(*((*temp_arrays)[no_sample_indices[j]]))]
        ENDFOR
        these_arrays = these_arrays[1:*]
        these_relerrs = these_relerrs[1:*]

        ;; get weighted average and new relerror
        (*(commie_dataset_struct.dummy_arrays))[k] = $
           ptr_new(commie_weighted_averages(these_arrays, these_relerrs, $
                                            /no_threshold)) 
        ;; now store the new relerror_array
        temp_relerrors[k] = ptr_new(these_relerrs)
        ;; add 1 to our counting variable
        k += 1
        ;; free the arrays
        FOR j=0,n_elements(these_arrays)-1 DO BEGIN
           ptr_free,these_arrays[j]
        ENDFOR
     ENDIF
  ENDFOR
  ;; don't forget to free the temp_arrays and to hand over the
  ;; temp_relerrors array.
  FOR i=0,(n_elements(*(temp_arrays))-1) DO BEGIN
     ptr_free, (*(temp_arrays))[i]
  ENDFOR
  ptr_free, temp_arrays
  FOR i=0,n_elements(*commie_dataset_struct.dummy_relerror_arrays)-1 DO BEGIN
     IF ptr_valid((*commie_dataset_struct.dummy_relerror_arrays)[i]) THEN $
        ptr_free,(*commie_dataset_struct.dummy_relerror_arrays)[i]
  ENDFOR
  ptr_free, commie_dataset_struct.dummy_relerror_arrays
  commie_dataset_struct.dummy_relerror_arrays = ptr_new(temp_relerrors)

  ;; Give us an update b/c this may take a while
  status_string = 'Merged by exposure time.'
  commie_messages,status=status_string

  ;; let the user look at the generated arrays if the /verbose keyword
  ;; is specified
  IF *commie_script_struct.verbose THEN BEGIN
     commie_verbose_output, commie_dataset_struct, commie_script_struct, $
                            /merge_by_exp
  ENDIF ;; verbose
END
