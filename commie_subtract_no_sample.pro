PRO commie_subtract_no_sample, commie_dataset_struct, commie_script_struct

  compile_opt idl2, hidden

  these_indices = *(commie_dataset_struct.dummy_indices)
  ;; First get all the dummy_arrays into temp_arrays. Then re-allocate
  ;; memory to fit for the reduced size of datafiles after they have been
  ;; subtracted. Note that we need to hand over every individual array
  temp_arrays = $
     ptr_new(ptrarr(n_elements(*(commie_dataset_struct.dummy_arrays))))
  FOR i=0,(n_elements(*(commie_dataset_struct.dummy_arrays))-1) DO BEGIN
     (*temp_arrays)[i]=ptr_new(*((*(commie_dataset_struct.dummy_arrays))[i]))
  ENDFOR
  ;; now de-allocate the dummy_arrays and re-allocate them. Need to know 
  ;; how many arrays there will be - this will be determined in the first
  ;; for loop. Define the variable count for that
  FOR i=0,(n_elements(*(commie_dataset_struct.dummy_arrays))-1) DO BEGIN
     ptr_free, (*(commie_dataset_struct.dummy_arrays))[i]
  ENDFOR
  
  ;; this will hold the reduced set of indices.
  temp_indices = 0

  ;; array counter  
  n_arrays = 0

  ;; now let's see how many arrays we will have in the end. Here we assume,
  ;; that there are as many sample as there are no_sample files
  FOR i=0,(n_elements(*(commie_dataset_struct.exp_seconds))-1) DO BEGIN
     sample_indices = $
        Where((((*commie_dataset_struct.are_samples)[these_indices]) EQ 1) AND $
              (((*commie_dataset_struct.exp_indices)[these_indices]) EQ i), $
              count)
     IF count NE 0 THEN BEGIN
        n_arrays += count
        ;; store all indices that apply here
        temp_indices = [temp_indices,these_indices[sample_indices]]
     ENDIF
  ENDFOR
  ;; now collapse dummy_indices
  ptr_free, commie_dataset_struct.dummy_indices
  commie_dataset_struct.dummy_indices = ptr_new(temp_indices[1:*])
  
  ;; now we can allocate memory for the new dummy_arrays
  *(commie_dataset_struct.dummy_arrays) = ptrarr(n_arrays)
  ;; also allocate memory for the reduced set of relerror_arrays
  temp_relerrors = ptrarr(n_arrays)

  ;; Set up the loop through all exposure times, figure out what is sample
  ;; and what is no_sample and then subtract no_sample from sample.
  ;; Note: if the files have already been normalized wrt exposure time, 
  ;; this should still work. BUT: careful, not every positon has every
  ;; exposure time. So have an own counter for dummy_arrays.
  m = 0
  svec = [commie_dataset_struct.nx,commie_dataset_struct.ny]
  FOR i=0,(n_elements(*(commie_dataset_struct.exp_seconds))-1) DO BEGIN
     sample_indices = $
        Where(((*(commie_dataset_struct.are_samples))[these_indices] EQ 1) AND $
              ((*(commie_dataset_struct.exp_indices))[these_indices]) EQ i, $
              count)
     no_sample_indices = $
        Where(((*(commie_dataset_struct.are_samples))[these_indices] EQ 0) AND $
              ((*(commie_dataset_struct.exp_indices))[these_indices]) EQ i)
     IF count NE 0 THEN BEGIN
        ;; assuming there are as many samples as there are no_samples. If 
        ;; commie_merge_by_exp was run before, there should be only one
        ;; sample/no_sample pair per exposure time.
        FOR j=0,(count -1) DO BEGIN
           this_sample = *((*temp_arrays)[sample_indices[j]])
           this_no_sample = *((*temp_arrays)[no_sample_indices[j]])
           these_relerrs = $
         [ptr_new(*((*commie_dataset_struct.dummy_relerror_arrays)[sample_indices[j]])),$
        ptr_new(*((*commie_dataset_struct.dummy_relerror_arrays)[no_sample_indices[j]]))]
           def_is = Where(this_sample,def_count)
           IF def_count NE 0 THEN $
              ;; this will leave some pixels negative. Set to zero
              ;; in the end with commie_zero_negatives.
              this_sample[def_is] -= this_no_sample[def_is]
           
           (*(commie_dataset_struct.dummy_arrays))[count*m+j] =  $
              ptr_new(this_sample)
           temp_relerrors[count*m+j]= $
              ptr_new(commie_propagate_errors(these_relerrs))
           ;; free the relerrs
           ptr_free,these_relerrs[0],these_relerrs[1]
        ENDFOR
        m += 1
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
  status_string = 'Subtracted no_sample.'
  commie_messages,status=status_string

  ;; If the user wishes more output, only display the first
  ;; file each
  IF (*commie_script_struct.verbose) THEN BEGIN
     commie_verbose_output, commie_dataset_struct, commie_script_struct, $
                            /subtract_no_sample
  ENDIF ;; verbose
END
