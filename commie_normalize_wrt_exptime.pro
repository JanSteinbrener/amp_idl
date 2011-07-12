;; This routine will normalize everything to 1 sec exposure time.
PRO commie_normalize_wrt_exptime, commie_dataset_struct
  
  compile_opt idl2, hidden

  FOR i=0,(n_elements(*(commie_dataset_struct.dummy_arrays))-1) DO BEGIN
     this_index = (*commie_dataset_struct.dummy_indices)[i]
     this_exp_index = (*commie_dataset_struct.exp_indices)[this_index]
     this_exp = (*commie_dataset_struct.exp_seconds)[this_exp_index]
     *((*commie_dataset_struct.dummy_arrays)[i]) *= 1/this_exp
  ENDFOR

  ;; Give us an update b/c this may take a while
  status_string = 'Normalized wrt exposure time.'
  commie_messages,status=status_string
END
