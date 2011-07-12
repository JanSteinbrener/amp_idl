;; This routine will simply average over all dummy arrays. It will
;; however notice if we already subtracted the no_samples or not. 
PRO commie_merge_dummy_arrays, commie_dataset_struct, commie_script_struct
 
  compile_opt idl2, hidden

  ;; first normalize the dummy_arrays
  commie_flux_normalization, commie_dataset_struct, commie_script_struct, $
                             /merge_dummy_arrays

  ;; lets see if we already subtracted the no_samples
  these_indices = *(commie_dataset_struct.dummy_indices)
  no_samples = $
     Where((*(commie_dataset_struct.are_samples))[these_indices] EQ 0, ns_count)
  ;; in this case we want to merge sample and no_sample arrays separately
  IF ns_count NE 0 THEN BEGIN
     samples = $
        Where((*(commie_dataset_struct.are_samples))[these_indices] EQ 1, count)
     these_arrays = ptrarr(count)
     these_relerrs = ptrarr(count)
     both_relerrs = ptrarr(2)
     FOR i=0, count-1 DO BEGIN
        these_arrays[i]=$
           ptr_new(*((*(commie_dataset_struct.dummy_arrays))[samples[i]]))
        ptr_free, (*(commie_dataset_struct.dummy_arrays))[samples[i]]
        these_relerrs[i]=$
        ptr_new(*((*(commie_dataset_struct.dummy_relerror_arrays))[samples[i]]))
        ptr_free, (*(commie_dataset_struct.dummy_relerror_arrays))[samples[i]]
     ENDFOR

     this_sample = commie_weighted_averages(these_arrays,these_relerrs)
     both_relerrs[0] = ptr_new(these_relerrs)
     
     ;; free the arrays
     FOR i=0,n_elements(these_arrays)-1 DO BEGIN
        ptr_free,these_arrays[i]
     ENDFOR

     ;; now we can average over the temp_nosamples
     these_arrays = ptrarr(ns_count)
     these_relerrs = ptrarr(ns_count)
     FOR i=0, ns_count-1 DO BEGIN
        these_arrays[i]=$
           ptr_new(*((*(commie_dataset_struct.dummy_arrays))[no_samples[i]]))
        ptr_free, (*(commie_dataset_struct.dummy_arrays))[no_samples[i]]
        these_relerrs[i]=$
     ptr_new(*((*(commie_dataset_struct.dummy_relerror_arrays))[no_samples[i]]))
       ptr_free, (*(commie_dataset_struct.dummy_relerror_arrays))[no_samples[i]]
     ENDFOR
     
     this_nosample = commie_weighted_averages(these_arrays,these_relerrs)
     both_relerrs[1] = ptr_new(these_relerrs)
     ;; free the arrays
     FOR i=0,n_elements(these_arrays)-1 DO BEGIN
        ptr_free,these_arrays[i]
     ENDFOR

     ;; free the dummy_arrays and relerror arrays first
     ptr_free,commie_dataset_struct.dummy_arrays, $
              commie_dataset_struct.dummy_relerror_arrays
     ;; store the result in dummy_arrays. This is now two arrays,
     ;; first the sample then the nosample array for a given position
     commie_dataset_struct.dummy_arrays = ptr_new(ptrarr(2))
     (*commie_dataset_struct.dummy_arrays)[0] = ptr_new(this_sample)
     (*commie_dataset_struct.dummy_arrays)[1] = ptr_new(this_nosample)
     
     commie_dataset_struct.dummy_relerror_arrays = ptr_new(both_relerrs)
  ENDIF ELSE BEGIN
     ;; this will reduce the number of dummy_arrays to 1, so we first
     ;; need to store all dummy_arrays in temp_arrays and then free the 
     ;; dummy_arrays.
     count = n_elements(*(commie_dataset_struct.dummy_arrays))
     these_arrays = *commie_dataset_struct.dummy_arrays
     these_relerrs = *commie_dataset_struct.dummy_relerror_arrays
     ;; now de-allocate the dummy_arrays.
     ptr_free, commie_dataset_struct.dummy_arrays, $
               commie_dataset_struct.dummy_relerror_arrays
     
      ;; store the result in dummy_arrays. This is only one single array!
     commie_dataset_struct.dummy_arrays = $
        ptr_new(commie_weighted_averages(these_arrays,these_relerrs))
   
     commie_dataset_struct.dummy_relerror_arrays = ptr_new(these_relerrs)

     ;; free the arrays
     FOR i=0,n_elements(these_arrays)-1 DO BEGIN
        ptr_free,these_arrays[i]
     ENDFOR
  ENDELSE

  ;; let the user look at the generated arrays if the /verbose keyword
  ;; is specified
  IF *commie_script_struct.verbose THEN BEGIN
     commie_verbose_output, commie_dataset_struct, commie_script_struct, $
                             /merge_dummy_arrays
  ENDIF ;; verbose
END
