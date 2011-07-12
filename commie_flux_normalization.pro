PRO commie_flux_normalization, commie_dataset_struct, commie_script_struct, $
                               merge_by_exp=merge_by_exp, $
                               merge_dummy_arrays=merge_dummy_arrays, $
                               assemble_merged_pos=assemble_merged_pos
  
  compile_opt idl2, hidden
  
  ;; We want to calculate the normalization by looking at pixels
  ;; that are defined in both images, i.e. non-saturated, above
  ;; threshold. We normalize all images to the first in dummy_arrays

  ;; First distinguish between assemble_merged_pos and rest
  IF Keyword_Set(assemble_merged_pos) THEN BEGIN
     ;; prepare the arrays to hand over
     these_arrays = *commie_dataset_struct.pos_merged_samples
     ptr_free,commie_dataset_struct.pos_merged_samples
     
     commie_dataset_struct.pos_merged_samples = $
        ptr_new(commie_compute_normalization(these_arrays, $
                                      *commie_dataset_struct.relerror_samples))
     ;; now same for no_samples if applicable
     IF ptr_valid((*commie_dataset_struct.pos_merged_nosamples)[0]) THEN BEGIN
       ;; prepare the arrays to hand over
        these_arrays = *commie_dataset_struct.pos_merged_nosamples
        ptr_free,commie_dataset_struct.pos_merged_nosamples
     
        commie_dataset_struct.pos_merged_nosamples = $
           ptr_new(commie_compute_normalization(these_arrays, $
                                    *commie_dataset_struct.relerror_no_samples))
     ENDIF
  ENDIF ELSE IF Keyword_Set(merge_by_exp) THEN BEGIN
     ;; this is basically the same as for merge_dummy_arrays except
     ;; for the fact that we normalize only within a certain exposure
     ;; time. 
     dummy_inds = *commie_dataset_struct.dummy_indices
     ;; Need to do separately for samples, no_sample if necessary
     FOR i=0,n_elements(*commie_dataset_struct.exp_seconds)-1 DO BEGIN
        these_s = $
           Where(((*commie_dataset_struct.are_samples)[dummy_inds] EQ 1) $
                 AND ((*commie_dataset_struct.exp_indices)[dummy_inds] EQ i), $
                 exp_count)
        IF exp_count NE 0 THEN BEGIN
           these_thresh = fltarr(exp_count)
           these_arrays = ptrarr(exp_count)
           ;; now hand over the arrays and determine thresholds
           FOR j=0, exp_count-1 DO BEGIN  
              these_arrays[j] = $
                 ptr_new(*((*commie_dataset_struct.dummy_arrays)[these_s[j]]))
              ptr_free, (*commie_dataset_struct.dummy_arrays)[these_s[j]]
              ;; Saturation has been removed, so determine threshold -
              ;; samples first!
              adc_slow = $
                 (*commie_dataset_struct.adc_are_slow)[dummy_inds[these_s[j]]]
              this_exp = (*commie_dataset_struct.exp_seconds)[i]
              this_slope = (*commie_dataset_struct.dk_sigma_slope)[adc_slow]
              this_pedestal = $
                 (*commie_dataset_struct.dk_sigma_pedestal)[adc_slow] 
              these_thresh[j] = (*commie_script_struct.thresh_data_factor) $
                                *5*(this_exp*this_slope + this_pedestal)
           ENDFOR
           these_arrays = $
              commie_compute_normalization(these_arrays, $
                      (*commie_dataset_struct.dummy_relerror_arrays)[these_s], $
                                          these_thresh=these_thresh)
           FOR j=0,exp_count-1 DO BEGIN
              (*commie_dataset_struct.dummy_arrays)[these_s[j]] = $
                 ptr_new(*(these_arrays[j]))
              ptr_free,these_arrays[j]
           ENDFOR
        ENDIF
        ;; now the same for the no_samples
        these_ns = $
           Where(((*commie_dataset_struct.are_samples)[dummy_inds] EQ 0) $
                 AND ((*commie_dataset_struct.exp_indices)[dummy_inds] EQ $
                      i), exp_count)
        IF exp_count NE 0 THEN BEGIN
           these_thresh = fltarr(exp_count)
           these_arrays = ptrarr(exp_count)
           ;; now hand over the arrays and determine thresholds
           FOR j=0, exp_count-1 DO BEGIN  
              these_arrays[j] = $
                 ptr_new(*((*commie_dataset_struct.dummy_arrays)[these_ns[j]]))
              ptr_free, (*commie_dataset_struct.dummy_arrays)[these_ns[j]]
              ;; Saturation has been removed, so determine threshold -
              adc_slow = $
                 (*commie_dataset_struct.adc_are_slow)[dummy_inds[these_ns[j]]]
              this_exp = (*commie_dataset_struct.exp_seconds)[i]
              this_slope = (*commie_dataset_struct.dk_sigma_slope)[adc_slow]
              this_pedestal = $
                 (*commie_dataset_struct.dk_sigma_pedestal)[adc_slow] 
              these_thresh[j] = (*commie_script_struct.thresh_data_factor) $
                                *5*(this_exp*this_slope + this_pedestal)
           ENDFOR
           these_arrays = $
              commie_compute_normalization(these_arrays, $
                     (*commie_dataset_struct.dummy_relerror_arrays)[these_ns], $
                                          these_thresh=these_thresh)
           FOR j=0,exp_count-1 DO BEGIN
              (*commie_dataset_struct.dummy_arrays)[these_ns[j]] = $
                 ptr_new(*(these_arrays[j]))
              ptr_free,these_arrays[j]
           ENDFOR
        ENDIF
     ENDFOR
  ENDIF ELSE IF Keyword_Set(merge_dummy_arrays) THEN BEGIN
     dummy_inds = *commie_dataset_struct.dummy_indices
     ;; Need to do separately for samples, no_sample if necessary
     samples = Where((*commie_dataset_struct.are_samples)[dummy_inds] EQ 1, $
                     scount)
     nsamples = Where((*commie_dataset_struct.are_samples)[dummy_inds] EQ 0, $
                      nscount)
     ;; Saturation has been removed, data has been thresholded -
     ;; samples first!
     these_arrays = ptrarr(scount)
    
     ;; now loop through the rest and determine the common defined
     ;; images. 
     FOR i=0,scount-1 DO BEGIN
        these_arrays[i] = $
           ptr_new(*((*commie_dataset_struct.dummy_arrays)[samples[i]]))
        ptr_free,(*commie_dataset_struct.dummy_arrays)[samples[i]]
     ENDFOR
     these_arrays = $
        commie_compute_normalization(these_arrays, $
                        (*commie_dataset_struct.dummy_relerror_arrays)[samples])
     FOR i=0,scount-1 DO BEGIN
        (*commie_dataset_struct.dummy_arrays)[samples[i]] = $
                 ptr_new(*(these_arrays[i]))
        ptr_free,these_arrays[i]
     ENDFOR
     ;; now same for no_samples
     IF nscount NE 0 THEN BEGIN
        ;; Saturation has been removed, data has been thresholded -
        ;; samples first!
        these_arrays = ptrarr(nscount)
        
        ;; now loop through the rest and determine the common defined
        ;; images. 
        FOR i=0,nscount-1 DO BEGIN
           these_arrays[i] = $
              ptr_new(*((*commie_dataset_struct.dummy_arrays)[nsamples[i]]))
           ptr_free,(*commie_dataset_struct.dummy_arrays)[nsamples[i]]
        ENDFOR
        these_arrays = $
           commie_compute_normalization(these_arrays, $
                       (*commie_dataset_struct.dummy_relerror_arrays)[nsamples])
        FOR i=0,nscount-1 DO BEGIN
           (*commie_dataset_struct.dummy_arrays)[nsamples[i]] = $
              ptr_new(*(these_arrays[i]))
           ptr_free,these_arrays[i]
        ENDFOR
     ENDIF
  ENDIF
END
