;; this routine will threshold all data except for the longest
;; exposure and ALL no_sample files. The threshold is determined by
;; looking at the histogram of pixel values.
PRO commie_threshold_data, commie_dataset_struct, commie_script_struct

  compile_opt idl2, hidden

  these_indices = (*commie_dataset_struct.dummy_indices)
  svec = [commie_dataset_struct.nx,commie_dataset_struct.ny]
  no_samples = $
     Where((*(commie_dataset_struct.are_samples))[these_indices] EQ 0, ns_count)
  ;; in this case we want to 
  ;; i) threshold all no_samples and all but the longest exposure of
  ;;    the samples.
  ;; ii) adjust the threshold with the ring current normalization
  ;;     factor. 
  ;; iii) set all damaged pixels to zero - NO SCALING so far.
  IF ns_count NE 0 THEN BEGIN
     ;; threshold all data except the longest exposure times. Also set
     ;; all damaged pixels to 0.
     sam = Where((*commie_dataset_struct.are_samples) EQ 1, scount)
     nosam = Where((*commie_dataset_struct.are_samples) EQ 0, nscount)
     FOR i=0, n_elements(these_indices) -1 DO BEGIN
        this_exp_index = (*commie_dataset_struct.exp_indices)[these_indices[i]]
        this_exp = (*commie_dataset_struct.exp_seconds)[this_exp_index]
        ;; first set all damaged pixels to 0 except for the highest
        ;; exposure time. Don't scale this for now...
        IF (this_exp_index LT $
            n_elements(*commie_dataset_struct.exp_seconds)-1) THEN BEGIN
           damaged_pixels = $
              *((*commie_dataset_struct.damaged_pixels)[this_exp_index])
           ;; in case we don't have any!!
           IF damaged_pixels[0] NE -1 THEN $ 
              (*((*commie_dataset_struct.dummy_arrays)[i]))[damaged_pixels] = 0.
        ENDIF
        
        ;; compute the threshold based on fast or slow readout.
        IF ((*commie_dataset_struct.adc_are_slow)[these_indices[i]] $
            EQ 1) THEN BEGIN
           this_slope = (*commie_dataset_struct.dk_sigma_slope)[1]
           this_pedestal = (*commie_dataset_struct.dk_sigma_pedestal)[1]
        ENDIF ELSE BEGIN
           this_slope = (*commie_dataset_struct.dk_sigma_slope)[0]
           this_pedestal = (*commie_dataset_struct.dk_sigma_pedestal)[0]
        ENDELSE
        ;; threshold to 5 SDs above centroid.
        this_threshold = (*commie_script_struct.thresh_data_factor) $
                         *5*(this_exp*this_slope + this_pedestal)
        thresh_is = Where(*((*commie_dataset_struct.dummy_arrays)[i]) LT $
                          this_threshold, count)
        IF count NE 0 THEN BEGIN
           ;; make sure we don't threshold the highest exposure
           ;; for samples only.
           IF ((*(commie_dataset_struct.are_samples))[these_indices[i]] $ 
               EQ 1) THEN BEGIN
              IF (this_exp_index LT $
                  (n_elements(*commie_dataset_struct.exp_seconds)-1)) THEN BEGIN
                 (*((*commie_dataset_struct.dummy_arrays)[i]))[thresh_is] = $
                    0.
              ENDIF ELSE BEGIN
                 ;; for all other arrays set the negatives to zero
                 neg_indices = $
                    Where(*((*commie_dataset_struct.dummy_arrays)[i]) LT 0, $
                          neg_count)
                 IF neg_count NE 0 THEN $
                 (*((*commie_dataset_struct.dummy_arrays)[i]))[neg_indices] = 0.
              ENDELSE
           ENDIF ELSE BEGIN
              (*((*commie_dataset_struct.dummy_arrays)[i]))[thresh_is] = 0.
           ENDELSE
        ENDIF
     ENDFOR
     ;; now if only samples are left, then threshold all but the
     ;; longest exposure taking the ring current normalization into
     ;; account.    
  ENDIF ELSE BEGIN 
     ;; i) threshold all data except the longest exposure times.
     ;; ii) zero out negative pixels - 
     ;; iii) all damaged pixels to 0 - NO SCALING so far.
     sam = Where((*commie_dataset_struct.are_samples) EQ 1, scount)
     FOR i=0, n_elements(these_indices) -1 DO BEGIN
        this_exp_index = (*commie_dataset_struct.exp_indices)[these_indices[i]]
        this_exp = (*commie_dataset_struct.exp_seconds)[this_exp_index]
        ;; first set all damaged pixels to 0 except for the highest
        ;; exposure time. Don't scale this for now...
        IF (this_exp_index LT $
            n_elements(*commie_dataset_struct.exp_seconds)-1) THEN BEGIN
           damaged_pixels = $
              *((*commie_dataset_struct.damaged_pixels)[this_exp_index])
           ;; in case we don't have any!!
           IF damaged_pixels[0] NE -1 THEN $ 
              (*((*commie_dataset_struct.dummy_arrays)[i]))[damaged_pixels] = 0.
        ENDIF
        
        ;; compute the threshold based on fast or slow readout.
        IF ((*commie_dataset_struct.adc_are_slow)[these_indices[i]] $
            EQ 1) THEN BEGIN
           print,'slow'
           this_slope = (*commie_dataset_struct.dk_sigma_slope)[1]
           this_pedestal = (*commie_dataset_struct.dk_sigma_pedestal)[1]
           print, this_slope, this_pedestal
        ENDIF ELSE BEGIN
           print,'fast'
           this_slope = (*commie_dataset_struct.dk_sigma_slope)[0]
           this_pedestal = (*commie_dataset_struct.dk_sigma_pedestal)[0]
        ENDELSE
        ;; threshold to 5 SDs above centroid. NOTE this also
        ;; thresholds any negatives due to no_sample subtraction
        this_threshold = (*commie_script_struct.thresh_data_factor) $
                         *5*(this_exp*this_slope + this_pedestal)
        thresh_is = Where(*((*commie_dataset_struct.dummy_arrays)[i]) LT $
                          this_threshold, count)
        IF count NE 0 THEN BEGIN
           ;; make sure we don't threshold the highest exposure
           ;; for samples only.
           IF (this_exp_index LT $
               (n_elements(*commie_dataset_struct.exp_seconds)-1)) THEN BEGIN
              (*((*commie_dataset_struct.dummy_arrays)[i]))[thresh_is] = .0
           ENDIF ELSE BEGIN
              ;; for all other arrays set the negatives to zero
              neg_indices = $
                 Where(*((*commie_dataset_struct.dummy_arrays)[i]) LT 0, $
                       neg_count)
              IF neg_count NE 0 THEN $
                 (*((*commie_dataset_struct.dummy_arrays)[i]))[neg_indices] = 0.
           ENDELSE
        ENDIF
     ENDFOR
  ENDELSE

  ;; Give us an update b/c this may take a while
  status_string = 'Thresholded data.'
  commie_messages,status=status_string

  ;; let the user look at the generated arrays if the /verbose keyword
  ;; is specified
  IF *commie_script_struct.verbose THEN BEGIN
     commie_verbose_output, commie_dataset_struct, commie_script_struct, $
                            /threshold_data
  ENDIF ;; verbose
END
