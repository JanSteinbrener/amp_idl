;; This routine will subtract dark current and pedestal from all dummy_arrays.
;; The dark current is determined by a routine called dk_classify. 

PRO commie_subtract_dks, commie_dataset_struct, commie_script_struct
  
  compile_opt idl2, hidden

  exp_seconds = float((*commie_dataset_struct.exp_seconds))
  temp_indices = *(commie_dataset_struct.dummy_indices)

  ;; see if dk_by_pix was invoked. If there are some exposure times
  ;; left, then let the regular method take care of those after a
  ;; warning message.
  IF commie_dataset_struct.dk_by_pix THEN BEGIN
     dk_exp_seconds = float(*((*commie_dataset_struct.dk_exp_seconds)[0]))

     ;; first check if all necessary dk_avg_arrays are present
     IF ptr_valid(commie_dataset_struct.dk_avg_arrays) THEN BEGIN
        dummy = where(exp_seconds EQ dk_exp_seconds, count)
        IF count NE n_elements(exp_seconds) THEN BEGIN
           commie_messages,commie_dataset_struct, commie_script_struct, $
                           /subtract_dks
           ;; if user didn't bail out than we need to find the
           ;; common exposure times and then hand the other ones over
           ;; to the standard subtraction down below.
           leftovers = 0
           FOR i=0,n_elements(exp_seconds)-1 DO BEGIN
              this_dk_index = Where(dk_exp_seconds EQ exp_seconds[i], count)
              ;; only take first element 
              this_dk_index = this_dk_index[0]
              IF count NE 0 THEN BEGIN
                 these_dks = (*commie_dataset_struct.dk_avg_arrays)
                 ;; subtract the dk_avg_arrays from those
                 these_inds = $
                    Where((*commie_dataset_struct.exp_indices)[temp_indices] $
                          EQ i, sub_count)
                 IF sub_count NE 0 THEN BEGIN
                    FOR j=0,sub_count-1 DO BEGIN
                       this_dummy_i = temp_indices[these_inds[j]]
                       ;; determine adc_speed
                       adc_slow = $
                          (*commie_dataset_struct.adc_are_slow)[this_dummy_i]
                       *((*commie_dataset_struct.dummy_arrays)[these_inds[j]]) $
                          -= *((*these_dks[adc_slow])[this_dk_index]) 
                    ENDFOR
                 ENDIF
              ENDIF ELSE BEGIN
                 ;; add the index to the list of indices being passed
                 ;; on to the standard subtraction below
                 leftovers = [leftovers,i]
              ENDELSE
           ENDFOR
           exp_seconds = exp_seconds[leftovers[1:*]]
        ENDIF ELSE BEGIN
           ;; subtract dk_avg_arrays from each file. This_dk_index has
           ;; to exist because we checked it before
           FOR i=0,n_elements(exp_seconds)-1 DO BEGIN
              this_dk_index = Where(dk_exp_seconds EQ exp_seconds[i])
              ;; only take first element if
              this_dk_index = this_dk_index[0]
              these_dks = (*commie_dataset_struct.dk_avg_arrays)
              ;; subtract the dk_avg_arrays from those
              these_inds = $
                 Where((*commie_dataset_struct.exp_indices)[temp_indices] EQ i,$
                       sub_count)
              IF sub_count NE 0 THEN BEGIN
                 FOR j=0,sub_count-1 DO BEGIN
                    this_dummy_i = temp_indices[these_inds[j]]
                    ;; determine adc_speed
                    adc_slow = $
                       (*commie_dataset_struct.adc_are_slow)[this_dummy_i]
                    *((*commie_dataset_struct.dummy_arrays)[these_inds[j]]) $
                       -= *((*these_dks[adc_slow])[this_dk_index]) 
                 ENDFOR
              ENDIF
           ENDFOR
           ;; this will de-facto undefine exp_seconds
           temp=temporary(exp_seconds)
        ENDELSE
     ENDIF ELSE BEGIN
        commie_messages,commie_dataset_struct, commie_script_struct, $
                        /subtract_dks
        ;; If user wishes to continue from here, then we automatically
        ;; fall back to old dk subtraction
     ENDELSE
  ENDIF

  ;; use this to check if there are exposure times left after
  ;; dk_by_pix. If it has not been invoked than exp_seconds is just
  ;; commie_dataset_struct.exp_seconds
  IF n_elements(exp_seconds) NE 0 THEN BEGIN
     ;; now subtract the dk + pedestal based on exposure time
     FOR i=0,n_elements(exp_seconds)-1 DO BEGIN
        these_indices = $
           Where((*(commie_dataset_struct.exp_indices))[temp_indices] EQ i, $
                 count)
        this_exp = exp_seconds[i]
        IF count NE 0 THEN BEGIN
           FOR j=0,count-1 DO BEGIN
              ;; let's work with a local copy of the array to avoid cluttered 
              ;; syntax.
              this_array = $
                 *((*(commie_dataset_struct.dummy_arrays))[these_indices[j]])
              ;; now free the pointer
              ptr_free,(*(commie_dataset_struct.dummy_arrays))[these_indices[j]]
              ;; determine which adc speed we have to subtract
              this_dummy_index = temp_indices[these_indices[j]]
              IF ((*commie_dataset_struct.adc_are_slow)[this_dummy_index] $
                  EQ 1) THEN BEGIN
                 this_slope = (*commie_dataset_struct.dk_max_slope)[1]
                 this_pedestal = (*commie_dataset_struct.dk_max_pedestal)[1]
              ENDIF ELSE BEGIN
                 this_slope = (*commie_dataset_struct.dk_max_slope)[0]
                 this_pedestal = (*commie_dataset_struct.dk_max_pedestal)[0]
              ENDELSE
              this_dk = (this_exp*this_slope + this_pedestal)
              these_inds = Where(this_array GT 0, gt_count)
              IF gt_count NE 0 THEN $
                 this_array[these_inds] -= this_dk
              ;; now assign the pointer again to the dk-subtracted array
              (*(commie_dataset_struct.dummy_arrays))[these_indices[j]] = $
                 ptr_new(this_array)
           ENDFOR
        ENDIF
     ENDFOR
  ENDIF
  ;; Give us an update b/c this may take a while
  status_string = 'Subtracted dark current.'
  commie_messages,status=status_string

  ;; If the user wishes more output, only display the first
  ;; file each
  IF (*commie_script_struct.verbose) THEN BEGIN
     commie_verbose_output, commie_dataset_struct, commie_script_struct, $
                            /subtract_dks
  ENDIF ;; verbose
END
  
