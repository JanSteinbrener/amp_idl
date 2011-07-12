;; this routine takes the arrays from dummy_arrays and normalizes them
;; wrt the beam current. The result is in turn stored again in dummy_arrays
PRO commie_normalize_wrt_current, commie_dataset_struct, commie_script_struct

  compile_opt idl2, hidden

  these_indices = $
     *(commie_dataset_struct.dummy_indices)
  FOR i=0,(n_elements(these_indices)-1) DO BEGIN
     if PTR_VALID(commie_script_struct.ring_amps) then begin
        ring_amps = *commie_script_struct.ring_amps
     endif else begin
        read_dt,(*(commie_dataset_struct.filenames))[these_indices[i]], $
                this_dt_par, /header_only
        ring_amps = this_dt_par.ring_amps
        if ring_amps eq 0 then begin
           string = STRJOIN(['WARNING: no ring current defined! Assuming 0.4', $
                             ' Amps. Use ring_amps to define otherwise!'])
           commie_messages,status=string
           ring_amps = 0.4
        endif
     endelse
     *((*(commie_dataset_struct.dummy_arrays))[i]) /= ring_amps
  ENDFOR

  ;; Give us an update b/c this may take a while
  status_string = $
     'Normalized wrt beam current.'
  commie_messages,status=status_string
END
