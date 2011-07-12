FUNCTION commie_weighted_averages, these_arrays, these_relerrs, $
                                   no_threshold=no_threshold
  
  compile_opt idl2, hidden

  this_num = Dblarr(size(*(these_arrays[0]),/dimensions))
  this_denom = this_num
  weighted_avg_array = this_num
  this_relerr = this_num
  FOR i=0, n_elements(these_arrays)-1 DO BEGIN
     this_array = *(these_arrays[i])

;     if i eq 0 then begin
;        disp_arr = this_array
;        negs = where(disp_arr lt 0, neg_count)
;        if neg_count ne 0 then $
;           disp_arr[negs] = 0
;        js_display_image,alog10(disp_arr+0.001)
;     endif

     ;; for merge_by_exp we want all pixels to count towards average,
     ;; even if they are negative. Also need to take into account the
     ;; relerrors since we are dividing by them further down.
     IF Keyword_Set(no_threshold) THEN BEGIN
        def_is = Where(this_array AND *(these_relerrs[i]), def_count)
     ENDIF ELSE BEGIN
        def_is = Where((this_array GT 0.) AND *(these_relerrs[i]), def_count)
     ENDELSE
     dummy = Where(*(these_relerrs[i]) GT 1,ct)
     IF def_count NE 0 THEN BEGIN
        ;; compute absolute error arrays
        abs_errors = *(these_relerrs[i])*this_array
        ;; add to weighted average
        this_num[def_is] += $
           this_array[def_is]/((abs_errors[def_is])^2)
        this_denom[def_is] +=1/((abs_errors[def_is])^2)
        ;; add towards new relerror
        this_relerr[def_is]+= 1/(abs_errors[def_is])^2
     ENDIF
  ENDFOR
  ;; check for pixels that are 0 in denominator
  def_is = Where(this_denom, count)
  IF count NE 0 THEN $
     weighted_avg_array[def_is] = this_num[def_is]/this_denom[def_is]

  ;; and compute the new relerror
  def_rel_is = Where(this_relerr GT 0, count)
  IF count NE 0 THEN BEGIN
     ;; this is the absolute error
     this_relerr[def_rel_is] = 1/sqrt(this_relerr[def_rel_is])
     ;; convert to relative error by dividing with avg_array wherever
     ;; it is defined.
     this_relerr[def_is]=this_relerr[def_is]/weighted_avg_array[def_is]
  ENDIF
  ;; now free the these_relerrs and hand back the new array of
  ;; relerrs. 
  FOR i=0, n_elements(these_relerrs)-1 DO BEGIN
     ptr_free, these_relerrs[i]
  ENDFOR
  these_relerrs = this_relerr

  RETURN, weighted_avg_array
END
