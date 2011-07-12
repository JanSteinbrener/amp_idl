;; this routine rotates the arrays correctly and hands them back in
;; double precision
PRO commie_read_dt, commie_script_struct, commie_dataset_struct,filename, $
                    dt_par, this_array, $
                    header_only=header_only
  
  compile_opt idl2, hidden

  IF Keyword_Set(header_only) THEN BEGIN
     read_dt, filename, dt_par, /header_only
  ENDIF ELSE BEGIN
     read_dt, filename, dt_par, this_array
     IF *commie_dataset_struct.rotate GT 0 THEN $
        this_array = Rotate(this_array, *commie_dataset_struct.rotate)
     this_array = Double(this_array)

     ;; look at subscripts
     dims = SIZE(this_array,/dimensions)
     subset_inds = *(commie_script_struct.subset_indices)
     xstart = (subset_inds[0] eq -1) ? 0:subset_inds[0]
     xstop = subset_inds[1] eq -1 ? (dims[0]-1):subset_inds[1]
     ystart = (subset_inds[2] eq -1) ? 0:subset_inds[2]
     ystop = subset_inds[3] eq -1 ? (dims[1]-1):subset_inds[3]

     ;; make sure we don't exceed size of the array
     if xstop ge dims[0] then begin
        status_string = '[WARNING] COMMIE_READ_DT: Subset exceeds size of '+$
                        'array in X. Using all of it instead.'
        COMMIE_MESSAGES,status=status_string
        xstop = dims[0]-1
     endif
     if ystop ge dims[1] then begin
        status_string = '[WARNING] COMMIE_READ_DT: Subset exceeds size of '+$
                        'array in Y. Using all of it instead.'
        COMMIE_MESSAGES,status=status_string
        ystop = dims[1]-1
     endif
     this_array = this_array[xstart:xstop,ystart:ystop]
  ENDELSE
  
END
