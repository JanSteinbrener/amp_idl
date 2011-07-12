FUNCTION commie_compute_normalization, these_arrays, these_relerrs, $
                                       these_thresh=these_thresh
  
  compile_opt idl2,hidden
  
  ;; We only need to normalize if we have more than one array of
  ;; course. 
  n_arrays = N_ELEMENTS(these_arrays)
  IF n_arrays GT 1 THEN BEGIN
     ;; first find the array that produces most overlap with all other
     ;; arrays. This will be the reference.
     def_counts = LONARR(n_arrays,n_arrays)
     def_indices = PTRARR(n_arrays,n_arrays)
     FOR i=0,n_arrays-1 DO BEGIN
        ;; this will compute the normalization with respect to the first
        ;; array.
        reference = *(these_arrays[i])
        ref_errors = *(these_relerrs[i])*reference
        ref_thresh = 0
        IF Keyword_Set(these_thresh) THEN $
           ref_thresh = these_thresh[i]
     
        ;; now loop through the rest and determine the common defined
        ;; indices in all images. 
        dig=0
        FOR j=0,n_arrays-1 DO BEGIN
           IF j NE i THEN BEGIN
              this_array = *(these_arrays[j])
              this_errors = this_array*(*(these_relerrs[j]))
              
              ;; Now determine indices that are defined in both reference
              ;; and this_array
              this_thresh = 0
              IF Keyword_Set(these_thresh) THEN $
                 this_thresh = these_thresh[j]
              ;; need to also exclude pixels where ref_error zero because we
              ;; will divide by abs_errors further down
              these_inds = Where(((reference GT ref_thresh) AND $
                                  (this_array GT this_thresh) AND $
                                  (this_errors)), def_count)
              these_is_arr = dblarr(size(reference,/dimensions))
              def_counts[i,j] = def_count
              def_indices[i,j] = PTR_NEW(these_inds)
              IF def_count NE 0 THEN $
                 these_is_arr[these_inds] = 1
              IF dig THEN BEGIN
                 csection_inds = Where(these_is_arr AND all_is_arr,cs_count)
                 these_is_arr *= 0
                 IF cs_count NE 0 THEN $
                    these_is_arr[csection_inds] = 1
              ENDIF
              all_is_arr = these_is_arr
              dig =1
           ENDIF
        ENDFOR
        IF i GT 0 THEN BEGIN
           dummy = Where(all_is_arr,new_count)
           IF new_count GT count THEN $
              final_is_arr = all_is_arr
        ENDIF ELSE BEGIN
           final_is_arr = all_is_arr
        ENDELSE
        dummy = Where(final_is_arr, count)
     ENDFOR
     ;; discard redundant half
     for i=0, n_arrays-1 do begin
        for j=0,n_arrays-1 do begin
           if j gt i then $
              def_counts[i,j] = 0
        endfor
     endfor
     print,def_counts
     ;; Now loop through it again and calculate normalization wrt indices
     ;; in all_is_arr
     norm_inds = Where(final_is_arr,norm_count)

                                ;array=bytarr(1028,1028)
                                ;array[norm_inds] = 255
                                ;js_display_image,array
     IF norm_count LT 100 THEN BEGIN
        ;; instead of normalizing to one reference array, normalize
        ;; in pairs. First we normalize the pair of arrays with
        ;; maximum number of commonly defined pixels. Then we look for
        ;; the next array that has the next highest number of commonly
        ;; defined pixels with one of the arrays we just normalized.
        PRINT, 'COMMIE_COMPUTE_NORMALIZATION: Too few common pixels to '+$
               'normalize all arrays. Normalizing in pairs instead.'
        
        sort_inds = ARRAY_INDICES(def_counts,REVERSE(SORT(def_counts)))
        
        ;; loop through the sort indices and determine order of
        ;; columns to normalize
        norm_order = sort_inds[*,0]
        for i=1, N_ELEMENTS(sort_inds[0,*])-1 do begin
           used_inds = norm_order[UNIQ(norm_order, SORT(norm_order))]
           counter = 0
           this_used_i = -1
           for j=0, N_ELEMENTS(used_inds)-1 do begin
              ;; search each column for new index
              these_inds = WHERE(sort_inds[*,i] eq used_inds[j], n_inds)
              if n_inds ne 0 then $
                 this_used_i = [this_used_i,used_inds[j]]
              counter += n_inds
           endfor

           ;; if and only if one index has been used before then we
           ;; have the next arrays to normalize. If 0 or two
           ;; indices have been used before, we continue the search
           if counter eq 1 then begin
              ;; make sure reference array is in column 0!
              other_i = WHERE(sort_inds[*,i] ne this_used_i[1], $
                              complement=this_i)
              these_inds = [(sort_inds[*,i])[this_i],(sort_inds[*,i])[other_i]]
              norm_order = [[norm_order],[these_inds]]
           endif
        endfor
        print, norm_order
        ;; now loop through norm_order and normalize the pairs
        for i=0,N_ELEMENTS(norm_order[0,*])-1 do begin
           ;; we only need to do this of course if the two indices are
           ;; not the same
           if norm_order[0,i] ne norm_order[1,i] then begin
              reference = *(these_arrays[norm_order[0,i]])
              ref_errors = *(these_relerrs[norm_order[0,i]])*reference
              ref_thresh = 0
              IF Keyword_Set(these_thresh) THEN $
                 ref_thresh = these_thresh[norm_order[0,i]]
              
              this_array = *(these_arrays[norm_order[1,i]])
              this_errors = this_array*(*(these_relerrs[norm_order[1,i]]))
              
              ;; we can redefine norm_inds
              norm_inds = *(def_indices[norm_order[0,i],norm_order[1,i]])
              ;; make sure that we have overlap at all
              if norm_inds[0] eq -1 then begin 
                 PRINT, 'COMMIE_COMPUTE_NORMALIZATION: No common pixels '+ $
                        'for normalization of pair ['+ $
                        STRTRIM(norm_order[0,i],2)+','+ $
                        STRTRIM(norm_order[1,i],2)+ $
                        ']. Check verbose output!!'
              endif else begin
                 norm_count = N_ELEMENTS(norm_inds)
                 
                 ;; calculate absolute error after Stefano
                 abs_errs = this_errors[norm_inds]^2 + ref_errors[norm_inds]^2
                 
                 ;; now go through all indices and determine the
                 ;; normalization constant
                 this_num = Double(0)
                 this_denom = Double(0)
                 FOR j=long(0),norm_count-1 DO BEGIN 
                    this_num += Abs(this_array[norm_inds[j]])* $
                                Abs(reference[norm_inds[j]])/abs_errs[j]
                    this_denom += this_array[norm_inds[j]]^2/abs_errs[j]
                 ENDFOR
                 this_c = this_num/this_denom
                 ;; now normalize this_array using this_c
                 *(these_arrays[norm_order[1,i]]) *= this_c 
                 print,this_c
              endelse
           endif
        endfor
     ENDIF ELSE BEGIN
        FOR i=1,n_arrays-1 DO BEGIN
           this_array = *(these_arrays[i])
           this_errors = this_array*(*(these_relerrs[i]))
           
           ;; calculate absolute error after Stefano
           abs_errs = this_errors[norm_inds]^2 + ref_errors[norm_inds]^2
           dummy = Where(~Finite(this_array),arr_nan)
           dummy = Where(~Finite(abs_errs),abs_nan)
           ;; now go through all indices and determine the
           ;; normalization constant
           this_num = Double(0)
           this_denom = Double(0)
           FOR j=long(0),norm_count-1 DO BEGIN 
              this_num += Abs(this_array[norm_inds[j]])* $
                          Abs(reference[norm_inds[j]])/abs_errs[j]
              this_denom += this_array[norm_inds[j]]^2/abs_errs[j]
           ENDFOR
           this_c = this_num/this_denom
           ;; now normalize this_array using this_c
           *(these_arrays[i]) *= this_c
        ENDFOR 
     ENDELSE

     ;; don't forget to free the pointers
     for i=0, N_ELEMENTS(def_indices)-1 do $
        PTR_FREE, def_indices[i]
     
  ENDIF
  RETURN, these_arrays
END
