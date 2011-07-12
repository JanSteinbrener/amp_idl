FUNCTION commie_compute_shifts, these_arrays, reference=reference, $
                                no_smooth_ref=no_smooth_ref, $
                                subtract_bs=subtract_bs

  compile_opt idl2,hidden

  dims = size(*(these_arrays[0]),/dimensions)
  
  IF Keyword_Set(reference) THEN BEGIN
     ref_i = reference
  ENDIF ELSE BEGIN
     ref_i = 0
  ENDELSE

  if not KEYWORD_SET(subtract_bs) then begin
     subtract_bs = INTARR(N_ELEMENTS(these_arrays))
     subtract_bs += 1
  endif

  print,ref_i
  ref_array = *(these_arrays[ref_i])
  IF NOT KEYWORD_SET(no_smooth_ref) THEN $
     ref_array = SMOOTH(ref_array,10)

  ref_array = alog10(ref_array+0.001)
  ;; take a fairly large edgegauss sigma, since we are only interested
  ;; in regions around the beamstop
  edgegauss_sigma = 20.
  edgegauss, ref_array, edgegauss_sigma, dc, /zero_edge
  
  ref_array = Sobel(ref_array)
 
  max = max(ref_array)
  these_inds = Where(ref_array LT (max/6),count)
  IF count NE 0 THEN $
     ref_array[these_inds] = 0
 
  ref_fft = shift(ref_array,dims[0]/2,dims[1]/2)
  ref_fft = fft(ref_fft,-1,/overwrite)
  ref_fft = shift(ref_fft,dims[0]/2,dims[1]/2)
  ref_fft = Conj(Temporary(ref_fft))

  ;; this will hold the shifts
  these_shifts = Intarr(2,n_elements(these_arrays))
  
  FOR i=0,n_elements(these_arrays)-1 DO BEGIN
     IF i NE ref_i THEN BEGIN
        if subtract_bs[i] eq 1 then begin
           this_array = *(these_arrays[i])
           this_array = SMOOTH(this_array,8)
           
           this_array = alog10(this_array+0.001)
           ;window,/free,xsize=600,ysize=600
           ;Tvscl, Congrid(this_array,600,600)
           
           edgegauss, this_array, edgegauss_sigma, dc, /zero_edge
           
           this_array = Sobel(this_array)
           
           max = max(this_array)
           these_inds = Where(this_array LT (max/6),count)
           IF count NE 0 THEN $
              this_array[these_inds] = 0
           
           this_fft = shift(this_array,dims[0]/2,dims[1]/2)
           this_fft = fft(this_fft,-1,/overwrite)
           this_fft = shift(this_fft,dims[0]/2,dims[1]/2)

           this_fft = temporary(this_fft)*ref_fft
           
           this_fft = shift(temporary(this_fft),dims[0]/2,dims[1]/2)
           this_fft = fft(this_fft,1,/overwrite)
           this_fft = shift(temporary(this_fft),dims[0]/2,dims[1]/2)
           this_fft = abs(temporary(this_fft))
           
           sort_inds = Sort(this_fft)
           this_ind = n_elements(sort_inds)-1
           
           max_i = sort_inds[this_ind]
           
           this_shift = Array_Indices(dims,max_i,/dimensions)
           this_shift = this_shift - dims/2
           print, this_shift
        
        ;; we will move in y at least several pixels or it is the same
        ;; position. 
        ;WHILE Floor(Abs(this_shift[1])) LT 5 DO BEGIN
        ;   this_ind -= 1
        ;   max_i = sort_inds[this_ind]
        ;   
        ;   this_shift = array_indices(dims,max_i,/dimensions)
        ;   this_shift = - this_shift + dims/2
        ;ENDWHILE        
           these_shifts[0,i] = this_shift[0]
           these_shifts[1,i] = this_shift[1]
        endif else begin
           ;; indicate the lack of beamstop with an obvious value
           these_shifts[0,i] = -9999
           these_shifts[1,i] = -9999
        endelse
     ENDIF
  ENDFOR
  
  ;; now rearrange to have shifts wrt first position
  this_xoffset = these_shifts[0,0]
  this_yoffset = these_shifts[1,0]
  for i=0,N_ELEMENTS(these_shifts[0,*])-1 do begin
     if these_shifts[0,i] ne -9999 then begin
        these_shifts[0,i] -= this_xoffset
        these_shifts[1,*] -= this_yoffset
     endif
  endfor
  RETURN, these_shifts
END
