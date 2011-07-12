PRO commie_find_shifts, commie_script_struct, commie_dataset_struct

  compile_opt idl2, hidden

  ;; sort out one array per position that is has longest exposure time
  ;; without being saturated
  these_arrays = ptrarr(n_elements(*commie_dataset_struct.position_names))
  
  FOR i=0, n_elements(*commie_dataset_struct.position_names)-1 DO BEGIN
     these_inds = Where(((*commie_dataset_struct.position_indices EQ i) AND $
                         (*commie_dataset_struct.are_samples EQ 1)),count)
     ;; now look for files that have high exposure times but are not
     ;; saturated. 
     IF count NE 0 THEN BEGIN
        inds = 0
        FOR j=0,n_elements(*commie_dataset_struct.exp_seconds)-1 DO BEGIN
           dummy_i = $
                 Where(((*commie_dataset_struct.exp_indices)[these_inds]) $
                       EQ j, exp_count)
           IF exp_count NE 0 THEN $
              inds = [inds, dummy_i[0]]
        ENDFOR
        inds = inds[1:*]
        ;; start with the high indices and check for saturation
        dummy_i = n_elements(inds) - 1
        this_i = these_inds[inds[dummy_i]]
        commie_read_dt, commie_script_struct, commie_dataset_struct, $
                        (*commie_dataset_struct.filenames)[this_i], $
                        this_par, this_arr
        sat_inds = $
           Where_Saturated(this_arr,sat_count, threshold = $
                           (*commie_script_struct.sat_threshold_high),$
                           low_threshold = $
                           (*commie_script_struct.sat_threshold_low))
        WHILE ((sat_count NE 0) AND (dummy_i GT 0)) DO BEGIN
           dummy_i -= 1
           this_i = these_inds[inds[dummy_i]]
           commie_read_dt,commie_script_struct,commie_dataset_struct, $
                          (*commie_dataset_struct.filenames)[this_i], $
                          this_par, this_arr
           sat_inds = $
              Where_Saturated(this_arr,sat_count, threshold = $
                              (*commie_script_struct.sat_threshold_high),$
                              low_threshold = $
                              (*commie_script_struct.sat_threshold_low))
        ENDWHILE
        ;; If there still is saturation then we will just give it a
        ;; try.
        these_arrays[i] = ptr_new(this_arr)
     ENDIF
  ENDFOR
  
  ;; prompt user for reference array for now
  answer = ''
  ref_i=0
  WHILE Strcmp(answer,'Yes') EQ 0 DO BEGIN
     window,0,xsize=600,ysize=600,title='Is this a good reference?'
     Tvscl,alog10(congrid(*(these_arrays[ref_i])+0.001,600,600))
     answer = Dialog_message('Is this a good reference array?', /Question)
     ref_i++
     IF strcmp(answer,'Yes') EQ 1 THEN $
        ref_i--
  ENDWHILE 
  wdelete,0

  ;; do we want to subtract the beamstop from all arrays
  subtract_bs = INTARR(N_ELEMENTS(these_arrays))
  answer = ''
  WINDOW,/free,xsize=600,ysize=600, $
         title='Include this position for BS subtraction?' 
  this_win = !D.WINDOW
  for i=0,N_ELEMENTS(these_arrays)-1 do begin
     TVSCL,ALOG10(CONGRID(*(these_arrays[i])+0.001,600,600))
     answer = DIALOG_MESSAGE('Include this position for BS subtraction?', $
                             /question)
     if STRCMP(answer,'Yes') eq 1 then begin
        subtract_bs[i] = 1 
     endif else begin
        subtract_bs[i] = 0
     endelse
  endfor
  WDELETE,this_win
  
  ;; compute the shifts
  these_shifts = commie_compute_shifts(these_arrays, reference=ref_i, $
                                       subtract_bs=subtract_bs)
  these_xshifts = these_shifts[0,*]
  these_yshifts = these_shifts[1,*]
  
  ;; now add up the arrays shifted and display them
  disp_arr = *(these_arrays[0])
  FOR i=1,n_elements(these_arrays)-1 DO BEGIN
     ;; only if the array has a beamstop
     if these_xshifts[i] ne -9999 then begin
        this_array = $
           shift(*(these_arrays[i]),-these_xshifts[i],-these_yshifts[i])
        disp_arr += this_array
     endif
  ENDFOR
  window,0,xsize=600,ysize=600,title='All positions aligned?'
  Tvscl,alog10(congrid(disp_arr+0.001,600,600))
  
  ;; confirm shifts by looking at some means of quality check
  answer = dialog_message('Do these arrays seem to be aligned?', $
                          /question)    
  ;; destroy the window
  Wdelete,0
  
  IF (Strcmp(answer,'No') EQ 1) THEN BEGIN
     ;; if not then manual align
     commie_manual_align, these_arrays, these_xshifts, these_yshifts, $
                          subtract_bs
  ENDIF ELSE BEGIN
     ;; now attempt to determine the beamstop center, by having a centered
     ;; test object (a square) and compute the distance to it
     nx = commie_dataset_struct.nx
     ny = commie_dataset_struct.ny
     test_object = Bytarr(nx,ny)
     test_object += 1
     ;; have reasonable size regardless of new or old camera chip
     side_length = nx/10
     test_object[(nx-side_length)/2:(nx+side_length)/2, $
                 (ny-side_length)/2:(ny+side_length)/2] = 0
     test_object = Rot(test_object,45)
     those_arrays = [ptr_new(test_object),ptr_new(*(these_arrays[0]))]
     ;; don't smooth the reference array since it is a test object
     those_shifts = commie_compute_shifts(those_arrays, /no_smooth_ref)
     ;; now calculate center
     bs_x = nx/2 + those_shifts[0,1]
     bs_y = ny/2 + those_shifts[1,1]

     ;; free those_arrays
     ptr_free,those_arrays[0],those_arrays[1]
     disp_arr_max = max(disp_arr)
     disp_arr[(bs_x-2):(bs_x+2),(bs_y-2):(bs_y+2)]=disp_arr_max
     window,0,xsize=600,ysize=600,title='BS center correct?'
     Tvscl,alog10(congrid(disp_arr+0.001,600,600))
     
     ;; confirm shifts by looking at some means of quality check
     answer = $
        dialog_message('Is this point within the head of the beamstop?', $
                       /question)
     IF (Strcmp(answer,'No') EQ 1) THEN BEGIN
        commie_messages, $
           status='Click on a point within the head of the beamstop!'
        cursor,bs_x, bs_y,/normal
        bs_x = Fix(nx*bs_x)
        bs_y = Fix(ny*bs_y)
     ENDIF

     wdelete,0
     bs_present = WHERE(these_xshifts ne -9999, bs_count, $
                        complement=no_bs_present, ncomplement=no_bs_count)

     if bs_count ne 0 then begin
        these_xshifts[bs_present] += bs_x
        these_yshifts[bs_present] += bs_y
     endif
     if no_bs_count ne 0 then begin
        these_xshifts[no_bs_present] = 0
        these_yshifts[no_bs_present] = 0
     endif
  ENDELSE
  
  ;; free the these_arrays
  FOR i=0, n_elements(these_arrays)-1 DO BEGIN
     IF ptr_valid(these_arrays[i]) THEN $
        ptr_free,these_arrays[i]
  ENDFOR
  
  ;; now store in dataset_struct
  commie_dataset_struct.bs_xs = ptr_new(these_xshifts)
  commie_dataset_struct.bs_ys = ptr_new(these_yshifts)
END
