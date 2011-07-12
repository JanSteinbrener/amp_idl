;; this little routine will search the headers of all files in
;; filenames and tries to classify the motor positions used. It will
;; sort all unique BSY positions from low to high and associate them
;; with top_low, top_int, high, bs_moved, bot_int, bot_low
;; respectively. The other motor positions are then determined only
;; once per setting.
PRO commie_get_motor_positions, commie_dataset_struct, commie_script_struct

  compile_opt idl2, hidden

  ;; the files we want to get the info from
  these_files = Where(*commie_dataset_struct.are_samples EQ 1)
  filenames = $
     (*commie_dataset_struct.filenames)[these_files]

  ;; make a list of all motor positions
  bsy_positions = Fltarr(n_elements(filenames))
  bsx_positions = Fltarr(n_elements(filenames))
  dnx_positions = Fltarr(n_elements(filenames))
  dny_positions = Fltarr(n_elements(filenames))

  FOR i=0, n_elements(filenames)-1 DO BEGIN
     read_dt,filenames[i],this_dt_par,/header_only
     bsy_positions[i] = this_dt_par.bsystg_m
     bsx_positions[i] = this_dt_par.bsxstg_m
     if (this_dt_par.filever gt 5) then begin
        dnx_positions[i] = this_dt_par.up2xstg_m
     endif else begin
        dnx_positions[i] = this_dt_par.dnystg_m
     endelse
     
     if (this_dt_par.filever gt 5) then begin
        dny_positions[i] = this_dt_par.up2ystg_m
     endif else begin
        dny_positions[i] = this_dt_par.dnystg_m
     endelse
  ENDFOR

  ;; sort these in ascending order
  sort_indices = Uniq(bsy_positions,Sort(bsy_positions))
  bsy_positions=bsy_positions[sort_indices]
  bsx_positions=bsx_positions[sort_indices]
  dnx_positions=dnx_positions[sort_indices]
  dny_positions=dny_positions[sort_indices]
  
  ;; of those only keep these values that are more than the specified
  ;; tolerance apart
  same_inds = 0
  FOR i=0,n_elements(bsy_positions)-2 DO BEGIN
     FOR j=i+1,n_elements(bsy_positions)-1 DO BEGIN
        IF Abs(bsy_positions[i] - bsy_positions[j]) LE $
           *(commie_script_struct.tolerance_meters) THEN BEGIN
           same_inds = [same_inds, j]
        ENDIF ;; else it is greater than the tolerance
     ENDFOR
  ENDFOR

  IF n_elements(same_inds) GT 1 THEN BEGIN
     same_inds = same_inds[1:*]
     ;; some indices might be named numerous times
     same_inds = same_inds[Uniq(same_inds,Sort(same_inds))]
     all_inds = Intarr(N_ELEMENTS(bsy_positions))
     all_inds +=1
     all_inds[same_inds] = 0
     diff_inds = Where(all_inds)
     bsy_positions=bsy_positions[diff_inds]
     bsx_positions=bsx_positions[diff_inds]
     dnx_positions=dnx_positions[diff_inds]
     dny_positions=dny_positions[diff_inds]
  ENDIF

  ;; now create the position names 
  positions = strarr(n_elements(bsy_positions))
  FOR i=0, n_elements(bsy_positions) -1 DO $
     positions[i] = Strjoin(['Position_',Strtrim(i,2)])

  ;; now create the motor names
  motor_names = ['bsx','bsy','dnx','dny']

  ;; now create the 2d motor_values array
  motor_values = DBLARR(4,n_elements(positions))
  motor_values[0,*] = Double(bsx_positions)
  motor_values[1,*] = Double(bsy_positions)
  motor_values[2,*] = Double(dnx_positions)
  motor_values[3,*] = Double(dny_positions)

  ;; store the info in the dataset struct
  commie_dataset_struct.position_names = ptr_new(positions)
  commie_dataset_struct.motor_names = ptr_new(motor_names[0:*])
  commie_dataset_struct.motor_values = ptr_new(motor_values)

  ;; add the info to the scriptfile
  commie_scriptfile_io,*commie_script_struct.script_filename, $
                       commie_script_struct, commie_dataset_struct, $
                       /add_motor_positions
END
