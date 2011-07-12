;; this little routine will search the headers of all files in
;; filenames and tries to classify the motor positions used. It will
;; sort all unique BSY positions from low to high and associate them
;; with top_low, top_int, high, bs_moved, bot_int, bot_low
;; respectively. The other motor positions are then determined only
;; once per setting.
PRO get_motor_positions, filenames, motor_positions

  ;; hardwire the filenames for now
  this_dir='/common/Users/jsteinbr/data/dt_2008_06_13/'
  this_header='dt_2008_06_13'
  this_ext='.nc'
  this_array = [374,423]
  
  filenames = ''
  FOR i=(this_array[0]),(this_array[1]) DO BEGIN
     temp_name = strjoin([this_dir,this_header, $
                          '_', string(FORMAT='(I04)',i),this_ext])
     filenames = [filenames, temp_name]
  ENDFOR
  filenames = filenames[1:*]

  ;; make a list of all motor positions
  bsy_positions = Fltarr(n_elements(filenames))
  bsx_positions = Fltarr(n_elements(filenames))
  dnx_positions = Fltarr(n_elements(filenames))
  dny_positions = Fltarr(n_elements(filenames))

  FOR i=0, n_elements(filenames)-1 DO BEGIN
     read_dt,filenames[i],this_dt_par,/header_only
     bsy_positions[i] = this_dt_par.bsystg_m
     bsx_positions[i] = this_dt_par.bsxstg_m
     dnx_positions[i] = this_dt_par.dnxstg_m
     dny_positions[i] = this_dt_par.dnystg_m
  ENDFOR
  ;; sort these in ascending order
  sort_indices = Uniq(bsy_positions,Sort(bsy_positions))
  bsy_positions=bsy_positions[sort_indices]
  bsx_positions=bsx_positions[sort_indices]
  dnx_positions=dnx_positions[sort_indices]
  dny_positions=dny_positions[sort_indices]

  FOR i=0, n_elements(bsy_positions)-1 DO BEGIN
     print,'Position ',Strtrim(i,2), ', bsx = ',Strtrim(bsx_positions[i],2), $
           '  , bsy = ',Strtrim(bsy_positions[i],2), $
           '  , dnx = ',Strtrim(dnx_positions[i],2), $
           '  , dny = ',Strtrim(dny_positions[i],2)  
  ENDFOR
END
