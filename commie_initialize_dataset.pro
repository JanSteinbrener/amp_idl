; This routine tries to match the files from commie_dataset_struct.filenames
; to positions defined in the position_names, motor_names, motor_values array 
; of commie_dataset_struct
; Supported motor keywords for the scriptfile are (as of now):
;        BSX, BSY, DNX, DNY
; Note: motor_values is a 2D array where the columns represent the different
;       motors and the rows represent the different positions
PRO commie_initialize_dataset, commie_script_struct, $
                               commie_dataset_struct
  
  compile_opt idl2, hidden

  path_sep = path_sep()   

  ;; see if we have any motors defined yet
  IF NOT ptr_valid(commie_dataset_struct.position_names) THEN BEGIN
     commie_get_motor_positions, commie_dataset_struct, $
                                 commie_script_struct
  ENDIF

  ;; see what kind of motors have been defined. Then just use IF need_bsx
  ;; in the following routines
  motor_count = size(*(commie_dataset_struct.motor_names))
  ;; de-activate all by default
  need_bsx = 0
  need_bsy = 0
  need_dnx = 0
  need_dny = 0
  ;; need this later-on 
  j_min = Intarr(motor_count[1])
  ;; now activate those that are in scriptfile
  FOR i=0,(motor_count[1]-1) DO BEGIN
     IF (Strcmp((*(commie_dataset_struct.motor_names))[i],'BSX',3,/Fold_Case) $
         EQ 1) THEN BEGIN
        ;; get index
        bsx_index = i
        need_bsx = 1
     ENDIF
     IF (Strcmp((*(commie_dataset_struct.motor_names))[i],'BSY',3,/Fold_Case) $
         EQ 1) THEN BEGIN
        ;; get index
        bsy_index = i
        need_bsy = 1 
     ENDIF
     IF (Strcmp((*(commie_dataset_struct.motor_names))[i],'DNX',3,/Fold_Case) $
        EQ 1) THEN BEGIN
        ;; get index
        dnx_index = i
        need_dnx = 1 
     ENDIF 
     IF (Strcmp((*(commie_dataset_struct.motor_names))[i],'DNY',3,/Fold_Case) $
         EQ 1) THEN BEGIN
        ;; get index
        dny_index = i
        need_dny = 1 
     ENDIF
  ENDFOR

  ;; now check for the appropriate routine to open the files
  IF (Strcmp(*(commie_script_struct.filetype),'dt_nc',/Fold_Case) $
      EQ 1) THEN BEGIN
     svec = size(*(commie_dataset_struct.filenames))
     ;; this will contain the position indices of all arrays
     max_indices = Intarr(svec[1])
     count_pos = (size(*(commie_dataset_struct.position_names)))[1]
     ;; now create the array that keeps track of which motor positions
     ;; of a file correspond to which positions in the scriptfile within 
     ;; the tolerance. 
     ;; Columns is motors and rows is positions. 1 indicates a match, 0 
     ;; if it doesn't match. Note: orders of motors is hardwired to:
     ;; BSX, BSY, DNX, DNY.
     j_min = Intarr(motor_count[1], count_pos)
     ;; this array will hold the exposure seconds for each file
     temp_exp = Fltarr(svec[1])
     ;; this will hold the adc_readout speeds. NOTE that the default
     ;; is fast read-out speed
     temp_adc = Intarr(svec[1])
     ;; this will hold all the systimes
     temp_systimes = ''
     ;; this will hold the tilt angles
     temp_theta_x = dblarr(svec[1])
     ;; read-in one datafile to determine the size of the arrays
     commie_read_dt,commie_script_struct,commie_dataset_struct, $
                    (*(commie_dataset_struct.filenames))[0], $
                    this_dt_par, this_array
     arraysize = size(this_array)
     ;; store the array size in the dataset_struct
     commie_dataset_struct.nx = arraysize[1]
     commie_dataset_struct.ny = arraysize[2]
     ;; it's a 2d array, so total number of elements is 5th
     ;; array element. 
     commie_dataset_struct.n = arraysize[4]

     ;; now define the default for sat_threshold_high
     IF NOT ptr_valid(commie_script_struct.sat_threshold_high) THEN BEGIN
        IF arraysize[1] GT 2000 THEN BEGIN
           commie_script_struct.sat_threshold_high = ptr_new(45000)
        ENDIF ELSE BEGIN
           commie_script_struct.sat_threshold_high = ptr_new(long((2L^16-1)*.9))
        ENDELSE
     ENDIF
     
     ;; read in the header files 
     FOR i=0,(svec[1]-1) DO BEGIN
        commie_read_dt,commie_script_struct,commie_dataset_struct, $
                       (*(commie_dataset_struct.filenames))[i], $
                       this_dt_par, /header_only
        ;; determine the smallest ring_amps that was used to record
        ;; files. Use this to generate a threshold if user did not
        ;; supply one.
        IF i EQ 0 THEN $
           this_amps = this_dt_par.ring_amps

        IF this_dt_par.ring_amps LT this_amps THEN $
           this_amps = this_dt_par.ring_amps

        ;; get info on specimen_name and lambda_meters from first
        ;; datafile.
        IF i EQ 0 THEN BEGIN
           commie_dataset_struct.specimen_name = $
              ptr_new(Strtrim(this_dt_par.sample,2))
           ;; make sure that it is actually defined
           IF NOT ptr_valid(commie_dataset_struct.lambda_meters) THEN BEGIN
              commie_messages,commie_dataset_struct,/lambda_meters
           ENDIF
        ENDIF
        ;; get the information about the adc read-out speed... this
        ;; only works for file_version 5 and above.
        IF this_dt_par.filever GT 4 THEN BEGIN
           IF this_dt_par.ccd_adc_is_fast EQ 0 THEN $
              temp_adc[i] = 1
        ENDIF
        ;; add the rotation angle. At the moment only
        ;; gmrstg_millidegrees is stored so convert to radians here.
        temp_theta_x[i] = (this_dt_par.gmrstg_millidegrees/360000)*2*!dpi

        ;; add the systime
        temp_systimes = [temp_systimes,Strtrim(this_dt_par.systime,2)]
        ;; first easily generate the exposure seconds array. Assuming shortest
        ;; exposure is 1ms!! This rounding is necessary for matching
        temp_exp[i] = (Float(Round(1000*Float(this_dt_par.image_seconds))))/1000
        ;; reset the j_min array for the new file
        j_min[0:*] = 1
        IF need_bsx THEN BEGIN
           temp_bsx = this_dt_par.bsxstg_m
           FOR j=0,count_pos -1 DO BEGIN
              ;; if new value falls within tolerance of minimum leave  
              ;; it to 1. If it is greater set it to 0.
              IF Abs(temp_bsx -$
                     (*(commie_dataset_struct.motor_values))[bsx_index,j]) GT $
                 *(commie_script_struct.tolerance_meters) THEN BEGIN
                 j_min[0,j] = 0
              ENDIF ;; else it is within the tolerance
           ENDFOR
        ENDIF
        IF need_bsy THEN BEGIN
           temp_bsy = this_dt_par.bsystg_m
           FOR j=0,count_pos -1 DO BEGIN
              ;; if new value falls within tolerance of minimum leave  
              ;; it to 1. If it is greater set it to 0.
              IF Abs(temp_bsy -$
                     (*(commie_dataset_struct.motor_values))[bsy_index,j]) GT $
                 *(commie_script_struct.tolerance_meters) THEN BEGIN
                 j_min[1,j] = 0
              ENDIF ;; else it is within the tolerance
           ENDFOR
        ENDIF
        IF need_dnx THEN BEGIN
           if (this_dt_par.filever gt 5) then begin
              temp_dnx = this_dt_par.up2xstg_m
           endif else begin
              temp_dnx = this_dt_par.dnxstg_m
           endelse
           FOR j=0,count_pos -1 DO BEGIN
              ;; if new value falls within tolerance of minimum leave  
              ;; it to 1. If it is greater set it to 0.
              IF Abs(temp_dnx -$
                     (*(commie_dataset_struct.motor_values))[dnx_index,j]) GT $
                 *(commie_script_struct.tolerance_meters) THEN BEGIN
                 j_min[2,j] = 0
              ENDIF ;; else it is within the tolerance
           ENDFOR
        ENDIF
        IF need_dny THEN BEGIN
           if (this_dt_par.filever gt 5) then begin
              temp_dny = this_dt_par.up2ystg_m
           endif else begin
              temp_dny = this_dt_par.dnystg_m
           endelse
           FOR j=0,count_pos -1 DO BEGIN
              ;; if new value falls within tolerance of minimum leave  
              ;; it to 1. If it is greater set it to 0.
              IF Abs(temp_dny -$
                     (*(commie_dataset_struct.motor_values))[dny_index,j]) GT $
                 *(commie_script_struct.tolerance_meters) THEN BEGIN
                 j_min[3,j] = 0
              ENDIF ;; else it is within the tolerance
           ENDFOR
        ENDIF

        ;; now check which position has the most 1's
        FOR j=0,(count_pos -1) DO BEGIN
           IF j EQ 0 THEN BEGIN
              dummy = where(j_min[*,j] EQ 1, count)
              max_index = j
           ENDIF
           IF j GT 0 THEN BEGIN
              dummy = where(j_min[*,j] EQ 1, count2)
              IF count2 GT count THEN BEGIN
                 count = count2
                 max_index = j
              ENDIF
           ENDIF
        ENDFOR
        max_indices[i] = max_index
     ENDFOR
     ;; calculate threshold default if we need it
     IF NOT ptr_valid(commie_script_struct.thresh_data_factor) THEN begin
        if this_amps eq 0 then begin
           if PTR_VALID(commie_script_struct.ring_amps) then begin
              this_amps = *commie_script_struct.ring_amps
           endif else begin
              string = STRJOIN(['[WARNING] commie_initialize_dataset: ', $
                                'dt_par.ring_current is zero! Assuming 0.4 ', $
                                'instead. Use ring_amps to set otherwise.'])
              commie_messages, status=string
              this_amps = 0.4
           endelse
        endif
        commie_script_struct.thresh_data_factor = ptr_new(1/this_amps)
     endif
     ;; store rotation angles in pointer
     commie_dataset_struct.theta_x_radians = ptr_new(temp_theta_x)
     ;; check if all angles are the same within a quarter degree
     ;; tolerance. 
     dummy = Where(((temp_theta_x LT (temp_theta_x[0]+4.4e-3)) AND $
                    (temp_theta_x GT (temp_theta_x[0]-4.4e-3))),count)
     IF count NE svec[1] THEN BEGIN
        commie_messages,commie_dataset_struct,commie_script_struct,/angles
     ENDIF
     ;; store systimes in pointer
     commie_dataset_struct.systimes = ptr_new(temp_systimes[1:*])
     ;; sort the exp_seconds in ascending order
     temp_exp=temp_exp[Uniq(temp_exp,Sort(temp_exp))]
     ;; now associate each file with an exposure by index reference
     temp_arr = Intarr(svec[1])
     FOR i=0,(svec[1]-1) DO BEGIN
        commie_read_dt, commie_script_struct, commie_dataset_struct, $
                        (*(commie_dataset_struct.filenames))[i], $
                        this_dt_par, /header_only
        FOR j=0,(n_elements(temp_exp)-1) DO BEGIN
           ;; perform the same rounding like before, assuming shortest exposure
           ;; is 1ms.
           this_exp = (Float(Round(1000*Float(this_dt_par.image_seconds))))/1000
           IF (this_exp EQ temp_exp[j]) THEN $
              temp_arr[i]=j
        ENDFOR
     ENDFOR
     ;; now store all in commie_dataset_struct
     commie_dataset_struct.exp_seconds = ptr_new(temp_exp)
     commie_dataset_struct.position_indices = ptr_new(max_indices)
     commie_dataset_struct.exp_indices = ptr_new(temp_arr)
     commie_dataset_struct.adc_are_slow = ptr_new(temp_adc)

     ;; print out some additional info for verbose purposes
     IF *commie_script_struct.verbose THEN BEGIN
        commie_verbose_output, commie_dataset_struct, commie_script_struct, $
                               /initialize_dataset
     ENDIF
  ENDIF ;; filetype = dt_nc

  ;; if user didn't specify an output filename we generate one from 
  ;; the tilt-angle specification. 
  IF NOT ptr_valid(commie_script_struct.output) THEN BEGIN
     commie_read_dt, commie_script_struct, commie_dataset_struct, $
                     (*(commie_dataset_struct.filenames))[0], $
                     this_dt_par, /header_only
     ;; get tilt info and round to next 1/4 degree angle
     this_angle = $
        String(FORMAT='(F+07.2)', $
               (Double(Round(double(this_dt_par.gmrstg_millidegrees)*0.004))/4))
     commie_script_struct.output = $
        ptr_new(Strjoin([*(commie_script_struct.topdir),path_sep,$ 
                         'cell_alpha_',this_angle,'.h5']))
  ENDIF 
  ;; now check if the file already exists and prompt user for
  ;; confirmation to overwrite or to give new filename
  f_info = FILE_INFO(*commie_script_struct.output)
  IF (f_info.exists) THEN $
     commie_messages, commie_dataset_struct, commie_script_struct, /output

  ;; check if the user already determined the bad data, if not then have user 
  ;; decide for each individual datafile then write the bad_data to the
  ;; scriptfile using commie_scriptfile_io,/bad_data
  temp_bads = Intarr(n_elements(*(commie_dataset_struct.filenames)))
  IF NOT ptr_valid(commie_script_struct.bad_data_dirs) THEN BEGIN
     ;; in that case the user indicated that there are no bad datafiles
     IF ptr_valid(commie_script_struct.bad_data_indices) THEN BEGIN
        commie_dataset_struct.are_bad = ptr_new(temp_bads)
     ENDIF ELSE BEGIN
        Window,/free,xsize=600,ysize=600
        win_id = !D.Window
        FOR i=0,(n_elements(*(commie_dataset_struct.filenames))-1) DO BEGIN
           commie_read_dt, commie_script_struct, commie_dataset_struct, $
                           (*(commie_dataset_struct.filenames))[i],this_dt_par,$
                           temp_array 
           ;; have user decide which are good/bad
           ;; turns out the new CCD has very low biases so we have to
           ;; account for the fact that there might be pixels with 0
           ;; count rate.
           temp_array += 0.001
           temp_array=Alog10(temp_array)
           temp_array=congrid(temp_array,600,600)
           TVScl,temp_array
           answer=''
           print,'Is ', $
                 strtrim((*(commie_dataset_struct.filenames))[i],2), $
                 ' good data? (y/n)'
           answer = Get_Kbrd()
           WHILE ((Strcmp(answer,'y',/Fold_case) $
                   || Strcmp(answer,'n',/Fold_case)) EQ 0) DO BEGIN
              print,'Please answer y/n.'
              answer = Get_Kbrd()
           ENDWHILE
           ;; get index for the bad data files
           IF (Strcmp(answer,'n',/Fold_case) EQ 1) THEN BEGIN
              temp_bads[i] = 1
           ENDIF
        ENDFOR
        wdelete, win_id
        commie_dataset_struct.are_bad = ptr_new(temp_bads)
        ;; now update the scriptfile with the bad_data information
        commie_scriptfile_io, *(commie_script_struct.script_filename), $
                              commie_script_struct, commie_dataset_struct, $
                              error_string, /bad_data
     ENDELSE
  ENDIF ELSE BEGIN ;; get bad data info from scriptfile
     this_array = *(commie_script_struct.bad_data_indices)
     this_dir = *(commie_script_struct.bad_data_dirs)
     this_header = *(commie_script_struct.bad_data_headers)
     svec = size(this_array)
     IF (Strcmp(*(commie_script_struct.filetype),'dt_nc',/Fold_Case) $
         EQ 1) THEN BEGIN
        this_ext = '.nc'
     ENDIF
     FOR i=0,(svec[1]-1) DO BEGIN
        FOR j=(this_array[i,0]),(this_array[i,1]) DO BEGIN
           temp_name = strjoin([this_dir[i],path_sep,this_header[i], $
                                '_', string(FORMAT='(I04)',j),this_ext])
           this_index = $
              Where(Strcmp(*(commie_dataset_struct.filenames),temp_name) EQ 1)
           temp_bads[this_index] = 1
        ENDFOR
     ENDFOR
     commie_dataset_struct.are_bad = ptr_new(temp_bads)
  ENDELSE
  ;; Give us an update b/c this may take a while
  status_string = 'Initialized dataset.'
  commie_messages,status=status_string
END
