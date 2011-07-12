PRO commie_init_relerrs, commie_dataset_struct

  compile_opt idl2, hidden

  ;; loop through the dummy arrays and for each array calculate the
  ;; rms-sum of background/readout and photon noise.
  dummy_inds = *commie_dataset_struct.dummy_indices
  
  ;; calculate the photon energy
  photon_ev = 1.239852e-6/(*commie_dataset_struct.lambda_meters)

  dk_by_pix = 0
  IF commie_dataset_struct.dk_by_pix THEN BEGIN
     ;; see if all necessary relerror files are there if not then fall
     ;; back on non dk_by_pix
     dk_by_pix = 1
     exp_seconds = float((*commie_dataset_struct.exp_seconds))
     dk_exp_seconds = float(*((*commie_dataset_struct.dk_exp_seconds)[0]))
     dummy = Where(exp_seconds EQ dk_exp_seconds, count)
     IF count NE n_elements(exp_seconds) THEN BEGIN
        dk_by_pix = 0
        err_string = Strjoin(['commie_init_relerrs: Relerror arrays ', $
                              'incomplete, using average sigma instead.'])
        commie_messages,status=err_string
     ENDIF
  ENDIF 
  FOR i=0, n_elements(*commie_dataset_struct.dummy_arrays)-1 DO BEGIN
     this_array = *((*commie_dataset_struct.dummy_arrays)[i])
     ;; determine adc-speed
     adc_slow = (*commie_dataset_struct.adc_are_slow)[dummy_inds[i]]

     ;; determine corresponding dk_rms_array
     exp_i = (*commie_dataset_struct.exp_indices)[dummy_inds[i]]   
     this_exp = (*commie_dataset_struct.exp_seconds)[exp_i]
     IF dk_by_pix THEN BEGIN
        dk_exp_i = $
           Where(*((*commie_dataset_struct.dk_exp_seconds)[adc_slow]) EQ $
                 this_exp, count)
        dk_exp_i = dk_exp_i[0]
        
        ;; start with the corresponding dark current variance array
        this_relerr = $
           *((*((*commie_dataset_struct.dk_rms_arrays)[adc_slow]))[dk_exp_i])
     ENDIF ELSE BEGIN
        ;; determine the correct standard deviation from dk_sigma
        this_slope = (*commie_dataset_struct.dk_sigma_slope)[adc_slow]
        this_pedestal = (*commie_dataset_struct.dk_sigma_pedestal)[adc_slow]
        this_sigma = this_exp*this_slope + this_pedestal
        
        ;; determine max to calculate relative error
        this_slope = (*commie_dataset_struct.dk_max_slope)[adc_slow]
        this_pedestal = (*commie_dataset_struct.dk_max_pedestal)[adc_slow]
        this_max = this_exp*this_slope + this_pedestal
        
        this_relerr = Dblarr(commie_dataset_struct.nx, $
                             commie_dataset_struct.ny)
        this_relerr += this_sigma/this_max
     ENDELSE
     
     ;; define array to hold photon relerrs
     photon_relerrs = $
        Dblarr(commie_dataset_struct.nx,commie_dataset_struct.ny)
     
     ;; now go through each pixel and calculate the rms of the
     ;; background plus photon noise. Assume that we need 3.63eV per
     ;; electron and that the gain is roughly 1
     def_inds = Where(this_array GT 0, def_count)
     ;; Since we haven't thresholded yet we need to make
     ;; sure that negative pixels are supported, i.e. don't
     ;; do anything for those.
     IF def_count NE 0 THEN BEGIN          
        photon_relerrs[def_inds] = $
           sqrt(photon_ev/(this_array[def_inds]*23.44))   
     ENDIF
     (*commie_dataset_struct.dummy_relerror_arrays)[i] = $
        ptr_new(sqrt(photon_relerrs^2 + this_relerr^2))
  ENDFOR
  ;; Give us an update b/c this may take a while
  status_string = 'Initialized error arrays.'
  commie_messages,status=status_string
END
