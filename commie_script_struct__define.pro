;; This file contains the definitions of ALL structures used in COMMIE
;; and provides routines for their garbage clean-up

PRO commie_free_script_struct, this_script_struct

  compile_opt idl2, hidden

  If ptr_valid(this_script_struct.commie_version) THEN $
     ptr_free, this_script_struct.commie_version

  If ptr_valid(this_script_struct.script_filename) THEN $
     ptr_free, this_script_struct.script_filename

  If ptr_valid(this_script_struct.bs_filename) THEN $
     ptr_free, this_script_struct.bs_filename

  If ptr_valid(this_script_struct.dk_saveread_file) THEN $
     ptr_free, this_script_struct.dk_saveread_file

  If ptr_valid(this_script_struct.mp_saveread_dir) THEN $
     ptr_free, this_script_struct.mp_saveread_dir

  If ptr_valid(this_script_struct.filetype) THEN $
     ptr_free, this_script_struct.filetype

  If ptr_valid(this_script_struct.topdir) THEN $
     ptr_free, this_script_struct.topdir
  
  If ptr_valid(this_script_struct.sample_indices) THEN $
     ptr_free, this_script_struct.sample_indices
  
  If ptr_valid(this_script_struct.sample_dirs) THEN $
     ptr_free, this_script_struct.sample_dirs

  If ptr_valid(this_script_struct.sample_headers) THEN $
     ptr_free, this_script_struct.sample_headers

  If ptr_valid(this_script_struct.no_sample_dirs) THEN $
     ptr_free, this_script_struct.no_sample_dirs

  If ptr_valid(this_script_struct.no_sample_indices) THEN $
     ptr_free, this_script_struct.no_sample_indices
  
  If ptr_valid(this_script_struct.no_sample_headers) THEN $
     ptr_free, this_script_struct.no_sample_headers

  If ptr_valid(this_script_struct.subset_indices) THEN $
     ptr_free, this_script_struct.subset_indices
  
  If ptr_valid(this_script_struct.bad_data_dirs) THEN $
     ptr_free, this_script_struct.bad_data_dirs

  If ptr_valid(this_script_struct.bad_data_headers) THEN $
     ptr_free, this_script_struct.bad_data_headers

  If ptr_valid(this_script_struct.bad_data_indices) THEN $
     ptr_free, this_script_struct.bad_data_indices

  If ptr_valid(this_script_struct.dark_current_dirs) THEN $
     ptr_free, this_script_struct.dark_current_dirs

  If ptr_valid(this_script_struct.dark_current_headers) THEN $
     ptr_free, this_script_struct.dark_current_headers

  If ptr_valid(this_script_struct.dark_current_indices) THEN $
     ptr_free, this_script_struct.dark_current_indices

  If ptr_valid(this_script_struct.tolerance_meters) THEN $
     ptr_free, this_script_struct.tolerance_meters

  If ptr_valid(this_script_struct.sat_threshold_high) THEN $
     ptr_free, this_script_struct.sat_threshold_high

  If ptr_valid(this_script_struct.sat_threshold_low) THEN $
     ptr_free, this_script_struct.sat_threshold_low

  If ptr_valid(this_script_struct.ring_amps) THEN $
     ptr_free, this_script_struct.ring_amps

  If ptr_valid(this_script_struct.output) THEN $
     ptr_free, this_script_struct.output

  If ptr_valid(this_script_struct.verbose) THEN $
     ptr_free, this_script_struct.verbose

  If ptr_valid(this_script_struct.median_percent) THEN $
     ptr_free, this_script_struct.median_percent

  If ptr_valid(this_script_struct.thresh_data_factor) THEN $
     ptr_free, this_script_struct.thresh_data_factor
END

PRO commie_free_dataset_struct, this_dataset_struct

  compile_opt idl2, hidden

  If ptr_valid(this_dataset_struct.specimen_name) THEN $
     ptr_free, this_dataset_struct.specimen_name

  IF ptr_valid(this_dataset_struct.filenames) THEN $
     ptr_free, this_dataset_struct.filenames

  IF ptr_valid(this_dataset_struct.systimes) THEN $
     ptr_free, this_dataset_struct.systimes

  IF ptr_valid(this_dataset_struct.theta_x_radians) THEN $
     ptr_free, this_dataset_struct.theta_x_radians

  IF ptr_valid(this_dataset_struct.lambda_meters) THEN $
     ptr_free, this_dataset_struct.lambda_meters

  IF ptr_valid(this_dataset_struct.are_samples) THEN $
     ptr_free, this_dataset_struct.are_samples

  IF ptr_valid(this_dataset_struct.are_bad) THEN $
     ptr_free, this_dataset_struct.are_bad
  
  IF ptr_valid(this_dataset_struct.adc_are_slow) THEN $
     ptr_free, this_dataset_struct.adc_are_slow

  IF ptr_valid(this_dataset_struct.position_indices) THEN $
     ptr_free, this_dataset_struct.position_indices

  If ptr_valid(this_dataset_struct.rotate) THEN $
     ptr_free, this_dataset_struct.rotate

  IF ptr_valid(this_dataset_struct.position_names) THEN $
     ptr_free, this_dataset_struct.position_names

  IF ptr_valid(this_dataset_struct.motor_names) THEN $
     ptr_free, this_dataset_struct.motor_names
  
  IF ptr_valid(this_dataset_struct.motor_values) THEN $
     ptr_free, this_dataset_struct.motor_values

  IF ptr_valid(this_dataset_struct.mask_filenames) THEN $
     ptr_free, this_dataset_struct.mask_filenames

  IF ptr_valid(this_dataset_struct.mask_position_indices) THEN $
     ptr_free, this_dataset_struct.mask_position_indices

  IF ptr_valid(this_dataset_struct.dk_filenames) THEN $
     ptr_free, this_dataset_struct.dk_filenames

  IF ptr_valid(this_dataset_struct.exp_seconds) THEN $
     ptr_free, this_dataset_struct.exp_seconds

  IF ptr_valid(this_dataset_struct.exp_indices) THEN $
     ptr_free, this_dataset_struct.exp_indices

  IF ptr_valid(this_dataset_struct.dummy_indices) THEN $
     ptr_free, this_dataset_struct.dummy_indices

  IF ptr_valid(this_dataset_struct.dummy_arrays) THEN BEGIN
     FOR i=0,(n_elements(*(this_dataset_struct.dummy_arrays))-1) DO BEGIN
        ptr_free, (*(this_dataset_struct.dummy_arrays))[i]
     ENDFOR
     ptr_free, this_dataset_struct.dummy_arrays
  ENDIF

  IF ptr_valid(this_dataset_struct.dummy_relerror_arrays) THEN BEGIN
     FOR i=0,(n_elements(*(this_dataset_struct.dummy_relerror_arrays))-1) DO $
        BEGIN
        ptr_free, (*(this_dataset_struct.dummy_relerror_arrays))[i]
     ENDFOR
     ptr_free, this_dataset_struct.dummy_relerror_arrays
  ENDIF

  IF ptr_valid(this_dataset_struct.relerror_samples) THEN BEGIN
     ptr_free, this_dataset_struct.relerror_samples
  ENDIF

  IF ptr_valid(this_dataset_struct.relerror_no_samples) THEN BEGIN
     ptr_free, this_dataset_struct.relerror_no_samples
  ENDIF

  IF ptr_valid(this_dataset_struct.pos_merged_samples) THEN BEGIN
     FOR i=0,(n_elements(*(this_dataset_struct.pos_merged_samples))-1) DO BEGIN
        ptr_free, (*(this_dataset_struct.pos_merged_samples))[i]
     ENDFOR
     ptr_free, this_dataset_struct.pos_merged_samples
  ENDIF

  IF ptr_valid(this_dataset_struct.pos_merged_nosamples) THEN BEGIN
    FOR i=0,(n_elements(*(this_dataset_struct.pos_merged_nosamples))-1) DO BEGIN
       IF ptr_valid((*(this_dataset_struct.pos_merged_nosamples))[i]) THEN $
          ptr_free, (*(this_dataset_struct.pos_merged_nosamples))[i]
     ENDFOR
     ptr_free, this_dataset_struct.pos_merged_nosamples
  ENDIF

  IF ptr_valid(this_dataset_struct.damaged_pixels) THEN BEGIN
    FOR i=0,(n_elements(*(this_dataset_struct.damaged_pixels))-1) DO BEGIN
       IF ptr_valid((*(this_dataset_struct.damaged_pixels))[i]) THEN $
          ptr_free, (*(this_dataset_struct.damaged_pixels))[i]
     ENDFOR
     ptr_free, this_dataset_struct.damaged_pixels
  ENDIF

  IF ptr_valid(this_dataset_struct.dk_exp_seconds) THEN BEGIN
    FOR i=0,(n_elements(*(this_dataset_struct.dk_exp_seconds))-1) DO BEGIN
       IF ptr_valid((*(this_dataset_struct.dk_exp_seconds))[i]) THEN $
          ptr_free, (*(this_dataset_struct.dk_exp_seconds))[i]
     ENDFOR
     ptr_free, this_dataset_struct.dk_exp_seconds
  ENDIF

  IF ptr_valid(this_dataset_struct.dk_rms_arrays) THEN BEGIN
    FOR i=0,(n_elements(*(this_dataset_struct.dk_rms_arrays))-1) DO BEGIN
       IF ptr_valid((*(this_dataset_struct.dk_rms_arrays))[i]) THEN BEGIN
       FOR j=0,(n_elements(*((*(this_dataset_struct.dk_rms_arrays))[i]))-1) DO $
            BEGIN
            IF ptr_valid((*(*(this_dataset_struct.dk_rms_arrays))[i])[j]) THEN $
               ptr_free,(*(*(this_dataset_struct.dk_rms_arrays))[i])[j]
         ENDFOR
         ptr_free, (*(this_dataset_struct.dk_rms_arrays))[i]
       ENDIF
    ENDFOR
    ptr_free, this_dataset_struct.dk_rms_arrays
  ENDIF

  IF ptr_valid(this_dataset_struct.dk_avg_arrays) THEN BEGIN
    FOR i=0,(n_elements(*(this_dataset_struct.dk_avg_arrays))-1) DO BEGIN
       IF ptr_valid((*(this_dataset_struct.dk_avg_arrays))[i]) THEN BEGIN
       FOR j=0,(n_elements(*((*(this_dataset_struct.dk_avg_arrays))[i]))-1) DO $
            BEGIN
            IF ptr_valid((*(*(this_dataset_struct.dk_avg_arrays))[i])[j]) THEN $
               ptr_free,(*(*(this_dataset_struct.dk_avg_arrays))[i])[j]
         ENDFOR
         ptr_free, (*(this_dataset_struct.dk_avg_arrays))[i]
       ENDIF
    ENDFOR
    ptr_free, this_dataset_struct.dk_avg_arrays
  ENDIF

  IF ptr_valid(this_dataset_struct.assembled_sample) THEN $
     ptr_free, this_dataset_struct.assembled_sample
  
  IF ptr_valid(this_dataset_struct.assembled_nosample) THEN $
     ptr_free, this_dataset_struct.assembled_nosample

  IF ptr_valid(this_dataset_struct.bs_xs) THEN $
     ptr_free, this_dataset_struct.bs_xs
  
  IF ptr_valid(this_dataset_struct.bs_ys) THEN $
     ptr_free, this_dataset_struct.bs_ys

  IF ptr_valid(this_dataset_struct.bs_array) THEN $
     ptr_free, this_dataset_struct.bs_array

  IF ptr_valid(this_dataset_struct.dk_max_pedestal) THEN $
     ptr_free, this_dataset_struct.dk_max_pedestal

  IF ptr_valid(this_dataset_struct.dk_max_slope) THEN $
     ptr_free, this_dataset_struct.dk_max_slope

  IF ptr_valid(this_dataset_struct.dk_sigma_pedestal) THEN $
     ptr_free, this_dataset_struct.dk_sigma_pedestal
  
  IF ptr_valid(this_dataset_struct.dk_sigma_slope) THEN $
     ptr_free, this_dataset_struct.dk_sigma_slope
END

PRO commie_free_all, this_dataset_struct, this_script_struct
  compile_opt idl2, hidden
  commie_free_script_struct, this_script_struct
  commie_free_dataset_struct, this_dataset_struct
END

PRO commie_script_struct__define
  
  compile_opt idl2, hidden

    s = {commie_script_struct, $
         commie_version:ptr_new(), $
         script_filename:ptr_new(), $
         bs_filename:ptr_new(), $
         dk_saveread_file:ptr_new(), $
         mp_saveread_dir:ptr_new(), $
         filetype:ptr_new(), $
         topdir:ptr_new(), $
         sample_dirs:ptr_new(), $
         sample_indices:ptr_new(), $
         sample_headers:ptr_new(), $
         no_sample_dirs:ptr_new(), $
         no_sample_indices:ptr_new(), $
         no_sample_headers:ptr_new(), $
         subset_indices:ptr_new(), $
         bad_data_dirs:ptr_new(), $
         bad_data_headers:ptr_new(), $
         bad_data_indices:ptr_new(), $
         dark_current_dirs:ptr_new(), $
         dark_current_headers:ptr_new(), $
         dark_current_indices:ptr_new(), $
         tolerance_meters:ptr_new(), $
         sat_threshold_high:ptr_new(), $
         sat_threshold_low:ptr_new(), $
         ring_amps:ptr_new(), $
         output:ptr_new(), $
         verbose:ptr_new(), $
         median_percent:ptr_new(), $
         thresh_data_factor:ptr_new()}

    s = {commie_dataset_struct, $
         nx:0, $
         ny:0, $
         n:long(0), $
         dk_by_pix:0, $
	 specimen_name:ptr_new(), $
         filenames:ptr_new(), $
         systimes:ptr_new(), $
         theta_x_radians:ptr_new(), $
         lambda_meters:ptr_new(), $
         are_samples:ptr_new(), $
         are_bad:ptr_new(), $
         adc_are_slow:ptr_new(), $
         dk_max_pedestal:ptr_new(), $
         dk_max_slope:ptr_new(), $
         dk_sigma_pedestal:ptr_new(), $
         dk_sigma_slope:ptr_new(), $
         dk_exp_seconds:ptr_new(), $
         dk_rms_arrays:ptr_new(), $
         dk_avg_arrays:ptr_new(), $
         exp_seconds:ptr_new(), $
         exp_indices:ptr_new(), $
         rotate:ptr_new(), $
         position_indices:ptr_new(), $
         position_names:ptr_new(), $
         motor_names:ptr_new(), $
         motor_values:ptr_new(), $
         mask_filenames:ptr_new(), $
         mask_position_indices:ptr_new(), $
         dk_filenames:ptr_new(), $
         dummy_indices:ptr_new(), $
         dummy_arrays:ptr_new(), $
         dummy_relerror_arrays:ptr_new(), $
         relerror_samples:ptr_new(), $
         relerror_no_samples:ptr_new(), $
         pos_merged_samples:ptr_new(), $
         pos_merged_nosamples:ptr_new(), $
         assembled_sample:ptr_new(), $
         assembled_nosample:ptr_new(), $
         bs_xs:ptr_new(), $
         bs_ys:ptr_new(), $
         bs_array:ptr_new(), $
         damaged_pixels:ptr_new(), $
         merge_first:0, $
         no_background:0}
 END
