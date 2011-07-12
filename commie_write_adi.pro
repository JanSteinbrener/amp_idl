PRO commie_write_adi, commie_dataset_struct, commie_script_struct
  
  compile_opt idl2, hidden

  filename = (*commie_script_struct.output)
  error_string = ''
 
  ;; get a file_id for our file
  IF (dm_h5_create(filename,h5_file_id,error_string) $
      EQ 1) THEN BEGIN
      print,error_string
      return
   ENDIF

  ;; define the comments we want to add to the file
  specimen_name = (*commie_dataset_struct.specimen_name)
  collection_date = systime()
  comments = ['',Strjoin(['Merged by Commie v', $
                          String(FORMAT='(F5.2)', $
                                 (*commie_script_struct.commie_version)), $
                          ' on ', Strtrim(collection_date,2),'.'])]
 
  IF (dm_h5_create_comments(h5_file_id,specimen_name,collection_date,$
                           comments,error_string) EQ 1) THEN BEGIN
     print,error_string
     dm_h5_close,h5_file_id
     return
  ENDIF
 
  ;; first get a structure to hold all the necessary data.
  dm_init_adi_struct,adi_struct
  
  ;; fill-in the values from the structures.
  adi_struct.photon_scaling = 0.
  adi_struct.error_scaling = 0.
  adi_struct.lambda_meters = (*commie_dataset_struct.lambda_meters)
  adi_struct.camera_z_meters = 0.
  adi_struct.camera_x_pixelsize_meters = 0.
  adi_struct.camera_y_pixelsize_meters = 0.
  adi_struct.camera_alpha_radians = 0.
  adi_struct.camera_beta_radians = 0.
  adi_struct.camera_gamma_radians = 0.
  adi_struct.median_filter_width = 0
  adi_struct.median_filter_threshold = 0
  adi_struct.saturation_min = (*commie_script_struct.sat_threshold_low)
  adi_struct.saturation_max = (*commie_script_struct.sat_threshold_high)
  adi_struct.theta_x_radians = (*commie_dataset_struct.theta_x_radians)[0]
  adi_struct.theta_y_radians = 0.
  adi_struct.theta_z_radians = 0.
  adi_struct.xcenter_offset_pixels = 0.
  adi_struct.ycenter_offset_pixels = 0.
 
  svec = [commie_dataset_struct.nx,commie_dataset_struct.ny]
  adi_array = (*commie_dataset_struct.assembled_sample)
  adi_error_array = (*commie_dataset_struct.relerror_samples)

  ;; shift the array to fft-centered even if we're not sure
  ;; that the center of the diffraction pattern is also the center of
  ;; the array. This way people can use their own centering routines.
  adi_array = SHIFT(adi_array,svec[0]/2,svec[1]/2)
  adi_error_array = SHIFT(adi_error_array,svec[0]/2,svec[1]/2)

  IF (dm_h5_write_adi(h5_file_id,adi_struct,adi_array,adi_error_array,$
                      error_string) EQ 1) THEN BEGIN
     print,error_string
     dm_h5_close,h5_file_id
     return
  ENDIF ELSE BEGIN
     print,'  Wrote "/adi" group'
  ENDELSE
  
  ;; Create an /ainfo group
  dm_init_ainfo_struct,ainfo_struct
  ainfo_struct.no_background = commie_dataset_struct.no_background
  ainfo_struct.dk_by_pix = commie_dataset_struct.dk_by_pix
  ainfo_struct.merge_first = commie_dataset_struct.merge_first
  
  ainfo_struct.filename_array = ptr_new((*commie_dataset_struct.filenames))
  ainfo_struct.systime_array = ptr_new((*commie_dataset_struct.systimes))

  ainfo_struct.n_frames = n_elements((*commie_dataset_struct.filenames))
  IF (dm_h5_write_ainfo(h5_file_id,ainfo_struct,error_string) EQ 1) THEN BEGIN
     print,error_string
     dm_h5_close,h5_file_id
     return
  ENDIF ELSE BEGIN
     print,'  Wrote "/ainfo" group'
  ENDELSE
  dm_h5_close,h5_file_id 
  ;; don't forget to free the pointers
  dm_ptr_free_ainfo, ainfo_struct

  print,'Wrote file "'+filename+'"'
END
