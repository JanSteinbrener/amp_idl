PRO commie_saveread_merged_pos, commie_script_struct, commie_dataset_struct, $
                                read=read, write=write

  compile_opt idl2, hidden
  
  ;; the control number is here to make sure that the user always gets
  ;; what he wants. If files of different background or dark current
  ;; handling were saved before, the program won't just assume
  ;; that the keywords are the same.
  control_number = 0
  IF (commie_dataset_struct.no_background OR $
      commie_dataset_struct.merge_first) THEN $
         control_number += 1
  IF commie_dataset_struct.dk_by_pix THEN $
     control_number += 2
  control_number = Strjoin([Strtrim(control_number,2),'_'])
  ;; define filenames first
  filenames = ['sample_','relerr_sample_','no_sample_','relerr_no_sample_']
  ;; Get the path separator
  path_sep = path_sep()

  IF Keyword_Set(read) THEN BEGIN
     ;; check existence of files
     IF commie_dataset_struct.merge_first THEN $
        inds = [0,1,2,3] ELSE inds = [0,1]
     ;; if no_background is specified then we want the filenames to
     ;; have a control number of 1 or 3.
     IF commie_dataset_struct.no_background THEN $
        inds = [0,1]
     FOR i=0,n_elements(*commie_dataset_struct.position_names)-1 DO BEGIN
        this_pos = (*commie_dataset_struct.position_names)[i]
        FOR j=0,n_elements(inds)-1 DO BEGIN
           this_name = Strjoin([*commie_script_struct.mp_saveread_dir, $
                                path_sep,filenames[inds[j]],control_number, $
                                this_pos,'.bin'])
           info = File_info(this_name,/Noexpand_path)
           IF NOT info.exists THEN BEGIN
              commie_messages,/read_merged_pos
              RETURN
           ENDIF
        ENDFOR
     ENDFOR
     ;; If all files exist then let's read them in
     these_samples = ptrarr(n_elements(*commie_dataset_struct.position_names))
     these_s_relerrs = ptrarr(n_elements(*commie_dataset_struct.position_names))
     these_nsamples = $
        ptrarr(n_elements(*commie_dataset_struct.position_names))
     these_ns_relerrs = $
        ptrarr(n_elements(*commie_dataset_struct.position_names))
     
     FOR i=0,n_elements(*commie_dataset_struct.position_names)-1 DO BEGIN
        this_pos = (*commie_dataset_struct.position_names)[i]
        FOR j=0,n_elements(inds)-1 DO BEGIN
           this_name = Strjoin([*commie_script_struct.mp_saveread_dir, $
                                path_sep,filenames[inds[j]],control_number,$
                                this_pos,'.bin'])
           IF inds[j] EQ 0 THEN BEGIN
              these_samples[i] = ptr_new(read_bin(this_name))
           ENDIF ELSE IF inds[j] EQ 1 THEN BEGIN
              these_s_relerrs[i] = ptr_new(read_bin(this_name))
           ENDIF ELSE IF inds[j] EQ 2 THEN BEGIN
              these_nsamples[i] = ptr_new(read_bin(this_name))
           ENDIF ELSE IF inds[j] EQ 3 THEN BEGIN
              these_ns_relerrs[i] = ptr_new(read_bin(this_name))
           ENDIF
        ENDFOR
     ENDFOR
     ;; store everything in the correct place
     commie_dataset_struct.pos_merged_samples = ptr_new(these_samples)
     commie_dataset_struct.relerror_samples = ptr_new(these_s_relerrs)
     commie_dataset_struct.pos_merged_nosamples = ptr_new(these_nsamples)
     commie_dataset_struct.relerror_no_samples = ptr_new(these_ns_relerrs)
     
     commie_messages,status='Succesfully read merged positions from disk.'
  ENDIF ELSE IF Keyword_Set(write) THEN BEGIN
     ;; save merged positions and relerrors as .bin files
     filebase = *commie_script_struct.mp_saveread_dir
     FOR i=0,n_elements(*commie_dataset_struct.pos_merged_samples)-1 DO BEGIN
        this_pos = (*commie_dataset_struct.position_names)[i]
        this_name = Strjoin([filebase,path_sep,'sample_',control_number, $
                             this_pos,'.bin'])
        write_bin,this_name,*((*commie_dataset_struct.pos_merged_samples)[i])
        ;; same for no_sample
        IF ptr_valid((*commie_dataset_struct.pos_merged_nosamples)[0]) THEN $
           BEGIN
           this_name = Strjoin([filebase,path_sep,'no_sample_',control_number, $
                                this_pos,'.bin'])
           write_bin,this_name, $
                     *((*commie_dataset_struct.pos_merged_nosamples)[i])
        ENDIF
     ENDFOR
     FOR i=0,n_elements(*commie_dataset_struct.pos_merged_samples)-1 DO BEGIN
        this_pos = (*commie_dataset_struct.position_names)[i]
        this_name = $
           Strjoin([filebase,path_sep,'relerr_sample_',control_number, $
                    this_pos,'.bin'])
        write_bin,this_name,*((*commie_dataset_struct.relerror_samples)[i])
        ;; same for no_sample
        IF ptr_valid((*commie_dataset_struct.pos_merged_nosamples)[0]) THEN $
           BEGIN
           this_name = $
              Strjoin([filebase,path_sep,'relerr_no_sample_',control_number, $
                       this_pos,'.bin'])
           write_bin,this_name, $
                     *((*commie_dataset_struct.relerror_no_samples)[i])
        ENDIF
     ENDFOR
     commie_messages,status='Succesfully wrote merged positions to disk.'
  ENDIF
END
