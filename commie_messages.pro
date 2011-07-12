PRO commie_messages, commie_dataset_struct, commie_script_struct, $
                     angles=angles, dk_read=dk_read, $
                     subtract_dks=subtract_dks, $
                     status=status, lambda_meters=lambda_meters, $
                     read_merged_pos=read_merged_pos, $
                     output=output

  compile_opt idl2, hidden
  
  path_sep = path_sep()

  IF Keyword_Set(angles) THEN BEGIN
     answer=''
     print, Strjoin(['commie_initialize_dataset: Not all angles are the same.',$
                     ' Do you wish to continue anyway?'])
     WHILE ((Strcmp(answer,'y') NE 1) AND $
            (Strcmp(answer,'n') NE 1)) DO BEGIN
        print,'Please answer (y/n).'
        answer = Get_Kbrd()
     ENDWHILE
     IF (Strcmp(answer,'n') EQ 1) THEN BEGIN
        print,'As you wish.... aborting!'
        commie_free_all, commie_dataset_struct, commie_script_struct
        RETALL
     ENDIF
  ENDIF ELSE IF Keyword_Set(dk_read) THEN BEGIN
     print, Strjoin(['commie_saveread_dk: ', $
                     'Failed to get dk info from disk. Will call ccd_classify',$
                     ' to determine from raw data.'])
  ENDIF ELSE IF Keyword_Set(subtract_dks) THEN BEGIN
     answer=''
     print, Strjoin(['commie_subtract_dks: dk_avg_arrays incomplete.',$
                     ' Do you wish to supplement dk_by_pix with standard ', $
                     'dk subtraction?'])
     WHILE ((Strcmp(answer,'y') NE 1) AND $
            (Strcmp(answer,'n') NE 1)) DO BEGIN
        print,'Please answer (y/n).'
        answer = Get_Kbrd()
     ENDWHILE
     IF (Strcmp(answer,'n') EQ 1) THEN BEGIN
        print,'As you wish.... aborting!'
        commie_free_all, commie_dataset_struct, commie_script_struct
        RETALL
     ENDIF
  ENDIF ELSE IF Keyword_Set(status) THEN BEGIN
     print,status
  ENDIF ELSE IF Keyword_Set(lambda_meters) THEN BEGIN
     lambda = ''
     answer = ''
     print, Strjoin(['Scriptfile did not indicate energy of radiation.', $
                     ' Please specify energy in eV!'])
     lambda_label:
     Read,lambda
     WHILE ((Strcmp(answer,'y') NE 1) AND $
            (Strcmp(answer,'n') NE 1)) DO BEGIN
        print,Strjoin(['Energy entered: ',lambda,' (eV).', $
                      ' Is this correct?'])
        print,'Please answer (y/n).'
        answer = Get_Kbrd()
     ENDWHILE
     IF (Strcmp(answer,'n') EQ 1) THEN BEGIN
        print,'Please specify energy in eV!'
        answer=''
        goto,lambda_label
     ENDIF
     commie_dataset_struct.lambda_meters = $
        ptr_new(Double(1239.852)/(Double(1.e9)*Double(lambda)))
  ENDIF ELSE IF Keyword_Set(read_merged_pos) THEN BEGIN
     print, Strjoin(['commie_saveread_merged_pos: ', $
                     'Failed to load merged position arrays from disk. ', $
                     'Will call commie_merge_by_pos.'])
  ENDIF ELSE IF KEYWORD_SET(output) THEN BEGIN
     answer=''
     print, Strjoin(['commie_initialize_dataset: Output filename: "', $
                    *commie_script_struct.output, '" already exists.', $
                    ' Overwrite?'])
     WHILE ((Strcmp(answer,'y') NE 1) AND $
            (Strcmp(answer,'n') NE 1)) DO BEGIN
        print,'Please answer (y/n).'
        answer = Get_Kbrd()
     ENDWHILE
     IF (Strcmp(answer,'n') EQ 1) THEN BEGIN
        name = ''
        answer = ''
        print, Strjoin(['Specify a new filename with sub-directory if ', $
                       'desired. It will automatically be appended to the ', $
                       'top-directory.'])
        name_label:
        Read,name
        WHILE ((Strcmp(answer,'y') NE 1) AND $
               (Strcmp(answer,'n') NE 1)) DO BEGIN
           name = Strjoin([*commie_script_struct.topdir, path_sep, $
                           Strtrim(name,2)])
           print,Strjoin(['New filename will be: "', name, $
                          '" Is this correct?'])
           print,'Please answer (y/n).'
           answer = Get_Kbrd()
        ENDWHILE
        IF (Strcmp(answer,'n') EQ 1) THEN BEGIN
           print,'Specify a new filename with sub-directory if desired.'
           answer=''
           goto,name_label
        ENDIF
        *commie_script_struct.output = name
     ENDIF
  ENDIF
END
