PRO commie_verbose_output, commie_dataset_struct, commie_script_struct, $
                           scriptfile_io=scriptfile_io, $
                           initialize_dataset=initialize_dataset, $
                           find_bs=find_bs,$
                           subtract_dks=subtract_dks, $
                           merge_by_exp=merge_by_exp,$
                           subtract_no_sample=subtract_no_sample, $
                           threshold_data=threshold_data, $
                           merge_dummy_arrays=merge_dummy_arrays, $
                           assemble_merged_pos=assemble_merged_pos, $
                           remove_bs=remove_bs, $
                           subtract_assembled_ns=subtract_assembled_ns, $
                           final_array=final_array
  
  compile_opt idl2, hidden

  ;; get the path separator first since it is always needed
  path_sep=path_sep()

  ;; keep each verbose output completely separate from other
  ;; routines. This way it is easier to incorporate changes for one
  ;; routine only afterwards. 
  IF Keyword_Set(scriptfile_io) THEN BEGIN
     ;; check if output to file is required
     IF (size(*commie_script_struct.verbose,/type) EQ 7) THEN BEGIN
        Catch, error_code
        IF (error_code NE 0) THEN BEGIN
           error_string = 'commie_scriptfile_io: Error opening file.'
           print,error_string
           Catch, /Cancel
           RETURN
        ENDIF
        ;; create the directory if necessary
        File_mkdir,File_dirname(*(commie_script_struct.verbose))
        ;; this will delete any existing file of the same name in this dir.
        OpenW, lun, *(commie_script_struct.verbose), /Get_Lun
        Catch, /Cancel
        
        Catch, error_code
        IF (error_code NE 0) THEN BEGIN
           error_string = 'commie_scriptfile_io: Error updating file.'
           Close, lun
           Free_lun, lun
           Catch, /Cancel
           RETURN
        ENDIF
     ENDIF ELSE BEGIN ;; specify lun of terminal
        lun = -1
     ENDELSE
     ;; see if the user requested to have sample removed individually
     ;; or in the end.
     IF commie_dataset_struct.merge_first THEN BEGIN
        printf, lun, 'No_sample subtracted in the very end'
        printf,lun, ''
     ENDIF ELSE BEGIN
        printf, lun, 'No_sample subtracted per position'
        printf,lun, ''
     ENDELSE

     ;; first start with the position names and associated motor values
     printf,lun, 'Assembly parameters from scriptfile or defaults:'
     printf,lun, 'Top directory: ', $
            Strtrim(*commie_script_struct.topdir,2)
     printf,lun, 'Motor position tolerance (meters): ', $
            Strtrim(*commie_script_struct.tolerance_meters,2)
     printf,lun, 'Saturation threshold low: ', $
            Strtrim(*commie_script_struct.sat_threshold_low,2)
     printf,lun, 'Raw data filetype: ', $
            Strtrim(*commie_script_struct.filetype,2)
     printf,lun, 'Median filter (percent): ', $
            Strtrim(*commie_script_struct.median_percent,2)
     ;; now close file if we need to 
     IF (size(*commie_script_struct.verbose,/type) EQ 7) THEN BEGIN
        Catch,/Cancel
        Close, lun
        free_lun, lun
     ENDIF
  ENDIF ELSE IF Keyword_Set(initialize_dataset) THEN BEGIN
     ;; check if output to file is required
     IF (size(*commie_script_struct.verbose,/type) EQ 7) THEN BEGIN
        Catch, error_code
        IF (error_code NE 0) THEN BEGIN
           error_string = 'commie_initialize_dataset: Error opening file.'
           print,error_string
           Catch, /Cancel
           RETURN
        ENDIF
        ;; create the directory if necessary
        File_mkdir,File_dirname(*(commie_script_struct.verbose))
        ;; this will delete any existing file of the same name in this dir.
        OpenU, lun, *(commie_script_struct.verbose), /Get_Lun, /Append
        Catch, /Cancel
        
        Catch, error_code
        IF (error_code NE 0) THEN BEGIN
           error_string = 'commie_initialize_dataset: Error updating file.'
           Close, lun
           Free_lun, lun
           Catch, /Cancel
           RETURN
        ENDIF
     ENDIF ELSE BEGIN ;; specify lun of terminal
        lun = -1
     ENDELSE
     ;; first start with the position names and associated motor
     ;; values
     printf,lun, 'Saturation threshold high: ', $
            Strtrim(*commie_script_struct.sat_threshold_high,2)
     printf,lun, 'Threshold factor to compensate normalization wrt current: ', $
            Strtrim(*commie_script_struct.thresh_data_factor,2)
     printf,lun,''
     printf,lun, 'Position names and associated motor values:'
     FOR i=0,n_elements(*(commie_dataset_struct.position_names))-1 DO BEGIN
        printf,lun, Strjoin(['Position ', $
                             (*(commie_dataset_struct.position_names))[i], $
                             ': '])
        FOR j=0,n_elements(*(commie_dataset_struct.motor_names))-1 DO BEGIN
           printf, lun, Strjoin(['     ', $
                                 (*(commie_dataset_struct.motor_names))[j], $
                 ': ', Strtrim((*(commie_dataset_struct.motor_values))[j,i],2)])
        ENDFOR
     ENDFOR
     printf,lun, ''
     ;; now print how sample/no_sample were associated with the positions
     printf,lun, 'Position indices for the sample/no_sample files:'
     FOR i=0,n_elements(*(commie_dataset_struct.filenames))-1 DO BEGIN
        this_index = (*(commie_dataset_struct.position_indices))[i]
        this_exp_index = (*(commie_dataset_struct.exp_indices))[i]
        printf,lun, Strjoin([(*(commie_dataset_struct.filenames))[i],$
                             ' has been associated with position: ',$
                         (*(commie_dataset_struct.position_names))[this_index]])
        printf,lun, '    It has an exposure time of ', $
               Strtrim((*commie_dataset_struct.exp_seconds)[this_exp_index],$
                       2),' sec.'
     ENDFOR
     printf,lun,''
     ;; now close file if we need to 
     IF (size(*commie_script_struct.verbose,/type) EQ 7) THEN BEGIN
        Catch,/Cancel
        Close, lun
        free_lun, lun
     ENDIF
  ENDIF ELSE IF Keyword_Set(find_bs) THEN BEGIN
     ;; check if output to file is desired
     IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
        Catch, error_code
        IF (error_code NE 0) THEN BEGIN
           error_string = 'commie_find_bs: Error opening file.'
           print,error_string
           Catch, /Cancel
           RETURN
        ENDIF
        ;; create the directory if necessary
        File_mkdir,File_dirname(*(commie_script_struct.verbose))
        ;; this will open the existing verbose file for updating
        OpenU, lun, *(commie_script_struct.verbose), /Get_Lun,/Append
        Catch, /Cancel
        
        Catch, error_code
        IF (error_code NE 0) THEN BEGIN
           error_string = 'commie_find_bs: Error updating file.'
           print, error_string
           Close, lun
           Free_lun, lun
           Catch, /Cancel
           RETURN
        ENDIF
     ENDIF ELSE BEGIN ;; specify lun of terminal
        lun = -1
     ENDELSE
     ;; print the bs_xs and bs_ys positions
     printf,lun,'Beamstop center positions (x, y) in pixels:'
     FOR i=0,n_elements(*commie_dataset_struct.position_names)-1 DO BEGIN
        printf,lun,Strjoin(['     ', $
                            (*(commie_dataset_struct.position_names))[i], $
                            ': ', Strtrim((*commie_dataset_struct.bs_xs)[i],2),$
                            ', ', Strtrim((*commie_dataset_struct.bs_ys)[i],2)])
     ENDFOR
     printf,lun,''
     ;; now close file if we need to 
     IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
        Catch,/Cancel
        Close, lun
        free_lun, lun
     ENDIF
     
     ;; now plot the array
     IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
        ;; define the path and create that direcory
        filepath = Strjoin([File_Dirname(*commie_script_struct.verbose),$
                            path_sep,'find_bs',path_sep])
        File_mkdir, filepath
        ;; Postscript output of plot
        ;; save pre-existing graphics devices and font
        old_plot = !d.name
        old_font = !p.font
        ;; go to PostScript and use native PostScript fonts (!p.font =0)
        set_plot, 'ps'
        !p.font = 0
        filename = Strjoin([filepath,'bs_array.eps'])
        device,file=filename,/encap, xsize=6.7,ysize=6.5,/inch
        Tvscl, Congrid((*commie_dataset_struct.bs_array),670,650)
        XYOuts, 1.2, 1.2, 'BS Array as determined by commie_find_bs.pro', $
                /Device, Charsize=0.8
        device, /close
        print, 'Wrote file "'+filename+'"'
        ;; go back to pre-existing graphics mode and font
        set_plot, old_plot
        !p.font = old_font
     ENDIF ELSE BEGIN
        window,/free,xsize=670,ysize=650, $
               title='BS as determined by commie_find_bs.pro'
        win_id = !D.Window
        TVScl, Congrid((*commie_dataset_struct.bs_array),670,650)
        print,'Destroy bs_array window (y/n)?'
        answer = Get_Kbrd()
        WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
           (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
           answer = Get_Kbrd()
        ENDWHILE
        IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN $
           wdelete, win_id
     ENDELSE
  ENDIF ELSE IF Keyword_Set(subtract_dks) THEN BEGIN
     ;; define the path if necessary
     IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
        filepath = Strjoin([File_Dirname(*commie_script_struct.verbose),$
                            path_sep,'subtract_dks',path_sep])
     ENDIF
     temp_indices = *(commie_dataset_struct.dummy_indices)
     FOR i=0,n_elements(*(commie_dataset_struct.exp_seconds))-1 DO BEGIN
        this_exp = (*commie_dataset_struct.exp_seconds)[i]
        these_samples = $
           Where(((*commie_dataset_struct.exp_indices)[temp_indices] EQ i) $
                 AND ((*commie_dataset_struct.are_samples)[temp_indices] EQ 1),$
                 scount)
        these_nsamples = $
           Where(((*commie_dataset_struct.exp_indices)[temp_indices] EQ i) $
                 AND ((*commie_dataset_struct.are_samples)[temp_indices] EQ 0),$
                 nscount)
        titles = $
           Strjoin(['Exemplary sample exposure of ', $
                    Strtrim(this_exp,2), $
                    ' seconds after DK has been subtracted - log scale'])
        titlens = $
           Strjoin(['Exemplary no_sample exposure of ', $
                    Strtrim(this_exp,2), $
                    ' seconds after DK has been subtracted - log scale'])
        ;; prepare the arrays to be plotted
        IF scount NE 0 THEN BEGIN
           sarr = *((*commie_dataset_struct.dummy_arrays)[these_samples[0]])
           negs = Where(sarr LT 0, neg_count)
           IF neg_count NE 0 THEN $
              sarr[negs] = 0
           sarr += 0.001
           sarr = Alog10(Congrid(sarr,670,650))
        ENDIF
        IF nscount NE 0 THEN BEGIN
           nsarr = *((*commie_dataset_struct.dummy_arrays)[these_nsamples[0]])
           negs = Where(nsarr LT 0, neg_count)
           IF neg_count NE 0 THEN $
              nsarr[negs] = 0
           nsarr += 0.001
           nsarr = Alog10(Congrid(nsarr,670,650))
        ENDIF
        ;; now distinguish between terminal output or saving
        IF size(*commie_script_struct.verbose,/type) EQ 2 THEN BEGIN
           IF scount NE 0 THEN BEGIN
              ;; display the first array
              Window,/free,xsize=670,ysize=650,title=titles
              swin = !D.Window
              TVScl, sarr
           ENDIF
           IF nscount NE 0 THEN BEGIN
              ;; display the first array
              Window,/free,xsize=670,ysize=650,title=titlens
              nswin = !D.Window
              TVScl, nsarr
           ENDIF
           ;; Prompt user to destroy windows
           IF ((scount NE 0) AND (nscount NE 0)) THEN BEGIN
              print,'Destroy these windows (y/n)?'
              answer = Get_Kbrd()
              WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
                 (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
                 answer = Get_Kbrd()
              ENDWHILE
              IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN $
                 wdelete, swin, nswin
           ENDIF ELSE IF scount NE 0 THEN BEGIN
              print,'Destroy this window (y/n)?'
              answer = Get_Kbrd()
              WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
                 (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
                 answer = Get_Kbrd()
              ENDWHILE
              IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN $
                 wdelete, swin
           ENDIF ELSE IF nscount NE 0 THEN BEGIN
              print,'Destroy this window (y/n)?'
              answer = Get_Kbrd()
              WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
                 (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
                 answer = Get_Kbrd()
              ENDWHILE
              IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN $
                 wdelete, nswin
           ENDIF
        ENDIF ELSE IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
           ;; create the folder 
           File_mkdir, filepath

           ;; in this case we would like to save to disk. Make sure we
           ;; incoporate positions into filenames!!
           ;; Postscript output of plot save pre-existing graphics
           ;; devices and font.
           old_plot = !d.name
           old_font = !p.font
           ;; go to PostScript and use native PostScript fonts (!p.font =0)
           set_plot, 'ps'
           !p.font = 0
           ;; set the colors
           TVLCT, 255,255,255,100 ;white
           
           ;; see if we have to save a sample file
           IF scount NE 0 THEN BEGIN
              ;; Get the current position
              this_i = temp_indices[these_samples[0]]
              pos_i=(*commie_dataset_struct.position_indices)[this_i]
              this_pos = (*commie_dataset_struct.position_names)[pos_i]
              filename = $
                 Strjoin([filepath, this_pos , $
                          '_sample_', Strtrim(this_exp,2),'.eps'])
              device,/color
              device,file=filename,/encap, xsize=6.7,ysize=6.5,/inch
              Tvscl, sarr
              string = $
                 Strjoin(['Exemplary sample after subtract_dks. Exposure time',$
                          ' of ',Strtrim(this_exp,2),' seconds.'])
              XYOuts, 1.2, 1.2, string, /Device, Charsize=0.8,color=100
              device, /close
              print, 'Wrote file "'+filename+'"'
           ENDIF
           ;; see if we have to save a no_sample file as well.
           IF nscount NE 0 THEN BEGIN
              ;; Get the current position
              this_i = temp_indices[these_nsamples[0]]
              pos_i=(*commie_dataset_struct.position_indices)[this_i]
              this_pos = (*commie_dataset_struct.position_names)[pos_i]
              filename = $
                 Strjoin([filepath,this_pos , $
                          '_no_sample_', Strtrim(this_exp,2), '.eps'])
              device,/color
              device,file=filename,/encap, xsize=6.7,ysize=6.5,/inch
              Tvscl, nsarr
              string = $
                 Strjoin(['Exemplary no_sample after subtract_dks. Exposure ', $
                          'time of ',Strtrim(this_exp,2),' seconds.'])
              XYOuts, 1.2, 1.2, string, /Device, Charsize=0.8,color=100
              device, /close
              print, 'Wrote file "'+filename+'"'
           ENDIF
           ;; go back to pre-existing graphics mode and font
           set_plot, old_plot
           !p.font = old_font
        ENDIF
     ENDFOR
  ENDIF ELSE IF Keyword_Set(merge_by_exp) THEN BEGIN
     ;; define the path if necessary
     IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
        filepath = Strjoin([File_Dirname(*commie_script_struct.verbose),$
                            path_sep,'merge_by_exp',path_sep])
     ENDIF
     ;; loop through the exposure times.
     FOR i=0,n_elements(*commie_dataset_struct.exp_seconds)-1 DO BEGIN
        this_exp = (*commie_dataset_struct.exp_seconds)[i]
        temp_indices = (*commie_dataset_struct.dummy_indices)
        these_samples = $
           Where(((*commie_dataset_struct.exp_indices)[temp_indices] EQ i) $
                AND ((*commie_dataset_struct.are_samples)[temp_indices] EQ 1),$
                scount)
        these_nsamples = $
           Where(((*commie_dataset_struct.exp_indices)[temp_indices] EQ i) $
                AND ((*commie_dataset_struct.are_samples)[temp_indices] EQ 0),$
                 nscount)
        titles = $
           Strjoin(['Merged all samples with ', $
                    Strtrim(this_exp,2), $
                    ' seconds exposure - log scale'])
        titlens = $
           Strjoin(['Merged all no_samples with ', $
                    Strtrim(this_exp,2), $
                    ' seconds exposure - log scale'])

        ;; prepare the arrays to be plotted
        IF scount NE 0 THEN BEGIN
           sarr = *((*commie_dataset_struct.dummy_arrays)[these_samples[0]])
           negs = Where(sarr LT 0, neg_count)
           IF neg_count NE 0 THEN $
              sarr[negs] = 0
           sarr += 0.001
           sarr = Alog10(Congrid(sarr,670,650))
        ENDIF
        IF nscount NE 0 THEN BEGIN
           nsarr = *((*commie_dataset_struct.dummy_arrays)[these_nsamples[0]])
           negs = Where(nsarr LT 0, neg_count)
           IF neg_count NE 0 THEN $
              nsarr[negs] = 0
           nsarr += 0.001
           nsarr = Alog10(Congrid(nsarr,670,650))
        ENDIF
        ;; now distinguish between terminal output or saving
        IF size(*commie_script_struct.verbose,/type) EQ 2 THEN BEGIN
           IF scount NE 0 THEN BEGIN
              ;; display the first array
              Window,/free,xsize=670,ysize=650,title=titles
              swin = !D.Window
              TVScl, sarr
           ENDIF
           IF nscount NE 0 THEN BEGIN
              ;; display the first array
              Window,/free,xsize=670,ysize=650,title=titlens
              nswin = !D.Window
              TVScl, nsarr
           ENDIF
           ;; Prompt user to destroy windows
           IF ((scount NE 0) AND (nscount NE 0)) THEN BEGIN
              print,'Destroy these windows (y/n)?'
              answer = Get_Kbrd()
              WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
                 (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
                 answer = Get_Kbrd()
              ENDWHILE
              IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN $
                 wdelete, swin, nswin
           ENDIF ELSE IF scount NE 0 THEN BEGIN
              print,'Destroy this window (y/n)?'
              answer = Get_Kbrd()
              WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
                 (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
                 answer = Get_Kbrd()
              ENDWHILE
              IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN $
                 wdelete, swin
           ENDIF ELSE IF nscount NE 0 THEN BEGIN
              print,'Destroy this window (y/n)?'
              answer = Get_Kbrd()
              WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
                 (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
                 answer = Get_Kbrd()
              ENDWHILE
              IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN $
                 wdelete, nswin
           ENDIF
        ENDIF ELSE IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
           ;; create the folder
           File_mkdir, filepath

           ;; in this case we would like to save to disk. Make sure we
           ;; incoporate positions into filenames!!
           ;; Postscript output of plot save pre-existing graphics
           ;; devices and font. 

           old_plot = !d.name
           old_font = !p.font
           ;; go to PostScript and use native PostScript fonts (!p.font =0)
           set_plot, 'ps'
           !p.font = 0
           ;; set the colors
           TVLCT, 255,255,255,100 ;white

           ;; see if we have to save a sample file
           IF scount NE 0 THEN BEGIN
	      ;; Get the current position
	      this_i = temp_indices[these_samples[0]]
	      pos_i=(*commie_dataset_struct.position_indices)[this_i]
              this_pos = (*commie_dataset_struct.position_names)[pos_i]
	      filename = $
                 Strjoin([filepath,this_pos , $
                          '_sample_', Strtrim(this_exp,2),'.eps'])
              device,/color
              device,file=filename,/encap, xsize=6.7,ysize=6.5,/inch
              Tvscl, sarr
              string = $
                Strjoin(['Merged samples with exposure time',$
                          ' of ',Strtrim(this_exp,2),' seconds.'])
              XYOuts, 1.2, 1.2, string, /Device, Charsize=0.8,color=100
              device, /close
              print, 'Wrote file "'+filename+'"'
           ENDIF
           ;; see if we have to save a no_sample file as well.
           IF nscount NE 0 THEN BEGIN
 	      ;; Get the current position
	      this_i = temp_indices[these_nsamples[0]]
	      pos_i=(*commie_dataset_struct.position_indices)[this_i]
              this_pos = (*commie_dataset_struct.position_names)[pos_i]
	      filename = $
                 Strjoin([filepath,this_pos , $
                          '_no_sample_', Strtrim(this_exp,2), '.eps'])
              device,/color
              device,file=filename,/encap, xsize=6.7,ysize=6.5,/inch
              Tvscl, nsarr
              string = $
                Strjoin(['Merged no_samples with exposure ', $
                          'time of ',Strtrim(this_exp,2),' seconds.'])
              XYOuts, 1.2, 1.2, string, /Device, Charsize=0.8,color=100
              device, /close
              print, 'Wrote file "'+filename+'"'
           ENDIF
           ;; go back to pre-existing graphics mode and font
           set_plot, old_plot
           !p.font = old_font
        ENDIF
     ENDFOR
  ENDIF ELSE IF Keyword_Set(subtract_no_sample) THEN BEGIN
     ;; define the path if necessary
     IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
        filepath = Strjoin([File_Dirname(*commie_script_struct.verbose),$
                            path_sep,'subtract_no_sample',path_sep])
     ENDIF
     these_indices = (*commie_dataset_struct.dummy_indices)
     FOR i=0,(n_elements(*(commie_dataset_struct.exp_seconds))-1) DO BEGIN
        ;; no_sample has already been subtracted, so only sample is left
        sample_indices = $
           Where(((*(commie_dataset_struct.exp_indices))[these_indices]) EQ i, $
                 count)
        ;; not every position has all exposure times
        IF count NE 0 THEN BEGIN
           ;; look at the first example only
           this_sample = $
              *((*commie_dataset_struct.dummy_arrays)[sample_indices[0]])
           
           ;; get the exposure time
           this_exp = (*commie_dataset_struct.exp_seconds)[i]
           titles = $
              Strjoin(['Exemplary sample exposure of ', $
                       Strtrim(this_exp,2), $
                       ' seconds after no_sample has been subtracted', $
                       '- log scale'])
           
           ;; prepare the arrays to be plotted
           negs = Where(this_sample LT 0, neg_count)
           IF neg_count NE 0 THEN $
              this_sample[negs] = 0
           this_sample += 0.001
           this_sample = Alog10(Congrid(this_sample,670,650))
           
           ;; now distinguish between terminal output or saving
           IF size(*commie_script_struct.verbose,/type) EQ 2 THEN BEGIN
              ;; display the first array
              Window,/free,xsize=670,ysize=650,title=titles
              swin = !D.Window
              TVScl, this_sample
              
              ;; Prompt user to destroy window
              print,'Destroy this window (y/n)?'
              answer = Get_Kbrd()
              WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
                 (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
                 answer = Get_Kbrd()
              ENDWHILE
              IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN $
                 wdelete, swin
           ENDIF ELSE IF size(*commie_script_struct.verbose,/type) EQ 7 $
           THEN BEGIN
              ;; create the folder
              File_mkdir, filepath
              
              ;; in this case we would like to save to disk. Make sure we
              ;; incoporate positions into filenames!!
              ;; Postscript output of plot save pre-existing graphics
              ;; devices and font. 
              old_plot = !d.name
              old_font = !p.font
              ;; go to PostScript and use native PostScript fonts
              ;; (!p.font =0) 
              set_plot, 'ps'
              !p.font = 0
              ;; set the colors
              TVLCT, 255,255,255,100 ;white
              
              ;; Get the current position
              this_i = these_indices[sample_indices[0]]
              pos_i=(*commie_dataset_struct.position_indices)[this_i]
              this_pos = (*commie_dataset_struct.position_names)[pos_i]
              filename = $
                 Strjoin([filepath,this_pos , $
                          '_', Strtrim(this_exp,2),'.eps'])
              device,/color
              device,file=filename,/encap, xsize=6.7,ysize=6.5,/inch
              Tvscl, this_sample
              string = $
                 Strjoin(['Exemplary sample with subtracted no_sample.', $
                          'Exposure time of ',Strtrim(this_exp,2), $
                          ' seconds.'])
              XYOuts, 1.2, 1.2, string, /Device, Charsize=0.8,color=100
              device, /close
              print, 'Wrote file "'+filename+'"'
              
              ;; go back to pre-existing graphics mode and font
              set_plot, old_plot
              !p.font = old_font
           ENDIF       ;; verbose-type EQ 7
        ENDIF
     ENDFOR
  ENDIF ELSE IF Keyword_Set(threshold_data) THEN BEGIN
     ;; define the path if necessary
     IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
        filepath = Strjoin([File_Dirname(*commie_script_struct.verbose),$
                            path_sep,'threshold_data',path_sep])
     ENDIF
     ;; loop through the exposure times.
     FOR i=0,n_elements(*commie_dataset_struct.exp_seconds)-1 DO BEGIN
        this_exp = (*commie_dataset_struct.exp_seconds)[i]
        temp_indices = (*commie_dataset_struct.dummy_indices)
	these_samples = $
           Where(((*commie_dataset_struct.exp_indices)[temp_indices] EQ i) $
                AND ((*commie_dataset_struct.are_samples)[temp_indices] EQ 1),$
                scount)
        these_nsamples = $
           Where(((*commie_dataset_struct.exp_indices)[temp_indices] EQ i) $
                AND ((*commie_dataset_struct.are_samples)[temp_indices] EQ 0),$
                 nscount)
        titles = $
           Strjoin(['Thresholded all samples with ', $
                    Strtrim(this_exp,2), $
                    ' seconds exposure - log scale'])
        titlens = $
           Strjoin(['Thresholded all no_samples with ', $
                    Strtrim(this_exp,2), $
                    ' seconds exposure - log scale'])

        ;; prepare the arrays to be plotted
        IF scount NE 0 THEN BEGIN
           sarr = *((*commie_dataset_struct.dummy_arrays)[these_samples[0]])
           negs = Where(sarr LT 0, neg_count)
           IF neg_count NE 0 THEN $
              sarr[negs] = 0
           sarr += 0.001
           sarr = Alog10(Congrid(sarr,670,650))
        ENDIF
        IF nscount NE 0 THEN BEGIN
           nsarr = *((*commie_dataset_struct.dummy_arrays)[these_nsamples[0]])
           negs = Where(nsarr LT 0, neg_count)
           IF neg_count NE 0 THEN $
              nsarr[negs] = 0
           nsarr += 0.001
           nsarr = Alog10(Congrid(nsarr,670,650))
        ENDIF
       
        ;; now distinguish between terminal output or saving
        IF size(*commie_script_struct.verbose,/type) EQ 2 THEN BEGIN
           IF scount NE 0 THEN BEGIN
              ;; display the first array
              Window,/free,xsize=670,ysize=650,title=titles
              swin = !D.Window
              TVScl, sarr
           ENDIF
           IF nscount NE 0 THEN BEGIN
              ;; display the first array
              Window,/free,xsize=670,ysize=650,title=titlens
              nswin = !D.Window
              TVScl, nsarr
           ENDIF
           
           ;; Prompt user to destroy windows
           IF ((scount NE 0) AND (nscount NE 0)) THEN BEGIN
              print,'Destroy these windows (y/n)?'
              answer = Get_Kbrd()
              WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
                 (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
                 answer = Get_Kbrd()
              ENDWHILE
              IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN BEGIN
                 wdelete, swin, nswin
              ENDIF
           ENDIF ELSE IF scount NE 0 THEN BEGIN
              print,'Destroy these windows (y/n)?'
              answer = Get_Kbrd()
              WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
                 (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
                 answer = Get_Kbrd()
              ENDWHILE
              IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN BEGIN
                 wdelete, swin
              ENDIF
           ENDIF ELSE IF nscount NE 0 THEN BEGIN
              print,'Destroy these windows (y/n)?'
              answer = Get_Kbrd()
              WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
                 (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
                 answer = Get_Kbrd()
              ENDWHILE
              IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN BEGIN
                 wdelete, nswin
              ENDIF
           ENDIF
        ENDIF ELSE IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
           ;; create the folder
           File_mkdir, filepath

           ;; in this case we would like to save to disk. Make sure we
           ;; incoporate positions into filenames!!
           ;; Postscript output of plot save pre-existing graphics
           ;; devices and font.
           old_plot = !d.name
           old_font = !p.font
           ;; go to PostScript and use native PostScript fonts (!p.font =0)
           set_plot, 'ps'
           !p.font = 0
           ;; set the colors
           TVLCT, 255,255,255,100 ;white

           ;; see if we have to save a sample file
           IF scount NE 0 THEN BEGIN
	      ;; Get the current position
	      this_i = temp_indices[these_samples[0]]
	      pos_i=(*commie_dataset_struct.position_indices)[this_i]
              this_pos = (*commie_dataset_struct.position_names)[pos_i]
	      filename = $
                 Strjoin([filepath,this_pos ,$
                          '_sample_', Strtrim(this_exp,2),'.eps'])
              device,/color
              device,file=filename,/encap, xsize=6.7,ysize=6.5,/inch
              Tvscl, sarr
              string = $
                Strjoin(['Thresholded sample with exposure time',$
                          ' of ',Strtrim(this_exp,2),' seconds.'])
              XYOuts, 1.2, 1.2, string, /Device, Charsize=0.8,color=100
              device, /close
              print, 'Wrote file "'+filename+'"'
           ENDIF
           ;; see if we have to save a no_sample file as well.
           IF nscount NE 0 THEN BEGIN
 	      ;; Get the current position
	      this_i = temp_indices[these_nsamples[0]]
	      pos_i=(*commie_dataset_struct.position_indices)[this_i]
              this_pos = (*commie_dataset_struct.position_names)[pos_i]
	      filename = $
                 Strjoin([filepath,this_pos , $
                          '_no_sample_', Strtrim(this_exp,2), '.eps'])
              device,/color
              device,file=filename,/encap, xsize=6.7,ysize=6.5,/inch
              Tvscl, nsarr
              string = $
                Strjoin(['Thresholded no_sample with exposure ', $
                          'time of ',Strtrim(this_exp,2),' seconds.'])
              XYOuts, 1.2, 1.2, string, /Device, Charsize=0.8,color=100
              device, /close
              print, 'Wrote file "'+filename+'"'
           ENDIF
           ;; go back to pre-existing graphics mode and font
           set_plot, old_plot
           !p.font = old_font
        ENDIF
     ENDFOR
  ENDIF ELSE IF Keyword_Set(merge_dummy_arrays) THEN BEGIN
     ;; there are only one or two arrays (if no_sample hasn't
     ;; been subtracted yet) left.
     ;; Get the position
     these_indices = (*commie_dataset_struct.dummy_indices)
     no_samples = $
        Where((*(commie_dataset_struct.are_samples))[these_indices] EQ 0, $
              ns_count)
     pos_i = (*commie_dataset_struct.position_indices)[these_indices[0]]
     this_pos = (*commie_dataset_struct.position_names)[pos_i]
     titles = $
        Strjoin(['Merged all samples of position ', Strtrim(this_pos,2)])
     titlens = $
        Strjoin(['Merged all no_samples of position ', Strtrim(this_pos,2)])

     ;; prepare the arrays to be plotted
     IF ns_count EQ 0 THEN BEGIN
        sarr = *commie_dataset_struct.dummy_arrays
     ENDIF ELSE BEGIN
        sarr = *((*commie_dataset_struct.dummy_arrays)[0])
     ENDELSE
     negs = Where(sarr LT 0, neg_count)
     IF neg_count NE 0 THEN $
        sarr[negs] = 0
     sarr += 0.001
     sarr = Alog10(Congrid(sarr,670,650))

     IF ns_count NE 0 THEN BEGIN
        nsarr = *((*commie_dataset_struct.dummy_arrays)[1])
        negs = Where(nsarr LT 0, neg_count)
        IF neg_count NE 0 THEN $
           nsarr[negs] = 0
        nsarr += 0.001
        nsarr = Alog10(Congrid(nsarr,670,650))
     ENDIF

     ;; now distinguish between terminal output or saving
     IF size(*commie_script_struct.verbose,/type) EQ 2 THEN BEGIN
        ;; display the first array
        Window,/free,xsize=670,ysize=650,title=titles
        swin = !D.Window
        TVScl, sarr
        IF ns_count NE 0 THEN BEGIN
           ;; display the first array
           Window,/free,xsize=670,ysize=650,title=titlens
           nswin = !D.Window
           TVScl, nsarr
        ENDIF

        ;; Prompt user to destroy windows
        IF (ns_count NE 0) THEN BEGIN
           print,'Destroy these windows (y/n)?'
           answer = Get_Kbrd()
           WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
              (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
              answer = Get_Kbrd()
           ENDWHILE
           IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN $
              wdelete, swin, nswin
        ENDIF ELSE BEGIN
           print,'Destroy this window (y/n)?'
           answer = Get_Kbrd()
           WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
              (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
              answer = Get_Kbrd()
           ENDWHILE
           IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN $
              wdelete, swin
        ENDELSE
     ENDIF ELSE IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
        ;; define the path and create that direcory
        filepath = Strjoin([File_Dirname(*commie_script_struct.verbose),$
                            path_sep,'merge_dummy_arrays',path_sep])
        File_mkdir, filepath
        ;; in this case we would like to save to disk. Make sure we
        ;; incoporate positions into filenames!!
        ;; Postscript output of plot save pre-existing graphics
        ;; devices and font.
        old_plot = !d.name
        old_font = !p.font
        ;; go to PostScript and use native PostScript fonts (!p.font =0)
        set_plot, 'ps'
        !p.font = 0
        ;; set the colors
        TVLCT, 255,255,255,100  ;white
        
        filename = $
           Strjoin([filepath,this_pos,'_samples.eps'])
        device,/color
        device,file=filename,/encap, xsize=6.7,ysize=6.5,/inch
        Tvscl, sarr
        string = $
           Strjoin(['Merged all samples of position ',Strtrim(this_pos,2), $
                   '. Scaling steps are on purpose.'])
        XYOuts, 1.2, 1.2, string, /Device, Charsize=0.8,color=100
        device, /close
        print, 'Wrote file "'+filename+'"'

        ;; see if we have to save a no_sample file as well.
        IF ns_count NE 0 THEN BEGIN
           filename = $
              Strjoin([filepath,this_pos, '_no_samples.eps'])
           device,/color
           device,file=filename,/encap, xsize=6.7,ysize=6.5,/inch
           Tvscl, nsarr
           string = $
             Strjoin(['Merged all no_samples of position ',Strtrim(this_pos,2),$
                     '. Scaling steps are on purpose.'])
           XYOuts, 1.2, 1.2, string, /Device, Charsize=0.8,color=100
           device, /close
           print, 'Wrote file "'+filename+'"'
        ENDIF
        ;; go back to pre-existing graphics mode and font
        set_plot, old_plot
        !p.font = old_font
     ENDIF
  ENDIF ELSE IF Keyword_Set(assemble_merged_pos) THEN BEGIN
     ;; define the path if necessary
     IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
        filepath = Strjoin([File_Dirname(*commie_script_struct.verbose),$
                            path_sep,'assemble_merged_pos',path_sep])
        fsname = $
           Strjoin([filepath,'sample.eps'])
        fnsname = $
           Strjoin([filepath,'no_sample.eps'])     
     ENDIF

     titles = $
        Strjoin(['Sample - after assembling all.'])
     titlens = $
        Strjoin(['No_sample - after assembling all.'])
   
    ;; prepare the arrays to be plotted
    sarr = *commie_dataset_struct.assembled_sample
    negs = Where(sarr LT 0, neg_count)
    IF neg_count NE 0 THEN $
       sarr[negs] = 0
    sarr += 0.001
    sarr = Alog10(Congrid(sarr,670,650))
    
    IF ptr_valid(commie_dataset_struct.assembled_nosample) THEN BEGIN
       nsarr = *commie_dataset_struct.assembled_nosample
       negs = Where(nsarr LT 0, neg_count)
       IF neg_count NE 0 THEN $
          nsarr[negs] = 0
       nsarr += 0.001
       nsarr = Alog10(Congrid(nsarr,670,650))
    ENDIF

    ;; now distinguish between terminal output or saving
    IF size(*commie_script_struct.verbose,/type) EQ 2 THEN BEGIN
       ;; display the first array
       Window,/free,xsize=670,ysize=650,title=titles
       swin = !D.Window
       TVScl, sarr

       ;; check for no_sample
       IF ptr_valid(commie_dataset_struct.assembled_nosample) THEN BEGIN
          ;; display the first array
          Window,/free,xsize=670,ysize=650,title=titlens
          nswin = !D.Window
          TVScl, nsarr          

          print,'Destroy these windows (y/n)?'
          answer = Get_Kbrd()
          WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
             (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
             answer = Get_Kbrd()
          ENDWHILE
          IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN $
             wdelete, nswin, swin
       ENDIF ELSE BEGIN
          print,'Destroy these windows (y/n)?'
          answer = Get_Kbrd()
          WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
             (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
             answer = Get_Kbrd()
          ENDWHILE
          IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN $
             wdelete, swin
       ENDELSE      
    ENDIF ELSE IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
       ;; create the folder
       File_mkdir, filepath

       ;; in this case we would like to save to disk. Make sure we
       ;; incoporate positions into filenames!!
       ;; Postscript output of plot save pre-existing graphics
       ;; devices and font.       
       old_plot = !d.name
       old_font = !p.font
       ;; go to PostScript and use native PostScript fonts (!p.font =0)
       set_plot, 'ps'
       !p.font = 0
       ;; set the colors
       TVLCT, 255,255,255,100   ;white
       device,/color
       device,file=fsname,/encap, xsize=6.7,ysize=6.5,/inch
       Tvscl, sarr
       XYOuts, 1.2, 1.2, titles, /Device, Charsize=0.8,color=100
       device, /close
       print, 'Wrote file "'+fsname+'"'
       
       ;; see if we have to save a no_sample file as well.
       IF ptr_valid(commie_dataset_struct.assembled_nosample) THEN BEGIN
          device,file=fnsname,/encap, xsize=6.7,ysize=6.5,/inch
          device,/color
          Tvscl, nsarr
          XYOuts, 1.2, 1.2, titlens, /Device, Charsize=0.8,color=100
          device, /close
          print, 'Wrote file "'+fnsname+'"'
       ENDIF
       ;; go back to pre-existing graphics mode and font
       set_plot, old_plot
       !p.font = old_font
    ENDIF
 ENDIF ELSE IF Keyword_Set(remove_bs) THEN BEGIN
    ;; define the path if necessary
    IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
       filepath = Strjoin([File_Dirname(*commie_script_struct.verbose),$
                           path_sep,'remove_bs',path_sep])
    ENDIF
    ;; loop through the positions.
    FOR i=0,n_elements(*commie_dataset_struct.pos_merged_samples)-1 DO BEGIN
       this_pos = (*commie_dataset_struct.position_names)[i]
       titles = $
          Strjoin(['Samples of position ',Strtrim(this_pos,2), ' with BS', $
                   ' removed.'])
       IF ptr_valid((*commie_dataset_struct.pos_merged_nosamples)[0]) $
       THEN BEGIN
          titlens = $
             Strjoin(['No_samples of position ',Strtrim(this_pos,2), $
                      ' with BS removed.'])
       ENDIF
       ;; prepare the arrays to be plotted
       sarr = *((*commie_dataset_struct.pos_merged_samples)[i])
       negs = Where(sarr LT 0, neg_count)
       IF neg_count NE 0 THEN $
          sarr[negs] = 0
       sarr += 0.001
       sarr = Alog10(Congrid(sarr,670,650))
       
       IF ptr_valid((*commie_dataset_struct.pos_merged_nosamples)[0]) $
       THEN BEGIN
          nsarr = *((*commie_dataset_struct.pos_merged_nosamples)[i])
          negs = Where(nsarr LT 0, neg_count)
          IF neg_count NE 0 THEN $
             nsarr[negs] = 0
          nsarr += 0.001
          nsarr = Alog10(Congrid(nsarr,670,650))
       ENDIF
       ;; now distinguish between terminal output or saving
       IF size(*commie_script_struct.verbose,/type) EQ 2 THEN BEGIN
          ;; display the first array
          Window,/free,xsize=670,ysize=650,title=titles
          swin = !D.Window
          TVScl, sarr
          
          IF ptr_valid((*commie_dataset_struct.pos_merged_nosamples)[0]) $
          THEN BEGIN
             ;; display the first array
             Window,/free,xsize=670,ysize=650,title=titlens
             nswin = !D.Window
             TVScl, nsarr
             
             ;; user to destroy windows
             print,'Destroy these windows (y/n)?'
             answer = Get_Kbrd()
             WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
                (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
                answer = Get_Kbrd()
             ENDWHILE
             IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN $
                wdelete, swin, nswin
          ENDIF ELSE BEGIN
             print,'Destroy this window (y/n)?'
             answer = Get_Kbrd()
             WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
                (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
                answer = Get_Kbrd()
             ENDWHILE
             IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN $
                wdelete, swin
          ENDELSE
       ENDIF ELSE IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
          ;; create the directory
          File_mkdir, filepath
          
          ;; in this case we would like to save to disk. Make sure we
          ;; incoporate positions into filenames!!
          ;; Postscript output of plot save pre-existing graphics
          ;; devices and font. 
          old_plot = !d.name
          old_font = !p.font
          ;; go to PostScript and use native PostScript fonts (!p.font =0)
          set_plot, 'ps'
          !p.font = 0
          ;; set the colors
          TVLCT, 255,255,255,100 ;white
          
          filename = Strjoin([filepath,this_pos ,'_sample.eps'])
          device,/color
          device,file=filename,/encap, xsize=6.7,ysize=6.5,/inch
          Tvscl, sarr
          string = $
             Strjoin(['Merged samples of position ',Strtrim(this_pos,2), $
                      ' with bs removed.'])
          XYOuts, 1.2, 1.2, string, /Device, Charsize=0.8,color=100
          device, /close
          print, 'Wrote file "'+filename+'"'
          
          ;; see if we have to save a no_sample file as well.
          IF ptr_valid((*commie_dataset_struct.pos_merged_nosamples)[0]) $
          THEN BEGIN
             filename = $
                Strjoin([filepath,this_pos ,'_no_samples.eps'])
             device,/color
             device,file=filename,/encap, xsize=6.7,ysize=6.5,/inch
             Tvscl, nsarr
             string = $
                Strjoin(['Merged samples of position ',Strtrim(this_pos,2), $
                         ' with bs removed.'])
             XYOuts, 1.2, 1.2, string, /Device, Charsize=0.8,color=100
             device, /close
             print, 'Wrote file "'+filename+'"'
          ENDIF
          ;; go back to pre-existing graphics mode and font
          set_plot, old_plot
          !p.font = old_font
       ENDIF
    ENDFOR
 ENDIF ELSE IF Keyword_Set(subtract_assembled_ns) THEN BEGIN
    ;; define the path if necessary
    IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
       filepath = Strjoin([File_Dirname(*commie_script_struct.verbose),$
                           path_sep,'subtract_assembled_ns',path_sep])
       fsname = $
          Strjoin([filepath,'sample.eps'])
    ENDIF
    
    titles = $
       Strjoin(['Assembled sample - after subtracting nosample.'])
    
    ;; prepare the arrays to be plotted
    sarr = *commie_dataset_struct.assembled_sample
    negs = Where(sarr LT 0, neg_count)
    IF neg_count NE 0 THEN $
       sarr[negs] = 0
    sarr += 0.001
    sarr = Alog10(Congrid(sarr,670,650))
    
    ;; now distinguish between terminal output or saving
    IF size(*commie_script_struct.verbose,/type) EQ 2 THEN BEGIN
       ;; display the first array
       Window,/free,xsize=670,ysize=650,title=titles
       swin = !D.Window
       TVScl, sarr
       
       ;; Prompt user to destroy windows
       print,'Destroy this window (y/n)?'
       answer = Get_Kbrd()
       WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
          (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
          answer = Get_Kbrd()
       ENDWHILE
       IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN $
          wdelete, swin
    ENDIF ELSE IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
       ;; create the folder
       File_mkdir, filepath
       
       ;; in this case we would like to save to disk. Make sure we
       ;; incoporate positions into filenames!!
       ;; Postscript output of plot save pre-existing graphics
       ;; devices and font.
       old_plot = !d.name
       old_font = !p.font
       ;; go to PostScript and use native PostScript fonts (!p.font =0)
       set_plot, 'ps'
       !p.font = 0
       ;; set the colors
       TVLCT, 255,255,255,100   ;white
       
       device,/color
       device,file=fsname,/encap, xsize=6.7,ysize=6.5,/inch
       Tvscl, sarr
       XYOuts, 1.2, 1.2, titles, /Device, Charsize=0.8,color=100
       device, /close
       print, 'Wrote file "'+fsname+'"'
       
       ;; go back to pre-existing graphics mode and font
       set_plot, old_plot
       !p.font = old_font
    ENDIF
 ENDIF ELSE IF Keyword_Set(final_array) THEN BEGIN
    ;; define the path if necessary
    IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
       filepath = Strjoin([File_Dirname(*commie_script_struct.verbose),$
                           path_sep,'final_array',path_sep])
       fsname = $
          Strjoin([filepath,'final_sample.eps'])
    ENDIF
    titles = $
       Strjoin(['Assembled sample - final result.'])
    
    ;; prepare the arrays to be plotted
    sarr = *commie_dataset_struct.assembled_sample
    negs = Where(sarr LT 0, neg_count)
    IF neg_count NE 0 THEN $
       sarr[negs] = 0
    sarr += 0.001
    sarr = Alog10(Congrid(sarr,670,650))
    
    ;; now distinguish between terminal output or saving
    IF size(*commie_script_struct.verbose,/type) EQ 2 THEN BEGIN
       ;; display the first array
       Window,/free,xsize=670,ysize=650,title=titles
       swin = !D.Window
       TVScl, sarr
       
       ;; Prompt user to destroy windows
       print,'Destroy this window (y/n)?'
       answer = Get_Kbrd()
       WHILE (Strcmp(answer,'n',/Fold_case) EQ 0) AND $
          (Strcmp(answer,'y',/Fold_case) EQ 0) DO BEGIN
          answer = Get_Kbrd()
       ENDWHILE
       IF Strcmp(answer,'y',/Fold_Case) EQ 1 THEN $
          wdelete, swin
    ENDIF ELSE IF size(*commie_script_struct.verbose,/type) EQ 7 THEN BEGIN
       ;; create the folder
       File_mkdir, filepath
       
       ;; in this case we would like to save to disk. Make sure we
       ;; incoporate positions into filenames!!
       ;; Postscript output of plot save pre-existing graphics
       ;; devices and font.         
       old_plot = !d.name
       old_font = !p.font
       ;; go to PostScript and use native PostScript fonts (!p.font =0)
       set_plot, 'ps'
       !p.font = 0
       ;; set the colors
       TVLCT, 255,255,255,100   ;white
       
       device,/color
       device,file=fsname,/encap, xsize=6.7,ysize=6.5,/inch
       Tvscl, sarr
       XYOuts, 1.2, 1.2, titles, /Device, Charsize=0.8,color=100
       device, /close
       print, 'Wrote file "'+fsname+'"'
       
       ;; go back to pre-existing graphics mode and font
       set_plot, old_plot
       !p.font = old_font
    ENDIF
 ENDIF
END
