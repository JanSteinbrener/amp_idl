PRO commie_find_bs_widget_event, event 
  
  compile_opt idl2, hidden
  
  Widget_Control,event.top, Get_UValue=info,/No_Copy
  CASE event.id OF  
     info.bs_display: BEGIN
        IF info.bs_disp_butt_event THEN BEGIN
           IF event.type EQ 0 THEN BEGIN
              info.new_roi_index = event.y*info.nx + event.x
              info.bs_disp_butt_event = 0
           ENDIF
        ENDIF
        Widget_Control,event.top,Set_UValue=info,/No_Copy
     END
     info.dbutt: BEGIN
        ;; The ROI definition will be stored in the variable
        ;; roi_indices: 
        roi_indices = $
           CW_DEFROI(info.bs_display,/Restore) 
        IF (roi_indices[0] NE -1) then BEGIN 
           ;; Show the size of the ROI definition array: 
           IF ptr_valid(info.roi_indices) THEN $
              ptr_free, info.roi_indices
           info.roi_indices = ptr_new(roi_indices)
           ;; Duplicate the original image. 
           ;image2 = image 
           
           ;; Set the points in the ROI array Q equal to a single 
           ;; color value: 
           ;image2(Q)=!P.COLOR-1 
           ;; Get the window ID of the draw widget: 
           ;Widget_Control,info.bs_display,Get_Value=bs_displayID
           ;Wset,bs_displayID
           
           ;; Load the image plus the ROI into the draw widget: 
           ;TV, image2 
        ENDIF ELSE BEGIN
           IF ptr_valid(info.roi_indices) THEN $
              ptr_free, info.roi_indices
           info.roi_indices = ptr_new()
        ENDELSE
        Widget_Control,event.top,Set_UValue=info,/No_Copy
     END
     info.histo_display: BEGIN
        IF (event.type EQ 1) THEN BEGIN
           Widget_Control,/hourglass
           ;; calculate the index for nbins
           this_index = Fix((event.x-info.plot_offset)*info.scale_factor) -1
           ;; if user clicks on point outside of the actual plot then
           ;; set index to max/min value possible.
           IF this_index GE n_elements(info.bins) THEN BEGIN
              this_index = n_elements(info.bins) -1
           ENDIF ELSE IF this_index LT 0 THEN BEGIN
              this_index = 0
           ENDIF 
           info.histo_x = info.bins[this_index]
           ;; update the textwindow
           Widget_Control,info.threshold_label, Set_Value=info.histo_x
           
           ;; copy the histogram from the pixmap window
           Widget_Control, info.histo_display, Get_Value=histo_displayID
           Wset, histo_displayID
           Device, Copy=[0,0,400,160,0,0,info.pixmap_winID]
           
           ;; now overplot a vertical line indicating the current threshold
           xs = [info.histo_x,info.histo_x]
           ys = [(info.y_range)[0],(info.y_range)[1]]
           PlotS,xs,ys
           
           ;; now update the display in bs_display
           threshold = info.histo_x
           commie_region_grow, *(info.this_array), info.new_roi_index, $
                               threshold,info.threshold_scaling, $
                               info.roi_indices,bs_array

           *(info.ptr_bs_array) = bs_array
           ;; Set the bs-window
           Widget_Control,info.bs_display,Get_Value=bs_displayID
           Wset,bs_displayID
           IF ptr_valid(info.old_bs_array) THEN BEGIN
              disp_array = intarr(info.nx,info.ny)
              these_inds = $
                 Where(*info.old_bs_array AND bs_array, count)
              IF count NE 0 THEN $
                 disp_array[these_inds] = 1
           ENDIF ELSE BEGIN
              disp_array = bs_array
           ENDELSE
           Tvscl,disp_array
        ENDIF
        Widget_Control,event.top,Set_UValue=info,/No_Copy
     END
     info.data_display: BEGIN
        ;; get the value of the pixel that the mouse points at and
        ;; display it in the textfield.
        IF (event.type EQ 2) THEN BEGIN
           pixel_value = Long((*info.disp_array)[event.x,event.y])
           Widget_Control, info.counts, Set_Value=pixel_value
           Widget_Control,event.top,Set_UValue=info,/No_Copy
        ENDIF
     END
     info.done: BEGIN 
        ;; store the old_bs_array together with the current one in
        ;; ptr_bs_array if it exists.
        IF ptr_valid(info.old_bs_array) THEN BEGIN
           this_array = Intarr(info.nx,info.ny)
           these_inds = Where(*info.old_bs_array AND *info.ptr_bs_array, count)
           IF count NE 0 THEN $
              this_array[these_inds] = 1
           *info.ptr_bs_array = this_array
        ENDIF
        Widget_Control,event.top,Set_UValue=info,/No_Copy
        WIDGET_CONTROL, event.top, /DESTROY  
     END
     info.ioi_butt: BEGIN
        info.bs_disp_butt_event = 1
        Widget_Control,event.top,Set_UValue=info,/No_Copy
     END
     ;info.reset: BEGIN
     ;   IF ptr_valid(info.roi_indices) THEN $
     ;      ptr_free, info.roi_indices
     ;   info.roi_indices = ptr_new()
     ;   info.new_roi_index = info.roi_index
     ;   
     ;   ;; reset the threshold and redetermine bs_array
     ;   high_threshold = (*info.this_array)[info.roi_index]
     ;   Widget_Control,info.threshold_label,Set_Value=high_threshold
     ;   commie_region_grow, *(info.this_array), info.new_roi_index, $
     ;                       high_threshold, $
     ;                       info.threshold_scaling, $
     ;                       info.roi_indices, bs_array
     ;   ;; Set the bs-window
     ;   Widget_Control,info.bs_display,Get_Value=bs_displayID
     ;   Wset,bs_displayID
     ;   Tvscl,bs_array
     ;   *(info.ptr_bs_array) = bs_array
     ;   
     ;   Widget_Control,event.top,Set_UValue=info,/No_Copy
     ;END
     info.next_roi: BEGIN
        ;; add the current bs_array to old_bs_array. We have to use
        ;; the where function to make sure that we count overlaps only
        ;; once. 
        IF ptr_valid(info.old_bs_array) THEN BEGIN
           this_array = Intarr(info.nx,info.ny)
           these_inds = Where(*info.old_bs_array AND *info.ptr_bs_array, count)
           IF count NE 0 THEN $
              this_array[these_inds] = 1
           *info.old_bs_array = this_array
        ENDIF ELSE BEGIN
           ;; in this case we just keep the existing one
           info.old_bs_array = ptr_new(*info.ptr_bs_array)
        ENDELSE
        Widget_Control,event.top,Set_UValue=info,/No_Copy
     END
     info.reset_all: BEGIN
        IF ptr_valid(info.roi_indices) THEN $
           ptr_free, info.roi_indices
        info.roi_indices = ptr_new()
        info.new_roi_index = info.roi_index

        IF ptr_valid(info.old_bs_array) THEN $
           ptr_free,info.old_bs_array
        info.old_bs_array = ptr_new()

        ;; reset the threshold and redetermine bs_array
        high_threshold = (*info.this_array)[info.roi_index]
        Widget_Control,info.threshold_label,Set_Value=high_threshold
        commie_region_grow, *(info.this_array), info.new_roi_index, $
                            high_threshold, $
                            info.threshold_scaling, $
                            info.roi_indices, bs_array
        ;; Set the bs-window
        Widget_Control,info.bs_display,Get_Value=bs_displayID
        Wset,bs_displayID
        Tvscl,bs_array
        *(info.ptr_bs_array) = bs_array
        
        Widget_Control,event.top,Set_UValue=info,/No_Copy
     END
     info.threshold_label: BEGIN
        Widget_Control,/hourglass
        ;; Calculate the new bs_array
        Widget_Control, info.threshold_label, Get_Value=high_threshold

        ;; redraw the histogram (copy from pixmap), if threshold is
        ;; bigger than what is currently supported by bins,
        ;; then don't draw any vertical line. 
        Widget_Control, info.histo_display, Get_Value=histo_displayID
        Wset, histo_displayID
        Device, Copy=[0,0,400,160,0,0,info.pixmap_winID]
        IF ((high_threshold GE info.bins[0]) AND $
           high_threshold LE info.bins[(n_elements(info.bins) -1)]) THEN BEGIN
           info.histo_x = high_threshold
           xs = [info.histo_x,info.histo_x]
           ys = [(info.y_range)[0],(info.y_range)[1]]
           PlotS,xs,ys
        ENDIF
        ;; prepare to update bs_display
        threshold = high_threshold
        commie_region_grow, *(info.this_array), info.new_roi_index,threshold, $
                            info.threshold_scaling, $
                            info.roi_indices, bs_array
 
        *(info.ptr_bs_array) = bs_array
        ;; Set the bs-window
        Widget_Control,info.bs_display,Get_Value=bs_displayID
        Wset,bs_displayID
        IF ptr_valid(info.old_bs_array) THEN BEGIN
           disp_array = intarr(info.nx,info.ny)
           these_inds = $
              Where(*info.old_bs_array AND bs_array, count)
           IF count NE 0 THEN $
              disp_array[these_inds] = 1
        ENDIF ELSE BEGIN
           disp_array = bs_array
        ENDELSE
        Tvscl,disp_array
        Widget_Control,event.top,Set_UValue=info,/No_Copy
     END
     ELSE : Widget_Control,event.top,Set_UValue=info,/No_Copy
  ENDCASE  
END  

PRO commie_find_bs_widget_cleanup,tlb
  compile_opt idl2, hidden
  Widget_Control, tlb, Get_UValue=info,/No_Copy
  IF ptr_valid(info.disp_array) THEN $
     ptr_free, info.disp_array
  IF ptr_valid(info.this_array) THEN $
     ptr_free, info.this_array
  IF ptr_valid(info.roi_indices) THEN $
     ptr_free, info.roi_indices
  IF ptr_valid(info.old_bs_array) THEN $
     ptr_free, info.old_bs_array
END

PRO commie_find_bs_widget, commie_dataset_struct, this_array, bs_array, $
                           scan=scan
  
  compile_opt idl2, hidden

  IF Keyword_Set(scan) THEN BEGIN
     roi_index = commie_dataset_struct
  ENDIF ELSE BEGIN
     roi_index = $
        Long(((*commie_dataset_struct.bs_ys)[0]))*$
        Long((commie_dataset_struct.nx))+Long((*commie_dataset_struct.bs_xs)[0])
  ENDELSE

  ;; determine the scaling of the threshold
  commie_intens_scaling, this_array, roi_index,threshold_scaling
  
  dummy_roi=ptr_new()
  
  threshold = this_array[roi_index]
  commie_region_grow, this_array, roi_index, threshold, $
                      threshold_scaling,dummy_roi, bs_array
  
  ;; now create the widget
  tlb = Widget_Base(/Col,tlb_size_events=1,title='Interactive Region_Grow',$
                   Kill_Notify='commie_find_bs_widget_cleanup')
  sb1 = Widget_Base(tlb,/Row)
  sb11 = Widget_Base(sb1,/Col)
  bs_display = Widget_Draw(sb11,retain=2,xsize=commie_dataset_struct.nx, $
                           ysize=commie_dataset_struct.ny, $ 
                           x_scroll_size=600, y_scroll_size=600, $
                           /button,/motion)
  sb111 = Widget_Base(sb11,/Row)
  dbutt = WIDGET_BUTTON(sb111, VALUE='Draw ROI')
  ioi_butt = Widget_button(sb111, Value='Select IOI')
  ;reset = Widget_Button(sb111,Value='Reset this ROI/IOI')
  sb112 = Widget_Base(sb11,/Row)
  reset_all = Widget_Button(sb112, Value='Start over')
  next_roi = Widget_Button(sb112,Value='Save and select next ROI')
  
  sb12 = Widget_Base(sb1,/Col)
  data_display =  Widget_Draw(sb12,retain=2,xsize=400,ysize=400,/motion_events)
  counts = CW_Field(sb12,title='Counts',/noedit,/long,xsize=15)
  histo_display = Widget_Draw(sb12,retain=2, xsize=400,ysize=160,/button_events)
  threshold_label = CW_Field(sb12,title='Upper Threshold',/long,xsize=15, $
                             /Return_Events)
  done=WIDGET_BUTTON(tlb, VALUE='Done')  
  Widget_Control,tlb,/Realize 
  
  Widget_Control,bs_display,Get_Value=bs_displayID
  Widget_Control,data_display,Get_Value=data_displayID
  Widget_Control,histo_display,Get_Value=histo_displayID

  ;; now switch to the beamstop display
  Wset,bs_displayID
  TVscl,bs_array

  Wset, data_displayID
  disp_array = Congrid(this_array,400,400)
  TVscl,disp_array

  Wset,histo_displayID
  ;; Get some colors
  ;red = GetColor('red',!D.Table_Size-2)
  ;white = GetColor('white',!D.Table_Size-3)
  nbins = 1000

  ;; determine extend of histogram, go 2500 in either direction (or
  ;; from 0 to 5000) of pixel value at roi_index.

  min = Long(this_array[roi_index]) - Long(2500)
  IF min LT 0 THEN min = 0
  max = Long(this_array[roi_index]) + Long(2500)
  IF min EQ 0 THEN max = 5000

  histo = Histogram(this_array,nbins=nbins,max=max,min=min)
  bins=Findgen(n_elements(histo))*((max-min)/nbins) +min
  
  ;; Plot histogram, force the x-axis to go from min to max, otherwise
  ;; our scale factor gets screwed up.
  Plot,bins, histo, psym=10,xtitle='Pixel counts',ytitle='Occurence', $
       xstyle=1,xrange=[min,max]
  
  ;; determine the plot ranges, this will be used to determine the
  ;; height of the vertical bar and the scale factor.
  y_range = [!y.crange[0],!y.crange[1]]
  
  ;; now overplot a vertical line indicating the current threshold
  xs = [threshold,threshold]
  ys = [y_range[0],y_range[1]]
  PlotS,xs,ys

  ;; create the pixmap window and plot the histogram into it.
  Window,/free,/pixmap, xsize=400,ysize=160
  pixmap_winID = !D.Window
  Plot,bins, histo, psym=10,xtitle='Pixel counts',ytitle='Occurence', $
       xstyle=1,xrange=[min,max]

  ;; finally determine the scale factor in our graphics window. We
  ;; need that to accurately place the vertical threshold line.
  plot_offset = Fix(!D.X_Ch_Size*!X.Margin[0])
  plot_pixels = 400. - !D.X_Ch_Size*(!X.Margin[0]+!X.Margin[1])
  scale_factor = nbins/plot_pixels
  

  Widget_Control, threshold_label, Set_Value=threshold
  ptr_bs_array = ptr_new(bs_array)

  info = {nx:Long((commie_dataset_struct.nx)), $
          ny:Long((commie_dataset_struct.ny)), $
          data_display:data_display, $
          bs_display:bs_display, $
          bs_disp_butt_event:0, $
          histo_display:histo_display, $
          pixmap_winID:pixmap_winID, $
          histo_x:threshold, $
          histo:histo, $
          y_range:y_range, $
          bins:bins, $
          done:done, $
          counts:counts, $
          threshold_label:threshold_label, $
          roi_index:roi_index, $
          new_roi_index:roi_index, $
          disp_array:ptr_new(disp_array), $
          this_array:ptr_new(this_array), $
          ptr_bs_array:ptr_bs_array, $
          old_bs_array:ptr_new(), $
          ;red:red, $
          ;white:white, $
          dbutt:dbutt, $
          ;reset:reset, $
          ioi_butt:ioi_butt, $
          next_roi:next_roi, $
          reset_all:reset_all, $
          roi_indices:ptr_new(), $
          plot_offset:plot_offset, $
          scale_factor:scale_factor, $
          threshold_scaling:threshold_scaling}
  
  Widget_Control, tlb, Set_UValue=info,/No_Copy
  XManager,'commie_find_bs_widget',tlb

  ;; hand over the bs_array
  bs_array = *(ptr_bs_array)
  ptr_free, ptr_bs_array
  ;; destroy the pixmap window
  Wdelete, pixmap_winID
  RETURN
END
