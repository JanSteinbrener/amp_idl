PRO commie_manual_align_rescale, disp_max, disp_array
  these_inds = Where(disp_array GT disp_max, count)
  IF count NE 0 THEN $
     disp_array[these_inds] = disp_max
END

PRO commie_manual_align_event, event  

  compile_opt idl2, hidden

  Widget_Control,event.top, Get_UValue=info,/No_Copy
  CASE event.id OF  
     info.display: BEGIN
        IF event.type EQ 0 THEN BEGIN
           Widget_Control,info.xoffset, Get_Value=xoffset
           Widget_Control,info.yoffset, Get_Value=yoffset
           *info.bs_x = event.x + xoffset
           *info.bs_y = event.y + yoffset
           Widget_Control,info.bs_x_field, Set_Value = *info.bs_x
           Widget_Control,info.bs_y_field, Set_Value = *info.bs_y
           Widget_Control, info.display, draw_button_events = 0
           Widget_Control, info.next, sensitive = 1
           status = Strjoin(['Load next position with NEXT or redo ', $
                             ' selection by hitting Select BS position.'])
           Widget_Control, info.status, Set_Value= status
        ENDIF ELSE BEGIN
           ;; this is the case for keyboard events
           ;; Only act on down
           IF event.press EQ 1 THEN BEGIN
              Widget_Control,info.xoffset, Get_Value=xoffset
              Widget_Control,info.yoffset, Get_Value=yoffset
              Widget_Control,info.disp_max, Get_Value=disp_max
              IF event.key EQ 5 THEN BEGIN
                 ;; shift left
                 ;; compute new offsets
                 xoffset = $
                    ((xoffset+1) LT (info.nx-600)) ? (xoffset+1) : $
                    (info.nx-600)
                 ;; redisplay the array
                 IF ptr_valid(info.old_array) THEN BEGIN
                    disp_array = *(info.old_array) + $
                                 ((*(info.new_array))[xoffset:(xoffset+599),$
                                                      yoffset:(yoffset+599)])
                 ENDIF ELSE BEGIN
                    disp_array = (*(info.new_array))[xoffset:(xoffset+599),$
                                                     yoffset:(yoffset+599)]
                 ENDELSE
              ENDIF ELSE IF event.key EQ 6 THEN BEGIN
                 ;; shift right
                 ;; compute new offsets
                 xoffset = ((xoffset-1) GT 0) ? (xoffset-1) : 0
                 ;; redisplay the array
                 IF ptr_valid(info.old_array) THEN BEGIN
                    disp_array = *(info.old_array) + $
                                 ((*(info.new_array))[xoffset:(xoffset+599),$
                                                      yoffset:(yoffset+599)])
                 ENDIF ELSE BEGIN
                    disp_array = (*(info.new_array))[xoffset:(xoffset+599),$
                                                     yoffset:(yoffset+599)]
                 ENDELSE
              ENDIF ELSE IF event.key EQ 7 THEN BEGIN
                 ;; shift up
                 ;; compute new offsets
                 yoffset = ((yoffset-1) GT 0) ? (yoffset-1) : 0
                 ;; redisplay the array
                 IF ptr_valid(info.old_array) THEN BEGIN
                    disp_array = *(info.old_array) + $
                                 ((*(info.new_array))[xoffset:(xoffset+599),$
                                                      yoffset:(yoffset+599)])
                 ENDIF ELSE BEGIN
                    disp_array = (*(info.new_array))[xoffset:(xoffset+599),$
                                                     yoffset:(yoffset+599)]
                 ENDELSE
              ENDIF ELSE IF event.key EQ 8 THEN BEGIN
                 ;; shift down
                 ;; compute new offsets
                 yoffset = $
                    ((yoffset+1) LT (info.ny-600)) ? (yoffset+1) : $
                    (info.ny-600)
                 ;; redisplay the array
                 IF ptr_valid(info.old_array) THEN BEGIN
                    disp_array = *(info.old_array) + $
                                 ((*(info.new_array))[xoffset:(xoffset+599),$
                                                      yoffset:(yoffset+599)])
                 ENDIF ELSE BEGIN
                    disp_array = (*(info.new_array))[xoffset:(xoffset+599),$
                                                     yoffset:(yoffset+599)]
                 ENDELSE
              ENDIF
              ;; rescale if necessary
              commie_manual_align_rescale, disp_max, disp_array
              ;; display the array
              TVscl, disp_array

              ;; set the new offsets
              Widget_Control,info.xoffset, Set_Value=xoffset
              Widget_Control,info.yoffset, Set_Value=yoffset
           ENDIF
        ENDELSE
        Widget_Control,event.top,Set_UValue=info,/No_Copy
     END
     info.xoffset: BEGIN
        Widget_Control,info.xoffset, Get_Value=xoffset
        Widget_Control,info.yoffset, Get_Value=yoffset
        Widget_Control,info.disp_max, Get_Value=disp_max
        IF xoffset LT 0 THEN $
           xoffset = 0
        IF xoffset GT info.nx-600 THEN $
           xoffset = info.nx-600
        ;; redisplay the array
        IF ptr_valid(info.old_array) THEN BEGIN
           disp_array = *(info.old_array) + $
                        ((*(info.new_array))[xoffset:(xoffset+599),$
                                             yoffset:(yoffset+599)])
        ENDIF ELSE BEGIN
           disp_array = (*(info.new_array))[xoffset:(xoffset+599),$
                                            yoffset:(yoffset+599)]
        ENDELSE
        ;; rescale if necessary
        commie_manual_align_rescale, disp_max, disp_array
        ;; display the array
        TVscl, disp_array

        Widget_Control,info.xoffset, Set_Value=xoffset
        Widget_Control,event.top,Set_UValue=info,/No_Copy
     END
     info.yoffset: BEGIN
        Widget_Control,info.xoffset, Get_Value=xoffset
        Widget_Control,info.yoffset, Get_Value=yoffset
        Widget_Control,info.disp_max, Get_Value=disp_max
        IF yoffset LT 0 THEN $
           yoffset = 0
        IF yoffset GT info.ny-600 THEN $
           yoffset = info.ny-600
        ;; redisplay the array
        IF ptr_valid(info.old_array) THEN BEGIN
           disp_array = *(info.old_array) + $
                        ((*(info.new_array))[xoffset:(xoffset+599),$
                                             yoffset:(yoffset+599)])
        ENDIF ELSE BEGIN
           disp_array = (*(info.new_array))[xoffset:(xoffset+599),$
                                            yoffset:(yoffset+599)]
        ENDELSE
        ;; rescale if necessary
        commie_manual_align_rescale, disp_max, disp_array
        ;; display the array
        TVscl, disp_array

        Widget_Control,info.yoffset, Set_Value=yoffset
        Widget_Control,event.top,Set_UValue=info,/No_Copy
     END
     info.done: BEGIN 
        Widget_Control,info.xoffset, Get_Value=xoffset
        Widget_Control,info.yoffset, Get_Value=yoffset
        ;; store the offsets
        (*info.these_xshifts)[info.count] = xoffset
        (*info.these_yshifts)[info.count] = yoffset
        Widget_Control,event.top,Set_UValue=info,/No_Copy
        WIDGET_CONTROL, event.top, /DESTROY  
     END
     info.next: BEGIN
        ;; store the current array as old array and load the next
        ;; array. Display them overlayed
        Widget_Control,info.xoffset, Get_Value=xoffset
        Widget_Control,info.yoffset, Get_Value=yoffset
        ;; store the offsets
        (*info.these_xshifts)[info.count] = xoffset
        (*info.these_yshifts)[info.count] = yoffset
        
        IF ptr_valid(info.old_array) THEN BEGIN
           *info.old_array += (*(info.new_array))[xoffset:(xoffset+599),$
                                                  yoffset:(yoffset+599)]
           ;; renormalize to keep the contrast
           *info.old_array /= 2
        ENDIF ELSE BEGIN
           info.old_array = ptr_new((*(info.new_array))[xoffset:(xoffset+599),$
                                                        yoffset:(yoffset+599)])
        ENDELSE
        ;; augment to the next array
        info.count ++

        ;; is this an array we want to subtract the BS from
        if info.subtract_bs[info.count] eq 1 then begin
           *info.new_array = (*((*info.these_arrays)[info.count]))
           
           ;; redisplay the array
           disp_array = *(info.old_array) + $
                        ((*(info.new_array))[0:599, 0:599])
           Tvscl,disp_array
        
           ;; reset some values
           Widget_Control,info.xoffset, Set_Value=0
           Widget_Control,info.yoffset, Set_Value=0
           Widget_Control,info.disp_max, Set_Value=max(disp_array)
        endif 
        
        IF (info.count+1) EQ n_elements(*info.these_arrays) THEN BEGIN
           Widget_Control, info.next, sensitive=0
           Widget_Control, info.done, sensitive=1
           status = Strjoin(['Align last position with previous. ', $
                             'Confirm final selection with DONE.'])
           Widget_Control, info.status, Set_Value=status
        ENDIF ELSE BEGIN
           status = Strjoin(['Align this position with previous. ', $
                             'Confirm this and load next position with NEXT.'])
           Widget_Control, info.status, Set_Value= status
        ENDELSE
        
        Widget_Control,event.top,Set_UValue=info,/No_Copy
     END
     info.bs_pos: BEGIN
        Widget_Control, info.display, draw_button_events = 1
        Widget_Control, info.status, Set_Value='Click within head of beamstop.'
        Widget_Control, event.top, Set_UValue=info,/No_Copy
     END
     info.disp_max: BEGIN
        Widget_Control,info.xoffset, Get_Value=xoffset
        Widget_Control,info.yoffset, Get_Value=yoffset
        Widget_Control, info.disp_max, Get_Value=disp_max
        ;; redisplay the array
        IF ptr_valid(info.old_array) THEN BEGIN
           disp_array = *(info.old_array) + $
                        ((*(info.new_array))[xoffset:(xoffset+599),$
                                             yoffset:(yoffset+599)])
        ENDIF ELSE BEGIN
           disp_array = (*(info.new_array))[xoffset:(xoffset+599),$
                                            yoffset:(yoffset+599)]
        ENDELSE
        ;; rescale if necessary
        commie_manual_align_rescale, disp_max, disp_array
        ;; display the array
        TVscl, disp_array
        
        Widget_Control, event.top, Set_UValue=info,/No_Copy
     END
     info.reset: BEGIN
        Widget_Control,info.xoffset, Get_Value=xoffset
        Widget_Control,info.yoffset, Get_Value=yoffset
        ;; redisplay the array
        IF ptr_valid(info.old_array) THEN BEGIN
           disp_array = *(info.old_array) + $
                        ((*(info.new_array))[xoffset:(xoffset+599),$
                                             yoffset:(yoffset+599)])
        ENDIF ELSE BEGIN
           disp_array = (*(info.new_array))[xoffset:(xoffset+599),$
                                            yoffset:(yoffset+599)]
        ENDELSE
        ;; display the array
        TVscl, disp_array
        ;; reset the disp_max variable
        Widget_Control,info.disp_max, Set_Value=max(disp_array)
        
        Widget_Control, event.top, Set_UValue=info,/No_Copy
     END
  ENDCASE  
END  

PRO commie_manual_align_cleanup,tlb
  compile_opt idl2, hidden
  Widget_Control, tlb, Get_UValue=info,/No_Copy
  IF ptr_valid(info.new_array) THEN $
     ptr_free, info.new_array
  IF ptr_valid(info.old_array) THEN $
     ptr_free, info.old_array
  IF ptr_valid(info.these_arrays) THEN BEGIN
     FOR i=0,n_elements(*info.these_arrays) -1 DO BEGIN
        IF ptr_valid((*info.these_arrays)[i]) THEN $
           ptr_free,(*info.these_arrays)[i]
     ENDFOR
     ptr_free,info.these_arrays
  ENDIF
END

PRO commie_manual_align, these_arrays, these_xs, these_ys, subtract_bs

  svec = size(*(these_arrays[1]))
  these_xshifts = ptr_new(intarr(n_elements(these_arrays)))
  these_yshifts = ptr_new(intarr(n_elements(these_arrays)))
  bs_x = ptr_new(0)
  bs_y = ptr_new(0)
  
  ;; convert the arrays to log_scale
  FOR i=0,n_elements(these_arrays)-1 DO $
     *these_arrays[i] = Alog10(*these_arrays[i] + 0.001)

  tlb = Widget_Base(/Col,tlb_size_events=1,title='Manual Align',$
                   Kill_Notify='commie_manual_align_cleanup')
  status_str = Strjoin(['Shift to display most of the beamstop.', $
                       ' Then click Select BS position.'])
  status = $
     Widget_Text(tlb,Value=status_str)
  display = Widget_Draw(tlb,retain=2,xsize=600,ysize=600,/keyboard_events)
  sb2 = Widget_Base(tlb,/row)
  bs_pos = Widget_Button(sb2,Value='Select BS position')
  bs_x_field = CW_Field(sb2,title='BS x',/noedit)
  bs_y_field = CW_Field(sb2,title='BS y',/noedit)
  sb1 = Widget_Base(tlb,/row)
  xoffset = CW_Field(sb1,title='Xoffset',Value=0,/Return_events)
  yoffset = CW_Field(sb1,title='Yoffset',Value=0,/Return_events)
  next = Widget_Button(sb1,Value='Next',sensitive=0)
  done=WIDGET_BUTTON(sb1, VALUE='Done',sensitive=0)  
  sb3 = Widget_Base(tlb,/row)
  reset = Widget_Button(sb3, Value='Reset')
  disp_max = CW_Field(sb3,title='Display max',Value=max((*these_arrays[0])[0:599,0:599]), $
                      /Return_events)
  Widget_Control,tlb,/Realize 
  Widget_Control,display,Get_Value=displayID
  Wset,displayID
  
  ;;define indizes of the array that should be displayed
  TVscl,(*these_arrays[0])[0:599,0:599]

  info = {display:display, $
          status:status, $
          count:0, $
          bs_x:bs_x, $
          bs_y:bs_y, $
          bs_x_field:bs_x_field, $
          bs_y_field:bs_y_field, $
          bs_pos:bs_pos, $
          disp_max:disp_max, $
          reset:reset, $
          done:done, $
          next:next, $
          xoffset:xoffset, $
          yoffset:yoffset, $
          these_arrays:ptr_new(these_arrays), $
          these_xshifts:these_xshifts, $
          these_yshifts:these_yshifts, $
          old_array:ptr_new(), $
          new_array:ptr_new(*(these_arrays[0])), $
          nx:svec[1], $
          ny:svec[2], $
          subtract_bs:subtract_bs}
  Widget_Control, tlb, Set_UValue=info,/No_Copy
  XManager,'commie_manual_align',tlb
  
  ;; calculate the absolute shift to bs center position
  this_x = (*these_xshifts)[0]
  this_y = (*these_yshifts)[0]
  FOR i=0,n_elements(*these_xshifts)-1 DO BEGIN 
     (*these_xshifts)[i] -= this_x
     (*these_yshifts)[i] -= this_y
  ENDFOR

  these_xs = Intarr(n_elements(*these_xshifts))
  these_ys = Intarr(n_elements(*these_yshifts))

  FOR i=0,n_elements(these_xs)-1 DO BEGIN
     if subtract_bs[i] eq 1 then begin
        these_xs[i] = (*these_xshifts)[i] + *bs_x
        these_ys[i] = (*these_yshifts)[i] + *bs_y
     endif else begin
        these_xs[i] = 0
        these_ys[i] = 0
     endelse
  ENDFOR
  
  ;; don't forget to free the pointers
  ptr_free, bs_x, bs_y, these_xshifts,these_yshifts
  RETURN
END
