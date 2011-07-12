FUNCTION commie_propagate_errors, these_relerrs
  
  compile_opt idl2,hidden

  this_relerr = Double(0)
  FOR i=0, n_elements(these_relerrs)-1 DO BEGIN
     this_relerr += (*(these_relerrs[i]))^2
  ENDFOR
  new_relerror_array = sqrt(this_relerr)
  RETURN, new_relerror_array
END
