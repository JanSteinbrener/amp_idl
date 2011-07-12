;; this routine will set all negatives in
;; dataset_struct.assembled_sample to zero. To be called in the end
;; when no_sample is already subtracted
PRO commie_zero_negatives, commie_dataset_struct

  compile_opt idl2, hidden

  neg_indices = Where(*commie_dataset_struct.assembled_sample LT 0, count)
  IF count NE 0 THEN $
     (*commie_dataset_struct.assembled_sample)[neg_indices] = 0
END
