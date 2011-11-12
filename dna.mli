type dna = string

val encode : string -> dna
  (* Encode any string in base 4 using 'a', 'c', 'g' and 't' for digits. *)

val decode : dna -> string
  (* Decode a string composed of the letters 'a', 'c', 'g' and 't'.
     Its length must be a multiple of 4. *)
