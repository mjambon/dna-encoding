open Printf

type dna = string

let char_of_code = function
    0 -> 'a'
  | 1 -> 'c'
  | 2 -> 'g'
  | 3 -> 't'
  | _ -> assert false

let code_of_char = function
    'a' | 'A' -> 0
  | 'c' | 'C' -> 1
  | 'g' | 'G' -> 2
  | 't' | 'T' -> 3
  | c -> invalid_arg (sprintf "code_of_char %C" c)

let char0 c =
  char_of_code  ((0b11000000 land (Char.code c)) lsr 6)

let char1 c =
  char_of_code ((0b00110000 land (Char.code c)) lsr 4)

let char2 c =
  char_of_code ((0b00001100 land (Char.code c)) lsr 2)

let char3 c =
  char_of_code (0b00000011 land (Char.code c))

let encode s =
  let n = String.length s in
  let s' = String.create (4*n) in
  for i = 0 to n - 1 do
    let c = s.[i] in
    let i' = 4 * i in
    s'.[i'] <- char0 c;
    s'.[i'+1] <- char1 c;
    s'.[i'+2] <- char2 c;
    s'.[i'+3] <- char3 c;
  done;
  s'

let decode s' =
  let n' = String.length s' in
  if n' mod 4 <> 0 then
    invalid_arg "dna_decode (length is not a multiple of 4)"
  else
    let n = n' / 4 in
    let s = String.create n in
    for i = 0 to n - 1 do
      let i' = 4 * i in
      let byte =
        (code_of_char s'.[i'] lsl 6) 
        lor (code_of_char s'.[i'+1] lsl 4) 
        lor (code_of_char s'.[i'+2] lsl 2) 
        lor (code_of_char s'.[i'+3]) 
      in
      s.[i] <- Char.chr byte;
    done;
    s
