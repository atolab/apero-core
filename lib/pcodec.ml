(* open Property
open Common.Result
open Tcodec

let encode_properties ps =
  if ps = Properties.empty then return 
  else (encode_seq encode_property) ps 
  
let decode_properties = (decode_seq decode_property) 
 *)
