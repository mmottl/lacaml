module Bytes = struct
  let make = String.make
  let blit_string = String.blit
  let unsafe_to_string bts = bts
end  (* Bytes *)
