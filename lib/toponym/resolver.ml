


type address = Ipaddr.t * int
type endpoints = Pending | Bound of address list | Neg



module type S = sig
  type +'a stream

  type resolver = {
    scheme: string;
    resolve_fn: string list -> endpoints stream 
  }




  type resolve_fn = (string list -> endpoints stream)

  val make: scheme:string -> resolve_fn:string list -> resolver
  val scheme: resolver -> string
  
  val resolve: resolver -> string list -> endpoints stream
  
end








