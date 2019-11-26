


type address = Ipaddr.t * int


module type S = sig
  type +'a io

  val announce: string list -> address -> unit io

end 
