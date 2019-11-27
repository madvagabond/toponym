


type address = Ipaddr.t * int


module type S = sig
  type +'a io

  val announce: Fpath.t -> address -> unit io

end 
