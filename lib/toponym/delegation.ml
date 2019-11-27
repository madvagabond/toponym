type path = Fpath.path


type entry = {src: path; dest: path}










let apply rule path =
  match Fpath.rem_prefix path rule.src  with
  | Some suffix ->  Fpath.append suffix rule.dest
  | None -> path



let make src dest = {src;dest}



let from_strings src dest =
  let src, dest = Fpath.v src, Fpath.v dest in
  {src; dest}


let (=>) = from_strings

let (>>) = make













module Dtab = struct


  type t = {entries: entry list}




 


  let from_list entries =
    {entries = List.rev entries}

  
  let lookup t path =
    List.filter (fun x -> Fpath.is_prefix x.src path) t.entries



  let add t delegation =
    {entries = [delegation] @ t.entries}

  let remove t delegation =
    {entries = List.filter (fun x -> x != delegation) t.entries }


  let remove_all l r =
  
    let entries =   List.filter (fun x ->
        List.exists (fun y -> y = x) r
      ) l.entries
    in

    {entries}



  let join l r =
    {entries = r.entries @ l.entries}

  

  
  
    
  module Infix = struct

    let (+) = add
    let (-) = remove
      
    let (++) = join
    let (--) = remove_all
  end
  
  
end 


  
