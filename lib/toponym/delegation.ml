type path = string list 

type delegation = {src: path; dest: path}





let has_prefix prefix path =


  let rec aux prefix path =
    match prefix, path with
    | pfx_hd :: pfx_tl, hd :: tl when pfx_hd = hd ->
      aux pfx_tl tl

    | [], _ -> true

    | _ -> false
  in

  if List.length path < List.length prefix then
    false
  else aux prefix path



let strip_prefix prefix path = 

  let rec get_suffix l i =
    if i >= List.length path then
      l
    else
      let e = List.nth path i in
      get_suffix (l @ [e]) (i + 1)          
  in
  

  
    if has_prefix prefix path then
      let start = List.length prefix in
      Some (get_suffix [] start )
    else

      None 
   



let apply_delegation delegation path =
  match (strip_prefix delegation.src path) with
  | Some suffix -> Some(
      delegation.dest @ suffix
    )


  | None -> None



  





module Dtab = struct


  type t = {entries: delegation list}




 


  let from_list entries =
    {entries = List.rev entries}

  
  let lookup t path =
    List.filter (fun x -> has_prefix x.src path) t.entries



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


  
