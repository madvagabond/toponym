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









module Infix = struct
  let (=>) = from_strings
  let (>>) = make
end 





module Dtab = struct


  type t = {entries: entry list}

  



  
  let parse s =
    let open Infix in 

    let read_entry s =
      Astring.String.find_sub s ~sub:"=>" |> function
      | Some i ->
        let prefix = Astring.String.with_range s ~len:(i - 1) in
        let dest = Astring.String.with_range s ~first:(i + 2) in
        Some (prefix => dest)


      | None -> None
    in


    let fields = Astring.String.fields ~is_sep:(fun c -> c = '\n' || c = ',') s in


    let aux acc x =
      read_entry x |> function
      | Some e -> e :: acc
      | None  -> acc
    in

    List.fold_left aux [] fields



 


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


  
