[@@@warning "+A"]

module GenericAligner (T: Aligner_sig.CONTENTS)
  : Aligner_sig.ALIGNER
    with type contents = T.contents
  =
  (struct
    type contents = T.contents

    type _tree_contents =
      | Leaf of contents
      | Node of tree_contents list

    and tree_contents = {
      node: _tree_contents;
      fill_with: char;
      align: Alignment.alignment;
    }

    let leaf ?(fill_with: char = ' ') ?(align: Alignment.alignment=Alignment.Left) (contents: contents) : tree_contents =
      {node = Leaf contents; fill_with; align}

    let node ?(fill_with: char = ' ') ?(align: Alignment.alignment=Alignment.Left) (contents: tree_contents list) : tree_contents =
      {node = Node contents; fill_with; align}

    let rec tree_size : tree_contents -> Size_tree.t = function
      | {node=Leaf s; _} -> Size_tree.{children = []; width = T.contents_length s}
      | {node=Node l; _} ->
        let children = List.map tree_size l in
        Size_tree.{children; width = List.fold_left (fun a node -> a + node.width) 0 children}

    let tree_size_of_list : tree_contents list -> Size_tree.t option = function
      | [] -> None
      | h::t -> Some (List.fold_left Size_tree.merge (tree_size h) (List.map tree_size t))

    let print_tree_with_size (size: Size_tree.t) (fmt: Format.formatter) (str: tree_contents) : unit =
      let pad (fill_with: char) (fmt: Format.formatter) (n: int) : unit =
        Format.pp_print_string fmt (String.make n fill_with)
      in
      let rec aux (str: tree_contents) (size: Size_tree.t) : int * (Format.formatter -> unit) =
        match str with
        | {node=Leaf s; fill_with; align} ->
          let pad = pad fill_with in
          let d = size.Size_tree.width - T.contents_length s in
          let pp fmt =
            let open Alignment in
            match align with
            | Left -> Format.fprintf fmt "%a%a" T.pp s pad d
            | Right -> Format.fprintf fmt "%a%a" pad d T.pp s
            | Center ->
              let half = d / 2 in
              Format.fprintf fmt "%a%a%a" pad half T.pp s pad (d - half)
          in
          size.Size_tree.width, pp
        | {node=Node l; fill_with; align} ->
          let rec aux2 l m : int * (Format.formatter -> unit) =
            match l, m with
            | [], _ -> 0, ignore
            | t1::q1, t2::q2 ->
              let hd_width, hd_pp = aux t1 t2 in
              let tl_width, tl_pp = aux2 q1 q2 in
              hd_width + tl_width, (fun fmt -> hd_pp fmt; tl_pp fmt)
            | _::_, [] -> raise (Exn.PrintError [__LOC__; Format.asprintf "print_tree_with_size: pattern inconsistent with string tree"])
          in
          let size_s, pp = aux2 l size.Size_tree.children in
          let d = size.Size_tree.width - size_s in
          let pad = pad fill_with in
          let pp fmt =
            let open Alignment in
            match align with
            | Left -> Format.fprintf fmt "%t%a" pp pad d
            | Right -> Format.fprintf fmt "%a%t" pad d pp
            | Center ->
              let half = d / 2 in
              Format.fprintf fmt "%a%t%a" pad half pp pad (d - half)
          in
          size.Size_tree.width, pp
      in
      let _, pp = aux str size in
      pp fmt

    let pp_of_table (str_tree : tree_contents list) : (Format.formatter -> unit) list =
      match tree_size_of_list str_tree with
      | None -> []
      | Some size_tree ->
        let pp = print_tree_with_size size_tree in
        List.map (fun tree fmt -> pp fmt tree) str_tree

    let kprint_table (f: Format.formatter -> unit) (fmt: Format.formatter) (str_tree : tree_contents list) : unit =
      List.iter (Format.kfprintf f fmt "%t\n") (pp_of_table str_tree)

    let print_table (fmt: Format.formatter) (str_tree : tree_contents list) : unit =
      kprint_table ignore fmt str_tree

    let stringify_table (str_tree : tree_contents list) : string list =
      List.map (Format.asprintf "%t") (pp_of_table str_tree)
  end)
