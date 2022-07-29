
let rec splitx list  = match list with 
    []->[]|
    (x,y)::t-> x::splitx t;;
(*function for only Y*)
let rec splity list  = match list with 
    []->[]|
    (x,y)::t-> y::splity t;;

(*function for Lj formula*)
let lj list = (fun x j -> let search = list in
                let rec pi list = 
                  match list with 
                    []-> (1.)|  
                    h::t ->  if List.nth search j !=  h then (x -. h) /. ( (List.nth  search j ) -. h) *. pi t
                      else pi t
                                        
                in pi ( list) 
              
              );;

(* main function which use lj function .*)
let lagrange ylist = (fun kx  -> let ljfun = lj (splitx ylist) in   
                       let rec sum ylist indexes = 
                         match ylist with
                           []->0. |
                           h::t -> (h *. ljfun kx indexes) +. sum t (indexes+1)  
                       in sum (splity ylist) 0  
              
                     ) ;;


