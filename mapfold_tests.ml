(* 
			 CS 51 Problem Set 2
	    Higher Order Functional Programming -- Testing
			     Spring 2017
 *)

open Mapfold ;;



let test () =

  assert (negate_all [] = []) ;
  assert (negate_all [1; -2; 0] = [-1; 2; 0]) ;

  assert (sum [] = 0) ;
  assert (sum [1; -2; 0] = -1) ;
  assert (sum [5] = 5) ;

  assert (sum_rows [] = []) ;
  assert (sum_rows [[1; 2]; [3; 4]] = [3; 7]) ;
  assert (sum_rows [[5]] = [5]) ; 

  assert (filter_odd [] = []) ;
  assert (filter_odd [1; 4; 5; -3] = [1; 5; -3]) ;
  assert (filter_odd [0; 0; 0; 2; 2; 719] = [719]) ;

  assert (num_occurs 2 [] = 0) ;
  assert (num_occurs 4 [1; 3; 4; 5; 4] = 2) ;
  assert (num_occurs (-1) [-2; -1; 9; 849; 101] = 1) ;

  assert (super_sum [[1; 2; 3]; []; [5]] = 11) ;
  assert (super_sum [] = 0) ;
  assert (super_sum [[-2; -1; 9; 849; 101]] = 956) ;

  assert (filter_range [1; 3; 4; 5; 2] (1, 3) = [1; 3; 2]) ;
  assert (filter_range [] (0,0) = []) ;
  assert (filter_range [1; -3; 4; 5; 2] (-3, 3) = [1; -3; 2]) ;
  assert (filter_range [1; 3; 4; 5; 2] (1, 1) = [1]) ;

  assert (floats_of_ints [1; 3; 4; 5; 2] = [1.; 3.; 4.; 5.; 2.]) ;
  assert (floats_of_ints [] = []) ;
  assert (floats_of_ints [-1] = [-1.]) ;

  assert (log10s [1.0; 10.0; -10.0]  = [Some 0.; Some 1.; None]) ;
  assert (log10s [] = []) ;
  assert (log10s [100.0] = [Some 2.]) ;

  assert (deoptionalize [Some 3; None; Some 5; Some 10]  = [3; 5; 10]) ;
  assert (deoptionalize [] = []) ;
  assert (deoptionalize [None; Some (-2)] = [-2]) ;

  assert (some_sum [Some 3; None; Some 5; Some 10]  = 18) ;
  assert (some_sum [] = 0) ;
  assert (some_sum [None; Some (-2); Some 3] = 1) ;

  assert (mult_odds [1; 4; 5; -3]  = -15) ;
  assert (mult_odds [] = 1) ;
  assert (mult_odds [0; 0; 0; -3; 2; 2; 719]  = -2157) ;

  assert (concat [[1; 4; 5; -3]]  = [1; 4; 5; -3]) ;
  assert (concat [] = []) ;
  assert (concat [[1];[2;3];[4;5;6]]  = [ 1; 2; 3; 4; 5; 6]) ;

  assert (filter_by_year [("Joe", 2010); ("Bob", 2010); ("Tom", 2013)] 2010  = ["Joe"; "Bob"]) ;
  assert (filter_by_year [] 2016 = []) ;
  assert (filter_by_year [("Joe", 2016); ("Bob", 2015)] 2013  = []) 

;;

test () ;;

print_endline "All tests passed.";;
