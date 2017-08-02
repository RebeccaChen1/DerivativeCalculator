(* 
			 CS 51 Problem Set 2
		 Higher Order Functional Programming
			     Spring 2017
*)

(*======================================================================
Problem 1: Higher-order functional programming

Solve each problem in this part using List.map, List.fold_left,
List.fold_right, or List.filter. You are permitted to use other
functions from the list module. However, you should think about
whether they’re necessary and be mindful of their usage.

A solution, even a working one, that does not use one of these
higher-order functions will receive little or no credit. However, if
you can express your solution to one one particular problem in terms
of another function from another part, you may do so.

You MAY NOT change the definition of these functions to make them
recursive.

........................................................................
Problem 1.1: The function "negate_all" flips the sign of each element
in a list. 
......................................................................*)

(* flips the sign of each element in a list *)
let negate_all (nums : int list) : int list = 
  List.map (fun x -> -x) nums ;;

(*......................................................................
Problem 1.2: The function "sum" returns the sum of the elements in 
the list. 
......................................................................*)

(* returns the sum of the elements in the list *)
let sum (nums : int list) : int =
  List.fold_left ( + ) 0 nums  ;;

(*......................................................................
Problem 1.3: The function "sum_rows" takes a list of "rows", each an 
int list and returns a one-dimensional list of ints. Each int in this list
is equal to the sum of the corresponding rows in the input. For example:

sum_rows [[1; 2]; [3; 4]] = 
- : int list = [3; 7] 
......................................................................*)

(* returns a list of the sum of inner lists *)
let sum_rows (rows : int list list) : int list =
  List.map (sum) rows ;;

(*......................................................................
Problem 1.4: The function "filter_odd" retrains only the odd numbers
from the given list. For example:

filter_odd [1; 4; 5; -3] = 
- : int list = [1; 5; -3]
......................................................................*)

(* retains only the odd numbers from a given list *)
let filter_odd (nums : int list) : int list =
  List.filter (fun x -> x mod 2 <> 0) nums ;;

(*......................................................................
Problem 1.5: The function "num_occurs" returns the number of times a given
number appears in a list. For example:

num_occurs 4 [1; 3; 4; 5; 4] =
- : int = 2
......................................................................*)

(* returns the number of times a given number appears in a list *)
let num_occurs (n : int) (nums : int list) : int =
  List.fold_left (fun a x -> if x = n then (a + 1) else a) 0 nums ;;

(*......................................................................
Problem 1.6: The function "super_sum" sums all of the numbers in a list
of int lists. For example:

super_sum [[1; 2; 3]; []; [5]] = 
- : int = 11
......................................................................*)

(* sums all the numbers in a list of int lists *)
let super_sum (nlists : int list list) : int =
  List.fold_left ( + ) 0 (List.map (List.fold_left ( + ) 0) nlists) ;;

(*......................................................................
Problem 1.7: The function "filter_range" returns a list of numbers in 
the input list within a given range (inclusive), in the same order they
appeared in the input list. For example:

filter_range [1; 3; 4; 5; 2] (1, 3) = 
- : int list = [1; 3; 2]
......................................................................*)


let filter_range (nums : int list) (range : int * int) : int list =
  List.filter (fun x -> let a,b = range in (x >= a && x <= b)) nums ;;

(*......................................................................
Problem 1.8: The function "float_of_ints" converts an int list into 
a float list. 
......................................................................*)

let floats_of_ints (nums : int list) : float list =
  List.map (float_of_int) nums ;;


(*......................................................................
Problem 1.9: The function "log10s" applies the log10 function to all
members of a list of floats. The function log10 is not defined for
numbers n <= 0, so undefined results should be None. For example:

log10s [1.0; 10.0; -10.0] = 
- : float option list = [Some 0.; Some 1.; None]
......................................................................*)

let log10s (lst : float list) : float option list =
  List.map (fun x -> if x < 0. then None else Some (log10 x)) lst ;;

(*......................................................................
Problem 1.10: The function "deoptionalize" extracts values from a list 
of options. For example:

deoptionalize [Some 3; None; Some 5; Some 10] = 
- : 'a list = [3; 5; 10]
......................................................................*)

let deoptionalize (lst : 'a option list) : 'a list =
  List.map (fun opt -> match opt with
                       | Some num -> num
                       | None -> raise (Invalid_argument "None"))
           ((List.filter (fun x -> x <> None)) lst) ;;

(*......................................................................
Problem 1.11: The function "some_sum" sums all of the numbers in a list 
of int options but ignores None values.
......................................................................*)

let some_sum (nums : int option list) : int =
  List.fold_left ( + ) 0 (deoptionalize (nums)) ;;


(*......................................................................
Problem 1.12: The function "mult_odds" returns the product of all
of the odd elements of a list. For example:

mult_odds [1; 3; 0; 2; -5] = 
- : int = -15
......................................................................*)

let mult_odds (nums : int list) : int =
  List.fold_left ( * ) 1 (filter_odd (nums)) ;;

(*......................................................................
Problem 1.13: The function "concat" concatenates a list of lists. 
......................................................................*)

let concat (lists : 'a list list) : 'a list =
  List.concat lists ;;

(*......................................................................
Problem 1.14: We begin by defining a type that represents a student 
as a tuple of the student's name and year. The function "filter_by_year"
returns the name of students in a given year. For example:

let students = [("Joe", 2010); ("Bob", 2010); ("Tom", 2013)] ;;
filter_by_year students 2010 = 
- : name list = ["Joe"; "Bob"]
......................................................................*)

type name = string
type year = int
type student = name * year

let filter_by_year (slist : student list) (yr : year) : name list =
  (* extracts name of student from given year *)
  List.map (fun std -> match std with | (a, _b) -> a)
           (* returns list of students from given year *) 
           (List.filter (fun (_a, b) -> b = yr) (slist))
;;

(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete.  We care
about your responses and will use them to help guide us in creating
future assignments.
......................................................................*)

let minutes_spent_on_part1 () : int = 360 ;;
