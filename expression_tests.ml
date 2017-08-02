open Expression ;;
open ExpressionLibrary ;;

let test () =
  assert (contains_var (parse "x+3"));
  assert (not (contains_var (parse "2")));

  assert (evaluate (parse "x^4 + 3") 2.0 = 19.0);
  assert (evaluate (parse "x") 3.0 = 3.0);
  assert (evaluate (parse "4") 15.0 = 4.0);
  assert (evaluate (parse "ln(x) + x^2 / ~4") 1.0 = -0.25);

  assert (to_string (derivative (parse "x")) = "1.");
  assert (to_string (derivative (parse "1")) = "0.");
  assert (to_string (derivative (parse "cos x")) = "((~((sin(x))))*1.)");
  assert (to_string (derivative (parse "sin x")) = "((cos(x))*1.)");
  assert (to_string (derivative (parse "ln x")) = "((1./x)*1.)");
  assert (to_string (derivative (parse "x/x")) = "(((1.*x)-(x*1.))/(x^2.))");
  assert (to_string (derivative (parse "x*x")) = "((x*1.)+(1.*x))" );
  assert (to_string (derivative (parse "x+x")) = "(1.+1.)") ;
  assert (to_string (derivative (parse "x-x")) = "(1.-1.)");
  assert (to_string (derivative (parse "x^2")) = "(2.*(1.*(x^(2.-1.))))");
  assert (to_string (derivative (parse "x^x")) = "((x^x)*((1.*(ln(x)))+((1.*x)/x)))");

  assert (find_zero (parse "x+5") 10.0 0.5 2 = Some (-5.));
  assert (find_zero (parse "x^4") 10.0 0.5 1 = None)


;;

test();;
print_endline "All tests passed.";;
