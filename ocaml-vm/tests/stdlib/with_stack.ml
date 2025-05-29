let t = Stack.create () ;;

Stack.push 5 t;;

Stack.push 6 t;;
Stack.push 7 t;;

print_int (Stack.top t);;
print_int (Stack.pop t);;
print_int (Stack.pop t);;
print_int (Stack.pop t);;

try print_int (Stack.pop t) with 
| Stack.Empty -> print_int 42;;
