program tree_demo

   use yaftree_m
   implicit none (type, external)

   type(dict_t) :: dict
   type(set_t) :: set

   print *, 'call insert(set, "abc")'
   call insert(set, "abc")
   print *, 'call insert(set, 321)'
   ! any type without pointers or allocatable components is valid
   call insert(set, 321)

   ! size overloaded for consistency with arrays
   print *, "size(set)", "=", size(set)
   ! .in. operator -- like Python
   print *, '"abc" .in. set',  "=", "abc" .in. set

   print *, 'call insert(dict, "my", "value")'
   call insert(dict, "my", "value")
   print *, 'call insert(dict, 15, 35)'
   call insert(dict, 15, 35)

   print *, '"my" .in. dict',  "=", "my" .in. dict

   ! typically you will retrieve stored values like this
   select type (val => get(dict, "my"))
   type is (character(len=*))
      print *, 'get(dict, "my")',  "=", val
   end select

   ! when key is not found, special object is returned
   select type (val => get(dict, "ohno"))
   type is (key_not_found_t)
      print *, "key not found"
   end select

   ! no explicit cleanup call is needed, since all data structure
   ! is build on allocatables

end program
