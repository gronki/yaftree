# yaftree -- Yet Another Fortran Tree!

This library provides a very minimal (1 module, less than 400 lines of code, no dependencies) and lightweight implementation of a dictionary (``key``--``value`` pairs) and set (only keys) in Modern Fortran. Some features, such as interface for popular data types (``INTEGER``, ``CHARACTER`` etc) are intentionally omitted to keep the size down to the minimum. This means that you likely need many ``SELECT TYPE`` statements or create your wrappers to help you interpret retrieved items. Since the tree is built solely using ``ALLOCATABLE``, it does not need to be explicitly deallocated. Be careful on assignments and avoid returning it from a function, or you might face a lot of copies. Best use ``dict_t`` and ``set_t`` as arguments for subroutines, as that will prevent unneccesary copy.

The implementation uses ``PURE`` procedures wherever possible. Since this meant that using pointers was out of the picture, traversing the tree uses recursion. This should be fine up to hundreds thousands of items, but if you want to process really large sets, you might want to try another implementation. Pushing and retrieving values from a hashmap currently involves copies, so consider that if you store large objects (consider using ``POINTER`` wrapped in a derived type).

Critical features that have not yet been implemented:

- deletion of items,
- copy-less insertion of ``dict_t`` values (by ``MOVE_ALLOC``),
- copy-less retrieving of ``dict_t`` value (by ``POINTER``),
- iterating over items of a tree (likely will be very inefficient due to pure implementation).

## Quick start

Check out ``example/demo.f90`` by running ``fpm run --example``.
```fortran
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
```

## Compiler compatibility

Tested successfully on:

- ifx (IFX) 2025.0.0 20241008
- GNU Fortran 14.2.1 20241007
