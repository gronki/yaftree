program test_copy

   use yaftree_m
   implicit none (type, external)

   print '(a)', "     ***** TESTING DEEPCOPY *****"

   block
      type(set_t) :: set1, set2
      set1%hasher => fnv_hash
      set2%hasher => fnv_hash

      call insert(set1, "a")
      call insert(set1, "b")

      set2 = set1

      call insert(set2, "c")

      if ( ("c" .in. set2) .eqv. .FALSE. ) error stop
      if ( ("c" .in. set1) .eqv. .TRUE. ) error stop
   end block

   block

      type(set_t) :: set1, set2
      set1%hasher => fnv_hash
      set2%hasher => fnv_hash
      
      call insert(set1, 1)
      set1 = set2

      if ( size(set1) /= 0 ) error stop

   end block
   print '(a)', "  OK"
end program
