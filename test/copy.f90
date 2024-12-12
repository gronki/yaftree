program test_copy

   use yaftree_m

   print '(a)', "     ***** TESTING DEEPCOPY *****"

   block
      type(set_t) :: set1, set2

      call set1%insert("a")
      call set1%insert("b")

      set2 = set1

      call set2 % insert("c")

      if ( ("c" .in. set2) .eqv. .FALSE. ) error stop
      if ( ("c" .in. set1) .eqv. .TRUE. ) error stop
   end block

   block

      type(set_t) :: set1, set2

      call set1 % insert(1)
      set1 = set2

      if ( size(set1) /= 0 ) error stop

   end block
   print '(a)', "  OK"
end program
