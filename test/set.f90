program settest

   use yaftree_m
   implicit none

   type(set_t) :: myset

   print '(a)', " ********* TESTING SET *********"

   call insert(myset, "abc")
   call insert(myset, 3)
   call insert(myset, 1.0)

   if (("abc" .in. myset) .neqv. .true.) error stop
   if ((3 .in. myset) .neqv. .true.) error stop
   if ((34 .in. myset) .neqv. .false.) error stop
   if ((1.0 .in. myset) .neqv. .true.) error stop
   if (size(myset) /= 3) error stop

end program
