program test_type_preserve

   use iso_fortran_env
   use yaftree_m

   implicit none

   character(len=8) :: s1
   integer(int64) :: a

   s1 = "hey ho"
   a = transfer(s1, int(1, kind=int64))

   block
      type(set_t) :: set
      set % hasher => fnv_hash

      call insert(set, s1)
      if ( (s1 .in. set) .neqv. .true. ) error stop 101
      if ( (a .in. set) .neqv. .false. ) error stop 102
   end block
   
   block
      type(set_t) :: set
      set % hasher => fnv_hash

      call insert(set, a)
      if ( (s1 .in. set) .neqv. .false. ) error stop 201
      if ( (a .in. set) .neqv. .true. ) error stop 202
   end block

end program
