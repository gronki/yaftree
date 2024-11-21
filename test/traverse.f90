program test_traverse

   use yaftree_m

   type(set_t) :: set
   character(len=*), parameter :: words(*) = &
      [character(len=12) :: "this", "is", "my", "body", "and", "my", "soul", "and", "my", "mind"]
   character(len=*), parameter :: uniq_words(*) = &
      [character(len=12) :: "this", "is", "my", "body", "and", "soul", "mind"]
   integer :: i
   character(len=12), allocatable :: occurences(:)

   do i = 1, size(words)
      call set%insert(trim(words(i)))
   end do

   allocate(occurences(size(set)))

   associate (items => set%keys())
      do i =1, size(set)
         select type(key => items(i) % key)
         type is (character(len=*))
            print *, key
            occurences(i) = key
         end select
      end do
   end associate

   do i = 1, size(uniq_words)
      if (count(uniq_words(i) == occurences) /= 1) error stop
   end do

   print *, size(set)

   if (size(set) /= size(uniq_words)) error stop

end program test_traverse
