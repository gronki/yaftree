program test_traverse

   use yaftree_m
   implicit none (type, external)

   type(set_t) :: set
   character(len=*), parameter :: words(*) = &
      [character(len=12) :: "this", "is", "my", "body", "and", "my", "soul", "and", "my", "mind"]
   character(len=*), parameter :: uniq_words(*) = &
      [character(len=12) :: "this", "is", "my", "body", "and", "soul", "mind"]
   integer :: i, count_uniq
   character(len=12), allocatable :: occurences(:)

   do i = 1, size(words)
      call insert(set, trim(words(i)))
   end do

   do i = 1, size(words)
      if (trim(words(i)) .notin. set) then
         print *, "word ", trim(words(i)), " inserted but lost in the set"
         error stop 
      endif
   end do

   print *, "no word was lost"

   print *, size(set)

   if (size(set) /= size(uniq_words)) error stop "size(set) /= size(uniq_words)"

   allocate(occurences(size(set)))

   associate (items => set % keys())
      do i =1, size(set)
         select type(key => items(i) % key)
         type is (character(len=*))
            print *, key
            occurences(i) = key
         end select
      end do
   end associate

   do i = 1, size(uniq_words)
      count_uniq = count(uniq_words(i) == occurences)
      print *, trim(uniq_words(i)), " occurs times: ", count_uniq
      if (count_uniq /= 1) error stop "count(uniq_words(i) == occurences) /= 1"
   end do

end program test_traverse
