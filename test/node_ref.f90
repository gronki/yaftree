program test_move_ref
   use yaftree_m

   implicit none(type, external)

   integer, allocatable :: b
   type(dict_t) :: dict
   type(binary_tree_node_t), pointer :: node

   dict%hasher => fnv_hash

   call insert(dict, "c", 15)

   b = 10

   node => get_node(dict, "b")
   if (.not. associated(node)) error stop

   call move_alloc(b, node%value)

   select type (b1 => get(dict, "b"))
   type is (key_not_found_t)
      error stop
   type is (integer)
      print *, "ok ", b1
   class default
      error stop
   end select

   node => get_node(dict, "b", .false.)
   if (.not. associated(node)) error stop

   node => get_node(dict, "a", .false.)
   if (associated(node)) error stop


end program
