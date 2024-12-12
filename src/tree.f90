submodule (yaftree_m) tree_impl
contains

 !> Test if logical argument is present and set to true.
elemental logical function present_and_true(arg)
   !> Argument to be checked.
   logical, intent(in), optional :: arg
   present_and_true = .false.
   if (present(arg)) then
      present_and_true = arg
   end if
end function


recursive pure subroutine get_or_create_node(hashed_key, node, get_value, put_value, alloc_node)
   type(hashed_key_t), intent(in) :: hashed_key
   type(binary_tree_node_t), intent(inout), allocatable :: node
   class(*), intent(inout), allocatable, optional :: get_value
   class(*), intent(in), optional :: put_value
   logical, intent(in), optional :: alloc_node

   if (.not. allocated(node)) then
      if (present(get_value)) then
         get_value = key_not_found_t()
      end if
      if (present(put_value) .or. present_and_true(alloc_node)) then
         allocate(node)
         node % hashed_key = hashed_key
      end if
      if (present(put_value)) then
         node % value = put_value
      end if
      return
   end if

   if (hashed_key == node % hashed_key) then
      if (present(get_value)) get_value = node % value
      if (present(put_value)) node % value = put_value
      return
   end if

   if (hashed_key < node % hashed_key) then
      call get_or_create_node(hashed_key, node % lo_leaf, &
         get_value=get_value, put_value=put_value, alloc_node=alloc_node)
   else
      call get_or_create_node(hashed_key, node % hi_leaf, &
         get_value=get_value, put_value=put_value, alloc_node=alloc_node)
   end if

end subroutine


recursive pure subroutine get_value_recursive(hashed_key, node, val)
   type(hashed_key_t), intent(in) :: hashed_key
   type(binary_tree_node_t), allocatable, intent(in) :: node
   class(*), intent(inout), allocatable :: val

   if (.not. allocated(node)) then
      val = key_not_found_t()
      return
   end if

   if (hashed_key == node % hashed_key) then
      val = node % value
      return
   end if

   if (hashed_key < node % hashed_key) then
      call get_value_recursive(hashed_key, node % lo_leaf, val)
   else
      call get_value_recursive(hashed_key, node % hi_leaf, val)
   end if

end subroutine


recursive pure subroutine check_key_recursive(hashed_key, node, key_found)
   type(hashed_key_t), intent(in) :: hashed_key
   type(binary_tree_node_t), allocatable, intent(in) :: node
   logical, intent(out) :: key_found

   if (.not. allocated(node)) then
      key_found = .false.
      return
   end if

   if (hashed_key == node % hashed_key) then
      key_found = .true.
      return
   end if

   if (hashed_key < node % hashed_key) then
      call check_key_recursive(hashed_key, node % lo_leaf, key_found)
   else
      call check_key_recursive(hashed_key, node % hi_leaf, key_found)
   end if

end subroutine


recursive pure subroutine count_size_recursive(node, tree_size)
   type(binary_tree_node_t), allocatable, intent(in) :: node
   integer, intent(inout) :: tree_size

   if (.not. allocated(node)) then
      return
   end if

   tree_size = tree_size + 1

   call count_size_recursive(node % lo_leaf, tree_size)
   call count_size_recursive(node % hi_leaf, tree_size)

end subroutine

recursive pure subroutine collect_items_recursive(node, items, item_index, what)
   type(binary_tree_node_t), allocatable, intent(in) :: node
   type(item_t), intent(inout) :: items(:)
   integer, intent(inout) :: item_index
   character(len=1), intent(in) :: what

   if (.not. allocated(node)) then
      return
   end if

   item_index = item_index + 1

   if (what /= 'V') items(item_index) % key = node % hashed_key % orig_key
   if (what /= 'K') items(item_index) % value = node % value

   call collect_items_recursive(node % lo_leaf, items, item_index, what)
   call collect_items_recursive(node % hi_leaf, items, item_index, what)

end subroutine


elemental module function key_in_tree(key, tree) result(contains)
   class(*), intent(in) :: key
   class(dict_set_base_t), intent(in) :: tree
   logical :: contains

   call check_key_recursive(new_hashed_key(tree % hasher, key), tree % root, contains)

end function


elemental module function tree_size(tree)
   class(dict_set_base_t), intent(in) :: tree
   integer :: tree_size

   tree_size = 0
   call count_size_recursive(tree % root, tree_size)

end function

!> retrieve copy of all stored keys
pure module function get_tree_keys(tree) result(keys)
   !> Container to be queried.
   class(dict_set_base_t), intent(in) :: tree
   !> List of items
   type(item_t), allocatable :: keys(:)

   integer :: item_num, n_elems

   n_elems = size(tree)
   allocate(keys(n_elems))
   if (n_elems == 0) return

   item_num = 0

   call collect_items_recursive(tree % root, keys, item_num, 'K')

   if (item_num /= n_elems) &
      error stop "yaftree: inconsistency while collecting tree items."

end function

pure recursive module subroutine binary_tree_copy( source_node, dest_node )
   type(binary_tree_node_t), intent(in), allocatable :: source_node
   type(binary_tree_node_t), intent(inout), allocatable :: dest_node

   if ( .not. allocated(source_node) ) return

   allocate ( dest_node )

   dest_node % hashed_key = source_node % hashed_key

   if ( allocated(source_node % value) ) then
      allocate( dest_node % value, source = source_node % value )
   end if

   call binary_tree_copy( source_node % lo_leaf, dest_node % lo_leaf )
   call binary_tree_copy( source_node % hi_leaf, dest_node % hi_leaf )

end subroutine

pure module subroutine dict_set_copy(dest, source)
   class(dict_set_base_t), intent(inout) :: dest
   class(dict_set_base_t), intent(in) :: source

   if ( allocated(dest % root) ) deallocate( dest % root )

   call binary_tree_copy( dest_node = dest % root, source_node = source % root )
   dest % hasher => source % hasher

end subroutine

pure module subroutine dict_insert(tab, key, val)
   class(dict_t), intent(inout) :: tab
   class(*), intent(in) :: key, val

   call get_or_create_node(new_hashed_key(tab % hasher, key), &
      tab % root, put_value=val)

end subroutine


pure module function dict_get(tab, key) result(val)
   class(dict_t), intent(in) :: tab
   class(*), intent(in) :: key
   class(*), allocatable :: val

   call get_value_recursive(new_hashed_key(tab % hasher, key), tab % root, val)

end function


pure module subroutine set_insert(tab, key)
   class(set_t), intent(inout) :: tab
   class(*), intent(in) :: key

   call get_or_create_node(new_hashed_key(tab % hasher, key), &
      tab % root, alloc_node=.true.)

end subroutine


end submodule
