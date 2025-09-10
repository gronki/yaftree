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


recursive subroutine get_or_create_node(key, hashed_key, node, get_value, put_value, alloc_node)
   class(*), intent(in) :: key
   type(hashed_key_t), intent(in) :: hashed_key
   type(binary_tree_node_t), intent(inout), allocatable :: node
   class(*), intent(inout), allocatable, optional :: get_value
   class(*), intent(in), optional :: put_value
   logical, intent(in), optional :: alloc_node

   if (.not. allocated(node)) then
      if (present(get_value)) get_value = key_not_found_t()
      if (present(put_value) .or. present_and_true(alloc_node)) then
         allocate(node)
         node % orig_key = key
         node % hashed_key = hashed_key
      end if
      if (present(put_value))  node % value = put_value
      return
   end if

   if (hashed_key == node % hashed_key .and. same_type_as(key, node % orig_key)) then
      if (present(get_value)) get_value = node % value
      if (present(put_value)) node % value = put_value
      return
   end if

   if (hashed_key < node % hashed_key) then
      call get_or_create_node(key, hashed_key, node % lo_leaf, &
         get_value=get_value, put_value=put_value, alloc_node=alloc_node)
   else
      call get_or_create_node(key, hashed_key, node % hi_leaf, &
         get_value=get_value, put_value=put_value, alloc_node=alloc_node)
   end if

end subroutine

recursive subroutine node_ref_recursive(key, hashed_key, node, create, node_ref)
   class(*), intent(in) :: key
   type(hashed_key_t), intent(in) :: hashed_key
   type(binary_tree_node_t), intent(inout), allocatable, target :: node
   type(binary_tree_node_t), pointer :: node_ref
   logical, intent(in) :: create

   if (.not. allocated(node)) then
      if (.not. create) then
         nullify(node_ref); return
      end if
      allocate(node)
      node % orig_key = key
      node % hashed_key = hashed_key
      node_ref => node
      return
   end if

   if (hashed_key == node % hashed_key .and. same_type_as(key, node % orig_key)) then
      node_ref => node
      return
   end if

   if (hashed_key < node % hashed_key) then
      call node_ref_recursive(key, hashed_key, node % lo_leaf, create, node_ref)
   else
      call node_ref_recursive(key, hashed_key, node % hi_leaf, create, node_ref)
   end if

end subroutine


recursive subroutine get_value_recursive(key, hashed_key, node, val)
   class(*), intent(in) :: key
   type(hashed_key_t), intent(in) :: hashed_key
   type(binary_tree_node_t), allocatable, intent(in) :: node
   class(*), intent(inout), allocatable :: val

   if (.not. allocated(node)) then
      val = key_not_found_t()
      return
   end if

   if (hashed_key == node % hashed_key .and. same_type_as(key, node % orig_key)) then
      val = node % value
      return
   end if

   if (hashed_key < node % hashed_key) then
      call get_value_recursive(key, hashed_key, node % lo_leaf, val)
   else
      call get_value_recursive(key, hashed_key, node % hi_leaf, val)
   end if

end subroutine


recursive subroutine get_value_ptr_recursive(key, hashed_key, node, ptr)
   class(*), intent(in) :: key
   type(hashed_key_t), intent(in) :: hashed_key
   type(binary_tree_node_t), allocatable, intent(in), target :: node
   class(*), intent(inout), pointer :: ptr

   if (.not. allocated(node)) then
      ptr => not_found
      return
   end if

   if (hashed_key == node % hashed_key .and. same_type_as(key, node % orig_key)) then
      ptr => node % value
      return
   end if

   if (hashed_key < node % hashed_key) then
      call get_value_ptr_recursive(key, hashed_key, node % lo_leaf, ptr)
   else
      call get_value_ptr_recursive(key, hashed_key, node % hi_leaf, ptr)
   end if

end subroutine


pure recursive subroutine check_key_recursive(key, hashed_key, node, key_found)
   class(*), intent(in) :: key
   type(hashed_key_t), intent(in) :: hashed_key
   type(binary_tree_node_t), allocatable, intent(in) :: node
   logical, intent(out) :: key_found

   if (.not. allocated(node)) then
      key_found = .false.
      return
   end if

   if (hashed_key == node % hashed_key .and. same_type_as(key, node % orig_key)) then
      key_found = .true.
      return
   end if

   if (hashed_key < node % hashed_key) then
      call check_key_recursive(key, hashed_key, node % lo_leaf, key_found)
   else
      call check_key_recursive(key, hashed_key, node % hi_leaf, key_found)
   end if

end subroutine


pure recursive subroutine count_size_recursive(node, tree_size)
   type(binary_tree_node_t), allocatable, intent(in) :: node
   integer, intent(inout) :: tree_size

   if (.not. allocated(node)) then
      return
   end if

   tree_size = tree_size + 1

   call count_size_recursive(node % lo_leaf, tree_size)
   call count_size_recursive(node % hi_leaf, tree_size)

end subroutine

recursive subroutine collect_items_recursive(node, items, item_index, what)
   type(binary_tree_node_t), allocatable, intent(in) :: node
   type(dict_set_item_t), intent(inout) :: items(:)
   integer, intent(inout) :: item_index
   character(len=1), intent(in) :: what

   if (.not. allocated(node)) then
      return
   end if

   item_index = item_index + 1

   if (what /= 'V') items(item_index) % key = node % orig_key
   if (what /= 'K') items(item_index) % value = node % value

   call collect_items_recursive(node % lo_leaf, items, item_index, what)
   call collect_items_recursive(node % hi_leaf, items, item_index, what)

end subroutine


elemental module function key_in_tree(key, tree) result(contains)
   class(*), intent(in) :: key
   class(dict_set_base_t), intent(in) :: tree
   logical :: contains

   if (.not. associated(tree % hasher)) &
      error stop "ERROR: hash function not associated for set, example: container%hasher => fnv_hash"

   call check_key_recursive(key, hashed_key_t(tree % hasher, key), tree % root, contains)

end function

elemental module function key_not_in_tree(key, tree) result(not_contains)
   !> Key or set item to be queried.
   class(*), intent(in) :: key
   !> Hashmap.
   class(dict_set_base_t), intent(in) :: tree
   !> Return value, or yahft_not_found if key is not present.
   logical :: not_contains

   not_contains = .not. key_in_tree(key, tree)
end function

elemental module function tree_size(tree)
   class(dict_set_base_t), intent(in) :: tree
   integer :: tree_size

   tree_size = 0
   call count_size_recursive(tree % root, tree_size)

end function

!> retrieve copy of all stored keys
module function get_tree_keys(tree) result(result_keys)
   !> Container to be queried.
   class(dict_set_base_t), intent(in) :: tree
   !> List of items
   type(dict_set_item_t), allocatable :: result_keys(:)

   integer :: item_num, n_elems

   n_elems = size(tree)
   allocate(result_keys(n_elems))
   if (n_elems == 0) return

   item_num = 0

   call collect_items_recursive(tree % root, result_keys, item_num, 'K')

   if (item_num /= n_elems) &
      error stop "yaftree: inconsistency while collecting tree items."

end function

recursive module subroutine binary_tree_copy( source_node, dest_node )
   type(binary_tree_node_t), intent(in), allocatable :: source_node
   type(binary_tree_node_t), intent(inout), allocatable :: dest_node

   if ( .not. allocated(source_node) ) return

   allocate ( dest_node )

   dest_node % hashed_key = source_node % hashed_key
   allocate( dest_node % orig_key, source = source_node % orig_key )

   if ( allocated(source_node % value) ) then
      allocate( dest_node % value, source = source_node % value )
   end if

   call binary_tree_copy( source_node % lo_leaf, dest_node % lo_leaf )
   call binary_tree_copy( source_node % hi_leaf, dest_node % hi_leaf )

end subroutine

module subroutine dict_set_copy(dest, source)
   class(dict_set_base_t), intent(inout) :: dest
   class(dict_set_base_t), intent(in) :: source

   if ( allocated(dest % root) ) deallocate( dest % root )

   call binary_tree_copy( dest_node = dest % root, source_node = source % root )
   dest % hasher => source % hasher

end subroutine

module subroutine dict_insert(tab, key, val)
   type(dict_t), intent(inout) :: tab
   class(*), intent(in) :: key, val

   if (.not. associated(tab % hasher)) &
      error stop "ERROR: hash function not associated for set, example: mydict%hasher => fnv_hash"

   call get_or_create_node(key, hashed_key_t(tab % hasher, key), &
      tab % root, put_value=val)

end subroutine


module function dict_get(tab, key) result(val)
   type(dict_t), intent(in) :: tab
   class(*), intent(in) :: key
   class(*), allocatable :: val

   if (.not. associated(tab % hasher)) &
      error stop "ERROR: hash function not associated for set, example: mydict%hasher => fnv_hash"

   call get_value_recursive(key, hashed_key_t(tab % hasher, key), &
      tab % root, val)

end function

module function dict_get_ptr(tab, key) result(ptr)
   type(dict_t), intent(in), target :: tab
   class(*), intent(in) :: key
   class(*), pointer :: ptr

   if (.not. associated(tab % hasher)) &
      error stop "ERROR: hash function not associated for set, example: mydict%hasher => fnv_hash"

   call get_value_ptr_recursive(key, hashed_key_t(tab % hasher, key), &
      tab % root, ptr)

end function

module function dict_get_node(tab, key, create) result(ptr)
   !> Hashmap.
   type(dict_t), intent(inout), target :: tab
   !> Key or set item to be queried.
   class(*), intent(in) :: key
   !> Node pointer.
   type(binary_tree_node_t), pointer :: ptr
   !> create node if does not exist?
   logical, intent(in) :: create

   if (.not. associated(tab % hasher)) &
      error stop "ERROR: hash function not associated for set, example: mydict%hasher => fnv_hash"

   call node_ref_recursive(key, hashed_key_t(tab % hasher, key), &
      tab % root, create, ptr)

end function

module function dict_get_node_create(tab, key) result(ptr)
   !> Hashmap.
   type(dict_t), intent(inout), target :: tab
   !> Key or set item to be queried.
   class(*), intent(in) :: key
   !> Node pointer.
   type(binary_tree_node_t), pointer :: ptr

   if (.not. associated(tab % hasher)) &
      error stop "ERROR: hash function not associated for set, example: mydict%hasher => fnv_hash"

   ptr => dict_get_node(tab, key, .true.)
end function

module subroutine set_insert(tab, key)
   type(set_t), intent(inout) :: tab
   class(*), intent(in) :: key

   if (.not. associated(tab % hasher)) &
      error stop "ERROR: hash function not associated for set, example: myset%hasher => fnv_hash"

   call get_or_create_node(key, hashed_key_t(tab % hasher, key), &
      tab % root, alloc_node=.true.)

end subroutine


end submodule
