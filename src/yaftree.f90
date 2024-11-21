!>
!> Yaftree - Yet Another Fortran Tree
!> (c) Dominik Gronkiewicz
!> Distributed under BSD 3-clause license, see LICENSE file.
!>
module yaftree_m

use iso_fortran_env, only: int8, int32
private

integer, parameter :: hash_k = int32
public :: hash_k

abstract interface
   !< Hashing function prototype.
   pure function hasher_proto(data) result(hash)
      import :: int8, hash_k
      !> Data, TRANSFERed to int8 array.
      integer(int8), intent(in) :: data(:)
      !> Computed hash.
      integer(hash_k) :: hash
   end function
end interface

interface
   !> FNV hash algorithm.
   pure module function fnv_hash(data) result(hash)
      !> Data, TRANSFERed to int8 array.
      integer(int8), intent(in) :: data(:)
      !> Computed hash.
      integer(hash_k) :: hash
   end function
end interface

public :: fnv_hash

 !> Type storing one hashed data item alongside the hash.
type :: hashed_key_t
   !> Key, TRANSFERed to int8 array.
   integer(kind=int8), allocatable :: key(:)
   !> Original key, only kept for retrieval
   class(*), allocatable :: orig_key
   !> Computer hash value.
   integer(kind=hash_k) :: hash = 0
end type

interface hashed_key_t
   !> Convenience function for hashing any type.
   pure module function new_hashed_key(hasher, key) result(hashed_key)
      !> Hasher procedure.
      procedure(hasher_proto) :: hasher
      !> Key of any type to be hashed.
      !> Keep in mind that derived types with pointer or allocatable components
      !> will not produce the expected behavior.
      class(*), intent(in) :: key
      !> Resulting hash.
      type(hashed_key_t) :: hashed_key
   end function
end interface

interface operator(==)
   elemental module function hashed_keys_equal(key, other)
      type(hashed_key_t), intent(in) :: key, other
      logical :: hashed_keys_equal
   end function
end interface

interface operator(<)
   elemental module function hashed_keys_less(key, other)
      type(hashed_key_t), intent(in) :: key, other
      logical :: hashed_keys_less
   end function
end interface


 !> Object of type key_not_found_t is returned by get() when they key was not found.
type key_not_found_t
end type

public :: key_not_found_t

 !> Binary tree node.
type :: binary_tree_node_t
   !> Lower and higher leaf.
   type(binary_tree_node_t), allocatable :: lo_leaf, hi_leaf
   !> Hashed key.
   type(hashed_key_t) :: hashed_key
   !> Allocatable value.
   class(*), allocatable :: value
end type

!> helper class to wrap allocatable items for keys/values
type :: item_t
   class(*), allocatable :: key, value
end type

type :: dict_set_base_t
   !> Tree root.
   type(binary_tree_node_t), allocatable, private :: root
   !> Hashing procedure. May be exchanged for anything else matching
   !> the hasher_proto inteface.
   procedure(hasher_proto), nopass, pointer :: hasher => fnv_hash
contains
   procedure, pass(tree) :: contains => key_in_tree
   ! below bugs ifx2025, so need to use interface
   ! generic :: operator(.in.) => contains
   procedure :: keys => get_tree_keys
end type

interface
   !> Subroutine to get value.
   elemental module function key_in_tree(key, tree) result(contains)
      !> Key or set item to be queried.
      class(*), intent(in) :: key
      !> Hashmap.
      class(dict_set_base_t), intent(in) :: tree
      !> Return value, or yahft_not_found if key is not present.
      logical :: contains
   end function
   !> retrieve copy of all stored keys
   pure module function get_tree_keys(tree) result(keys)
      !> Container to be queried.
      class(dict_set_base_t), intent(in) :: tree
      !> List of items
      type(item_t), allocatable :: keys(:)
   end function
end interface

interface operator(.in.)
   module procedure key_in_tree
end interface

public :: operator(.in.)

interface size
   elemental module function tree_size(tree)
      class(dict_set_base_t), intent(in) :: tree
      integer :: tree_size
   end function
end interface

public :: size

 !> Binary tree dict/hashmap implementation.
type, extends(dict_set_base_t) :: dict_t
contains
   procedure :: insert => dict_insert
   procedure :: get => dict_get
end type

interface
   !> Subroutine for inserting a hashable key to a dictionary/set.
   pure module subroutine dict_insert(tab, key, val)
      !> Hashmap.
      class(dict_t), intent(inout) :: tab
      !> Key or set item to be inserted.
      class(*), intent(in) :: key
      !> Value to be allocated in the dictionary.
      class(*), intent(in) :: val
   end subroutine

   !> Subroutine to get value.
   pure module function dict_get(tab, key) result(val)
      !> Hashmap.
      class(dict_t), intent(in) :: tab
      !> Key or set item to be queried.
      class(*), intent(in) :: key
      !> Return value, or yahft_not_found if key is not present.
      class(*), allocatable :: val
   end function
end interface

public :: dict_t

 !> Binary tree dict/hashmap implementation.
type, extends(dict_set_base_t) :: set_t
contains
   procedure :: insert => set_insert
end type

interface
   !> Subroutine for inserting a hashable key to a dictionary/set.
   pure module subroutine set_insert(tab, key)
      !> Set.
      class(set_t), intent(inout) :: tab
      !> Key or set item to be inserted.
      class(*), intent(in) :: key
   end subroutine
end interface

public :: set_t

end module yaftree_m
