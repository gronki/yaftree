!>
!> Yaftree - Yet Another Fortran Tree
!> (c) Dominik Gronkiewicz
!> Distributed under BSD 3-clause license, see LICENSE file.
!>
module yaftree_m

use iso_fortran_env, only: int8, int32
implicit none (type, external)
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

interface
pure recursive module subroutine binary_tree_copy( source_node, dest_node )
   type(binary_tree_node_t), intent(in), allocatable :: source_node
   type(binary_tree_node_t), intent(inout), allocatable :: dest_node
end subroutine
end interface

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
   ! gfortran 14 crashed trying to deep copy recursive types
   ! so we help him
   procedure, non_overridable, private :: dict_set_copy
   generic :: assignment(=) => dict_set_copy
   procedure, non_overridable :: keys => get_tree_keys
end type

interface size
elemental module function tree_size(tree)
   class(dict_set_base_t), intent(in) :: tree
   integer :: tree_size
end function
end interface

interface operator(.in.)
 !> Subroutine to get value.
elemental module function key_in_tree(key, tree) result(contains)
   !> Key or set item to be queried.
   class(*), intent(in) :: key
   !> Hashmap.
   class(dict_set_base_t), intent(in) :: tree
   !> Return value, or yahft_not_found if key is not present.
   logical :: contains
end function
end interface

interface keys
!> retrieve copy of all stored keys
!> Warning: in gfortran, use ``tree%keys()`` rather than ``keys(tree)`` with ``associate``.
pure module function get_tree_keys(tree) result(result_keys)
   !> Container to be queried.
   class(dict_set_base_t), intent(in) :: tree
   !> List of items
   type(item_t), allocatable :: result_keys(:)
end function
end interface

interface ! bound to type dict_set_base_t
 !> Assignment (copy)
pure module subroutine dict_set_copy(dest, source)
   class(dict_set_base_t), intent(inout) :: dest
   class(dict_set_base_t), intent(in) :: source
end subroutine
end interface

 !> Binary tree dict/hashmap implementation.
type, extends(dict_set_base_t) :: dict_t
end type

interface insert
 !> Subroutine for inserting a hashable key to a dictionary/set.
pure module subroutine dict_insert(tab, key, val)
   !> Hashmap.
   type(dict_t), intent(inout) :: tab
   !> Key or set item to be inserted.
   class(*), intent(in) :: key
   !> Value to be allocated in the dictionary.
   class(*), intent(in) :: val
end subroutine
end interface

interface get
 !> Subroutine to get value.
pure module function dict_get(tab, key) result(val)
   !> Hashmap.
   type(dict_t), intent(in) :: tab
   !> Key or set item to be queried.
   class(*), intent(in) :: key
   !> Return value, or yahft_not_found if key is not present.
   class(*), allocatable :: val
end function
end interface

 !> Binary tree dict/hashmap implementation.
type, extends(dict_set_base_t) :: set_t
end type

interface insert
 !> Subroutine for inserting a hashable key to a dictionary/set.
pure module subroutine set_insert(tab, key)
   !> Set.
   type(set_t), intent(inout) :: tab
   !> Key or set item to be inserted.
   class(*), intent(in) :: key
end subroutine
end interface

public :: dict_t, set_t, item_t, insert, get, size, keys, operator(.in.)

end module yaftree_m
