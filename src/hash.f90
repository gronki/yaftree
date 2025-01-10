submodule (yaftree_m) default_hash

use iso_fortran_env, only: int8, int32, int64

contains


pure module function fnv_hash(data) result(hash)
   integer(int8), intent(in) :: data(:)
   integer(hash_k) :: hash
   integer(int64) :: hash_wide

   integer(hash_k), parameter :: initial_hash = int(z"811c9dc5", kind(hash))
   integer(hash_k), parameter :: prime = int(z"01000193", kind(hash))
   integer :: i

   hash_wide = initial_hash

   do i = 1, size(data)
      hash_wide = ieor(hash_wide, int(data(i), kind(hash_wide)))
      hash_wide = hash_wide * prime
      hash_wide = modulo(hash_wide, int(huge(hash), kind(hash_wide)) + 1)
   end do

   hash = int(hash_wide, kind(hash))

end function


pure module function new_hashed_key(hasher, key) result(hashed_key)
   procedure(hasher_proto) :: hasher
   class(*), intent(in) :: key
   type(hashed_key_t) :: hashed_key

   hashed_key % orig_key = key
   hashed_key % key = transfer(key, hashed_key % key)
   hashed_key % hash = hasher(hashed_key % key)

end function


elemental module function hashed_keys_equal(key, other)
   type(hashed_key_t), intent(in) :: key, other
   logical :: hashed_keys_equal
   integer :: i

   hashed_keys_equal = .false.
   ! hashes must be equal
   if (key % hash /= other % hash) return
   ! dynamic types of key must be equal
   if (.not. same_type_as(key % orig_key, other % orig_key)) return
   ! storage size of keys must be equal
   if (size(key % key) /= size(other % key)) return
   ! byte values of keys must be equal
   do i = 1, size(key % key)
      if (key % key(i) /= other % key(i)) return
   end do
   hashed_keys_equal = .true.

end function


elemental module function hashed_keys_less(key, other)
   type(hashed_key_t), intent(in) :: key, other
   logical :: hashed_keys_less

   hashed_keys_less = (key % hash < other % hash)

end function


end submodule
