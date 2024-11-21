submodule (yaftree_m) default_hash

   use iso_fortran_env, only: int8, int32, int64
   implicit none (type, external)

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

      hashed_key % key = transfer(key, hashed_key % key)
      hashed_key % hash = hasher(hashed_key % key)
   end function

   elemental module function hashed_keys_equal(key, other)
      type(hashed_key_t), intent(in) :: key, other
      logical :: hashed_keys_equal

      hashed_keys_equal = .false.
      if (key % hash /= other % hash) return
      if (size(key % key) /= size(other % key)) return
      if (any(key % key /= other % key)) return
      hashed_keys_equal = .true.
   end function

   elemental module function hashed_keys_less(key, other)
      type(hashed_key_t), intent(in) :: key, other
      logical :: hashed_keys_less

      hashed_keys_less = (key % hash < other % hash)
   end function

end submodule