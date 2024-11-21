program benchmark_maps

    use yaftree_m
    implicit none

    type :: test_case_t
        character(len=20) :: word
        integer :: pos
    end type

    type(test_case_t), parameter :: test_cases(*) = [ &
        test_case_t(word="shopping", pos=360), &
        test_case_t(word="collections", pos=3134), &
        test_case_t(word="eugene", pos=7990), &
        test_case_t(word="researched", pos=15881), &
        test_case_t(word="mandolin", pos=22974), &
        test_case_t(word="kukunamuniu", pos=-1), &
        test_case_t(word="raiser", pos=28671), &
        test_case_t(word="yorktown", pos=29399), &
        test_case_t(word="brookhaven", pos=29946), &
        test_case_t(word="zzzzzzzzz", pos=-1), &
        test_case_t(word="distinction", pos=8137), &
        test_case_t(word="meade", pos=17943), &
        test_case_t(word="combos", pos=22671), &
        test_case_t(word="output", pos=1477), &
        test_case_t(word="craft", pos=4241) ]

    print '(a)', " ********* TESTING AND BENCHMARKING DICT *********"


    block
        type(dict_t) :: map
        print *, "=========== HASH = FNV_HASH ==========="
        call load_dataset(map)
        call test_correctness(map)
        print *, size(map)
      !   call benchmark(map)
    end block
   !  block
   !      type(dict_t) :: map
   !      map % hasher => len_hash
   !      print *, "=========== HASH = LEN_HASH ==========="
   !      call load_dataset(map)
   !      call test_correctness(map)
   !      print *, size(map)
   !      !   call benchmark(map)
   !  end block
   !  block
   !      type(dict_t) :: map
   !      map % hasher => sum_hash
   !      print *, "=========== HASH = SUM_HASH ==========="
   !      call load_dataset(map)
   !      call test_correctness(map)
   !      print *, size(map)
   !      !   call benchmark(map)
   !  end block

contains

    subroutine load_dataset(map)
        class(dict_t) :: map
        integer :: lun, errno
        character(len=20) :: row
        integer :: i
        open(newunit=lun, file="english30k.txt", action="read")
        do i = 1, 99999
            read (lun, *, iostat=errno) row
            if (errno /= 0) exit
            call map % insert(trim(row), i)
        end do
        close(lun)
        print *, 'load and insert complete'
    end subroutine

    subroutine test_correctness(map)
        class(dict_t) :: map
        logical :: is_correct
        integer :: i

        is_correct = .true.

        do i = 1, size(test_cases)
            is_correct = is_correct .and. &
                testkey(map, trim(test_cases(i) % word), test_cases(i) % pos)
        end do

        print *, "** " // trim(merge("OK  ", "FAIL", is_correct)) // " **"

    end subroutine


    function testkey(map, key, expected_pos) result(is_correct)
        type(dict_t) :: map
        character(len=*) :: key
        integer :: expected_pos
        logical :: is_correct1, is_correct2, is_correct

        select type(actual_pos => map % get(key))
          type is(integer)
            is_correct1 = (actual_pos == expected_pos)

            write (*, "(a18, a10, i8, a10, i8, a5)") key, " expected: ", expected_pos, &
                " retrieved:", actual_pos, " " // trim(merge("   ", "ERR", is_correct1))
          type is (key_not_found_t)
            is_correct1 = expected_pos == -1
            write (*, "(a18, a10, i8, a10, a8, a5)") key, " expected: ", expected_pos, &
                " retrieved:", "(NOTFND)", " " // trim(merge("   ", "ERR", is_correct1))
        end select

        is_correct2 = (expected_pos /= -1) .eqv. map % contains(key)
        write (*, "(a18, a10, l8, a10, l8, a5)") key, " exists ", expected_pos /= -1, &
            " retrieved:", map%contains(key), " " // trim(merge("   ", "ERR", is_correct2))

        is_correct = is_correct1 .and. is_correct2
    end function


    subroutine benchmark(map)
        use iso_fortran_env, only: real64
        class(dict_t) :: map
        logical :: is_correct
        integer, parameter :: loops = 1000
        real(real64) :: t_start, t_end
        integer :: i, j

        is_correct = .true.

        call cpu_time(t_start)

        do j = 1, loops
            do i = 1, size(test_cases)
                select type (val => map % get(trim(test_cases(i) % word)))
                  type is (integer)
                    is_correct = is_correct .and. (val == test_cases(i) % pos)
                  type is (key_not_found_t)
                    is_correct = is_correct .and. (-1 == test_cases(i) % pos)
                  class default
                end select
            end do
        end do
        call cpu_time(t_end)

        print '(a,f8.3)', "** " // trim(merge("OK  ", "FAIL", is_correct)) // " **    time[s]: ", t_end - t_start

    end subroutine

    pure function len_hash(data) result(hash)
        use iso_fortran_env, only: int8
        integer(int8), intent(in) :: data(:)
        integer(hash_k) :: hash

        hash = size(data)
    end function

    pure function sum_hash(data) result(hash)
        use iso_fortran_env, only: int8
        integer(int8), intent(in) :: data(:)
        integer(hash_k) :: hash
        integer :: i

        hash = sum([ (int(data(i), hash_k) + huge(data(i)), i = 1, size(data)) ])
    end function

end program
