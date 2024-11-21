program settest

    use yaftree_m
    implicit none

    type(set_t) :: myset

    print '(a)', " ********* TESTING SET *********"

    call myset % insert("abc")
    call myset % insert(3)
    call myset % insert(1.0)

    if (myset % contains("abc") .neqv. .true.) error stop
    if (myset % contains(3) .neqv. .true.) error stop
    if (myset % contains(34) .neqv. .false.) error stop
    if ((1.0 .in. myset) .neqv. .true.) error stop
    if (size(myset) /= 3) error stop

end program
