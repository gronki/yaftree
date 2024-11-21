program tree_demo

    use yaftree_m

    type(dict_t) :: dict
    type(set_t) :: set

    print *, 'call set % insert("abc")'
    call set % insert("abc")
    print *, 'call set % insert(321)'
    ! any type without pointers or allocatable components is valid
    call set % insert(321)

    ! size overloaded for consistency with arrays
    print *, "size(set)", "=", size(set)
    ! .in. operator -- like Python
    print *, '"abc" .in. set',  "=", "abc" .in. set
    ! contains procedure is equivalent to .in.
    print *, 'set%contains(321)',  "=", set%contains(321)

    print *, 'call dict % insert("my", "value")'
    call dict % insert("my", "value")
    print *, 'call dict % insert(15, 35)'
    call dict % insert(15, 35)

    print *, '"my" .in. dict',  "=", "my" .in. dict

    ! typically you will retrieve stored values like this
    select type (val => dict % get("my"))
      type is (character(len=*))
        print *, 'dict % get("my")',  "=", val
    end select

    ! when key is not found, special object is returned
    select type (val => dict % get("ohno"))
      type is (key_not_found_t)
        print *, "key not found"
    end select

    ! no explicit cleanup call is needed, since all data structure
    ! is build on allocatables

end program
