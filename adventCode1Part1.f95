program adventCode1
    implicit none
    character(len=1000), allocatable :: s(:)
    integer :: i, p, first, last, num, res, numLines, unit, ios
    res = 0
    unit = 10

    open(unit, file='input.txt', status='old', action='read', iostat=ios)

    numLines = 0

    do
        read(unit, '(A)', iostat=ios)
        if (ios /= 0) exit
        numLines = numLines + 1
    end do 

    allocate(s(numLines))

    rewind(unit)

    do i = 1, numLines
        read(unit, '(A)', iostat=ios) s(i:i)
    end do

    close(unit)

    do i = 1, numLines
        last = -1
        first = -1
        do p = 1, len_trim(s(i))
            if(first == -1 .and. is_number(s(i)(p:p))) then
                num = ichar(s(i)(p:p)) - ichar('0')
                first = num
            end if
            if(is_number(s(i)(p:p))) then
                num = ichar(s(i)(p:p)) - ichar('0')
                last = num
            end if
        end do
        res = res + ((first * 10) + last)
    end do

    write(*,*) res

contains
    logical function is_number(str)
        implicit none
        character(len=*), intent(in) :: str
        if(ichar(str) >= ichar('0') .and. ichar(str) <= ichar('9')) then
            is_number = .true.
        else
            is_number = .false.
        end if
    end function is_number
end program