program adventCode1Part2
    integer :: i, p, j, unit, numLines, first, last, num, sum, ios
    character(len=1000), allocatable :: str(:)
    numLines = 0
    unit = 10
    sum = 0
    open(unit, file="input.txt", status='old', action='read', iostat=ios)

    do
        read(unit, '(A)', iostat=ios)
        numLines = numLines + 1
        if(ios /= 0) exit
    end do

    allocate(character(len=1000) :: str(numLines))

    rewind(unit)

    do i=1, numLines
        read(unit, '(A)', iostat=ios) str(i)
    end do

    do i=1, numLines
        first = -1
        last = -1
        do p=1, len_trim(str(i))
            do j=p, len_trim(str(i))
                if(is_number((str(i)(p:j)))) then
                    if(first == -1) then
                        call char_to_int(str(i)(p:j), num)
                        first = num
                    end if
                    call char_to_int(str(i)(p:j), num)
                    last = num
                end if
            end do
        end do
        write(*,*) (10 * first) + last
        sum = sum + (10 * first) + last
    end do

    write(*,*) sum

contains 
    logical function is_number(str)
        implicit none
        character(len=*), intent(in) :: str
        if (str .eq. "one" .or. str .eq. "two" .or. str .eq. "three" .or. &
            str .eq. "four" .or. str .eq. "five" .or. str .eq. "six" .or. str .eq. "seven" .or. &
            str .eq. "eight" .or. str .eq. "nine") then
            is_number = .true.
        elseif (ichar(str) >= ichar('0') .and. ichar(str) <= ichar('9')) then
            is_number = .true.
        else
            is_number = .false.
        end if
    end function is_number

    subroutine char_to_int(char, num)
        implicit none
        character(len=*), intent(in) :: char
        integer, intent(out) :: num
        if (char .eq. "zero") then
            num = 0
        elseif (char .eq. "one") then
            num = 1
        elseif (char .eq. "two") then
            num = 2
        elseif (char .eq. "three") then
            num = 3
        elseif (char .eq. "four") then
            num = 4
        elseif (char .eq. "five") then
            num = 5
        elseif (char .eq. "six") then
            num = 6
        elseif (char .eq. "seven") then
            num = 7
        elseif (char .eq. "eight") then
            num = 8
        elseif (char .eq. "nine") then
            num = 9
        else
            num = ichar(char) - ichar('0')
        end if
    end subroutine char_to_int
    
end program adventCode1Part2