program adventCode1Part2
    integer :: i, p, unit, numLines
    numLines = 0
    unit = 10
    open(unit, file="input.txt", status='old', action='read', iostat=ios)

    do
        read(unit, '(A)', iostat=ios)
        if(ios /= 0) exit
        numLines = numLines + 1
    end do

    rewind(unit)

    do i=1,numLines
        read(unit, '(A)', iostat=ios)
    end do


end program adventCode1Part2