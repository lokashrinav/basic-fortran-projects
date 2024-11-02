program calculator
    implicit none
    character (len=20) :: op
    integer :: a, b
    real :: res
    write(*,*) "Insert 'add', 'sub', 'mul', or 'div' follwed by two numbers"
    read(*,*) op, a, b
    if(op .eq. 'add') then 
        res = a + b
    elseif(op .eq. 'sub') then
        res = a - b
    elseif(op .eq. 'mul') then
        res = a * b
    elseif(op .eq. 'div') then
        if(b == 0) then
            write(*,*) "Can't Input 0 as Denominator"
            stop
        else
            res = a / b
        end if
    else
        write(*,*) "Invalid Operation"
    end if
    write(*,*) res

end program calculator