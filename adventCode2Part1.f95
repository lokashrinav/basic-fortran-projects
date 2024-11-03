program adventCode2Part1
    implicit none
    character(len=1000), allocatable :: s(:)
    character(len=1000) :: afterColon, beforeColon, color, gameString, game, line
    integer :: ios, unit, pos, numLines, i, p, count, number, gameNum, sum
    logical :: idk
    sum = 0
    unit = 10

    open(unit, file='input.txt', status='old', action='read', iostat=ios)
    numLines = 0

    do
        read(unit, '(A)', iostat=ios) line
        if(ios /= 0) exit
        numLines = numLines + 1
    end do

    rewind(unit)
    allocate(character(len=1000) :: s(numLines))

    do i=1, numLines
        read(unit, '(A)', iostat=ios) s(i)
    end do

    do p=1, 1
        pos = index(s(p), ":")
        idk = .true.
        gameString = trim(s(p)(1:pos))
        read(gameString, *, iostat=ios) gameNum
        afterColon = trim(s(p)(pos+1:))
        count = 0
        do i=1, len_trim(afterColon)
            if(afterColon(i:i) .eq. ";" .or. afterColon(i:i) .eq. ",") then
                count = count + 1
            end if
        end do
        do i=1, count+1
            if(index(afterColon, ",") == -1) then 
                pos = index(afterColon, ";")
            elseif(index(afterColon, ';') == -1) then 
                pos = index(afterColon, ",")
            else
                write(*,*) afterColon   
                pos = min(index(afterColon, ","), index(afterColon, ";"))
            end if
            beforeColon = trim(afterColon(2:pos-1))
            afterColon = trim(afterColon(pos+1:len_trim(afterColon)))
            write(*,*) beforeColon, afterColon   
            read(beforeColon, *, iostat=ios) number, color
            if(color .eq. "blue" .and. number .gt. 14) then
                idk = .false.
            elseif(color .eq. "green" .and. number .gt. 13) then
                idk = .false.
            elseif(color .eq. "red" .and. number .gt. 12) then
                idk = .false.
            end if
        end do
        if(idk) then
            sum = sum + gameNum
        end if
    end do 

    write(*,*) sum    
    
end program adventCode2Part1