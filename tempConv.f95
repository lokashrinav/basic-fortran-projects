program tempConv
    real :: degree_input
    real :: res
    character(len=10) :: firstTemp, secondTemp
    write(*,*) "Please enter the following in this order: Degree (Number), the scale you wish to convert, &"
    write(*,*) "and the scale you would like to convert to"
    read(*,*) degree_input, firstTemp, secondTemp
    if(firstTemp .eq. "Kelvin") then
        if(secondTemp .eq. "Fahrenheit") then 
            res = (degree_input - 273.15) * 9/5 + 32
        elseif(secondTemp .eq. "Celsius") then 
            res = degree_input - 273.15
        end if
    elseif(firstTemp .eq. "Celsius") then
        if(secondTemp .eq. "Fahrenheit") then 
            res = (degree_input - 32) / 1.8
        elseif(secondTemp .eq. "Kelvin") then 
            res = degree_input + 273.15
        end if
    elseif(firstTemp .eq. "Fahrenheit") then
        if(secondTemp .eq. "Celsius") then 
            res = (degree_input - 32) / 1.8
        elseif(secondTemp .eq. "Kelvin") then 
            res = ((degree_input - 32) / 1.8) + 273.15
        end if
    else
        write(*,*) "Invalid Program"
    end if
    write(*,*) res

end program tempConv