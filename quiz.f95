program quiz
    character(len=1000), dimension(10) :: arr
    integer :: i, re
    character(len=1000) answer
    character(len=1000), dimension(10) :: correct_answers

    arr(1) = "What is the boiling point of water?"
    arr(2) = "What planet is known as the Red Planet?"
    arr(3) = "How many planets are in our solar system?"
    arr(4) = "What is the process by which plants make their food called?"
    arr(5) = "What gas do humans breathe in from the air?"
    arr(6) = "What is the largest organ in the human body?"
    arr(7) = "What force pulls objects toward the Earth?"
    arr(8) = "What is H2O more commonly known as?"
    arr(9) = "What do bees collect from flowers to make honey?"
    arr(10) = "What is the center of an atom called?"
    res = 0

    correct_answers(1) = "100"
    correct_answers(2) = "Mars"
    correct_answers(3) = "8"
    correct_answers(4) = "Photosynthesis"
    correct_answers(5) = "Oxygen"
    correct_answers(6) = "Skin"
    correct_answers(7) = "Gravity"
    correct_answers(8) = "Water"
    correct_answers(9) = "Nectar"
    correct_answers(10) = "Nucleus"

    write(*,*) "Answer the following questions: "
    do i = 1, 10
        write(*,'(A)', advance='no') trim(arr(i))
        read(*,*) answer
        if (trim(adjustl(answer)) == trim(adjustl(correct_answers(i)))) then
            res = res + 1
        end if
    end do 
    write(*,*) res
    end program quiz