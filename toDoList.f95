program toDoList
    implicit none
    integer :: ex, numTasks, i
    type :: task
        character(len=1000) :: taskName
        character(len=1000) :: dayToComplete
        logical :: complete
    end type task
    type(task), allocatable :: tasksArr(:), tempTaskArr(:)
    integer :: del
    integer :: index
    type(task) :: newTask
    character(len=1000) :: s, changedItem
    character(len=1000) :: tOrf
    numTasks = 0
    allocate(tasksArr(numTasks))
    do 
        write(*,'(A)') "Welcome to the To-Do App! Please enter a number:"
        write(*,'(A)') "1. Add a new task"
        write(*,'(A)') "2. View all tasks"
        write(*,'(A)') "3. Mark a task as complete"
        write(*,'(A)') "4. Remove a task"
        write(*,'(A)') "5. Edit a task"
        write(*,'(A)') "6. Save tasks"
        write(*,'(A)') "7. Exit"
        write(*,'(A)') "Please enter your choice: "
        read(*,*) ex
        if(ex == 1) then
            newTask = first()
            numTasks = numTasks + 1
            allocate(tempTaskArr(numTasks))
            tempTaskArr(1:(numTasks - 1)) = tasksArr
            tempTaskArr(numTasks) = newTask
            call move_alloc(tempTaskArr, tasksArr)
        elseif(ex == 2) then
            do i=1, numTasks
                write(*,'(A, A, A, A)') "Task: ", (tasksArr(i)%taskName), "Day To Be Completed: ", (tasksArr(i)%dayToComplete)
            end do
        elseif(ex == 3) then
            write(*,'(A)', advance='No') "Please enter the index of the task you'd like complete(1-index): "
            read(*,*) index
            tasksArr(index)%complete = .FALSE.
        elseif(ex == 4) then
            write(*,'(A)', advance='No') "Please enter the index of the task you'd like deleted(1-index): "
            read(*,*) del
            allocate(tempTaskArr(numTasks - 1))
            tempTaskArr(1:(del - 1)) = tasksArr(1:(del-1))
            tempTaskArr(1:(numTasks-1)) = tasksArr((del+1):numTasks)
            numTasks = numTasks - 1
            call move_alloc(tempTaskArr, tasksArr)
        elseif(ex == 5) then
            write(*,'(A)', advance='No') "Would you like to change the taskName or dayToComplete?: "
            read(*,*) s
            if(s .eq. "taskName") then
                write(*,'(A)', advance='No') "What would you like to change it into?: "
                read(*,*) changedItem
                write(*,'(A)', advance='No') "What is the index?: "
                read(*,*) index
                tasksArr(index)%taskName = changedItem
            elseif(s .eq. "dayToComplete") then
                write(*,'(A)', advance='No') "What would you like to change it into?: "
                read(*,*) changedItem
                write(*,'(A)', advance='No') "What is the index?: "
                read(*,*) index
                tasksArr(index)%dayToComplete = changedItem
            end if
        else if(ex == 6) then
            write(*,*) "Happy Halloween"
        elseif(ex == 7) then 
            stop
        end if
    end do
contains
    function first() result(newTask)
        type(task) :: newTask
        character(len=1000) :: tOrf
        write(*,'(A)', advance='No') "Please enter the name of the task you'd like to add: "
        read(*,*) newTask%taskName
        write(*,'(A)', advance='No') "Which day of the week you plan to complete it?: " 
        read(*,*) newTask%dayToComplete
        write(*,'(A)', advance='No') "Is it Complete? True or False: "
        read(*,*) tOrF
        if(tOrF .eq. "True") then
            newTask%complete = .TRUE.
        elseif(tOrF .eq. "False") then
            newTask%complete = .FALSE.
        end if
    end function first
end program toDoList
