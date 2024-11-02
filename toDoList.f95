program toDoList
    implicit none
    integer :: ex, numTasks, i
    type :: task
        character(len=1000) :: taskName
        character(len=1000) :: dayToComplete
    end type task
    type(task), allocatable = taskArr(:), tempTaskArr(:)
    type(task) :: newTask
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
            tempTaskArr(1:(numTasks - 1)) = taskArr
            tempTaskArr(numTasks) = newTask
            call move_alloc(tempTaskArr, taskArr)
        elseif(ex == 2) then
            do i=0, numTasks
                
            end do
        elseif(ex == 7) then 
            stop
        end if
    end do
end program toDoList

task function first
    type :: task
        character(len=1000) :: taskName
        character(len=1000) :: dayToComplete
    end type task
    type(task) :: newTask
    write(*,'(A)', advance='No') "Please enter the name of the task you'd like to add: "
    read(*,*) newTask%taskName
    write(*,'(A)', advance='No') "Which day of the week you plan to complete it?: " 
    read(*,*) newTask%dayToComplete
end function newTask