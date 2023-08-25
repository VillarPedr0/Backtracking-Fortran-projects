program BackTrackingExample
    implicit none
    integer, parameter :: maxElements = 4
    integer, parameter :: target = 10
    integer :: set(maxElements) = [2, 4, 6, 8]
    integer :: partial(maxElements)
    integer :: numSolutions

    !Subroutine for Backtracking Function
    recursive subroutine Backtrack(index, currentSum)
        integer, intent(in) :: index, currentSum
        if(currentSum == target) then
            !There is a solution, print it.
            numSolutions = numSolutions + 1
            write(*, '(A, I2, A)', advance='no') "Combination ", numSolutions, ": ["
            do i = 1, index - 1
                write(*, '(I2, A)', advance='no') partial(i), ", "
            end do
            write(*, '(I2, A)', advance='no') partial(index)
            write(*, 'A)', advance='no') "]"
        else if (index <= maxElements) then
            !Try to add the actual solution to partial solution
            partial(index) = set(index)
            call Backtrack(index + 1, currentSum + set(index))
            !Try to not add the actual solution to partial solution
            call Backtrack(index + 1, currentSum)
        end if
    end subroutine Backtrack

    !Call Backtracking Algorithm
    numSolutions = 0
    call Backtrack(1,0)

end program BackTrackingExample
