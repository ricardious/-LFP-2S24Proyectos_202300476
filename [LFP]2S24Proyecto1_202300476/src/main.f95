program main
   use file_reader
   use analyzerModule

   implicit none
   character(len=100) :: input_file_name
   character(len=:), allocatable :: entry
   type(Analyzer) :: evaluator
   integer :: ios

   open (unit=10, file="file_path.txt", status="old", iostat=ios)
   if (ios /= 0) then
      print *, "Error: no se puede abrir file_path.txt"
      stop
   end if

   read (10, '(A)') input_file_name
   close (10)

   call read_file_and_process(trim(input_file_name))
   entry = get_file_content()

   ! Initialize the state of the evaluator
   call evaluator%initializeState()
   call evaluator%analyze(entry)

end program main

