module file_reader
   implicit none
   ! Public interface
   public :: read_file_and_process, get_file_content

   character(len=:), allocatable :: file_content  ! Holds the file content as it is read

contains

   !-------------------------------------------------------------------------
   ! Subroutine: read_file_and_process
   ! Purpose: Reads the content of a file line by line and stores it in the
   !          file_content variable. It processes empty lines by marking them.
   !
   ! Arguments:
   !   - file_name (in): A string representing the name of the file to be read.
   !
   ! Notes:
   !   - If the file cannot be opened, an error message is printed, and the
   !     subroutine terminates.
   !   - Each line is trimmed, and if it is empty, it is replaced by the string
   !     '! EMPTY_LINE'. All lines are appended together in the file_content
   !     variable.
   !-------------------------------------------------------------------------

   subroutine read_file_and_process(file_name)
      implicit none
      character(len=*), intent(in) :: file_name  ! Name of the file to read
      integer :: io, stat  ! File unit and I/O status
      character(len=512) :: error_message  ! For file opening errors
      character(len=100) :: line  ! Holds each line read from the file

      ! Initialize the file content
      file_content = ''

      ! Try to open the file
      open (newunit=io, file=file_name, status="old", action="read", iostat=stat, iomsg=error_message)

      ! Handle file open error
      if (stat /= 0) then
         print *, "-ERROR: Could not open file:", trim(error_message)
         return
      end if

      ! Read the file line by line
      do
         read (io, '(A)', iostat=stat) line

         ! Exit if end of file is reached or an error occurs
         if (stat /= 0) exit

         ! Trim the line and append to file content
         if (len_trim(line) == 0) then
            line = '! EMPTY_LINE'
         end if

         file_content = file_content//trim(line)//new_line('A')

      end do

      ! Close the file after reading
      close (io)

   end subroutine read_file_and_process

   !-------------------------------------------------------------------------
   ! Function: get_file_content
   ! Purpose: Returns the content of the file that was read as a single string.
   !
   ! Returns:
   !   - A string containing the entire file content with new lines separating
   !     each line from the file.
   !-------------------------------------------------------------------------

   function get_file_content() result(res)
      implicit none
      character(len=:), allocatable :: res
      res = file_content
   end function get_file_content

end module file_reader
