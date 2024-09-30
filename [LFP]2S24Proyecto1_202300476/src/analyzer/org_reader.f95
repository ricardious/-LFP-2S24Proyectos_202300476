module file_reader
    implicit none
    ! Public interface
    public :: read_file_and_process, get_file_content

    character(len=:), allocatable :: file_content ! Holds the file content as it is read

contains

    ! Subroutine to read and process the contents of a file line by line
    
    subroutine read_file_and_process(file_name)
        implicit none
        character(len=*), intent(in) :: file_name  ! Name of the file to read
        integer :: io, stat, i, line_len ! File unit, I/O status, loop index, line length
        character(len=512) :: error_message  ! For file opening errors
        character(len=100) :: line  ! Holds each line read from the file
        character(len=:), allocatable :: temp_string  ! Temporary string for processing

        ! Initialize the file content
        file_content = ''

        ! Try to open the file
        open(newunit=io, file=file_name, status="old", action="read", iostat=stat, iomsg=error_message)

        ! Handle file open error
        if (stat /= 0) then
            print *, "-ERROR: Could not open file:", trim(error_message)
            return
        end if

    
        ! Read the file line by line
        do
            read(io, '(A)', iostat=stat) line

            ! Exit if end of file is reached or an error occurs
            if (stat /= 0) exit

            ! Trim the line and get its actual length
            line_len = len_trim(line)

            ! Process each character in the line
            do i = 1, line_len
                    file_content = file_content // line(i:i)
            end do

            file_content = file_content // new_line('A')

        end do

        ! Close the file after reading
        close(io)

    end subroutine read_file_and_process

    ! Function to return the file content as a string
    function get_file_content() result(res)
        implicit none
        character(len=:), allocatable :: res
        res = file_content
    end function get_file_content

end module file_reader
