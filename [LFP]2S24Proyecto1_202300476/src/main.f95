program main
    use file_reader
    use analyzerModule

    implicit none
    character(len=100) :: input_file_name

    character(len=100) :: file_name
    character(len=:), allocatable :: entry
    type(Analyzer) :: evaluator

    file_name = "./data/input/Corto1.ORG"

    call read_file_and_process(file_name)
    entry = get_file_content()

    ! Initialize the state of the evaluator
    call evaluator%initializeState()
    call evaluator%analyze(entry)


end program main
