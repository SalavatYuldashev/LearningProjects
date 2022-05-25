! Copyright 2015 Fyodorov S. A.

module Group_IO
   use Environment

   implicit none
   integer, parameter :: SURNAME_LEN   = 15
   integer, parameter :: INITIALS_LEN  = 5
   integer, parameter :: YEARS_LEN     = 4

   ! Структура данных для хранения данных о человеке.
   type person
      character(SURNAME_LEN, kind=CH_)    :: Surname              = ""
      character(INITIALS_LEN, kind=CH_)   :: Initials             = ""
      character(YEARS_LEN, kind=CH_)      :: Year                 = ""
      character(kind=CH_)                 :: Registration         = ""
      character(kind=CH_)                 :: Gender               = ""
      type(person), pointer               :: next                 => Null()
   end type person

contains
   ! Чтение списка класса: фамилии, инициалы, полы и оценки.
   function Read_class_list(Input_File) result(Class_List)
      type(person), pointer      :: Class_List
      character(*), intent(in)   :: Input_File
      integer  In

      open (file=Input_File, encoding=E_, newunit=In)
         Class_List => Read_person(In)
      close (In)
   end function Read_class_list

   ! Чтение следующего студента.
   recursive function Read_person(In) result(Pers)
      type(person), pointer   :: Pers
      integer, intent(in)     :: In
      integer  IO
      character(:), allocatable  :: format
	  
      allocate (Pers)
      format = '(4(a, 1x), a)'
      read (In, format, iostat=IO) pers%Surname, pers%Initials, pers%Year, pers%Registration, pers%Gender
      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
          Pers%next => Read_person(In)
      else
         deallocate (Pers)
         nullify (Pers)
      end if
   end function Read_person

   ! Вывод списка класса со средним баллом или без него.
   subroutine Output_class_list(Output_File, Class_List, List_Name, Position)
      character(*), intent(in)   :: Output_File, Position, List_Name
      type(person), intent(in)  :: Class_List
      integer  :: Out
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_person(Out, Class_List)
      close (Out)
   end subroutine Output_class_list

   recursive subroutine Output_person(Out, Pers)
      integer, intent(in)        :: Out
      type(person), intent(in)   :: Pers
      
      integer  :: IO
      character(:), allocatable  :: format

      format = '(4(a, 1x), a)'
      write (Out, format, iostat=IO) pers%Surname, pers%Initials, pers%Year, pers%Registration, pers%Gender
      call Handle_IO_status(IO, "writing person")
      if (Associated(Pers%next)) &
         call Output_person(Out, Pers%next)
   end subroutine Output_person
   
   subroutine Output_result_list(Output_File, Class_List, List_Name, Position)
      character(*), intent(in)   :: Output_File, Position, List_Name
      type(person), intent(in)  :: Class_List
      integer  :: Out
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_person_partial(Out, Class_List)
      close (Out)
   end subroutine Output_result_list
   
   recursive subroutine Output_person_partial(Out, Pers)
      integer, intent(in)        :: Out
      type(person), intent(in)   :: Pers
      
      integer  :: IO
      character(:), allocatable  :: format

      format = '(2(a, 1x), a)'
      write (Out, format, iostat=IO) pers%Surname, pers%Initials, pers%Year
      call Handle_IO_status(IO, "writing person")
      if (Associated(Pers%next)) &
         call Output_person_partial(Out, Pers%next)
   end subroutine Output_person_partial
end module Group_IO 
