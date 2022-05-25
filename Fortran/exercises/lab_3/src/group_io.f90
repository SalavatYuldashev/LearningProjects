! Copyright 2015 Fyodorov S. A.

#define pure

module Group_IO
   use Environment

   implicit none
   integer, parameter :: SURNAME_LEN   = 15
   
   ! Структура данных для хранения данных о человеке.
   type student
      character(SURNAME_LEN, kind=CH_)  :: Surname                    = ""
      type(student), pointer            :: next                       => Null()
   end type student

contains
   ! Чтение списка класса: фамилия
   function Read_list(Input_File, format) result(Class_List)
      type(student), pointer     :: Class_List
      character(*), intent(in)   :: Input_File, format
      integer  In

      open (file=Input_File, encoding=E_, newunit=In)
         Class_List => Read_student(In, format)
      close (In)
   end function Read_list

   ! Чтение следующего студента.
   recursive function Read_student(In, format) result(Stud)
      type(student), pointer   :: Stud
      integer, intent(in)      :: In
      character(*), intent(in) :: format
      integer  IO, i
      character(kind=CH_) :: c
	  
      allocate (Stud)
      if (format == '(a)') then
         read (In, format, iostat=IO) Stud%surname
      else
         read (In, format, iostat=IO) i, c, Stud%surname
      end if
      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
          Stud%next => Read_student(In, format)
      else
         deallocate (Stud)
         nullify (Stud)
      end if
   end function Read_student

   ! Вывод списка класса с номерами или без них.
   subroutine Output_class_list(Output_File, Class_List, List_Name, Position, Index)
      character(*), intent(in)   :: Output_File, Position, List_Name
      type(student), intent(in)  :: Class_List
      integer, intent(in)        :: Index
      integer  :: Out
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         if (Index == 0) then
            call Output_student(Out, Class_List)
         else
            call Output_student_with_number(Out, Class_List, Index)
         end if
      close (Out)
   end subroutine Output_class_list

   ! Вывод одного студента без номера
   recursive subroutine Output_student(Out, Stud)
      integer, intent(in)         :: Out
      type(student), intent(in)   :: Stud
      
      integer  :: IO
      character(:), allocatable  :: format

      format = '(a)'
      write (Out, format, iostat=IO) stud%Surname
      call Handle_IO_status(IO, "writing student")
      if (Associated(Stud%next)) &
         call Output_student(Out, Stud%next)
   end subroutine Output_student
   
   ! Вывод одного студента с номером
   recursive subroutine Output_student_with_number(Out, Stud, Number)
      integer, intent(in)         :: Out, Number
      type(student), intent(in)   :: Stud
      
      integer  :: IO
      character(:), allocatable  :: format

      format = '(i0,a,a)'
      write (Out, format, iostat=IO) Number, '.', stud%Surname
      call Handle_IO_status(IO, "writing student")
      if (Associated(Stud%next)) &
         call Output_student_with_number(Out, Stud%next, Number + 1)
   end subroutine Output_student_with_number
end module Group_IO 
