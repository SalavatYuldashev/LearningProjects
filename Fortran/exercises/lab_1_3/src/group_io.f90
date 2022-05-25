! Copyright 2015 Fyodorov S. A.

module Group_IO
   use Environment

   implicit none
   integer, parameter :: PEOPLE_AMOUNT = 15
   integer, parameter :: SURNAME_LEN   = 15
   integer, parameter :: INITIALS_LEN  = 5
   integer, parameter :: YEARS_LEN     = 4
   integer, parameter :: MEN_AMOUNT    = 3

   ! Структура данных для хранения данных о человеке.
   type person
      character(SURNAME_LEN, kind=CH_)    :: Surname              = ""
      character(INITIALS_LEN, kind=CH_)   :: Initials             = ""
      character(YEARS_LEN, kind=CH_)      :: Year                 = ""
      character(kind=CH_)                 :: Registration         = ""
      character(kind=CH_)                 :: Gender               = ""
   end type person
   
contains
   ! Создание неформатированного файла данных.
   subroutine Create_data_file(Input_File, Data_File)
      character(*), intent(in)   :: Input_File, data_file
      
      type(person)               :: pers
      integer                    :: In, Out, IO, i, recl
      character(:), allocatable  :: format
      
      open (file=Input_File, encoding=E_, newunit=In)
      recl = (SURNAME_LEN + INITIALS_LEN + YEARS_LEN + 2)*CH_
      open (file=Data_File, form='unformatted', newunit=Out, access='direct', recl=recl)
         format = '(4(a, 1x), a)'
         do i = 1, PEOPLE_AMOUNT
            read (In, format, iostat=IO) pers
            ! Можно явно указывать поля в записи при чтении (не рекомендуется):
            !read (In, format, iostat=IO) stud%Surname, stud%Initials, stud%Sex, &
            !   stud%Marks, stud%Aver_mark
			
            call Handle_IO_status(IO, "reading formatted class list, line " // i)
            
            write (Out, iostat=IO, rec=i) pers
            call Handle_IO_status(IO, "creating unformatted file with class list, record " // i)
         end do
      close (In)
      close (Out)
   end subroutine Create_data_file

   ! Чтение списка класса: фамилии, инициалы, полы и оценки.
   function Read_class_list(Data_File) result(Group)
      type(person)                 Group(PEOPLE_AMOUNT)
      character(*), intent(in)   :: Data_File

      integer In, IO, recl
      
      recl = (SURNAME_LEN + INITIALS_LEN + YEARS_LEN + 2) * CH_ * PEOPLE_AMOUNT
      open (file=Data_File, form='unformatted', newunit=In, access='direct', recl=recl)
         read (In, iostat=IO, rec=1) Group
         call Handle_IO_status(IO, "reading unformatted class list")
      close (In)
   end function Read_class_list
 
   ! Вывод списка класса.
   subroutine Output_class_list(Output_File, Group, List_name, Position)
      character(*), intent(in)   :: Output_File, Position, List_name
      type(person), intent(in)   :: Group(:)

      integer                    :: Out, IO
      character(:), allocatable  :: format
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(4(a, 1x), a)'
         write (Out, format, iostat=IO) Group
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_class_list
   
   subroutine Output_result_list(Output_File, Group, List_name, Position)
      character(*), intent(in)   :: Output_File, Position, List_name
      type(person), intent(in)   :: Group(:)

      integer                    :: Out, IO, i
      character(:), allocatable  :: format
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(2(a, 1x), a)'
         write (Out, format, iostat=IO) (Group(i)%Surname, Group(i)%Initials, Group(i)%Year, i = 1, MEN_AMOUNT)
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_result_list
end module Group_IO 