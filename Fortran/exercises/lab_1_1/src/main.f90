! Copyright 2015 Fyodorov S. A

program reference_lab_1_1
   use Environment

   implicit none
   integer, parameter               :: PEOPLE_AMOUNT = 15, SURNAME_LEN = 15, INITIALS_LEN = 5, YEARS_LEN = 4, MEN_AMOUNT = 3
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), PETERSBURGER = Char(1055, CH_)

   character(:), allocatable  :: input_file, output_file, format

   ! Массивы фамилий, инициалов, лет, прописок и полов
   character(SURNAME_LEN, kind=CH_)                :: Surnames(PEOPLE_AMOUNT) = "", Men_Surnames(MEN_AMOUNT) = ""
   
   character(INITIALS_LEN, kind=CH_)               :: Initials(PEOPLE_AMOUNT) = "", Men_Initials(MEN_AMOUNT) = ""
   
   character(YEARS_LEN, kind=CH_)                  :: Years(PEOPLE_AMOUNT) = "", Men_Years(PEOPLE_AMOUNT) = ""
   
   character(kind=CH_)                             :: Registrations(PEOPLE_AMOUNT) = ""
   
   character(kind=CH_)                             :: Gender(PEOPLE_AMOUNT) = ""
   
   integer, allocatable                            :: Peters_Pos(:)

   logical, allocatable                            :: Is_A_Peterburger(:)
   integer                                         :: Peters_Amount = 0

   integer :: In, Out, IO, i, j
   integer, parameter                              :: INDEXES(*) = [(i, i = 1, PEOPLE_AMOUNT)]

   input_file = "../data/class.txt"
   output_file = "output.txt"
   ! Чтение списка класса: фамилии, инициалы, года, прописки и пола
   open (file=input_file, encoding=E_, newunit=In)
      format = '(4(a, 1x), a)'
      read (In, format, iostat=IO) (Surnames(i), Initials(i), Years(i), Registrations(i), Gender(i), i = 1, PEOPLE_AMOUNT)
   close (In)
   ! ПОЯСНЕНИЯ К СПОСОБАМ ЧТЕНИЯ:
   ! 1. можно программировать форматный список, не приравнивая к нему, а записывая в него -- так менее наглядно:
   !write (format, '(a, i0, a)') '(3(a, 1x), ', MARKS_AMOUNT, 'i1, f5.2)'
   !read (In, format, iostat=IO) (Surnames(i), Initials(i), Gender(i), Marks(i, :), i = 1, STUD_AMOUNT)
   !
   ! 2. Можно проводить запись без использования списка -- так не профессионально (больше ошибок):
   !do i = 1, STUD_AMOUNT
   !   read (In, format, iostat=IO) surnames(i), Initials(i), Gender(i), Marks(i, :)
   !end do
   !
   ! 3. Можно записать без использования именованной константы -- так не профессионально (сложнее поддерживать код):
   ! поступать:
   !read (In, '(3(a, 1x), 5i1, f5.2)', iostat=IO) (Surnames(i), Initials(i), Gender(i), Marks(i, :), i = 1, STUD_AMOUNT)
   !
   ! 4. Можно записать без использования списка и констант -- так не профессионально (больше ошибок и сложнее поддерживать код):
   !do i = 1, STUD_AMOUNT
   !   read (In, '(3(a, 1x), 5i1, f5.2 )', iostat=IO) Surnames(i), Initials(i), Gender(i), Marks(i, :) 
   !end do
  
   
   ! Обработка статуса чтения.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while reading class list."
      case(1:)
         write (Out, '(a)') "Error while reading class list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while reading class list: ", io
   end select

   ! Составление логической маски, соответствующей петербуржцам.
   Is_A_Peterburger = Gender == MALE .AND. Registrations == PETERSBURGER
   Peters_Amount    = Count(Is_A_Peterburger)
   
   Peters_Pos   = Pack(INDEXES, Is_A_Peterburger)
   do concurrent (i = 1:Peters_Amount)
      if (Men_Years(1) < Years(Peters_Pos(i))) then
         do j = 3, 2, -1
            Men_Surnames(j) = Men_Surnames(j - 1)
            Men_Initials(j) = Men_Initials(j - 1)
            Men_Years(j)    = Men_Years(j - 1)
         end do
         Men_Surnames(1) = Surnames(Peters_Pos(i))
         Men_Initials(1) = Initials(Peters_Pos(i))
         Men_Years(1)    = Years(Peters_Pos(i))
      else if (Men_Years(2) < Years(Peters_Pos(i))) then
         Men_Surnames(3) = Men_Surnames(2)
         Men_Initials(3) = Men_Initials(2)
         Men_Years(3)    = Men_Years(2)
         Men_Surnames(2) = Surnames(Peters_Pos(i))
         Men_Initials(2) = Initials(Peters_Pos(i))
         Men_Years(2)    = Years(Peters_Pos(i))
      else if (Men_Years(3) < Years(Peters_Pos(i))) then
         Men_Surnames(3) = Surnames(Peters_Pos(i))
         Men_Initials(3) = Initials(Peters_Pos(i))
         Men_Years(3)    = Years(Peters_Pos(i))
      end if
   end do
   
   ! Вывод списка класса.
   open (file=output_file, encoding=E_, newunit=Out)
      format = '(4(a, 1x), a)'
      write (In, format, iostat=IO) (Surnames(i), Initials(i), Years(i), Registrations(i), Gender(i), i = 1, PEOPLE_AMOUNT)
      write (Out, '(a)') "Трое наиболее молодых петербуржца:"
      ! Пояснения к записи те же, что и к чтению.
      format = '(2(a1, 1x), a1)'
      write (Out, format, iostat=IO) (Men_Surnames(i), Men_Initials(i), Men_Years(i), i = 1, MEN_AMOUNT)
   close (Out)
   ! Обработка статуса записи.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing class list."
      case(1:)
         write (Out, '(a)') "Error while writing class list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing class list: ", io
   end select
   
end program reference_lab_1_1
