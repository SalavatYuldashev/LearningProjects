! Copyright 2015 Fyodorov S. A.

program reference_lab_1_2
   use Environment

   implicit none
   integer, parameter               :: PEOPLE_AMOUNT = 15, SURNAME_LEN = 15, INITIALS_LEN = 5, YEARS_LEN = 4, MEN_AMOUNT = 3
   integer                          :: i, Peters_Amount
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), PETERSBURGER = Char(1055, CH_)
   character(:), allocatable        :: input_file, output_file

   ! Массивы фамилий, инициалов, полов, оценок и средних оценов.
   character(kind=CH_)  :: Surnames(PEOPLE_AMOUNT, SURNAME_LEN)  = "", &
                           Initials(PEOPLE_AMOUNT, INITIALS_LEN) = "", &
                           Years(PEOPLE_AMOUNT, YEARS_LEN)       = "", &
                           Registrations(PEOPLE_AMOUNT)          = "", &
                           Genders(PEOPLE_AMOUNT)                = ""
   character(kind=CH_), allocatable  :: Peters_Surnames(:, :), Peters_Initials(:, :), Peters_Years(:, :)
   character(kind=CH_)  :: Result_Surnames(MEN_AMOUNT, SURNAME_LEN)
   character(kind=CH_)  :: Result_Initials(MEN_AMOUNT, INITIALS_LEN)
   character(kind=CH_)  :: Result_Years(MEN_AMOUNT, YEARS_LEN)

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   
   call Read_class_list(input_file, Surnames, Initials, Years, Registrations, Genders)
   
   call Output_class_list(output_file, Surnames, Initials, Years, Registrations, Genders, &
      "Исходный список:", "rewind")
      
   call Get_list_by_gender_and_cities(Surnames, Initials, Years, Registrations, Genders, Peters_Surnames, &
      Peters_Initials, Peters_Years, Peters_Amount)
	  
   call Sort_result_list(Peters_Surnames, Peters_Initials, Peters_Years, Result_Surnames, Result_Initials, &
      Result_Years, Peters_Amount)  
	  
   call Output_result_list(output_file, Result_Surnames, Result_Initials, Result_Years, &
      "Трое наиболее молодых петербуржца:", "append")

contains
   ! Чтение списка класса: фамилии, инициалы, полы, оценки и средний.
   subroutine Read_class_list(Input_File, Surnames, Initials, Years, Registrations, Genders)
      character(*)         Input_File
      character(kind=CH_)  Surnames(:, :), Initials(:, :), Years(:, :), Registrations(:), Genders(:)
      intent (in)          Input_File
      intent (out)         Surnames, Initials, Years, Registrations, Genders

      integer In, IO, i
      character(:), allocatable  :: format
      
      open (file=Input_File, encoding=E_, newunit=In)
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // INITIALS_LEN // 'a1, 1x, ' &
            // YEARS_LEN  // 'a1, 1x, a, 1x, a)'
         read (In, format, iostat=IO) (Surnames(i, :), Initials(i, :), Years(i, :), Registrations(i), Genders(i), &
            i = 1, PEOPLE_AMOUNT)
         call Handle_IO_status(IO, "reading class list")
      close (In)
   end subroutine Read_class_list
   
   ! Вывод списка класса.
   subroutine Output_class_list(Output_File, Surnames, Initials, Years, Registrations, Genders, List_name, Position)
      character(*)         Output_File, Position, List_name
      character(kind=CH_)  Surnames(:, :), Initials(:, :), Years(:, :), Registrations(:), Genders(:)
      intent (in)          Output_File, Surnames, Initials, Years, Registrations, Genders, List_name, Position

      integer                    :: Out, i, IO
      character(:), allocatable  :: format
   
      open (file=output_file, encoding=E_, position=position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // INITIALS_LEN // 'a1, 1x, ' &
            // YEARS_LEN  // 'a1, 1x, a, 1x, a)'
         write(Out, format, iostat=IO) (Surnames(i, :), Initials(i, :), Years(i, :), Registrations(i), Genders(i), &
            i = 1, PEOPLE_AMOUNT)
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_class_list
   
   ! Получение списков по полу.
   pure subroutine Get_list_by_gender_and_cities(Surnames, Initials, Years, Registrations, Genders, &
         Peters_Surnames, Peters_Initials, Peters_Years, Peters_Amount)
      character(kind=CH_)  Surnames(:, :), Initials(:, :), Years(:, :), Registrations(:), Genders(:)
      character(kind=CH_)  Peters_Surnames(:, :), Peters_Initials(:, :), Peters_Years(:, :)
	  integer              Peters_Amount
      intent(in)           Surnames, Initials, Years, Registrations, Genders
      intent(out)          Peters_Surnames, Peters_Initials, Peters_Years, Peters_Amount
      allocatable          Peters_Surnames, Peters_Initials, Peters_Years

      logical, allocatable :: Is_A_Peterburger(:)
      integer, allocatable :: Peters_Pos(:)
      integer, parameter   :: INDEXES(*) = [(i, i = 1, PEOPLE_AMOUNT)]

      ! Составление логической маски, соответствующей петербуржцам.
      Is_A_Peterburger = Genders == MALE .AND. Registrations == PETERSBURGER
      Peters_Amount    = Count(Is_A_Peterburger)

      Peters_Pos  = Pack(INDEXES, Is_A_Peterburger)
      allocate (Peters_Surnames(Peters_Amount, SURNAME_LEN), &
         Peters_Initials(Peters_Amount, INITIALS_LEN), Peters_Years(Peters_Amount, YEARS_LEN))
      ! Получение двумерных списков
      do concurrent (i = 1:Peters_Amount)
         Peters_Surnames(i, :)  = Surnames(Peters_Pos(i), :)
         Peters_Initials(i, :)  = Initials(Peters_Pos(i), :)
         Peters_Years(i, :)     = Years(Peters_Pos(i), :)
      end do
   end subroutine Get_list_by_gender_and_cities
   
   pure subroutine Sort_result_list(Surnames, Initials, Years, Result_Surnames, Result_Initials, &
      Result_Years, Peters_Amount)
      character(kind=CH_)  Surnames(:, :), Initials(:, :), Years(:, :), Result_Surnames(:, :), &
         Result_Initials(:, :), Result_Years(:, :)
	  integer           Peters_Amount
      intent (in)       Surnames, Initials, Years, Peters_Amount
      intent (out)      Result_Surnames, Result_Initials, Result_Years
      integer           i, j
	  
      do concurrent (i = 1:Peters_Amount)
        if (Swap(Result_Years, Years, 1, i)) then
           do j = 3, 2, -1
              call Shift(Result_Surnames(j, :), Result_Surnames(j - 1, :), Result_Initials(j, :), &
                 Result_Initials(j - 1, :), Result_Years(j, :), Result_Years(j - 1, :))
           end do
           call Shift(Result_Surnames(1, :), Surnames(i, :), Result_Initials(1, :), &
              Initials(i, :), Result_Years(1, :), Years(i, :))
        else if (Swap(Result_Years, Years, 2, i)) then
           call Shift(Result_Surnames(3, :), Result_Surnames(2, :), Result_Initials(3, :), &
              Result_Initials(2, :), Result_Years(3, :), Result_Years(2, :))
           call Shift(Result_Surnames(2, :), Surnames(i, :), Result_Initials(2, :), &
              Initials(i, :), Result_Years(2, :), Years(i, :))
        else if (Swap(Result_Years, Years, 3, i)) then
           call Shift(Result_Surnames(3, :), Surnames(i, :), Result_Initials(3, :), &
              Initials(i, :), Result_Years(3, :), Years(i, :))
        end if
     end do
   end subroutine Sort_result_list
   
   pure logical function Swap(Result_Years, Years, i, j)
      character(kind=CH_)  Result_Years(:, :), Years(:, :)
      integer              i, j
      intent (in) :: Result_Years, Years, i, j

      Swap = .false.
      if (LT(Result_Years(i, :), Years(j, :))) then
         Swap = .true.
      end if
   end function Swap
   
   pure logical function LT(arr1, arr2)
      character(kind=CH_), intent(in) :: arr1(:), arr2(:)
      integer :: i 

      ! Поиск первого отличного символа или остановка на последнем символе.
      do i = 1, Min(Size(arr1), Size(arr2)) - 1
         if (arr1(i) /= arr2(i)) &
            exit
      end do
      LT = arr1(i) < arr2(i)  
   end function LT
   
   pure subroutine Shift(Surnames1, Surnames2, Initials1, Initials2, Years1, Years2)
      character(kind=CH_), intent(in) :: Surnames2(:), Initials2(:), Years2(:)
      character(kind=CH_), intent(out) :: Surnames1(:), Initials1(:), Years1(:)
      Surnames1 = Surnames2
      Initials1 = Initials2
      Years1 = Years2
   end subroutine Shift
   
   ! Вывод списка класса.
   subroutine Output_result_list(Output_File, Result_Surnames, Result_Initials, Result_Years, List_name, Position)
      character(*)         Output_File, Position, List_name
      character(kind=CH_)  Result_Surnames(:, :), Result_Initials(:, :), Result_Years(:, :)
      intent (in)          Output_File, Result_Surnames, Result_Initials, Result_Years, List_name, Position

      integer                    :: Out, i, IO
      character(:), allocatable  :: format
   
      open (file=output_file, encoding=E_, position=position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // INITIALS_LEN // 'a1, 1x, ' &
            // YEARS_LEN  // 'a1)'
         write(Out, format, iostat=IO) (Result_Surnames(i, :), Result_Initials(i, :), Result_Years(i, :), &
            i = 1, MEN_AMOUNT)
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_result_list
   
end program reference_lab_1_2
