! Copyright 2015 Fyodorov S. A.

module Group_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Group_IO

   implicit none

contains
   ! Получение списков по полу и городу.
   pure recursive subroutine Get_list_by_gender_and_city(Pers, List, Amount, Gender, Registration)
      type(person), intent(in)         :: Pers
      type(person), pointer            :: List
      integer(I_), intent(inout)       :: Amount
      character(kind=CH_), intent(in)  :: Gender
      character(kind=CH_), intent(in)  :: Registration
     
      ! Если найден человек нужного пола и города, то размещаем в новом списке элемент и копируем его данные.
      if (Pers%Gender == Gender .AND. Pers%Registration == Registration) then
         allocate (List, source=Pers)
         Amount = Amount + 1
         List%next => Null()
         ! Если ещё остались студенты, сканируем дальше, а в создаваемом списке передаём место СОСЕДА.
         if (Associated(Pers%next)) &
            call Get_list_by_gender_and_city(Pers%next, List%next, Amount, Gender, Registration)
      ! Если ещё остались студенты, сканируем дальше, а в создаваемом списке передаём ПРЕЖНЕЕ место.
      else if (Associated(Pers%next)) then
         call Get_list_by_gender_and_city(Pers%next, List, Amount, Gender, Registration)
      end if
   end subroutine Get_list_by_gender_and_city
   
   ! Сортировка списка класса по среднему баллу.
   pure recursive subroutine Sort_result_list(List1, List2, N)
      type(person), pointer :: List1, List2
      integer, intent(in)         :: N
	  
      if (Swap(List2, List1)) then
         call Change(List2%next, List2)
         call Change(List2%next%next, List2%next)
         call Change(List2, List1)
      else if (Swap(List2%next, List1)) then
         call Change(List2%next%next, List2%next)
         call Change(List2%next, List1)
      else if (Swap(List2%next%next, List1)) then
         call Change(List2%next%next, List1)
      end if
	  
	  ! Если необходимо, делаем то же с последними N-1 элементами.
      if (N > 1) &
         call Sort_result_list(List1%Next, List2, N-1)
   end subroutine Sort_result_list

   ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
   pure logical function Swap(List2, List1)
      type(person), intent(in)  :: List1, List2

      Swap = .false.
      if (List2%Year < List1%Year) then
         Swap = .true.
      end if
   end function Swap
   
   pure subroutine Change(List2, List1)
      type(person), intent(in)   :: List1
      type(person), intent(inout)  :: List2
	  
      List2%Surname = List1%Surname
      List2%Initials = List1%Initials
      List2%Year = List1%Year
   end subroutine Change
end module Group_process
