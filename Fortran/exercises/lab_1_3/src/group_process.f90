! Copyright 2015 Fyodorov S. A.

module Group_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Group_IO

   implicit none
   
contains
   ! Сортировка списка класса по среднему баллу.
   pure subroutine Sort_result_list(Group1, Group2)
      type(person), intent(inout)  :: Group1(:)
      type(person), intent(out)    :: Group2(:)
      integer                      :: i
	  
      do i = 1, size(Group1)
        if (Swap(Group2, Group1, 1, i)) then
           Group2(3) = Group2(2)
           Group2(2) = Group2(1)
           Group2(1) = Group1(i)
        else if (Swap(Group2, Group1, 2, i)) then
           Group2(3) = Group2(2)
           Group2(2) = Group1(i)
        else if (Swap(Group2, Group1, 3, i)) then
           Group2(3) = Group1(i)
        end if
     end do
   end subroutine Sort_result_list

   ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
   pure logical function Swap(Group2, Group1, i, j)
      type(person), intent(in)  :: Group1(:)
      type(person), intent(in) :: Group2(:)
      integer, intent(in) :: i, j

      Swap = .false.
      if (Group2(i)%Year < Group1(j)%Year) then
         Swap = .true.
      end if
   end function Swap
end module group_process
