! Copyright 2015 Fyodorov S. A.

#define pure

module Group_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Group_IO

   implicit none

contains
   ! Вставка всех элементов списка F2 в упорядоченный список F1
   pure recursive subroutine Insert(F1, F2)
      type(student), pointer :: F1, F2
	  
      if (Associated(F2)) then
         call Insert_Item(F2, F1)
         call Insert(F1, F2%next)
      end if
   end subroutine Insert

   ! Вставка одного студента в упорядоченный список
   pure recursive subroutine Insert_Item(Stud, List)
      type(student), intent(in)     :: Stud
      type(student), pointer        :: List
      type(student), pointer        :: S
	  
      if (Associated(List) .AND. Associated(List%Next)) then
         if ((List%Surname < Stud%Surname) .AND. (List%Next%Surname > Stud%Surname)) then
            allocate(S)
            S%Surname = Stud%Surname
            S%Next => List%next
            List%next => S
         else
            call Insert_Item(Stud, List%Next)
         end if
      else if (Associated(List)) then
         allocate(List%Next)
         List%Next%Surname = Stud%Surname
      end if
    end subroutine Insert_Item

    ! Удаление списка
    pure recursive subroutine Remove_List(List)
       type(student), pointer        :: List
	   
       if (Associated(List)) then
          call Remove_List(List%next)
          deallocate(List)
       end if
    end subroutine Remove_List
end module Group_process
