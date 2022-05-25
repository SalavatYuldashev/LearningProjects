module Stack_Impl

implicit none
   
   ! Узел стека
   type node
      character (:), allocatable :: value
      type(node), pointer :: next => Null()
   end type node

   ! Стек
   type stack
      type(node), pointer :: head => Null()
      
      contains
         procedure :: push
         procedure :: top
         procedure :: pop
   end type stack

contains
   
   ! Добавление на вершину стека
   recursive subroutine push(this, v)
      class(stack), intent(inout) :: this
      character(*), intent(in) :: v
      class(node), pointer  :: temp
     
      if (.NOT.(associated(this%head))) then
         allocate(this%head)
         this%head%value = v
      else
         allocate(temp)
         temp%value = v
         temp%next => this%head
         this%head => temp
     end if
   end subroutine push
   
   ! Вершина стека
   function top(this) result(t)
      class(stack), intent(in) :: this
      class(node), pointer     :: t
	  
      t => this%head
   end function top
   
   ! Исключение вершины стека
   recursive subroutine pop(this)
      class(stack), intent(inout) :: this
      class(node), pointer :: temp
     
      if (.NOT.(associated(this%head))) then
         return
      else
         temp => this%head
         this%head => this%head%next
         temp%next => Null()
         deallocate(temp)
         nullify(temp)
      end if
   end subroutine pop

end module Stack_Impl