  module implement_mod
    use pqueue_mod, only : element_t
    implicit none
    private
    public myelement_t

    ! Example data type to be used in priority queue data structure
    type, extends(element_t) :: myelement_t
      character(len=20) :: item
      integer :: ipriority
      type(myelement_t), pointer :: original => null()
      integer :: index = -1
    contains
      ! these methods are deferred from the abstract class
      procedure :: priority, updateindex, element2index
      ! this method is just for debugging
      procedure :: display
    end type

  contains

    integer function priority(this)
      class(myelement_t), intent(in) :: this
      priority = this % ipriority
    end function



    subroutine updateindex(this, index)
      class(myelement_t), intent(inout) :: this
      integer, intent(in) :: index
!
! Called by "pqueue" methods to advise on the index of element's copy in
! pqueue's heap array. It is used by "update" or "remove" to localize the
! element in O(1) time.
!
      if (associated(this % original)) then
        this % original % index = index
      else
        print *, 'warning, index not associated'
      endif
    end subroutine 



    integer function element2index(this) result(index)
      class(myelement_t), intent(in) :: this
! 
! Called by "update" and "remove" methods to localize the index.
! Return valid index (1...n) to the element
! Return "-1" if element is not present
!
      index = this % index
    end function



    subroutine display(this, fid)
      class(myelement_t), intent(in) :: this
      integer, intent(in) :: fid
      write(fid,'(a,i0,a)',advance='no') '['//trim(this%item)//' P=', &
         this%ipriority,'] '
    end subroutine

  end module implement_mod
