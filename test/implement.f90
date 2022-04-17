  module implement_mod
    use pqueue_mod, only : element_t
    implicit none
    private
    public myelement_t

    type, extends(element_t) :: myelement_t
      character(len=20) :: item
      integer :: ipriority
    contains
      procedure :: priority
      procedure :: ass
    end type

  contains

    function priority(this)
      class(myelement_t), intent(in) :: this
      integer :: priority
      priority = this % ipriority
    end function

    subroutine ass(lhs, rhs)
      class(myelement_t), intent(out) :: lhs
      class(element_t), intent(in) :: rhs

      select type(rhs)
      class is (myelement_t)
        lhs % item = rhs % item
        lhs % ipriority = rhs % ipriority
      class default
        error stop 'rhs is not myelement_t'
      end select
    end subroutine
  end module implement_mod


