  module pqueue_mod
    implicit none
    private
    public element_t, pqueue_t

    type, abstract :: element_t
    contains
      procedure(priority), deferred :: priority
      procedure(ass), deferred :: ass
      generic :: assignment(=) => ass
    end type

    abstract interface
      function priority(this) 
        import element_t
        class(element_t), intent(in) :: this
        integer :: priority
      end function

      subroutine ass(lhs, rhs)
        import element_t
        class(element_t), intent(out) :: lhs
        class(element_t), intent(in) :: rhs
      end subroutine
    end interface

    type :: pqueue_t
      class(element_t), allocatable :: array(:)
      integer :: n=0, nalloc=0
    contains
      procedure :: top
      procedure :: peek
      procedure :: insert
      procedure :: remove
      procedure :: update
      procedure :: size => pqueue_size
    end type

  contains

    subroutine top(this, element)
      class(pqueue_t), intent(inout) :: this
      class(element_t), intent(out) :: element

      if (this % n < 1) error stop 'top on empty queue'
      element = this % array(1)

      this % array(1) = this % array(this % n)
      this % n = this % n - 1

      if (this % n == 0) then
        deallocate(this % array)
        this % nalloc = 0
        return
      endif

      if (this % n == 1) return

      call pushDown(this, 1)
    end subroutine top



    subroutine peek(this, element) 
      class(pqueue_t), intent(in) :: this
      class(element_t), intent(out)  :: element
      if (this % n < 1) error stop 'peek on empty queue'
      element = this % array(1)
    end subroutine peek



    subroutine insert(this, element)
      class(pqueue_t), intent(inout) :: this
      class(element_t), intent(in) :: element

      class(element_t), allocatable :: newarray(:)
      integer :: i

      if (.not. allocated(this % array)) then
        this % nalloc = 1
        allocate(this % array(this % nalloc), mold=element)
      endif

      if (this % n == this % nalloc) then
        this % nalloc = this % nalloc * 2
        allocate(newarray(this % nalloc), mold=element)
        do i = 1, this % n
          newarray(i) = this % array(i)
        enddo
        call move_alloc(newarray, this % array)
      endif

      this % n = this % n + 1
      this % array(this % n) = element
      call bubbleUp(this, this % n)

    end subroutine insert

    subroutine remove(this, element)
      class(pqueue_t), intent(inout) :: this
      class(element_t), intent(in) :: element
    end subroutine remove

    subroutine update(this, element, newelement)
      class(pqueue_t), intent(inout) :: this
      class(element_t), intent(in) :: element, newelement
    end subroutine update

    function pqueue_size(this)
      integer :: pqueue_size
      class(pqueue_t), intent(in) :: this
      pqueue_size = this % n
    end function pqueue_size



    subroutine bubbleUp(this, index)
      class(pqueue_t), intent(inout) :: this
      integer, intent(in) :: index

      class(element_t), allocatable :: ecur
      integer :: icur, ipar

      allocate(ecur, mold=this % array(1))
      icur = index
      ecur = this % array(icur)

      do
        if (icur == 1) exit
        ipar = icur / 2
        if (this%array(ipar) % priority() >= ecur % priority()) exit
        this % array(icur) = this % array(ipar)
        icur = ipar
      enddo
      this % array(icur) = ecur
    end subroutine bubbleUp



    subroutine pushDown(this, index)
      class(pqueue_t), intent(inout) :: this
      integer, intent(in) :: index

      class(element_t), allocatable :: ecur
      integer :: icur, ilch, irch, ich

      allocate(ecur, mold=this % array(1))
      icur = index
      ecur = this % array(icur)

      do
        if (icur*2 > this % n) exit 

        ! select a child with higher priority
        if (icur*2 + 1 > this % n) then
          ich = icur*2
        else
          ilch = icur*2
          irch = icur*2 + 1
          if (this%array(ilch)%priority() > this%array(irch)%priority()) then
            ich = ilch
          else
            ich = irch
          endif
        endif

        if (this % array(ich) % priority() <= ecur % priority()) exit
        this % array(icur) = this % array(ich)
        icur = ich
      enddo
      this % array(icur) = ecur
    end subroutine pushDown

  end module pqueue_mod
