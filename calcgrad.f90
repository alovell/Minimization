   subroutine calcgrad(parm,fparm,nf,data,nl,grad)
   use constants
   use F90library
   implicit none
   integer, intent(in) :: nf,nl,fparm(nf)
   real*8, intent(inout) :: parm(12),grad(nf)
   real*8, intent(in) :: data(nl)
   integer i,j
   real*8 jac(nf,nf),chip,chim,step
   real*8, allocatable :: d(:)
   real*8, allocatable :: e(:)
   
   !do i=1,nf
   !   print*, fparm(i)
   !enddo 
   
   ! for each parameter that's being fit, calculate chi^2 at \pm 0.1*parm
   step = 0.1d0
   do i=1,nf
      ! origianal parm + step, calculate chi square value
      parm(fparm(i)) = step + parm(fparm(i))
      call inputfile(parm)
      call system("fresco < newfit.in > newfit.out")
      call chicalc(data,nl,chip)
      
      ! original parm - step, calculate chi square value
      parm(fparm(i)) = parm(fparm(i)) - 2*step
      call inputfile(parm)
      call system("fresco < newfit.in > newfit.out")
      call chicalc(data,nl,chim)
      
      ! put parameter back at it's original value and move to next
      parm(fparm(i)) = parm(fparm(i)) + step
      !print *, parm(fparm(i)),chip,chim
      
      ! calculate derivative
      grad(i) = (chip - chim)/(2*step)
      !print *, grad(i)
   enddo 
   
   ! form the approximate jacobian
   do i=1,nf
      do j=i,nf
         jac(i,j) = grad(i)*grad(j)
	 jac(j,i) = jac(i,j)
	 !print *, jac(i,j),i,j
      enddo 
   enddo 
   
   ! diagonalize the jacobian and return it in grad
   ! uses a diagonalization routine recieved from M. Hjorth-Jensen
   allocate (e(nf))
   allocate (d(nf))
   call tred2(jac,nf,d,e)
   call tqli(d,e,nf)
   do i=1,nf
      grad(i) = d(i)
      print *, grad(i)
   enddo 
   deallocate(e)
   deallocate(d)
   
   end subroutine calcgrad