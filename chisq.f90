   ! calculate a gaussian
   real function gau(x,xo,sig)
   implicit none
   real*8, intent(in) :: xo,sig,x
   real*8 pi
   
   ! define pi
   pi = 4.d0*datan(1.d0)
   
   gau = exp(-(x-xo)**2/(2*sig**2))/(sqrt(2*pi*sig**2))
   
   return
   end function gau
   
   subroutine chisq(parm,data,na,nl,chi)
   implicit none
   integer, intent(in) :: na,nl
   real*8, intent(inout) :: chi
   real*8 parm(na),data(nl,2)
   real*8 gau,residue,theory,const,pi
   integer j
   
   ! define pi
   pi = 4.d0*datan(1.d0)
   !print *, pi
   
   chi = 0
   do j=1,nl
      const = sqrt(2*pi*parm(2)**2)
      !const = 1
      theory = parm(3)*exp(-((data(j,1)-parm(1))**2)/(2*parm(2)**2))
      residue = data(j,2) - theory
      !print *, residue,theory,data(j,2)
      !print *, data(j,2), theory
      chi = chi + residue**2
   enddo 
   
   end subroutine chisq