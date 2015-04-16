   program gradmin
   implicit none
   real*8 parm(12),initparm(12),chi,chinew
   integer nlines,io,i,nfit,j
   real*8, allocatable :: data(:,:)
   integer, allocatable :: fitparm(:)
   real*8, allocatable :: grad(:)
   
   ! read in inital parameters - given by user and specified in original input file
   ! can put this into a subroutine
   nfit = 0
   open(unit=1,file="fittingparm.txt")
   do i=1,12
      read(1,*) parm(i)
      if (parm(i)==0) then
         nfit = nfit + 1
      end if 
   enddo 
   close(1)
   !print *, "nfit", nfit
   
   allocate (fitparm(nfit))
   
   ! read in the data points
   open(unit=2,file="xexp.txt")
   nlines = 0
   do 
      read(2,*,iostat=io)
      if (io/=0) exit
      nlines = nlines + 1
   enddo 
   
   rewind(2)
   
   allocate (data(3,nlines))
   
   do i=1,nlines
      read(2,*) data(1,i),data(2,i),data(3,i)
   enddo
   
   close(2)
   
   ! read in the initial parameters
   j = 1
   open(unit=3,file="initialparm.txt")
   do i=1,12
      read(3,*) initparm(i)
      if (parm(i)==0) then
         parm(i) = initparm(i)
      else 
         fitparm(j) = i
	 !print *,fitparm(j),j
	 j = j + 1
      end if 
      !print *, parm(i)
   enddo 
   
   ! need to repeat from here
   
   ! replace values in the input file
   call inputfile(parm)
   
   ! run fresco
   call system("fresco < newfit.in > newfit.out")
   
   ! calculate chi square
   call chicalc(data,nlines,chi)
   print *, chi
   
   ! find the gradiant of chi square function
   allocate (grad(nfit))
   call calcgrad(parm,fitparm,nfit,data,nlines,grad)
   
   ! move the points
   
   ! calculate chi square
   call chicalc(data,nlines,chinew)
   
   ! need a condition for ending - if |newchi - chi| < some value
   !if (abs(chinew - chi) < 0.1) then
   !   exit
   !else
   !   chi = chinew
   !end if 
   
   ! repeat until here
   
   ! print final values
   do i=1,12
      print *, parm(i)
   enddo 
   
   end program gradmin