   program gradmin
   implicit none
   real*8 parm(12),initparm(12),chi,chinew,step
   integer nlines,io,i,nfit,j,k
   real*8, allocatable :: data(:,:)
   integer, allocatable :: fitparm(:)
   real*8, allocatable :: grad(:)
   
   ! define the step size
   step = 0.01d0
   
   ! read in inital parameters
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
   ! count the number of iterations
   k=0
   do 
     ! print *, k
   
   ! replace values in the input file
   call inputfile(parm)
   
   ! run fresco
   call system("fresco < newfit.in > newfit.out")
   
   ! calculate chi square
   call chicalc(data,nlines,chi)
   print *, "chi=",chi
   
   ! find the gradiant of chi square function
   allocate (grad(nfit))
   call calcgrad(parm,fitparm,nfit,data,nlines,grad)
   
   ! move the points - gradient descent
   do i=1,12
   do j=1,nfit
      if (fitparm(j)==i) then
         !print *, parm(i), "before",grad(j)
         parm(i) = parm(i) - step*grad(j)
	 !print *, parm(i), "after move"
      end if
   enddo 
   enddo 
   
   deallocate (grad)
   
   ! keep parameters within physical bounds
   !call keepphysical(parm,fitparm,nfit)
   
   ! remake input file and run fresco
   call inputfile(parm)
   call system("fresco < newfit.in > newfit.out")
   
   ! calculate chi square
   call chicalc(data,nlines,chinew)
   print *, "newchi=", chinew
   
   ! need a condition for ending - if |newchi - chi| < some value
   if (abs(chinew - chi) < 0.1d0) then
      exit
   !else
   !   chi = chinew
   end if 
   
   ! repeat until here
   k = k + 1
   !print *, k
   !if (k>10) exit
   enddo
   !print *, k,chinew
   
   ! print final values
   do i=1,12
      print *, parm(i)
   enddo 
   
   end program gradmin