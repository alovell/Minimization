   program DHMin
   implicit none
   integer io,nlines,i,narg,chitries,parmtries
   real*8 datatotal,step,chi,chiold,ran,stepi
   real*8, allocatable :: data(:,:)
   real*8, allocatable :: parm(:)
   character (len=20) temp
   
   ! it's going to be very hard to couple this to fresco
   ! let's initially write this to fit a gaussian to some "data"
   
   ! define a step for searching
   stepi = 0.1d0
   
   ! read initial parameters from command line
   ! convert the strings to reals
   narg = IARGC()
   allocate (parm(narg))
   parm = 0
   do i=1,narg
      temp = ""
      call getarg(i,temp)
      !print *, temp
      read(temp,*) parm(i)
      !print *, parm(i)
   enddo 
   
   ! read in data and normalize (if not already normalized)
   open(unit=3,file="data.txt")
   nlines=0
   do 
      read(3,*,iostat=io) 
      if (io/=0) exit
      nlines=nlines+1
   enddo 
   
   rewind(3)
   
   allocate (data(nlines,2))
   
   datatotal = 0
   do i=1,nlines
      read(3,*) data(i,1), data(i,2)
      datatotal = datatotal + data(i,2)
      !print *, data(i,1), data(i,2), datatotal
   enddo 
   
   close(3)
   
   ! normalize
   !do i=1,nlines
   !   data(i,2) = data(i,2)/datatotal
      !print *, data(i,2)
   !enddo 
   
   ! let's try this again
   chitries = 0
   
   ! calculate chi square before changing parameters
   call chisq(parm,data,narg,nlines,chiold)
   
   ! do the minimization
   ! change step size based on random number
   call random_seed()
   
   ! initialize step to stepi
   step = stepi
   
   parmtries = 0
   open(unit=3,file="chisqruns.txt")
   do while (parmtries < 2*narg)
      do i=1,narg
      chitries = 0
      do while (chitries < 2)
         ! iterate first parameter and compare chi square values
         parm(i) = parm(i) + step
         call chisq(parm,data,narg,nlines,chi)
         !print *, chi,chiold
         if (chi > chiold) then
            !print *, chi, chiold, chitries
	    write(3,*) parm(1),parm(2),chi,step
	    call random_number(ran)
            step = stepi*ran
            chiold = chi
            chitries = chitries + 1
	    parmtries = parmtries + 1
         else
            !print *, chi,chiold,chitries
	    write(3,*) parm(1),parm(2),parm(3),chi,step
            chiold = chi
	    call random_number(ran)
            step = stepi*ran
            chitries = 0
	    parmtries = 0
         end if
      enddo
      enddo
   enddo
   
   close(3)
   
   print *, parm(1),parm(2),parm(3),chitries,parmtries
   
   end program DHMin