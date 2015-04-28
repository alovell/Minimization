   subroutine keepphysical(parm,fparm,nf)
   implicit none
   real*8, intent(inout) :: parm(12)
   integer, intent(in) :: nf
   integer, intent(in) :: fparm(nf)
   real*8 bound(12,2) ! 1 is lower, 2 is upper
   integer i
   
   ! define the bounds for each parameter
   bound(1,1) = 25d0
   bound(1,2) = 200d0
   bound(2,1) = 0.7d0
   bound(2,2) = 1.5d0
   bound(3,1) = 0.4d0
   bound(3,2) = 0.8d0
   bound(4,1) = 0.0d0
   bound(4,2) = 50d0
   bound(5,1) = 0.7d0
   bound(5,2) = 1.5d0
   bound(6,1) = 0.1d0
   bound(6,2) = 0.7d0
   bound(7,1) = 0.0d0
   bound(7,2) = 70d0
   bound(8,1) = 0.7d0
   bound(8,2) = 1.5d0
   bound(9,1) = 0.1d0
   bound(9,2) = 0.5d0
   bound(10,1) = 1d0
   bound(10,2) = 20d0
   bound(11,1) = 0.7d0
   bound(11,2) = 1.5d0
   bound(12,1) = 0.3d0
   bound(12,2) = 0.8d0
   
   !do i=1,nf
   !  print *, i,fparm(i)
   !enddo
   
   ! if fitted parm higher than bounds, lower it until inside
   ! if fitted parm lower than bounds, raise it until inside
   do i=1,nf
      do while (parm(fparm(i)) > bound(fparm(i),2))
         parm(fparm(i)) = parm(fparm(i)) - parm(fparm(i))/2.d0
	 !print *, parm(fparm(i)),bound(fparm(i),2),fparm(i)
      enddo 
      do while (parm(fparm(i)) < bound(fparm(i),1)) 
         parm(fparm(i)) = parm(fparm(i)) + 2.d0 * abs(parm(fparm(i)))
	 !print *, parm(fparm(i)),bound(fparm(i),1),fparm(i)
      enddo 
      !if (parm(fparm(i)) > bound(fparm(i),2)) then
      !end if
      print *, parm(fparm(i)) 
   enddo
            
   end subroutine keepphysical