    PROGRAM LUNDYASSIGNMENT4Explicit
        IMPLICIT NONE

		INTEGER, PARAMETER :: M=(50000), N=(100)
		REAL, PARAMETER :: k= (10./M), h= (0.04/N)
		INTEGER :: t, y ! t,y are i,j
		REAL, dimension(M+1,N+1) :: u
        
		!some IC's:
		
		u = 0.
		
		do  t=1, M+1
			u(t,1)=50 !bottom boundary
		enddo
		!end of IC's
		
		do t=2, M+1
			do y=N, 2, -1
				u(t,y)=(u(t-1,y) + .000217*(k/(h**2))*(u(t-1,y+1)-2*u(t-1,y)+u(t-1,y-1)))
			enddo
		enddo
		!PRINT *, u
		PRINT *, "Saving output in Assignment4Explicit.txt"
		! now make a text file with the output
		open(unit=25,file="Assignment4Explicit.txt")
		!make the output text file directly pasteable into matlab or excel, fixing the column major ordering.
		do y=1, N+1
			write(25,*) (u(t,y) , t=1,M+1),";" 
		enddo
		close(25)
		
    END PROGRAM LUNDYASSIGNMENT4Explicit