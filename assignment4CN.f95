    PROGRAM LUNDYASSIGNMENT4CN
        IMPLICIT NONE

		INTEGER, PARAMETER :: M=(200), N=(10)
		REAL, PARAMETER :: k= (10./M), h= (0.04/N)
		INTEGER :: t, y ! t,y are i,j
		REAL :: a, b, c, d
		REAL, dimension(M+1,N+1) :: u, F, DD, G
        
		!some IC's:
		
		u = 0.
		
		do  t=1, M+1
			u(t,1)=50 !bottom boundary
		enddo
		F=0
		G=u
		DD=0
		d=.000217*k/(2*h**2)
		a=-d
		b=1+2*d
		c=a
		!end of IC's
		
		do t=2, M+1
		
			do y=2, N
				DD(t,y)=u(t-1,y)*(1-2*d)+d*(u(t-1,y+1)+u(t-1,y-1))
				F(t,y)=c/(b-a*F(t,y-1))
				G(t,y)=(DD(t,y)-a*G(t,y-1))/(b-a*F(t,y-1))
			enddo
			do y=N, 2, -1
			
				u(t,y)=(-u(t,y+1)*F(t,y)+G(t,y))
			enddo
			
		enddo
		!PRINT *, u
		PRINT *, "Saving output in Assignment4CN.txt"
		! now make a text file with the output
		open(unit=25,file="Assignment4CN.txt")
		!make the output text file directly pasteable into matlab or excel, fixing the column major ordering.
		do y=1, N+1
			write(25,*) (u(t,y) , t=1,M+1),";" 
		enddo
		close(25)
		
    END PROGRAM LUNDYASSIGNMENT4CN