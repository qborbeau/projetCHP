Module Matrices

use mod_lecture

Contains

subroutine Mat(A,Nx,Ny,dx,dy,D)
  Integer, intent(in) :: Nx,Ny
  Real(PR), intent(in) :: dx,dy,D
  Real(PR), dimension(Nx*Ny,Nx*Ny), intent(inout) :: A
  Integer :: i,j
 
  do i=1,Nx*Ny
     do j=1,Ny*Ny
        if(i==j) then                           !! diagonale
           A(i,j) = (2/Hx**2)+(2/Hy**2)
        elseif(i==j-1) .AND. (i/=Nx*Ny) .AND. (modulo(i,Nx)/=0) !diagonale sup
           A(i,j) = -1/(dx**2)
        elseif(i==j+1) .AND. (i/=1) .AND. (modulo(j,Ny)/=0)  !diagonale inf
           A(i,j) = -1/(dy**2)
        elseif(j==Ny+i)                        ! diagonale tout en haut
           A(i,j) = -1/(dy**2)
        elseif(i==j+Nx)                                !diagonale tout en bas
           A(i,j) = -1/(dx**2)
        else
          A(i,j) = 0
       end if
    end do
 end do

End subroutine Mat

subroutine secondMembre(F,Nx,Ny,dx,dy,D,fsource,g,h)
  real(PR), dimension(Nx*Ny,1), intent(out) :: F
  real(PR), dimension(Nx,Ny), intent(out) :: fsource
  real(PR), dimension(Nx), intent(in) :: g
  real(PR), dimension(Ny), intent(in) :: h
  Integer, intent(in) :: Nx,Ny
  Real(PR), intent(in) :: dx,dy,D
  Integer :: i,j
  C=0
  do i=1,Nx
     do j=1,Ny
        F(C) = fsource(i,j)
        if ((i==1) .OR. (i==Nx)) then
           F(C) = F(C) + g(j)*dy**2/D
           if ((j==1) .OR. (j==Ny)) then
              F(C) = F(C) + h(i)*dx**2/D
           end if
        elseif ((j==1) .OR. (j==Ny)) then
           F(C) = F(C) + h(i)*dx**2/D
           if ((i==1) .OR. (i==Nx)) then
              F(C) = F(C) + g(j)*dy**2/D
           end if
        end if
        C = C + 1
     end do
  end do

End Module Matrices
