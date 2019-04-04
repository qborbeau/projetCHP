Module Matrices

use mod_lecture

Contains

subroutine Mat(Nx,Ny,dx,dy,D,AA,JA,IA)
  Integer, intent(in) :: Nx,Ny
  Real(PR), intent(in) :: dx,dy,D
  Real(PR), dimension(:), intent(out) :: AA,JA,IA
  Integer :: i,j,k,nbNN
 
  nbNN = 5*(Nx*Ny) - 2*Nx - 2*Ny !! nombre d'éléments non nuls ?
  allocate(AA(nbNN),IA(nbNN),JA(nbNN))

  k = 0
  do i=1,Nx*Ny
     do j=1,Ny*Ny
        if(i==j) then                           !! diagonale
           AA(k) = (2/Hx**2)+(2/Hy**2)
           IA(k) = i
           JA(k) = j
           k = k + 1
        elseif(i==j-1) .AND. (i/=Nx*Ny) .AND. (modulo(i,Nx)/=0) !diagonale sup
           AA(k) = -1/(dx**2)
           IA(k) = i
           JA(k) = j
           k = k + 1
        elseif(i==j+1) .AND. (i/=1) .AND. (modulo(j,Ny)/=0)  !diagonale inf
           AA(k) = -1/(dy**2)
           IA(k) = i
           JA(k) = j
           k = k + 1
        elseif(j==Ny+i)                        ! diagonale tout en haut
           AA(k) = -1/(dy**2)
           IA(k) = i
           JA(k) = j
           k = k + 1
        elseif(i==j+Nx)                                !diagonale tout en bas
           AA(k) = -1/(dx**2)
           IA(k) = i
           JA(k) = j
           k = k + 1
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
