!----------------------------------------!
!        Lecture fichier paramètres       !
!----------------------------------------!

Module mod_lecture

  !-------------------- Modules -------------------------!

  !------------------------------------------------------!

  Implicit None


  Contains
  
    Subroutine lec_param(nom,param_def)
      Character (len=*), Intent(In)  :: nom
      Integer :: ierr, k
      Integer, Parameter :: iu = 20, n=12
      Character (len=20), Dimension(n) :: nom_param 
      Real :: param
      Character (len=1000) :: ligne
      Character (len=10) :: mot
      Logical, Dimension(n) :: if_param
      Real, Intent(Out), Dimension(n) :: param_def

      !initialisation
      nom_param(1) = "Nx"
      nom_param(2) = "Ny"
      nom_param(3) = "Lx"
      nom_param(4) = "Ly"
      nom_param(5) = "D"
      nom_param(6) = "dt"

      param_def(1) = 100
      param_def(2) = 100
      param_def(3) = 1.
      param_def(4) = 1.
      param_def(5) = 1.2
      param_def(6) = 0.0001

      if_param(1) = .False.
      if_param(2) = .False.
      if_param(3) = .False.
      if_param(4) = .False.
      if_param(5) = .False.
      if_param(6) = .False.

      !instructions
      Open (unit=iu,file=nom,action="read")
      Do k=1,n
         Do
            Read (iu,"(a)" ,iostat=ierr) ligne !lis la ligne et la stocke
            If (ierr /= 0) Exit
            Read (ligne,*) mot !lis le premier mot de la ligne
            If (mot == nom_param(k)) Then !recherche du mot dans la ligne
               Read (ligne,*)mot, param
               Print*,"Valeur prise pour ",mot, " ,", param
               if_param(k) = .True.
            End If
         End Do
         If (if_param(k) .eqv. .False.) Then
            Print*,"Valeur par defaut prise pour ",nom_param(k), " ,",param_def(k) 
         End If
         Rewind(unit=iu, iostat=ierr) !retourne au début du fichier
      End Do
      Close(iu)

    End Subroutine lec_param
      
End Module mod_lecture
