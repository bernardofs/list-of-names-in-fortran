subroutine Menu 

! Menu de Comandos do programa

write(*,*) ""
write(*,*) ""
write(*,*) "................................."
write(*,*) "................................."
write(*,*) ""
write(*,*) "     Menu de funcionalidades"
write(*,*) ""
write(*,*) "1 - Inserir um nome"
write(*,*) "2 - Consultar um nome"
write(*,*) "3 - Exibir todos os nomes"
write(*,*) "4 - Excluir um nome"
write(*,*) "5 - Encerrar o programa"
write (*,*) ""
write (*,*) "Digite a opcao desejada"
write(*,*) "................................."
write(*,*) "................................."
write(*,*) ""
write(*,*) ""

end subroutine Menu

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine inserir(tamanhoVetor)

integer :: tamanhoVetor, flag, cont
character(len=30) :: nomes(100), consulta
COMMON/ZERANUM/nomes
 
flag = 0
cont = 1
 
write (*,*) "Insira um nome"
read (*,*) consulta
 
do while (cont <= tamanhoVetor .AND. flag /= 1)
 
  if (consulta == nomes(cont)) then
 
    write (*,*)  "O nome ja pertence a lista!!!"
       
	flag = 1
 
  end if  
 
  cont = cont + 1
 
end do
 
  if (flag == 0) then
 
    tamanhoVetor = tamanhoVetor + 1
 
    nomes(tamanhoVetor) = consulta
 
  end if
 
End subroutine inserir

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine consultar(tamanhoVetor)

!!Consultar um nome
 
integer :: tamanhoVetor, flag, cont
character(len=30) :: nomes(100), consulta
COMMON/ZERANUM/nomes
 
flag = 0
cont = 1
 
write (*,*) "Insira um nome"
read (*,*) consulta
 
do while (cont <= tamanhoVetor .AND. flag /= 1)
 
  if (consulta == nomes(cont)) then
 
    write (*,*)  "O nome pertence a lista!!!"
       
	flag = 1
 
  end if  
 
  cont = cont + 1
 
end do
 
  if (flag == 0) then
 
    write (*,*) "O nome nao pertence a lista!!!"
 
  end if
 
End subroutine consultar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine exibir(tamanhoVetor)

!! Exibir um nome

character(len=30) :: nomes(100)
integer :: tamanhoVetor, cont
COMMON/ZERANUM/nomes

cont = 1
 
do while (cont <= tamanhoVetor)
 
  write (*,*) nomes(cont)
 
  cont = cont + 1
 
end do

if (tamanhoVetor == 0) then

  write(*,*) "A Lista Esta Vazia"

end if

End subroutine exibir

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine excluir(tamanhoVetor)

character(len=30) ::  nomes(100), consulta
integer :: tamanhoVetor, cont, flag
COMMON/ZERANUM/nomes

!! Excluir um nome

flag = 0
cont = 1
   
write (*,*) "Insira um nome que deseja excluir"
read (*,*) consulta
 
do while (cont <= tamanhoVetor .AND. flag /= 1)
 
  if (consulta == nomes(cont)) then
 
    write (*,*)  "O nome foi excluido com Sucesso!!!"
	
    flag = 1
    nomes(cont) = nomes(tamanhoVetor)
    tamanhoVetor = tamanhoVetor - 1
 
  end if  
 
  cont = cont + 1
 
end do
 
if (flag == 0) then
 
  write (*,*) "O nome nao pertence a Lista!!!"
 
end if

End subroutine excluir

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine lerNomes(tamanhoVetor)
character(len=30) ::  nomes(100)
integer :: cont, x, tamanhoVetor
COMMON/ZERANUM/nomes

!! Ler o Arquivo

open(7, file ="ListaDeNomes.txt",status = "old", action = "read", position = "rewind", iostat = x)

cont = 1

read(7,*, iostat = x) nomes(cont)

do while (x==0)

  tamanhoVetor = tamanhoVetor + 1
  cont = cont + 1
  read(7,*, iostat = x) nomes(cont)
  
end do

close(7)


end subroutine lerNomes

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine escreverNomes(tamanhoVetor)

character(len=30) ::  nomes(100)
integer :: cont, x2, tamanhoVetor
COMMON/ZERANUM/nomes

!!Escrever no Arquivo

open(7, file ="ListaDeNomes.txt",status = "replace", action = "write", position = "rewind", iostat = x2)

cont = 1

do while (cont<=tamanhoVetor)
  
  write (7,*) nomes(cont)

  cont = cont + 1

end do  

close(7)
       
end subroutine escreverNomes




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!! Início do programa !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





program AgendaDenomes
IMPLICIT NONE

character(len=30) :: nomes(100)
integer :: opt, qtidade = 0
COMMON/ZERANUM/nomes

call lerNomes(qtidade)

call Menu

read(*,*) opt

do while (opt /= 5)

 if (opt == 1) then

   !Inserir um nome

   call inserir(qtidade)
   call Menu
   
 
 else if (opt == 2) then

   !Consultar um nome
   
   call consultar(qtidade)
   call Menu
 
 else if (opt == 3) then

   !Exibir todos os nomes
 
   call exibir(qtidade) 
   call Menu
 
 else if (opt == 4) then

   !Excluir um nome

   call excluir(qtidade) 
   call Menu
 
 else 

   !! Mensagem se digitar um num /= de 1 a 5
 
   write (*,*) "Digite um numero de 1 a 5"
 
   call Menu
 
 end if
 
 read(*,*) opt 

end do

write (*,*) ""
write (*,*) ""
write (*,*) "Programa encerrado com sucesso!!!"

call escreverNomes(qtidade) !! Escrever no arquivo

end program AgendaDenomes
