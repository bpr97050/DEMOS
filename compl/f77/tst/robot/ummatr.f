c
c************************************************
c       п/п умножения матриц (а) на (в)
c       результат записывается в (с)
c************************************************
c
c       na -номер матрицы (а)
c       nb -номер матрицы (в)
c       nc -номер матрицы (с)
c       l1 -число столбцов матрицы (а)
c       l2 -число столбцов матрицы (в)
c       kk -число строк матриц (а) и (в)
c
c
	subroutine ummatr(a,b,c,na,nb,nc,l1,l2,kk)
	dimension a(10,4,4),b(10,4,4),c(10,4,4)
	call obmatr(c,nc,kk,l2)
	do 10 k=1,kk
	do 10 l=1,l2
	do 10 i=1,l1
10      c(nc,k,l)=c(nc,k,l)+(a(na,k,i)*b(nb,i,l))
	return
	end
