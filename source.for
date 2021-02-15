        SUBROUTINE error(i)
            GOTO(1,2,3) i
1           PAUSE 'ERROR! THE MATRIX IS EMPTY'
            GOTO 4
2           PAUSE 'R/W ERROR'
            GOTO 4
3           PAUSE 'OVERFLOW'
4           STOP
        END
        SUBROUTINE convert_if(n,di,ia,al,au)
        DIMENSION di(*),ia(*),al(*),au(*)
        IF (n .EQ. 0) CALL error(1)
        OPEN (1,FILE='convert_matrix.txt')  
        DO i=1,n
          DO j=1,n
            k=i-j
            if (k)10,11,12
10          if (abs(k) .GT. (ia(i+1)-ia(i))) GOTO 13
            WRITE(1,100)au(ia(i)+abs(k)-1)
            GOTO 14
11          WRITE(1,100) di(i)
            GOTO 14
12          if (k .GT. (ia(j+1)-ia(j))) GOTO 13
            WRITE(1,100)al(ia(j)+k-1)
            GOTO 14
13          WRITE(1,100)0.0
14          CONTINUE
          END DO
          WRITE(1,'(/\)')
        END DO
        CLOSE(1)
100     FORMAT(F5.1\)
      END
      FUNCTION i_size(n,ia)
        DIMENSION ia(*)
        i_size=ia(n+1)
        RETURN
      END
      SUBROUTINE float_data_iof(mode,a,i1,i2,j,file_name)
        !a-common matrix
        !i1,i2-first and last pos in matrix
        !j-first record in file
        !mode 1 - read text file
        !mode 2 - read bin file
        !mode 3 - write bin file
        !mode 4 - write text file
        DIMENSION a(*)
        CHARACTER *10 file_name
        IF ((mode .EQ. 1) .OR. (mode .EQ. 4)) THEN
          OPEN(1,FILE=file_name)
          k=1
20        IF (k. EQ. j) GOTO 21
          READ(1,*,ERR=22)
          k=k+1
          GOTO 20
        ELSE 
          OPEN(1,FILE=file_name,FORM='UNFORMATTED',ACCESS='DIRECT',
     &    RECL=4)
        END IF
21      CONTINUE
        DO i=i1,i2
          IF (mode .EQ. 1) READ(1,*,ERR=22) a(i)
          IF (mode .EQ. 2) READ(1,REC=j,ERR=22) a(i)
          IF (mode .EQ. 3) WRITE(1,REC=j,ERR=22) a(i)
          IF (mode .EQ. 4) WRITE(1,*,ERR=22) a(i)
          j=j+1
        END DO
        CLOSE(1)
        GOTO 23
22      CALL error(2)
23      CONTINUE
      END
      SUBROUTINE int_data_iof(mode,ia,i1,i2,j,file_name)
        !a-common matrix
        !i1,i2-first and last pos in matrix
        !j-first record in file
        !mode 1 - read text file
        !mode 2 - read bin file
        !mode 3 - write bin file
        !mode 4 - write text file
        DIMENSION ia(100)
        CHARACTER *10 file_name
        IF ((mode .EQ. 1) .OR. (mode .EQ. 4)) THEN
          OPEN(1,FILE=file_name)
          k=1
30        IF (k. EQ. j) GOTO 31
          READ(1,*,ERR=32)
          k=k+1
          GOTO 30
        ELSE 
          OPEN(1,FILE=file_name,FORM='UNFORMATTED',ACCESS='DIRECT',
     &    RECL=4)
        END IF
31      CONTINUE
        DO i=i1,i2
          IF (mode .EQ. 1) READ(1,*,ERR=32) ia(i)
          IF (mode .EQ. 2) READ(1,REC=j,ERR=32) ia(i)
          IF (mode .EQ. 3) WRITE(1,REC=j,ERR=32) ia(i)
          IF (mode .EQ. 4) WRITE(1,*,ERR=32) ia(i)
          j=j+1
        END DO
        CLOSE(1)
        GOTO 33
32      CALL error(2)
33      CONTINUE
      END
      
      SUBROUTINE matrix_io(mode,a,n,mtrx_file)       
        DIMENSION a(*)
        CHARACTER *10 mtrx_file
        IF (n) 38,38,40
38      OPEN(1,FILE='size.txt')
        READ(1,*,ERR=39)n
        CLOSE(1)
        GOTO 40
39      CALL error(2)
        !n - size of matrix
        !i1,i2-positions
40      CONTINUE
        DO j=1,4
          GOTO (41,42,43,44) j
          !r/w di
41        i1=1
          i2=n
          GOTO 45
          !r/w ia
42        i1=n+1
          i2=2*n+1
          GOTO 46
          !i_size(n,a(n+1))-1-size of al,au
          !r/w al
43        i1=2*n+2
          i2=2*n+i_size(n,a(n+1))
          GOTO 45
          !r/w au
44        i1=2*n+i_size(n,a(n+1))+1
          i2=2*n+2*i_size(n,a(n+1))-1
45        CALL float_data_iof(mode,a,i1,i2,i1,mtrx_file)
          GOTO 47
46        CALL int_data_iof(mode,a,i1,i2,i1,mtrx_file)
47        CONTINUE
        END DO
      END
       SUBROUTINE text_bin_files(a,mode)
        !a - common matrix
        !n - size of matrix
        !mode=1 text to bin
        !mode=2 bin to text
        !i=1 read text/bin file
        !i=2 write bin/text file
        DIMENSION a(*)
        OPEN(1,FILE='size.txt')
        READ(1,*,ERR=49)n
        CLOSE(1)
        GOTO 50
49      CALL error(2)
50      CONTINUE
        DO i=1,2
          GOTO (51,52,52,51)mode
51        CALL matrix_io(mode,a,n,'matrix.txt')
          CALL vector_io(mode,a,n,'vector.txt')
          GOTO 53
52        CALL matrix_io(mode,a,n,'matrix.bin')
          CALL vector_io(mode,a,n,'vector.bin')
53        CONTINUE 
          mode=mode+2
        END DO  
      END      
      SUBROUTINE vector_io(mode,a,n,vec_file)
        DIMENSION a(*)
        CHARACTER *10 vec_file
        if (n) 58,58,60
58      OPEN(1,FILE='size.txt')
        READ(1,*,ERR=59)n
        CLOSE(1)
        GOTO 60
59      CALL error(2)
        !n - size of matrix
        !i1,i2-positions
        !r/w vector
60      CONTINUE
        i1=2*n+2+2*(i_size(n,a(n+1))-1)
        i2=3*n+1+2*(i_size(n,a(n+1))-1)
        CALL float_data_iof(mode,a,i1,i2,1,vec_file)
      END
      SUBROUTINE result_o(a,n)
        DIMENSION a(*)
        !a - common matrix
        !n-size of matrix
        k=i_size(n,a(n+1))
        i1=3*n+1+2*(k-1)+1
        i2=3*n+1+2*(k-1)+n
        CALL float_data_iof(4,a,i1,i2,1,'result.txt')
      END
      SUBROUTINE matrix_generate(n,di,ia,al,au)
        DIMENSION di(*),ia(*),al(*),au(*)
        !n/2 - max profil
        !generate vector ia
        k=0
        ia(1)=1
        DO i=1,n
          IF (i .LE. n/2) THEN 
            k=k+1
          ELSE 
            k=k-1
          END IF
          k=MAX(0,k)
          ia(i+1)=ia(i)+k
        END DO
        !generate di
        DO i=1,n
          di(i)=3
        END DO
        !generate al,au
        DO i=1,(n-mod(n,2))**2/4
          al(i)=1
          au(i)=2
        END DO     
      END      
      SUBROUTINE vector_generate(n,vec)
        DIMENSION vec(*)
        DO i=1,n
          vec(i)=1
        END DO
      END
      SUBROUTINE data_generate(n,a)
        COMMON nmax
        DIMENSION a(*)
        !nmax-size of a
        !nsize=di+ia+al+au+vec+result
        !na=size of al and au
        na=(n-MOD(n,2))**2/4
        nsize=4*n+2*na+1
        IF ((n.LT.1) .OR. (nsize.GT.nmax)) CALL error(3)
        CALL matrix_generate(n,a(1),a(n+1),a(2*n+2),a(2*n+2+na))
        CALL vector_generate(n,a(2*n+2+2*na))
        CALL matrix_io(3,a,n,'matrix.bin')
        CALL vector_io(3,a,n,'vector.bin')
        OPEN(1,FILE='size.txt')
        WRITE(1,*)n
        CLOSE(1)
      END                  
     
      SUBROUTINE multiplication(n,di,ia,al,au,vec,res)
        DIMENSION di(*),ia(*),al(*),au(*),vec(*),res(*)
        IF (n .EQ. 0) CALL error(1)
        DO i=1,n
          res(i)=res(i)+di(i)
          DO j=1,ia(i+1)-ia(i)
            res(j+i)=res(j+i)+al(ia(i)+j-1)*vec(i)
            res(i)=res(i)+au(ia(i)+j-1)*vec(i+j)
         END DO
        END DO
      END
      PROGRAM main
      !a(1)-a(n) - pos of di
      !a(n+1)-a(2n+1) - pos of ia
      !k=a(2n+1) - size of au/al +1
      !a(2n+2)-a(2n+k) - pos of al
      !a(2n+k+1)-a(2n+2k-1) - pos of au
      !a(2n+2k)-a(3n+2k-1) - pos of vector
      !a(3n+2k)-a(4n+2k-1) - pos of result
        DIMENSION a(420000000)
        COMMON nmax
        nmax=420000000
        n=0
70      PRINT *,'1 - read matrix out of file'
        PRINT *,'2 - read vector out of file'
        PRINT *,'3 - generate data'
        PRINT *,'4 - convert to plotniy format'
        PRINT *,'5 - convert text or bin files'
        PRINT *,'6 - matrix multiplication'
        PRINT *,'7 - exit'
        READ(*,*)I
        GOTO(71,72,73,74,75,76,77)I
        GOTO 70
71      CALL matrix_io(2,a,n,'matrix.bin')
        k=i_size(n,a(n+1))
        GOTO 70
72      CALL vector_io(2,a,n,'vector.bin')
        GOTO 70
73      PRINT *,'enter size of matrix'
        READ (*,*)n
        CALL data_generate(n,a)
        n=0
        GOTO 70
74      CALL convert_if(n,a(1),a(n+1),a(2*n+2),a(2*n+k+1))
        GOTO 70
75      PRINT *,'1 - text to bin'
        PRINT *,'2 - bin to text'
        READ(*,*)i
        CALL text_bin_files(a,i)
        GOTO 70        
76      CALL multiplication(n,a(1),a(n+1),a(2*n+2),a(2*n+k+1),
     &  a(2*n+2*k),a(3*n+2*k))
        CALL result_o(a,n)
        GOTO 70
77      CONTINUE
      END
        
        
