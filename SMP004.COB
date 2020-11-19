       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP004.
      *AUTHOR. ADRIANO GALAN DA SILVA.
      **************************************
      * MANUTENCAO DO CADASTRO DE PACIENTE   *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADPACI ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CPF
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES.

           SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOMEC WITH DUPLICATES
                    ALTERNATE RECORD KEY IS PLANO WITH DUPLICATES.

           SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS COD
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS ENDERECO  
                                       WITH DUPLICATES.

      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADPACI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPACI.DAT". 
       01 REGPACI.
          03 CPF           PIC 9(11).
          03 NOME          PIC X(30).
          03 DNASC.
             05 DIANAS     PIC 9(02).
             05 MESNAS     PIC 9(02).
             05 ANONAS     PIC 9(04).
          03 SEXO          PIC X(01).
          03 GENERO        PIC X(01).
          03 CONVENIO        PIC 9(04). 
          03 EMAIL         PIC X(30).
          03 TELEFONE      PIC 9(11).
          03 CEP           PIC 9(08).
          
      *-------------------------------------
       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT". 
       01 REGCONV.
          03 CODIGO        PIC 9(04).
          03 NOMEC         PIC X(30).
          03 PLANO         PIC 9(02).
      
      *-----------------------------------------
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT". 
       01 REGCEP.
          03 COD           PIC 9(08).
          03 ENDERECO      PIC X(30).
          03 BAIRRO        PIC X(20).
          03 CIDADE        PIC X(20).
          03 UF            PIC X(02).
          

      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-CONT2       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES. 
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 W-SEL         PIC 9(01) VALUE ZEROS.

       01 TXTESPEC      PIC X(15) VALUE SPACES.
       01 TXTPLANO      PIC X(50) VALUE SPACES.
       01 TXTUF         PIC X(20) VALUE SPACES.
       01 IND           PIC 9(02) VALUE ZEROS.
       
       
     
       01 TABSEXOX.
          03 FILLER     PIC X(11) VALUE "MMASCULINO".
          03 FILLER     PIC X(11) VALUE "FFEMENINO".
       01 TABESP REDEFINES TABSEXOX.
          03 TBSEXO   PIC X(11) OCCURS 2 TIMES.
       01 TXTSEXO.
          03 TXTSEXOCPF PIC X(01) VALUE SPACES.
          03 TXTSEXOTEXTO PIC X(10) VALUE SPACES.      

   

       01 TABGENEROX.
          03 FILLER     PIC X(15) VALUE "HETEROSEXUAL".
          03 FILLER     PIC X(15) VALUE "TRANSEXUAL".
          03 FILLER     PIC X(15) VALUE "BISSESUXUAL".
          03 FILLER     PIC X(15) VALUE "PANSEXUAL".
          03 FILLER     PIC X(15) VALUE "OUTRO".
          03 FILLER     PIC X(15) VALUE "NAO DECLARADO".
       01 TABGENERO REDEFINES TABGENEROX.
          03 TBGENERO   PIC X(15) OCCURS 6 TIMES.
       01 TXTGENERO.
          03 TXTGENERO1 PIC X(01) VALUE SPACES.
          03 TXTGENERO2 PIC X(14) VALUE SPACES. 

       01 TABPLANO.
          03 FILLER     PIC X(50) VALUE " ENFERMARIA REGINAL".
          03 FILLER     PIC X(50) VALUE " ENFERMARIA NACIONAL".
          03 FILLER     PIC X(50) VALUE " ENFERMARIA INTERNACIONAL".
          03 FILLER     PIC X(50) VALUE " APTO PADRÃO REGINAL".
          03 FILLER     PIC X(50) VALUE " APTO PADRAO NACIONAL".
          03 FILLER     PIC X(50) VALUE " APTO PADRAO INTERNACIONAL".
          03 FILLER     PIC X(50) VALUE " EMERGENCIA REGIONAL".
          03 FILLER     PIC X(50) VALUE " EMERGENCIA NACIONAL".
          03 FILLER     PIC X(50) VALUE " EMERCENCIA INTERNACIONAL".
          03 FILLER     PIC X(50) VALUE " PLANO GLOBAL ".
       01 TABEPLANO REDEFINES TABPLANO.
          03 TBPLANO   PIC X(50) OCCURS 10 TIMES.


      *-----------------------------------------------------------------
       SCREEN SECTION.  
       01  TELAPAC.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "  *********************** CADASTRO PACIE".
           05  LINE 02  COLUMN 41 
               VALUE  "NTE ******************************".
           05  LINE 04  COLUMN 01 
               VALUE  "  CPF:".
           05  LINE 05  COLUMN 01 
               VALUE  "  NOME:".
           05  LINE 06  COLUMN 01 
               VALUE  "  DATA NASCIMENTO:".
           05  LINE 07  COLUMN 01 
               VALUE  "  SEXO:".
           05  LINE 08  COLUMN 01 
               VALUE  "  GENERO:".
           05  LINE 09  COLUMN 01 
               VALUE  "  TELENONE:               EMAIL:".
           05  LINE 11  COLUMN 01 
               VALUE  "  CODIGO CONVENIO:          NOME CONVENIO".
           05  LINE 11  COLUMN 41 
               VALUE  "O:".
           05  LINE 12  COLUMN 01 
               VALUE  "  PLANO:".
           05  LINE 14  COLUMN 01 
               VALUE  "  CEP:".
           05  LINE 15  COLUMN 01 
               VALUE  "  RUA:                                 B".
           05  LINE 15  COLUMN 41 
               VALUE  "AIRRO:".
           05  LINE 16  COLUMN 01 
               VALUE  "  CIDADE:                              U".
           05  LINE 16  COLUMN 41 
               VALUE  "F:".
          
           05  TCPF
               LINE 04  COLUMN 08  PIC 999.999.999.99
               USING  CPF
               HIGHLIGHT.
           05  TNOME
               LINE 05  COLUMN 09  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TNASC
               LINE 06  COLUMN 20  PIC 99/99/9999
               USING  DNASC
               HIGHLIGHT.
           05  TSEXO
               LINE 07  COLUMN 09  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TTXTSEXO
               LINE 07  COLUMN 11  PIC X(10)
               USING  TXTSEXO
               HIGHLIGHT.
           05  TGENERO
               LINE 08  COLUMN 11  PIC X(01)
               USING  GENERO
               HIGHLIGHT.
           05  TTXTGENERO
               LINE 08  COLUMN 13  PIC X(15)
               USING  TXTGENERO
               HIGHLIGHT.
           05  TTELEFONE
               LINE 09  COLUMN 13  PIC 99.999999999
               USING  TELEFONE
               HIGHLIGHT.
           05  TEMAIL
               LINE 09  COLUMN 34  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
           05  TCONVE
               LINE 11  COLUMN 22  PIC 9(04)
               USING  CONVENIO
               HIGHLIGHT.
          05  TNOMEC
               LINE 11  COLUMN 43  PIC X(30)
               USING  NOMEC
               HIGHLIGHT.
           05  TTXTPLANO
               LINE 12  COLUMN 09  PIC X(50)
               USING  TXTPLANO
               HIGHLIGHT.
           05  TCEP
               LINE 14  COLUMN 07  PIC 99999.999
               USING  CEP
               HIGHLIGHT.
           05  TRUA
               LINE 15  COLUMN 08  PIC X(30)
               USING  ENDERECO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 15  COLUMN 48  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 16  COLUMN 11  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 16  COLUMN 44  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TTXTUF
               LINE 16  COLUMN 47  PIC X(20)
               USING  TXTUF
               HIGHLIGHT.
           

       01  TELAGEN.
           05  LINE 10  COLUMN 41 
               VALUE  "         N - NAO DECLARADO".
           05  LINE 11  COLUMN 41 
               VALUE  "         H - HETEROSEXUAL".
           05  LINE 12  COLUMN 41 
               VALUE  "         B - BISEXUAL". 
           05  LINE 13  COLUMN 41 
               VALUE  "         T - TRANSEXUAL".      
           05  LINE 14  COLUMN 41 
               VALUE  "         P - PANSEXUAL".
           05  LINE 15  COLUMN 41 
               VALUE  "         O - OUTRO".

       01  TELASEXO.
           05  LINE 10  COLUMN 41 
               VALUE  "         M - MASCULINO".
           05  LINE 11  COLUMN 41 
               VALUE  "         F - FEMENINO".      
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
   
      ********** ABRE OU CRIA ARQUIVO*************************
       R0.
           OPEN I-O CADPACI
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADPACI
                 CLOSE CADPACI
                 MOVE "*** ARQUIVO CADPACI FOI CRIADO **" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADMIGO" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim
           ELSE
                 NEXT SENTENCE.

       R0A.
           OPEN INPUT CADCONV
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CADCONV NAO ENCONTRADO **" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CADCONV " TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim
           ELSE
                 NEXT SENTENCE.

       R0B.
           OPEN INPUT CADCEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CEP NAO ENCONTRADO **" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CEP " TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim
           ELSE
                 NEXT SENTENCE.           
                 
      ********** FIM ABRE OU CRIA ARQUIVO*************************
      *********ZERAR CAMPOS E MOSTRAR A TELA**********************     
       R1.
           MOVE SPACES TO NOME SEXO EMAIL TXTSEXO GENERO TXTGENERO
           MOVE SPACES TO TXTPLANO NOMEC ENDERECO BAIRRO CIDADE UF
           MOVE ZEROS TO CPF PLANO DNASC TELEFONE CONVENIO CEP CODIGO
           MOVE ZEROS TO COD 
           DISPLAY TELAPAC. 
           
      *********FIM ZERAR CAMPOS E MOSTRAR A TELA**********************
       R2.
           ACCEPT TCPF
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO RotinaFim.
           

      ***** VERIFICA SE FOI DIGITADO UMA CPF
           IF CPF = 0
              MOVE "*** DIGITE O CPF ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R2.

       LER-CADPACI.
           READ CADPACI
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                PERFORM R5A
                PERFORM R6A
                PERFORM R9A
                PERFORM R10A
                DISPLAY TELAPAC
                GO TO Ace01
                
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADPACI" TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO RotinaFim
           ELSE
                NEXT SENTENCE.
       
       R3. 
           DISPLAY TELAPAC 
           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1. 
           IF NOME = SPACES 
              MOVE "DIGITE O NOME DO PACIENTE" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R3.  

       R4.    
           DISPLAY TELAPAC    
           ACCEPT TNASC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3. 
           IF DNASC = 0 
              MOVE "*** DIGITE A DATA DE NASCIMENTO ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R4.
           IF MESNAS < 1 OR MESNAS > 12
              MOVE "*** MES INVALIDO ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R4.    
           IF DIANAS < 1 OR DIANAS > 31
              MOVE "*** DIA INVALIDO ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R4.   
           IF ANONAS < 1900 OR ANONAS > 2020
              MOVE "*** ANO INVALIDO ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R4. 
   
       R5.  
           DISPLAY TELASEXO   
           MOVE 1 TO IND

           ACCEPT TSEXO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4. 
           IF SEXO = SPACES 
              MOVE "*** ESCOLHA UM SEXO ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R5. 
       R5A.    
           MOVE TBSEXO(IND) TO TXTSEXO
           IF TXTSEXOCPF NOT = SEXO
              ADD 1 TO IND
              IF IND < 3
                 GO TO R5A
              ELSE
                 MOVE "*** SEXO INVALIDO***" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO R5 
           ELSE
                MOVE ZEROS TO IND
                MOVE TXTSEXOTEXTO TO TXTSEXO
                DISPLAY TTXTSEXO.   
                DISPLAY TELAPAC.
       R6.
           DISPLAY TELAGEN
           MOVE 1 TO IND
           ACCEPT TGENERO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
       R6A.
           MOVE TBGENERO(IND) TO TXTGENERO
           IF TXTGENERO1 NOT = GENERO
              ADD 1 TO IND
              IF IND < 10
                 GO TO R6A
              ELSE
                 MOVE "*** TIPO GENERO INCORRETO***" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO R6
           ELSE
               MOVE ZEROS TO IND
               DISPLAY TTXTGENERO.
               DISPLAY TELAPAC.
        

       R7.       
           ACCEPT TTELEFONE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6. 
           IF TELEFONE = 0 
              MOVE "***DIGITE O TELEFONE***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R7.

       R8.    
           DISPLAY TELAPAC    
           ACCEPT TEMAIL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R7. 
           IF EMAIL = SPACES 
              MOVE "*** DIGITE UM EMAIL ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R8.

       R9.
           ACCEPT TCONVE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAPAC
                   GO TO R8.
           IF CONVENIO = 0 
                 MOVE "*** DIGOTE UM CODIGO  ***" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO R9.
           
      *    DISPLAY TELAPAC. 

       R9A.
           MOVE CONVENIO TO CODIGO.
           READ CADCONV
           MOVE TBPLANO(PLANO) TO TXTPLANO
           DISPLAY TXTPLANO 
           
           IF ST-ERRO NOT = "00"
                IF ST-ERRO = "23"
                    MOVE " ** CONVENIO NÃO CADASTRADO **" TO MENS
                    PERFORM RMensa THRU RMensa-FIM
                    GO TO R9
                ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADCONV" TO MENS
                  PERFORM RMensa THRU RMensa-FIM
                  GO TO RotinaFim
           ELSE
                DISPLAY TELAPAC. 
       R9A-FIM.     
    
       R10.
           ACCEPT TCEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAPAC
                   GO TO R9.
           IF CEP  = 0 
                 MOVE "*** DIGOTE UM CEP  ***" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO R10.        
      

       R10A.
           MOVE CEP TO COD.
           READ CADCEP
           
           IF ST-ERRO NOT = "00"
                IF ST-ERRO = "23"
                    MOVE " ** CEP NAO CADASTRADO **" TO MENS
                    PERFORM RMensa THRU RMensa-FIM
                    GO TO R10
                ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADCEP" TO MENS
                  PERFORM RMensa THRU RMensa-FIM
                  GO TO RotinaFim
           ELSE
                DISPLAY TELAPAC.
       R10A-FIM.         

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (24, 40) "DADOS OK (S/N) : ".
                ACCEPT (24, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R7.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGPACI
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM RMensa THRU RMensa-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  
                  GO TO AltRw1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DO CARRO"
                                                       TO MENS
                      PERFORM RMensa THRU RMensa-FIM
                      GO TO RotinaFim.  


      ***************** GRUD ****************
  
       Ace01.
                DISPLAY (24, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (24, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO Ace01.
                MOVE SPACES TO MENS
                DISPLAY (24, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3.
      *          
       ExcOpc.
                DISPLAY (24, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (24, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO ExcOpc.
       ExcDl1.
                DELETE CADPACI RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO RotinaFim.
      *
       AltOpc.
                DISPLAY (24, 40) "ALTERAR  (S/N) : ".
                ACCEPT (24, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R8.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO AltOpc.
       AltRw1.
                REWRITE REGPACI
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO CARRO"   TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO RotinaFim.
    

       RotinaFim.
           CLOSE CADPACI.
           CLOSE CADCEP.
           CLOSE CADCONV.
           EXIT PROGRAM.

      *********** Mensagem **********************
       RMensa.
                MOVE ZEROS TO W-CONT
                MOVE ZEROS TO W-CONT2.

       RMensa1.
               IF W-CONT2 < 10
                  DISPLAY (24, 12) Mens
               ELSE
                  DISPLAY (24, 12) Limpa
                  MOVE SPACES TO MENS
                  GO TO RMensa-Fim.

       RMensa2.
                ADD 1 TO W-CONT
                IF W-CONT < 1000
                   GO TO RMensa2
                ELSE
                   ADD 1 TO W-CONT2
                   MOVE ZEROS TO W-CONT
                   DISPLAY (24, 12) LIMPA.
       RMensa3.
                ADD 1 TO W-CONT
                IF W-CONT < 1000
                   GO TO RMensa3
                ELSE
                   ADD 1 TO W-CONT2
                   MOVE ZEROS TO W-CONT
                   DISPLAY (24, 12) MENS.
                   GO TO RMensa1.
            
       RMensa-FIM.
                EXIT.
       FIM-ROT-TEMPO.
    