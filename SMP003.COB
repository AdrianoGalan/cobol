       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP003.
      *AUTHOR. ADRIANO GALAN DA SILVA.
      **************************************
      * MANUTENCAO DO CADASTRO CONVENIO  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOMEC WITH DUPLICATES
                    ALTERNATE RECORD KEY IS PLANO WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT". 
       01 REGCONV.
          03 CODIGO        PIC 9(04).
          03 NOMEC         PIC X(30).
          03 PLANO         PIC 9(02).
        
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

       01 TXTPLANO      PIC X(50) VALUE SPACES.
       01 IND           PIC 9(02) VALUE ZEROS.

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
       01  TELACONV.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "              ************ CADASTRO DO C".
           05  LINE 02  COLUMN 41 
               VALUE  "ONVENIO ***************".
           05  LINE 04  COLUMN 01 
               VALUE  "  CODIGO:".
           05  LINE 06  COLUMN 01 
               VALUE  "  NOMEC:".
           05  LINE 08  COLUMN 01 
               VALUE  "  PLANO:".
           05  TCODIGO
               LINE 04  COLUMN 11  PIC 9(04)
               USING  CODIGO
               HIGHLIGHT.
           05  TNOMEC
               LINE 06  COLUMN 09  PIC X(30)
               USING  NOMEC
               HIGHLIGHT.
           05  TPLANO
               LINE 08  COLUMN 10  PIC 9(02)
               USING  PLANO
               HIGHLIGHT.
           05  TTXTPLANO
               LINE 08  COLUMN 13  PIC X(50)
               USING  TXTPLANO
               HIGHLIGHT.   

       01  TELAPLANO.
           05  LINE 10  COLUMN 35 
               VALUE  "         1 - ENFERMARIA REGINAL".
           05  LINE 11  COLUMN 35 
               VALUE  "         2 - ENFERMARIA NACIONAL".
           05  LINE 12  COLUMN 35 
               VALUE  "         3 - ENFERMARIA INTERNACIONAL". 
           05  LINE 13  COLUMN 35 
               VALUE  "         4 - APTO PADRÃO REGINAL".      
           05  LINE 14  COLUMN 35 
               VALUE  "         5 - APTO PADRAO NACIONAL".
           05  LINE 15  COLUMN 35 
               VALUE  "         6 - APTO PADRAO INTERNACIONAL".
           05  LINE 16  COLUMN 35 
               VALUE  "         7 - EMERGENCIA REGIONAL".
           05  LINE 17  COLUMN 35 
               VALUE  "         8 - EMERGENCIA NACIONAL".
           05  LINE 18  COLUMN 35 
               VALUE  "         9 - EMERCENCIA INTERNACIONAL".
           05  LINE 19  COLUMN 35 
               VALUE  "        10 - PLANO GLOBAL".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
   
      ********** ABRE OU CRIA ARQUIVO*************************
       R0.
           OPEN I-O CADCONV
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCONV
                 CLOSE CADCONV
                 MOVE "*** ARQUIVO CADCONV FOI CRIADO **" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADMIGO" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
                 
      ********** FIM ABRE OU CRIA ARQUIVO*************************
      *********ZERAR CAMPOS E MOSTRAR A TELA**********************     
       R1.
           MOVE SPACES TO NOMEC
           MOVE SPACES TO TXTPLANO
           MOVE ZEROS TO PLANO
           MOVE ZEROS TO CODIGO
           DISPLAY TELACONV. 
           
      *********FIM ZERAR CAMPOS E MOSTRAR A TELA**********************
       R2.
           ACCEPT TCODIGO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           

      ***** VERIFICA SE FOI DIGITADO UM CODIGO
           IF CODIGO = 0
              MOVE "*** DIGITE O CODIGO ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R2.

       LER-CADCONV.
           READ CADCONV
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                PERFORM R4A
                DISPLAY TELACONV
                GO TO ACE-001
                
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCONV" TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.
       
       R3. 
           DISPLAY TELACONV 
           ACCEPT TNOMEC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2. 
           IF NOMEC = SPACES 
              MOVE "DIGITE A NOMEC DO CODIGO" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R3.  

       R4.   
           DISPLAY TELACONV 
           DISPLAY TELAPLANO
           ACCEPT TPLANO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3. 
           IF PLANO = 0 OR PLANO > 10
              MOVE "*** ESCOLHA UM PLANO ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R4. 
       R4A.    
           MOVE TBPLANO(PLANO) TO TXTPLANO
           DISPLAY TXTPLANO. 

           DISPLAY TELACONV.
             
                  

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R3.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCONV
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM RMensa THRU RMensa-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  
                  GO TO ALT-RW1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DO CARRO"
                                                       TO MENS
                      PERFORM RMensa THRU RMensa-FIM
                      GO TO ROT-FIM.  


      *************** GRUD *********************************
       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3.
      *          
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADCONV RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R3.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCONV
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO CARRO"   TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO ROT-FIM.



           


       ROT-FIM.
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
