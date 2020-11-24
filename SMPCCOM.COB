       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMPCCOM.
      *AUTHOR. ADRIANO GALAN DA SILVA.
      **************************************************
      * CONSULTA DE CONVENIO*
      **************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOMEC WITH DUPLICATES
                    ALTERNATE RECORD KEY IS PLANO WITH DUPLICATES.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *
       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT". 
       01 REGCONV.
          03 CODIGO        PIC 9(04).
          03 NOMEC         PIC X(30).
          03 PLANO         PIC 9(02).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-SEL       PIC 9(01) VALUE ZEROS.
       01 W-CONT      PIC 9(04) VALUE ZEROS.
       77 W-CONT2     PIC 9(06) VALUE ZEROS.
       01 W-OPCAO     PIC X(01) VALUE SPACES.
       01 ST-ERRO     PIC X(02) VALUE "00".
       01 W-ACT       PIC 9(02) VALUE ZEROS.
       01 MENS        PIC X(50) VALUE SPACES.
       01 LIMPA       PIC X(55) VALUE SPACES.
       01 SOLIC       PIC X(20) VALUE SPACES.
       01 CONLIN      PIC 9(03) VALUE 001.
       01 CEPENTR     PIC 9(09) VALUE ZEROS.
       01 LOGRENTR    PIC X(30) VALUE SPACES.
       01 IND         PIC 9(05) VALUE ZEROS.

       01 TABCEP.
          03 TBCEP    PIC 9(08) OCCURS 1000 TIMES.
       01 NUMREG      PIC 9(04) VALUE ZEROS.
       
       01 TXTPLANO      PIC X(50) VALUE SPACES.
      
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
       01  TELACCON.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                       ** CONSULTA CONVE".
           05  LINE 02  COLUMN 41 
               VALUE  "NIO **".
           05  LINE 04  COLUMN 01 
               VALUE  " CODIGO:".
           05  LINE 06  COLUMN 01 
               VALUE  " NOME:".
           05  LINE 08  COLUMN 01 
               VALUE  " PLANO:".
           05  LINE 12  COLUMN 01 
               VALUE  " OPCAO   (E=ENCERRA, P=PROXIMO, A=ANTERI".
           05  LINE 12  COLUMN 41 
               VALUE  "OR)".
           05  TCODIGO
               LINE 04  COLUMN 10  PIC 9(04)
               USING  CODIGO
               HIGHLIGHT.
           05  TNOME
               LINE 06  COLUMN 08  PIC X(30)
               USING  NOMEC
               HIGHLIGHT.
           05  TTXTPLANO
               LINE 08  COLUMN 09  PIC X(50)
               USING  TXTPLANO
               HIGHLIGHT.
           05  TW-OPCAO
               LINE 12  COLUMN 08  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT  CADCONV
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADCONV NAO EXISTE" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO CADCONV"  TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim2.
           MOVE 1 TO IND
           MOVE ZEROS TO CODIGO NUMREG.
           START CADCONV KEY IS NOT LESS CODIGO INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim.

       LER-CONV.           
           READ CADCONV NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE IND TO NUMREG
                 ADD -1 TO NUMREG
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO INC-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCONV"  TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim
           ELSE
              MOVE CODIGO TO TBCEP(IND)
              ADD 1 TO IND              
              IF IND > 1000
                 MOVE "*** TABELA ESTOURADA ***" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO INC-001
              ELSE
                 GO TO LER-CONV.

      *
       INC-001.
           MOVE SPACES TO NOMEC
           MOVE ZEROS TO CODIGO PLANO
           DISPLAY TELACCON. 
       INC-001A.
           ACCEPT TCODIGO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADCONV
                      GO TO RotinaFim.
           IF W-ACT > 02
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM RMensa THRU RMensa-FIM   
                      GO TO INC-001A.
           MOVE 1 TO IND.

       R111.
           IF TBCEP(IND) < CODIGO
              ADD 1 TO IND
              IF IND >1000
                 MOVE "*** MEDICO NAO ENCONTRADO ***" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO INC-001A
              ELSE
                 GO TO R111.
       R112.
           MOVE TBCEP(IND) TO CODIGO.

       INC-RD2.
           READ CADCONV 
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "*** REGISTRO NAO ENCONTRADO ***" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO INC-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCONV"  TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim
           ELSE
              NEXT SENTENCE.

                       

       ROT-MONTAR.
            
           MOVE TBPLANO(PLANO) TO TXTPLANO
  
              

           DISPLAY TELACCON.

       ROT-SOL.
           ACCEPT  TW-OPCAO  
           IF W-OPCAO = "E"
                  GO TO RotinaFim 
           ELSE
             IF W-OPCAO = "P"
                 IF IND < NUMREG
                   ADD 1 TO IND
                   GO TO R112
                 ELSE
                   MOVE "*** ULTIMO REGISTRO ***" TO MENS
                           PERFORM RMensa THRU RMensa-FIM
                   GO TO ROT-SOL
             ELSE
                IF W-OPCAO = "A"
                    IF IND > 1
                       ADD -1 TO IND
                       GO TO R112
                    ELSE
                       MOVE "*** PRIMEIRO REGISTRO ***" TO MENS
                           PERFORM RMensa THRU RMensa-FIM
                       GO TO ROT-SOL
                ELSE
                
                     MOVE "*** OPCAO NAO DISPONIVEL ***" TO MENS
                     PERFORM RMensa THRU RMensa-FIM
                     GO TO ROT-SOL.

      

      ****** ROTINA FIM ******************
       RotinaFim.

           CLOSE CADCONV.
       RotinaFim2.
           EXIT PROGRAM.
       RotinaFim3.
           STOP RUN.
      

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