       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENUS.

      *AUTHOR. ADRIANO GALAN DA SILVA.
      **************************************
      * MENU TRABALHO FINAL PROGRAMACAO COBOL  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-CONT       PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC 9(02) VALUE ZEROS.
       77 W-CONT2       PIC 9(06) VALUE ZEROS.
       01 ST-ERRO      PIC X(02) VALUE "00".
       01 MENS         PIC X(50) VALUE SPACES.
       01 LIMPA        PIC X(50) VALUE SPACES.
       01 W-PROGR      PIC X(07) VALUE SPACES.

      *-----------------------------------------------------------------
       01 TAB-PROGR-R.
           03 FILLER PIC X(07) VALUE "SMP001".
           03 FILLER PIC X(07) VALUE "SMP002".
           03 FILLER PIC X(07) VALUE "SMP003".
           03 FILLER PIC X(07) VALUE "SMP004".
           03 FILLER PIC X(07) VALUE "SMP005".
           03 FILLER PIC X(07) VALUE "SMP006".
           03 FILLER PIC X(07) VALUE "SMP040A".
           03 FILLER PIC X(07) VALUE "CONSMED".
           03 FILLER PIC X(07) VALUE "SMP009".

      *
       01 TAB-PROGR   REDEFINES TAB-PROGR-R.
          03 TAB-PROG  PIC X(07) OCCURS 10 TIMES.
      **********************
      *
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.
       01  SMTMEN.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                             SISTEMA DE".
           05  LINE 02  COLUMN 41 
               VALUE  "CONSULTAS".
           05  LINE 05  COLUMN 01 
               VALUE  "                          1 - CADASTRO D".
           05  LINE 05  COLUMN 41 
               VALUE  "E MEDICOS".
           05  LINE 06  COLUMN 01 
               VALUE  "                          2 - CADASTRO D".
           05  LINE 06  COLUMN 41 
               VALUE  "E DOENCAS".
           05  LINE 07  COLUMN 01 
               VALUE  "                          3 - CADASTRO D".
           05  LINE 07  COLUMN 41 
               VALUE  "E CONVENIO".
           05  LINE 08  COLUMN 01 
               VALUE  "                          4 - CADASTRO D".
           05  LINE 08  COLUMN 41 
               VALUE  "E PACIENTE".
           05  LINE 09  COLUMN 01 
               VALUE  "                          5 - CADASTRO D".
           05  LINE 09  COLUMN 41 
               VALUE  "E CEP".
           05  LINE 10  COLUMN 01 
               VALUE  "                          6 - CADASTRO D".
           05  LINE 10  COLUMN 41 
               VALUE  "E CONSULTAS".
           05  LINE 11  COLUMN 01 
               VALUE  "                          7 - CONSULTA D".
           05  LINE 11  COLUMN 41 
               VALUE  "E CEP".
           05  LINE 12  COLUMN 01 
               VALUE  "                          8 - CONSULTA D".
           05  LINE 12  COLUMN 41 
               VALUE  "E MEDICO".
           05  LINE 13  COLUMN 01 
               VALUE  "                          9 - GERAR RELA".
           05  LINE 13  COLUMN 41 
               VALUE  "TORIO DO CADCEP".
           05  LINE 14  COLUMN 01 
               VALUE  "                         10 - RELATORIO ".
           05  LINE 14  COLUMN 41 
               VALUE  "DE MEDICO".
           05  LINE 20  COLUMN 01 
               VALUE  "                             OPCAO :".
           05  LINE 20  COLUMN 41 
               VALUE  " ( 00 - ENCERRA )".
           05  T-OPCAO
               LINE 20  COLUMN 38  PIC 9(02)
               USING  W-OPCAO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.

      ****************************
      * ROTINA DE SELECAO NIVEL1 *
      ****************************
      *
       NIVEL1.
           DISPLAY SMTMEN.
           
           ACCEPT T-OPCAO.
           IF W-OPCAO = 00
                      GO TO Rfim1.
           IF W-OPCAO > 10 
                 MOVE "*** FUNCAO NAO DISPONIVEL *** " TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO NIVEL1.
           MOVE TAB-PROG(W-OPCAO) TO W-PROGR
           IF W-PROGR = SPACES
                 MOVE "*** FUNCAO NAO IMPLEMENTADA *** " TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO NIVEL1.
      *
       Rcall.
           CALL W-PROGR ON OVERFLOW
              MOVE "* ERRO NA CHAMADA DO PROGRAMA SELECIONADO *" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO Rcall1.
           CANCEL W-PROGR.
       Rcall1.
           GO TO NIVEL1.

      
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
           
      *****************Rotina Fim ***************
       Rfim.
           EXIT PROGRAM.
       Rfim1.
           DISPLAY (01, 01) ERASE.
           STOP RUN.