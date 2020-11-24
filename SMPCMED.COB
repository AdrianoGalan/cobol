       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMPCMED.
      *AUTHOR. ADRIANO GALAN DA SILVA.
      **************************************************
      * CONSULTA DE MEDICO*
      **************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       SELECT CADMED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CRM
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *
       FD CADMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMED.DAT". 
       01 REGMED.
          03 CRM           PIC 9(06).
          03 NOME          PIC X(30).
          03 ESPECIALIDADE PIC 9(02).
          03 SEXO          PIC X(01).
          03 DNASC.
             05 DIANAS     PIC 9(02).
             05 MESNAS     PIC 9(02).
             05 ANONAS     PIC 9(04).
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

       01 TXTESPEC    PIC X(15) VALUE SPACES.  

       
       01 TXTSEXO     PIC X(10) VALUE SPACES. 
         

       01 TABESPECX.
          03 FILLER     PIC X(15) VALUE "CLINICA MEDICA".
          03 FILLER     PIC X(15) VALUE "UROLOGIA".
          03 FILLER     PIC X(15) VALUE "GINICOLOGISTA".
          03 FILLER     PIC X(15) VALUE "PEDIATRIA".
          03 FILLER     PIC X(15) VALUE "CARDIOLOGISTA ".
          03 FILLER     PIC X(15) VALUE "OUTRO".
       01 TABESPEC REDEFINES TABESPECX.
          03 TBESPEC   PIC X(15) OCCURS 6 TIMES.   

      *-----------------------------------------------------------------
     
       SCREEN SECTION.
       01  TELACONMED.
           05  BLANK SCREEN. 
           05  LINE 02  COLUMN 01 
               VALUE  "                           ** CONSULTA D".
           05  LINE 02  COLUMN 41 
               VALUE  "E MEDICO **".
           05  LINE 04  COLUMN 01 
               VALUE  " CRM:".
           05  LINE 06  COLUMN 01 
               VALUE  " NOME:".
           05  LINE 08  COLUMN 01 
               VALUE  " ESPECIALIDADE:".
           05  LINE 10  COLUMN 01 
               VALUE  " SEXO:".
           05  LINE 12  COLUMN 01 
               VALUE  " DATA NASC:".
           05  LINE 15  COLUMN 01 
               VALUE  " OPCAO   (E=ENCERRA, P=PROXIMO, A=ANTERI".
           05  LINE 15  COLUMN 41 
               VALUE  "OR)".
           05  TCRM
               LINE 04  COLUMN 07  PIC 9(09)
               USING  CRM
               HIGHLIGHT.
           05  TNOME
               LINE 06  COLUMN 08  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TTXTESPEC
               LINE 08  COLUMN 17  PIC X(20)
               USING  TXTESPEC
               HIGHLIGHT.
           05  TTXTSEXO
               LINE 10  COLUMN 08  PIC X(10)
               USING  TXTSEXO
               HIGHLIGHT.
           05  TDNASC
               LINE 12  COLUMN 13  PIC 99.99.9999
               USING  DNASC
               HIGHLIGHT.
           05  TW-OPCAO
               LINE 15  COLUMN 08  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT  CADMED
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADMED NAO EXISTE" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO CADMED"  TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim2.
           MOVE 1 TO IND
           MOVE ZEROS TO CRM NUMREG.
           START CADMED KEY IS NOT LESS CRM INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim.

       LER-MEDICO.           
           READ CADMED NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE IND TO NUMREG
                 ADD -1 TO NUMREG
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO INC-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADMED"  TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim
           ELSE
              MOVE CRM TO TBCEP(IND)
              ADD 1 TO IND              
              IF IND > 1000
                 MOVE "*** TABELA ESTOURADA ***" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO INC-001
              ELSE
                 GO TO LER-MEDICO.

      *
       INC-001.
           MOVE SPACES TO NOME SEXO  TXTSEXO TXTESPEC
           MOVE ZEROS TO CRM ESPECIALIDADE DNASC 
           DISPLAY TELACONMED. 
       INC-001A.
           ACCEPT TCRM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADMED
                      GO TO RotinaFim.
           IF W-ACT > 02
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM RMensa THRU RMensa-FIM   
                      GO TO INC-001A.
           MOVE 1 TO IND.

       R111.
           IF TBCEP(IND) < CRM
              ADD 1 TO IND
              IF IND >1000
                 MOVE "*** MEDICO NAO ENCONTRADO ***" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO INC-001A
              ELSE
                 GO TO R111.
       R112.
           MOVE TBCEP(IND) TO CRM.

       INC-RD2.
           READ CADMED 
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "*** REGISTRO NAO ENCONTRADO ***" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO INC-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADMED"  TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim
           ELSE
              NEXT SENTENCE.

                       

       ROT-MONTAR.
            
           MOVE TBESPEC(ESPECIALIDADE) TO TXTESPEC
  
           IF SEXO = "M"
                MOVE "MASCULINO" TO TXTSEXO
           ELSE
               MOVE "FEMININO" TO TXTSEXO.    

           DISPLAY TELACONMED.

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

           CLOSE CADMED.
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