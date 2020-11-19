       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP002.
      *AUTHOR. ADRIANO GALAN DA SILVA.
      **************************************
      * MANUTENCAO DO CADASTRO DE CID  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCID ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODCID
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS DENOMINACAO WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCID
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCID.DAT". 
       01 REGCID.
          03 CODCID        PIC 9(04).
          03 DENOMINACAO   PIC X(30).
        
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



      *-----------------------------------------------------------------
       SCREEN SECTION.  
       01  TELACID.
           05  BLANK SCREEN. 
           05  LINE 02  COLUMN 01 
               VALUE  "                         ***** CADASTRO".
           05  LINE 02  COLUMN 41 
               VALUE  "CID *****".
           05  LINE 04  COLUMN 01 
               VALUE  "   CID:".
           05  LINE 06  COLUMN 01 
               VALUE  "   DENOMINACAO:".
           05  TCID
               LINE 04  COLUMN 09  PIC 9(04)
               USING  CODCID
               HIGHLIGHT.
           05  TDENOMINACAO
               LINE 06  COLUMN 17  PIC X(30)
               USING  DENOMINACAO
               HIGHLIGHT.      
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
   
      ********** ABRE OU CRIA ARQUIVO*************************
       R0.
           OPEN I-O CADCID
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCID
                 CLOSE CADCID
                 MOVE "*** ARQUIVO CADCID FOI CRIADO **" TO MENS
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
           MOVE SPACES TO DENOMINACAO
           MOVE ZEROS TO CODCID
           DISPLAY TELACID. 
           
      *********FIM ZERAR CAMPOS E MOSTRAR A TELA**********************
       R2.
           ACCEPT TCID
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           

      ***** VERIFICA SE FOI DIGITADO UM CID
           IF CODCID = 0
              MOVE "*** DIGITE O CID ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R2.

       LER-CADCID.
           READ CADCID
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                DISPLAY TELACID
                GO TO ACE-001
                
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCID" TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.
       
       R3. 
           DISPLAY TELACID 
           ACCEPT TDENOMINACAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2. 
           IF DENOMINACAO = SPACES 
              MOVE "DIGITE A DENOMINACAO DO CID" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R3.  

             
                  

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
                WRITE REGCID
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
                DELETE CADCID RECORD
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
                REWRITE REGCID
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO CARRO"   TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO ROT-FIM.



           


       ROT-FIM.
           CLOSE CADCID.
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
