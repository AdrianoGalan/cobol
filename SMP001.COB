       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP001.
      *AUTHOR. ADRIANO GALAN DA SILVA.
      **************************************
      * MANUTENCAO DO CADASTRO DE MEDICO   *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-CONT2       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES. 
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 W-SEL         PIC 9(01) VALUE ZEROS.

       01 TXTESPEC      PIC X(15) VALUE SPACES.
       01 IND           PIC 9(02) VALUE ZEROS.
       
     
       01 TABSEXOX.
          03 FILLER     PIC X(11) VALUE "MMASCULINO".
          03 FILLER     PIC X(11) VALUE "FFEMENINO".
       01 TABESP REDEFINES TABSEXOX.
          03 TBSEXO   PIC X(11) OCCURS 2 TIMES.
       01 TXTSEXO.
          03 TXTSEXOCRM PIC X(01) VALUE SPACES.
          03 TXTSEXOTEXTO PIC X(10) VALUE SPACES.      



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
       01  TELAMED.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                             CADASTRO ME".
           05  LINE 02  COLUMN 41 
               VALUE  "EDICO".
           05  LINE 04  COLUMN 01 
               VALUE  "   CRM:".
           05  LINE 07  COLUMN 01 
               VALUE  "   NOME:".
           05  LINE 10  COLUMN 01 
               VALUE  "   ESPECIALIDADE:".
           05  LINE 13  COLUMN 01 
               VALUE  "   SEXO:".
           05  LINE 16  COLUMN 01 
               VALUE  "   DATA NASCIMENTO:".
           05  TCRM
               LINE 04  COLUMN 09  PIC 9(09)
               USING  CRM
               HIGHLIGHT.
           05  TNOME
               LINE 07  COLUMN 10  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TESPECIALIDADE
               LINE 10  COLUMN 19  PIC X(02)
               USING  ESPECIALIDADE
               HIGHLIGHT.
           05  TTXTESPEC
               LINE 10  COLUMN 22  PIC X(20)
               USING  TXTESPEC
               HIGHLIGHT.
           05  TSEXO
               LINE 13  COLUMN 10  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TTXTSEXO
               LINE 13  COLUMN 12  PIC X(10)
               USING  TXTSEXO
               HIGHLIGHT.
           05  TDNASC
               LINE 16  COLUMN 21  PIC X(08)
               USING  DNASC
               HIGHLIGHT.
          
       01  TELAESP.
           05  LINE 10  COLUMN 41 
               VALUE  "         1 - CLINICA MEDICA".
           05  LINE 11  COLUMN 41 
               VALUE  "         2 - UROLOGIA".
           05  LINE 12  COLUMN 41 
               VALUE  "         3 - GINICOLOGISTA". 
           05  LINE 13  COLUMN 41 
               VALUE  "         4 - PEDIATRIA".      
           05  LINE 14  COLUMN 41 
               VALUE  "         5 - CARDIOLOGISTA".
           05  LINE 15  COLUMN 41 
               VALUE  "         6 - OUTRO".

       01  TELASEXO.
           05  LINE 10  COLUMN 41 
               VALUE  "         M - MASCULINO".
           05  LINE 11  COLUMN 41 
               VALUE  "         F - FEMENINO".      
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
   
      ********** ABRE OU CRIA ARQUIVO*************************
       R0.
           OPEN I-O CADMED
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADMED
                 CLOSE CADMED
                 MOVE "*** ARQUIVO CADMED FOI CRIADO **" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADMIGO" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim
           ELSE
                 NEXT SENTENCE.
                 
      ********** FIM ABRE OU CRIA ARQUIVO*************************
      *********ZERAR CAMPOS E MOSTRAR A TELA**********************     
       R1.
           MOVE SPACES TO NOME SEXO  TXTSEXO TXTESPEC
           MOVE ZEROS TO CRM ESPECIALIDADE DNASC 
           DISPLAY TELAMED. 
           
      *********FIM ZERAR CAMPOS E MOSTRAR A TELA**********************
       R2.
           ACCEPT TCRM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO RotinaFim.
           

      ***** VERIFICA SE FOI DIGITADO UMA PLACA
           IF CRM = 0
              MOVE "*** DIGITE O CRM ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R2.

       LER-CADMED.
           READ CADMED
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
               
                PERFORM R5A
                DISPLAY TELAMED
                GO TO Ace01
                
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADMED" TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO RotinaFim
           ELSE
                NEXT SENTENCE.
       
       R3. 
           DISPLAY TELAMED 
           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2. 
           IF NOME = SPACES 
              MOVE "DIGITE O NOME DO MEDICO" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R3.  

       R4.   
           DISPLAY TELAMED 
           DISPLAY TELAESP
           ACCEPT TESPECIALIDADE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3. 
           IF ESPECIALIDADE = 0 OR ESPECIALIDADE > 6
              MOVE "*** ESCOLHA UMA ESPECIALIDADE ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R4. 
       R4A.    
           MOVE TBESPEC(ESPECIALIDADE) TO TXTESPEC
           DISPLAY TXTESPEC. 

           DISPLAY TELAMED.
   
       

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
           IF TXTSEXOCRM NOT = SEXO
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
                DISPLAY TELAMED.

       R6.    
           DISPLAY TELAMED    
           ACCEPT TDNASC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5. 
           IF DNASC = 0 
              MOVE "*** DIGITE A DATA DE NASCIMENTO ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R6.
           IF MESNAS < 1 OR MESNAS > 12
              MOVE "*** MES INVALIDO ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R6.    
           IF DIANAS < 1 OR DIANAS > 31
              MOVE "*** DIA INVALIDO ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R6.   
           IF ANONAS < 1900 OR ANONAS > 2005
              MOVE "*** ANO INVALIDO ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R6. 

                     
                  

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (24, 40) "DADOS OK (S/N) : ".
                ACCEPT (24, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R6.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGMED
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
                DELETE CADMED RECORD
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
                IF W-ACT = 01 GO TO R6.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO AltOpc.
       AltRw1.
                REWRITE REGMED
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO CARRO"   TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO RotinaFim.

       
  

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
       RotinaFim.
           CLOSE CADMED.
           EXIT PROGRAM.
         
    