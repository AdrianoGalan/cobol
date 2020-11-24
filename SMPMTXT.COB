       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMPMTXT.

      *AUTHOR. ADRIANO GALAN DA SILVA.
      ***********************************************
      * CONVERCAO DO ARQUIVO CADMED PARA TXT        *
      ***********************************************
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
           SELECT CADMEDTX ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS  IS ST-ERRO.
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
       FD CADMEDTX
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMEDTX.TXT".
       01 REGMEDTX.
                03 CRMTX           PIC 9(06).
                03 NOMETX          PIC X(30).
                03 TXTESPECTX        PIC X(15).
                03 TXTSEXOTX       PIC X(10).
                03 DNASCTX.
                   05 DIANASTX     PIC 9(02).
                   05 MESNASTX     PIC 9(02).
                   05 ANONASTX     PIC 9(04).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL      PIC 9(01) VALUE ZEROS.
       77 W-CONT     PIC 9(06) VALUE ZEROS.
       77 W-CONT2       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO    PIC X(01) VALUE SPACES.
       77 ST-ERRO    PIC X(02) VALUE "00". 
       77 W-ACT      PIC 9(02) VALUE ZEROS.
       77 MENS       PIC X(50) VALUE SPACES.
       77 LIMPA      PIC X(50) VALUE SPACES.
      
       01 TABESPECX.
          03 FILLER     PIC X(15) VALUE "CLINICA MEDICA".
          03 FILLER     PIC X(15) VALUE "UROLOGIA".
          03 FILLER     PIC X(15) VALUE "GINICOLOGISTA".
          03 FILLER     PIC X(15) VALUE "PEDIATRIA".
          03 FILLER     PIC X(15) VALUE "CARDIOLOGISTA ".
          03 FILLER     PIC X(15) VALUE "OUTRO".
       01 TABESPEC REDEFINES TABESPECX.
          03 TBESPEC   PIC X(15) OCCURS 6 TIMES.

      *
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELAMTXT.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                *** CONVERSAO DE ARQUIVO".
           05  LINE 02  COLUMN 41 
               VALUE  " DE MEDICO PARA TXT ***".
           05  LINE 12  COLUMN 01 
               VALUE  "                         CONFIRMA CONVER".
           05  LINE 12  COLUMN 41 
               VALUE  "SAO (S ou N ) :".
           05  TW-OPCAO
               LINE 12  COLUMN 57  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO. 
      *
       INC-001.
                MOVE SPACES TO NOME SEXO.
                MOVE ZEROS  TO CRM ESPECIALIDADE DNASC.
                DISPLAY TELAMTXT.
       INC-OPC.
                ACCEPT TW-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO ROT-FIM.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO INC-OPC.
      *
       INC-OP0.
           OPEN INPUT CADMED
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO CEP NAO EXISTE *" TO MENS
                      PERFORM RMensa THRU RMensa-FIM
                      GO TO ROT-FIM
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CEP" TO MENS
                      PERFORM RMensa THRU RMensa-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
      *
           OPEN OUTPUT CADMEDTX
           IF ST-ERRO NOT = "00"
                      MOVE "ERRO ABERTURA DO ARQUIVO CADMEDTX" TO MENS
                      PERFORM RMensa THRU RMensa-FIM
                      GO TO ROT-FIM.
      *
       LER-MED01.
                READ CADMED NEXT
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "10"
                      MOVE "*** FIM DO CADMED ***" TO MENS
                      PERFORM RMensa THRU RMensa-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA LEITURA CADMED"   TO MENS
                      PERFORM RMensa THRU RMensa-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
           MOVE CRM TO CRMTX
           MOVE NOME TO NOMETX
           MOVE DNASC TO DNASCTX
           IF SEXO = "M"
                MOVE "MASCULINO" TO TXTSEXOTX
           ELSE
                MOVE "FEMININO" TO TXTSEXOTX.
           
           MOVE  TBESPEC(ESPECIALIDADE) TO TXTESPECTX.

       INC-WR1.
                WRITE REGMEDTX
                IF ST-ERRO = "00" OR "02"
                      DISPLAY (20, 20) CRM
                      GO TO LER-MED01
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO CADMEDTX"
                                                       TO MENS
                      PERFORM RMensa THRU RMensa-FIM
                      GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                CLOSE CADMED CADMEDTX.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
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
