       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP006.
      *AUTHOR. ADRIANO GALAN DA SILVA.
      **************************************
      * MANUTENCAO DO CADASTRO DE CONSULTA   *
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

           SELECT CADCONSU ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS KEYPRINCIPAL
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CODMEDICO  
                                       WITH DUPLICATES.
          
           SELECT CADMED ASSIGN TO DISK
                        ORGANIZATION IS INDEXED
                        ACCESS MODE  IS DYNAMIC
                        RECORD KEY   IS CRM
                        FILE STATUS  IS ST-ERRO
                        ALTERNATE RECORD KEY IS NOMEM
                                           WITH DUPLICATES.

           SELECT CADCID ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODCID
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS DENOMINACAO 
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
          03 CONVENIO      PIC 9(04). 
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
       FD CADCONSU
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONSU.DAT". 
       01 REGCONSU.
          03 KEYPRINCIPAL.
             05 CPFC          PIC 9(11).
             05 DATACON1.       
                07 DIACON1     PIC 9(02).
                07 MESCON1     PIC 9(02).
                07 ANOCON1     PIC 9(04).
          03 CODMEDICO        PIC 9(06).
          03 DOCONVENIO       PIC 9(04).
          03 CODCID1          PIC 9(04).
          03 DESCRICAO1       PIC X(60).
          03 DESCRICAO2       PIC X(60).
      *--------------------------------------------------------
       FD CADMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMED.DAT". 
       01 REGMED.
          03 CRM           PIC 9(06).
          03 NOMEM         PIC X(30).
          03 ESPECIALIDADE PIC 9(02).
          03 SEXOMEDICO    PIC X(01).
          03 DNASCM.
             05 DIANAS     PIC 9(02).
             05 MESNAS     PIC 9(02).
             05 ANONAS     PIC 9(04).
      *------------------------------------------------------
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

       01 TXTESPEC      PIC X(15) VALUE SPACES.
       01 TXTPLANO      PIC X(50) VALUE SPACES.
       01 TXTUF         PIC X(20) VALUE SPACES.
       01 IND           PIC 9(02) VALUE ZEROS.
       01 FLAG          PIC 9(01) VALUE ZEROS.  
       01 TXTSEXOMEDICO PIC X(10) VALUE SPACES.
       01 ESPEC         PIC 9(02) VALUE ZEROS.
      
  
       
     
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
       01  TELACON2.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                      *** CADASTRO CONSU".
           05  LINE 02  COLUMN 41 
               VALUE  "LTA ***".
           05  LINE 04  COLUMN 01 
               VALUE  "PACIENTE(CPF):".
           05  LINE 04  COLUMN 41 
               VALUE  " DATA CONSULTA:".
           05  LINE 06  COLUMN 01 
               VALUE  "NOME:".
           05  LINE 07  COLUMN 01 
               VALUE  "DATA NASCIMENTO:             SEXO:  -".
           05  LINE 07  COLUMN 41 
               VALUE  "       GENERO:  -".
           05  LINE 09  COLUMN 01 
               VALUE  "CONVENIO:      -                      PL".
           05  LINE 09  COLUMN 41 
               VALUE  "ANO:    -".
           05  LINE 11  COLUMN 01 
               VALUE  "MEDICO".
           05  LINE 12  COLUMN 01 
               VALUE  "CRM:          NOME:".
           05  LINE 12  COLUMN 41 
               VALUE  "            SEXO  -".
           05  LINE 13  COLUMN 01 
               VALUE  "ESPECIALIDADE:   -".
           05  LINE 15  COLUMN 01 
               VALUE  "CONSULTA:".
           05  LINE 16  COLUMN 01 
               VALUE  "CID:       DENOMINACAO:".
           05  LINE 17  COLUMN 01 
               VALUE  "DESCRICAO 1:".
           05  LINE 18  COLUMN 01 
               VALUE  "DESCRICAO 2:".
           05  TCPF
               LINE 04  COLUMN 16  PIC 999.999.999.99
               USING  CPF
               HIGHLIGHT.
           05  TDATACON
               LINE 04  COLUMN 57  PIC 99.99.9999
               USING  DATACON1
               HIGHLIGHT.
           05  TNOME
               LINE 06  COLUMN 07  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TDNASC
               LINE 07  COLUMN 18  PIC 99.99.9999
               USING  DNASC
               HIGHLIGHT.
           05  TSEXO
               LINE 07  COLUMN 36  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TTXTSEXO
               LINE 07  COLUMN 38  PIC X(09)
               USING  TXTSEXO
               HIGHLIGHT.
           05  TGENERO
               LINE 07  COLUMN 56  PIC X(01)
               USING  GENERO
               HIGHLIGHT.
           05  TTXTGENERO
               LINE 07  COLUMN 58  PIC X(12)
               USING  TXTGENERO
               HIGHLIGHT.
           05  TCODIGO
               LINE 09  COLUMN 11  PIC 9999
               USING  CODIGO
               HIGHLIGHT.
           05  TNOMEC
               LINE 09  COLUMN 18  PIC X(20)
               USING  NOMEC
               HIGHLIGHT.
           05  TPLANO
               LINE 09  COLUMN 46  PIC 99
               USING  PLANO
               HIGHLIGHT.
           05  TTXTPLANO
               LINE 09  COLUMN 51  PIC X(20)
               USING  TXTPLANO
               HIGHLIGHT.
           05  TCRM
               LINE 12  COLUMN 06  PIC 999.999
               USING  CRM
               HIGHLIGHT.
           05  TNOMEM
               LINE 12  COLUMN 21  PIC X(30)
               USING  NOMEM.
           05  TSEXOMEDICO
               LINE 12  COLUMN 58  PIC X(01)
               USING  SEXOMEDICO
               HIGHLIGHT.
           05  TTXTSECOMEDICO
               LINE 12  COLUMN 60  PIC X(10)
               USING  TXTSEXOMEDICO
               HIGHLIGHT.
           05  TESPEC
               LINE 13  COLUMN 16  PIC 9(02)
               USING  ESPEC
               HIGHLIGHT.
           05  TTXTESPEC
               LINE 13  COLUMN 19  PIC X(20)
               USING  TXTESPEC
               HIGHLIGHT.
           05  TCODCID
               LINE 16  COLUMN 06  PIC 9(04)
               USING  CODCID
               HIGHLIGHT.
           05  TDENOMINACAO
               LINE 16  COLUMN 25  PIC X(30)
               USING  DENOMINACAO
               HIGHLIGHT.
           05  TDESCRICAO1
               LINE 17  COLUMN 14  PIC X(60)
               USING  DESCRICAO1
               HIGHLIGHT.
           05  TDESCRICAO2
               LINE 18  COLUMN 14  PIC X(60)
               USING  DESCRICAO2
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
           
           
           OPEN I-O CADCONSU
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCONSU
                 CLOSE CADCONSU
                 MOVE "*** ARQUIVO CADCONSU FOI CRIADO **" TO MENS
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
           OPEN INPUT CADPACI
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CADPACI NAO ENCONTRADO **" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CADPACI " TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim
           ELSE
                 NEXT SENTENCE.           

       R0C.
           OPEN INPUT CADMED
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CADMED NAO ENCONTRADO **" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CADMED " TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim
           ELSE
                 NEXT SENTENCE.     

       R0D.
           OPEN INPUT CADCID
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CADCID NAO ENCONTRADO **" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CADCID " TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim
           ELSE
                 NEXT SENTENCE.
         

      ********** FIM ABRE OU CRIA ARQUIVO*************************
      *********ZERAR CAMPOS E MOSTRAR A TELA**********************     
       R1.
           MOVE SPACES TO NOME SEXO EMAIL TXTSEXO GENERO TXTGENERO
           MOVE SPACES TO TXTPLANO NOMEC  DESCRICAO1 DESCRICAO2 
           MOVE SPACES TO TXTESPEC TXTSEXOMEDICO SEXOMEDICO NOMEM
           MOVE SPACES TO DENOMINACAO
           MOVE ZEROS TO CPF PLANO DNASC ESPEC CODCID CRM CODIGO
           MOVE ZEROS TO ESPECIALIDADE DATACON1 CPFC CODCID1
           MOVE ZEROS TO CODMEDICO
           DISPLAY TELACON2. 
           
      *********FIM ZERAR CAMPOS E MOSTRAR A TELA**********************
       R2.
           ACCEPT TCPF
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO RotinaFim.
             
      ****** VERIFICA SE FOI DIGITADO UMA CPF
           IF CPF = 0
              MOVE "*** DIGITE O CPF ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R2.  
       
       LER-CADPACI.

           READ CADPACI
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                MOVE CPF TO CPFC
                PERFORM LER-CADPACIA
                PERFORM LER-CADPACIB
                PERFORM LER-CADPACIC
                DISPLAY TELACON2
                GO TO R3
                
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADPACI" TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO RotinaFim
           ELSE
                 MOVE "PACIENTE NAO CADASTRADO" TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO R1.
       LER-CADPACIA.    
           MOVE TBSEXO(IND) TO TXTSEXO
           IF TXTSEXOCPF NOT = SEXO
              ADD 1 TO IND
              IF IND < 3
                 GO TO LER-CADPACIA
              ELSE
                 MOVE "*** SEXO INVALIDO***" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO  RotinaFim
           ELSE
                MOVE ZEROS TO IND
                MOVE TXTSEXOTEXTO TO TXTSEXO
                DISPLAY TTXTSEXO.   
                DISPLAY TELACON2.

       LER-CADPACIB.
           MOVE TBGENERO(IND) TO TXTGENERO
           IF TXTGENERO1 NOT = GENERO
              ADD 1 TO IND
              IF IND < 10
                 GO TO LER-CADPACIB
              ELSE
                 MOVE "*** TIPO GENERO INCORRETO***" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO RotinaFim
           ELSE
               MOVE ZEROS TO IND
               DISPLAY TTXTGENERO.
               DISPLAY TELACON2.         
       LER-CADPACIC.
           MOVE CONVENIO TO CODIGO.
           READ CADCONV
           MOVE TBPLANO(PLANO) TO TXTPLANO
           DISPLAY TXTPLANO 
           
           IF ST-ERRO NOT = "00"
                IF ST-ERRO = "23"
                    MOVE " ** CONVENIO NÃO CADASTRADO **" TO MENS
                    PERFORM RMensa THRU RMensa-FIM
                    GO TO RotinaFim
                ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADCONV" TO MENS
                  PERFORM RMensa THRU RMensa-FIM
                  GO TO RotinaFim
           ELSE
                DISPLAY TELACON2.
       R3.
           ACCEPT TDATACON
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1.
           IF DATACON1 = 0 
              MOVE "*** DIGITE A DATA DA CONSULTA ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R3.
           IF MESCON1 < 1 OR MESCON1 > 12
              MOVE "*** MES INVALIDO ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R3.    
           IF DIACON1 < 1 OR DIACON1 > 31
              MOVE "*** DIA INVALIDO ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R3.   
           IF ANOCON1 < 2020 OR ANOCON1 > 2030
              MOVE "*** ANO INVALIDO ***" TO MENS
              PERFORM RMensa THRU RMensa-FIM
              GO TO R3. 

       LER-CONSULTA.

           READ CADCONSU
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
             
                 

                MOVE CODMEDICO TO CRM
                MOVE 1 TO FLAG
                PERFORM LER-MEDICO

                MOVE CODCID1 TO CODCID 
                MOVE 1 TO FLAG            
                PERFORM LER-CID

                DISPLAY TELACON2
                GO TO Ace01
                
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADPACI" TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO RotinaFim
           ELSE
                NEXT SENTENCE.
       R4.
           ACCEPT TCRM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3. 
           IF CRM < 1
              MOVE "DIGITE UM CRM" TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO R4
           ELSE 
               MOVE CRM TO CODMEDICO.
               

       LER-MEDICO.
           READ CADMED
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"

                PERFORM LER-MEDICOA   
                PERFORM LER-MEDICOB             
                DISPLAY TELACON2 
                IF FLAG = ZERO 
                    GO TO R5
                ELSE
                   MOVE ZERO TO FLAG
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADMED" TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO RotinaFim
           ELSE
                MOVE "MEDICO NAO CADASTRADO" TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO R4.        
       LER-MEDICOA.    
           MOVE TBSEXO(IND) TO TXTSEXO
           IF TXTSEXOCPF NOT = SEXOMEDICO
              ADD 1 TO IND
              IF IND < 3
                 GO TO LER-MEDICOA
              ELSE
                 MOVE "*** SEXO INVALIDO***" TO MENS
                 PERFORM RMensa THRU RMensa-FIM
                 GO TO  RotinaFim
           ELSE
                MOVE ZEROS TO IND
                MOVE TXTSEXOTEXTO TO TXTSEXOMEDICO
                DISPLAY TXTSEXOMEDICO.   
                DISPLAY TELACON2.

       LER-MEDICOB. 
           MOVE TBESPEC(ESPECIALIDADE) TO TXTESPEC
           DISPLAY TXTESPEC.
           

       R5.
           ACCEPT TCODCID
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4. 
           IF CODCID < 1
              MOVE "DIGITE UM CID" TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO R5
           ELSE 
               MOVE CODCID TO CODCID1.

           
       LER-CID.
           READ CADCID
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
 
                         
                DISPLAY TELACON2
                   IF FLAG = ZERO 
                    GO TO R6
                ELSE
                   MOVE ZERO TO FLAG
                
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCID" TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO RotinaFim
           ELSE
                MOVE "CID NAO CADASTRADO" TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO R5. 
       R6.
           ACCEPT TDESCRICAO1
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5. 
       R7.
           ACCEPT TDESCRICAO2
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6. 

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
                WRITE REGCONSU
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
                      GO TO R4.
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
                DELETE CADCONSU RECORD
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
                IF W-ACT = 01 GO TO R7.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO AltOpc.
       AltRw1.
                REWRITE REGCONSU
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM RMensa THRU RMensa-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO CARRO"   TO MENS
                PERFORM RMensa THRU RMensa-FIM
                GO TO RotinaFim.      
    

       RotinaFim.
       
           CLOSE CADCONSU.
           CLOSE CADCONV.
           CLOSE CADPACI.
           CLOSE CADMED.
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
    