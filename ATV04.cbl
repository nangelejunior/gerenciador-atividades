      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
       PROGRAM-ID. ATV04.
       AUTHOR. NEUCLAIR J ANGELE JUNIOR.
       DATE-WRITTEN. 22 SET 2011.
       DATE-COMPILED.
      *REMARKS. *******************************************************
      *         *#NOME:# ATV04                                        *
      *         *******************************************************
      *         *#TIPO:# BATCH - COBOL-LE                             *
      *         *******************************************************
      *         *#FUNC:# GERAR MOVIMENTO DE ATIVIDADES                *
      *         *******************************************************
      *         *#ANALISTA:# NEUCLAIR J. ANGELE JUNIOR                *
      *         *******************************************************
      *
      ******************************************************************
       ENVIRONMENT                     DIVISION.
      ******************************************************************
      *
      ******************************************************************
       CONFIGURATION                   SECTION.
      ******************************************************************
      *
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      ******************************************************************
       INPUT-OUTPUT                    SECTION.
      ******************************************************************
      *
       FILE-CONTROL.
           SELECT  SYS010    ASSIGN    TO 'SYS010.DAT'
                     ORGANIZATION INDEXED
                     ACCESS MODE DYNAMIC
                     RECORD KEY FS-CHAVE
                     ALTERNATE KEY FS-DT-CONCL WITH DUPLICATES
                     FILE STATUS IS WS-STATUS-SYS010.
      *
           SELECT  SYS011    ASSIGN    TO 'SYS011.DAT'
                     ORGANIZATION INDEXED
                     ACCESS MODE DYNAMIC
                     RECORD KEY FD-CHAVE
                     ALTERNATE KEY FD-DT-CONCL WITH DUPLICATES
                     FILE STATUS IS WS-STATUS-SYS011.
      *
           SELECT  SYS020    ASSIGN    TO 'SYS020.TXT'
                     ORGANIZATION LINE SEQUENTIAL
                     FILE STATUS IS WS-STATUS-SYS020.
      *
           SELECT  SYS030    ASSIGN    TO 'SYS030.TMP'
                     FILE STATUS IS WS-STATUS-SYS030.
      *
      ******************************************************************
       DATA                            DIVISION.
      ******************************************************************
      *
      ******************************************************************
       FILE                            SECTION.
      ******************************************************************
      *
       FD          SYS010
                              DATA RECORD FS-SYS010.
      *
       01          FS-SYS010.
      *
           COPY WATV01                 REPLACING ==::== BY ==FS==.
      *
       FD          SYS011
                              DATA RECORD FD-SYS011.
      *
       01          FD-SYS011.
      *
           COPY WATV01                 REPLACING ==::== BY ==FD==.
      *
       FD          SYS020
                              RECORDING MODE IS F
                              DATA RECORD FD-SYS020
                              BLOCK CONTAINS 0 RECORDS
                              LABEL RECORD IS STANDARD.
      *
       01          FD-SYS020.
           03      FILLER          PIC     X(320)        VALUE SPACES.
      *
       SD          SYS030
                              DATA RECORD SD-SYS030.
      *
       01          SD-SYS030.
      *
           COPY WATV01                 REPLACING ==::== BY ==SD==.
      *
      ******************************************************************
       WORKING-STORAGE                 SECTION.
      ******************************************************************
      *
       77          FILLER          PIC     X(32)         VALUE
                                   'III WORKING-STORAGE SECTION III'.
      *
       01          WS-WORKING.
      *
      ******************************************************************
      *    AREA DE ACUMULADORES                                        *
      ******************************************************************
           03      WS-ACUMULADORES.
             05    FILLER          PIC     X(14)         VALUE
                                   '=ACUMULADORES='.
             05    AC-LIDOS-SYS011 PIC    S9(03) COMP-3  VALUE ZEROS.
             05    AC-GRAVA-SYS020 PIC    S9(03) COMP-3  VALUE ZEROS.
      *
      ******************************************************************
      *    AREA DE AUXILIARES                                          *
      ******************************************************************
           03      WS-AUXILIARES.
             05    FILLER          PIC     X(12)         VALUE
                                   '=AUXILIARES='.
             05    WS-MASCARA      PIC     ZZZ.ZZZ.ZZ9   VALUE ZEROS.
             05    WS-MSG          PIC     X(73)         VALUE SPACES.
             05    WS-OPT          PIC     X(01)         VALUE SPACES.
             05    WS-TECLA        PIC     X(02).
               88  CN-ESC                                VALUE '01'.
               88  CN-PF12                               VALUE '93'.
             05    WS-MES-INI      PIC     9(02)         VALUE ZEROS.
             05    WS-STATUS-MES   PIC     X(01).
               88  CN-MES-INI                            VALUE 'I'.
               88  CN-MES-FIM                            VALUE 'F'.
             05    WS-DATA-CORR.
               07  WS-ANO          PIC     X(04)         VALUE SPACES.
               07  WS-MES          PIC     X(02)         VALUE SPACES.
               07  WS-DIA          PIC     X(02)         VALUE SPACES.
      *
      ******************************************************************
      *    AREA DE FILE STATUS                                         *
      ******************************************************************
           03      WS-FILE-STATUS.
             05    FILLER          PIC     X(13)         VALUE
                                   '=FILE STATUS='.
             05    WS-STATUS-SYS010
                                   PIC     X(02).
               88  CN-SYS010-OK                          VALUE '00'.
               88  CN-SYS010-DPL                         VALUE '02'.
               88  CN-SYS010-NOK                         VALUE '23'.
               88  CN-SYS010-EOF                         VALUE '10'.
      *
             05    WS-STATUS-SYS011
                                   PIC     X(02).
               88  CN-SYS011-OK                          VALUE '00'.
               88  CN-SYS011-DPL                         VALUE '02'.
               88  CN-SYS011-NOK                         VALUE '23'.
               88  CN-SYS011-EOF                         VALUE '10'.
      *
             05    WS-STATUS-SYS020
                                   PIC     X(02).
               88  CN-SYS020-OK                          VALUE '00'.
               88  CN-SYS020-NOK                         VALUE '23'.
               88  CN-SYS020-EOF                         VALUE '10'.
      *
             05    WS-STATUS-SYS030
                                   PIC     X(02).
               88  CN-SYS030-OK                          VALUE '00'.
               88  CN-SYS030-NOK                         VALUE '23'.
               88  CN-SYS030-EOF                         VALUE '10'.
      *
      ******************************************************************
      *    AREA DE MENSAGENS                                           *
      ******************************************************************
           03      WS-MENSAGENS.
             05    FILLER          PIC     X(11)         VALUE
                                   '=MENSAGENS='.
             05    WS-MSGARQ-901.
               07  FILLER          PIC     X(50)         VALUE
                   '#ATV04.901I - ERRO NO OPEN ARQ. SYS011    STATUS: '.
               07  WS-RETCOD-901   PIC     X(02)         VALUE ZEROS.
      *
             05    WS-MSGARQ-902.
               07  FILLER          PIC     X(50)         VALUE
                   '#ATV04.902I - ERRO NO OPEN ARQ. SYS020    STATUS: '.
               07  WS-RETCOD-902   PIC     X(02)         VALUE ZEROS.
      *
             05    WS-MSGARQ-903.
               07  FILLER          PIC     X(50)         VALUE
                   '#ATV04.903I - ERRO NO READ ARQ. SYS011    STATUS: '.
               07  WS-RETCOD-903   PIC     X(02)         VALUE ZEROS.
      *
             05    WS-MSGARQ-904.
               07  FILLER          PIC     X(50)         VALUE
                   '#ATV04.904I - ARQUIVO SYS011 VAZIO                '.
      *
             05    WS-MSGARQ-905.
               07  FILLER          PIC     X(50)         VALUE
                   '#ATV04.905I - NAO ENCONTROU REGISTROS     STATUS: '.
               07  WS-RETCOD-905   PIC     X(02)         VALUE ZEROS.
      *
             05    WS-MSGARQ-906.
               07  FILLER          PIC     X(50)         VALUE
                   '#ATV04.906I - ERRO NO WRITE ARQ. SYS020   STATUS: '.
               07  WS-RETCOD-906   PIC     X(02)         VALUE ZEROS.
      *
             05    WS-MSGARQ-907.
               07  FILLER          PIC     X(50)         VALUE
                   '#ATV04.907I - ERRO NO CLOSE ARQ. SYS011   STATUS: '.
               07  WS-RETCOD-907   PIC     X(02)         VALUE ZEROS.
      *
             05    WS-MSGARQ-908.
               07  FILLER          PIC     X(50)         VALUE
                   '#ATV04.908I - ERRO NO CLOSE ARQ. SYS020   STATUS: '.
               07  WS-RETCOD-908   PIC     X(02)         VALUE ZEROS.
      *
             05    WS-MSGARQ-909.
               07  FILLER          PIC     X(50)         VALUE
                   '#ATV04.909I - MES SEM MOVIMENTO                   '.
               07  WS-RETCOD-909   PIC     X(02)         VALUE ZEROS.
      *
             05    WS-MSGARQ-999.
               07  FILLER          PIC     X(35)         VALUE
                   '#ATV04.999I - TOTAL DE ATIVIDADES: '.
               07  WS-MASCAR-999   PIC     ZZZ.ZZZ.ZZ9   VALUE ZEROS.
      *
      ******************************************************************
      *    AREA DE SUBROTINAS                                          *
      ******************************************************************
           03      WS-SUBROTINAS.
             05    FILLER          PIC     X(12)         VALUE
                                   '=SUBROTINAS='.
      *
      ******************************************************************
      *    AREA DE COPYBOOKS                                           *
      ******************************************************************
      *
       01          WS-SYS011.
      *
           COPY WATV01                 REPLACING ==::== BY ==WS==.
      *
       01          WS-SYS020.
      *
           COPY WATV02                 REPLACING ==::== BY ==WS==.
      *
       01          FILLER          PIC     X(32)         VALUE
                                   'FFF FIM DA WORKING-STORAGE FFF'.
      *
      ******************************************************************
       LINKAGE                         SECTION.
      ******************************************************************
      *
       01          LK-MSG          PIC     X(73).
      *
      ******************************************************************
       SCREEN                          SECTION.
      ******************************************************************
      *
       01          SC-SCREEN.
           03      SC-CONFIG.
             05    BLANK SCREEN FOREGROUND-COLOR 7 BACKGROUND-COLOR 1.
      *
      ******************************************************************
      *    BORDA SUPERIOR                                              *
      ******************************************************************
           03      SC-SUPERIOR.
             05    LINE 02 COLUMN 02                     VALUE
                   'ษอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            'อออออออออออออออออออออออออป'.
      *
      ******************************************************************
      *    BORDA ESQUERDA                                              *
      ******************************************************************
           03      SC-ESQUERDA.
             05    LINE 03 COLUMN 02                     VALUE 'บ'.
             05    LINE 04 COLUMN 02                     VALUE 'บ'.
             05    LINE 05 COLUMN 02                     VALUE 'บ'.
             05    LINE 06 COLUMN 02                     VALUE 'บ'.
             05    LINE 07 COLUMN 02                     VALUE 'บ'.
             05    LINE 08 COLUMN 02                     VALUE 'บ'.
             05    LINE 09 COLUMN 02                     VALUE 'บ'.
             05    LINE 10 COLUMN 02                     VALUE 'บ'.
             05    LINE 11 COLUMN 02                     VALUE 'บ'.
             05    LINE 12 COLUMN 02                     VALUE 'บ'.
             05    LINE 13 COLUMN 02                     VALUE 'บ'.
             05    LINE 14 COLUMN 02                     VALUE 'บ'.
             05    LINE 15 COLUMN 02                     VALUE 'บ'.
             05    LINE 16 COLUMN 02                     VALUE 'บ'.
             05    LINE 17 COLUMN 02                     VALUE 'บ'.
             05    LINE 18 COLUMN 02                     VALUE 'บ'.
             05    LINE 19 COLUMN 02                     VALUE 'บ'.
             05    LINE 20 COLUMN 02                     VALUE 'บ'.
             05    LINE 21 COLUMN 02                     VALUE 'บ'.
             05    LINE 22 COLUMN 02                     VALUE 'บ'.
             05    LINE 23 COLUMN 02                     VALUE 'บ'.
      *
      ******************************************************************
      *    BORDA DIREITA                                               *
      ******************************************************************
           03      SC-DIREITA.
             05    LINE 03 COLUMN 79                     VALUE 'บ'.
             05    LINE 04 COLUMN 79                     VALUE 'บ'.
             05    LINE 05 COLUMN 79                     VALUE 'บ'.
             05    LINE 06 COLUMN 79                     VALUE 'บ'.
             05    LINE 07 COLUMN 79                     VALUE 'บ'.
             05    LINE 08 COLUMN 79                     VALUE 'บ'.
             05    LINE 09 COLUMN 79                     VALUE 'บ'.
             05    LINE 10 COLUMN 79                     VALUE 'บ'.
             05    LINE 11 COLUMN 79                     VALUE 'บ'.
             05    LINE 12 COLUMN 79                     VALUE 'บ'.
             05    LINE 13 COLUMN 79                     VALUE 'บ'.
             05    LINE 14 COLUMN 79                     VALUE 'บ'.
             05    LINE 15 COLUMN 79                     VALUE 'บ'.
             05    LINE 16 COLUMN 79                     VALUE 'บ'.
             05    LINE 17 COLUMN 79                     VALUE 'บ'.
             05    LINE 18 COLUMN 79                     VALUE 'บ'.
             05    LINE 19 COLUMN 79                     VALUE 'บ'.
             05    LINE 20 COLUMN 79                     VALUE 'บ'.
             05    LINE 21 COLUMN 79                     VALUE 'บ'.
             05    LINE 22 COLUMN 79                     VALUE 'บ'.
             05    LINE 23 COLUMN 79                     VALUE 'บ'.
      *
      ******************************************************************
      *    BORDA INFERIOR                                              *
      ******************************************************************
           03      SC-INFERIOR.
             05    LINE 24 COLUMN 02                     VALUE
                   'ศอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            'อออออออออออออออออออออออออผ'.
      *
      ******************************************************************
      *    CAMPOS                                                      *
      ******************************************************************
           03      SC-TITULO.
             05    LINE 04 COLUMN 18                     VALUE
                   '      *** GERAR MOVIMENTO DAS ATIVIDADES ***      '
                                   HIGHLIGHT FOREGROUND-COLOR 6.
           03      SC-MES.
             05    LINE 08 COLUMN 24                     VALUE
                   'DIGITE O MES.: '.
             05    LINE 08 COLUMN 39
                                   PIC     9(02) USING WS-MES-INI
                                   AUTO.
           03      SC-MESES.
             05    LINE 07 COLUMN 44                     VALUE
                   '                  '
                                   HIGHLIGHT BACKGROUND-COLOR 3.
             05    LINE 08 COLUMN 44                     VALUE
                   '  01 - JANEIRO    '
                                   HIGHLIGHT BACKGROUND-COLOR 3.
             05    LINE 09 COLUMN 44                     VALUE
                   '  02 - FEVEREIRO  '
                                   HIGHLIGHT BACKGROUND-COLOR 3.
             05    LINE 10 COLUMN 44                     VALUE
                   '  03 - MARCO      '
                                   HIGHLIGHT BACKGROUND-COLOR 3.
             05    LINE 11 COLUMN 44                     VALUE
                   '  04 - ABRIL      '
                                   HIGHLIGHT BACKGROUND-COLOR 3.
             05    LINE 12 COLUMN 44                     VALUE
                   '  05 - MAIO       '
                                   HIGHLIGHT BACKGROUND-COLOR 3.
             05    LINE 13 COLUMN 44                     VALUE
                   '  06 - JUNHO      '
                                   HIGHLIGHT BACKGROUND-COLOR 3.
             05    LINE 14 COLUMN 44                     VALUE
                   '  07 - JULHO      '
                                   HIGHLIGHT BACKGROUND-COLOR 3.
             05    LINE 15 COLUMN 44                     VALUE
                   '  08 - AGOSTO     '
                                   HIGHLIGHT BACKGROUND-COLOR 3.
             05    LINE 16 COLUMN 44                     VALUE
                   '  09 - SETEMRO    '
                                   HIGHLIGHT BACKGROUND-COLOR 3.
             05    LINE 17 COLUMN 44                     VALUE
                   '  10 - OUTUBRO    '
                                   HIGHLIGHT BACKGROUND-COLOR 3.
             05    LINE 18 COLUMN 44                     VALUE
                   '  11 - NOVEMBRO   '
                                   HIGHLIGHT BACKGROUND-COLOR 3.
             05    LINE 19 COLUMN 44                     VALUE
                   '  12 - DEZEMBRO   '
                                   HIGHLIGHT BACKGROUND-COLOR 3.
             05    LINE 20 COLUMN 44                     VALUE
                   '  99 - TODOS      '
                                   HIGHLIGHT BACKGROUND-COLOR 3.
             05    LINE 21 COLUMN 44                     VALUE
                   '                  '
                                   HIGHLIGHT BACKGROUND-COLOR 3.
      *
      ******************************************************************
       PROCEDURE                       DIVISION USING LK-MSG.
      ******************************************************************
      ******************************************************************
      *    ROTINA PRINCIPAL                                            *
      ******************************************************************
       RTPRINCIPAL                     SECTION.
      ******************************************************************
      *
           PERFORM RTINICIAR.
      *
           PERFORM RTPROCESSAR.
      *
           PERFORM RTFINALIZAR.
      *
      ******************************************************************
       RTPRINCIPAL-EXIT.               EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA INICIAL                                              *
      ******************************************************************
       RTINICIAR                       SECTION.
      ******************************************************************
      *
           SORT  SYS030  ON ASCENDING  KEY SD-DT-CONCL
                                       USING  SYS010
                                       GIVING SYS011.

           INITIALIZE WS-ACUMULADORES
                      WS-AUXILIARES
                      WS-FILE-STATUS
                      WS-SYS011.
      *
           SET CN-MES-INI              TO TRUE.
      *
           OPEN INPUT  SYS011
                OUTPUT SYS020.
      *
           IF CN-SYS011-OK
               CONTINUE
           ELSE
               MOVE WS-STATUS-SYS011   TO WS-RETCOD-901
               MOVE WS-MSGARQ-901      TO WS-MSG
               PERFORM RTFINALIZAR
           END-IF.
      *
           IF CN-SYS020-OK
               CONTINUE
           ELSE
               MOVE WS-STATUS-SYS020   TO WS-RETCOD-902
               MOVE WS-MSGARQ-902      TO WS-MSG
               PERFORM RTFINALIZAR
           END-IF.
      *
           PERFORM RTLER-SYS011.
      *
           IF CN-SYS011-EOF
               MOVE WS-MSGARQ-904      TO WS-MSG
               PERFORM RTCANCELAR
           END-IF.
      *
           MOVE ZEROS                  TO AC-LIDOS-SYS011.
      *
           DISPLAY SC-SCREEN.
      *
      ******************************************************************
       RTINICAR-EXIT.                  EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA DE PROCESSAMENTO                                     *
      ******************************************************************
       RTPROCESSAR                     SECTION.
      ******************************************************************
      *
           ACCEPT WS-DATA-CORR         FROM DATE YYYYMMDD.
      *
           PERFORM                     UNTIL WS-MES-INI GREATER 00 AND
                                             WS-MES-INI LESS    13 OR
                                             WS-MES-INI EQUAL   99 OR
                                             CN-ESC
               ACCEPT SC-MES
               ACCEPT WS-TECLA         FROM ESCAPE KEY
           END-PERFORM.
      *
           EVALUATE TRUE
               WHEN CN-ESC
                   CONTINUE
               WHEN WS-MES-INI         EQUAL 99
                   MOVE SPACES         TO FD-DT-CONCL
                   PERFORM RTSTART-SYS011
                   PERFORM RTLER-SYS011
                   PERFORM             UNTIL CN-SYS011-EOF
                       IF WS-MES-CONCL OF WS-SYS011
                                       EQUAL SPACES
                           CONTINUE
                       ELSE
                           PERFORM RTMONTAR-SYS020
                           PERFORM RTGRAVAR-SYS020
                       END-IF
                       PERFORM RTLER-SYS011
                   END-PERFORM
               WHEN OTHER
                   STRING WS-ANO WS-MES-INI '01'
                                       DELIMITED BY SIZE
                                       INTO FD-DT-CONCL
                   PERFORM RTSTART-SYS011
                   PERFORM RTLER-SYS011
                   PERFORM             UNTIL CN-MES-FIM OR
                                             CN-SYS011-EOF
                       PERFORM RTVERIFICAR
                   END-PERFORM
           END-EVALUATE.
      *
           PERFORM RTTOTALIZAR.
      *
           PERFORM RTFECHAR.
      *
      ******************************************************************
       RTPROCESSAR-EXIT.               EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA PARA LEITURA DO ARQUIVO SYS011                       *
      ******************************************************************
       RTLER-SYS011                    SECTION.
      ******************************************************************
      *
           READ SYS011 NEXT            INTO WS-SYS011.
      *
           EVALUATE TRUE
               WHEN CN-SYS011-OK
               WHEN CN-SYS011-DPL
                   ADD 1               TO AC-LIDOS-SYS011
               WHEN CN-SYS011-EOF
                   CONTINUE
               WHEN OTHER
                   MOVE WS-STATUS-SYS011
                                       TO WS-RETCOD-903
                   MOVE WS-MSGARQ-903  TO WS-MSG
                   PERFORM RTCANCELAR
           END-EVALUATE.
      *
      ******************************************************************
       RTLER-SYS011-EXIT.              EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA PARA STARTAR O ARQUIVO SYS011                        *
      ******************************************************************
       RTSTART-SYS011                  SECTION.
      ******************************************************************
      *
           START SYS011         KEY IS GREATER THAN OR
                                       EQUAL FD-DT-CONCL
               INVALID KEY
                   MOVE WS-STATUS-SYS011
                                       TO WS-RETCOD-905
                   MOVE WS-MSGARQ-905  TO WS-MSG
                   PERFORM RTCANCELAR
           END-START.
      *
      ******************************************************************
       RTSTART-SYS011-EXIT.            EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA PARA MONTAR ARQUIVO SYS020                           *
      ******************************************************************
       RTMONTAR-SYS020                 SECTION.
      ******************************************************************
      *
           MOVE WS-SS                  OF WS-SYS011
                                       TO WS-SS            OF WS-SYS020.
           MOVE WS-ST                  OF WS-SYS011
                                       TO WS-ST            OF WS-SYS020.
           MOVE WS-ANO-RECEB           OF WS-SYS011
                                       TO WS-ANO-RECEB     OF WS-SYS020.
           MOVE WS-MES-RECEB           OF WS-SYS011
                                       TO WS-MES-RECEB     OF WS-SYS020.
           MOVE WS-DIA-RECEB           OF WS-SYS011
                                       TO WS-DIA-RECEB     OF WS-SYS020.
           MOVE WS-ANO-TERM            OF WS-SYS011
                                       TO WS-ANO-TERM      OF WS-SYS020.
           MOVE WS-MES-TERM            OF WS-SYS011
                                       TO WS-MES-TERM      OF WS-SYS020.
           MOVE WS-DIA-TERM            OF WS-SYS011
                                       TO WS-DIA-TERM      OF WS-SYS020.
           MOVE WS-HOR-TERM            OF WS-SYS011
                                       TO WS-HOR-TERM      OF WS-SYS020.
           MOVE WS-MIN-TERM            OF WS-SYS011
                                       TO WS-MIN-TERM      OF WS-SYS020.
           MOVE WS-SEG-TERM            OF WS-SYS011
                                       TO WS-SEG-TERM      OF WS-SYS020.
           MOVE WS-HR-ESFORCO          OF WS-SYS011
                                       TO WS-HR-ESFORCO    OF WS-SYS020.
           MOVE WS-NM-SOLIC            OF WS-SYS011
                                       TO WS-NM-SOLIC      OF WS-SYS020.
           MOVE WS-TEL-SOLIC           OF WS-SYS011
                                       TO WS-TEL-SOLIC     OF WS-SYS020.
           MOVE WS-RAMAL-SOLIC         OF WS-SYS011
                                       TO WS-RAMAL-SOLIC   OF WS-SYS020.
           MOVE WS-LINGUAGEM           OF WS-SYS011
                                       TO WS-LINGUAGEM     OF WS-SYS020.
           MOVE WS-NM-MOD              OF WS-SYS011
                                       TO WS-NM-MOD        OF WS-SYS020.
           MOVE WS-SIGL-SIST           OF WS-SYS011
                                       TO WS-SIGL-SIST     OF WS-SYS020.
           MOVE WS-NM-PGM              OF WS-SYS011
                                       TO WS-NM-PGM        OF WS-SYS020.
           MOVE WS-VERS-PGM            OF WS-SYS011
                                       TO WS-VERS-PGM      OF WS-SYS020.
           MOVE WS-OBS                 OF WS-SYS011
                                       TO WS-OBS           OF WS-SYS020.
           MOVE WS-ANO-CONCL           OF WS-SYS011
                                       TO WS-ANO-CONCL     OF WS-SYS020.
           MOVE WS-MES-CONCL           OF WS-SYS011
                                       TO WS-MES-CONCL     OF WS-SYS020.
           MOVE WS-DIA-CONCL           OF WS-SYS011
                                       TO WS-DIA-CONCL     OF WS-SYS020.
           MOVE WS-HOR-CONCL           OF WS-SYS011
                                       TO WS-HOR-CONCL     OF WS-SYS020.
           MOVE WS-MIN-CONCL           OF WS-SYS011
                                       TO WS-MIN-CONCL     OF WS-SYS020.
           MOVE WS-SEG-CONCL           OF WS-SYS011
                                       TO WS-SEG-CONCL     OF WS-SYS020.
           MOVE WS-NR-DUVID            OF WS-SYS011
                                       TO WS-NR-DUVID      OF WS-SYS020.
      *
      ******************************************************************
       RTMONTAR-SYS020-EXIT.           EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA PARA GRAVAR ARQUIVO SYS020                           *
      ******************************************************************
       RTGRAVAR-SYS020                 SECTION.
      ******************************************************************
      *
           WRITE FD-SYS020             FROM WS-SYS020.
      *
           IF CN-SYS020-OK
               ADD 1                   TO AC-GRAVA-SYS020
           ELSE
               MOVE WS-STATUS-SYS020   TO WS-RETCOD-906
               MOVE WS-MSGARQ-906      TO WS-MSG
               PERFORM RTCANCELAR
           END-IF.
      *
      ******************************************************************
       RTGRAVAR-SYS020-EXIT.           EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA PARA VERIFICAR O FIM DO MOVIMENTO                    *
      ******************************************************************
       RTVERIFICAR                     SECTION.
      ******************************************************************
      *
           EVALUATE TRUE
               WHEN WS-MES-CONCL OF WS-SYS011
                                       LESS    WS-MES-INI
                   PERFORM RTLER-SYS011
               WHEN WS-MES-CONCL OF WS-SYS011
                                       EQUAL   WS-MES-INI
                   PERFORM RTMONTAR-SYS020
                   PERFORM RTGRAVAR-SYS020
                   PERFORM RTLER-SYS011
               WHEN WS-MES-CONCL OF WS-SYS011
                                       GREATER WS-MES-INI
                   SET CN-MES-FIM      TO TRUE
           END-EVALUATE.
      *
      ******************************************************************
       RTVERIFICAR-EXIT.               EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA PARA FECHAMENTO DE ARQUIVOS                          *
      ******************************************************************
       RTFECHAR                        SECTION.
      ******************************************************************
      *
           CLOSE SYS011
                 SYS020.
      *
           IF CN-SYS011-OK
               CONTINUE
           ELSE
               MOVE WS-STATUS-SYS011   TO WS-RETCOD-907
               MOVE WS-MSGARQ-907      TO WS-MSG
               PERFORM RTFINALIZAR
           END-IF.
      *
           IF CN-SYS020-OK
               CONTINUE
           ELSE
               MOVE WS-STATUS-SYS020   TO WS-RETCOD-908
               MOVE WS-MSGARQ-908      TO WS-MSG
               PERFORM RTFINALIZAR
           END-IF.
      *
      ******************************************************************
       RTFECHAR-EXIT.                  EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA PARA TOTALIZAR                                       *
      ******************************************************************
       RTTOTALIZAR                     SECTION.
      ******************************************************************
      *
           IF AC-GRAVA-SYS020          LESS OR EQUAL ZEROS
               MOVE WS-MSGARQ-909      TO WS-MSG
           ELSE
               MOVE AC-GRAVA-SYS020    TO WS-MASCAR-999
               MOVE WS-MSGARQ-999      TO WS-MSG
           END-IF.
      *
      ******************************************************************
       RTTOTALIZAR-EXIT.               EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA PARA CANCELAMENTO DO PROGRAMA                        *
      ******************************************************************
       RTCANCELAR                      SECTION.
      ******************************************************************
      *
           PERFORM RTFECHAR.
      *
           PERFORM RTFINALIZAR.
      *
      ******************************************************************
       RTCANCELAR-EXIT.                EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA FINAL                                                *
      ******************************************************************
       RTFINALIZAR                     SECTION.
      ******************************************************************
      *
           MOVE WS-MSG                 TO LK-MSG.
      *
           MOVE ZEROS                  TO RETURN-CODE.
      *
           GOBACK.
      *
      ******************************************************************
       RTFINALIZAR-EXIT.               EXIT.
      ******************************************************************
      ******************************************************************
      *                        FIM DO PROGRAMA                         *
      ******************************************************************
