      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
       PROGRAM-ID. ATV03.
       AUTHOR. NEUCLAIR J ANGELE JUNIOR.
       DATE-WRITTEN. 22 SET 2011.
       DATE-COMPILED.
      *REMARKS. *******************************************************
      *         *#NOME:# ATV03                                        *
      *         *******************************************************
      *         *#TIPO:# BATCH - COBOL-LE                             *
      *         *******************************************************
      *         *#FUNC:# BAIXA DE ATIVIDADES                          *
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
                     RECORD KEY FD-CHAVE
                     ALTERNATE KEY FD-DT-CONCL WITH DUPLICATES
                     FILE STATUS IS WS-STATUS-SYS010.
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
                              DATA RECORD FD-SYS010.
      *
       01          FD-SYS010.
      *
           COPY WATV01                 REPLACING ==::== BY ==FD==.
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
             05    AC-CAMPO        PIC    S9(03) COMP-3  VALUE ZEROS.
      *
      ******************************************************************
      *    AREA DE AUXILIARES                                          *
      ******************************************************************
           03      WS-AUXILIARES.
             05    FILLER          PIC     X(12)         VALUE
                                   '=AUXILIARES='.
             05    WS-MASCARA      PIC     ZZZ.ZZZ.ZZ9   VALUE ZEROS.
             05    WS-MENSAG.
               07  FILLER          PIC     X(08)         VALUE
                                   ' MSG..: '.
               07  WS-MSG          PIC     X(72)         VALUE SPACES.
             05    WS-OPT          PIC     X(01)         VALUE SPACES.
             05    WS-TECLA        PIC     X(02).
               88  CN-ESC                                VALUE '01'.
               88  CN-PF12                               VALUE '93'.
             05    WS-STATUS-FLAG  PIC     X(01)         VALUE 'N'.
               88  CN-BAIXAR-OK                          VALUE 'S'.
               88  CN-BAIXAR-NOK                         VALUE 'N'.
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
      ******************************************************************
      *    AREA DE MENSAGENS                                           *
      ******************************************************************
           03      WS-MENSAGENS.
             05    FILLER          PIC     X(11)         VALUE
                                   '=MENSAGENS='.
             05    WS-MSGARQ-901.
               07  FILLER          PIC     X(50)         VALUE
                   '#ATV03.901I - ERRO NO OPEN ARQ. SYS010    STATUS: '.
               07  WS-RETCOD-901   PIC     X(02)         VALUE ZEROS.
      *
             05    WS-MSGARQ-902.
               07  FILLER          PIC     X(50)         VALUE
                   '#ATV03.902I - ERRO NO READ ARQ. SYS010    STATUS: '.
               07  WS-RETCOD-902   PIC     X(02)         VALUE ZEROS.
      *
             05    WS-MSGARQ-903.
               07  FILLER          PIC     X(50)         VALUE
                   '#ATV03.903I - ERRO NO REWRITE ARQ. SYS010 STATUS: '.
               07  WS-RETCOD-903   PIC     X(02)         VALUE ZEROS.
      *
             05    WS-MSGARQ-904.
               07  FILLER          PIC     X(50)         VALUE
                   '#ATV03.904I - ERRO NO CLOSE ARQ. SYS010   STATUS: '.
               07  WS-RETCOD-904   PIC     X(02)         VALUE ZEROS.
      *
             05    WS-MSGARQ-999.
               07  FILLER          PIC     X(50)         VALUE
                   '#ATV03.999I - EXECUTADO COM SUCESSO               '.
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
       01          WS-SYS010.
      *
           COPY WATV01                 REPLACING ==::== BY ==WS==.
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
      *    BORDA SUPERIOR                                             *
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
                   '          *** BAIXA DE ATIVIDADES ***             '
                                   HIGHLIGHT FOREGROUND-COLOR 6.
           03      SC-SS.
             05    LINE 08 COLUMN 04                     VALUE
                   'SS..: '        HIGHLIGHT.
             05    LINE 08 COLUMN 10
                                   PIC     9(06) USING WS-SS
                                   AUTO REQUIRED.
           03      SC-ST.
             05    LINE 08 COLUMN 18                     VALUE
                   'ST..: '        HIGHLIGHT.
             05    LINE 08 COLUMN 24
                                   PIC     9(06) USING WS-ST
                                   AUTO REQUIRED.
           03      SC-NM-PGM.
             05    LINE 08 COLUMN 40                     VALUE
                   'NOME PGM.....: '.
             05    LINE 08 COLUMN 55
                                   PIC     X(08) USING WS-NM-PGM
                                   AUTO PROMPT IS 'ฒ'.
           03      SC-DT-RECEB.
             05    LINE 10 COLUMN 04                     VALUE
                   'DT. RECEBIM.: '.
             05    LINE 10 COLUMN 18
                                   PIC     X(02) USING WS-DIA-RECEB
                                   AUTO PROMPT IS 'ฒ'.
             05    LINE 10 COLUMN 20                     VALUE '/'.
             05    LINE 10 COLUMN 21
                                   PIC     X(02) USING WS-MES-RECEB
                                   AUTO PROMPT IS 'ฒ'.
             05    LINE 10 COLUMN 23                     VALUE '/'.
             05    LINE 10 COLUMN 24
                                   PIC     X(04) USING WS-ANO-RECEB
                                   AUTO PROMPT IS 'ฒ'.
           03      SC-DT-TERM.
             05    LINE 12 COLUMN 04                     VALUE
                   'PREV. TERM..: '.
             05    LINE 12 COLUMN 18
                                   PIC     X(02) USING WS-DIA-TERM
                                   AUTO PROMPT IS 'ฒ'.
             05    LINE 12 COLUMN 20                     VALUE '/'.
             05    LINE 12 COLUMN 21
                                   PIC     X(02) USING WS-MES-TERM
                                   AUTO PROMPT IS 'ฒ'.
             05    LINE 12 COLUMN 23                     VALUE '/'.
             05    LINE 12 COLUMN 24
                                   PIC     X(04) USING WS-ANO-TERM
                                   AUTO PROMPT IS 'ฒ'.
           03      SC-HR-TERM.
             05    LINE 12 COLUMN 29
                                   PIC     X(02) USING WS-HOR-TERM
                                   AUTO PROMPT IS 'ฒ'.
             05    LINE 12 COLUMN 31                     VALUE ':'.
             05    LINE 12 COLUMN 32
                                   PIC     X(02) USING WS-MIN-TERM
                                   AUTO PROMPT IS 'ฒ'.
             05    LINE 12 COLUMN 34                     VALUE ':'.
             05    LINE 12 COLUMN 35
                                   PIC     X(02) USING WS-SEG-TERM
                                   AUTO PROMPT IS 'ฒ'.
           03      SC-DT-CONCL.
             05    LINE 14 COLUMN 04                     VALUE
                   'CONCLUIDO...: '.
             05    LINE 14 COLUMN 18
                                   PIC     X(02) USING WS-DIA-CONCL
                                   AUTO PROMPT IS 'ฒ'.
             05    LINE 14 COLUMN 20                     VALUE '/'.
             05    LINE 14 COLUMN 21
                                   PIC     X(02) USING WS-MES-CONCL
                                   AUTO PROMPT IS 'ฒ'.
             05    LINE 14 COLUMN 23                     VALUE '/'.
             05    LINE 14 COLUMN 24
                                   PIC     X(04) USING WS-ANO-CONCL
                                   AUTO PROMPT IS 'ฒ'.
           03      SC-HR-CONCL.
             05    LINE 14 COLUMN 29
                                   PIC     9(02) USING WS-HOR-CONCL
                                   AUTO.
             05    LINE 14 COLUMN 31                     VALUE ':'.
             05    LINE 14 COLUMN 32
                                   PIC     9(02) USING WS-MIN-CONCL
                                   AUTO.
             05    LINE 14 COLUMN 34                     VALUE ':'.
             05    LINE 14 COLUMN 35
                                   PIC     9(02) USING WS-SEG-CONCL
                                   AUTO.
           03      SC-NR-DUVID.
             05    LINE 16 COLUMN 04                     VALUE
                   'NUM. DUVID..: '.
             05    LINE 16 COLUMN 18
                                   PIC     9(02) USING WS-NR-DUVID
                                   AUTO.
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
           INITIALIZE WS-ACUMULADORES
                      WS-AUXILIARES
                      WS-FILE-STATUS
                      WS-SYS010.
      *
           OPEN I-O SYS010.
      *
           IF CN-SYS010-OK
               CONTINUE
           ELSE
               MOVE WS-STATUS-SYS010   TO WS-RETCOD-901
               MOVE WS-MSGARQ-901      TO WS-MSG
               PERFORM RTFINALIZAR
           END-IF.
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
           PERFORM                     UNTIL CN-ESC
               PERFORM RTINICIALIZAR-TELA
               PERFORM RTOBTER-CHAVE
               IF AC-CAMPO             LESS OR EQUAL ZEROS
                   SET CN-ESC          TO TRUE
               ELSE
                   PERFORM RTLER-SYS010
                   IF CN-BAIXAR-OK
                       PERFORM RTOBTER-ENTRADA
                       PERFORM RTOPCAO
                       IF CN-PF12
                           PERFORM RTATUALIZAR
                       ELSE
                           MOVE SPACES TO WS-TECLA
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
      *
           PERFORM RTFECHAR.
      *
      ******************************************************************
       RTPROCESSAR-EXIT.               EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA PARA INICIALIZAR TELA                                *
      ******************************************************************
       RTINICIALIZAR-TELA              SECTION.
      ******************************************************************
      *
           INITIALIZE WS-ACUMULADORES
                      WS-AUXILIARES
                      WS-FILE-STATUS
                      WS-SYS010.
      *
           MOVE WS-MSGARQ-999          TO WS-MSG.
      *
           DISPLAY SC-SCREEN.
      *
           DISPLAY 'ESC - SAI'         AT 2502.
      *
           MOVE 1                      TO AC-CAMPO.
      *
      ******************************************************************
       RTINICIALIZAR-TELA-EXIT.        EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA PARA OBTER CHAVE                                     *
      ******************************************************************
       RTOBTER-CHAVE                   SECTION.
      ******************************************************************
      *
           PERFORM                     UNTIL AC-CAMPO EQUAL   0 OR
                                                      GREATER 2
               EVALUATE AC-CAMPO
                   WHEN 1
                       ACCEPT SC-SS
                   WHEN 2
                       ACCEPT SC-ST
               END-EVALUATE
               PERFORM RTPOSICIONAR
           END-PERFORM.
      *
      ******************************************************************
       RTOBTER-CHAVE-EXIT.             EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA PARA LEITURA DO ARQUIVO SYS010                       *
      ******************************************************************
       RTLER-SYS010                    SECTION.
      ******************************************************************
      *
           MOVE WS-CHAVE               TO FD-CHAVE.
      *
           READ SYS010                 INTO WS-SYS010.
      *
           EVALUATE TRUE
               WHEN CN-SYS010-OK
               WHEN CN-SYS010-DPL
                   IF WS-DT-CONCL      EQUAL SPACES AND
                      WS-HR-CONCL      EQUAL ZEROS
                       SET CN-BAIXAR-OK
                                       TO TRUE
                   END-IF
               WHEN CN-SYS010-NOK
                   SET CN-BAIXAR-NOK   TO TRUE
               WHEN OTHER
                   MOVE WS-STATUS-SYS010
                                       TO WS-RETCOD-902
                   MOVE WS-MSGARQ-902  TO WS-MSG
                   PERFORM RTCANCELAR
           END-EVALUATE.
      *
      ******************************************************************
       RTLER-SYS010-EXIT.     .        EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA PARA OBTER ENTRADA                                   *
      ******************************************************************
       RTOBTER-ENTRADA                 SECTION.
      ******************************************************************
      *
           ACCEPT WS-DT-CONCL          FROM DATE YYYYMMDD.
           ACCEPT WS-HR-CONCL          FROM TIME.
      *
           DISPLAY SC-SCREEN.
      *
           DISPLAY 'ENTER - AVANCA / ESC - VOLTA'
                                       AT 2502.
      *
           PERFORM                     UNTIL AC-CAMPO EQUAL   2 OR
                                                      GREATER 5
               EVALUATE AC-CAMPO
                   WHEN 3
                       ACCEPT SC-DT-CONCL
                   WHEN 4
                       ACCEPT SC-HR-CONCL
                   WHEN 5
                       ACCEPT SC-NR-DUVID
               END-EVALUATE
               PERFORM RTPOSICIONAR
           END-PERFORM.
      *
      ******************************************************************
       RTOBTER-ENTRADA-EXIT.           EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA PARA POSICIONAR O CURSOR                             *
      ******************************************************************
       RTPOSICIONAR                    SECTION.
      ******************************************************************
      *
           ACCEPT WS-TECLA             FROM ESCAPE KEY.
      *
           IF CN-ESC
               SUBTRACT 1              FROM AC-CAMPO
           ELSE
               ADD      1              TO   AC-CAMPO
           END-IF.
      *
      ******************************************************************
       RTPOSICIONAR-EXIT.              EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA PARA OBTER OPCAO                                     *
      ******************************************************************
       RTOPCAO                         SECTION.
      ******************************************************************
      *
           DISPLAY SC-SCREEN.
      *
           DISPLAY 'ESC - CANCELA / F12 - CONFIRMA'
                                       AT 2502.
      *
           PERFORM                     UNTIL CN-ESC OR
                                             CN-PF12
               ACCEPT WS-OPT AT 2580   WITH AUTO UPDATE
               ACCEPT WS-TECLA         FROM ESCAPE KEY
           END-PERFORM.
      *
      ******************************************************************
       RTOPCAO-EXIT.                   EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA PARA ATUALIZAR REGISTRO                              *
      ******************************************************************
       RTATUALIZAR                     SECTION.
      ******************************************************************
      *
           REWRITE FD-SYS010           FROM WS-SYS010.
      *
           IF CN-SYS010-OK             OR CN-SYS010-DPL
               CONTINUE
           ELSE
               MOVE WS-STATUS-SYS010   TO WS-RETCOD-903
               MOVE WS-MSGARQ-903      TO WS-MSG
               PERFORM RTCANCELAR
           END-IF.
      *
      ******************************************************************
       RTATUALIZAR-EXIT.               EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA PARA FECHAMENTO DE ARQUIVO                           *
      ******************************************************************
       RTFECHAR                        SECTION.
      ******************************************************************
      *
           CLOSE SYS010.
      *
           IF CN-SYS010-OK
               CONTINUE
           ELSE
               MOVE WS-STATUS-SYS010   TO WS-RETCOD-904
               MOVE WS-MSGARQ-904      TO WS-MSG
               PERFORM RTFINALIZAR
           END-IF.
      *
      ******************************************************************
       RTFECHAR-EXIT.                  EXIT.
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
