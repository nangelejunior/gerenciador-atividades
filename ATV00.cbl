      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
       PROGRAM-ID. ATV00.
       AUTHOR. NEUCLAIR J. ANGELE JR.
       DATE-WRITTEN. 29 SET 2011.
       DATE-COMPILED.
      *REMARKS. *******************************************************
      *         *#NOME:# ATV00                                        *
      *         *******************************************************
      *         *#TIPO:# BATCH - COBOL-LE                             *
      *         *******************************************************
      *         *#FUNC:# CHAMADOR DOS SUBPROGRAMAS                    *
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
       DATA                            DIVISION.
      ******************************************************************
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
      *    AREA DE AUXILIARES                                          *
      ******************************************************************
           03      WS-AUXILIARES.
             05    FILLER          PIC     X(12)         VALUE
                                   '=AUXILIARES='.
             05    WS-MSG          PIC     X(73)         VALUE SPACES.
             05    WS-OPCAO        PIC     X(02)         VALUE SPACES.
             05    WS-TECLA        PIC     X(02).
               88  CN-BAIXO                              VALUE '00'.
               88  CN-ESC                                VALUE '01'.
               88  CN-PF12                               VALUE '93'.
               88  CN-CIMA                               VALUE '99'.
      *
      ******************************************************************
      *    AREA DE ACUMULADORES                                        *
      ******************************************************************
           03      WS-ACUMULADORES.
             05    FILLER          PIC     X(14)         VALUE
                                   '=ACUMULADORES='.
             05    WS-CT-OPCAO     PIC     9(03) COMP-3  VALUE ZEROS.
      *
      ******************************************************************
      *    AREA DE SUBROTINAS                                          *
      ******************************************************************
           03      WS-SUBROTINAS.
             05    FILLER          PIC     X(12)         VALUE
                                   '=SUBROTINAS='.
             05    WS-ATV01        PIC     X(08)         VALUE
                                   'ATV01   '.
             05    WS-ATV02        PIC     X(08)         VALUE
                                   'ATV02   '.
             05    WS-ATV03        PIC     X(08)         VALUE
                                   'ATV03   '.
             05    WS-ATV04        PIC     X(08)         VALUE
                                   'ATV04   '.
      *
       01          FILLER          PIC     X(32)         VALUE
                                   'FFF FIM DA WORKING-STORAGE FFF'.
      ******************************************************************
      *
      ******************************************************************
       SCREEN                          SECTION.
      ******************************************************************
      *
       01          SC-TELA.
           03      BLANK SCREEN FOREGROUND-COLOR 7 BACKGROUND-COLOR 1.
      *
      ******************************************************************
      *    BORDA SUPERIOR                                              *
      ******************************************************************
           03      SC-BR-SUPERIOR1.
               05  LINE 02 COLUMN 02                     VALUE
                   'ษอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            'อออออออออออออออออออออออออป'.
           03      SC-BR-SUPERIOR2.
               05  LINE 04 COLUMN 02                     VALUE
                   'ออออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            'อออออออออออออออออออออออออ'.
      *
      ******************************************************************
      *    BORDA ESQUERDA                                              *
      ******************************************************************
           03      SC-BR-ESQUERDA.
               05  LINE 03 COLUMN 02                     VALUE 'บ'.
               05  LINE 04 COLUMN 02                     VALUE 'ฬ'.
               05  LINE 05 COLUMN 02                     VALUE 'บ'.
               05  LINE 06 COLUMN 02                     VALUE 'บ'.
               05  LINE 07 COLUMN 02                     VALUE 'บ'.
               05  LINE 08 COLUMN 02                     VALUE 'บ'.
               05  LINE 09 COLUMN 02                     VALUE 'บ'.
               05  LINE 10 COLUMN 02                     VALUE 'บ'.
               05  LINE 11 COLUMN 02                     VALUE 'บ'.
               05  LINE 12 COLUMN 02                     VALUE 'บ'.
               05  LINE 13 COLUMN 02                     VALUE 'บ'.
               05  LINE 14 COLUMN 02                     VALUE 'บ'.
               05  LINE 15 COLUMN 02                     VALUE 'บ'.
               05  LINE 16 COLUMN 02                     VALUE 'บ'.
               05  LINE 17 COLUMN 02                     VALUE 'บ'.
               05  LINE 18 COLUMN 02                     VALUE 'บ'.
               05  LINE 19 COLUMN 02                     VALUE 'บ'.
               05  LINE 20 COLUMN 02                     VALUE 'บ'.
               05  LINE 21 COLUMN 02                     VALUE 'บ'.
               05  LINE 22 COLUMN 02                     VALUE 'บ'.
               05  LINE 23 COLUMN 02                     VALUE 'บ'.
      *
      ******************************************************************
      *    BORDA DIREITA                                               *
      ******************************************************************
           03      SC-BR-DIREITA.
               05  LINE 03 COLUMN 79                     VALUE 'บ'.
               05  LINE 04 COLUMN 79                     VALUE 'น'.
               05  LINE 05 COLUMN 79                     VALUE 'บ'.
               05  LINE 06 COLUMN 79                     VALUE 'บ'.
               05  LINE 07 COLUMN 79                     VALUE 'บ'.
               05  LINE 08 COLUMN 79                     VALUE 'บ'.
               05  LINE 09 COLUMN 79                     VALUE 'บ'.
               05  LINE 10 COLUMN 79                     VALUE 'บ'.
               05  LINE 11 COLUMN 79                     VALUE 'บ'.
               05  LINE 12 COLUMN 79                     VALUE 'บ'.
               05  LINE 13 COLUMN 79                     VALUE 'บ'.
               05  LINE 14 COLUMN 79                     VALUE 'บ'.
               05  LINE 15 COLUMN 79                     VALUE 'บ'.
               05  LINE 16 COLUMN 79                     VALUE 'บ'.
               05  LINE 17 COLUMN 79                     VALUE 'บ'.
               05  LINE 18 COLUMN 79                     VALUE 'บ'.
               05  LINE 19 COLUMN 79                     VALUE 'บ'.
               05  LINE 20 COLUMN 79                     VALUE 'บ'.
               05  LINE 21 COLUMN 79                     VALUE 'บ'.
               05  LINE 22 COLUMN 79                     VALUE 'บ'.
               05  LINE 23 COLUMN 79                     VALUE 'บ'.
      *
      ******************************************************************
      *    BORDA INFERIOR                                              *
      ******************************************************************
           03      SC-BR-INFERIOR.
               05  LINE 24 COLUMN 02                     VALUE
                   'ศอออออออออออออออออออออออออออออออออออออออออออออออออออ
      -            'อออออออออออออออออออออออออผ'.
      *
      ******************************************************************
      *    CAMPOS                                                      *
      ******************************************************************
           03      SC-TX-TITULO.
               05  LINE 03 COLUMN 22                     VALUE
                   '* P R O G R A M A   C E N T R A L *'.
           03      SC-TX-MSG1.
               05  LINE 25 COLUMN 01                     VALUE ' MSG.: '
                   HIGHLIGHT FOREGROUND-COLOR 6 BACKGROUND-COLOR 4.
           03      SC-TX-MSG1.
               05  LINE 25 COLUMN 08
                                   PIC X(73)             FROM WS-MSG
                   HIGHLIGHT FOREGROUND-COLOR 6 BACKGROUND-COLOR 4.
           03      SC-TX-OPCAO01.
               05  LINE 10 COLUMN 16                     VALUE
                   'INCLUSAO E ALTERACAO DE ATIVIDADES                '.
           03      SC-TX-OPCAO02.
               05  LINE 12 COLUMN 16                     VALUE
                   'EXCLUSAO DE ATIVIDADES                            '.
           03      SC-TX-OPCAO03.
               05  LINE 14 COLUMN 16                     VALUE
                   'BAIXA DE ATIVIDADES                               '.
           03      SC-TX-OPCAO04.
               05  LINE 16 COLUMN 16                     VALUE
                   'GERAR MOVIMENTO MENSAL/ANUAL DE ATIVIDADES        '.
           03      SC-TX-AUTOR.
               05  LINE 23 COLUMN 04                     VALUE
                   'NEUCLAIR. J. ANGELE JR.'.
           03      SC-TX-SELECIONA.
               05  LINE 23 COLUMN 50                     VALUE
                   'F12 - SELECIONAR'.
           03      SC-TX-SAIR.
               05  LINE 23 COLUMN 68                     VALUE
                   'ESC - SAIR'.
      *
      ******************************************************************
       PROCEDURE                       DIVISION.
      ******************************************************************
      ******************************************************************
      *    ROTINA PRINCIPAL                                            *
      ******************************************************************
       RTPRINCIPAL                     SECTION.
      ******************************************************************
      *
           PERFORM RTINICIALIZA.
      *
           PERFORM RTPROCESSA.
      *
           PERFORM RTFINALIZA.
      *
      ******************************************************************
       RTPRINCIPAL-EXIT.               EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA DE INICIALIZACAO                                     *
      ******************************************************************
       RTINICIALIZA                    SECTION.
      ******************************************************************
      *
           DISPLAY SC-TELA.
      *
           MOVE 1                      TO WS-CT-OPCAO.
      *
           PERFORM RTSELECIONA.
      *
      ******************************************************************
       RTINICIALIZA-EXIT.              EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA DE INICIALIZACAO                                     *
      ******************************************************************
       RTPROCESSA                      SECTION.
      ******************************************************************
      *
           PERFORM                     UNTIL CN-ESC
               ACCEPT WS-OPCAO AT 2580 WITH AUTO UPDATE
               ACCEPT WS-TECLA         FROM ESCAPE KEY
               MOVE SPACES             TO WS-MSG
               EVALUATE TRUE
                   WHEN CN-BAIXO
                       ADD 1           TO WS-CT-OPCAO
                       IF WS-CT-OPCAO  GREATER 4
                           MOVE 1      TO WS-CT-OPCAO
                       END-IF
                       PERFORM RTSELECIONA
                   WHEN CN-PF12
                       PERFORM RTCHAMASUB
                   WHEN CN-CIMA
                       SUBTRACT 1      FROM WS-CT-OPCAO
                       IF WS-CT-OPCAO  LESS 1
                           MOVE 4      TO WS-CT-OPCAO
                       END-IF
                       PERFORM RTSELECIONA
               END-EVALUATE
           END-PERFORM.
      *
      ******************************************************************
       RTPROCESSA-EXIT.                EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA DE SELECAO                                           *
      ******************************************************************
       RTSELECIONA                     SECTION.
      ******************************************************************
      *
           DISPLAY SC-TELA.
      *
           EVALUATE WS-CT-OPCAO
               WHEN 1
                   DISPLAY '  INCLUSAO E ALTERACAO DE ATIVIDADES
      -            '          '
                                       WITH REVERSE-VIDEO AT 1014
               WHEN 2
                   DISPLAY '  EXCLUSAO DE ATIVIDADES
      -            '          '
                                       WITH REVERSE-VIDEO AT 1214
               WHEN 3
                   DISPLAY '  BAIXA DE ATIVIDADES
      -            '          '        WITH REVERSE-VIDEO AT 1414
               WHEN 4
                   DISPLAY '  GERAR MOVIMENTO MENSAL/ANUAL DE ATIVIDADES
      -            '          '        WITH REVERSE-VIDEO AT 1614
           END-EVALUATE.
      *
      ******************************************************************
       RTSELECIONA-EXIT.               EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA DE CHAMADA DOS SUBPROGRAMAS                          *
      ******************************************************************
       RTCHAMASUB                      SECTION.
      ******************************************************************
      *
           INITIALIZE WS-MSG.
      *
           EVALUATE WS-CT-OPCAO
               WHEN 1
                   CALL WS-ATV01       USING WS-MSG
               WHEN 2
                   CALL WS-ATV02       USING WS-MSG
               WHEN 3
                   CALL WS-ATV03       USING WS-MSG
               WHEN 4
                   CALL WS-ATV04       USING WS-MSG
           END-EVALUATE.
      *
           PERFORM RTSELECIONA.
      *
      ******************************************************************
       RTCHAMASUB-EXIT.                EXIT.
      ******************************************************************
      ******************************************************************
      *    ROTINA DE FINALIZACAO                                       *
      ******************************************************************
       RTFINALIZA                      SECTION.
      ******************************************************************
      *
           STOP RUN.
      *
      ******************************************************************
       RTFINALIZA-EXIT.                EXIT.
      ******************************************************************
      ******************************************************************
      *                         FIM DO PROGRAMA                        *
      ******************************************************************
