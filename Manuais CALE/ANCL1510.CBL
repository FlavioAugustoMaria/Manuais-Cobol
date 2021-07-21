      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. ANCL1510.
       AUTHOR.     RODRIGO F M STAEJAK.
      *================================================================*
      *             C P M  B R A X I S  -  S I S T E M A S             *
      *----------------------------------------------------------------*
      *    PROGRAMA....: ANCL1510                                      *
      *    PROGRAMADOR.: RODRIGO F M STAEJAK        - CPMBRAXIS / ALPHA*
      *    ANALISTA CPM: RODRIGO F M STAEJAK        - CPMBRAXIS / ALPHA*
      *    ANALISTA....: ANALICE B CHEFFER          - TI MELHORIAS     *
      *    DATA........: 09/10/2009                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: INCLUIR INTERVALOS DE FAIXA DE ANALISE DE     *
      *      CLIENTES NA TABELA ANCLB012.                              *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS.....:                                      *
      *    ANCLW520   BOOK DE ENTRADA DO SERVICO COORDENADOR.          *
      *    ANCLW525   BOOK DE SAIDA DO SERVICO COORDENADOR.            *
      *                                                                *
      *----------------------------------------------------------------*
      *    BOOKS ARQUITETURAIS..:                                      *
      *                                                                *
      *    I#FRWKAQ COMMAREA ENTRE PROGRAMA/FRAMEWORK                  *
      *    I#FRWK04 COMMAREA ENTRE PROGRAMA/FRWK1200(GAM)              *
      *    I#FRWKGE COMMAREA FRWK1999 (LOG DE ERRO)                    *
      *    I#FRWKHE COMMAREA FRWK1999 (AREA COMUM ERROS)               *
      *    I#FRWKCI COMMAREA FRWK1999 (LOG DE ERROS CICS)              *
      *    I#FRWKMD COMMAREA FRWK1999 (LOG DE ERROS MODULO)            *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    FRWK1999 - PROCEDIMENTOS PARA GRAVACAO DE LOGS DE ERRO      *
      *    FRWK1200 - MODULO DE ACESSO AO GAM                          *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                  PIC  X(050)             VALUE
           '*** INICIO DA WORKING ANCL1510 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROES.
       77  IND-2                       PIC  9(003) COMP-3  VALUE ZEROES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       01  WRK-VARIAVEIS-AUXILIARES.
           05  WRK-FRWK1999            PIC  X(008)     VALUE 'FRWK1999'.
           05  WRK-FRWK1200            PIC  X(008)     VALUE 'FRWK1200'.
           05  WRK-PROGRAMA            PIC  X(008)     VALUE 'ANCL1510'.
           05  WRK-CALE1000            PIC  X(008)     VALUE 'CALE1000'.
           05  WRK-FIM-CURSOR          PIC  X(001)     VALUE SPACES.
           05  WRK-TRATAMENTO-ERROS.
              10  WRK-BLOCO-RETORNO.
                 15 WRK-COD-RETORNO    PIC  9(002)         VALUE ZEROES.
                 15 WRK-COD-ERRO       PIC  X(004)         VALUE SPACES.
                 15 WRK-COD-MENSAGEM   PIC  X(008)         VALUE SPACES.

       01  WRK-QTDE-FIM-AUX            PIC  9(013) COMP-3  VALUE ZEROES.
       01  WRK-CINTVL-FAIXA-AUX        PIC  9(003) COMP-3  VALUE ZEROES.
       01  WRK-PERC-FIM-AUX            PIC  9(003)V9(006)
                                                   COMP-3  VALUE ZEROES.
       01  WRK-VLOR-FIM-AUX            PIC  9(013)V99
                                                   COMP-3  VALUE ZEROES.
       01  WRK-CINTVL-FAIXA-ANLSE      PIC S9(003) COMP-3  VALUE ZEROES.
       01  WRK-DINIC-VGCIA-FAIXA       PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                  PIC X(050)             VALUE
           'AREA PARA VARIAVEIS DE DATA'.
      *----------------------------------------------------------------*

       01  WRK-DATA-AUX.
           05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROES.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROES.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROES.

       01  WRK-DATA-BOOK               PIC  9(008)         VALUE ZEROES.
       01  FILLER                      REDEFINES WRK-DATA-BOOK.
           05  WRK-ANO-BOOK            PIC  9(004).
           05  WRK-MES-BOOK            PIC  9(002).
           05  WRK-DIA-BOOK            PIC  9(002).

       01  WRK-DATA-INCLUSAO           PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                  PIC X(050)             VALUE
           'AREA PARA VARIAVEIS DE DATA'.
      *----------------------------------------------------------------*

       01  WRK-ANCLB012-NULL.
           05 WRK-CUSUAR-INCL-NULL     PIC S9(004) COMP    VALUE ZEROES.
           05 WRK-CUSUAR-MANUT-NULL    PIC S9(004) COMP    VALUE ZEROES.
           05 WRK-CUSUAR-VALDC-NULL    PIC S9(004) COMP    VALUE ZEROES.
           05 WRK-HINCL-REG-NULL       PIC S9(004) COMP    VALUE ZEROES.
           05 WRK-HMANUT-REG-NULL      PIC S9(004) COMP    VALUE ZEROES.
           05 WRK-HVALDC-REG-NULL      PIC S9(004) COMP    VALUE ZEROES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           'AREA DE COMUNICACAO DO PROGRAMA FRWK1999'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO.                                               
           COPY 'I#FRWKGE'.
           05  WRK-BLOCO-INFO-ERRO.                                     
               10 WRK-CHAR-INFO-ERRO PIC X(01) OCCURS 0 TO 30000 TIMES  
                                      DEPENDING ON FRWKGHEA-TAM-DADOS.  

      *----------------------------------------------------------------*
       01  FILLER                  PIC X(050)             VALUE         
           'AREA DE TRATAMENTO DE ERRO MODULO (API-ERRO)'.              
      *----------------------------------------------------------------*
       01  WRK-COPY-MOD.                                                
           COPY 'I#FRWKMD'.

      *----------------------------------------------------------------*
       01  FILLER                  PIC X(050)             VALUE         
           'AREA DE TRATAMENTO DE ERRO CICS(API-ERRO)'.                 
      *----------------------------------------------------------------*
       01  WRK-COPY-CICS.
           COPY 'I#FRWKCI'.

      *----------------------------------------------------------------*
       01  FILLER                  PIC X(050)             VALUE
           'AREA DE TRATAMENTO DE ERRO DB2  (API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-DB2.
           COPY 'I#FRWKDB'.

      *----------------------------------------------------------------*
       01  FILLER                  PIC X(050)             VALUE
           'AREA PARA BUSCAR DADOS DE SESSAO'.
      *----------------------------------------------------------------*
       01  WRK-AREA-SESSAO.
           COPY 'FRWKWAAA'.

      *----------------------------------------------------------------*
       01  FILLER                  PIC X(050)             VALUE         
           'AREA DE TRATAMENTO DE INTERFACE (GAM)'.                     
      *----------------------------------------------------------------*
       01  WRK-AREA-FRWK1200.                                           
           COPY 'I#FRWK04'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE COMUNICACAO COM O CALE1000'.
      *----------------------------------------------------------------*
       COPY 'I#CALE01'.

      *----------------------------------------------------------------*
       01  FILLER                  PIC X(050)             VALUE
           'AREA DE INTERFACE PARA DADOS DE ENTRADA'.
      *----------------------------------------------------------------*
       01  WRK-ANCLW520.
           COPY ANCLW520.

      *----------------------------------------------------------------*
       01  FILLER                  PIC X(050)             VALUE
           'AREA DE INTERFACE PARA DADOS DE SAIDA'.
      *----------------------------------------------------------------*
       01  WRK-ANCLW525.
           COPY ANCLW525.

      *----------------------------------------------------------------*
       01  FILLER                  PIC X(050)             VALUE
           'AREA PARA DB2'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE ANCLB011
           END-EXEC.

           EXEC SQL
               INCLUDE ANCLB012
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                  PIC X(050)             VALUE
           'AREA PARA CURSORES'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-ANCLB012 CURSOR FOR
               SELECT
                   CINTVL_FAIXA_ANLSE,
                   DINIC_VGCIA_FAIXA
             FROM  DB2PRD.TINTVL_FAIXA_ANLSE
             WHERE NFAIXA_ANLSE_CLI   = :ANCLB012.NFAIXA-ANLSE-CLI
               AND CINTVL_FAIXA_ANLSE > :ANCLB012.CINTVL-FAIXA-ANLSE
               AND DFIM_VGCIA_FAIXA   = :ANCLB012.DFIM-VGCIA-FAIXA
             ORDER BY CINTVL_FAIXA_ANLSE
             FETCH FIRST 101 ROWS ONLY
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                        PIC X(050)        VALUE
           '***  FIM DA WORKING ANCL1510 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           COPY I#FRWKAQ.

      *================================================================*
       PROCEDURE                       DIVISION  USING  DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA.                               *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR

           PERFORM 2000-PROCESSAR

           PERFORM 3000-FINALIZAR
           .

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE INICIALIZACAO DO PROGRAMA.                        *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  FRWKGHEA-REGISTRO
                       FRWKGCIC-REGISTRO
                       FRWKGMOD-REGISTRO
                       FRWKGDB2-REGISTRO

           PERFORM 6000-OBTER-DADOS-ENTRADA

           PERFORM 1200-CONSISTIR-DADOS-ENTRADA

           PERFORM 1300-CONSISTIR-DADOS-LOGICO

           PERFORM 6100-RECUPERA-DADOS-SESSAO
           .

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSISITENCIA DOS DADOS DE ENTRADA FISICAMENTE. *
      *----------------------------------------------------------------*
       1200-CONSISTIR-DADOS-ENTRADA    SECTION.
      *----------------------------------------------------------------*

           IF (ANCLW520-NFAIXA-ANLSE-CLI NOT NUMERIC) OR
              (ANCLW520-NFAIXA-ANLSE-CLI EQUAL ZEROS)
               MOVE 08                   TO FRWKGLAQ-COD-RETORNO
               MOVE '0010'               TO FRWKGLAQ-COD-ERRO
               MOVE 'ANCL0002'           TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF

           IF (ANCLW520-CINDCD-TPO-FAIXA NOT EQUAL 'P' AND 'Q' AND 'V')
               MOVE 08                   TO FRWKGLAQ-COD-RETORNO
               MOVE '0020'               TO FRWKGLAQ-COD-ERRO
               MOVE 'ANCL0017'           TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF

           IF (ANCLW520-NUM-LINHAS       NOT NUMERIC) OR
              (ANCLW520-NUM-LINHAS       EQUAL ZEROS) OR
              (ANCLW520-NUM-LINHAS       GREATER 100)
               MOVE 08                   TO FRWKGLAQ-COD-RETORNO
               MOVE '0030'               TO FRWKGLAQ-COD-ERRO
               MOVE 'ANCL0002'           TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF

           IF (ANCLW520-DINIC-VGCIA-FAIXA EQUAL SPACES OR LOW-VALUES)
               MOVE 08                    TO FRWKGLAQ-COD-RETORNO
               MOVE '0040'                TO FRWKGLAQ-COD-ERRO
               MOVE 'ANCL0023'            TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           ELSE
               INSPECT ANCLW520-DINIC-VGCIA-FAIXA
                                          REPLACING ALL '/' BY '.'
               MOVE ANCLW520-DINIC-VGCIA-FAIXA
                                          TO CALE01-VLR-ARGUMENTO-INI
               PERFORM 1210-CONSISTIR-DATA
               IF  CALE01-CONSIST-DT-HORA EQUAL 'I'
                   MOVE 08                TO FRWKGLAQ-COD-RETORNO
                   MOVE '0050'            TO FRWKGLAQ-COD-ERRO
                   MOVE 'ANCL0023'        TO FRWKGLAQ-COD-MENSAGEM
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF

           PERFORM VARYING IND-1       FROM 1 BY 1 UNTIL
              (IND-1                   GREATER ANCLW520-NUM-LINHAS)

               IF (ANCLW520-CINTVL-FAIXA-ANLSE(IND-1)
                                               NOT NUMERIC)  OR
                  (ANCLW520-CINTVL-FAIXA-ANLSE(IND-1)
                                               EQUAL ZEROS)
                   MOVE 08                     TO FRWKGLAQ-COD-RETORNO
                   MOVE '0060'                 TO FRWKGLAQ-COD-ERRO
                   MOVE 'ANCL0002'             TO FRWKGLAQ-COD-MENSAGEM
                   PERFORM 3000-FINALIZAR
               END-IF

               IF (ANCLW520-RINTVL-FAIXA-ANLSE(IND-1)
                                             EQUAL SPACES OR LOW-VALUES)
                   MOVE 08                     TO FRWKGLAQ-COD-RETORNO
                   MOVE '0070'                 TO FRWKGLAQ-COD-ERRO
                   MOVE 'ANCL0002'             TO FRWKGLAQ-COD-MENSAGEM
                   PERFORM 3000-FINALIZAR
               END-IF

               EVALUATE TRUE
                   WHEN ANCLW520-CINDCD-TPO-FAIXA  EQUAL 'Q'

                    IF  ANCLW520-QINIC-FAIXA-ANLSE(IND-1)
                                           NOT NUMERIC OR
                        ANCLW520-QINIC-FAIXA-ANLSE(IND-1)
                                           EQUAL ZEROS
                        MOVE 08            TO FRWKGLAQ-COD-RETORNO
                        MOVE '0080'        TO FRWKGLAQ-COD-ERRO
                        MOVE 'ANCL0002'    TO FRWKGLAQ-COD-MENSAGEM
                        PERFORM 3000-FINALIZAR
                    END-IF
                    IF  ANCLW520-QFNAL-FAIXA-ANLSE(IND-1)
                                           NOT NUMERIC OR
                        ANCLW520-QFNAL-FAIXA-ANLSE(IND-1)
                                           EQUAL ZEROS
                        MOVE 08            TO FRWKGLAQ-COD-RETORNO
                        MOVE '0090'        TO FRWKGLAQ-COD-ERRO
                        MOVE 'ANCL0002'    TO FRWKGLAQ-COD-MENSAGEM
                        PERFORM 3000-FINALIZAR
                    END-IF

                   WHEN ANCLW520-CINDCD-TPO-FAIXA  EQUAL 'P'

                    IF  ANCLW520-PINIC-FAIXA-ANLSE(IND-1)
                                           NOT NUMERIC OR
                        ANCLW520-PINIC-FAIXA-ANLSE(IND-1)
                                           EQUAL ZEROS
                        MOVE 08            TO FRWKGLAQ-COD-RETORNO
                        MOVE '0100'        TO FRWKGLAQ-COD-ERRO
                        MOVE 'ANCL0002'    TO FRWKGLAQ-COD-MENSAGEM
                        PERFORM 3000-FINALIZAR
                    END-IF
                    IF  ANCLW520-PFNAL-FAIXA-ANLSE(IND-1)
                                           NOT NUMERIC OR
                        ANCLW520-PFNAL-FAIXA-ANLSE(IND-1)
                                           EQUAL ZEROS
                        MOVE 08            TO FRWKGLAQ-COD-RETORNO
                        MOVE '0110'        TO FRWKGLAQ-COD-ERRO
                        MOVE 'ANCL0002'    TO FRWKGLAQ-COD-MENSAGEM
                        PERFORM 3000-FINALIZAR
                    END-IF

                   WHEN ANCLW520-CINDCD-TPO-FAIXA  EQUAL 'V'

                    IF  ANCLW520-VINIC-FAIXA-ANLSE(IND-1)
                                           NOT NUMERIC OR
                        ANCLW520-VINIC-FAIXA-ANLSE(IND-1)
                                           EQUAL ZEROS
                        MOVE 08            TO FRWKGLAQ-COD-RETORNO
                        MOVE '0120'        TO FRWKGLAQ-COD-ERRO
                        MOVE 'ANCL0002'    TO FRWKGLAQ-COD-MENSAGEM
                        PERFORM 3000-FINALIZAR
                    END-IF
                    IF  ANCLW520-VFNAL-FAIXA-ANLSE(IND-1)
                                           NOT NUMERIC OR
                        ANCLW520-VFNAL-FAIXA-ANLSE(IND-1)
                                           EQUAL ZEROS
                        MOVE 08            TO FRWKGLAQ-COD-RETORNO
                        MOVE '0130'        TO FRWKGLAQ-COD-ERRO
                        MOVE 'ANCL0002'    TO FRWKGLAQ-COD-MENSAGEM
                        PERFORM 3000-FINALIZAR
                    END-IF
               END-EVALUATE
           END-PERFORM
           .
      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSITENCIA DE DATA.                            *
      *----------------------------------------------------------------*
       1210-CONSISTIR-DATA             SECTION.
      *----------------------------------------------------------------*

           MOVE 'F2'                   TO CALE01-FUNCAO
           MOVE 'SF2001'               TO CALE01-SUB-FUNCAO
           MOVE 007                    TO CALE01-FORMATO-ARGUMENTO-INI

           PERFORM 5100-CHAMAR-CALE1000
           .

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSISITENCIA DOS DADOS DE ENTRADA LOGICAMENTE. *
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS-LOGICO     SECTION.
      *----------------------------------------------------------------*

           PERFORM 1310-OBTER-DATA-ATUAL

           MOVE ANCLW520-DINIC-VGCIA-FAIXA
                                       TO WRK-DATA-AUX
           MOVE WRK-DIA-AUX            TO WRK-DIA-BOOK
           MOVE WRK-MES-AUX            TO WRK-MES-BOOK
           MOVE WRK-ANO-AUX            TO WRK-ANO-BOOK

           IF  WRK-DATA-BOOK           NOT GREATER
                                       CALE01-DT-GREGO-AAAAMMDD
               MOVE 08                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0140'             TO FRWKGLAQ-COD-ERRO
               MOVE 'ANCL0002'         TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF

           MOVE 1                      TO IND-1
           MOVE ZEROS                  TO IND-2

           PERFORM UNTIL
              (IND-1                   EQUAL ANCLW520-NUM-LINHAS)
               COMPUTE IND-2   = IND-1 + 1
               MOVE ANCLW520-CINTVL-FAIXA-ANLSE(IND-1)
                                       TO WRK-CINTVL-FAIXA-AUX
               COMPUTE WRK-CINTVL-FAIXA-AUX = (WRK-CINTVL-FAIXA-AUX + 1)
               IF  ANCLW520-CINTVL-FAIXA-ANLSE(IND-2)
                                       NOT EQUAL WRK-CINTVL-FAIXA-AUX
                   MOVE 08             TO FRWKGLAQ-COD-RETORNO
                   MOVE '0150'         TO FRWKGLAQ-COD-ERRO
                   MOVE 'ANCL0026'     TO FRWKGLAQ-COD-MENSAGEM
                   PERFORM 3000-FINALIZAR
               END-IF
               ADD 1                   TO IND-1
           END-PERFORM

           EVALUATE ANCLW520-CINDCD-TPO-FAIXA
             WHEN 'Q'
               PERFORM 1320-CONSISITIR-QUANTIDADE

             WHEN 'P'
               PERFORM 1330-CONSISITIR-PERCENTUAL

             WHEN 'V'
               PERFORM 1340-CONSISITIR-VALOR

           END-EVALUATE
           .
      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA OBTER A DATA DO DIA.                            *
      *----------------------------------------------------------------*
       1310-OBTER-DATA-ATUAL           SECTION.
      *----------------------------------------------------------------*

           MOVE 'F3'                   TO CALE01-FUNCAO
           MOVE 'SF3002'               TO CALE01-SUB-FUNCAO
           MOVE ZEROS                  TO CALE01-COD-IDIOMA
                                          CALE01-COD-LOCALIDADE

           PERFORM 5100-CHAMAR-CALE1000
           .

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSISTIR QUANTIDADE.                           *
      *----------------------------------------------------------------*
       1320-CONSISITIR-QUANTIDADE      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1
           MOVE ZEROS                  TO IND-2

           PERFORM UNTIL
              (IND-1                   EQUAL ANCLW520-NUM-LINHAS)
               COMPUTE IND-2   = IND-1 + 1
               MOVE ANCLW520-QFNAL-FAIXA-ANLSE(IND-1)
                                       TO WRK-QTDE-FIM-AUX
               COMPUTE WRK-QTDE-FIM-AUX = (WRK-QTDE-FIM-AUX + 1)
               IF  ANCLW520-QINIC-FAIXA-ANLSE(IND-2)
                                       NOT EQUAL WRK-QTDE-FIM-AUX
                   MOVE 08             TO FRWKGLAQ-COD-RETORNO
                   MOVE '0160'         TO FRWKGLAQ-COD-ERRO
                   MOVE 'ANCL0026'     TO FRWKGLAQ-COD-MENSAGEM
                   PERFORM 3000-FINALIZAR
               END-IF
               ADD 1                   TO IND-1
           END-PERFORM
           .

      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSISTIR PERCENTUAL.                           *
      *----------------------------------------------------------------*
       1330-CONSISITIR-PERCENTUAL      SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1
           MOVE ZEROS                  TO IND-2

           PERFORM UNTIL
              (IND-1                   EQUAL ANCLW520-NUM-LINHAS)
               COMPUTE IND-2   = IND-1 + 1
               MOVE ANCLW520-PFNAL-FAIXA-ANLSE(IND-1)
                                       TO WRK-PERC-FIM-AUX
               COMPUTE WRK-PERC-FIM-AUX = (WRK-PERC-FIM-AUX + 0,000001)
               IF  ANCLW520-PINIC-FAIXA-ANLSE(IND-2)
                                       NOT EQUAL WRK-PERC-FIM-AUX
                   MOVE 08             TO FRWKGLAQ-COD-RETORNO
                   MOVE '0170'         TO FRWKGLAQ-COD-ERRO
                   MOVE 'ANCL0026'     TO FRWKGLAQ-COD-MENSAGEM
                   PERFORM 3000-FINALIZAR
               END-IF
               ADD 1                   TO IND-1
           END-PERFORM
           .

      *----------------------------------------------------------------*
       1330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CONSISTIR VALOR.                                *
      *----------------------------------------------------------------*
       1340-CONSISITIR-VALOR           SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1
           MOVE ZEROS                  TO IND-2

           PERFORM UNTIL
              (IND-1                   EQUAL ANCLW520-NUM-LINHAS)
               COMPUTE IND-2   = IND-1 + 1
               MOVE ANCLW520-VFNAL-FAIXA-ANLSE(IND-1)
                                       TO WRK-VLOR-FIM-AUX
               COMPUTE WRK-VLOR-FIM-AUX = (WRK-VLOR-FIM-AUX + 0,01)
               IF  ANCLW520-VINIC-FAIXA-ANLSE(IND-2)
                                       NOT EQUAL WRK-VLOR-FIM-AUX
                   MOVE 08             TO FRWKGLAQ-COD-RETORNO
                   MOVE '0180'         TO FRWKGLAQ-COD-ERRO
                   MOVE 'ANCL0026'     TO FRWKGLAQ-COD-MENSAGEM
                   PERFORM 3000-FINALIZAR
               END-IF
               ADD 1                   TO IND-1
           END-PERFORM
           .

      *----------------------------------------------------------------*
       1340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO CENTRAL DO PROGRAMA.                   *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-TRATAR-ATUALIZACAO

           PERFORM VARYING IND-1       FROM 1 BY 1 UNTIL
              (IND-1                   GREATER ANCLW520-NUM-LINHAS)
               PERFORM 2200-TRATAR-INCLUSAO
           END-PERFORM

           MOVE ANCLW520-NFAIXA-ANLSE-CLI
                                       TO ANCLW525-S-NFAIXA-ANLSE-CLI
           MOVE ANCLW520-CINDCD-TPO-FAIXA
                                       TO ANCLW525-S-CINDCD-TPO-FAIXA
           MOVE ANCLW520-DINIC-VGCIA-FAIXA
                                       TO ANCLW525-S-DINIC-VGCIA-FAIXA

           PERFORM 6200-INFORMAR-BLOCO-SAIDA

           MOVE ZEROS                  TO FRWKGLAQ-COD-RETORNO
           MOVE '0000'                 TO FRWKGLAQ-COD-ERRO
           MOVE 'ANCL0022'             TO FRWKGLAQ-COD-MENSAGEM
           .

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR ATUALIZACAO NA TABELA ANCLB012.          *
      *----------------------------------------------------------------*
       2100-TRATAR-ATUALIZACAO         SECTION.
      *----------------------------------------------------------------*

           PERFORM 2110-OPEN-CURSOR

           PERFORM 2120-FETCH-CURSOR

           PERFORM 2130-SUBTRAIR-DATA

           PERFORM VARYING IND-1       FROM 1 BY 1 UNTIL
              (IND-1                   GREATER ANCLW520-NUM-LINHAS) OR
              (WRK-FIM-CURSOR          EQUAL 'S')
               PERFORM 2140-ATUALIZAR-REGISTRO
               PERFORM 2120-FETCH-CURSOR
           END-PERFORM

           PERFORM 2150-CLOSE-CURSOR

           .
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ABRIR O CURSOR DA TABELA ANCLB012.              *
      *----------------------------------------------------------------*
       2110-OPEN-CURSOR                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ANCLB012

           MOVE 'N'                    TO WRK-FIM-CURSOR
           MOVE ANCLW520-NFAIXA-ANLSE-CLI
                                       TO NFAIXA-ANLSE-CLI   OF ANCLB012
           MOVE ZEROS                  TO CINTVL-FAIXA-ANLSE OF ANCLB012
           MOVE '31.12.2100'           TO DFIM-VGCIA-FAIXA   OF ANCLB012

           EXEC SQL
               OPEN CSR01-ANCLB012
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-OPEN            TO TRUE
               MOVE '0200'             TO FRWKGLAQ-COD-ERRO
               MOVE 'DB2PRD.TINTVL_FAIXA_ANLSE'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2110-OPEN-CURSOR' TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE 'ANCL0004'         TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 9100-ERRO-DB2
           END-IF
           .

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA LER O CURSOR DA TABELA ANCLB012.                *
      *----------------------------------------------------------------*
       2120-FETCH-CURSOR               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ANCLB012

           EXEC SQL
             FETCH CSR01-ANCLB012
              INTO :ANCLB012.CINTVL-FAIXA-ANLSE
                  ,:ANCLB012.DINIC-VGCIA-FAIXA
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               SET DB2-FETCH            TO TRUE
               MOVE '0210'              TO FRWKGLAQ-COD-ERRO
               MOVE 'DB2PRD.TINTVL_FAIXA_ANLSE'
                                        TO FRWKGDB2-NOME-TABELA
               MOVE '2120-FETCH-CURSOR' TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE 'ANCL0016'          TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 9100-ERRO-DB2
           END-IF

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
           ELSE
               MOVE CINTVL-FAIXA-ANLSE OF ANCLB012
                                       TO WRK-CINTVL-FAIXA-ANLSE
               MOVE DINIC-VGCIA-FAIXA  OF ANCLB012
                                       TO WRK-DINIC-VGCIA-FAIXA
           END-IF
           .

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA SUBTRAIR UM DIA DA DATA.                        *
      *----------------------------------------------------------------*
       2130-SUBTRAIR-DATA              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CALE01-REGISTRO

           MOVE 'F3'                   TO CALE01-FUNCAO
           MOVE 'SF3006'               TO CALE01-SUB-FUNCAO
           MOVE 7                      TO CALE01-FORMATO-ARGUMENTO-INI
           INSPECT ANCLW520-DINIC-VGCIA-FAIXA
                                       REPLACING ALL '/' BY '.'
           MOVE ANCLW520-DINIC-VGCIA-FAIXA
                                       TO CALE01-VLR-ARGUMENTO-INI
           MOVE 1                      TO CALE01-VLR-INCREMENTO-CALC
           MOVE 'N'                    TO CALE01-SINAL-INCREMENTO-CALC

           PERFORM 5100-CHAMAR-CALE1000

           MOVE CALE01-DT-GREGO-P-DDMMAAAA
                                       TO WRK-DATA-INCLUSAO
           .

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA ATUALIZAR REGISTROS.                            *
      *----------------------------------------------------------------*
       2140-ATUALIZAR-REGISTRO         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-ANCLB012-NULL

           MOVE ANCLW520-NFAIXA-ANLSE-CLI
                                       TO NFAIXA-ANLSE-CLI   OF ANCLB012
           MOVE WRK-DATA-INCLUSAO      TO DFIM-VGCIA-FAIXA   OF ANCLB012
           MOVE FRWKWAAA-CAUTEN-SEGRC(1:9)
                                       TO CUSUAR-MANUT       OF ANCLB012

           EXEC SQL
             UPDATE DB2PRD.TINTVL_FAIXA_ANLSE
               SET DFIM_VGCIA_FAIXA   = :ANCLB012.DFIM-VGCIA-FAIXA,
                   CUSUAR_MANUT       = :ANCLB012.CUSUAR-MANUT
                                          :WRK-CUSUAR-MANUT-NULL,
                   HMANUT_REG         = CURRENT TIMESTAMP
             WHERE NFAIXA_ANLSE_CLI   = :ANCLB012.NFAIXA-ANLSE-CLI
              AND  CINTVL_FAIXA_ANLSE = :WRK-CINTVL-FAIXA-ANLSE
              AND  DINIC_VGCIA_FAIXA  = :WRK-DINIC-VGCIA-FAIXA
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               SET DB2-UPDATE           TO TRUE
               MOVE '0220'              TO FRWKGLAQ-COD-ERRO
               MOVE 'DB2PRD.TINTVL_FAIXA_ANLSE'
                                        TO FRWKGDB2-NOME-TABELA
               MOVE '2140-ATUALIZAR-REGISTRO'
                                        TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE 'ANCL0016'          TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 9100-ERRO-DB2
           END-IF
           .

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA FECHAR O CURSOR DA TABELA ANCLB012.             *
      *----------------------------------------------------------------*
       2150-CLOSE-CURSOR               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-ANCLB012
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               SET DB2-CLOSE            TO TRUE
               MOVE '0230'              TO FRWKGLAQ-COD-ERRO
               MOVE 'DB2PRD.TINTVL_FAIXA_ANLSE'
                                        TO FRWKGDB2-NOME-TABELA
               MOVE '2140-CLOSE-CURSOR' TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE 'ANCL0004'          TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 9100-ERRO-DB2
           END-IF
           .

      *----------------------------------------------------------------*
       2150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAR INCLUSAO DE FAIXA DE ANALISE.            *
      *----------------------------------------------------------------*
       2200-TRATAR-INCLUSAO            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-ANCLB012-NULL
                      ANCLB012

           MOVE ANCLW520-NFAIXA-ANLSE-CLI
                                       TO NFAIXA-ANLSE-CLI   OF ANCLB012
           MOVE ANCLW520-DINIC-VGCIA-FAIXA
                                       TO DINIC-VGCIA-FAIXA  OF ANCLB012
           MOVE '31.12.2100'           TO DFIM-VGCIA-FAIXA   OF ANCLB012
           MOVE ANCLW520-CINTVL-FAIXA-ANLSE(IND-1)
                                       TO CINTVL-FAIXA-ANLSE OF ANCLB012
           MOVE ANCLW520-RINTVL-FAIXA-ANLSE(IND-1)
                                       TO RINTVL-FAIXA-ANLSE OF ANCLB012

           EVALUATE ANCLW520-CINDCD-TPO-FAIXA
             WHEN 'Q'
               MOVE ANCLW520-QINIC-FAIXA-ANLSE(IND-1)
                                       TO QINIC-FAIXA-ANLSE  OF ANCLB012
               MOVE ANCLW520-QFNAL-FAIXA-ANLSE(IND-1)
                                       TO QFNAL-FAIXA-ANLSE  OF ANCLB012
             WHEN 'P'
               MOVE ANCLW520-PINIC-FAIXA-ANLSE(IND-1)
                                       TO PINIC-FAIXA-ANLSE  OF ANCLB012
               MOVE ANCLW520-PFNAL-FAIXA-ANLSE(IND-1)
                                       TO PFNAL-FAIXA-ANLSE  OF ANCLB012
             WHEN 'V'
               MOVE ANCLW520-VINIC-FAIXA-ANLSE(IND-1)
                                       TO VINIC-FAIXA-ANLSE  OF ANCLB012
               MOVE ANCLW520-VFNAL-FAIXA-ANLSE(IND-1)
                                       TO VFNAL-FAIXA-ANLSE  OF ANCLB012
           END-EVALUATE

           MOVE FRWKWAAA-CAUTEN-SEGRC(1:9)
                                       TO CUSUAR-INCL        OF ANCLB012

           MOVE -1                     TO WRK-CUSUAR-MANUT-NULL
                                          WRK-CUSUAR-VALDC-NULL
                                          WRK-HMANUT-REG-NULL
                                          WRK-HVALDC-REG-NULL

           EXEC SQL
             INSERT INTO DB2PRD.TINTVL_FAIXA_ANLSE
                  (NFAIXA_ANLSE_CLI,
                   CINTVL_FAIXA_ANLSE,
                   DINIC_VGCIA_FAIXA,
                   DFIM_VGCIA_FAIXA,
                   RINTVL_FAIXA_ANLSE,
                   QINIC_FAIXA_ANLSE,
                   QFNAL_FAIXA_ANLSE,
                   PINIC_FAIXA_ANLSE,
                   PFNAL_FAIXA_ANLSE,
                   VINIC_FAIXA_ANLSE,
                   VFNAL_FAIXA_ANLSE,
                   CUSUAR_INCL,
                   CUSUAR_MANUT,
                   CUSUAR_VALDC,
                   HINCL_REG,
                   HMANUT_REG,
                   HVALDC_REG)
               VALUES
                  (:ANCLB012.NFAIXA-ANLSE-CLI,
                   :ANCLB012.CINTVL-FAIXA-ANLSE,
                   :ANCLB012.DINIC-VGCIA-FAIXA,
                   :ANCLB012.DFIM-VGCIA-FAIXA,
                   :ANCLB012.RINTVL-FAIXA-ANLSE,
                   :ANCLB012.QINIC-FAIXA-ANLSE,
                   :ANCLB012.QFNAL-FAIXA-ANLSE,
                   :ANCLB012.PINIC-FAIXA-ANLSE,
                   :ANCLB012.PFNAL-FAIXA-ANLSE,
                   :ANCLB012.VINIC-FAIXA-ANLSE,
                   :ANCLB012.VFNAL-FAIXA-ANLSE,
                   :ANCLB012.CUSUAR-INCL
                     :WRK-CUSUAR-INCL-NULL,
                   :ANCLB012.CUSUAR-MANUT
                     :WRK-CUSUAR-MANUT-NULL,
                   :ANCLB012.CUSUAR-VALDC
                     :WRK-CUSUAR-VALDC-NULL,
                   CURRENT TIMESTAMP,
                   :ANCLB012.HMANUT-REG
                     :WRK-HMANUT-REG-NULL,
                   :ANCLB012.HVALDC-REG
                     :WRK-HVALDC-REG-NULL)
           END-EXEC

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               SET DB2-INSERT           TO TRUE
               MOVE '0240'              TO FRWKGLAQ-COD-ERRO
               MOVE 'DB2PRD.TINTVL_FAIXA_ANLSE'
                                        TO FRWKGDB2-NOME-TABELA
               MOVE '2200-TRATAR-INCLUSAO'
                                        TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE 'ANCL0021'          TO FRWKGLAQ-COD-MENSAGEM
               PERFORM 9100-ERRO-DB2
           END-IF
           .

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA FINALIZAR O PROCESSAMENTO.                         *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           EXEC CICS
               RETURN
           END-EXEC
           .

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    EFETUAR CHAMADA AO MODULO FRWK1200 - GAM                    *
      *----------------------------------------------------------------*
       5000-ACESSAR-GAM                SECTION.                         
      *----------------------------------------------------------------*
          
           EXEC CICS LINK
                PROGRAM  (WRK-FRWK1200)
                COMMAREA (WRK-AREA-FRWK1200)
                LENGTH   (LENGTH OF WRK-AREA-FRWK1200)
                NOHANDLE
           END-EXEC

           IF  EIBRESP                     NOT EQUAL DFHRESP(NORMAL)
               MOVE 16                     TO FRWKGLAQ-COD-RETORNO
               MOVE '0250'                 TO FRWKGLAQ-COD-ERRO
               MOVE 'ANCL0009'             TO FRWKGLAQ-COD-MENSAGEM
               MOVE '5000-ACESSAR-GAM'     TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF

           IF  FRWKEL04-COD-RETORNO        NOT EQUAL ZEROS
               MOVE 16                     TO FRWKGLAQ-COD-RETORNO
               MOVE '0260'                 TO FRWKGLAQ-COD-ERRO
               MOVE FRWKEL04-COD-MENSAGEM  TO FRWKGLAQ-COD-MENSAGEM
               MOVE WRK-FRWK1200           TO FRWKGMOD-NOME-MODULO
               MOVE FRWKEL04-BLOCO-RETORNO TO WRK-BLOCO-RETORNO
               MOVE '5000-ACESSAR-GAM'     TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9300-ERRO-MODULO
           END-IF
           .

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA CHAMAR O MODULO CALE1000.                       *
      *----------------------------------------------------------------*
       5100-CHAMAR-CALE1000            SECTION.
      *----------------------------------------------------------------*

           MOVE 'CALE0001'             TO CALE01-ID-BLOCO
           MOVE LENGTH                 OF CALE01-REGISTRO
                                       TO CALE01-TAM-BLOCO

           EXEC CICS LINK
                PROGRAM  (WRK-CALE1000)
                COMMAREA (CALE01-REGISTRO)
                LENGTH   (LENGTH OF CALE01-REGISTRO)
                NOHANDLE
           END-EXEC

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE  16                TO FRWKGLAQ-COD-RETORNO
               MOVE '0270'             TO FRWKGLAQ-COD-ERRO
               MOVE 'ANCL0009'         TO FRWKGLAQ-COD-MENSAGEM
               MOVE '5100-CHAMAR-CALE1000'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF

           EVALUATE CALE01-COD-RETORNO
             WHEN ZEROS
             WHEN 01
             WHEN 08
               CONTINUE

             WHEN OTHER
               MOVE CALE01-COD-RETORNO TO FRWKGLAQ-COD-RETORNO
               MOVE CALE01-COD-ERRO    TO FRWKGLAQ-COD-ERRO
               MOVE CALE01-COD-MENSAGEM-GMSG
                                       TO FRWKGLAQ-COD-MENSAGEM
               MOVE WRK-CALE1000       TO FRWKGMOD-NOME-MODULO
               MOVE '5100-CHAMAR-CALE1000'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9300-ERRO-MODULO
           END-EVALUATE
           .

      *----------------------------------------------------------------*
       5100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA OBTER OS DADOS DE ENTRADA.                      *
      *----------------------------------------------------------------*
       6000-OBTER-DADOS-ENTRADA        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO

           SET FRWKEL04-FUN-CONSULTAR  TO TRUE

           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE ANCLW520-COD-LAYOUT    TO FRWKEL04-ID-BLOCO
           MOVE ANCLW520-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO

           PERFORM 5000-ACESSAR-GAM

           MOVE FRWKEL04-BUFFER(1:FRWKEL04-TAM-BLOCO)
                                       TO WRK-ANCLW520
           .

      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA RECUPERAR DADOS DE SESSAO.                      *
      *----------------------------------------------------------------*
       6100-RECUPERA-DADOS-SESSAO      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO

           SET FRWKEL04-FUN-CONSULTAR  TO  TRUE
           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO  FRWKEL04-IDENT-AREA-GLOBAL
           MOVE 'FRWKEL83'             TO  FRWKEL04-ID-BLOCO
           MOVE FRWKWAAA-TAM-LAYOUT    TO  FRWKEL04-TAM-BLOCO

           PERFORM 5000-ACESSAR-GAM

           MOVE FRWKEL04-BUFFER(1:FRWKEL04-TAM-BLOCO)
                                       TO  WRK-AREA-SESSAO
           .

      *----------------------------------------------------------------*
       6100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA OBTER OS DADOS DE ENTRADA.                      *
      *----------------------------------------------------------------*
       6200-INFORMAR-BLOCO-SAIDA       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO

           SET FRWKEL04-FUN-CRIAR      TO TRUE
           SET FRWKEL04-PERM-ATUALIZAR TO TRUE
           SET FRWKEL04-IND-BL-SAIDA   TO TRUE

           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE ANCLW525-COD-LAYOUT    TO FRWKEL04-ID-BLOCO

           MOVE LENGTH OF WRK-ANCLW525 TO FRWKEL04-TAM-BLOCO
                                          ANCLW525-TAM-LAYOUT

           MOVE WRK-ANCLW525           TO FRWKEL04-BUFFER
                                           (1:FRWKEL04-TAM-BLOCO)

           PERFORM 5000-ACESSAR-GAM
           .
      *----------------------------------------------------------------*
       6200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO DB2.                         *
      *----------------------------------------------------------------*
       9100-ERRO-DB2                   SECTION.
      *----------------------------------------------------------------*

           SET  ERRO-DB2               TO TRUE
           MOVE 12                     TO FRWKGLAQ-COD-RETORNO
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGDB2-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE SQLSTATE               TO FRWKGDB2-SQLSTATE
           MOVE SQLCA                  TO FRWKGDB2-SQLCA
           MOVE SQLCODE                TO FRWKGDB2-SQLCODE

           MOVE WRK-COPY-DB2           TO WRK-BLOCO-INFO-ERRO
                                          (1:FRWKGHEA-TAM-DADOS)

           PERFORM 9999-API-ERROS
           .

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO MODULO                       *
      *----------------------------------------------------------------*
       9300-ERRO-MODULO                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           SET ERRO-MODULO             TO TRUE                          
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA        
           MOVE FRWKGMOD-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS            
           MOVE WRK-BLOCO-RETORNO      TO FRWKGMOD-BLOCO-RETORNO        
           MOVE WRK-COPY-MOD           TO WRK-BLOCO-INFO-ERRO           
                                               (1:FRWKGHEA-TAM-DADOS)

           PERFORM 9999-API-ERROS                                       
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO CICS.                        *
      *----------------------------------------------------------------*
       9400-ERRO-CICS                  SECTION.
      *----------------------------------------------------------------*

           SET  ERRO-CICS              TO TRUE
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGCIC-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE EIBFN                  TO FRWKGCIC-EIBFN
           MOVE EIBRCODE               TO FRWKGCIC-EIBRCODE
           MOVE EIBRSRCE               TO FRWKGCIC-EIBRSRCE
           MOVE EIBRESP                TO FRWKGCIC-EIBRESP
           MOVE EIBRESP2               TO FRWKGCIC-EIBRESP2
           MOVE EIBTASKN               TO FRWKGCIC-EIBTASKN
           MOVE WRK-COPY-CICS          TO WRK-BLOCO-INFO-ERRO
                                          (1:FRWKGHEA-TAM-DADOS)

           PERFORM 9999-API-ERROS
           .

      *----------------------------------------------------------------*
       9400-CICS-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO CHAMADA AO FRWK1999.                 *
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.                         
      *----------------------------------------------------------------*

           EXEC CICS LINK                                               
               PROGRAM  (WRK-FRWK1999)                                  
               COMMAREA (WRK-AREA-ERRO)                                 
               LENGTH   (LENGTH OF WRK-AREA-ERRO)                       
               NOHANDLE                                                 
           END-EXEC                                                     
                                                                        
           IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)        
               CONTINUE                                                 
           END-IF                                                       
                                                                        
           PERFORM 3000-FINALIZAR                                       
           .                                                            
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
