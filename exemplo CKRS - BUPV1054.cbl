      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. BUPV1054.
       AUTHOR.     RAPHAEL MARQUES.
      *================================================================*
      *                   P R I M E  I N F O R M A T I C A             *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  BUPV1054                                     *
      *    PROGRAMADOR.:  RAPHAEL MARQUES                   -  PRIME   *
      *    ANALISTA....:  MAURO                             -  PRIME   *
      *    DATA........:  08/12/2009                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  ESTE PROGRAMA FAZ O BATIMENTO ENTRE APORTES  *
      *                   DE CCORRASSS EXTERNAS RECEBIDAS COM A TABELA *
      *                   AMT32 DO SISTEMA ALTAMIRA.                   *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                TABLE                          INCLUDE/BOOK     *
      *                DB2PRD.TCTRL_EXTRC_DADO           BUPVB023      *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME           I/O           INCLUDE/BOOK     *
      *                EARQAPOR          I              I#BUPV35       *
      *                EARQCCOR          I              I#BUPV3A       *
      *                SARQAPOR          O              I#BUPV35       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO PELA BRAD7100       *
      *    I#BUPV35 - AREA PARA ARQUIVOS DE APORTES                    *
      *    I#BUPV3A - AREA CCORRASS                                    *
      *    I#BUPV00 - AREA DE COMUNICACAO                              *
      *    I#BUPV99 - ESTATISTICAS DE PROCESSAMENTO                    *
      *    I#CKRS01 - AREA DE CONTROLE DE COMMIT/RESTART               *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - AREA PARA TRATAMENTO DE ERRO PELA BRAD7100       *
      *    BRAD7600 - DATA E HORA ATUAL                                *
      *    BRAD0560 - OBTEM INFORMACOES DO JOB                         *
      *    BUPVM100 - GRAVAR LOG DE ERRO                               *
      *    CKRS1000 - CONEXAO COM O DB2                                *
      *    CKRS0100 - TRATAMENTO DE COMMIT/RESTART                     *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT EARQAPOR  ASSIGN     TO   UT-S-EARQAPOR
           FILE STATUS                 IS   WRK-FS-EARQAPOR.

           SELECT EARQCCOR  ASSIGN     TO   UT-S-EARQCCOR
           FILE STATUS                 IS   WRK-FS-EARQCCOR.

           SELECT SARQAPOR  ASSIGN     TO   UT-S-SARQAPOR
           FILE STATUS                 IS   WRK-FS-SARQAPOR.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *   INPUT:   APORTES                                             *
      *            ORG. SEQUENCIAL     -   LRECL   =   356             *
      *----------------------------------------------------------------*
       FD  EARQAPOR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  FD-EARQAPOR                 PIC  X(356).

      *----------------------------------------------------------------*
      *   INPUT:   ULTIMA RESPOSTA CLIENTE                             *
      *            ORG. SEQUENCIAL     -   LRECL   =   238             *
      *----------------------------------------------------------------*
       FD  EARQCCOR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  FD-EARQCCOR                 PIC  X(238).

      *----------------------------------------------------------------*
      *   OUTPUT:  APORTES                                             *
      *            ORG. SEQUENCIAL     -   LRECL   =   356             *
      *----------------------------------------------------------------*
       FD  SARQAPOR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  FD-SARQAPOR                 PIC  X(356).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER                      PIC  X(050)        VALUE
           '*** INICIO DA WORKING-STORAGE SECTION         ****'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE AUXILIARES                         ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       77  ACU-LIDOS-EARQAPOR          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-EARQCCOR          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-SARQAPOR       PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-DESPREZA-EARQAPOR       PIC  9(009) COMP-3  VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE AUXILIARES                         ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
      
       77  WRK-PGM                     PIC  X(008)         VALUE
           'BUPV1054'.

       01  WRK-SQLCODE-S9-9            PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-SQLCODE-S9-9.
           05 FILLER                   PIC  X(006).
           05 WRK-SQLCODE-S9-3         PIC S9(003).

       01  WRK-AUX-S9-2                PIC +9(002)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AUX-S9-2.
           05 FILLER                   PIC  X(001).
           05 WRK-AUX-9-2              PIC  9(002).

       01  WRK-AUX-S9-7                PIC +9(007)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-AUX-S9-7.
           05 FILLER                   PIC  X(001).
           05 WRK-AUX-9-7              PIC  9(007).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE TESTE DO FILE-STATUS               ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-FS-EARQAPOR             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-EARQCCOR             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-SARQAPOR             PIC  X(002)         VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MSG ERRO FILE STATUS               ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-ERRO-FS.
           05 FILLER                   PIC  X(011)         VALUE
              'BUPV1054 - '.
           05 WRK-ERRO-FS-7100.
              10 FILLER                PIC  X(005)         VALUE
                 'ERRO '.
              10 WRK-OPERACAO          PIC  X(013)         VALUE SPACES.
                 88 WRK-ABERTURA                           VALUE
                    'NA ABERTURA'.
                 88 WRK-LEITURA                            VALUE
                    'NA LEITURA'.
                 88 WRK-GRAVACAO                           VALUE
                    'NA GRAVACAO'.
                 88 WRK-FECHAMENTO                         VALUE
                    'NO FECHAMENTO'.
              10 FILLER                PIC  X(012)         VALUE
                 ' DO ARQUIVO '.
              10 WRK-NOME-ARQ          PIC  X(008)         VALUE SPACES.
              10 FILLER                PIC  X(015)         VALUE
                 ' FILE STATUS = '.
              10 WRK-FILE-STATUS       PIC  X(002)         VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGEM DE ERRO DE MODULOS        ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-ERRO-MODULO.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA DO MODULO '.
           05 WRK-MODULO               PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE
              ' - FILE STATUS = '.
           05 WRK-COD-RETORNO          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE ' **'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA UTILIZADA PELO MODULO RESTART         ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-RESTART.
           05 WRK-AR-LDS-EARQAPOR      PIC  9(009) COMP-3  VALUE ZEROS.
           05 WRK-AR-LDS-EARQCCOR      PIC  9(009) COMP-3  VALUE ZEROS.
           05 WRK-AR-GRAV-SARQAPOR     PIC  9(009) COMP-3  VALUE ZEROS.
           05 WRK-AR-DESP-EARQAPOR     PIC  9(009) COMP-3  VALUE ZEROS.
           05 WRK-AR-CHV-APORTE        PIC  X(021)         VALUE SPACES.
           05 WRK-AR-CHV-APORTE-ANT    PIC  X(021)         VALUE SPACES.
           05 WRK-AR-CHV-CCORRASS      PIC  X(021)         VALUE SPACES.
           05 WRK-AR-CHV-CCORRASS-ANT  PIC  X(021)         VALUE SPACES.
           05 WRK-AR-REG-EARQCCOR      PIC  X(238)         VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DAS CHAVES                            ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-CHV-APORTE.
           05 WRK-CBALCAO-APORTE       PIC  X(005)         VALUE SPACES.
           05 WRK-CPRODUTO-APORTE      PIC  X(002)         VALUE SPACES.
           05 WRK-CSUBPROD-APORTE      PIC  X(002)         VALUE SPACES.
           05 WRK-CNUMECTA-APORTE      PIC  X(006)         VALUE SPACES.
           05 WRK-CDIGICTA-APORTE      PIC  X(001)         VALUE SPACES.
           05 WRK-ZMOVCTA-APORTE       PIC  9(005)         VALUE ZEROS.

       01  WRK-CHV-CCORRASS.
           05 WRK-CBALCAO-CCORRASS     PIC  X(005)         VALUE SPACES.
           05 WRK-CPRODUTO-CCORRASS    PIC  X(002)         VALUE SPACES.
           05 WRK-CSUBPROD-CCORRASS    PIC  X(002)         VALUE SPACES.
           05 WRK-CNUMECTA-CCORRASS    PIC  X(006)         VALUE SPACES.
           05 WRK-CDIGICTA-CCORRASS    PIC  X(001)         VALUE SPACES.
           05 WRK-ZMOVCTA-CCORRASS     PIC  9(005)         VALUE ZEROS.

       01  WRK-CHV-APORTE-ANT.
           05 WRK-CBALCAO-APORTE-ANT   PIC  X(005)         VALUE SPACES.
           05 WRK-CPRODUTO-APORTE-ANT  PIC  X(002)         VALUE SPACES.
           05 WRK-CSUBPROD-APORTE-ANT  PIC  X(002)         VALUE SPACES.
           05 WRK-CNUMECTA-APORTE-ANT  PIC  X(006)         VALUE SPACES.
           05 WRK-CDIGICTA-APORTE-ANT  PIC  X(001)         VALUE SPACES.
           05 WRK-ZMOVCTA-APORTE-ANT   PIC  9(005)         VALUE ZEROS.

       01  WRK-CHV-CCORRASS-ANT.
           05 WRK-CBALC-CCORRASS-ANT   PIC  X(005)         VALUE SPACES.
           05 WRK-CPROD-CCORRASS-ANT   PIC  X(002)         VALUE SPACES.
           05 WRK-CSUBPRO-CCORRASS-ANT PIC  X(002)         VALUE SPACES.
           05 WRK-CNUMCTA-CCORRASS-ANT PIC  X(006)         VALUE SPACES.
           05 WRK-CDIGCTA-CCORRASS-ANT PIC  X(001)         VALUE SPACES.
           05 WRK-ZMOVCTA-CCORRASS-ANT PIC  9(005)         VALUE ZEROS.

       01  WRK-CHAVE-AUX.
           05 WRK-ZMOVCTA-AUX          PIC +9(005)         VALUE ZEROS.
       01  WRK-CHAVE-AUX-R             REDEFINES WRK-CHAVE-AUX.
           05 FILLER                   PIC  X(001).
           05 WRK-ZMOVCTA-AUX-R        PIC  9(005).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC X(050)          VALUE
           '*** AREA DA BRAD7600                           ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-7600-DATA-HORA.
           05 WRK-7600-DT-JULIANA      PIC  9(005) COMP-3  VALUE ZEROS.
           05 WRK-7600-DT-AAMMDDD      PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-7600-DT-AAAAMMDD     PIC  9(009) COMP-3  VALUE ZEROS.
           05 WRK-7600-DT-HHMMSS       PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-7600-DT-HHMMSSSS     PIC  9(013) COMP-3  VALUE ZEROS.
           05 WRK-7600-TIMESTAMP.
              10 WRK-7600-ANO          PIC  9(004)         VALUE ZEROS.
              10 WRK-7600-MES          PIC  9(002)         VALUE ZEROS.
              10 WRK-7600-DIA          PIC  9(002)         VALUE ZEROS.
              10 WRK-7600-HORA         PIC  9(002)         VALUE ZEROS.
              10 WRK-7600-MINUTOS      PIC  9(002)         VALUE ZEROS.
              10 WRK-7600-SEGUNDOS     PIC  9(002)         VALUE ZEROS.
              10 WRK-7600-MICROSEG     PIC  9(006)         VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD0560                           ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-BRAD0560.
           05 WRK-JOBNAME-0560         PIC  X(008)         VALUE SPACES.
           05 WRK-JOBNUMBER-0560       PIC  9(005)         VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100                           ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       COPY I#BRAD7C.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE CONTROLE DE COMMI/RESTART          ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       COPY I#CKRS01.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ENTRATA EARQPOR E SAIDA SARQAPOR   ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       COPY I#BUPV35.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ENTRADA EARQCCOR                   ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       COPY I#BUPV3A.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO                        ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       COPY I#BUPV00.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** ESTATISTICA DE PROCESSAMENTO               ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       COPY I#BUPV99.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DAS TABELAS                           ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE BUPVB023
           END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING-STORAGE SECTION             ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO-RESTART.

           PERFORM 3000-PROCESSAR      UNTIL     WRK-CHV-APORTE
                                       EQUAL     HIGH-VALUES.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BU00-REGISTRO
                      BU35-REGISTRO
                      BU3A-REGISTRO.

           PERFORM 1100-INICIALIZAR-CKRS.

           PERFORM 1200-OBTER-SEQUENCIA.

           OPEN INPUT  EARQAPOR
                       EARQCCOR
                OUTPUT SARQAPOR.

           SET WRK-ABERTURA            TO   TRUE.

           PERFORM 1300-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-INICIALIZAR-CKRS           SECTION.
      *----------------------------------------------------------------*

           CALL 'CKRS1000'.

           INITIALIZE CKRS01-INTERFACE
                      WRK-AREA-RESTART.

      *--- MOVER 'I' PARA CK01-FUNCAO
           SET CK01-INICIALIZAR        TO   TRUE.

           MOVE SPACES                 TO   CK01-PLAN.
           MOVE 'DB2'                  TO   CK01-ID-DB2.
           MOVE WRK-AREA-RESTART       TO   CK01-AREA-RESTART.
           MOVE LENGTH                 OF   WRK-AREA-RESTART
                                       TO   CK01-TAM-AREA-RESTART.

           PERFORM 1110-CHAMAR-CKRS.

           IF CK01-RESTART
              MOVE CK01-AREA-RESTART(1:CK01-TAM-AREA-RESTART)
                                       TO   WRK-AREA-RESTART
              MOVE WRK-AR-LDS-EARQAPOR TO   ACU-LIDOS-EARQAPOR
              MOVE WRK-AR-LDS-EARQCCOR TO   ACU-LIDOS-EARQCCOR
              MOVE WRK-AR-GRAV-SARQAPOR
                                       TO   ACU-GRAVADOS-SARQAPOR
              MOVE WRK-AR-DESP-EARQAPOR
                                       TO   ACU-DESPREZA-EARQAPOR
              MOVE WRK-AR-CHV-APORTE   TO   WRK-CHV-APORTE
              MOVE WRK-AR-CHV-APORTE-ANT
                                       TO   WRK-CHV-APORTE-ANT
              MOVE WRK-AR-CHV-CCORRASS TO   WRK-CHV-CCORRASS
              MOVE WRK-AR-CHV-CCORRASS-ANT
                                       TO   WRK-CHV-CCORRASS-ANT
              MOVE WRK-AR-REG-EARQCCOR TO   BU3A-REGISTRO
              DISPLAY '****************** BUPV1054 *******************' 
              DISPLAY '*                                             *' 
              DISPLAY '*              EFETUADO RESTART               *' 
              DISPLAY '*                                             *' 
              DISPLAY '* LIDOS-EARQAPOR       = ' ACU-LIDOS-EARQAPOR    
                      '            *'                                      
              DISPLAY '* LIDOS-EARQCCOR       = ' ACU-LIDOS-EARQCCOR    
                      '            *'                                      
              DISPLAY '* GRAVADOS-SARQAPOR    = '                       
                      ACU-GRAVADOS-SARQAPOR     '            *'         
              DISPLAY '* DESPREZADOS-EARQAPOR = '                       
                      ACU-DESPREZA-EARQAPOR     '            *'         
              DISPLAY '* CHAVE-APORTE         = ' WRK-CHV-APORTE    '*'
              DISPLAY '* CHAVE-APORTE-ANT     = ' WRK-CHV-APORTE-ANT'*'
              DISPLAY '* CHAVE-CCORRASS       = ' WRK-CHV-CCORRASS  '*'
              DISPLAY '* CHAVE-CCORRASS-ANT   = ' 
                      WRK-CHV-CCORRASS-ANT                          '*'
              DISPLAY '*                                             *' 
              DISPLAY '****************** BUPV1054 *******************' 
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-CHAMAR-CKRS                SECTION.
      *----------------------------------------------------------------*

           MOVE 'CKRS0100'             TO    WRK-MODULO.

           CALL WRK-MODULO             USING CKRS01-INTERFACE.

           IF CK01-CODIGO-RETORNO      NOT EQUAL ZEROS
              DISPLAY '***************** BUPV1054 ******************'
              DISPLAY '*                                           *'
              DISPLAY '*         ERRO NA CHAMADA AO MODULO         *'
              DISPLAY '*             CKRS0100 (RESTART)            *'
              DISPLAY '*                                           *'
              DISPLAY '*               RETORNO =   ' CK01-CODIGO-RETORNO
                      '              *'
              DISPLAY '*                                           *'
              DISPLAY '*          PROCESSAMENTO ENCERRADO          *'
              DISPLAY '*                                           *'
              DISPLAY '***************** BUPV1054 ******************'
              MOVE 'APL'               TO   ERR-TIPO-ACESSO
              STRING CK01-CODIGO-RETORNO
                 DELIMITED BY SIZE     INTO WRK-COD-RETORNO
              END-STRING
              MOVE WRK-ERRO-MODULO     TO   ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-OBTER-SEQUENCIA            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT  CPERDC_PROCM_SIST,
                       NSEQ_PROCM_EFETU,
               VALUE   (CINDCD_SIT_PREVD, 0)
               INTO    :BUPVB023.CPERDC-PROCM-SIST,
                       :BUPVB023.NSEQ-PROCM-EFETU,
                       :BUPVB023.CINDCD-SIT-PREVD
               FROM    DB2PRD.TCTRL_EXTRC_DADO
               WHERE   CFNCAO_PROCM        =   1
               AND     CPERDC_PROCM_SIST   IN (1, 3)
               AND     CDADO_TRATD         =   1
               AND     CCTRO_CUSTO_ORIGE   =  'ALTA'
               AND     CINDCD_SIT_PREVD    IN (12, 13)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
              MOVE ZEROS               TO CPERDC-PROCM-SIST OF BUPVB023
                                          NSEQ-PROCM-EFETU  OF BUPVB023
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'TCTRL_EXTRC_DADO'  TO ERR-DBD-TAB
                                          BU00-TABELA
              MOVE 'SELECT'            TO ERR-FUN-COMANDO
                                          BU00-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
                                          WRK-SQLCODE-S9-9
              MOVE WRK-SQLCODE-S9-3    TO BU00-SQLCODE
              MOVE '1200'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-PGM
                                          ERR-SEGM
              MOVE 998                 TO BU00-ES-COD-RETORNO
              MOVE WRK-PGM             TO BU00-COD-PROGRAMA
              MOVE '001'               TO BU00-LOCAL
              MOVE 'EXTRACAO ALTA – APORTES'
                                       TO BU00-COMPLEMENTO
              PERFORM 9996-MOVIMENTACAO-LOG-ERRO                         
              PERFORM 9998-EXIBIR-LOG-ERRO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-TESTAR-FILE-STATUS                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1310-TESTAR-FS-EARQAPOR.

           PERFORM 1320-TESTAR-FS-EARQCCOR.

           PERFORM 1330-TESTAR-FS-SARQAPOR.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1310-TESTAR-FS-EARQAPOR         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-EARQAPOR          NOT EQUAL '00'
              MOVE 'APL'               TO   ERR-TIPO-ACESSO
              MOVE 'EARQAPOR'          TO   WRK-NOME-ARQ
              MOVE WRK-FS-EARQAPOR     TO   WRK-FILE-STATUS
              MOVE WRK-ERRO-FS-7100    TO   ERR-TEXTO
              MOVE WRK-ERRO-FS         TO   BU00-SS-MENSAGEM
              EVALUATE TRUE
                 WHEN WRK-ABERTURA
                      MOVE 037         TO   BU00-ES-COD-RETORNO
                 WHEN WRK-LEITURA
                      MOVE 038         TO   BU00-ES-COD-RETORNO
                 WHEN WRK-FECHAMENTO
                      MOVE 040         TO   BU00-ES-COD-RETORNO
              END-EVALUATE
              PERFORM 9996-MOVIMENTACAO-LOG-ERRO
              PERFORM 9998-EXIBIR-LOG-ERRO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE ZEROS                  TO   BU00-ES-COD-RETORNO.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1320-TESTAR-FS-EARQCCOR         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-EARQCCOR          NOT EQUAL '00'
              MOVE 'APL'               TO   ERR-TIPO-ACESSO
              MOVE 'EARQCCOR'          TO   WRK-NOME-ARQ
              MOVE WRK-FS-EARQCCOR     TO   WRK-FILE-STATUS
              MOVE WRK-ERRO-FS-7100    TO   ERR-TEXTO
              MOVE WRK-ERRO-FS         TO   BU00-SS-MENSAGEM
              EVALUATE TRUE
                 WHEN WRK-ABERTURA
                      MOVE 037         TO   BU00-ES-COD-RETORNO
                 WHEN WRK-LEITURA
                      MOVE 038         TO   BU00-ES-COD-RETORNO
                 WHEN WRK-FECHAMENTO
                      MOVE 040         TO   BU00-ES-COD-RETORNO
              END-EVALUATE
              PERFORM 9996-MOVIMENTACAO-LOG-ERRO
              PERFORM 9998-EXIBIR-LOG-ERRO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE ZEROS                  TO   BU00-ES-COD-RETORNO.

      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1330-TESTAR-FS-SARQAPOR         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-SARQAPOR          NOT EQUAL '00'
              MOVE 'APL'               TO   ERR-TIPO-ACESSO
              MOVE 'SARQAPOR'          TO   WRK-NOME-ARQ
              MOVE WRK-FS-SARQAPOR     TO   WRK-FILE-STATUS
              MOVE WRK-ERRO-FS-7100    TO   ERR-TEXTO
              MOVE WRK-ERRO-FS         TO   BU00-SS-MENSAGEM
              EVALUATE TRUE
                 WHEN WRK-ABERTURA
                      MOVE 037         TO   BU00-ES-COD-RETORNO
                 WHEN WRK-GRAVACAO
                      MOVE 039         TO   BU00-ES-COD-RETORNO
                 WHEN WRK-FECHAMENTO
                      MOVE 040         TO   BU00-ES-COD-RETORNO
              END-EVALUATE
              PERFORM 9996-MOVIMENTACAO-LOG-ERRO
              PERFORM 9998-EXIBIR-LOG-ERRO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE ZEROS                  TO   BU00-ES-COD-RETORNO.

      *----------------------------------------------------------------*
       1330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO-RESTART    SECTION.
      *----------------------------------------------------------------*

           IF CK01-RESTART
              PERFORM 2100-LER-EARQAPOR
              IF WRK-AR-LDS-EARQCCOR   EQUAL     ZEROS
                 PERFORM 2200-LER-EARQCCOR
              END-IF
           ELSE
              PERFORM 2100-LER-EARQAPOR
              IF WRK-FS-EARQAPOR       EQUAL     '10'
                 DISPLAY '**************** BUPV1054 ****************'
                 DISPLAY '*                                        *'
                 DISPLAY '*         ARQUIVO EARQAPOR VAZIO         *'
                 DISPLAY '*                                        *'
                 DISPLAY '*           BUPV1054 FINALIZADO          *'
                 DISPLAY '*                                        *'
                 DISPLAY '**************** BUPV1054 ****************'
                 PERFORM 4000-FINALIZAR
             END-IF
             PERFORM 2200-LER-EARQCCOR
              IF WRK-FS-EARQCCOR       EQUAL     '10'
                 DISPLAY '**************** BUPV1054 ****************'
                 DISPLAY '*                                        *'
                 DISPLAY '*         ARQUIVO EARQCCOR VAZIO         *'
                 DISPLAY '*                                        *'
                 DISPLAY '*                                        *'
                 DISPLAY '**************** BUPV1054 ****************'
             END-IF
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-EARQAPOR               SECTION.
      *----------------------------------------------------------------*

           READ EARQAPOR               INTO BU35-REGISTRO.

           IF WRK-FS-EARQAPOR          EQUAL     '10'
              MOVE HIGH-VALUES         TO   WRK-CHV-APORTE
           ELSE
              SET WRK-LEITURA          TO   TRUE
              PERFORM 1310-TESTAR-FS-EARQAPOR
              PERFORM 2110-MONTAR-CHAVE-APORTE
              PERFORM 2120-TESTAR-SEQUENCIA-EARQAPOR
              ADD 1                    TO   ACU-LIDOS-EARQAPOR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-MONTAR-CHAVE-APORTE        SECTION.
      *----------------------------------------------------------------*

           MOVE BU35-CBALCAO           TO   WRK-CBALCAO-APORTE.
           MOVE BU35-CPRODUTO          TO   WRK-CPRODUTO-APORTE.
           MOVE BU35-CSUBPROD          TO   WRK-CSUBPROD-APORTE.
           MOVE BU35-CNUMECTA          TO   WRK-CNUMECTA-APORTE.
           MOVE BU35-CDIGICTA          TO   WRK-CDIGICTA-APORTE.
           
           MOVE BU35-ZMOVCTA           TO   WRK-ZMOVCTA-AUX.
           MOVE WRK-ZMOVCTA-AUX-R      TO   WRK-ZMOVCTA-APORTE.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2120-TESTAR-SEQUENCIA-EARQAPOR  SECTION.
      *----------------------------------------------------------------*

           IF WRK-CHV-APORTE           LESS      WRK-CHV-APORTE-ANT
              MOVE 'ARQUIVO EARQAPOR FORA DE SEQUENCIA – PROCESSAMENTO C
      -            'ANCELADO'          TO   ERR-TEXTO
              MOVE 'APL'               TO   ERR-TIPO-ACESSO
              MOVE 042                 TO   BU00-ES-COD-RETORNO
              STRING 'ARQUIVO EARQAPOR FORA DE SEQUENCIA'
                     ' - CHAVE ATUAL: '     WRK-CHV-APORTE
                     ' - CHAVE ANTERIOR: '  WRK-CHV-APORTE-ANT
                 DELIMITED BY SIZE     INTO BU00-SS-MENSAGEM
              END-STRING
              PERFORM 9996-MOVIMENTACAO-LOG-ERRO
              PERFORM 9998-EXIBIR-LOG-ERRO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-LER-EARQCCOR               SECTION.
      *----------------------------------------------------------------*

           READ EARQCCOR               INTO BU3A-REGISTRO.

           IF WRK-FS-EARQCCOR          EQUAL     '10'
              MOVE HIGH-VALUES         TO   WRK-CHV-CCORRASS
           ELSE
              SET WRK-LEITURA          TO   TRUE
              PERFORM 1320-TESTAR-FS-EARQCCOR
              PERFORM 2210-MONTAR-CHAVE-CCORRASS
              PERFORM 2220-TESTAR-SEQUENCIA-EARQCCOR
              ADD 1                    TO   ACU-LIDOS-EARQCCOR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-MONTAR-CHAVE-CCORRASS      SECTION.
      *----------------------------------------------------------------*

           MOVE BU3A-CBALCAO           TO   WRK-CBALCAO-CCORRASS.
           MOVE BU3A-CPRODUTO          TO   WRK-CPRODUTO-CCORRASS.
           MOVE BU3A-CSUBPROD          TO   WRK-CSUBPROD-CCORRASS.
           MOVE BU3A-CNUMECTA          TO   WRK-CNUMECTA-CCORRASS.
           MOVE BU3A-CDIGICTA          TO   WRK-CDIGICTA-CCORRASS.
           MOVE BU3A-ZMOVCTA           TO   WRK-ZMOVCTA-AUX.
           MOVE WRK-ZMOVCTA-AUX-R      TO   WRK-ZMOVCTA-CCORRASS.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2220-TESTAR-SEQUENCIA-EARQCCOR  SECTION.
      *----------------------------------------------------------------*

           IF WRK-CHV-CCORRASS         LESS      WRK-CHV-CCORRASS-ANT
              MOVE 'ARQUIVO EARQCCOR FORA DE SEQUENCIA – PROCESSAMENTO C
      -            'ANCELADO'          TO   ERR-TEXTO
              MOVE 'APL'               TO   ERR-TIPO-ACESSO
              MOVE 042                 TO   BU00-ES-COD-RETORNO
              STRING 'ARQUIVO EARQCCOR FORA DE SEQUENCIA'
                     ' - CHAVE ATUAL: '     WRK-CHV-CCORRASS
                     ' - CHAVE ANTERIOR: '  WRK-CHV-CCORRASS-ANT
                 DELIMITED BY SIZE     INTO BU00-SS-MENSAGEM
              END-STRING
              PERFORM 9996-MOVIMENTACAO-LOG-ERRO
              PERFORM 9998-EXIBIR-LOG-ERRO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF WRK-CHV-APORTE           EQUAL     WRK-CHV-CCORRASS
              PERFORM 3100-GRAVAR-SARQAPOR
              MOVE WRK-CHV-APORTE      TO        WRK-CHV-APORTE-ANT
              PERFORM 2100-LER-EARQAPOR
           ELSE
              IF WRK-CHV-APORTE        GREATER   WRK-CHV-CCORRASS
                 MOVE WRK-CHV-CCORRASS TO        WRK-CHV-CCORRASS-ANT
                 PERFORM 2200-LER-EARQCCOR
              ELSE
                 ADD 1                 TO        ACU-DESPREZA-EARQAPOR
                 MOVE 050              TO        BU00-ES-COD-RETORNO
                 STRING 'BUPV1054 - APORTE SEM CCORRASS.'
                        ' - BU35-CBALCAO: '   WRK-CBALCAO-APORTE
                        ' - BU35-CPRODUTO: '  WRK-CPRODUTO-APORTE
                        ' - BU35-CSUBPROD: '  WRK-CSUBPROD-APORTE
                        ' - BU35-CNUMECTA: '  WRK-CNUMECTA-APORTE
                        ' - BU35-CDIGICTA: '  WRK-CDIGICTA-APORTE
                        ' - BU35-ZMOVASSOC: ' WRK-ZMOVCTA-APORTE
                    DELIMITED BY SIZE  INTO  BU00-SS-MENSAGEM
                 END-STRING
                 PERFORM 9996-MOVIMENTACAO-LOG-ERRO
                 PERFORM 9997-GRAVAR-LOG-ERRO   
                 MOVE WRK-CHV-APORTE   TO        WRK-CHV-APORTE-ANT
                 PERFORM 2100-LER-EARQAPOR
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-GRAVAR-SARQAPOR            SECTION.
      *----------------------------------------------------------------*

           MOVE BU3A-COPERAC           TO        BU35-COPERAC.
           MOVE BU3A-COPER             TO        BU35-COPER.
           MOVE BU3A-DCONTD            TO        BU35-DCONTD.
           MOVE BU3A-DPROC             TO        BU35-DPROC.
           
           WRITE FD-SARQAPOR           FROM      BU35-REGISTRO.

           SET WRK-GRAVACAO            TO   TRUE.

           PERFORM 1330-TESTAR-FS-SARQAPOR.

           ADD 1                       TO        ACU-GRAVADOS-SARQAPOR.

           PERFORM 3200-FUNCAO-CHECKPOINT.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-FUNCAO-CHECKPOINT          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           SET CK01-CHECKPOINT         TO   TRUE.

           MOVE 'DB2'                  TO   CK01-ID-DB2.

           MOVE ACU-LIDOS-EARQAPOR     TO   WRK-AR-LDS-EARQAPOR.
           MOVE ACU-LIDOS-EARQCCOR     TO   WRK-AR-LDS-EARQCCOR.
           MOVE ACU-GRAVADOS-SARQAPOR  TO   WRK-AR-GRAV-SARQAPOR.
           MOVE ACU-DESPREZA-EARQAPOR  TO   WRK-AR-DESP-EARQAPOR.

           MOVE WRK-CHV-APORTE         TO   WRK-AR-CHV-APORTE.
           MOVE WRK-CHV-APORTE-ANT     TO   WRK-AR-CHV-APORTE-ANT.
           MOVE WRK-CHV-CCORRASS       TO   WRK-AR-CHV-CCORRASS.
           MOVE WRK-CHV-CCORRASS-ANT   TO   WRK-AR-CHV-CCORRASS-ANT.

           MOVE BU3A-REGISTRO          TO   WRK-AR-REG-EARQCCOR.

           MOVE WRK-AREA-RESTART       TO   CK01-AREA-RESTART.
           MOVE LENGTH                 OF   WRK-AREA-RESTART
                                       TO   CK01-TAM-AREA-RESTART.

           PERFORM 1110-CHAMAR-CKRS.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-TOTAIS.

           CLOSE EARQAPOR
                 EARQCCOR
                 SARQAPOR.

           SET WRK-FECHAMENTO          TO   TRUE.

           PERFORM 1300-TESTAR-FILE-STATUS.

           PERFORM 4200-FINALIZAR-CKRS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-EMITIR-TOTAIS              SECTION.
      *----------------------------------------------------------------*

           PERFORM 4110-OBTER-DATA-HORA.

           PERFORM 4120-OBTER-NUMERO-JOB.

           MOVE WRK-PGM                TO   WRK-DS6-PROGNAME.

           MOVE 'EXTRACAO ALTA – APORTES'
                                       TO   WRK-DS14-COMENTARIO(22:).

           DISPLAY WRK-DISP1.
           DISPLAY WRK-DISP2.
           DISPLAY WRK-DISP14.
           DISPLAY WRK-DISP3.
           DISPLAY WRK-DISP4.
           DISPLAY WRK-DISP5.
           DISPLAY WRK-DISP6.
           DISPLAY WRK-DISP3.
           DISPLAY WRK-DISP7.
           DISPLAY WRK-DISP8.

           MOVE 'EARQAPOR'             TO   WRK-DS9-DDNAME.
           MOVE 'I'                    TO   WRK-DS9-I-O.
           MOVE 'ARQUIVO DE APORTES'   TO   WRK-DS9-DESCARQ.
           MOVE ACU-LIDOS-EARQAPOR     TO   WRK-DS9-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'EARQCCOR'             TO   WRK-DS9-DDNAME.
           MOVE 'I'                    TO   WRK-DS9-I-O.
           MOVE 'DESPREZADOS (BALANCE)'
                                       TO   WRK-DS9-DESCARQ.
           MOVE ACU-DESPREZA-EARQAPOR  TO   WRK-DS9-QTDEARQ.
           DISPLAY WRK-DISP9.
           
           MOVE 'EARQCCOR'             TO   WRK-DS9-DDNAME.
           MOVE 'I'                    TO   WRK-DS9-I-O.
           MOVE 'ARQUIVO DE CCORRASSS'
                                       TO   WRK-DS9-DESCARQ.
           MOVE ACU-LIDOS-EARQCCOR     TO   WRK-DS9-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'SARQAPOR'             TO   WRK-DS9-DDNAME.
           MOVE 'O'                    TO   WRK-DS9-I-O.
           MOVE 'ARQUIVO DE APORTES'   TO   WRK-DS9-DESCARQ.
           MOVE ACU-GRAVADOS-SARQAPOR  TO   WRK-DS9-QTDEARQ.
           DISPLAY WRK-DISP9.

           DISPLAY WRK-DISP3.
           DISPLAY WRK-DISP1.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4110-OBTER-DATA-HORA            SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD7600'             USING WRK-7600-DATA-HORA.

           STRING WRK-7600-DIA '.'
                  WRK-7600-MES '.'
                  WRK-7600-ANO
              DELIMITED BY SIZE        INTO WRK-DS6-DATAPROC
           END-STRING.

           MOVE WRK-DS6-DATAPROC       TO   WRK-DS6-DATAMOV.

           STRING WRK-7600-HORA    ':'
                  WRK-7600-MINUTOS ':'
                  WRK-7600-SEGUNDOS
              DELIMITED BY SIZE        INTO WRK-DS6-HORAPROC
           END-STRING.

      *----------------------------------------------------------------*
       4110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4120-OBTER-NUMERO-JOB           SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0560'             USING WRK-JOBNAME-0560
                                             WRK-JOBNUMBER-0560.

           MOVE WRK-JOBNAME-0560       TO   WRK-DS6-JOBNAME.

      *----------------------------------------------------------------*
       4120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4200-FINALIZAR-CKRS             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE CKRS01-INTERFACE.

           SET CK01-FINALIZAR          TO   TRUE.

           MOVE 'DB2'                  TO   CK01-ID-DB2

           PERFORM 1110-CHAMAR-CKRS.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9996-MOVIMENTACAO-LOG-ERRO      SECTION.     
      *----------------------------------------------------------------*

           MOVE SPACES                 TO BU00-AREA-ENTRADA
                                          BU00-AREA-SAIDA.
           MOVE 1                      TO BU00-EE-FUNCAO-PROCESSAMENTO.
           MOVE CPERDC-PROCM-SIST      OF BUPVB023
                                       TO WRK-AUX-S9-2.
           MOVE WRK-AUX-9-2            TO BU00-EE-PERIOD-PROCESSAMENTO.
           MOVE 1                      TO BU00-EE-DADO-TRATADA.
           MOVE 'ALTA'                 TO BU00-EE-CENTRO-CUSTO-ORIGEM.
           MOVE NSEQ-PROCM-EFETU       OF BUPVB023
                                       TO WRK-AUX-S9-7.
           MOVE WRK-AUX-9-7            TO BU00-EE-SEQ-PROCESSAMENTO.
           MOVE 2                      TO BU00-SS-SITUAC-PROCESSAMENTO.
      
      *----------------------------------------------------------------*
       9996-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------* 
       9997-GRAVAR-LOG-ERRO            SECTION.
      *----------------------------------------------------------------* 
      
           MOVE 'BUPVM100'             TO        WRK-MODULO.
           
           CALL WRK-MODULO             USING     BU00-REGISTRO.
           
      *----------------------------------------------------------------* 
       9997-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
            
      *----------------------------------------------------------------*
       9998-EXIBIR-LOG-ERRO            SECTION.
      *----------------------------------------------------------------*

           IF ERR-TIPO-ACESSO          EQUAL     'DB2'
              DISPLAY '**************** BUPV1054 ****************'
              DISPLAY '*                                        *'
              DISPLAY '*            LOG DE ERRO DB2             *'
              DISPLAY '*                                        *'
              DISPLAY '* FUNCAO PROC.......: '
                         BU00-EE-FUNCAO-PROCESSAMENTO
                                              '                 *'
              DISPLAY '* PERIODO PROC......: '
                         BU00-EE-PERIOD-PROCESSAMENTO
                                              '                 *'
              DISPLAY '* DADO TRATADO......: '
                         BU00-EE-DADO-TRATADA '                 *'
              DISPLAY '* CENTRO CUSTO ORIG.: '
                         BU00-EE-CENTRO-CUSTO-ORIGEM
                                                '               *'
              DISPLAY '* SEQ. PROC.........: '
                         BU00-EE-SEQ-PROCESSAMENTO '            *'
              DISPLAY '* COD. RETORNO......: '
                         BU00-ES-COD-RETORNO   '                *'
              DISPLAY '* SITU. PROC........: '
                         BU00-SS-SITUAC-PROCESSAMENTO
                                             '                  *'
              DISPLAY '* COD. PROGRAMA.....: '
                         BU00-COD-PROGRAMA          '           *'
              DISPLAY '* LOCAL.............: '
                         BU00-LOCAL            '                *'
              DISPLAY '* TABELA............: '
                         BU00-TABELA                          ' *'
              DISPLAY '* COMANDO...........: '
                         BU00-COMANDO                 '         *'
              DISPLAY '* SQLCODE...........: '
                         BU00-SQLCODE           '               *'
              DISPLAY '*                                        *'
              DISPLAY '* COMPLEMENTO: '
                                     BU00-COMPLEMENTO(01:25)  ' *'
              DISPLAY '* '           BU00-COMPLEMENTO(26:38)  ' *'
              DISPLAY '* '           BU00-COMPLEMENTO(64:38)  ' *'
              DISPLAY '* '           BU00-COMPLEMENTO(102:02)
                          '                                     *'
              DISPLAY '*                                        *'
              DISPLAY '**************** BUPV1054 ****************'
           ELSE
              DISPLAY '**************** BUPV1054 ****************'
              DISPLAY '*                                        *'
              DISPLAY '*              LOG DE ERRO               *'
              DISPLAY '*                                        *'
              DISPLAY '* FUNCAO PROCESSAMENTO.: '
                         BU00-EE-FUNCAO-PROCESSAMENTO
                                                 '              *'
              DISPLAY '* PERIODO PROCESSAMENTO: '
                         BU00-EE-PERIOD-PROCESSAMENTO
                                                 '              *'
              DISPLAY '* DADO TRATADO.........: '
                         BU00-EE-DADO-TRATADA    '              *'
              DISPLAY '* CENTRO CUSTO ORIGEM..: '
                         BU00-EE-CENTRO-CUSTO-ORIGEM
                                                   '            *'
              DISPLAY '* SEQ. PROCESSAMENTO...: '
                         BU00-EE-SEQ-PROCESSAMENTO    '         *'
              DISPLAY '* COD. RETORNO.........: '
                         BU00-ES-COD-RETORNO      '             *'
              DISPLAY '* SITU. PROCESSAMENTO..: '
                         BU00-SS-SITUAC-PROCESSAMENTO
                                                '               *'
              DISPLAY '*                                        *'
              DISPLAY '* MENSAGEM: ' BU00-SS-MENSAGEM(01:28)  ' *'
              DISPLAY '* '           BU00-SS-MENSAGEM(29:38)  ' *'
              DISPLAY '* '           BU00-SS-MENSAGEM(67:38)  ' *'
              DISPLAY '* '           BU00-SS-MENSAGEM(105:38) ' *'
              DISPLAY '* '           BU00-SS-MENSAGEM(143:08)
                                '                               *'
              DISPLAY '*                                        *'
              DISPLAY '**************** BUPV1054 ****************'
           END-IF.

      *----------------------------------------------------------------*
       9998-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-PGM                TO   ERR-PGM.

           IF ERR-TIPO-ACESSO          EQUAL     'DB2'
              CALL 'BRAD7100'          USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           ELSE
              CALL 'BRAD7100'          USING WRK-BATCH
                                             ERRO-AREA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*