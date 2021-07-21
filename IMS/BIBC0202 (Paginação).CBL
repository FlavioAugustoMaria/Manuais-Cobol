      *================================================================*        
       IDENTIFICATION                  DIVISION.                                
      *================================================================*

       PROGRAM-ID. BIBC0202.
       AUTHOR.     FLAVIO AUGUSTO MARIA.
      *================================================================*        
      *                  P R I M E - I N F O R M A T I C A             *        
      *----------------------------------------------------------------*        
      *    PROGRAMA....:   BIBC0202                                    *
      *    PROGRAMADOR.:   FLAVIO AUGUSTO MARIA      - PRIME           *
      *    ANALISTA....:   RONALDO NATIVIDADE        - PRIME           *
      *    DATA........:   08/02/2011                                  *
      *----------------------------------------------------------------*        
      *    OBJETIVO....:   CONSULTAR TODAS AS TRANSACOES CONTABEIS E   *
      *           NAO CONTABEIS DISPONIVEIS, E QUE FORAM CARREGADOS NO *
      *           MOVIMENTO DIARIO.                                    *
      *----------------------------------------------------------------*        
      *    TELAS/MAPAS.:                                               *        
      *     NOME     DESCRICAO                                         *        
      *    --------  ----------------------------                      *        
      *    BIBCG2TM  TELA PARA CONSULTA SUMARIZADA DE TRANSACOES.      *
      *                                                                *        
      *----------------------------------------------------------------*        
      *    TABELAS DB2.:                                               *        
      *     DCLGEN   NOME                                              *        
      *    BIBCB000  DB2PRD.SMARI_TRANS_CTBIL                          *
      *    BIBCB005  DB2PRD.TRANS_POSTO_ATDMT                          *
      *    YMKCB001  DB2PRD.TCAD_CORSP_BCRIO                           *
      *----------------------------------------------------------------*        
      *    INC'S.......:                                               *        
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO PELA BRAD7100.      *
      *    I#BIBC47 - AREA DE CHAMADA AO MODULO MESU9023.              *
      *    I#MESUM6 - AREA PARA O MODULO MESU9015.                     *
      *----------------------------------------------------------------*        
      *    MODULOS.....:                                               *        
      *    POOL5000 - ROTINA PARA TRAFEGAR AS MENSAGENS DO IMS.        *
      *    BRAD0015 - CALCULA NUMERO DE DIAS ENTRE DUAS DATAS.         *
      *    BRAD0660 - ROTINA PARA COMPACTAR A AREA DA TRANSACAO.       *
      *    BRAD7100 - ROTINA PARA TRATAMENTO DE ERRO.                  *
      *    POOL7600 - OBTENCAO DE DATA E HORA CORRENTE.                *
      *    BRAD1080 - MODULO DE IMPRESSAO.                             *
      *    POOL1205 - VERIFICA SE A DATA E VALIDA.                     *
      *    MESU9015 - MODULO PARA OBTER NATUREZA DO DEPTO.             *
      *    MESU9023 - MODULO PARA OBTER DEPTO ATRELADO AGENCIA.        *
      *    BIBC0031 - MODULO PARA IMPRESSAO.                           *
      *----------------------------------------------------------------*        
      *    NAVEGACAO...:                                               *        
      *    CHAMADO POR.:                               CHAMA:          *        
      *    BIBC0030                                                    *
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
       77  FILLER                      PIC  X(50)         VALUE                 
           '*** INICIO DA WORKING STORAGE SECTION ***'.                         
      *----------------------------------------------------------------*
                                                                                
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE                
           '*** INDEXADORES ***'.                                               
      *----------------------------------------------------------------*
                                                                                
       77  IND-1                       PIC  9(009) COMP-3  VALUE ZEROS.
                                                                                
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE                
           '*** AREA DE ATRIBUTOS ***'.                                         
      *----------------------------------------------------------------*
                                                                                
       01  WRK-ATRIBUTOS.                                                       
      *------ DESPROTEGIDO, BRILHANTE, ALFA, POSICIONAR C-U-R-S-O-R             
           05 WRK-49353                PIC S9(008) COMP    VALUE +49353.        
      *------ PROTEGIDO, ALFA, NORMAL                                           
           05 WRK-161                  PIC S9(008) COMP    VALUE +161.          
      *------ DESPROTEGIDO, ALFA, NORMAL, POSICIONAR C-U-R-S-O-R                
           05 WRK-49345                PIC S9(008) COMP    VALUE +49345.        
                                                                                
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE                
           '*** AREA DE COMUNICACAO ***'.                                       
      *----------------------------------------------------------------*
                                                                                
       01  WRK-COMU-AREA.                                                       
           05  WRK-COMU-LL             PIC S9(004) COMP    VALUE +124.          
           05  WRK-COMU-ZZ             PIC S9(004) COMP    VALUE ZEROS.         
           05  WRK-COMU-TRANCODE       PIC  X(015)         VALUE SPACES.        
           05  WRK-COMU-SENHA          PIC  X(037)         VALUE SPACES.        
           05  WRK-COMU-COMANDO        PIC  X(068)         VALUE ZEROS.         
                                                                                
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE                
           'VARIAVEIS AUXILIARES'.                                              
      *----------------------------------------------------------------*
                                                                                
       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       01  WRK-DESPREZA                PIC  9(009) COMP   VALUE ZEROS.
       01  WRK-FIM-CURSOR              PIC  X(001)         VALUE SPACES.
       01  WRK-PAGINA                  PIC  9(003) COMP   VALUE ZEROS.
                                                                                
       01  WRK-DATA-DE                 PIC  9(008)         VALUE ZEROS.         
       01  WRK-DATA-ATE                PIC  9(008)         VALUE ZEROS.         
       01  WRK-DATA-TRANS              PIC  9(008)         VALUE ZEROS.         
                                                                                
       01  WRK-DATA-AAAAMMDD           PIC  9(008)         VALUE ZEROS.         
       01  WRK-DATA-AAAAMMDD-A         REDEFINES   WRK-DATA-AAAAMMDD.           
           03 WRK-DATA-AAAA            PIC  9(004).                             
           03 WRK-DATA-MM              PIC  9(002).                             
           03 WRK-DATA-DD              PIC  9(002).                             

       01  WRK-DATA-INI                PIC  X(010)         VALUE SPACES.
       01  WRK-DATA-FIM                PIC  X(010)         VALUE SPACES.

       01  WRK-S9-5-4                  PIC +9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-5-4.
           05  FILLER                  PIC X(001).
           05  FILLER                  PIC 9(001).
           05  WRK-09-5-4              PIC 9(004).

       01  WRK-S9-5                    PIC +9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-5.
           05  FILLER                  PIC X(001).
           05  WRK-09-5                PIC 9(005).

       01  WRK-S9-9                    PIC +9(009)         VALUE ZEROS.
       01  WRK-S9-9-R                  REDEFINES WRK-S9-9.
           05  FILLER                  PIC X(001).
           05  FILLER                  PIC 9(003).
           05  WRK-09-9-6              PIC 9(006).
       01  FILLER                      REDEFINES WRK-S9-9-R.
           05  FILLER                  PIC X(001).
           05  WRK-09-9-R              PIC 9(009).

       01  WRK-S9-15V2                 PIC +9(015)V99      VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-S9-15V2.
           05  FILLER                  PIC X(001).
           05  FILLER                  PIC 9(007).
           05  WRK-S9-15-8V2           PIC 9(008)V99.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE                
           '*** AREA AUX DE TRAT DE INCONSISTENCIA ***'.                        
      *----------------------------------------------------------------*
                                                                                
       01  WRK-INCONSIS.                                                        
           03 WRK-INCON-EMP            PIC  X(001)         VALUE SPACES.        
           03 WRK-INCON-AGORI          PIC  X(001)         VALUE SPACES.        
           03 WRK-INCON-PACB           PIC  X(001)         VALUE SPACES.        
           03 WRK-INCON-TRAN           PIC  X(001)         VALUE SPACES.        
           03 WRK-INCON-DATAS.                                                  
              05 WRK-INCON-DT-DE       PIC  X(001)         VALUE SPACES.        
              05 WRK-INCON-DT-ATE      PIC  X(001)         VALUE SPACES.        

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA A BRAD0015'.
      *----------------------------------------------------------------*

       01  WRK-DT-SUPERIOR-0015        PIC  X(008)         VALUE SPACES.
       01  WRK-DT-INFERIOR-0015        PIC  X(008)         VALUE SPACES.
       01  WRK-NRO-DIAS-0015           PIC S9(005) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE                
           '*** AREA DO MODULO MESU9023 ***'.                                   
      *----------------------------------------------------------------*
                                                                                
       COPY 'I#BIBC47'.                                                         
                                                                                
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE                
           '*** AREA DO MODULO MESU9015 ***'.                                   
      *----------------------------------------------------------------*
                                                                                
       COPY 'I#MESUM6'.                                                         
                                                                                
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE                
           '*** AREA DO BIBC0031   ***'.                                        
      *----------------------------------------------------------------*
                                                                                
       01  WRK-AREA-BIBC0031.                                                   
           03  WRK-STATUS-RET          PIC  9(002)         VALUE ZEROS.         
           03  WRK-CHAVE-PESQ.                                                  
               05  WRK-BANCO           PIC  9(003)         VALUE ZEROS.         
               05  WRK-EMPRESA         PIC  9(005)         VALUE ZEROS.         
               05  WRK-AGENCIA         PIC  9(005)         VALUE ZEROS.         
               05  WRK-PACB-ORI        PIC  9(005)         VALUE ZEROS.         
               05  WRK-DTMOV-INI       PIC  9(008)         VALUE ZEROS.         
               05  WRK-DTMOV-FIM       PIC  9(008)         VALUE ZEROS.         
               05  WRK-TRANSAC         PIC  X(004)         VALUE SPACES.        
               05  WRK-PAGINA-0031     PIC  9(006)         VALUE ZEROS.
               05  WRK-IMPR            PIC  X(008)         VALUE SPACES.        
          03   WRK-TOTAIS-IMPR.                                                 
               05  WRK-TOT-PG          PIC  9(009)V9(002)  VALUE ZEROS.         
               05  WRK-TOT-REC         PIC  9(009)V9(002)  VALUE ZEROS.         
               05  WRK-TOT-EST         PIC  9(009)V9(002)  VALUE ZEROS.         
               05  WRK-QTD-PG          PIC  9(006)         VALUE ZEROS.         
               05  WRK-QTD-REC         PIC  9(006)         VALUE ZEROS.         
               05  WRK-QTD-EST         PIC  9(006)         VALUE ZEROS.         
          03   WRK-CH-RESTART.                                                  
               05  WRK-REST-TRANS      PIC  X(004)         VALUE SPACES.        
               05  WRK-REST-DTMOV      PIC  9(008)         VALUE ZEROS.         
                                                                                
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)        VALUE                  
           '** AREA DA BRAD1080 **'.
      *----------------------------------------------------------------*
                                                                                
       01  WRK-IMPRESSORA              PIC  X(08)        VALUE SPACES.          
                                                                                
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE                
           '*** AREA PARA A POOL7600 ***'.
      *----------------------------------------------------------------*
                                                                                
       01  WRK-DATA-HORA.                                                       
           03  WRK-DT-JULIANA          PIC  9(005) COMP-3  VALUE ZEROS.         
           03  WRK-DT-AAMMDD           PIC  9(007) COMP-3  VALUE ZEROS.         
           03  WRK-DT-AAAAMMDD         PIC  9(009) COMP-3  VALUE ZEROS.         
           03  WRK-TI-HHMMSS           PIC  9(007) COMP-3  VALUE ZEROS.         
           03  WRK-TI-HHMMSSMMMMMM     PIC  9(013) COMP-3  VALUE ZEROS.         
           03  WRK-TIMESTAMP.                                                   
               05  WRK-ANO             PIC  9(004)         VALUE ZEROS.         
               05  WRK-MES             PIC  9(002)         VALUE ZEROS.         
               05  WRK-DIA             PIC  9(002)         VALUE ZEROS.         
               05  WRK-HOR             PIC  9(002)         VALUE ZEROS.         
               05  WRK-MIN             PIC  9(002)         VALUE ZEROS.         
               05  WRK-SEG             PIC  9(002)         VALUE ZEROS.         
               05  FILLER              PIC  X(006)         VALUE SPACES.        
                                                                                
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE                
           '*** AREA PARA A POOL1205 ***'.
      *----------------------------------------------------------------*
                                                                                
       01  WRK-MSG-1205                PIC  X(50)          VALUE SPACES.        
                                                                                
       01  WRK-DATA-POOL1205.
           03  WRK-DT-ENVIADA          PIC  9(08)  COMP-3  VALUE ZEROS.         
           03  WRK-OPCAO               PIC  X(01)          VALUE SPACES.        
           03  WRK-DT-JULIANA          PIC  9(07)  COMP-3  VALUE ZEROS.         
           03  WRK-DT-EDITADA          PIC  X(10)          VALUE SPACES.        
           03  WRK-DT-GREGORI          PIC  9(08)          VALUE ZEROS.         
           03  WRK-DIA-SEMANA          PIC  X(13)          VALUE SPACES.        
           03  WRK-MES-EXTENSO         PIC  X(09)          VALUE SPACES.        
           03  WRK-DIA-UT-ANTER        PIC  9(08)          VALUE ZEROS.         
           03  WRK-DIA-UT-POSTE        PIC  9(08)          VALUE ZEROS.         
                                                                                
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE                
           '*** AREA PARA MENSAGENS ***'.                                       
      *----------------------------------------------------------------*
                                                                                
       01  WRK-MENSA01                 PIC  X(079)         VALUE                
           'TECLE <PF4> PARA CONTINUAR IMPRESSAO'.
       01  WRK-MENSA02                 PIC  X(079)         VALUE
           'EMPRESA INVALIDA'.
       01  WRK-MENSA03                 PIC  X(079)         VALUE                
           'IMPRESSAO EFETUADA COM SUCESSO'.                                    
       01  WRK-MENSA04                 PIC  X(079)         VALUE                
           'DIGITE OS DADOS E TECLE <ENTER>'.                                   
       01  WRK-MENSA05                 PIC  X(079)         VALUE                
           'PACB INVALIDO'.                                                     
       01  WRK-MENSA06                 PIC  X(079)         VALUE                
           'EMPRESA INVALIDA'.
       01  WRK-MENSA07                 PIC  X(079)         VALUE                
           'NAO EXISTE PAGINA POSTERIOR'.                                       
       01  WRK-MENSA08                 PIC  X(079)         VALUE                
           'NAO EXISTE PAGINA ANTERIOR'.                                        
       01  WRK-MENSA09                 PIC  X(079)         VALUE                
           'PF INVALIDA'.                                                       
       01  WRK-MENSA10                 PIC  X(079)         VALUE                
           'CONSULTA EFETUADA COM SUCESSO'.
       01  WRK-MENSA11                 PIC  X(079)         VALUE
           'DATA INFORMADA NAO PODE SER MAIOR QUE DATA ATUAL'.
       01  WRK-MENSA12                 PIC  X(079)         VALUE
           'CAMPOS BRILHANTES INVALIDOS'.
       01  WRK-MENSA13                 PIC  X(079)         VALUE
           'NAO EXISTE DADOS P/ ARGUMENTO INFORMADO'.
       01  WRK-MENSA14.
           05 FILLER                   PIC  X(052)         VALUE
           'INTERVALO ENTRE DATA "DE" E DATA "ATE" NAO PODE SER '.
           05 FILLER                   PIC  X(027)         VALUE
           'SUPERIOR A 7 DIAS'.
       01  WRK-MENSA15                 PIC  X(079)         VALUE                
           'DATA MOVIMENTO DE INVALIDA'.                                        
       01  WRK-MENSA16                 PIC  X(079)         VALUE                
           'DATA MOVIMENTO ATE INVALIDA'.                                       
       01  WRK-MENSA17                 PIC  X(079)         VALUE                
           'DATA DE DEVE SER MENOR OU IGUAL A DATA ATE'.
       01  WRK-MENSA18                 PIC  X(079)         VALUE
           'NAO EXISTEM DADOS P/ ARGUMENTO INFORMADO'.
       01  WRK-MENSA21                 PIC  X(075)         VALUE
           'AGENCIA DO USUARIO NAO ENCONTRADA'.
       01  WRK-MENSA22                 PIC  X(075)         VALUE
           'AGENCIA DO USUARIO DIFERENTE DA INFORMADA'.
       01  WRK-MENSA23                 PIC  X(075)         VALUE
           'DEPARTAMENTO DO USUARIO NAO ENCONTRADO'.
       01  WRK-MENSA26                 PIC  X(079)         VALUE                
           'IMPRESSORA INDISPONIVEL'.                                           
       01  WRK-MENSA27                 PIC  X(075)         VALUE                
           'PROBLEMA DE COMUNICACAO - DADOS INCONSISTENTES'.                    
       01  WRK-MENSA28                 PIC  X(079)         VALUE
           'AGENCIA INVALIDA'.
                                                            
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE                
           '*** AREAS DA POOL5000 ***'.                                         
      *----------------------------------------------------------------*
                                                                                
       01  WRK-GU                      PIC  X(004)         VALUE 'GU'.          
       01  WRK-CHNG                    PIC  X(004)         VALUE 'CHNG'.        
       01  WRK-ISRT                    PIC  X(004)         VALUE 'ISRT'.        
                                                                                
       01  WRK-COD-USER.                                                        
           05 WRK-COD-USER-N           PIC  9(007)         VALUE ZEROS.         
                                                                                
       01  WRK-VERSAO                  PIC X(06)   VALUE 'VRS002'.              
                                                                                
       01  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.        
       01  WRK-MODNAME                 PIC  X(008)         VALUE SPACES.        
                                                                                
       01  WRK-COD-DEPTO.                                                       
           03 FILLER                   PIC  X(002)         VALUE SPACES.        
           03 WRK-COD-DEPTO-N          PIC  9(004)         VALUE ZEROS.         
                                                                                
       01  WRK-MENSAGEM.                                                        
           03 FILLER                   PIC  X(004)         VALUE SPACES.        
           03 WRK-TRANSACAO            PIC  X(008)         VALUE SPACES.        
           03 FILLER                   PIC  X(1988)        VALUE SPACES.        
                                                                                
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE                
           'AREA DE ERRO DA BRAD0660'.
      *----------------------------------------------------------------*
                                                                                
       01  WRK-ERRO-BRAD0660.
           03  FILLER                  PIC  X(045)         VALUE                
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           03  FILLER                  PIC  X(012)         VALUE                
               '- RET.COD ='.                                                   
           03  WRK-RC                  PIC  9(002)         VALUE ZEROS.         
           03  FILLER                  PIC  X(011)         VALUE                
               ' - LOCAL ='.                                                    
           03  WRK-LOCAL-ERRO          PIC  9(003)         VALUE ZEROS.         
           03  FILLER                  PIC  X(002)         VALUE ' *'.          
                                                                                
      *----------------------------------------------------------------*        
       01  FILLER                      PIC X(050)          VALUE                
           '*** TELA BIBCG2TM - INPUT ***'.
      *----------------------------------------------------------------*        

       01  WRK-INPUT-G2TM.
           05 FILLER                      PIC  X(018)      VALUE SPACES.
           05 WRK-INP-DADOS-G2TM.
             10 WRK-INP-PFK-G2TM          PIC  X(001)      VALUE SPACES.
             10 WRK-INP-SENHA-G2TM        PIC  X(037)      VALUE SPACES.
             10 WRK-INP-COMANDO-G2TM      PIC  X(068)      VALUE SPACES.
             10 WRK-INP-FASE-G2TM.
                15 WRK-INP-FASE-G2TM-N    PIC  9(001)      VALUE ZEROS.
             10 WRK-INP-ULTPAG-G2TM       PIC  X(001)      VALUE SPACES.
             10 WRK-INP-NATURE-G2TM       PIC  X(001)      VALUE SPACES.
             10 WRK-INP-SOMPAG-G2TM.
                15 WRK-INP-SOMPAG-G2TM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-INP-PAGINA-G2TM.
                15 WRK-INP-PAGINA-G2TM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-INP-LIQEST-G2TM       PIC  X(020)      VALUE SPACES.
             10 WRK-INP-TPACES-G2TM       PIC  9(001)      VALUE ZEROS.
             10 WRK-INP-INCONS-G2TM       PIC  X(006)      VALUE SPACES.
             10 WRK-INP-PAGIMP-G2TM.
                15 WRK-INP-PAGIMP-G2TM-N  PIC  9(006)      VALUE ZEROS.
             10 WRK-INP-TOTIMP-G2TM.
                15 WRK-INP-TOT-PG-G2TM      PIC  9(009)V9(002)
                                                           VALUE ZEROS.
                15 WRK-INP-TOT-REC-G2TM     PIC  9(009)V9(002)
                                                           VALUE ZEROS.
                15 WRK-INP-TOT-EST-G2TM     PIC  9(009)V9(002)
                                                           VALUE ZEROS.
                15 WRK-INP-QTD-PG-G2TM      PIC  9(006)    VALUE ZEROS.
                15 WRK-INP-QTD-REC-G2TM     PIC  9(006)    VALUE ZEROS.
                15 WRK-INP-QTD-EST-G2TM     PIC  9(006)    VALUE ZEROS.
             10 WRK-INP-BCO-G2TM.
                15 WRK-INP-BCO-G2TM-N     PIC  9(003)      VALUE ZEROS.
             10 WRK-INP-EMP-G2TM.
                15 WRK-INP-EMP-G2TM-N     PIC  9(005)      VALUE ZEROS.
             10 WRK-INP-AGORI-G2TM.
                15 WRK-INP-AGORI-G2TM-N   PIC  9(005)      VALUE ZEROS.
             10 WRK-INP-PACB-G2TM.
                15 WRK-INP-PACB-G2TM-N    PIC  9(003)      VALUE ZEROS.
             10 WRK-INP-DEDIA-G2TM.
                15 WRK-INP-DEDIA-G2TM-N   PIC  9(002)      VALUE ZEROS.
             10 WRK-INP-DEMES-G2TM.
                15 WRK-INP-DEMES-G2TM-N   PIC  9(002)      VALUE ZEROS.
             10 WRK-INP-DEANO-G2TM.
                15 WRK-INP-DEANO-G2TM-N   PIC  9(004)      VALUE ZEROS.
             10 WRK-INP-ATEDIA-G2TM.
                15 WRK-INP-ATEDIA-G2TM-N  PIC  9(002)      VALUE ZEROS.
             10 WRK-INP-ATEMES-G2TM.
                15 WRK-INP-ATEMES-G2TM-N  PIC  9(002)      VALUE ZEROS.
             10 WRK-INP-ATEANO-G2TM.
                15 WRK-INP-ATEANO-G2TM-N  PIC  9(004)      VALUE ZEROS.
             10 WRK-INP-TRAN-G2TM         PIC  X(004)      VALUE SPACES.
             10 WRK-INP-LITPAG-G2TM       PIC  X(011)      VALUE SPACES.
             10 WRK-INP-TOTPAG-G2TM.
                15 WRK-INP-TOTPAG-G2TM-N  PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-INP-LIQPAG-G2TM       PIC  X(016)      VALUE SPACES.
             10 WRK-INP-QTDPAG-G2TM.
                15 WRK-INP-QTDPAG-G2TM-N  PIC  9(006)      VALUE ZEROS.
             10 WRK-INP-LITREC-G2TM       PIC  X(015)      VALUE SPACES.
             10 WRK-INP-QTDREC-G2TM.
                15 WRK-INP-QTDREC-G2TM-N  PIC  9(006)      VALUE ZEROS.
             10 WRK-INP-CHREST-G2TM.
                15 WRK-INP-REST-TRANS-G2TM PIC  X(004)     VALUE SPACES.
                15 WRK-INP-REST-DTMOV-G2TM PIC  9(008)     VALUE ZEROS.
             10 WRK-INP-TAB-OCOR-G2TM.
               15 WRK-INP-TABELA-G2TM     OCCURS 011  TIMES.
                 20 WRK-INP-ODATA-G2TM    PIC  X(010)      VALUE SPACES.
                 20 WRK-INP-OTRAN-G2TM    PIC  X(004)      VALUE SPACES.
                 20 WRK-INP-DESCR-G2TM    PIC  X(024)      VALUE SPACES.
                 20 WRK-INP-QTDTRX-G2TM.
                    25 WRK-INP-QTDTRX-G2TM-N
                                          PIC  9(006)      VALUE ZEROS.
                 20 WRK-INP-VTOTCH-G2TM.
                    25 WRK-INP-VTOTCH-G2TM-N
                                        PIC  ------.--9,99 VALUE ZEROS.
                 20 WRK-INP-VTOTDN-G2TM.
                    25 WRK-INP-VTOTDN-G2TM-N
                                        PIC  ------.--9,99 VALUE ZEROS.
                 20 WRK-INP-PAGREC-G2TM   PIC  X(001)      VALUE SPACES.
             10 WRK-INP-TOTREC-G2TM.
                15 WRK-INP-TOTREC-G2TM-N    PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-INP-LIQREC-G2TM       PIC  X(020)      VALUE SPACES.
             10 WRK-INP-LITEST-G2TM       PIC  X(015)      VALUE SPACES.
             10 WRK-INP-TOTEST-G2TM.
                15 WRK-INP-TOTEST-G2TM-N  PIC  ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
             10 WRK-INP-QTDEST-G2TM.
                15 WRK-INP-QTDEST-G2TM-N  PIC  9(006)      VALUE ZEROS.
             10 WRK-INP-MENSA-G2TM        PIC  X(079)      VALUE SPACES.
                                                                                
      *---------------------------------------------------------------*         
       01  FILLER                      PIC X(050)          VALUE                
           '*** TELA BIBCG2TM - OUTPUT ***'.
      *---------------------------------------------------------------*         

       01  WRK-OUTPUT-G2TM.
           05 WRK-OUT-LL-G2TM             PIC S9(004) COMP VALUE +1291.
           05 WRK-OUT-ZZ-G2TM             PIC  9(004) COMP VALUE ZEROS.
           05 WRK-OUT-DADOS-G2TM.
             10 WRK-OUT-SENHA-G2TM        PIC  X(037)      VALUE SPACES.
             10 WRK-OUT-COMANDO-G2TM      PIC  X(068)      VALUE SPACES.
             10 WRK-OUT-FASE-G2TM.
               15 WRK-OUT-FASE-G2TM-N     PIC  9(001)      VALUE ZEROS.
             10 WRK-OUT-ULTPAG-G2TM       PIC  X(001)      VALUE SPACES.
             10 WRK-OUT-NATURE-G2TM       PIC  X(001)      VALUE SPACES.
             10 WRK-OUT-SOMPAG-G2TM.
                15 WRK-OUT-SOMPAG-G2TM-N  PIC  9(003)      VALUE ZEROS.
             10 WRK-OUT-PAGINA-G2TM.
               15 WRK-OUT-PAGINA-G2TM-N   PIC  9(003)      VALUE ZEROS.
             10 WRK-OUT-LIQEST-ATTR-G2TM  PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-LIQEST-G2TM       PIC  X(020)      VALUE SPACES.
             10 WRK-OUT-TPACES-G2TM       PIC  9(001)      VALUE ZEROS.
             10 WRK-OUT-INCONS-G2TM       PIC  X(006)      VALUE SPACES.
             10 WRK-OUT-PAGIMP-G2TM.
               15 WRK-OUT-PAGIMP-G2TM-N   PIC  9(006)      VALUE ZEROS.
             10 WRK-OUT-TOTIMP-G2TM.
                15 WRK-OUT-TOT-PG-G2TM      PIC  9(009)V9(002)
                                                           VALUE ZEROS.
                15 WRK-OUT-TOT-REC-G2TM     PIC  9(009)V9(002)
                                                           VALUE ZEROS.
                15 WRK-OUT-TOT-EST-G2TM     PIC  9(009)V9(002)
                                                           VALUE ZEROS.
                15 WRK-OUT-QTD-PG-G2TM      PIC  9(006)    VALUE ZEROS.
                15 WRK-OUT-QTD-REC-G2TM     PIC  9(006)    VALUE ZEROS.
                15 WRK-OUT-QTD-EST-G2TM     PIC  9(006)    VALUE ZEROS.
             10 WRK-OUT-BCO-G2TM.
               15 WRK-OUT-BCO-G2TM-N      PIC  9(003)      VALUE ZEROS.
             10 WRK-OUT-EMP-ATTR-G2TM     PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-EMP-G2TM.
               15 WRK-OUT-EMP-G2TM-N      PIC  9(005)      VALUE ZEROS.
             10 WRK-OUT-AGORI-ATTR-G2TM   PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-AGORI-G2TM.
               15 WRK-OUT-AGORI-G2TM-N    PIC  9(005)      VALUE ZEROS.
             10 WRK-OUT-PACB-ATTR-G2TM    PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-PACB-G2TM.
               15 WRK-OUT-PACB-G2TM-N     PIC  9(003)      VALUE ZEROS.
             10 WRK-OUT-DEDIA-ATTR-G2TM   PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-DEDIA-G2TM.
               15 WRK-OUT-DEDIA-G2TM-N    PIC  9(002)      VALUE ZEROS.
             10 WRK-OUT-DEMES-ATTR-G2TM   PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-DEMES-G2TM.
               15 WRK-OUT-DEMES-G2TM-N    PIC  9(002)      VALUE ZEROS.
             10 WRK-OUT-DEANO-ATTR-G2TM   PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-DEANO-G2TM.
               15 WRK-OUT-DEANO-G2TM-N    PIC  9(004)      VALUE ZEROS.
             10 WRK-OUT-ATEDIA-ATTR-G2TM  PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-ATEDIA-G2TM.
               15 WRK-OUT-ATEDIA-G2TM-N   PIC  9(002)      VALUE ZEROS.
             10 WRK-OUT-ATEMES-ATTR-G2TM  PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-ATEMES-G2TM.
               15 WRK-OUT-ATEMES-G2TM-N   PIC  9(002)      VALUE ZEROS.
             10 WRK-OUT-ATEANO-ATTR-G2TM  PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-ATEANO-G2TM.
               15 WRK-OUT-ATEANO-G2TM-N   PIC  9(004)      VALUE ZEROS.
             10 WRK-OUT-TRAN-ATTR-G2TM    PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-TRAN-G2TM.
               15 WRK-OUT-TRAN-G2TM-N     PIC  9(004)      VALUE ZEROS.
             10 WRK-OUT-LITPAG-ATTR-G2TM  PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-LITPAG-G2TM.
               15 WRK-OUT-LITPAG-G2TM-N   PIC  9(011)      VALUE ZEROS.
             10 WRK-OUT-TOTPAG-ATTR-G2TM  PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-TOTPAG-G2TM.
               15 WRK-OUT-TOTPAG-G2TM-N   PIC  9(014)      VALUE ZEROS.
             10 WRK-OUT-LIQPAG-ATTR-G2TM  PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-LIQPAG-G2TM.
               15 WRK-OUT-LIQPAG-G2TM-N   PIC  9(016)      VALUE ZEROS.
             10 WRK-OUT-QTDPAG-ATTR-G2TM  PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-QTDPAG-G2TM.
               15 WRK-OUT-QTDPAG-G2TM-N   PIC  9(006)      VALUE ZEROS.
             10 WRK-OUT-LITREC-ATTR-G2TM  PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-LITREC-G2TM.
               15 WRK-OUT-LITREC-G2TM-N   PIC  9(015)      VALUE ZEROS.
             10 WRK-OUT-QTDREC-ATTR-G2TM  PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-QTDREC-G2TM.
               15 WRK-OUT-QTDREC-G2TM-N   PIC  9(006)      VALUE ZEROS.
             10 WRK-OUT-CHREST-G2TM.
                15 WRK-OUT-REST-TRANS-G2TM PIC  X(004)     VALUE SPACES.
                15 WRK-OUT-REST-DTMOV-G2TM PIC  9(008)     VALUE ZEROS.
             10 WRK-OUT-TAB-OCOR-G2TM.
               15 WRK-OUT-TABELA-G2TM     OCCURS 011  TIMES.
                 20 WRK-OUT-ODATA-G2TM    PIC  X(010)      VALUE SPACES.
                 20 WRK-OUT-OTRAN-G2TM    PIC  X(004)      VALUE SPACES.
                 20 WRK-OUT-DESCR-G2TM    PIC  X(024)      VALUE SPACES.
                 20 WRK-OUT-QTDTRX-G2TM.
                   25 WRK-OUT-QTDTRX-G2TM-N
                                          PIC  9(006)      VALUE ZEROS.
                 20 WRK-OUT-VTOTCH-G2TM.
                    25 WRK-OUT-VTOTCH-G2TM-N
                                        PIC  ------.--9,99 VALUE ZEROS.
                 20 WRK-OUT-VTOTDN-G2TM.
                    25 WRK-OUT-VTOTDN-G2TM-N
                                        PIC  ------.--9,99 VALUE ZEROS.
                 20 WRK-OUT-PAGREC-G2TM PIC  X(001)      VALUE SPACES.
             10 WRK-OUT-TOTREC-ATTR-G2TM  PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-TOTREC-G2TM.
               15 WRK-OUT-TOTREC-G2TM-N   PIC  9(014)      VALUE ZEROS.
             10 WRK-OUT-LIQREC-ATTR-G2TM  PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-LIQREC-G2TM       PIC  X(020)      VALUE SPACES.
             10 WRK-OUT-LITEST-ATTR-G2TM  PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-LITEST-G2TM.
               15 WRK-OUT-LITEST-G2TM-N   PIC  9(015)      VALUE ZEROS.
             10 WRK-OUT-TOTEST-ATTR-G2TM  PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-TOTEST-G2TM.
               15 WRK-OUT-TOTEST-G2TM-N   PIC  9(014)      VALUE ZEROS.
             10 WRK-OUT-QTDEST-ATTR-G2TM  PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-QTDEST-G2TM.
               15 WRK-OUT-QTDEST-G2TM-N   PIC  9(006)      VALUE ZEROS.
             10 WRK-OUT-MENSA-G2TM        PIC  X(079)      VALUE SPACES.
                                                                                
      *---------------------------------------------------------------*         
       01  FILLER                      PIC X(070)          VALUE                
           '*** TELA BIBCG2TM - BRAD0660 ***'.
      *---------------------------------------------------------------*         

       01  WRK-660-BIBCG2TM.
           05 WRK-660-LL-AREA-G2TM        PIC  9(004) COMP VALUE  0230.
           05 WRK-660-LL-MENSAGEM-G2TM    PIC  9(004) COMP VALUE  1291.
           05 WRK-660-DADOS-TETM.
             10 WRK-660-SENHA-G2TM        PIC  9(004) COMP VALUE  0037.
             10 WRK-660-COMANDO-G2TM      PIC  9(004) COMP VALUE  1068.
             10 WRK-660-FASE-G2TM         PIC  9(004) COMP VALUE  0001.
             10 WRK-660-ULTPAG-G2TM       PIC  9(004) COMP VALUE  0001.
             10 WRK-660-NATURE-G2TM       PIC  9(004) COMP VALUE  0001.
             10 WRK-660-SOMPAG-G2TM       PIC  9(004) COMP VALUE  0003.
             10 WRK-660-PAGINA-G2TM       PIC  9(004) COMP VALUE  0003.
             10 WRK-660-LIQEST-G2TM       PIC  9(004) COMP VALUE  2022.
             10 WRK-660-TPACES-G2TM       PIC  9(004) COMP VALUE  0001.
             10 WRK-660-INCONS-G2TM       PIC  9(004) COMP VALUE  0006.
             10 WRK-660-PAGIMP-G2TM       PIC  9(004) COMP VALUE  0006.
             10 WRK-660-TOTIMP-G2TM       PIC  9(004) COMP VALUE  0051.
             10 WRK-660-BCO-G2TM          PIC  9(004) COMP VALUE  0003.
             10 WRK-660-EMP-G2TM          PIC  9(004) COMP VALUE  2007.
             10 WRK-660-AGORI-G2TM        PIC  9(004) COMP VALUE  2007.
             10 WRK-660-PACB-G2TM         PIC  9(004) COMP VALUE  2005.
             10 WRK-660-DEDIA-G2TM        PIC  9(004) COMP VALUE  3004.
             10 WRK-660-DEMES-G2TM        PIC  9(004) COMP VALUE  3004.
             10 WRK-660-DEANO-G2TM        PIC  9(004) COMP VALUE  3006.
             10 WRK-660-ATEDIA-G2TM       PIC  9(004) COMP VALUE  3004.
             10 WRK-660-ATEMES-G2TM       PIC  9(004) COMP VALUE  3004.
             10 WRK-660-ATEANO-G2TM       PIC  9(004) COMP VALUE  3006.
             10 WRK-660-TRAN-G2TM         PIC  9(004) COMP VALUE  2006.
             10 WRK-660-LITPAG-G2TM       PIC  9(004) COMP VALUE  2013.
             10 WRK-660-TOTPAG-G2TM       PIC  9(004) COMP VALUE  2016.
             10 WRK-660-LIQPAG-G2TM       PIC  9(004) COMP VALUE  2018.
             10 WRK-660-QTDPAG-G2TM       PIC  9(004) COMP VALUE  2008.
             10 WRK-660-LITREC-G2TM       PIC  9(004) COMP VALUE  2017.
             10 WRK-660-QTDREC-G2TM       PIC  9(004) COMP VALUE  2008.
             10 WRK-660-CHREST-G2TM       PIC  9(004) COMP VALUE  0012.
             10 WRK-660-TAB-OCOR-G2TM.
               15 WRK-660-TABELA-G2TM     OCCURS 011  TIMES.
                 20 WRK-660-ODATA-G2TM    PIC  9(004) COMP VALUE  0010.
                 20 WRK-660-OTRAN-G2TM    PIC  9(004) COMP VALUE  0004.
                 20 WRK-660-DESCR-G2TM    PIC  9(004) COMP VALUE  0024.
                 20 WRK-660-QTDTRX-G2TM   PIC  9(004) COMP VALUE  0006.
                 20 WRK-660-VTOTCH-G2TM   PIC  9(004) COMP VALUE  0013.
                 20 WRK-660-VTOTDN-G2TM   PIC  9(004) COMP VALUE  0013.
                 20 WRK-660-PAGREC-G2TM   PIC  9(004) COMP VALUE  0001.
             10 WRK-660-TOTREC-G2TM       PIC  9(004) COMP VALUE  2016.
             10 WRK-660-LIQREC-G2TM       PIC  9(004) COMP VALUE  2022.
             10 WRK-660-LITEST-G2TM       PIC  9(004) COMP VALUE  2017.
             10 WRK-660-TOTEST-G2TM       PIC  9(004) COMP VALUE  2016.
             10 WRK-660-QTDEST-G2TM       PIC  9(004) COMP VALUE  2008.
             10 WRK-660-MENSA-G2TM        PIC  9(004) COMP VALUE  0079.
                                                                                
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE                
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*
                                                                                
       COPY 'I#BRAD7C'.
                                                                                
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE                
           '*** AREA PARA DB2 ***'.                                             
      *----------------------------------------------------------------*
                                                                                
           EXEC SQL                                                             
               INCLUDE SQLCA                                                    
           END-EXEC.                                                            
                                                                                
           EXEC SQL                                                             
               INCLUDE BIBCB000
           END-EXEC.                                                            
                                                                                
           EXEC SQL                                                             
               INCLUDE BIBCB005
           END-EXEC.                                                            
                                                                                
           EXEC SQL                                                             
               INCLUDE YMKCB001                                                 
           END-EXEC.                                                            
                                                                                
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE                
           '*** DECLARACAO DE CURSORES ***'.                                    
      *----------------------------------------------------------------*
                                                                                
           EXEC SQL                                                             
              DECLARE  CSR01-BIBCB000      CURSOR  FOR
                SELECT
                      CAG_BCRIA_ORIGE,
                      CEMPR_INC_CORSP,
                      CTRANS_POSTO_ATDMT,
                      DTRANS_REG_POSTO,
                      QTOT_TRANS_POSTO,
                      VTOT_CHEQ_PG_TRANS,
                      VTOT_ESPCE_PG,
                      VTOT_CHEQ_RECBD,
                      VTOT_ESPCE_RECBD
                FROM   DB2PRD.SMARI_TRANS_CTBIL
                WHERE CEMPR_INC_CORSP    = :BIBCB000.CEMPR-INC-CORSP
                AND   DTRANS_REG_POSTO   BETWEEN (:WRK-DATA-INI) AND
                                                 (:WRK-DATA-FIM)
              ORDER BY
                      CEMPR_INC_CORSP ASC,
                      CAG_BCRIA_ORIGE ASC,
                      CPOSTO_ATDMT_CORSP ASC,
                      DTRANS_REG_POSTO ASC,
                      CTRANS_POSTO_ATDMT ASC
           END-EXEC.                                                            

           EXEC SQL
              DECLARE  CSR02-BIBCB000      CURSOR  FOR
                SELECT
                      CAG_BCRIA_ORIGE,
                      CEMPR_INC_CORSP,
                      CTRANS_POSTO_ATDMT,
                      DTRANS_REG_POSTO,
                      QTOT_TRANS_POSTO,
                      VTOT_CHEQ_PG_TRANS,
                      VTOT_ESPCE_PG,
                      VTOT_CHEQ_RECBD,
                      VTOT_ESPCE_RECBD
                FROM   DB2PRD.SMARI_TRANS_CTBIL
                WHERE CEMPR_INC_CORSP    = :BIBCB000.CEMPR-INC-CORSP
                AND   CAG_BCRIA_ORIGE    = :BIBCB000.CAG-BCRIA-ORIGE
                AND   DTRANS_REG_POSTO   BETWEEN (:WRK-DATA-INI) AND
                                                 (:WRK-DATA-FIM)
              ORDER BY
                      CEMPR_INC_CORSP ASC,
                      CAG_BCRIA_ORIGE ASC,
                      CPOSTO_ATDMT_CORSP ASC,
                      DTRANS_REG_POSTO ASC,
                      CTRANS_POSTO_ATDMT ASC
           END-EXEC.

           EXEC SQL
              DECLARE  CSR03-BIBCB000      CURSOR  FOR
                SELECT
                      CAG_BCRIA_ORIGE,
                      CEMPR_INC_CORSP,
                      CTRANS_POSTO_ATDMT,
                      DTRANS_REG_POSTO,
                      QTOT_TRANS_POSTO,
                      VTOT_CHEQ_PG_TRANS,
                      VTOT_ESPCE_PG,
                      VTOT_CHEQ_RECBD,
                      VTOT_ESPCE_RECBD
                FROM   DB2PRD.SMARI_TRANS_CTBIL
                WHERE CEMPR_INC_CORSP    = :BIBCB000.CEMPR-INC-CORSP
                AND   CAG_BCRIA_ORIGE    = :BIBCB000.CAG-BCRIA-ORIGE
                AND   CPOSTO_ATDMT_CORSP = :BIBCB000.CPOSTO-ATDMT-CORSP
                AND   DTRANS_REG_POSTO   BETWEEN (:WRK-DATA-INI) AND
                                                 (:WRK-DATA-FIM)
              ORDER BY
                      CEMPR_INC_CORSP ASC,
                      CAG_BCRIA_ORIGE ASC,
                      CPOSTO_ATDMT_CORSP ASC,
                      DTRANS_REG_POSTO ASC,
                      CTRANS_POSTO_ATDMT ASC
           END-EXEC.

           EXEC SQL
              DECLARE  CSR04-BIBCB000      CURSOR  FOR
                SELECT
                      CAG_BCRIA_ORIGE,
                      CEMPR_INC_CORSP,
                      CTRANS_POSTO_ATDMT,
                      DTRANS_REG_POSTO,
                      QTOT_TRANS_POSTO,
                      VTOT_CHEQ_PG_TRANS,
                      VTOT_ESPCE_PG,
                      VTOT_CHEQ_RECBD,
                      VTOT_ESPCE_RECBD
                FROM   DB2PRD.SMARI_TRANS_CTBIL
                WHERE CEMPR_INC_CORSP    = :BIBCB000.CEMPR-INC-CORSP
                AND   CTRANS_POSTO_ATDMT = :BIBCB000.CTRANS-POSTO-ATDMT
                AND   DTRANS_REG_POSTO   BETWEEN (:WRK-DATA-INI) AND
                                                 (:WRK-DATA-FIM)
              ORDER BY
                      CEMPR_INC_CORSP ASC,
                      CAG_BCRIA_ORIGE ASC,
                      CPOSTO_ATDMT_CORSP ASC,
                      DTRANS_REG_POSTO ASC,
                      CTRANS_POSTO_ATDMT ASC
           END-EXEC.

           EXEC SQL
              DECLARE  CSR05-BIBCB000      CURSOR  FOR
                SELECT
                      CAG_BCRIA_ORIGE,
                      CEMPR_INC_CORSP,
                      CTRANS_POSTO_ATDMT,
                      DTRANS_REG_POSTO,
                      QTOT_TRANS_POSTO,
                      VTOT_CHEQ_PG_TRANS,
                      VTOT_ESPCE_PG,
                      VTOT_CHEQ_RECBD,
                      VTOT_ESPCE_RECBD
                FROM   DB2PRD.SMARI_TRANS_CTBIL
                WHERE CEMPR_INC_CORSP    = :BIBCB000.CEMPR-INC-CORSP
                AND   CAG_BCRIA_ORIGE    = :BIBCB000.CAG-BCRIA-ORIGE
                AND   CTRANS_POSTO_ATDMT = :BIBCB000.CTRANS-POSTO-ATDMT
                AND   DTRANS_REG_POSTO   BETWEEN (:WRK-DATA-INI) AND
                                                 (:WRK-DATA-FIM)
              ORDER BY
                      CEMPR_INC_CORSP ASC,
                      CAG_BCRIA_ORIGE ASC,
                      CPOSTO_ATDMT_CORSP ASC,
                      DTRANS_REG_POSTO ASC,
                      CTRANS_POSTO_ATDMT ASC
           END-EXEC.

           EXEC SQL
              DECLARE  CSR06-BIBCB000      CURSOR  FOR
                SELECT
                      CAG_BCRIA_ORIGE,
                      CEMPR_INC_CORSP,
                      CTRANS_POSTO_ATDMT,
                      DTRANS_REG_POSTO,
                      QTOT_TRANS_POSTO,
                      VTOT_CHEQ_PG_TRANS,
                      VTOT_ESPCE_PG,
                      VTOT_CHEQ_RECBD,
                      VTOT_ESPCE_RECBD
                FROM   DB2PRD.SMARI_TRANS_CTBIL
                WHERE CEMPR_INC_CORSP    = :BIBCB000.CEMPR-INC-CORSP
                AND   CAG_BCRIA_ORIGE    = :BIBCB000.CAG-BCRIA-ORIGE
                AND   CPOSTO_ATDMT_CORSP = :BIBCB000.CPOSTO-ATDMT-CORSP
                AND   CTRANS_POSTO_ATDMT = :BIBCB000.CTRANS-POSTO-ATDMT
                AND   DTRANS_REG_POSTO   BETWEEN (:WRK-DATA-INI) AND
                                                 (:WRK-DATA-FIM)
              ORDER BY
                      CEMPR_INC_CORSP ASC,
                      CAG_BCRIA_ORIGE ASC,
                      CPOSTO_ATDMT_CORSP ASC,
                      DTRANS_REG_POSTO ASC,
                      CTRANS_POSTO_ATDMT ASC
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE                
           '*** FIM DA AREA DE WORKING ***'.                                    
      *----------------------------------------------------------------*
                                                                                
      *----------------------------------------------------------------*        
       LINKAGE                         SECTION.                                 
      *----------------------------------------------------------------*        
                                                                                
       01  LNK-IO-PCB.                                                          
           05  LNK-IO-LTERM            PIC  X(008).                             
           05  FILLER                  PIC  X(002).                             
           05  LNK-IO-STATUS           PIC  X(002).                             
           05  FILLER                  PIC  X(012).                             
           05  LNK-IO-MODNAME          PIC  X(008).                             
                                                                                
       01  LNK-ALT-PCB.                                                         
           05  LNK-ALT-LTERM           PIC  X(008).                             
           05  FILLER                  PIC  X(002).                             
           05  LNK-ALT-STATUS          PIC  X(002).                             
           05  FILLER                  PIC  X(012).                             
           05  LNK-ALT-MODNAME         PIC  X(008).                             
                                                                                
      *================================================================*        
       PROCEDURE                       DIVISION.                                
      *================================================================*        
                                                                                
           ENTRY 'DLITCBL'             USING LNK-IO-PCB                         
                                             LNK-ALT-PCB.                       
                                                                                
           MOVE WRK-GU                 TO    WRK-FUNCAO.                        
                                                                                
      *----------------------------------------------------------------*        
       0000-INICIAR                    SECTION.                                 
      *----------------------------------------------------------------*        
                                                                                
           CALL 'POOL5000'             USING WRK-VERSAO                         
                                             WRK-FUNCAO                         
                                             LNK-IO-PCB                         
                                             LNK-ALT-PCB                        
                                             WRK-MENSAGEM                       
                                             WRK-MODNAME                        
                                             WRK-COD-USER                       
                                             WRK-COD-DEPTO.                     
                                                                                
                                                                                
           IF  RETURN-CODE             EQUAL 04 OR 12                           
               PERFORM 5000-FINALIZAR                                           
           END-IF.                                                              
                                                                                
           MOVE SPACES                 TO WRK-OUT-DADOS-G2TM.
                                                                                
           PERFORM 1000-PROCESSAR.                                              
                                                                                
           GO TO 0000-INICIAR.                                                  
                                                                                
      *----------------------------------------------------------------*        
       0000-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       1000-PROCESSAR                  SECTION.                                 
      *----------------------------------------------------------------*        
                                                                                
           MOVE 'BIBCG2TM'             TO WRK-MODNAME.
                                                                                
           IF  WRK-TRANSACAO           NOT EQUAL 'BIBC0202'
               PERFORM 2000-RECEBER-CONTROLE                                    
           ELSE                                                                 
               PERFORM 3000-PROCESSAR-BIBCG2TM
           END-IF.
                                                                                
           IF  WRK-MODNAME             EQUAL 'BIBCG2TM'
               PERFORM 4000-PROCESSAR-BRAD0660
           END-IF.                                                              
                                                                                
      *----------------------------------------------------------------*        
       1000-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       2000-RECEBER-CONTROLE           SECTION.                                 
      *----------------------------------------------------------------*        
                                                                                
           MOVE WRK-MENSAGEM(1:LENGTH OF WRK-COMU-AREA)
                                       TO WRK-COMU-AREA.
           MOVE WRK-COMU-SENHA         TO WRK-OUT-SENHA-G2TM.
           MOVE 1                      TO WRK-OUT-FASE-G2TM-N.
           MOVE 001                    TO WRK-OUT-PAGINA-G2TM-N
                                          WRK-PAGINA.
           MOVE 237                    TO WRK-OUT-BCO-G2TM-N.
                                                                                
           PERFORM 2300-OBTER-DT-ATUAL.
           
           MOVE WRK-DIA                TO  WRK-OUT-DEDIA-G2TM-N
                                           WRK-OUT-ATEDIA-G2TM-N.
           MOVE WRK-MES                TO  WRK-OUT-DEMES-G2TM-N
                                           WRK-OUT-ATEMES-G2TM-N.
           MOVE WRK-ANO                TO  WRK-OUT-DEANO-G2TM-N
                                           WRK-OUT-ATEANO-G2TM-N.
                                                                                
           MOVE WRK-MENSA04            TO WRK-OUT-MENSA-G2TM.
                                                                                
      *----------------------------------------------------------------*        
       2000-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        

      *----------------------------------------------------------------*        
       2300-OBTER-DT-ATUAL             SECTION.
      *----------------------------------------------------------------*        
                                                                                
           CALL 'POOL7600'             USING WRK-DATA-HORA.
                                                                                
      *----------------------------------------------------------------*        
       2300-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       3000-PROCESSAR-BIBCG2TM         SECTION.
      *----------------------------------------------------------------*        
                                                                                
           MOVE WRK-MENSAGEM(1:LENGTH OF WRK-INPUT-G2TM)
                                      TO WRK-INPUT-G2TM.
                                                                                
           PERFORM 3010-INSPECT-CAMPOS.
                                                                                
           EVALUATE TRUE                                                        
      *-----> RETORNO DO HELP                                                   
               WHEN WRK-INP-PFK-G2TM   EQUAL 'H'     OR
                   (WRK-INP-COMANDO-G2TM   EQUAL 'PFK01' AND
                    WRK-INP-PFK-G2TM   EQUAL '.' )
                   PERFORM 3100-TRATAR-RET-HELP

               WHEN (WRK-INP-PFK-G2TM      EQUAL '.' AND
                     WRK-INP-COMANDO-G2TM  EQUAL 'PFK02')
                     PERFORM 3110-DESCONECTAR
                                                                                
      *-----> MENU ROTINAS                                                      
               WHEN WRK-INP-PFK-G2TM   EQUAL '5'     OR
                   (WRK-INP-COMANDO-G2TM   EQUAL 'PFK05' AND
                    WRK-INP-PFK-G2TM   EQUAL '.' )
                   PERFORM 3200-RETORNAR-MENU-ROTINAS                           
                                                                                
      *-----> RETORNA CHAMADOR                                                  
               WHEN  WRK-INP-PFK-G2TM  EQUAL '3'     OR
                    (WRK-INP-COMANDO-G2TM  EQUAL 'PFK03' AND
                    WRK-INP-PFK-G2TM   EQUAL '.' )
                   PERFORM 3300-RETORNAR-CHAMADOR                               
                                                                                
      *-----> NOVA CONSULTA                                                     
               WHEN WRK-INP-PFK-G2TM   EQUAL '9' OR
                   (WRK-INP-COMANDO-G2TM   EQUAL 'PFK09' AND
                    WRK-INP-PFK-G2TM   EQUAL '.' )
                   PERFORM 3400-LIMPA-CONSULTA                                  
                                                                                
      *-----> TRATA FASE 1                                                      
               WHEN (WRK-INP-PFK-G2TM    EQUAL '.'   AND
                     WRK-INP-FASE-G2TM-N EQUAL 1     AND
                     WRK-INP-COMANDO-G2TM  EQUAL SPACES)
                   PERFORM 3500-TRATAR-ENTER
                                                                                
      *-----> TRATA RETORNAR                                                    
               WHEN (WRK-INP-PFK-G2TM   EQUAL '7'   OR
                    (WRK-INP-COMANDO-G2TM   EQUAL 'PFK07' AND
                     WRK-INP-PFK-G2TM    EQUAL '.')) AND
                     WRK-INP-FASE-G2TM-N EQUAL 2
                   PERFORM 3600-TRATA-PF7                                       
                                                                                
      *-----> TRATA AVANCAR                                                     
               WHEN (WRK-INP-PFK-G2TM   EQUAL '8'   OR
                    (WRK-INP-COMANDO-G2TM   EQUAL 'PFK08' AND
                     WRK-INP-PFK-G2TM    EQUAL '.')) AND
                     WRK-INP-FASE-G2TM-N EQUAL 2
                   PERFORM 3700-TRATA-PF8                                       
                                                                                
      *-----> TRATA IMPRESSAO                                                   
               WHEN (WRK-INP-PFK-G2TM   EQUAL '4'   OR
                    (WRK-INP-COMANDO-G2TM   EQUAL 'PFK04' AND
                     WRK-INP-PFK-G2TM    EQUAL '.')) AND
                     WRK-INP-FASE-G2TM-N EQUAL 2
                   PERFORM 3900-TRATA-PF4                                       
                                                                                
               WHEN OTHER                                                       
                   PERFORM 3800-DEVOLVER-TELA                                   
                   MOVE SPACES             TO WRK-INCONSIS                      
                   MOVE WRK-MENSA09        TO WRK-OUT-MENSA-G2TM
                                                                                
           END-EVALUATE.                                                        
                                                                                
      *----------------------------------------------------------------*        
       3000-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       3010-INSPECT-CAMPOS             SECTION.
      *----------------------------------------------------------------*        

           IF  WRK-INP-EMP-G2TM        NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-EMP-G2TM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-AGORI-G2TM      NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-AGORI-G2TM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-PACB-G2TM       NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-PACB-G2TM   REPLACING LEADING ' ' BY '0'
           END-IF.                                                              
                                                                                
           IF  WRK-INP-DEDIA-G2TM      NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-DEDIA-G2TM  REPLACING LEADING ' ' BY '0'
           END-IF.                                                              
                                                                                
           IF  WRK-INP-DEMES-G2TM      NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-DEMES-G2TM  REPLACING LEADING ' ' BY '0'
           END-IF.                                                              
                                                                                
           IF  WRK-INP-DEANO-G2TM      NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-DEANO-G2TM  REPLACING LEADING ' ' BY '0'
           END-IF.                                                              
                                                                                
           IF  WRK-INP-ATEDIA-G2TM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-ATEDIA-G2TM REPLACING LEADING ' ' BY '0'
           END-IF.                                                              
                                                                                
           IF  WRK-INP-ATEMES-G2TM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-ATEMES-G2TM REPLACING LEADING ' ' BY '0'
           END-IF.                                                              
                                                                                
           IF  WRK-INP-ATEANO-G2TM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-ATEANO-G2TM REPLACING LEADING ' ' BY '0'
           END-IF.                                                              
                                                                                
      *----------------------------------------------------------------*        
       3010-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       3100-TRATAR-RET-HELP            SECTION.                                 
      *----------------------------------------------------------------*        
                                                                                
           MOVE WRK-INP-INCONS-G2TM    TO WRK-INCONSIS.
           MOVE WRK-INP-MENSA-G2TM     TO WRK-OUT-MENSA-G2TM.
                                                                                
           PERFORM 3800-DEVOLVER-TELA.                                          
                                                                                
      *----------------------------------------------------------------*        
       3100-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        

      *----------------------------------------------------------------*
       3110-DESCONECTAR                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA.

           MOVE 'BIBC0202'              TO WRK-MODNAME
                                           WRK-COMU-TRANCODE.
           MOVE 'DISC'                  TO WRK-COMU-COMANDO.
           MOVE WRK-CHNG                TO WRK-FUNCAO.
           MOVE WRK-INP-SENHA-G2TM      TO WRK-COMU-SENHA.
           MOVE LENGTH OF WRK-COMU-AREA TO WRK-COMU-LL.
           MOVE WRK-COMU-AREA           TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*        
       3200-RETORNAR-MENU-ROTINAS      SECTION.                                 
      *----------------------------------------------------------------*        
                                                                                
           MOVE LENGTH OF WRK-COMU-AREA                                         
                                       TO WRK-COMU-LL.                          
           MOVE  WRK-CHNG              TO WRK-FUNCAO.                           
           MOVE 'BIBC0202'             TO WRK-MODNAME.
           MOVE  WRK-INP-SENHA-G2TM    TO WRK-COMU-SENHA.
           MOVE 'PFK10'                TO WRK-COMU-COMANDO.                     
           MOVE  WRK-COMU-AREA         TO WRK-MENSAGEM.                         
                                                                                
      *----------------------------------------------------------------*        
       3200-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       3300-RETORNAR-CHAMADOR          SECTION.                                 
      *----------------------------------------------------------------*        
                                                                                
           MOVE LENGTH OF WRK-COMU-AREA                                         
                                       TO WRK-COMU-LL.                          
           MOVE  WRK-CHNG              TO WRK-FUNCAO.                           
           MOVE 'BIBC0030'             TO WRK-MODNAME.
           MOVE 'BIBC0202'             TO WRK-COMU-TRANCODE.
           MOVE  WRK-INP-SENHA-G2TM    TO WRK-COMU-SENHA.
           MOVE  WRK-COMU-AREA         TO WRK-MENSAGEM.                         
                                                                                
      *----------------------------------------------------------------*        
       3300-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       3400-LIMPA-CONSULTA             SECTION.                                 
      *----------------------------------------------------------------*        
                                                                                
           MOVE WRK-INP-SENHA-G2TM     TO WRK-OUT-SENHA-G2TM.
           MOVE 1                      TO WRK-OUT-FASE-G2TM-N.
           MOVE 001                    TO WRK-OUT-PAGINA-G2TM-N.
           MOVE 237                    TO WRK-OUT-BCO-G2TM-N.
           MOVE WRK-MENSA04            TO WRK-OUT-MENSA-G2TM.
           MOVE WRK-INP-NATURE-G2TM    TO WRK-OUT-NATURE-G2TM.
                                                                                
           PERFORM 2300-OBTER-DT-ATUAL.
           
           MOVE WRK-DIA                TO  WRK-OUT-DEDIA-G2TM-N
                                           WRK-OUT-ATEDIA-G2TM-N.
           MOVE WRK-MES                TO  WRK-OUT-DEMES-G2TM-N
                                           WRK-OUT-ATEMES-G2TM-N.
           MOVE WRK-ANO                TO  WRK-OUT-DEANO-G2TM-N
                                           WRK-OUT-ATEANO-G2TM-N.
                                                                                
           MOVE WRK-INP-LITPAG-G2TM    TO  WRK-OUT-LITPAG-G2TM.
           MOVE WRK-INP-LIQPAG-G2TM    TO  WRK-OUT-LIQPAG-G2TM.
           MOVE WRK-INP-LITREC-G2TM    TO  WRK-OUT-LITREC-G2TM.
           MOVE WRK-INP-LIQREC-G2TM    TO  WRK-OUT-LIQREC-G2TM.
           MOVE WRK-INP-LITEST-G2TM    TO  WRK-OUT-LITEST-G2TM.
           MOVE WRK-INP-LIQEST-G2TM    TO  WRK-OUT-LIQEST-G2TM.
           MOVE SPACES                 TO  WRK-INCONSIS                         
                                           WRK-INP-CHREST-G2TM
                                           WRK-INP-TOTIMP-G2TM
                                           WRK-INP-EMP-G2TM
                                           WRK-OUT-EMP-G2TM
                                           WRK-INP-AGORI-G2TM
                                           WRK-OUT-AGORI-G2TM
                                           WRK-INP-PACB-G2TM
                                           WRK-OUT-PACB-G2TM
                                           WRK-INP-TRAN-G2TM
                                           WRK-OUT-TRAN-G2TM.
                                                                                
      *----------------------------------------------------------------*        
       3400-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       3500-TRATAR-ENTER               SECTION.                                 
      *----------------------------------------------------------------*        

           PERFORM 3810-DEVOLVER-FIXOS.

           PERFORM 3510-CONSISTE-ENTRADA.

           IF  WRK-INCONSIS            NOT EQUAL SPACES AND LOW-VALUES
               GO                      TO 3500-99-FIM
           ELSE
               PERFORM 3519-VERIFICAR-DEPARTAMENTO
           END-IF.

           IF  WRK-INCONSIS            NOT EQUAL SPACES AND LOW-VALUES
               GO                      TO 3500-99-FIM
           ELSE
               IF (WRK-INP-AGORI-G2TM NOT EQUAL SPACES AND LOW-VALUES)
              AND (WRK-INP-NATURE-G2TM NOT EQUAL 'D')
                   PERFORM 3520-VERIFICAR-ACESSO-AG
               END-IF
           END-IF.

           IF  WRK-INCONSIS            NOT EQUAL SPACES AND LOW-VALUES
               GO                      TO 3500-99-FIM
           END-IF.                                                              

           MOVE WRK-INP-PAGINA-G2TM-N  TO WRK-PAGINA.

           PERFORM 3530-MONTAR-TELA.
                                                                                
      *----------------------------------------------------------------*        
       3500-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       3510-CONSISTE-ENTRADA           SECTION.
      *----------------------------------------------------------------*        

           PERFORM 3511-CONSISTENCIA-FISICA.
           
           IF  WRK-INCONSIS            EQUAL SPACES 
               PERFORM 3516-CONSISTENCIA-LOGICA
           END-IF.

      *----------------------------------------------------------------*        
       3510-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        

      *----------------------------------------------------------------*
       3511-CONSISTENCIA-FISICA        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-EMP-G2TM       NOT EQUAL SPACES AND LOW-VALUES
               IF WRK-INP-EMP-G2TM-N  NOT NUMERIC OR
                  WRK-INP-EMP-G2TM-N  EQUAL ZEROS
                  MOVE WRK-MENSA02    TO WRK-OUT-MENSA-G2TM
                  MOVE 'X'            TO WRK-INCON-EMP
                  GO                  TO 3511-99-FIM
               END-IF
           END-IF.

           IF  WRK-INP-AGORI-G2TM      NOT EQUAL SPACES AND LOW-VALUES
               IF WRK-INP-AGORI-G2TM-N NOT NUMERIC OR
                  WRK-INP-AGORI-G2TM-N EQUAL ZEROS
                  MOVE WRK-MENSA28     TO WRK-OUT-MENSA-G2TM
                  MOVE 'X'             TO WRK-INCON-AGORI
                  GO                   TO 3511-99-FIM
               END-IF
           END-IF.

           IF  WRK-INP-PACB-G2TM       NOT EQUAL SPACES AND LOW-VALUES
               IF WRK-INP-PACB-G2TM-N  NOT NUMERIC OR
                  WRK-INP-PACB-G2TM-N  EQUAL ZEROS
                  MOVE WRK-MENSA05     TO  WRK-OUT-MENSA-G2TM
                  MOVE 'X'             TO  WRK-INCON-PACB
                  GO                   TO  3511-99-FIM
               END-IF
           END-IF.

           PERFORM 3512-CONSISTIR-DATA.

           IF  WRK-INCONSIS            NOT EQUAL SPACES AND LOW-VALUES
               GO TO 3511-99-FIM
           END-IF.

           PERFORM 3515-CONSISTE-COMBINACAO.
           
      *----------------------------------------------------------------*
       3511-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3512-CONSISTIR-DATA             SECTION.
      *----------------------------------------------------------------*

           IF (WRK-INP-DEDIA-G2TM      EQUAL SPACES OR LOW-VALUES ) OR
              (WRK-INP-DEMES-G2TM      EQUAL SPACES OR LOW-VALUES ) OR
              (WRK-INP-DEANO-G2TM      EQUAL SPACES OR LOW-VALUES )
               MOVE 'X'                TO WRK-INCON-DT-DE
               MOVE WRK-MENSA15        TO WRK-OUT-MENSA-G2TM
               GO                      TO 3512-99-FIM
           ELSE
               IF WRK-INP-DEDIA-G2TM   EQUAL ZEROS OR
                  WRK-INP-DEMES-G2TM   EQUAL ZEROS OR
                  WRK-INP-DEANO-G2TM   EQUAL ZEROS
                  MOVE 'X'             TO WRK-INCON-DT-DE
                  MOVE WRK-MENSA15     TO WRK-OUT-MENSA-G2TM
                  GO                   TO 3512-99-FIM
               ELSE
                  STRING WRK-INP-DEANO-G2TM
                         WRK-INP-DEMES-G2TM
                         WRK-INP-DEDIA-G2TM
                  DELIMITED BY SIZE       INTO WRK-DATA-AAAAMMDD-A
                  IF  WRK-DATA-AAAAMMDD   NOT NUMERIC
                      MOVE 'X'            TO WRK-INCON-DT-DE
                      MOVE WRK-MENSA15    TO WRK-OUT-MENSA-G2TM
                      GO                  TO 3512-99-FIM
                  END-IF
                  PERFORM 3513-ACESSAR-POOL1205
                  IF WRK-OPCAO            EQUAL 'I'
                     MOVE 'X'             TO WRK-INCON-DT-DE
                     MOVE WRK-MENSA15     TO WRK-OUT-MENSA-G2TM
                     GO                   TO 3512-99-FIM
                  END-IF
                  MOVE WRK-DATA-AAAAMMDD  TO WRK-DATA-DE
               END-IF
           END-IF.

           IF (WRK-INP-ATEDIA-G2TM     EQUAL SPACES OR LOW-VALUES ) OR
              (WRK-INP-ATEMES-G2TM     EQUAL SPACES OR LOW-VALUES ) OR
              (WRK-INP-ATEANO-G2TM     EQUAL SPACES OR LOW-VALUES )
               MOVE 'X'                TO WRK-INCON-DT-ATE
               MOVE WRK-MENSA16        TO WRK-OUT-MENSA-G2TM
               GO                      TO 3512-99-FIM
           ELSE
               IF WRK-INP-ATEDIA-G2TM  EQUAL ZEROS OR
                  WRK-INP-ATEMES-G2TM  EQUAL ZEROS OR
                  WRK-INP-ATEANO-G2TM  EQUAL ZEROS
                  MOVE 'X'             TO WRK-INCON-DT-ATE
                  MOVE WRK-MENSA16     TO WRK-OUT-MENSA-G2TM
                  GO                   TO 3512-99-FIM
               ELSE
                  STRING WRK-INP-ATEANO-G2TM
                         WRK-INP-ATEMES-G2TM
                         WRK-INP-ATEDIA-G2TM
                  DELIMITED BY SIZE       INTO WRK-DATA-AAAAMMDD-A
                  IF  WRK-DATA-AAAAMMDD   NOT NUMERIC
                      MOVE 'X'            TO WRK-INCON-DT-ATE
                      MOVE WRK-MENSA16    TO WRK-OUT-MENSA-G2TM
                      GO                  TO 3512-99-FIM
                  END-IF
                  PERFORM 3513-ACESSAR-POOL1205
                  IF WRK-OPCAO            EQUAL 'I'
                     MOVE 'X'             TO WRK-INCON-DT-ATE
                     MOVE WRK-MENSA16     TO WRK-OUT-MENSA-G2TM
                     GO                   TO 3512-99-FIM
                  END-IF
                  MOVE WRK-DATA-AAAAMMDD  TO WRK-DATA-ATE
               END-IF
           END-IF.

           IF WRK-DATA-DE              GREATER WRK-DATA-ATE
              MOVE 'X'                 TO WRK-INCON-DT-DE
                                          WRK-INCON-DT-ATE
              MOVE WRK-MENSA17         TO WRK-OUT-MENSA-G2TM
              GO                       TO 3512-99-FIM
           END-IF.

           PERFORM 2300-OBTER-DT-ATUAL.

           MOVE WRK-DIA                TO WRK-DATA-DD.
           MOVE WRK-MES                TO WRK-DATA-MM.
           MOVE WRK-ANO                TO WRK-DATA-AAAA.
           MOVE WRK-DATA-AAAAMMDD      TO WRK-DATA-TRANS.

           IF  WRK-DATA-DE             GREATER WRK-DATA-TRANS 
               MOVE 'X'                TO WRK-INCON-DT-DE
               MOVE WRK-MENSA11        TO WRK-OUT-MENSA-G2TM
               GO                      TO 3512-99-FIM
           END-IF.

           IF  WRK-DATA-ATE            GREATER WRK-DATA-TRANS
               MOVE 'X'                TO WRK-INCON-DT-ATE
               MOVE WRK-MENSA11        TO WRK-OUT-MENSA-G2TM
               GO                      TO 3512-99-FIM
           END-IF.

           PERFORM 3514-CALCULAR-DIF-DATA.

      *----------------------------------------------------------------*
       3512-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3513-ACESSAR-POOL1205           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-DATA-POOL1205.
           
           MOVE SPACES                TO WRK-MSG-1205.
           MOVE WRK-DATA-AAAAMMDD     TO WRK-DT-ENVIADA.
                                         
           CALL 'POOL1205'            USING WRK-DATA-POOL1205
                                            WRK-MSG-1205.
                                            
           IF  RETURN-CODE             EQUAL 4
               MOVE WRK-MSG-1205      TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       3513-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3514-CALCULAR-DIF-DATA   SECTION.
      *----------------------------------------------------------------*
           
           
           STRING WRK-INP-DEDIA-G2TM
                  WRK-INP-DEMES-G2TM
                  WRK-INP-DEANO-G2TM
           DELIMITED BY SIZE           INTO WRK-DT-INFERIOR-0015.

           STRING WRK-INP-ATEDIA-G2TM
                  WRK-INP-ATEMES-G2TM
                  WRK-INP-ATEANO-G2TM
           DELIMITED BY SIZE           INTO WRK-DT-SUPERIOR-0015.

           CALL 'BRAD0015'             USING WRK-DT-SUPERIOR-0015
                                             WRK-DT-INFERIOR-0015
                                             WRK-NRO-DIAS-0015.

           MOVE WRK-NRO-DIAS-0015    TO WRK-S9-5.

           IF  WRK-09-5                GREATER 7
               MOVE 'X'                TO  WRK-INCON-DT-DE
                                           WRK-INCON-DT-ATE
               MOVE WRK-MENSA14        TO  WRK-OUT-MENSA-G2TM
           END-IF.

      *----------------------------------------------------------------*
       3514-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3515-CONSISTE-COMBINACAO        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-EMP-G2TM        NOT EQUAL SPACES
               IF WRK-INP-PACB-G2TM    NOT EQUAL SPACES
                  MOVE 'X'             TO  WRK-INCON-EMP
                                           WRK-INCON-PACB
                  MOVE WRK-MENSA12     TO  WRK-OUT-MENSA-G2TM
                  
               ELSE
                  IF WRK-INP-AGORI-G2TM EQUAL SPACES OR LOW-VALUES
                     MOVE 1             TO WRK-INP-TPACES-G2TM
                                           WRK-OUT-TPACES-G2TM
                  ELSE
                     MOVE 2             TO WRK-INP-TPACES-G2TM
                                           WRK-OUT-TPACES-G2TM
                  END-IF
               END-IF
           ELSE
               IF WRK-INP-AGORI-G2TM   NOT EQUAL SPACES AND LOW-VALUES
                  IF WRK-INP-PACB-G2TM EQUAL SPACES OR LOW-VALUES
                     MOVE 'X'          TO WRK-INCON-AGORI
                                          WRK-INCON-PACB
                     MOVE WRK-MENSA12  TO WRK-OUT-MENSA-G2TM
                  ELSE
                     MOVE 3             TO WRK-INP-TPACES-G2TM
                                           WRK-OUT-TPACES-G2TM
                  END-IF
               ELSE
                   MOVE 'X'            TO WRK-INCON-EMP
                                          WRK-INCON-AGORI
                                          WRK-INCON-PACB
                   MOVE WRK-MENSA12    TO WRK-OUT-MENSA-G2TM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3515-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3516-CONSISTENCIA-LOGICA        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-EMP-G2TM       NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 3517-VALIDAR-EMPRESA
           END-IF.

           IF  WRK-INCONSIS           NOT EQUAL SPACES AND LOW-VALUES
               GO TO 3516-99-FIM
           ELSE
               IF (WRK-INP-AGORI-G2TM NOT EQUAL SPACES AND LOW-VALUES)
               AND (WRK-INP-PACB-G2TM NOT EQUAL SPACES AND LOW-VALUES)
                   PERFORM 3518-VALIDAR-AGE-PACB
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3516-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*        
       3517-VALIDAR-EMPRESA            SECTION.
      *----------------------------------------------------------------*        
                                                                                
           MOVE  WRK-INP-EMP-G2TM-N    TO CEMPR-CORSP     OF YMKCB001.
                                                                                
           EXEC SQL                                                             
                SELECT CEMPR_CORSP                                              
                INTO  :YMKCB001.CEMPR-CORSP                                     
                FROM   DB2PRD.TCAD_CORSP_BCRIO                                  
                WHERE  CEMPR_CORSP    = :YMKCB001.CEMPR-CORSP
           END-EXEC.

           IF  SQLCODE                NOT EQUAL ZEROS AND +100 AND -811
               MOVE 'DB2'             TO ERR-TIPO-ACESSO
               MOVE 'TCAD_CORSP_BCRIO'
                                      TO ERR-DBD-TAB
               MOVE 'SELECT  '        TO ERR-FUN-COMANDO
               MOVE  SQLCODE          TO ERR-SQL-CODE
               MOVE '0010'            TO ERR-LOCAL
               MOVE  SPACES           TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.
                                                                                
           IF  SQLCODE                  EQUAL  +100
               MOVE WRK-MENSA06         TO WRK-OUT-MENSA-G2TM
               MOVE 'X'                 TO WRK-INCON-EMP
           END-IF.                                                              
                                                                                
      *----------------------------------------------------------------*        
       3517-99-FIM.                    EXIT.
      *----------------------------------------------------------------*        

      *----------------------------------------------------------------*
       3518-VALIDAR-AGE-PACB           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-AGORI-G2TM-N   TO CAG-BCRIA       OF YMKCB001.
           MOVE WRK-INP-PACB-G2TM-N    TO CPOSTO-BCRIO    OF YMKCB001.

           EXEC SQL
                SELECT CEMPR_CORSP
                INTO  :YMKCB001.CEMPR-CORSP
                FROM   DB2PRD.TCAD_CORSP_BCRIO
                WHERE  CAG_BCRIA    = :YMKCB001.CAG-BCRIA AND
                       CPOSTO_BCRIO = :YMKCB001.CPOSTO-BCRIO 
           END-EXEC.

           IF  SQLCODE                NOT EQUAL ZEROS AND +100
               MOVE 'DB2'             TO ERR-TIPO-ACESSO
               MOVE 'TCAD_CORSP_BCRIO'
                                      TO ERR-DBD-TAB
               MOVE 'SELECT  '        TO ERR-FUN-COMANDO
               MOVE  SQLCODE          TO ERR-SQL-CODE
               MOVE '0020'            TO ERR-LOCAL
               MOVE  SPACES           TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                  EQUAL  +100
               MOVE WRK-MENSA13         TO WRK-OUT-MENSA-G2TM
               MOVE 'X'                 TO WRK-INCON-AGORI
                                           WRK-INCON-PACB
           ELSE
               MOVE CEMPR-CORSP         OF YMKCB001
                                        TO WRK-S9-5
               MOVE WRK-09-5            TO WRK-OUT-EMP-G2TM-N
           END-IF.

      *----------------------------------------------------------------*
       3518-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3519-VERIFICAR-DEPARTAMENTO     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-POOL7100
                                          WRK-SQLCA
                                          COMU-PARM.

           INITIALIZE COMU-PARM.

           MOVE  WRK-OUT-BCO-G2TM-N    TO COMU-BANCO.
           MOVE  'BIBC0202'            TO COMU-NOME-PGM.
           MOVE  WRK-COD-DEPTO-N       TO COMU-DEPDC.
           MOVE  'MESU9015'            TO WRK-MODULO.

           CALL WRK-MODULO             USING COMU-PARM
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF COMU-COD-RETORNO         EQUAL ZEROS
               IF COMU-NATUREZA-DEPDC  NOT EQUAL 'G' AND   'R'
                                           AND   'A' AND   'D'
                  MOVE  'P'            TO  WRK-INP-NATURE-G2TM
                                           WRK-OUT-NATURE-G2TM
               ELSE
                  MOVE COMU-NATUREZA-DEPDC
                                       TO WRK-INP-NATURE-G2TM
                                          WRK-OUT-NATURE-G2TM
               END-IF
           ELSE
               MOVE  WRK-MENSA23       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.
           
      *----------------------------------------------------------------*
       3519-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3520-VERIFICAR-ACESSO-AG        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  MESU9023-PARM
                                       MESU9023-AMBIENTE
                                       MESU9023-IO-PCB
                                       MESU9023-ALT-PCB
                                       MESU9023-BRAD7100
                                       MESU9023-SQLCA.

           MOVE 'BIBC0202'             TO  MESU9023-NOME-PGM.
           MOVE 'O'                    TO  MESU9023-AMBIENTE.
           MOVE 237                    TO  MESU9023-EMPRESA.
           MOVE WRK-INP-AGORI-G2TM-N   TO  MESU9023-DEPENDENCIA.

           EVALUATE WRK-INP-NATURE-G2TM
                WHEN 'G'
                   MOVE 17             TO MESU9023-TIPO-RELAC
                WHEN 'R'
                   MOVE 11             TO MESU9023-TIPO-RELAC
                WHEN 'O'
                   MOVE 04             TO MESU9023-TIPO-RELAC
           END-EVALUATE.

           MOVE 'MESU9023'             TO WRK-MODULO.

           CALL WRK-MODULO             USING MESU9023-AMBIENTE
                                             MESU9023-IO-PCB
                                             MESU9023-ALT-PCB
                                             MESU9023-PARM
                                             MESU9023-BRAD7100
                                             MESU9023-SQLCA.

           IF  MESU9023-COD-RETORNO    NOT EQUAL ZEROS AND 01
               MOVE WRK-MENSA21        TO WRK-OUT-MENSA-G2TM
               MOVE 'X'                TO WRK-INCON-AGORI
               GO TO 3520-99-FIM
           END-IF.

           IF  MESU9023-COD-RETORNO    EQUAL 01
               IF WRK-INP-AGORI-G2TM-N NOT EQUAL MESU9023-DEPTO-RELAC
                  MOVE WRK-MENSA22     TO WRK-OUT-MENSA-G2TM
                  MOVE 'X'             TO WRK-INCON-AGORI
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*        
       3530-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*        

           IF  WRK-INP-TRAN-G2TM       EQUAL SPACES OR LOW-VALUES
               PERFORM 3531-TRATAR-CURSOR-SEM-TRAN
           ELSE
               PERFORM 3540-TRATAR-CURSOR-TRAN
           END-IF.

           IF  WRK-OUT-ODATA-G2TM(1)   EQUAL SPACES
               MOVE WRK-MENSA18        TO WRK-OUT-MENSA-G2TM
               MOVE 1                  TO WRK-OUT-FASE-G2TM-N
               PERFORM 3538-TRATAR-INCONSISTENCIA
           ELSE
               MOVE WRK-MENSA10        TO WRK-OUT-MENSA-G2TM
               MOVE 2                  TO WRK-OUT-FASE-G2TM-N
           END-IF.
                                                                                
      *----------------------------------------------------------------*        
       3530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*        

      *----------------------------------------------------------------*
       3531-TRATAR-CURSOR-SEM-TRAN     SECTION.
      *----------------------------------------------------------------*

           PERFORM 3532-VALORIZAR-CHAVE-CSR.

           PERFORM 3533-OPEN-CSR-SEM-TRAN.

           COMPUTE WRK-DESPREZA          = (WRK-PAGINA - 1) * 11 + 1.
           
           MOVE 01                       TO IND-1.

           PERFORM 3534-FETCH-CSR-SEM-TRAN UNTIL
                   IND-1                 GREATER WRK-DESPREZA OR
                   WRK-FIM-CURSOR        EQUAL 'S'.

           MOVE 01                       TO IND-1.

           PERFORM UNTIL IND-1           GREATER 11 OR
                     WRK-FIM-CURSOR      EQUAL 'S'
               PERFORM 3536-MONTAR-OCORRENCIAS
               PERFORM 3534-FETCH-CSR-SEM-TRAN
           END-PERFORM.

           IF  WRK-FIM-CURSOR            EQUAL 'S'
               MOVE 'S'                  TO WRK-INP-ULTPAG-G2TM
                                            WRK-OUT-ULTPAG-G2TM
           ELSE
               MOVE SPACES               TO WRK-INP-ULTPAG-G2TM
                                            WRK-OUT-ULTPAG-G2TM
           END-IF.

           MOVE WRK-PAGINA               TO WRK-OUT-PAGINA-G2TM-N.

           PERFORM 3535-CLOSE-CSR-SEM-TRAN.

      *----------------------------------------------------------------*
       3531-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3532-VALORIZAR-CHAVE-CSR        SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-OUT-EMP-G2TM-N     TO CEMPR-INC-CORSP OF BIBCB000.
               
           EVALUATE WRK-INP-TPACES-G2TM
             WHEN 1
               CONTINUE
               
             WHEN 2
               MOVE WRK-INP-AGORI-G2TM-N
                                       TO CAG-BCRIA-ORIGE OF BIBCB000

             WHEN 3
               MOVE WRK-INP-AGORI-G2TM-N
                                       TO CAG-BCRIA-ORIGE OF BIBCB000
               MOVE WRK-INP-PACB-G2TM-N
                                       TO CPOSTO-ATDMT-CORSP OF BIBCB000
           END-EVALUATE.

           STRING  WRK-INP-DEDIA-G2TM-N
                   '.'
                   WRK-INP-DEMES-G2TM-N
                   '.'
                   WRK-INP-DEANO-G2TM-N
           DELIMITED BY SIZE INTO WRK-DATA-INI.

           STRING  WRK-INP-ATEDIA-G2TM-N
                   '.'
                   WRK-INP-ATEMES-G2TM-N
                   '.'
                   WRK-INP-ATEANO-G2TM-N
           DELIMITED BY SIZE INTO WRK-DATA-FIM.

      *----------------------------------------------------------------*
       3532-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*        
       3533-OPEN-CSR-SEM-TRAN          SECTION.
      *----------------------------------------------------------------*        

           MOVE SPACES                   TO WRK-FIM-CURSOR.

           EVALUATE WRK-INP-TPACES-G2TM
             WHEN 1
               EXEC SQL
                   OPEN CSR01-BIBCB000
               END-EXEC
               
             WHEN 2
               EXEC SQL
                   OPEN CSR02-BIBCB000
               END-EXEC
               
             WHEN 3
               EXEC SQL
                   OPEN CSR03-BIBCB000
               END-EXEC
           END-EVALUATE.
                                                                                
           IF (SQLCODE                 NOT EQUAL ZEROS) OR                      
              (SQLWARN0                EQUAL 'W'      )                         
               MOVE 'DB2'              TO ERR-TIPO-ACESSO                       
               MOVE 'SMARI_TRANS_CTBIL'
                                       TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO                       
               MOVE  SQLCODE           TO ERR-SQL-CODE                          
               MOVE '0030'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM                              
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                               
           END-IF.                                                              
                                                                                
      *----------------------------------------------------------------*        
       3533-99-FIM.                    EXIT.
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       3534-FETCH-CSR-SEM-TRAN         SECTION.
      *----------------------------------------------------------------*        

           EVALUATE WRK-INP-TPACES-G2TM
             WHEN 1
               EXEC SQL
                   FETCH CSR01-BIBCB000 INTO
                      :BIBCB000.CAG-BCRIA-ORIGE,
                      :BIBCB000.CEMPR-INC-CORSP,
                      :BIBCB000.CTRANS-POSTO-ATDMT,
                      :BIBCB000.DTRANS-REG-POSTO,
                      :BIBCB000.QTOT-TRANS-POSTO,
                      :BIBCB000.VTOT-CHEQ-PG-TRANS,
                      :BIBCB000.VTOT-ESPCE-PG,
                      :BIBCB000.VTOT-CHEQ-RECBD,
                      :BIBCB000.VTOT-ESPCE-RECBD
               END-EXEC

             WHEN 2
               EXEC SQL
                   FETCH CSR02-BIBCB000 INTO
                      :BIBCB000.CAG-BCRIA-ORIGE,
                      :BIBCB000.CEMPR-INC-CORSP,
                      :BIBCB000.CTRANS-POSTO-ATDMT,
                      :BIBCB000.DTRANS-REG-POSTO,
                      :BIBCB000.QTOT-TRANS-POSTO,
                      :BIBCB000.VTOT-CHEQ-PG-TRANS,
                      :BIBCB000.VTOT-ESPCE-PG,
                      :BIBCB000.VTOT-CHEQ-RECBD,
                      :BIBCB000.VTOT-ESPCE-RECBD
               END-EXEC

             WHEN 3
               EXEC SQL
                   FETCH CSR03-BIBCB000 INTO
                      :BIBCB000.CAG-BCRIA-ORIGE,
                      :BIBCB000.CEMPR-INC-CORSP,
                      :BIBCB000.CTRANS-POSTO-ATDMT,
                      :BIBCB000.DTRANS-REG-POSTO,
                      :BIBCB000.QTOT-TRANS-POSTO,
                      :BIBCB000.VTOT-CHEQ-PG-TRANS,
                      :BIBCB000.VTOT-ESPCE-PG,
                      :BIBCB000.VTOT-CHEQ-RECBD,
                      :BIBCB000.VTOT-ESPCE-RECBD
               END-EXEC
           END-EVALUATE.
                                                                                
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR             
              (SQLWARN0                EQUAL 'W' )                              
               MOVE 'DB2'              TO ERR-TIPO-ACESSO                       
               MOVE 'SMARI_TRANS_CTBIL'
                                       TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE                          
               MOVE '0040'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM                              
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                               
           END-IF.                                                              
                                                                                
           IF  SQLCODE                 EQUAL +100                               
               MOVE 'S'                TO WRK-FIM-CURSOR
           ELSE
               IF WRK-INP-NATURE-G2TM  EQUAL 'A'
                  MOVE CAG-BCRIA-ORIGE OF BIBCB000
                                       TO WRK-S9-5-4
                  IF WRK-09-5-4        EQUAL WRK-COD-DEPTO-N
                     ADD 1             TO IND-1
                  END-IF
               ELSE
                  ADD 1                TO IND-1
               END-IF
               
           END-IF.
                                                                                
      *----------------------------------------------------------------*        
       3534-99-FIM.                    EXIT.
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       3535-CLOSE-CSR-SEM-TRAN         SECTION.
      *----------------------------------------------------------------*        

           EVALUATE WRK-INP-TPACES-G2TM
             WHEN 1
               EXEC SQL
                   CLOSE CSR01-BIBCB000
               END-EXEC

             WHEN 2
               EXEC SQL
                   CLOSE CSR02-BIBCB000
               END-EXEC

             WHEN 3
               EXEC SQL
                   CLOSE CSR03-BIBCB000
               END-EXEC
           END-EVALUATE.
                                                                                
           IF (SQLCODE                 NOT EQUAL ZEROS) OR                      
              (SQLWARN0                EQUAL 'W'      )                         
               MOVE 'DB2'              TO ERR-TIPO-ACESSO                       
               MOVE 'SMARI_TRANS_CTBIL'
                                       TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE                          
               MOVE '0050'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM                              
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                               
           END-IF.                                                              
                                                                                
      *----------------------------------------------------------------*        
       3535-99-FIM.                    EXIT.
      *----------------------------------------------------------------*        

      *----------------------------------------------------------------*
       3536-MONTAR-OCORRENCIAS         SECTION.
      *----------------------------------------------------------------*

      *DESPREZA REGISTRO COM CAG-BCRIA-ORIGE IGUAL A WRK-COD-DEPTO
      *QUANDO WRK-INP-NATURE-G2TM IGUAL A 'A'.
           IF  WRK-INP-NATURE-G2TM    EQUAL 'A'
               MOVE CAG-BCRIA-ORIGE   OF BIBCB000
                                      TO WRK-S9-5-4
               IF WRK-09-5-4          NOT EQUAL WRK-COD-DEPTO-N
                  GO TO 3536-99-FIM
               END-IF
           END-IF.

           MOVE DTRANS-REG-POSTO       OF BIBCB000
                                       TO WRK-OUT-ODATA-G2TM(IND-1).

           INSPECT WRK-OUT-ODATA-G2TM(IND-1)   REPLACING ALL '.' BY '/'.

           MOVE CTRANS-POSTO-ATDMT     OF BIBCB000
                                       TO WRK-OUT-OTRAN-G2TM(IND-1).

           PERFORM 3537-OBTER-DESC-TRANSACAO.

           MOVE QTOT-TRANS-POSTO       OF BIBCB000
                                       TO WRK-S9-9.
           MOVE WRK-09-9-6             TO WRK-OUT-QTDTRX-G2TM-N(IND-1)
                                          
           IF  CCLASF-TRANS-POSTO      OF BIBCB005 EQUAL 1
               MOVE 'P'                TO WRK-OUT-PAGREC-G2TM(IND-1)
               MOVE VTOT-CHEQ-PG-TRANS OF BIBCB000
                                       TO WRK-S9-15V2
               MOVE WRK-S9-15-8V2      TO WRK-OUT-VTOTCH-G2TM-N(IND-1)
               MOVE VTOT-ESPCE-PG      OF BIBCB000
                                       TO WRK-S9-15V2
               MOVE WRK-S9-15-8V2      TO WRK-OUT-VTOTDN-G2TM-N(IND-1)
           ELSE
               MOVE 'R'                TO WRK-OUT-PAGREC-G2TM(IND-1)
               MOVE VTOT-CHEQ-RECBD    OF BIBCB000
                                       TO WRK-S9-15V2
               MOVE WRK-S9-15-8V2      TO WRK-OUT-VTOTCH-G2TM-N(IND-1)
               MOVE VTOT-ESPCE-RECBD   OF BIBCB000
                                       TO WRK-S9-15V2
               MOVE WRK-S9-15-8V2      TO WRK-OUT-VTOTDN-G2TM-N(IND-1)
           END-IF.

      *----------------------------------------------------------------*
       3536-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3537-OBTER-DESC-TRANSACAO      SECTION.
      *----------------------------------------------------------------*

           MOVE 237                   TO CEMPR-INC          OF BIBCB005.
           MOVE CEMPR-INC-CORSP       OF BIBCB000
                                      TO CEMPR-INC-CORSP    OF BIBCB005.
           MOVE CTRANS-POSTO-ATDMT    OF BIBCB000
                                      TO CTRANS-POSTO-ATDMT OF BIBCB005.

           EXEC SQL
               SELECT  RTRANS_POSTO_ATDMT,
                       CCLASF_TRANS_POSTO
               INTO   :BIBCB005.RTRANS-POSTO-ATDMT,
                      :BIBCB005.CCLASF-TRANS-POSTO
               FROM    DB2PRD.TRANS_POSTO_ATDMT
               WHERE   CEMPR_INC        = :BIBCB005.CEMPR-INC
               AND     CEMPR_INC_CORSP  = :BIBCB005.CEMPR-INC-CORSP
               AND     CTRANS_POSTO_ATDMT = :BIBCB005.CTRANS-POSTO-ATDMT
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W' )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TRANS_POSTO_ATDMT'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0060'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE RTRANS-POSTO-ATDMT OF BIBCB005
               (1:LENGTH OF WRK-OUT-DESCR-G2TM(IND-1))
                                       TO WRK-OUT-DESCR-G2TM(IND-1)
           ELSE
               MOVE ALL '*'            TO WRK-OUT-DESCR-G2TM(IND-1)
           END-IF.

      *----------------------------------------------------------------*
       3537-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3538-TRATAR-INCONSISTENCIA      SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-EMP-G2TM        NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'X'                TO WRK-INCON-EMP
           ELSE
               MOVE SPACES             TO WRK-OUT-EMP-G2TM
           END-IF.

           IF  WRK-INP-AGORI-G2TM      NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'X'                TO WRK-INCON-AGORI
           ELSE
               MOVE SPACES             TO WRK-OUT-AGORI-G2TM
           END-IF.

           IF  WRK-INP-PACB-G2TM       NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'X'                TO  WRK-INCON-PACB
           ELSE
               MOVE SPACES             TO WRK-OUT-PACB-G2TM
           END-IF.

      *----------------------------------------------------------------*
       3538-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3540-TRATAR-CURSOR-TRAN         SECTION.
      *----------------------------------------------------------------*

           PERFORM 3532-VALORIZAR-CHAVE-CSR.

           PERFORM 3541-OPEN-CSR-TRAN.

           COMPUTE WRK-DESPREZA          = (WRK-PAGINA - 1) * 11 + 1.
           
           MOVE 01                       TO IND-1.

           PERFORM 3542-FETCH-CSR-TRAN   UNTIL
                   IND-1                 GREATER WRK-DESPREZA OR
                   WRK-FIM-CURSOR        EQUAL 'S'.

           MOVE 01                       TO IND-1.

           PERFORM UNTIL IND-1           GREATER 11 OR
                     WRK-FIM-CURSOR      EQUAL 'S'
               PERFORM 3536-MONTAR-OCORRENCIAS
               PERFORM 3542-FETCH-CSR-TRAN
           END-PERFORM.

           IF  WRK-FIM-CURSOR            EQUAL 'S'
               MOVE 'S'                  TO WRK-INP-ULTPAG-G2TM
                                            WRK-OUT-ULTPAG-G2TM
           ELSE
               MOVE SPACES               TO WRK-INP-ULTPAG-G2TM
                                            WRK-OUT-ULTPAG-G2TM
           END-IF.

           MOVE WRK-PAGINA               TO WRK-OUT-PAGINA-G2TM-N.

           PERFORM 3543-CLOSE-CSR-TRAN.

      *----------------------------------------------------------------*
       3540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*        
       3541-OPEN-CSR-TRAN              SECTION.
      *----------------------------------------------------------------*        

           MOVE SPACES                 TO WRK-FIM-CURSOR.
           MOVE WRK-INP-TRAN-G2TM      TO CTRANS-POSTO-ATDMT
                                                       OF BIBCB000.

           EVALUATE WRK-INP-TPACES-G2TM
             WHEN 1
               EXEC SQL
                   OPEN CSR04-BIBCB000
               END-EXEC

             WHEN 2
               EXEC SQL
                   OPEN CSR05-BIBCB000
               END-EXEC

             WHEN 3
               EXEC SQL
                   OPEN CSR06-BIBCB000
               END-EXEC
           END-EVALUATE.
                                                                                
           IF (SQLCODE                 NOT EQUAL ZEROS) OR                      
              (SQLWARN0                EQUAL 'W'      )                         
               MOVE 'DB2'              TO ERR-TIPO-ACESSO                       
               MOVE 'SMARI_TRANS_CTBIL'
                                       TO ERR-DBD-TAB                           
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO                       
               MOVE  SQLCODE           TO ERR-SQL-CODE                          
               MOVE '0070'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM                              
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                               
           END-IF.                                                              
                                                                                
      *----------------------------------------------------------------*        
       3541-99-FIM.                    EXIT.
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       3542-FETCH-CSR-TRAN             SECTION.
      *----------------------------------------------------------------*        

           EVALUATE WRK-INP-TPACES-G2TM
             WHEN 1
               EXEC SQL
                   FETCH CSR04-BIBCB000 INTO
                      :BIBCB000.CAG-BCRIA-ORIGE,
                      :BIBCB000.CEMPR-INC-CORSP,
                      :BIBCB000.CTRANS-POSTO-ATDMT,
                      :BIBCB000.DTRANS-REG-POSTO,
                      :BIBCB000.QTOT-TRANS-POSTO,
                      :BIBCB000.VTOT-CHEQ-PG-TRANS,
                      :BIBCB000.VTOT-ESPCE-PG,
                      :BIBCB000.VTOT-CHEQ-RECBD,
                      :BIBCB000.VTOT-ESPCE-RECBD
               END-EXEC

             WHEN 2
               EXEC SQL
                   FETCH CSR05-BIBCB000 INTO
                      :BIBCB000.CAG-BCRIA-ORIGE,
                      :BIBCB000.CEMPR-INC-CORSP,
                      :BIBCB000.CTRANS-POSTO-ATDMT,
                      :BIBCB000.DTRANS-REG-POSTO,
                      :BIBCB000.QTOT-TRANS-POSTO,
                      :BIBCB000.VTOT-CHEQ-PG-TRANS,
                      :BIBCB000.VTOT-ESPCE-PG,
                      :BIBCB000.VTOT-CHEQ-RECBD,
                      :BIBCB000.VTOT-ESPCE-RECBD
               END-EXEC

             WHEN 3
               EXEC SQL
                   FETCH CSR06-BIBCB000 INTO
                      :BIBCB000.CAG-BCRIA-ORIGE,
                      :BIBCB000.CEMPR-INC-CORSP,
                      :BIBCB000.CTRANS-POSTO-ATDMT,
                      :BIBCB000.DTRANS-REG-POSTO,
                      :BIBCB000.QTOT-TRANS-POSTO,
                      :BIBCB000.VTOT-CHEQ-PG-TRANS,
                      :BIBCB000.VTOT-ESPCE-PG,
                      :BIBCB000.VTOT-CHEQ-RECBD,
                      :BIBCB000.VTOT-ESPCE-RECBD
               END-EXEC
           END-EVALUATE.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR             
              (SQLWARN0                EQUAL 'W' )                              
               MOVE 'DB2'              TO ERR-TIPO-ACESSO                       
               MOVE 'SMARI_TRANS_CTBIL'
                                       TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE                          
               MOVE '0080'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM                              
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                               
           END-IF.                                                              

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
           ELSE
               IF WRK-INP-NATURE-G2TM  EQUAL 'A'
                  MOVE CAG-BCRIA-ORIGE OF BIBCB000
                                       TO WRK-S9-5-4
                  IF WRK-09-5-4        EQUAL WRK-COD-DEPTO-N
                     ADD 1             TO IND-1
                  END-IF
               ELSE
                  ADD 1                TO IND-1
               END-IF
           END-IF.
                                                                                
      *----------------------------------------------------------------*        
       3542-99-FIM.                    EXIT.
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       3543-CLOSE-CSR-TRAN             SECTION.
      *----------------------------------------------------------------*        

           EVALUATE WRK-INP-TPACES-G2TM
             WHEN 1
               EXEC SQL
                   CLOSE CSR04-BIBCB000
               END-EXEC

             WHEN 2
               EXEC SQL
                   CLOSE CSR05-BIBCB000
               END-EXEC

             WHEN 3
               EXEC SQL
                   CLOSE CSR06-BIBCB000
               END-EXEC
           END-EVALUATE.
                                                                                
           IF (SQLCODE                 NOT EQUAL ZEROS) OR                      
              (SQLWARN0                EQUAL 'W'      )                         
               MOVE 'DB2'              TO ERR-TIPO-ACESSO                       
               MOVE 'SMARI_TRANS_CTBIL'
                                       TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE                          
               MOVE '0090'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM                              
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                               
           END-IF.                                                              
                                                                                
      *----------------------------------------------------------------*        
       3543-99-FIM.                    EXIT.
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       3600-TRATA-PF7                  SECTION.                                 
      *----------------------------------------------------------------*        
                                                                                
           IF  WRK-INP-PAGINA-G2TM-N   EQUAL  1
               PERFORM 3800-DEVOLVER-TELA                                       
               MOVE WRK-MENSA08        TO WRK-OUT-MENSA-G2TM
               GO TO 3600-99-FIM                                                
           END-IF.

           MOVE SPACES                 TO WRK-OUT-TAB-OCOR-G2TM.
           MOVE WRK-INP-PAGINA-G2TM-N  TO WRK-PAGINA.

           SUBTRACT  1                 FROM WRK-PAGINA.
                                                                                
           PERFORM 3810-DEVOLVER-FIXOS.
           
           PERFORM 3530-MONTAR-TELA.
                                                                                
      *----------------------------------------------------------------*        
       3600-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       3700-TRATA-PF8                  SECTION.                                 
      *----------------------------------------------------------------*        
                                                                                
           IF WRK-INP-ULTPAG-G2TM      EQUAL 'S'
               PERFORM 3800-DEVOLVER-TELA                                       
               MOVE WRK-MENSA07        TO  WRK-OUT-MENSA-G2TM
               GO TO 3700-99-FIM                                                
           END-IF.                                                              

           MOVE SPACES                 TO WRK-OUT-TAB-OCOR-G2TM.
           MOVE WRK-INP-PAGINA-G2TM-N  TO WRK-PAGINA.
           
           ADD 1                       TO WRK-PAGINA.
           
           PERFORM 3810-DEVOLVER-FIXOS.
           
           PERFORM 3530-MONTAR-TELA.
                                                                                
      *----------------------------------------------------------------*        
       3700-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       3800-DEVOLVER-TELA               SECTION.                                
      *----------------------------------------------------------------*        
                                                                                
           PERFORM 3810-DEVOLVER-FIXOS.
                                                                                
           PERFORM                     VARYING                                  
               IND-1 FROM 1 BY 1       UNTIL                                    
               IND-1                   GREATER 11 OR
               WRK-INP-ODATA-G2TM(IND-1) EQUAL SPACES OR LOW-VALUES
               MOVE WRK-INP-ODATA-G2TM(IND-1)
                                       TO WRK-OUT-ODATA-G2TM(IND-1)
               MOVE WRK-INP-OTRAN-G2TM(IND-1)
                                       TO WRK-OUT-OTRAN-G2TM(IND-1)
               MOVE WRK-INP-DESCR-G2TM(IND-1)
                                       TO WRK-OUT-DESCR-G2TM(IND-1)
               MOVE WRK-INP-QTDTRX-G2TM(IND-1)
                                       TO WRK-OUT-QTDTRX-G2TM(IND-1)
               MOVE WRK-INP-VTOTCH-G2TM(IND-1)
                                       TO WRK-OUT-VTOTCH-G2TM(IND-1)
               MOVE WRK-INP-VTOTDN-G2TM(IND-1)
                                       TO WRK-OUT-VTOTDN-G2TM(IND-1)
               MOVE WRK-INP-PAGREC-G2TM(IND-1)
                                       TO WRK-OUT-PAGREC-G2TM(IND-1)
           END-PERFORM.                                                         
                                                                                
      *----------------------------------------------------------------*        
       3800-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       3810-DEVOLVER-FIXOS             SECTION.
      *----------------------------------------------------------------*        
                                                                                
           MOVE WRK-INP-SENHA-G2TM     TO  WRK-OUT-SENHA-G2TM.
           MOVE WRK-INP-FASE-G2TM      TO  WRK-OUT-FASE-G2TM.
           MOVE WRK-INP-ULTPAG-G2TM    TO  WRK-OUT-ULTPAG-G2TM.
           MOVE WRK-INP-NATURE-G2TM    TO  WRK-OUT-NATURE-G2TM.
           MOVE WRK-INP-SOMPAG-G2TM    TO  WRK-OUT-SOMPAG-G2TM.
           MOVE WRK-INP-PAGINA-G2TM-N  TO  WRK-OUT-PAGINA-G2TM-N.
           MOVE WRK-INP-LIQEST-G2TM    TO  WRK-OUT-LIQEST-G2TM.
           MOVE WRK-INP-TPACES-G2TM    TO  WRK-OUT-TPACES-G2TM.
           MOVE WRK-INP-PAGIMP-G2TM    TO  WRK-OUT-PAGIMP-G2TM.
           MOVE WRK-INP-TOTIMP-G2TM    TO  WRK-OUT-TOTIMP-G2TM.
           MOVE WRK-INP-BCO-G2TM       TO  WRK-OUT-BCO-G2TM.
           MOVE WRK-INP-EMP-G2TM       TO  WRK-OUT-EMP-G2TM.
           MOVE WRK-INP-AGORI-G2TM     TO  WRK-OUT-AGORI-G2TM.
           MOVE WRK-INP-PACB-G2TM      TO  WRK-OUT-PACB-G2TM.
           MOVE WRK-INP-DEDIA-G2TM     TO  WRK-OUT-DEDIA-G2TM.
           MOVE WRK-INP-DEMES-G2TM     TO  WRK-OUT-DEMES-G2TM.
           MOVE WRK-INP-DEANO-G2TM     TO  WRK-OUT-DEANO-G2TM.
           MOVE WRK-INP-ATEDIA-G2TM    TO  WRK-OUT-ATEDIA-G2TM.
           MOVE WRK-INP-ATEMES-G2TM    TO  WRK-OUT-ATEMES-G2TM.
           MOVE WRK-INP-ATEANO-G2TM    TO  WRK-OUT-ATEANO-G2TM.
           MOVE WRK-INP-TRAN-G2TM      TO  WRK-OUT-TRAN-G2TM.
           MOVE WRK-INP-LITPAG-G2TM    TO  WRK-OUT-LITPAG-G2TM.
           MOVE WRK-INP-TOTPAG-G2TM    TO  WRK-OUT-TOTPAG-G2TM.
           MOVE WRK-INP-LIQPAG-G2TM    TO  WRK-OUT-LIQPAG-G2TM.
           MOVE WRK-INP-QTDPAG-G2TM    TO  WRK-OUT-QTDPAG-G2TM.
           MOVE WRK-INP-LITREC-G2TM    TO  WRK-OUT-LITREC-G2TM.
           MOVE WRK-INP-QTDREC-G2TM    TO  WRK-OUT-QTDREC-G2TM.
           MOVE WRK-INP-CHREST-G2TM    TO  WRK-OUT-CHREST-G2TM.
           MOVE WRK-INP-LIQREC-G2TM    TO  WRK-OUT-LIQREC-G2TM.
           MOVE WRK-INP-LITEST-G2TM    TO  WRK-OUT-LITEST-G2TM.
           MOVE WRK-INP-QTDEST-G2TM    TO  WRK-OUT-QTDEST-G2TM.
           MOVE WRK-INP-TOTREC-G2TM    TO  WRK-OUT-TOTREC-G2TM.
           MOVE WRK-INP-TOTEST-G2TM    TO  WRK-OUT-TOTEST-G2TM.
                                                                                
      *----------------------------------------------------------------*        
       3810-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       3900-TRATA-PF4                  SECTION.                                 
      *----------------------------------------------------------------*        
                                                                                
           MOVE SPACES                 TO WRK-IMPRESSORA.                       
                                                                                
           CALL 'BRAD1080'             USING LNK-IO-PCB
                                             WRK-INP-SENHA-G2TM
                                             WRK-IMPRESSORA.                    
                                                                                
           IF  RETURN-CODE             NOT EQUAL ZEROS                          
               MOVE WRK-MENSA26        TO WRK-OUT-MENSA-G2TM
               PERFORM 3800-DEVOLVER-TELA                                       
               GO                      TO 3900-99-FIM                           
           END-IF.                                                              
                                                                                
           IF  WRK-INP-PAGIMP-G2TM     GREATER ZEROS
               MOVE WRK-INP-PAGIMP-G2TM-N
                                       TO WRK-PAGINA-0031
           ELSE                                                                 
               MOVE ZEROS              TO WRK-PAGINA-0031
           END-IF.                                                              
                                                                                
           MOVE WRK-INP-BCO-G2TM-N     TO WRK-BANCO
           MOVE WRK-INP-EMP-G2TM-N     TO WRK-EMPRESA
           MOVE WRK-INP-AGORI-G2TM-N   TO WRK-AGENCIA
           MOVE WRK-INP-PACB-G2TM-N    TO WRK-PACB-ORI
                                                                                
           STRING  WRK-INP-DEANO-G2TM
                   WRK-INP-DEMES-G2TM
                   WRK-INP-DEDIA-G2TM
           DELIMITED BY SIZE INTO WRK-DATA-AAAAMMDD-A.                          
                                                                                
           MOVE WRK-DATA-AAAAMMDD      TO WRK-DTMOV-INI                         
                                                                                
           STRING  WRK-INP-ATEANO-G2TM
                   WRK-INP-ATEMES-G2TM
                   WRK-INP-ATEDIA-G2TM
           DELIMITED BY SIZE INTO WRK-DATA-AAAAMMDD-A.                          
                                                                                
           MOVE WRK-DATA-AAAAMMDD      TO WRK-DTMOV-FIM                         
           MOVE WRK-INP-TRAN-G2TM      TO WRK-TRANSAC
           MOVE WRK-IMPRESSORA         TO WRK-IMPR                              
           MOVE WRK-INP-TOT-PG-G2TM    TO WRK-TOT-PG
           MOVE WRK-INP-TOT-REC-G2TM   TO WRK-TOT-REC
           MOVE WRK-INP-TOT-EST-G2TM   TO WRK-TOT-EST
           MOVE WRK-INP-QTD-PG-G2TM    TO WRK-QTD-PG
           MOVE WRK-INP-QTD-REC-G2TM   TO WRK-QTD-REC
           MOVE WRK-INP-QTD-EST-G2TM   TO WRK-QTD-EST
                                                                                
           MOVE WRK-INP-REST-TRANS-G2TM
                                       TO WRK-REST-TRANS                        
           MOVE WRK-INP-REST-DTMOV-G2TM
                                       TO WRK-REST-DTMOV                        
           MOVE 'BIBC0031'             TO WRK-MODULO.                           
                                                                                
           CALL WRK-MODULO                 USING LNK-IO-PCB                     
                                                 LNK-ALT-PCB                    
                                                 WRK-AREA-BIBC0031              
                                                 ERRO-AREA                      
                                                 SQLCA.                         
                                                                                
           EVALUATE  WRK-STATUS-RET                                             
               WHEN  ZEROS                                                      
                     MOVE WRK-MENSA03     TO WRK-OUT-MENSA-G2TM
                     MOVE SPACES          TO WRK-INP-CHREST-G2TM
                                             WRK-INP-TOTIMP-G2TM
                                                                                
               WHEN  01                                                         
                     MOVE WRK-MENSA01     TO WRK-OUT-MENSA-G2TM
                     MOVE WRK-TOTAIS-IMPR TO WRK-INP-TOTIMP-G2TM
                     MOVE WRK-CH-RESTART  TO WRK-INP-CHREST-G2TM
                     MOVE WRK-PAGINA-0031 TO WRK-INP-PAGIMP-G2TM-N
                                                                                
               WHEN  02                                                         
                     MOVE WRK-MENSA26     TO WRK-OUT-MENSA-G2TM
                     MOVE WRK-TOTAIS-IMPR TO WRK-INP-TOTIMP-G2TM
                     MOVE WRK-CH-RESTART  TO WRK-INP-CHREST-G2TM
                                                                                
               WHEN  03                                                         
                     MOVE WRK-MENSA27     TO ERR-TEXTO                          
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO                         
                                                                                
                WHEN  OTHER                                                     
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO                         
           END-EVALUATE.                                                        
                                                                                
           PERFORM 3800-DEVOLVER-TELA.                                          
                                                                                
      *----------------------------------------------------------------*        
       3900-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       4000-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*        

           PERFORM 4100-VERIFICAR-FASE.
           
           MOVE LENGTH OF WRK-OUTPUT-G2TM
                                       TO WRK-OUT-LL-G2TM
                                          WRK-660-LL-MENSAGEM-G2TM.
           MOVE LENGTH OF WRK-660-BIBCG2TM
                                       TO WRK-660-LL-AREA-G2TM.
                                                                                
           CALL 'BRAD0660'             USING WRK-OUTPUT-G2TM
                                             WRK-660-BIBCG2TM.
                                                                                
           IF  RETURN-CODE             NOT EQUAL ZEROS                          
               MOVE 'APL'              TO ERR-TIPO-ACESSO                       
               MOVE RETURN-CODE        TO WRK-RC                                
               MOVE 50                 TO WRK-LOCAL-ERRO                        
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                               
           END-IF.                                                              
                                                                                
           MOVE WRK-ISRT               TO WRK-FUNCAO.                           
           MOVE WRK-OUTPUT-G2TM        TO WRK-MENSAGEM.
                                                                                
      *----------------------------------------------------------------*        
       4000-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        

      *----------------------------------------------------------------*
       4100-VERIFICAR-FASE             SECTION.
      *----------------------------------------------------------------*

           IF  WRK-OUT-FASE-G2TM-N     NOT EQUAL 1
               MOVE WRK-161            TO  WRK-OUT-EMP-ATTR-G2TM
                                           WRK-OUT-AGORI-ATTR-G2TM
                                           WRK-OUT-PACB-ATTR-G2TM
                                           WRK-OUT-DEDIA-ATTR-G2TM
                                           WRK-OUT-DEMES-ATTR-G2TM
                                           WRK-OUT-DEANO-ATTR-G2TM
                                           WRK-OUT-ATEDIA-ATTR-G2TM
                                           WRK-OUT-ATEMES-ATTR-G2TM
                                           WRK-OUT-ATEANO-ATTR-G2TM
                                           WRK-OUT-TRAN-ATTR-G2TM
           ELSE
               IF  WRK-INCONSIS            EQUAL SPACES
                   MOVE WRK-49345          TO WRK-OUT-EMP-ATTR-G2TM
               ELSE
                   IF  WRK-INCON-EMP       EQUAL 'X'
                       MOVE WRK-49353      TO WRK-OUT-EMP-ATTR-G2TM
                   END-IF
                   IF  WRK-INCON-AGORI     EQUAL 'X'
                       MOVE WRK-49353      TO WRK-OUT-AGORI-ATTR-G2TM
                   END-IF
                   IF  WRK-INCON-PACB      EQUAL 'X'
                       MOVE WRK-49353      TO WRK-OUT-PACB-ATTR-G2TM
                   END-IF
                   IF  WRK-INCON-TRAN      EQUAL 'X'
                       MOVE WRK-49353      TO WRK-OUT-TRAN-ATTR-G2TM
                   END-IF
                   IF  WRK-INCON-DT-DE     EQUAL 'X'
                       MOVE WRK-49353      TO WRK-OUT-DEDIA-ATTR-G2TM
                                              WRK-OUT-DEMES-ATTR-G2TM
                                              WRK-OUT-DEANO-ATTR-G2TM
                   END-IF
                   IF  WRK-INCON-DT-ATE    EQUAL 'X'
                       MOVE WRK-49353      TO WRK-OUT-ATEDIA-ATTR-G2TM
                                              WRK-OUT-ATEMES-ATTR-G2TM
                                              WRK-OUT-ATEANO-ATTR-G2TM
                   END-IF
                   MOVE WRK-INCONSIS       TO WRK-OUT-INCONS-G2TM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
                                                                                
      *----------------------------------------------------------------*        
       5000-FINALIZAR                  SECTION.                                 
      *----------------------------------------------------------------*        
                                                                                
           GOBACK.                                                              
                                                                                
      *----------------------------------------------------------------*        
       5000-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        
                                                                                
      *----------------------------------------------------------------*        
       9999-PROCESSAR-ROTINA-ERRO      SECTION.                                 
      *----------------------------------------------------------------*        
                                                                                
           MOVE 'BIBC0202'             TO ERR-MODULO.
           MOVE 'SENHAS02'             TO ERR-PGM.                              
           MOVE WRK-COD-USER           TO ERR-COD-USER.                         
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO.                        
                                                                                
           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'                              
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA                          
                                             LNK-ALT-PCB                        
                                             SQLCA                              
           ELSE                                                                 
               MOVE 'BIBC0202'         TO ERR-PGM
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA                          
                                             LNK-ALT-PCB                        
           END-IF.                                                              
                                                                                
           PERFORM 5000-FINALIZAR.                                              
                                                                                
      *----------------------------------------------------------------*        
       9999-99-FIM.                    EXIT.                                    
      *----------------------------------------------------------------*        
