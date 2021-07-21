      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
                                                                        
       PROGRAM-ID. TEST0300.                                            
       AUTHOR.     FLAVIO AUGUSTO MARIA.                                
      *================================================================*
      *                   P R I M E  I N F O R M A T I C A             *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  TEST0300                                     *
      *    PROGRAMADOR.:  FLAVIO AUGUSTO MARIA              -  PRIME   *
      *    ANALISTA....:                                    -          *
      *    DATA........:  13/07/2010                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  SIMULADOR DA BRAD0300 PARA LEITURA DE ARQUIVO*
      *                   COMPRIMIDO.                                  *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO PELA BRAD7100       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - MODULO TRATAMENTO DE ERRO.                       *
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
       
           SELECT CADACLIE  ASSIGN     TO UT-S-CADACLIE                  
                      FILE STATUS      IS WRK-FS-CADACLIE.               
                                                                        
      *================================================================*
       DATA DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
                 
      *----------------------------------------------------------------*
      *    INPUT  :  ARQUIVO DE ENTRADA                                *
      *              ORG. SEQUENCIAL   -   LRECL = XXX                 *
      *----------------------------------------------------------------*
                                                                        
       FD  CADACLIE                                                      
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-CADACLIE                  PIC  X(180).                     
                                                          
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** INICIO DA WORKING TEST0300                 ***'.        
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** VARIAVEIS AUXILIARES                       ***'.        
      *----------------------------------------------------------------*

       77  WRK-ABRIR                   PIC  X(001)         VALUE 'S'.
       77  WRK-FECHAR                  PIC  X(001)         VALUE 'N'.
       77  WRK-PGM                     PIC  X(008)         VALUE        
           'TEST0300'.                                                  
       77  WRK-BATCH                   PIC  X(008)         VALUE        
           'BATCH'.                                                     
                   
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** MENSAGEM DE ERRO DE FILE-STATUS            ***'.        
      *----------------------------------------------------------------*
                                                                        
       01  WRK-FS-CADACLIE             PIC  X(002)         VALUE SPACES.
                                                                        
       01  WRK-ERRO-ARQUIVO.                                            
           05  FILLER                  PIC  X(005)         VALUE        
               'ERRO '.                                                 
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
               88  WRK-ABERTURA                            VALUE        
                   ' NA ABERTURA '.                                     
               88  WRK-LEITURA                             VALUE        
                   ' NA  LEITURA '.                                     
               88  WRK-GRAVACAO                            VALUE        
                   ' NA GRAVACAO '.                                     
               88  WRK-FECHAMENTO                          VALUE        
                   'NO FECHAMENTO'.                                     
           05  FILLER                  PIC  X(012)         VALUE        
               ' DO ARQUIVO '.                                          
           05  WRK-NOME-ARQUIVO        PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE        
               ' - FILE STATUS = '.                                     
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
                           
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DA BRAD7100                           ***'.        
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#BRAD7C'.                                                   
                                                                        
      *----------------------------------------------------------------*
       LINKAGE SECTION.                                                 
      *----------------------------------------------------------------*

       01  LNK-DADOS.
           05  LNK-NOME-ARQUIVO        PIC  X(008).
           05  REG-CADACLIE.                                                
             10  CAD-CHAVE.                                               
                 15  CAD-AGENCIA          PIC  9(05)     COMP-3.          
                 15  CAD-RAZAO            PIC  9(05)     COMP-3.          
                 15  CAD-CONTA            PIC  9(07)     COMP-3.          
             10  CAD-DIG-CTA              PIC  X(01).                     
             10  CAD-CGC-CPF.                                             
                 15  CAD-PRINCIPAL        PIC  9(09)     COMP-3.          
                 15  CAD-FILIAL           PIC  9(05)     COMP-3.          
                 15  CAD-CTLE             PIC  9(02).                     
             10  CAD-NOME-CLIE            PIC  X(40).                     
             10  CAD-ENDERECO             PIC  X(40).                     
             10  CAD-CEP.                                                 
                 15  CAD-NRO-CEP          PIC  9(05)     COMP-3.          
                 15  CAD-COMPL-CEP        PIC  9(03).                     
                 15  CAD-COMPL-CEP-NULL   REDEFINES                       
                     CAD-COMPL-CEP        PIC  X(03).                     
             10  CAD-TELEFONE.                                            
                 15  CAD-DDD-FONE         PIC  X(04).                     
                 15  CAD-NRO-FONE         PIC  9(09)     COMP-3.          
             10  CAD-FAX.                                                 
                 15  CAD-DDD-FAX          PIC  X(04).                     
                 15  CAD-NRO-FAX          PIC  9(09)     COMP-3.          
             10  CAD-POSTO-SERV           PIC  9(03)     COMP-3.          
             10  CAD-RAMO-ATIV            PIC  9(05)     COMP-3.          
             10  CAD-IDENTF-CTA           PIC  X(02).                     
             10  CAD-DT-ABERT             PIC  9(09)     COMP-3.          
             10  CAD-DT-ULTMV             PIC  9(09)     COMP-3.          
             10  CAD-DT-NASCI             PIC  9(09)     COMP-3.          
             10  CAD-SEXO                 PIC  X(01).                     
             10  CAD-TP-CHEQ              PIC  X(01).                     
             10  CAD-TP-EXTR              PIC  X(01).                     
             10  CAD-RESERVA              PIC  X(30).                     
                        
                        
                                                                                   
      *================================================================*
       PROCEDURE DIVISION              USING     LNK-DADOS.             
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1000-INICIALIZAR.
           
           PERFORM 2000-LER-ARQUIVO.
           
           IF  WRK-FECHAR              EQUAL 'S'
               PERFORM 3000-FECHAR-ARQUIVO
           END-IF.                 
           
           PERFORM 4000-FINALIZAR.                                      
                                                                        
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                    
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.                         
      *----------------------------------------------------------------*
                             
           INITIALIZE REG-CADACLIE.                                       

           IF  WRK-ABRIR               EQUAL 'S'
               OPEN INPUT CADACLIE
               SET WRK-ABERTURA        TO TRUE                         
               PERFORM 1100-TESTAR-FILE-STATUS
               MOVE 'N'                TO WRK-ABRIR               
           END-IF.                                                          

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF WRK-FS-CADACLIE          NOT EQUAL '00'                  
              MOVE 'CADACLIE'          TO WRK-NOME-ARQUIVO              
              MOVE 'APL'               TO ERR-TIPO-ACESSO               
              MOVE WRK-FS-CADACLIE     TO WRK-FILE-STATUS               
              MOVE WRK-ERRO-ARQUIVO    TO ERR-TEXTO                     
              PERFORM 9999-ROTINA-ERRO                                  
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-LER-ARQUIVO                SECTION.                         
      *----------------------------------------------------------------*
                             
           READ CADACLIE               INTO REG-CADACLIE.
           
           IF WRK-FS-CADACLIE          EQUAL '10'
              MOVE 'S'                 TO WRK-FECHAR                       
              GO TO 2000-99-FIM                                       
           END-IF.                                                       
           
           SET WRK-LEITURA                TO TRUE.                       
           
           PERFORM 1100-TESTAR-FILE-STATUS.                  
           
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       3000-FECHAR-ARQUIVO             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CLOSE CADACLIE.                                                          
               
           SET WRK-FECHAMENTO          TO TRUE.                         
                                                                        
           PERFORM 1100-TESTAR-FILE-STATUS.  
           
           MOVE 04                     TO RETURN-CODE.                
                                                                        
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-PGM                TO ERR-PGM.                      
                                                                        
           CALL 'BRAD7100'             USING WRK-BATCH                  
                                             ERRO-AREA.                 
                                                                        
           PERFORM 4000-FINALIZAR.                                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
