      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
                                                                        
       PROGRAM-ID. CADU1201.                                            
       AUTHOR.     FLAVIO AUGUSTO MARIA.                                
      *================================================================*
      *                    P R I M E - I N F O R M A T I C A           *
      *================================================================*
      *    PROGRAMA....:  CADU1201                                     *
      *    PROGRAMADOR.:  FLAVIO AUGUSTO MARIA     - PRIME             *
      *    ANALISTA....:  NAJARA SANTOS            - PRIME             *
      *    DATA........:  09/06/2010                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  MENU PARAMETROS DE SERVICOS.                 *
      *----------------------------------------------------------------*
      *    TELA........:                                               *
      *    CADUTBTM   -   TELA DO MENU PARAMETROS DE SERVICOS.         *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#BRAD7C   -   AREA DE TRATAMENTO DE ERROS.                 *
      *    I#CADUAA   -   BOOK PARA O CADU8000.                        *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660   -   COMPACTAR MENSAGENS DE NAVEGACAO.            *
      *    BRAD7100   -   TRATAMENTO DE ERROS.                         *
      *    POOL5000   -   CONEXAO IMS.                                 *
      *    CADU8000   -   OBTEM MENSAGENS DO SISTEMA.                  *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *            CHAMADO POR:                 CHAMA:                 *
      *              CADU1925                  CADU1202                *
      *                                        CADU1203                *
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
       77  FILLER                      PIC  X(050)         VALUE        
           '*** INICIO DA WORKING STORAGE SECTION ***'.                 
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.                    
      *----------------------------------------------------------------*
                                                                        
       77  WRK-OPCAO-CONSIS            PIC  X(001)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA ATRIBUTOS ***'.                               
      *----------------------------------------------------------------*
                                                                        
      *--- POS. CURSOR, DESPROTEGE, ALFA, NORMAL, DISP ----------------*
       77  WRK-49345                   PIC S9(008) COMP    VALUE +49345.
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE        
           '*** AREAS AUXILIARES PARA POOL5000 ***'.                    
      *----------------------------------------------------------------*
                                                                        
       77  WRK-GU                      PIC  X(004)         VALUE 'GU'.  
       77  WRK-ISRT                    PIC  X(004)         VALUE 'ISRT'.
       77  WRK-CHNG                    PIC  X(004)         VALUE 'CHNG'.
       77  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.
       77  WRK-TELA                    PIC  X(008)         VALUE SPACES.
       77  WRK-VERSAO                  PIC  X(006)         VALUE        
           'VRS002'.                                                    

       01  WRK-COD-USER                PIC  X(007)         VALUE SPACES.
                                                                        
       01  WRK-COD-DEPTO.                                               
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-COD-DEPTO-N         PIC  9(004)         VALUE ZEROS. 
                                                                        
       01  WRK-MENSAGEM.                                                
           05  FILLER                  PIC  X(004)         VALUE SPACES.
           05  WRK-TRANSACAO           PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(1988)        VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DE ERRO BRAD0660 ***'.                             
      *----------------------------------------------------------------*
                                                                        
       01  WRK-ERRO-BRAD0660.                                           
           05  FILLER                  PIC  X(045)         VALUE        
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.          
           05  FILLER                  PIC  X(012)         VALUE        
               '- RET.COD ='.                                           
           05  WRK-RETURN-CODE         PIC  9(002)         VALUE ZEROS. 
           05  FILLER                  PIC  X(011)         VALUE        
               ' - LOCAL ='.                                            
           05  WRK-LOCAL-ERRO          PIC  9(003)         VALUE ZEROS. 
           05  FILLER                  PIC  X(002)         VALUE ' *'.  
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DE COMUNICACAO ***'.                               
      *----------------------------------------------------------------*
                                                                        
       01  WRK-COMU-AREA.                                               
           05 WRK-COMU-LL              PIC S9(004)  COMP   VALUE ZEROS. 
           05 WRK-COMU-ZZ              PIC  9(004)  COMP   VALUE ZEROS. 
           05 WRK-COMU-TRANCODE        PIC  X(015)         VALUE SPACES.
           05 WRK-COMU-SENHAS          PIC  X(037)         VALUE SPACES.
           05 WRK-COMU-COMANDO         PIC  X(068)         VALUE SPACES.
           05 WRK-COMU-OPCAO           PIC  X(001)         VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA INPUT - CADUTBTM ***'.                        
      *----------------------------------------------------------------*
                                                                        
       01  WRK-INPUT-TBTM.                                              
           05 WRK-FILLER                  PIC  X(018)      VALUE SPACES.
           05 WRK-INP-DADOS-TBTM.                                       
             10 WRK-INP-PFK-TBTM          PIC  X(001)      VALUE SPACES.
             10 WRK-INP-SENHAS-TBTM       PIC  X(037)      VALUE SPACES.
             10 WRK-INP-COMANDO-TBTM      PIC  X(068)      VALUE SPACES.
             10 WRK-INP-POSI-TBTM.                                      
               15 WRK-INP-LINHA-TBTM      PIC  9(004) COMP VALUE ZEROS. 
               15 WRK-INP-COLUNA-TBTM     PIC  9(004) COMP VALUE ZEROS. 
             10 WRK-INP-SEL1-TBTM         PIC  X(001)      VALUE SPACES.
             10 WRK-INP-SEL2-TBTM         PIC  X(001)      VALUE SPACES.
             10 WRK-INP-SEL3-TBTM         PIC  X(001)      VALUE SPACES.
             10 WRK-INP-SEL4-TBTM         PIC  X(001)      VALUE SPACES.
             10 WRK-INP-MENSA-TBTM        PIC  X(079)      VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA OUTPUT - CADUTBTM ***'.                       
      *----------------------------------------------------------------*
                                                                        
       01  WRK-OUTPUT-TBTM.                                             
           05 WRK-OUT-LL-TBTM             PIC S9(004) COMP VALUE +0194. 
           05 WRK-OUT-ZZ-TBTM             PIC  9(004) COMP VALUE ZEROS. 
           05 WRK-OUT-DADOS-TBTM.                                       
             10 WRK-OUT-SENHAS-TBTM       PIC  X(037)      VALUE SPACES.
             10 WRK-OUT-COMANDO-ATTR-TBTM PIC  9(004) COMP VALUE ZEROS. 
             10 WRK-OUT-COMANDO-TBTM      PIC  X(068)      VALUE SPACES.
             10 WRK-OUT-SEL1-TBTM         PIC  X(001)      VALUE SPACES.
             10 WRK-OUT-SEL2-TBTM         PIC  X(001)      VALUE SPACES.
             10 WRK-OUT-SEL3-TBTM         PIC  X(001)      VALUE SPACES.
             10 WRK-OUT-SEL4-TBTM         PIC  X(001)      VALUE SPACES.
             10 WRK-OUT-MENSA-TBTM        PIC  X(079)      VALUE SPACES.
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA MASCARA - CADUTBTM ***'.                      
      *----------------------------------------------------------------*
                                                                        
       01  WRK-660-CADUTBTM.                                            
           05 WRK-660-LL-AREA-TBTM     PIC  9(004) COMP    VALUE  0018. 
           05 WRK-660-LL-MENSAGEM-TBTM PIC  9(004) COMP    VALUE  0194. 
           05 WRK-660-SENHAS-TBTM      PIC  9(004) COMP    VALUE  0037. 
           05 WRK-660-COMANDO-TBTM     PIC  9(004) COMP    VALUE  2070. 
           05 WRK-660-SEL1-TBTM        PIC  9(004) COMP    VALUE  1001. 
           05 WRK-660-SEL2-TBTM        PIC  9(004) COMP    VALUE  1001.
           05 WRK-660-SEL3-TBTM        PIC  9(004) COMP    VALUE  1001.
           05 WRK-660-SEL4-TBTM        PIC  9(004) COMP    VALUE  1001.
           05 WRK-660-MENSA-TBTM       PIC  9(004) COMP    VALUE  0079. 
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA DE COMUNICACAO COM CADU8000 ***'.                  
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#CADUAA'.                                                   
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** AREA PARA TRATAMENTO DE ERRO BRAD7100 ***'.             
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#BRAD7C'.                                                   
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '*** FIM DA WORKING STORAGE SECTION ***'.                    
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       01  LNK-IO-PCB.                                                  
           05 LNK-IO-LTERM             PIC  X(008).                     
           05 FILLER                   PIC  X(002).                     
           05 LNK-IO-STATUS            PIC  X(002).                     
           05 FILLER                   PIC  X(012).                     
           05 LNK-IO-MODNAME           PIC  X(008).                     
                                                                        
       01  LNK-ALT-PCB.                                                 
           05 LNK-ALT-LTERM            PIC  X(008).                     
           05 FILLER                   PIC  X(002).                     
           05 LNK-ALT-STATUS           PIC  X(002).                     
           05 FILLER                   PIC  X(012).                     
           05 LNK-ALT-MODNAME          PIC  X(008).                     
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
                                                                        
           ENTRY 'DLITCBL'             USING LNK-IO-PCB                 
                                             LNK-ALT-PCB.               
                                                                        
           MOVE WRK-GU                 TO  WRK-FUNCAO.                  
                                                                        
      ******************************************************************
      * CHAMADA DA POOL5000                                            *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-PROCESSAR-TRANSACAO        SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CALL 'POOL5000'             USING WRK-VERSAO                 
                                             WRK-FUNCAO                 
                                             LNK-IO-PCB                 
                                             LNK-ALT-PCB                
                                             WRK-MENSAGEM               
                                             WRK-TELA                   
                                             WRK-COD-USER             
                                             WRK-COD-DEPTO.             
                                                                        
           IF  RETURN-CODE             EQUAL 04  OR 12                  
               PERFORM 2000-FINALIZAR                                   
           END-IF.                                                      
                                                                        
           MOVE SPACES                 TO  WRK-OUT-DADOS-TBTM.          
                                                                        
           PERFORM 1000-ROTINA-PRINCIPAL.                               
                                                                        
           GO TO 0000-PROCESSAR-TRANSACAO.                              
                                                                        
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
      * ROTINA PRINCIPAL                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-ROTINA-PRINCIPAL           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE 'CADUTBTM'             TO  WRK-TELA.                    
                                                                        
           IF  WRK-TRANSACAO           NOT EQUAL 'CADU1201'             
               PERFORM 1100-RECEBER-CONTROLE                            
           ELSE                                                         
               PERFORM 1200-PROCESSAR-CADU1201                          
           END-IF.                                                      
                                                                        
           IF  WRK-TELA                EQUAL 'CADUTBTM'                 
               PERFORM 1300-PROCESSAR-BRAD0660                          
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
      * RECEBE CONTROLE DO PROGRAMA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE WRK-AREA-CADU8000.                                
                                                                        
           MOVE WRK-MENSAGEM(1:LENGTH  OF WRK-COMU-AREA)           
                                       TO WRK-COMU-AREA.                
           MOVE 'A854'                 TO WRK-COD-MENSAGEM-8000.        
                                                                        
           PERFORM 1110-ACESSAR-CADU8000.                               
                                                                        
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
      * ACESSO AO MODULO CADU8000 PARA OBTER MENSAGENS                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-ACESSAR-CADU8000           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE 'CADU8000'                 TO WRK-MODULO.               
                                                                        
           CALL WRK-MODULO                 USING WRK-AREA-CADU8000.     
                                                                        
           IF  WRK-RETURN-CODE-8000        EQUAL 03                     
               MOVE WRK-ERRO-AREA-8000     TO ERRO-AREA                 
               MOVE WRK-SQLCA-8000         TO WRK-SQLCA                 
               MOVE 'CADU8000'             TO ERR-MODULO                
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
           MOVE WRK-MENSAGEM-SAIDA-8000    TO WRK-OUT-MENSA-TBTM.       
                                                                        
      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
      * TRATAMENTO DE PFS                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-PROCESSAR-CADU1201         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-MENSAGEM(1:LENGTH      OF WRK-INPUT-TBTM)               
                                           TO WRK-INPUT-TBTM.           
                                                                        
           EVALUATE TRUE                                                
                                                                        
               WHEN ((WRK-INP-PFK-TBTM     EQUAL 'H')     OR            
                     (WRK-INP-PFK-TBTM     EQUAL '.'      AND           
                      WRK-INP-COMANDO-TBTM EQUAL 'PFK01'))              
                      MOVE WRK-INP-MENSA-TBTM                           
                                           TO WRK-OUT-MENSA-TBTM        
                                                                        
               WHEN (WRK-INP-PFK-TBTM      EQUAL '.' AND                
                     WRK-INP-COMANDO-TBTM  EQUAL 'PFK02')               
                     PERFORM 1210-TRATAR-DESCONEXAO                     
                                                                        
               WHEN  WRK-INP-PFK-TBTM      EQUAL '3' OR 
                    (WRK-INP-PFK-TBTM      EQUAL '.' AND
                     WRK-INP-COMANDO-TBTM  EQUAL 'PFK03')               
                     PERFORM 1230-RETORNAR-CHAMADOR                     
                                                                        
               WHEN  WRK-INP-PFK-TBTM      EQUAL '.' 
                     PERFORM 1220-TRATAR-ENTER                          
                                                                        
               WHEN OTHER                                               
                     INITIALIZE WRK-AREA-CADU8000                       
                     MOVE 'E717'           TO WRK-COD-MENSAGEM-8000     
                     PERFORM 1110-ACESSAR-CADU8000                      
                     MOVE WRK-49345        TO WRK-OUT-COMANDO-ATTR-TBTM 
                                                                        
           END-EVALUATE.                                                
                                                                        
      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
      * DESCONEXAO                                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-TRATAR-DESCONEXAO          SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE WRK-COMU-AREA.                                    
                                                                        
           MOVE WRK-CHNG                   TO WRK-FUNCAO.               
           MOVE LENGTH OF WRK-COMU-AREA    TO WRK-COMU-LL.              
           MOVE 'CADU1201'                 TO WRK-TELA                  
                                              WRK-COMU-TRANCODE.        
           MOVE 'DISC'                     TO WRK-COMU-COMANDO.         
           MOVE WRK-COMU-AREA              TO WRK-MENSAGEM.             
                                                                        
      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
      * TRATAMENTO DE <ENTER>                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1220-TRATAR-ENTER               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           INITIALIZE WRK-COMU-AREA.                                    
                                                                        
           MOVE 'N'                    TO WRK-OPCAO-CONSIS.                 
                                                                        
           IF (WRK-INP-LINHA-TBTM      EQUAL 08  AND                    
               WRK-INP-COLUNA-TBTM     EQUAL 29) OR                     
               WRK-INP-COMANDO-TBTM    EQUAL '1' OR '01'
               MOVE 'CADU1202'         TO WRK-TELA
               MOVE 'S'                TO WRK-OPCAO-CONSIS
           END-IF.               
                      
           IF ((WRK-INP-LINHA-TBTM     EQUAL 10  AND                    
               WRK-INP-COLUNA-TBTM     EQUAL 29) OR                     
               WRK-INP-COMANDO-TBTM    EQUAL '2' OR '02') 
               MOVE 'CADU1203'         TO WRK-TELA          
               MOVE 'S'                TO WRK-OPCAO-CONSIS
               MOVE '2'                TO WRK-COMU-OPCAO
           END-IF.                              

           IF ((WRK-INP-LINHA-TBTM     EQUAL 12  AND                    
                WRK-INP-COLUNA-TBTM    EQUAL 29) OR                     
                WRK-INP-COMANDO-TBTM   EQUAL '3' OR '03')             
               MOVE 'CADU1203'         TO WRK-TELA          
               MOVE 'S'                TO WRK-OPCAO-CONSIS
               MOVE '3'                TO WRK-COMU-OPCAO
           END-IF.                              

           IF ((WRK-INP-LINHA-TBTM     EQUAL 14  AND                    
                WRK-INP-COLUNA-TBTM    EQUAL 29) OR                     
                WRK-INP-COMANDO-TBTM   EQUAL '4' OR '04')               
               MOVE 'CADU1203'         TO WRK-TELA          
               MOVE 'S'                TO WRK-OPCAO-CONSIS
               MOVE '4'                TO WRK-COMU-OPCAO
           END-IF.                              
           
           IF  WRK-OPCAO-CONSIS        EQUAL 'S'
               PERFORM 1221-PASSAR-CONTROLE                                              
           ELSE
               MOVE 'A854'             TO WRK-COD-MENSAGEM-8000         
               PERFORM 1110-ACESSAR-CADU8000 
           END-IF.
                                                                        
      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
      * PASSAR CONTROLE CONFORME OPCAO SELECIONADA                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1221-PASSAR-CONTROLE            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE WRK-CHNG                   TO WRK-FUNCAO.               
           MOVE LENGTH OF WRK-COMU-AREA    TO WRK-COMU-LL.              
           MOVE 'CADU1201'                 TO WRK-COMU-TRANCODE.         
           MOVE SPACES                     TO WRK-COMU-COMANDO.         
           MOVE WRK-COMU-AREA              TO WRK-MENSAGEM.             
                                                                        
      *----------------------------------------------------------------*
       1221-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
      * RETORNAR AO PROGRAMA CHAMADOR (CADU1925)                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1230-RETORNAR-CHAMADOR          SECTION.                         
      *----------------------------------------------------------------*
                         
           INITIALIZE WRK-COMU-AREA.                                    
                                                                        
           MOVE WRK-CHNG                   TO WRK-FUNCAO.               
           MOVE LENGTH OF WRK-COMU-AREA    TO WRK-COMU-LL.              
           MOVE 'CADU1201'                 TO WRK-COMU-TRANCODE.
           MOVE 'CADU1925'                 TO WRK-TELA.                   
           MOVE WRK-COMU-AREA              TO WRK-MENSAGEM.             
                                                                        
      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
      * ENVIAR TELA                                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           CALL 'BRAD0660'             USING WRK-OUTPUT-TBTM            
                                             WRK-660-CADUTBTM.          
                                                                        
           IF  RETURN-CODE             NOT EQUAL ZEROS                  
               MOVE 'APL'              TO ERR-TIPO-ACESSO               
               MOVE RETURN-CODE        TO WRK-RETURN-CODE               
               MOVE 10                 TO WRK-LOCAL-ERRO                
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO                     
               PERFORM 9999-PROCESSAR-ROTINA-ERRO                       
           END-IF.                                                      
                                                                        
           MOVE WRK-ISRT               TO WRK-FUNCAO.                   
           MOVE WRK-OUTPUT-TBTM        TO WRK-MENSAGEM.                 
                                                                        
      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
      * SAIR DO PROGRAMA                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-FINALIZAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           GOBACK.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
      * ROTINA DE ERRO                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  ERR-MODULO              EQUAL SPACES                     
               MOVE 'SENHAS02'         TO ERR-PGM                       
               MOVE 'CADU1201'         TO ERR-MODULO                    
           ELSE                                                         
               MOVE 'CADU1201'         TO ERR-PGM                       
           END-IF.                                                      
                                                                        
           MOVE WRK-COD-USER           TO ERR-COD-USER.                 
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO.                
                            
           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'                      
               CALL 'BRAD7100'         USING LNK-IO-PCB                 
                                             ERRO-AREA                  
                                             LNK-ALT-PCB                
                                             WRK-SQLCA                  
           ELSE                                                         
               CALL 'BRAD7100'         USING LNK-IO-PCB                 
                                             ERRO-AREA                  
                                             LNK-ALT-PCB                
           END-IF.                                                      
                                                                        
           PERFORM 2000-FINALIZAR.                                      
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
