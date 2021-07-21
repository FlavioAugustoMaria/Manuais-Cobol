//DC61CEZA  JOB  'BRCO,5900,BN,CDBA6110','16-9999XXXX-2177714',         
// MSGCLASS=Z,CLASS=A,REGION=8192K                                      
//JOBLIB   DD   DSN=AV.BIBGERTT,DISP=SHR                                
//         DD   DSN=AV.BIBGERAL,DISP=SHR                                
//         DD   DSN=IMSVS.TST.TPGMLIB,DISP=SHR                          
//         DD   DSN=IMSVS.TST.PGMLIB.VLF,DISP=SHR                       
//         DD   DSN=DB2A2.R2.DSNLOAD,DISP=SHR                           
//         DD   DSN=SYS1.CEE.SCEERUN,DISP=SHR                           
//         DD   DSN=MQ2A2.MQ4.USERAUTH,DISP=SHR                         
//STEP1    EXEC PGM=SBAT0050                                            
//SYSUDUMP DD   SYSOUT=*                                                
//SYSOUT   DD   SYSOUT=*                                                
//RELATO   DD   DSN=AD.TEMP.SIMM.LISTAMQ,                               
//         DISP=(,CATLG,DELETE),                                        
//         SPACE=(TRK,(0030,0020),RLSE),                                
//         UNIT=(DISCO),                                                
//         DCB=(LRECL=133,BLKSIZE=1330,RECFM=FBA)                       
//*-------------------------------------------------------------------* 
//*             A R G U M E N T O S   D E   P E S Q U I S A           * 
//*                                                                   * 
//*   QUEUE_MANAGER    =                                              * 
//*   QUEUE            = MQ11.DEAD.QUEUE                              * 
//*   SALTA_MENSAGENS  = 0000000                                      * 
//*   LIMITE_MENSAGENS = 0001000                                      * 
//*   DATAHORA_INICIAL = 2002-01-02-00.00.00.00                       * 
//*   DATAHORA_FINAL   = 2002-01-02-99.99.99.99                       * 
//*-------------------------------------------------------------------* 
//ENTRADA  DD   *                                                       
QUEUE_MANAGER    =                                                      
QUEUE            = TPLR.QL.REQ.SIMM.TPLR.01                             
SALTA_MENSAGENS  = 0000000                                              
LIMITE_MENSAGENS = 0001000                                              
DATAHORA_INICIAL = 2012-01-01-00.00.00.00                               
DATAHORA_FINAL   = 2012-12-31-99.99.99.99                               
