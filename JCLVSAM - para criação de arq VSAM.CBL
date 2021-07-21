//==>> JCL PARA CRIACAO DE ARQUIVO VSAM
//==>> INFORMAR EM 'RECODRSIZE' O TAMANHO TOTAL DO ARQUIVO.
//==>> INFORMAR EM 'KEYS' O TAMANHO TOTAL DA CHAVE DO ARQUIVO.
//==>> OS DEMAIS DADOS PERMANECEM INALTERADOS.


//ENTRAR NO DIRETORIO: PR.GERAL.JOBLIB(JOB200V)
//COPIAR O JOB ABAIXO E SUBMETER


//PR17VSAM JOB ,'PRIME31',CLASS=C,MSGCLASS=X,NOTIFY=PRIME31,            
//         RESTART=STEP001                                              
//*--------------------------------------------------------------------*
//*              JOB DE CRIACAO DE ARQUIVO VSAM                        *
//*--------------------------------------------------------------------*
//STEP001  EXEC PGM=IDCAMS,COND=(0,NE)                                  
//SYSPRINT DD   SYSOUT=*                                                
//SYSIN    DD  *                                                        
   DELETE PR.GERAL.TEMP.ISPPARMX PURGE CLUSTER                          
        IF MAXCC NE 0 THEN SET MAXCC = 0                                
        DEFINE CLUSTER (NAME(PR.GERAL.TEMP.ISPPARMX)    -               
                 CYLINDERS            (3,2)             -               
                 VOL (PR39T7)                           -               
                 CONTROLINTERVALSIZE  (8192)            -               
                 INDEXED                                -               
                 KEYS                 (02 0)            -               
                 RECORDSIZE           (256 256)         -               
                 REUSE                                  -               
                 LOG(NONE)                              -               
                 BWO(NO)                                -               
                 SHAREOPTIONS         (2 3))            -               
          DATA  (NAME(PR.GERAL.TEMP.ISPPARMX.DATA)      -               
                 CONTROLINTERVALSIZE(8192))             -               
          INDEX (NAME(PR.GERAL.TEMP.ISPPARMX.INDEX)     -               
                  CONTROLINTERVALSIZE(1024))                            
