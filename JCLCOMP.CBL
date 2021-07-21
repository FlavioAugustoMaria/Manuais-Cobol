//**UTILIZADO PARA COMPRIMIR ARQUIVO NORMAL
//**
//DC54RCMP JOB 'CBPP,4135,BN,ELOI-AMS',MSGCLASS=Z,CLASS=F,REGION=0M            
//*                                                                            
//JOBLIB   DD DSN=AV.BIBGERAL,DISP=SHR            * DESENVOLVIMENTO            
//         DD DSN=AV.BIBGERTT,DISP=SHR            * PRODUCAO                   
//         DD DSN=DB2A2.R2.DSNLOAD,DISP=SHR                                    
//*--------------------------------------------------------------------        
//*                                                                            
//STEP1    EXEC PGM=POOL0002                                                   
//ENTRA    DD DSN=AD.TEMP.GIPS2206.CADACLIE,                            0000013
//         DISP=SHR                                                     0000013
//SAI      DD DSN=AD.TEMP.GIPS2206.CLIECOMP,                            0000013
//         DISP=(,CATLG,DELETE),UNIT=DISCO,SPACE=(TRK,(10,10),RLSE),           
//         DCB=(LRECL=0180,RECFM=FB)                                           
//SYSOUT   DD SYSOUT=T                                                         
//SYSUDUMP DD SYSOUT=T                                                         
//*                                                                            
