XWBBRK ;ISC-SF/EG - DHCP BROKER PROTOYPE ;07/08/2004  11:08
 ;;1.1;RPC BROKER;**2,4,10,16,26,35**;Mar 28, 1997
PRSP(P) ;Parse Protocol
 ;M Extrinsic Function
 ;
 ;Inputs
 ;P        Protocol string with the form
 ;         Protocol := Protocol Header^Message where
 ;         Protocol Header := LLLWKID;WINH;PRCH;WISH;MESG
 ;           LLL  := length of protocol header (3 numeric)
 ;           WKID := Workstation ID (ALPHA)
 ;           WINH := Window handle (ALPHA)
 ;           PRCH := Process handle (ALPHA)
 ;           WISH := Window server handle (ALPHA)
 ;           MESG := Unparsed message
 ;Outputs
 ;ERR      0 for success, "-1^Text" if error
 ;
 N ERR,C,M,R,X,U
 S U="U",R=0,C=";",ERR=0,M=512 ;Maximum buffer input
 IF $E(P,1,5)="{XWB}" S P=$E(P,6,$L(P)) ;drop out prefix
 IF '+$G(P) S ERR="-1^Required input reference is NULL"
 IF +ERR=0 D
 . S XWB(R,"LENG")=+$E(P,1,3)
 . S X=$E(P,4,XWB(R,"LENG")+3)
 . S XWB(R,"MESG")=$E(P,XWB(R,"LENG")+4,M)
 . S XWB(R,"WKID")=$P(X,C)
 . S XWB(R,"WINH")=$P(X,C,2)
 . S XWB(R,"PRCH")=$P(X,C,3)
 . S XWB(R,"WISH")=$P(X,C,4)
 Q ERR
 ;
PRSM(P) ;Parse message
 ;M Extrinsic Function
 ;
 ;Inputs
 ;P        Message string with the form
 ;         Message := Header^Content
 ;           Header  := LLL;FLAG
 ;             LLL     := length of entire message (3 numeric)
 ;             FLAG    := 1 indicates variables follow
 ;           Content := Contains API call information
 ;Outputs
 ;ERR      0 for success, "-1^Text" if error
 N C,ERR,M,R,X,U
 S U="^",R=1,C=";",ERR=0,M=512 ;Max buffer
 IF '+$G(P) S ERR="-1^Required input reference is NULL"
 IF +ERR=0 D
 . S XWB(R,"LENG")=+$E(P,1,5)
 . S XWB(R,"FLAG")=$E(P,6,6)
 . S XWB(R,"TEXT")=$E(P,7,M)
 Q ERR
 ;
PRSA(P) ;Parse API information, get calling info
 ;M Extrinsic Function
 ;Inputs
 ;P        Content := API Name^Param string
 ;           API     := .01 field of API file
 ;           Param   := Parameter information
 ;Outputs
 ;ERR      0 for success, "-1^Text" if error
 ;
 N C,DR,ERR,M,R,T,X,U
 S U="^",R=2,C=";",ERR=0,M=512 ;Max buffer
 IF '+$L(P) S ERR="-1^Required input reference is NULL"
 IF +ERR=0 D
 . S XWB(R,"CAPI")=$P(P,U)
 . S XWB(R,"PARM")=$E(P,$F(P,U),M)
 . S T=$O(^XWB(8994,"B",XWB(R,"CAPI"),0))
 . I '+T S ERR="-1^Remote Procedure '"_XWB(R,"CAPI")_"' doesn't exist on the server." Q  ;P10 - dpc
 . S T(0)=$G(^XWB(8994,T,0))
 . I $P(T(0),U,6)=1!($P(T(0),U,6)=2) S ERR="-1^Remote Procedure '"_XWB(R,"CAPI")_"' cannot be run at this time." Q  ;P10. Check INACTIVE field. - dpc.
 . S XWB(R,"NAME")=$P(T(0),"^")
 . S XWB(R,"RTAG")=$P(T(0),"^",2)
 . S XWB(R,"RNAM")=$P(T(0),"^",3)
 . S XWBPTYPE=$P(T(0),"^",4)
 . S XWBWRAP=+$P(T(0),"^",8)
 Q ERR
 ;
PRSB(P) ;Parse Parameter information
 ;M Extrinsic Function
 ;Inputs
 ;P        Param   := M parameter list
 ;           Param   := LLL,Name,Value
 ;             LLL     := length of variable name and value
 ;             Name    := name of M variable
 ;             Value   := a string
 ;Outputs
 ;ERR      0 for success, "-1^Text" if error
 ;
 N A,ERR,F,FL,I,K,L,M,P1,P2,P3,P4,P5,MAXP,R
 S R=3,MAXP=+$E(P,1,5)
 S P1=$E(P,6,MAXP+5) ;only param string
 S ERR=0,F=3,M=512
 IF '+$D(P) S ERR="-1^Required input reference is NULL"
 S FL=+$G(XWB(1,"FLAG"))
 S I=0
 IF '+ERR D
 . ;IF 'FL S P1=$E(P,F+1,MAXP) Q
 . IF 'FL,+MAXP=0 S P1="",ERR=1 Q
 . F  D  Q:P1=""
 . . Q:P1=""
 . . S L=+$E(P1,1,3)-1
 . . S P3=+$E(P1,4,4)
 . . S P1=$E(P1,5,MAXP)
 . . S XWB(R,"P",I)=$S(P3'=1:$E(P1,1,L),1:$$GETV($E(P1,1,L)))
 . . IF FL=1,P3=2 D  ;XWB*1.1*2
 . . . S A=$$OARY^XWBBRK2,XWBARY=A
 . . . S XWB(R,"P",I)=$$CREF^XWBBRK2(A,XWB(R,"P",I))
 . . S P1=$E(P1,L+1,MAXP)
 . . S K=I,I=I+1
 . IF 'FL Q
 . S P3=P
 . S L=+$E(P3,1,5)
 . S P1=$E(P3,F+3,L+F)
 . ;IF FL=1 S P1=$$CREF^XWBBRK2(A,P1) ;convert array ref to namespace ref
 . S P2=$E(P3,L+F+3,M)
 . ;instantiate array
 . ;S DM=0
 . F  D  Q:+L=0
 . . ;Sumtimes the array is null, so there isn't data for first read.
 . . S L=+$$BREAD^XWBRW(3,15,1) Q:L=0  S P3=$$BREAD^XWBRW(L)
 . . S L=+$$BREAD^XWBRW(3) IF L'=0 S P4=$$BREAD^XWBRW(L)
 . . IF L=0 Q
 . . IF P3=0,P4="" S L=0 Q  ; P4=0 changed to P4="" JLI 021114
 . . IF FL=1 D LINST(A,P3,P4)
 . . IF FL=2 D GINST
 IF ERR Q P1
 S P1=""
 D  Q P1
 . F I=0:1:K D
 . . IF FL,$E(XWB(R,"P",I),1,5)=".XWBS" D  Q  ;XWB*1.1*2
 . . . S P1=P1_"."_$E(XWB(R,"P",I),2,$L(XWB(R,"P",I)))
 . . . IF I'=K S P1=P1_","
 . . S P1=P1_"XWB("_R_",""P"","_I_")"
 . . IF I'=K S P1=P1_","
 IF '+ERR Q P1
 Q ERR
 ;
CALLP(XWBP,P,DEBUG) ;make API call using Protocol string
 ;ERR will be 0 or "-1^text"
 N ERR,S
 S ERR=0 K XWB("BADPARAM")
 IF '$D(DEBUG) S DEBUG=0
 ;IF 'DEBUG D:$D(XRTL) T0^%ZOSV ;start RTL
 S ERR=$$PRSP(P)
 IF '+ERR S ERR=$$PRSM(XWB(0,"MESG"))
 IF '+ERR S ERR=$$PRSA(XWB(1,"TEXT")) I $G(XWB(2,"CAPI"))="XUS SET SHARED" S XWBSHARE=1 Q
 I +ERR S XWBSEC=$P(ERR,U,2) ;P10 -- dpc
 ;Check the RPC is allowed to run before any parameter is parsed
 I '+ERR D CHKPRMIT^XWBSEC(XWB(2,"CAPI")) ;checks if RPC allowed to run
 S:$L($G(XWBSEC)) ERR="-1^"_XWBSEC
 IF '+ERR S S=$$PRSB(XWB(2,"PARM")) I $D(XWB("BADPARAM")) S XWBSEC="Invalid parameter",ERR="-1^"_XWBSEC
 ;IF 'DEBUG S:$D(XRT0) XRTN="RPC BROKER READ/PARSE" D:$D(XRT0) T1^%ZOSV ;stop RTL
 IF '+ERR,(+S=0)!(+S>0) D
 . ;Logging
 . I $G(XWBDEBUG)>1 D LOG^XWBDLOG("RPC: "_XWB(2,"CAPI"))
 . D CAPI^XWBBRK2(.XWBP,XWB(2,"RTAG"),XWB(2,"RNAM"),S)
 E  D CLRBUF ;p10
 IF 'DEBUG K XWB
 IF $D(XWBARY) K @XWBARY,XWBARY
 Q
 ;
LINST(A,X,XWBY) ;instantiate local array
 IF XWBY=$C(1) S XWBY=""
 I '$$LITS^XWBPRS(X) S XWB("BADPARAM")=1 Q
 S X=A_"("_X_")"
 S @X=XWBY
 Q
GINST ;global list parameters are not supported by this parser
 S XWB("BADPARAM")=1
 Q
 ;
GETV(V) ;get value of V - reference parameter
 Q $$GETV^XWBPRS(V)
CLRBUF ;p10  Empties Input buffer
 N %
 F  R %#1:XWBTIME(1) Q:%=""
 Q
