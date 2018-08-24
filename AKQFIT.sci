mode(-1);       // suprime a impressão dos comandos na tela
clear();        // apaga todas as variáveis definidas

// Descrição dos cálculos no documento k0eAlfaV1.pdf

// Leitura do arquivo de dados de entrada
arquivoDeEntrada=input("Nome do arquivo de dados: ","s");
entrada=mopen(arquivoDeEntrada,"r");
entradaAu=mopen("au.dat");


// Lê as duas primeiras linhas
comentario=mgetl(entrada,1);
cabecalho=mgetl(entrada,1);
mgetl(entradaAu,1);
mgetl(entradaAu,1);

// O arquivo de entrada deve ter a seguinte estrutura de colunas (24 colunas)
// "Isotopo Egama Eficiencia sigma Eres sig% Asp_Cd sigma Asp sigma k0 sig% Fcd sigma Gepi sigma Gth sigma I0 sig% Sigma0 sig% Q0 sigma"
// Leitura dos dados
[n,isotopoSuperior,N,sN,NCd,sNCd,fz,sfz,fzCd,sfzCd,fa,sfa,faCd,sfaCd,D,sD,DCd,sDCd,C,sC,CCd,sCCd,S,sS,SCd,sSCd,w,sw,wCd,swCd,EgamaSuperior,IN,epsilon,sigmaEps,Er,sigErPorcento,k0,sigk0Porcento,Fcd,sigmaFcd,Gepi,sigmaGepi,Gth,sigmaGth,I0,sigI0Porcento,S0,sigS0Porcento,Q0,sigmaQ0,idouro] = mfscanf(-1,entrada,"%s %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %i %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %i");

[nAu,isotopoSuperiorAu,NAu,sNAu,NCdAu,sNCdAu,fzAu,sfzAu,fzCdAu,sfzCdAu,faAu,sfaAu,faCdAu,sfaCdAu,DAu,sDAu,DCdAu,sDCdAu,CAu,sCAu,CCdAu,sCCdAu,SAu,sSAu,SCdAu,sSCdAu,wAu,swAu,wCdAu,swCdAu,EgamaSuperiorAu,INAu,epsilonAu,sigmaEpsAu,ErAu,sigErPorcentoAu,k0Au,sigk0PorcentoAu,FcdAu,sigmaFcdAu,GepiAu,sigmaGepiAu,GthAu,sigmaGthAu,I0Au,sigI0PorcentoAu,S0Au,sigS0PorcentoAu,Q0Au,sigmaQ0Au] = mfscanf(-1,entrada,"%s %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %i %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %i");

mclose(entrada);
mclose(entradaAu);

dim = size(EgamaSuperior);

NGamas = dim(1,1)

// ---------- Impressão dos dados de entrada ----------
// ---- Cabeçalho da tabela 
//cabecalho="Isotopo N, Fz Fa D C S w Egama IN   Efic   sigma     Eres     sig%   k0   sig%   Fcd     sig     Gepi     sigma     Gth    sigma      I0    sig%   sigma0 sig%     Q0       sigma      idouro";
// Tabela de entrada
//Tab = [N sN NCd sNCd fz sfz fzCd sfzCd fa sfa faCd sfaCd D sD DCd sDCd C sC CCd sCCd S sS SCd sSCd w sw wCd swCd EgamaSuperior IN epsilon sigmaEps Er sigErPorcento k0 sigk0Porcento Fcd sigmaFcd Gepi sigmaGepi Gth sigmaGth I0 sigI0Porcento S0 sigS0Porcento Q0 sigmaQ0 idouro];

// Impressão
//cabecalho=sprintf("Isotopo  Egama  Efic sigma  Eres sig%  Asp_Cd sigma  Asp sigma  Rcd sigma  k0 sig%  Fcd  Gepi sigma  Gth sigma  I0 sig%  sigma0 sig%  Q0 sigma");
//formato="%5s %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %2i %8.2e %7.2e  %7.2f %5.1f  %10.4e %7.2e  %10.4e %7.2e %6.2f %4.2f %7.2e %4.2f  %6.4f %6.4f %9.6f %8.5f %8.5f %8.5f %8.3f %4.1f %8.3f %4.1f %10.6f %10.6f %2i"; 

//for i=1:NGamas
 //StrDados(i) =sprintf(formato,isotopoSuperior(i),Tab(i,1),Tab(i,2),Tab(i,3),Tab(i,4),Tab(i,5),Tab(i,6),Tab(i,7),Tab(i,8),Tab(i,9),Tab(i,10),Tab(i,11),Tab(i,12),Tab(i,13),Tab(i,14),Tab(i,15),Tab(i,16),Tab(i,17),Tab(i,18),Tab(i,19),Tab(i,20),Tab(i,21),Tab(i,22),Tab(i,23),Tab(i,24),Tab(i,25),Tab(i,26),Tab(i,27),Tab(i,28),Tab(i,29),Tab(i,30),Tab(i,31),Tab(i,32),Tab(i,33),Tab(i,34),Tab(i,35),Tab(i,36),Tab(i,37),Tab(i,38),Tab(i,39),Tab(i,40),Tab(i,41),Tab(i,42),Tab(i,43),Tab(i,44),Tab(i,45),Tab(i,46),Tab(i,47),Tab(i,48),Tab(i,49));
//xend;

//write(%io(2),"");
//write(%io(2),"Dados de entrada:");
//write(%io(2),cabecalho);
//for i=1:NGamas
//  write(%io(2),StrDados(i));
//end;

// Calcula os erros absolutos
sigmaEr = sigErPorcento.*Er/100;
sigmak0 = sigk0Porcento.*k0/100;

// Identifica o indice dos dados do Au198
//iAu=1;
//for i=1:NGamas
//  if(isotopoSuperior(i)=="Au198")
//    iAu = i;
//  end;
//end;


//iAu2=1;
//for i=1:NGamas
//  if(isotopoSuperior(i)=="Au198")
//    iAu2 = i;
//  end;
//end;


//  ----------------------------------------------------------------------------------------------------
// ----- Cálculo de alfa e dos k0 simultaneamente
// Usando a aproximação linear de Q0

for i=1:NGamas
    isotopoInferior(i) = isotopoSuperior(i);
    EgamaInferior(i) = EgamaSuperior(i);
end;
for i=1:NGamas
    ApSCd(i)=(N(i)*fa(i)*fz(i))/(D(i)*C(i)*S(i)*w(i))
    ApCd(i)=(NCd(i)*faCd(i)*fzCd(i))/(DCd(i)*CCd(i)*SCd(i)*wCd(i))
    ApSCdAu(i)=(NAu(i)*faAu(i)*fzAu(i))/(DAu(i)*CAu(i)*SAu(i)*wAu(i))
    ApCdAu(i)=(NCdAu(i)*faCdAu(i)*fzCdAu(i))/(DCdAu(i)*CCdAu(i)*SCdAu(i)*wCdAu(i))
end;

for i=1:NGamas
    Ysuperior(i)=log(ApCd(i)./Q0(i)./epsilon(i)./Fcd(i)./Gepi(i));
end;

//for i=1:NGamas
//    function Ysuperior2=F(ApCd,Q0,epsilon,Fcd,Gepi);
//    Ysuperior2=log(ApCd(i)./Q0(i)./epsilon(i)./Fcd(i)./Gepi(i));
//endfunction
//end;

//for i=1:NGamas
//dYsdApCd(i)=numderivative (F,ApCd(i));
//dYsdQ0(i)=numderivative (F,Q0(i));
//dYsdepsilon(i)=numderivative (F,epsilon(i));
//dYsdFcd(i)=numderivative (F,Fcd(i));
//dYsdGepi(i)=numderivative (F,Gepi(i));
//end;

for i=1:NGamas
    Yinferior(i)=log((ApSCd(i)-ApCd(i)./Fcd(i))./(ApSCdAu(idouro(i))-ApCdAu(idouro(i))./FcdAu(idouro(i))).*GthAu(idouro(i))./Gth(i).*epsilonAu(idouro(i))./epsilon(i));
end;
pause
//for i=1:NGamas
//    function Yinferio=G(ApSCd,ApCd,Fcd,ApSCdAu,ApCdAu,FcdAu,GthAu,Gth,epsilonAu,epsilon);
//    Yinferio=log(((ApSCd(i)-(ApCd(i)./Fcd(i)))./(ApSCdAu(idouro(i))-((ApCdAu(idouro(i))./FcdAu(idouro(i)))))).*(((GthAu(idouro(i))./Gth(i)).*(epsilonAu(idouro(i))))./epsilon(i)));
//endfunction
//end;

//for i=1:NGamas
//dYidApSCd(i)=numderivative (G,ApSCd(i));
//dYidApCd(i)=numderivative (G,ApCd(i));
//dYidFcd(i)=numderivative (G,Fcd(i));
//dYidGth(i)=numderivative (G,Gth(i));
//dYidepsilon(i)=numderivative (G,epsilon(i));
//dYidApSCdAu(i)=numderivative (G,ApSCdAu(idouro(i)));
//dYidApCdAu(i)=numderivative (G,ApCdAu(idouro(i)));
//dYidFcdAu(i)=numderivative (G,FcdAu(idouro(i)));
//dYidGthAu(i)=numderivative (G,GthAu(idouro(i)));
//dYidepsilonAu(i)=numderivative (G,epsilonAu(idouro(i)));
//end;

// vetor dos Y
// Primeira parte (SUPERIOR): Expressão 1 (Fcd será retirado, todos são 1 e sem erro)


//NInferior = indiceInferior;
isotopo=[isotopoSuperior;isotopoSuperior];
Egama=[EgamaSuperior;EgamaSuperior];
//
//((ApSCd-ApCd./Fcd)./(ApSCd(iAu)-ApCd(iAu)./Fcd(iAu)).*Gth(iAu)./Gth.*epsilon(iAu)./epsilon);
//Yinferior=log
//Yinferior=log((ApSCd-ApCd./Fcd)./(ApSCd(idouro)-ApCd(idouro)./Fcd(idouro)).*Gth(idouro)./Gth.*epsilon(idouro)./epsilon);

// O vetor Y total é composto da parte superior e da inferior
Y=[Ysuperior;Yinferior];

// Cálculos das derivadas parciais
// Ysuperior
dYsdApCd      = ApCd.^(-1);
dYsdQ0        = -Q0.^(-1);
dYsdepsilon   = -epsilon.^(-1);
dYsdFcd       = -Fcd.^(-1);
dYsdGepi      = -Gepi.^(-1);
// Yinferior
dYidApSCd     =  (ApSCd-ApCd./Fcd).^(-1);
dYidApCd      = -(ApSCd.*Fcd-ApCd).^(-1);
dYidFcd       =  ApCd.*(((ApSCd.*Fcd-ApCd).*Fcd).^(-1));
dYidGth       = -Gth.^(-1);
dYidepsilon   = -epsilon.^(-1);

//dYidApSCdAu   = -(ApSCd(iAu)-ApCd(iAu)/Fcd(iAu))^(-1);
//dYidApCdAu    =  (ApSCd(iAu)*Fcd(iAu)-ApCd(iAu))^(-1);
//dYidFcdAu     = -ApCd(iAu)*(((ApSCd(iAu)*Fcd(iAu)-ApCd(iAu))*Fcd(iAu))^(-1));
//dYidGthAu     =  Gth(iAu)^(-1);
//dYidepsilonAu =  epsilon(iAu)^(-1);

dYidApSCdAu   = -(ApSCdAu(idouro)-ApCdAu(idouro)./FcdAu(idouro)).^(-1);
dYidApCdAu    =  (ApSCdAu(idouro).*FcdAu(idouro)-ApCdAu(idouro)).^(-1);
dYidFcdAu     = -ApCdAu(idouro).*(((ApSCdAu(idouro).*FcdAu(idouro)-ApCdAu(idouro)).*FcdAu(idouro)).^(-1));
dYidGthAu     =  GthAu(idouro).^(-1);
dYidepsilonAu =  epsilonAu(idouro).^(-1);

// ----- Variância de Y (varY): propagação do erro de cada grandeza que compõe Y
// Parte superior  (Equações 17 e 18)
varYsupTodos = (dYsdApCd.*sigmaApCd).^2 + (dYsdQ0.*sigmaQ0).^2 + (dYsdepsilon.*sigmaEps).^2 + (dYsdFcd.*sigmaFcd).^2 + (dYsdGepi.*sigmaGepi).^2;

for i=1:NGamas
    varYsuperior(i)=varYsupTodos(i);
end;

varYinfTodos = (dYidApSCd.*sigmaApSCd).^2 + (dYidApCd.*sigmaApCd).^2 + (dYidApSCdAu.*sigmaApSCdAu(idouro)).^2 + (dYidApCdAu.*sigmaApCdAu(idouro)).^2 + (dYidFcd.*sigmaFcd).^2 + (dYidFcdAu.*sigmaFcdAu(idouro)).^2 + (dYidGth.*sigmaGth).^2 + (dYidepsilon.*sigmaEps).^2 + (dYidGthAu.*sigmaGthAu(idouro)).^2 + (dYidepsilonAu.*sigmaEpsAu(idouro)).^2;

for i=1:NGamas
    varYinferior(i)=varYinfTodos(i);
end;

// A variância 
varY=[varYsuperior;varYinferior];

// Diagonal do Vy
Vy=diag(varY);
// Covariâncias
//// Equação (20)
//for i=1:NGamas
//    if(isotopo(i)~="Au198")
//    Vy(idouro(i),i) = dYsdApCd(idouro(i)).*dYidApCdAu2(idouro(i)).*sigmaApCd(idouro(i)).^2 + dYsdepsilon(idouro(i)).*dYidepsilonAu2(idouro(i)).*sigmaEps(idouro(i)).^2 + dYsdFcd(idouro(i)).*dYidFcdAu2(idouro(i)).*sigmaFcd(idouro(i)).^2; 
//    // + falta cov. dos epsilons
//    Vy(i,idouro(i)) = Vy(idouro(i),i);
//  end; // if
//end; // for i

// Equação 21
for i=1:NGamas
  for j=i+1:NGamas
    if(isotopo(i)==isotopo(j)) 
        if(idouro(i)==idouro(j))
            Vy(i,j) = dYsdQ0(i).*dYsdQ0(j).*sigmaQ0(i).^2 + dYsdFcd(i).*dYsdFcd(j).*sigmaFcd(i).^2 + dYsdGepi(i).*dYsdGepi(j).*sigmaGepi(i).^2;
            // + falta cov. dos epsilons
            Vy(j,i) = Vy(i,j);
        end
    end
  end; // for j
end; // for i

// Equação (22)
for i=1:NGamas
  for j=NGamas+1:NGamas*2
    if(Egama(i)==Egama(j))
        if(idouro(i)==idouro(j-NGamas))
        k = j-NGamas;  // j está entre NGamas+1 e NGamas+NInferior, k é um índice entre 1 e NGamas
        Vy(i,j) = dYsdApCd(i)*dYidApCd(k)*sigmaApCd(i)^2 + dYsdFcd(i)*dYidFcd(k)*sigmaFcd(i)^2 + dYsdepsilon(i)*dYidepsilon(k)*sigmaEps(i)^2;
        if (idouro(i)==idouro(k)) then
            Vy(j,i)=0
            Vy(j,i) = Vy(i,j);
           end
        end
    end
  end; // for j
end; // for i

// Equação (23)
for i=1:NGamas
  for j=NGamas+1:NGamas*2
    if(isotopo(i)==isotopo(j) & Egama(i)~=Egama(j))
      if(idouro(i)==idouro(j-NGamas))
      k = j-NGamas;  // j está entre NGamas+1 e NGamas+NInferior, k é um índice entre 1 e NGamas
      Vy(i,j) = dYsdFcd(i)*dYidFcd(k)*sigmaFcd(i)^2; // + falta cov. dos epsilons
      if (idouro(i)==idouro(k)) then
        Vy(j,i)=0
        Vy(j,i) = Vy(i,j);
        end
      end
    end; // if
  end; // for j
end; // for i

// Equação (24)
for i=NGamas+1:NGamas*2
  for j=i+1:NGamas*2
    if(isotopo(i)==isotopo(j))
        k = i-NGamas; l = j-NGamas;;  // j está entre NGamas+1 e NGamas+NInferior, k é um índice entre 1 e NGamas
        if (idouro(k)==idouro(l))
            Vy(i,j) = dYidApCdAu(idouro(l)).*dYidApCdAu(idouro(l)).*sigmaApCdAu(idouro(l)).^2 + dYidApSCdAu(idouro(l)).*dYidApSCdAu(idouro(l)).*sigmaApCdAu(idouro(l)).^2 + dYidFcdAu(idouro(l)).*dYidFcdAu(idouro(l)).*sigmaFcdAu(idouro(l)).^2 + dYidGthAu(idouro(l)).*dYidGthAu(idouro(l)).*sigmaGthAu(idouro(l)).^2 + dYidepsilonAu(idouro(l)).*dYidepsilonAu(idouro(l)).*sigmaEpsAu(idouro(l)).^2 + dYidGth(k).*dYidGth(l).*sigmaGth(k).^2 + dYidFcd(k).*dYidFcd(l).*sigmaFcd(k).^2; 
          // + falta cov. dos epsilons
            Vy(j,i) = Vy(i,j);
        end
    end; // if
  end; // for j
end; // for i

// Equação (25)
for i=NGamas+1:NGamas*2
  for j=i+1:NGamas*2
    if(isotopo(i)~=isotopo(j))
        k = i-NGamas; l = j-NGamas;
        if (idouro(k)==idouro(l))  // j está entre NGamas+1 e NGamas+NInferior, k é um índice entre 1 e NGamas
            Vy(i,j) = dYidApCdAu(idouro(l)).*dYidApCdAu(idouro(l)).*sigmaApCdAu(idouro(l)).^2 + dYidApSCdAu(idouro(l)).*dYidApSCdAu(idouro(l)).*sigmaApCdAu(idouro(l)).^2 + dYidFcdAu(idouro(l)).*dYidFcdAu(idouro(l)).*sigmaFcdAu(idouro(l)).^2 + dYidGthAu(idouro(l)).*dYidGthAu(idouro(l)).*sigmaGthAu(idouro(l)).^2 + dYidepsilonAu(idouro(l)).*dYidepsilonAu(idouro(l)).*sigmaEpsAu(idouro(l)).^2; 
            Vy(j,i) = Vy(i,j);
        end
    end; // if
  end; // for j
end; // for i

// A matriz de planejamento
// Terá 1 coluna para o a, 1 para o alfa e (NGamas-1) colunas para os ln(k0)
col1Superior=ones(NGamas,1);
col1Inferior=zeros(NGamas,1);
col1 = [col1Superior;col1Inferior];

for i=1:NGamas
    col2Superior(i)=((Q0(i)+0.429).*log(Er(i))-0.602)./Q0(i);  // aproximação linear do ln[Q0(alfa)]
end;

col2Inferior=zeros(NGamas,1);
col2 = [col2Superior;col2Inferior];

NTotal=NGamas*2;
X = zeros(NTotal,NGamas);

// Parte superior
for i=1:NGamas
    X(i,i) = 1;
    X(NGamas + i,i) = 1;
end; // for i

// Matriz de Planejamento
X = [col1 col2 X];

// -----------------------  Ajuste pelos mínimos quadrados
dimX=size(X);
NumeroDeParametros = dimX(1,2);
VyInv = inv(Vy);
Vpar = inv(X'*VyInv*X);
Par = Vpar*X'*VyInv*Y;
// --- Calcula as diferenças
Yaju=X*Par;
D=(Y-Yaju);

// Calcula o qui-quadrado
Qui2=D'*VyInv*D;

// Calcula o qui-quadrado reduzido
GrausDeLiberdade=NTotal-NumeroDeParametros;
Qui2Red=Qui2/GrausDeLiberdade;
//--------------------------------------------------------------
// --- Resultados do ajuste
a = Par(1,1); Sigmaa=sqrt(Vpar(1,1));
alfa = Par(2,1); Sigmaalfa=sqrt(Vpar(2,2));
correlacaoa_alfa = Vpar(1,2)/(Sigmaa*Sigmaalfa);

format("v",10);
write(%io(2),"a = "+string(a)+" +- "+string(Sigmaa));
write(%io(2),"alfa = "+string(alfa)+" +- "+string(Sigmaalfa));
write(%io(2),"correlacao(a,alfa) = "+string(correlacaoa_alfa));

write(%io(2),"");
format("v",10);

// --- Imprime o qui-quadrado e o qui-quadrado reduzido
write(%io(2),"Qui-quadrado: "+string(Qui2));
write(%io(2),"com "+string(GrausDeLiberdade)+" graus de liberdade.");
write(%io(2),"Qui-quadrado reduzido: "+string(Qui2Red));
write(%io(2),"Probabilidade do qui-quadrado exceder "+string(Qui2)+" :"+string((1-cdfchi("PQ",Qui2,GrausDeLiberdade))*100)+"%");
write(%io(2),"");

// Valores dos k0:
write(%io(2),"");
write(%io(2)," Isotopo   Gama           k0       sigma(k0)     tabela: k0 +- sigma(k0)");
format("v",10);

for i=1:NGamas
    k=i+2
   write(%io(2),isotopo(i)+"  "+string(Egama(i))+"  "+string(exp(Par(k,1)))+ " +- "+string(exp(Par(k,1))*sqrt(Vpar(k,k)))+  "    "+string(k0(i))+ " +- "+string(sigmak0(i)));
end; // for i

//--------------------------------------------------
// Falta transferir o erro de Er e Q0 para Y
write(%io(2),"");
write(%io(2),"");
write(%io(2),"--- Transfere o erro de Er e Q0 da variável X para a variável Y");
//dYsdQ02=dYsdQ0
VyVelho = Vy;
// Transferência do erro de Er e Q0 para a variável Y
// derivadas parciais

for i=1:NGamas
    dYsdEr(i) = alfa*((Q0(i)+0.429).*((Er(i).*Q0(i)).^(-1)));
    dYsdQ0alfa(i)= alfa*(log(Er(i))./Q0(i))-(alfa*(log(Er(i)).*(Q0(i)+0.429)-0.602)./(Q0(i).^2));
    sigmaErb(i)=sigmaEr(i);
    sigmaQ0b(i)=sigmaQ0(i);
    dYsdQ0b(i)=dYsdQ0(i)
end

// Termos diagonais:
varYsupNovo = varYsuperior + (dYsdEr.*sigmaErb).^2 + (dYsdQ0b.*sigmaQ0b).^2;
sigmaYsupNovo=sqrt(varYsupNovo);

for i=1:NGamas
 Vy(i,i) = varYsupNovo(i,1);
end;

// Termos covariantes
for i=1:NGamas
  for j=i+1:NGamas
    if(isotopo(i)==isotopo(j))
        if (idouro(i)==idouro(j)) 
          Vy(i,j) = Vy(i,j) + dYsdEr(i)*dYsdEr(j)*sigmaErb(i)^2 + dYsdQ0alfa(i)*dYsdQ0alfa(j)*sigmaQ0b(i)^2;
          Vy(j,i) = Vy(i,j);
        end
    end; // if
  end; // for j
end; // for i

// Começando o levenberg

Aa=grand(1,1,'unf',2, 30)
Aalfa=grand(1,1,'unf',0.0001, 0.1)
A2=[Aa;Aalfa]

for i=1:NGamas
    A2(i+2) = log(i + grand(1,1,'unf',1.001, 5))
end

Yexp=Y

R=X'*VyInv*X

//valores de lambda e parâmetros para o loop na hora de fazer a análise
chidif = 1;
lambda=0.000000001;
X2=0;
chi2=-1;    
recalcularY=1;

while 1
    if recalcularY == 1 then
        a2=A2(1)
        alfa2=A2(2)

        for i=1:NGamas
            k0b=A2(i+2)
            YajuSup(i)=a2+((alfa2./Q0(i)).*((Q0(i)+0.429).*log(Er(i))-0.602))+log(exp(k0b))
            YajuInftodos(i)=log(exp(k0b))
        end

        Yaju2=[YajuSup;YajuInftodos]
        Datb=Yexp-Yaju2
    
        Yy=Yexp-Yaju2
        if chi2 == -1 then
            chi2=Datb'*inv(Vy)*Datb
        end
    end

    RLambda = zeros(NGamas+2,NGamas+2);
    for i=1:NGamas+2
        for j=1:NGamas+2
            if (i==j) //se são valores diagonais, adiciona-se lambda
                RLambda(i,j)=(1+lambda)*R(i,j);
            else //caso contrátio...
                RLambda(i,j)=R(i,j);
            end;
        end;
    end;

    DA=inv(RLambda)*X'*inv(Vy)*Yy;
    Ynovo=X*DA;
    D=Yy-Ynovo
    chi2novo=D'*inv(Vy)*D
//pause
    chidif = chi2novo - chi2;
        
    if chidif >= 0
        lambda = lambda * 10;
        recalcularY=0
    else
        lambda = lambda / 10;
        Anovo = A2+DA
//        for i=1:NGamas
//            Anovo(i+2) = exp(Anovo(i+2))
//        end
        A2 = Anovo
        chi2=chi2novo
        recalcularY = 1
    end

    if abs(chidif) < 0.001 then
        if abs(chidif) > 0.0001 then
            break;
        end
    end
//    pause;
end

expA=A2;
for i=1:NGamas
    expA(i+2)=exp(expA(i+2))
end


Qui2Rednovo=chi2novo/GrausDeLiberdade;

write(%io(2), "Saiiiiiiiiiiii");


// -----------------------  Ajuste pelos mínimos quadrados
VyInv = inv(Vy);
Vpar = inv(X'*VyInv*X);
Par = Vpar*X'*VyInv*Y;
q=inv(RLambda);

for i=1:NGamas+2
    for j=1:NGamas+2
        Sigmaanovo=sqrt(q(i,i));
        Sigmaalfanovo=sqrt(q(j,j));
        
        correlacaonovo(i,j)=(q(i,j)/(Sigmaanovo*Sigmaalfanovo))*1000.0;
    end
end

// --- Calcula as diferenças
Yaju=X*Par;
D2=(Y-Yaju);
Dq=Yy
// Calcula o qui-quadrado
Qui2=D2'*VyInv*D2;

// Calcula o qui-quadrado reduzido
   Qui2Red=Qui2/GrausDeLiberdade;
//--------------------------------------------------------------

// Covariâncias e correlações
parametros(1)="   a"; 
parametros(2)=" alfa";

  format("v",3);
for i=3:NumeroDeParametros
 parametros(i) = "k0_" + string(i-2);
end;
tabelaCor = "         ";
for i=1:6
 tabelaCor = tabelaCor + parametros(i)+"     ";
end;
for i=7:NumeroDeParametros
 tabelaCor = tabelaCor + parametros(i)+"     ";
end;


//CovCor=zeros(size(Vpar));
for i=1:NumeroDeParametros
 for j=i:NumeroDeParametros
  format("v",5);
  CovCor(j,i)=string((Vpar(j,i)/(sqrt(Vpar(j,j)*Vpar(i,i))))*1000);
  format("v",5);
  CovCor(i,j)=string((Vpar(j,i)/(sqrt(Vpar(j,j)*Vpar(i,i))))*1000);
 end;
end;

write(%io(2),"");
write(%io(2),tabelaCor);
mode(0);
[parametros CovCor]
mode(-1);
write(%io(2),tabelaCor);

write(%io(2),"");



// --- Resultados do ajuste
a = Par(1,1); Sigmaa=sqrt(Vpar(1,1));
alfa = Par(2,1); Sigmaalfa=sqrt(Vpar(2,2));
correlacaoa_alfa = Vpar(1,2)/(Sigmaa*Sigmaalfa);
format("v",10);
write(%io(2),"a = "+string(a)+" +- "+string(Sigmaa));
write(%io(2),"alfa = "+string(alfa)+" +- "+string(Sigmaalfa));
write(%io(2),"correlacao(a,alfa) = "+string(correlacaoa_alfa));

write(%io(2),"");
format("v",10);

// --- Imprime o qui-quadrado e o qui-quadrado reduzido
write(%io(2),"Qui-quadrado: "+string(chi2novo));
write(%io(2),"com "+string(GrausDeLiberdade)+" graus de liberdade.");
write(%io(2),"Qui-quadrado reduzido: "+string(Qui2Rednovo));
write(%io(2),"Probabilidade do qui-quadrado exceder "+string(chi2novo)+" :"+string((1-cdfchi("PQ",chi2novo,GrausDeLiberdade))*100)+"%");
write(%io(2),"");

// Valores dos k0:
write(%io(2),"");
write(%io(2),"Isotopo  Gama      k0     sigma(k0)   tabela: k0 +- sigma(k0)");
format("v",10);

for i=1:NGamas
   k=2+i;
   write(%io(2),isotopo(i)+"  "+string(Egama(i))+"  "+string(exp(Par(k,1)))+ " +- "+string(exp(Par(k,1))*sqrt(Vpar(k,k)))+  "    "+string(k0(i))+ " +- "+string(sigmak0(i)));
end; // for i

q=inv(RLambda)


//--- Impressão da tabela de resultados na tela
for i=1:NTotal
 sigmaY(i) = sqrt(Vy(i,i));
end;


MatP = [Egama Y Yaju D D./sigmaY]

write(%io(2),"");
write(%io(2),"  i isótopo Egama       Y      Yaju    diferença dif. ponder.");
for i=1:NTotal
  StrPrint(i)="";
  formato=" %2i  %s %7.2f  %8.4f  %8.4f  %8.4f  %8.4f";
  StrPrint(i) =sprintf(formato,i,isotopo(i),MatP(i,1),MatP(i,2),MatP(i,3),MatP(i,4),MatP(i,5));
  write(%io(2),StrPrint(i)); 
end;
write(%io(2),"");


// Gráficos
// Resíduos ponderados pelo desvio padrão
clf();
for i=1:NTotal vetorX(i)= 0.95+i*1.005; end;
plot2d(vetorX,D./sigmaY);
differ=D./sigmaY;

// Muda as configuracoes do grafico
// foram obtidas olhando a janela do Figure Properties
   a=get("current_axes");  // pega o grafico

   a.box          = "on";
   a.thickness    = 2;
   a.sub_tics     = [4 4];
   a.font_style   = 8;
   a.font_size    = 3;
   a.grid         = [-1 0];
   a.x_label.text = "índice";
   a.y_label.text = "(Y-Yajustado)/sigmaY";
   a.x_label.font_style = 8;
   a.y_label.font_style = 8;
   a.x_label.font_size =  4;
   a.y_label.font_size =  4;

   p1=a.children.children(1);  // pontos
   p1.line_mode       ="off";
   p1.mark_mode       = "on";
   p1.mark_style      = 0;
   p1.mark_foreground = 2; // cor azul
   p1.mark_background = 2; // cor azul
   p1.mark_size_unit  = "point"; // referencia do tamanho 
   p1.mark_size       = 6; // tamanho da marca

// Gráficos de comparação dos k0:
grafK0=input(" Quer gráfico do k0?","string");

if(grafK0=="S" | grafK0=="s" | grafK0=="sim")
 abscissas = 1:NGamas;
 //k0aju(1) = 1.0; sigmak0aju(1) = 0.0;  // k0 do Au
 //k0aju(2) = 1.0; sigmak0aju(1) = 0.0;  // k0 do Au
 k0aju(1:NGamas)=expA(3:NumeroDeParametros);
 for i=1:NGamas
   sigmak0aju(i) = expA(i+2)*sqrt(q(i+2,i+2));
 end; // for
 razaok0 = k0aju./k0;
 sigmaRazaok0 = sqrt((sigmak0aju./k0).^2+(sigmak0.*k0aju./k0.^2).^2);
 clf();
 plot2d(abscissas,razaok0);
 errbar(abscissas,razaok0,sigmaRazaok0,sigmaRazaok0);
 xsegs([0 26],[1 1]);
end; // if grafK0


    
