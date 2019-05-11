//----------------SOMATÓRIOS-----------------------------------------
function somatorioYX = SomatorioXY(n, X , Y )
    i=1;
    somaYX = 0;
    while(i<=n)
        somaYX = somaYX + (X(i)*Y(i));
        i = i+1;
        end
    somatorioYX  = somaYX
endfunction

function somatorioY = SomatorioY(n , Y)
    i=1;
    somaY = 0;
    while(i<=n)
        somaY = somaY + Y(i);
        i = i+1;
    end
    somatorioY = somaY;
endfunction

function somatorioX = SomatorioX(n , X )
    i=1;
    somaX = 0;
    while(i<=n)
        somaX = somaX + X(i);
        i = i+1;
    end
    somatorioX = somaX;
endfunction

//---------- SXX-------------------------------------------------------------( S Q T )
// VARIAÇÃO DE Y EM TORNO DA SUA MÉDIA
function somatorioXX = Sxx(n , X )
    i=1;
    somatoriox = SomatorioX(n,X);
    x1 = somatoriox/n;
    somaXX = 0;
    while(i<=n)
        somaXX = somaXX + (X(i)-x1)^2;
        i = i+1;
    end
    somaXX = somaXX/(n-1);
    somatorioXX = somaXX;
endfunction
//----------SXY--------------------------------------------------------------
function somaXY = Sxy(n,X,Y)
    
    somaXY =  ((SomatorioXY(n,X,Y) - ((1/n) * SomatorioY(n,Y) * SomatorioX(n,X)))/(n-1));
    
endfunction
//-------------------------------------------------------------------------( S Q E )
// VARIAÇÃO DE Y EM TORNO DA RETA
function q = sqe(n,X,Y)
    i = 1;
    soma = 0;
    b0 = B0(n,X,Y);
    b1 = B1(n,X,Y);
    while (i<=n)
        y1 = b0+(b1*X(i)); 
        soma = soma + ((Y(i)-y1)^2);
        i = i+1;
    end
    q = soma;
endfunction
//-------------------------------------------------------------------------( S Q R E G )
// VARIAÇÃO DA RETA EM TORNO DA MÉDIA DE Y
function reg = sqreg(n,X,Y)
    i = 1;
    soma = 0;
    b0 = B0(n,X,Y);
    b1 = B1(n,X,Y);
    y2 = (SomatorioY(n,Y))/n;
    while (i<=n)
        y1 = b0+(b1*X(i)); 
        soma = soma + ((y1 - y2)^2);
        i = i+1;
    end
    reg = soma;
    
endfunction

//-------------------------------------------------------------------------( S Q T )
// VARIAÇÃO DE Y EM TORNO DA SUA MÉDIA
function d = sqt(n,X,Y)
    
    SQE = sqe(n,X,Y);
    SQREG = sqreg(n,X,Y);
    d = SQE + SQREG ;
endfunction
// ----------------------------TESTE PARA A ESCOLHA DE MODELO SIMPLES E O MRLS -----------
function test(n,X,Y,F)
    t = (sqreg(n,X,Y))/(sqe(n,X,Y)/(n-2));
    if (t > F) then
        disp("B1 É DIFERENTE DE O ")
    end
    if (t < F) then
        disp("B1 PODE SER IGUAL A O ")
    end
endfunction

// ----------------------------TABELA ANOVA -----------
function ANOVA(n,X,Y)
    
    SQE = sqe(n,X,Y);
    SQREG = sqreg(n,X,Y);
    SQT = SQE + SQREG;
    th = (n-2);
    p = SQE/(n-2);
    h = 0;
    l = n-1;
    w = ((SQREG)/(SQE/(n-2)));
    // DADOS
       FV = ["REGRESSÃO","ERRO","TOTAL"];
       GL = [1,th,l];
       SQ = [SQREG,SQE,SQT];
       QM = [SQREG,p,h];
       Fo = [w,h,h];
       disp("--------------------------------------------------------");
       disp("Tabela ANOVA");
       disp("--------------------------------------------------------");
       disp("FV      GL      SQ      QM      FO");
       disp(string(FV(1))+"   "+string(GL(1))+"   "+string(SQ(1))+"   "+string(QM(1))+"   "+string(Fo(1)));
       disp(string(FV(2))+"   "+string(GL(2))+"   "+string(SQ(2))+"   "+string(QM(2))+"   "+string(Fo(2)));
       disp(string(FV(3))+"   "+string(GL(3))+"   "+string(SQ(3))+"   "+string(QM(3))+"   "+string(Fo(3)));
       disp("--------------------------------------------------------");
endfunction

// função B0 E B1
//-------------------------------------------------------------------------
function b1 = B1(n,X,Y)
    
    b1 =  Sxy(n,X,Y) / Sxx(n,X);
    
endfunction

function b0 = B0(n,X,Y)
    b0 = 1/n*((SomatorioY(n,Y)) - (B1(n,X,Y)* SomatorioX(n,X)));
endfunction
//------------------- MELHOR RETA-----------------------------------------------

function p = func( n , X , Y)
    b0 = B0(n,X,Y);
    b1 = B1(n,X,Y);
    //disp("Y = "+string(b1)+ "x" +" + "+ string(b0) );
    p = "Y = "+string(b1)+ "x" +" + "+ string(b0);
endfunction

//-------------------FUNÇÃO DE CORRELAÇÃO-----------------------------------------------
function t= Rxy(n,X,Y)
    
    t = Sxy(n,X,Y)/(sqrt(Sxx(n,X))*sqrt(Sxx(n,Y)));
    //disp("Correlação:  "+string(r));
    
endfunction
//-------------------------RESÍDUOS---------------------------------------------------------+++++++++++++++++ RETORNA UM  ARRAY DE RESIDUOS
function res = e(n,X,Y)
    i=1;
    b0 = B0(n,X,Y);
    b1 = B1(n,X,Y);
    while (i <= n)
        y1 = (b1*X(i)) + b0;
        E(i) = Y(i) - y1;
        i = i+1;
    end
    res = E;
endfunction
//------------------------------RESÍDUOS MINIMO MEDIO MAX---------------------------------------------
function resi(n,X,Y)
    E = e(n,X,Y);
    i = 1;
    minimo = 0;
    medio = 0;
    maximo = 0;
    while (i<=n)
        if(minimo>X(i))
            minimo = X(i);
        end
        if (maximo<X(i))
            maximo = X(i);
        end
        medio = medio + X(i);
        i = i+1;
    end
    medio = medio/n;
    disp("residuos");
    disp("-----------------------------");
    disp("MINIMO:"+string(minimo));
    disp("MEDIO:"+string(medio));
    disp("MAXIMO:"+string(maximo));
    disp("-----------------------------");
endfunction
//------------------ESPERANÇA ---------------------------------------------------------------
function espe = esp(n,X)
    i = 1;
    soma = 0;
    while (i<=n)
        soma =  soma + X(i);
        i = i+1;
    end
    espe = soma/n;
endfunction
//------------------VARIÂNCIA-----------------------------------------------------------------
function vari = var(n,X,Y)
    i = 1;
    soma = 0;
    b0 = B0(n,X,Y);
    b1 = B1(n,X,Y);
    
    med = esp(n,X);
    while (i<=n)
        y1 = b0+b1*X(i);
        soma = soma + (((Y(i)- y1)^2)/(n-2)); 
        i = i+1;
    end
    vari = soma;
endfunction
//------------------------FUNÇÃO PRINCIPAL---------------------------------------------------
function f(n,X,Y)
    
    bo = B0(n,X,Y);
    b1 = B1(n,X,Y);
    correlacao = Rxy(n,X,Y);
    RESIDUOS = e(n,X,Y);
    esperancaX = esp(n,X);
    esperancaY = esp(n,Y);
    variancaX = var(n,X);
    variancaY = var(n,Y);
    melhorReta = func(n,X,Y);
     
     disp("       RESPOSTA:  ");
     disp("       B0 :  "+string(bo));
     disp("       B1 :  "+string(b1));
     disp("       CORRELAÇÃO :  "+string(correlacao));
     disp("       ESPERANÇA X :  "+string(esperancaX));
     disp("       ESPERANÇA Y :  "+string(esperancaY));
     disp("       VARIÂNCIA  :  "+string(variancaX));
     disp("       MELHOR RETA :  "+string(melhorReta));
     ANOVA(n,X,Y);
     resi(n,X,Y);
endfunction
//================   FAZENDO ============================================================================
//------------------------retirar os pontos influentes E criar um array de pontos---------------------------------------------------
function m = ret(n,X,Y,m1,m2)
    i = 1;
    j = 1;
    while (i<=n)
        if(Y(i)< m1 || Y(i)> m2)
            Vx(j) = X(i);
            Vy(j) = Y(i);
            j = j+1;
        end
        i = i+1;
    end
    //f(j,Vx,Vy);
    m = Vy;
    
endfunction
