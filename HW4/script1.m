clc;clear
Tableall = readtable('HW4_1Data.xls','ReadRowNames',true);
Number=1:200;
Y=table2array(Tableall(Number,:));

COVMatrix=cov(Y);
MeanMatrix=mean(Y);
Ystan=[Y-MeanMatrix]./repmat((diag(COVMatrix)').^.5,length(Number),1);
CorMatrix=corr(Ystan);
[EigVec,EigVal]=EigenCalc(CorMatrix);
X=(EigVec'*Ystan')';
RhoXY=EigVec.*[((diag(EigVal).^.5)*(diag(CorMatrix))'.^-.5)]';






[coeff1,score1,latent,tsquared,explained,mu1]  = pca(Ystan,'Centered',true);
% [coeff1,score1,latent,tsquared,explained,mu1] 
figure
plot(tsquared)


Number=201:300;
Z=table2array(Tableall(Number,:));
COVMatrix=cov(Y);
MeanMatrix=mean(Y);
Zstan=[Z-MeanMatrix]./repmat((diag(COVMatrix)').^.5,length(Number),1);
X2=(EigVec'*Zstan')';

TotalT2=X2.^2./diag(EigVal)';
plot([TotalT2(:,1)+TotalT2(:,2)]);hold on
plot([0 100],[1 1]*chi2inv(0.95,2),'r')
figure
plot([sum(TotalT2,2)]);hold on
plot([0 100],[1 1]*chi2inv(0.95,10),'r')

%(size(Ystan,2)-1)*size(Ystan,1)/(size(Ystan,2)-size(Ystan,2))* finv(0.95,,)


function [EiegenVector,EigenValue]=EigenCalc(Matrix)
    [EiegenVector EigenValue]=eig(Matrix); 
    [d,ind] = sort(diag(EigenValue),'descend');
EiegenVector = EiegenVector(:,ind);
EigenValue = diag(d);
if EiegenVector(1,1)<0
    EiegenVector=EiegenVector*-1;
end
end