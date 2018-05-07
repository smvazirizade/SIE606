clc;clear;close all
x(:,1)=[3 5 5 7 7 7 8 9 10 11]
x(:,2)=[2.3 1.9 1 0.7 0.3 1 1.05 0.45 0.7 0.3]
figure
scatter(x(:,1),x(:,2),'filled');
xlabel('age')
ylabel('price')
xlim([0 12])
fprintf('mean value of x1=%.02f \n',mean(x(:,1)))
fprintf('mean value of x2=%.02f \n',mean(x(:,2)))
fprintf('variance of x1=%.02f \n',var(x(:,1)))
fprintf('variance of x2=%.02f \n',var(x(:,2)))
fprintf(' Cov of x1 and x2= \n %.02f %.02f \n %.02f %.02f \n',cov(x))
fprintf('Corr of x1 and x2= \n %.02f %.02f \n %.02f %.02f \n',corrcoef(x))


Sum=0

for i=1:length(x(:,1))
   Sum=Sum+(x(i,1)-7.2).*(x(i,2)-0.97);
end
Sum/i
Sum/(i-1)