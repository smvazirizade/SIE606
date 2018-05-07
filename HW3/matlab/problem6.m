clc;clear;close all
alpha=5/100
Table = readtable('Book3.xlsx','ReadVariableNames',true,'ReadRowNames',true)

lin_md1 = fitlm(Table, 'LiteracyRate ~ Newspapers + Radios + TV')
coefCI(lin_md1,alpha)
