%% Load specific organoid 
load('zAllOrganoids.mat')
load('DTFT_X_shift_Organoids.mat')
load('DTFT_freq_Organoids.mat')


%% Extract Fourier analysis time series featurese


numMode90All = zeros(263,40);
ampMean90All = zeros(263,40);
ampStd90All = zeros(263,40);
for o = 1:40
    organoidNum = o; % i =1,...,40
    disp(['Organoid number: ' num2str(o) ]);
    organoidName = allOrganoids{organoidNum,1};
    imageIndices = find(contains(fileLog(:,1),organoidName)); % index of all organoid i's images
    numTimePoints = length(imageIndices);
    for im = 1:numTimePoints
        
        % Load (x,y) pairs as complex numbers: z = x + iy
        z = zAll{im,o}';
        Nsample  = length(z);
        Xshifted = XAll{im,o}';
%         N = length(z);
        
        
%         fs = 6; % samples per cycle (unit length along contour)
%         df = fs/N;
%         freq = (0:df:(fs-df)) - (fs-mod(N,2)*df)/2;
%         freq= freq';
        

        
        % # modes needed to get 90% of energy
        energyMax = 0.9;
        xOriginal = ifftshift(Xshifted);
        XmagOriginal = abs(xOriginal);
        XmagOriginalSorted = -1*sort(-XmagOriginal);
        
        totalEnergy = sum(XmagOriginalSorted(2:end));
        amplitudeCDF = zeros(length(XmagOriginalSorted)-1,1);
        for i = 1:length(amplitudeCDF)
            amplitudeCDF(i) = sum(XmagOriginalSorted(2:i+1));
        end
        [~, numMode90] = min( abs(amplitudeCDF-energyMax*totalEnergy));
        numMode90=numMode90+1;
        ampMean90 = mean(XmagOriginalSorted(2:numMode90));
        ampStd90 = std(XmagOriginalSorted(2:numMode90));
        


        numMode90All(im,o) = numMode90;
        ampMean90All(im,o) = ampMean90;
        ampStd90All(im,o) = ampStd90;
    end
end

%%
colFGF2 = [235,68,182]/255;
colBasal = [68,235,121]/255;

    
close all
figure()
set(gcf,'color','white','position',[10,10,1700,600])


% set(gcf,'color','white','position',[10,10,600,600])
subplot(1,3,1)
for i=1:40
    if i<= 10
        
        p = plot(1:261,numMode90All(1:261,i),'color',colBasal);
        p.Color(4) = 1;
    else
        p =  plot(1:261,numMode90All(1:261,i),'color',colFGF2);
        p.Color(4) = 0.4;
    end
    hold on
end
axis([0,262,0,150])
xlabel('Time','interpreter','latex')
str = {['Number of modes'],[ 'in 90\% energy signal']};
title(str,'interpreter','latex')
ylabel('\# modes','interpreter','latex')
set(gca,'fontsize',12)

lw = 1;
% figure()
% set(gcf,'color','white','position',[10,10,600,600])
subplot(1,3,2)
for i=1:40
    if i<= 10
        
        p = plot(1:261,ampMean90All(1:261,i),'color',colBasal,'linewidth',lw);
        p.Color(4) = 1;
    else
        p =  plot(1:261,ampMean90All(1:261,i),'color',colFGF2,'linewidth',lw);
        p.Color(4) = 0.35;
    end
    hold on
end
% axis([0,262,10^1,10^5])
axis([0,262,0,15000])
xlabel('Time','interpreter','latex')
str = {['Mean of mode amplitudes'],[ 'in 90\% energy signal']};
title(str,'interpreter','latex')
ylabel('$$ \mu_{X} $$','interpreter','latex')
set(gca,'fontsize',12)


lw = 1;
% figure()
% set(gcf,'color','white','position',[10,10,600,600])
subplot(1,3,3)
for i=1:40
    if i<= 10
        
        p = plot(1:261,ampStd90All(1:261,i),'color',colBasal,'linewidth',lw);
        p.Color(4) = 1;
    else
        p =  plot(1:261,ampStd90All(1:261,i),'color',colFGF2,'linewidth',lw);
        p.Color(4) = 0.35;
    end
    hold on
end
% axis([0,262,10^1,10^5])
axis([0,262,0,20001])
xlabel('Time','interpreter','latex')
str = {['Standard deviation of mode'],[ ' amplitudes in 90\% energy signal']};
title(str,'interpreter','latex')
ylabel('$$ \sigma_X$$','interpreter','latex')
set(gca,'fontsize',12)



%% Plot organoid frequency response 

close all
organoid = 27;
timePoint = 260;  % goes from 1:261/262/263 depending on organoid
name = allOrganoids{organoid,1};

Xshifted = XAll{timePoint,organoid}';
z = zAll{timePoint,organoid}';
freq = fAll{timePoint,organoid}';
N = length(z);
Xmag = abs(Xshifted);


fs = 6; % samples per cycle (unit length along contour)
df = fs/N;
freq = (0:df:(fs-df)) - (fs-mod(N,2)*df)/2;
freq= freq';




figure()
set(gcf,'color','white','position',[500,0,600,900])
subplot(2,1,2)
plot(freq, abs(Xshifted),'linewidth',2,'color','black')
title('Frequency-Amplitude spectrum','interpreter','latex')
xlabel('Frequency (length$$^{-1}$$)','interpreter','latex')
ylabel('Amplitude')
title(name)
set(gca,'fontsize',14)
axis([-0.5,0.5,0,2*10^4])

% figure()
% subplot(2,1,1)
% plot(freq,real(Xshifted))
% ylabel('$$Re[X[n]]$$','interpreter','latex')
% xlabel('Frequency (length$$^{-1}$$)','interpreter','latex')
% title('Real part of DTFT')
% set(gca,'fontsize',12)
% subplot(2,1,2)
% plot(freq,imag(Xshifted))
% ylabel('$$Im[X[n]]$$','interpreter','latex')
% xlabel('Frequency (length$$^{-1}$$)','interpreter','latex')
% title('Imaginary part of DTFT')
% set(gca,'fontsize',12)


% figure()
subplot(2,1,1)
plot(z,'linewidth',3)
% lim = 60;
% axis([-lim,lim,-lim,lim])
axis square
title(['$$t =' num2str(timePoint) ' $$'],'interpreter','latex','fontsize',20)


% freqAmplitude = [ freq, abs(Xshifted)  ];
% [freqAmplitude, index] = sortrows(freqAmplitude,2);
% freqAmplitude = flipud(freqAmplitude);
% index = flipud(index);

% if mod(N,2) == 0
%     zerothFreq = N/2 + 2;
% else
%     zerothFreq = ceil(N/2)+1;
% end




energyMax = 0.9;

xOriginal = ifftshift(Xshifted);
XmagOriginal = abs(xOriginal);

% if XmagOriginal(2)>XmagOriginal(1)
%     highest = XmagOriginal(2);
%     secondHighest = XmagOriginal(1);
%     XmagOriginal(1) = highest;
%     XmagOriginal(2) = secondHighest;
% end
[XmagOriginalSorted ,sortedIndices ] =  sort(-XmagOriginal);
XmagOriginalSorted = -1*XmagOriginalSorted;
totalEnergy = sum(XmagOriginalSorted(2:end));
amplitudeCDF = zeros(length(XmagOriginalSorted)-1,1);
for i = 1:length(amplitudeCDF)
    amplitudeCDF(i) = sum(XmagOriginalSorted(2:i+1));
end
figure()
plot(1:length(amplitudeCDF),amplitudeCDF)

% # modes needed to get 90% of energy
[val, numMode90] = min( abs(amplitudeCDF-energyMax*totalEnergy));
numMode90=numMode90+1

ampMean90 = mean(XmagOriginalSorted(2:numMode90));
ampStd90 = std(XmagOriginalSorted(2:numMode90));
XmagShifted = abs(Xshifted);



%% Reconstruct Approximate Signal - select # of +/- frequencies


close all
% Approach 1: Reconstruct using unsorted list of frequencies (first few
% frequencies)

% % for X
% maxFreq = N-4; %<--- change this to top 'maxFreq' frequencies instead of
% % freqKeepIndex = [1:maxFreq, (L-maxFreq+2):L];
% freqKeepIndex = [1:maxFreq];

% for Xshifted
nFreqs =5; % number of +/- frequencies about 0th frequency

% if mod(N,2) == 0
%   zerothFreq = N/2;  
% else
%   zerothFreq = (N-(N-1)/2);
% end
[val, zerothFreq ] = max(Xmag);

freqKeepIndex = unique([zerothFreq-nFreqs:zerothFreq, zerothFreq:zerothFreq+nFreqs]);

figure()
plot(1:N,abs(Xshifted))
axis([zerothFreq-100,zerothFreq+100,0,max(abs(Xshifted))])
xlabel('frequency #')
ylabel('abs(X)')
set(gca,'fontsize',14)  

freqKeepIndex = 1101;
z_reduce = zeros(length(Xshifted),1);
% z_reduce(freqKeepIndex) = X(freqKeepIndex);
z_reduce(freqKeepIndex) = Xshifted(freqKeepIndex);
z_reconstruct = ifft(ifftshift(z_reduce));

z_reconstruct = flip(z_reconstruct);

error = sqrt(mean((abs(z- z_reconstruct)).^2))



figure()
% set(gcf,'position',[10,10,1200,450])
% subplot(1,2,1)
set(gcf,'position',[1200,10,600,600])
plot(z,'k.-')
hold on
plot(z_reconstruct,'r.-')
xlabel('$$x$$','interpreter','latex')
ylabel('$$iy$$','interpreter','latex')
set(gca,'fontsize',14)
title(['DFT reconstruction: 0^{th} and first ' num2str(nFreqs) ' +/- frequencies'])
legend( 'Original','Approximation', 'Location', 'NorthEast' );
t = text(0.5*max(z),min(z)+(max(z)-min(z))*0.1,['RMSE = ' num2str(round(error,3,'significant'))]);
t.FontSize = 14;
t.FontWeight = 'bold';
t.HorizontalAlignment = 'center';
% minZ = -min(abs(z));
% maxZ = max(abs(z));
% axis([minZ,maxZ,minZ,maxZ]*2)
lim = 70;
axis([-lim,lim,-lim,lim])





%% Reconstruct Approximate Signal - 90% energy signal


close all

energyMax = 0.9;

xOriginal = ifftshift(Xshifted);
XmagShifted = abs(Xshifted);

% if XmagOriginal(2)>XmagOriginal(1)
%     highest = XmagOriginal(2);
%     secondHighest = XmagOriginal(1);
%     XmagOriginal(1) = highest;
%     XmagOriginal(2) = secondHighest;
% end
[XmagShiftSorted ,sortedIndices ] =  sort(-XmagShifted);
XmagShiftSorted = -1*XmagShiftSorted;
totalEnergy = sum(XmagShiftSorted(2:end));
amplitudeCDF = zeros(length(XmagShiftSorted)-1,1);
for i = 1:length(amplitudeCDF)
    amplitudeCDF(i) = sum(XmagShiftSorted(2:i+1));
end

% # modes needed to get 90% of energy
[val, numMode90] = min( abs(amplitudeCDF-energyMax*totalEnergy));
numMode90=numMode90+1;



figure()
plot(1:length(amplitudeCDF),amplitudeCDF,'b-','linewidth',2')
hold on 
plot([0,length(z)],[amplitudeCDF(numMode90) amplitudeCDF(numMode90)],'k--','linewidth',2')
legend('Amplitude CDF','90% signal energy')
set(gca,'fontsize',14) 




figure()
set(gcf,'color','white','position',[500,0,600,400])
plot(freq,abs(Xshifted),'k-','linewidth',2')
axis([freq(zerothFreq-100),freq(zerothFreq+100),0,max(abs(Xshifted))])
xlabel('frequency #')
ylabel('abs(X)')
set(gca,'fontsize',14) 
hold on
plot(freq(freqKeepIndex),abs(Xshifted(freqKeepIndex)),'ro')
legend('Full spectrum','Frequencies in 90% signal')


freqKeepIndex = sortedIndices(1:numMode90);
z_reduce = zeros(length(Xshifted),1);
z_reduce(freqKeepIndex) = Xshifted(freqKeepIndex);
z_reconstruct = ifft(ifftshift(z_reduce));

z_reconstruct = flip(z_reconstruct);

error = sqrt(mean((abs(z- z_reconstruct)).^2))



figure()
% set(gcf,'position',[10,10,1200,450])
% subplot(1,2,1)
set(gcf,'position',[1200,10,600,600])
plotOrig = plot(z,'k-','linewidth',2)
plotOrig.Color(4) = 0.7;
hold on
plotReco = plot(z_reconstruct,'r-','linewidth',2)
plotReco.Color(4) = 0.7;
xlabel('$$x$$','interpreter','latex')
ylabel('$$iy$$','interpreter','latex')
set(gca,'fontsize',14)
title(['DFT reconstruction - 90% energy signal, # modes: ' num2str(numMode90)])
legend( 'Original','Approximation', 'Location', 'NorthEast' );
t = text(0.5*max(z),min(z)+(max(z)-min(z))*0.1,['RMSE = ' num2str(round(error,3,'significant'))]);
t.FontSize = 14;
t.FontWeight = 'bold';
t.HorizontalAlignment = 'center';
minZ = -min(abs(z));
maxZ = max(abs(z));
% axis([minZ,maxZ,minZ,maxZ]*2)


%%

sum(abs(z))/length(z)


%% sum of e^2pi i n /N
clc

sumK1 = 0;
N = length(z);
sumSeries = zeros(N,1);
for n= 0:N-1
    sumK1 = sumK1 + z(n+1)*exp(1i*2*pi*n/N);
    sumSeries(n+1) = sumK1;
end
sumK1

abs(sumK1)

figure()
plot(1:N,imag(sumSeries))



