% Generate three types of image sets of all organoids
% 1. Thumbnails of contour series (set thumbnail = 1 below)
% 2. Full image of contour series (set thumbnail = 0 below)
% 3. First and last contour supreimposed (third section below)


% Load z = x + iy for all data
load('zAllOrganoids.mat')
%% test single contour
organoid = 39;
zOrganoid = zAll(:,organoid);

close all
t = 261;
x = real(zOrganoid{t,:})';
y = -imag(zOrganoid{t,:})';

lim = 180;

plot(x,y,'k-','lineWidth',0.5)
hold on
plot(0,0,'ro')
axis([-lim,lim,-lim,lim])
axis square


xBar = [0 100 100 0];
yBar = [0.1 0.1 0.16 0.16]*lim/2-lim;
fill(xBar,yBar,[0 0 0])
text(-70, yBar(3), '100 $$\mu$$m','interpreter','latex','fontsize',12)


        
%% Thumbnails or full photos
close all
clc
thumbnail = 0;
if thumbnail == 0
    mkdir image_fullImage
elseif thumbnail == 1
    mkdir image_thumbnail
end
fontSize = 8;
fontSizeTitle = 8;

for organoid=4:40
    %   organoid = 1;
    if organoid <= 10
        type = 'Basal';
    else
        type = 'FGF2';
    end
    
    zOrganoid = zAll(:,organoid);
    numTimePoints = find(~cellfun(@isempty,zOrganoid), 1, 'last' );
    
    
    % % Perimeter and Area time history
    % figure()
    % set(gcf,'position',[100,100,1600,800])
    % subplot(2,2,1)
    % plot(1:numTimePoints,perimeterList,'k-', 'linewidth',2)
    % xlabel('Time points')
    % ylabel('Perimeter')
    % set(gca,'fontsize',14)
    % subplot(2,2,3)
    % plot(1:numTimePoints, areaList,'k-', 'linewidth',2)
    % xlabel('Time points')
    % ylabel('Area')
    % set(gca,'fontsize',14)
    % lim = 0.6*max(perimeterList)/pi;
    
    zArray = [];
    for t = 1:numTimePoints
        zArray = horzcat(zArray,zOrganoid{t,:});
    end
    lim = round(1.05*max(abs(zArray)));
    
    if thumbnail == 0
        sizeMainX = 1.75;
        sizeMainY = 2;
        figure()
        fig = gcf;
        fig.Units = 'inches';
        fig.OuterPosition = [0 0 sizeMainX sizeMainY];
        fig.InnerPosition = [0 0 sizeMainX sizeMainY];
        fig.PaperUnits = 'inches';
        fig.PaperSize = [sizeMainX sizeMainY]; 
        fig.PaperPosition = [0 0 sizeMainX sizeMainY];
        fig.Position = [ 5 5 sizeMainX sizeMainY];
    else
        figure()
        set(gcf,'position',[10,10,800,800])
    end
    % subplot(2,2,[2,4])
    if strcmp(type,'Basal')
        p = colormap(winter(numTimePoints));
    else
        p = colormap(copper(numTimePoints));
    end
    
    
    for i = 1:numTimePoints
        
        x = real(zOrganoid{i,:})';
        y = imag(zOrganoid{i,:})';
        
        % Isolated image
        if thumbnail == 1
            plot(x,y,'k-','lineWidth',0.5,'Color', p(i,:))
        else
            plot(x,y,'k-','lineWidth',0.5,'Color', p(i,:))
        end
        hold on
        plot(0,0,'ro')
        axis([-lim,lim,-lim,lim])
        axis square
        hold on
        if thumbnail == 1
            box off
            axis off;
        end
    end
    
    if thumbnail == 0
        c = colorbar('Ticks',[0,1],'TickLabels',{'t = 0','t=t_f'},'fontsize',6,'location','southoutside');
        ax = gca;
        axpos = ax.Position;
        c.Position(1) = c.Position(1)-0.05;
        c.Position(2) = c.Position(2)-0.24;
        c.Position(4) = 0.7*c.Position(4); % height (thickness) of horizontal colorbar
        c.Position(3) = 1.4*c.Position(3);
        ax.Position = axpos;
        set(gca,'fontsize',fontSize)
        % xlabel('X')
        % ylabel('Y')
        hold on
        plot(0,0,'k+','MarkerSize',10)
        hold on
        plot(0,0,'ko','MarkerSize',10)
        %         t = title({['Type: ' type ];['Organoid: ' num2str(organoid)]});
        t = title([type ', Organoid: ' num2str(organoid)]);
        set(t,'interpreter','none','fontsize',fontSizeTitle)
        hold off
    end
    
    if thumbnail == 0
        
        % position = [left bottom width height]
        axesMain = gca;
%         axesMain.Position(1) = axesMain.Position(1)- 0.15;   % Left
        axesMain.Position(2) = axesMain.Position(2) - 0.3;  % Bottom
%         axesMain.Position(3) = axesMain.Position(3)*1;    % Width
        axesMain.Position(3) = 0.85;    % Width
%         axesMain.Position(4) = axesMain.Position(4)*1;    % Height
        axesMain.Position(4) = 0.65;    % Height
 
        %         filename = ['organoid_timeseries_' num2str(organoid) '.png'];
        %         saveas(gcf,filename)
        %         filename = ['organoid_timeseries_' num2str(organoid) '.eps'];
        %         print(filename,'-depsc')
        %         filename = ['image_fullImage/organoid_timeseries_' num2str(organoid) '.tiff'];
        %         print(filename,'-dtiff','-r400')
        filename = ['image_fullImage/organoid_timeseries_' num2str(organoid) '.pdf'];
        print(filename,'-painters','-dpdf')
    else
        fig = gcf;
        fig.PaperUnits = 'inches';
        fig.PaperPosition = [0 0 2 2];
        filename = ['image_thumbnail/organoid_timeseries_thmb_' num2str(organoid)];
        print(filename, '-dpng','-r200')
    end

end


close all
%% First & Last contour

thumbnail = 1;
fontSize = 30;
fontSizeTitle = 40;

mkdir image_firstLast
pFirst = [110 239 151; 242 113 201]/255;
pLast = [8, 79, 31;99, 17, 73]/255;
for organoid=1:40
    %   organoid = 1;
    if organoid <= 10
        type = 'Basal';
    else
        type = 'FGF2';
    end
    
    zOrganoid = zAll(:,organoid);
    numTimePoints = find(~cellfun(@isempty,zOrganoid), 1, 'last' );
    
    
    zArray = [];
    t = 1;
    zArray = horzcat(zArray,zOrganoid{t,:});
    t = numTimePoints;
    zArray = horzcat(zArray,zOrganoid{t,:});
    lim = round(1.05*max(abs(zArray)));
    
    
    
    
    
    figure()
    set(gcf,'position',[10,10,800,800])

    % First contour
    i = 1;
    xFirst = real(zOrganoid{i,:})';
    yFirst = imag(zOrganoid{i,:})';
    [areaFirst, ~, ~] = geom(xFirst,yFirst);
    
    % Last contour
    i = numTimePoints;
    xLast = real(zOrganoid{i,:})';
    yLast = imag(zOrganoid{i,:})';
    [areaLast, ~, ~] = geom(xLast,yLast);
    
    % Isolated image
    if strcmp(type,'Basal')
        if areaFirst < areaLast
            plot(xLast,yLast,'k-','lineWidth',1,'Color', pLast(1,:))
            fill(xLast,yLast,pLast(1,:))
            hold on
            plot(xFirst,yFirst,'k-','lineWidth',1,'Color', pLast(1,:))
            fill(xFirst,yFirst,pFirst(1,:));
        else
            plot(xFirst,yFirst,'k-','lineWidth',1,'Color', pLast(1,:))
            fill(xFirst,yFirst,pFirst(1,:));
            hold on
            plot(xLast,yLast,'k-','lineWidth',1,'Color', pLast(1,:))
            fill(xLast,yLast,pLast(1,:))
        end
    else
        if areaFirst < areaLast
            plot(xLast,yLast,'k-','lineWidth',1,'Color', pLast(2,:))
            fill(xLast,yLast,pLast(2,:))
            hold on
            plot(xFirst,yFirst,'k-','lineWidth',1,'Color', pLast(2,:))
            fill(xFirst,yFirst,pFirst(2,:))
        else
            plot(xFirst,yFirst,'k-','lineWidth',1,'Color', pLast(2,:))
            fill(xFirst,yFirst,pFirst(2,:))
            hold on
            plot(xLast,yLast,'k-','lineWidth',1,'Color', pLast(2,:))
            fill(xLast,yLast,pLast(2,:))
        end
        
        
    end

    
    hold on
    plot(0,0,'ro')
    axis([-lim,lim,-lim,lim])
    axis square
    hold on
    box off
    axis off;
    
    
    
    
    fig = gcf;
    fig.PaperUnits = 'inches';
    fig.PaperPosition = [0 0 2 2];
    filename = ['image_firstLast/organoid_timeseries_last_contour_' num2str(organoid)];
    print(filename, '-dpng','-r200')
    
    
    
    
end


